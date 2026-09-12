import copy
import hashlib
import json
import sys
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
REPO = ROOT.parent
sys.path.insert(0, str(ROOT / "src"))

from ai_agent_workflow.schema_validation import SchemaValidationError, validate_document  # noqa: E402


COMPILER_PATH = "agent-workflows/src/ai_agent_workflow/software_profiles.py"
COMPILER_DIGEST = "sha256:e0618b987f356e6348c28bf51c952586fdd21e894fa014c36434d56fc06af12c"
CURRENT_WAVE_2 = {
    COMPILER_PATH: COMPILER_DIGEST,
    "agent-workflows/tests/test_software_profiles.py": "sha256:b125acb66b7e820025726d0bd2c3e4d0758d23c270b1cc146d791c2383851c09",
    "agent-workflows/schemas/software-profile-result-v1.schema.json": "sha256:b3a1168f04a83697574852e86c456a0ae5e4052db2a53331e3c2e0212cd5ca60",
    "agent-workflows/schemas/software-profile-manifest-v1.schema.json": "sha256:1c090552dffcec60cd64c8395b091ab843cf674f7570fcb97bd7f1f9c05f016d",
}
ACCEPTED_UPSTREAM = {
    "agent-workflows/catalog.yaml": "sha256:2fd1caaddf4509d2f56bd31a68b062290e70f2f2da33facf1d8187d476bb3f1d",
    "agent-workflows/src/ai_agent_workflow/workflow_composition.py": "sha256:380d7186ea66bfec18491deaee3d0439f3d93f100ac7fbc5109842b453fe40c3",
    "agent-workflows/src/ai_agent_workflow/planning_system.py": "sha256:bfcddf4d99d2c4584225bfe75e9be09bb969a8c121b1b095a50496a97403e2c0",
    "agent-workflows/src/ai_agent_workflow/execution_group.py": "sha256:745c63fd8ca761148b91d116549a71bb140d02e65ada39b8a1148e67abaeba9a",
    "agent-workflows/groups/planning.json": "sha256:8efffa6dcfadf818685dfd11658ab770828736ebe0a559ef1189b5ea8cd0ba80",
    "agent-workflows/groups/execution.json": "sha256:00c97a8fa32c90048bfde36be9a4491ccb059da1a349b0615014a44da83775bc",
    "docs/plans/ai-agent-workflow-step-catalog.md": "sha256:5bfbb5342bb8d3fbc1f85876ce588adb00eedaa4d961e41b14121603ead1da4b",
}
HOSTS = {
    "feature": ["group.D.D4", "group.D.D4", "group.D.D4", "group.D.D6", "group.E.E3", "group.E.E3", "group.E.E3", "group.E.E9"],
    "bug-fix": ["group.D.D2", "group.D.D2", "group.D.D2", "group.D.D2", "group.D.D2", "group.D.D5", "group.E.E3", "group.E.E9"],
    "improvement": ["group.D.D2", "group.D.D2", "group.D.D2", "group.D.D6", "group.E.E3", "group.E.E9", "group.E.E9"],
}


class BindingError(ValueError):
    pass


def raw(path):
    return "sha256:" + hashlib.sha256((REPO / path).read_bytes()).hexdigest()


def load(path):
    return json.loads((REPO / path).read_text(encoding="utf-8"))


def method_name(profile, local_id):
    return "test_%s_%s" % (profile.replace("-", "_"), local_id.lower())


def test_selector(profile, local_id):
    return "%s::SoftwareProfileSelectorEvidenceTests::%s" % (
        "agent-workflows/tests/test_software_profiles_binding.py",
        method_name(profile, local_id),
    )


def require(condition, message):
    if not condition:
        raise BindingError(message)


def validate_manifest(profile, document=None):
    manifest = copy.deepcopy(document) if document is not None else load("agent-workflows/profiles/%s.json" % profile)
    manifest_schema = load("agent-workflows/schemas/software-profile-manifest-v1.schema.json")
    evidence_schema = load("agent-workflows/schemas/qualified-contract-acceptance-v1.schema.json")
    validate_document(manifest, manifest_schema, manifest_schema["$defs"])

    catalog = load("agent-workflows/catalog.yaml")
    expected = catalog["profile_steps"][profile]
    contracts = manifest["contracts"]
    require([item["order"] for item in contracts] == list(range(1, len(expected) + 1)), "reordered profile contracts")
    require([item["local_id"] for item in contracts] == [item[0] for item in expected], "local identity drift")
    require([item["contract_name"] for item in contracts] == [item[1] for item in expected], "catalog name drift")
    require([item["qualified_id"] for item in contracts] == ["profile.%s.%s" % (profile, item[0]) for item in expected], "qualified identity drift")
    require([item["host_selector"] for item in contracts] == HOSTS[profile], "host mapping drift")

    evidence_paths = set()
    selectors = set()
    compiler_refs = set()
    for item in contracts:
        local_id = item["local_id"]
        for reference in ("template_ref", "selector_ref", "evidence_ref"):
            require(raw(item[reference]["path"]) == item[reference]["digest"], "digest drift: %s" % reference)
        require(item["selector_ref"]["selector"] == "SoftwareProfileV1.compile(%s)" % item["qualified_id"], "compiler selector drift")
        compiler_refs.add((item["selector_ref"]["path"], item["selector_ref"]["digest"]))
        require(item["evidence_ref"]["path"] not in evidence_paths, "duplicate evidence path")
        evidence_paths.add(item["evidence_ref"]["path"])

        evidence = load(item["evidence_ref"]["path"])
        validate_document(evidence, evidence_schema, evidence_schema.get("$defs", {}))
        require(evidence["contract_id"] == item["qualified_id"], "evidence identity drift")
        require(evidence["contract_name"] == item["contract_name"], "evidence name drift")
        require(evidence["implementation_kind"] == "workflow-profile-step", "implementation kind drift")
        require(evidence["implementation_ref"] == {"path": item["selector_ref"]["path"], "digest": item["selector_ref"]["digest"]}, "implementation ref drift")
        expected_selector = test_selector(profile, local_id)
        require(evidence["test_ref"]["selector"] == expected_selector, "test selector drift")
        require(expected_selector not in selectors, "duplicate test selector")
        selectors.add(expected_selector)
        require(evidence["status"] == "passed", "evidence is not passed")
    require(compiler_refs == {(COMPILER_PATH, COMPILER_DIGEST)}, "shared compiler drift")
    return manifest


class SoftwareProfilePhysicalBindingTests(unittest.TestCase):
    def assert_rejected(self, profile, manifest):
        with self.assertRaises((BindingError, SchemaValidationError)):
            validate_manifest(profile, manifest)

    def test_three_exact_manifests_catalog_evidence_and_raw_digests(self):
        compiler_refs = set()
        counts = {}
        for profile in HOSTS:
            manifest = validate_manifest(profile)
            counts[profile] = len(manifest["contracts"])
            compiler_refs.update((item["selector_ref"]["path"], item["selector_ref"]["digest"]) for item in manifest["contracts"])
        self.assertEqual({"feature": 8, "bug-fix": 8, "improvement": 7}, counts)
        self.assertEqual({(COMPILER_PATH, COMPILER_DIGEST)}, compiler_refs)

    def test_current_wave_2_and_accepted_upstream_are_unchanged(self):
        for path, digest in {**CURRENT_WAVE_2, **ACCEPTED_UPSTREAM}.items():
            with self.subTest(path=path):
                self.assertEqual(digest, raw(path))

    def test_identity_host_path_selector_order_and_digest_mutations_fail_closed(self):
        base = validate_manifest("bug-fix")
        mutations = {}
        for field in ("qualified_id", "local_id", "contract_name"):
            changed = copy.deepcopy(base)
            changed["contracts"][0][field], changed["contracts"][1][field] = changed["contracts"][1][field], changed["contracts"][0][field]
            mutations[field + "-swap"] = changed
        changed = copy.deepcopy(base)
        changed["contracts"][0]["host_selector"], changed["contracts"][5]["host_selector"] = changed["contracts"][5]["host_selector"], changed["contracts"][0]["host_selector"]
        mutations["host-swap"] = changed
        changed = copy.deepcopy(base)
        changed["contracts"][0]["evidence_ref"]["path"], changed["contracts"][1]["evidence_ref"]["path"] = changed["contracts"][1]["evidence_ref"]["path"], changed["contracts"][0]["evidence_ref"]["path"]
        mutations["path-swap"] = changed
        changed = copy.deepcopy(base)
        changed["contracts"][0]["selector_ref"]["selector"], changed["contracts"][1]["selector_ref"]["selector"] = changed["contracts"][1]["selector_ref"]["selector"], changed["contracts"][0]["selector_ref"]["selector"]
        mutations["selector-swap"] = changed
        changed = copy.deepcopy(base); changed["contracts"].reverse(); mutations["order-swap"] = changed
        changed = copy.deepcopy(base); changed["contracts"].pop(); mutations["missing"] = changed
        changed = copy.deepcopy(base); changed["contracts"][1] = copy.deepcopy(changed["contracts"][0]); mutations["duplicate"] = changed
        changed = copy.deepcopy(base); changed["contracts"][0]["qualified_id"] = "profile.bug-fix.UNKNOWN"; mutations["unknown"] = changed
        for reference in ("template_ref", "selector_ref", "evidence_ref"):
            changed = copy.deepcopy(base); changed["contracts"][0][reference]["digest"] = "sha256:" + "0" * 64
            mutations[reference + "-digest-drift"] = changed
        changed = copy.deepcopy(base); changed["unexpected"] = True; mutations["unknown-field"] = changed
        for name, manifest in mutations.items():
            with self.subTest(mutation=name):
                self.assert_rejected("bug-fix", manifest)

    def test_cross_profile_template_and_evidence_substitution_fail_closed(self):
        feature = validate_manifest("feature")
        bug_fix = validate_manifest("bug-fix")
        template = copy.deepcopy(feature)
        template["contracts"][0]["template_ref"] = copy.deepcopy(bug_fix["contracts"][0]["template_ref"])
        self.assert_rejected("feature", template)
        evidence = copy.deepcopy(feature)
        evidence["contracts"][0]["evidence_ref"] = copy.deepcopy(bug_fix["contracts"][0]["evidence_ref"])
        self.assert_rejected("feature", evidence)


class SoftwareProfileSelectorEvidenceTests(unittest.TestCase):
    pass


def selector_test(profile, index):
    def test(self):
        manifest = validate_manifest(profile)
        item = manifest["contracts"][index]
        evidence = load(item["evidence_ref"]["path"])
        self.assertEqual(item["qualified_id"], evidence["contract_id"])
        self.assertEqual(test_selector(profile, item["local_id"]), evidence["test_ref"]["selector"])

    return test


for _profile, _hosts in HOSTS.items():
    _steps = load("agent-workflows/catalog.yaml")["profile_steps"][_profile]
    for _index, (_local_id, _name) in enumerate(_steps):
        setattr(SoftwareProfileSelectorEvidenceTests, method_name(_profile, _local_id), selector_test(_profile, _index))


if __name__ == "__main__":
    unittest.main()
