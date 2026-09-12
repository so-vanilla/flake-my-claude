from __future__ import annotations

import copy
import hashlib
import json
import subprocess
import sys
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
REPO = ROOT.parent
sys.path.insert(0, str(ROOT / "src"))

from ai_agent_workflow.schema_validation import SchemaValidationError, validate_document  # noqa: E402


COMPILER_PATH = "agent-workflows/src/ai_agent_workflow/decision_outcome_lifecycle.py"
COMPILER_DIGEST = "sha256:43bd36552dbba8fe37e074965fedae638093c704ca3caddfd6fb70b21863f22b"
FIXED_HEAD = "e5b87e08d0730e0f2a2c99c9a4883a172c0078e7"
CURRENT_WAVE_2 = {
    COMPILER_PATH: COMPILER_DIGEST,
    "agent-workflows/schemas/decision-outcome-lifecycle-v1.schema.json": "sha256:91dc5009bf771f078cdd2b0e46c8cf69de755e8c70c830a203b4bd851e55f57e",
    "agent-workflows/tests/test_decision_outcome_lifecycle.py": "sha256:658ab36733fbcc131eeae8b47d0cff46fa8d8684af0dc6379f4486c90eef48db",
}
GROUPS = {
    "G": {
        "manifest": "agent-workflows/groups/decisions.json",
        "schema": "agent-workflows/schemas/group-g-manifest-v1.schema.json",
        "classes": [
            "G1_ScanDecisionEvidenceTests",
            "G2_ClassifyDecisionStatusTests",
            "G3_DeduplicateAndLinkTests",
            "G4_DraftDurableRecordsTests",
            "G5_ApprovePromotionTests",
            "G6_PromoteAndBacklinkTests",
        ],
    },
    "H": {
        "manifest": "agent-workflows/groups/completion.json",
        "schema": "agent-workflows/schemas/group-h-manifest-v1.schema.json",
        "classes": [
            "H1_AuditObjectiveTests",
            "H2_DecideRunOutcomeTests",
            "H3_ArchiveOrContinueTests",
        ],
    },
}


class BindingError(ValueError):
    pass


def require(condition, message):
    if not condition:
        raise BindingError(message)


def raw(path):
    return "sha256:" + hashlib.sha256((REPO / path).read_bytes()).hexdigest()


def load(path):
    return json.loads((REPO / path).read_text(encoding="utf-8"))


def frontmatter(path):
    content = (REPO / path).read_text(encoding="utf-8")
    require(content.startswith("---\n"), "Skill frontmatter is missing")
    block = content.split("---", 2)[1]
    values = {}
    for line in block.splitlines():
        key, separator, value = line.partition(":")
        if separator:
            values[key.strip()] = value.strip()
    return values


def evidence_selector(item):
    method = "test_%s_%s" % (
        item["local_id"].lower(),
        item["contract_name"].replace("-", "_"),
    )
    return "agent-workflows/tests/test_decision_outcome_binding.py::DecisionOutcomeSelectorEvidenceTests::%s" % method


def validate_evidence(item, document=None):
    evidence = copy.deepcopy(document) if document is not None else load(item["evidence_ref"]["path"])
    schema = load("agent-workflows/schemas/qualified-contract-acceptance-v1.schema.json")
    validate_document(evidence, schema, schema.get("$defs", {}))
    require(evidence["contract_id"] == item["qualified_id"], "evidence identity drift")
    require(evidence["contract_name"] == item["contract_name"], "evidence catalog name drift")
    require(evidence["implementation_kind"] == "skill", "evidence implementation kind drift")
    require(evidence["implementation_ref"] == item["source_ref"], "evidence source binding drift")
    require(evidence["test_ref"]["selector"] == evidence_selector(item), "evidence selector drift")
    require(evidence["status"] == "passed", "evidence is not passed")
    forbidden_claims = {
        "grants_approval",
        "performs_promotion",
        "objective_outcome_claimed",
        "performs_archive",
        "performs_cleanup",
    }
    require(forbidden_claims.isdisjoint(evidence), "evidence claims live lifecycle effects")
    return evidence


def validate_group(group, document=None):
    settings = GROUPS[group]
    manifest = copy.deepcopy(document) if document is not None else load(settings["manifest"])
    schema = load(settings["schema"])
    validate_document(manifest, schema, schema["$defs"])

    expected = load("agent-workflows/catalog.yaml")["named_contracts"][group]
    contracts = manifest["contracts"]
    require(manifest["group"] == group, "manifest group drift")
    require(manifest["group_id"] == "agent-workflows.group.%s" % group, "manifest group identity drift")
    require(manifest["implementation_kind"] == "skill", "implementation kind drift")
    require(len(contracts) == len(expected), "contract coverage drift")
    require([item["order"] for item in contracts] == list(range(1, len(expected) + 1)), "contract order drift")
    require([item["local_id"] for item in contracts] == [item[0] for item in expected], "local identity drift")
    require([item["contract_name"] for item in contracts] == [item[1] for item in expected], "catalog name drift")
    require([item["qualified_id"] for item in contracts] == ["group.%s.%s" % (group, item[0]) for item in expected], "qualified identity drift")
    require([item["group"] for item in contracts] == [group] * len(expected), "contract group drift")
    require([item["acceptance_class"] for item in contracts] == settings["classes"], "acceptance class drift")

    sources = set()
    selectors = set()
    evidence_paths = set()
    acceptance_classes = set()
    compiler_refs = set()
    for item in contracts:
        for reference in ("source_ref", "selector_ref", "evidence_ref"):
            require(raw(item[reference]["path"]) == item[reference]["digest"], "raw digest drift: %s" % reference)
        metadata = frontmatter(item["source_ref"]["path"])
        require(metadata.get("name") == item["contract_name"], "Skill/catalog name drift")
        acceptance_ref = metadata.get("acceptance-test", "")
        require(acceptance_ref.rsplit("::", 1)[-1] == item["acceptance_class"], "Skill acceptance class drift")
        require(item["selector_ref"]["selector"] == "DecisionOutcomeLifecycleV1.compile(%s)" % item["qualified_id"], "compiler selector drift")
        validate_evidence(item)

        source_key = (item["source_ref"]["path"], item["source_ref"]["digest"])
        require(source_key not in sources, "duplicate Skill source")
        sources.add(source_key)
        require(item["selector_ref"]["selector"] not in selectors, "duplicate compiler selector")
        selectors.add(item["selector_ref"]["selector"])
        require(item["evidence_ref"]["path"] not in evidence_paths, "duplicate evidence path")
        evidence_paths.add(item["evidence_ref"]["path"])
        require(item["acceptance_class"] not in acceptance_classes, "duplicate acceptance class")
        acceptance_classes.add(item["acceptance_class"])
        compiler_refs.add((item["selector_ref"]["path"], item["selector_ref"]["digest"]))
    require(compiler_refs == {(COMPILER_PATH, COMPILER_DIGEST)}, "shared compiler drift")
    return manifest


class DecisionOutcomePhysicalBindingTests(unittest.TestCase):
    def assert_rejected(self, group, manifest):
        with self.assertRaises((BindingError, SchemaValidationError)):
            validate_group(group, manifest)

    def test_exact_six_and_three_catalog_bindings_and_raw_digests(self):
        manifests = {group: validate_group(group) for group in GROUPS}
        self.assertEqual({"G": 6, "H": 3}, {group: len(value["contracts"]) for group, value in manifests.items()})
        self.assertEqual(
            {"group.G.G%d" % number for number in range(1, 7)} | {"group.H.H%d" % number for number in range(1, 4)},
            {item["qualified_id"] for manifest in manifests.values() for item in manifest["contracts"]},
        )

    def test_current_wave_two_is_unchanged_and_baseline_is_retained(self):
        for path, digest in CURRENT_WAVE_2.items():
            with self.subTest(path=path):
                self.assertEqual(digest, raw(path))
        if not (REPO / ".git").exists():
            # Packaged sources have no Git history; physical bindings above
            # still apply. A published checkout may advance past the baseline.
            return
        ancestry = subprocess.run(
            ["git", "merge-base", "--is-ancestor", FIXED_HEAD, "HEAD"],
            cwd=REPO,
            text=True,
            capture_output=True,
        )
        self.assertEqual(0, ancestry.returncode, ancestry.stderr)

    def test_missing_duplicate_extra_and_unknown_keys_fail_closed(self):
        base = validate_group("G")
        mutations = {}
        changed = copy.deepcopy(base); changed["contracts"].pop(); mutations["missing"] = changed
        changed = copy.deepcopy(base); changed["contracts"][1] = copy.deepcopy(changed["contracts"][0]); mutations["duplicate"] = changed
        changed = copy.deepcopy(base); changed["contracts"].append(copy.deepcopy(changed["contracts"][-1])); mutations["extra"] = changed
        changed = copy.deepcopy(base); changed["contracts"][0]["qualified_id"] = "group.G.UNKNOWN"; mutations["unknown-identity"] = changed
        changed = copy.deepcopy(base); changed["unexpected"] = True; mutations["unknown-top-level-key"] = changed
        changed = copy.deepcopy(base); changed["contracts"][0]["source_ref"]["unexpected"] = True; mutations["unknown-nested-key"] = changed
        for name, manifest in mutations.items():
            with self.subTest(mutation=name):
                self.assert_rejected("G", manifest)

    def test_identity_name_order_path_digest_selector_evidence_and_group_swaps_fail_closed(self):
        base = validate_group("G")
        mutations = {}
        for field in ("qualified_id", "local_id", "contract_name", "order", "acceptance_class"):
            changed = copy.deepcopy(base)
            changed["contracts"][0][field], changed["contracts"][1][field] = changed["contracts"][1][field], changed["contracts"][0][field]
            mutations[field + "-swap"] = changed
        changed = copy.deepcopy(base); changed["contracts"].reverse(); mutations["array-order-swap"] = changed
        for reference in ("source_ref", "evidence_ref"):
            changed = copy.deepcopy(base)
            changed["contracts"][0][reference], changed["contracts"][1][reference] = changed["contracts"][1][reference], changed["contracts"][0][reference]
            mutations[reference + "-swap"] = changed
        changed = copy.deepcopy(base)
        changed["contracts"][0]["selector_ref"]["selector"], changed["contracts"][1]["selector_ref"]["selector"] = changed["contracts"][1]["selector_ref"]["selector"], changed["contracts"][0]["selector_ref"]["selector"]
        mutations["selector-swap"] = changed
        for reference in ("source_ref", "selector_ref", "evidence_ref"):
            changed = copy.deepcopy(base); changed["contracts"][0][reference]["digest"] = "sha256:" + "0" * 64
            mutations[reference + "-digest"] = changed
        changed = copy.deepcopy(base); changed["group"] = "H"; mutations["manifest-group-swap"] = changed
        changed = copy.deepcopy(base); changed["contracts"][0]["group"] = "H"; mutations["contract-group-swap"] = changed
        for name, manifest in mutations.items():
            with self.subTest(mutation=name):
                self.assert_rejected("G", manifest)

    def test_cross_group_source_selector_evidence_and_contract_swaps_fail_closed(self):
        group_g = validate_group("G")
        group_h = validate_group("H")
        swaps = {}
        for reference in ("source_ref", "selector_ref", "evidence_ref"):
            changed = copy.deepcopy(group_g)
            changed["contracts"][0][reference] = copy.deepcopy(group_h["contracts"][0][reference])
            swaps[reference] = changed
        changed = copy.deepcopy(group_g); changed["contracts"][0] = copy.deepcopy(group_h["contracts"][0]); swaps["whole-contract"] = changed
        for name, manifest in swaps.items():
            with self.subTest(mutation=name):
                self.assert_rejected("G", manifest)

    def test_evidence_identity_name_source_selector_status_and_group_swaps_fail_closed(self):
        group_g = validate_group("G")
        group_h = validate_group("H")
        item = group_g["contracts"][0]
        evidence = validate_evidence(item)
        mutations = {}
        changed = copy.deepcopy(evidence); changed["contract_id"] = group_g["contracts"][1]["qualified_id"]; mutations["identity"] = changed
        changed = copy.deepcopy(evidence); changed["contract_name"] = group_g["contracts"][1]["contract_name"]; mutations["name"] = changed
        changed = copy.deepcopy(evidence); changed["implementation_ref"] = copy.deepcopy(group_g["contracts"][1]["source_ref"]); mutations["source"] = changed
        changed = copy.deepcopy(evidence); changed["test_ref"]["selector"] = evidence_selector(group_g["contracts"][1]); mutations["selector"] = changed
        changed = copy.deepcopy(evidence); changed["status"] = "not-run"; mutations["status"] = changed
        changed = copy.deepcopy(evidence); changed["contract_id"] = group_h["contracts"][0]["qualified_id"]; mutations["group"] = changed
        changed = copy.deepcopy(evidence); changed["performs_promotion"] = True; mutations["live-effect-claim"] = changed
        for name, document in mutations.items():
            with self.subTest(mutation=name), self.assertRaises((BindingError, SchemaValidationError)):
                validate_evidence(item, document)


class DecisionOutcomeSelectorEvidenceTests(unittest.TestCase):
    def assert_selector(self, group, index):
        item = validate_group(group)["contracts"][index]
        evidence = validate_evidence(item)
        self.assertEqual(evidence_selector(item), evidence["test_ref"]["selector"])

    def test_g1_scan_decision_evidence(self):
        self.assert_selector("G", 0)

    def test_g2_classify_decision_status(self):
        self.assert_selector("G", 1)

    def test_g3_deduplicate_and_link(self):
        self.assert_selector("G", 2)

    def test_g4_draft_durable_records(self):
        self.assert_selector("G", 3)

    def test_g5_approve_promotion(self):
        self.assert_selector("G", 4)

    def test_g6_promote_and_backlink(self):
        self.assert_selector("G", 5)

    def test_h1_audit_objective(self):
        self.assert_selector("H", 0)

    def test_h2_decide_run_outcome(self):
        self.assert_selector("H", 1)

    def test_h3_archive_or_continue(self):
        self.assert_selector("H", 2)


if __name__ == "__main__":
    unittest.main()
