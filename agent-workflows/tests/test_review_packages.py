import copy
import unittest

from ai_agent_workflow.review_packages import (
    ReviewPackageError,
    accept_review_result,
    build_review_package,
)

DIGEST = "sha256:" + "a" * 64


def ref(identifier):
    return {"id": identifier, "digest": DIGEST}


def candidate():
    return {
        "candidate_ref": ref("candidate"),
        "spec_ref": ref("spec"),
        "dependency_refs": [ref("dependency")],
        "environment_ref": ref("environment"),
        "source_paths": ["src/a.py", "src/b.py"],
    }


def requirements():
    return [
        {"requirement_id": "R1", "requirement_ref": ref("R1"), "scope": ["src/a.py"]},
        {"requirement_id": "R2", "requirement_ref": ref("R2"), "scope": ["src/b.py"]},
    ]


def impact(**changes):
    value = {"known": True, "changed_paths": [], "affected_requirements": [], "affected_interfaces": [], "affected_tests": []}
    value.update(changes)
    return value


def assignment(axis="architecture-safety"):
    return {"assignment_id": "assignment-1", "actor_id": "actor-1", "context_epoch": "epoch-1"}


class ReviewPackageTests(unittest.TestCase):
    def test_initial_review_is_complete_and_physical(self):
        package = build_review_package(candidate(), requirements(), [], impact(), requested_mode="initial", axis="architecture-safety", assignment=assignment())
        self.assertEqual(["R1", "R2"], package["required_coverage"])
        self.assertEqual(["src/a.py", "src/b.py"], package["read_scope"])
        self.assertNotIn("conversation", package)

    def test_delta_review_contains_finding_and_changed_impact_only(self):
        findings = [{"finding_id": "F1", "finding_ref": ref("F1"), "status": "resolved", "scope": ["src/a.py"], "resolution_ref": ref("resolution") }]
        package = build_review_package(candidate(), requirements(), findings, impact(changed_paths=["src/a.py"], affected_requirements=["R1"], affected_tests=["test-a"]), requested_mode="delta", axis="integration-operability", assignment=assignment())
        self.assertEqual("delta", package["mode"])
        self.assertEqual(["R1"], package["required_coverage"])
        self.assertEqual(["src/a.py"], package["read_scope"])

    def test_unknown_impact_expands_delta_to_full_review(self):
        findings = [{"finding_id": "F1", "finding_ref": ref("F1"), "status": "required", "scope": ["src/a.py"], "resolution_ref": None}]
        package = build_review_package(candidate(), requirements(), findings, impact(known=False), requested_mode="delta", axis="architecture-safety", assignment=assignment())
        self.assertEqual("full-impact-unknown", package["mode"])
        self.assertEqual(["R1", "R2"], package["required_coverage"])

    def test_delta_finding_scope_cannot_escape_candidate_source_paths(self):
        findings = [{"finding_id": "F1", "finding_ref": ref("F1"), "status": "resolved", "scope": ["outside/secret"], "resolution_ref": ref("resolution")}]
        with self.assertRaises(ReviewPackageError):
            build_review_package(
                candidate(),
                requirements(),
                findings,
                impact(changed_paths=["src/a.py"], affected_requirements=["R1"]),
                requested_mode="delta",
                axis="architecture-safety",
                assignment=assignment(),
            )

    def test_result_must_cover_and_bind_the_exact_package(self):
        package = build_review_package(candidate(), requirements(), [], impact(), requested_mode="initial", axis="architecture-safety", assignment=assignment())
        result = {
            "schema": "loop-review-assessment/v1", "review_id": "review-1",
            "axis": "architecture-safety", "actor_id": "actor-1", "context_epoch": "epoch-1",
            "candidate_digest": DIGEST, "package_digest": package["package_digest"],
            "coverage": ["R1", "R2"], "completed": True, "unevaluated": [], "finding_refs": [],
        }
        self.assertEqual(result, accept_review_result(package, result))
        with self.assertRaises(ReviewPackageError):
            accept_review_result(package, dict(result, coverage=["R1"]))
        with self.assertRaises(ReviewPackageError):
            accept_review_result(package, dict(result, package_digest="sha256:" + "b" * 64))

    def test_tampered_package_and_unproven_delta_fail_closed(self):
        package = build_review_package(candidate(), requirements(), [], impact(), requested_mode="initial", axis="architecture-safety", assignment=assignment())
        tampered = copy.deepcopy(package)
        tampered["read_scope"].append("src/escape.py")
        result = {
            "schema": "loop-review-assessment/v1", "review_id": "review-1", "axis": "architecture-safety",
            "actor_id": "actor-1", "context_epoch": "epoch-1", "candidate_digest": DIGEST,
            "package_digest": package["package_digest"], "coverage": ["R1", "R2"], "completed": True,
            "unevaluated": [], "finding_refs": [],
        }
        with self.assertRaises(ReviewPackageError):
            accept_review_result(tampered, result)
        with self.assertRaises(ReviewPackageError):
            build_review_package(candidate(), requirements(), [], impact(), requested_mode="delta", axis="architecture-safety", assignment=assignment())


if __name__ == "__main__":
    unittest.main()
