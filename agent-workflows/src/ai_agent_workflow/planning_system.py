"""Pure, digest-bound compiler for the Group D planning contracts.

This is deliberately a planning seam: it copies supplied candidate material,
validates it, and returns either a candidate or an upstream refusal.  It does
not create Run state, update HEAD, grant approval, dispatch a worker, or write
an artifact named by an input reference.
"""
from __future__ import annotations

import copy
import hashlib
import json
from collections.abc import Mapping
from pathlib import Path
from typing import Any

from .schema_validation import SchemaValidationError, validate_document
from .workflow_composition import WorkflowCompositionError, WorkflowCompositionV1


_ROOT = Path(__file__).resolve().parents[2]
_SCHEMA = json.loads((_ROOT / "schemas" / "planning-system-v1.schema.json").read_text())
_BUNDLE_SCHEMA = json.loads((_ROOT / "schemas" / "planning-epoch-bundle-v1.schema.json").read_text())
_IDS = {"group.D.D%d" % number for number in range(1, 13)}
_INVENTORY_AREAS = {
    "entrypoints", "domain_terms", "data_control_flows", "external_contracts",
    "tests", "constraints", "prior_decisions", "verification_surfaces",
}
_SPEC_AREAS = {"behavior", "scenarios", "capabilities", "constraints", "non_goals", "edges", "errors", "acceptance"}
_DESIGN_AREAS = {"architecture", "responsibilities", "interfaces", "flow", "errors", "compatibility", "migration", "observability", "security", "test_seams"}


class PlanningSystemError(ValueError):
    """A compiler context or a physical planning bundle is malformed."""


def _digest(value: Any) -> str:
    return "sha256:" + hashlib.sha256(json.dumps(value, sort_keys=True, separators=(",", ":")).encode()).hexdigest()


def _ref(value: Any) -> bool:
    return isinstance(value, Mapping) and isinstance(value.get("path"), str) and bool(value["path"]) and isinstance(value.get("version"), str) and bool(value["version"]) and isinstance(value.get("digest"), str) and len(value["digest"]) == 71 and value["digest"].startswith("sha256:")


def _refs(value: Any) -> bool:
    return isinstance(value, list) and bool(value) and all(_ref(item) for item in value)


def _contains_placeholder(value: Any) -> bool:
    if isinstance(value, str):
        return any(token in value.lower() for token in ("todo", "tbd", "placeholder", "<unknown>"))
    if isinstance(value, Mapping):
        return any(_contains_placeholder(item) for item in value.values())
    if isinstance(value, list):
        return any(_contains_placeholder(item) for item in value)
    return False


class PlanningSystemV1:
    """Compile Group D candidates without retaining or mutating caller state."""

    def compile(self, qualified_id: str, inputs: Mapping[str, Any], authority: Mapping[str, Any], expected_head: Mapping[str, Any]) -> dict[str, Any]:
        values = copy.deepcopy(dict(inputs)) if isinstance(inputs, Mapping) else {}
        auth = copy.deepcopy(dict(authority)) if isinstance(authority, Mapping) else authority
        head = copy.deepcopy(dict(expected_head)) if isinstance(expected_head, Mapping) else expected_head
        self._context(auth, head)
        if qualified_id not in _IDS:
            return self._refusal("group.D.D1", "unknown_selector", auth, head)
        if values.get("observed_head") is not None and values["observed_head"] != head:
            return self._refusal(qualified_id, "blocked_stale_head", auth, head)
        if values.get("dispatch") or values.get("grant_approval") or values.get("write"):
            return self._refusal(qualified_id, "blocked_side_effect", auth, head)
        handlers = {
            "group.D.D1": self._d1, "group.D.D2": self._d2, "group.D.D3": self._d3,
            "group.D.D4": self._d4, "group.D.D5": self._d5, "group.D.D6": self._d6,
            "group.D.D7": self._d7, "group.D.D8": self._d8, "group.D.D9": self._d9,
            "group.D.D10": self._d10, "group.D.D11": self._d11, "group.D.D12": self._d12,
        }
        output = handlers[qualified_id](values, auth)
        if isinstance(output, str):
            return self._refusal(qualified_id, output, auth, head)
        return self._artifact(qualified_id, values, auth, head, output)

    def compose_epoch(self, bundle: Mapping[str, Any], previous_bundle: Mapping[str, Any] | None = None) -> dict[str, Any]:
        candidate = copy.deepcopy(dict(bundle)) if isinstance(bundle, Mapping) else bundle
        self._validate_bundle(candidate)
        epoch = candidate["epoch_id"]
        if epoch == "D-01":
            if previous_bundle is not None:
                raise PlanningSystemError("D-01 has no predecessor")
            return candidate
        if previous_bundle is None:
            return self._bundle_refusal(epoch, "blocked_stale_predecessor")
        # A predecessor is a supplied closed object, not a request to reopen
        # and recursively compose its own already-bound predecessor.
        previous = copy.deepcopy(dict(previous_bundle)) if isinstance(previous_bundle, Mapping) else previous_bundle
        self._validate_bundle(previous)
        expected = {"D-02": "D-01", "D-03": "D-02"}[epoch]
        binding = candidate.get("predecessor_ref")
        if previous["epoch_id"] != expected or not isinstance(binding, Mapping) or binding.get("epoch_id") != expected or binding.get("digest") != _digest(previous):
            return self._bundle_refusal(epoch, "blocked_stale_predecessor")
        if candidate.get("authority_digest") != previous.get("authority_digest"):
            return self._bundle_refusal(epoch, "blocked_stale_authority")
        return candidate

    @staticmethod
    def _context(authority: Any, head: Any) -> None:
        if not isinstance(authority, Mapping) or set(authority) != {"authority_ref", "namespace", "scope", "owner_ref"} or not _ref(authority.get("authority_ref")) or not _ref(authority.get("owner_ref")) or not isinstance(authority.get("namespace"), str) or not authority["namespace"] or authority.get("scope") not in {"candidate-generic", "fixture-only"}:
            raise PlanningSystemError("authority must bind physical authority/owner refs, namespace, and scope")
        if not isinstance(head, Mapping) or set(head) != {"revision", "transaction_digest"} or not isinstance(head.get("revision"), int) or isinstance(head["revision"], bool) or head["revision"] < 0 or not isinstance(head.get("transaction_digest"), str) or len(head["transaction_digest"]) != 71 or not head["transaction_digest"].startswith("sha256:"):
            raise PlanningSystemError("expected_head must bind nonnegative revision and sha256 transaction_digest")

    def _d1(self, v: Mapping[str, Any], _: Mapping[str, Any]) -> dict[str, Any] | str:
        if v.get("domain") in (None, "", "unresolved"):
            return "needs_user_domain"
        if v.get("lifecycle_owner") in {"AI-DLC", "Superpowers"} or v.get("lifecycle_owner_conflict"):
            return "blocked_lifecycle_owner_conflict"
        manifest_ref = v.get("workflow_manifest_ref")
        if not _ref(manifest_ref):
            return "blocked_unknown_workflow"
        try:
            source_root = _ROOT.parent.resolve()
            path = (source_root / manifest_ref["path"]).resolve()
            if source_root not in path.parents or not path.is_file():
                return "blocked_unknown_workflow"
            raw = path.read_bytes()
            if "sha256:" + hashlib.sha256(raw).hexdigest() != manifest_ref["digest"]:
                return "blocked_unvalidated_workflow"
            manifest = json.loads(raw)
        except (OSError, ValueError, json.JSONDecodeError):
            return "blocked_unvalidated_workflow"
        try:
            receipt = WorkflowCompositionV1(source_root=_ROOT.parent).validate(manifest)
        except WorkflowCompositionError:
            return "blocked_unvalidated_workflow"
        if v.get("workflow_id") not in (None, manifest.get("workflow_id")):
            return "blocked_unknown_workflow"
        return {"kind": "workflow_profile", "workflow_manifest_ref": manifest_ref, "workflow_receipt": receipt, "execution_policy_digest": receipt.get("execution_policy", {}).get("digest")}

    def _d2(self, v: Mapping[str, Any], _: Mapping[str, Any]) -> dict[str, Any] | str:
        inventory = v.get("inventory")
        if not isinstance(inventory, Mapping) or not _INVENTORY_AREAS.issubset(inventory):
            return "blocked_unsourced_gap"
        for key in _INVENTORY_AREAS:
            item = inventory[key]
            if not isinstance(item, Mapping) or not _refs(item.get("source_refs")) or item.get("material_gap"):
                return "blocked_unsourced_gap"
        return {"kind": "current_system", "inventory": inventory}

    def _d3(self, v: Mapping[str, Any], _: Mapping[str, Any]) -> dict[str, Any] | str:
        sources, constraints = v.get("guidance_sources"), v.get("constraints")
        if not _refs(sources) or len(sources) < 2 or not isinstance(constraints, list) or not constraints:
            return "blocked_unsupported_convention"
        if v.get("authority_conflict") or any(not isinstance(item, Mapping) or not _refs(item.get("source_refs")) for item in constraints):
            return "blocked_authority_conflict"
        return {"kind": "global_constraints", "constraints": constraints}

    def _d4(self, v: Mapping[str, Any], _: Mapping[str, Any]) -> dict[str, Any] | str:
        spec = v.get("specification")
        if not isinstance(spec, Mapping) or not _SPEC_AREAS.issubset(spec):
            return "blocked_incomplete_specification"
        if v.get("technical_choices") or spec.get("technical_choices") or _contains_placeholder(spec):
            return "blocked_ambiguous_specification"
        questions = spec.get("questions", [])
        if any(not isinstance(q, Mapping) or not q.get("owner") for q in questions):
            return "blocked_unowned_question"
        return {"kind": "what_why_specification", "specification": spec}

    def _d5(self, v: Mapping[str, Any], _: Mapping[str, Any]) -> dict[str, Any] | str:
        options = v.get("options")
        if v.get("selected_option") is not None or v.get("approval_receipt") is not None:
            return "blocked_implicit_approval"
        if not isinstance(options, list) or not 2 <= len(options) <= 3 or not isinstance(v.get("status_quo"), Mapping):
            return "blocked_insufficient_options"
        if any(not isinstance(o, Mapping) or not o.get("falsification_condition") for o in options):
            return "blocked_insufficient_options"
        if not v.get("recommendation"):
            return "blocked_insufficient_options"
        return {"kind": "unapproved_options", "options": options, "status_quo": v["status_quo"], "recommendation": v["recommendation"]}

    def _d6(self, v: Mapping[str, Any], _: Mapping[str, Any]) -> dict[str, Any] | str:
        receipt, design = v.get("approved_option_receipt"), v.get("design")
        if not isinstance(receipt, Mapping) or not _ref(receipt.get("receipt_ref")) or receipt.get("explicit") is not True or receipt.get("source") != "human" or receipt.get("decision") != "approve" or receipt.get("scope") != "option-selection" or receipt.get("actor_ref") != _.get("owner_ref"):
            return "blocked_missing_approved_option"
        if not isinstance(design, Mapping) or not _DESIGN_AREAS.issubset(design):
            return "blocked_incomplete_design"
        if any(isinstance(item, Mapping) and item.get("critical") and not item.get("owner") for item in design.get("unresolved", [])):
            return "blocked_critical_decision"
        return {"kind": "solution_design", "design": design, "approved_option_receipt": receipt}

    def _d7(self, v: Mapping[str, Any], _: Mapping[str, Any]) -> dict[str, Any] | str:
        contracts = v.get("contracts")
        required = {"owner", "version", "consumes", "produces", "schema_ref", "fixture_refs", "failures", "idempotency", "backward_compatibility", "mutable_writes"}
        if not isinstance(contracts, list) or not contracts:
            return "blocked_ownerless_contract"
        seen: set[str] = set()
        for contract in contracts:
            if not isinstance(contract, Mapping) or not required.issubset(contract) or not contract.get("owner") or not _ref(contract.get("schema_ref")) or not _refs(contract.get("fixture_refs")):
                return "blocked_ownerless_contract"
            writes = set(contract.get("mutable_writes", []))
            if writes.intersection(seen):
                return "blocked_inseparable_write_overlap"
            seen.update(writes)
        return {"kind": "versioned_contracts", "contracts": contracts}

    def _d8(self, v: Mapping[str, Any], _: Mapping[str, Any]) -> dict[str, Any] | str:
        tasks = v.get("tasks")
        required = {"task_id", "files", "interface", "inputs", "outputs", "checks", "stop", "report_path", "parent_outcome_contribution"}
        if not isinstance(tasks, list) or not tasks:
            return "blocked_vague_task"
        if any(not isinstance(t, Mapping) or not required.issubset(t) or not t.get("files") or not t.get("parent_outcome_contribution") or str(t.get("interface", "")).lower() in {"component", "placeholder"} for t in tasks):
            return "blocked_vague_task"
        return {"kind": "task_decomposition", "tasks": tasks}

    def _d9(self, v: Mapping[str, Any], _: Mapping[str, Any]) -> dict[str, Any] | str:
        tasks, edges = v.get("tasks"), v.get("edges", [])
        if not isinstance(tasks, list) or not all(isinstance(t, Mapping) and t.get("task_id") for t in tasks):
            return "blocked_unknown_dag_endpoint"
        ids = {t["task_id"] for t in tasks}
        if any(not isinstance(e, Mapping) or e.get("from") not in ids or e.get("to") not in ids for e in edges):
            return "blocked_unknown_dag_endpoint"
        if self._cyclic(ids, [(e["from"], e["to"]) for e in edges]):
            return "blocked_dependency_cycle"
        writes = {t["task_id"]: set(t.get("files", [])) for t in tasks}
        parallel = v.get("parallel_batches", [])
        serialized: list[list[str]] = []
        edge_pairs = {(e["from"], e["to"]) for e in edges}
        for batch in parallel:
            if not isinstance(batch, list) or not set(batch).issubset(ids):
                return "blocked_unknown_dag_endpoint"
            overlap = any(
                writes[a].intersection(writes[b]) or (a, b) in edge_pairs
                for a in batch for b in batch if a != b
            )
            if overlap:
                serialized.append(list(batch))
        convergence = v.get("convergence", [])
        for point in convergence:
            if not isinstance(point, Mapping) or not point.get("worker") or not point.get("fresh_reviewer") or point.get("worker") == point.get("fresh_reviewer"):
                return "blocked_ownerless_convergence"
        return {"kind": "execution_dag", "task_ids": sorted(ids), "edges": edges, "parallel_batches": [b for b in parallel if b not in serialized], "serialized_batches": serialized, "convergence": convergence}

    def _d10(self, v: Mapping[str, Any], _: Mapping[str, Any]) -> dict[str, Any] | str:
        briefs = v.get("briefs")
        required = {"task_id", "purpose_ref", "task", "interface", "write_scope", "checks", "stop", "report_path", "exploration_refs"}
        if not isinstance(briefs, list) or not briefs:
            return "blocked_missing_brief"
        if any(not isinstance(b, Mapping) or not required.issubset(b) or not _ref(b.get("purpose_ref")) or not _refs(b.get("exploration_refs")) or b.get("whole_plan") for b in briefs):
            return "blocked_ambiguous_brief"
        return {"kind": "worker_briefs", "briefs": briefs}

    def _d11(self, v: Mapping[str, Any], _: Mapping[str, Any]) -> dict[str, Any] | str:
        budgets, gates = v.get("task_budgets"), v.get("gates")
        required_gates = {"test", "review", "finding_validation", "e2e", "dry_run", "rollback", "post_check", "activation", "git", "external"}
        if not isinstance(budgets, list) or not budgets or not isinstance(gates, Mapping) or not required_gates.issubset(gates):
            return "blocked_incomplete_verification"
        for budget in budgets:
            if not isinstance(budget, Mapping) or not isinstance(budget.get("wall_clock_minutes"), int) or budget["wall_clock_minutes"] <= 0 or not isinstance(budget.get("review_rounds"), int) or budget["review_rounds"] <= 0 or not isinstance(budget.get("fix_attempts"), int) or not 0 <= budget["fix_attempts"] <= 5:
                return "blocked_unbounded_budget"
        if v.get("gates_grant_approval"):
            return "blocked_implicit_approval"
        return {"kind": "verification_recovery", "task_budgets": budgets, "gates": gates}

    def _d12(self, v: Mapping[str, Any], _: Mapping[str, Any]) -> dict[str, Any] | str:
        route = v.get("upstream_route")
        evidence = v.get("evidence_refs")
        if not _refs(evidence):
            return {"kind": "readiness", "status": "not_ready", "evidence": [], "upstream_route": route or "group.D.D4"}
        blockers = any(v.get(key) for key in ("contradictions", "placeholders", "critical_unknowns", "stale_authority", "missing_human_approval"))
        risks = v.get("open_risks", [])
        accepted = all(isinstance(r, Mapping) and r.get("accepted") is True and _ref(r.get("receipt_ref")) for r in risks)
        if blockers or (risks and not accepted):
            return {"kind": "readiness", "status": "not_ready", "evidence": evidence, "upstream_route": route or "group.D.D4"}
        status = "ready_with_accepted_risks" if risks else "ready"
        return {"kind": "readiness", "status": status, "evidence": evidence, "upstream_route": route}

    @staticmethod
    def _cyclic(nodes: set[str], edges: list[tuple[str, str]]) -> bool:
        graph = {node: [] for node in nodes}
        for left, right in edges:
            graph[left].append(right)
        active, complete = set(), set()
        def visit(node: str) -> bool:
            if node in active: return True
            if node in complete: return False
            active.add(node)
            found = any(visit(child) for child in graph[node])
            active.remove(node); complete.add(node)
            return found
        return any(visit(node) for node in nodes)

    def _artifact(self, qualified_id: str, values: Mapping[str, Any], authority: Mapping[str, Any], head: Mapping[str, Any], output: Mapping[str, Any]) -> dict[str, Any]:
        refs = values.get("input_refs", [])
        if not _refs(refs):
            raise PlanningSystemError("successful planning candidates require non-empty physical input_refs")
        result = {"schema": "planning-system-artifact/v1", "qualified_id": qualified_id, "authority": authority, "expected_head": head, "input_refs": refs, "output": output}
        validate_document(result, _SCHEMA, _SCHEMA.get("$defs", {}))
        return result

    def _refusal(self, qualified_id: str, reason: str, authority: Mapping[str, Any], head: Mapping[str, Any]) -> dict[str, Any]:
        result = {"schema": "planning-system-refusal/v1", "qualified_id": qualified_id, "reason": reason, "authority": authority, "expected_head": head, "upstream_route": self._route(qualified_id)}
        validate_document(result, _SCHEMA, _SCHEMA.get("$defs", {}))
        return result

    @staticmethod
    def _route(qualified_id: str) -> str:
        number = int(qualified_id.rsplit("D", 1)[-1])
        return "group.D.D%d" % max(1, number - 1)

    @staticmethod
    def _bundle_refusal(epoch: str, code: str) -> dict[str, Any]:
        return {"schema": "planning-epoch-refusal/v1", "epoch_id": epoch, "code": code}

    @staticmethod
    def _validate_bundle(bundle: Any) -> None:
        try:
            validate_document(bundle, _BUNDLE_SCHEMA, _BUNDLE_SCHEMA.get("$defs", {}))
        except SchemaValidationError as error:
            raise PlanningSystemError(str(error)) from error
        epoch = bundle["epoch_id"]
        required = {"D-01": {"D1", "D2", "D3", "D4"}, "D-02": {"D5", "D6", "D7"}, "D-03": {"D8", "D9", "D10", "D11", "D12"}}[epoch]
        if set(bundle["selector_receipts"]) != required:
            raise PlanningSystemError("%s must bind exactly its selector receipts" % epoch)


__all__ = ["PlanningSystemError", "PlanningSystemV1"]
