"""Pure Group G/H decision and outcome lifecycle compiler.

The compiler reads digest-bound evidence and persisted predecessor candidates,
but never writes Run state, promotes a decision, changes an ADR, archives a
Run, deletes an artifact, or grants approval.  A persisted candidate is
accepted only when replaying its original inputs produces the exact bytes-level
semantic document again.
"""

from __future__ import annotations

import copy
import hashlib
import json
import re
from collections.abc import Mapping, Sequence
from pathlib import Path
from typing import Any, Optional

from .schema_validation import SchemaValidationError, validate_document
from .software_profiles import SoftwareProfileV1


class DecisionOutcomeLifecycleError(ValueError):
    """The compiler source or a requested physical binding is malformed."""


class _Refusal(ValueError):
    pass


_DEFAULT_SOURCE_ROOT = Path(__file__).resolve().parents[3]
_SHA256 = re.compile(r"^sha256:[0-9a-f]{64}$")
_CHAINS = {
    "G": tuple("group.G.G%d" % number for number in range(1, 7)),
    "H": tuple("group.H.H%d" % number for number in range(1, 4)),
}
_RESULT_KINDS = {
    "group.G.G1": "decision-source-inventory-candidate",
    "group.G.G2": "decision-classification-candidate",
    "group.G.G3": "decision-graph-candidate",
    "group.G.G4": "durable-decision-record-preview-candidate",
    "group.G.G5": "decision-promotion-approval-validation-candidate",
    "group.G.G6": "decision-promotion-transaction-candidate",
    "group.H.H1": "objective-audit-candidate",
    "group.H.H2": "run-outcome-validation-candidate",
    "group.H.H3": "archive-or-continue-candidate",
}
_ACCEPTED_UPSTREAM = {
    "catalog": ("agent-workflows/catalog.yaml", "sha256:2fd1caaddf4509d2f56bd31a68b062290e70f2f2da33facf1d8187d476bb3f1d"),
    "objective_system": ("agent-workflows/src/ai_agent_workflow/objective_system.py", "sha256:e2aa018cc9e48438ad181b53cc9fa8346809583f43abaae951c6dcb67165ed09"),
    "outcome_system": ("agent-workflows/src/ai_agent_workflow/outcome_system.py", "sha256:e4ab3113da334ad15c842aa656fae52a5c84e3b4ce5f90f1b39aacb83989d646"),
    "closure_protocol": ("agent-workflows/src/ai_agent_workflow/closure_protocol.py", "sha256:5f6a7fd51b6d92733f03a36e5eaade48d16618ee771b05b38f5d97c875521b82"),
    "software_profiles": ("agent-workflows/src/ai_agent_workflow/software_profiles.py", "sha256:1e356025b34ab68dd7cc5730f59532de8620dbe87ddd35266109dee766dde555"),
    "feature_profile": ("agent-workflows/profiles/feature.json", "sha256:8e5cd6c0bb9df1de672acc1a85d40b3bb0ec4f68badfde9bfcd51c98f68578d4"),
    "bug_fix_profile": ("agent-workflows/profiles/bug-fix.json", "sha256:fa5f3c9e2eb20b6dd3fde92559ff04339671f62e4fd8f0b08eb63ec27ef9e46b"),
    "improvement_profile": ("agent-workflows/profiles/improvement.json", "sha256:48bf85cc950241978e341bb3c4238735f70ee0857cb70e61fc7f21a5d53276e9"),
    "objective_manifest": ("agent-workflows/groups/objective.json", "sha256:572091b7a39c4b5bd7b0bbb77827cf9e5e089fdea9799d051826804ba055af36"),
    "outcomes_manifest": ("agent-workflows/groups/outcomes.json", "sha256:fc717686363fa35eac25eb7885e3027f3bb7169faa1ce5e84234faf2a68d346a"),
}


def _digest(value: Any) -> str:
    raw = json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=True).encode()
    return "sha256:" + hashlib.sha256(raw).hexdigest()


def _bytes_digest(value: bytes) -> str:
    return "sha256:" + hashlib.sha256(value).hexdigest()


def _require(condition: bool, code: str) -> None:
    if not condition:
        raise _Refusal(code)


def _nonempty(value: Any) -> bool:
    return isinstance(value, str) and bool(value.strip()) and value.strip().lower() not in {
        "todo", "tbd", "placeholder", "unknown", "unresolved", "<unknown>"
    }


def _exact(value: Any, keys: set[str], code: str) -> Mapping[str, Any]:
    _require(isinstance(value, Mapping) and set(value) == keys, code)
    return value


def _head(value: Any) -> dict[str, Any]:
    head = _exact(value, {"revision", "transaction_digest"}, "blocked_malformed_head")
    _require(isinstance(head["revision"], int) and not isinstance(head["revision"], bool) and head["revision"] >= 0, "blocked_malformed_head")
    _require(isinstance(head["transaction_digest"], str) and _SHA256.fullmatch(head["transaction_digest"]) is not None, "blocked_malformed_head")
    return copy.deepcopy(dict(head))


def _ref_key(value: Mapping[str, Any]) -> tuple[str, str, str]:
    return value["path"], value["version"], value["digest"]


class DecisionOutcomeLifecycleV1:
    """Compile one G/H candidate or compose complete, replayed G/H chains."""

    def __init__(
        self,
        *,
        source_root: Optional[Path] = None,
        reference_root: Optional[Path] = None,
        schema_path: Optional[Path] = None,
    ) -> None:
        self.source_root = Path(source_root or _DEFAULT_SOURCE_ROOT).resolve()
        self.reference_root = Path(reference_root or self.source_root).resolve()
        path = Path(schema_path or self.source_root / "agent-workflows/schemas/decision-outcome-lifecycle-v1.schema.json")
        try:
            self.schema = json.loads(path.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError) as error:
            raise DecisionOutcomeLifecycleError("cannot read decision/outcome schema: %s" % error) from error

    def _upstream_refs(self) -> dict[str, dict[str, str]]:
        refs: dict[str, dict[str, str]] = {}
        for name, (relative, accepted) in _ACCEPTED_UPSTREAM.items():
            try:
                actual = _bytes_digest((self.source_root / relative).read_bytes())
            except OSError as error:
                raise _Refusal("blocked_upstream_unavailable:%s" % name) from error
            _require(actual == accepted, "blocked_upstream_digest_drift:%s" % name)
            refs[name] = {"path": relative, "digest": actual}
        return refs

    def _load_ref(self, value: Any, *, version: Optional[str] = None) -> tuple[dict[str, Any], dict[str, str]]:
        ref = _exact(value, {"path", "version", "digest"}, "blocked_malformed_physical_ref")
        _require(_nonempty(ref["path"]) and _nonempty(ref["version"]), "blocked_malformed_physical_ref")
        _require(isinstance(ref["digest"], str) and _SHA256.fullmatch(ref["digest"]) is not None, "blocked_malformed_physical_ref")
        relative = Path(ref["path"])
        _require(not relative.is_absolute() and ".." not in relative.parts, "blocked_unsafe_physical_ref")
        resolved: Optional[Path] = None
        for root in dict.fromkeys((self.reference_root, self.source_root)):
            candidate = (root / relative).resolve()
            try:
                candidate.relative_to(root)
            except ValueError:
                continue
            if candidate.is_file():
                resolved = candidate
                break
        _require(resolved is not None, "blocked_missing_physical_ref:%s" % ref["path"])
        try:
            raw = resolved.read_bytes()
            document = json.loads(raw.decode("utf-8"))
        except (OSError, UnicodeDecodeError, json.JSONDecodeError) as error:
            raise _Refusal("blocked_unreadable_physical_ref:%s" % ref["path"]) from error
        _require(_bytes_digest(raw) == ref["digest"], "blocked_digest_drift:%s" % ref["path"])
        _require(isinstance(document, Mapping) and document.get("schema") == ref["version"], "blocked_ref_version_mismatch:%s" % ref["path"])
        if version is not None:
            _require(ref["version"] == version, "blocked_ref_kind_substitution:%s" % ref["path"])
        return copy.deepcopy(dict(document)), copy.deepcopy(dict(ref))

    def _authority(self, value: Any) -> tuple[dict[str, Any], str]:
        authority = _exact(value, {"authority_ref", "namespace", "scope", "owner_ref"}, "blocked_malformed_authority")
        _require(_nonempty(authority["namespace"]), "blocked_malformed_authority")
        _require(authority["scope"] in {"decision-outcome-candidate", "candidate-generic", "fixture-only"}, "blocked_authority_scope")
        self._load_ref(authority["authority_ref"])
        self._load_ref(authority["owner_ref"])
        copied = copy.deepcopy(dict(authority))
        return copied, _digest(copied)

    def _refs(self, values: Any, *, allow_empty: bool = False) -> tuple[list[dict[str, str]], dict[tuple[str, str, str], dict[str, Any]]]:
        _require(isinstance(values, list) and (allow_empty or bool(values)), "blocked_missing_evidence_refs")
        refs: list[dict[str, str]] = []
        documents: dict[tuple[str, str, str], dict[str, Any]] = {}
        for value in values:
            document, ref = self._load_ref(value)
            key = _ref_key(ref)
            _require(key not in documents, "blocked_duplicate_physical_ref")
            refs.append(ref)
            documents[key] = document
        return refs, documents

    @staticmethod
    def _doc(ref: Any, documents: Mapping[tuple[str, str, str], dict[str, Any]], code: str) -> dict[str, Any]:
        _require(isinstance(ref, Mapping), code)
        key = (ref.get("path"), ref.get("version"), ref.get("digest"))
        _require(key in documents, code)
        return documents[key]

    def _predecessors(
        self,
        selector: str,
        values: Any,
        authority_digest: str,
        expected_head: Mapping[str, Any],
    ) -> tuple[list[dict[str, str]], list[dict[str, Any]]]:
        group = selector.split(".")[1]
        expected = _CHAINS[group][:_CHAINS[group].index(selector)]
        _require(isinstance(values, list) and len(values) == len(expected), "blocked_missing_or_extra_predecessor")
        refs: list[dict[str, str]] = []
        documents: list[dict[str, Any]] = []
        for value, expected_selector in zip(values, expected):
            document, ref = self._load_ref(value, version="decision-outcome-candidate/v1")
            _require(document.get("group") == group, "blocked_cross_group_predecessor")
            _require(document.get("qualified_id") == expected_selector, "blocked_reordered_predecessor")
            _require(document.get("expected_head") == expected_head, "blocked_stale_predecessor_head")
            _require(document.get("authority_digest") == authority_digest, "blocked_stale_predecessor_authority")
            unsigned = {key: copy.deepcopy(item) for key, item in document.items() if key != "candidate_digest"}
            _require(document.get("candidate_digest") == _digest(unsigned), "blocked_predecessor_candidate_digest")
            _require(document.get("predecessor_artifact_refs") == refs, "blocked_predecessor_chain")
            self._recompile_candidate(document)
            document["_physical_ref"] = copy.deepcopy(ref)
            refs.append(ref)
            documents.append(document)
        return refs, documents

    def _recompile_candidate(self, document: Mapping[str, Any]) -> None:
        inputs = {
            "predecessor_artifact_refs": document.get("predecessor_artifact_refs"),
            "evidence_refs": document.get("evidence_refs"),
            "payload": document.get("payload"),
            "human_event_ref": document.get("human_event_ref"),
        }
        recompiled = self.compile(document.get("qualified_id"), inputs, document.get("authority"), document.get("expected_head"))
        _require(recompiled.get("schema") == "decision-outcome-candidate/v1", "blocked_persisted_candidate_semantics:%s" % recompiled.get("reason", "invalid"))
        _require(recompiled == document, "blocked_persisted_candidate_replay_mismatch")

    def compile(self, qualified_id: str, inputs: Mapping[str, Any], authority: Mapping[str, Any], expected_head: Mapping[str, Any]) -> dict[str, Any]:
        """Return a deterministic candidate or typed, non-authorizing refusal."""

        group = qualified_id.split(".")[1] if isinstance(qualified_id, str) and qualified_id.count(".") == 2 else None
        try:
            head = _head(expected_head)
            upstream = self._upstream_refs()
            _require(qualified_id in _RESULT_KINDS, "blocked_unknown_selector")
            group = qualified_id.split(".")[1]
            values = _exact(inputs, {"predecessor_artifact_refs", "evidence_refs", "payload", "human_event_ref"}, "blocked_malformed_inputs")
            bound_authority, authority_digest = self._authority(authority)
            predecessors, predecessor_documents = self._predecessors(qualified_id, values["predecessor_artifact_refs"], authority_digest, head)
            evidence_refs, documents = self._refs(values["evidence_refs"])
            payload = copy.deepcopy(dict(values["payload"])) if isinstance(values["payload"], Mapping) else {}
            normalized, human_event_ref = self._validate_payload(
                qualified_id, payload, documents, predecessor_documents, values["human_event_ref"], authority_digest, head
            )
            result = {
                "schema": "decision-outcome-candidate/v1",
                "qualified_id": qualified_id,
                "group": group,
                "step_id": qualified_id.rsplit(".", 1)[-1],
                "result_kind": _RESULT_KINDS[qualified_id],
                "status": "candidate",
                "expected_head": head,
                "authority": bound_authority,
                "authority_digest": authority_digest,
                "predecessor_artifact_refs": predecessors,
                "evidence_refs": evidence_refs,
                "payload": normalized,
                "human_event_ref": human_event_ref,
                "source_interfaces": upstream,
                "non_mutating": True,
                "grants_approval": False,
                "performs_promotion": False,
                "performs_archive": False,
                "performs_cleanup": False,
                "objective_outcome_claimed": False,
            }
            result["candidate_digest"] = _digest(result)
            return self._validated(result)
        except (_Refusal, SchemaValidationError, DecisionOutcomeLifecycleError) as error:
            return self._refusal(qualified_id, group, str(error), expected_head)

    def compose_lifecycle(
        self,
        g_artifact_refs: Sequence[Mapping[str, Any]],
        h_artifact_refs: Sequence[Mapping[str, Any]],
        authority: Mapping[str, Any],
        expected_head: Mapping[str, Any],
    ) -> dict[str, Any]:
        """Compose only complete ordered G/H chains after exact semantic replay."""

        try:
            head = _head(expected_head)
            upstream = self._upstream_refs()
            bound_authority, authority_digest = self._authority(authority)
            refs_by_group: dict[str, list[dict[str, str]]] = {}
            documents_by_group: dict[str, list[dict[str, Any]]] = {}
            for group, values in (("G", g_artifact_refs), ("H", h_artifact_refs)):
                _require(isinstance(values, Sequence) and not isinstance(values, (str, bytes)) and len(values) == len(_CHAINS[group]), "blocked_incomplete_%s_chain" % group.lower())
                refs: list[dict[str, str]] = []
                documents: list[dict[str, Any]] = []
                for value, selector in zip(values, _CHAINS[group]):
                    document, ref = self._load_ref(value, version="decision-outcome-candidate/v1")
                    _require(document.get("qualified_id") == selector and document.get("group") == group, "blocked_reordered_%s_artifact" % group.lower())
                    _require(document.get("expected_head") == head, "blocked_stale_artifact_head")
                    _require(document.get("authority_digest") == authority_digest and document.get("authority") == bound_authority, "blocked_stale_artifact_authority")
                    _require(document.get("predecessor_artifact_refs") == refs, "blocked_%s_predecessor_chain" % group.lower())
                    unsigned = {key: copy.deepcopy(item) for key, item in document.items() if key != "candidate_digest"}
                    _require(document.get("candidate_digest") == _digest(unsigned), "blocked_artifact_candidate_digest")
                    self._recompile_candidate(document)
                    refs.append(ref)
                    documents.append(document)
                refs_by_group[group] = refs
                documents_by_group[group] = documents
            decision_ref = documents_by_group["H"][0]["payload"]["decision_promotion_ref"]
            _require(decision_ref is None or decision_ref == refs_by_group["G"][-1], "blocked_cross_group_decision_binding")
            result = {
                "schema": "decision-outcome-lifecycle-composition/v1",
                "status": "candidate",
                "g_artifact_refs": refs_by_group["G"],
                "h_artifact_refs": refs_by_group["H"],
                "selectors": [*_CHAINS["G"], *_CHAINS["H"]],
                "expected_head": head,
                "authority": bound_authority,
                "authority_digest": authority_digest,
                "source_interfaces": upstream,
                "non_mutating": True,
                "grants_approval": False,
                "performs_promotion": False,
                "performs_archive": False,
                "performs_cleanup": False,
                "objective_outcome_claimed": False,
            }
            result["composition_digest"] = _digest(result)
            return self._validated(result)
        except (_Refusal, SchemaValidationError, DecisionOutcomeLifecycleError) as error:
            return self._refusal("compose_lifecycle", None, str(error), expected_head)

    def _validate_payload(
        self,
        selector: str,
        payload: dict[str, Any],
        documents: Mapping[tuple[str, str, str], dict[str, Any]],
        predecessors: list[dict[str, Any]],
        human_event_value: Any,
        authority_digest: str,
        head: Mapping[str, Any],
    ) -> tuple[dict[str, Any], Optional[dict[str, str]]]:
        _require((selector in {"group.G.G5", "group.H.H2"}) == (human_event_value is not None), "blocked_unexpected_or_missing_human_event")
        handlers = {
            "group.G.G1": self._g1, "group.G.G2": self._g2, "group.G.G3": self._g3,
            "group.G.G4": self._g4, "group.G.G5": self._g5, "group.G.G6": self._g6,
            "group.H.H1": self._h1, "group.H.H2": self._h2, "group.H.H3": self._h3,
        }
        if human_event_value is None:
            if selector == "group.G.G2":
                return self._g2(payload, documents, predecessors, authority_digest, head), None
            if selector == "group.H.H1":
                return self._h1(payload, documents, predecessors, authority_digest, head), None
            return handlers[selector](payload, documents, predecessors), None
        event, event_ref = self._load_ref(human_event_value)
        normalized = handlers[selector](payload, documents, predecessors, event, event_ref, authority_digest, head)
        return normalized, event_ref

    def _g1(self, value: dict[str, Any], docs: Mapping[Any, Any], _pred: list[dict[str, Any]]) -> dict[str, Any]:
        v = _exact(value, {"run_id", "period", "groups", "source_refs", "searched_scopes", "unreadable_scopes", "excluded_scopes", "coverage_status"}, "blocked_incomplete_evidence:group.G.G1")
        _require(all(_nonempty(v[key]) for key in ("run_id", "period")), "blocked_incomplete_inventory_scope")
        _require(isinstance(v["groups"], list) and v["groups"] and all(_nonempty(item) for item in v["groups"]), "blocked_incomplete_inventory_scope")
        _require(isinstance(v["searched_scopes"], list) and v["searched_scopes"] and all(_nonempty(item) for item in v["searched_scopes"]), "blocked_incomplete_inventory_coverage")
        _require(isinstance(v["unreadable_scopes"], list) and isinstance(v["excluded_scopes"], list), "blocked_incomplete_inventory_coverage")
        _require(v["coverage_status"] in {"complete", "partial"}, "blocked_incomplete_inventory_coverage")
        _require((v["coverage_status"] == "complete") == (not v["unreadable_scopes"]), "blocked_inventory_coverage_contradiction")
        _require(isinstance(v["source_refs"], list) and v["source_refs"], "blocked_missing_decision_sources")
        for ref in v["source_refs"]:
            self._doc(ref, docs, "blocked_unbound_decision_source")
        return copy.deepcopy(dict(v))

    def _g2(self, value: dict[str, Any], docs: Mapping[Any, Any], pred: list[dict[str, Any]], authority_digest: str, head: Mapping[str, Any]) -> dict[str, Any]:
        v = _exact(value, {"classifications"}, "blocked_incomplete_evidence:group.G.G2")
        _require(pred[0]["payload"]["coverage_status"] == "complete", "blocked_partial_inventory_predecessor")
        items = v["classifications"]
        _require(isinstance(items, list) and items, "blocked_missing_classification")
        expected = {_ref_key(item) for item in pred[0]["payload"]["source_refs"]}
        seen: set[tuple[str, str, str]] = set()
        for item in items:
            row = _exact(item, {"candidate_ref", "status", "evidence_refs", "authority_ref", "approval_receipt_ref", "confidence"}, "blocked_malformed_classification")
            candidate_key = _ref_key(self._doc(row["candidate_ref"], docs, "blocked_unbound_classification_candidate") and row["candidate_ref"])
            seen.add(candidate_key)
            _require(row["status"] in {"observation", "assumption", "proposal", "temporary-ruling", "approved-decision", "superseding-candidate"}, "blocked_unknown_decision_status")
            _require(isinstance(row["confidence"], (int, float)) and not isinstance(row["confidence"], bool) and 0 <= row["confidence"] <= 1, "blocked_invalid_confidence")
            _require(isinstance(row["evidence_refs"], list) and row["evidence_refs"], "blocked_missing_classification_evidence")
            for ref in row["evidence_refs"]:
                self._doc(ref, docs, "blocked_unbound_classification_evidence")
            if row["status"] == "approved-decision":
                _require(row["authority_ref"] is not None and row["approval_receipt_ref"] is not None, "blocked_unknown_or_agent_only_approval")
                approval = self._doc(row["approval_receipt_ref"], docs, "blocked_unknown_or_agent_only_approval")
                actor = approval.get("actor") if isinstance(approval, Mapping) else None
                _exact(approval, {"schema", "actor", "approved", "candidate_ref", "authority_ref", "authority_digest", "expected_head"}, "blocked_unbound_approval_receipt")
                _require(approval.get("schema") == "decision-approval-receipt/v1" and approval.get("approved") is True, "blocked_unknown_or_agent_only_approval")
                _require(isinstance(actor, Mapping) and set(actor) == {"type", "id"} and actor.get("type") in {"human", "delegated-authority"} and _nonempty(actor.get("id")), "blocked_unknown_or_agent_only_approval")
                self._doc(row["authority_ref"], docs, "blocked_unknown_or_agent_only_approval")
                _require(approval["candidate_ref"] == row["candidate_ref"] and approval["authority_ref"] == row["authority_ref"], "blocked_unbound_approval_receipt")
                _require(approval["authority_digest"] == authority_digest and approval["expected_head"] == head, "blocked_stale_approval_receipt")
            else:
                _require(row["approval_receipt_ref"] is None, "blocked_unvalidated_approval_receipt")
        _require(seen == expected and len(seen) == len(items), "blocked_unclassified_or_duplicate_candidate")
        return copy.deepcopy(dict(v))

    def _g3(self, value: dict[str, Any], _docs: Mapping[Any, Any], pred: list[dict[str, Any]]) -> dict[str, Any]:
        v = _exact(value, {"nodes", "relationships"}, "blocked_incomplete_evidence:group.G.G3")
        nodes, relationships = v["nodes"], v["relationships"]
        _require(isinstance(nodes, list) and nodes and isinstance(relationships, list), "blocked_malformed_decision_graph")
        classified = {_ref_key(item["candidate_ref"]) for item in pred[1]["payload"]["classifications"]}
        chronology: dict[tuple[str, str, str], int] = {}
        for node in nodes:
            row = _exact(node, {"candidate_ref", "disposition", "chronology"}, "blocked_malformed_decision_graph")
            key = _ref_key(row["candidate_ref"])
            _require(key not in chronology and row["disposition"] in {"new", "duplicate", "conflict", "superseding"}, "blocked_malformed_decision_graph")
            _require(isinstance(row["chronology"], int) and not isinstance(row["chronology"], bool) and row["chronology"] >= 0, "blocked_malformed_chronology")
            chronology[key] = row["chronology"]
        _require(set(chronology) == classified, "blocked_graph_coverage")
        edges: list[tuple[tuple[str, str, str], tuple[str, str, str]]] = []
        normalized_edges: list[dict[str, Any]] = []
        for edge in relationships:
            row = _exact(edge, {"source_ref", "target_ref", "kind"}, "blocked_malformed_decision_relationship")
            source, target = _ref_key(row["source_ref"]), _ref_key(row["target_ref"])
            _require(source in chronology and target in chronology and source != target, "blocked_unknown_graph_endpoint")
            _require(row["kind"] in {"duplicate-of", "conflicts-with", "supersedes"}, "blocked_unknown_relationship")
            if row["kind"] == "supersedes":
                _require(chronology[source] > chronology[target], "blocked_chronology_rewrite")
            edges.append((source, target))
            normalized_edges.append(copy.deepcopy(dict(row)))
        outgoing: dict[tuple[str, str, str], list[str]] = {key: [] for key in chronology}
        for edge in normalized_edges:
            outgoing[_ref_key(edge["source_ref"])].append(edge["kind"])
        required_relation = {"duplicate": "duplicate-of", "conflict": "conflicts-with", "superseding": "supersedes"}
        for node in nodes:
            kinds = outgoing[_ref_key(node["candidate_ref"])]
            if node["disposition"] == "new":
                _require(not kinds, "blocked_new_node_relation_mismatch")
            else:
                expected_kind = required_relation[node["disposition"]]
                _require(kinds and set(kinds) == {expected_kind}, "blocked_disposition_relation_mismatch")
        _require(not self._cyclic(set(chronology), edges), "blocked_decision_cycle")
        normalized_nodes = sorted((copy.deepcopy(dict(item)) for item in nodes), key=lambda item: (item["chronology"], _ref_key(item["candidate_ref"])))
        normalized_edges.sort(key=lambda item: (item["kind"], _ref_key(item["source_ref"]), _ref_key(item["target_ref"])))
        return {"nodes": normalized_nodes, "relationships": normalized_edges}

    def _g4(self, value: dict[str, Any], docs: Mapping[Any, Any], pred: list[dict[str, Any]]) -> dict[str, Any]:
        v = _exact(value, {"records"}, "blocked_incomplete_evidence:group.G.G4")
        records = v["records"]
        _require(isinstance(records, list) and records, "blocked_missing_durable_record_preview")
        graph_refs = {_ref_key(item["candidate_ref"]) for item in pred[2]["payload"]["nodes"] if item["disposition"] in {"new", "conflict", "superseding"}}
        covered: set[tuple[str, str, str]] = set()
        record_ids: set[str] = set()
        required = {"record_id", "graph_candidate_ref", "record_kind", "context", "decision", "drivers", "options", "rationale_evidence_refs", "rejected_alternatives", "consequences", "owner", "revisit_trigger", "source_refs", "historical_rule_refs"}
        for record in records:
            row = _exact(record, required, "blocked_malformed_durable_record_preview")
            _require(_nonempty(row["record_id"]) and row["record_id"] not in record_ids, "blocked_duplicate_record_id")
            record_ids.add(row["record_id"])
            _require(row["record_kind"] in {"current-candidate", "superseding-candidate"}, "blocked_historical_rule_as_current")
            _require(all(_nonempty(row[key]) for key in ("context", "decision", "owner", "revisit_trigger")), "blocked_incomplete_durable_record")
            for key in ("drivers", "options", "rationale_evidence_refs", "rejected_alternatives", "consequences", "source_refs", "historical_rule_refs"):
                _require(isinstance(row[key], list), "blocked_incomplete_durable_record")
            _require(row["drivers"] and row["options"] and row["rationale_evidence_refs"] and row["consequences"] and row["source_refs"], "blocked_incomplete_durable_record")
            _require(all(_nonempty(item) for key in ("drivers", "options", "rejected_alternatives", "consequences") for item in row[key]), "blocked_incomplete_durable_record")
            key = _ref_key(row["graph_candidate_ref"])
            _require(key in graph_refs and key not in covered, "blocked_combined_or_unbound_decision")
            covered.add(key)
            if row["record_kind"] == "superseding-candidate":
                _require(row["historical_rule_refs"], "blocked_missing_superseded_history")
            for ref in [*row["rationale_evidence_refs"], *row["source_refs"], *row["historical_rule_refs"]]:
                self._doc(ref, docs, "blocked_unbound_durable_record_evidence")
        _require(covered == graph_refs, "blocked_missing_durable_record_preview")
        return copy.deepcopy(dict(v))

    def _g5(self, value: dict[str, Any], _docs: Mapping[Any, Any], pred: list[dict[str, Any]], event: dict[str, Any], event_ref: dict[str, str], authority_digest: str, head: Mapping[str, Any]) -> dict[str, Any]:
        v = _exact(value, {"candidate_ref", "actions"}, "blocked_incomplete_evidence:group.G.G5")
        _require(v["candidate_ref"] == pred[3].get("_physical_ref", v["candidate_ref"]), "blocked_approval_candidate_binding")
        _require(isinstance(v["actions"], list) and v["actions"], "blocked_missing_promotion_decision")
        expected_records = {item["record_id"] for item in pred[3]["payload"]["records"]}
        seen: set[str] = set()
        for action in v["actions"]:
            row = _exact(action, {"record_id", "decision", "target", "diff_digest"}, "blocked_malformed_promotion_decision")
            _require(row["record_id"] in expected_records and row["record_id"] not in seen, "blocked_promotion_record_coverage")
            seen.add(row["record_id"])
            _require(row["decision"] in {"adopt", "modify", "hold", "reject"}, "blocked_unknown_promotion_decision")
            _exact(row["target"], {"path", "version"}, "blocked_missing_promotion_target")
            _require(_nonempty(row["target"]["path"]) and _nonempty(row["target"]["version"]), "blocked_missing_promotion_target")
            _require(isinstance(row["diff_digest"], str) and _SHA256.fullmatch(row["diff_digest"]) is not None, "blocked_missing_promotion_diff")
        _require(seen == expected_records, "blocked_promotion_record_coverage")
        required = {"schema", "actor", "candidate_ref", "actions", "authority_digest", "expected_head"}
        _exact(event, required, "blocked_malformed_promotion_event")
        actor = event.get("actor")
        _require(event["schema"] == "decision-promotion-approval/v1", "blocked_malformed_promotion_event")
        _require(isinstance(actor, Mapping) and set(actor) == {"type", "id"} and actor.get("type") in {"human", "delegated-authority"} and _nonempty(actor.get("id")), "blocked_agent_self_approval")
        _require(event["candidate_ref"] == v["candidate_ref"] and event["actions"] == v["actions"], "blocked_approval_event_mismatch")
        _require(event["authority_digest"] == authority_digest and event["expected_head"] == head, "blocked_stale_approval_event")
        return copy.deepcopy(dict(v))

    def _g6(self, value: dict[str, Any], docs: Mapping[Any, Any], pred: list[dict[str, Any]]) -> dict[str, Any]:
        v = _exact(value, {"transactions", "write_requested", "source_events_immutable", "overwrite_existing"}, "blocked_incomplete_evidence:group.G.G6")
        _require(v["write_requested"] is False and v["source_events_immutable"] is True and v["overwrite_existing"] is False, "blocked_mutating_promotion_request")
        approved = {item["record_id"]: item for item in pred[4]["payload"]["actions"] if item["decision"] in {"adopt", "modify"}}
        disallowed = {item["record_id"] for item in pred[4]["payload"]["actions"] if item["decision"] in {"hold", "reject"}}
        records = {item["record_id"]: item for item in pred[3]["payload"]["records"]}
        _require(isinstance(v["transactions"], list), "blocked_malformed_promotion_transactions")
        seen: set[str] = set()
        required = {"record_id", "decision", "source_event_refs", "candidate_ref", "approval_event_ref", "durable_target", "validation_refs", "backlinks", "supersedes_ref"}
        for transaction in v["transactions"]:
            row = _exact(transaction, required, "blocked_malformed_promotion_transaction")
            _require(row["record_id"] in approved and row["record_id"] not in disallowed and row["record_id"] not in seen, "blocked_unapproved_promotion")
            seen.add(row["record_id"])
            _require(row["decision"] == approved[row["record_id"]]["decision"], "blocked_promotion_action_mismatch")
            _require(row["candidate_ref"] == pred[3]["_physical_ref"] and row["approval_event_ref"] == pred[4]["human_event_ref"], "blocked_promotion_binding")
            _require(row["source_event_refs"] == records[row["record_id"]]["source_refs"], "blocked_promotion_source_binding")
            _require(isinstance(row["validation_refs"], list) and row["validation_refs"], "blocked_missing_promotion_evidence")
            target = _exact(row["durable_target"], {"path", "version"}, "blocked_missing_promotion_target")
            _require(target == approved[row["record_id"]]["target"], "blocked_promotion_target_mismatch")
            expected_backlinks = [{"source_ref": ref, "durable_target": target} for ref in row["source_event_refs"]]
            _require(row["backlinks"] == expected_backlinks, "blocked_backlink_source_binding")
            if row["supersedes_ref"] is not None:
                _require(row["supersedes_ref"].get("path") != target["path"], "blocked_old_record_overwrite")
            for ref in [*row["source_event_refs"], *row["validation_refs"]]:
                self._doc(ref, docs, "blocked_unbound_promotion_evidence")
        _require(seen == set(approved), "blocked_missing_approved_promotion")
        return copy.deepcopy(dict(v))

    def _h1(self, value: dict[str, Any], docs: Mapping[Any, Any], _pred: list[dict[str, Any]], authority_digest: str, head: Mapping[str, Any]) -> dict[str, Any]:
        v = _exact(value, {"objective_ref", "outcome_system_ref", "closure_ref", "profile_ref", "decision_promotion_ref", "delivery", "trajectory", "objective_state", "unverified_items"}, "blocked_incomplete_evidence:group.H.H1")
        bindings = {
            "objective_ref": "blocked_missing_accepted_objective",
            "outcome_system_ref": "blocked_missing_outcome_system",
            "closure_ref": "blocked_missing_shared_closure",
            "profile_ref": "blocked_missing_profile_evidence",
        }
        loaded = {key: self._doc(v[key], docs, code) for key, code in bindings.items()}
        _require(loaded["objective_ref"].get("schema") == "accepted-objective/v1" and loaded["objective_ref"].get("status") == "accepted", "blocked_missing_accepted_objective")
        _require(loaded["outcome_system_ref"].get("schema") == "outcome-validation/v1" and loaded["outcome_system_ref"].get("status") == "valid", "blocked_missing_outcome_system")
        _require(loaded["closure_ref"].get("schema") == "closure-operation-result/v1" and loaded["closure_ref"].get("status") == "compiled", "blocked_missing_shared_closure")
        profile_document = loaded["profile_ref"]
        _require(profile_document.get("schema") == "software-profile-composition/v1" and profile_document.get("status") == "candidate", "blocked_missing_profile_evidence")
        _require(profile_document.get("expected_head") == head and profile_document.get("authority_digest") == authority_digest, "blocked_stale_profile_evidence")
        profile_replay = SoftwareProfileV1(source_root=self.source_root, reference_root=self.reference_root).compose_profile(
            profile_document.get("profile"),
            profile_document.get("workflow_manifest_ref"),
            profile_document.get("artifact_refs"),
            profile_document.get("authority"),
            profile_document.get("expected_head"),
        )
        _require(profile_replay.get("schema") == "software-profile-composition/v1", "blocked_persisted_profile_semantics:%s" % profile_replay.get("reason", "invalid"))
        _require(profile_replay == profile_document, "blocked_persisted_profile_replay_mismatch")
        if v["decision_promotion_ref"] is not None:
            decision = self._doc(v["decision_promotion_ref"], docs, "blocked_missing_decision_promotion")
            _require(decision.get("schema") == "decision-outcome-candidate/v1" and decision.get("qualified_id") == "group.G.G6", "blocked_missing_decision_promotion")
            _require(decision.get("expected_head") == head and decision.get("authority_digest") == authority_digest, "blocked_stale_decision_promotion")
            self._recompile_candidate(decision)
        status_sets = {
            "delivery": {"completed", "incomplete", "unknown"},
            "trajectory": {"improved", "not-improved", "unknown"},
            "objective_state": {"achieved", "not-achieved", "unknown"},
        }
        claim_ref_sets: dict[str, set[tuple[str, str, str]]] = {}
        for claim, statuses in status_sets.items():
            item = _exact(v[claim], {"status", "evidence_refs"}, "blocked_malformed_%s_claim" % claim)
            _require(item["status"] in statuses and isinstance(item["evidence_refs"], list) and item["evidence_refs"], "blocked_malformed_%s_claim" % claim)
            claim_ref_sets[claim] = {_ref_key(ref) for ref in item["evidence_refs"]}
            _require(len(claim_ref_sets[claim]) == len(item["evidence_refs"]), "blocked_duplicate_%s_evidence" % claim)
            for ref in item["evidence_refs"]:
                receipt = self._doc(ref, docs, "blocked_unbound_%s_evidence" % claim)
                _exact(receipt, {"schema", "dimension", "status", "objective_ref"}, "blocked_malformed_%s_evidence" % claim)
                _require(receipt["schema"] == "objective-audit-evidence/v1" and receipt["dimension"] == claim, "blocked_cross_dimension_evidence")
                _require(receipt["status"] == item["status"] and receipt["objective_ref"] == v["objective_ref"], "blocked_unbound_%s_evidence" % claim)
        _require(
            claim_ref_sets["delivery"].isdisjoint(claim_ref_sets["trajectory"])
            and claim_ref_sets["delivery"].isdisjoint(claim_ref_sets["objective_state"])
            and claim_ref_sets["trajectory"].isdisjoint(claim_ref_sets["objective_state"]),
            "blocked_shared_cross_dimension_evidence",
        )
        _require(isinstance(v["unverified_items"], list), "blocked_malformed_unverified_items")
        for item in v["unverified_items"]:
            row = _exact(item, {"item_id", "material", "reason"}, "blocked_malformed_unverified_item")
            _require(_nonempty(row["item_id"]) and isinstance(row["material"], bool) and _nonempty(row["reason"]), "blocked_malformed_unverified_item")
        return copy.deepcopy(dict(v))

    def _h2(self, value: dict[str, Any], _docs: Mapping[Any, Any], pred: list[dict[str, Any]], event: dict[str, Any], event_ref: dict[str, str], authority_digest: str, head: Mapping[str, Any]) -> dict[str, Any]:
        v = _exact(value, {"audit_ref", "outcome", "reason", "remaining_tasks"}, "blocked_incomplete_evidence:group.H.H2")
        _require(v["outcome"] in {"achieved", "partially-achieved", "not-achieved", "superseded", "abandoned"}, "blocked_unknown_run_outcome")
        _require(_nonempty(v["reason"]) and isinstance(v["remaining_tasks"], list) and all(_nonempty(item) for item in v["remaining_tasks"]), "blocked_incomplete_run_outcome")
        audit = pred[0]["payload"]
        _require(v["audit_ref"] == pred[0]["_physical_ref"], "blocked_run_outcome_audit_binding")
        if v["outcome"] == "achieved":
            _require(audit["objective_state"]["status"] == "achieved", "blocked_achieved_without_objective_state")
            _require(not any(item["material"] for item in audit["unverified_items"]), "blocked_achieved_with_material_unverified")
        required = {"schema", "actor", "audit_ref", "outcome", "reason", "remaining_tasks", "authority_digest", "expected_head"}
        _exact(event, required, "blocked_malformed_run_outcome_event")
        actor = event.get("actor")
        _require(event["schema"] == "human-run-outcome/v1" and isinstance(actor, Mapping) and set(actor) == {"type", "id"} and actor.get("type") == "human" and _nonempty(actor.get("id")), "blocked_nonhuman_run_outcome")
        for key in ("audit_ref", "outcome", "reason", "remaining_tasks"):
            _require(event[key] == v[key], "blocked_run_outcome_event_mismatch")
        _require(event["authority_digest"] == authority_digest and event["expected_head"] == head, "blocked_stale_run_outcome_event")
        return copy.deepcopy(dict(v))

    def _h3(self, value: dict[str, Any], docs: Mapping[Any, Any], pred: list[dict[str, Any]]) -> dict[str, Any]:
        v = _exact(value, {"outcome_ref", "mode", "retention", "cleanup_candidates", "decision_candidates", "important_decisions", "rollback_artifact_refs", "next_run_handoff_ref"}, "blocked_incomplete_evidence:group.H.H3")
        _require(v["outcome_ref"] == pred[1]["_physical_ref"], "blocked_retention_outcome_binding")
        _require(v["mode"] in {"archive", "continue"}, "blocked_unknown_retention_mode")
        _require(isinstance(v["retention"], list) and v["retention"], "blocked_missing_retention_evidence")
        for item in v["retention"]:
            row = _exact(item, {"artifact_ref", "retain", "rationale"}, "blocked_malformed_retention")
            _require(isinstance(row["retain"], bool) and _nonempty(row["rationale"]), "blocked_malformed_retention")
            self._doc(row["artifact_ref"], docs, "blocked_missing_retention_evidence")
        _require(isinstance(v["rollback_artifact_refs"], list) and v["rollback_artifact_refs"], "blocked_missing_rollback_evidence")
        for ref in v["rollback_artifact_refs"]:
            self._doc(ref, docs, "blocked_missing_rollback_evidence")
        retained = {_ref_key(item["artifact_ref"]) for item in v["retention"] if item["retain"] is True}
        _require(all(_ref_key(ref) in retained for ref in v["rollback_artifact_refs"]), "blocked_unretained_rollback_artifact")
        _require(isinstance(v["important_decisions"], list), "blocked_malformed_important_decisions")
        for item in v["important_decisions"]:
            row = _exact(item, {"candidate_ref", "promoted", "promotion_ref"}, "blocked_malformed_important_decision")
            _require(row["promoted"] is True and row["promotion_ref"] is not None, "blocked_unpromoted_important_decision")
            self._doc(row["candidate_ref"], docs, "blocked_unbound_important_decision")
            self._doc(row["promotion_ref"], docs, "blocked_unbound_important_decision")
        _require(isinstance(v["cleanup_candidates"], list), "blocked_malformed_cleanup_candidates")
        for item in v["cleanup_candidates"]:
            row = _exact(item, {"path", "reason", "requires_human_approval", "executed"}, "blocked_malformed_cleanup_candidate")
            _require(_nonempty(row["path"]) and _nonempty(row["reason"]) and row["requires_human_approval"] is True and row["executed"] is False, "blocked_executed_cleanup")
        _require(isinstance(v["decision_candidates"], list), "blocked_malformed_decision_candidates")
        for ref in v["decision_candidates"]:
            self._doc(ref, docs, "blocked_unbound_decision_candidate")
        if v["mode"] == "continue":
            _require(v["next_run_handoff_ref"] is not None, "blocked_missing_next_run_handoff")
            self._doc(v["next_run_handoff_ref"], docs, "blocked_missing_next_run_handoff")
        return copy.deepcopy(dict(v))

    @staticmethod
    def _cyclic(nodes: set[tuple[str, str, str]], edges: list[tuple[tuple[str, str, str], tuple[str, str, str]]]) -> bool:
        graph = {node: [] for node in nodes}
        for left, right in edges:
            graph[left].append(right)
        active: set[tuple[str, str, str]] = set()
        complete: set[tuple[str, str, str]] = set()
        def visit(node: tuple[str, str, str]) -> bool:
            if node in active:
                return True
            if node in complete:
                return False
            active.add(node)
            found = any(visit(child) for child in graph[node])
            active.remove(node)
            complete.add(node)
            return found
        return any(visit(node) for node in nodes)

    def _validated(self, result: dict[str, Any]) -> dict[str, Any]:
        validate_document(result, self.schema, self.schema.get("$defs", {}))
        return copy.deepcopy(result)

    def _refusal(self, qualified_id: Any, group: Any, reason: str, expected_head: Any) -> dict[str, Any]:
        safe_head: Optional[dict[str, Any]]
        try:
            safe_head = _head(expected_head)
        except _Refusal:
            safe_head = None
        result = {
            "schema": "decision-outcome-refusal/v1",
            "qualified_id": qualified_id if _nonempty(qualified_id) else "unknown",
            "group": group if group in {"G", "H"} else None,
            "reason": reason or "blocked_invalid_request",
            "expected_head": safe_head,
            "non_mutating": True,
            "grants_approval": False,
            "performs_promotion": False,
            "performs_archive": False,
            "performs_cleanup": False,
            "objective_outcome_claimed": False,
        }
        validate_document(result, self.schema, self.schema.get("$defs", {}))
        return result


__all__ = ["DecisionOutcomeLifecycleError", "DecisionOutcomeLifecycleV1"]
