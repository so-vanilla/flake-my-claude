"""Dependency-free validator for the JSON-schema subset used by A6R."""

from __future__ import annotations

import re
from collections.abc import Mapping
from typing import Any, Optional


class SchemaValidationError(ValueError):
    """The document does not satisfy a local A6R schema."""


def _matches_type(value: Any, expected: str) -> bool:
    return {
        "object": isinstance(value, Mapping),
        "array": isinstance(value, list),
        "string": isinstance(value, str),
        "integer": isinstance(value, int) and not isinstance(value, bool),
        "number": isinstance(value, (int, float)) and not isinstance(value, bool),
        "boolean": isinstance(value, bool),
        "null": value is None,
    }.get(expected, True)


def validate_document(
    document: Any,
    schema: Mapping[str, Any],
    registry: Optional[Mapping[str, Mapping[str, Any]]] = None,
    path: str = "$",
) -> None:
    """Validate local schemas' type, shape, digest, and enum constraints."""

    registry = registry or {}
    reference = schema.get("$ref")
    if reference:
        name = str(reference).rsplit("/", 1)[-1]
        target = registry.get(name)
        if target is None:
            raise SchemaValidationError("%s: unresolved schema reference %s" % (path, reference))
        validate_document(document, target, registry, path)
        return
    if "const" in schema and document != schema["const"]:
        raise SchemaValidationError("%s: expected const %r" % (path, schema["const"]))
    if "enum" in schema and document not in schema["enum"]:
        raise SchemaValidationError("%s: value is not in enum" % path)
    expected_type = schema.get("type")
    if expected_type is not None:
        choices = expected_type if isinstance(expected_type, list) else [expected_type]
        if not any(_matches_type(document, choice) for choice in choices):
            raise SchemaValidationError("%s: expected %s" % (path, expected_type))
    if isinstance(document, str):
        if len(document) < schema.get("minLength", 0):
            raise SchemaValidationError("%s: shorter than minLength" % path)
        if "pattern" in schema and re.search(schema["pattern"], document) is None:
            raise SchemaValidationError("%s: pattern mismatch" % path)
    if isinstance(document, Mapping):
        missing = [key for key in schema.get("required", []) if key not in document]
        if missing:
            raise SchemaValidationError("%s: missing %s" % (path, ", ".join(missing)))
        if len(document) < schema.get("minProperties", 0):
            raise SchemaValidationError("%s: fewer than minProperties" % path)
        properties = schema.get("properties", {})
        additional = schema.get("additionalProperties", True)
        for key, value in document.items():
            if key in properties:
                validate_document(value, properties[key], registry, "%s.%s" % (path, key))
            elif additional is False:
                raise SchemaValidationError("%s: unsupported property %s" % (path, key))
            elif isinstance(additional, Mapping):
                validate_document(value, additional, registry, "%s.%s" % (path, key))
    if isinstance(document, list) and isinstance(schema.get("items"), Mapping):
        if len(document) < schema.get("minItems", 0):
            raise SchemaValidationError("%s: fewer than minItems" % path)
        if "maxItems" in schema and len(document) > schema["maxItems"]:
            raise SchemaValidationError("%s: more than maxItems" % path)
        if schema.get("uniqueItems"):
            fingerprints = [repr(item) for item in document]
            if len(fingerprints) != len(set(fingerprints)):
                raise SchemaValidationError("%s: items are not unique" % path)
        for index, value in enumerate(document):
            validate_document(value, schema["items"], registry, "%s[%s]" % (path, index))
    if isinstance(document, (int, float)) and not isinstance(document, bool) and document < schema.get("minimum", document):
        raise SchemaValidationError("%s: below minimum" % path)
    for keyword in ("anyOf", "oneOf", "allOf"):
        alternatives = schema.get(keyword)
        if alternatives is None:
            continue
        successes = 0
        for alternative in alternatives:
            try:
                validate_document(document, alternative, registry, path)
            except SchemaValidationError:
                continue
            successes += 1
        if keyword == "anyOf" and successes < 1:
            raise SchemaValidationError("%s: anyOf constraint failed" % path)
        if keyword == "oneOf" and successes != 1:
            raise SchemaValidationError("%s: oneOf constraint failed" % path)
        if keyword == "allOf" and successes != len(alternatives):
            raise SchemaValidationError("%s: allOf constraint failed" % path)


__all__ = ["SchemaValidationError", "validate_document"]
