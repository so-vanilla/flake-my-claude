"""Black-box business acceptance; kept outside the implementation project."""
import csv
import io
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import time
import unittest

PROJECT = Path(sys.argv[1]).resolve()
sys.argv = sys.argv[:1]
NOW = "2026-09-06T09:00:00+09:00"
HEADER = ["id", "priority", "status", "due_at", "summary"]


def row(identifier="A", priority="normal", status="open", due="2026-09-05T23:00:00Z", summary="問い合わせ"):
    return [identifier, priority, status, due, summary]


class BusinessAcceptance(unittest.TestCase):
    def run_cli(self, rows=None, *, raw=None, fmt="json", as_of=NOW, missing=False):
        with tempfile.TemporaryDirectory(prefix="sla-acceptance-") as temporary:
            path = Path(temporary) / "tickets.csv"
            if not missing:
                if raw is None:
                    buffer = io.StringIO(newline="")
                    writer = csv.writer(buffer)
                    writer.writerow(HEADER)
                    writer.writerows(rows or [])
                    raw = buffer.getvalue()
                path.write_text(raw, encoding="utf-8")
            return subprocess.run(
                [sys.executable, "-m", "sla_report", str(path), "--as-of", as_of, "--format", fmt],
                cwd=PROJECT, text=True, capture_output=True, timeout=5,
                env={**os.environ, "PYTHONHASHSEED": "123", "PYTHONDONTWRITEBYTECODE": "1"},
            )

    def output(self, rows):
        result = self.run_cli(rows)
        self.assertEqual(0, result.returncode, result.stderr)
        parsed = json.loads(result.stdout)
        self.assertEqual(len(parsed["tickets"]), parsed["count"])
        return parsed

    def reject(self, result, *, field=None):
        self.assertEqual(2, result.returncode, result.stderr)
        self.assertEqual("", result.stdout)
        self.assertTrue(result.stderr.strip())
        if field:
            self.assertIn(field, result.stderr)
            self.assertRegex(result.stderr, r"\d+")

    def test_overdue_boundary_and_status(self):
        result = self.output([row("late"), row("equal", due=NOW), row("future", due="2026-09-06T00:00:01Z"), row("closed", status="closed"), row("active", status="in_progress")])
        self.assertEqual(["active", "late"], [r["id"] for r in result["tickets"]])

    def test_priority_instant_and_id_sort(self):
        rows = [row("normal"), row("critical", "critical"), row("low", "low"), row("z", "high", due="2026-09-05T23:00:00Z"), row("a", "high", due="2026-09-06T08:00:00+09:00"), row("earlier", "high", due="2026-09-06T06:30:00+09:00")]
        result = self.output(rows)
        self.assertEqual(["critical", "earlier", "a", "z", "normal", "low"], [r["id"] for r in result["tickets"]])

    def test_non_ascii_fields_preserved(self):
        data = row("日本語", summary="調査, 見積もり 🚀")
        self.assertEqual(dict(zip(HEADER, data)), self.output([data])["tickets"][0])

    def test_empty_valid_input(self):
        self.assertEqual({"count": 0, "tickets": []}, self.output([]))

    def test_equivalent_as_of_offsets(self):
        a = self.run_cli([row()], as_of="2026-09-06T09:00:00+09:00")
        b = self.run_cli([row()], as_of="2026-09-06T00:00:00Z")
        self.assertEqual(0, a.returncode, a.stderr)
        self.assertEqual(0, b.returncode, b.stderr)
        self.assertEqual(json.loads(a.stdout), json.loads(b.stdout))

    def test_duplicate_even_if_closed(self):
        self.reject(self.run_cli([row(), row(status="closed")]), field="id")

    def test_empty_id(self):
        self.reject(self.run_cli([row(identifier="")]), field="id")

    def test_invalid_priority_even_if_closed(self):
        self.reject(self.run_cli([row(status="closed", priority="urgent")]), field="priority")

    def test_invalid_status(self):
        self.reject(self.run_cli([row(status="done")]), field="status")

    def test_invalid_due_date(self):
        self.reject(self.run_cli([row(due="2026-02-30T00:00:00Z")]), field="due_at")

    def test_timezone_missing(self):
        self.reject(self.run_cli([row(due="2026-09-05T23:00:00")]), field="due_at")

    def test_invalid_as_of(self):
        self.reject(self.run_cli([row()], as_of="yesterday"))

    def test_naive_as_of(self):
        self.reject(self.run_cli([row()], as_of="2026-09-06T09:00:00"))

    def test_missing_header(self):
        self.reject(self.run_cli(raw="id,priority,status,due_at\nA,normal,open,2026-09-05T23:00:00Z\n"))

    def test_missing_file(self):
        self.reject(self.run_cli(missing=True))

    def test_malformed_quote(self):
        self.reject(self.run_cli(raw=",".join(HEADER)+'\nA,normal,open,2026-09-05T23:00:00Z,"unterminated\n'))

    def test_invalid_later_row_never_emits_partial_report(self):
        self.reject(self.run_cli([row(), row("B", status="bad")]), field="status")

    def test_markdown_escapes_delimiters_and_newlines(self):
        result = self.run_cli([row(summary="a|b\n日本語")], fmt="markdown")
        self.assertEqual(0, result.returncode, result.stderr)
        self.assertIn("日本語", result.stdout)
        self.assertRegex(result.stdout, r"(?:a\\\|b|a&#124;b|a&#x7[cC];b|a&vert;b)")
        self.assertNotIn("a|b\n", result.stdout)
        self.assertRegex(result.stdout, r"1")

    def test_help(self):
        result = subprocess.run([sys.executable, "-m", "sla_report", "--help"], cwd=PROJECT, text=True, capture_output=True, timeout=5)
        self.assertEqual(0, result.returncode, result.stderr)
        self.assertIn("--as-of", result.stdout)


    def test_unquoted_quote_is_invalid(self):
        self.reject(self.run_cli(raw=",".join(HEADER)+'\nA,normal,open,2026-09-05T23:00:00Z,unquoted"quote\n'))

    def test_literal_quote_and_multiline_valid(self):
        data = row(summary='引用 "hello"\n次の行')
        self.assertEqual(dict(zip(HEADER, data)), self.output([data])["tickets"][0])

    def test_submicrosecond_precision_rejected(self):
        self.reject(self.run_cli([row(due="2026-09-05T23:00:00.0000001Z")]), field="due_at")
        self.reject(self.run_cli([row()], as_of="2026-09-06T00:00:00.0000002Z"))

    def test_one_to_six_fraction_digits_and_microsecond_boundary(self):
        for digits in ("1", "12", "123", "1234", "12345", "123456"):
            result = self.run_cli([row(due="2026-09-05T23:00:00."+digits+"Z")])
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(json.loads(result.stdout)["count"], 1)
        result = self.run_cli([row("before", due="2026-09-06T00:00:00.000001Z"),
                               row("equal", due="2026-09-06T00:00:00.000002Z")],
                              as_of="2026-09-06T00:00:00.000002Z")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual([r["id"] for r in json.loads(result.stdout)["tickets"]], ["before"])

    def test_header_only_and_row_width(self):
        self.reject(self.run_cli(raw=",".join(HEADER)+"\nA,normal,open\n"))
        self.reject(self.run_cli(raw=",".join(HEADER)+"\nA,normal,open,2026-09-05T23:00:00Z,s,extra\n"))


if __name__ == "__main__":
    start = time.monotonic()
    result = unittest.TextTestRunner(verbosity=2).run(unittest.defaultTestLoader.loadTestsFromTestCase(BusinessAcceptance))
    print(json.dumps({"tests": result.testsRun, "failures": len(result.failures), "errors": len(result.errors), "seconds": round(time.monotonic()-start, 3)}))
    raise SystemExit(not result.wasSuccessful())
