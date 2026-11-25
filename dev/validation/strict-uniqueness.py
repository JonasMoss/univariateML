"""Require a reviewed proof site for each source path that publishes a singleton.

The inventory records outcome expressions and the guards that assign
singleton provenance. Keep the inventory tied to these expressions rather
than to line numbers so ordinary moves within a header need no update.
"""
import csv
import re
from collections import Counter
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent.parent
CORE = ROOT / "univariateML-cpp/include/cppunivariate"

# Explicit false is the typed set path. The one-argument legacy overload is
# checked too, with the snorm exception recorded in the inventory.
CLAIMS = re.compile(
    r"(?P<call>FitOutcome::(?:singleton|estimate)\s*\()"
    r"|(?P<assignment>\b\w+\.singleton_proved\s*=(?!=)"
    r"|\boutcome\.unique\s*=(?!=)"
    r"|\boutcome\.uniqueness_known\s*=(?!=))"
    r"|(?P<aggregate>\{\s*OutcomeKind::Estimate\s*,)"
)


def claim_end(source, match):
    # Balance nested arguments, including lambdas in proof guards.
    if match.lastgroup == "call":
        depth = 1
        for i in range(match.end(), len(source)):
            depth += (source[i] == "(") - (source[i] == ")")
            if depth == 0:
                return i + 1
    else:
        depth = int(match.lastgroup == "aggregate")
        for i in range(match.end(), len(source)):
            symbol = source[i]
            if symbol == ";" and depth == 0:
                return i
            depth += (symbol in "({[") - (symbol in ")}]")
    raise ValueError(f"unterminated singleton proof site {match[0]}")


def proof_sites():
    headers = list((CORE / "distributions").glob("*/*.hpp"))
    headers.extend((ROOT / "bindings/common").rglob("*.hpp"))
    for path in sorted(headers):
        source = re.sub(r"/\*.*?\*/|//[^\n]*", "", path.read_text(), flags=re.S)
        for match in CLAIMS.finditer(source):
            expression = " ".join(source[match.start():claim_end(source, match)].split())
            if re.fullmatch(r"FitOutcome::estimate\([^,]+,\s*false\)", expression):
                continue
            yield str(path.relative_to(ROOT)), expression


def main():
    with (HERE / "singleton-proof-sites.csv").open(newline="") as source:
        rows = list(csv.DictReader(source))
    expected = Counter((row["file"], row["expression"]) for row in rows)
    actual = Counter(proof_sites())
    extra, missing = actual - expected, expected - actual
    for path, expression in extra:
        print(f"NEW singleton-proof site {path} {expression}")
    for path, expression in missing:
        print(f"Missing singleton-proof site {path} {expression}")
    missing_proofs = []
    for row in rows:
        if row["proof"].startswith("TASK-254 (temporary exemption"):
            continue
        references = row["proof"].split("; ")
        if any(not reference or not (ROOT / reference.split("#", 1)[0].split(" (", 1)[0]).is_file()
               for reference in references):
            missing_proofs.append(row["file"])
    for path in missing_proofs:
        print(f"NEW missing singleton evidence reference for {path}")
    print(f"Singleton proof inventory covers {sum(actual.values())} source sites")
    return int(bool(extra or missing or missing_proofs))


if __name__ == "__main__":
    raise SystemExit(main())
