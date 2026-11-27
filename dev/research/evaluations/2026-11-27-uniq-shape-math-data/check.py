"""Check exact symbolic identities and the report's atlas fragment."""
import csv
import re
from pathlib import Path

import sympy as s

DATA = Path(__file__).resolve().parent
REPORT = DATA.with_name(DATA.name.removesuffix("-data") + "-report.md")
ATLAS = DATA.parent.parent / "existence-atlas"
ROOT = DATA.parents[3]


def assert_equal(left, right):
    assert s.simplify(left - right) == 0, (left, right)


def check_scores():
    y, a, b, c, p, q, k, A, B, t, eta, alpha = s.symbols(
        "y a b c p q k A B t eta alpha", positive=True
    )
    v = s.exp(a * (y - b))
    logistic = v / (1 + v)
    ew = s.log(a) + s.log(v) - v + (c - 1) * s.log(1 - s.exp(-v))
    assert_equal(s.diff(ew, y), a * (1 - v + (c - 1) * v / (s.exp(v) - 1)))
    pgw = s.log(logistic) + c * s.log(1 + v) - (1 + v) ** c
    assert_equal(s.diff(pgw, y), a * (1 - logistic) + a * c * logistic * (1 - (1 + v) ** c))
    assert_equal(s.diff(p * s.log(v) - (p + q) * s.log(1 + v), y), a * (p - (p + q) * logistic))
    signed = k * a * y - A * s.exp(a * y)
    assert_equal(s.diff(signed, y), k * a - A * a * s.exp(a * y))
    gb = p * a * y + (q - 1) * s.log(1 - s.exp(a * (y - B)))
    assert_equal(s.diff(gb, y), p * a - (q - 1) * a / (s.exp(a * (B - y)) - 1))
    bprime = (c * p - 1) * s.log(t) - (p + q) * s.log(1 + (t / A) ** c)
    h = (t / A) ** c
    assert_equal(s.diff(bprime, t), (c * p - 1 - c * (p + q) * h / (1 + h)) / t)
    fatigue = s.log(t + b) - 3 * s.log(t) / 2 - (t / b + b / t - 2) / (2 * alpha**2)
    assert_equal(s.diff(fatigue, t), 1 / (t + b) - 3 / (2 * t) - (1 / b - b / t**2) / (2 * alpha**2))
    sgomp = -b * t - eta * s.exp(-b * t) + s.log(1 + eta * (1 - s.exp(-b * t)))
    v = s.exp(-b * t)
    assert_equal(s.diff(sgomp, t), -b + b * eta * v * (1 + 1 / (1 + eta - eta * v)))
    assert_equal(s.diff(-(a + 1) * s.log(t) - (b / t) ** a, t),
                 -(a + 1) / t + a * b**a / t**(a + 1))
    assert_equal(-s.diff((a - 1) * s.log(t) - (t / b) ** a, t),
                 -(a - 1) / t + a * t**(a - 1) / b**a)
    scale, shape = s.symbols("scale shape", positive=True)
    gpd = -s.log(scale) - (1 / shape + 1) * s.log(1 + shape * t / scale)
    assert_equal(s.diff(gpd, t), -(1 + shape) / (scale + shape * t))


def check_triangular():
    width, gap, weight, left_count, right_count = s.symbols(
        "width gap weight left_count right_count", positive=True
    )
    log_height = s.log(2 / width) + weight * s.log(1 - gap / width)
    assert_equal(s.diff(log_height, width).subs(width, gap * (1 + weight)), 0)
    optimized = s.log(2 / gap) - s.log(1 + weight) + weight * s.log(weight / (1 + weight))
    assert_equal(s.diff(optimized, weight), s.log(weight / (1 + weight)))
    a = width * left_count / right_count
    assert_equal(left_count * (1 - gap / a) + right_count * (1 + gap / width),
                 left_count + right_count)


def headings(path):
    return {
        re.sub(r"[^\w -]", "", line.lower()).replace(" ", "-")
        for line in re.findall(r"^#{1,6}\s+(.+)$", path.read_text(), re.M)
    }


def check_structure():
    body = REPORT.read_text()
    table = dict(re.findall(r"^\| (.+?) \| (?:.*?) \| (.+?) \|$", body, re.M))
    sections = re.findall(r"^### (\w+) held (.+)$", body, re.M)
    assert len(set(sections)) == len(sections) == 52
    assert {f"{family} held {held}" for family, held in sections} <= table.keys()
    with (ATLAS / "atlas.csv").open() as stream:
        atlas = {(row["family"], row["held"]) for row in csv.DictReader(stream)}
    with (ATLAS / "proofs.csv").open() as stream:
        fields = next(csv.reader(stream))
    with (DATA / "proofs.csv").open() as stream:
        reader = csv.DictReader(stream)
        assert reader.fieldnames == fields
        rows = list(reader)
    keys = [(row["family"], row["held"] or "all free") for row in rows]
    assert keys == sections and set(keys) <= atlas
    for row in rows:
        assert row["question"] == "uniqueness" and row["status"] == "written"
        assert row["direction"] in {"one-sided", "exact"}
        filename, anchor = row["evidence"].split("#")
        assert ROOT / filename == REPORT
        assert anchor in headings(REPORT), anchor
    for document in (REPORT, DATA / "method.md"):
        content = document.read_text()
        assert all(ord(char) >= 32 or char == "\n" for char in content), document
        for link in re.findall(r"\]\(([^)\s]+)\)", content):
            if link.startswith("https://"):
                continue
            filename, _, anchor = link.partition("#")
            path = document.parent / filename if filename else document
            assert path.is_file(), (document, link)
            assert not anchor or anchor in headings(path), (document, link)
    count = sum(row["direction"] == "exact" for row in rows)
    print(f"Atlas rows and evidence anchors pass ({len(rows)} regimes, {count} exact)")


if __name__ == "__main__":
    check_scores()
    check_triangular()
    print("Exact symbolic score and triangular identities pass")
    check_structure()
