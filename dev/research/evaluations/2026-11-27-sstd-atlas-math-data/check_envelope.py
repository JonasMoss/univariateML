"""High-precision contact/collapse replays and proof-row integrity checks."""
import csv
import re
import sys
from pathlib import Path

import mpmath as mp

mp.mp.dps = 80
STEPS = [mp.mpf(10)**-p for p in (8, 16, 24)]

def moments(q):
    if q == mp.inf:
        k = mp.sqrt(2 / mp.pi)
        return k, k, mp.mpf(1)
    k = 2 * mp.gamma((q + 1) / 2) / (mp.sqrt(q * mp.pi) * mp.gamma(q / 2))
    return k, k*q / (q-1), q / (q-2) if q != 2 else mp.inf

def constrained(q, w, mean, sd):
    _, m, v = moments(q)
    z = 1 - 2*w
    width = sd / mp.sqrt(v*(1 + 3*z*z)/4 - m*m*z*z)
    return mean - m*z*width, width

def kernel(q, r):
    return mp.exp(-r*r/2) if q == mp.inf else (1 + r*r/q)**(-(q+1)/2)

def likelihood(xs, q, w, c, t):
    k, _, _ = moments(q)
    return mp.fprod(k/t * kernel(q, (x-c)/(t*(w if x < c else 1-w))) for x in xs)

def contacts():
    xs = (0, 0, 1)
    for endpoint in (mp.mpf(3), mp.inf):
        k, m, v = moments(endpoint)
        sd = mp.sqrt(v - m*m)
        contact_ratio = m*v / (2*(v-m*m))
        for mode in ("fixed", "matched") if endpoint == 3 else (0, 1, 10):
            errors = []
            for w in STEPS:
                if mode == "matched":
                    q = mp.findroot(lambda q: constrained(q, w, m, sd)[0], (3, 3+w))
                    assert q > 3
                    ratio = 0
                elif mode == "fixed":
                    q, ratio = endpoint, contact_ratio
                else:
                    h = w*w if mode == 0 else mode*w
                    q, ratio = 1/h, contact_ratio*(1 + mp.mpf(mode)/2)
                c, t = constrained(q, w, m, sd)
                target = k**3 * kernel(endpoint, 1) * kernel(endpoint, ratio)**2
                got = likelihood(xs, q, w, c, t)
                errors.append(abs(mp.log(got/target)))
            assert errors[2] < errors[1] < errors[0]
            assert errors[-1] < mp.mpf("1e-18")
            print("contact", endpoint, mode, mp.nstr(errors[-1], 5))

def collapse():
    xs = (-1, 0, 0, 0, 0, 2)
    for w in (mp.mpf(1)/2, mp.mpf(1)/5):
        for mean_held in (False, True):
            for sd_held in (False, True):
                z = 1 - 2*w
                residual = z/(1-w) if mean_held else 0
                j = (1 + residual**2)**(-mp.mpf(3)/2)
                target = (w*(1-w)/2)**3 * j**4
                errors = []
                for eps in STEPS:
                    q = 2 + eps
                    _, m, _ = moments(q)
                    if sd_held:
                        _, t = constrained(q, w, 0, 1)
                    else:
                        t = mp.root(eps, 4)
                    c = -m*z*t if mean_held else 0
                    errors.append(abs(mp.log(likelihood(xs, q, w, c, t)/target)))
                assert errors[2] < errors[1] < errors[0]
                assert errors[-1] < mp.mpf("1e-5")
                print("collapse", mp.nstr(w, 3), mean_held, sd_held, mp.nstr(errors[-1], 5))

def audit_rows(report):
    atlas = Path("dev/research/existence-atlas")
    data = Path(__file__).resolve().parent
    with (atlas / "atlas.csv").open() as stream:
        masks = {row["held"] for row in csv.DictReader(stream) if row["family"] == "sstd"}
    with (atlas / "proofs.csv").open() as stream:
        columns = next(csv.reader(stream))
    with (data / "proofs.csv").open() as stream:
        reader = csv.DictReader(stream)
        assert reader.fieldnames == columns
        rows = list(reader)
    anchor = lambda title: re.sub(r"[^\w -]", "", title.lower()).replace(" ", "-")
    def headings(path):
        return {anchor(title) for title in re.findall(r"^#+ (.+)$", path.read_text(), re.M)}

    anchors = headings(report)
    content = report.read_text()
    assert all(ord(char) >= 32 or char == "\n" for char in content)
    for destination in re.findall(r"\[[^\[\]]+\]\(([^()]+)\)", content):
        path, _, section = destination.partition("#")
        target_path = report.parent / path
        assert target_path.is_file(), destination
        if section and not re.fullmatch(r"L\d+", section):
            assert section in headings(target_path), destination
    for row in rows:
        assert row["family"] == "sstd"
        assert (row["held"] or "all free") in masks
        path, section = row["evidence"].split("#")
        assert Path(path).name == "2026-11-27-sstd-atlas-math-report.md"
        assert section == anchor("sstd held " + (row["held"] or "all free"))
        assert section in anchors
        assert row["status"] == "written"
        assert row["direction"] == {"criterion": "iff", "failure": "exact", "uniqueness": "exact"}[row["question"]]
    actual = {(row["held"], row["question"]) for row in rows}
    target = (masks - {"all free", "nu+sd+xi", "mean+nu+sd+xi"}) | {""}
    expect = {(mask, question) for mask in target for question in ("criterion", "failure")}
    expect |= {(mask, "uniqueness") for mask in target - {"mean+sd+xi", "mean+xi", "xi"}}
    assert actual == expect and len(actual) == len(rows)
    print("atlas row audit", len(rows), "rows")

if __name__ == "__main__":
    contacts()
    collapse()
    audit_rows(Path(sys.argv[1]) if len(sys.argv) > 1 else Path(__file__).resolve().parent.with_name(
        "2026-11-27-sstd-atlas-math-report.md"))
