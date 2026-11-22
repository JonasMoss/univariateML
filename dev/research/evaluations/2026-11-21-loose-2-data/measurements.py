"""Join reproducible fitter observations to the independent references."""
from collections import defaultdict
import csv
import json
import math
from pathlib import Path
import re
import subprocess
import sys

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
DISTS = ROOT / "univariateML-cpp/include/cppunivariate/distributions"
MODIFIERS = ROOT / "univariateML-cpp/tests/modifiers"

refs = defaultdict(list)
with (HERE / "references.csv").open() as file:
    for row in csv.DictReader(file):
        refs[row["fixture"]].append(float(row["binary64"]))


def observations(prefix, mode):
    with (HERE / f"{prefix}-{mode}.csv").open() as file:
        return {key: values for key, *values in csv.reader(file)}


native = observations("probe", "native")
double = observations("probe", "double")
out = csv.writer(sys.stdout, lineterminator="\n")
out.writerow(("site", "replicate", "coefficient", "class", "old_comparison", "new_comparison",
              "old_reference", "new_reference", "native_fit", "double_fit", "native_error", "double_error"))


def entry(site, fixture, coords, old, ref, before, after, *, kind="frozen", source=None, rep=0):
    left, right = source if source is not None else (native, double)
    for coord, previous, target, position in zip(coords, old, ref, range(len(coords))):
        ln, rn = float(left[fixture][position]), float(right[fixture][position])
        scale = abs(target) or 1.
        out.writerow((site, rep, coord, kind, before, after, previous, target, ln, rn,
                      abs(ln-target)/scale, abs(rn-target)/scale))


def saved(family, *keys):
    return [json.loads((DISTS / family / "golden.json").read_text())["estimates"][key]
            for key in keys]


bhatt_coords = ["mu", "sigma", "a"]
entry("bhatt/test.cpp:112–118", "bhatt/golden", bhatt_coords,
      saved("bhatt", *bhatt_coords), refs["bhatt/golden"], "kBhattGoldenTol",
      "kBhattGoldenTol + relative reference")
entry("bhatt/test.cpp:273", "bhatt/newton", ["mu"], [float("-.00380006892159174971312956250794")],
      refs["bhatt/golden"][:1], "Approx(1e-8)", "relative reference")
p = (DISTS / "bhatt/test.cpp").relative_to(ROOT)
prior = subprocess.check_output(["git", "show", f"460a0ff2:{p}"], cwd=ROOT, text=True).splitlines()
for case, line, columns, ref_key, start in [
    (0,636,[2],0,1),(1,643,[2],1,1),(2,650,[2],2,1),(3,657,[2],3,1),(4,664,[2],4,1),
    (5,669,[0],5,0),(5,671,[2],5,2),(6,676,[0],6,0),(6,678,[2],6,2),
    (7,685,[2],1,1),(8,695,[2],8,0),(9,702,[2],9,0),(10,709,[2],10,0),
    (11,716,[2],11,0),(12,723,[2],12,0),(13,730,[2],12,0),
]:
    fixture = f"bhatt/near/{case}"
    old = float(re.search(r"Approx\(([^)]+)\)", prior[line-1])[1])
    source = tuple({fixture: [measurements[fixture][i] for i in columns]}
                   for measurements in (native, double))
    entry(f"bhatt/test.cpp:{line}", fixture, [bhatt_coords[i] for i in columns],
          [old], refs[f"bhatt/near/{ref_key}"][start:start+len(columns)], "Approx(1e-10) scale 1",
          "relative reference", kind="recovery" if case in (7,13) else "frozen", source=source)
gb2_coords = ["shape1", "shape2", "scale"]
entry("gb2/test.cpp:191–194", "gb2/burr", gb2_coords, saved("burr", *gb2_coords),
      refs["gb2/burr"], "kBurrGoldenTol", "relative reference")
mixed_old = [[.57834363213651174,1.7944752190424211],[3.3085427329308605,.43155118384080654]]
mixed_coords = [["shape2","shape3"], ["shape1","shape2"]]
for i in range(2):
    key = f"gb2/mixed/{i}"
    entry("gb2/test.cpp:447–448", key, mixed_coords[i], mixed_old[i], refs[key],
          "Approx(1e-7)", "relative reference", rep=i)
entry("gengamma/test.cpp:308","gengamma/normal",["mu"],[.5],[.5],
      "Approx default","exact", kind="analytic")
entry("kumar/test.cpp:58","kumar/clipped",["a"],[.007765975729959528],refs["kumar/clipped"],
      "Approx(1e-8)","relative reference")
entry("sgomp/test.cpp:21","sgomp/exponential",["b"],[5/11],[5/11],
      "Approx default","relative reference",kind="analytic")
entry("sstd/test.cpp:365–366","sstd/ties",["mu left","mu right"],[-math.sqrt(3),math.sqrt(3)],
      [-math.sqrt(3),math.sqrt(3)],"Approx(1e-8)","relative reference",kind="analytic")
entry("sstd/test.cpp:377","sstd/log-scale",["log(sigma)"],[5.894765235143508],refs["sstd/log-scale"],
      "Approx(1e-8)","relative reference")
entry("st/test.cpp:178","st/slant",["alpha"],[.16456272666935215],refs["st/slant"],
      "Approx(1e-8)","relative reference")
entry("tests/modifiers/zinb.cpp:201","zinb/held-inflation",["prob"],[.34428033824686793],
      refs["zinb/held-inflation"][:1],"Approx(2e-7)","relative 1e-12 + golden check",rep=2)

prior = HERE.parent / "2026-11-21-loose-tests-data/cpp-measurements.csv"
with prior.open() as file:
    rows = {(r["file"], int(r["baseline_line"]), int(r["replicate"]), r["mode"]): r
            for r in csv.DictReader(file)}
for file, line, coef, before, after in [
    ("bnbinom",252,"free coordinate","Approx(1e-8)","relative 1e-8"),
    ("bnbinom",277,"alpha","Approx(1e-8)","relative 1e-8"),
    ("expweibull",240,"exponent","Approx(1e-6)","relative 1e-6"),
    ("ged",475,"shape","Approx(1e-5)","relative 1e-6"),
    ("lst",521,"mu","Approx(1e-7)","contract scale floor + retained check"),
    ("pig",64,"mean","Approx(1e-6)","relative 1e-9"),
    ("std",326,"mu","Approx(1e-7)","contract scale floor + retained check"),
]:
    filename = f"{file}/test.cpp"
    i = 0
    while (filename,line,i,"native") in rows:
        left, right = [rows[(filename,line,i,mode)] for mode in ("native","double")]
        out.writerow((f"{filename}:{line}", i, coef, left["class"], before, after,
                      left["old"], left["reference"], left["fit"], right["fit"],
                      left["error"], right["error"]))
        i += 1

if "--cpp-only" in sys.argv:
    sys.exit(0)

r = tuple(observations("r-probe", mode) for mode in ("native","double"))
entry("tests/testthat/test_golden_regression.R:254–265","bhatt/golden",bhatt_coords,
      saved("bhatt", *bhatt_coords), refs["bhatt/golden"], "2e-6", "2e-6 + relative 1e-10", source=r)
for i, line in enumerate((233,235)):
    key = f"gb2/mixed/{i}"
    entry(f"gb2/test.R:{line}",key,mixed_coords[i],mixed_old[i],refs[key],
          "expect_equal(1e-7)","componentwise relative 1e-10",source=r)
for line, key, coords, values in [
    ("107","bhatt/constant/1",bhatt_coords,[2,1,0]), ("110","bhatt/constant/2",["mu"],[2]),
    ("239","bhatt/normal/1",["a"],[0]), ("242","bhatt/normal/2",bhatt_coords,[1,4,0]),
    ("279","bhatt/normal/3",bhatt_coords,[0,4,0]),
    ("324","bhatt/normal/4",bhatt_coords,[0,2,0]), ("90","sgomp/constant",["b","eta"],[2,0]),
]:
    family = key.split("/")[0]
    entry(f"{family}/test.R:{line}",key,coords,values,values,"expect_equal default","exact",
          kind="analytic",source=r)
entry("sgomp/test.R:40–41","sgomp/exponential",["b","eta"],[1/(11/5),0],[5/11,0],
      "expect_equal default","relative 1e-10 and exact eta",kind="analytic",source=r)
