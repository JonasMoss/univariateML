"""Compare the exact binary64 inputs of both density implementations with mpmath."""
import csv
import math
from pathlib import Path
import subprocess
import sys
import tarfile

import mpmath as mp

ROOT = Path(__file__).resolve().parents[4]
BASE = "5f1317e288f557d9b373385838d74442765f9d3f"
INCLUDE = "univariateML-cpp/include"
SOURCE = """#include "cppunivariate/distributions/invtrgamma/dpqr.hpp"
#include <iomanip>
#include <iostream>
int main() {
  namespace d = cppunivariate::distributions;
  double x, a, c, s;
  bool inverse;
  std::cout << std::setprecision(17);
  while (std::cin >> inverse >> x >> a >> c >> s)
    std::cout << (inverse ? d::dinvtrgamma(x,a,c,s,true) :
                             d::dtrgamma(x,a,c,s,true)) << '\\n';
}
"""


def points():
    for exponent in (2, 4, 5, 8, 12, 16, 20, 23, 28, 30, 40, 60, 100, 200, 300):
        a = 10.0**exponent
        for standardized in (-100, -12, -4, -1, 0, 1, 4, 12, 100):
            for inverse in (0, 1):
                # Hold the observed x in a compact range. Round the ideal
                # scale before evaluating either implementation or reference.
                x, c = 1.25, 2.0
                lu = mp.log(a) + standardized / mp.sqrt(a)
                s = float(x * mp.exp((2*inverse-1)*lu/c))
                yield inverse, x, a, c, s
        for z in (-550, -10, -1, -.01, .01, 1, 10, 550):
            for inverse in (0, 1):
                x, c = 1.25, 2.0
                s = float(x * mp.exp((2*inverse-1)*(mp.log(a)+z)/c))
                yield inverse, x, a, c, s
    for a in (1e8, 1e23, 1e100, 1e300):
        for num, den in ((math.nextafter(1., math.inf), 1.), (1e300, 1e-300)):
            for w in (-9, 0, 9):
                c = float((mp.log(a)+w/mp.sqrt(a))/mp.log(mp.mpf(num)/mp.mpf(den)))
                yield 0, num, a, c, den
                yield 1, den, a, c, num
    # This square differs from the rounded a by a single bit at 2^-104
    # relative to a. It exercises the fixed-point fallback with a nonzero z.
    for significand in ("0x1p+500", "0x1.0000000000001p+500"):
        num = float.fromhex(significand)
        a = float.fromhex("0x1p+1000" if num == 2.0**500 else "0x1.0000000000002p+1000")
        yield 0, num, a, 2., 1.
        yield 1, 1., a, 2., num
    yield 0, math.sqrt(sys.float_info.max), sys.float_info.max, 2., 1.
    yield 1, 1., sys.float_info.max, 2., math.sqrt(sys.float_info.max)

    for x in ("0x1.1cd03c7e16dbfp+0", "0x1.1cd03c7e18152p+0", "0x1.1cd03c7e194e4p+0",
              "0x1.1cd03c7e1a876p+0", "0x1.1cd03c7e1cf9bp+0"):
        yield (1, float.fromhex(x), 8.615406571602086e22,
               float.fromhex("0x1.faf47fa66b243p+0"), 424695594989.53754)


def main():
    work = ROOT / "build/scratch/trgamma-grid"
    work.mkdir(parents=True, exist_ok=True)
    archive = work / "baseline.tar"
    with archive.open("wb") as output:
        subprocess.run(["git", "archive", BASE, INCLUDE], cwd=ROOT, stdout=output, check=True)
    with tarfile.open(archive) as bundle:
        bundle.extractall(work, filter="data")
    archive.unlink()
    program = work / "probe.cpp"
    program.write_text(SOURCE)
    boost = subprocess.check_output(
        ["Rscript", "-e", 'cat(system.file("include",package="BH"))'], text=True)
    mp.mp.dps = 420
    grid = list(points())
    data = "".join(" ".join(map(str, point)) + "\n" for point in grid)
    outputs = []
    for label, include in [("before", work/INCLUDE), ("after", ROOT/INCLUDE)]:
        exe = work / label
        subprocess.run(["ccache", "c++", "-std=gnu++17", "-O2", "-DNDEBUG",
                        "-I" + str(include), "-I" + boost, str(program), "-o", str(exe)],
                       check=True)
        result = subprocess.run([str(exe)], input=data, capture_output=True, text=True, check=True)
        outputs.append(list(map(float, result.stdout.split())))
    writer = csv.writer(sys.stdout, lineterminator="\n")
    worst = 0.
    writer.writerow(["inverse", "x", "shape1", "shape2", "scale", "z", "reference",
                     "before", "after", "error_before", "error_after"])
    for (inverse, x, a, c, s), before, after in zip(grid, *outputs, strict=True):
        mx, ma, mc, ms = map(mp.mpf, (x, a, c, s))
        lu = (1-2*inverse)*mc*mp.log(mx/ms)
        z = lu-mp.log(ma)
        # Original density, exact rounded inputs. At 420 digits the raw
        # cancellation costs at most about 311 digits on this grid.
        ref = mp.log(mc/mx) + ma*lu - mp.exp(lu) - mp.loggamma(ma)
        r = float(ref)
        def error(value):
            if value == r:
                return 0.
            if not math.isfinite(value) or not math.isfinite(r):
                return math.inf
            return abs(value-r)/max(1, abs(r))
        worst = max(worst, error(after))
        writer.writerow([inverse, x.hex(), a, c, s.hex(), mp.nstr(z, 20), r,
                         before, after, error(before), error(after)])


if __name__ == "__main__":
    sys.exit(main())
