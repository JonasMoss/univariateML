"""Compare finite CDF and quantile inputs in the regenerated truth fixtures.

Run density-grid.py first to stage the baseline headers in build/scratch.
"""
from collections import defaultdict
import json
import math
from pathlib import Path
import subprocess

ROOT = Path(__file__).resolve().parents[4]
WORK = ROOT / "build/scratch/trgamma-grid"
INCLUDE = "univariateML-cpp/include"
SOURCE = """#include "cppunivariate/distributions/invtrgamma/dpqr.hpp"
#include <iomanip>
#include <iostream>
int main() {
  namespace d = cppunivariate::distributions;
  bool inverse, quantile, lower, log;
  double x, a, c, s;
  std::cout << std::setprecision(17);
  while (std::cin >> inverse >> quantile >> lower >> log >> x >> a >> c >> s) {
    if (quantile)
      std::cout << (inverse ? d::qinvtrgamma(x,a,c,s,lower,log) :
                               d::qtrgamma(x,a,c,s,lower,log));
    else
      std::cout << (inverse ? d::pinvtrgamma(x,a,c,s,lower,log) :
                               d::ptrgamma(x,a,c,s,lower,log));
    std::cout << '\\n';
  }
}
"""


def requests(inverse, truth):
    params = truth["parameters"]
    for i, x, _, lower, upper in truth["values"]:
        if math.isfinite(float(x)):
            yield ("log CDF", i, float(lower)), (inverse, 0, 1, 1, x, *params[i])
            yield ("log CDF", i, float(upper)), (inverse, 0, 0, 1, x, *params[i])
    for i, lower, log, p, x, *_ in truth["quantiles"]:
        if math.isfinite(float(p)):
            yield ("quantile", i, float(x)), (inverse, 1, lower, log, p, *params[i])


def measure(exe, inverse, family):
    path = ROOT / INCLUDE / f"cppunivariate/distributions/{family}/truth.json"
    truth = json.loads(path.read_text())
    expectations, probes = zip(*requests(inverse, truth))
    data = "".join(" ".join(map(str, probe)) + "\n" for probe in probes)
    result = subprocess.run([str(exe)], input=data, text=True, capture_output=True, check=True)
    worst = defaultdict(float)
    for (kind, i, expected), observed in zip(expectations, map(float, result.stdout.split()), strict=True):
        if observed == expected:
            error = 0.
        elif not (math.isfinite(observed) and math.isfinite(expected)):
            error = math.inf
        else:
            scale = max(1, abs(expected)) if kind == "log CDF" else abs(expected)
            error = abs(observed - expected) / scale if scale else math.inf
        worst[kind, i] = max(worst[kind, i], error)
    return [(kind, i, a, worst[kind, i]) for i, (a, *_) in enumerate(truth["parameters"])
            if a >= 1e5 for kind in ("log CDF", "quantile")]


def main():
    source = WORK / "probability.cpp"
    source.write_text(SOURCE)
    boost = subprocess.check_output(
        ["Rscript", "-e", 'cat(system.file("include",package="BH"))'], text=True)
    print("version,family,kind,point,shape1,max_scaled_error")
    for label, include in (("before", WORK/INCLUDE), ("after", ROOT/INCLUDE)):
        exe = WORK / ("probability-" + label)
        subprocess.run(["ccache", "c++", "-std=gnu++17", "-O2", "-DNDEBUG",
                        "-I" + str(include), "-I" + boost, str(source), "-o", str(exe)], check=True)
        for inverse, family in enumerate(("trgamma", "invtrgamma")):
            for kind, i, a, error in measure(exe, inverse, family):
                print(f"{label},{family},{kind},{i},{a},{error:.17g}")


if __name__ == "__main__":
    main()
