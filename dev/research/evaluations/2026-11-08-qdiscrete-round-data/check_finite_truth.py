#!/usr/bin/env python3
"""Replay every finite, above-2^53 discrete oracle target against its CDF.

From the repository root, run through /tmp/umvl-cpu/run. Uses installed BH
headers, the configured CXX (or c++), and build/scratch for its small products.
Set UMVL_INCLUDE_OVERFLOW=1 to include overflow targets and verify +Inf
against DBL_MAX. This is a computed-CDF check, not a high-precision oracle.
"""
import json
import math
import os
from pathlib import Path
import subprocess

root = Path(__file__).resolve().parents[4]
source_dir = root / "univariateML-cpp/include/cppunivariate/distributions"
scratch = root / "build/scratch"
scratch.mkdir(parents=True, exist_ok=True)
blocks = []
families = []
include_overflow = os.environ.get("UMVL_INCLUDE_OVERFLOW") == "1"
for table in sorted(source_dir.glob("*/truth.json")):
    if "type: integer" not in table.with_name("README.md").read_text():
        continue
    truth = json.loads(table.read_text())
    family = table.parent.name
    for index, row in enumerate(truth["quantiles"]):
        point, lower, log, target, expected = row[:5]
        target, expected = float(target), float(expected)
        interior = math.isfinite(target) and target < 0 if log else 0 < target < 1
        if not interior or (not math.isfinite(expected) and not include_overflow) or abs(expected) <= 2**53:
            continue
        if family not in families:
            families.append(family)
        args = ",".join(map(str, truth["parameters"][int(point)]))
        flags = f"{str(bool(lower)).lower()},{str(bool(log)).lower()}"
        blocks.append(f'''{{
  const double p={target!r}, h=cppunivariate::distributions::q{family}(p,{args},{flags});
  const double at=cppunivariate::distributions::p{family}(h,{args},{flags});
  const double before=cppunivariate::distributions::p{family}(std::nextafter(h,-INFINITY),{args},{flags});
  if (!((std::isfinite(h) || {str(math.isinf(expected)).lower()} && h==INFINITY) && ({str(bool(lower)).lower()} ? at>=p && before<p : at<=p && before>p))) {{
    ++failures; std::cerr<<"{family} row {index} failed\\n";
  }}
}}''')
source = "\n".join(f'#include "cppunivariate/distributions/{f}/dpqr.hpp"' for f in families)
source += "\n#include <iostream>\nint main() { int failures=0;\n" + "\n".join(blocks)
source += f'\nstd::cout<<"{len(blocks)} targets, "<<failures<<" failures\\n"; return failures ? 1 : 0; }}\n'
cpp = scratch / "finite-truth-properties.cpp"
binary = scratch / "finite-truth-properties"
cpp.write_text(source)
boost = subprocess.check_output(["Rscript", "-e", 'cat(system.file("include", package="BH"))'], text=True)
subprocess.run([os.environ.get("CXX", "c++"), "-std=c++17", "-O1", f"-I{boost}",
                f"-I{root}/univariateML-cpp/include", str(cpp), "-o", str(binary)], check=True)
subprocess.run([str(binary)], check=True)
