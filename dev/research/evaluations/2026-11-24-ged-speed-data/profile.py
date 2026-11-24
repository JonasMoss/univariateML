"""Profile both guarded CDF workloads with the prior study's probe."""
import os
from pathlib import Path
import subprocess

import mpmath as mp
from sweep import SHAPES, POWERS, scale_for

data = Path(__file__).resolve().parent
work = Path("build/scratch/ged-speed").resolve()
previous = data.parent / "2026-11-22-pged-tail-data"


def profile(label, include):
    boost = os.environ.get("BOOST_INCLUDE") or subprocess.check_output(
        ["Rscript", "-e", 'cat(system.file("include", package="BH"))'], text=True)
    binary = work / "profile"
    subprocess.run([os.environ.get("CXX", "g++"), "-std=c++17", "-O2",
                    "-fno-inline", "-pg", "-I", boost, "-I", str(include),
                    str(previous / "probe.cpp"), "-o", str(binary)], check=True)
    groups = []
    for shape in SHAPES:
        nu = mp.mpf(shape)
        scale = scale_for(1, nu)
        groups.append([f"p {float(scale * mp.mpf(power)**(1/nu)):.17g} "
                       f"0 1 {shape:.17g} 0 0\n" for power in POWERS[:12]])
    workloads = {
        "grouped": "".join(row for group in groups for _ in range(64) for row in group),
        "changing": "".join(group[9] for _ in range(32) for group in groups),
    }
    for name, rows in workloads.items():
        subprocess.run([binary, "--time"], input=rows, text=True, cwd=work, check=True)
        flat = subprocess.check_output(["gprof", "-b", "-p", binary], cwd=work, text=True)
        graph = subprocess.check_output(
            ["gprof", "-b", "-q", "-F", "main", binary], cwd=work, text=True)
        (data / f"profile-{label}-{name}.txt").write_text(
            "\n".join(flat.splitlines()[:24]) + "\n\n" +
            "\n".join(line for line in graph.splitlines()
                      if any(key in line for key in ("three_word::", "logscale_uncached",
                                                      "physical_radius", "probability_precise"))
                      and len(line) < 240) + "\n")


if __name__ == "__main__":
    import sys
    profile(sys.argv[1], Path(sys.argv[2]).resolve())
