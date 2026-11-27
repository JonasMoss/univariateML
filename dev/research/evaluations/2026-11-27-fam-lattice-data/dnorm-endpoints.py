"""Compare one narrow cell against a 100-decimal reference.

Build each dnorm probe with dev/validation/fit-snapshot.py --family dnorm.
Pass the resulting build/dnorm executables as command-line arguments.
"""
import json
from math import exp
from pathlib import Path
import subprocess
import sys

import mpmath as mp


def compare(probe):
    # Binary-exact endpoints a and b for cell zero at this fixed pair.
    a, scale = 32, 2**20
    b = a + 1 / scale
    run = subprocess.run([probe], input=f"3 1 {-a * scale} {scale} 0\n",
                         capture_output=True, text=True, check=True)
    fitted = json.loads(run.stdout)
    log_mass = float.fromhex(fitted["logLik"])
    reference_mass = (mp.erfc(a / mp.sqrt(2)) - mp.erfc(b / mp.sqrt(2))) / 2
    reference_log = mp.log(reference_mass)

    # Reconstruct the same double endpoint-ratio subtraction as the
    # ordinary fixed-scale score in dnorm's evaluator.
    log_root_two_pi = 0.91893853320467274178
    score = exp(-0.5 * a * a - log_root_two_pi - log_mass) - exp(
        -0.5 * b * b - log_root_two_pi - log_mass
    )
    reference_score = (mp.exp(-mp.mpf(a)**2 / 2) - mp.exp(-mp.mpf(b)**2 / 2)) / (
        mp.sqrt(2 * mp.pi) * reference_mass
    )
    print(Path(probe).parents[1].name)
    print("  log mass", mp.nstr(reference_log, 35))
    print("  relative mass error", mp.nstr(mp.expm1(log_mass - reference_log), 12))
    print("  relative score error", mp.nstr(score / reference_score - 1, 12))


if __name__ == "__main__":
    mp.mp.dps = 100
    for executable in sys.argv[1:]:
        compare(executable)
