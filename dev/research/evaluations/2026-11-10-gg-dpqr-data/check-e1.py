"""Compare the limiting-gamma E1 helper with mpmath at 60 digits."""
import math
import mpmath as mp
import subprocess
mp.mp.dps=60
rows=subprocess.check_output(['build/scratch/gg-e1'],text=True).splitlines()
worst=0
for row in rows:
    lx, got=map(float,row.split())
    ref=float(mp.log(mp.e1(mp.exp(mp.mpf(lx)))))
    if not math.isfinite(got): raise SystemExit(f'nonfinite at {lx}: {got}')
    err=abs(got-ref)/max(1,abs(ref))
    worst=max(worst,err)
    if err>1e-12: raise SystemExit(f'mismatch at {lx}: {got} vs {ref}')
print(f'{len(rows)} E1 points; worst scaled log error {worst:.3g}')
