"""Check gamma-shape tail and inverse across very large signed shapes."""
import math
import mpmath as mp
import subprocess
mp.mp.dps=100
worst=[0,0]
for row in subprocess.check_output(['build/scratch/gg-small-gamma'],text=True).splitlines():
    q,lx,lp,quant=map(float,row.split())
    # The C++ coordinate inputs are rounded binary64; reconstruct them exactly.
    mu=float(-2*math.log(q)); x=float(math.exp(lx))
    a=1/mp.mpf(q)**2
    logarg=mp.log(a)+mp.mpf(q)*(mp.log(mp.mpf(x))-mp.mpf(mu))/mp.mpf(q)
    arg=mp.exp(logarg)
    ref=mp.log(mp.gammainc(a,arg,mp.inf)/mp.gamma(a))
    err=abs(mp.mpf(lp)-ref)/max(1,abs(ref))
    qe=abs(quant/x-1)
    worst[0]=max(worst[0],float(err));worst[1]=max(worst[1],qe)
    if not math.isfinite(lp) or err>mp.mpf('1e-10') or not math.isfinite(quant) or qe>1e-8:
        raise SystemExit(f'mismatch q={q:g}, lx={lx:g}: log p={lp}, ref={ref}, q error={qe}')
print(f'48 small-gamma cases; worst scaled log-tail error {worst[0]:.3g}, relative quantile error {worst[1]:.3g}')
