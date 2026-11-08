"""Independent mpmath gamma checks; run from repository root after building probe.cpp.
Probe path argument optional. No third-party packages beyond installed mpmath.
"""
import json, math, pathlib, subprocess, sys
import mpmath as mp
out = pathlib.Path(__file__).resolve().parent
points = [math.nextafter(2,math.inf),2+2**-30,2.01,2.5,3.,5.,10.,63.,64.,127.,math.nextafter(128,0),128.,math.nextafter(128,math.inf),math.nextafter(129,0),129.,math.nextafter(129,math.inf),1e4,1e8,1e12,1e16,1e100,1e300,sys.float_info.max]
# Additional reproducible logarithmic sweep of the whole binary64 shape range.
points += [2+2**(-i) for i in range(1,51)]
points += [2+10**(i/8) for i in range(-16,2465)]
points=sorted(set(points))
p = subprocess.run([sys.argv[1] if len(sys.argv)>1 else 'build/scratch/moment-probe'],input='\n'.join(map(repr,points))+'\n',text=True,capture_output=True,check=True)
print(p.stderr,end='')
rows=[]
for line in p.stdout.splitlines():
    parts=list(map(float,line.split())); n=parts[0]
    # Increase working precision so subtracting huge log-gammas still leaves
    # at least 90 correct decimal places; record 80 significant digits.
    with mp.workdps(max(100, int(math.log10(n))+100)):
        nu=mp.mpf(n)
        m=mp.gamma((nu-1)/2)/mp.gamma(nu/2)*mp.sqrt((nu-2)/mp.pi)
        lc=mp.loggamma((nu+1)/2)-mp.loggamma(nu/2)-mp.log((nu-2)*mp.pi)/2
        gm=mp.gamma(2/nu)/mp.sqrt(mp.gamma(1/nu)*mp.gamma(3/nu))
        lam=mp.sqrt(mp.power(2,-2/nu)*mp.gamma(1/nu)/mp.gamma(3/nu))
        sigma=mp.sqrt(1+mp.mpf('2.25')*(1-m*m)); mu=mp.mpf('1.5')*m
        density=mp.mpf('.8')*sigma*mp.exp(lc-(nu+1)/2*mp.log1p((mu/2)**2/(nu-2)))
        gsigma=mp.sqrt(1+mp.mpf('2.25')*(1-gm*gm)); gmu=mp.mpf('1.5')*gm
        gdensity=mp.mpf('.8')*gsigma*nu/(lam*mp.power(2,1+1/nu)*mp.gamma(1/nu))*mp.exp(-mp.mpf('.5')*mp.power(gmu/2/lam,nu))
        expected=[m,lc,gm,lam,density,gdensity,sigma,mu]
        errors=[float(abs(mp.mpf(a)-e)/math.ulp(float(e))) for a,e in zip(parts[1:],expected)]
        rows.append(dict(nu=repr(n),reference=[mp.nstr(e,80) for e in expected],computed=parts[1:],ulp_error=errors))
print('points',len(rows),'maximum ulp errors:',[max(r['ulp_error'][i] for r in rows) for i in range(8)])
assert max(r['ulp_error'][0] for r in rows)<5
# Table for checked-in closed-form tests; independent 80-digit literals.
fixed={math.nextafter(2,math.inf),2+2**-30,2.01,2.5,3.,5.,10.,63.,64.,127.,math.nextafter(128,0),128.,math.nextafter(128,math.inf),math.nextafter(129,0),129.,math.nextafter(129,math.inf),1e4,1e8,1e12,1e16,1e100,1e300,sys.float_info.max}
selected=[r for r in rows if float(r['nu']) in fixed]
(out/'regression-rows.txt').write_text('\n'.join('  {'+r['nu']+', '+', '.join(r['reference'])+'},' for r in selected)+'\n')
summary={'points':len(rows),'maximum_ulp_error':[max(r['ulp_error'][i] for r in rows) for i in range(8)],'worst':[max(rows,key=lambda r:r['ulp_error'][i]) for i in range(8)],'regressions':selected}
summary['maximum_ulp_error_nu_ge_128']=[max(r['ulp_error'][i] for r in rows if float(r['nu'])>=128) for i in range(8)]
summary['maximum_absolute_error']=[max(r['ulp_error'][i]*math.ulp(float(r['reference'][i])) for r in rows) for i in range(8)]
assert summary['maximum_ulp_error'][2]<4
assert summary['maximum_ulp_error'][3]<4
(out/('checks-no-long-double.json' if 'no-long' in (sys.argv[1] if len(sys.argv)>1 else '') else 'checks.json')).write_text(json.dumps(summary,indent=2)+'\n')
