"""100-digit maximum of the logarithm's Jensen gap, independent bisection.
Run under one CPU slot. No production Mills or profile evaluator is used.
"""
from pathlib import Path
import subprocess,csv,json,math,sys
import mpmath as m
m.mp.dps=100
p=Path(__file__).resolve().parent;scratch=Path('build/scratch/emg-perf');summary=[]
for mode in ['native','double']:
    flags=[] if mode=='native' else ['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS']
    binary=scratch/f'gap-{mode}'
    subprocess.run(['c++','-std=c++17','-O2',*flags,'-IunivariateML-cpp/include',str(p/'gap-check.cpp'),'-o',str(binary)],check=True)
    path=p/f'gap-{mode}.csv'
    with path.open('w') as out:subprocess.run([str(binary)],stdout=out,check=True)
    checked=unavailable=0;surplus=[]
    for row in csv.DictReader(path.open()):
        lo,hi=m.mpf(float(row['lo'])),m.mpf(float(row['hi']));n=int(row['n']);upper=float(row['upper'])
        assert not math.isnan(upper),row
        if math.isinf(upper):
            assert mode=='double' and hi/lo>sys.float_info.max,row
            unavailable+=1;continue
        r=(hi/lo)**2;logr=m.log(r)
        if r==1:gap=m.mpf(0)
        else:
            left,right=m.mpf(0),m.mpf(1)
            for _ in range(340):
                w=(left+right)/2
                derivative=(r-1)/(1+w*(r-1))-logr
                if derivative>0:left=w
                else:right=w
            w=(left+right)/2
            gap=m.log1p(w*(r-1))-w*logr
        reference=-10+n*gap/2
        error=m.mpf(upper)-reference
        assert error>=0,(mode,row,str(reference),str(error))
        surplus.append(error);checked+=1
    summary.append(dict(mode=mode,precision_digits=m.mp.dps,finite_bounds_checked=checked,safe_unavailable_bounds=unavailable,minimum_upper_surplus=str(min(surplus))))
(p/'gap-validation.json').write_text(json.dumps(summary,indent=2)+'\n')
print(json.dumps(summary,indent=2))
