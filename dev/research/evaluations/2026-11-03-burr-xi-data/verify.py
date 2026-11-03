"""Independent 75-digit seam solves for selected difficult workloads.

Numerical corroboration, not validated interval arithmetic. Run under one slot.
"""
import csv
import json
from pathlib import Path
import sys

import mpmath as mp
import numpy as np

import study

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent/'2026-11-03-burr-closure-data'))
from check import weibull, conditional


def main():
    mp.mp.dps = 75
    samples = json.loads((HERE/'samples.json').read_text())
    diagnostics = {r['sample']:r for r in csv.DictReader((HERE/'diagnostics.csv').open())}
    names = ['golden','burr-k1000-r0','weibull-r0','weibull-quantiles',
             'nearly-constant','closure-negative_slope_finite','closure-zero-left']
    rows = []
    for name in names:
        x = np.array(samples[name])
        yn = (np.log(x)-np.mean(np.log(x)))/np.std(np.log(x))
        # Treat the binary64 normalized logs as exact input to this check.
        y = [mp.mpf(float(t)) for t in yn]
        n = len(y)
        w,A,C,c,eta = weibull(y)
        numerical = study.seam_fit(yn,0.)
        assert abs(2*C-mp.mpf(float(numerical['curvature']))) < mp.mpf('1e-10')*n
        row = dict(sample=name,A=str(A),C=str(C),h='0',minimum_tail_slack='',
                   maximum_displacement_ratio='',relative_B_difference='')
        h = mp.mpf(diagnostics[name]['signed_h'])
        if h:
            R = mp.mpf(diagnostics[name]['signed_radius'])
            v = [mp.exp(c*t+eta) for t in y]
            norms = [mp.sqrt(t*t+1) for t in y]
            lower = [t*mp.exp(-R*r) for t,r in zip(v,norms)]
            upper = [t*mp.exp(R*r) for t,r in zip(v,norms)]
            weights = [a/(1+h*b)**2 for a,b in zip(lower,upper)]
            matrix = mp.matrix([[n/(c+R)**2+sum(a*t*t for a,t in zip(weights,y)),
                                 sum(a*t for a,t in zip(weights,y))],
                                [sum(a*t for a,t in zip(weights,y)),sum(weights)]])
            mu = (matrix[0,0]+matrix[1,1]-mp.sqrt((matrix[0,0]-matrix[1,1])**2+4*matrix[0,1]**2))/2
            d = mp.matrix([sum(t*a*(a-1) for t,a in zip(y,v)),sum(a*(a-1) for a in v)])
            E = sum(a*a*abs(a-1)*r for a,r in zip(v,norms))
            D = mp.norm(d)+h*E
            B0 = sum(min(a,1)**2/2-min(a,1)**3/3 for a in v)
            B0 += sum((a*a/2-a**3/3-mp.mpf(1)/6)/(1+h*a)**3 for a in v if a>1)
            B = B0+(mp.sqrt((d.T*mp.lu_solve(matrix,d))[0])+h*E/mp.sqrt(mu))**2/2
            assert A<0 and h*D<mu*R and h*B<=-A/2
            slacks, ratios = [], []
            for f in [mp.mpf('.125'),mp.mpf('.5'),mp.mpf(1)]:
                xi = h*f
                value, cc, aa, residual = conditional(y,1/xi,(w,A,C,c,eta))
                fitted_eta = aa+mp.log(1/xi)
                slack = A*xi+B*xi*xi-(value-w)
                ratio = mp.sqrt((cc-c)**2+(fitted_eta-eta)**2)/R
                assert slack > -mp.mpf('1e-50') and ratio < 1
                slacks.append(slack)
                ratios.append(ratio)
            row.update(h=str(h),minimum_tail_slack=str(min(slacks)),
                       maximum_displacement_ratio=str(max(ratios)),
                       relative_B_difference=str(abs(B-mp.mpf(diagnostics[name]['signed_B']))/max(1,abs(B))))
        rows.append(row)
        print(name,'passed',flush=True)
    with (HERE/'high-precision.csv').open('w') as f:
        writer=csv.DictWriter(f,fieldnames=list(rows[0])); writer.writeheader(); writer.writerows(rows)
    print('All seven independent workload checks passed at 75 digits.')


if __name__ == '__main__':
    main()
