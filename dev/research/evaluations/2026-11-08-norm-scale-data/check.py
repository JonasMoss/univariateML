#!/usr/bin/env python3
"""Independent Decimal(100) oracle on exact binary64 inputs; run under a CPU slot.
Rebuilds the normal probe in native and forced-double modes. Writes only its
small JSON result beside this script and compiler products under build/scratch.
"""
from decimal import Decimal, localcontext
import json
from pathlib import Path
import subprocess

ROOT = Path(__file__).resolve().parents[4]
OUT = Path(__file__).resolve().parent
SCRATCH = ROOT / 'build/scratch/norm-scale-oracle'
SCRATCH.mkdir(parents=True, exist_ok=True)
SOURCE = r'''
#include <cppunivariate/distributions/norm/mle.hpp>
#include <iomanip>
#include <iostream>
#include <vector>
int main() {
  std::cout << std::setprecision(17);
  for(double unit:{1e-200,1e200}) for(int mask=0;mask<4;++mask) {
    const std::vector<double> x={unit,2*unit};
    const auto r=cppunivariate::norm_mle({x.data(),x.size()},mask&1?0:NAN,mask&2?unit:NAN);
    std::cout << unit << ' ' << mask << ' ' << static_cast<int>(r.status) << ' '
              << r.mean << ' ' << r.sd << ' ' << r.logLik << '\n';
  }
}
'''
source = SCRATCH / 'probe.cpp'
source.write_text(SOURCE)
records = []
with localcontext() as ctx:
    ctx.prec = 100
    # 100-digit pi, independent of the C++ constants header.
    pi = Decimal('3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679')
    for mode, flags in [('extended', []), ('double', ['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'])]:
        exe = SCRATCH / mode
        subprocess.run(['c++', '-std=c++17', '-O2', *flags,
                        '-I'+str(ROOT/'univariateML-cpp/include'),str(source),'-o',str(exe)],check=True)
        for line in subprocess.check_output([str(exe)], text=True).splitlines():
            u, mask, status, mean, sd, ll = line.split()
            unit = Decimal.from_float(float(u)); mask=int(mask)
            data = [unit, Decimal.from_float(2*float(u))]
            m = Decimal(0) if mask&1 else sum(data)/2
            ss = sum((v-m)**2 for v in data)
            s = unit if mask&2 else (ss/2).sqrt()
            expected_ll = -2*(s.ln()+(2*pi).ln()/2)-ss/(2*s*s)
            mean_error = abs(Decimal.from_float(float(mean))-m)/unit
            scale_error = abs(Decimal.from_float(float(sd))/s-1)
            ll_error = abs(Decimal.from_float(float(ll))-expected_ll)
            assert status=='0' and mean_error<Decimal('1e-13') and scale_error<Decimal('1e-13') and ll_error<Decimal('1e-10'), line
            records.append(dict(mode=mode,unit=u,mask=mask,expected_mean=str(m),expected_sd=str(s),expected_logLik=str(expected_ll),
                                mean_error_in_units=str(mean_error),relative_sd_error=str(scale_error),absolute_logLik_error=str(ll_error)))
(OUT/'normal-oracle.json').write_text(json.dumps(records,indent=2)+'\n')
print(f'{len(records)} independent 100-digit comparisons passed')
