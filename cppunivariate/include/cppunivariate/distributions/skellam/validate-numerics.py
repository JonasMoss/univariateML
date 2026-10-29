"""Run from the repository root under /tmp/umvl-cpu/run; no installs needed.

Independent mpmath checks. Temporary C++ source and binary stay under /tmp.
"""
from pathlib import Path
import subprocess
import tempfile
import mpmath as mp

root = Path(__file__).resolve().parents[5]
mp.mp.dps = 180
cases = [
    (0, 1e-200, 1e-200), (1, 1e-200, 1e-200),
    (1000, 1e-100, 10), (0, 1000, 1000), (0, 5000, 5000),
    (1, 5001, 5000), (0, 1e6, 1e6), (1000, 1e6, 1e6),
    (1000000, 1e-6, 1e-6), (0, 1e12, 1e12),
    (1000000, 1e12, 1e12), (0, 1e100, 1e100),
    (1, 1e100, 1e100), (1e50, 1e100, 1e100),
    (10000, 1e-100, 1e100), (10000, 3, 5),
    (10000, 1e4, 1), (9999, 3, 5),
]
source = r'''
#include "cppunivariate/distributions/skellam/dpqr.hpp"
#include <iostream>
#include <iomanip>
int main() {
  double k,a,b;
  while(std::cin>>k>>a>>b)
    std::cout<<std::setprecision(17)
      <<cppunivariate::distributions::dskellam(k,a,b,true)<<"\n";
}
'''
with tempfile.TemporaryDirectory(prefix='skellam-', dir='/tmp') as work:
    work = Path(work)
    (work/'probe.cpp').write_text(source)
    bh = subprocess.check_output(['Rscript', '-e',
        'cat(system.file("include",package="BH"))'], text=True)
    subprocess.run(['c++', '-std=c++17', '-O2', '-I'+bh,
        '-I'+str(root/'cppunivariate/include'), str(work/'probe.cpp'),
        '-o', str(work/'probe')], check=True)
    values = subprocess.check_output([str(work/'probe')],
        input=''.join(f'{k} {a} {b}\n' for k,a,b in cases), text=True).splitlines()
    for (k,a,b), actual in zip(cases, values):
        aa,bb,n = mp.mpf(a),mp.mpf(b),mp.mpf(k)
        reference = -aa-bb+n/2*mp.log(aa/bb)+mp.log(mp.besseli(abs(n),2*mp.sqrt(aa*bb)))
        error = abs(mp.mpf(actual)-reference)
        print(k,a,b,actual,'absolute error',float(error))
        assert error < mp.mpf('2e-10')*max(1,abs(reference))
print(f'{len(cases)}/{len(cases)} independent mpmath log-mass checks passed')
