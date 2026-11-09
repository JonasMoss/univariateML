"""Create a diagnostic-only copy; production headers never print or log.
Optional --old-padding restores the deliberately overlarge residual allowance.
Compile debug.cpp with -Ibuild/scratch after running this script.
"""
from pathlib import Path
import sys
s=Path('univariateML-cpp/include/cppunivariate/distributions/sged/held_moment_certificate.hpp').read_text()
s=s.replace('namespace sged_held_moment {','namespace sged_held_moment_debug {')
if '--old-padding' in sys.argv:
    start=s.index('      const double qpad=')
    end=s.index('      double lower=0;',start)
    s=s[:start]+'      q=sged_profile::padded(q.lo,q.hi,n,1+p+1/p);\n'+s[end:]
s=s.replace('const auto stop=[&](bool budget) {','const auto stop=[&](int line,bool budget) {\n    std::cerr << "stop line "<<line<<" work "<<work<<" xi "<<best.xi<<"\\n";')
s=s.replace('stop(', 'stop(__LINE__,')
s=s.replace('  if(winners.empty() && winning_faces.size()==1) {','  std::cerr << "root span "<< std::atanh(hi)-std::atanh(lo)<<" target "<<target<<" cells "<<roots.size()<<"\\n";\n  if(winners.empty() && winning_faces.size()==1) {')
Path('build/scratch/held-debug.hpp').write_text(s)
