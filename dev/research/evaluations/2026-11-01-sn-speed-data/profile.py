"""Generate temporary counter-instrumented copies; never modify production headers."""
from pathlib import Path
root=Path.cwd()
out=root/'build/scratch'
data=root/'dev/research/evaluations/2026-11-01-sn-speed-data'
for name,path in [('old',data/'baseline.hpp'),('new',root/'univariateML-cpp/include/cppunivariate/distributions/sn/mle.hpp')]:
 s=('#include <iostream>\n'+path.read_text()).replace('#include "dpqr.hpp"','#include "cppunivariate/distributions/sn/dpqr.hpp"').replace('#include "../../core/span.hpp"','#include "cppunivariate/core/span.hpp"')
 ns='sn_baseline_detail' if name=='old' else 'sn_detail'
 decl='''struct Counters { long inner=0, iterations=0, objectives=0, phi=0, tails=0, score_mills=0, face_observations=0; };
inline Counters counts;
inline double counted_phi(double t) { ++counts.phi; if(t < -10)++counts.tails; return cppunivariate::sn_detail::log_phi_cdf(t); }
inline double counted_density(double z,double a) { return std::log(2.0)-0.5*std::log(2*M_PI)-0.5*z*z+counted_phi(a*z); }
inline double counted_mills(double t) { ++counts.score_mills; return cppunivariate::sn_detail::inverse_mills(t); }
'''
 # Replace calls before injecting wrappers so wrappers keep calling originals.
 s=s.replace('cppunivariate::sn_detail::log_phi_cdf(', 'counted_phi(').replace('cppunivariate::sn_detail::log_density(', 'counted_density(').replace('cppunivariate::sn_detail::inverse_mills(', 'counted_mills(')
 if name=='new':
  s=s.replace('log_phi_cdf(', 'counted_phi(').replace('log_density(', 'counted_density(').replace('inverse_mills(', 'counted_mills(')
  s=s.replace('if(t < -10) {\n        double f=0,df=0;', 'if(t < -10) {\n        ++counts.phi; ++counts.tails;\n        double f=0,df=0;')
 else:
  # The gradient's tail recurrence is a second special-function evaluation.
  s=s.replace('if(t < -10) {','if(t < -10) {\n        ++counts.tails;')
 # Qualify external calls to wrappers, which live in the detail namespace.
 s=s.replace(ns+'::counted_',ns+'::counted_')
 s=s.replace('sum+=d*counted_mills(', 'sum+=d*'+ns+'::counted_mills(').replace('ll+=counted_density(eta*', 'll+='+ns+'::counted_density(eta*')
 decl+='inline int profile_mask=0;\n'
 decl+='struct InnerTrace { double a; Counters before=counts; ~InnerTrace() { std::cerr << "'+name+'" << "," << profile_mask << "," << a << "," << counts.iterations-before.iterations << "," << counts.objectives-before.objectives << "," << counts.phi-before.phi << "," << counts.tails-before.tails << \"\\n\"; } };\n'
 s=s.replace('namespace '+ns+' {','namespace '+ns+' {\n'+decl,1)
 s=s.replace('  const bool fx=std::isfinite(xi),fw=std::isfinite(omega);\n  double eta=', '  ++counts.inner; InnerTrace trace{a};\n  const bool fx=std::isfinite(xi),fw=std::isfinite(omega);\n  double eta=')
 s=s.replace('    if (!(e>0))', '    ++counts.objectives;\n    if (!(e>0))')
 s=s.replace('for(int it=0;it<maxit;++it) {','for(int it=0;it<maxit;++it) {\n    ++counts.iterations;')
 s=s.replace('for(double v:x) { if(sign*(v-c)<0)', 'for(double v:x) { ++'+ns+'::counts.face_observations; if(sign*(v-c)<0)')
 (out/(name+'-profile.hpp')).write_text(s)
