"""Instrument a research-only copy of the final held certificate; never edit it."""
from pathlib import Path
import hashlib
folder=Path(__file__).resolve().parent
header=Path('univariateML-cpp/include/cppunivariate/distributions/gev/held_certificate.hpp')
s=header.read_text()
s=s[s.index('inline GevResult gev_held_certificate('):]
s=s.replace('gev_held_certificate(', 'diagnostic_certificate(',1)
s=s.replace('for(double d:data)if(!gev_public_contains(d,fallback.mu,fallback.sigma,fallback.xi))return fallback;',
            'for(double d:data)if(!gev_public_contains(d,fallback.mu,fallback.sigma,fallback.xi))return diagnostic_early(fallback,"normalized_support");')
s=s.replace('return fallback;', 'return diagnostic_early(fallback,"early_"+std::to_string(__LINE__));')
s=s.replace('  const auto result=gev_held_detail::cover(', '''  int precision_calls=0;bool precise_seen=false;gev_held_detail::Box last{};
  auto audit_precise=[&](gev_held_detail::Box b,double t) {
    ++precision_calls;last=b;const bool ok=precise(b,t);precise_seen=precise_seen||ok;return ok;
  };
  const auto result=gev_held_detail::cover(''')
s=s.replace('maxit,eval,precise,!hm,', 'maxit,eval,audit_precise,!hm,')
s=s.replace('  if(result.contradicted) {', '''  const bool local=std::isfinite(face_value)||precise_seen;
  const std::string phase=result.certified?"complete":(result.contradicted?"counterexample":
    (result.evaluations>=certificate_budget(maxit)?(local?"global_budget":"local_budget"):
     (local?"global_stagnation":(precision_calls?"coefficient_inclusion":"local_inclusion"))));
  const auto stuck=phase=="global_stagnation"?eval(result.root):gev_held_detail::Evaluation{};
  std::cout<<diagnostic_id<<','<<diagnostic_hold<<','<<phase<<','<<result.evaluations<<','
    <<std::isfinite(face_value)<<','<<precision_calls<<','<<last.x.hi-last.x.lo<<','<<last.y.hi-last.y.lo<<','<<result.root.x.lo<<','<<result.root.x.hi<<','<<result.root.y.lo<<','<<result.root.y.hi<<','<<stuck.ok<<','<<stuck.upper<<','<<face_value<<'\\n';
  diagnostic_printed=true;
  if(result.contradicted) {''')
prefix='''#include "cppunivariate/special/special.hpp"
#include "cppunivariate/distributions/gev/mle.hpp"
#include "cppunivariate/distributions/gev/dpqr.hpp"
#include <iomanip>
#include <iostream>
#include <random>
#include <string>
namespace cppunivariate {
int diagnostic_id=0;std::string diagnostic_hold;bool diagnostic_printed=false;
GevResult diagnostic_early(GevResult r,std::string phase) {
  if(!diagnostic_printed)std::cout<<diagnostic_id<<','<<diagnostic_hold<<','<<phase<<",0,0,0,0,0,0,0,0,0,0,nan,nan\\n";
  diagnostic_printed=true;return r;
}
'''
driver='''
} // namespace cppunivariate
int main(int argc,char** argv) {
  const int maxit=argc>1?std::stoi(argv[1]):100;
  std::mt19937_64 rng(17020261110ULL);std::uniform_real_distribution<double> uniform(0,1);int id=0;
  std::cout<<"sample_id,hold,phase,evaluations,face_isolated,precision_calls,shape_width,nuisance_width,remaining_xlo,remaining_xhi,remaining_ylo,remaining_yhi,remaining_ok,remaining_upper,face_value\\n"<<std::setprecision(17);
  for(double shape:{-.9,-.5,0.,.5,1.,2.})for(int n:{3,10,50,100,500})for(int rep=0;rep<2;++rep) {
    const int sample=id++;std::vector<double> x;
    for(int j=0;j<n;++j)x.push_back(cppunivariate::distributions::qgev(uniform(rng),0,1,shape,true,false));
    for(bool hs:{false,true}) {
      const std::vector<int> selected=hs?std::vector<int>{4,5,8,9,76,78,88,90,92,94,96,98}:
        std::vector<int>{4,8,9,78,88,92,96,98};
      if(std::find(selected.begin(),selected.end(),sample)==selected.end())continue;
      auto normalized=x;std::sort(normalized.begin(),normalized.end());
      auto& sample_data=normalized;
      const double origin=sample_data.front(),unit=hs?1:std::max(sample_data.back()-origin,std::abs(origin));
      for(double& v:sample_data)v=(v-origin)/unit;
      const cppunivariate::span z{sample_data.data(),sample_data.size()};const double mu=hs?NAN:-origin/unit,sigma=hs?1:NAN;
      auto objective=[&](double xi){return cppunivariate::gev_held_profile(z,mu,sigma,xi,1e-10,maxit).logLik;};
      const auto opt=cppunivariate::extreme_detail::maximize(objective,-1,1,1e-10,maxit);
      auto fit=cppunivariate::gev_held_profile(z,mu,sigma,opt.x,1e-10,maxit);
      if(!opt.ok)fit.status=cppunivariate::GevStatus::IterationFailure;
      cppunivariate::diagnostic_id=sample;cppunivariate::diagnostic_hold=hs?"sigma":"mu";
      cppunivariate::diagnostic_printed=false;
      cppunivariate::diagnostic_certificate(z,mu,sigma,1e-10,maxit,fit,origin,unit);
    }
    if(shape>=0) {++id;for(int j=0;j<n;++j)uniform(rng);}
  }
}
'''
(folder/'phase-check.cpp').write_text('// Source SHA256: '+hashlib.sha256(header.read_bytes()).hexdigest()+'\n'+prefix+s+driver)
