// Source SHA256: 8ed463917a404ac5bd63c915090b7869746ccadf29bb805b591a43fbfc78231b
#include "cppunivariate/special/special.hpp"
#include "cppunivariate/distributions/gev/mle.hpp"
#include "cppunivariate/distributions/gev/dpqr.hpp"
#include <iomanip>
#include <iostream>
#include <random>
#include <string>
namespace cppunivariate {
int diagnostic_id=0;std::string diagnostic_hold;bool diagnostic_printed=false;
GevResult diagnostic_early(GevResult r,std::string phase) {
  if(!diagnostic_printed)std::cout<<diagnostic_id<<','<<diagnostic_hold<<','<<phase<<",0,0,0,0,0,0,0,0,0,0,nan,nan\n";
  diagnostic_printed=true;return r;
}
inline GevResult diagnostic_certificate(span data,double mu,double sigma,double tol,int maxit,GevResult fallback,
                                      double public_origin,double public_unit) {
  if(fallback.status!=GevStatus::Ok||!std::isfinite(fallback.logLik)||
     tol<1024*std::numeric_limits<double>::epsilon()||!(public_unit>0)||
     !std::isfinite(public_unit)||!std::isfinite(public_origin))return diagnostic_early(fallback,"early_"+std::to_string(__LINE__));
  for(double d:data)if(!gev_public_contains(d,fallback.mu,fallback.sigma,fallback.xi))return diagnostic_early(fallback,"normalized_support");
  const bool hm=!std::isnan(mu);const double n=static_cast<double>(data.size()),level=fallback.logLik/n;
  gev_held_detail::Box domain{{-1,1},{0,0}};
  if(hm) {
    double logs=0;std::size_t nonzero=0;
    for(double d:data)if(d!=mu) {logs+=std::log(std::abs(d-mu));++nonzero;}
    if(2*nonzero>data.size())domain.y={(fallback.logLik+2*logs)/(2*nonzero-n),-level};
    else {
      // With data on both sides of the hold, support forces |xi|=O(sigma).
      // A negative residual then has -L>=C/sigma, uniformly in shape.
      const double below=mu-data[0],above=data[data.size()-1]-mu;
      if(!(below>0))return diagnostic_early(fallback,"early_"+std::to_string(__LINE__));
      const bool straddles=above>0;
      const double C=straddles?std::min(below,above)/(1+std::min(below,above)/std::max(below,above)):below;
      double t=straddles?std::max(4.,std::sqrt(2*n)):std::max(8.,2*n);bool compact=false;
      for(int it=0;it<maxit;++it,t*=2) {
        const double bound=n*(std::log(t)-std::log(C))-(straddles?t*t/4:t/2);
        if(bound<fallback.logLik-1e-8*(n+std::abs(fallback.logLik))) {compact=true;break;}
        if(!std::isfinite(t))break;
      }
      if(!compact)return diagnostic_early(fallback,"early_"+std::to_string(__LINE__));
      domain.y={std::log(C)-std::log(t),-level};
    }
  } else {
    const double radius=std::sqrt(sigma*std::exp(-level));
    domain.y={data[0]-radius,data[data.size()-1]+radius};
  }
  if(!gev_joint_detail::finite(domain.y)||!(domain.y.lo<domain.y.hi))return diagnostic_early(fallback,"early_"+std::to_string(__LINE__));
  // Strictly outside the level set; these boundaries cannot be maximizers.
  domain.y=gev_joint_detail::pad(domain.y,1e-8*(1+gev_joint_detail::magnitude(domain.y)));
  auto eval=[&](gev_held_detail::Box b) {return gev_held_detail::gev_evaluate(data,b,hm,hm?mu:sigma,maxit);};
  auto precise=[&](gev_held_detail::Box b,double t) {
    const double x=(b.x.lo+b.x.hi)/2,y=(b.y.lo+b.y.hi)/2;
    return b.x.hi-b.x.lo<=t*.25*std::abs(x)&&
      (hm ? std::expm1(b.y.hi-b.y.lo)<=t*.25 :
        public_unit*(b.y.hi-b.y.lo)<=t*.25*std::max(std::abs(public_origin+public_unit*y),public_unit*sigma));
  };
  gev_held_detail::Box face_basin{};double face_value=NAN;
  // At the active reverse-exponential endpoint use a one-sided value
  // envelope. No derivative is evaluated at a zero support residual.
  if(fallback.xi== -1) {
    const double M=data[data.size()-1],s0=hm?M-mu:sigma;
    if(s0>0 && (hm?fallback.sigma==s0:fallback.mu==M-sigma)) {
      double S=0,L=0,H=0,k=0;
      for(double v:data) {
        const double d=(M-v)/s0;S+=d;
        if(d==0)++k;else {L+=std::log(d);H+=1/d;}
      }
      const double A=2*n-S;
      double c=1;
      for(int it=0;it<maxit;++it,c/=2) {
        const double a=1/(1+c);
        if(!(a<1))break;
        const double rate=hm?A/4:a*n/2;
        const double C=S+n/std::exp(1.)+L+k*(std::log(c*k/rate)-1)+(hm?n-S:0);
        if(rate>0 && c*H<(hm?A/4:a*n/2) && C< -1e-8*(n+std::abs(L)+S)) {
          face_basin={{-1,-a},hm?gev_joint_detail::Range{std::max(domain.y.lo,std::log(a*s0)),std::min(domain.y.hi,std::log(2*a*s0))}:domain.y};
          // The held-mu face has sigma>=s0; extend below it because
          // all such points in this corner are outside support only at -1.
          // For xi>-1, sigma can be a*s0: use that moving lower boundary
          // via the independent face exclusion in the evaluator below.
          face_value=fallback.logLik/n;break;
        }
      }
    }
  }
  double minimum_count=0;for(double v:data)if(v==data[0])++minimum_count;
  const double location_high=data[0]+sigma*std::log(2*n/minimum_count);
  const double location_margin=128*std::numeric_limits<double>::epsilon()*(1+std::abs(location_high)+sigma);
  const gev_joint_detail::Range location_roots{data[0],location_high+location_margin};
  int precision_calls=0;bool precise_seen=false;gev_held_detail::Box last{};
  auto audit_precise=[&](gev_held_detail::Box b,double t) {
    ++precision_calls;last=b;const bool ok=precise(b,t);precise_seen=precise_seen||ok;return ok;
  };
  const auto result=gev_held_detail::cover(domain,fallback.xi,hm?std::log(fallback.sigma):fallback.mu,std::min(tol,.01),maxit,eval,audit_precise,!hm,face_basin,face_value,NAN,location_roots,sigma);
  const bool local=std::isfinite(face_value)||precise_seen;
  const std::string phase=result.certified?"complete":(result.contradicted?"counterexample":
    (result.evaluations>=certificate_budget(maxit)?(local?"global_budget":"local_budget"):
     (local?"global_stagnation":(precision_calls?"coefficient_inclusion":"local_inclusion"))));
  const auto stuck=phase=="global_stagnation"?eval(result.root):gev_held_detail::Evaluation{};
  std::cout<<diagnostic_id<<','<<diagnostic_hold<<','<<phase<<','<<result.evaluations<<','
    <<std::isfinite(face_value)<<','<<precision_calls<<','<<last.x.hi-last.x.lo<<','<<last.y.hi-last.y.lo<<','<<result.root.x.lo<<','<<result.root.x.hi<<','<<result.root.y.lo<<','<<result.root.y.hi<<','<<stuck.ok<<','<<stuck.upper<<','<<face_value<<'\n';
  diagnostic_printed=true;
  if(result.contradicted) {
    fallback.xi=result.competitor.x.lo;
    if(hm)fallback.sigma=std::exp(result.competitor.y.lo);
    else fallback.mu=result.competitor.y.lo;
    fallback.logLik=gev_loglik(data,fallback.mu,fallback.sigma,fallback.xi);
    fallback.status=GevStatus::SearchContradicted;return diagnostic_early(fallback,"early_"+std::to_string(__LINE__));
  }
  if(!result.certified)return diagnostic_early(fallback,"early_"+std::to_string(__LINE__));
  fallback.xi=(result.root.x.lo+result.root.x.hi)/2;
  if(hm)fallback.sigma=std::exp((result.root.y.lo+result.root.y.hi)/2);
  else fallback.mu=(result.root.y.lo+result.root.y.hi)/2;
  fallback.logLik=gev_loglik(data,fallback.mu,fallback.sigma,fallback.xi);
  fallback.certified=true;return diagnostic_early(fallback,"early_"+std::to_string(__LINE__));
}

} // namespace cppunivariate
int main() {
  std::mt19937_64 rng(17020261110ULL);std::uniform_real_distribution<double> uniform(0,1);int id=0;
  std::cout<<"sample_id,hold,phase,evaluations,face_isolated,precision_calls,shape_width,nuisance_width,remaining_xlo,remaining_xhi,remaining_ylo,remaining_yhi,remaining_ok,remaining_upper,face_value\n"<<std::setprecision(17);
  for(double shape:{-.9,-.5,0.,.5,1.,2.})for(int n:{3,10,50,100,500})for(int rep=0;rep<2;++rep) {
    const int sample=id++;std::vector<double> x;
    for(int j=0;j<n;++j)x.push_back(cppunivariate::distributions::qgev(uniform(rng),0,1,shape,true,false));
    if(sample==8||sample==10||sample==20||sample==60||sample==80||sample==96) {
      const bool hs=sample==8;std::sort(x.begin(),x.end());
      const double origin=x.front(),unit=hs?1:std::max(x.back()-origin,std::abs(origin));
      for(double& v:x)v=(v-origin)/unit;
      const cppunivariate::span z{x.data(),x.size()};const double mu=hs?NAN:-origin/unit,sigma=hs?1:NAN;
      auto objective=[&](double xi){return cppunivariate::gev_held_profile(z,mu,sigma,xi,1e-10,100).logLik;};
      const auto opt=cppunivariate::extreme_detail::maximize(objective,-1,1,1e-10,100);
      auto fit=cppunivariate::gev_held_profile(z,mu,sigma,opt.x,1e-10,100);
      if(!opt.ok)fit.status=cppunivariate::GevStatus::IterationFailure;
      cppunivariate::diagnostic_id=sample;cppunivariate::diagnostic_hold=hs?"sigma":"mu";
      cppunivariate::diagnostic_printed=false;
      cppunivariate::diagnostic_certificate(z,mu,sigma,1e-10,100,fit,origin,unit);
    }
    if(shape>=0) {++id;for(int j=0;j<n;++j)uniform(rng);}
  }
}
