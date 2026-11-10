#pragma once
#include "../../core/work_budget.hpp"
#include "../../core/stop_detail.hpp"
#include "../../core/face_compare.hpp"
#include "../../core/precision.hpp"
#include "../gb2/mle.hpp"
#include "../gamma/mle.hpp"
#include "../kumar/mle.hpp"
#include <array>

namespace cppunivariate {
enum class GenbetaStatus { Ok, UnitGammaBoundary, PowerBoundary, InvalidData,
  InvalidFixed, SearchUnresolved, Unbounded };
enum class GenbetaCertificateFailure { None, Arithmetic, ShapeSolve, ShapeSigns,
  Tail, RootSigns, CoefficientInclusion, Budget, GammaNeighborhood, PowerNeighborhood, LegacyContradicted };
// Bounds concern the all-free conditional beta problem, at the proposed power.
// They do not certify maximization over power.
struct GenbetaConditionalBounds {
  double shape1_lower=NAN,shape1_upper=NAN,shape2_lower=NAN,shape2_upper=NAN;
  double loglik_lower=NAN,loglik_upper=INFINITY;
  bool enclosed=false;
  double loglik_value=NAN;
};
struct GenbetaTailBounds {
  // h=shape3*mean(log(scale/x)); bounds in original total-likelihood units.
  double small_cutoff=NAN,small_upper=INFINITY,large_cutoff=NAN,large_upper=INFINITY;
  bool small_excluded=false,large_excluded=false;
  double gamma_loglik_upper=INFINITY,small_power_domain=NAN,small_power_slope=INFINITY,
    small_quadratic_domain=NAN,small_quadratic_coefficient=INFINITY;
  double ridge_shape2_lower=NAN,ridge_shape2_upper=NAN,ridge_loglik_upper=INFINITY;
};
struct GenbetaResult {
  double shape1, shape2, shape3, scale, logLik;
  int iterations;
  GenbetaStatus status;
 StopDetail stop_detail{};
 GenbetaCertificateFailure certificate_failure=GenbetaCertificateFailure::None;
 GenbetaConditionalBounds conditional_bounds{};
 GenbetaTailBounds tail_bounds{};
 bool allfree_certified=false; };
namespace genbeta_detail {
struct Point { double a=1,b=1,t=1,ll=-std::numeric_limits<double>::infinity(),score=0; bool ok=false; };
// Concave beta exponential-family problem from sufficient statistics, without
// rounding exp(-t*y) to a support endpoint. Held shapes restrict its tangent.
inline Point profile(const std::vector<double>& y,double t,double aa,double bb,int maxit) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double n=static_cast<double>(y.size());
  CPPUNIV_EXTENDED_REAL ss=0,rr=0,dd=0;
  for(double v:y) {
    const double z=t*v;
    ss+=z;
    rr-=z<.6931471805599453?std::log(-std::expm1(-z)):std::log1p(-std::exp(-z));
    dd+=z>700?0:z/std::expm1(z);
  }
  const double s=static_cast<double>(ss/n),r=static_cast<double>(rr/n);
  const bool fa=std::isnan(aa),fb=std::isnan(bb);
  if(!(s>0)||!std::isfinite(s)||!std::isfinite(r)||(fb&&!(r>0)))return {};
  double a=fa?std::max(.01,.5/s):aa,b=fb?std::max(.01,.5/r):bb;
  auto ll=[&](double av,double bv) {return -av*s-(bv-1)*r-gb2_detail::logbeta(av,bv);};
  if(a==1&&!fa) {
    if(fb)b=1/r;
    const double value=n*(std::log(t)+ll(a,b));
    return {a,b,t,value,1-a*s+(b-1)*static_cast<double>(dd/n),std::isfinite(value)&&std::isfinite(b)};
  }
  bool ok=false;
  for(int it=0;it<maxit;++it) {
    const double u=trgamma_detail::log_minus_digamma(a+b);
    const double ga=fa?a*(-s+std::log1p(b/a)+trgamma_detail::log_minus_digamma(a)-u):0;
    const double gb=fb?b*(-r+std::log1p(a/b)+trgamma_detail::log_minus_digamma(b)-u):0;
    if(std::hypot(ga,gb)<1e-9){ok=true;break;}
    const double tri=special::trigamma(a+b);
    const double h1=a*a*(special::trigamma(a)-tri),h2=b*b*(special::trigamma(b)-tri),h12=-a*b*tri;
    double da=0,db=0;
    if(fa&&fb) {
      const double det=h1*h2-h12*h12;
      if(!(det>0))break;
      da=(h2*ga-h12*gb)/det;db=(h1*gb-h12*ga)/det;
    } else if(fa)da=ga/h1;
    else if(fb)db=gb/h2;
    else {ok=true;break;}
    const double norm=std::max(std::abs(da),std::abs(db));
    if(norm>2){da*=2/norm;db*=2/norm;}
    bool accepted=false;
    double step=1;
    for(std::int64_t backtrack=0;backtrack<solver_budget(maxit) && step>1e-12;++backtrack,step*=.5) {
      const double an=a*std::exp(step*da),bn=b*std::exp(step*db);
      if(std::isfinite(an+bn)&&ll(an,bn)>=ll(a,b)-1e-13) {a=an;b=bn;accepted=true;break;}
    }
    if(!accepted)break;
  }
  const double value=n*(std::log(t)+ll(a,b));
  return {a,b,t,value,1-a*s+(b-1)*static_cast<double>(dd/n),ok&&std::isfinite(value)&&std::isfinite(a)&&std::isfinite(b)};
}
} // namespace genbeta_detail
} // namespace cppunivariate

#include "power_profile_certificate.hpp"
#include "free_p_certificate.hpp"
#include "allfree_bounds.hpp"
#include "allfree_certificate.hpp"
#include "power_face_certificate.hpp"
#include "gamma_face_certificate.hpp"

namespace cppunivariate {

inline GenbetaResult genbeta_mle(span x,double scale,double tol=1e-7,int maxit=300,
    double a=std::numeric_limits<double>::quiet_NaN(),
    double b=std::numeric_limits<double>::quiet_NaN(),
    double t=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double nan=std::numeric_limits<double>::quiet_NaN();
  GenbetaResult bad{a,b,t,scale,nan,0,GenbetaStatus::InvalidData};
  if(!(scale>0)||!std::isfinite(scale)){bad.status=GenbetaStatus::InvalidFixed;return bad;}
  for(double v:{a,b,t})if(!std::isnan(v)&&(!(v>0)||!std::isfinite(v))){bad.status=GenbetaStatus::InvalidFixed;return bad;}
  if(x.size()==0||!(tol>0)||!std::isfinite(tol)||maxit<1)return bad;
  std::vector<double> y;CPPUNIV_EXTENDED_REAL jac=0,sy=0;double ymin=std::numeric_limits<double>::infinity();
  for(double v:x) {
    if(!(v>0&&v<scale)||!std::isfinite(v))return bad;
    const double z=v>scale*.5?-std::log1p((v-scale)/scale):std::log(scale)-std::log(v);
    y.push_back(z);jac-=std::log(v);sy+=z;ymin=std::min(ymin,z);
  }
  const double n=static_cast<double>(x.size()),mean=static_cast<double>(sy/n);
  bool constant=true;for(double v:y)constant=constant&&v==y[0];
  // Free concentration can collapse on constant samples. With both beta
  // shapes held, the one-dimensional scale-in-y problem remains meaningful.
  if(constant&&std::isnan(b)&&(std::isnan(a)||std::isnan(t))) {
    bool identical=true;for(double v:x)identical=identical&&v==x[0];
    bad.status=identical?GenbetaStatus::Unbounded:GenbetaStatus::SearchUnresolved;
    if(!identical)bad.certificate_failure=GenbetaCertificateFailure::Arithmetic;
    return bad;
  }
  double face=-std::numeric_limits<double>::infinity();
  GenbetaStatus fs=GenbetaStatus::SearchUnresolved;
  double gamma_shape=nan,gamma_scale=nan;
  auto consider=[&](double ll,GenbetaStatus s){if(std::isfinite(ll)&&ll>face){face=ll;fs=s;}};
  if(std::isnan(t)&&std::isnan(a)) {
    if(std::isnan(b)) {
      const auto g=gamma_mle({y.data(),y.size()},1e-10,maxit);
      if(g.status!=GammaStatus::Ok){bad.status=GenbetaStatus::SearchUnresolved;return bad;}
      gamma_shape=g.shape;gamma_scale=g.scale;
      consider(g.logLik,GenbetaStatus::UnitGammaBoundary);
      CPPUNIV_EXTENDED_REAL total_gap=0;for(double v:y)total_gap+=v-ymin;
      const double gap=static_cast<double>(total_gap/n);
      if(gap>0)consider(n*(-std::log(gap)-1),GenbetaStatus::PowerBoundary);
    } else {
      CPPUNIV_EXTENDED_REAL sl=0;for(double v:y)sl+=std::log(v);
      const double logmean=static_cast<double>(sl/n);
      gamma_shape=b;gamma_scale=mean/b;
      const double contrast=gamma_detail::robust_s({y.data(),y.size()},mean);
      const double base=b>=100?.5*std::log(b)-.5*std::log(6.2831853071795864769)-gamma_detail::stirlerr(b):b*std::log(b)-b-special::lgamma(b);
      consider(n*(base-b*contrast-logmean),GenbetaStatus::UnitGammaBoundary);
      if(b!=1)consider(n*(-std::log(mean)-1),GenbetaStatus::PowerBoundary);
    }
  }
  // beta=1 is an attained ridge: only alpha*tau is identified.
  if(b==1) {
    const double rate=1/mean;
    if(std::isnan(a)&&std::isnan(t)){a=rate;t=1;}
    else if(std::isnan(a))a=rate/t;
    else if(std::isnan(t))t=rate/a;
    const double value=n*(std::log(a*t)-a*t*mean)+static_cast<double>(jac);
    return {a,b,t,scale,value,0,std::isfinite(value)&&std::isfinite(a)&&std::isfinite(t)?GenbetaStatus::Ok:GenbetaStatus::SearchUnresolved};
  }
  // Held p=1 is exactly Kumaraswamy, not a numerical free-power profile.
  // Keep the held-q scalar route below: it resolves much smaller q than
  // the general Kumaraswamy proposer. A free fitted p near one is not enough.
  if(a==1&&std::isnan(b)&&std::isnan(t)) {
    std::vector<double> u;u.reserve(x.size());
    for(double v:x) {
      const double value=v/scale;
      if(!(value>0&&value<1)) {
        bad.status=GenbetaStatus::SearchUnresolved;bad.certificate_failure=GenbetaCertificateFailure::Arithmetic;return bad;
      }
      u.push_back(value);
    }
    const auto k=kumar_mle({u.data(),u.size()},tol,maxit);
    const double shift=-n*std::log(scale);
    GenbetaResult out{1,k.b,k.a,scale,k.logLik+shift,k.iterations,
      k.status==KumarStatus::Ok?GenbetaStatus::Ok:GenbetaStatus::SearchUnresolved};
    out.stop_detail=k.stop_detail;
    if(k.status!=KumarStatus::Ok)out.certificate_failure=k.stop_detail.budget_exhausted?
      GenbetaCertificateFailure::Budget:GenbetaCertificateFailure::Arithmetic;
    // Parent candidates are (power,q); all likelihoods must be in X units.
    for(auto& c:out.stop_detail.candidates) {
      if(c.coef.size()!=2){out.stop_detail={};return out;}
      c.coef={1,c.coef[1],c.coef[0],scale};
      c.loglik_lower+=shift;c.loglik_upper+=shift;
    }
    out.stop_detail.loglik_lower+=shift;
    out.stop_detail.loglik_upper+=shift;
    out.stop_detail.segment_loglik_upper+=shift;
    return out;
  }
  if(std::isnan(t)&&!std::isnan(a)&&std::isnan(b))
    return genbeta_power_cert::fit(y,a,scale,mean,static_cast<double>(jac),tol,maxit);
  // Held beta shapes: strictly concave in power, including 0 < b < 1.
  // The dimensionless score is t*L'(t)/n; its sign brackets the sole root.
  if(std::isnan(t)&&!std::isnan(a)&&!std::isnan(b)) {
    const double ymax=*std::max_element(y.begin(),y.end());
    double score_error=INFINITY;
    const auto score=[&](double power) {
      const bool centered=b<1&&power*ymax<.01;
      CPPUNIV_EXTENDED_REAL correction=0,magnitude=0,omitted=0;
      for(double v:y) {
        const double z=power*v;
        if(b<1) {
          // z/expm1(z)-1, without subtracting two numbers near one.
          const double z2=z*z;
          const double remainder=z2*(1.0/12+z2*(-1.0/720+z2*(1.0/30240-z2/1209600)));
          const double term=centered ? remainder : (z<.01 ? -.5*z+remainder : (z>700 ? -1 : z/std::expm1(z)-1));
          correction+=term;magnitude+=std::abs(term);
          if(z<.01)omitted+=z2*z2*z2*z2*z2/47900160;
        } else {
          // Weight before the exponential tail underflows: a large held q
          // can make (q-1)*z/expm1(z) material even beyond z=700.
          const double term=z<=50 ? (b-1)*(z/std::expm1(z)) :
            (std::isfinite(z)?z*std::exp(std::log(b-1)-z):0);
          correction+=term/n;
          magnitude+=std::abs(term)*(1+(z>50?std::abs(std::log(b-1))+z:0));
        }
      }
      const double linear=centered?(a-.5+.5*b)*power*mean:a*power*mean;
      const double base=b<1?b:1;
      const double weight=b<1?std::abs(b-1):1;
      score_error=64*std::numeric_limits<double>::epsilon()*
        (std::abs(base)+std::abs(linear)+weight*static_cast<double>(magnitude/n))+
        weight*static_cast<double>(omitted/n);
      return base-linear+(b<1?(b-1)*static_cast<double>(correction/n):static_cast<double>(correction));
    };
    double lo=0,hi=(b<1?b:1+std::log(b))/(a*mean);
    bool bracket=std::isfinite(hi)&&hi>0;
    for(std::int64_t j=0;bracket&&score(hi)>0&&j<solver_budget(maxit);++j)hi*=2;
    bracket=bracket&&std::isfinite(hi)&&score(hi)<=0;
    bool converged=false;int iterations=0;
    for(std::int64_t j=0;bracket&&j<solver_budget(maxit);++j) {
      ++iterations;const double mid=lo+(hi-lo)/2;
      if(score(mid)>0)lo=mid;else hi=mid;
      if(lo>0&&hi-lo<=2e-13*hi){converged=true;break;}
    }
    if(!converged){
      bad.status=GenbetaStatus::SearchUnresolved;bad.iterations=iterations;
      if(bracket) {
        const auto witness=genbeta_detail::profile(y,lo+(hi-lo)/2,a,b,maxit);
        const double ll=witness.ll+static_cast<double>(jac);
        if(witness.ok&&std::isfinite(ll)) {
          bad.stop_detail.candidates={{{a,b,witness.t,scale},ll,ll,StopModel::None}};
          bad.stop_detail.loglik_lower=ll;bad.stop_detail.loglik_upper=INFINITY;
        }
      }
      return bad;
    }
    const auto p=genbeta_detail::profile(y,lo+(hi-lo)/2,a,b,maxit);
    // Bisection proposes; padded opposite signs certify coefficient inclusion.
    // Do not use a raw rounded zero as evidence of an exact root. The narrow
    // final bracket stays inside this tol-relative enclosure of the proposal.
    const double radius=std::min(tol,1e-10)*.25;
    const double left=p.t*(1-radius),right=p.t*(1+radius);
    const double sl=score(left),el=score_error;
    const double sr=score(right),er=score_error;
    const bool certified=left>0&&right>left&&std::isfinite(el)&&std::isfinite(er)&&sl>el&&sr< -er;
    GenbetaResult out{a,b,p.t,scale,p.ll+static_cast<double>(jac),iterations,
      p.ok&&certified?GenbetaStatus::Ok:GenbetaStatus::SearchUnresolved};
    if(out.status!=GenbetaStatus::Ok&&p.ok&&std::isfinite(out.logLik)) {
      out.certificate_failure=GenbetaCertificateFailure::CoefficientInclusion;
      const double pad=128*std::numeric_limits<double>::epsilon()*(1+std::abs(out.logLik));
      out.stop_detail.candidates={{{a,b,p.t,scale},out.logLik-pad,out.logLik+pad,StopModel::None}};
      out.stop_detail.loglik_lower=out.logLik-pad;out.stop_detail.loglik_upper=INFINITY;
    }
    return out;
  }
  if(std::isnan(t)&&std::isnan(a)&&!std::isnan(b))
    return genbeta_free_p_cert::fit(y,b,scale,mean,static_cast<double>(jac),tol,maxit);
  int evals=0;
  auto at=[&](double z){
    if(evals>=certificate_budget(maxit))return genbeta_detail::Point{};
    ++evals;return genbeta_detail::profile(y,std::exp(z),a,b,maxit);
  };
  genbeta_detail::Point best;
  bool edge=false;
  if(!std::isnan(t)){++evals;best=genbeta_detail::profile(y,t,a,b,maxit); }
  else {
    // Logarithmic mesh nominates every visible local maximum. Refine each
    // sign change of the analytic envelope score by bisection.
    const double lo=-12-std::log(mean);
    double hi=std::log(100/ymin);
    if(!std::isnan(a))hi=std::max(hi,std::log(100.0)-std::log(a)-std::log(mean));
    if(!std::isnan(b))hi=std::max(hi,std::log(std::max(100.0,std::log(b)+100))-std::log(ymin));
    const int count=static_cast<int>(std::min<std::int64_t>(240,certificate_budget(maxit)));
    auto prev=at(lo); if(prev.ok)best=prev;
    for(int i=1;i<=count;++i) {
      const double z=lo+(hi-lo)*i/count;
      auto cur=at(z);
      if(cur.ok&&cur.ll>best.ll){best=cur;edge=i==count;}
      if(prev.ok&&cur.ok&&prev.score>0&&cur.score<0) {
        double l=z-(hi-lo)/count,h=z;
        for(int j=0;j<maxit;++j) {
          const double m=(l+h)/2;auto p=at(m);
          if(!p.ok)break;
          if(p.ll>best.ll||(p.ll>=best.ll-1e-12*n&&std::abs(p.score)<std::abs(best.score))){best=p;edge=false;}
          if(std::abs(p.score)<std::min(tol,1e-9)||h-l<1e-11)break;
          if(p.score>0)l=m;else h=m;
        }
      }
      prev=cur;
    }
  }
  genbeta_power_cert::Sum certificate_total,certificate_jac;
  if(std::isnan(a)&&std::isnan(b)&&std::isnan(t)) {
    for(double z:y)certificate_total.add(z);
    for(double z:x)certificate_jac.add(-std::log(z));
  }
  const double certificate_mean=static_cast<double>(certificate_total.value/n),
    certificate_shift=static_cast<double>(certificate_jac.value);
  auto finish=[&](GenbetaResult out) {
    if(std::isnan(a)&&std::isnan(b)&&std::isnan(t)) {
      if(best.ok&&!out.conditional_bounds.enclosed)
        out.conditional_bounds=genbeta_allfree_bounds::conditional(y,best.t,best.a,best.b,
          certificate_shift,maxit);
      out.tail_bounds=genbeta_allfree_bounds::tails(y,certificate_mean,certificate_shift,
        gamma_shape,out.conditional_bounds.enclosed?out.conditional_bounds.loglik_lower:-INFINITY,maxit);
      // A tolerance-based face proposal cannot survive a proved finite
      // improvement on its entire tail neighborhood. Retain both witnesses
      // as an unresolved comparison; compact global coverage is still open.
      const bool ridge_excluded=out.status==GenbetaStatus::Ok&&
        best.b>=out.tail_bounds.ridge_shape2_lower&&best.b<=out.tail_bounds.ridge_shape2_upper;
      if(ridge_excluded) {
        const double gap=certificate_mean-ymin,endpoint=*std::max_element(x.begin(),x.end());
        const double value=n*(-std::log(gap)-1)+certificate_shift;
        const double pad=4096*std::numeric_limits<double>::epsilon()*(1+std::abs(value));
        out.stop_detail=comparison_detail({{endpoint,1/gap},value-pad,value+pad,StopModel::Power},
          {{best.a,best.b,best.t,scale},out.conditional_bounds.loglik_lower,
            out.conditional_bounds.loglik_upper,StopModel::None});
        out.status=GenbetaStatus::SearchUnresolved;
        out.certificate_failure=GenbetaCertificateFailure::LegacyContradicted;
        out.stop_detail.comparison_tie=false;
      }
      const bool contradicted=(out.status==GenbetaStatus::UnitGammaBoundary&&out.tail_bounds.small_excluded)||
        (out.status==GenbetaStatus::PowerBoundary&&out.tail_bounds.large_excluded);
      if(contradicted&&!out.stop_detail.candidates.empty()) {
        const auto face_candidate=out.stop_detail.candidates.front();
        out.stop_detail=comparison_detail(face_candidate,{{best.a,best.b,best.t,scale},
          out.conditional_bounds.loglik_lower,out.conditional_bounds.loglik_upper,StopModel::None});
        out.status=GenbetaStatus::SearchUnresolved;
        out.certificate_failure=GenbetaCertificateFailure::LegacyContradicted;
        out.stop_detail.comparison_tie=false;
      }
      if(out.status==GenbetaStatus::UnitGammaBoundary)
        return genbeta_gamma_face_cert::verify(y,certificate_mean,certificate_shift,tol,maxit,out);
      if(out.status==GenbetaStatus::PowerBoundary||out.status==GenbetaStatus::Ok) {
        const auto legacy=out;
        auto checked=out.status==GenbetaStatus::PowerBoundary?
          genbeta_power_face_cert::verify(y,*std::max_element(x.begin(),x.end()),
            certificate_shift,gamma_shape,maxit,out):
          genbeta_allfree_cert::verify(y,certificate_mean,certificate_shift,tol,maxit,out);
        if(checked.allfree_certified)return checked;
        // A partial certificate may withhold certification, not a successful
        // legacy answer. A proved improving witness still invalidates it.
        const double pad=4096*std::numeric_limits<double>::epsilon()*
          (1+std::abs(legacy.logLik)+std::abs(certificate_shift));
        for(const auto& candidate:checked.stop_detail.candidates)
          if(candidate.loglik_lower>legacy.logLik+std::max(pad,1e-8*n)) {
            checked.certificate_failure=GenbetaCertificateFailure::LegacyContradicted;
            checked.stop_detail.budget_exhausted=false;
            checked.stop_detail.comparison_tie=false;
            return checked;
          }
        if(legacy.status==GenbetaStatus::PowerBoundary&&
           !valid_stop_coefficients(legacy.stop_detail.limit_model,legacy.stop_detail.limit_coef))
          return checked;
        out=legacy;out.allfree_certified=false;
        out.certificate_failure=checked.certificate_failure;
        out.iterations=checked.iterations;
        out.stop_detail.candidates=checked.stop_detail.candidates;
        out.stop_detail.loglik_lower=legacy.logLik-pad;
        out.stop_detail.loglik_upper=INFINITY;
        // Exhaustion describes the optional certificate, not the legacy fit.
        out.stop_detail.budget_exhausted=false;
        out.stop_detail.comparison_tie=false;
        return out;
      }
    }
    return out;
  };
  if(std::isnan(a)&&std::isnan(b)&&std::isnan(t)&&evals>=certificate_budget(maxit)) {
    bad.status=GenbetaStatus::SearchUnresolved;bad.iterations=evals;
    bad.certificate_failure=GenbetaCertificateFailure::Budget;bad.stop_detail.budget_exhausted=true;
    if(best.ok) {
      const double value=best.ll+static_cast<double>(jac),pad=2048*std::numeric_limits<double>::epsilon()*(1+std::abs(value));
      bad.stop_detail.candidates={{{best.a,best.b,best.t,scale},value-pad,value+pad,StopModel::None}};
      bad.stop_detail.loglik_lower=value-pad;bad.stop_detail.loglik_upper=INFINITY;
    }
    return bad;
  }
  if(best.ok && face_numerical_tie(face,best.ll,1e-8*n)) {
    GenbetaResult r={nan,nan,nan,scale,face+static_cast<double>(jac),evals,GenbetaStatus::SearchUnresolved};
    const double ll=best.ll+static_cast<double>(jac);
    FitCandidate face_candidate;
    face_candidate.loglik_lower=r.logLik;face_candidate.loglik_upper=r.logLik;
    if(fs==GenbetaStatus::UnitGammaBoundary){face_candidate.coef={gamma_shape,gamma_scale,scale};face_candidate.model=StopModel::UnitGamma;}
    if(fs==GenbetaStatus::PowerBoundary){face_candidate.coef={std::isnan(b)?*std::max_element(x.begin(),x.end()):scale,1/(std::isnan(b)?mean-ymin:mean)};face_candidate.model=StopModel::Power;}
    r.stop_detail=comparison_detail(face_candidate,{{best.a,best.b,best.t,scale},ll,ll,StopModel::None});return finish(r);
  }
  if(best.ok&&face>=best.ll-1e-8*n) {
    GenbetaResult r={nan,nan,nan,scale,face+static_cast<double>(jac),evals,fs};
    if(fs==GenbetaStatus::UnitGammaBoundary)r.stop_detail=limit_detail({gamma_shape,gamma_scale,scale},r.logLik,StopModel::UnitGamma);
    if(fs==GenbetaStatus::PowerBoundary)r.stop_detail=limit_detail({std::isnan(b)?*std::max_element(x.begin(),x.end()):scale,1/(std::isnan(b)?mean-ymin:mean)},r.logLik,StopModel::Power);
    return finish(r);
  }
  const bool allfree=std::isnan(a)&&std::isnan(b)&&std::isnan(t);
  GenbetaConditionalBounds conditional;
  if(allfree&&best.ok)
    conditional=genbeta_allfree_bounds::conditional(y,best.t,best.a,best.b,
      certificate_shift,maxit);
  const bool ok=best.ok&&!edge&&(!std::isnan(t)||std::abs(best.score)<tol);
  GenbetaResult out{best.a,best.b,best.t,scale,best.ll+static_cast<double>(jac),evals,
    ok?GenbetaStatus::Ok:GenbetaStatus::SearchUnresolved};
  out.conditional_bounds=conditional;
  if(out.status!=GenbetaStatus::Ok&&best.ok&&std::isfinite(out.logLik)) {
    const double pad=128*std::numeric_limits<double>::epsilon()*(1+std::abs(out.logLik));
    out.stop_detail.candidates={{{best.a,best.b,best.t,scale},out.logLik-pad,out.logLik+pad,StopModel::None}};
    out.stop_detail.loglik_lower=out.logLik-pad;out.stop_detail.loglik_upper=INFINITY;
  }
  return finish(out);
}
} // namespace cppunivariate
