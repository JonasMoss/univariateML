#pragma once
#include "../../core/work_budget.hpp"
#include "../../core/stop_detail.hpp"
#include "../../core/face_compare.hpp"
#include "../emg/logconcave_profile.hpp"
#include "../../core/precision.hpp"
#include "../../core/multistart.hpp"
#include "dpqr.hpp"
#include "../norm/mle.hpp"
#include "../../core/span.hpp"
#include <vector>
#include <map>
namespace cppunivariate {
enum class BhattStatus {Ok,NormalLimit,UniformLimit,Unbounded,InvalidInput,IterationFailure};
struct BhattResult {double mu,sigma,a,logLik;int iterations;BhattStatus status; StopDetail stop_detail{}; };
namespace bhatt_detail {
struct Profile {
  const std::vector<double>& x;double sigma,a,face;double held_mu=special::kNaN; int maxit=500;
  double location(double s,double b) const {
    if(!std::isnan(held_mu))return held_mu;
    if(b/s<1e-7) {double mean=0;for(double z:x)mean+=z/x.size();return mean;}
    double lo=x.front(),hi=x.back(),m=0;
    for(double z:x)m+=z/x.size();
    // s and b are this call's fixed parameters (only m moves across Newton
    // steps), so their logs/square are loop-invariant: hoisted out of both
    // loops instead of being recomputed at every point on every iteration.
    const CPPUNIV_EXTENDED_REAL log2b=std::log(2*static_cast<CPPUNIV_EXTENDED_REAL>(b));
    const CPPUNIV_EXTENDED_REAL logs=std::log(static_cast<CPPUNIV_EXTENDED_REAL>(s));
    const CPPUNIV_EXTENDED_REAL s2=static_cast<CPPUNIV_EXTENDED_REAL>(s)*s;
    for(int k=0;k<maxit;++k) {
      CPPUNIV_EXTENDED_REAL score=0,deriv=0;
      for(double z:x) {
        const CPPUNIV_EXTENDED_REAL u=(static_cast<CPPUNIV_EXTENDED_REAL>(z)-m-b)/s,v=(static_cast<CPPUNIV_EXTENDED_REAL>(z)-m+b)/s;
        const CPPUNIV_EXTENDED_REAL lu=logphi_extended(u),lv=logphi_extended(v);
        const CPPUNIV_EXTENDED_REAL ld=distributions::dbhatt(z,m,s,b,true)+log2b;
        const CPPUNIV_EXTENDED_REAL A=std::exp(lu-ld),B=std::exp(lv-ld);
        CPPUNIV_EXTENDED_REAL g=0;
        if(lu!=lv) {
          const CPPUNIV_EXTENDED_REAL ln=std::max(lu,lv)+std::log(-std::expm1(-std::abs(lu-lv)));
          g=(lu>lv?1:-1)*std::exp(ln-ld-logs);
        }
        score+=g;deriv+=(u*A-v*B)/s2-g*g;
      }
      if(score==0)return m;
      if(score>0)lo=m;else hi=m;
      double next=m-static_cast<double>(score/deriv);
      if(!(deriv<0)||!std::isfinite(next)||next<=lo||next>=hi)next=lo/2+hi/2;
      if(std::abs(next-m)<1e-11*std::min(s,1.0))return next;
      m=next;
    }
    return NAN;
  }

  // Reuse only identical held-profile coordinates, including across starts.
  // Clearing the bounded cache changes cost only, never search decisions.
  mutable std::map<std::vector<double>,BhattResult> evaluations{};
  BhattResult evaluate(const std::vector<double>& t) const {
    if(std::isnan(held_mu) && std::isnan(sigma) && std::isnan(a)) return evaluate_uncached(t);
    for(double v:t) if(std::isnan(v) || v==0) return evaluate_uncached(t);
    const auto found=evaluations.find(t);
    if(found!=evaluations.end()) return found->second;
    const auto value=evaluate_uncached(t);
    if(evaluations.size()>=4096) evaluations.clear();
    evaluations.emplace(t,value);
    return value;
  }
  BhattResult evaluate_uncached(const std::vector<double>& t) const {
    int k=0;const double s=std::isnan(sigma)?std::exp(t[k++]):sigma,b=std::isnan(a)?std::exp(t[k++]):a;
    if(!(s>0&&b>0&&std::isfinite(s)&&std::isfinite(b)))return {0,s,b,special::kNegInf,0,BhattStatus::IterationFailure};
    // Exact upper bounds screen only candidates unable to beat an attained
    // supremum face. A screened value is never returned as an interior fit:
    // the final exact face comparison always replaces it.
    if(std::isfinite(face)) {
      const double n=x.size(),peak_u=-std::log(2*b),peak_n=-std::log(s)-.91893853320467274178;
      double bound=n*std::min(peak_u,peak_n);
      if(bound<=face)return {0,s,b,bound,0,BhattStatus::Ok};
      if(x.front()!=x.back()) {
        const CPPUNIV_EXTENDED_REAL radius=(static_cast<CPPUNIV_EXTENDED_REAL>(x.back())-x.front())/2;
        const double pair=static_cast<double>(2*logQ_extended((radius-b)/s));
        bound=std::min(bound,2*peak_u+(n-2)*std::min(peak_u,peak_n)+pair);
        if(bound<=face)return {0,s,b,bound,0,BhattStatus::Ok};
      }
    }
    const double m=location(s,b);double ll=0;for(double z:x)ll+=distributions::dbhatt(z,m,s,b,true);
    return {m,s,b,ll,0,BhattStatus::Ok};
  }
};
inline BhattResult search(const Profile& p,std::vector<double> start,double tol,int maxit) {
  maxit = static_cast<int>(solver_budget(maxit));
  const int d=static_cast<int>(start.size());
  if(!d) return p.evaluate(start);
  std::vector<std::vector<double>> v(d+1,start);
  for(int i=0;i<d;++i) v[i+1][i]+=.4;
  std::vector<BhattResult> f;for(const auto& t:v)f.push_back(p.evaluate(t));
  for(int it=0;it<maxit;++it) {
    for(int i=0;i<=d;++i)for(int j=i+1;j<=d;++j)if(f[j].logLik>f[i].logLik){std::swap(f[i],f[j]);std::swap(v[i],v[j]);}
    double diameter=0;for(int i=1;i<=d;++i)for(int j=0;j<d;++j)diameter=std::max(diameter,std::abs(v[i][j]-v[0][j]));
    if(diameter<tol) {f[0].iterations=it;return f[0];}
    std::vector<double> c(d,0),r(d),e(d);
    for(int j=0;j<d;++j){for(int i=0;i<d;++i)c[j]+=v[i][j]/d;r[j]=2*c[j]-v[d][j];}
    auto fr=p.evaluate(r);
    if(fr.logLik>f[0].logLik){for(int j=0;j<d;++j)e[j]=c[j]+2*(r[j]-c[j]);auto fe=p.evaluate(e);if(fe.logLik>fr.logLik){v[d]=e;f[d]=fe;}else{v[d]=r;f[d]=fr;}}
    else if(fr.logLik>f[d-1].logLik){v[d]=r;f[d]=fr;}
    else {
      const bool outside=fr.logLik>f[d].logLik;
      for(int j=0;j<d;++j)e[j]=c[j]+.5*((outside?r[j]:v[d][j])-c[j]);
      auto fe=p.evaluate(e);
      if(fe.logLik>(outside?fr.logLik:f[d].logLik)){v[d]=e;f[d]=fe;}
      else for(int i=1;i<=d;++i){for(int j=0;j<d;++j)v[i][j]=(v[i][j]+v[0][j])/2;f[i]=p.evaluate(v[i]);}
    }
  }
  auto best=*std::max_element(f.begin(),f.end(),[](const BhattResult& a,const BhattResult& b){return a.logLik<b.logLik;});
  best.iterations=maxit;best.status=BhattStatus::IterationFailure;
  std::vector<double> theta;
  if(std::isnan(p.held_mu))theta.push_back(best.mu);
  if(std::isnan(p.sigma))theta.push_back(std::log(best.sigma));
  if(std::isnan(p.a))theta.push_back(std::log(best.a));
  const auto likelihood=[&](const std::vector<double>& t) {
    int k=0;
    const double m=std::isnan(p.held_mu)?t[k++]:p.held_mu;
    const double s=std::isnan(p.sigma)?std::exp(t[k++]):p.sigma;
    const double b=std::isnan(p.a)?std::exp(t[k++]):p.a;
    double ll=0;for(double x:p.x)ll+=distributions::dbhatt(x,m,s,b,true);
    return ll;
  };
  if(multistart_score_acceptable(theta,likelihood)) {
    best.logLik=likelihood(theta);best.status=BhattStatus::Ok;
  }
  return best;
}
}
inline BhattResult bhatt_mle(span x,double tol=1e-8,int maxit=500,double sigma=special::kNaN,double a=special::kNaN,double mu=special::kNaN) {
  maxit = static_cast<int>(solver_budget(maxit));
  BhattResult bad={special::kNaN,special::kNaN,special::kNaN,special::kNaN,0,BhattStatus::InvalidInput};
  if(x.empty()||!(tol>0&&std::isfinite(tol))||maxit<1||(!std::isnan(sigma)&&!(sigma>0&&std::isfinite(sigma)))||(!std::isnan(a)&&!(a>=0&&std::isfinite(a))))return bad;
  if(!std::isnan(mu)&&!std::isfinite(mu))return bad;
  if(a==0) {
    const auto r=norm_mle(x,mu,sigma);
    if(!(r.sd>0)){bad.status=BhattStatus::Unbounded;return bad;}
    return {r.mean,r.sd,0,r.logLik,0,BhattStatus::Ok};
  }
  std::vector<double> y(x.begin(),x.end());for(double v:y)if(!std::isfinite(v))return bad;std::sort(y.begin(),y.end());
  const double centre=y.front()/2+y.back()/2;double scale=y.back()/2-y.front()/2;
  if(!std::isnan(mu))scale=std::max(scale,std::abs(mu-centre));
  if(!(scale>0)){if(std::isnan(sigma)&&std::isnan(a)){bad.status=BhattStatus::Unbounded;return bad;}scale=std::isnan(sigma)?a:sigma;}
  if(!std::isnan(sigma))scale=std::max(scale,sigma);
  if(!std::isnan(a))scale=std::max(scale,a);
  for(double& v:y)v=static_cast<double>((static_cast<CPPUNIV_EXTENDED_REAL>(v)-centre)/scale);
  const double n=y.size(),held_mu=std::isnan(mu)?mu:(mu-centre)/scale;
  const double held_s=std::isnan(sigma)?sigma:sigma/scale,held_a=std::isnan(a)?a:a/scale;
  // With width held and covering the sample, pointwise and complementary
  // endpoint bounds prove that the uniform face dominates every interior fit.
  if(std::isnan(sigma)&&!std::isnan(a)&&(std::isnan(mu)?y.back()-y.front()<=2*held_a:std::max(std::abs(y.front()-held_mu),std::abs(y.back()-held_mu))<=held_a)) {
    double ll=-n*std::log(2*held_a);
    if(!std::isnan(mu)){for(double z:y)if(std::abs(z-held_mu)==held_a)ll-=std::log(2.0);}
    else if(y.back()-y.front()==2*held_a) {
      double left=0,right=0;for(double z:y){if(z==y.front())++left;if(z==y.back())++right;}
      ll+=left*std::log(left/(left+right))+right*std::log(right/(left+right));
    }
    BhattResult face_fit={std::isnan(mu)?centre:mu,0,a,ll-n*std::log(scale),0,BhattStatus::UniformLimit};
    face_fit.stop_detail=limit_detail({face_fit.mu-a,face_fit.mu+a},face_fit.logLik,StopModel::Unif);
    return face_fit;
  }
  double face=special::kNegInf;
  if(std::isnan(a)) {
    double mean=0;for(double z:y)mean+=z/n;if(!std::isnan(mu))mean=held_mu;
    double var=0;for(double z:y)var+=(z-mean)*(z-mean)/n;
    const double sd=std::isnan(sigma)?std::sqrt(var):held_s;
    face=-n*(std::log(sd)+.91893853320467274178)-n*var/(2*sd*sd);
  }
  if(std::isnan(sigma)&&std::isnan(a))face=std::max(face,-n*std::log(std::isnan(mu)?y.back()-y.front():2*std::max(std::abs(y.front()-held_mu),std::abs(y.back()-held_mu))));
  bhatt_detail::Profile p{y,held_s,held_a,face,held_mu,maxit};
  BhattResult best={0,0,0,special::kNegInf,0,BhattStatus::IterationFailure};
  auto attempted=best;
  bool faces_only=false;
  auto fallback_reason=logconcave_detail::FallbackReason::InnerFailure;
  logconcave_detail::reset_fallback();
  const std::vector<double> starts={-4,-1,0,1};
  const std::vector<double> widths={-4,-1,-.3,-.05};
  if(std::isnan(mu)&&std::isnan(sigma)&&std::isnan(a)) {
    const auto candidate=logconcave_detail::search(y,[](CPPUNIV_EXTENDED_REAL z,CPPUNIV_EXTENDED_REAL k) {
      // Conditional latent-uniform moments avoid subtracting nearly equal
      // normal probabilities and their derivatives for a narrow interval.
      if(k*(1+std::abs(z))<0.05) {
        constexpr CPPUNIV_EXTENDED_REAL nodes[4]={CPPUNIV_EXTENDED_LITERAL(.18343464249564980494,.18343464249564980494L),CPPUNIV_EXTENDED_LITERAL(.52553240991632898582,.52553240991632898582L),CPPUNIV_EXTENDED_LITERAL(.79666647741362673959,.79666647741362673959L),CPPUNIV_EXTENDED_LITERAL(.96028985649753623168,.96028985649753623168L)};
        constexpr CPPUNIV_EXTENDED_REAL weights[4]={CPPUNIV_EXTENDED_LITERAL(.36268378337836198297,.36268378337836198297L),CPPUNIV_EXTENDED_LITERAL(.31370664587788728734,.31370664587788728734L),CPPUNIV_EXTENDED_LITERAL(.22238103445337447054,.22238103445337447054L),CPPUNIV_EXTENDED_LITERAL(.10122853629037625915,.10122853629037625915L)};
        CPPUNIV_EXTENDED_REAL sum=0,m=0,m2=0;
        for(int i=0;i<4;++i)for(int sign:{-1,1}) {
          const CPPUNIV_EXTENDED_REAL t=sign*k*nodes[i],w=weights[i]*std::exp(z*t-t*t/2)/2;
          sum+=w;m+=w*t;m2+=w*t*t;
        }
        m/=sum;m2/=sum;
        return std::array<CPPUNIV_EXTENDED_REAL,3>{{bhatt_detail::logphi_extended(z)+std::log(sum),m-z,m2-m*m-1}};
      }
      const CPPUNIV_EXTENDED_REAL u=static_cast<CPPUNIV_EXTENDED_REAL>(z)-k,v=static_cast<CPPUNIV_EXTENDED_REAL>(z)+k;
      const CPPUNIV_EXTENDED_REAL lu=bhatt_detail::logphi_extended(u),lv=bhatt_detail::logphi_extended(v);
      const CPPUNIV_EXTENDED_REAL logd=(u < -12 && v > 12)?0:
        (u>=0?bhatt_detail::logdiff_extended(bhatt_detail::logQ_extended(u),bhatt_detail::logQ_extended(v)):
        bhatt_detail::logdiff_extended(bhatt_detail::logQ_extended(-v),bhatt_detail::logQ_extended(-u)));
      const CPPUNIV_EXTENDED_REAL A=std::exp(lu-logd),B=std::exp(lv-logd);
      CPPUNIV_EXTENDED_REAL g=0;
      if(lu!=lv) {const CPPUNIV_EXTENDED_REAL ln=std::max(lu,lv)+std::log(-std::expm1(-std::abs(lu-lv)));g=(lv>lu?1:-1)*std::exp(ln-logd);}
      return std::array<CPPUNIV_EXTENDED_REAL,3>{{logd-std::log(2*static_cast<CPPUNIV_EXTENDED_REAL>(k)),g,u*A-v*B-g*g}};
    },maxit);
    if(candidate.ok && candidate.faces_only) {
      faces_only=true;best=attempted={0,0,special::kNaN,candidate.ll,0,BhattStatus::IterationFailure};
    } else if(candidate.ok) {
      const double sig=1/candidate.b,width=candidate.k/candidate.b;
      const auto polished=bhatt_detail::search(p,{std::log(sig),std::log(width)},tol,maxit);
      if(polished.status==BhattStatus::Ok) {
        best=attempted=polished;
        // Retain the better of the profile polish and one existing central
        // multistart seed; this also preserves the established fit precision.
        const auto central=bhatt_detail::search(p,{-1,-.3},tol,maxit);
        if(central.status==BhattStatus::Ok && central.logLik>=best.logLik)best=attempted=central;
      } else fallback_reason=logconcave_detail::FallbackReason::PolishFailure;
    }
  }
  auto run_multistart=[&]() {
  for(double s:starts)for(double b:widths){
    if(!std::isnan(sigma)&&s!=starts.front())continue;
    if(!std::isnan(a)&&b!=widths.front())continue;
    std::vector<double> t;if(std::isnan(sigma))t.push_back(s);if(std::isnan(a))t.push_back(b);
    const auto f=bhatt_detail::search(p,t,tol,maxit);if(std::isfinite(f.logLik) && f.logLik>attempted.logLik)attempted=f;
    if(f.status==BhattStatus::Ok && std::isfinite(f.logLik) && f.logLik>best.logLik)best=f;
  }
  };
  if(!std::isfinite(best.logLik)) {
    if(std::isnan(mu)&&std::isnan(sigma)&&std::isnan(a))logconcave_detail::record_fallback(fallback_reason);
    run_multistart();
  }
  if(!std::isfinite(best.logLik))best=attempted;
  const double tie=1e-8*n;
  const auto tied_face=[&](std::vector<double> coef,double ll,StopModel model) {
    StopDetail d;d.comparison_tie=true;
    const double face=ll-n*std::log(scale);d.candidates.push_back({coef,face,face,model});
    d.loglik_lower=face;d.loglik_upper=INFINITY;
    if(best.sigma>0 && best.a>=0) {
      const double finite=best.logLik-n*std::log(scale);
      d.candidates.push_back({{centre+scale*best.mu,scale*best.sigma,scale*best.a},finite,finite,StopModel::None});
      d.loglik_lower=std::max(d.loglik_lower,finite);
    }
    best.status=BhattStatus::IterationFailure;best.stop_detail=d;
  };
  auto compare_faces=[&]() {
  double comparison=attempted.logLik;
  if(std::isnan(a)) {
    double m=0;for(double z:y)m+=z/n;if(!std::isnan(mu))m=held_mu;double var=0;for(double z:y)var+=(z-m)*(z-m)/n;
    const double s=std::isnan(sigma)?std::sqrt(var):p.sigma;
    double ll=-n*(std::log(s)+.91893853320467274178)-n*var/(2*s*s);
    if(face_numerical_tie(ll,comparison,tie)) {tied_face({centre+scale*m,scale*s},ll,StopModel::Norm);return;}
    if(ll>=comparison-tie){best={m,s,0,ll,0,BhattStatus::Ok};comparison=ll;}
  }
  if(std::isnan(sigma)) {
    const double b=std::isnan(a)?(std::isnan(mu)?(y.back()-y.front())/2:std::max(std::abs(y.front()-held_mu),std::abs(y.back()-held_mu))):p.a;
    if(std::isnan(mu)?2*b>=y.back()-y.front():std::max(std::abs(y.front()-held_mu),std::abs(y.back()-held_mu))<=b) {
      double ll=-n*std::log(2*b);
      if(!std::isnan(a)&&!std::isnan(mu)){for(double z:y)if(std::abs(z-held_mu)==b)ll-=std::log(2.0);}
      else if(!std::isnan(a)&&2*b==y.back()-y.front()) {
        double left=0,right=0;for(double z:y){if(z==y.front())++left;if(z==y.back())++right;}
        ll+=left*std::log(left/(left+right))+right*std::log(right/(left+right));
      }
    if(face_numerical_tie(ll,comparison,(best.a==0?0:tie))) {
      const double midpoint=std::isnan(mu)?centre+scale*(y.front()+y.back())/2:mu;
      tied_face({midpoint-scale*b,midpoint+scale*b},ll,StopModel::Unif);return;
    }
      if(ll>=comparison-(best.a==0?0:tie))best={0,0,b,ll,0,BhattStatus::UniformLimit};
    }
  }
  };
  compare_faces();
  if(faces_only && best.status==BhattStatus::IterationFailure) {
    logconcave_detail::record_fallback(logconcave_detail::FallbackReason::UnresolvedFaces);
    best={0,0,0,special::kNegInf,0,BhattStatus::IterationFailure};attempted=best;
    run_multistart();
    if(!std::isfinite(best.logLik))best=attempted;
    compare_faces();
  }
  best.mu=std::isnan(mu)?centre+scale*best.mu:mu;best.sigma=std::isnan(sigma)?scale*best.sigma:sigma;best.a=std::isnan(a)?scale*best.a:a;best.logLik-=n*std::log(scale);
  if(best.status==BhattStatus::UniformLimit) {
    const double midpoint=std::isnan(mu)?centre+scale*(y.front()+y.back())/2:mu;
    best.stop_detail=limit_detail({midpoint-best.a,midpoint+best.a},best.logLik,StopModel::Unif);
  }
  if(best.status==BhattStatus::NormalLimit)best.stop_detail=limit_detail({best.mu,best.sigma},best.logLik,StopModel::Norm);
  return best;
}
}
