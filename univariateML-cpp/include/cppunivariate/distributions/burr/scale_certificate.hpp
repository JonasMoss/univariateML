// Held-scale scalar certificate. Mathematical proof and arithmetic contract:
// README.md, "Held-scale certificate (TASK-116.4)".
#pragma once
#include "certificate.hpp"
namespace cppunivariate {
namespace burr_cert_detail {
struct ScalePoint {
  double g=0, gp=0, remainder=0, score=NAN, value=NAN;
};
inline ScalePoint scale_point(const std::vector<double>& y,double t) {
  ScalePoint p;
  double d=0;
  for(double v:y) {
    const double z=t*v;
    p.g+=softplus(z);p.gp+=v*sigmoid(z);
    p.remainder+=softplus(-z);d+=v*sigmoid(-z);
  }
  const double n=y.size();
  p.score=n/t-n*p.gp/p.g+d;
  p.value=n*(std::log(n)+std::log(t)-std::log(p.g)-1)-p.remainder;
  return p;
}
inline bool scale_bounds(const std::vector<Interval>& y,Interval t,
    Interval& g,Interval& score,Interval& value) {
  Sum sg,sgp,sd,sr;
  for(Interval v:y) {
    const Interval z=t*v;
    sg.add(sp(z));sgp.add(v*sig(z));sd.add(v*sig(-z));sr.add(sp(-z));
  }
  const double n=y.size();g=sg.get();
  score=n/t-n*sgp.get()/g+sd.get();
  value=n*(log(Interval(n))+log(t)-log(g)-1)-sr.get();
  return g.valid()&&g.lo>0&&score.valid()&&value.valid();
}
inline Result scale_fit(span x,double scale,double tol,int maxit,double proposal=NAN) {
  Result out;out.scale=scale;
  if(x.size()==0||!(scale>0)||!std::isfinite(scale)||!(tol>0)||!std::isfinite(tol)||maxit<1) {
    out.outcome=Outcome::Invalid;return out;
  }
  bool constant=true,all_ge=true;int zeros=0;
  double unit=0,jac=0;
  std::vector<double> z;std::vector<Interval> box;Sum logx;
  for(double v:x) {
    if(!(v>0)||!std::isfinite(v)){out.outcome=Outcome::Invalid;return out;}
    constant=constant&&v==x[0];all_ge=all_ge&&v>=scale;if(v==scale)++zeros;
    // Close ratios use log1p; subtraction has small relative error there.
    // Otherwise logs avoid overflow/underflow of x/scale.
    const double l=(v>=scale/2&&v/2<=scale)?std::log1p((v-scale)/scale):std::log(v)-std::log(scale);
    if(!std::isfinite(l))return out;
    z.push_back(l);
    box.push_back((v>=scale/2&&v/2<=scale)?padded(l,l):padded(std::log(v),std::log(v))-padded(std::log(scale),std::log(scale)));
    unit=std::max(unit,std::abs(l));jac-=std::log(v);logx.add(padded(std::log(v),std::log(v)));
  }
  if(constant&&x[0]<=scale){out.outcome=Outcome::Unbounded;out.logLik=INFINITY;return out;}
  const double n=x.size();
  if(all_ge) {
    Sum total;double s=0;for(std::size_t i=0;i<z.size();++i){total.add(box[i]);s+=z[i];}
    const Interval alpha=n/total.get(),value=n*log(alpha)-n-logx.get()-zeros*log(Interval(2));
    if(!alpha.valid()||alpha.lo<=0||!value.valid())return out;
    out.outcome=Outcome::Pareto;
    out.stop_detail.limit_coef={n/s,scale};
    out.stop_detail.limit_loglik=n*std::log(n/s)-n+jac-zeros*std::log(2.);
    out.stop_detail.loglik_lower=value.lo;out.stop_detail.loglik_upper=value.hi;
    out.stop_detail.candidates.push_back({{n/s,scale},value.lo,value.hi,StopModel::Pareto});
    return out;
  }
  if(!(unit>0)||!std::isfinite(unit))return out;
  for(std::size_t i=0;i<z.size();++i){z[i]/=unit;box[i]=box[i]/unit;}
  jac-=n*std::log(unit);
  const Interval jacbox=-logx.get()-n*log(Interval(unit));
  if(!jacbox.valid())return out;
  out.stop_detail.loglik_upper=INFINITY;
  // Keep a bounded finite witness even when the proposal budget runs out.
  Interval wg,ws,wv;
  if(scale_bounds(box,Interval(1),wg,ws,wv)) {
    const auto witness=scale_point(z,1);
    if(std::isfinite(n/witness.g)&&std::isfinite(1/unit)) {
      const Interval value=wv+jacbox;
      out.stop_detail.loglik_lower=value.lo;
      out.stop_detail.candidates.push_back({{n/witness.g,1/unit,scale},value.lo,value.hi});
    }
  }
  double lo=0,hi=1;
  ScalePoint p;
  // These evaluations propose a bracket only. The final endpoint signs
  // below certify the unique global root, independently of this search.
  for(;;) {
    if(out.profiles>=maxit){out.outcome=Outcome::Budget;return out;}
    ++out.profiles;p=scale_point(z,hi);
    if(!std::isfinite(p.score)||!std::isfinite(p.value))return out;
    if(p.score<0)break;
    lo=hi;hi*=2;if(!std::isfinite(hi))return out;
  }
  double t=(lo+hi)/2;
  for(;;) {
    if(out.profiles>=maxit){out.outcome=Outcome::Budget;return out;}
    ++out.profiles;p=scale_point(z,t);
    if(!std::isfinite(p.score)||!std::isfinite(p.value))return out;
    if(p.score>0)lo=t;else hi=t;
    if(hi-lo<=1e-13*t)break;
    t=(lo+hi)/2;
  }
  t=(lo+hi)/2;
  // Preserve historical coefficients only as a checked accuracy witness.
  // The legacy solve has no authority over existence or globality.
  if(proposal>0&&std::isfinite(proposal)&&std::abs(proposal*unit-t)<1e-8*t)
    t=proposal*unit;
  p=scale_point(z,t);
  // Strict concavity turns two signed endpoint comparisons into a global
  // certificate. The coefficient image must also be sufficiently narrow.
  Interval gl,sl,vl,gh,sh,vh,gc,sc,vc;
  bool signed_bracket=false;
  double radius=1e-10*t;
  for(int i=0;i<12;++i,radius*=2) {
    lo=t-radius;hi=t+radius;
    if(!(lo>0))return out;
    if(!scale_bounds(box,Interval(lo),gl,sl,vl)||!scale_bounds(box,Interval(hi),gh,sh,vh))return out;
    if(sl.lo>0&&sh.hi<0){signed_bracket=true;break;}
  }
  if(!signed_bracket){out.outcome=Outcome::Tie;return out;}
  if(!scale_bounds(box,Interval(lo,hi),gc,sc,vc))return out;
  const Interval k=n/gc,c=Interval(lo,hi)/unit;
  if(!k.valid()||!c.valid()||k.lo<=0||c.lo<=0)return out;
  if((k.hi-k.lo)>1e-6*k.lo||(c.hi-c.lo)>1e-6*c.lo){out.outcome=Outcome::Tie;return out;}
  // A concave tangent at either endpoint bounds the peak from above.
  const Interval upper_left=vl+sl*(hi-lo),upper_right=vh-sh*(hi-lo);
  Interval gp,spc,vp;
  if(!scale_bounds(box,Interval(t),gp,spc,vp)||!upper_left.valid()||!upper_right.valid())return out;
  out.k=n/p.g;out.c=t/unit;out.logLik=p.value+jac;
  if(!(out.k>0&&out.c>0)||!std::isfinite(out.k)||!std::isfinite(out.c)||!std::isfinite(out.logLik)){out.k=out.c=out.logLik=NAN;return out;}
  const Interval peak=vp+jacbox,upper=Interval(std::min(upper_left.hi,upper_right.hi))+jacbox;
  if(!peak.valid()||!upper.valid()||peak.lo>upper.hi){out.k=out.c=out.logLik=NAN;return out;}
  out.stop_detail.loglik_lower=peak.lo;
  out.stop_detail.loglik_upper=upper.hi;
  out.stop_detail.candidates={{{out.k,out.c,scale},peak.lo,peak.hi}};
  out.outcome=Outcome::Finite;return out;
}
} // namespace burr_cert_detail
} // namespace cppunivariate
