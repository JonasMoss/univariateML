#pragma once
#include "../../core/precision.hpp"
#include "dpqr.hpp"
#include "../../core/span.hpp"
#include <algorithm>
#include <limits>
#include <vector>
namespace cppunivariate {
enum class SlashStatus { Ok, Unbounded, CollapseLimit, InvalidInput, IterationFailure };
struct SlashResult {double mu,sigma,logLik;int iterations;SlashStatus status;};
namespace slash_detail {
// Evaluate on the original observations again after affine back-transformation:
// subnormal rounding of the returned scale can invalidate an internal root.
template <typename Sample>
inline double scale_score(const Sample& x,double m,double log_s) {
  double g=0;
  for(double v:x) {
    const double logt=2*distributions::slash_log_distance_cached(v,m,log_s)-std::log(2.0);
    const double t=std::exp(std::min(logt,4.0));
    g+=t<1e-7?-1+t-t*t/6:(t>=42?1:1-2*t/std::expm1(t));
  }
  return g;
}
struct Profile {
  const std::vector<double>& x; double held; int maxit=65;
  SlashResult operator()(double m) const {
    double s=held;
    if(std::isnan(s)) {
      CPPUNIV_EXTENDED_REAL radius=0;for(double v:x)radius=std::max(radius,std::abs(static_cast<CPPUNIV_EXTENDED_REAL>(v)-m));
      // Each scale-score term decreases from +1 to -1. Verified signs
      // bracket the unique scale maximum; the gate uses the represented scale.
      const double lower=std::log(std::numeric_limits<double>::denorm_min()),upper=std::log(std::numeric_limits<double>::max());
      double lo=std::max(lower,-350.0),hi=std::min(upper,static_cast<double>(std::log(radius))+1);
      auto score=[&](double mid){return scale_score(x,m,mid);};
      double step=32;while(score(lo)<=0 && lo>lower){lo=std::max(lower,lo-step);step*=2;}
      step=32;while(score(hi)>=0 && hi<upper){hi=std::min(upper,hi+step);step*=2;}
      if(!(score(lo)>0 && score(hi)<0))return {m,s,-INFINITY,0,SlashStatus::IterationFailure};
      for(int k=0;k<std::min(maxit,65);++k){const double mid=lo/2+hi/2;if(mid==lo||mid==hi)break;if(score(mid)>0)lo=mid;else hi=mid;}
      s=std::exp(lo/2+hi/2);
      if(!(s>0 && std::isfinite(s) && std::abs(score(std::log(s)))<1e-3))return {m,s,-INFINITY,0,SlashStatus::IterationFailure};
    }
    const double log_s=std::log(s);
    double ll=0;for(double v:x)ll+=distributions::slash_log_density_cached(v,m,s,log_s);
    return {m,s,ll,0,SlashStatus::Ok};
  }
};

// Upper curvature bound and location score at m. The variance bound is
// complemented by the closed-form tail bound documented in the dossier.
inline bool held_slope_curvature(const Profile& p,double m,double lo,double hi,
                                 double& slope,double& curvature) {
  const double s=p.held;
  slope=0;curvature=0;
  if(!(s>0&&std::isfinite(s)))return false;
  for(double v:p.x) {
    const double z=(v-m)/s, t=(.5*z)*z;
    double score;
    if(t<1e-4)score=z*(.5-t/12+t*t*t/720);
    else if(t>=700)score=2/z;
    else score=2/z-z/std::expm1(t);
    slope+=score/s;
    const double left=std::abs((v-lo)/s),right=std::abs((v-hi)/s);
    const double upper=std::max(left,right);
    const double lower=(v>=lo&&v<=hi)?0:std::min(left,right);
    double bound=.25*upper*upper;
    if(lower>0) {
      const double q=(.5*lower)*lower;
      const double tail=2/(lower*lower)+(q>=700?0:1/std::expm1(q));
      bound=std::min(bound,tail);
    }
    curvature+=(bound/s)/s;
  }
  return std::isfinite(slope)&&std::isfinite(curvature);
}

struct HeldInterval {double lo,hi,flo,fhi;};

// Conditional on floating-point comparisons: exact-arithmetic Lipschitz and
// curvature bounds, with a practical likelihood margin, not interval math.
inline bool certify_held(const Profile& p,const std::vector<double>& grid,
                         const std::vector<double>& ll,double offset,
                         SlashResult& best,double& polish_lo,double& polish_hi,
                         std::size_t budget=1000000) {
  if(!(p.held>0&&std::isfinite(p.held))||!std::isfinite(best.logLik)||!std::isfinite(offset))return false;
  const double lip=static_cast<double>(p.x.size())*1.4142135623730950488/p.held;
  if(!std::isfinite(lip))return false;
  std::vector<HeldInterval> stack;
  for(std::size_t i=1;i<grid.size();++i) {
    if(!std::isfinite(ll[i-1])||!std::isfinite(ll[i]))return false;
    stack.push_back({grid[i-1],grid[i],ll[i-1],ll[i]});
  }
  std::size_t work=0;
  while(!stack.empty()) {
    if(++work>budget)return false;
    const HeldInterval q=stack.back();stack.pop_back();
    const double margin=1e-9*(1+std::abs(best.logLik-offset));
    const double width=q.hi-q.lo;
    const double upper=std::max(std::max(q.flo,q.fhi),
        (q.flo/2+q.fhi/2)+(lip*width)/2);
    if(upper<=best.logLik+margin)continue;
    const double mid=q.lo/2+q.hi/2;
    if(!(mid>q.lo&&mid<q.hi))return false;
    const SlashResult f=p(mid);
    if(!std::isfinite(f.logLik))return false;
    if(f.logLik>best.logLik){best=f;polish_lo=q.lo;polish_hi=q.hi;}
    const double current_margin=1e-9*(1+std::abs(best.logLik-offset));
    double slope,curvature;
    if(held_slope_curvature(p,mid,q.lo,q.hi,slope,curvature)) {
      const double radius=std::max(mid-q.lo,q.hi-mid);
      const double bound=f.logLik+std::abs(slope)*radius+.5*curvature*radius*radius;
      if(bound<=best.logLik+current_margin)continue;
    }
    stack.push_back({q.lo,mid,q.flo,f.logLik});
    stack.push_back({mid,q.hi,f.logLik,q.fhi});
  }
  return true;
}
}
inline SlashResult slash_mle(span x,double tol=1e-9,int maxit=200,double sigma=special::kNaN,double mu=special::kNaN) {
  SlashResult bad={special::kNaN,special::kNaN,special::kNaN,0,SlashStatus::InvalidInput};
  if(x.empty()||!(tol>0&&std::isfinite(tol))||maxit<1||(!std::isnan(sigma)&&!(sigma>0&&std::isfinite(sigma))))return bad;

  if(!std::isnan(mu)) {
    if(!std::isfinite(mu))return bad;
    std::vector<double> y(x.begin(),x.end());for(double v:y)if(!std::isfinite(v))return bad;
    if(std::isnan(sigma)) {
      const std::size_t k=static_cast<std::size_t>(std::count(y.begin(),y.end(),mu));
      if(2*k>y.size()){bad.status=SlashStatus::Unbounded;return bad;}
      // At half mass the fixed-location scale likelihood lies strictly
      // below its collapse value for every positive scale.
      if(2*k==y.size()){bad.status=SlashStatus::CollapseLimit;return bad;}
    }
    double unit=std::isnan(sigma)?0:sigma;for(double v:y)unit=std::max(unit,std::abs(v-mu));
    if(!std::isfinite(unit)) {
      unit=std::max(std::abs(mu),std::isnan(sigma)?0:sigma);
      for(double v:y)unit=std::max(unit,std::abs(v));
    }
    if(!(unit>0))return bad;
    for(double& v:y)v=special::scaled_difference(v,mu,unit);
    slash_detail::Profile p{y,std::isnan(sigma)?sigma:sigma/unit,maxit};
    auto r=p(0);r.mu=mu;r.sigma=std::isnan(sigma)?unit*r.sigma:sigma;r.logLik-=y.size()*std::log(unit);
    if(r.status==SlashStatus::Ok && !(r.sigma>0 && std::isfinite(r.sigma) && std::isfinite(r.logLik)))r.status=SlashStatus::IterationFailure;
    if(r.status==SlashStatus::Ok && std::isnan(sigma) && !(std::abs(slash_detail::scale_score(x,mu,std::log(r.sigma)))<1e-3))r.status=SlashStatus::IterationFailure;
    return r;
  }
  std::vector<double> y(x.begin(),x.end());for(double v:y)if(!std::isfinite(v))return bad;
  std::sort(y.begin(),y.end());const double centre=y[y.size()/2];
  std::vector<double> distances;for(double v:y)distances.push_back(static_cast<double>(std::abs(static_cast<CPPUNIV_EXTENDED_REAL>(v)-centre)));
  std::sort(distances.begin(),distances.end());double scale=distances[distances.size()/2];
  if(!(scale>0)||!std::isfinite(scale))scale=y.back()/2-y.front()/2;
  if(!(scale>0)) {if(std::isnan(sigma)){bad.status=SlashStatus::Unbounded;return bad;}scale=sigma;}
  if(!std::isnan(sigma))scale=std::max(scale,sigma);
  for(double& v:y)v=static_cast<double>((static_cast<CPPUNIV_EXTENDED_REAL>(v)-centre)/scale);
  const double n=y.size();double boundary=special::kNegInf;
  if(std::isnan(sigma)) {
    for(std::size_t i=0;i<y.size();) {
      std::size_t j=i+1;while(j<y.size()&&y[j]==y[i])++j;
      if(2*(j-i)>y.size()){bad.status=SlashStatus::Unbounded;return bad;}
      if(2*(j-i)==y.size()) {
        double ll=-n*.91893853320467274178-(j-i)*std::log(2.0);
        for(double v:y)if(v!=y[i])ll-=2*std::log(std::abs(v-y[i]));
        boundary=std::max(boundary,ll);
      }
      i=j;
    }
  }
  slash_detail::Profile p{y,std::isnan(sigma)?sigma:sigma/scale,maxit};
  std::vector<double> grid;
  for(int i=0;i<=128;++i)grid.push_back(static_cast<double>((1-i/CPPUNIV_EXTENDED_LITERAL(128.0,128.0L))*y.front()+(i/CPPUNIV_EXTENDED_LITERAL(128.0,128.0L))*y.back()));
  for(std::size_t i=0;i<y.size();++i) {
    grid.push_back(y[i]);
    if(i&&y[i]>y[i-1])for(int k=1;k<8;++k)grid.push_back(y[i-1]+(y[i]-y[i-1])*k/8);
  }
  if(std::isnan(sigma)&&std::isfinite(boundary)) {
    for(std::size_t i=0;i<y.size();) {
      std::size_t j=i+1;while(j<y.size()&&y[j]==y[i])++j;
      if(2*(j-i)==y.size()) for(int k=1;k<=12;++k) {
        const double step=std::pow(10.0,-k);
        if(y[i]-step>y.front())grid.push_back(y[i]-step);
        if(y[i]+step<y.back())grid.push_back(y[i]+step);
      }
      i=j;
    }
  }
  std::sort(grid.begin(),grid.end());grid.erase(std::unique(grid.begin(),grid.end()),grid.end());
  SlashResult best=p(grid.front());bool refined=grid.size()==1,failed=false;std::vector<double> ll;
  for(double m:grid){auto f=p(m);ll.push_back(f.logLik);if(f.logLik>best.logLik)best=f;}
  for(std::size_t i=0;i<grid.size();++i)if((i==0||ll[i]>=ll[i-1])&&(i+1==grid.size()||ll[i]>=ll[i+1])) {
    double lo=grid[i==0?0:i-1],hi=grid[i+1==grid.size()?i:i+1];int it=0;
    // Golden-section refinement of every grid-local maximum.
    double a=hi-(hi-lo)*.6180339887498948482,b=lo+(hi-lo)*.6180339887498948482;
    auto fa=p(a),fb=p(b);
    for(;it<maxit&&hi-lo>tol;++it) {
      if(fa.logLik>fb.logLik){hi=b;b=a;fb=fa;a=hi-(hi-lo)*.6180339887498948482;fa=p(a);}
      else{lo=a;a=b;fa=fb;b=lo+(hi-lo)*.6180339887498948482;fb=p(b);}
    }
    auto f=fa.logLik>fb.logLik?fa:fb;f.iterations=it;
    if(it==maxit){f.status=SlashStatus::IterationFailure;failed=true;}else refined=true;
    if(f.logLik>best.logLik)best=f;
  }
  if(failed || !refined)best.status=SlashStatus::IterationFailure;
  if(!std::isnan(sigma)&&best.status==SlashStatus::Ok) {
    double lo=0,hi=0;
    if(!slash_detail::certify_held(p,grid,ll,n*std::log(scale),best,lo,hi)) {
      bad.status=SlashStatus::IterationFailure;return bad;
    }
    if(hi>lo) {
      int it=0;
      double a=hi-(hi-lo)*.6180339887498948482,b=lo+(hi-lo)*.6180339887498948482;
      auto fa=p(a),fb=p(b);
      for(;it<maxit&&hi-lo>tol;++it) {
        if(fa.logLik>fb.logLik){hi=b;b=a;fb=fa;a=hi-(hi-lo)*.6180339887498948482;fa=p(a);}
        else{lo=a;a=b;fa=fb;b=lo+(hi-lo)*.6180339887498948482;fb=p(b);}
      }
      if(it==maxit){bad.status=SlashStatus::IterationFailure;return bad;}
      auto f=fa.logLik>fb.logLik?fa:fb;
      if(f.logLik>best.logLik){f.iterations=it;best=f;}
    }
  }
  if(best.status!=SlashStatus::Ok || !std::isfinite(best.logLik)){bad.status=SlashStatus::IterationFailure;return bad;}
  if(boundary>=best.logLik-1e-8*n){bad.status=SlashStatus::CollapseLimit;return bad;}
  best.mu=centre+scale*best.mu;best.sigma=std::isnan(sigma)?scale*best.sigma:sigma;best.logLik-=n*std::log(scale);
  if(!std::isnan(sigma)&&(!std::isfinite(best.mu)||!std::isfinite(best.logLik))) {
    bad.status=SlashStatus::IterationFailure;return bad;
  }
  if(best.status==SlashStatus::Ok && std::isnan(sigma) && !(best.sigma>0 && std::isfinite(best.sigma) && std::isfinite(best.logLik) && std::abs(slash_detail::scale_score(x,best.mu,std::log(best.sigma)))<1e-3))best.status=SlashStatus::IterationFailure;
  return best;
}
}
