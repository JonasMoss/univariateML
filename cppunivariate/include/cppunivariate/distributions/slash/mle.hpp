#pragma once
#include "dpqr.hpp"
#include "../../core/span.hpp"
#include <algorithm>
#include <vector>
namespace cppunivariate {
enum class SlashStatus { Ok, Unbounded, CollapseLimit, InvalidInput, IterationFailure };
struct SlashResult {double mu,sigma,logLik;int iterations;SlashStatus status;};
namespace slash_detail {
struct Profile {
  const std::vector<double>& x; double held;
  SlashResult operator()(double m) const {
    double s=held;
    if(std::isnan(s)) {
      long double radius=0;for(double v:x)radius=std::max(radius,std::abs(static_cast<long double>(v)-m));
      double lo=-350,hi=static_cast<double>(std::log(std::max(radius,1e-150L)))+1;
      for(int k=0;k<65;++k) {
        const double mid=(lo+hi)/2,ss=std::exp(mid);double score=0;
        for(double v:x) {const double z=(v-m)/ss,t=z*z/2;score+=t<1e-7 ? -1+t-t*t/6 : (t>700?1:1-2*t/std::expm1(t));}
        if(score>0)lo=mid;else hi=mid;
      }
      s=std::exp((lo+hi)/2);
    }
    double ll=0;for(double v:x)ll+=distributions::dslash(v,m,s,true);
    return {m,s,ll,0,SlashStatus::Ok};
  }
};
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
    if(!(unit>0))return bad;
    for(double& v:y)v=(v-mu)/unit;
    slash_detail::Profile p{y,std::isnan(sigma)?sigma:sigma/unit};
    auto r=p(0);r.mu=mu;r.sigma=std::isnan(sigma)?unit*r.sigma:sigma;r.logLik-=y.size()*std::log(unit);return r;
  }
  std::vector<double> y(x.begin(),x.end());for(double v:y)if(!std::isfinite(v))return bad;
  std::sort(y.begin(),y.end());const double centre=y[y.size()/2];
  std::vector<double> distances;for(double v:y)distances.push_back(static_cast<double>(std::abs(static_cast<long double>(v)-centre)));
  std::sort(distances.begin(),distances.end());double scale=distances[distances.size()/2];
  if(!(scale>0)||!std::isfinite(scale))scale=y.back()/2-y.front()/2;
  if(!(scale>0)) {if(std::isnan(sigma)){bad.status=SlashStatus::Unbounded;return bad;}scale=sigma;}
  if(!std::isnan(sigma))scale=std::max(scale,sigma);
  for(double& v:y)v=static_cast<double>((static_cast<long double>(v)-centre)/scale);
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
  slash_detail::Profile p{y,std::isnan(sigma)?sigma:sigma/scale};
  std::vector<double> grid;
  for(int i=0;i<=128;++i)grid.push_back(static_cast<double>((1-i/128.0L)*y.front()+(i/128.0L)*y.back()));
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
  SlashResult best=p(grid.front());bool refined=grid.size()==1;std::vector<double> ll;
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
    if(it==maxit)f.status=SlashStatus::IterationFailure;else refined=true;
    if(f.logLik>best.logLik)best=f;
  }
  if(!refined)best.status=SlashStatus::IterationFailure;
  if(boundary>=best.logLik-1e-8*n){bad.status=SlashStatus::CollapseLimit;return bad;}
  best.mu=centre+scale*best.mu;best.sigma=std::isnan(sigma)?scale*best.sigma:sigma;best.logLik-=n*std::log(scale);
  return best;
}
}
