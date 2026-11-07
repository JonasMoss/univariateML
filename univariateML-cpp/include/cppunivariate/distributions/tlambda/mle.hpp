#pragma once
#include "../../core/work_budget.hpp"
#include "dpqr.hpp"
#include "../../core/span.hpp"
#include <algorithm>
#include <array>
#include <cmath>
#include <limits>
#include <vector>
#include <map>
namespace cppunivariate {
inline constexpr double kTlambdaLambdaLower = -1.0;
inline constexpr double kTlambdaLambdaUpper = 1.0;

enum class TlambdaStatus { Ok, InvalidData, InvalidShape, IterationFailure, Unbounded, CollapseLimit, InvalidSupport };
struct TlambdaResult { double mu,sigma,lambda,logLik; int iterations; TlambdaStatus status; unsigned restricted = 0; };
namespace tlambda_detail {
inline TlambdaResult uniform_fit(double lo,double hi,double n,double lambda,int maxit) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double mu=lo+(hi-lo)/2;
  double sigma=std::max(mu-lo,hi-mu)*lambda;
  // Outward rounding makes both observed endpoints belong to the represented
  // support even when midpoint and width cannot be represented simultaneously.
  for(int j=0;j<maxit && (!std::isfinite(distributions::dtlambda(lo,mu,sigma,lambda,true)) || !std::isfinite(distributions::dtlambda(hi,mu,sigma,lambda,true)));++j)
    sigma=std::nextafter(sigma,INFINITY);
  const double ll=-n*(std::log(sigma)+std::log(2/lambda));
  return {mu,sigma,lambda,ll,0,valid(mu,sigma,lambda) && std::isfinite(ll)?TlambdaStatus::Ok:TlambdaStatus::IterationFailure};
}
struct Point { std::array<double,3> t; double f; };
inline double likelihood(span x,double mu,double sigma,double lambda) {
  double ll=0;
  for(double v:x) ll+=distributions::dtlambda(v,mu,sigma,lambda,true);
  return ll;
}
// Multistart Nelder--Mead. Shape boundaries are separately optimized faces.
inline Point search(span x,std::array<double,3> start,bool fixed,double tol,int maxit,int& iterations,bool& converged,bool hold_mu=false,bool hold_sigma=false) {
  maxit = static_cast<int>(solver_budget(maxit));
  std::vector<int> index; if(!hold_mu)index.push_back(0);if(!hold_sigma)index.push_back(1);if(!fixed)index.push_back(2);
  const int dim=static_cast<int>(index.size());
  std::map<std::array<double,3>,double> evaluations;
  auto eval=[&](std::array<double,3> t) -> double {
    if((!fixed && !hold_mu && t[2]<-1) || (!fixed && t[2]>1) || !std::isfinite(t[0]) || !std::isfinite(t[1]) || !(std::exp(t[1])>0) || !std::isfinite(std::exp(t[1]))) return INFINITY;
    if(!hold_sigma) return -likelihood(x,t[0],std::exp(t[1]),t[2]);
    for(double v:t) if(std::isnan(v) || v==0) return -likelihood(x,t[0],std::exp(t[1]),t[2]);
    const auto found=evaluations.find(t);
    if(found!=evaluations.end()) return found->second;
    const double value=-likelihood(x,t[0],std::exp(t[1]),t[2]);
    if(evaluations.size()>=4096) evaluations.clear();
    evaluations.emplace(t,value);
    return value;
  };
  if(dim==0){converged=true;return {start,eval(start)};}
  std::vector<Point> v;
  v.push_back({start,eval(start)});
  for(int j=0;j<dim;++j) {auto s=start;s[index[j]]+=(index[j]==2?.08:.15);v.push_back({s,eval(s)});}
  auto order=[](const Point& a,const Point& b){return a.f<b.f;};
  converged=false;
  for(int it=0;it<maxit;++it) {
    ++iterations;
    std::sort(v.begin(),v.end(),order);
    double diameter=0;
    for(int j=1;j<=dim;++j) for(int k=0;k<dim;++k) diameter=std::max(diameter,std::abs(v[j].t[index[k]]-v[0].t[index[k]]));
    if(std::isfinite(v[dim].f) && diameter<tol && v[dim].f-v[0].f<tol*x.size()) {converged=true;break;}
    std::array<double,3> c=v[0].t;
    for(int k=0;k<dim;++k) {c[index[k]]=0;for(int j=0;j<dim;++j)c[index[k]]+=v[j].t[index[k]]/dim;}
    auto trial=[&](double a) {auto t=c;for(int k=0;k<dim;++k)t[index[k]]=c[index[k]]+a*(c[index[k]]-v[dim].t[index[k]]);return Point{t,eval(t)};};
    Point r=trial(1);
    if(r.f<v[0].f) {Point e=trial(2);v[dim]=e.f<r.f?e:r;}
    else if(r.f<v[dim-1].f) v[dim]=r;
    else {
      Point s=trial(r.f<v[dim].f?.5:-.5);
      if(s.f<std::min(r.f,v[dim].f)) v[dim]=s;
      else for(int j=1;j<=dim;++j) {for(int k=0;k<dim;++k)v[j].t[index[k]]=(v[j].t[index[k]]+v[0].t[index[k]])/2;v[j].f=eval(v[j].t);}
    }
  }
  std::sort(v.begin(),v.end(),order);
  return v[0];
}
}
inline TlambdaResult tlambda_mle_impl(span x,double tol=1e-7,int maxit=1000,double lambda=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double nan=std::numeric_limits<double>::quiet_NaN();
  TlambdaResult bad{nan,nan,lambda,nan,0,TlambdaStatus::InvalidData};
  const bool fixed=!std::isnan(lambda);
  if(fixed && !std::isfinite(lambda)) {bad.status=TlambdaStatus::InvalidShape;return bad;}
  if(x.size()<3 || !(tol>0) || !std::isfinite(tol) || maxit<1) return bad;
  std::vector<double> y(x.begin(),x.end());
  for(double v:y) if(!std::isfinite(v)) return bad;
  std::sort(y.begin(),y.end());
  std::size_t run=1;
  for(std::size_t j=1;j<y.size();++j) {run=y[j]==y[j-1]?run+1:1;if(fixed && lambda<0 ? run*std::abs(lambda)>=y.size()-run : 2*run>=y.size())return bad;}
  const double origin=y.front(),upper=y.back(),range=upper-origin,n=static_cast<double>(y.size());
  if(!(range>0) || !std::isfinite(range)) return bad;
  if(fixed && (lambda==1 || lambda==2)) return tlambda_detail::uniform_fit(y.front(),y.back(),n,lambda,maxit);
  double previous=origin;
  for(std::size_t j=0;j<y.size();++j) {
    const double raw=y[j];y[j]=(raw-origin)/range;
    if(j>0 && raw>previous && !(y[j]>y[j-1])) return bad;
    previous=raw;
  }
  const span z{y.data(),y.size()};
  double best=-INFINITY,unresolved=-INFINITY;
  TlambdaResult result=bad;
  // Exact uniform member, preferred within 1e-8*n log likelihood units.
  if(!fixed) {best=0;result={.5,.5,1,0,0,TlambdaStatus::Ok};}
  int iterations=0;
  bool any_converged=false;
  std::vector<double> shapes=fixed?std::vector<double>{lambda}:std::vector<double>{-1,-.75,-.5,-.25,0,.14,.3,.5,.7,.9,.99};
  for(double l:shapes) {
    const double q1=distributions::qtlambda(.25,0,1,l),q3=-q1;
    double s=(y[3*y.size()/4]-y[y.size()/4])/(q3-q1);
    s=std::max(s,1e-4);
    for(double centre:std::vector<double>{y[y.size()/4],y[y.size()/2],y[3*y.size()/4]}) {
      double scale=s;
      if(l>0) scale=std::max(scale,1.05*l*std::max(centre,1-centre));
      bool done=false;
      auto p=tlambda_detail::search(z,{centre,std::log(scale),l},fixed || l==-1,tol,maxit,iterations,done);
      any_converged=any_converged || done;
      const double ll=-p.f;
      if(!done && std::isfinite(ll)) unresolved=std::max(unresolved,ll);
      if(done && std::isfinite(ll) && ll>best+((result.lambda==1 || result.lambda== -1)?1e-8*n:1e-12)) {
        best=ll;result={p.t[0],std::exp(p.t[1]),p.t[2],ll,iterations,TlambdaStatus::Ok};
      }
    }
  }
  if(!any_converged || unresolved>best+1e-8*n || result.status!=TlambdaStatus::Ok) {bad.status=TlambdaStatus::IterationFailure;return bad;}
  if(result.lambda==1) {result=tlambda_detail::uniform_fit(origin,upper,n,1,maxit);result.iterations=iterations;return result;}
  result.mu=origin+range*result.mu;result.sigma*=range;
  result.logLik=tlambda_detail::likelihood(x,result.mu,result.sigma,result.lambda);
  if(!tlambda_detail::valid(result.mu,result.sigma,result.lambda) || !std::isfinite(result.logLik)) result.status=TlambdaStatus::IterationFailure;
  result.iterations=iterations;
  return result;
}
inline TlambdaResult tlambda_mle_impl(span x,double tol,int maxit,double lambda,double mu,double sigma) {
  maxit = static_cast<int>(solver_budget(maxit));
  if(std::isnan(mu)&&std::isnan(sigma))return tlambda_mle_impl(x,tol,maxit,lambda);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  TlambdaResult bad{nan,nan,lambda,nan,0,TlambdaStatus::InvalidData};
  const bool fm=!std::isnan(mu),fs=!std::isnan(sigma),fl=!std::isnan(lambda);
  if(fl&&!std::isfinite(lambda)){bad.status=TlambdaStatus::InvalidShape;return bad;}
  if(x.empty()||!(tol>0)||!std::isfinite(tol)||maxit<1)return bad;
  std::vector<double> y(x.begin(),x.end());for(double v:y)if(!std::isfinite(v))return bad;std::sort(y.begin(),y.end());
  double collapse=-INFINITY;
  if(!fs) {
    if(fm && !fl && std::count(y.begin(),y.end(),mu)>0){bad.status=TlambdaStatus::Unbounded;return bad;}
    const double k=static_cast<double>(std::count(y.begin(),y.end(),mu));
    const double exponent=(fl&&lambda<0)?(y.size()-k)/std::abs(lambda)-k:(fl?k==y.size()?-1:1:y.size()-2*k);
    if(exponent<0){bad.status=TlambdaStatus::Unbounded;return bad;}
    if(exponent==0){
      if(fl){bad.status=TlambdaStatus::CollapseLimit;return bad;}
      collapse=-3*k*std::log(2.0);for(double v:y)if(v!=mu)collapse-=2*std::log(std::abs(v-mu));
    }
  }
  const double origin=y.front()/2+y.back()/2;
  double unit=y.back()/2-y.front()/2;if(fs)unit=std::max(unit,sigma);if(fm)unit=std::max(unit,std::abs(mu-origin));
  if(!(unit>0)||!std::isfinite(unit))return bad;
  for(double& v:y)v=(v-origin)/unit;
  const double m=fm?(mu-origin)/unit:nan,s=fs?sigma/unit:nan,n=y.size();
  const span z{y.data(),y.size()};
  TlambdaResult best=bad;double value=-INFINITY,unresolved=-INFINITY;
  // The exact uniform member is feasible at the closed support endpoints.
  if(!fl||lambda==1||lambda==2) {
    const double l=fl?lambda:1;
    const double centre=fm?m:y.front()/2+y.back()/2;
    const double radius=std::max(std::abs(y.front()-centre),std::abs(y.back()-centre));
    const double scale=fs?s:radius*l;
    if(scale>0&&radius<=scale/l){value=-n*std::log(2*scale/l);best={centre,scale,l,value,0,TlambdaStatus::Ok};}
    if(fl) {
      if(best.status!=TlambdaStatus::Ok){bad.status=TlambdaStatus::InvalidSupport;return bad;}
      best.mu=fm?mu:origin+unit*best.mu;best.sigma=fs?sigma:unit*best.sigma;
      if(!fs)for(int j=0;j<maxit&&!std::isfinite(tlambda_detail::likelihood(x,best.mu,best.sigma,l));++j)best.sigma=std::nextafter(best.sigma,INFINITY);
      best.logLik=tlambda_detail::likelihood(x,best.mu,best.sigma,l);return best;
    }
  }
  if(fl&&fs&&lambda>0) {
    const double radius=fm?std::max(std::abs(y.front()-m),std::abs(y.back()-m)):(y.back()-y.front())/2;
    if(lambda<1 ? radius>=s/lambda : radius>s/lambda){bad.status=TlambdaStatus::InvalidSupport;return bad;}
  }
  const std::vector<double> shapes=fl?std::vector<double>{lambda}:std::vector<double>{-16,-8,-4,-2,-1,-.75,-.5,-.25,0,.14,.3,.5,.7,.9,.99};
  int iterations=0;
  for(double l:shapes)for(double centre:fm?std::vector<double>{m}:std::vector<double>{y[y.size()/4],y[y.size()/2],y[3*y.size()/4]}) {
    if(!fm && l< -1) continue;
    double scale=fs?s:.5;
    if(!fs&&l>0)scale=std::max(scale,1.05*l*std::max(std::abs(y.front()-centre),std::abs(y.back()-centre)));
    bool done=false;
    auto p=tlambda_detail::search(z,{centre,std::log(scale),l},fl||(!fm && l== -1),tol,maxit,iterations,done,fm,fs);
    if(!done)unresolved=std::max(unresolved,-p.f);
    if(done&&std::isfinite(p.f)&&-p.f>value+(best.lambda==1?1e-8*n:1e-12)){value=-p.f;best={p.t[0],std::exp(p.t[1]),p.t[2],-p.f,iterations,TlambdaStatus::Ok};}
  }
  if(best.status!=TlambdaStatus::Ok||unresolved>value+1e-8*n){bad.status=TlambdaStatus::IterationFailure;return bad;}
  best.mu=fm?mu:origin+unit*best.mu;best.sigma=fs?sigma:unit*best.sigma;
  if(!fs&&best.lambda==1)for(int j=0;j<maxit&&!std::isfinite(tlambda_detail::likelihood(x,best.mu,best.sigma,1));++j)best.sigma=std::nextafter(best.sigma,INFINITY);
  best.logLik=tlambda_detail::likelihood(x,best.mu,best.sigma,best.lambda);
  if(collapse>=best.logLik-1e-8*n){bad.status=TlambdaStatus::CollapseLimit;return bad;}
  best.iterations=iterations;return best;
}


inline TlambdaResult tlambda_mle(span x,double tol=1e-7,int maxit=1000,double lambda=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  auto r=tlambda_mle_impl(x,tol,maxit,lambda);
  if(r.status==TlambdaStatus::Ok && (std::isnan(lambda)) && r.lambda==kTlambdaLambdaLower) r.restricted|=16u;
  if(r.status==TlambdaStatus::Ok && (std::isnan(lambda)) && r.lambda==kTlambdaLambdaUpper) r.restricted|=32u;
  return r;
}
inline TlambdaResult tlambda_mle(span x,double tol,int maxit,double lambda,double mu,double sigma) {
  maxit = static_cast<int>(solver_budget(maxit));
  auto r=tlambda_mle_impl(x,tol,maxit,lambda,mu,sigma);
  if(r.status==TlambdaStatus::Ok && (std::isnan(lambda) && std::isnan(mu)) && r.lambda==kTlambdaLambdaLower) r.restricted|=16u;
  if(r.status==TlambdaStatus::Ok && (std::isnan(lambda)) && r.lambda==kTlambdaLambdaUpper) r.restricted|=32u;
  return r;
}
}
