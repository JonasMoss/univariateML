#pragma once
#include "dpqr.hpp"
#include "../../core/span.hpp"
#include <algorithm>
#include <array>
#include <cmath>
#include <limits>
#include <vector>
namespace cppunivariate {
enum class TlambdaStatus { Ok, InvalidData, InvalidShape, IterationFailure };
struct TlambdaResult { double mu,sigma,lambda,logLik; int iterations; TlambdaStatus status; };
namespace tlambda_detail {
inline TlambdaResult uniform_fit(double lo,double hi,double n,double lambda) {
  const double mu=lo+(hi-lo)/2;
  double sigma=std::max(mu-lo,hi-mu)*lambda;
  // Outward rounding makes both observed endpoints belong to the represented
  // support even when midpoint and width cannot be represented simultaneously.
  for(int j=0;j<8 && (!std::isfinite(distributions::dtlambda(lo,mu,sigma,lambda,true)) || !std::isfinite(distributions::dtlambda(hi,mu,sigma,lambda,true)));++j)
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
inline Point search(span x,std::array<double,3> start,bool fixed,double tol,int maxit,int& iterations,bool& converged) {
  const int dim=fixed?2:3;
  auto eval=[&](std::array<double,3> t) -> double {
    if(t[2]<-1 || t[2]>1 || !std::isfinite(t[0]) || !std::isfinite(t[1]) || !(std::exp(t[1])>0) || !std::isfinite(std::exp(t[1]))) return INFINITY;
    return -likelihood(x,t[0],std::exp(t[1]),t[2]);
  };
  std::vector<Point> v;
  v.push_back({start,eval(start)});
  for(int j=0;j<dim;++j) {auto s=start;s[j]+=(j==2?.08:.15);v.push_back({s,eval(s)});}
  auto order=[](const Point& a,const Point& b){return a.f<b.f;};
  converged=false;
  for(int it=0;it<maxit;++it) {
    ++iterations;
    std::sort(v.begin(),v.end(),order);
    double diameter=0;
    for(int j=1;j<=dim;++j) for(int k=0;k<dim;++k) diameter=std::max(diameter,std::abs(v[j].t[k]-v[0].t[k]));
    if(std::isfinite(v[dim].f) && diameter<tol && v[dim].f-v[0].f<tol*x.size()) {converged=true;break;}
    std::array<double,3> c=v[0].t;
    for(int k=0;k<dim;++k) {c[k]=0;for(int j=0;j<dim;++j)c[k]+=v[j].t[k]/dim;}
    auto trial=[&](double a) {auto t=c;for(int k=0;k<dim;++k)t[k]=c[k]+a*(c[k]-v[dim].t[k]);return Point{t,eval(t)};};
    Point r=trial(1);
    if(r.f<v[0].f) {Point e=trial(2);v[dim]=e.f<r.f?e:r;}
    else if(r.f<v[dim-1].f) v[dim]=r;
    else {
      Point s=trial(r.f<v[dim].f?.5:-.5);
      if(s.f<std::min(r.f,v[dim].f)) v[dim]=s;
      else for(int j=1;j<=dim;++j) {for(int k=0;k<dim;++k)v[j].t[k]=(v[j].t[k]+v[0].t[k])/2;v[j].f=eval(v[j].t);}
    }
  }
  std::sort(v.begin(),v.end(),order);
  return v[0];
}
}
inline TlambdaResult tlambda_mle(span x,double tol=1e-7,int maxit=1000,double lambda=std::numeric_limits<double>::quiet_NaN()) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  TlambdaResult bad{nan,nan,lambda,nan,0,TlambdaStatus::InvalidData};
  const bool fixed=!std::isnan(lambda);
  if(fixed && (!std::isfinite(lambda) || lambda< -1 || (lambda>1 && lambda!=2))) {bad.status=TlambdaStatus::InvalidShape;return bad;}
  if(x.size()<3 || !(tol>0) || !std::isfinite(tol) || maxit<1) return bad;
  std::vector<double> y(x.begin(),x.end());
  for(double v:y) if(!std::isfinite(v)) return bad;
  std::sort(y.begin(),y.end());
  std::size_t run=1;
  for(std::size_t j=1;j<y.size();++j) {run=y[j]==y[j-1]?run+1:1;if(2*run>=y.size())return bad;}
  const double origin=y.front(),upper=y.back(),range=upper-origin,n=static_cast<double>(y.size());
  if(!(range>0) || !std::isfinite(range)) return bad;
  if(fixed && (lambda==1 || lambda==2)) return tlambda_detail::uniform_fit(y.front(),y.back(),n,lambda);
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
  if(result.lambda==1) {result=tlambda_detail::uniform_fit(origin,upper,n,1);result.iterations=iterations;return result;}
  result.mu=origin+range*result.mu;result.sigma*=range;
  result.logLik=tlambda_detail::likelihood(x,result.mu,result.sigma,result.lambda);
  if(!tlambda_detail::valid(result.mu,result.sigma,result.lambda) || !std::isfinite(result.logLik)) result.status=TlambdaStatus::IterationFailure;
  result.iterations=iterations;
  return result;
}
}
