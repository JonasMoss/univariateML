#pragma once
#include "dpqr.hpp"
#include "../sn/mle.hpp"
#include "../ht/mle.hpp"
#include "../hnorm/mle.hpp"
#include "../../core/bfgs.hpp"
#include "../../core/span.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>
namespace cppunivariate {
enum class StStatus {Ok, InvalidInput, Unbounded, ScaleBoundary, NumericalFailure, SkewNormalBoundary, RightHalfTBoundary, LeftHalfTBoundary, RightHalfNormalBoundary, LeftHalfNormalBoundary};
struct StResult {double xi,omega,alpha,nu,logLik;int iterations;StStatus status;unsigned restricted=0;unsigned reason=0;};
namespace st_detail {
struct Cost {
 span x;
 double fixed[4];
 bool free[4];
 int dim() const {return int(free[0])+int(free[1])+int(free[2])+int(free[3]);}
 void unpack(const double* p,double* v) const {
  int j=0;for(int i=0;i<4;++i)v[i]=free[i]?p[j++]:fixed[i];
  if(free[1])v[1]=std::exp(v[1]);
  if(free[3])v[3]=1+std::exp(v[3]);
 }
 double operator()(const double* p) const {
  double v[4];unpack(p,v);
  if(!valid(v[0],v[1],v[2],v[3]))return INFINITY;
  double ll=0;for(double y:x){double q=logdensity(y,v[0],v[1],v[2],v[3]);if(!std::isfinite(q))return INFINITY;ll+=q;}
  return -ll;
 }
 void gradient(const double* p,double* g) const {
  double v[4];unpack(p,v);double score[3]={0,0,0};
  for(double xval:x) {
   double z=(xval-v[0])/v[1],den=std::hypot(std::sqrt(v[3]),z);
   double h=v[2]*(z/den)*std::sqrt(v[3]+1);
   double logh=std::log(std::abs(h));
   double logcdf=student_detail::probability_log(logh,h<0,v[3]+1,true,true);
   double ratio=std::exp(log_t_density(logh,v[3]+1)-logcdf);
   double gz=-(v[3]+1)*z/(den*den)+ratio*v[2]*(v[3]/den/den)*(std::sqrt(v[3]+1)/den);
   score[0]-=gz/v[1];score[1]+=-1-z*gz;score[2]+=ratio*(z/den)*std::sqrt(v[3]+1);
  }
  int j=0;for(int i=0;i<3;++i)if(free[i])g[j++]=-score[i];
  if(free[3]) {
   std::vector<double> q(p,p+dim());double h=1e-4*(1+std::abs(p[j]));
   q[j]=p[j]+h;double a=(*this)(q.data());q[j]=p[j]-h;double b=(*this)(q.data());g[j]=(a-b)/(2*h);
  }
 }
};
inline std::vector<double> pack(const Cost& c,const double* v) {
 std::vector<double> p;for(int i=0;i<4;++i)if(c.free[i])p.push_back(i==1?std::log(v[i]):i==3?std::log(v[i]-1):v[i]);return p;
}
struct Candidate {double v[4]={0,1,0,2};double ll=-INFINITY;bool ok=false;int iterations=0;};
inline Candidate optimize(Cost c,const double* start,double tol,int maxit) {
 Candidate out;auto p=pack(c,start);
 if(p.empty()){double q[1]={0};out.ll=-c(q);for(int i=0;i<4;++i)out.v[i]=c.fixed[i];out.ok=std::isfinite(out.ll);return out;}
 BfgsControl ctl;ctl.grad_tol=std::min(tol,1e-5);ctl.maxit=maxit;ctl.reltol=1e-14;ctl.step_tol=1e-10;
 auto r=bfgs_minimize(c,p.data(),ctl);
 if(r.theta.empty()||!std::isfinite(r.f))return out;
 std::vector<double> g(r.theta.size());c.gradient(r.theta.data(),g.data());double norm=0;for(double q:g)norm=std::hypot(norm,q);
 if(!std::isfinite(norm)||norm>1e-3)return out;
 c.unpack(r.theta.data(),out.v);out.ll=-c(r.theta.data());out.iterations=r.iterations;out.ok=std::isfinite(out.ll);return out;
}
}
inline StResult st_mle(span data,double tol=1e-6,int maxit=200,double xi=special::kNaN,double omega=special::kNaN,double alpha=special::kNaN,double nu=special::kNaN) {
 const double nan=special::kNaN;StResult bad{nan,nan,nan,nan,nan,0,StStatus::InvalidInput};
 if(data.empty()||!(tol>0)||maxit<=0||(!std::isnan(xi)&&!std::isfinite(xi))||(!std::isnan(omega)&&(!std::isfinite(omega)||omega<=0))||(!std::isnan(alpha)&&!std::isfinite(alpha))||(!std::isnan(nu)&&(!std::isfinite(nu)||nu<=0)))return bad;
 std::vector<double> sorted(data.begin(),data.end());for(double v:sorted)if(!std::isfinite(v))return bad;std::sort(sorted.begin(),sorted.end());
 const std::size_t n=sorted.size();std::size_t maxrun=1,run=1;for(std::size_t i=1;i<n;++i){run=sorted[i]==sorted[i-1]?run+1:1;maxrun=std::max(maxrun,run);}
 if(std::isnan(omega)) {
  std::size_t k=std::isnan(xi)?maxrun:std::count(sorted.begin(),sorted.end(),xi);
  double floor=std::isnan(nu)?1:nu;
  if(k>floor*(n-k)){bad.status=StStatus::Unbounded;return bad;}
  if(k==floor*(n-k)){bad.status=StStatus::ScaleBoundary;return bad;}
 }
 double origin=sorted[n/2],unit=sorted.back()-sorted.front();
 if(!(unit>0)||!std::isfinite(unit)){if(std::isfinite(omega))unit=omega;else return bad;}
 std::vector<double> y;for(double x:data)y.push_back((x-origin)/unit);span sy{y.data(),y.size()};
 double fx=std::isnan(xi)?nan:(xi-origin)/unit,fw=std::isnan(omega)?nan:omega/unit;
 st_detail::Candidate best;double profile_best=-INFINITY;
 const std::vector<double> as=std::isnan(alpha)?std::vector<double>{-8,-3,-1,0,1,3,8}:std::vector<double>{alpha};
 const std::vector<double> ns=std::isnan(nu)?std::vector<double>{1,1.5,3,8,30,200}:std::vector<double>{nu};
 for(double a:as)for(double d:ns) {
  st_detail::Cost inner{sy,{fx,fw,a,d},{std::isnan(fx),std::isnan(fw),false,false}};
  double start[4]={0,0.5,a,d};auto p=st_detail::optimize(inner,start,tol,maxit);if(!p.ok)continue;
  profile_best=std::max(profile_best,p.ll);
  bool boundary=std::isnan(nu)&&d==1;
  st_detail::Cost full{sy,{fx,fw,alpha,boundary?1.:nu},{std::isnan(fx),std::isnan(fw),std::isnan(alpha),std::isnan(nu)&&!boundary}};
  auto q=st_detail::optimize(full,p.v,tol,maxit);
  if(q.ok && std::isnan(nu) && q.v[3]<=1+1e-6) {
   // A transformed interior iterate can round to the restriction. Check
   // its untransformed score rather than accepting a vanishing Jacobian.
   const double h=1e-4;double near=0;
   for(double v:y)near+=st_detail::logdensity(v,q.v[0],q.v[1],q.v[2],q.v[3]+h);
   const double score=(near-q.ll)/h;
   if(!std::isfinite(score)||(q.v[3]==1 ? score>1e-3 : std::abs(score)>1e-3))q.ok=false;
  }
  if(q.ok&&q.ll>best.ll)best=q;
 }
 if(!std::isfinite(profile_best)){bad.status=StStatus::NumericalFailure;bad.reason=1;return bad;}
 const double reference=std::max(best.ll,profile_best),tie=1e-8*n;
 StResult boundary{nan,nan,nan,nan,-INFINITY,0,StStatus::NumericalFailure};
 if(std::isnan(nu)) {
  auto sn=sn_mle(sy,tol,maxit,fx,fw,alpha);
  if(sn.status!=SnStatus::Ok&&sn.status!=SnStatus::RightHalfNormalBoundary&&sn.status!=SnStatus::LeftHalfNormalBoundary){bad.status=StStatus::NumericalFailure;bad.reason=2;return bad;}
  if(sn.status==SnStatus::Ok&&(sn.logLik>reference+tie||(best.ok&&best.v[3]>1e5&&sn.logLik>=reference-tie)))
   boundary={origin+unit*sn.xi,unit*sn.omega,sn.alpha,INFINITY,sn.logLik,sn.iterations,StStatus::SkewNormalBoundary};
  if((sn.status==SnStatus::RightHalfNormalBoundary||sn.status==SnStatus::LeftHalfNormalBoundary)&&sn.logLik>=reference-tie)
   boundary={origin+unit*sn.xi,unit*sn.omega,sn.alpha,INFINITY,sn.logLik,sn.iterations,sn.status==SnStatus::RightHalfNormalBoundary?StStatus::RightHalfNormalBoundary:StStatus::LeftHalfNormalBoundary};
 }
 if(std::isnan(alpha))for(double sign:{1.,-1.}) {
  std::vector<double> side;for(double v:y)side.push_back(sign*v);
  const double mu=std::isnan(fx)?nan:sign*fx;
  bool feasible=true;if(std::isfinite(mu))for(double v:side)if(v<mu)feasible=false;
  if(!feasible)continue;
  span sx{side.data(),side.size()};
  double face=-INFINITY;HtResult winner{};bool failed=false;
  auto value=[&](double d){auto h=ht_mle(sx,mu,d,fw);if(h.status!=HtStatus::Ok&&h.status!=HtStatus::NormalLimit)failed=true;if((h.status==HtStatus::Ok||h.status==HtStatus::NormalLimit)&&h.logLik>face){face=h.logLik;winner=h;}return (h.status==HtStatus::Ok||h.status==HtStatus::NormalLimit)?h.logLik:-INFINITY;};
  if(!std::isnan(nu))value(nu);
  else if(std::isnan(mu))value(nan);
  else {
   std::vector<double> grid;for(int j=0;j<=48;++j)grid.push_back(j*.25);
   std::vector<double> vals;for(double t:grid)vals.push_back(value(std::exp(t)));
   for(std::size_t j=1;j+1<grid.size();++j)if(vals[j]>=vals[j-1]&&vals[j]>=vals[j+1]){
    double l=grid[j-1],r=grid[j+1],a=r-(r-l)*.6180339887498949,b=l+(r-l)*.6180339887498949;
    double fa=value(std::exp(a)),fb=value(std::exp(b));
    for(int k=0;k<80&&r-l>1e-9;++k){if(fa>fb){r=b;b=a;fb=fa;a=r-(r-l)*.6180339887498949;fa=value(std::exp(a));}else{l=a;a=b;fa=fb;b=l+(r-l)*.6180339887498949;fb=value(std::exp(b));}}
    if(r-l>1e-9)failed=true;
   }
   auto h=hnorm_mle(sx,mu,fw);
   if(h.status==HnormStatus::Ok&&h.logLik>face){face=h.logLik;winner={INFINITY,h.sigma,h.logLik,h.iterations,HtStatus::NormalLimit,0,h.mu};}
  }
  if(failed){bad.status=StStatus::NumericalFailure;bad.reason=3;return bad;}
  if(std::isfinite(mu))for(double v:side)if(v==mu)face-=std::log(2.);
  if(face>=reference-tie&&face>boundary.logLik){bool normal=winner.status==HtStatus::NormalLimit;boundary={origin+unit*sign*winner.mu,unit*winner.sigma,sign*INFINITY,winner.nu,face,winner.iterations,normal?(sign>0?StStatus::RightHalfNormalBoundary:StStatus::LeftHalfNormalBoundary):(sign>0?StStatus::RightHalfTBoundary:StStatus::LeftHalfTBoundary)};}
 }
 if(std::isfinite(boundary.logLik)){boundary.logLik-=n*std::log(unit);return boundary;}
 if(!best.ok||profile_best>best.ll+tie){bad.status=StStatus::NumericalFailure;bad.reason=4;return bad;}
 if(std::abs(best.v[2])>1e5||best.v[3]>1e6){bad.status=StStatus::NumericalFailure;bad.reason=5;return bad;}
 return {std::isfinite(xi)?xi:origin+unit*best.v[0],std::isfinite(omega)?omega:unit*best.v[1],best.v[2],best.v[3],best.ll-n*std::log(unit),best.iterations,StStatus::Ok,(std::isnan(nu)&&best.v[3]==1)?64u:0u};
}
}
