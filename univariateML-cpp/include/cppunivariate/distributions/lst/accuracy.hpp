#pragma once
#include "../st/local_accuracy.hpp"
#include <vector>
namespace cppunivariate { namespace t_local_accuracy {
// Location is measured in units of the returned physical scale. Shape uses
// its public relative error, including the offset in the log coordinate.
template<class Result,class Sample>
inline bool refine(Result& r,const Sample& x,double held_mu,double held_scale,
                   double held_shape,double tol,int maxit,bool standardized,double& shape){
 tol=std::min(tol,1e-10);
 const double origin=r.mu,unit=r.sigma;
 if(!(unit>0&&std::isfinite(unit)&&std::isfinite(origin)))return false;
 double low=x[0],high=x[0];for(double v:x){low=std::min(low,v);high=std::max(high,v);}
 double spread=high-low;if(!std::isfinite(spread))spread=high/2-low/2;
 const bool empirical_floor=!standardized&&!std::isnan(held_scale)&&spread>0;
 std::vector<double> y;for(double v:x)y.push_back((v-origin)/unit);
 const double floor=standardized?2:std::isnan(held_mu)?1:0;
 const bool hn=!std::isnan(held_shape),edge=!hn&&floor==1&&shape<1.001;
 std::array<double,4> p{{0,0,hn?0:edge?shape:std::log(shape-floor),0}};
 const auto objective=[&](const std::array<local_accuracy::Jet,4>& q){
  const auto d=hn?local_accuracy::Jet(shape):edge?q[2]:floor+local_accuracy::exp(q[2]);
  if(edge&&d.v<floor)return local_accuracy::Jet(NAN);
  const auto scale=local_accuracy::exp(q[1]);
  const auto physical=standardized?scale*local_accuracy::sqrt((d-2)/d):scale;
  local_accuracy::JetSum f(x.size()*(local_accuracy::log(physical)-st_accuracy::mode(d)));
  for(double v:y){auto z=(v-q[0])/physical;f.add(std::abs(z.v)>1e70?(d+1)/2*local_accuracy::softplus(2*local_accuracy::log(local_accuracy::abs(z))-local_accuracy::log(d)):st_accuracy::kernel(d,z*z));}return f.value();
 };
 const auto error=[&](const std::array<double,4>& q,const std::array<double,4>& step){
  auto s=step;if(!hn)s[2]*=edge?1/q[2]:std::exp(q[2])/(floor+std::exp(q[2]));
  const double location_scale=empirical_floor?spread/unit:std::exp(q[1]);
  return std::max(std::abs(s[0])/std::max(std::abs(origin/unit+q[0]),location_scale),std::max(std::abs(s[1]),std::abs(s[2])));
 };
 bool boundary=false;
 if(edge){auto at=p;at[2]=floor;boundary=objective(local_accuracy::variables(at)).g[2]>=0;if(boundary)p[2]=floor;}
 bool free_shape=!hn&&(!edge||!boundary);
 if(!local_accuracy::subset(p,{{std::isnan(held_mu),std::isnan(held_scale),free_shape,false}},tol,maxit,objective,error,r.stop_detail))return false;
 // Nuisance refinement may uncover a small inward shape score previously
 // hidden by a likelihood tie. Continue in natural shape coordinates.
 if(edge&&!free_shape&&objective(local_accuracy::variables(p)).g[2]<0){
  free_shape=true;
  if(!local_accuracy::subset(p,{{std::isnan(held_mu),std::isnan(held_scale),true,false}},tol,maxit,objective,error,r.stop_detail))return false;
 }
 r.mu=std::isnan(held_mu)?origin+unit*p[0]:held_mu;
 r.sigma=std::isnan(held_scale)?unit*std::exp(p[1]):held_scale;
 const double fitted_shape=hn?shape:edge?p[2]:floor+std::exp(p[2]);
 shape=fitted_shape;
 auto represented=p;represented[0]=(r.mu-origin)/unit;represented[1]=std::log(r.sigma/unit);
 if(!hn&&!edge)represented[2]=std::log(fitted_shape-floor);
 r.logLik=-objective(local_accuracy::variables(represented)).v-x.size()*std::log(unit);
 return (std::isfinite(r.logLik)&&(!std::isnan(held_mu)||local_accuracy::represented(r.mu,std::max(std::abs(r.mu),empirical_floor?spread:r.sigma),tol))&&(!std::isnan(held_scale)||local_accuracy::represented(r.sigma,r.sigma,tol)));
}
} }
