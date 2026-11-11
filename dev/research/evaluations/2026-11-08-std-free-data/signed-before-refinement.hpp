// Constrained normal-face proof. Failure leaves the legacy decision available.
// Exact-arithmetic inequalities with t_cert_detail's practical margins.
#pragma once
#include "shape_certificate.hpp"
namespace cppunivariate {
namespace t_signed_detail {
struct Result {
  bool proved=false,budget_exhausted=false;
  int evaluations=0;
  double coefficient_lower=NAN,coefficient_upper=NAN,remainder=NAN;
  double cutoff=NAN,lower_shape=NAN;
  StopDetail detail{};
};
inline t_cert_detail::I coefficient(const std::vector<t_cert_detail::I>& q,bool variance) {
  t_cert_detail::Sum first,second;
  for(auto z:q){first.add(z);second.add(t_cert_detail::sq(z));}
  return (second.get()-first.get()*(variance?6.:2.)+
    t_cert_detail::I(q.size())*(variance?3.:-1.))*.25;
}
// R(u)/u^2 = integral_0^1 t/(1+ut)^2 dt is decreasing.
inline t_cert_detail::I remainder_ratio(t_cert_detail::I u){
  if(!u.valid()||u.hi<0)return t_cert_detail::bad();
  const auto at=[](double z){
    if(z==0)return t_cert_detail::I(.5);
    if(z<1e-100)return t_cert_detail::pad(.5-2*z/3,.5);
    return t_cert_detail::shape_remainder(t_cert_detail::I(z))/t_cert_detail::sq(t_cert_detail::I(z));
  };
  const auto low=at(u.hi),high=at(std::max(0.,u.lo));
  if(!low.valid()||!high.valid())return t_cert_detail::bad();
  return {std::max(0.,low.lo),std::min(.5,high.hi)};
}
// Combine the scaled gamma score before interval evaluation at infinity.
inline t_cert_detail::I scaled_gamma(t_cert_detail::I v,bool sd){
  const auto r=sd?v-2:v;
  if(!v.valid()||!(v.lo>0)||!r.valid()||!(r.lo>0))return t_cert_detail::bad();
  const auto inverse=t_cert_detail::I(1)/v;
  const auto error=t_cert_detail::sq(t_cert_detail::I(1)/v.lo)*.125;
  t_cert_detail::I bound((t_cert_detail::I(.25)-error).lo,.25);
  auto natural=t_shape_detail::derivative(v,3)*t_cert_detail::sq(v);
  if(sd){
    auto a=t_cert_detail::I(1)-inverse*2;
    a.lo=std::max(0.,a.lo);a.hi=std::min(1.,a.hi);
    if(!a.valid())return t_cert_detail::bad();
    const auto f=[](double z){return t_cert_detail::I(z)*z*.25-z;};
    bound={(f(a.hi)-t_cert_detail::sq(a)*error).lo,f(a.lo).hi};
    natural=(t_shape_detail::derivative(v,3)-t_cert_detail::I(1)/(v*r))*t_cert_detail::sq(r);
  }
  if(!bound.valid())return t_cert_detail::bad();
  if(natural.valid())bound={std::max(bound.lo,natural.lo),std::min(bound.hi,natural.hi)};
  return bound;
}
struct Engine {
  std::vector<t_cert_detail::I> y,q;
  bool free_scale=false,variance=false; int maxit=0;
  double n=0,minimum=INFINITY,range=0,normal_mu=NAN,normal_scale=NAN;
  t_cert_detail::I normal,constant;
  Result out;
  bool spend(){
    if(out.evaluations>=certificate_budget(maxit)){out.budget_exhausted=true;return false;}
    ++out.evaluations;return true;
  }
  // Positive location score; for free scale, the strictly decreasing
  // log-scale score. h is 1/nu (ordinary) or 1/(nu-2) (held sd).
  t_cert_detail::I nuisance(t_cert_detail::I h,t_cert_detail::I p,bool derivative=false) const {
    t_cert_detail::Sum sum;
    const auto multiplier=free_scale?t_cert_detail::exp(p*(-2)):t_cert_detail::I(1);
    for(std::size_t j=0;j<q.size();++j){
      const auto r=free_scale?t_cert_detail::I():y[j]-p;
      auto z=free_scale?q[j]*multiplier:t_cert_detail::sq(r);
      z.lo=std::max(0.,z.lo);
      auto den=t_cert_detail::I(1)+z*h;den.lo=std::max(1.,den.lo);
      if(free_scale)sum.add(derivative?-z*(t_cert_detail::I(1)+h)*2/den/den:(z-1)/den);
      else sum.add(derivative?-(t_cert_detail::I(1)-z*h)/den/den:r/den);
    }
    return sum.get();
  }
  std::vector<t_cert_detail::I> squares(t_cert_detail::I p) const {
    std::vector<t_cert_detail::I> z;z.reserve(q.size());
    const auto multiplier=free_scale?t_cert_detail::exp(p*(-2)):t_cert_detail::I(1);
    for(std::size_t j=0;j<q.size();++j){
      auto v=free_scale?q[j]*multiplier:t_cert_detail::sq(y[j]-p);
      v.lo=std::max(0.,v.lo);z.push_back(v);
    }
    return z;
  }
  t_cert_detail::I value(t_cert_detail::I v,t_cert_detail::I p) const {
    t_shape_detail::Engine e(0,1,variance&&!free_scale,0);
    e.q=squares(p);e.constant=constant-(free_scale?p*n:t_cert_detail::I());
    return e.value(v);
  }
  t_cert_detail::I shape_score(t_cert_detail::I v,t_cert_detail::I p) const {
    const bool sd=variance&&!free_scale;
    const auto r=sd?v-2:v;
    t_cert_detail::Sum sum;
    for(auto z:squares(p)){
      auto u=z/r;u.lo=std::max(0.,u.lo);
      auto den=t_cert_detail::I(1)+u;den.lo=std::max(1.,den.lo);
      auto z2=t_cert_detail::sq(z);z2.lo=std::max(0.,z2.lo);
      sum.add(-z2*remainder_ratio(u)*.5+z/den*(sd?1.5:.5));
    }
    return scaled_gamma(v,sd)*n+sum.get();
  }
  // All conditional scale roots for every shape in v, not just the midpoint.
  bool scale_box(t_cert_detail::I v,t_cert_detail::I& box,bool inherited){
    const auto h=t_cert_detail::I(1)/v;
    if(!inherited){
    double p=0;
    for(int it=0;it<maxit;++it){
      if(!spend())return false;
      auto g=nuisance(1/v.mid(),p),d=nuisance(1/v.mid(),p,true);
      if(!g.valid()||!d.valid()||!(d.hi<0))return false;
      const double step=g.mid()/d.mid();
      if(!std::isfinite(step))return false;
      p-=std::max(-1.,std::min(1.,step));
      if(std::abs(step)<1e-10)break;
    }
    double radius=.02;
    bool bracket=false;
    for(int it=0;it<maxit;++it){
      if(!spend())return false;
      box={p-radius,p+radius};
      auto left=nuisance(h,box.lo),right=nuisance(h,box.hi);
      if(left.valid()&&right.valid()&&left.lo>0&&right.hi<0){bracket=true;break;}
      radius*=2;if(!std::isfinite(radius))return false;
    }
    if(!bracket)return false;
    }
    for(int it=0;it<maxit;++it){
      if(!spend())return false;
      const auto d=nuisance(h,box,true),g=nuisance(h,box.mid());
      if(!d.valid()||!g.valid()||!(d.hi<0))break;
      const auto image=t_cert_detail::I(box.mid())-g/d;
      if(!image.valid())break;
      t_cert_detail::I next(std::max(box.lo,image.lo),std::min(box.hi,image.hi));
      if(!next.valid())return false;
      const double old=box.hi-box.lo;box=next;
      if(box.hi-box.lo>=old*.8||box.hi-box.lo<1e-11)break;
    }
    return true;
  }
  bool tail(double& H){
    const bool sd=variance&&!free_scale;
    const auto A=coefficient(q,sd);
    out.coefficient_lower=A.lo;out.coefficient_upper=A.hi;
    if(!A.valid()||!(A.hi<0))return false;
    const double rho=.05;
    t_cert_detail::Sum second,lipschitz;
    for(std::size_t j=0;j<q.size();++j){
      auto z=free_scale?q[j]*t_cert_detail::exp(t_cert_detail::I(2*rho)):
        t_cert_detail::sq(y[j]+t_cert_detail::I(-rho,rho));
      z.lo=std::max(0.,z.lo);second.add(t_cert_detail::sq(z));
      if(!free_scale)lipschitz.add((y[j]+t_cert_detail::I(-rho,rho))*((sd?3.:1.)-z));
    }
    t_cert_detail::Sum fourth;for(auto z:q)fourth.add(t_cert_detail::sq(z));
    const auto scale_gradient=t_cert_detail::I(n)*t_cert_detail::exp({-2*rho,2*rho})-
      fourth.get()*t_cert_detail::exp({-4*rho,4*rho});
    const double M=free_scale?scale_gradient.mag():lipschitz.get().mag();
    const double curvature=free_scale?(t_cert_detail::I(2*n)*t_cert_detail::exp(-2*rho)).lo:n;
    const auto R=second.get()*(sd?.75:.25)+n*(sd?.5:0.)+n/192;
    const auto K=R+t_cert_detail::I(M)*M/(2*curvature);
    if(!K.valid()||!(K.hi>0))return false;
    H=std::min(.125,-A.hi/(2*K.hi));
    if(!free_scale)H=std::min(H,.25/(1+range*range));
    for(int it=0;it<maxit;++it){
      if(!spend()||!(H>0)||!std::isfinite(1/H))return false;
      const auto left=nuisance({0,H},-rho),right=nuisance({0,H},rho);
      const auto sign=A+t_cert_detail::I(H)*K;
      if(left.valid()&&right.valid()&&left.lo>0&&right.hi<0&&sign.valid()&&sign.hi<0){
        out.remainder=K.hi;out.cutoff=(t_cert_detail::I(1)/H+(sd?2.:0.)).hi;return true;
      }
      H*=.5;
    }
    return false;
  }
};
inline Result solve(span x,double mu,double scale,bool variance,int maxit){
  Engine e;e.maxit=static_cast<int>(solver_budget(maxit));e.n=x.size();e.variance=variance;
  e.free_scale=std::isnan(scale);
  if(x.size()<2||e.maxit<1||e.free_scale==std::isnan(mu))return e.out;
  const double unit=e.free_scale?std::max(std::abs(x[0]/2-mu/2)*2,std::abs(x[x.size()-1]/2-mu/2)*2):scale;
  double u=unit;
  if(e.free_scale)for(double z:x)u=std::max(u,std::abs(z/2-mu/2)*2);
  if(!(u>0)||!std::isfinite(u))return e.out;
  t_cert_detail::Sum mean,second;
  const double origin=e.free_scale?mu:x[0];
  for(double z:x){
    if(!std::isfinite(z))return e.out;
    auto y=(t_cert_detail::I(z)-origin)/u;
    if(!y.valid())return e.out;
    e.y.push_back(y);mean.add(y);second.add(t_cert_detail::sq(y));
  }
  if(e.free_scale){
    const auto var=second.get()/e.n;
    if(!var.valid()||!(var.lo>0))return e.out;
    e.normal_mu=mu;e.normal_scale=u*t_cert_detail::root(var).mid();
    e.constant=t_cert_detail::I(-e.n)*(.5*std::log(2*pi)+std::log(u)+t_cert_detail::log(var)*.5);
    e.normal=e.constant-e.n*.5;
    for(auto y:e.y){auto q=t_cert_detail::sq(y)/var;if(!q.valid()||!(q.lo>0))return e.out;e.q.push_back(q);e.minimum=std::min(e.minimum,q.lo);}
  }else{
    const auto m=mean.get()/e.n;
    e.normal_mu=(t_cert_detail::I(origin)+m*u).mid();e.normal_scale=scale;
    e.constant=t_cert_detail::I(-e.n)*(.5*std::log(2*pi)+std::log(scale));
    t_cert_detail::Sum q;
    double low=INFINITY,high=-INFINITY;
    for(auto& y:e.y){y=y-m;auto z=t_cert_detail::sq(y);z.lo=std::max(0.,z.lo);e.q.push_back(z);q.add(z);low=std::min(low,y.lo);high=std::max(high,y.hi);}
    e.normal=e.constant-q.get()*.5;e.range=high-low;
  }
  const auto witness=[&](double v,double p){
    StopDetail point;
    const double m=e.free_scale?mu:e.normal_mu+scale*p;
    double physical=e.free_scale?e.normal_scale*std::exp(p):scale;
    if(variance&&e.free_scale)physical*=std::sqrt(v/(v-2));
    t_shape_detail::retain_point(x,m,physical,v,variance,point);
    return point;
  };
  double H=0;if(!e.normal.valid()||!e.tail(H))return e.out;
  if(!e.spend())return e.out;
  const auto initial=witness(variance?3.:1.,0);
  if(!initial.candidates.empty()&&initial.loglik_lower>e.normal.hi){e.out.detail=initial;return e.out;}
  const double floor=variance?2.:(e.free_scale?0.:1.);
  double lower=floor,offset=variance&&!e.free_scale?2.:0.;
  if(e.free_scale&&!variance){
    t_cert_detail::Sum logs;for(auto z:e.q)logs.add(t_cert_detail::log(z));
    lower=std::min(1.,std::exp((logs.get().lo-e.n-2)/e.n));
    const auto bound=t_cert_detail::log(t_cert_detail::I(lower))*(e.n*.5)-logs.get()*.5+e.n*.5;
    if(!(lower>0)||!bound.valid()||!(bound.hi<0))return e.out;
  }
  if(!e.free_scale&&variance){
    std::vector<double> sorted(x.begin(),x.end());std::sort(sorted.begin(),sorted.end());
    double w=0;
    const std::size_t cap=(2*sorted.size()-1)/3;
    std::vector<std::size_t> windows;
    for(std::size_t j=1;j<=cap;){windows.push_back(j);if(j>cap/2)break;j*=2;}
    for(auto j:{sorted.size()/3,sorted.size()/2,cap})if(j>0&&j<=cap)windows.push_back(j);
    std::sort(windows.begin(),windows.end());windows.erase(std::unique(windows.begin(),windows.end()),windows.end());
    for(auto j:windows){
      if(!e.spend())return e.out;
      double gap=INFINITY;
      for(std::size_t i=0;i+j<sorted.size();++i){
        const auto distance=t_cert_detail::I(sorted[i+j])-sorted[i];
        if(!distance.valid())return e.out;
        gap=std::min(gap,distance.lo);
      }
      const auto Q=t_cert_detail::sq(t_cert_detail::I(gap)/(4*scale));
      if(!Q.valid()||!(Q.lo>0))continue;
      const double exponent=e.n-1.5*j;
      const auto C=t_cert_detail::I(-e.n)*std::log(scale)-e.n*.5*std::log(2*pi)+e.n*.5*std::log(3.)-
        t_cert_detail::log(Q)*(1.5*(e.n-j));
      const double trial=std::min({1.,Q.lo,std::exp((e.normal.lo-C.hi-1)/exponent)});
      if(!(trial>0))continue;
      const double rounded=(2+trial)-2;
      const auto bound=C+t_cert_detail::log(t_cert_detail::I(rounded))*exponent;
      if(rounded>0&&rounded<=std::min(1.,Q.lo)&&bound.valid()&&bound.hi<e.normal.lo)w=std::max(w,rounded);
    }
    if(!(w>0))return e.out;
    lower=2+w;
  }
  e.out.lower_shape=lower;
  struct Box {t_cert_detail::I shape,nuisance;bool included=false;};
  t_cert_detail::I location;
  if(!e.free_scale){double lo=INFINITY,hi=-INFINITY;for(auto y:e.y){lo=std::min(lo,y.lo);hi=std::max(hi,y.hi);}location={lo,hi};}
  std::vector<Box> pending{{{lower-offset,e.out.cutoff-offset},location}};
  while(!pending.empty()){
    if(!e.spend())return e.out;
    auto b=pending.back();pending.pop_back();
    const auto v=b.shape+offset;
    bool valid=true;
    if(e.free_scale){valid=e.scale_box(v,b.nuisance,b.included);b.included=valid;}
    else {
      const auto h=t_cert_detail::I(1)/(v-(variance?2.:0.));
      const auto g=e.nuisance(h,b.nuisance);
      if(g.valid()&&(g.lo>0||g.hi<0))continue;
    }
    if(e.out.budget_exhausted)return e.out;
    if(valid){
      const auto s=e.shape_score(v,b.nuisance);
      if(s.valid()&&(s.lo>0||(s.hi<0&&v.lo>floor)))continue;
      const auto value=e.value(v,b.nuisance);
      if(value.valid()&&value.hi<e.normal.lo)continue;
      if(!e.spend())return e.out;
      const auto point=witness(v.mid(),b.nuisance.mid());
      if(!point.candidates.empty()&&point.loglik_lower>e.normal.hi){e.out.detail=point;return e.out;}
    }
    const double width=std::log(b.shape.hi/b.shape.lo);
    if(e.free_scale||width>b.nuisance.hi-b.nuisance.lo){
      const double mid=std::sqrt(b.shape.lo)*std::sqrt(b.shape.hi);
      if(!(mid>b.shape.lo&&mid<b.shape.hi))return e.out;
      pending.push_back({{b.shape.lo,mid},b.nuisance,b.included});pending.push_back({{mid,b.shape.hi},b.nuisance,b.included});
    }else{
      const double mid=b.nuisance.mid();
      if(!(mid>b.nuisance.lo&&mid<b.nuisance.hi))return e.out;
      pending.push_back({b.shape,{b.nuisance.lo,mid}});pending.push_back({b.shape,{mid,b.nuisance.hi}});
    }
  }
  e.out.proved=true;
  e.out.detail=t_shape_detail::normal_point(x,e.normal_mu,e.normal_scale);
  e.out.detail.limit_model=StopModel::Norm;e.out.detail.limit_coef={e.normal_mu,e.normal_scale};
  e.out.detail.limit_loglik=e.normal.mid();e.out.detail.loglik_upper=e.normal.hi;
  return e.out;
}
} // namespace t_signed_detail
} // namespace cppunivariate
