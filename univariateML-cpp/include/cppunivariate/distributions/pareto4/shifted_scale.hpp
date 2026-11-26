#pragma once

namespace cppunivariate {
namespace pareto4_detail {

inline double excess_residual(double x,double min,double excess) {
  const double b=excess-x;
  return (x-(excess-b))+(-min-b);
}

// Form the small numerator before the division. In the close branch
// excess-scale is exact by Sterbenz, and cancellation with the residual
// is either exact or incurs only relative rounding in the small result.
inline double shifted_log(double x,double min,double scale) {
  const double excess=x-min;
  const double residual=excess_residual(x,min,excess);
  if(excess>=scale/2 && excess/2<=scale)
    return std::log1p(((excess-scale)+residual)/scale);
  return std::log(excess)-std::log(scale)+std::log1p(residual/excess);
}

inline double centered_log(double x,double anchor,double min) {
  const double y=anchor-min, delta=x-anchor;
  const double ratio=delta/y;
  if(std::abs(ratio)<.5) {
    const double correction=excess_residual(x,anchor,delta)/y-
        ratio*(excess_residual(anchor,min,y)/y);
    return std::log1p(ratio+correction);
  }
  const double excess=x-min;
  return std::log(excess)-std::log(y)+std::log1p(excess_residual(x,min,excess)/excess)-
      std::log1p(excess_residual(anchor,min,y)/y);
}

inline BurrMLE shifted_scale_fit(span x,double min,double scale,double a,double b,
                               double tol,int maxit) {
  namespace cert=burr_cert_detail;
  BurrMLE out{a,b,scale,NAN,0,BurrStatus::NumericalFailure};
  std::vector<double> z;
  std::vector<cert::Interval> box;
  cert::Sum logx;
  double unit=0,jac=0;
  int zeros=0;
  bool all_ge=true,constant=true;
  for(double v:x) {
    const double y=v-min, residual=excess_residual(v,min,y);
    const double l=shifted_log(v,min,scale);
    if(!std::isfinite(l))return out;
    const bool close=y>=scale/2 && y/2<=scale;
    const double error=16*std::numeric_limits<double>::epsilon()*
        (close?std::abs(l):std::abs(std::log(y))+std::abs(std::log(scale))+std::abs(l))+
        std::numeric_limits<double>::denorm_min();
    z.push_back(l);
    box.emplace_back(l-error,l+error);
    unit=std::max(unit,std::abs(l));
    const double ly=std::log(y)+std::log1p(residual/y);
    jac-=ly;
    logx.add(cert::padded(ly,ly));
    const bool equal=y==scale && residual==0;
    zeros+=equal;
    all_ge=all_ge && (y>scale || (y==scale && residual>=0));
    constant=constant && v==x[0];
  }
  const double n=x.size();
  if(std::isnan(b) && (zeros==n || (std::isnan(a) && constant && !all_ge))) {
    out.status=BurrStatus::Unbounded;
    return out;
  }
  if(std::isnan(a) && std::isnan(b) && all_ge) {
    if(!(unit>0))return out;
    cert::Sum sum;
    double point=0;
    for(std::size_t i=0;i<z.size();++i) {
      point+=z[i]/unit;
      sum.add(box[i]/unit);
    }
    const auto ratio=n/sum.get();
    const double alpha=(n/point)/unit, penalty=zeros*std::log(2.);
    if(!ratio.valid() || (ratio.hi-ratio.lo)>tol*ratio.lo || !std::isfinite(alpha))return out;
    const auto value=n*(cert::log(ratio)-cert::log(cert::Interval(unit)))-n-logx.get()-penalty;
    if(!value.valid())return out;
    out.status=BurrStatus::AllGeOne;
    out.stop_detail=limit_detail({alpha,scale},n*std::log(alpha)-n+jac-penalty,StopModel::Pareto);
    out.stop_detail.loglik_lower=value.lo;
    out.stop_detail.loglik_upper=value.hi;
    out.stop_detail.candidates={{ {alpha,scale},value.lo,value.hi,StopModel::Pareto }};
    if(zeros)attach_endpoint_factor(out.stop_detail,-std::log(2.));
    return out;
  }
  if(std::isnan(b)) {
    if(!(unit>0))return out;
    for(std::size_t i=0;i<z.size();++i) {
      z[i]/=unit;
      box[i]=box[i]/unit;
    }
    const auto score=[&](double t) {
      if(std::isnan(a))return cert::scale_point(z,t).score;
      double g=n/t;
      for(double l:z)g+=l*(cert::sigmoid(-t*l)-a*cert::sigmoid(t*l));
      return g;
    };
    const auto bounds=[&](double lo,double hi,cert::Interval& total,
                          cert::Interval& g,cert::Interval& value) {
      const cert::Interval t(lo,hi);
      if(std::isnan(a))return cert::scale_bounds(box,t,total,g,value);
      cert::Sum sum;
      for(auto l:box)sum.add(l*(cert::sig(-(t*l))-a*cert::sig(t*l)));
      g=n/t+sum.get();
      return g.valid();
    };
    double lo=0,hi=1;
    const auto budget=certificate_budget(maxit);
    while(out.iterations<budget && score(hi)>0) {
      ++out.iterations;
      hi*=2;
    }
    while(out.iterations<budget && hi-lo>std::min(tol/32,1e-13)*hi) {
      ++out.iterations;
      const double mid=lo+(hi-lo)/2;
      if(mid==lo || mid==hi)break;
      if(score(mid)>0)lo=mid;else hi=mid;
    }
    const double t=lo+(hi-lo)/2, radius=tol*t/8;
    lo=t-radius;
    hi=t+radius;
    cert::Interval total,left,right,lower,upper,g,value;
    if(!(lo>0) || !bounds(lo,lo,total,left,lower) || !bounds(hi,hi,total,right,upper) ||
       !(left.lo>0 && right.hi<0) || !bounds(lo,hi,total,g,value))return out;
    if(std::isnan(a)) {
      const auto shape=n/total;
      if(!shape.valid() || shape.lo<=0 || shape.hi-shape.lo>tol*shape.lo)return out;
      const auto jacbox=-logx.get()-n*cert::log(cert::Interval(unit));
      const auto peak=value+jacbox;
      if(!peak.valid())return out;
      out.stop_detail.loglik_lower=peak.lo;
      out.stop_detail.loglik_upper=peak.hi;
      a=n/cert::scale_point(z,t).g;
    }
    b=t/unit;
  } else if(std::isnan(a)) {
    cert::Sum sum;
    double point=0;
    for(std::size_t i=0;i<z.size();++i) {
      const double lo=b*box[i].lo,hi=b*box[i].hi;
      sum.add(cert::sp(cert::Interval(lo,hi)));
      point+=cert::softplus(b*z[i]);
    }
    const auto shape=n/sum.get();
    if(!shape.valid() || shape.lo<=0 || shape.hi-shape.lo>tol*shape.lo)return out;
    a=n/point;
  }
  if(!(a>0 && b>0) || !std::isfinite(a) || !std::isfinite(b))return out;
  out.logLik=n*(std::log(a)+std::log(b))+jac;
  for(double v:x) {
    const double t=b*shifted_log(v,min,scale);
    out.logLik-=cert::softplus(-t)+a*cert::softplus(t);
  }
  if(!std::isfinite(out.logLik))return out;
  out.shape1=a;
  out.shape2=b;
  out.status=BurrStatus::Ok;
  if(std::isnan(out.stop_detail.loglik_lower))return out;
  out.stop_detail.candidates.push_back({{a,b,scale},out.stop_detail.loglik_lower,out.stop_detail.loglik_upper});
  return out;
}
}
}
