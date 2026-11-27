// Finite joint Student profiles with one nuisance coefficient held.
#pragma once
#include "signed_face_certificate.hpp"
#include "../../core/tie_collapse.hpp"

namespace cppunivariate {
namespace t_cert_detail {
namespace held_profile {
struct Box { I nuisance,shape; };
struct System {
  std::array<I,2> score;
  std::array<std::array<I,2>,2> curvature;
};
inline bool includes(Box box,double df,const System& center,const System& system_bounds) {
  const double a=center.curvature[0][0].mid(),b=center.curvature[0][1].mid(),
    c=center.curvature[1][1].mid(),det=std::fma(a,c,-b*b);
  if(!(a<0 && det>0) || !std::isfinite(det))return false;
  const std::array<std::array<double,2>,2> inverse{{{{c/det,-b/det}},{{-b/det,a/det}}}};
  const std::array<double,2> radii{{std::min(-box.nuisance.lo,box.nuisance.hi),
    std::min(df-box.shape.lo,box.shape.hi-df)}};
  const std::array<double,2> extents{{box.nuisance.mag(),
    std::max(df-box.shape.lo,box.shape.hi-df)}};
  for(int i=0;i<2;++i) {
    const I move=I(inverse[i][0])*center.score[0]+I(inverse[i][1])*center.score[1];
    if(!move.valid() || !(radii[i]>0))return false;
    double defect=0;
    for(int j=0;j<2;++j) {
      I product;
      for(int k=0;k<2;++k)product=product+I(inverse[i][k])*system_bounds.curvature[k][j];
      if(!product.valid())return false;
      defect+=(I(i==j?1:0)-product).mag()*extents[j];
    }
    if(!(move.mag()+defect<radii[i]*(1-eps)))return false;
  }
  return true;
}
struct Cover {
  std::vector<I> data;
  I constant;
  bool variance;
  int maxit;
  std::int64_t limit;
  int work=0;

  bool spend() {
    if(work>=limit)return false;
    ++work;
    return true;
  }

  t_shape_detail::Engine profile(I location) const {
    t_shape_detail::Engine result(0,1,variance,maxit);
    result.constant=constant;
    result.q.reserve(data.size());
    for(I value:data) {
      I squared=sq(value-location);
      squared.lo=std::max(0.,squared.lo);
      result.q.push_back(squared);
    }
    return result;
  }

  System system(Box box,bool curvature) const {
    const I df=box.shape, width=variance?df-2:df;
    Sum location,location_location,location_shape;
    for(I value:data) {
      const I residual=value-box.nuisance;
      const I squared=sq(residual),denominator=width+squared;
      location.add(residual/denominator*(df+1));
      if(curvature) {
        const I denominator2=sq(denominator);
        location_location.add((squared-width)*(df+1)/denominator2);
        location_shape.add(residual*(squared-(variance?3.:1.))/denominator2);
      }
    }
    const auto shape=profile(box.nuisance);
    System result{};
    result.score={{location.get(),shape.score(df,false)}};
    if(curvature) {
      result.curvature[0]={{location_location.get(),location_shape.get()}};
      result.curvature[1]={{result.curvature[0][1],shape.score(df,true)}};
    }
    return result;
  }


  bool excludes(Box initial,Box basin,double floor,double target) {
    std::vector<Box> pending{initial};
    while(!pending.empty()) {
      if(!spend())return false;
      const Box box=pending.back();pending.pop_back();
      if(box.nuisance.lo>=basin.nuisance.lo && box.nuisance.hi<=basin.nuisance.hi &&
         box.shape.lo>=basin.shape.lo && box.shape.hi<=basin.shape.hi)continue;
      const auto likelihood=profile(box.nuisance).value(box.shape);
      if(likelihood.valid() && likelihood.hi<target)continue;
      if(!spend())return false;
      const auto score=system(box,false).score;
      if(score[0].valid() && (score[0].lo>0 || score[0].hi<0))continue;
      if(score[1].valid() && (score[1].lo>0 || (score[1].hi<0 && box.shape.lo>floor)))continue;
      Box left=box,right=box;
      const double offset=variance?2.:0.;
      const double shape_width=std::log((box.shape.hi-offset)/(box.shape.lo-offset));
      if(box.nuisance.hi-box.nuisance.lo>=shape_width) {
        const double mid=box.nuisance.mid();
        if(!(mid>box.nuisance.lo && mid<box.nuisance.hi))return false;
        left.nuisance.hi=right.nuisance.lo=mid;
      } else {
        const double mid=offset+std::sqrt(box.shape.lo-offset)*std::sqrt(box.shape.hi-offset);
        if(!(mid>box.shape.lo && mid<box.shape.hi))return false;
        left.shape.hi=right.shape.lo=mid;
      }
      pending.push_back(right);pending.push_back(left);
    }
    return true;
  }
};

inline int certify(span x,double mu,double scale,double df,bool variance,double tol,
    int maxit,std::int64_t budget) {
  const double floor=variance?2.:1.;
  tol=std::min(tol,.01);
  if(!(df>floor) || !(scale>0) || !std::isfinite(df) || budget<=0)return 0;
  Cover cover{{},I(),variance,maxit,budget};
  Sum average,ss;
  I location(INFINITY,-INFINITY);
  for(double v:x) {
    const auto y=(I(v)-mu)/scale;
    if(!y.valid())return 0;
    cover.data.push_back(y);average.add(y);
    location.lo=std::min(location.lo,y.lo);location.hi=std::max(location.hi,y.hi);
  }
  const double n=x.size();
  if(!(location.lo<location.hi) || !location.valid())return 0;
  cover.constant=-(log(I(scale))+.5*std::log(2*pi))*n;
  if(!cover.spend())return 0;
  const auto witness=cover.profile(0).value(df);
  if(!witness.valid())return 0;
  const I center_normal=average.get()/n;
  for(I y:cover.data)ss.add(sq(y-center_normal));
  const auto normal=cover.constant-ss.get()*.5;
  if(!normal.valid() || !(witness.lo>normal.hi))return 0;
  Sum fourth_bound;
  for(I y:cover.data)fourth_bound.add(sq(sq(y-location)));
  const auto fourth=fourth_bound.get();
  double upper=0;
  for(int j=0;j<maxit;++j) {
    if(!cover.spend())return 0;
    upper=std::ldexp(std::max(df*2,4.),j);
    if(!std::isfinite(upper))return 0;
    const I h=I(1)/(I(upper)-(variance?2.:0.));
    const auto bound=normal+fourth*h*(I(1)+h*(variance?3.:1.))*.25+(variance?h*n:I());
    if(bound.valid() && bound.hi<witness.lo)break;
    if(j==maxit-1)return 0;
  }

  double lower=floor;
  if(variance) {
    if(!cover.spend())return 0;
    std::vector<double> sorted(x.begin(),x.end());std::sort(sorted.begin(),sorted.end());
    const auto mass=collapse_multiplicity(x);
    const double p=n-mass, exponent=1.5*p-.5*n;
    if(!(exponent>0))return 0;
    I gap(INFINITY,INFINITY);
    for(std::size_t i=0;i+mass<sorted.size();++i) {
      const auto d=(I(sorted[i+mass])-sorted[i])/scale;
      if(!d.valid())return 0;
      gap.lo=std::min(gap.lo,d.lo);gap.hi=std::min(gap.hi,d.hi);
    }
    if(!(gap.lo>0))return 0;
    const auto q=sq(gap/4);
    const auto constant=cover.constant+n*.5*std::log(3.)-log(q)*(1.5*p);
    if(!constant.valid())return 0;
    const double w=std::min({1.,q.lo*.5,(df-2)*.5,
      std::exp((witness.lo-constant.hi-1)/exponent)});
    lower=2+w;
    const auto width=I(lower)-2;
    const auto bound=constant+log(width)*exponent;
    if(!width.valid() || !(width.lo>0 && width.hi<=std::min(1.,q.lo)) ||
       !bound.valid() || !(bound.hi<witness.lo))return 0;
  }

  if(!cover.spend())return 0;
  const auto center=cover.system({0,df},true);
  const double dr=std::min(tol*df*.8,(df-lower)/4);
  const double mr=std::min(.01,tol*.8*std::max(1.,std::abs(mu/scale)));
  const Box accurate{{-mr,mr},{df-dr,df+dr}};
  if(!cover.spend() || !includes(accurate,df,center,cover.system(accurate,true)))return 0;

  double basin_m=.1,basin_df=std::min(.1*df,(df-lower)/2);
  for(int j=0;j<maxit;++j) {
    if(basin_m<mr || basin_df<dr)return 0;
    const Box basin{{-basin_m,basin_m},{df-basin_df,df+basin_df}};
    if(!cover.spend())return 0;
    if(includes(basin,df,center,cover.system(basin,true)))
      return cover.excludes({location,{lower,upper}},basin,floor,witness.lo)?cover.work:0;
    basin_m/=2;basin_df/=2;
  }
  return 0;
}
// A held location leaves a unique conditional log-scale root. Cover
// shape intervals using those scale tubes, including the lower shape
// boundary where the ordinary scale stays positive.
inline int certify_location(span x,double mu,double scale,double df,bool variance,
    double tol,int maxit,std::int64_t budget) {
  if(!(df>(variance?2.:0.)) || !(scale>0) || !std::isfinite(df) || budget<=0)return 0;
  const double unit=variance?scale*std::sqrt((df-2)/df):scale;
  if(!(unit>0) || !std::isfinite(unit))return 0;
  t_signed_detail::Engine e;
  e.n=x.size();e.maxit=maxit;e.evaluation_limit=budget;e.free_scale=true;
  Data data;
  data.y.resize(x.size());
  Sum squares,distance_logs;
  std::size_t knots=0;
  for(double value:x) {
    const I y=(I(value)-mu)/unit;
    I q=sq(y);
    if(!q.valid())return 0;
    q.lo=std::max(0.,q.lo);
    e.q.push_back(q);data.x.push_back(y);squares.add(q);
    if(value==mu)++knots;
    if(!variance) {
      const auto distance=log(q)*.5;
      if(!distance.valid())return 0;
      distance_logs.add(distance);
    }
  }
  if(variance && knots>=2*(x.size()-knots))return 0;
  const auto log_unit=log(I(unit));
  e.constant=-(log_unit+.5*std::log(2*pi))*e.n;
  if(!e.spend())return 0;
  const auto witness=e.value(df,0);
  const I normal_log_scale=log(squares.get()/e.n)*.5;
  if(!witness.valid() || !normal_log_scale.valid())return 0;
  const I normal=e.constant-(normal_log_scale+.5)*e.n;
  if(!normal.valid() || !(witness.lo>normal.hi))return 0;

  const auto normal_tube=normal_log_scale+I(-.1,.1);
  Sum fourth;
  for(I q:e.squares(normal_tube))fourth.add(sq(q));
  double upper=0;
  for(int j=0;j<maxit;++j) {
    if(!e.spend())return 0;
    upper=std::ldexp(std::max(df*2,4.),j);
    if(!std::isfinite(upper))return 0;
    const I h=I(1)/upper;
    const auto left=e.nuisance({0,h.hi},normal_tube.lo);
    const auto right=e.nuisance({0,h.hi},normal_tube.hi);
    const auto bound=normal+fourth.get()*h*(I(1)+h)*.25;
    if(left.valid() && right.valid() && left.lo>0 && right.hi<0 &&
       bound.valid() && bound.hi<witness.lo)break;
    if(j==maxit-1)return 0;
  }
  const double floor=variance?2.:0.;
  double lower=floor;
  if(!variance) {
    if(!e.spend())return 0;
    const auto distances=distance_logs.get()+log_unit*e.n;
    lower=std::min(df/2,2*std::exp((witness.lo+distances.lo-1)/e.n));
    const auto bound=log(I(lower)/2)*e.n-distances;
    if(!(lower>0) || !bound.valid() || !(bound.hi<witness.lo))return 0;
  }

  const auto system=[&](Box box) {
    const auto full=ordinary(data,{{0,box.nuisance,box.shape/df}},df);
    System reduced{{{-full.g[1],-full.g[2]/df}},{}};
    reduced.curvature[0]={{-full.h[1][1],-full.h[1][2]/df}};
    reduced.curvature[1]={{reduced.curvature[0][1],-full.h[2][2]/df/df}};
    return reduced;
  };
  if(!e.spend())return 0;
  const auto center=system({0,df});
  const double accuracy=std::min(tol,.01);
  const double dr=std::min(accuracy*(df-floor)*.2,(df-lower)/4),sr=accuracy*.2;
  const Box precise{{-sr,sr},{df-dr,df+dr}};
  if(!e.spend() || !includes(precise,df,center,system(precise)))return 0;
  I public_scale=exp(precise.nuisance)*unit;
  if(variance)public_scale=public_scale*root(precise.shape/(precise.shape-2));
  const auto relative=public_scale/scale-1;
  if(!relative.valid() || relative.mag()>accuracy || (precise.shape/df-1).mag()>accuracy)return 0;

  Box basin;
  bool enclosed=false;
  double radius=.1,df_radius=std::min(.1*df,(df-lower)/2);
  for(int j=0;j<maxit && radius>=sr && df_radius>=dr;++j) {
    if(!e.spend())return 0;
    basin={{-radius,radius},{df-df_radius,df+df_radius}};
    if(includes(basin,df,center,system(basin))){enclosed=true;break;}
    radius/=2;df_radius/=2;
  }
  if(!enclosed)return 0;

  struct Cell {Box box;bool bracketed=false;};
  std::vector<Cell> pending{{{{},{lower,upper}},false}};
  while(!pending.empty()) {
    if(!e.spend())return 0;
    auto cell=pending.back();pending.pop_back();
    const auto initial=cell;
    if(e.scale_box(cell.box.shape,cell.box.nuisance,cell.bracketed)) {
      cell.bracketed=true;
      const auto& b=cell.box;
      if(b.nuisance.lo>=basin.nuisance.lo && b.nuisance.hi<=basin.nuisance.hi &&
         b.shape.lo>=basin.shape.lo && b.shape.hi<=basin.shape.hi)continue;
      if(!e.spend())return 0;
      const auto value=e.value(b.shape,b.nuisance);
      if(value.valid() && value.hi<witness.lo)continue;
      if(!e.spend())return 0;
      const auto score=e.shape_score(b.shape,b.nuisance);
      if(score.valid() && (score.lo>0 || (score.hi<0 && b.shape.lo>floor)))continue;
    } else {
      cell=initial;
    }
    const double mid=std::sqrt(cell.box.shape.lo)*std::sqrt(cell.box.shape.hi);
    if(!(mid>cell.box.shape.lo && mid<cell.box.shape.hi))return 0;
    auto left=cell,right=cell;left.box.shape.hi=right.box.shape.lo=mid;
    pending.push_back(right);pending.push_back(left);
  }
  return e.out.evaluations;
}
} // namespace held_profile
} // namespace t_cert_detail
} // namespace cppunivariate
