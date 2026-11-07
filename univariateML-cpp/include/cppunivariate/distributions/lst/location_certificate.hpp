// Fixed scale and shape: exhaustive scalar stationary-point isolation.
#pragma once
#include "certificate.hpp"
#include "../../special/special.hpp"
namespace cppunivariate {
namespace t_location_detail {
struct Result {
  double location=NAN; int evaluations=0;
  // 0 success, 1 budget, 2 unresolved comparison, 3 arithmetic/root failure.
  int failure=3; StopDetail detail{};
};
inline Result solve(span x,double scale,double df,int maxit,std::int64_t budget_override=-1) {
  Result out;
  if(x.empty()||!(scale>0)||!std::isfinite(scale)||!(df>0)||!std::isfinite(df))return out;
  double low=INFINITY,high=-INFINITY;
  for(double v:x){if(!std::isfinite(v))return out;low=std::min(low,v);high=std::max(high,v);}
  if(low==high){out.location=low;out.failure=0;return out;}
  const double center=low/2+high/2,half=std::max(high-center,center-low);
  if(!(half>0)||!std::isfinite(half))return out;
  t_cert_detail::Sum moments;std::vector<t_cert_detail::I> standardized;
  for(double v:x){auto z=(t_cert_detail::I(v)-center)/half;standardized.push_back(z);moments.add(z);}
  const auto mean=moments.get()/x.size();t_cert_detail::Sum variance;
  for(auto z:standardized)variance.add(t_cert_detail::sq(z-mean));
  const auto empirical_sd=t_cert_detail::I(half)*t_cert_detail::root(variance.get()/x.size());
  if(!empirical_sd.valid()||!(empirical_sd.lo>0))return out;
  const double unit=empirical_sd.mid();
  const double a=(scale/unit)*std::sqrt(df);
  if(!(unit>0)||!std::isfinite(unit)||!(a>0)||!std::isfinite(a))return out;
  const auto enclosed_a=(t_cert_detail::I(scale)/unit)*t_cert_detail::root(t_cert_detail::I(df));
  std::vector<t_cert_detail::I> y;
  for(double v:x)y.push_back((t_cert_detail::I(v)-center)/unit);
  const auto score=[&](t_cert_detail::I m,bool derivative){
    t_cert_detail::Sum sum;
    for(auto v:y){
      auto r=v-m,z=r/enclosed_a,z2=t_cert_detail::sq(z);
      if(!z2.valid())return t_cert_detail::bad();
      z2.lo=std::max(0.,z2.lo); // The exact square is nonnegative.
      auto den=t_cert_detail::I(1)+z2;den.lo=std::max(1.,den.lo);
      // S=sum (m-x)/(1+((x-m)/a)^2), S'>0 at a maximum.
      // Avoid squaring a wide denominator: that enclosure can lose its
      // positive lower bound. The exact fraction lies between zero and one.
      if(derivative){
        auto fraction=z2/den;if(!fraction.valid())return t_cert_detail::bad();
        fraction.lo=std::max(0.,fraction.lo);fraction.hi=std::min(1.,fraction.hi);
        if(!fraction.valid())return t_cert_detail::bad();
        sum.add((t_cert_detail::I(1)-fraction*2)/den);
      } else sum.add(-r/den);
    }
    return sum.get();
  };
  const auto value=[&](t_cert_detail::I m){t_cert_detail::Sum sum;for(auto v:y)sum.add(-t_cert_detail::log1p(t_cert_detail::sq((v-m)/enclosed_a)));return sum.get();};
  const auto normalizer=df>=.5?t_cert_detail::gamma(df,0)-.5*std::log(2*pi):
    t_cert_detail::pad(special::lgamma((df+1)/2)-special::lgamma(df/2)-.5*std::log(df*pi),
                      special::lgamma((df+1)/2)-special::lgamma(df/2)-.5*std::log(df*pi));
  if(!normalizer.valid())return out;
  const auto witness=t_cert_detail::I(x.size())*(normalizer-std::log(scale))+value(0)*((df+1)/2);
  if(witness.valid()) {
    out.detail.candidates.push_back({{center,scale,df},witness.lo,witness.hi,StopModel::Lst});
    out.detail.loglik_lower=witness.lo;out.detail.loglik_upper=INFINITY;
  }
  const auto left_endpoint=(t_cert_detail::I(low)-center)/unit;
  const auto right_endpoint=(t_cert_detail::I(high)-center)/unit;
  if(!left_endpoint.valid()||!right_endpoint.valid())return out;
  std::vector<t_cert_detail::I> pending{{left_endpoint.lo,right_endpoint.hi}},roots,regions;
  const auto budget=budget_override<0?certificate_budget(maxit):std::min(budget_override,certificate_budget(maxit));
  while(!pending.empty()){
    if(out.evaluations>=budget){out.failure=1;out.detail.budget_exhausted=true;break;}
    ++out.evaluations;auto box=pending.back();pending.pop_back();
    bool covered=false;for(auto region:regions)if(box.lo>=region.lo&&box.hi<=region.hi)covered=true;
    if(covered)continue;
    auto g=score(box,false);if(!g.valid())return out;
    if(g.lo>0||g.hi<0)continue;
    auto h=score(box,true);if(!h.valid())return out;
    if(h.hi<0)continue; // Strictly convex log likelihood: no interior maximum.
    if(h.lo>0){
      const double extra=1e-8*std::max(1.,box.hi-box.lo);
      t_cert_detail::I region(box.lo-extra,box.hi+extra);
      auto curvature=score(region,true),left=score(region.lo,false),right=score(region.hi,false);
      if(curvature.valid()&&curvature.lo>0&&left.hi<0&&right.lo>0){
        auto root=region;
        const double target=std::min(1e-10,1e-8*std::min(1.,scale/unit));
        for(int it=0;it<solver_budget(maxit)&&root.hi-root.lo>target;++it){
          const double mid=root.mid();auto gm=score(mid,false);
          if(!gm.valid())return out;
          // The lower slope bound maps score uncertainty to root error.
          const double error=gm.mag()/curvature.lo;
          root.lo=std::max(root.lo,mid-error);root.hi=std::min(root.hi,mid+error);
          if(gm.hi<0)root.lo=std::max(root.lo,mid);
          if(gm.lo>0)root.hi=std::min(root.hi,mid);
        }
        if(!root.valid()||root.hi-root.lo>1e-8*std::min(1.,scale/unit))return out;
        bool duplicate=false;for(auto old:roots)if(root.lo<=old.hi&&root.hi>=old.lo)duplicate=true;
        if(!duplicate){roots.push_back(root);regions.push_back(region);}
        continue;
      }
    }
    const double mid=box.mid();if(!(mid>box.lo&&mid<box.hi))return out;
    pending.push_back({box.lo,mid});pending.push_back({mid,box.hi});
  }
  // Values omit a common normalizer and positive factor (df+1)/2.
  // These reduced values stay internal; public stop details use full ll below.
  const bool exhausted=out.failure==1;
  if(roots.empty()) {
    if(!exhausted)return out;
    roots.push_back(t_cert_detail::I(0));
  }
  std::size_t best=0;std::vector<t_cert_detail::I> values;
  for(auto root:roots){auto v=value(root);if(!v.valid())return out;values.push_back(v);if(v.lo>values[best].lo)best=values.size()-1;}
  bool tie=false;for(std::size_t j=0;j<roots.size();++j)if(j!=best&&values[j].hi>=values[best].lo)tie=true;
  std::vector<FitCandidate> displayed;
  double lower=-INFINITY,upper=-INFINITY;
  for(std::size_t j=0;j<roots.size();++j){
    const auto root_ll=t_cert_detail::I(x.size())*(normalizer-std::log(scale))+values[j]*((df+1)/2);
    const double physical=center+unit*roots[j].mid();
    // A displayed coefficient is rounded in physical units. Its likelihood
    // enclosure must evaluate that point, separately from the root value.
    const auto represented=(t_cert_detail::I(physical)-center)/unit;
    const auto ll=t_cert_detail::I(x.size())*(normalizer-std::log(scale))+value(represented)*((df+1)/2);
    if(!root_ll.valid()||!ll.valid())return out;
    displayed.push_back({{physical,scale,df},ll.lo,ll.hi,StopModel::Lst});
    lower=std::max(lower,ll.lo);upper=std::max(upper,root_ll.hi);
  }
  out.detail.candidates=displayed;out.detail.loglik_lower=lower;out.detail.loglik_upper=upper;
  if(exhausted){out.detail.loglik_upper=INFINITY;return out;}
  if(tie){out.failure=2;out.detail.comparison_tie=true;return out;}
  const auto location=t_cert_detail::location_units(roots[best],unit,center);
  if(!location.valid()||location.hi-location.lo>1e-6*empirical_sd.lo)return out;
  out.location=location.mid();out.failure=0;return out;
}
// A uniform-in-location sufficient condition for a held ordinary-scale
// profile to decrease on df>=1. See the held-subset derivation for the
// derivative bound and the analytic infinite-tail exclusion.
struct ShapeDecrease { int result=0,evaluations=0; }; // 1 proof, 2 budget
inline ShapeDecrease decreasing_shape(span x,double scale,int maxit) {
  ShapeDecrease out;
  if(x.empty()||!(scale>0)||!std::isfinite(scale))return out;
  double low=INFINITY,high=-INFINITY;
  for(double v:x){if(!std::isfinite(v))return out;low=std::min(low,v);high=std::max(high,v);}
  const auto range=(t_cert_detail::I(high)-low)/(2*scale);
  const auto squared=t_cert_detail::sq(range);
  if(!squared.valid()||!(squared.lo>1))return out;
  const double Q=squared.lo,n=x.size();
  const auto tail=t_cert_detail::I(Q)*Q/8-Q-(2*n-1)/2;
  if(!tail.valid()||!(tail.lo>0))return out;
  std::vector<t_cert_detail::I> pending{{1,Q}};
  while(!pending.empty()) {
    if(out.evaluations>=certificate_budget(maxit)){out.result=2;return out;}
    ++out.evaluations;const auto v=pending.back();pending.pop_back();
    const auto b=[&](double q){return t_cert_detail::log1p(t_cert_detail::I(q)/v)-
      (t_cert_detail::I(1)+t_cert_detail::I(1)/v)*(t_cert_detail::I(q)/(v+q));};
    const auto bound=t_cert_detail::I(n)*t_cert_detail::derivative(v,3)-(b(Q)+b(1)*(n-1))*.5;
    if(!bound.valid())return out;
    if(bound.hi<0)continue;
    if(bound.lo>0)return out; // This sufficient bound cannot close the segment.
    const double mid=std::sqrt(v.lo)*std::sqrt(v.hi);
    if(!(mid>v.lo&&mid<v.hi))return out;
    pending.push_back({v.lo,mid});pending.push_back({mid,v.hi});
  }
  out.result=1;return out;
}
// Joint fixed-shape fits with df>=1 have one stationary nuisance fit under
// the strict atom inequality (geodesic convexity). Newton only nominates;
// a two-dimensional contraction inclusion establishes the returned root.
inline bool joint_inclusion(span x,double df,int maxit,double& mu,double& scale) {
  t_cert_detail::Data d;if(!t_cert_detail::data(x,d))return false;
  std::array<double,2> p{{(mu-d.center)/d.unit,std::log(scale/d.unit)}};
  for(int it=0;it<solver_budget(maxit);++it){
    const auto h=t_cert_detail::ordinary(d,{{p[0],p[1],1}},df);
    std::array<std::array<double,3>,3> C{};if(!t_cert_detail::inverse(h,2,C))return false;
    std::array<double,2> step{};for(int i=0;i<2;++i)for(int j=0;j<2;++j)step[i]+=C[i][j]*h.g[j].mid();
    if(!std::isfinite(step[0])||!std::isfinite(step[1]))return false;
    if(std::max(std::abs(step[0]),std::abs(step[1]))<1e-13)break;
    // The EM nomination is already score-checked. Reject a large Newton
    // displacement rather than treating this stage as a new search.
    if(std::max(std::abs(step[0]),std::abs(step[1]))>.1)return false;
    for(int j=0;j<2;++j)p[j]-=step[j];
  }
  const auto h=t_cert_detail::ordinary(d,{{p[0],p[1],1}},df);
  std::array<std::array<double,3>,3> C{};if(!t_cert_detail::inverse(h,2,C))return false;
  // Width trials are certificate evaluations, hence grow with iterlim.
  for(std::int64_t trial=0;trial<certificate_budget(maxit);++trial){
    const double radius=1e-11*std::pow(2.,static_cast<double>(trial));
    if(radius>1e-7)return false; // accuracy requirement, not a work cap
    const auto box=t_cert_detail::ordinary(d,{{{p[0]-radius,p[0]+radius},{p[1]-radius,p[1]+radius},1}},df);
    bool included=true;
    for(int i=0;i<2;++i){t_cert_detail::I displacement;
      for(int j=0;j<2;++j)displacement=displacement+t_cert_detail::I(C[i][j])*h.g[j];
      double defect=0;for(int j=0;j<2;++j){t_cert_detail::I product;for(int k=0;k<2;++k)product=product+t_cert_detail::I(C[i][k])*box.h[k][j];defect+=(t_cert_detail::I(i==j?1:0)-product).mag();}
      if(!displacement.valid()||!std::isfinite(defect)||!(defect<1&&displacement.mag()+defect*radius<radius))included=false;
    }
    if(included){
      auto m=t_cert_detail::location_units({p[0]-radius,p[0]+radius},d.unit,d.center);
      auto s=t_cert_detail::scale_units(t_cert_detail::exp({p[1]-radius,p[1]+radius}),d.unit);
      if(!m.valid()||!s.valid()||s.lo<=0||m.hi-m.lo>1e-6*d.unit||s.hi-s.lo>1e-6*s.mid())return false;
      if(std::abs(std::log(s.mid())-std::log(d.unit)-p[1])>5e-9)return false;
      mu=m.mid();scale=s.mid();return std::isfinite(mu)&&std::isfinite(scale);
    }
  }
  return false;
}
} // namespace t_location_detail
} // namespace cppunivariate
