#pragma once
#include "profile_bounds.hpp"
namespace sged_mode_skew {
struct Powers {double A=0,B=0,C=0,E=0,H=0,J=0;};
inline Powers powers(span y,double c,double p) {
  Powers out;sged_profile::Sum A,B,C,E,H,J;
  bool left_singular=false,right_singular=false;
  for(double x:y) {
    if(x<c){const double r=c-x;A.add(std::pow(r,p));C.add(std::pow(r,p-1));H.add(std::pow(r,p-2));}
    else if(x>c){const double r=x-c;B.add(std::pow(r,p));E.add(std::pow(r,p-1));J.add(std::pow(r,p-2));}
    else if(p<2){left_singular=true;right_singular=true;}
    else if(p==2){H.add(1);J.add(1);} // conservative one-sided Hessian
  }
  out.A=A.value;out.B=B.value;out.C=C.value;out.E=E.value;out.H=left_singular?INFINITY:H.value;out.J=right_singular?INFINITY:J.value;return out;
}
inline sged_profile::Range side_hessian(span y,double lo,double hi,double p,bool left) {
  sged_profile::Sum lower,upper;bool finite=true;
  for(double x:y) {
    const double a=left?std::max(0.,lo-x):std::max(0.,x-hi),b=left?std::max(0.,hi-x):std::max(0.,x-lo);
    if(b==0) {
      if(lo==hi && x==lo) {
        if(p<2)finite=false;else if(p==2)upper.add(1);
      }
      continue;
    }
    double low=0,high=0;
    if(a>0){low=std::min(std::pow(a,p-2),std::pow(b,p-2));high=std::max(std::pow(a,p-2),std::pow(b,p-2));}
    else {low=p>2?0:std::pow(b,p-2);high=p<2?INFINITY:std::max(1.,std::pow(b,p-2));
      // If this observation changes side inside the box, its absent-side
      // contribution is zero, even when the present-side Hessian diverges.
      if((left && x>=lo) || (!left && x<=hi))low=0;
    }
    lower.add(low);if(std::isfinite(high))upper.add(high);else finite=false;
  }
  const double pad=sged_profile::margin(lower.value+upper.value,y.size())*(1+p);
  return {std::max(0.,lower.value-pad),finite?upper.value+pad:INFINITY};
}
struct Box {sged_profile::Range score{-INFINITY,INFINITY},curvature{-INFINITY,INFINITY};sged_profile::PublicBox coefficients;double upper=INFINITY;bool valid=false;};
inline Box box(span y,double lo,double hi,double p) {
  Box out;const auto l=powers(y,lo,p),h=powers(y,hi,p);
  const double n=y.size(),r=1/(p+1),f=p/(p+1);
  const auto positive=[&](double lo,double hi) {
    const double pad=4096*std::numeric_limits<double>::epsilon()*(1+p)*(1+n*std::numeric_limits<double>::epsilon())*hi+n*std::numeric_limits<double>::denorm_min();
    return sged_profile::Range{std::max(0.,lo-pad),hi+pad};
  };
  const auto A=positive(l.A,h.A),B=positive(h.B,l.B),C=positive(l.C,h.C),E=positive(h.E,l.E);
  const auto S=sged_profile::add(sged_profile::power(A.lo,A.hi,r,r),sged_profile::power(B.lo,B.hi,r,r));
  if(!(S.lo>0))return out;
  out.upper=n*(std::log(p)-special::lgamma(1/p)-(std::log(p/n)+1)/p-(1+1/p)*std::log(S.lo));
  out.upper+=sged_profile::margin(std::abs(out.upper)+n*10,n)*(1+p);
  double lower=A.hi>0?C.lo/std::pow(A.hi,f):0,upper=B.hi>0?E.lo/std::pow(B.hi,f):0;
  double left_high=A.lo>0?C.hi/std::pow(A.lo,f):INFINITY,right_high=B.lo>0?E.hi/std::pow(B.lo,f):INFINITY;
  // At the two domain ends the apparent 0/0 has a signed infinite limit.
  // Before the nearest distinct knot its exact term is k^r distance^-r.
  if(lo==y[0]) {
    std::size_t k=0;while(k<y.size() && y[k]==y[0])++k;
    if(k<y.size() && hi<=y[k] && hi>lo)lower=std::pow(static_cast<double>(k),r)*std::pow(hi-lo,-r);
  }
  if(hi==y[y.size()-1]) {
    std::size_t k=0;while(k<y.size() && y[y.size()-1-k]==hi)++k;
    if(k<y.size() && lo>=y[y.size()-1-k] && hi>lo)upper=std::pow(static_cast<double>(k),r)*std::pow(hi-lo,-r);
  }
  out.score={lower-right_high,left_high-upper};
  if(std::isfinite(out.score.lo))out.score.lo-=sged_profile::margin(std::abs(lower)+std::abs(right_high),n)*(1+p);
  if(std::isfinite(out.score.hi))out.score.hi+=sged_profile::margin(std::abs(left_high)+std::abs(upper),n)*(1+p);
  if(A.lo>0 && B.lo>0) {
    const auto H=side_hessian(y,lo,hi,p,true),J=side_hessian(y,lo,hi,p,false);
    const auto pos=sged_profile::mul({p-1,p-1},sged_profile::add(sged_profile::divide(H,sged_profile::power(A.lo,A.hi,f,f)),sged_profile::divide(J,sged_profile::power(B.lo,B.hi,f,f))));
    const auto neg=sged_profile::mul({f*p,f*p},sged_profile::add(sged_profile::divide(sged_profile::square(C),sged_profile::power(A.lo,A.hi,f+1,f+1)),sged_profile::divide(sged_profile::square(E),sged_profile::power(B.lo,B.hi,f+1,f+1))));
    out.curvature=sged_profile::add(pos,sged_profile::neg(neg));
    if(std::isfinite(out.curvature.lo))out.curvature.lo-=sged_profile::margin(std::abs(pos.lo)+std::abs(neg.hi),n)*(1+p);
    if(std::isfinite(out.curvature.hi))out.curvature.hi+=sged_profile::margin(std::abs(pos.hi)+std::abs(neg.lo),n)*(1+p);
    const auto t=sged_profile::exp_range(sged_profile::mul({1/p,1/p},sged_profile::add({std::log(p/n),std::log(p/n)},sged_profile::logrange(S))));
    out.coefficients=sged_profile::public_box({lo,hi},sged_profile::mul(t,sged_profile::power(A.lo,A.hi,r,r)),sged_profile::mul(t,sged_profile::power(B.lo,B.hi,r,r)),p,p);
  }
  out.valid=!std::isnan(out.score.lo+out.score.hi) && out.score.lo!=INFINITY && out.score.hi!=-INFINITY && std::isfinite(out.upper);return out;
}
struct Point {double c=NAN,a=NAN,b=NAN,ll=-INFINITY;bool valid=false;};
inline Point point(span y,double c,double p) {
  const auto sums=powers(y,c,p);const double r=1/(p+1),ar=std::pow(sums.A,r),br=std::pow(sums.B,r),S=ar+br;
  const double t=std::exp(std::log(p*S/y.size())/p);
  Point out{c,t*ar,t*br,y.size()*(std::log(p)-special::lgamma(1/p)-std::log(t*S)-1/p),false};
  out.valid=S>0 && t>0 && std::isfinite(out.a+out.b+out.ll) &&
    (sums.A>0?out.a>0:c==y[0]) && (sums.B>0?out.b>0:c==y[y.size()-1]);return out;
}
inline SgedResult fit(span x,double /*tol*/,int maxit,double p) {
  SgedResult failed{NAN,NAN,p,NAN,NAN,0,SgedStatus::CertificateFailure};failed.stop_detail.loglik_upper=INFINITY;
  const auto mm=std::minmax_element(x.begin(),x.end());if(*mm.first==*mm.second){failed.status=SgedStatus::Unbounded;return failed;}
  sged_profile::System system(x);if(!system.valid)return failed;
  const auto y=system.data();const double n=y.size();const auto cap=certificate_budget(maxit);std::int64_t work=1;
  auto best=point(y,y[0],p);if(!best.valid)return failed;
  const auto candidate=[&](const Point& v) {
    if(v.a>0 && v.b>0) {
      const auto r=system.recover(v.c,v.a,v.b,p,v.ll);
      return r.status==SgedStatus::Ok?sged_profile::witness(r,n):FitCandidate{};
    }
    if((v.a==0 && v.c!=y[0]) || (v.b==0 && v.c!=y[y.size()-1]))return FitCandidate{};
    const auto model=v.a==0?StopModel::HalfGed:StopModel::ReflectedHalfGed;
    const double endpoint=v.c==y[0]?*mm.first:*mm.second,scale=system.unit*(v.a+v.b);
    const double ll=v.ll-system.offset,pad=sged_profile::margin(std::abs(ll)+n*10,n)*(1+p);
    if(!(scale>0 && std::isfinite(scale)) || std::abs(std::log(scale/system.unit)-std::log(v.a+v.b))>2e-8)return FitCandidate{};
    sged_profile::Sum penalty;for(double z:x) {
      if((model==StopModel::HalfGed && z<endpoint) || (model==StopModel::ReflectedHalfGed && z>endpoint))return FitCandidate{};
      penalty.add(std::pow(std::abs(z-endpoint)/scale,p));
    }
    const double actual=n*(std::log(p)-special::lgamma(1/p)-std::log(scale))-penalty.value;
    if(!std::isfinite(actual) || std::abs(actual-ll)>pad)return FitCandidate{};
    return FitCandidate{{endpoint,scale,p},std::min(ll,actual)-pad,std::max(ll,actual)+pad,model};
  };
  const auto stopped=[&](bool budget) {
    auto r=failed;r.iterations=static_cast<int>(work);r.stop_detail.budget_exhausted=budget;
    const auto c=candidate(best);if(!c.coef.empty()){r.stop_detail.candidates.push_back(c);r.stop_detail.loglik_lower=c.loglik_lower;}
    return r;
  };
  std::vector<double> knots;
  for(double z:y)if(knots.empty() || z!=knots.back())knots.push_back(z);
  if(knots.size()>static_cast<std::size_t>(cap-work))return stopped(true);
  std::vector<Point> points;std::vector<double> uppers;std::vector<sged_profile::Range> regions;
  for(double c:knots) {
    if(work>=cap)return stopped(true);++work;const auto v=point(y,c,p);if(!v.valid)return stopped(false);
    const auto witness=candidate(v);if(witness.coef.empty())return stopped(false);
    const auto derivative=box(y,c,c,p);
    if(c==knots.front() || c==knots.back() || !derivative.valid || (derivative.score.lo<=0 && derivative.score.hi>=0)) {
      points.push_back(v);regions.push_back({c,c});uppers.push_back(v.ll+sged_profile::margin(std::abs(v.ll)+n*10,n)*(1+p));
    }
    if(v.ll>best.ll)best=v;
  }
  struct Segment{double lo,hi;};std::vector<Segment> pending,retained;
  for(std::size_t j=1;j<knots.size();++j)pending.push_back({knots[j-1],knots[j]});
  while(!pending.empty()) {
    if(work>=cap)return stopped(true);++work;
    const auto cell=pending.back();pending.pop_back();const auto b=box(y,cell.lo,cell.hi,p);
    if(b.valid && (b.score.lo>0 || b.score.hi<0))continue;
    if(b.valid && std::isfinite(b.curvature.lo) && b.curvature.lo>0) {
      if(work+2>cap)return stopped(true);work+=2;
      const auto left=box(y,cell.lo,cell.lo,p),right=box(y,cell.hi,cell.hi,p);
      if((left.valid && left.score.lo>0) || (right.valid && right.score.hi<0))continue;
      double a=cell.lo,h=cell.hi;bool done=false;
      for(std::int64_t j=0;j<solver_budget(maxit);++j) {
        if(work>=cap)return stopped(true);++work;const double c=a/2+h/2;const auto sc=box(y,c,c,p);if(!sc.valid)break;
        if(sc.score.lo>0)h=c;else if(sc.score.hi<0)a=c;
        else {a=std::max(a,c-std::max(0.,sc.score.hi)/b.curvature.lo);h=std::min(h,c+std::max(0.,-sc.score.lo)/b.curvature.lo);}
        if(h-a<=2e-9){done=true;break;}
      }
      if(done){retained.push_back({a,h});continue;}
    }
    if(b.valid && b.upper<best.ll-sged_profile::margin(std::abs(best.ll)+n*10,n)*(1+p))continue;
    const double mid=cell.lo/2+cell.hi/2;if(mid==cell.lo || mid==cell.hi)return stopped(false);
    if(b.valid && cell.hi-cell.lo<=2e-9 && system.narrow(b.coefficients)){retained.push_back(cell);continue;}
    const auto v=point(y,mid,p);if(v.valid && v.ll>best.ll)best=v;
    pending.push_back({mid,cell.hi});pending.push_back({cell.lo,mid});
  }
  std::sort(retained.begin(),retained.end(),[](const Segment&a,const Segment&b){return a.lo<b.lo;});std::vector<Segment> clusters;
  for(auto c:retained){if(!clusters.empty() && c.lo<=clusters.back().hi)clusters.back().hi=c.hi;else clusters.push_back(c);}
  for(auto cell:clusters) {
    if(work>=cap)return stopped(true);++work;const auto enclosure=box(y,cell.lo,cell.hi,p);
    if(!enclosure.valid || !system.narrow(enclosure.coefficients) || cell.hi-cell.lo>2e-8)return stopped(false);
    double a=cell.lo,h=cell.hi;
    for(std::int64_t k=0;k<solver_budget(maxit) && h-a>1e-13;++k) {
      if(work>=cap)return stopped(true);++work;const double mid=a/2+h/2;const auto sc=box(y,mid,mid,p);if(!sc.valid)break;
      if(sc.score.lo>0)h=mid;else if(sc.score.hi<0)a=mid;else break;
    }
    const auto v=point(y,a/2+h/2,p);if(!v.valid || candidate(v).coef.empty())return stopped(false);
    points.push_back(v);regions.push_back({cell.lo,cell.hi});uppers.push_back(enclosure.upper);if(v.ll>best.ll)best=v;
  }
  std::size_t winner=0;for(std::size_t j=0;j<points.size();++j)if(points[j].ll>points[winner].ll)winner=j;
  best=points[winner];const auto winning=candidate(best);if(winning.coef.empty())return stopped(false);
  const double pad=sged_profile::margin(std::abs(best.ll)+n*10,n)*(1+p),tie=1e-8*n;
  double global=-INFINITY;for(double u:uppers)global=std::max(global,u);
  std::vector<FitCandidate> competing{winning};
  for(std::size_t j=0;j<points.size();++j)if(j!=winner && uppers[j]>=best.ll-pad-tie) {
    // An explicitly enumerated knot and an adjoining root box can describe
    // the same maximum. Discard duplicates only after coefficient enclosure.
    if(std::abs(points[j].c-best.c)<=2e-8 && points[j].a>0 && points[j].b>0 && best.a>0 && best.b>0) {
      const auto union_box=box(y,std::min(regions[j].lo,regions[winner].lo),std::max(regions[j].hi,regions[winner].hi),p);
      if(union_box.valid && system.narrow(union_box.coefficients)) {
        regions[winner]={std::min(regions[j].lo,regions[winner].lo),std::max(regions[j].hi,regions[winner].hi)};continue;
      }
    }
    const auto c=candidate(points[j]);if(c.coef.empty())return stopped(false);competing.push_back(c);
  }
  if(competing.size()>1) {auto r=stopped(false);r.stop_detail.candidates=competing;r.stop_detail.comparison_tie=true;r.stop_detail.loglik_upper=std::max(winning.loglik_upper,system.original_upper(global,p));return r;}
  if(winning.model!=StopModel::None) {
    auto r=failed;r.status=winning.model==StopModel::HalfGed?SgedStatus::RightHalfGedBoundary:SgedStatus::LeftHalfGedBoundary;r.certified=true;r.iterations=static_cast<int>(work);
    r.logLik=best.ll-system.offset;r.stop_detail=limit_detail(winning.coef,r.logLik,winning.model);
    r.stop_detail.loglik_lower=winning.loglik_lower;r.stop_detail.loglik_upper=std::max(winning.loglik_upper,system.original_upper(global,p));return r;
  }
  auto r=system.recover(best.c,best.a,best.b,p,best.ll);if(r.status!=SgedStatus::Ok)return stopped(false);
  const auto accepted_box=box(y,regions[winner].lo,regions[winner].hi,p);
  if(!system.compatible(accepted_box.coefficients,r))return stopped(false);
  r.certified=true;r.iterations=static_cast<int>(work);r.stop_detail.loglik_lower=winning.loglik_lower;r.stop_detail.loglik_upper=std::max(winning.loglik_upper,system.original_upper(global,p));return r;
}
} // namespace sged_mode_skew
