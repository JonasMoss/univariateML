#pragma once
#include "profile_bounds.hpp"
namespace sged_weighted_shape {
struct Weights {double left,right,lognormal;explicit Weights(double xi) {
  const double t=std::log(xi);left=std::exp(t-std::abs(t));right=std::exp(-t-std::abs(t));
  lognormal=std::log(1/left+1/right);
}};
inline sged_profile::Range score(span y,double c,double p,double q,const Weights& w,double radius=NAN) {
  if(std::isnan(radius)){radius=0;for(double x:y)radius=std::max(radius,(x<c?w.left:w.right)*std::abs(x-c));}
  if(!(radius>0))return {-INFINITY,INFINITY};
  sged_profile::Sum lo,hi,absolute;
  for(double x:y) {
    if(x==c) {if(p==1){lo.add(-w.right);hi.add(w.left);absolute.add(w.left+w.right);}continue;}
    const double weight=x<c?w.left:w.right;
    auto term=sged_profile::mul({weight,weight},sged_profile::power(weight*std::abs(x-c)/radius,weight*std::abs(x-c)/radius,p-1,q-1));
    if(x>c)term=sged_profile::neg(term);
    lo.add(term.lo);hi.add(term.hi);absolute.add(std::max(std::abs(term.lo),std::abs(term.hi)));
  }
  const double pad=sged_profile::margin(absolute.value,y.size())*(1+q);
  return {lo.value-pad,hi.value+pad};
}
inline sged_profile::Range locations(span y,double p,double q,const Weights& w,int maxit) {
  if(p==1 && q==1) {
    double slope=-static_cast<double>(y.size())*w.right;
    const double pad=sged_profile::margin(y.size()*(w.left+w.right),y.size());
    for(std::size_t j=0;j<y.size();) {
      const double knot=y[j];std::size_t end=j;while(end<y.size() && y[end]==knot)++end;
      slope+=(end-j)*(w.left+w.right);
      if(slope>pad)return {knot,knot};
      if(std::abs(slope)<=pad && end<y.size())return {knot,y[end]};
      j=end;
    }
    return {y[y.size()-1],y[y.size()-1]};
  }
  const double tolerance=std::max(3e-13,std::min(1e-4,(q-p)*.01));
  if(p>1) {
    double a=y[0],b=y[y.size()-1];
    for(std::int64_t j=0;j<solver_budget(maxit) && b-a>tolerance;++j) {
      const double c=a/2+b/2;double radius=0;
      for(double x:y)radius=std::max(radius,(x<c?w.left:w.right)*std::abs(x-c));
      if(!(radius>0))return {a,b};
      const auto g=score(y,c,p,q,w,radius);
      double low=a,high=b;if(g.hi<0)low=c;if(g.lo>0)high=c;
      sged_profile::Sum hessian;
      for(double x:y) {
        const bool left=x<a,right=x>b;const double weight=left?w.left:right?w.right:std::min(w.left,w.right);
        const double d=left?w.left*(a-x)/radius:right?w.right*(x-b)/radius:0;
        const double e=std::max(w.left*std::max(0.,b-x),w.right*std::max(0.,x-a))/radius;
        double h=0;
        if(d>0)h=std::exp(std::min({(p-2)*std::log(d),(q-2)*std::log(d),(p-2)*std::log(e),(q-2)*std::log(e)}));
        else if(q<=2 && e>0)h=std::min(std::pow(e,p-2),std::pow(e,q-2));
        hessian.add(weight*weight*h);
      }
      const double derivative=(p-1)*std::max(0.,hessian.value-sged_profile::margin(hessian.value,y.size())*(1+q))/radius;
      if(derivative>0 && std::isfinite(derivative)) {
        low=std::max(low,c-std::max(0.,g.hi)/derivative);
        high=std::min(high,c+std::max(0.,-g.lo)/derivative);
      }
      if(low>high)return {a,b};
      if(high-low<.9*(b-a)){a=low;b=high;continue;}
      const double l=a/2+c/2,h=c/2+b/2;
      if(score(y,l,p,q,w,radius).hi<0)low=std::max(low,l);
      if(score(y,h,p,q,w,radius).lo>0)high=std::min(high,h);
      if(high-low<.9*(b-a)){a=low;b=high;continue;}
      return {low,high};
    }
    return {a,b};
  }
  double a=y[0],b=y[y.size()-1];
  for(std::int64_t j=0;j<solver_budget(maxit) && b-a>tolerance;++j) {
    const double c=a/2+b/2;if(score(y,c,p,q,w).hi<0)a=c;else b=c;
  }
  const double lower=a;a=y[0];b=y[y.size()-1];
  for(std::int64_t j=0;j<solver_budget(maxit) && b-a>tolerance;++j) {
    const double c=a/2+b/2;if(score(y,c,p,q,w).lo>0)b=c;else a=c;
  }
  return {lower,b};
}
struct Box {
  sged_profile::Range mode{NAN,NAN},score{-INFINITY,INFINITY},curvature{-INFINITY,INFINITY},logh{-INFINITY,INFINITY};
  sged_profile::PublicBox coefficients;double logmoment_lower=-INFINITY,logmoment_upper=INFINITY;bool valid=false;
};
inline Box box(span y,double p,double q,const Weights& w,int maxit) {
  Box out;out.mode=locations(y,p,q,w,maxit);
  double radius=0;
  for(double x:y)radius=std::max(radius,std::max(w.left*std::max(0.,out.mode.hi-x),w.right*std::max(0.,x-out.mode.lo)));
  if(!(radius>0) || !std::isfinite(radius))return out;
  sged_profile::Sum lt,ht,lu,hu,lv,hv,lh,hh,lg,hg;
  bool finite_h=true;
  for(double x:y) {
    const bool left=x<out.mode.lo,right=x>out.mode.hi;
    const double weight=left?w.left:w.right;
    const double a=left?weight*(out.mode.lo-x)/radius:right?weight*(x-out.mode.hi)/radius:0;
    const double b=std::max(w.left*std::max(0.,out.mode.hi-x),w.right*std::max(0.,x-out.mode.lo))/radius;
    const auto T=sged_profile::power(a,b,p,q),U=sged_profile::powerlog(a,b,p,q),V=sged_profile::powerlog2(a,b,p,q);
    auto H=sged_profile::hessian_power(a,b,p,q),G=sged_profile::powerlog(a,b,p-1,q-1);
    const double wlo=left||right?weight:std::min(w.left,w.right),whi=left||right?weight:std::max(w.left,w.right);
    H=sged_profile::mul({wlo*wlo,whi*whi},H);
    G=sged_profile::mul({wlo,whi},G);
    if(right)G=sged_profile::neg(G);
    else if(!left){const double size=std::max(std::abs(G.lo),std::abs(G.hi));G={-size,size};}
    lt.add(T.lo);ht.add(T.hi);lu.add(U.lo);hu.add(U.hi);lv.add(V.lo);hv.add(V.hi);lh.add(H.lo);
    if(std::isfinite(H.hi))hh.add(H.hi);else finite_h=false;
    lg.add(G.lo);hg.add(G.hi);
  }
  const double n=y.size(),pad=sged_profile::margin(ht.value+std::max(std::abs(lu.value),std::abs(hu.value)),n)*(1+q);
  sged_profile::Range T{std::max(0.,lt.value-pad),ht.value+pad};
  const sged_profile::Range U{lu.value-pad,hu.value+pad};
  if(p==1 && q==1) {
    sged_profile::Sum value;const double c=out.mode.lo/2+out.mode.hi/2;
    for(double x:y)value.add((x<c?w.left:w.right)*std::abs(x-c)/radius);
    const double uncertainty=pad+sged_profile::margin(n*(w.left+w.right),n)*(out.mode.hi-out.mode.lo)/radius;
    T={std::max(0.,value.value-uncertainty),value.value+uncertainty};
  }
  if(!(T.lo>0) || !std::isfinite(T.hi))return out;
  const auto logT=sged_profile::logrange({T.lo/n,T.hi/n});
  const auto D=sged_profile::add(sged_profile::mul({p,q},sged_profile::divide(U,T)),sged_profile::neg(logT));
  const sged_profile::Range psi{special::digamma(1+1/q),special::digamma(1+1/p)};
  out.score=sged_profile::add(sged_profile::add(psi,{std::log(p),std::log(q)}),sged_profile::neg(D));
  const double err=sged_profile::margin(std::max(std::abs(out.score.lo),std::abs(out.score.hi)),n)*(1+q);
  out.score.lo-=err;out.score.hi+=err;
  const auto lr=sged_profile::divide(logT,{p,q});
  out.logmoment_lower=std::log(radius)+lr.lo;out.logmoment_upper=std::log(radius)+lr.hi;
  out.logh=sged_profile::add({std::log(radius),std::log(radius)},sged_profile::divide(sged_profile::add(logT,{std::log(p),std::log(q)}),{p,q}));
  const auto h=sged_profile::exp_range(out.logh);
  out.coefficients=sged_profile::public_box(out.mode,sged_profile::mul(h,{1/w.left,1/w.left}),sged_profile::mul(h,{1/w.right,1/w.right}),p,q);
  if(p>1) {
    const double hp=sged_profile::margin(hv.value+hh.value+std::max(std::abs(lg.value),std::abs(hg.value)),n)*(1+q);
    const sged_profile::Range V{std::max(0.,lv.value-hp),hv.value+hp},H{std::max(0.,lh.value-hp),finite_h?hh.value+hp:INFINITY},G{lg.value-hp,hg.value+hp};
    if(H.lo>0) {
      auto variance=sged_profile::add(sged_profile::divide(V,T),sged_profile::neg(sged_profile::square(sged_profile::divide(U,T))));
      variance.lo=std::max(0.,variance.lo);variance.hi=std::max(0.,variance.hi);
      const auto correction=sged_profile::divide(sged_profile::mul({p*p,q*q},sged_profile::square(G)),sged_profile::mul(sged_profile::mul({p-1,q-1},T),H));
      out.curvature=sged_profile::add(sged_profile::add({1/q,1/p},sged_profile::neg(sged_profile::mul({special::trigamma(1+1/p),special::trigamma(1+1/q)},{1/(q*q),1/(p*p)}))),sged_profile::add(sged_profile::neg(sged_profile::mul({p,q},variance)),correction));
      out.curvature=sged_profile::padded(out.curvature.lo,out.curvature.hi,n,1+q);
    }
  }
  out.valid=std::isfinite(out.score.lo+out.score.hi);return out;
}
struct Point {double mode=NAN,h=NAN,p=NAN,ll=-INFINITY;sged_profile::Range modebox{NAN,NAN};bool valid=false;};
inline Point point(span y,double p,const Weights& w,int maxit,double held=NAN) {
  Point out;out.p=p;out.modebox=std::isnan(held)?locations(y,p,p,w,maxit):sged_profile::Range{held,held};out.mode=out.modebox.lo/2+out.modebox.hi/2;
  double radius=0;for(double x:y)radius=std::max(radius,(x<out.mode?w.left:w.right)*std::abs(x-out.mode));
  if(!(radius>0))return out;
  sged_profile::Sum T;
  for(double x:y)T.add(std::pow((x<out.mode?w.left:w.right)*std::abs(x-out.mode)/radius,p));
  out.h=radius*std::exp((std::log(p)+std::log(T.value/y.size()))/p);
  out.ll=y.size()*(-special::lgamma(1+1/p)-w.lognormal-std::log(radius)-(std::log(p)+std::log(T.value/y.size())+1)/p);
  out.valid=out.h>0 && std::isfinite(out.h+out.ll) && out.modebox.hi-out.modebox.lo<=2e-8;
  // A Laplace interval is an exact face, not a failed mode solve.
  if(p==1 && std::isfinite(out.ll+out.h) && out.h>0)out.valid=true;
  return out;
}
inline double upper(span y,double p,double q,const Weights& w,int maxit) {
  // Minimized weighted Lp norms are nondecreasing in p, even though their
  // minimizing modes move. This closes finite intervals including p=1.
  const auto b=box(y,p,p,w,maxit);if(!b.valid)return INFINITY;
  const double h=-special::lgamma(1+1/q)-(1+std::log(q))/q;
  return y.size()*(h-w.lognormal-b.logmoment_lower)+sged_profile::margin(std::abs(y.size()*(h-w.lognormal-b.logmoment_lower)),y.size())*(1+q);
}
inline SgedResult fit(span x,double /*tol*/,int maxit,double xi) {
  SgedResult failed{NAN,NAN,NAN,xi,NAN,0,SgedStatus::CertificateFailure};failed.stop_detail.loglik_upper=INFINITY;
  const auto mm=std::minmax_element(x.begin(),x.end());if(*mm.first==*mm.second){failed.status=SgedStatus::Unbounded;return failed;}
  sged_profile::System system(x);Weights w(xi);if(!system.valid || !(w.left>0 && w.right>0) || !std::isfinite(w.lognormal))return failed;
  const auto y=system.data();const double n=y.size(),face=-n*std::log(y[y.size()-1]-y[0]);
  std::int64_t work=1;const auto cap=certificate_budget(maxit);
  auto best=point(y,1,w,maxit);if(!best.valid)return failed;
  auto recover=[&](const Point& v){auto r=system.recover(v.mode,v.h/w.left,v.h/w.right,v.p,v.ll);if(r.status==SgedStatus::Ok) {
    // Preserve the hold exactly and recheck its actual likelihood.
    r.xi=xi;r.logLik=system.actual(r.mu,r.sigma,r.nu,xi);
    if(!std::isfinite(r.logLik) || std::abs(r.logLik-(v.ll-system.offset))>sged_profile::margin(std::abs(r.logLik)+n*10,n)*(1+v.p))r.status=SgedStatus::CertificateFailure;
  }return r;};
  auto stopped=[&](bool budget) {
    auto r=recover(best);r.status=SgedStatus::CertificateFailure;r.certified=false;r.iterations=static_cast<int>(work);
    r.stop_detail={};r.stop_detail.budget_exhausted=budget;r.stop_detail.loglik_upper=INFINITY;
    if(std::isfinite(r.logLik) && r.sigma>0){const auto c=sged_profile::witness(r,n);r.stop_detail.candidates.push_back(c);r.stop_detail.loglik_lower=c.loglik_lower;}
    return r;
  };
  for(double p:{2.,4.}) {if(work>=cap)return stopped(true);++work;const auto v=point(y,p,w,maxit);if(v.valid && v.ll>best.ll)best=v;}
  struct Segment{double lo,hi;};std::vector<Segment> pending{{1,std::max(2.,n)}},retained;
  // The uniform-in-skew signed tail bound excludes p>=max(2,n).
  constexpr double euler=0.577215664901532860606512090082402431;
  const double tail_sign=std::log(std::max(2.,n))+1-euler-std::log(n);
  if(!(tail_sign>sged_profile::margin(std::log(n),n)))return stopped(false);
  while(!pending.empty()) {
    if(work>=cap)return stopped(true);++work;
    auto cell=pending.back();pending.pop_back();const auto b=box(y,cell.lo,cell.hi,w,maxit);
    if(b.valid && (b.score.lo>0 || b.score.hi<0))continue;
    if(b.valid && std::isfinite(b.curvature.hi) && b.curvature.hi<0 && cell.lo>1) {
      if(work+2>cap)return stopped(true);work+=2;
      const auto left=box(y,cell.lo,cell.lo,w,maxit),right=box(y,cell.hi,cell.hi,w,maxit);
      if((left.valid && left.score.hi<0) || (right.valid && right.score.lo>0))continue;
      double a=cell.lo,h=cell.hi;bool done=false;
      for(std::int64_t k=0;k<solver_budget(maxit);++k) {
        if(work>=cap)return stopped(true);++work;const double c=a/2+h/2;const auto sc=box(y,c,c,w,maxit);if(!sc.valid)break;
        if(sc.score.lo>0)a=c;else if(sc.score.hi<0)h=c;
        else {a=std::max(a,c-std::max(0.,-sc.score.lo)/(-b.curvature.hi));h=std::min(h,c+std::max(0.,sc.score.hi)/(-b.curvature.hi));}
        if(h-a<=2e-8*(1+c)){done=true;break;}
      }
      if(done){retained.push_back({a,h});continue;}
    }
    const double bound=upper(y,cell.lo,cell.hi,w,maxit),incumbent=std::max(best.ll,face);
    if(bound<incumbent-sged_profile::margin(std::abs(incumbent)+n*10,n)*(1+cell.hi))continue;
    const double mid=cell.lo/2+cell.hi/2;if(mid==cell.lo || mid==cell.hi)return stopped(false);
    if(b.valid && cell.hi-cell.lo<=2e-9*(1+mid) && system.narrow(b.coefficients)){retained.push_back(cell);continue;}
    const auto v=point(y,mid,w,maxit);if(v.valid && v.ll>best.ll)best=v;
    pending.push_back({mid,cell.hi});pending.push_back({cell.lo,mid});
  }
  std::sort(retained.begin(),retained.end(),[](const Segment&a,const Segment&b){return a.lo<b.lo;});
  std::vector<Segment> clusters;
  for(auto v:retained){if(!clusters.empty() && v.lo<=clusters.back().hi)clusters.back().hi=v.hi;else clusters.push_back(v);}
  std::vector<Point> points{point(y,1,w,maxit)};std::vector<double> uppers{upper(y,1,1,w,maxit)};
  std::vector<sged_profile::PublicBox> coefficient_boxes{box(y,1,1,w,maxit).coefficients};
  for(auto cell:clusters) {
    if(work>=cap)return stopped(true);++work;const auto enclosure=box(y,cell.lo,cell.hi,w,maxit);
    if(!enclosure.valid || cell.hi-cell.lo>2e-7*(1+cell.lo))return stopped(false);
    double a=cell.lo,h=cell.hi;
    if(std::isfinite(enclosure.curvature.hi) && enclosure.curvature.hi<0) {
      for(std::int64_t k=0;k<solver_budget(maxit);++k) {
        if(work>=cap)return stopped(true);++work;const double mid=a/2+h/2;const auto sc=box(y,mid,mid,w,maxit);if(!sc.valid)break;
        double low=a,high=h;
        if(sc.score.lo>0)low=mid;else if(sc.score.hi<0)high=mid;
        low=std::max(low,mid-std::max(0.,-sc.score.lo)/(-enclosure.curvature.hi));
        high=std::min(high,mid+std::max(0.,sc.score.hi)/(-enclosure.curvature.hi));
        if(low>high)return stopped(false);
        if(high-low>=.9*(h-a))break;
        a=low;h=high;
        if(h-a<=2e-12*(1+a))break;
      }
    }
    const auto final_box=box(y,a,h,w,maxit);
    if(!final_box.valid || !system.narrow(final_box.coefficients))return stopped(false);
    const auto v=point(y,a/2+h/2,w,maxit);if(!v.valid)return stopped(false);
    points.push_back(v);coefficient_boxes.push_back(final_box.coefficients);uppers.push_back(upper(y,a,h,w,maxit));
  }
  std::size_t winner=0;for(std::size_t j=0;j<points.size();++j)if(points[j].ll>points[winner].ll)winner=j;
  best=points[winner];const double pad=sged_profile::margin(std::abs(best.ll)+n*10,n)*(1+best.p),tie=1e-8*n;
  double global=face;for(double u:uppers)global=std::max(global,u);
  if(face>=best.ll-pad-tie) {
    double finite=-INFINITY;for(double u:uppers)finite=std::max(finite,u);
    auto r=stopped(false);
    const FitCandidate limit{{*mm.first,*mm.second},face-system.offset-pad,face-system.offset+pad,StopModel::Unif};
    if(finite>=face-pad) {r.stop_detail.candidates.push_back(limit);r.stop_detail.comparison_tie=true;r.stop_detail.loglik_upper=system.original_upper(global,best.p);return r;}
    r.status=SgedStatus::UniformBoundary;r.certified=true;r.logLik=face-system.offset;
    r.stop_detail=limit_detail({*mm.first,*mm.second},r.logLik,StopModel::Unif);r.stop_detail.loglik_lower=r.logLik-pad;r.stop_detail.loglik_upper=r.logLik+pad;return r;
  }
  auto out=recover(best);if(out.status!=SgedStatus::Ok)return stopped(false);
  for(std::size_t j=0;j<points.size();++j)if(j!=winner) {
    if(uppers[j]>=best.ll-pad-tie) {
      auto r=stopped(false),other=recover(points[j]);if(other.status!=SgedStatus::Ok)return r;
      r.stop_detail.candidates={sged_profile::witness(out,n),sged_profile::witness(other,n)};r.stop_detail.comparison_tie=true;r.stop_detail.loglik_upper=system.original_upper(global,best.p);return r;
    }
  }
  if(best.p==1 && best.modebox.hi>best.modebox.lo) {
    auto r=stopped(false);r.stop_detail.candidates.clear();
    for(double c:{best.modebox.lo,best.modebox.hi}) {const auto v=point(y,1,w,maxit,c);const auto candidate=recover(v);if(candidate.status!=SgedStatus::Ok)return stopped(false);r.stop_detail.candidates.push_back(sged_profile::witness(candidate,n));}
    r.stop_detail.comparison_tie=true;r.stop_detail.loglik_upper=system.original_upper(global,best.p);return r;
  }
  if(!system.compatible(coefficient_boxes[winner],out))return stopped(false);
  out.certified=true;out.iterations=static_cast<int>(work);if(out.nu==1)out.restricted=16u;
  const auto candidate_bounds=sged_profile::witness(out,n);
  out.stop_detail.loglik_lower=candidate_bounds.loglik_lower;out.stop_detail.loglik_upper=std::max(candidate_bounds.loglik_upper,system.original_upper(global,best.p));return out;
}
} // namespace sged_weighted_shape
