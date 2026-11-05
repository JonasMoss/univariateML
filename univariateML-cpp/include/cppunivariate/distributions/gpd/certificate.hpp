// Global scalar certificates. Proof and binary64 contract: README.md.
#pragma once
#include <queue>
namespace cppunivariate {
namespace gpd_cert_detail {
constexpr double margin=1e-8;
struct Sum {
  double value=0, correction=0;
  void add(double x) {const double y=x-correction,t=value+y;correction=(t-value)-y;value=t;}
};
struct Interval {double lo,hi;};
inline Interval add(Interval a,Interval b) {return {a.lo+b.lo,a.hi+b.hi};}
inline Interval neg(Interval a) {return {-a.hi,-a.lo};}
inline Interval mul(Interval a,Interval b) {
  return {std::min({a.lo*b.lo,a.lo*b.hi,a.hi*b.lo,a.hi*b.hi}),
          std::max({a.lo*b.lo,a.lo*b.hi,a.hi*b.lo,a.hi*b.hi})};
}
inline Interval inverse(Interval a) {return {1/a.hi,1/a.lo};}
struct Point {double t,b,D,P,R,h,error,score;};
struct Segment {
  Point a,b;double upper;
  bool operator<(const Segment& s)const{return upper<s.upper;}
};
struct Data {
  span y;double M,eta,c;bool held;
};
inline Point evaluate(const Data& d,double t) {
  const double b=std::expm1(t),e=std::exp(t);
  Sum D,P,R;
  for(double y:d.y) {
    const double v=y/d.M,z=b*v;
    double a=0,p=0,r=0;
    if(std::abs(z)<.25) {
      // D=E integral_0^v 1/(1+b s) ds. At |b v|<1/4,
      // forty terms bound all three relative remainders below 1e-20.
      double power=1;
      for(int k=0;k<40;++k) {
        a+=power/(k+1.);p+=power*(k+1.)/(k+2.);
        r+=power*(k+1.)*(k+2.)/(k+3.);power*=-z;
      }
      a*=v;p*=v*v;r*=v*v*v;
    } else {
      // Stable even very close to the moving endpoint. M-y is exact
      // by Sterbenz when cancellation matters; never form 1+b*v there.
      const double den=t<0?(d.M-y)/d.M+v*e:1+z;
      const double l=std::log(den),q=z/den;
      a=l/b;p=(l-q)/(b*b);r=(2*l-2*q-q*q)/(b*b*b);
    }
    D.add(a/d.y.size());P.add(p/d.y.size());R.add(r/d.y.size());
  }
  const double h=d.held?-(1/d.c+b)*D.value:-std::log(D.value)-b*D.value-1;
  const double score=e*(d.held?-D.value+(1/d.c+b)*P.value:
      P.value/D.value-D.value+b*P.value);
  const double error=d.eta*(1+std::abs(h)+std::abs(std::log(D.value))+std::abs(b*D.value));
  return {t,b,D.value,P.value,R.value,h,error,score};
}
struct Bounds {Interval score,curvature;double upper;};
inline Bounds bounds(const Data& d,const Point& a,const Point& b) {
  const double eta=d.eta;
  const Interval D{b.D*(1-eta),a.D*(1+eta)},P{b.P*(1-eta),a.P*(1+eta)},
      R{b.R*(1-eta),a.R*(1+eta)},B{a.b,b.b},E{std::exp(a.t),std::exp(b.t)};
  Interval g,k;
  if(d.held) {
    const Interval factor=add({1/d.c,1/d.c},B);
    g=add(neg(D),mul(factor,P));k=add(mul({2,2},P),neg(mul(factor,R)));
  } else {
    const Interval ratio=mul(P,inverse(D));
    g=add(add(ratio,neg(D)),mul(B,P));
    k=add(add(mul(ratio,ratio),neg(mul(R,inverse(D)))),
          add(mul({2,2},P),neg(mul(B,R))));
  }
  Interval curvature=add(mul(mul(E,E),k),mul(E,g));
  Interval score=mul(E,g);
  const double se=16*eta*(1+std::max(std::abs(score.lo),std::abs(score.hi)));
  const double ce=32*eta*(1+std::max(std::abs(curvature.lo),std::abs(curvature.hi)));
  score.lo-=se;score.hi+=se;curvature.lo-=ce;curvature.hi+=ce;
  const double width=b.t-a.t;
  const double upper=std::max(a.h+a.error,b.h+b.error)+std::max(0.,-curvature.lo)*width*width/8;
  return {score,curvature,upper+eta*(1+std::abs(upper))};
}
struct Root {Point p;double lo,hi,upper;};
inline bool isolate(const Data& d,Point a,Point b,double tol,int maxit,Root& out) {
  const auto box=bounds(d,a,b);
  const double se=32*d.eta*(1+std::max(std::abs(a.t),std::abs(b.t)));
  if(!(box.curvature.hi< -se && a.score>se && b.score< -se))return false;
  const double lo=a.t,hi=b.t;
  for(int j=0;j<maxit;++j) {
    Point p=evaluate(d,(a.t+b.t)/2);
    if(p.score>se)a=p;
    else if(p.score< -se)b=p;
    else {
      double center=p.t;
      for(int k=0;k<8;++k) {
        const Point q=evaluate(d,center);
        const double e=std::exp(q.t),g=q.score/e;
        const double curvature=e*e*(d.held?2*q.P-(1/d.c+q.b)*q.R:
            q.P*q.P/(q.D*q.D)-q.R/q.D+2*q.P-q.b*q.R)+e*g;
        const double next=center-q.score/curvature;
        if(!(next>lo && next<hi))break;
        center=next;
      }
      double radius=std::max(1e-11,4*se/(-box.curvature.hi));
      bool signed_box=false;
      for(int k=0;k<24;++k,radius*=2) {
        if(center-radius<=lo || center+radius>=hi)break;
        const Point l=evaluate(d,center-radius),r=evaluate(d,center+radius);
        if(l.score>se && r.score< -se){a=l;b=r;signed_box=true;break;}
      }
      if(!signed_box)return false;
    }
    const double shape_a=d.held?d.c*a.b:a.b*a.D,shape_b=d.held?d.c*b.b:b.b*b.D;
    const bool coefficient_box=(d.held || a.D/b.D-1<3e-7) &&
        std::abs(shape_b-shape_a)<3e-7*std::max({1.,std::abs(shape_a),std::abs(shape_b)});
    if(j>0 && (b.t-a.t<2e-7 || coefficient_box)) {
      Point p=evaluate(d,(a.t+b.t)/2);
      // Refine the representative, while retaining the signed enclosure.
      for(int k=0;k<12;++k) {
        const double e=std::exp(p.t),g=p.score/e;
        const double cur=e*e*(d.held?2*p.P-(1/d.c+p.b)*p.R:
            p.P*p.P/(p.D*p.D)-p.R/p.D+2*p.P-p.b*p.R)+e*g;
        const double next=p.t-p.score/cur;
        if(!(next>a.t && next<b.t))break;
        const double step=std::abs(next-p.t);
        p=evaluate(d,next);
        if(step<tol*(1+std::abs(next)))break;
      }
      const auto narrow=bounds(d,a,b);
      out={p,lo,hi,narrow.upper};return true;
    }
  }
  return false;
}
inline GpdResult fit(span y,double mu,double tol,int maxit,double sigma=NAN) {
  const bool held=std::isfinite(sigma);
  double M=0,min=INFINITY;std::size_t ties=0;
  for(double v:y){M=std::max(M,v);if(v>0)min=std::min(min,v);}
  auto stop=[&](GpdCertificateFailure reason,int work=0) {
    GpdResult out{mu,NAN,NAN,NAN,work,GpdStatus::IterationFailure};
    out.certificate_failure=reason;return out;
  };
  if(!(M>0)) {
    auto out=stop(GpdCertificateFailure::Tie);
    out.stop_detail.loglik_lower=out.stop_detail.loglik_upper=-static_cast<double>(y.size())*std::log(sigma);
    out.stop_detail.candidates={{{mu,sigma,-1},out.stop_detail.loglik_lower,out.stop_detail.loglik_upper},
                                {{mu,sigma,0},out.stop_detail.loglik_lower,out.stop_detail.loglik_upper}};
    return out;
  }
  if(held && sigma>=M)return {mu,sigma,-1,gpd_loglik(y,sigma,-1),0,GpdStatus::Ok};
  for(double v:y)if(v==M)++ties;
  Data d{y,M,512*std::numeric_limits<double>::epsilon()*(1+std::log(y.size()+1.)),held?sigma/M:1,held};
  if(!(min/M>=std::numeric_limits<double>::min()) || !(d.c>0) || !std::isfinite(1/d.c) || !(d.eta<1e-8))
    return stop(GpdCertificateFailure::Arithmetic);
  double low=-1,high=1,negative_tail_upper=-INFINITY;
  bool floor= !held || sigma>=M;
  if(held && floor)low=std::log1p(-M/sigma);
  else if(!held) {
    // On the admissible negative ray h=log(-b)-log(A)+A-1,
    // A=-bD increases towards the endpoint. For A<=1 the latter
    // expression decreases in A, so the entire omitted tail is bounded
    // by -log(A(low))+A(low)-1. A>=1 is dominated by the uniform fit.
    low=-32;
    const Point left=evaluate(d,low);
    const double A=-left.b*left.D*(1-4*d.eta);
    negative_tail_upper=A>=1?0.:-std::log(A)+A-1+left.error;
    if(A>=1) {
      double l=low,h=0;
      for(int j=0;j<48;++j) {
        const double mid=(l+h)/2;const Point p=evaluate(d,mid);
        if(-p.b*p.D*(1-4*d.eta)>1)l=mid;else h=mid;
      }
      low=l;
    }
  } else {
    // For t<-1: h'(t)>0 if (1-c)(1-exp(t))*wmax > exp(t)*(-t).
    const double w=static_cast<double>(ties)/y.size();
    while(low> -32 && !((1-d.c)*(-std::expm1(low))*w>std::exp(low)*(-low)*(1+32*d.eta)))low*=2;
    if(!((1-d.c)*(-std::expm1(low))*w>std::exp(low)*(-low)*(1+32*d.eta)))return stop(GpdCertificateFailure::Arithmetic);
  }
  // sigma=M: the finite uniform boundary is t=-infinity. Shape score
  // need not be checked there: h<=0 follows from nonnegative log ratios.
  if(held && sigma==M)return {mu,sigma,-1,gpd_loglik(y,sigma,-1),0,GpdStatus::Ok};
  auto tail=[&](double t) {
    const double b=std::expm1(t);
    return held ? d.c*b>2*t*(1+32*d.eta) && b*(min/M)>1+32*d.eta:
        b*(min/M)>t*(1+32*d.eta);
  };
  while(high<64 && !tail(high))high*=2;
  if(!tail(high) || !std::isfinite(low))return stop(GpdCertificateFailure::Arithmetic);
  Point a=evaluate(d,low),b=evaluate(d,high);
  if(!std::isfinite(a.h) || !std::isfinite(b.h))return stop(GpdCertificateFailure::Arithmetic);
  const double base=held?-std::log(sigma):-std::log(M),n=y.size();
  std::vector<Root> roots;
  std::priority_queue<Segment> queue;
  queue.push({a,b,bounds(d,a,b).upper});
  int work=2;const int budget=128*std::min(maxit,100000);
  double best=floor?0.:-INFINITY;
  Point bestpoint=evaluate(d,0);
  auto failure=[&](GpdCertificateFailure reason,double upper) {
    auto out=stop(reason,work);
    out.stop_detail.loglik_lower=n*(base+std::max(best,bestpoint.h-bestpoint.error));
    double global_upper=std::max({best,upper,negative_tail_upper,queue.empty()?-INFINITY:queue.top().upper,a.h+a.error,b.h+b.error});
    for(const auto& r:roots)global_upper=std::max(global_upper,r.upper);
    out.stop_detail.loglik_upper=n*(base+global_upper);
    const double point_scale=held?sigma:M*bestpoint.D;
    const double point_shape=held?d.c*bestpoint.b:bestpoint.b*bestpoint.D;
    if(point_scale>0 && std::isfinite(point_scale) && point_shape>=-1 &&
        std::isfinite(gpd_loglik(y,point_scale,point_shape)))
      out.stop_detail.candidates.push_back({{mu,point_scale,point_shape},n*(base+bestpoint.h-bestpoint.error),n*(base+bestpoint.h+bestpoint.error)});
    if(floor)out.stop_detail.candidates.push_back({{mu,held?sigma:M,-1},n*(base+0.),n*(base+0.)});
    for(const auto& r:roots) {
      const double scale=held?sigma:M*r.p.D,shape=held?d.c*r.p.b:r.p.b*r.p.D;
      if(scale>0 && std::isfinite(scale) && shape>=-1 && std::isfinite(gpd_loglik(y,scale,shape)))
        out.stop_detail.candidates.push_back({{mu,scale,shape},n*(base+r.p.h-r.p.error),n*(base+r.upper)});
    }
    return out;
  };
  if(maxit<2)return failure(GpdCertificateFailure::Budget,queue.top().upper);
  while(!queue.empty()) {
    const Segment s=queue.top();queue.pop();
    bool covered=false;
    for(const auto& r:roots)if(s.a.t>=r.lo && s.b.t<=r.hi){covered=true;break;}
    if(covered)continue;
    if(!held && -s.b.b*s.b.D>1+32*d.eta)continue;
    const auto box=bounds(d,s.a,s.b);
    if(s.upper<best-margin)continue;
    if(box.score.lo>0 || box.score.hi<0)continue;
    const double se=32*d.eta*(1+std::max(std::abs(s.a.t),std::abs(s.b.t)));
    if(box.curvature.hi< -se &&
        (s.a.score< -se || s.b.score>se))continue;
    if(work>=budget)return failure(GpdCertificateFailure::Budget,s.upper);
    if(s.b.t-s.a.t<.25) {
      Root r;
      bool valid=isolate(d,s.a,s.b,tol,maxit,r);
      if(!valid) {
        const double w=s.b.t-s.a.t;
        const Point l=evaluate(d,std::max(low,s.a.t-w)),h=evaluate(d,std::min(high,s.b.t+w));
        work+=2;valid=isolate(d,l,h,tol,maxit,r);
      }
      if(valid && (held || r.p.b*r.p.D>=-1)) {
        bool duplicate=false;
        for(const auto& old:roots)if(r.p.t>old.lo && r.p.t<old.hi)duplicate=true;
        if(!duplicate){roots.push_back(r);best=std::max(best,r.p.h-r.p.error);}
        continue;
      }
    }
    const double mid=(s.a.t+s.b.t)/2;
    if(!(mid>s.a.t && mid<s.b.t))return failure(GpdCertificateFailure::Arithmetic,s.upper);
    const Point p=evaluate(d,mid);++work;
    if(!std::isfinite(p.h) || !std::isfinite(p.error))return failure(GpdCertificateFailure::Arithmetic,s.upper);
    if((held || p.b*p.D>=-1) && p.h>bestpoint.h)bestpoint=p;
    queue.push({s.a,p,bounds(d,s.a,p).upper});queue.push({p,s.b,bounds(d,p,s.b).upper});
  }
  int winner=-1;
  double value=floor?0.:-INFINITY;
  for(std::size_t j=0;j<roots.size();++j)if(roots[j].p.h>value){winner=static_cast<int>(j);value=roots[j].p.h;}
  const double lower=winner<0?0.:roots[winner].p.h-roots[winner].p.error;
  if(negative_tail_upper>0 && !(lower>negative_tail_upper+margin))return failure(GpdCertificateFailure::Arithmetic,negative_tail_upper);
  if(winner>=0 && floor && !(lower>margin))return failure(GpdCertificateFailure::Tie,roots[winner].upper);
  for(std::size_t j=0;j<roots.size();++j)
    if(static_cast<int>(j)!=winner && !(lower>roots[j].upper+margin))return failure(GpdCertificateFailure::Tie,roots[j].upper);
  if(winner<0) {
    if(!floor)return failure(GpdCertificateFailure::Arithmetic,INFINITY);
    return {mu,held?sigma:M,-1,n*base,work,GpdStatus::Ok};
  }
  const Point p=roots[winner].p;
  const double scale=held?sigma:M*p.D,shape=held?d.c*p.b:p.b*p.D;
  const double ll=gpd_loglik(y,scale,shape);
  const double restored_t=std::log1p(shape*(M/scale));
  if(!(scale>0) || !std::isfinite(scale) || !std::isfinite(shape) || shape< -1 ||
      !std::isfinite(ll) || std::abs(ll/n-(base+p.h))>margin ||
      std::abs(scale/M-(held?d.c:p.D))>1e-8*(held?d.c:p.D) ||
      !std::isfinite(restored_t) || std::abs(restored_t-p.t)>1e-7)return failure(GpdCertificateFailure::Arithmetic,roots[winner].upper);
  return {mu,scale,shape,ll,work,GpdStatus::Ok};
}
} // namespace gpd_cert_detail
} // namespace cppunivariate
