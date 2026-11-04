// Scalar global certificate; arithmetic contract and proof in README.md.
// Included after LomaxResult and the optional Newton proposer in mle.hpp.
#pragma once
#include <queue>
namespace cppunivariate {
namespace lomax_cert_detail {
constexpr double margin = 1e-8;
struct Value { double y, logy, weight; };
struct Point { double z, A, B, C, h, score, error; };
struct Segment {
  Point a,b; double upper;
  bool operator<(const Segment& v) const { return upper<v.upper; }
};
// Compensated accumulation makes the arithmetic allowance essentially
// independent of n (the n*epsilon^2 term is retained in the pad).
struct Sum {
  double value=0, correction=0;
  void add(double x) { const double y=x-correction,t=value+y; correction=(t-value)-y; value=t; }
};
struct Data {
  std::vector<Value> values;
  double logmean=0, logubar=0, second=0, error=0;
};
inline Point evaluate(const Data& d,double z) {
  Sum a,b,c,delta,first;
  const double q=std::exp(z);
  for(const auto& v:d.values) {
    const double w=z+v.logy, e=std::exp(-std::abs(w));
    const double p=w>=0?1/(1+e):e/(1+e);
    a.add(v.weight*(std::max(w,0.)+std::log1p(e)));
    b.add(v.weight*p);c.add(v.weight*p*(1-p));
    if(z < -12) {
      const double r=q*v.y;
      // Taylor remainder bounded by r^5/6 below 1e-4.
      const double ratio=r<1e-4?r*(-.5+r*(1./3+r*(-.25+r*.2))):std::log1p(r)/r-1;
      delta.add(v.weight*v.y*ratio);first.add(v.weight*v.y);
    }
  }
  double h=z-std::log(a.value)-a.value;
  if(z < -12) h=-std::log1p(delta.value+first.value-1)-a.value;
  const double error=d.error*(1+std::abs(z)+std::abs(std::log(a.value))+a.value);
  return {z,a.value,b.value,c.value,h,1-b.value/a.value-b.value,error};
}
inline double upper(const Data& d,const Point& a,const Point& b) {
  // h'' >= -5/4, so h lies below its chord plus 5*(b-a)^2/32.
  const double q=std::exp(b.z+d.error), t=q*d.second*(1+4*d.error)/2;
  const double bound=t<1?std::min(1.25,t/(1-t)+q):1.25;
  const double value=std::max(a.h+a.error,b.h+b.error)+bound/8*(b.z-a.z)*(b.z-a.z);
  return value+d.error*(1+std::abs(value));
}
inline double curvature_upper(const Data& d,const Point& a,const Point& b) {
  // A and B increase; C is enclosed term by term using p(1-p).
  Sum low;
  for(const auto& v:d.values) {
    auto variance=[&](double z) {const double e=std::exp(-std::abs(z+v.logy));return e/((1+e)*(1+e));};
    low.add(v.weight*std::min(variance(a.z),variance(b.z)));
  }
  const double al=a.A*(1-d.error), ah=b.A*(1+d.error);
  const double bh=b.B*(1+d.error),cl=low.value*(1-d.error);
  if(!(al>0))return INFINITY;
  return bh*bh/(al*al)-cl/ah-cl+d.error;
}
struct Root { double lo=0,hi=0,rootlo=0,roothi=0,lambda=NAN,kappa=NAN,h=-INFINITY,error=0; bool valid=false; };
inline Root isolate(const Data& d,Point a,Point b,int maxit) {
  Root r;
  if(!(a.score>4*d.error && b.score< -4*d.error && curvature_upper(d,a,b)< -4*d.error))return r;
  r.lo=a.z;r.hi=b.z;
  // Endpoint score errors grow as B/A, but B/A <= 1 identically.
  // Stop only with signed endpoints and a narrow root enclosure.
  Point left=a,right=b;
  for(int j=0;j<maxit;++j) {
    const Point mid=evaluate(d,(left.z+right.z)/2);
    const double se=4*d.error;
    if(mid.score>se) left=mid;
    else if(mid.score< -se) right=mid;
    else {
      // Widen about the proposal until both signs can be certified.
      double center=mid.z;
      for(int k=0;k<8;++k) {
        const Point p=evaluate(d,center);
        const double slope=p.B*p.B/(p.A*p.A)-p.C/p.A-p.C;
        const double next=center-p.score/slope;
        if(!(next>a.z&&next<b.z))break;
        center=next;
      }
      const Point proposal=evaluate(d,center);
      const double slope=proposal.B*proposal.B/(proposal.A*proposal.A)-proposal.C/proposal.A-proposal.C;
      double radius=std::max(1e-10,5*d.error/std::abs(slope));
      bool signed_box=false;
      for(int k=0;k<24;++k,radius*=2) {
        if(center-radius<=a.z || center+radius>=b.z)break;
        const Point l=evaluate(d,center-radius),h=evaluate(d,center+radius);
        if(l.score>se && h.score< -se){left=l;right=h;signed_box=true;break;}
      }
      if(!signed_box)return r;
    }
    // Both exp(z) and 1/A(z) vary by at most exp(width), since B/A<=1.
    if(j>0 && right.z-left.z<7e-7) {
      r.rootlo=left.z-1e-7;r.roothi=right.z+1e-7;
      double z=(left.z+right.z)/2;
      for(int k=0;k<8;++k) {
        const Point p=evaluate(d,z);
        const double slope=p.B*p.B/(p.A*p.A)-p.C/p.A-p.C;
        const double next=z-p.score/slope;
        if(!(next>left.z&&next<right.z))break;
        z=next;
      }
      const Point center=evaluate(d,z);
      r.lambda=std::exp(center.z-d.logmean);r.kappa=1/center.A;
      r.h=center.h;r.error=center.error;
      r.valid=std::isfinite(r.lambda)&&r.lambda>0&&std::isfinite(r.kappa)&&r.kappa>0;
      // Positive subnormal results can have large relative rounding error.
      // Certify the coordinates of the stored coefficients, not merely the
      // real-valued exp/reciprocal proposals.
      if(r.valid)
        r.valid=std::abs(std::log(r.lambda)+d.logmean-center.z)<1e-8 &&
            std::abs(std::log(r.kappa)+std::log(center.A))<1e-8;
      return r;
    }
  }
  return r;
}
inline LomaxResult fit(span x,double tol,int maxit,double lambda0) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  auto stop=[&](LomaxStatus status,int work=0,
      LomaxCertificateFailure reason=LomaxCertificateFailure::Arithmetic){
    return LomaxResult{nan,nan,nan,work,status,
        status==LomaxStatus::IterationFailure?reason:LomaxCertificateFailure::None};
  };
  if(x.size()==0 || !(tol>0) || !std::isfinite(tol) || maxit<1)return stop(LomaxStatus::InvalidStart);
  double largest=0;
  bool constant=true;
  for(double v:x){if(!(v>0)||!std::isfinite(v))return stop(LomaxStatus::InvalidStart);largest=std::max(largest,v);if(v!=x[0])constant=false;}
  auto exponential=[&](int work) {
    auto out=stop(LomaxStatus::ExponentialLimit,work);
    double scaled_mean=0;for(double v:x)scaled_mean+=(v/largest)/x.size();
    const double logmean=std::log(largest)+std::log(scaled_mean);
    out.stop_detail.limit_coef={std::exp(-logmean)};
    out.stop_detail.limit_loglik=x.size()*(-logmean-1);
    return out;
  };
  if(constant)return exponential(0);
  const double n=static_cast<double>(x.size());
  Sum mean;
  for(double v:x)mean.add((v/largest)/n);
  if(!(mean.value>0))return stop(LomaxStatus::IterationFailure);
  Data d;d.logmean=std::log(largest)+std::log(mean.value);
  d.error=256*std::numeric_limits<double>::epsilon()*(1+std::abs(std::log(largest))+std::abs(std::log(mean.value)))*(1+n*std::numeric_limits<double>::epsilon());
  std::vector<double> sorted(x.begin(),x.end());std::sort(sorted.begin(),sorted.end());
  Sum logs,seconds;double maxlog=0;
  for(std::size_t i=0;i<sorted.size();) {
    std::size_t j=i+1;while(j<sorted.size()&&sorted[j]==sorted[i])++j;
    const double scaled=sorted[i]/largest;
    // A subnormal intermediate has no useful relative error bound.
    if(scaled<std::numeric_limits<double>::min())return stop(LomaxStatus::IterationFailure);
    const double y=scaled/mean.value,weight=(j-i)/n;
    if(!(y>0)||!std::isfinite(y))return stop(LomaxStatus::IterationFailure);
    const double ly=std::log(y);maxlog=std::max(maxlog,std::abs(ly));
    d.values.push_back({y,ly,weight});logs.add(weight*ly);seconds.add(weight*y*y);i=j;
  }
  d.error+=256*std::numeric_limits<double>::epsilon()*maxlog*(1+n*std::numeric_limits<double>::epsilon());
  d.logubar=-logs.value;d.second=seconds.value;
  // Left tail: A >= q-q^2 E[y^2]/2, hence h <= -log(1-q E[y^2]/2).
  // Right tail: A >= z + E[log y], hence h <= logubar-log(z-logubar).
  const double lo=std::log(1e-10)-std::log(std::max(1.,d.second));
  const double hi=d.logubar+std::exp(d.logubar+4*d.error);
  if(!std::isfinite(lo)||!std::isfinite(hi)||!(hi>lo))return stop(LomaxStatus::IterationFailure);
  Point a=evaluate(d,lo),b=evaluate(d,hi);
  if(!std::isfinite(a.h)||!std::isfinite(b.h))return stop(LomaxStatus::IterationFailure);
  std::priority_queue<Segment> queue;queue.push({a,b,upper(d,a,b)});
  Root root;int work=2;
  Point best_point=a.h>b.h?a:b;
  // lambda0 is only a nomination hint, never a restriction of the search.
  if(std::isfinite(lambda0)&&lambda0>0) {
    const double z=std::log(lambda0)+d.logmean;
    if(z>lo&&z<hi){Point p=evaluate(d,z);if(p.h>best_point.h)best_point=p;queue.pop();queue.push({a,p,upper(d,a,p)});queue.push({p,b,upper(d,p,b)});++work;}
  }
  // The work budget is explicit and independent of convergence tolerances.
  if(!(5.1e-11+4*d.error<margin))return stop(LomaxStatus::IterationFailure,work);
  auto budget_stop=[&](double upper_bound) {
    auto out=stop(LomaxStatus::IterationFailure,work,LomaxCertificateFailure::Budget);
    const double base=-d.logmean-1;
    out.stop_detail.loglik_lower=n*(base+(root.valid?std::max(0.,root.h-root.error):0.));
    out.stop_detail.loglik_upper=n*(base+std::max({margin,upper_bound,queue.empty()?-INFINITY:queue.top().upper,root.valid?root.h+root.error:0.}));
    const double lambda=std::exp(best_point.z-d.logmean),kappa=1/best_point.A;
    if(lambda>0 && std::isfinite(lambda) && kappa>0 && std::isfinite(kappa)) {
      const double lower=n*(base+best_point.h-best_point.error),upper=n*(base+best_point.h+best_point.error);
      out.stop_detail.candidates.push_back({{lambda,kappa},lower,upper});
      out.stop_detail.loglik_lower=std::max(out.stop_detail.loglik_lower,lower);
    }
    out.stop_detail.candidates.push_back({{std::exp(-d.logmean)},n*base,n*base,StopModel::Exp});
    if(root.valid)out.stop_detail.candidates.push_back({{root.lambda,root.kappa},n*(base+root.h-root.error),n*(base+root.h+root.error)});
    return out;
  };
  const int budget=std::min(maxit,100000)*128;
  while(!queue.empty()) {
    Segment s=queue.top();queue.pop();
    if(root.valid && s.a.z>=root.lo && s.b.z<=root.hi)continue;
    const double target=root.valid?root.h-root.error:margin;
    if(s.upper<target)continue;
    // A differentiable global maximum in the compact interior must have
    // zero score, including at an artificial subdivision boundary.
    const double al=s.a.A*(1-d.error),ah=s.b.A*(1+d.error);
    const double bl=s.a.B*(1-d.error),bh=s.b.B*(1+d.error);
    if(al>0 && (1-bh/al-bh>4*d.error || 1-bl/ah-bl< -4*d.error))continue;
    if(s.b.z-s.a.z<.25 && curvature_upper(d,s.a,s.b)< -4*d.error &&
       (s.a.score< -4*d.error || s.b.score>4*d.error))continue;
    if(work>=budget)return budget_stop(s.upper);
    if(s.b.z-s.a.z<.25) {
      Root trial=isolate(d,s.a,s.b,maxit);
      // A root may lie on a subdivision boundary whose score enclosure
      // contains zero. Descendants alone would then never have two signed
      // endpoints. An overlapping neighborhood closes that dependency.
      if(!trial.valid && s.a.score>=-4*d.error && s.b.score<=4*d.error) {
        const double width=s.b.z-s.a.z;
        const Point left=evaluate(d,std::max(lo,s.a.z-width));
        const Point right=evaluate(d,std::min(hi,s.b.z+width));
        if(work+2>budget)return budget_stop(s.upper);
        work+=2;
        trial=isolate(d,left,right,maxit);
      }
      if(trial.valid && trial.h-trial.error>margin && (!root.valid || trial.h-trial.error>root.h+root.error+1e-12)) {
        root=trial;
        // This whole segment is strictly concave and has exactly one root.
        continue;
      }
    }
    const double mid=(s.a.z+s.b.z)/2;
    if(!(mid>s.a.z&&mid<s.b.z))return stop(LomaxStatus::IterationFailure,work);
    const Point p=evaluate(d,mid);++work;
    if(p.h>best_point.h)best_point=p;
    if(!std::isfinite(p.h)||!std::isfinite(p.error))return stop(LomaxStatus::IterationFailure,work);
    queue.push({s.a,p,upper(d,s.a,p)});queue.push({p,s.b,upper(d,p,s.b)});
  }
  if(!root.valid)return exponential(work);
  if(!(root.h-root.error>margin) || !(5.1e-11+4*d.error<root.h-root.error))return stop(LomaxStatus::IterationFailure,work);
  // Preserve the historical Newton coefficients only when they lie in the
  // certified narrow root enclosure; convergence alone is never sufficient.
  CPPUNIV_EXTENDED_REAL total=0;
  for(double v:x)total+=v;
  const double oldmean=static_cast<double>(total/n);
  if(oldmean>0 && std::isfinite(oldmean)) {
    std::vector<double> y; y.reserve(x.size());
    for(double v:x)y.push_back(v/oldmean);
    auto candidate=lomax_newton_candidate(span{y.data(),y.size()},tol,maxit,
        std::isfinite(lambda0)?lambda0*oldmean:lambda0);
    const double z=std::log(candidate.lambda)+d.logmean-std::log(oldmean);
    if(candidate.status==LomaxStatus::Ok && z>=root.rootlo && z<=root.roothi &&
       std::abs(std::log(candidate.kappa/root.kappa))<2e-7) {
      candidate.lambda/=oldmean;
      candidate.logLik-=n*std::log(oldmean);candidate.iterations=work;
      if(candidate.lambda>0 && std::isfinite(candidate.lambda) && std::isfinite(candidate.logLik)) {
        const double restored_z=std::log(candidate.lambda)+d.logmean;
        if(restored_z>=root.rootlo && restored_z<=root.roothi)return candidate;
      }
    }
  }
  const double logLik=n*(root.h-d.logmean-1);
  if(!std::isfinite(logLik))return stop(LomaxStatus::IterationFailure,work);
  return {root.lambda,root.kappa,logLik,work,LomaxStatus::Ok};
}
} // namespace lomax_cert_detail
} // namespace cppunivariate
