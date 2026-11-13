#pragma once
#include "../../core/work_budget.hpp"
// Conditional exact-arithmetic certificate. See README.md for the score,
// tail inequalities, interval bounds and practical floating-point allowance.
namespace cppunivariate {
namespace ztnbinom_cert_detail {
struct Interval { double lo,hi; };
struct Sum {
  double value=0,correction=0;
  void add(double v) {const double y=v-correction,t=value+y;correction=(t-value)-y;value=t;}
};
inline double e(double u) {
  // 1/expm1(u)-1/u; omitted term below 2.1e-17 at u<.1.
  if(u<.1) {const double v=u*u;return -.5+u*(1./12-v*(1./720-v*(1./30240-v/1209600)));}
  return 1/std::expm1(u)-1/u;
}
inline double j(double t) {
  if(t<.1) {const double v=t*t;return t/2-v*(1./12-v*(1./720-v*(1./30240-v/1209600)));}
  return 1-t/std::expm1(t);
}
inline double jp(double t) {
  if(t<.1) {const double v=t*t;return .5-t*(1./6-v*(1./180-v*(1./5040-v/151200)));}
  const double q=std::exp(-t),d=-std::expm1(-t);
  return q*(t-1+q)/(d*d);
}
inline double mean(double r,double t) {
  const double u=r*t;
  return (std::expm1(t)/t)*(u==0?1:u/(-std::expm1(-u)));
}
inline Interval theta(double r,double m,int maxit=200) {
  maxit = static_cast<int>(solver_budget(maxit));
  double lo=0,hi=r==0?2*std::log1p(m):std::log1p(m/r);
  if(!std::isfinite(hi)||!(hi>0))return {NAN,NAN};
  hi*=1+4096*std::numeric_limits<double>::epsilon();
  const double pad=256*std::numeric_limits<double>::epsilon()*(1+m);
  if(!(mean(r,hi)>m+pad))return {NAN,NAN};
  for(int k=0;k<solver_budget(maxit);++k) {
    const double mid=lo+(hi-lo)/2,v=mean(r,mid);
    if(v<m-pad)lo=mid;
    else if(v>m+pad)hi=mid;
    else {
      const double delta=2048*std::numeric_limits<double>::epsilon()*(1+mid);
      const double a=std::max(lo,mid-delta),b=std::min(hi,mid+delta);
      if(mean(r,a)<m-pad && mean(r,b)>m+pad) {lo=a;hi=b;break;}
      // Retain the proven bracket if the comparison cannot be sharpened.
      break;
    }
    if((hi-lo)<1e-12*std::max(lo,1e-8))break;
  }
  return {lo,hi};
}
struct Data {
  std::vector<double> weights;
  double m=0,k=0,d=0;
  explicit Data(ispan x,int maxit=200) {
    int largest=1;for(int v:x)largest=std::max(largest,v);
    if(largest>enumeration_budget(maxit))return;
    std::vector<double> counts(largest+1,0);
    double total=0;for(int v:x){counts[v]+=1.;total+=v;}
    if(total>4503599627370496.)return;
    m=total/x.size();
    Sum tail,ks,ds;weights.resize(largest);
    for(int v=largest;v>0;--v){tail.add(counts[v]/x.size());weights[v-1]=tail.value;}
    for(std::size_t z=1;z<weights.size();++z){ks.add(weights[z]*z);ds.add(weights[z]*z*z);}
    k=ks.value;d=ds.value;
  }
  double a(double r) const {Sum out;for(std::size_t z=1;z<weights.size();++z)out.add(weights[z]/(r+z));return out.value;}
  double h(double r) const {Sum out;for(std::size_t z=1;z<weights.size();++z)out.add(weights[z]*z/(r+z));return out.value;}
  double h2(double r) const {Sum out;for(std::size_t z=1;z<weights.size();++z)out.add(weights[z]*z/((r+z)*(r+z)));return out.value;}
};
inline double pad(double scale,std::size_t count) {
  return 1024*std::numeric_limits<double>::epsilon()*(1+scale)*(1+count*std::numeric_limits<double>::epsilon());
}
inline Interval score(const Data& data,double a,double b,Interval ta,Interval tb) {
  const double low=data.m*j(tb.lo)-data.h(a),high=data.m*j(ta.hi)-data.h(b);
  const double margin=pad(data.m+data.h(a),data.weights.size());
  return {low-margin,high+margin};
}
inline Interval derivative(const Data& data,double a,double b,Interval ta,Interval tb) {
  // theta decreases in r; e is increasing and negative, jp decreasing.
  const double tl=tb.lo,th=ta.hi,el=e(a*tl),eh=e(b*th);
  const double dl=1+e(tl)-a*eh,dh=1+e(th)-b*el;
  if(!(dl>0))return {-INFINITY,INFINITY};
  const double tlo=th*el/dl,thi=tl*eh/dh;
  const double low=data.m*jp(tl)*tlo+data.h2(b),high=data.m*jp(th)*thi+data.h2(a);
  const double margin=pad(std::abs(data.m*jp(tl)*tlo)+data.h2(a),data.weights.size());
  return {low-margin,high+margin};
}
}
inline ZtnbinomResult ztnbinom_accuracy_search(ispan x,double tol=1e-10,int maxit=200,
    double fixed_size=std::numeric_limits<double>::quiet_NaN(),
    double fixed_prob=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double nan=std::numeric_limits<double>::quiet_NaN();
  double failure_lower=-std::numeric_limits<double>::infinity();
  int work=0;const std::int64_t budget=certificate_budget(maxit);
  auto fail=[&](bool exhausted=false,ZtnbinomCertificateFailure reason=ZtnbinomCertificateFailure::Arithmetic) {
    ZtnbinomResult out{nan,nan,nan,work,ZtnbinomStatus::IterationFailure};
    out.budget_exhausted=exhausted;out.stop_detail.budget_exhausted=exhausted;out.certificate_failure=exhausted?ZtnbinomCertificateFailure::Budget:reason;out.stop_detail.loglik_lower=failure_lower;out.stop_detail.loglik_upper=0;return out;
  };
  // Domain errors precede workload guards, including mixed invalid inputs.
  if(!std::isnan(fixed_size) && (!(fixed_size>=0)||!std::isfinite(fixed_size)))
    return {0,0,0,0,ZtnbinomStatus::InvalidSize};
  if(!zero_detail::positive(x)||!(tol>0)||!std::isfinite(tol)||maxit<1 ||
      (!std::isnan(fixed_prob) && (!(fixed_prob>0 && fixed_prob<=1)||!std::isfinite(fixed_prob))))
    return {0,0,0,0,ZtnbinomStatus::InvalidInput};
  if(fixed_prob==1)for(int v:x)if(v>1)return {0,0,0,0,ZtnbinomStatus::InvalidInput};
  // The old search and limit comparison only nominate coefficients.
  for(int v:x)if(v>100000)return fail(false,ZtnbinomCertificateFailure::Workload);
  auto candidate=ztnbinom_candidate_mle(x,tol,maxit,fixed_size,fixed_prob);
  if(candidate.status==ZtnbinomStatus::InvalidInput || candidate.status==ZtnbinomStatus::InvalidSize)return candidate;
  if((candidate.status==ZtnbinomStatus::Ok || candidate.status==ZtnbinomStatus::PoissonLimit) && candidate.prob>0 && candidate.prob<=1 && std::isfinite(candidate.logLik)) {
    int largest=1;double total=0;for(int v:x){largest=std::max(largest,v);total+=v;}
    const double allowance=1024*std::numeric_limits<double>::epsilon()*(1+std::abs(candidate.logLik)+(x.size()+total)*(1+std::log1p(largest)));
    failure_lower=candidate.logLik-allowance;
  }
  if(!(tol>0)||!std::isfinite(tol)||maxit<1)return fail();
  if(zero_detail::mean(x)==1 && std::isnan(fixed_prob))return candidate;
  if(!std::isnan(fixed_size) && !std::isnan(fixed_prob))return std::isfinite(candidate.logLik)?candidate:fail();
  const ztnbinom_cert_detail::Data data(x,maxit);
  if(data.weights.empty())return fail(*std::max_element(x.begin(),x.end())>enumeration_budget(maxit),ZtnbinomCertificateFailure::Workload);
  if(!std::isnan(fixed_prob)) {
    if(fixed_prob==1)return candidate;
    const double t=-std::log(fixed_prob);
    const double s0=data.a(0)-t/2,margin=ztnbinom_cert_detail::pad(data.a(0)+t,data.weights.size());
    if(s0< -margin)return ztnbinom_candidate_mle(x,tol,maxit,0,fixed_prob);
    if(!(s0>margin))return fail();
    // The held-probability score is strictly decreasing on [0,infinity).
    auto signed_score=[&](double r) {
      const double val=data.a(r)-t*(1+ztnbinom_cert_detail::e(r*t));
      const double eps=ztnbinom_cert_detail::pad(data.a(r)+t,data.weights.size());
      ++work;return ztnbinom_cert_detail::Interval{val-eps,val+eps};
    };
    double lo=0,hi=1;
    while(signed_score(hi).hi>=0) {if(work>=budget)return fail(true);hi*=2;if(!std::isfinite(hi))return fail();}
    while(lo==0 || (hi-lo)/lo>5e-7) {
      if(work>=budget)return fail(true);
      const double mid=lo+(hi-lo)/2;const auto s=signed_score(mid);
      if(s.lo>0)lo=mid;
      else if(s.hi<0)hi=mid;
      else {
        const double a=mid*(1-2e-7),b=mid*(1+2e-7);
        if(work+2>budget)return fail(true);
        if(!(signed_score(a).lo>0 && signed_score(b).hi<0))return fail();
        lo=a;hi=b;
      }
    }
    const double r=candidate.status==ZtnbinomStatus::Ok && candidate.size>=lo && candidate.size<=hi?candidate.size:lo+(hi-lo)/2;
    auto out=ztnbinom_candidate_mle(x,tol,maxit,r,fixed_prob);out.iterations=work;
    return std::isfinite(out.logLik)?out:fail();
  }
  if(!std::isnan(fixed_size)) {
    const auto t=ztnbinom_cert_detail::theta(fixed_size,data.m,maxit);
    if(!(t.lo>0) || (t.hi-t.lo)>1e-7*t.lo)return fail();
    const double pc=std::exp(-(t.lo+t.hi)/2);
    if(!(pc>0 && pc<1))return fail();
    if(!(candidate.prob>=std::exp(-t.hi) && candidate.prob<=std::exp(-t.lo)))
      candidate=ztnbinom_candidate_mle(x,tol,maxit,fixed_size,pc);
    return std::isfinite(candidate.logLik)?candidate:fail();
  }
  const auto t0=ztnbinom_cert_detail::theta(0,data.m,maxit);
  if(!(t0.lo>0))return fail();
  const double h0lo=data.a(0)-t0.hi/2,h0hi=data.a(0)-t0.lo/2;
  const double margin=ztnbinom_cert_detail::pad(data.a(0)+t0.hi,data.weights.size());
  const int first=h0lo>margin?1:(h0hi< -margin?-1:0);
  if(first==0)return fail();
  // Prove the initial interval has no stationary point, using monotonic A
  // and B(r,t)=t*(1+e(rt)), increasing in both coordinates.
  double lower=1e-3;
  for(;;) {
    if(++work>=budget)return fail(true);
    const auto t=ztnbinom_cert_detail::theta(lower,data.m,maxit);
    const double low=data.a(lower)-t0.hi*(1+ztnbinom_cert_detail::e(lower*t0.hi));
    const double high=data.a(0)-t.lo/2;
    if((first>0 && low>margin)||(first<0 && high< -margin))break;
    lower/=2;if(!(lower>1e-14))return fail();
  }
  // Poisson rate enclosure from its strictly increasing conditional mean.
  const auto pois=ztpois_mle(x);
  const double lp=pois.lambda;
  const double lp_margin=4096*std::numeric_limits<double>::epsilon()*(1+lp+data.m);
  const double llo=lp-lp_margin,lhi=lp+lp_margin;
  const double mean_margin=256*std::numeric_limits<double>::epsilon()*(1+data.m);
  if(!(llo>0 && llo/(-std::expm1(-llo))<data.m-mean_margin && lhi/(-std::expm1(-lhi))>data.m+mean_margin))return fail();
  const double tail_margin=ztnbinom_cert_detail::pad(data.k+data.m*lhi/2,data.weights.size());
  const double clo=data.m*llo/2-data.k-tail_margin,chi=data.m*lhi/2-data.k+tail_margin;
  const int last=clo>0?1:(chi<0?-1:0);
  if(last==0)return fail();
  double upper=last>0?2*(data.m*data.m*lhi+data.m*lhi*lhi/12)/clo:2*data.d/(-chi);
  upper=std::max(upper,2*lower);
  if(!std::isfinite(upper))return fail();
  struct Segment {double a,b;};
  std::vector<Segment> pending{{lower,upper}};
  std::vector<Segment> roots;
  while(!pending.empty()) {
    if(work+2>=budget)return fail(true);
    const auto box=pending.back();pending.pop_back();work+=2;
    const auto ta=ztnbinom_cert_detail::theta(box.a,data.m,maxit),tb=ztnbinom_cert_detail::theta(box.b,data.m,maxit);
    if(!(tb.lo>0))return fail();
    const auto s=ztnbinom_cert_detail::score(data,box.a,box.b,ta,tb);
    if(s.lo>0 || s.hi<0)continue;
    const auto d=ztnbinom_cert_detail::derivative(data,box.a,box.b,ta,tb);
    const auto sa=ztnbinom_cert_detail::score(data,box.a,box.a,ta,ta),sb=ztnbinom_cert_detail::score(data,box.b,box.b,tb,tb);
    if(d.hi<0 && sa.lo>0 && sb.hi<0){roots.push_back(box);continue;}
    if((d.lo>0 && (sa.lo>0 || sb.hi<0)) || (d.hi<0 && (sa.hi<0 || sb.lo>0)))continue;
    // This deliberately stops on minima/multiple extrema rather than making
    // an unproved global ordering assertion.
    if(d.lo>0 && sa.hi<0 && sb.lo>0)return fail(false,ZtnbinomCertificateFailure::AdditionalExtrema);
    const double mid=std::sqrt(box.a)*std::sqrt(box.b);
    if(!(mid>box.a && mid<box.b))return fail();
    pending.push_back({box.a,mid});pending.push_back({mid,box.b});
  }
  if(roots.empty()) {
    if(first!=last)return fail();
    if(first<0) {
      const double p=std::exp(-(t0.lo+t0.hi)/2);
      auto out=ztnbinom_candidate_mle(x,tol,maxit,0);
      if(!(out.prob>=std::exp(-t0.hi) && out.prob<=std::exp(-t0.lo)))out=ztnbinom_candidate_mle(x,tol,maxit,0,p);
      out.iterations=work;return std::isfinite(out.logLik)?out:fail();
    }
    ZtnbinomResult out{nan,nan,nan,work,ZtnbinomStatus::PoissonLimit};
    out.stop_detail.limit_coef={pois.lambda};out.stop_detail.limit_loglik=pois.logLik;
    // Termwise bounds over the signed rate enclosure, rather than treating
    // the rounded rate as an exact maximizer. Pad raw likelihood terms.
    ztnbinom_cert_detail::Sum low_sum,high_sum,scale;
    const double loglo=std::log(llo),loghi=std::log(lhi);
    const double normlo=std::log(-std::expm1(-llo)),normhi=std::log(-std::expm1(-lhi));
    for(int v:x) {
      const double fact=special::lgamma(v+1.);
      low_sum.add(-lhi+v*loglo-fact-normhi);
      high_sum.add(-llo+v*loghi-fact-normlo);
      scale.add(lhi+v*std::max(std::abs(loglo),std::abs(loghi))+std::abs(fact)+std::max(std::abs(normlo),std::abs(normhi)));
    }
    const double allowance=1024*std::numeric_limits<double>::epsilon()*(1+scale.value);
    out.stop_detail.loglik_lower=low_sum.value-allowance;
    out.stop_detail.loglik_upper=high_sum.value+allowance;
    return out;
  }
  if(roots.size()!=1 || first!=1 || last!=-1)return fail(false,ZtnbinomCertificateFailure::AdditionalExtrema);
  double lo=roots[0].a,hi=roots[0].b;
  auto wide=[&]() {
    const auto a=ztnbinom_cert_detail::theta(lo,data.m,maxit),b=ztnbinom_cert_detail::theta(hi,data.m,maxit);
    return !(b.lo>0) || (hi-lo)/lo>5e-7 || a.hi-b.lo>5e-7;
  };
  while(wide()) {
    if(++work>=budget)return fail(true);
    const double mid=lo+(hi-lo)/2;const auto t=ztnbinom_cert_detail::theta(mid,data.m,maxit);
    const auto s=ztnbinom_cert_detail::score(data,mid,mid,t,t);
    if(s.lo>0)lo=mid;
    else if(s.hi<0)hi=mid;
    else {
      const double a=mid*(1-2e-7),b=mid*(1+2e-7);
      const auto ta=ztnbinom_cert_detail::theta(a,data.m,maxit),tb=ztnbinom_cert_detail::theta(b,data.m,maxit);
      if(!(ztnbinom_cert_detail::score(data,a,a,ta,ta).lo>0 && ztnbinom_cert_detail::score(data,b,b,tb,tb).hi<0))return fail();
      lo=a;hi=b;
    }
  }
  double r=lo+(hi-lo)/2;
  if(candidate.status==ZtnbinomStatus::Ok && candidate.size>0) {
    if(candidate.size>=lo && candidate.size<=hi)r=candidate.size;
    else {
      // Historical objective refinement can sit outside the narrow root
      // bracket. Retain it only after an independent signed accuracy box.
      if(work+2>budget)return fail(true);
      work+=2;
      const double a=candidate.size*(1-1e-6),b=candidate.size*(1+1e-6);
      const auto ta=ztnbinom_cert_detail::theta(a,data.m,maxit),tb=ztnbinom_cert_detail::theta(b,data.m,maxit);
      if(ztnbinom_cert_detail::score(data,a,a,ta,ta).lo>0 &&
         ztnbinom_cert_detail::score(data,b,b,tb,tb).hi<0 && ta.hi-tb.lo<2e-6)r=candidate.size;
    }
  }
  auto out=ztnbinom_candidate_mle(x,tol,maxit,r);out.iterations=work;
  const auto t=ztnbinom_cert_detail::theta(r,data.m,maxit);
  if(!(t.lo>0) || t.hi-t.lo>1e-7*t.lo)return fail();
  if(!(out.prob>=std::exp(-t.hi) && out.prob<=std::exp(-t.lo)))out=ztnbinom_candidate_mle(x,tol,maxit,r,std::exp(-(t.lo+t.hi)/2));
  out.iterations=work;
  return std::isfinite(out.logLik)?out:fail();
}
} // namespace cppunivariate
