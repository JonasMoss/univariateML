#pragma once
#include "../../core/work_budget.hpp"
namespace cppunivariate {
namespace kumar_cert_detail {
struct Sum {
  double value=0,correction=0;
  void add(double x) {const double y=x-correction,t=value+y;correction=(t-value)-y;value=t;}
};
}
inline KumarResult kumar_mle(span x,double tol,int maxit,
    double start=std::numeric_limits<double>::quiet_NaN(),
    double fixed_a=std::numeric_limits<double>::quiet_NaN(),
    double fixed_b=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double nan=std::numeric_limits<double>::quiet_NaN();
  StopDetail evidence;
  auto fail=[&](int work,KumarCertificateFailure reason=KumarCertificateFailure::Arithmetic) {
    auto out=KumarResult{nan,nan,nan,work,KumarStatus::IterationFailure};
    out.stop_detail=evidence;
    out.certificate_failure=reason;out.stop_detail.budget_exhausted=reason==KumarCertificateFailure::Budget;return out;
  };
  if(!(tol>0)||!std::isfinite(tol)||maxit<1)return fail(0);
  auto candidate=kumar_candidate_mle(x,tol,maxit,start,fixed_a,fixed_b);
  if(std::isfinite(candidate.logLik)) {
    const double pad=1024*std::numeric_limits<double>::epsilon()*(1+std::abs(candidate.logLik))*(1+x.size());
    evidence.candidates.push_back({{candidate.a,candidate.b},candidate.logLik-pad,candidate.logLik+pad});
    evidence.loglik_lower=candidate.logLik-pad;evidence.loglik_upper=INFINITY;
  }
  if(candidate.status==KumarStatus::InvalidData || candidate.status==KumarStatus::Unbounded ||
      !std::isnan(fixed_a))return candidate;
  const double n=double(x.size());
  kumar_cert_detail::Sum acc;for(double v:x)acc.add(-std::log(v)/n);
  const double T=acc.value;
  auto evaluate=[&](double a,double& b,double& ll,double& error) {
    kumar_cert_detail::Sum gsum,dsum;
    for(double v:x) {
      const double t=-a*std::log(v);
      const double g=t>.6931471805599453?-std::log1p(-std::exp(-t)):-std::log(-std::expm1(-t));
      const double d=t>50?t*std::exp(-t)/(-std::expm1(-t)):t/std::expm1(t);
      gsum.add(g);dsum.add(d);
    }
    const double G=gsum.value/n,D=dsum.value/n;
    b=std::isnan(fixed_b)?1/G:fixed_b;
    ll=n*(std::log(a)+std::log(b)-(a-1)*T-(b-1)*G);
    const double s=1-a*T+(b-1)*D;
    error=256*std::numeric_limits<double>::epsilon()*(1+a*T+std::abs((b-1)*D))*(1+n*std::numeric_limits<double>::epsilon());
    if(!(G>std::numeric_limits<double>::min()) || !std::isfinite(b) || !std::isfinite(ll))error=INFINITY;
    return s;
  };
  double center=candidate.status==KumarStatus::Ok?candidate.a:1/T;
  if(!(center>0)||!std::isfinite(center))return fail(0);
  double lo=center,hi=center,b,ll,error;int work=0;
  const std::int64_t budget=certificate_budget(maxit);
  for(;;) {double s=evaluate(lo,b,ll,error);++work;if(s>error)break;
    if(work>=budget)return fail(work,KumarCertificateFailure::Budget);
    if(!(lo/2>0))return fail(work);
    lo/=2;
  }
  for(;;) {double s=evaluate(hi,b,ll,error);++work;if(s< -error)break;
    if(work>=budget)return fail(work,KumarCertificateFailure::Budget);
    if(!std::isfinite(hi*2))return fail(work);
    hi*=2;
  }
  while((hi-lo)/lo>5e-7/std::max(1.,hi*T)) {
    if(work>=budget)return fail(work,KumarCertificateFailure::Budget);
    const double mid=lo+(hi-lo)/2,s=evaluate(mid,b,ll,error);++work;
    if(s>error)lo=mid;
    else if(s< -error)hi=mid;
    else {
      const double radius=2e-7/std::max(1.,mid*T);
      const double l=mid*(1-radius),h=mid*(1+radius);
      if(work+2>budget)return fail(work,KumarCertificateFailure::Budget);
      double el,eh;const double sl=evaluate(l,b,ll,el),sh=evaluate(h,b,ll,eh);work+=2;
      if(!(sl>el&&sh< -eh))return fail(work);
      lo=l;hi=h;
    }
  }
  double a=lo+(hi-lo)/2;
  // Also enclose b: d log(b)/d log(a)=D/G can be large.
  if(work+3>budget)return fail(work,KumarCertificateFailure::Budget);
  double bl,bh,ll0,e0;evaluate(lo,bl,ll0,e0);evaluate(hi,bh,ll0,e0);work+=2;
  if(!(bl>0&&bh>0&&std::abs(std::log(bh/bl))<1e-6))return fail(work);
  if(candidate.status==KumarStatus::Ok && candidate.a>=lo && candidate.a<=hi &&
      candidate.b>=std::min(bl,bh)*(1-1e-12) && candidate.b<=std::max(bl,bh)*(1+1e-12)) {
    candidate.iterations=work;return candidate;
  }
  evaluate(a,b,ll,error);++work;
  if(!std::isfinite(ll)||!std::isfinite(error)||!(b>0))return fail(work);
  return {a,b,ll,work};
}
inline KumarResult kumar_profile_mle(span x,double tol,int maxit) {
  maxit = static_cast<int>(solver_budget(maxit));
  return kumar_mle(x,tol,maxit);
}
} // namespace cppunivariate
