#pragma once
#include "../../core/work_budget.hpp"
#include "../pois/mle.hpp"
namespace cppunivariate {
namespace nbinom_cert_detail {
struct Sum {
  double value=0,correction=0;
  void add(double x) {const double y=x-correction,t=value+y;correction=(t-value)-y;value=t;}
};
inline double log1pmx(double u) {
  // Used only for 0 <= u < .01: alternating series remainder after
  // degree 20 is below u^21/21, well inside the rounding allowance.
  double out=0,power=u*u;
  for(int k=2;k<=20;++k){out+=(k%2?1.:-1.)*power/k;power*=u;}
  return out;
}
}
// Exact-arithmetic root certificate conditional on padded computed signs.
// The practical arithmetic allowance is documented in README.md.
inline NbinomResult nbinom_mle(ispan x,double tol,int maxit,
    double fixed_size=std::numeric_limits<double>::quiet_NaN(),
    double fixed_prob=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double nan=std::numeric_limits<double>::quiet_NaN();
  StopDetail evidence;
  auto fail=[&](int work,NbinomCertificateFailure reason=NbinomCertificateFailure::Arithmetic) {
    auto out=NbinomResult{nan,nan,nan,work,false,NbinomStatus::PrecisionFailure};
    out.stop_detail=evidence;
    out.certificate_failure=reason;out.stop_detail.budget_exhausted=reason==NbinomCertificateFailure::Budget;return out;
  };
  bool poisson=false,overdispersed=false;
  // Guard the dispersion comparison independently of the legacy proposer.
  if(x.size()>0 && std::isnan(fixed_size) && std::isnan(fixed_prob)) {
    nbinom_cert_detail::Sum total_sum;
    double squares=0;
    for(int v:x) {if(v<0)return {nan,nan,nan,0,false,NbinomStatus::InvalidInput};total_sum.add(v);squares+=double(v)*v;}
    const double total=total_sum.value;
    const double n=double(x.size()),mean=total/n;
    nbinom_cert_detail::Sum centered_sum;for(int v:x)centered_sum.add((v-mean)*(v-mean)/n);
    const double centered=centered_sum.value;
    const double pad=256*std::numeric_limits<double>::epsilon()*(1+centered+mean)*(1+n*std::numeric_limits<double>::epsilon());
    // All integer operations are exact when these nonnegative products fit
    // binary64's integer range (a factor-two headroom guards the range check).
    // This includes ordinary equidispersed data.
    const bool exact=n*squares<=4503599627370496. && total*total+n*total<=4503599627370496.;
    if(!exact && std::abs(centered-mean)<=pad)return fail(0);
    poisson=mean>0 && (exact?n*squares<=total*total+n*total:centered<mean-pad);
    overdispersed=mean>0 && !poisson;
  }
  auto candidate=nbinom_candidate_mle(x,tol,maxit,fixed_size,fixed_prob);
  if(std::isfinite(candidate.logLik)) {
    const double pad=1024*std::numeric_limits<double>::epsilon()*(1+std::abs(candidate.logLik))*(1+x.size());
    evidence.candidates.push_back({{candidate.size,candidate.prob},candidate.logLik-pad,candidate.logLik+pad});
    evidence.loglik_lower=candidate.logLik-pad;evidence.loglik_upper=INFINITY;
  }
  if(candidate.status==NbinomStatus::InvalidInput)return candidate;
  if(poisson)candidate={nan,nan,nan,0,true,NbinomStatus::Underdispersed};
  else if(overdispersed && candidate.status==NbinomStatus::Underdispersed)candidate=fail(0);
  if(candidate.status==NbinomStatus::Underdispersed) {
    nbinom_cert_detail::Sum mean_sum,ll_sum;
    for(int v:x)mean_sum.add(double(v)/x.size());
    const double mean=mean_sum.value;
    for(int v:x)ll_sum.add(pois_detail::logpmf(v,mean));
    const double ll=ll_sum.value;
    candidate.stop_detail.limit_coef={mean};candidate.stop_detail.limit_loglik=ll;
    candidate.stop_detail.loglik_lower=ll;candidate.stop_detail.loglik_upper=ll;
    return candidate;
  }
  if(!std::isnan(fixed_size) && candidate.status==NbinomStatus::Ok && !std::isfinite(candidate.logLik))return fail(0);
  if(!std::isnan(fixed_size) || x.size()==0 || candidate.status==NbinomStatus::InvalidInput ||
      candidate.status==NbinomStatus::Underdispersed || candidate.size==0) return candidate;
  if(!(tol>0)||!std::isfinite(tol)||maxit<1)return fail(0);
  nbinom_cert_detail::Sum mean_sum;for(int v:x)mean_sum.add(double(v)/x.size());
  const double mean=mean_sum.value;
  if(!(mean>0))return candidate;
  std::map<int,std::size_t> counts;for(int v:x)++counts[v];
  // r times the mean score. Integer recurrence avoids digamma cancellation.
  // For large counts use Euler--Maclaurin with a bounded omitted term.
  auto score=[&](double r,double& error) {
    nbinom_cert_detail::Sum sum,scale,rem;
    const bool residual=std::isnan(fixed_prob) && r>100.*counts.rbegin()->first;
    for(const auto& kv:counts) {
      const int v=kv.first;double h=0,e=0;
      if(v<=4096) {nbinom_cert_detail::Sum acc;for(int j=0;j<v;++j)acc.add(residual?-double(j)/(r+j):r/(r+j));h=acc.value;}
      else if(r>=16) {
        const double u=1/r,w=1/(r+v),u2=u*u,w2=w*w;
        h=r*((residual?nbinom_cert_detail::log1pmx(v/r):std::log1p(v/r))+(u-w)/2+(u2-w2)/12-
          (u2*u2-w2*w2)/120+(u2*u2*u2-w2*w2*w2)/252-
          (std::pow(u,8)-std::pow(w,8))/240+
          (std::pow(u,10)-std::pow(w,10))/132);
        e=r*691./32760*std::pow(u,12);
      } else {
        h=r*(special::digamma(r+v)-special::digamma(r));
        e=128*std::numeric_limits<double>::epsilon()*r*(1+std::abs(special::digamma(r+v))+std::abs(special::digamma(r)));
      }
      const double weight=double(kv.second)/x.size();
      sum.add(weight*h);scale.add(weight*std::abs(h));rem.add(weight*e);
    }
    const double term=residual?-r*nbinom_cert_detail::log1pmx(mean/r):
      r*(std::isnan(fixed_prob)?-std::log1p(mean/r):std::log(fixed_prob));
    error=rem.value+128*std::numeric_limits<double>::epsilon()*((residual?std::numeric_limits<double>::min():1.)+scale.value+std::abs(term))*(1+counts.size()*std::numeric_limits<double>::epsilon());
    return sum.value+term;
  };
  double center=candidate.status==NbinomStatus::Ok?candidate.size:1;
  if(!(center>0)||!std::isfinite(center))center=1;
  double lo=center,hi=center,err=0;int work=0;
  const std::int64_t budget=certificate_budget(maxit);
  for(;;) {const double s=score(lo,err);++work;if(s>err)break;
    if(work>=budget)return fail(work,NbinomCertificateFailure::Budget);
    if(!(lo/2>0))return fail(work);
    lo/=2;
  }
  for(;;) {const double s=score(hi,err);++work;if(s< -err)break;
    if(work>=budget)return fail(work,NbinomCertificateFailure::Budget);
    if(!std::isfinite(hi*2))return fail(work);
    hi*=2;
  }
  while((hi-lo)/lo>5e-7) {
    if(work>=budget)return fail(work,NbinomCertificateFailure::Budget);
    const double mid=lo+(hi-lo)/2,s=score(mid,err);++work;
    if(s>err)lo=mid;
    else if(s< -err)hi=mid;
    else {
      // A small residual alone is never accepted. Both sides must sign.
      const double l=mid*(1-2e-7),h=mid*(1+2e-7);
      if(work+2>budget)return fail(work,NbinomCertificateFailure::Budget);
      double el,eh;const double sl=score(l,el),sh=score(h,eh);work+=2;
      if(!(sl>el && sh< -eh))return fail(work);
      lo=l;hi=h;
    }
  }
  double r=lo+(hi-lo)/2;
  if(candidate.status==NbinomStatus::Ok && candidate.size>=lo && candidate.size<=hi)r=candidate.size;
  const double p=std::isnan(fixed_prob)?r/(r+mean):fixed_prob;
  if(!(p>0 && p<1))return fail(work);
  const double ll=nbinom_candidate_mle(x,tol,maxit,r,p).logLik;
  if(!std::isfinite(ll))return fail(work);
  return {r,p,ll,work,false};
}
} // namespace cppunivariate
