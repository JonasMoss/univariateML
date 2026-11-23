#pragma once
#include "../../core/work_budget.hpp"
#include "../pois/mle.hpp"
namespace cppunivariate {
namespace nbinom_cert_detail {
struct Sum {
  double value=0,correction=0;
  void add(double x) {const double y=x-correction,t=value+y;correction=(t-value)-y;value=t;}
};
inline double log1p_remainder(double u) {
  // Remove the first two powers before evaluation. For |u| <= 1/8,
  // the error of the degree-22 truncation is below 2|u|^23/23.
  if(std::abs(u)>.125)return std::log1p(u)-u+u*u/2;
  double out=0,power=u*u*u;
  for(int k=3;k<=22;++k){out+=(k%2?1.:-1.)*power/k;power*=u;}
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
  double exact_dispersion=NAN;
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
    if (exact) exact_dispersion=((total*total+n*total)-n*squares)/(n*n);
    poisson=mean>0 && (exact?n*squares<=total*total+n*total:centered<mean-pad);
    overdispersed=mean>0 && !poisson;
  }
  auto candidate=nbinom_candidate_mle(x,tol,maxit,fixed_size,fixed_prob);
  if(candidate.status==NbinomStatus::Ok && !x.empty() &&
     std::all_of(x.begin(),x.end(),[](int v){return v==0;})) {
    // Otherwise a negative binomial has positive probability at 1.
    auto& set = candidate.maximizers;
    set.dimension = 2;
    set.add_box({{fixed(0), interval(0, 1, false, true)}});
    set.add_box({{interval(0, maxset_inf, true, false), fixed(1)}});
    std::array<std::optional<double>, maxset_dim> holds{};
    if (!std::isnan(fixed_size)) holds[0] = fixed_size;
    if (!std::isnan(fixed_prob)) holds[1] = fixed_prob;
    if (set.slice(holds) == MaxsetStatus::Ok) {
      candidate.unique = set.singleton();
      if (candidate.unique) set = {};
    } else {
      set = {};
    }
  }
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
    const double largest=counts.rbegin()->first;
    const bool dispersion_removed=std::isfinite(exact_dispersion) && r>largest;
    const bool mean_removed=dispersion_removed || (std::isnan(fixed_prob) && r>100*largest);
    // With only large counts, center the log integrals at the mean
    // before adding the Euler--Maclaurin endpoint corrections.
    const bool centered_integral=!mean_removed && std::isnan(fixed_prob) &&
      r>=16 && counts.begin()->first>4096;
    const auto logarithm=[&](double u) {
      if(!mean_removed)return std::log1p(u);
      const double remainder=nbinom_cert_detail::log1p_remainder(u);
      return dispersion_removed?remainder:remainder-u*u/2;
    };
    for(const auto& kv:counts) {
      const int v=kv.first;double h=0,e=0,endpoint_scale=0;
      if(v<=4096) {
        nbinom_cert_detail::Sum acc;
        for(int j=0;j<v;++j) {
          const double ratio=double(j)/(r+j);
          acc.add(dispersion_removed?(double(j)/r)*ratio:mean_removed?-ratio:r/(r+j));
        }
        h=acc.value;
      }
      else if(r>=16) {
        const double u=1/r,w=1/(r+v),u2=u*u,w2=w*w;
        const double offset=(v-mean)/(r+mean);
        const double integral=centered_integral?
          (offset<-.5?std::log((r+v)/(r+mean))-offset:
                       nbinom_cert_detail::log1p_remainder(offset)-offset*offset/2):
          logarithm(v/r);
        const double endpoint=dispersion_removed?-(v/r)*(v/r)*w/2:(v/r)*w/2;
        h=r*(integral+endpoint+(u2-w2)/12-
          (u2*u2-w2*w2)/120+(u2*u2*u2-w2*w2*w2)/252-
          (std::pow(u,8)-std::pow(w,8))/240+
          (std::pow(u,10)-std::pow(w,10))/132);
        endpoint_scale=2*r*std::abs(endpoint);
        e=(691./32760)*std::pow(u,11);
      } else {
        h=r*(special::digamma(r+v)-special::digamma(r));
        e=128*std::numeric_limits<double>::epsilon()*r*(1+std::abs(special::digamma(r+v))+std::abs(special::digamma(r)));
      }
      const double weight=double(kv.second)/x.size();
      sum.add(weight*h);scale.add(weight*(std::abs(h)+endpoint_scale));rem.add(weight*e);
    }
    const double term=centered_integral?0:r*(std::isnan(fixed_prob)?-logarithm(mean/r):std::log(fixed_prob));
    const double leading=dispersion_removed?(exact_dispersion/r)/2:0;
    error=rem.value+16*std::numeric_limits<double>::epsilon()*
      (scale.value+std::abs(term)+std::abs(leading));
    return (sum.value+term)+leading;
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
  // The midpoint has error at most half the signed bracket width.
  // Probe inside a tolerance-sized interval if the midpoint's sign is lost.
  // Retain a Newton proposal when computed signs enclose it to tolerance.
  if(candidate.status==NbinomStatus::Ok && work+2<=budget) {
    const double l=center*(1-.9*tol),h=center*(1+.9*tol);
    if(l>lo && l<center && l<hi) {
      const double sl=score(l,err);++work;
      if(sl>err)lo=l;
    }
    if(h<hi && h>center && h>lo) {
      const double sh=score(h,err);++work;
      if(sh< -err)hi=h;
    }
  }
  while((hi-lo)/lo>1.9*tol) {
    if(work>=budget)return fail(work,NbinomCertificateFailure::Budget);
    const double mid=lo+(hi-lo)/2;
    if(!(mid>lo && mid<hi))return fail(work);
    const double s=score(mid,err);++work;
    if(s>err)lo=mid;
    else if(s< -err)hi=mid;
    else {
      const double l=mid*(1-.9*tol),h=mid*(1+.9*tol);
      if(work+2>budget)return fail(work,NbinomCertificateFailure::Budget);
      double el,eh;const double sl=score(l,el),sh=score(h,eh);work+=2;
      const bool move_lo=l>lo && l<mid && sl>el;
      const bool move_hi=h<hi && h>mid && sh< -eh;
      if(!move_lo && !move_hi)return fail(work);
      if(move_lo)lo=l;
      if(move_hi)hi=h;
    }
  }
  double r=lo+(hi-lo)/2;
  if(candidate.status==NbinomStatus::Ok && center>=lo && center<=hi &&
     std::max(center-lo,hi-center)<=.95*tol*lo)r=center;
  const double p=std::isnan(fixed_prob)?r/(r+mean):fixed_prob;
  if(!(p>0 && p<1))return fail(work);
  const double ll=nbinom_candidate_mle(x,tol,maxit,r,p).logLik;
  if(!std::isfinite(ll))return fail(work);
  if(std::isnan(fixed_prob) && p>.99) {
    // A signed size enclosure does not constrain the likelihood loss from
    // rounding the profiled probability. Close to one, this can leave the
    // represented point below the feasible Poisson limit.
    nbinom_cert_detail::Sum poisson_ll;
    for(const auto& kv:counts)
      poisson_ll.add(kv.second*pois_detail::logpmf(kv.first,mean));
    const double pad=128*std::numeric_limits<double>::epsilon()*
        (1+std::abs(ll)+std::abs(poisson_ll.value));
    if(ll+pad<poisson_ll.value) {
      evidence.candidates={{{r,p},ll-pad,ll+pad}};
      evidence.loglik_lower=poisson_ll.value-pad;
      return fail(work);
    }
  }
  return {r,p,ll,work,false};
}
} // namespace cppunivariate
