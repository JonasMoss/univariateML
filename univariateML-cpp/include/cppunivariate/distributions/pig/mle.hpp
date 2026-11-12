#pragma once
#include "../../core/face_compare.hpp"
#include "../../core/precision.hpp"
#include "../../core/span.hpp"
#include "../../core/stop_detail.hpp"
#include <cstdint>
#include "../../special/special.hpp"
#include "dpqr.hpp"
#include "../pois/mle.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <map>
#include <vector>
namespace cppunivariate {
enum class PigStatus { Ok, PoissonLimit, ZeroLimit, InvalidInput, SearchFailure, AccuracyFailure };
struct PigResult {
  double mean, shape, logLik;
  int iterations;
  PigStatus status;
  StopDetail stop_detail{};
};
namespace pig_detail {
struct Sum {
  CPPUNIV_EXTENDED_REAL value=0,correction=0;
  void add(CPPUNIV_EXTENDED_REAL v) {
    const CPPUNIV_EXTENDED_REAL y=v-correction,t=value+y;
    correction=(t-value)-y;value=t;
  }
};
struct Counts {
  std::map<int, std::int64_t> frequencies;
  double n = 0, average = 0;
  explicit Counts(ispan x) {
    Sum total;
    for (int k : x) { ++frequencies[k];total.add(k); }
    n=static_cast<double>(x.size());average=static_cast<double>(total.value/n);
  }
  double likelihood(double mean, double shape) const {
    const auto lp = log_masses(frequencies.rbegin()->first, mean, shape);
    Sum ll;
    for (const auto &kv : frequencies) ll.add(static_cast<CPPUNIV_EXTENDED_REAL>(kv.second)*lp[kv.first]);
    return static_cast<double>(ll.value);
  }
  double poisson(double mean) const {
    Sum ll;
    for (const auto &kv : frequencies)
      ll.add(static_cast<CPPUNIV_EXTENDED_REAL>(kv.second)*pois_detail::logpmf(kv.first,mean));
    return static_cast<double>(ll.value);
  }
};
// D(z) = sum_i (x_i + E[J_i]), with J_i the degree under the
// positive half-integer Bessel polynomial Q_{x_i-1}(1/z).
// Streaming positive recurrence avoids coefficient overflow and cancellation.
inline double mean_certificate_degree(const Counts& data, double logz) {
  double previous_log=0, current_log=0, previous_degree=0, current_degree=0;
  Sum total;
  auto frequency=data.frequencies.begin();
  if(frequency->first==0) ++frequency;
  for(std::int64_t k=1;k<=data.frequencies.rbegin()->first;++k) {
    if(k>=2) {
      const double first=std::log(2.*k-3)-logz+current_log;
      const double next_log=logadd(first,previous_log);
      const double weight=std::exp(first-next_log);
      const double next_degree=weight*(current_degree+1)+(1-weight)*previous_degree;
      previous_log=current_log;current_log=next_log;
      previous_degree=current_degree;current_degree=next_degree;
    }
    if(frequency!=data.frequencies.end() && frequency->first==k) {
      total.add(static_cast<CPPUNIV_EXTENDED_REAL>(frequency->second)*(k+current_degree));++frequency;
    }
  }
  return static_cast<double>(total.value/data.n);
}
// Unique held-positive-shape maximum. See README for the monotonicity
// proof, global bracket and practical floating-point sign allowance.
inline PigResult held_shape(const Counts& data, double shape, double tol, int maxit) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  PigResult out{nan,shape,nan,0,PigStatus::SearchFailure};
  out.stop_detail.loglik_upper=std::numeric_limits<double>::infinity();
  const std::int64_t cap=std::min(maxit,100000);
  // Count-recurrence cells are enumeration work; do not allocate/scan an
  // unchecked enormous count range even when the sample itself is small.
  if(static_cast<std::int64_t>(data.frequencies.rbegin()->first)>100000*cap) {
    out.stop_detail.budget_exhausted=true;return out;
  }
  // The settled mass recurrence uses int k<=maxk and cannot advance past
  // INT_MAX. Do not enter it, even when a raised work budget permits it.
  if(data.frequencies.rbegin()->first==std::numeric_limits<int>::max())return out;
  auto finish=[&](double logmean,bool converged) {
    const double estimate=std::exp(logmean),ll=data.likelihood(estimate,shape);
    if(!std::isfinite(estimate) || !std::isfinite(ll))return out;
    out.mean=estimate;out.logLik=ll;
    if(converged) {out.status=PigStatus::Ok;out.stop_detail={};return out;}
    const double pad=256*std::numeric_limits<double>::epsilon()*(1+std::abs(ll))*
      (1+data.frequencies.rbegin()->first);
    out.stop_detail.candidates.push_back({{estimate,shape},ll-pad,ll+pad});
    out.stop_detail.loglik_lower=ll-pad;
    return out;
  };
  auto score=[&](double logmean) {
    const double logratio=std::log(2.)+2*logmean-std::log(shape);
    const double logq=-.5*logadd(0,logratio);
    const double logz=.5*(std::log(2.)+std::log(shape)+logadd(0,-logratio));
    const double degree=mean_certificate_degree(data,logz);
    return std::log(degree)-logmean+std::log1p(std::exp(logq))-std::log(2.);
  };
  double lo=std::log(data.average)-std::log(4.),hi=std::log(data.average)+std::log(2.);
  // Exact root lies in (xbar/2, 2*xbar); widen the lower endpoint
  // so tiny shape does not put the root within rounding of the endpoint.
  // Padded signs are still required at both endpoints.
  const double margin=256*std::numeric_limits<double>::epsilon()*
    (1+std::abs(lo)+std::abs(hi)+data.frequencies.rbegin()->first);
  if(!(score(lo)>margin && score(hi)<-margin))return finish(std::log(data.average),false);
  bool converged=false;
  for(std::int64_t it=0;it<cap;++it) {
    out.iterations=static_cast<int>(it+1);
    if(hi-lo<=tol) {converged=true;break;}
    const double mid=lo+(hi-lo)/2,s=score(mid);
    if(!std::isfinite(s))return finish(mid,false);
    if(s>margin)lo=mid;
    else if(s< -margin)hi=mid;
    else {
      // Residual alone is insufficient. Retain two independently signed
      // points enclosing the root, or stop at the requested resolution.
      const double left=std::max(lo,mid-tol*.49),right=std::min(hi,mid+tol*.49);
      if(!(score(left)>margin && score(right)<-margin))return finish(mid,false);
      lo=left;hi=right;converged=true;break;
    }
  }
  return finish(lo+(hi-lo)/2,converged || hi-lo<=tol);
}
// Held mean, binary counts: ell(w)=-n*mu*(1-w)+S*log((1-w)/(1+w))
// plus constants, 0<=w<1. Its derivative is n*mu-2*S/(1-w*w).
inline PigResult binary_shape(const Counts& data,double mean) {
  if(data.n>4503599627370496.)return {mean,NAN,NAN,0,PigStatus::SearchFailure};
  const double ones=static_cast<double>(data.frequencies.find(1)->second);
  const double target=2*ones,product=mean*data.n;
  // The integer target is exactly representable. At a rounded equality,
  // FMA recovers the product residual, avoiding a false Poisson decision.
  // Near target this residual cannot underflow (counts are int frequencies).
  const double residual=std::isfinite(product)?std::fma(mean,data.n,-product):0;
  if(product<target || (product==target && residual<=0)) {
    const double ll=data.poisson(mean);
    PigResult out{mean,INFINITY,ll,0,PigStatus::PoissonLimit};
    out.stop_detail=limit_detail({mean},ll,StopModel::Pois);
    const double pad=256*std::numeric_limits<double>::epsilon()*(1+std::abs(ll));
    out.stop_detail.loglik_lower=ll-pad;out.stop_detail.loglik_upper=ll+pad;
    out.stop_detail.candidates[0].loglik_lower=ll-pad;
    out.stop_detail.candidates[0].loglik_upper=ll+pad;
    return out;
  }
  const double delta=std::isfinite(product)?((product-target)+residual)/product:
    1-2*data.average/mean;
  const double w=std::sqrt(delta);
  const double shape=2*data.average*data.average/(w*(1+w)*(1+w));
  const double ll=data.likelihood(mean,shape);
  if(!(w>0 && w<=1 && shape>0) || !std::isfinite(shape) || !std::isfinite(ll))
    return {mean,shape,ll,0,PigStatus::SearchFailure};
  return {mean,shape,ll,0,PigStatus::Ok};
}
struct ShapeMoments {double degree=0,scaled_degree=0,scaled_difference=0;};
// Q_m/Q_{m-1}=(2m-1)/z+Q_{m-2}/Q_{m-1}. No large polynomial
// coefficients or logarithm differences are needed. Compute z*E[J] as
// well as E[J], preserving the nonzero Poisson-side limit when E is tiny.
inline ShapeMoments shape_moments(const Counts& data,double logz) {
  const double inverse_z=std::exp(-logz);
  double previous=0,current=0,previous_scaled=0,current_scaled=0;
  double ratio_difference=0,previous_difference=0,current_difference=0;
  ShapeMoments out;Sum degree,scaled_degree,difference;
  auto frequency=data.frequencies.begin();
  if(frequency->first==0)++frequency;
  for(std::int64_t k=1;k<=data.frequencies.rbegin()->first;++k) {
    if(k>=2) {
      const double a=2.*k-3,term=a*inverse_z;
      const double next_difference=term-ratio_difference/(1+ratio_difference);
      const double next_ratio=1+next_difference;
      const double weight=term/next_ratio;
      const double next=weight*(current+1)+(1-weight)*previous;
      const double next_scaled=weight*current_scaled+(1-weight)*previous_scaled+a/next_ratio;
      const double next_scaled_difference=weight*current_difference+(1-weight)*previous_difference
        -a*next_difference/next_ratio+weight*(k-2);
      previous_difference=current_difference;current_difference=next_scaled_difference;
      ratio_difference=next_difference;
      previous=current;current=next;previous_scaled=current_scaled;current_scaled=next_scaled;
    }
    if(frequency!=data.frequencies.end() && frequency->first==k) {
      degree.add(static_cast<CPPUNIV_EXTENDED_REAL>(frequency->second)*current);
      scaled_degree.add(static_cast<CPPUNIV_EXTENDED_REAL>(frequency->second)*current_scaled);
      difference.add(static_cast<CPPUNIV_EXTENDED_REAL>(frequency->second)*current_difference);++frequency;
    }
  }
  out.degree=static_cast<double>(degree.value/data.n);
  out.scaled_degree=static_cast<double>(scaled_degree.value/data.n);
  out.scaled_difference=static_cast<double>(difference.value/data.n);return out;
}
struct ShapeScore {double value,error;};
inline ShapeScore shape_score(const Counts& data,double mean,double logshape) {
  const double lm=std::log(mean),logratio=std::log(2.)+2*lm-logshape;
  const double logq=-.5*logadd(0,logratio),q=std::exp(logq);
  const double logz=.5*(std::log(2.)+logshape+logadd(0,-logratio));
  const auto moments=shape_moments(data,logz);
  double first,second,third;
  if(q>=.5) {
    // mu times the normalized w score, avoiding E/(1-q^2) cancellation.
    first=std::exp(std::log(2.)+2*lm+logq-2*std::log1p(q));
    second=data.average*mean;
    third=.5*(q+1/q)*moments.scaled_degree;
  } else {
    first=2*std::exp(lm+logq)/((1+q)*(1+q));
    second=data.average;
    third=(1+q*q)/(1-q*q)*moments.degree;
  }
  const double scale=std::numeric_limits<double>::min()+std::abs(first)+std::abs(second)+std::abs(third);
  const double error=256*std::numeric_limits<double>::epsilon()*
    (1+std::sqrt(double(data.frequencies.rbegin()->first)))*scale;
  double value=first-second+third;
  if(q>=.5 && logz>=0) {
    Sum factorial,total;
    for(const auto& kv:data.frequencies) {
      factorial.add(static_cast<CPPUNIV_EXTENDED_REAL>(kv.second)*kv.first*(kv.first-1));
      total.add(static_cast<CPPUNIV_EXTENDED_REAL>(kv.second)*kv.first);
    }
    const CPPUNIV_EXTENDED_REAL n=data.n,mu=mean;
    const double edge=mean==data.average?
      static_cast<double>((n*factorial.value-total.value*total.value)/(2*n*n)):
      static_cast<double>((n*mu*mu-2*mu*total.value+factorial.value)/(2*n));
    const double u=-std::expm1(logq),f=static_cast<double>(factorial.value/n);
    value=edge-.5*mean*mean*(u/(1+q))*(u/(1+q))
      +.5*(q+1/q)*moments.scaled_difference+u*u/(4*q)*f;
  }
  return {value,error};
}
inline PigResult poisson_face(const Counts& data,double mean,int iterations=0) {
  const double ll=data.poisson(mean);
  PigResult out{mean,INFINITY,ll,iterations,PigStatus::PoissonLimit};
  out.stop_detail=limit_detail({mean},ll,StopModel::Pois);
  const double pad=256*std::numeric_limits<double>::epsilon()*(1+std::abs(ll));
  out.stop_detail.loglik_lower=ll-pad;out.stop_detail.loglik_upper=ll+pad;
  if(!out.stop_detail.candidates.empty()) {
    out.stop_detail.candidates[0].loglik_lower=ll-pad;
    out.stop_detail.candidates[0].loglik_upper=ll+pad;
  }
  return out;
}
// Strict concavity in w supplies a global sign certificate, including the
// two exterior regions. See the dossier for the Bessel variance barrier.
inline PigResult free_shape(const Counts& data,double mean,bool mean_free,double tol,int maxit) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  PigResult out{mean,nan,nan,0,PigStatus::SearchFailure};
  out.stop_detail.loglik_upper=INFINITY;
  const int cap=std::min(maxit,100000);
  Sum total_sum,factorial_sum;
  for(const auto& kv:data.frequencies) {
    const CPPUNIV_EXTENDED_REAL k=kv.first;
    total_sum.add(kv.second*k);factorial_sum.add(kv.second*k*(k-1));
  }
  const double total=static_cast<double>(total_sum.value),factorial=static_cast<double>(factorial_sum.value);
  bool positive=false,negative=false;
  if(mean_free && data.n*factorial<=4503599627370496. && total*total<=4503599627370496.) {
    // All nonnegative integer products and sums here are exact binary64.
    positive=data.n*factorial>total*total;negative=!positive;
  } else if(!mean_free && mean==std::floor(mean) &&
      data.n*mean*mean+factorial<=4503599627370496. && 2*mean*total<=4503599627370496.) {
    positive=data.n*mean*mean+factorial>2*mean*total;negative=!positive;
  } else {
    const double f=factorial/data.n;
    if(mean>4*data.average || mean<f/(4*data.average))positive=true;
    else {
      const CPPUNIV_EXTENDED_REAL edge=static_cast<CPPUNIV_EXTENDED_REAL>(mean)-2*data.average+
        static_cast<CPPUNIV_EXTENDED_REAL>(f)/mean;
      const double pad=256*std::numeric_limits<double>::epsilon()*(mean+2*data.average+f/mean);
      positive=edge>pad;negative=edge< -pad;
    }
  }
  if(negative)return poisson_face(data,mean);
  auto witness=[&](double logshape) {
    const double shape=std::exp(logshape);
    if(!(shape>0)||!std::isfinite(shape))return;
    const double ll=data.likelihood(mean,shape);
    if(!std::isfinite(ll))return;
    const double pad=256*std::numeric_limits<double>::epsilon()*(1+std::abs(ll))*
      (1+std::sqrt(double(data.frequencies.rbegin()->first)));
    out.shape=shape;out.logLik=ll;
    out.stop_detail.candidates.push_back({{mean,shape},ll-pad,ll+pad});
    out.stop_detail.loglik_lower=ll-pad;
  };
  if(!positive) {
    const auto face=poisson_face(data,mean);
    out.stop_detail.candidates=face.stop_detail.candidates;
    out.stop_detail.loglik_lower=face.stop_detail.loglik_lower;
    out.stop_detail.comparison_tie=true;return out;
  }
  if(static_cast<std::int64_t>(data.frequencies.rbegin()->first)>100000LL*cap) {
    out.stop_detail.budget_exhausted=true;return out;
  }
  if(data.frequencies.rbegin()->first==std::numeric_limits<int>::max())return out;
  const double lower=std::log(std::numeric_limits<double>::denorm_min());
  const double upper=std::log(std::numeric_limits<double>::max());
  double lo=std::max(lower,std::min(upper,2*std::log(mean))),hi=lo,step=1;
  bool bracketed=false;
  for(;out.iterations<cap;++out.iterations) {
    const auto left=shape_score(data,mean,lo),right=shape_score(data,mean,hi);
    if(!std::isfinite(left.value)||!std::isfinite(right.value)) {witness(lo+(hi-lo)/2);return out;}
    if(left.value< -left.error && right.value>right.error) {bracketed=true;break;}
    const double next_lo=left.value< -left.error?lo:std::max(lower,lo-step);
    const double next_hi=right.value>right.error?hi:std::min(upper,hi+step);
    if(next_lo==lo && next_hi==hi) {witness(lo+(hi-lo)/2);return out;}
    lo=next_lo;hi=next_hi;step*=2;
  }
  if(!bracketed) {witness(lo+(hi-lo)/2);return out;}
  const double resolution=std::max(tol,2e-7);
  for(;out.iterations<cap;++out.iterations) {
    if(hi-lo<=resolution) {
      // The padded enclosure is the global certificate. Polish a point
      // strictly inside it to the requested solver resolution; raw signs
      // may move that point but can never invalidate the enclosing box.
      double a=lo,b=hi;
      bool polished=false;
      for(;out.iterations<cap;++out.iterations) {
        const double mid=a+(b-a)/2;
        if(b-a<=.01*tol || mid==a || mid==b) {polished=true;break;}
        const auto s=shape_score(data,mean,mid);
        if(!std::isfinite(s.value)) {witness(mid);return out;}
        if(s.value==0) {
          // A rounded zero is not a root enclosure. Keep independently
          // signed neighbours at the requested local coefficient resolution.
          const double left=std::max(a,mid-.004*tol),right=std::min(b,mid+.004*tol);
          if(!(shape_score(data,mean,left).value<0 && shape_score(data,mean,right).value>0)) {witness(mid);return out;}
          a=left;b=right;polished=true;break;
        }
        if(s.value>0)b=mid;else a=mid;
      }
      if(!polished) {witness(a+(b-a)/2);return out;}
      witness(a+(b-a)/2);
      if(!std::isfinite(out.logLik) ||
          std::nextafter(out.shape,INFINITY)-out.shape>resolution*out.shape)return out;
      if(!(out.shape>0) || std::abs(std::log(out.shape)-(a+(b-a)/2))>.25*tol) {
        {out.status=PigStatus::AccuracyFailure;{if(out.mean>0 && out.shape>0)record_accuracy(out,tol,{out.mean,out.shape},{mean_free?out.mean:0,out.shape});else{out.stop_detail.accuracy_stop=AccuracyStop::None;out.stop_detail.attainable_tol=NAN;}}}return out;
      }
      out.status=PigStatus::Ok;out.stop_detail={};return out;
    }
    const double mid=lo+(hi-lo)/2;
    const auto score=shape_score(data,mean,mid);
    if(!std::isfinite(score.value)) {witness(mid);return out;}
    if(score.value>score.error)hi=mid;
    else if(score.value< -score.error)lo=mid;
    else {
      const double left=std::max(lo,mid-resolution*.49),right=std::min(hi,mid+resolution*.49);
      const auto sl=shape_score(data,mean,left),sr=shape_score(data,mean,right);
      if(!(sl.value< -sl.error && sr.value>sr.error)) {witness(mid);return out;}
      lo=left;hi=right;
    }
  }
  witness(lo+(hi-lo)/2);return out;
}

}
// NaN means free. Each fixed subset is evaluated or profiled exactly;
// there is no generic multi-parameter optimiser.
inline PigResult pig_mle(ispan x, double tol=1e-10, int maxit=200,
 double mean=std::numeric_limits<double>::quiet_NaN(),
 double shape=std::numeric_limits<double>::quiet_NaN()) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const bool fm=!std::isnan(mean), fs=!std::isnan(shape);
  const auto invalid=PigResult{nan,nan,nan,0,PigStatus::InvalidInput};
  if (x.empty() || !(tol>0) || !std::isfinite(tol) || maxit<1 ||
      (fm && (!(mean>0)||!std::isfinite(mean))) ||
      (fs && (!(shape>=0)||!std::isfinite(shape)))) return invalid;
  for (int k:x) if (k<0) return invalid;
  const pig_detail::Counts data(x);
  if (shape==0) return data.average==0 ? PigResult{fm?mean:1,0,0,0,PigStatus::Ok} : invalid;
  if (data.average==0 && !fs) return {fm?mean:1,0,0,0,PigStatus::Ok};
  if(fs && static_cast<std::int64_t>(data.frequencies.rbegin()->first)>100000LL*std::min(maxit,100000)) {
    PigResult out{nan,nan,nan,0,PigStatus::SearchFailure};
    out.stop_detail.budget_exhausted=true;out.stop_detail.loglik_upper=INFINITY;return out;
  }
  if(fs && data.frequencies.rbegin()->first==std::numeric_limits<int>::max())
    return {nan,nan,nan,0,PigStatus::SearchFailure};
  if (fm && fs) {
    const double ll=data.likelihood(mean,shape);
    return {mean,shape,ll,0,std::isfinite(ll)?PigStatus::Ok:PigStatus::SearchFailure};
  }
  if (data.average==0) {
    PigResult out{fm?mean:0,fs?shape:0,0,0,PigStatus::ZeroLimit};
    out.stop_detail=limit_detail({0},0,StopModel::Pois);
    out.stop_detail.loglik_lower=0;out.stop_detail.loglik_upper=0;
    return out;
  }
  if (!fm && !fs) mean=data.average;
  if (fm && !fs && data.frequencies.rbegin()->first==1)
    return pig_detail::binary_shape(data,mean);
  if (!fs)return pig_detail::free_shape(data,mean,!fm,tol,maxit);
  return pig_detail::held_shape(data,shape,tol,maxit);
}
}
