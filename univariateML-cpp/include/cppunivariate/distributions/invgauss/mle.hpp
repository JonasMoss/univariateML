// Inverse Gaussian MLE: closed form.
//   mu = mean(x)
//   1/lambda = mean(1/x) - 1/mu

#pragma once
#include "../../core/outcome.hpp"
#include "cppunivariate/core/constants.hpp"
#include "../../core/precision.hpp"

#include "../../core/span.hpp"
#include "dpqr.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <initializer_list>
#include <limits>

namespace cppunivariate {

enum class InvGaussStatus { Ok, InvalidInput, Unbounded, IterationFailure };

struct InvGaussResult {
  double mu;
  double lambda;
  double logLik;
  InvGaussStatus status = InvGaussStatus::Ok;
};

inline InvGaussResult invgauss_mle(span x,double mu,double shape) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const bool free_mean=std::isnan(mu),free_shape=std::isnan(shape);
  if(x.empty() || (!free_mean && !(mu>0 && std::isfinite(mu))) ||
     (!free_shape && !(shape>0 && std::isfinite(shape))))
    return {nan,nan,nan,InvGaussStatus::InvalidInput};
  double scale=0;
  for(double v:x) {
    if(!(v>0 && std::isfinite(v)))return {nan,nan,nan,InvGaussStatus::InvalidInput};
    scale=std::max(scale,v);
  }
  const CPPUNIV_EXTENDED_REAL n=static_cast<CPPUNIV_EXTENDED_REAL>(x.size());
  // Scaling prevents overflow of the sum; compensated reductions also work
  // when extended intermediates have only binary64 precision.
  const auto add=[](CPPUNIV_EXTENDED_REAL term,CPPUNIV_EXTENDED_REAL& sum,
      CPPUNIV_EXTENDED_REAL& carry) {
    term-=carry;
    const CPPUNIV_EXTENDED_REAL next=sum+term;
    carry=(next-sum)-term;sum=next;
  };
  if(free_mean) {
    CPPUNIV_EXTENDED_REAL sum=0,carry=0;
    for(double v:x)add(static_cast<CPPUNIV_EXTENDED_REAL>(v)/scale,sum,carry);
    mu=static_cast<double>((sum/n)*scale);
  }
  // Retain the continuous mean as origin + offset, without rounding their
  // sum before subtracting it from a concentrated sample. Subtract in data
  // units first: dividing x and origin separately would lose the residual.
  CPPUNIV_EXTENDED_REAL offset=0,carry=0;
  if(free_mean) {
    for(double v:x)add((static_cast<CPPUNIV_EXTENDED_REAL>(v)-mu)/scale,offset,carry);
    offset/=n;
  }
  const CPPUNIV_EXTENDED_REAL center=static_cast<CPPUNIV_EXTENDED_REAL>(mu)/scale+offset;
  if(free_shape) {
    CPPUNIV_EXTENDED_REAL q=0;carry=0;
    for(double v:x) {
      const CPPUNIV_EXTENDED_REAL d=free_mean ?
        ((static_cast<CPPUNIV_EXTENDED_REAL>(v)-mu)/scale-offset)/center :
        (static_cast<CPPUNIV_EXTENDED_REAL>(v)-mu)/mu;
      add(d*d/(static_cast<CPPUNIV_EXTENDED_REAL>(v)/scale),q,carry);
    }
    if(q==0)return {mu,nan,INFINITY,InvGaussStatus::Unbounded};
    shape=static_cast<double>((n/q)*scale);
  }
  // The returned likelihood belongs to the public (rounded) coefficients.
  const auto likelihood=[&](double m) {
    double sum=0,correction=0;
    for(double v:x) {
      const double term=distributions::dinvgauss(v,m,shape,true)-correction;
      const double next=sum+term;correction=(next-sum)-term;sum=next;
    }
    return sum;
  };
  double logLik=likelihood(mu);
  if(free_mean) {
    const double rounded=static_cast<double>(center*scale);
    // On either side of the continuous mean Q is monotone. Compare its
    // neighboring public representations for the log-density seam, while
    // always retaining the continuous-mean shape, independently of the gap.
    for(double m:{std::nextafter(rounded,0.0),rounded,
        std::nextafter(rounded,std::numeric_limits<double>::infinity())}) {
      if(!(m>0 && std::isfinite(m)))continue;
      const double value=likelihood(m);
      if(value>logLik) {mu=m;logLik=value;}
    }
  }
  return {mu,shape,logLik,
    mu>0 && shape>0 && std::isfinite(mu) && std::isfinite(shape) && std::isfinite(logLik) ?
      InvGaussStatus::Ok : InvGaussStatus::IterationFailure};
}

inline InvGaussResult invgauss_mle(span x) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  return invgauss_mle(x,nan,nan);
}


inline FitOutcome outcome(const InvGaussResult& r) {
  switch (r.status) {
    case InvGaussStatus::Ok: return FitOutcome::estimate(true);
    case InvGaussStatus::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case InvGaussStatus::Unbounded: return FitOutcome::nonexistence(OutcomeReason::Unbounded);
    case InvGaussStatus::IterationFailure: return FitOutcome::failure(OutcomeReason::Evaluation);
  }
  return {};
}


}  // namespace cppunivariate
