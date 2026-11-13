// Poisson MLE: lambda = mean(x). Log-likelihood via Rmath dpois for
// bit-identical reproduction of pre-port behaviour.

#pragma once
#include "../../core/outcome.hpp"
#include "cppunivariate/core/constants.hpp"
#include "../../core/precision.hpp"

#include "../../core/span.hpp"
#include "../../special/special.hpp"

#include <cstddef>
#include <cmath>
#include <limits>

namespace cppunivariate {

enum class PoisStatus { Ok, InvalidInput, NumericalFailure };

struct PoisResult {
  double lambda;
  double logLik;
  PoisStatus status = PoisStatus::Ok;
};

namespace pois_detail {
inline double logpmf(double k, double lambda) {
    if(k<1e6) return ::cppunivariate::special::dpois(k,lambda,1);
    const double t=(k-lambda)/lambda;
    double dev;
    if(std::abs(t)<0.1) {
      double term=t*t, series=0.0;
      for(int j=2;j<60;++j) {
        const double add=term/(static_cast<double>(j)*(j-1));
        series+=(j%2==0)?add:-add;
        if(std::abs(add)<1e-17*std::abs(series)) break;
        term*=t;
      }
      dev=lambda*series;
    } else {
      dev=static_cast<double>(static_cast<CPPUNIV_EXTENDED_REAL>(k)*std::log(static_cast<CPPUNIV_EXTENDED_REAL>(k)/lambda)+lambda-k);
    }
    const double r=1.0/k;
    const double stirling=r*(1.0/12.0-r*r/360.0);
    return -0.5*(std::log(2.0*cppunivariate::pi)+std::log(k))-stirling-dev;
}
} // namespace pois_detail

inline PoisResult pois_mle(ispan x, double fixed=std::numeric_limits<double>::quiet_NaN()) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {kNaN,kNaN,PoisStatus::InvalidInput};

  for (int v : x) if (v < 0) return {kNaN,kNaN,PoisStatus::InvalidInput};

  if(!std::isnan(fixed)) {
    if(!(fixed>=0 && std::isfinite(fixed)))return {kNaN,kNaN,PoisStatus::InvalidInput};
    double ll=0;for(int v:x) {if(v<0)return {kNaN,kNaN,PoisStatus::InvalidInput};ll+=pois_detail::logpmf(v,fixed);}
    return {fixed,ll,std::isfinite(ll)?PoisStatus::Ok:PoisStatus::InvalidInput};
  }
  double sum = 0.0;
  for (auto v : x) sum += static_cast<double>(v);
  const double lambda = sum / static_cast<double>(n);

  double logLik = 0.0;
  for (auto v : x) {
    logLik += pois_detail::logpmf(static_cast<double>(v),lambda);
  }
  return {lambda,logLik,std::isfinite(logLik)?PoisStatus::Ok:PoisStatus::NumericalFailure};
}

}  // namespace cppunivariate

namespace cppunivariate {
inline FitOutcome outcome(const PoisResult& r) {
  switch(r.status) {
    case PoisStatus::Ok: return FitOutcome::estimate(true);
    case PoisStatus::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case PoisStatus::NumericalFailure: return FitOutcome::failure(OutcomeReason::Evaluation);
  }
  return {};
}
} // namespace cppunivariate
