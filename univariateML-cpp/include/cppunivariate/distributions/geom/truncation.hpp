// Exponential-family traits for closed-window truncation.
#pragma once
#include "../../core/outcome.hpp"
#include "../../core/truncation.hpp"
#include "../../special/special.hpp"
namespace cppunivariate {
struct GeomTruncationTraits {
  double size=1;
  double lower() const {return 0.;}
  double upper() const {return INFINITY;}
  bool valid() const {return true;}
  bool valid_parameter(double p) const {return p>0 && p<=1 && std::isfinite(p);}
  double natural(double p) const {return std::log1p(-p);}
  double coefficient(double e) const {return -std::expm1(e);}
  double mode(double e) const {return 0.;}
  double kernel(double k,double e) const {return k*e;}
  double ratio(double k,double e) const {return std::exp(e);}
  double tail_bound(double r,double e,int direction) const {return direction>0?std::max(r,r):r;}
  double atom(double p) const {return p==1?0:NAN;}
  double start(double m) const {return std::log(m/(m+1));}
  double eta_lower() const {return -744.;}
  double eta_upper() const {return -1e-16;}
  double lower_member() const {return 1.;}
  double upper_member() const {return NAN;}
  bool positive_valid() const {return true;}
  double boundary_mean(double lo,double hi) const {return lo+(hi-lo)/2;}
  double positive_member() const {return 1.;}
  bool positive_special() const {return false;}
  TruncationResult positive_fit(ispan,double,double,int) const {return {NAN,NAN,0,TruncationStatus::NotImplemented};}
  double reference() const {return .5;}
};
}

namespace cppunivariate {
// Shared closed-window result used by the count modifier routes. Boundary
// statuses retain the seam's existing plain-error rendering.
inline FitOutcome outcome(const TruncationResult& r) {
  switch(r.status) {
    case TruncationStatus::Ok: return FitOutcome::estimate(true);
    case TruncationStatus::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case TruncationStatus::NotImplemented: return FitOutcome::failure(OutcomeReason::Unsupported);
    case TruncationStatus::LowerLimit: return FitOutcome::failure(OutcomeReason::NoConvergence);
    case TruncationStatus::UpperLimit: return FitOutcome::failure(OutcomeReason::NoConvergence);
    case TruncationStatus::NumericalFailure: return FitOutcome::failure(OutcomeReason::NoConvergence);
  }
  return {};
}
} // namespace cppunivariate
