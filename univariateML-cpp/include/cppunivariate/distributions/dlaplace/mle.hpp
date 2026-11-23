#pragma once
#include "../../core/maximizer_set.hpp"
#include "../../core/outcome.hpp"
#include "../../core/discrete_stop.hpp"
#include "../../core/span.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>
namespace cppunivariate {
enum class DlaplaceStatus { Ok, InvalidInput, PointMass };
struct DlaplaceResult { double location, scale, logLik; bool not_unique; DlaplaceStatus status;   StopDetail stop_detail{}; MaximizerSet maximizers{};
};

inline FitOutcome outcome(const DlaplaceResult& r) {
  if (r.stop_detail.accuracy_stop == AccuracyStop::Tolerance) return FitOutcome::failure(OutcomeReason::Tolerance);
  if (r.stop_detail.accuracy_stop == AccuracyStop::Conditioning) return FitOutcome::failure(OutcomeReason::Conditioning);
  switch (r.status) {
    case DlaplaceStatus::Ok: return FitOutcome::estimate(true, !r.not_unique);
    case DlaplaceStatus::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case DlaplaceStatus::PointMass: return FitOutcome::limit(true);
  }
  return {};
}
inline DlaplaceResult dlaplace_mle(ispan x, double location=std::numeric_limits<double>::quiet_NaN(), double scale=std::numeric_limits<double>::quiet_NaN()) {
  auto stopped = [&]() -> DlaplaceResult {
  DlaplaceResult r{location,scale,0,false,DlaplaceStatus::InvalidInput};
  const bool free_location=std::isnan(location), free_scale=std::isnan(scale);
  if(x.size()==0 || (!free_location && (!std::isfinite(location)||location!=std::floor(location))) || (!free_scale && !(scale>0 && scale<1))) return r;
  double upper_median = r.location;
  if(free_location) {
    std::vector<int> y(x.begin(),x.end());std::sort(y.begin(),y.end());
    r.location=y[(y.size()-1)/2];upper_median=y[y.size()/2];r.not_unique=r.location!=upper_median;
  }
  double s=0;for(int v:x)s+=std::abs(double(v)-r.location);
  if(free_scale) {
    if(s==0){r.status=DlaplaceStatus::PointMass;return r;}
    const double d=s/x.size();r.scale=d/(std::hypot(1.0,d)+1);
    if(!(r.scale>0&&r.scale<1))return r;
  }
  r.logLik=x.size()*(std::log1p(-r.scale)-std::log1p(r.scale))+s*std::log(r.scale);
  r.status=DlaplaceStatus::Ok;
  // For every scale in (0,1), only the integer medians minimize the
  // absolute-deviation sum. Its value fixes the same profiled scale.
  if (r.not_unique) {
    Box piece;
    piece.axes = {interval(r.location, upper_median, true, true, true), fixed(r.scale)};
    piece.axes[1].range.lower_rounded = piece.axes[1].range.upper_rounded = free_scale;
    r.maximizers.dimension = 2;
    r.maximizers.add_box(piece);
  }
  return r;

  }();
  if(stopped.status==DlaplaceStatus::PointMass) stopped.stop_detail=discrete_point(x[0]);
  return stopped;
}
}
