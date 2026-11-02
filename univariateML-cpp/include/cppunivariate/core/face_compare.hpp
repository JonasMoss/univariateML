// Practical floating-point face decision; see the face-compare report.
#pragma once
#include <cmath>
#include <limits>
namespace cppunivariate {
enum class FaceComparison { FirstWins, SecondWins, NumericalTie };
// margin >= 0 favors first, preserving the historical family tolerance.
// The allowance covers rounding of stored objectives, not optimizer error.
// For a direct gain, cancellation_scale supplies the sum of absolute terms.
inline FaceComparison compare_face(double first, double second, double margin, double cancellation_scale = 0) {
  if (std::isnan(first) || std::isnan(second) || !(margin >= 0) || !std::isfinite(margin) || !(cancellation_scale >= 0) || !std::isfinite(cancellation_scale))
    return FaceComparison::NumericalTie;
  if (!std::isfinite(first) || !std::isfinite(second)) {
    if (first == second) return FaceComparison::NumericalTie;
    return first > second ? FaceComparison::FirstWins : FaceComparison::SecondWins;
  }
  const double allowance = (8 * std::numeric_limits<double>::epsilon()) * std::abs(first)
                         + (8 * std::numeric_limits<double>::epsilon()) * std::abs(second)
                         + (8 * std::numeric_limits<double>::epsilon()) * cancellation_scale;
  // Scale before subtraction so opposite finite extremes cannot overflow.
  const double scale = std::fmax(1., std::fmax(std::abs(first), std::abs(second)));
  const double delta = first / scale - second / scale + margin / scale;
  const double error = allowance / scale + 4 * std::numeric_limits<double>::epsilon()
      * (std::abs(first / scale) + std::abs(second / scale));
  if (delta > error) return FaceComparison::FirstWins;
  if (delta < -error) return FaceComparison::SecondWins;
  return FaceComparison::NumericalTie;
}
inline bool face_numerical_tie(double first, double second, double margin) {
  return compare_face(first, second, margin) == FaceComparison::NumericalTie;
}
} // namespace cppunivariate
