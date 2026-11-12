// One outcome type for every fitter (dev/design/consolidation.md).
//
// Each family maps its result to a FitOutcome with an overload of
// `outcome(const <Family>Result&)` next to its fitter. The family status enum
// stays as detail; generic code and the bindings read the outcome.
//
// A default-constructed outcome is a failure, never a success.
#pragma once

namespace cppunivariate {

enum class OutcomeKind { Estimate, Limit, Nonexistence, Budget, Failure };

enum class OutcomeReason {
  None,
  Unbounded,        // Nonexistence: the likelihood is unbounded (collapse, spike)
  Unattained,       // Nonexistence: finite supremum, not attained, no limit model to name
  InvalidInput,     // Failure: empty, non-finite or out-of-support data, or invalid held values
  InvalidArgument,  // Failure: a held value or control rejected before the fit
  Unsupported,      // Failure: a hold subset the family does not implement
  NoConvergence,    // Failure: a solver cap was reached or a search failed
  Evaluation,       // Failure: NaN, overflow or an unrepresentable result
  Tolerance,        // Failure: tol below the binary64 spacing at the maximizer
  Conditioning,     // Failure: rounding noise over curvature exceeds tol
  Unresolved        // Failure: an undecided comparison with no candidate to return
};

struct FitOutcome {
  OutcomeKind kind = OutcomeKind::Failure;
  OutcomeReason reason = OutcomeReason::Evaluation;
  // Estimate: the global-optimality claim. Limit: proved, not decided by a margin.
  bool certified = false;
  // Estimate: false when the fitter has established more than one maximizer.
  bool unique = true;

  static constexpr FitOutcome estimate(bool certified = true, bool unique = true) {
    return {OutcomeKind::Estimate, OutcomeReason::None, certified, unique};
  }
  static constexpr FitOutcome limit(bool certified = true) {
    return {OutcomeKind::Limit, OutcomeReason::None, certified, true};
  }
  static constexpr FitOutcome nonexistence(OutcomeReason reason) {
    return {OutcomeKind::Nonexistence, reason, true, true};
  }
  static constexpr FitOutcome budget() {
    return {OutcomeKind::Budget, OutcomeReason::None, false, true};
  }
  static constexpr FitOutcome failure(OutcomeReason reason) {
    return {OutcomeKind::Failure, reason, false, true};
  }
};

constexpr const char* outcome_kind_name(OutcomeKind kind) {
  switch (kind) {
    case OutcomeKind::Estimate: return "estimate";
    case OutcomeKind::Limit: return "limit";
    case OutcomeKind::Nonexistence: return "nonexistence";
    case OutcomeKind::Budget: return "budget";
    case OutcomeKind::Failure: return "failure";
  }
  return "failure";
}

constexpr const char* outcome_reason_name(OutcomeReason reason) {
  switch (reason) {
    case OutcomeReason::None: return "none";
    case OutcomeReason::Unbounded: return "unbounded";
    case OutcomeReason::Unattained: return "unattained";
    case OutcomeReason::InvalidInput: return "invalid-input";
    case OutcomeReason::InvalidArgument: return "invalid-argument";
    case OutcomeReason::Unsupported: return "unsupported";
    case OutcomeReason::NoConvergence: return "no-convergence";
    case OutcomeReason::Evaluation: return "evaluation";
    case OutcomeReason::Tolerance: return "tolerance";
    case OutcomeReason::Conditioning: return "conditioning";
    case OutcomeReason::Unresolved: return "unresolved";
  }
  return "evaluation";
}

}  // namespace cppunivariate
