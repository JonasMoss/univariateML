#pragma once
#include <cmath>
#include <vector>
#include <algorithm>
#include <limits>
#include <cstddef>
#include "outcome.hpp"
namespace cppunivariate {
// Coefficients use the family's public order; face candidates use their model's.
enum class StopModel { None, Weibull, Pareto, InvWeibull, Power, Exp, InvExp,
  Pois,
  Binom,
  Nbinom,
  ShiftedNbinom,
  Geom,
  Tpois,
  Ztpois,
  Zmpois,
  Zip,
  Cmp,
  PointMass,
  TwoPoint,
  Norm,
  Lst,
  TCollapsePath,
  Unif,
  Hnorm,
  Laplace,
  Lnorm,
  Sn,
  Ht,
  TranslatedExp,
  ReflectedExp,
  TruncatedExp,
  ReflectedHnorm,
  ReflectedHt,
  UnitGamma,
  HalfGed,
  ReflectedHalfGed,
  Trgamma, InvTrgamma, ShiftedTrgamma, ShiftedInvTrgamma
  , Gamma, InvGamma, ShiftedExp, ShiftedWeibull, ShiftedInvWeibull,
  ShiftedPareto, ShiftedPower, ShiftedLnorm, Dblpareto, NbinomWindowBoundary, ShiftedDblpareto
};
struct FitCandidate {
  std::vector<double> coef;
  double loglik_lower = NAN, loglik_upper = NAN;
  StopModel model = StopModel::None;
};
enum class AccuracyStop { None, Tolerance, Conditioning };
struct StopDetail {
  std::vector<FitCandidate> candidates;
  std::vector<double> limit_coef;
  double limit_loglik = NAN;
  // Bounds on the global supremum, when available; infinity is intentional.
  double loglik_lower = NAN, loglik_upper = NAN;
  // Last unresolved interior segment, not a global bound.
  double segment_loglik_upper = NAN;
  StopModel limit_model = StopModel::None;
  bool budget_exhausted = false;
  bool comparison_tie = false;
  AccuracyStop accuracy_stop = AccuracyStop::None;
  double attainable_tol = NAN;
  double conditioning_bound = NAN;
};
// Public-coordinate spacing, normalized by the accuracy contract's scale.
// Held coordinates have scale zero and do not constrain a request.
inline double coefficient_spacing(double value,double scale) {
  if(!std::isfinite(value) || !(scale>0) || !std::isfinite(scale))return NAN;
  const double a=std::abs(value);
  const double gap=a==std::numeric_limits<double>::max()?
    a-std::nextafter(a,0.):std::nextafter(a,INFINITY)-a;
  return gap/scale;
}
inline void record_accuracy(StopDetail& detail,double tol,
    const std::vector<double>& coef,double ll,const std::vector<double>& scales,
    double conditioning_bound=NAN) {
  // A method may require more than one public-coordinate spacing. Preserve
  // its diagnosed floor when the caller records the public witness.
  const bool method_limited=detail.accuracy_stop==AccuracyStop::Tolerance &&
    std::isfinite(detail.attainable_tol);
  double floor=method_limited?detail.attainable_tol:0;
  detail.accuracy_stop=AccuracyStop::None;detail.attainable_tol=NAN;
  if(!std::isfinite(ll)){detail.accuracy_stop=AccuracyStop::None;return;}
  if(coef.size()!=scales.size())return;
  for(std::size_t i=0;i<coef.size();++i) {
    if(!std::isfinite(coef[i])){detail.accuracy_stop=AccuracyStop::None;return;} // Evaluation breakdown is numerical.
    if(scales[i]>0)floor=std::max(floor,coefficient_spacing(coef[i],scales[i]));
  }
  detail.attainable_tol=floor;
  if(std::isfinite(conditioning_bound))detail.conditioning_bound=conditioning_bound;
  else conditioning_bound=detail.conditioning_bound;
  if(tol>0 && (method_limited || tol<1e-10) && tol<floor)detail.accuracy_stop=AccuracyStop::Tolerance;
  else if(std::isfinite(conditioning_bound) && conditioning_bound>tol)
    detail.accuracy_stop=AccuracyStop::Conditioning;
  if(std::isfinite(ll)) {
    bool present=false;
    for(const auto& c:detail.candidates)if(c.model==StopModel::None && c.coef==coef)present=true;
    if(!present)detail.candidates.push_back({coef,ll,ll,StopModel::None});
  }
}
template<class Result>
inline void record_accuracy(Result& result,double tol,const std::vector<double>& coef,
    const std::vector<double>& scales,double conditioning_bound=NAN) {
  record_accuracy(result.stop_detail,tol,coef,result.logLik,scales,conditioning_bound);
}
// A computed face must still be a representable proper member of its model.
// In particular, a positive scale calculated in log coordinates can underflow
// to zero even while the stored log likelihood remains finite.
inline bool valid_stop_coefficients(StopModel model,const std::vector<double>& v) {
  if(v.empty())return false;
  for(double value:v)if(!std::isfinite(value))return false;
  switch(model) {
    case StopModel::None:return true;
    case StopModel::Exp:
    case StopModel::InvExp:return v.size()==1 && v[0]>0;
    case StopModel::Gamma:
    case StopModel::InvGamma:
    case StopModel::Weibull:
    case StopModel::InvWeibull:
      return v.size()==2 && v[0]>0 && v[1]>0;
    case StopModel::Pareto:
    case StopModel::Power:return (v.size()==2 || (v.size()==3 && v[2]<0)) &&
      v[0]>0 && v[1]>0;
    case StopModel::Unif:return v.size()==2 && v[0]<v[1];
    case StopModel::Norm:
    case StopModel::Hnorm:
    case StopModel::Laplace:
    case StopModel::Lnorm:
    case StopModel::TranslatedExp:
    case StopModel::ReflectedExp:
    case StopModel::ReflectedHnorm:return v.size()==2 && v[1]>0;
    case StopModel::Sn:return v.size()==3 && v[1]>0;
    case StopModel::Ht:
    case StopModel::ReflectedHt:
    case StopModel::HalfGed:
    case StopModel::ReflectedHalfGed:return v.size()==3 && v[1]>0 && v[2]>0;
    case StopModel::NbinomWindowBoundary:return v.size()==3 && v[0]>0 &&
      v[1]>=0 && v[1]<v[2] && std::floor(v[1])==v[1] && std::floor(v[2])==v[2];
    case StopModel::TruncatedExp:return v.size()==3 && v[0]<v[1];
    case StopModel::Trgamma:
    case StopModel::InvTrgamma:return v.size()==3 && v[0]>0 && v[1]>0 && v[2]>0;
    case StopModel::Dblpareto:return v.size()==3 && v[0]>0 && v[1]>0 && v[2]>0;
    case StopModel::ShiftedDblpareto:return v.size()==4 && v[0]>0 && v[1]>0 && v[2]>0;
    case StopModel::ShiftedExp:return v.size()==2 && v[0]>0;
    case StopModel::ShiftedWeibull:
    case StopModel::ShiftedInvWeibull:return v.size()==3 && v[0]>0 && v[1]>0;
    case StopModel::ShiftedPareto:
    case StopModel::ShiftedPower:return
      (v.size()==3 || (v.size()==4 && v[3]<0)) && v[0]>0 && v[1]>0;
    case StopModel::ShiftedLnorm:return v.size()==3 && v[1]>0;
    case StopModel::ShiftedTrgamma:
    case StopModel::ShiftedInvTrgamma:return v.size()==4 && v[0]>0 && v[1]>0 && v[2]>0;
    case StopModel::UnitGamma:return v.size()==3 && v[0]>0 && v[1]>0 && v[2]>0;
    // Discrete and t faces: finite coefficients, checked by their families.
    default:return true;
  }
  return false;
}
// Transport a density evaluated on excesses y = x - offset. Translation
// leaves its likelihood and bounds unchanged. Family points already use
// their own public coordinates.
inline void translate_stop_faces(StopDetail& detail, double offset) {
  if(offset==0 || !std::isfinite(offset))return;
  const auto translate=[&](StopModel& model, std::vector<double>& coef) {
    if(!valid_stop_coefficients(model,coef))return;
    switch(model) {
      case StopModel::Dblpareto:model=StopModel::ShiftedDblpareto;break;
      case StopModel::Exp:model=StopModel::ShiftedExp;break;
      case StopModel::Weibull:model=StopModel::ShiftedWeibull;break;
      case StopModel::InvWeibull:model=StopModel::ShiftedInvWeibull;break;
      case StopModel::Pareto:model=StopModel::ShiftedPareto;break;
      case StopModel::Power:model=StopModel::ShiftedPower;break;
      case StopModel::Lnorm:model=StopModel::ShiftedLnorm;break;
      case StopModel::Trgamma:model=StopModel::ShiftedTrgamma;break;
      case StopModel::InvTrgamma:model=StopModel::ShiftedInvTrgamma;break;
      default:return;
    }
    if((model==StopModel::ShiftedPareto || model==StopModel::ShiftedPower) && coef.size()==3)
      coef.insert(coef.begin()+2,offset);
    else coef.push_back(offset);
  };
  translate(detail.limit_model,detail.limit_coef);
  for(auto& candidate:detail.candidates)translate(candidate.model,candidate.coef);
}
// A path can multiply the limiting density exactly at the held endpoint.
inline void attach_endpoint_factor(StopDetail& detail,double factor) {
  for(auto& candidate:detail.candidates)
    if(candidate.model==detail.limit_model && candidate.coef==detail.limit_coef)
      candidate.coef.push_back(factor);
  detail.limit_coef.push_back(factor);
}
inline StopDetail comparison_detail(FitCandidate first, FitCandidate second) {
  StopDetail d;
  for(const auto& c:{first,second})
    if(!valid_stop_coefficients(c.model,c.coef) || !std::isfinite(c.loglik_lower) ||
       !std::isfinite(c.loglik_upper) || c.loglik_lower>c.loglik_upper)return d;
  d.comparison_tie=true;d.candidates={first,second};
  d.loglik_lower=std::fmax(first.loglik_lower,second.loglik_lower);d.loglik_upper=INFINITY;
  return d;
}
inline StopDetail limit_detail(std::vector<double> coef, double ll, StopModel model = StopModel::None) {
  StopDetail d;
  if(!valid_stop_coefficients(model,coef) || !std::isfinite(ll))return d;
  d.limit_coef=coef; d.limit_loglik=ll; d.limit_model=model;
  d.candidates.push_back({coef,ll,ll,model});
  return d;
}
// Comparison outcomes (dev/design/consolidation.md, "Outcome model").
// A family point is a candidate of the family itself (model None, or the
// family's own model) whose coefficients are a point of the family; a face is
// a valid candidate of any other model. Each family states two predicates next
// to its outcome(): `point` (a family point) and `accurate` (a family point
// whose free coefficients the family's stopping rule puts within tol of a
// maximizer, and whose loglik_lower == loglik_upper is the family's point log
// likelihood, computed as for an Ok result).
inline double candidate_lower(const FitCandidate& c) {
  return std::isnan(c.loglik_lower)?-INFINITY:c.loglik_lower;
}
inline bool point_value(const FitCandidate& c) {
  return std::isfinite(c.loglik_lower) && c.loglik_lower==c.loglik_upper;
}
inline bool face_candidate(const FitCandidate& c,StopModel own=StopModel::None) {
  return c.model!=StopModel::None && c.model!=own && valid_stop_coefficients(c.model,c.coef);
}
// Lower bounds are computed values: they are compared at rounding resolution,
// and bounds within 8 ulps of each other count as equal.
inline int compare_lower(double a,double b) {
  if(a==b)return 0;
  if(!std::isfinite(a) || !std::isfinite(b))return a>b?1:-1;
  const double slack=8*std::numeric_limits<double>::epsilon()*std::max(std::abs(a),std::abs(b));
  if(a>b+slack)return 1;
  if(b>a+slack)return -1;
  return 0;
}
// The better candidate has the larger lower bound; equal lower bounds prefer a
// family point that meets the accuracy contract (`preferred`), then the first
// listed. A family point that cannot be returned earns no preference: bounds
// that differ only by rounding between builds must not turn a face into an
// unresolved stop.
template <class Preferred>
inline std::size_t better_candidate(const StopDetail& d,Preferred preferred) {
  std::size_t best=0;
  for(std::size_t i=1;i<d.candidates.size();++i) {
    const int order=compare_lower(candidate_lower(d.candidates[i]),candidate_lower(d.candidates[best]));
    if(order>0 || (order==0 && preferred(d.candidates[i]) && !preferred(d.candidates[best])))best=i;
  }
  return best;
}
// Candidates the certificate cannot separate: the better one as an
// uncertified estimate (an accurate family point) or an uncertified limit (a
// face); anything else is unresolved.
template <class Point,class Accurate>
inline FitOutcome comparison_outcome(const StopDetail& d,Point point,Accurate accurate,
    StopModel own=StopModel::None) {
  if(d.candidates.empty())return FitOutcome::failure(OutcomeReason::Unresolved);
  const FitCandidate& c=d.candidates[better_candidate(d,[&](const FitCandidate& x){return point(x) && accurate(x);})];
  if(point(c))return accurate(c)?FitOutcome::undetermined():FitOutcome::failure(OutcomeReason::Unresolved);
  if(face_candidate(c,own))return FitOutcome::limit(false);
  return FitOutcome::failure(OutcomeReason::Unresolved);
}
// A proved tie between limiting suprema uses the comparison's selected face.
inline FitOutcome equal_faces_outcome(const StopDetail& detail, bool certified,
                                      StopModel own = StopModel::None) {
  if (detail.candidates.size() >= 2 &&
      std::all_of(detail.candidates.begin(), detail.candidates.end(),
                  [&](const FitCandidate& candidate) { return face_candidate(candidate, own); }))
    return FitOutcome::limit(certified);
  return FitOutcome::failure(OutcomeReason::Unresolved);
}
// The same rules for a family result. A family declares, next to its
// outcome(), `family_point(const Result&, const FitCandidate&)` and
// `accurate_point(const Result&, const FitCandidate&)`; bindings build the
// payload from comparison_choice(), so payload and outcome agree.
template <class Result>
inline std::size_t comparison_choice(const Result& r) {
  return better_candidate(r.stop_detail,[&](const FitCandidate& c){return family_point(r,c) && accurate_point(r,c);});
}
template <class Result>
inline FitOutcome comparison_outcome_of(const Result& r,StopModel own=StopModel::None) {
  return comparison_outcome(r.stop_detail,[&](const FitCandidate& c){return family_point(r,c);},
    [&](const FitCandidate& c){return accurate_point(r,c);},own);
}
} // namespace cppunivariate
