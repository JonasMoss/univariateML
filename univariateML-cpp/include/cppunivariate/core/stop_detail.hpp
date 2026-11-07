#pragma once
#include <cmath>
#include <vector>
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
};
struct FitCandidate {
  std::vector<double> coef;
  double loglik_lower = NAN, loglik_upper = NAN;
  StopModel model = StopModel::None;
};
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
};
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
    case StopModel::Weibull:
    case StopModel::Pareto:
    case StopModel::InvWeibull:
    case StopModel::Power:return v.size()==2 && v[0]>0 && v[1]>0;
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
    case StopModel::TruncatedExp:return v.size()==3 && v[0]<v[1];
    case StopModel::Trgamma:
    case StopModel::InvTrgamma:return v.size()==3 && v[0]>0 && v[1]>0 && v[2]>0;
    case StopModel::ShiftedTrgamma:
    case StopModel::ShiftedInvTrgamma:return v.size()==4 && v[0]>0 && v[1]>0 && v[2]>0;
    case StopModel::UnitGamma:return v.size()==3 && v[0]>0 && v[1]>0 && v[2]>0;
    // Discrete and t faces: finite coefficients, checked by their families.
    default:return true;
  }
  return false;
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
} // namespace cppunivariate
