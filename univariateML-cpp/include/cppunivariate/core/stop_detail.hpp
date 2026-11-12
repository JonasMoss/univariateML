#pragma once
#include <cmath>
#include <vector>
#include <algorithm>
#include <limits>
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
  , Gamma, InvGamma
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
  double floor=0;
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
  if(tol>0 && tol<1e-10 && tol<floor)detail.accuracy_stop=AccuracyStop::Tolerance;
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
