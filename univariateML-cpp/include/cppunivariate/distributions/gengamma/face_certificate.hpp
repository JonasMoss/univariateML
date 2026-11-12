// Strictly winning power/Pareto faces of the all-free signed profile.
#pragma once
#include "../trgamma/mle.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

namespace cppunivariate {
namespace gengamma_fit_detail {

enum class FaceWinner { None, Power, Pareto };
struct FaceCertificate { FaceWinner winner=FaceWinner::None; int evaluations=0; };

// On [l,r], a(t) decreases and K'(t) increases, giving the pointwise
// score bounds 1/r-a(l)K'(r) <= Delta'(t) <= 1/l-a(r)K'(l).
// Integration from both ends gives an upper bound for the whole interval.
// The normal seam and large-power tail use the parent family's analytic
// bounds. Arithmetic comparisons use a practical binary64 allowance; this
// is exact-arithmetic certification conditional on those comparisons.
inline bool side_below_face(const trgamma_detail::LogData &d, double target,
                            int maxit, std::int64_t budget, int &evaluations,
                            double data_error, bool exact_zero_seam=false) {
  const double eps=std::numeric_limits<double>::epsilon();
  const double data_scale=std::min(d.sd,d.sd*d.umax);
  if (!(data_scale>0) || !std::isfinite(data_scale)) return false;
  const double arithmetic=256*eps*(1+static_cast<double>(d.n))+16*data_error/data_scale;
  const double own=trgamma_detail::power_delta(d);
  if (!(std::isfinite(target) && (target>0 || (exact_zero_seam && target==0)) &&
        target>=own && budget>0)) return false;
  const double B=std::max(d.umax,-d.umin);
  if (!(B>0 && std::isfinite(B))) return false;
  CPPUNIV_EXTENDED_REAL fourth=0;
  for (double u:d.u) fourth+=static_cast<CPPUNIV_EXTENDED_REAL>(u)*u*u*u/d.n;
  double seam=0.05/B;
  bool seam_excluded=false;
  for (std::int64_t i=0;i<budget;++i) {
    ++evaluations;
    auto seam_data=d;
    if (exact_zero_seam) seam_data.kappa3=-seam/1000;
    const double upper=trgamma_detail::normal_seam_upper(seam_data,seam);
    const double seam_pad=arithmetic*(1+std::abs(target)+std::abs(upper)+
        std::abs(d.kappa3)*seam+static_cast<double>(fourth)*seam*seam);
    // For an exactly symmetric sample the seam bound is C(0)t^2. Its
    // quadratic coefficient is nondecreasing in |kappa3|. At kappa3=-T/1000,
    // U(T) >= T^2/6000 + C(T/1000)T^2. Hence U(T)<T^2/6000 proves C(0)<0.
    const double threshold=exact_zero_seam ? seam*seam/6000 : target;
    if (std::isfinite(upper) && std::isfinite(seam_pad) && upper+seam_pad<threshold) {
      seam_excluded=true;break;
    }
    const double next=seam/2;
    if (!(next>0 && next<seam)) break;
    seam=next;
  }
  if (!seam_excluded || evaluations+2>budget) return false;
  const double n=static_cast<double>(d.n);
  const double end=4*(n+std::log(n)+10)/d.umax;
  if (!(std::isfinite(end) && end>seam)) return false;
  int profile_evals=0;
  struct Cell {trgamma_detail::ProfilePoint l,r;};
  const auto first=trgamma_detail::profile(d,seam,profile_evals,maxit);
  const auto last=trgamma_detail::profile(d,end,profile_evals,maxit);
  evaluations+=2;
  std::vector<Cell> cells{{first,last}};
  const int solver=static_cast<int>(solver_budget(maxit));
  while (!cells.empty()) {
    if (evaluations>=budget) return false;
    ++evaluations;
    const auto c=cells.back();cells.pop_back();
    if (!(c.l.t>0 && c.r.t>c.l.t && c.l.shape>0 && c.r.shape>0 &&
          c.l.K1>=0 && c.r.K1>=0 && std::isfinite(c.l.delta) &&
          std::isfinite(c.r.delta) && std::isfinite(c.l.shape) && std::isfinite(c.r.shape))) return false;
    // Enclose the two inverse-digamma roots before using monotonicity.
    double margin=64*eps, alo=0, ahi=0;
    bool enclosed=false;
    for (int j=0;j<solver && margin<0.5;++j) {
      alo=c.r.shape*(1-margin);ahi=c.l.shape*(1+margin);
      const double low_value=trgamma_detail::log_minus_digamma(alo);
      const double high_value=trgamma_detail::log_minus_digamma(ahi);
      const double low_pad=arithmetic*std::max(std::abs(low_value),std::abs(c.r.K));
      const double high_pad=arithmetic*std::max(std::abs(high_value),std::abs(c.l.K));
      if (low_value>c.r.K+low_pad && high_value+high_pad<c.l.K) {enclosed=true;break;}
      margin*=2;
    }
    if (!enclosed) return false;
    // The evaluated inner shapes are feasible proposals, not exact roots.
    // Concavity in shape bounds the missing improvement by the tangent
    // residual times the enclosing shape interval.
    const double left_residual=std::abs(trgamma_detail::log_minus_digamma(c.l.shape)-c.l.K);
    const double right_residual=std::abs(trgamma_detail::log_minus_digamma(c.r.shape)-c.r.K);
    const double left_error=left_residual*std::max(std::abs(c.l.shape-alo),std::abs(ahi-c.l.shape));
    const double right_error=right_residual*std::max(std::abs(c.r.shape-alo),std::abs(ahi-c.r.shape));
    const double upper_score=1/c.l.t-alo*c.l.K1;
    const double lower_score=1/c.r.t-ahi*c.r.K1;
    const double width=c.r.t-c.l.t;
    const double upper_left=c.l.delta+left_error+std::max(0.0,upper_score)*width;
    const double upper_right=c.r.delta+right_error+std::max(0.0,-lower_score)*width;
    const double pad=arithmetic*(1+std::abs(target)+std::abs(c.l.delta)+std::abs(c.r.delta)+
        std::abs(std::log(c.l.t))+std::abs(std::log(c.r.t))+
        std::abs(std::log(c.l.shape))+std::abs(std::log(c.r.shape))+
        std::abs(c.l.shape*c.l.K)+std::abs(c.r.shape*c.r.K)+width*(1/c.l.t+ahi*c.r.K1));
    if (!(std::isfinite(pad) && c.l.delta+pad<target && c.r.delta+pad<target)) return false;
    if (std::isfinite(pad) && std::min(upper_left,upper_right)+pad<target) continue;
    if (evaluations>=budget) return false;
    const double mid=std::sqrt(c.l.t)*std::sqrt(c.r.t);
    if (!(mid>c.l.t && mid<c.r.t)) return false;
    const auto p=trgamma_detail::profile(d,mid,profile_evals,maxit);
    ++evaluations;
    cells.push_back({c.l,p});cells.push_back({p,c.r});
  }
  return true;
}

inline FaceCertificate strict_winning_face(span y, int maxit, double data_error=0) {
  FaceCertificate out;
  if (y.size()<2 || maxit<1 || !(data_error>=0) || !std::isfinite(data_error)) return out;
  std::vector<double> reflected;reflected.reserve(y.size());
  for (double v:y) {if(!std::isfinite(v))return out;reflected.push_back(-v);}
  trgamma_detail::LogData positive,negative;
  if (!trgamma_detail::standardize(y,positive) ||
      !trgamma_detail::standardize({reflected.data(),reflected.size()},negative)) return out;
  const double plus=trgamma_detail::power_delta(positive);
  const double minus=trgamma_detail::power_delta(negative);
  const double data_scale=std::min({positive.sd,positive.sd*positive.umax,
                                   negative.sd,negative.sd*negative.umax});
  if (!(data_scale>0) || !std::isfinite(data_scale)) return out;
  const double pad=(256*std::numeric_limits<double>::epsilon()*(1+y.size())+
      16*data_error/data_scale)*
      (1+std::abs(plus)+std::abs(minus));
  if (!(std::isfinite(plus) && std::isfinite(minus) && std::max(plus,minus)>pad &&
        std::abs(plus-minus)>pad)) return out;
  const double target=std::max(plus,minus);
  const std::int64_t half=certificate_budget(maxit)/2;
  int e1=0,e2=0;
  const bool a=side_below_face(positive,target,maxit,half,e1,data_error);
  const bool b=a && side_below_face(negative,target,maxit,half,e2,data_error);
  out.evaluations=e1+e2;
  if (a && b) out.winner=plus>minus ? FaceWinner::Power : FaceWinner::Pareto;
  return out;
}

}  // namespace gengamma_fit_detail
}  // namespace cppunivariate
