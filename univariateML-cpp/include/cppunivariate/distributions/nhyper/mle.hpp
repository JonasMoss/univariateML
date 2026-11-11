#pragma once
#include "../../core/discrete_stop.hpp"
#include "../../core/precision.hpp"

#include "../hyper/mle.hpp"
#include <cmath>
#include <limits>

namespace cppunivariate {

enum class NhyperStatus { Ok, Tie, Unidentified, InvalidInput, Infeasible, NumericalFailure, FaceUnresolved, FaceTie };
struct NhyperResult {
  double m, N, r, logLik, tie;
  int iterations;
  NhyperStatus status;
  StopDetail stop_detail{};
  bool certified = true;
};

// Total draws until the r-th white ball. P(T=x) = r/x P(H_x=r).
// The hypergeometric saddle-point kernel avoids large log-gamma differences.
inline double nhyper_log_pmf(double x, double n, double m, double r) {
  const double neginf = -std::numeric_limits<double>::infinity();
  if (r == 0) return x == 0 ? 0 : neginf;
  if (!std::isfinite(x) || x != std::floor(x) || x < r || x > n + r)
    return neginf;
  return hyper_log_pmf(r, n + m, m, x) + std::log(r / x);
}

namespace nhyper_detail {
// Correct a rounded quotient using compensated integer product differences.
// Within the representable count range this prevents an off-by-one tail bound.
inline CPPUNIV_EXTENDED_REAL population_bound(double m, double b, double r) {
  CPPUNIV_EXTENDED_REAL q = std::floor(static_cast<CPPUNIV_EXTENDED_REAL>(m) * b / r);
  if (q > hyper_detail::kMaxCount) return q;
  while (hyper_detail::product_difference(m, b, q, r) < 0) --q;
  while (hyper_detail::product_difference(m, b, q + 1, r) >= 0) ++q;
  return q;
}
}  // namespace nhyper_detail

// NaN requests estimation. N held permits composition and stopping count
// free; m held permits a finite search with N free. Existing composition
// search with N and r held:
// L(m)/L(m-1) = prod m/(m-r) * (N-m+1-x+r)/(N-m+1).
// Each factor is nonincreasing in m, strictly so for r>0. Thus the
// likelihood is log concave on [r, N-max(x)+r], including its endpoints.
inline NhyperResult nhyper_mle(span x, double r, double N, double m);
inline NhyperResult nhyper_mle_search(span x, double r, double N, double m) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  NhyperResult out{m, N, r, nan, nan, 0, NhyperStatus::InvalidInput};
  if (std::isnan(N) && std::isnan(m)) {
    if (x.size() == 0 || (!std::isnan(r) && !hyper_detail::is_count(r))) return out;
    for (double v : x) if (!hyper_detail::is_count(v)) return out;
    const auto c = hyper_detail::tabulate(x);
    out.status = NhyperStatus::Infeasible;
    if (c.max == 0) {
      if (!std::isnan(r) && r != 0) return out;
      out.N = 0; out.m = 0; out.r = 0; out.logLik = 0;
      out.status = NhyperStatus::Ok; return out;
    }
    if (c.min == 0 || r == 0 || (!std::isnan(r) && r > c.min)) return out;
    if (c.min == c.max && (std::isnan(r) || r == c.min)) {
      out.N = c.min; out.m = c.min; out.r = c.min; out.logLik = 0;
      out.status = NhyperStatus::Ok; return out;
    }
    const double rlo = std::isnan(r) ? 1 : r;
    const double rhi = std::isnan(r) ? c.min : r;
    out.status = NhyperStatus::NumericalFailure;
    if (c.max > 100000 || rhi > 100000 ||
        (rhi-rlo+1) * static_cast<double>(c.value.size()) > 1000000 ||
        c.max * static_cast<double>(c.value.size()) > 1000000){
      double value=-INFINITY;
      for(double nn=c.max;nn<c.max+256 && nn<=hyper_detail::kMaxCount;++nn) {
        const auto seed=nhyper_mle(x,rlo,nn,nan);out.iterations+=seed.iterations;
        if(std::isfinite(seed.logLik) && seed.logLik>value) {
          value=seed.logLik;out.stop_detail.candidates={{{seed.m,seed.N,seed.r},value,value}};
        }
      }
      out.stop_detail=discrete_budget(out.stop_detail);
      return out;
    }
    double face_r=rlo, face_p=NAN;
    double face = -std::numeric_limits<double>::infinity();
    for (double rr = rlo; rr <= rhi; ++rr) {
      const double p = rr * c.h / c.sum;
      double ll = 0;
      for (std::size_t j = 0; j < c.value.size(); ++j) {
        const double v = c.value[j], y = v-rr;
        const double qterm = y == 0 ? 0 : y*std::log1p(-p);
        ll += c.count[j] * (std::lgamma(v)-std::lgamma(rr)-std::lgamma(y+1) + rr*std::log(p) + qterm);
      }
      if(ll>face){face=ll;face_r=rr;face_p=p;}
    }
    if (!std::isfinite(face)) return out;
    out.stop_detail=discrete_face(StopModel::ShiftedNbinom,{face_r,face_p,face_r},face);
    const double margin = 1e-9 * (1 + std::fabs(face));
    double best = -std::numeric_limits<double>::infinity();
    NhyperResult winner = out;
    int work = 0;
    bool face_tie = false;
    auto evaluate = [&](double nn) {
      for (double rr = rlo; rr <= rhi; ++rr) {
        const NhyperResult f = nhyper_mle(x, rr, nn, nan);
        work += std::max(1, f.iterations);
        if ((f.status != NhyperStatus::Ok && f.status != NhyperStatus::Tie &&
             f.status != NhyperStatus::Unidentified) || !std::isfinite(f.logLik)) return false;
        if (f.logLik > best) { best = f.logLik; winner = f;
          out.stop_detail.candidates.erase(std::remove_if(out.stop_detail.candidates.begin(),out.stop_detail.candidates.end(),
            [](const FitCandidate& c){return c.model==StopModel::None;}),out.stop_detail.candidates.end());
          out.stop_detail.candidates.push_back({{f.m,f.N,f.r},f.logLik,f.logLik}); }
        if (std::fabs(f.logLik-face) <= margin) face_tie = true;
        if(work>=hyper_detail::kGridBudget){out.stop_detail=discrete_budget(out.stop_detail);return false;}
      }
      return true;
    };
    const double lo = c.max;
    if (lo + 255 >= hyper_detail::kMaxCount) return out;
    for (double nn = lo; nn <= lo + 255; ++nn) if (!evaluate(nn)) return out;
    if (best <= face + margin) {
      // Total draws = failures + r: retain the translation in a non-package law.
      out.stop_detail=discrete_face(StopModel::ShiftedNbinom,{face_r,face_p,face_r},face);
      out.stop_detail.candidates.push_back({{winner.m,winner.N,winner.r},best,best});out.stop_detail.loglik_upper=INFINITY;
      out.status = face_tie ? NhyperStatus::FaceTie : NhyperStatus::FaceUnresolved;
      out.iterations = work; return out;
    }
    auto correction = [&](double nn) {
      double s = 0;
      for (std::size_t j = 0; j < c.value.size(); ++j)
        for (double h = 1; h < c.value[j]; ++h)
          s -= c.count[j] * std::log1p(-h/nn);
      return s;
    };
    double end = lo;
    while (face + correction(end) >= best-margin) {
      if (end >= hyper_detail::kMaxCount/2) return out;
      end *= 2;
    }
    double lower = lo;
    while (end-lower > 1) {
      const double mid = lower + std::floor((end-lower)/2);
      if (face + correction(mid) < best-margin) end = mid; else lower = mid;
    }
    if ((end-lo)*(rhi-rlo+1) > hyper_detail::kGridBudget/4){out.stop_detail=discrete_budget(out.stop_detail);return out;}
    for (double nn = lo; nn < end; ++nn) if (!evaluate(nn)) return out;
    out = winner; out.iterations = work; out.status = NhyperStatus::Ok;
    return out;
  }
  // New finite grids: N held alone, or m held with N free.
  if ((std::isnan(r) && std::isnan(m) && !std::isnan(N)) ||
      (std::isnan(N) && !std::isnan(m))) {
    if (x.size() == 0 || (!std::isnan(N) && !hyper_detail::is_count(N)) ||
        (!std::isnan(m) && !hyper_detail::is_count(m)) ||
        (!std::isnan(r) && !hyper_detail::is_count(r))) return out;
    for (double v : x) if (!hyper_detail::is_count(v)) return out;
    const auto c = hyper_detail::tabulate(x);
    out.status = NhyperStatus::Infeasible;
    if ((!std::isnan(N) && c.max > N) ||
        (!std::isnan(m) && !std::isnan(r) && r > m)) return out;
    if (c.max == 0) {
      if (!std::isnan(r) && r != 0) return out;
      out.r = 0;
      if (std::isnan(N)) out.N = m;
      if (std::isnan(m)) out.m = 0;
      out.logLik = 0; out.status = NhyperStatus::Ok;
      return out;
    }
    if (c.min == 0 || r == 0) return out;
    const bool fixed_N = !std::isnan(N);
    if(!fixed_N && c.min==c.max && m>=c.min && (std::isnan(r) || r==c.min)) {
      out.N=m;out.r=c.min;out.logLik=0;out.status=NhyperStatus::Ok;return out;
    }
    const double rlo = std::isnan(r) ? (fixed_N ? 1 : std::max(1.,m-(hyper_detail::kMaxCount-c.max))) : r;
    const double rhi = std::isnan(r) ? std::min(c.min, fixed_N ? N : m) : r;
    if (rlo > rhi || rhi > c.min) return out;
    // Preflight exact line lengths, stopping as soon as the budget is exceeded.
    CPPUNIV_EXTENDED_REAL size = 0;
    bool partial=std::isnan(r) && !fixed_N && rlo>1;
    out.status = NhyperStatus::NumericalFailure;
    for (double rr = rlo; rr <= rhi; ++rr) {
      const CPPUNIV_EXTENDED_REAL lo = fixed_N ? rr : static_cast<CPPUNIV_EXTENDED_REAL>(m) + (c.max - rr);
      const CPPUNIV_EXTENDED_REAL hi = fixed_N ? static_cast<CPPUNIV_EXTENDED_REAL>(N) - c.max + rr :
          std::max(lo, nhyper_detail::population_bound(m, c.max, rr));
      if(lo>hyper_detail::kMaxCount)return out;
      if(hi>hyper_detail::kMaxCount){partial=true;break;}
      size += hi - lo + 1;
      if (2 * size > hyper_detail::kGridBudget){partial=true;break;}
    }
    CPPUNIV_EXTENDED_REAL best = -std::numeric_limits<CPPUNIV_EXTENDED_REAL>::infinity();
    bool selected = false;
    for (int pass = 0; pass < 2; ++pass) {
      for (double rr = rlo; rr <= rhi; ++rr) {
        const double lo = fixed_N ? rr : static_cast<double>(static_cast<CPPUNIV_EXTENDED_REAL>(m) + (c.max - rr));
        const double hi = fixed_N ? N - c.max + rr : std::min(hyper_detail::kMaxCount,static_cast<double>(std::max(
            static_cast<CPPUNIV_EXTENDED_REAL>(lo), nhyper_detail::population_bound(m, c.max, rr))));
        for (double t = lo; t <= hi; ++t) {
          const double mm = fixed_N ? t : m, nn = fixed_N ? N : t;
          hyper_detail::Sum ll;
          for (std::size_t j = 0; j < c.value.size(); ++j) {
            const CPPUNIV_EXTENDED_REAL v = c.value[j];
            ll.add(c.count[j] * (hyper_detail::grid_choose(v - 1, rr - 1) +
                hyper_detail::grid_choose(static_cast<CPPUNIV_EXTENDED_REAL>(nn) - v, mm - rr) -
                hyper_detail::grid_choose(nn, mm)));
          }
          ++out.iterations;
          if (!std::isfinite(ll.value())) {
            if(partial && std::isfinite(out.logLik)){out.status=NhyperStatus::Ok;out.certified=false;}
            return out;
          }
          if(partial) {
            if(ll.value()>best) {
              best=ll.value();out.m=mm;out.N=nn;out.r=rr;out.logLik=static_cast<double>(best);
            }
            if(out.iterations>=hyper_detail::kGridBudget/2) {
              out.status=NhyperStatus::Ok;out.certified=false;out.stop_detail=discrete_budget(out.stop_detail);return out;
            }
          }
          if (pass == 0) best = std::max(best, ll.value());
          else if (hyper_detail::grid_tied(ll.value(), best) &&
              (!selected || mm < out.m || (mm == out.m &&
               (nn < out.N || (nn == out.N && rr < out.r))))) {
            out.m = mm; out.N = nn; out.r = rr;
            out.logLik = static_cast<double>(ll.value()); selected = true;
          }
        }
      }
    }
    if (selected) out.status = NhyperStatus::Ok;
    if(partial && std::isfinite(out.logLik)) {
      out.status=NhyperStatus::Ok;out.certified=false;out.stop_detail=discrete_budget(out.stop_detail);
    }
    return out;
  }
  if (std::isnan(r) && hyper_detail::is_count(N) &&
      hyper_detail::is_count(m) && m <= N && x.size() != 0) {
    for (double v : x) if (!hyper_detail::is_count(v)) return out;
    const auto c = hyper_detail::tabulate(x);
    if (c.max == 0) { out.r = 0; out.logLik = 0; out.status = NhyperStatus::Ok; return out; }
    out.status = NhyperStatus::Infeasible;
    const double lo = std::max(1.0, c.max - (N - m));
    const double hi = std::min(m, c.min);
    if (lo > hi) return out;
    // L(t)/L(t-1) = product (x-t+1)(m-t+1) /
    // ((t-1)(N-m-x+t)); each factor decreases in t.
    const auto peak = hyper_detail::unimodal_peak(lo, hi, false, [&](double t) {
      hyper_detail::Sum d;
      for (std::size_t j = 0; j < c.value.size(); ++j) {
        const CPPUNIV_EXTENDED_REAL v = c.value[j];
        const CPPUNIV_EXTENDED_REAL a = v - t + 1, b = static_cast<CPPUNIV_EXTENDED_REAL>(m) - t + 1;
        const CPPUNIV_EXTENDED_REAL den1 = t - 1, den2 = static_cast<CPPUNIV_EXTENDED_REAL>(N) - m - v + t;
        const CPPUNIV_EXTENDED_REAL z = hyper_detail::product_difference(a, b, den1, den2) / (den1 * den2);
        if (std::fabs(z) < CPPUNIV_EXTENDED_LITERAL(0.5,0.5L)) {
          d.add(c.count[j] * std::log1p(z));
        } else {
          d.add(c.count[j] * (std::log(a / den1) + std::log(b / den2)));
        }
      }
      return d;
    });
    out.r = peak.at; out.tie = peak.tie; out.iterations = peak.evaluations;
    out.status = std::isnan(peak.tie) ? NhyperStatus::Ok : NhyperStatus::Tie;
    CPPUNIV_EXTENDED_REAL ll = 0;
    for (std::size_t j = 0; j < c.value.size(); ++j)
      ll += c.count[j] * nhyper_log_pmf(c.value[j], N - m, m, out.r);
    out.logLik = static_cast<double>(ll);
    return out;
  }
  const bool free = std::isnan(m);
  if (x.size() == 0 || !hyper_detail::is_count(N) ||
      !hyper_detail::is_count(r) || r > N ||
      (!free && (!hyper_detail::is_count(m) || m < r || m > N))) return out;
  for (double v : x) if (!hyper_detail::is_count(v)) return out;
  const auto c = hyper_detail::tabulate(x);
  out.status = NhyperStatus::Infeasible;
  if (r == 0) {
    if (c.max != 0) return out;
    if (free) { out.m = 0; out.tie = N; }
    out.status = free && N > 0 ? NhyperStatus::Unidentified : NhyperStatus::Ok;
  } else {
    if (c.min < r || c.max > N) return out;
    const double hi = N - c.max + r;
    if (!free && m > hi) return out;
    if (free) {
      const auto peak = hyper_detail::unimodal_peak(r, hi, false, [&](double t) {
        hyper_detail::Sum d;
        for (std::size_t j = 0; j < c.value.size(); ++j) {
          const CPPUNIV_EXTENDED_REAL failures = static_cast<CPPUNIV_EXTENDED_REAL>(N) - t + 1;
          const CPPUNIV_EXTENDED_REAL y = c.value[j] - r;
          // Combine before taking logs to retain adjacent-integer signs.
          const CPPUNIV_EXTENDED_REAL z = hyper_detail::product_difference(
              r, failures, t, y) / ((t - r) * failures);
          d.add(c.count[j] * std::log1p(z));
        }
        return d;
      });
      out.m = peak.at; out.tie = peak.tie; out.iterations = peak.evaluations;
      out.status = std::isnan(peak.tie) ? NhyperStatus::Ok : NhyperStatus::Tie;
    } else out.status = NhyperStatus::Ok;
  }
  CPPUNIV_EXTENDED_REAL ll = 0;
  for (std::size_t j = 0; j < c.value.size(); ++j)
    ll += c.count[j] * nhyper_log_pmf(c.value[j], out.N - out.m, out.m, r);
  out.logLik = static_cast<double>(ll);
  return out;
}
inline NhyperResult nhyper_mle(span x, double r, double N, double m) {
  auto out=nhyper_mle_search(x, r, N, m);
  if(out.status==NhyperStatus::FaceUnresolved || out.status==NhyperStatus::FaceTie ||
     out.status==NhyperStatus::NumericalFailure) {
    for(const auto& candidate:out.stop_detail.candidates) {
      if(candidate.model!=StopModel::None || candidate.coef.size()!=3 || !std::isfinite(candidate.loglik_lower))continue;
      if(out.certified || candidate.loglik_lower>out.logLik) {
        out.m=candidate.coef[0];
        out.N=candidate.coef[1];
        out.r=candidate.coef[2];
        out.logLik=candidate.loglik_lower;out.certified=false;
      }
    }
    if(!out.certified)out.status=NhyperStatus::Ok;
  }
  return out;
}
}  // namespace cppunivariate
