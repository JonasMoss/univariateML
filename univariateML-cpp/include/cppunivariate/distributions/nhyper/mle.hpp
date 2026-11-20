#pragma once
#include "../../core/outcome.hpp"
#include "../../core/discrete_stop.hpp"
#include "../../core/precision.hpp"
#include "../../core/work_budget.hpp"

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
  // False when another (m, N, r) attains the same likelihood exactly.
  bool unique = true;
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
inline double log_likelihood(const hyper_detail::Counts& c, double N, double m, double r) {
  hyper_detail::Sum ll;
  for (std::size_t j = 0; j < c.value.size(); ++j)
    ll.add(c.count[j] * nhyper_log_pmf(c.value[j], N - m, m, r));
  return static_cast<double>(ll.value());
}

// The composition log likelihood is concave on [r, N-max(x)+r].
inline hyper_detail::Peak composition_peak(const hyper_detail::Counts& c, double N, double r) {
  return hyper_detail::unimodal_peak(r, N - c.max + r, false, [&](double t) {
    hyper_detail::Sum d;
    for (std::size_t j = 0; j < c.value.size(); ++j) {
      const ExtendedReal failures = static_cast<ExtendedReal>(N) - t + 1;
      const ExtendedReal y = c.value[j] - r;
      const ExtendedReal z = hyper_detail::product_difference(r, failures, t, y) /
          ((t - r) * failures);
      d.add(c.count[j] * std::log1p(z));
    }
    return d;
  });
}

// P(T=x) = (r/x) P(H_x=r). With m>r and x-r>=r+1, at least
// one adjacent hypergeometric mass is at least 1/4 of P(H_x=r).
inline double log_upper(const hyper_detail::Counts& c, double r, bool more_white = false) {
  hyper_detail::Sum ll;
  for (std::size_t j = 0; j < c.value.size(); ++j) {
    const double x = c.value[j];
    const double bound = more_white && x-r > r ? 0.8 : 1.0;
    ll.add(c.count[j] * std::log(bound * (r / x)));
  }
  return static_cast<double>(ll.value());
}

inline hyper_detail::Sum population_increment(const hyper_detail::Counts& c,
                                              double N, double m, double r) {
  const ExtendedReal A = N + 1;
  hyper_detail::Sum increment;
  for (std::size_t j = 0; j < c.value.size(); ++j) {
    const ExtendedReal x = c.value[j];
    const ExtendedReal failures = (A - m) - (x - r);
    const ExtendedReal numerator = hyper_detail::product_difference(m, x, r, A);
    const ExtendedReal z = numerator / (A * failures);
    const ExtendedReal term = z > -0.5 ? std::log1p(z) :
        std::log((A - m) / A) + std::log((A - x) / failures);
    increment.add(c.count[j] * term);
  }
  return increment;
}

// For m > r the per-observation log ratio, as a function of x,
// vanishes at r*A/m and has derivative (m-r)/[(A-x)(A-m-x+r)].
// As A grows, the ratio of the new derivative to the old one decreases
// with x. Rescaling at the old zero therefore shrinks positive integrals
// relative to negative ones. The zero also moves right, so a nonpositive
// aggregate ratio stays nonpositive. For m == r, every factor is <= 1.
inline NhyperResult population_peak(const hyper_detail::Counts& c, double m,
                                    double r, int maxit) {
  NhyperResult out{m, NAN, r, NAN, NAN, 0, NhyperStatus::Infeasible};
  const double excess = c.max - r;
  if (excess > hyper_detail::kMaxCount - m) return out;

  double lo = m + excess;
  const ExtendedReal bound = static_cast<ExtendedReal>(m) * c.max / r;
  constexpr double epsilon = 64 * std::numeric_limits<double>::epsilon();
  // Pad the quotient upward for the product and division roundoff.
  double hi = std::max(lo, static_cast<double>(std::min(
      ExtendedReal(hyper_detail::kMaxCount), std::ceil(bound * (1 + epsilon)))));
  out.status = NhyperStatus::NumericalFailure;
  if (m == r) hi = lo;
  if (maxit <= 0) return out;

  auto increment = [&](double N) {
    ++out.iterations;
    return population_increment(c, N, m, r);
  };
  while (lo < hi && out.iterations < maxit) {
    const double mid = lo + std::floor((hi - lo) / 2);
    const auto d = increment(mid);
    if (d.value() > 0) lo = mid + 1;
    else hi = mid;
  }
  if (lo != hi) return out;

  const double edge = m + excess;
  if (lo > edge && out.iterations < maxit) {
    const auto d = increment(lo - 1);
    if (std::fabs(d.value()) <= epsilon * d.scale) --lo;
  }
  // Endpoint signs suffice; cancellation in any earlier comparison need
  // not withdraw certification if both signs here can be separated.
  if (lo > edge) {
    out.certified = false;
    if (out.iterations < maxit) {
      const auto d = increment(lo - 1);
      out.certified = d.value() > epsilon * (1 + d.scale);
    }
  }
  if (m != r) {
    if (out.iterations < maxit) {
      const auto d = increment(lo);
      const auto margin = epsilon * (1 + d.scale);
      if (lo == hyper_detail::kMaxCount && d.value() > margin) return out;
      out.certified = out.certified && d.value() < -margin;
    } else {
      if (lo == hyper_detail::kMaxCount) return out;
      out.certified = false;
    }
  }
  out.N = lo;
  out.logLik = log_likelihood(c, lo, m, r);
  if (std::isfinite(out.logLik)) out.status = NhyperStatus::Ok;
  return out;
}
}  // namespace nhyper_detail

// NaN requests estimation. N held permits composition and stopping count
// free; m held permits a finite search with N free. Existing composition
// search with N and r held:
// L(m)/L(m-1) = prod m/(m-r) * (N-m+1-x+r)/(N-m+1).
// Each factor is nonincreasing in m, strictly so for r>0. Thus the
// likelihood is log concave on [r, N-max(x)+r], including its endpoints.
inline NhyperResult nhyper_mle(span x, double r, double N, double m, int maxit = 100);
inline NhyperResult nhyper_mle_search(span x, double r, double N, double m, int maxit) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  NhyperResult out{m, N, r, nan, nan, 0, NhyperStatus::InvalidInput};
  if (maxit < 1) return out;
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
      // Constant sample c with r = c: m = N makes the first c draws white with
      // probability one, so (N, N, c) has likelihood one for every N >= c.
      out.unique = false;
      out.status = NhyperStatus::Ok; return out;
    }
    const double rlo = std::isnan(r) ? 1 : r;
    const double rhi = std::isnan(r) ? c.min : r;
    out.status = NhyperStatus::NumericalFailure;
    if (c.max > 100000 || rhi > 100000 ||
        (rhi-rlo+1) * static_cast<double>(c.value.size()) > 1000000 ||
        c.max * static_cast<double>(c.value.size()) > 1000000){
      // At N = max(x) feasibility forces m = r. Start with the mode
      // on that boundary before spending work on other compositions.
      const auto diagonal = hyper_detail::unimodal_peak(rlo, rhi, false, [&](double t) {
        hyper_detail::Sum d;
        for (std::size_t j = 0; j < c.value.size(); ++j) {
          const ExtendedReal xj = c.value[j], n = c.max;
          const ExtendedReal z = hyper_detail::product_difference(1, xj - t + 1, t - 1, n - xj) /
              ((t - 1) * (n - t + 1));
          const ExtendedReal term = z > -0.5 ? std::log1p(z) :
              std::log(t / (t - 1)) + std::log((xj - t + 1) / (n - t + 1));
          d.add(c.count[j] * term);
        }
        return d;
      });
      out.iterations = diagonal.evaluations;
      double value = -INFINITY;
      auto retain = [&](const NhyperResult& f) {
        out.iterations += f.iterations;
        if (std::isfinite(f.logLik) && f.logLik > value) {
          value = f.logLik;
          out.stop_detail.candidates = {{{f.m,f.N,f.r},value,value}};
        }
      };
      const auto boundary = nhyper_detail::population_peak(c, diagonal.at, diagonal.at, maxit);
      retain(boundary);
      if (rlo == rhi && std::isfinite(boundary.logLik)) {
        const double bound = nhyper_detail::log_upper(c, rlo, true);
        if (boundary.logLik > bound + 1e-9 * (1 + std::fabs(bound))) return boundary;
      }

      // Profile populations at compositions selected by exact adjacent
      // ratios in two slices. The near slice adds one sample range to the
      // support edge; the far slice probes the growing-urn regime,
      // leaving room on either side for the population bisection.
      const double near = c.max + std::min(c.max - c.min, hyper_detail::kMaxCount - c.max);
      const double far = std::max(c.max, std::floor(hyper_detail::kMaxCount / 2));
      const auto limit = std::min(certificate_budget(maxit),
          enumeration_budget(maxit) / static_cast<std::int64_t>(c.value.size()));
      const auto profile = [&](double rr) {
        retain(nhyper_detail::population_peak(c, rr, rr, maxit));
        for (const double nn : {near, far}) {
          if (nn == c.max) continue;
          // Composition bisection plus the adjacent tie checks.
          const int composition_work = std::ilogb(nn - c.max) + 3;
          if (limit - out.iterations <= composition_work) continue;
          const auto seed = nhyper_detail::composition_peak(c, nn, rr);
          out.iterations += seed.evaluations;
          const int remaining = static_cast<int>(std::min<std::int64_t>(maxit, limit - out.iterations));
          retain(nhyper_detail::population_peak(c, seed.at, rr, remaining));
        }
      };
      profile(diagonal.at);
      for (std::int64_t i = 0; i <= rhi-rlo && out.iterations < limit; ++i) {
        const double rr = i % 2 == 0 ? rhi - i/2 : rlo + i/2;
        // Count a rejected line as a certificate evaluation too.
        ++out.iterations;
        if (rr != diagonal.at &&
            nhyper_detail::log_upper(c, rr) > value - 1e-9 * (1 + std::fabs(value)))
          profile(rr);
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
    std::int64_t work = 0;
    const auto grid_budget = enumeration_budget(maxit);
    bool face_tie = false;
    auto evaluate = [&](double nn) {
      for (double rr = rlo; rr <= rhi; ++rr) {
        const NhyperResult f = nhyper_mle(x, rr, nn, nan, maxit);
        work += std::max(1, f.iterations);
        if ((f.status != NhyperStatus::Ok && f.status != NhyperStatus::Tie &&
             f.status != NhyperStatus::Unidentified) || !std::isfinite(f.logLik)) return false;
        if (f.logLik > best) { best = f.logLik; winner = f;
          out.stop_detail.candidates.erase(std::remove_if(out.stop_detail.candidates.begin(),out.stop_detail.candidates.end(),
            [](const FitCandidate& c){return c.model==StopModel::None;}),out.stop_detail.candidates.end());
          out.stop_detail.candidates.push_back({{f.m,f.N,f.r},f.logLik,f.logLik}); }
        if (std::fabs(f.logLik-face) <= margin) face_tie = true;
        if(work>=grid_budget){out.stop_detail=discrete_budget(out.stop_detail);return false;}
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
      out.iterations = static_cast<int>(std::min<std::int64_t>(work, std::numeric_limits<int>::max())); return out;
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
    if ((end-lo)*(rhi-rlo+1) > grid_budget/4){out.stop_detail=discrete_budget(out.stop_detail);return out;}
    for (double nn = lo; nn < end; ++nn) if (!evaluate(nn)) return out;
    out = winner; out.iterations = static_cast<int>(std::min<std::int64_t>(work, std::numeric_limits<int>::max())); out.status = NhyperStatus::Ok;
    return out;
  }
  if ((std::isnan(r) && std::isnan(m) && !std::isnan(N)) ||
      (std::isnan(N) && !std::isnan(m))) {
    const bool fixed_N = !std::isnan(N);
    if (x.empty() || (fixed_N ? !hyper_detail::is_count(N) : !hyper_detail::is_count(m)) ||
        (!std::isnan(r) && !hyper_detail::is_count(r))) return out;
    for (double v : x) if (!hyper_detail::is_count(v)) return out;
    const auto c = hyper_detail::tabulate(x);
    out.status = NhyperStatus::Infeasible;
    if ((fixed_N && c.max > N) || (!fixed_N && r > m)) return out;
    if (c.max == 0) {
      if (!std::isnan(r) && r != 0) return out;
      out.r = 0;
      if (fixed_N) out.m = 0;
      else out.N = m;
      out.logLik = 0; out.status = NhyperStatus::Ok;
      return out;
    }
    if (c.min == 0 || r == 0) return out;
    if (!fixed_N && c.min == c.max && m >= c.min && (std::isnan(r) || r == c.min)) {
      out.N=m;out.r=c.min;out.logLik=0;out.status=NhyperStatus::Ok;return out;
    }
    const double rlo = std::isnan(r) ? (fixed_N ? 1 :
        std::max(1., m - (hyper_detail::kMaxCount - c.max))) : r;
    const double rhi = std::isnan(r) ? std::min(c.min, fixed_N ? N : m) : r;
    if (rlo > rhi || rhi > c.min) return out;
    out.status = NhyperStatus::NumericalFailure;
    if (!fixed_N) {
      const auto cap = certificate_budget(maxit);
      const bool partial = rhi-rlo >= cap;
      double unchecked = rlo > 1 && std::isnan(r)
          ? nhyper_detail::log_upper(c, rlo - 1) : -INFINITY;
      double best = -INFINITY;
      double alternative = -INFINITY;
      for (std::int64_t i = 0; i < cap && rlo + i <= rhi; ++i) {
        // If the allowance cannot cover every line, visit both ends
        // of the feasible stopping-count range.
        const double rr = !partial ? rlo + i : (i % 2 == 0 ? rhi - i/2 : rlo + i/2);
        const auto line = nhyper_detail::population_peak(c, m, rr, maxit);
        out.iterations += std::min(line.iterations, std::numeric_limits<int>::max() - out.iterations);
        if (line.status != NhyperStatus::Ok || !line.certified)
          unchecked = std::max(unchecked, nhyper_detail::log_upper(c, rr));
        if (line.status != NhyperStatus::Ok) continue;
        const bool tied = std::isfinite(best) && hyper_detail::grid_tied(line.logLik, best);
        const bool earlier = line.N < out.N || (line.N == out.N && line.r < out.r);
        if (!std::isfinite(best) || (tied ? earlier : line.logLik > best)) {
          if (std::isfinite(out.logLik)) alternative = std::max(alternative, out.logLik);
          out.N = line.N;
          out.r = line.r;
          out.logLik = line.logLik;
        } else {
          alternative = std::max(alternative, line.logLik);
        }
        best = std::max(best, line.logLik);
      }
      if (partial) {
        unchecked = std::max(unchecked, nhyper_detail::log_upper(c, rhi - (cap+1)/2));
        out.stop_detail = discrete_budget(out.stop_detail);
      }
      if (std::isfinite(out.logLik)) {
        const double margin = 1e-9 * (1 + std::fabs(out.logLik));
        out.certified = out.logLik > std::max(unchecked, alternative) + margin;
        out.status = NhyperStatus::Ok;
      }
      return out;
    }

    // Retain the fixed-population grid and its lexicographic tie convention.
    const double width = N - c.max + 1;
    const auto grid_budget = enumeration_budget(maxit);
    std::int64_t cells = 0;
    const bool partial = 2 * width * (rhi-rlo+1) > grid_budget;
    ExtendedReal best = -INFINITY;
    bool selected = false;
    for (int pass = 0; pass < 2; ++pass) {
      for (double rr = rlo; rr <= rhi; ++rr) {
        for (double mm = rr; mm <= N - c.max + rr; ++mm) {
          hyper_detail::Sum ll;
          for (std::size_t j = 0; j < c.value.size(); ++j) {
            const ExtendedReal v = c.value[j];
            ll.add(c.count[j] * (hyper_detail::grid_choose(v - 1, rr - 1) +
                hyper_detail::grid_choose(N - v, mm - rr) - hyper_detail::grid_choose(N, mm)));
          }
          ++cells;
          if (out.iterations < std::numeric_limits<int>::max()) ++out.iterations;
          if (!std::isfinite(ll.value())) {
            if (partial && std::isfinite(out.logLik)) {
              out.status = NhyperStatus::Ok;
              out.certified = false;
            }
            return out;
          }
          if (partial) {
            if (ll.value() > best) {
              best = ll.value(); out.m = mm; out.r = rr; out.logLik = static_cast<double>(best);
            }
            if (cells >= grid_budget / 2) {
              out.status = NhyperStatus::Ok;
              out.certified = false;
              out.stop_detail = discrete_budget(out.stop_detail);
              return out;
            }
          }
          if (pass == 0) best = std::max(best, ll.value());
          else if (hyper_detail::grid_tied(ll.value(), best) &&
              (!selected || mm < out.m || (mm == out.m && rr < out.r))) {
            out.m = mm; out.r = rr; out.logLik = static_cast<double>(ll.value()); selected = true;
          }
        }
      }
    }
    if (selected) out.status = NhyperStatus::Ok;
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
      const auto peak = nhyper_detail::composition_peak(c, N, r);
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
inline NhyperResult nhyper_mle(span x, double r, double N, double m, int maxit) {
  auto out=nhyper_mle_search(x, r, N, m, static_cast<int>(solver_budget(maxit)));
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
  if(out.status==NhyperStatus::Ok &&
      std::isnan(r)&&std::isnan(m)&&!std::isnan(N)) {
    // Use stable masses at the public coefficients, including fixed-N grid fits.
    hyper_detail::Sum ll;
    for(double v:x)ll.add(nhyper_log_pmf(v,out.N-out.m,out.m,out.r));
    out.logLik=static_cast<double>(ll.value());
  }
  return out;
}
}  // namespace cppunivariate

namespace cppunivariate {
inline bool family_point(const NhyperResult&, const FitCandidate& c) {
  return c.model == StopModel::None && c.coef.size() == 3 && std::isfinite(c.coef[0]) &&
         std::isfinite(c.coef[1]) && std::isfinite(c.coef[2]);
}
// nhyper_mle returns any family candidate of a FaceTie as an uncertified Ok; a
// FaceTie that survives lists the face alone.
inline bool accurate_point(const NhyperResult&, const FitCandidate&) { return false; }
inline FitOutcome outcome(const NhyperResult& r) {
  if (r.stop_detail.budget_exhausted && r.status != NhyperStatus::Ok &&
      r.status != NhyperStatus::Tie && r.status != NhyperStatus::Unidentified) return FitOutcome::budget();
  switch (r.status) {
    case NhyperStatus::Ok: return FitOutcome::estimate(r.certified, r.unique);
    case NhyperStatus::Tie: return FitOutcome::estimate(r.certified, false);
    case NhyperStatus::Unidentified: return FitOutcome::estimate(r.certified, false);
    case NhyperStatus::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case NhyperStatus::Infeasible: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case NhyperStatus::NumericalFailure: return FitOutcome::failure(OutcomeReason::Evaluation);
    case NhyperStatus::FaceUnresolved: return FitOutcome::failure(OutcomeReason::Unresolved);
    case NhyperStatus::FaceTie: return comparison_outcome_of(r);
  }
  return {};
}
} // namespace cppunivariate
