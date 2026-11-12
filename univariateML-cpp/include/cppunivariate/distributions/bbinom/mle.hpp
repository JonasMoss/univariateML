// Beta-binomial MLE with known size N.
//
// Parametrisation (Griffiths 1973): p = alpha/(alpha+beta), s = 1/(alpha+beta).
// With A_j = #{x_i > j} and C_j = #{x_i < N - j}, j = 0..N-1, the
// log-likelihood is a sum of log rising factorials,
//   l(p, s) = sum_i log C(N, x_i)
//           + sum_j [A_j log(p + j s) + C_j log(1 - p + j s) - h log(1 + j s)],
// so huge shapes need no lbeta subtraction; s = 0 is the binomial face.
//
// Free fit. For fixed s, l is strictly concave in p (d2l/dp2 < 0), so the
// problem is the profile L(s) = max_p l(p, s). For s > h H_{N-1} / m
// (m = #{0 < x_i < N}, H the harmonic number), dl/ds < 0 for every p, so the
// maximiser lies in [0, h H_{N-1} / m]. The search scans a log-grid of s on
// that interval, refines every grid local maximum by safeguarded Newton on
// the profile score, and compares the best interior value with the binomial
// face L(0). A signed uniform expansion excludes a neighbourhood of s=0
// before a strict face comparison; unclosed comparisons retain the finite search result uncertified.
// Endpoint-only samples (all x_i in
// {0, N}, both present) have l(p, s) strictly increasing in s: the supremum is
// the two-point law on {0, N}. All-0 / all-N samples reach a point mass.
// N = 1 (Bernoulli) leaves alpha + beta unidentified.
//
// One shape fixed. With alpha = a fixed, dl/dc > 0 for
// c < C_0 / (h sum_j 1/(a+j)) and dl/dc < 0 for c > (N - xbar)(a + N)/xbar,
// so the maximiser lies in that interval; same log-grid scan and Newton
// refinement. Only all-0 / all-N samples escape (to a point mass). Fixing
// beta is the mirror image (x -> N - x).
//
// Finite proposals also require a negative-curvature region and public-shape
// score enclosures; every point outside that region is excluded strictly.
// Cost: O(N) per likelihood evaluation, adaptive certificate work (at most
// 128 * min(maxit, 100000) interval evaluations); no unimodality assumption is made.

#pragma once
#include "accuracy.hpp"
#include "../../core/work_budget.hpp"
#include "../../core/discrete_stop.hpp"

#include "../../core/span.hpp"
#include "../../special/special.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

enum class BbinomStatus {
  Ok = 0,
  InvalidInput,      // empty data, negative count, invalid size/tol/maxit/fixed shape
  SizeTooSmall,      // max(x) > size
  BinomialLimit,     // alpha + beta -> infinity: Binomial(size, mean/size) wins
  TwoPointLimit,     // alpha, beta -> 0: all observations in {0, size}, both present
  PointMass,         // all observations 0 or all equal size
  Unidentified,      // size = 0, or size = 1 with both shapes free
  SizeBudget,
  ProfileTie,
  IterationFailure,
  FaceUnresolved
};

struct BbinomResult {
  double size, alpha, beta, logLik;
  int iterations;
  BbinomStatus status;
  StopDetail stop_detail{};
  bool certified = true;
  bool accuracy_failure = false;
};

namespace bbinom_detail {

inline BbinomResult retain_budget(BbinomResult child,const BbinomResult& parent) {
  if(!child.stop_detail.budget_exhausted && child.status!=BbinomStatus::SizeBudget)return child;
  auto& detail=child.stop_detail;
  detail.candidates.insert(detail.candidates.end(),parent.stop_detail.candidates.begin(),parent.stop_detail.candidates.end());
  if(detail.limit_model==StopModel::None && parent.stop_detail.limit_model!=StopModel::None) {
    detail.limit_model=parent.stop_detail.limit_model;detail.limit_coef=parent.stop_detail.limit_coef;
    detail.limit_loglik=parent.stop_detail.limit_loglik;
  }
  if(std::isfinite(parent.size) && std::isfinite(parent.alpha) && std::isfinite(parent.beta) && std::isfinite(parent.logLik))
    detail.candidates.push_back({{parent.size,parent.alpha,parent.beta},parent.logLik,parent.logLik});
  for(const auto& c:detail.candidates)if(std::isfinite(c.loglik_lower) && (!std::isfinite(detail.loglik_lower) || c.loglik_lower>detail.loglik_lower))detail.loglik_lower=c.loglik_lower;
  if(std::isfinite(detail.limit_loglik) && (!std::isfinite(detail.loglik_lower)||detail.limit_loglik>detail.loglik_lower))detail.loglik_lower=detail.limit_loglik;
  detail.loglik_upper=INFINITY;return child;
}
struct CellWork {
  std::int64_t cap;
  mutable std::int64_t used=0;
  mutable bool exhausted=false;
  explicit CellWork(std::int64_t value):cap(value){}
  bool consume(std::int64_t cells) const {
    if(cells>cap-used){exhausted=true;return false;}
    used+=cells;return true;
  }
};
struct Data {
  CellWork owned{enumeration_budget(500)};
  const CellWork& work;
  Data():work(owned){}
  explicit Data(const CellWork& input):work(input){}

  int size = 0;
  int maxit = 500;
  double n = 0, interior = 0, zeros = 0, tops = 0, sum = 0;
  long double squares = 0, total = 0;
  double log_choose = 0;  // sum_i log C(N, x_i)
  std::vector<double> A, C;  // length N
};

// l(p, s) without the binomial coefficients; q = 1 - p passed separately.
inline double loglik(const Data &d, double p, double q, double s) {
  if(!d.work.consume(d.size))return NAN;
  double ll = 0;
  for (int j = 0; j < d.size; ++j) {
    const double js = static_cast<double>(j) * s;
    if (d.A[j] > 0) ll += d.A[j] * std::log(p + js);
    if (d.C[j] > 0) ll += d.C[j] * std::log(q + js);
    ll -= d.n * std::log1p(js);
  }
  return ll;
}

// Binomial face: l(xbar/N, 0).
inline double binomial_face(const Data &d) {
  const double N = d.size, total = d.n * N;
  double ll = 0;
  if (d.sum > 0) ll += d.sum * std::log(d.sum / total);
  if (total - d.sum > 0) ll += (total - d.sum) * std::log((total - d.sum) / total);
  return ll;
}

struct Point {
  double s, p, value;
};

// argmax_p l(p, s): the score sum_j A_j/(p+js) - C_j/(1-p+js) decreases
// strictly from +inf to -inf on (0, 1). Safeguarded Newton.
inline double solve_p(const Data &d, double s, double p0) {
  if (s == 0) return d.sum / (d.n * d.size);
  double lo = 0, hi = 1, p = std::min(std::max(p0, 1e-300), 1 - 1e-16);
  if (!(p > 0 && p < 1)) p = 0.5;
  bool converged = false;
  for (int it = 0; it < d.maxit; ++it) {
    if(!d.work.consume(d.size))return NAN;
    double g = 0, H = 0;
    for (int j = 0; j < d.size; ++j) {
      const double js = static_cast<double>(j) * s;
      const double a = p + js, c = (1 - p) + js;
      g += d.A[j] / a - d.C[j] / c;
      H += d.A[j] / (a * a) + d.C[j] / (c * c);
    }
    if (g > 0) lo = p; else hi = p;
    double next = p + g / H;
    if (!(next > lo && next < hi)) next = 0.5 * (lo + hi);
    if (std::abs(next - p) <= 1e-15 * std::min(p, 1 - p) || hi - lo <= 1e-300) {
      p = next;
      converged = true;
      break;
    }
    p = next;
  }
  return converged ? p : std::numeric_limits<double>::quiet_NaN();
}

inline Point profile(const Data &d, double s, double p0) {
  const double p = solve_p(d, s, p0);
  return {s, p, loglik(d, p, 1 - p, s)};
}

// d/du and d2/du2 of the profile L at u = log s (envelope theorem; the
// second derivative is l_ss - l_ps^2 / l_pp in s, then the chain rule).
inline void profile_derivs(const Data &d, double s, double p, double &g, double &H) {
  if(!d.work.consume(d.size)){g=H=NAN;return;}
  double ls = 0, lpp = 0, lps = 0, lss = 0;
  for (int j = 0; j < d.size; ++j) {
    const double dj = static_cast<double>(j), js = dj * s;
    const double a = p + js, c = (1 - p) + js, e = 1 + js;
    const double ia = d.A[j] / a, ic = d.C[j] / c;
    const double ia2 = ia / a, ic2 = ic / c, ie2 = d.n / (e * e);
    ls += dj * (ia + ic - d.n / e);
    lpp -= ia2 + ic2;
    lps -= dj * (ia2 - ic2);
    lss -= dj * dj * (ia2 + ic2 - ie2);
  }
  const double L2 = lss - lps * lps / lpp;
  g = s * ls;
  H = s * ls + s * s * L2;
}

// One free shape b with the other shape k fixed; W_j = C_j (b = beta) or
// A_j (b = alpha). Returns d/du and d2/du2 of l at u = log b.
inline void shape_derivs(const Data &d, const std::vector<double> &W, double k, double b,
                         double &g, double &H) {
  if(!d.work.consume(d.size)){g=H=NAN;return;}
  g = 0; H = 0;
  for (int j = 0; j < d.size; ++j) {
    const double dj = static_cast<double>(j);
    const double bj = b + dj, t = k + b + dj;
    g += b * (W[j] / bj - d.n / t);
    H += W[j] * b * dj / (bj * bj) - d.n * b * (k + dj) / (t * t);
  }
}

// Maximise a smooth function of u over a grid scan. `deriv(u, g, H)` returns
// the value and sets the first two derivatives. Every grid interval on which
// g changes sign from + to - holds a local maximum; each is refined by
// Newton on g, safeguarded by bisection inside its bracket. The analytic
// derivative stays accurate where values are flat to rounding (near the
// binomial face), so no spurious maxima arise there. Returns the best value
// (-inf when no sign change exists) and its location.
template <typename Deriv>
inline double maximise_on_grid(const std::vector<double> &grid, double tol, int maxit,
                               Deriv deriv, double &best_u, int &iterations, std::vector<Point>& candidates, const double& profile_p=0) {
  maxit = static_cast<int>(solver_budget(maxit));
  double best = -std::numeric_limits<double>::infinity();
  std::vector<double> g(grid.size()), H(grid.size());
  for (std::size_t k = 0; k < grid.size(); ++k) {
    deriv(grid[k], g[k], H[k]);
    if(!std::isfinite(g[k]) || !std::isfinite(H[k]))
      return std::numeric_limits<double>::quiet_NaN();
  }
  for (std::size_t k = 0; k + 1 < grid.size(); ++k) {
    if (!(g[k] > 0 && g[k + 1] <= 0)) continue;
    double a = grid[k], b = grid[k + 1];
    double u = 0.5 * (a + b), gu, Hu;
    double value = deriv(u, gu, Hu);
    bool converged = false;
    for (int it = 0; it < maxit; ++it) {
      ++iterations;
      if (gu > 0) a = u; else b = u;
      double next = (Hu < 0) ? u - gu / Hu : 0.5 * (a + b);
      if (!(next > a && next < b)) next = 0.5 * (a + b);
      const double step = std::abs(next - u);
      u = next;
      value = deriv(u, gu, Hu);
      if (step < tol * (1 + std::abs(u)) || b - a < tol * (1 + std::abs(u))) { converged = true; break; }
    }
    if (!converged && !(std::isfinite(gu) && std::abs(gu) < 1e-3))
      return std::numeric_limits<double>::quiet_NaN();
    candidates.push_back({std::exp(u),profile_p,value});
    if (value > best) { best = value; best_u = u; }
  }
  return best;
}

// Certify the spaces between grid points without assuming unimodality.
// Write l = V(p,s) - D(s), with V jointly concave and D concave.
// A tangent to V and a chord to -D bound l above throughout each interval.
// The residual p score contributes at most |l_p|, since 0 <= p <= 1.
// This is a floating-point likelihood certificate, not interval arithmetic.
template <typename Bound>
inline bool certify(const std::vector<double> &grid, double best, double allowance,
                    Bound bound, int maxit, bool& exhausted) {
  maxit = static_cast<int>(solver_budget(maxit));
  struct Interval { double a, b; };
  std::vector<Interval> pending;
  for (std::size_t k = 0; k + 1 < grid.size(); ++k)
    pending.push_back({grid[k], grid[k + 1]});
  int evaluations = 0;
  while (!pending.empty()) {
    const Interval z = pending.back();
    pending.pop_back();
    if (++evaluations > certificate_budget(maxit)) {exhausted=true;return false;}
    const double m = z.a + (z.b - z.a) * 0.5;
    if (!(m > z.a && m < z.b)) return false;
    const double upper = bound(z.a, m, z.b);
    if (!std::isfinite(upper)) return false;
    if (upper <= best + allowance) continue;
    pending.push_back({z.a, m});
    pending.push_back({m, z.b});
  }
  return true;
}

template <typename Bound>
inline bool certify(const std::vector<double>& grid,double best,double allowance,Bound bound) {
  bool exhausted=false;
  return certify(grid,best,allowance,bound,500,exhausted);
}

inline double profile_bound(const Data &d, double a, double m, double b) {
  if(!d.work.consume(d.size))return NAN;
  const double p = solve_p(d, m, d.sum / (d.n * d.size));
  const double value = loglik(d, p, 1 - p, m);
  double vs = 0, gp = 0, da = 0, db = 0;
  for (int j = 0; j < d.size; ++j) {
    const double jm = j * m;
    const double ia = d.A[j] / (p + jm), ic = d.C[j] / (1 - p + jm);
    vs += j * (ia + ic);
    gp += ia - ic;
    da += d.n * std::log1p(j * (a - m) / (1 + jm));
    db += d.n * std::log1p(j * (b - m) / (1 + jm));
  }
  const double rounding = 128 * std::numeric_limits<double>::epsilon() *
                          (1 + std::abs(value) + d.n * d.size);
  return value + std::abs(gp) + std::max(vs * (a - m) - da, vs * (b - m) - db) + rounding;
}

inline double shape_bound(const Data &d, double k, double a, double m, double b) {
  if(!d.work.consume(d.size))return NAN;
  const double value = loglik(d, k / (k + m), m / (k + m), 1 / (k + m));
  double vs = 0, da = 0, db = 0;
  for (int j = 0; j < d.size; ++j) {
    vs += d.C[j] / (m + j);
    da += d.n * std::log1p((a - m) / (k + m + j));
    db += d.n * std::log1p((b - m) / (k + m + j));
  }
  const double rounding = 128 * std::numeric_limits<double>::epsilon() *
                          (1 + std::abs(value) + d.n * d.size);
  return value + std::max(vs * (a - m) - da, vs * (b - m) - db) + rounding;
}

// Signed face expansion, uniform over the profiled mean. For a strictly
// negative face score, the tangent to the jointly concave numerator gives
// L(s)-L(0) <= g0*s + B*s*s/2, B=h*sum(j*j). No restriction on p is used.
// At zero or small negative score, score signs enclose p(s), and its Schur
// curvature can enlarge the first-order strip.
// A zero integer numerator is accepted only when its integer arithmetic is
// exactly representable. All other comparisons include a practical margin.
inline double face_neighbourhood(const Data& d, long double squares) {
  const long double h=d.n, N=d.size, S=d.total;
  const long double a=N*h*squares, b=(N-1)*S*S, c=N*h*S;
  const long double numerator=a-b-c;
  const long double eps=std::numeric_limits<long double>::epsilon();
  const long double error=64*eps*(a+b+c);
  const long double p=S/(h*N), q=1-p;
  const long double g=(numerator+error)/(2*h*N*p*q);
  const double B=d.n*(N-1)*N*(2*N-1)/6;
  const double fallback=g<0 && B>0?static_cast<double>(-g/B):0;
  const bool exact_zero=numerator==0 && a+b+c<std::ldexp(1.0L,std::numeric_limits<long double>::digits);
  if(!(fallback>0 || exact_zero)) return 0;
  double radius=static_cast<double>(std::min(p,q))/4;
  double t=radius/(4*d.size);
  for(int it=0;it<d.maxit && t>fallback;++it,t*=0.25,radius*=0.5) {
    if(!d.work.consume(d.size))return fallback;
    const double lo=static_cast<double>(p)-radius, hi=static_cast<double>(p)+radius;
    double gl=0, gu=0, pp=0, ss=0, pslo=0, pshi=0, scale=0;
    for(int j=0;j<d.size;++j) {
      const double A=d.A[j], C=d.C[j], jt=j*t;
      gl+=A/(lo+jt)-C/(1-lo);
      gu+=A/hi-C/(1-hi+jt);
      const double al=A/((hi+jt)*(hi+jt)), au=A/(lo*lo);
      const double cl=C/((1-lo+jt)*(1-lo+jt)), cu=C/((1-hi)*(1-hi));
      pp+=al+cl;
      ss+=j*static_cast<double>(j)*(d.n-al-cl);
      pslo+=j*(cl-au); pshi+=j*(cu-al);
      scale+=j*static_cast<double>(j)*(d.n+au+cu);
    }
    const double cross=std::max(std::abs(pslo),std::abs(pshi));
    const double curvature=ss+cross*cross/pp;
    const double margin=1024*std::numeric_limits<double>::epsilon()*(1+scale+cross*cross/pp);
    if(gl>margin && gu< -margin && curvature< -margin)return t;
  }
  return fallback;
}

// Grid of n points from lo to hi inclusive, at most `step` apart.
inline std::vector<double> log_grid(double lo, double hi, double step) {
  const int n = std::max(3, static_cast<int>(std::ceil((hi - lo) / step)) + 1);
  std::vector<double> u(n);
  for (int i = 0; i < n; ++i) u[i] = lo + (hi - lo) * i / (n - 1);
  return u;
}

#include "acceptance.hpp"

}  // namespace bbinom_detail

// size: known number of trials (required). alpha_fixed / beta_fixed: NaN to
// estimate; both may be held. With size NaN, at least one shape must be held.
// tol: relative public-shape enclosure width; maxit: work limit per solve.
inline BbinomResult bbinom_mle_work(ispan x, double size, double tol, int maxit,
                               double alpha_fixed,double beta_fixed, const bbinom_detail::CellWork& work) {
  maxit = static_cast<int>(solver_budget(maxit));
  auto stopped = [&]() -> BbinomResult {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  BbinomResult out{size, nan, nan, nan, 0, BbinomStatus::InvalidInput};
  const bool fix_a = !std::isnan(alpha_fixed), fix_b = !std::isnan(beta_fixed);
  if (x.size() == 0 || !(tol > 0) || !std::isfinite(tol) || maxit < 1) return out;
  const auto uncertified_prefix=[&](int minimum,BbinomResult best,StopDetail detail) {
    for(std::int64_t j=1;j<solver_budget(maxit) && double(minimum)+j<=1e7 && !work.exhausted;++j) {
      const auto candidate=bbinom_mle_work(x,double(minimum)+j,tol,maxit,alpha_fixed,beta_fixed,work);
      if(candidate.status==BbinomStatus::Ok && std::isfinite(candidate.logLik) && candidate.logLik>best.logLik)best=candidate;
    }
    best.status=BbinomStatus::Ok;best.certified=false;best.stop_detail=detail;
    best.stop_detail.candidates.push_back({{best.size,best.alpha,best.beta},best.logLik,best.logLik});
    best.stop_detail.loglik_upper=INFINITY;
    if(work.exhausted)best.stop_detail=discrete_budget(best.stop_detail);
    return best;
  };
  if (std::isnan(size) && fix_a && !fix_b) {
    if (!(alpha_fixed > 0) || !std::isfinite(alpha_fixed)) return out;
    int mx = 0, mn = std::numeric_limits<int>::max();
    double sum = 0, coeff = 0, quadratic = 0;
    for (int v : x) {
      if (v < 0) return out;
      mx = std::max(mx, v); mn = std::min(mn, v); sum += v;
      quadratic += static_cast<double>(v) * (v + 1.0) / 2;
    }
    if (mx == 0) { out.status = BbinomStatus::Unidentified; return out; }
    if (mn == mx) { out.status = BbinomStatus::PointMass; return out; }
    const std::int64_t budget = enumeration_budget(maxit);
    if (mx > budget) { out.stop_detail=discrete_budget(out.stop_detail); out.status = BbinomStatus::SizeBudget; return out; }
    if(sum>work.cap-work.used || !work.consume(static_cast<std::int64_t>(sum))){out.status=BbinomStatus::SizeBudget;return out;}
    for (int v : x)
      for (int j = 0; j < v; ++j) coeff += std::log((alpha_fixed + j) / (j + 1.0));
    const double h = static_cast<double>(x.size()), a = alpha_fixed, mu = sum / h;
    const double face = coeff + sum * std::log(mu / (a + mu)) - h * a * std::log1p(mu / a);
    out.stop_detail=discrete_face(StopModel::Nbinom,{a,a/(a+mu)},face);
    const double margin = 2e-8 * h + 1024 * std::numeric_limits<double>::epsilon() *
                          (1 + std::abs(face) + std::abs(coeff) + h * a);
    if (!std::isfinite(face) || !std::isfinite(margin)) { out.status = BbinomStatus::IterationFailure; return out; }
    BbinomResult best{nan, a, nan, -std::numeric_limits<double>::infinity(), 0, BbinomStatus::IterationFailure};
    double runner = -std::numeric_limits<double>::infinity();
    auto excludes_tail = [&](double N, double value) {
      if (N <= mx || !(value > face + margin)) return false;
      // For beta <= cN, bound every pmf by P(0) times its exact ratio to P(0).
      // This bound and the correction for beta >= cN both decrease with N.
      const double common = coeff + sum * std::log(N / (N - mx));
      auto small = [&](double c) {
        return common + h * a * std::log((a + c * N) / (a + (1 + c) * N));
      };
      if (!(small(0) + margin < value)) return false;
      double lo = 0, hi = 1;
      for(std::int64_t bracket_pass=0; bracket_pass<solver_budget(maxit) && (hi < 1e12 && small(hi) + margin < value); ++bracket_pass) hi *= 2;
      for (int j = 0; j < maxit; ++j) {
        const double mid = (lo + hi) / 2;
        if (small(mid) + margin < value) lo = mid; else hi = mid;
      }
      const double c = lo * 0.999;
      if (!(c > 0) || !(small(c) + margin < value)) return false;
      const double b = c * N;
      const double correction = quadratic / (N - mx) + h * a * a * (1 / b + 1 / (b * b)) / 2;
      return std::isfinite(correction) && face + correction + margin < value;
    };
    for (std::int64_t n = mx; n <= budget; ++n) {
      const BbinomResult candidate = bbinom_mle_work(x, n, tol, maxit, a, nan,work);
      if (candidate.status != BbinomStatus::Ok) return bbinom_detail::retain_budget(candidate,out);
      out.stop_detail.candidates.push_back({{candidate.size,candidate.alpha,candidate.beta},candidate.logLik,candidate.logLik});
      if (candidate.logLik > best.logLik) { runner = best.logLik; best = candidate; }
      else runner = std::max(runner, candidate.logLik);
      // Neither outcome asserts that a larger finite size cannot improve the fit.
      if (n == mx && best.logLik <= face + margin) return uncertified_prefix(mx,best,out.stop_detail);
      if (n == mx && !excludes_tail(budget + 1.0, best.logLik)) return uncertified_prefix(mx,best,discrete_budget(out.stop_detail));
      if (excludes_tail(n + 1.0, best.logLik)) {
        if (runner + margin >= best.logLik) best.status = BbinomStatus::ProfileTie;
        best.stop_detail=out.stop_detail;
        return best;
      }
    }
    out.stop_detail=discrete_budget(out.stop_detail); out.status = BbinomStatus::SizeBudget;
    return out;
  }
  if (std::isnan(size) && !fix_a && fix_b) {
    if (!(beta_fixed > 0) || !std::isfinite(beta_fixed)) return out;
    int mx = 0, mn = std::numeric_limits<int>::max(), positives = 0;
    for (int v : x) {
      if (v < 0) return out;
      mx = std::max(mx, v); mn = std::min(mn, v);
      if (v > 0) ++positives;
    }
    if (mx == 0) { out.status = BbinomStatus::Unidentified; return out; }
    if (mn == mx) { out.status = BbinomStatus::PointMass; return out; }
    // At N+1 the alpha score is positive below m/(h H), H=sum 1/(b+j).
    // The size ratio is <=1 for alpha >= M(b-1)/(N+1-M).
    // (N+1-M)/H is increasing, so the first such N controls the tail.
    const auto seed=bbinom_mle_work(x,mx,tol,maxit,alpha_fixed,beta_fixed,work);
    if(seed.status!=BbinomStatus::Ok)return seed;
    out.stop_detail.candidates.push_back({{seed.size,seed.alpha,seed.beta},seed.logLik,seed.logLik});
    const std::int64_t budget = enumeration_budget(maxit);
    if (mx > budget) { out.stop_detail=discrete_budget(out.stop_detail); out.status = BbinomStatus::SizeBudget; return out; }
    if(!work.consume(static_cast<std::int64_t>(mx)+1)){out.status=BbinomStatus::SizeBudget;return out;}
    double harmonic = 0;
    for (int j = 0; j <= mx; ++j) harmonic += 1 / (beta_fixed + j);
    std::int64_t end = mx;
    const double margin = 1 + 256 * std::numeric_limits<double>::epsilon() * (budget + beta_fixed + 1);
    // H_N >= (N+1)/(beta+N). If this lower bound already defeats
    // every affordable endpoint, spend the allowance on finite profiles.
    if(beta_fixed>1 && positives<margin*x.size()*mx*(beta_fixed-1)/(beta_fixed+budget))
      return uncertified_prefix(mx,seed,discrete_budget(out.stop_detail));
    while (beta_fixed > 1 &&
           static_cast<double>(positives) * (end + 1.0 - mx) <
             margin * x.size() * mx * (beta_fixed - 1) * harmonic) {
      if (end >= budget) { out.stop_detail=discrete_budget(out.stop_detail); out.status = BbinomStatus::SizeBudget; return out; }
      if(!work.consume(1)){out.status=BbinomStatus::SizeBudget;return out;}
      ++end;
      harmonic += 1 / (beta_fixed + end);
    }
    BbinomResult best{nan, nan, beta_fixed, -std::numeric_limits<double>::infinity(), 0, BbinomStatus::IterationFailure};
    double runner = -std::numeric_limits<double>::infinity();
    for (std::int64_t n = mx; n <= end; ++n) {
      const BbinomResult candidate = bbinom_mle_work(x, n, tol, maxit, nan, beta_fixed,work);
      if (candidate.status != BbinomStatus::Ok) return bbinom_detail::retain_budget(candidate,out);
      out.stop_detail.candidates.push_back({{candidate.size,candidate.alpha,candidate.beta},candidate.logLik,candidate.logLik});
      if (candidate.logLik > best.logLik) { runner = best.logLik; best = candidate; }
      else runner = std::max(runner, candidate.logLik);
    }
    if (runner + 2e-8 * x.size() >= best.logLik) {
      best.status = BbinomStatus::ProfileTie; best.stop_detail=out.stop_detail;
    }
    return best;
  }
  if (std::isnan(size) && fix_a && fix_b) {
    if (!(alpha_fixed>0 && beta_fixed>0) || !std::isfinite(alpha_fixed+beta_fixed)) return out;
    int mx=0; for(int v:x) {if(v<0)return out;mx=std::max(mx,v);}
    // Each pmf is decreasing once size+1 >= x*(alpha+beta-1)/alpha.
    const double end=std::max(double(mx),std::ceil(mx*std::max(0.,alpha_fixed+beta_fixed-1)/alpha_fixed));
    const bool partial=end-mx+1>enumeration_budget(maxit);
    const double scan_end=std::min(end,mx+double(enumeration_budget(maxit))-1);
    out.logLik=-std::numeric_limits<double>::infinity();
    for (std::int64_t k=mx;k<=scan_end;++k) {
      auto candidate=bbinom_mle_work(x,k,tol,maxit,alpha_fixed,beta_fixed,work);
      if(candidate.status!=BbinomStatus::Ok)return bbinom_detail::retain_budget(candidate,out);
      if(candidate.logLik>out.logLik)out=candidate;
    }
    if(partial){out.certified=false;out.stop_detail=discrete_budget(out.stop_detail);}
    return out;
  }
  if (!std::isfinite(size) || size < 0 || size != std::floor(size) || size > 1e7) return out;

  if (fix_a && !(alpha_fixed > 0 && std::isfinite(alpha_fixed))) return out;
  if (fix_b && !(beta_fixed > 0 && std::isfinite(beta_fixed))) return out;

  if (fix_a && fix_b) {
    if(!work.consume(static_cast<std::int64_t>(size)*x.size())){out.status=BbinomStatus::SizeBudget;return out;}
    double ll=0;
    for(int v:x) {
      if(v<0)return out;
      if(v>size){out.status=BbinomStatus::SizeTooSmall;return out;}
      // For small shapes and large counts the gamma-ratio identity has
      // O(log size) terms, avoiding a million cancelling log increments.
      if(size>1024 && alpha_fixed+beta_fixed<16) {
        ll+=special::log_gamma_ratio(size,1,alpha_fixed+beta_fixed)
          +(v==0?special::lgamma(alpha_fixed):special::log_gamma_ratio(v,alpha_fixed,1))
          +(v==size?special::lgamma(beta_fixed):special::log_gamma_ratio(size-v,beta_fixed,1))
          -special::log_beta_positive(alpha_fixed,beta_fixed);
      } else {
        // Rising-factorial differences avoid cancellation of large shapes.
        double term=0,correction=0;
        const auto add=[&](double value) {
          const double y=value-correction,next=term+y;
          correction=(next-term)-y;term=next;
        };
        for(int j=0;j<v;++j) add(std::log((size-j)/(j+1.)) + std::log(alpha_fixed+j)-std::log(alpha_fixed+beta_fixed+j));
        for(int j=0;j<size-v;++j) add(std::log(beta_fixed+j)-std::log(alpha_fixed+beta_fixed+v+j));
        ll+=term;
      }
    }
    if(!std::isfinite(ll)){out.status=BbinomStatus::IterationFailure;return out;}
    return {size,alpha_fixed,beta_fixed,ll,0,BbinomStatus::Ok};
  }
  if(!work.consume(static_cast<std::int64_t>(size)*4)){out.status=BbinomStatus::SizeBudget;return out;}
  bbinom_detail::Data d(work); d.maxit=maxit;
  d.size = static_cast<int>(size);
  const int N = d.size;
  std::vector<double> counts(static_cast<std::size_t>(N) + 1, 0.0);
  for (int v : x) {
    if (v < 0) return out;
    if (v > N) { out.status = BbinomStatus::SizeTooSmall; return out; }
    counts[v] += 1;
  }
  d.n = static_cast<double>(x.size());
  d.zeros = counts[0];
  d.tops = counts[N];
  d.interior = N >= 1 ? d.n - d.zeros - d.tops : 0;
  const double lgN = special::lgamma(size + 1);
  long double reflected_squares=0, reflected_total=0;
  for (int v = 0; v <= N; ++v) {
    if (counts[v] == 0) continue;
    d.sum += counts[v] * v;
    d.squares += static_cast<long double>(counts[v])*v*v;
    d.total += static_cast<long double>(counts[v])*v;
    // Direct positive sums avoid cancellation when observations are near N.
    reflected_squares += static_cast<long double>(counts[v])*(N-v)*(N-v);
    reflected_total += static_cast<long double>(counts[v])*(N-v);
    if (v > 0 && v < N)
      d.log_choose += counts[v] * (lgN - special::lgamma(v + 1.0) - special::lgamma(N - v + 1.0));
  }
  if (fix_a) out.alpha = alpha_fixed;
  if (fix_b) out.beta = beta_fixed;
  if (N == 0) { out.logLik = 0; out.status = BbinomStatus::Unidentified; return out; }
  if (d.zeros == d.n || d.tops == d.n) {
    out.logLik = 0; out.status = BbinomStatus::PointMass; return out;
  }
  if (N == 1 && !fix_a && !fix_b) {
    out.logLik = d.tops * std::log(d.tops / d.n) + d.zeros * std::log(d.zeros / d.n);
    out.status = BbinomStatus::Unidentified;
    return out;
  }
  // Mirror x -> N - x when the mean exceeds N/2 (free fit) or beta is fixed,
  // so the probability solved for is at most about 1/2 and the free shape
  // in a fixed fit is always beta.
  const bool mirror = (fix_a || fix_b) ? fix_b : (d.sum > 0.5 * d.n * N);
  d.A.assign(N, 0.0);
  d.C.assign(N, 0.0);
  {
    // A_j = #{x > j}, C_j = #{x < N - j}; with mirroring, x -> N - x swaps them.
    std::vector<double> cum(static_cast<std::size_t>(N) + 2, 0.0);  // cum[k] = #{x < k}
    for (int v = 0; v <= N; ++v) cum[v + 1] = cum[v] + counts[v];
    for (int j = 0; j < N; ++j) {
      const double above = d.n - cum[j + 1];
      const double below = cum[N - j];
      d.A[j] = mirror ? below : above;
      d.C[j] = mirror ? above : below;
    }
    if (mirror) { std::swap(d.zeros, d.tops);
      d.squares=reflected_squares;
      d.total=reflected_total;
      d.sum=static_cast<double>(reflected_total); }
  }
  auto finish = [&](double a, double c, double value, int iterations, BbinomStatus status) {
    BbinomResult r{size, mirror ? c : a, mirror ? a : c, value + d.log_choose, iterations, status};
    return r;
  };

  if (fix_a || fix_b) {
    // Free shape c (beta after mirroring) with a held at the fixed value.
    const double a = fix_a ? alpha_fixed : beta_fixed;
    double inv = 0;
    for (int j = 0; j < N; ++j) inv += 1 / (a + j);
    const double c_lo = d.C[0] / (d.n * inv);
    const double c_hi = (d.n * N - d.sum) * (a + N) / d.sum;
    if(!(c_lo>0 && c_hi>0 && std::isfinite(c_lo) && std::isfinite(c_hi))) {
      out.status=BbinomStatus::IterationFailure;return out;
    }
    const double lo = std::log(c_lo) - 0.25, hi = std::log(c_hi) + 0.25;
    // Grid step 0.2 < 0.25: the end points lie where the sign of the score is known.
    const std::vector<double> u = bbinom_detail::log_grid(lo, hi, 0.2);
    auto deriv = [&](double uu, double &g, double &H) {
      const double c = std::exp(uu), t = a + c;
      bbinom_detail::shape_derivs(d, d.C, a, c, g, H);
      return bbinom_detail::loglik(d, a / t, c / t, 1 / t);
    };
    double best_u = 0;
    int iterations = 0;
    std::vector<bbinom_detail::Point> candidates;
    const double best = bbinom_detail::maximise_on_grid(u, tol, maxit, deriv, best_u, iterations,candidates);
    if (!std::isfinite(best)) {
      if(!candidates.empty()) {
        const auto& candidate=*std::max_element(candidates.begin(),candidates.end(),
          [](const bbinom_detail::Point& a,const bbinom_detail::Point& b){return a.value<b.value;});
        out=finish(a,candidate.s,candidate.value,iterations,BbinomStatus::IterationFailure);
        out.stop_detail.candidates.push_back({{out.size,out.alpha,out.beta},out.logLik,out.logLik});
      } else out.status=BbinomStatus::IterationFailure;
      return out;
    }
    std::vector<double> intervals;
    for (double uu : u) intervals.push_back(std::exp(uu));
    out=finish(a,std::exp(best_u),best,iterations,BbinomStatus::IterationFailure);
    out.stop_detail.candidates.push_back({{out.size,out.alpha,out.beta},out.logLik,out.logLik});
    bbinom_detail::Enclosure enclosure;
    if(!bbinom_detail::shape_enclosure(d,a,std::exp(best_u),tol,enclosure)) return out;
    const double margin=256*std::numeric_limits<double>::epsilon()*(1+std::abs(best)+d.n*N);
    for(const auto& candidate:candidates) {
      if((candidate.s<enclosure.lo || candidate.s>enclosure.hi) && candidate.value+margin>=best) {
        auto witness=finish(a,candidate.s,candidate.value,iterations,BbinomStatus::ProfileTie);
        out.stop_detail.candidates.push_back({{size,witness.alpha,witness.beta},witness.logLik,witness.logLik});
        out.stop_detail.comparison_tie=true;out.status=BbinomStatus::ProfileTie;return out;
      }
    }
    bool exhausted=false;
    if (!bbinom_detail::exclude_outside(intervals, enclosure, best-margin,
        [&](double l, double m, double h) { return bbinom_detail::shape_bound(d, a, l, m, h); },maxit,exhausted)) {
    if(exhausted)out.stop_detail=discrete_budget(out.stop_detail);
      out.status = BbinomStatus::IterationFailure;
      return out;
    }
    const double c=enclosure.shape;
    const double value=bbinom_detail::loglik(d,a/(a+c),c/(a+c),1/(a+c));
    if(!std::isfinite(value)){out.status=BbinomStatus::IterationFailure;return out;}
    return finish(a, c, value, iterations, BbinomStatus::Ok);
  }

  // Free fit, N >= 2, both 0 < xbar < N.
  const double face = bbinom_detail::binomial_face(d);
  const double tie = 1024 * std::numeric_limits<double>::epsilon() *
                     (1 + std::abs(face) + d.n * N);
  if (d.interior == 0) {
    // Endpoint-only: l increases strictly in s; supremum at the two-point law.
    out.logLik = d.tops * std::log(d.tops / d.n) + d.zeros * std::log(d.zeros / d.n);
    out.status = BbinomStatus::TwoPointLimit;
    return out;
  }
  // N=2 has two independent cell probabilities. When overdispersed,
  // the empirical frequencies themselves are a beta-binomial law, hence
  // attain the saturated multinomial upper bound (no search or tolerance).
  if(N==2) {
    const long double h=d.n, S=d.total, middle=d.interior;
    const long double left=4*static_cast<long double>(d.zeros)*d.tops;
    const long double right=middle*middle, delta=left-right;
    const long double margin=64*std::numeric_limits<long double>::epsilon()*(left+right);
    if(delta>margin) {
      const double a=static_cast<double>(S*middle/delta);
      const double b=static_cast<double>((2*h-S)*middle/delta);
      double value=0;
      for(double count:{d.zeros,d.interior,d.tops})
        if(count>0)value+=count*std::log(count/d.n);
      if(a>0 && b>0 && std::isfinite(a) && std::isfinite(b)) {
        const bool exact=left+right<std::ldexp(1.0L,std::numeric_limits<long double>::digits-2);
        const long double error=(exact?0:margin/delta)+8*std::numeric_limits<double>::epsilon();
        if(error<=tol)return finish(a,b,value-d.log_choose,0,BbinomStatus::Ok);
        auto failed=finish(a,b,value-d.log_choose,0,BbinomStatus::IterationFailure);
        failed.stop_detail.candidates.push_back({{size,failed.alpha,failed.beta},value,value});
        return failed;
      }
      out.status=BbinomStatus::IterationFailure;return out;
    }
  }
  out.logLik = face + d.log_choose;
  double harmonic = 0;
  for (int j = 1; j < N; ++j) harmonic += 1.0 / j;
  const double p0 = d.sum / (d.n * N);
  const double s_lo = 1e-12 * std::min(p0, 1 - p0) / (static_cast<double>(N) * N);
  const double s_hi = d.n * harmonic / d.interior;
  // The grid ends two steps beyond s_hi, where the profile score is negative.
  std::vector<double> u = bbinom_detail::log_grid(std::log(s_lo), std::log(s_hi) + 0.5, 0.25);
  double p_last = p0;
  auto deriv = [&](double uu, double &g, double &H) {
    const double s = std::exp(uu);
    p_last = bbinom_detail::solve_p(d, s, p_last);
    bbinom_detail::profile_derivs(d, s, p_last, g, H);
    return bbinom_detail::loglik(d, p_last, 1 - p_last, s);
  };
  double best_u = u[0];
  int iterations = 0;
  std::vector<bbinom_detail::Point> candidates;
  double best = bbinom_detail::maximise_on_grid(u, tol, maxit, deriv, best_u, iterations,candidates,p_last);
  if(best==-std::numeric_limits<double>::infinity()) {
    double g=0,H=0;const double value=deriv(u.front(),g,H);
    if(std::isfinite(value) && std::isfinite(g) && std::abs(g)<1e-3){best=value;best_u=u.front();}
  }
  if (std::isnan(best)) {
    if(!candidates.empty()) {
      const auto& candidate=*std::max_element(candidates.begin(),candidates.end(),
        [](const bbinom_detail::Point& a,const bbinom_detail::Point& b){return a.value<b.value;});
      if(candidate.p>0 && candidate.p<1) {
        out=finish(candidate.p/candidate.s,(1-candidate.p)/candidate.s,candidate.value,iterations,BbinomStatus::IterationFailure);
        out.stop_detail.candidates.push_back({{out.size,out.alpha,out.beta},out.logLik,out.logLik});
      }
    }
    out.status=BbinomStatus::IterationFailure;return out;
  }
  // A finite candidate must beat the face by a floating-point margin. A
  // near equality is a comparison stop, never a nonexistence declaration.
  const bool finite_wins=best>face+tie;
  const double neighbourhood=finite_wins?0:bbinom_detail::face_neighbourhood(d,d.squares);
  out.stop_detail=discrete_face(StopModel::Binom,{size,mirror?1-p0:p0},out.logLik);
  if(std::isfinite(best)) {
    const double bs=std::exp(best_u), bp=bbinom_detail::solve_p(d,bs,p0);
    if(!(std::isfinite(bs) && bs>0 && std::isfinite(bp) && bp>0 && bp<1)) {
      out.status=BbinomStatus::IterationFailure;return out;
    }
    out.stop_detail.candidates.push_back({{size,(mirror?1-bp:bp)/bs,(mirror?bp:1-bp)/bs},
                                        best+d.log_choose,best+d.log_choose});
  }
  if(!finite_wins && !(neighbourhood>0)) {
    out.stop_detail.comparison_tie=std::isfinite(best);
    out.status=std::isfinite(best)?BbinomStatus::ProfileTie:BbinomStatus::FaceUnresolved;return out;
  }
  std::vector<double> intervals{neighbourhood};
  for(double uu:u)if(std::exp(uu)>neighbourhood)intervals.push_back(std::exp(uu));
  bool exhausted=false;
  // For a face winner there is NO positive likelihood allowance. The
  // near-face signed expansion is strict for s>0; all remaining intervals
  // must lie strictly below the face, including the bound's rounding margin.
  bbinom_detail::Enclosure enclosure;
  if(finite_wins && !bbinom_detail::profile_enclosure(d,std::exp(best_u),tol,enclosure)) {
    out.status=BbinomStatus::IterationFailure;return out;
  }
  if(finite_wins)for(const auto& candidate:candidates) {
    if((candidate.s<enclosure.lo || candidate.s>enclosure.hi) && candidate.value+tie>=best) {
      const double cp=bbinom_detail::solve_p(d,candidate.s,p0);
      if(!std::isfinite(cp)){out.status=BbinomStatus::IterationFailure;return out;}
      auto witness=finish(cp/candidate.s,(1-cp)/candidate.s,candidate.value,iterations,BbinomStatus::ProfileTie);
      out.stop_detail.candidates.push_back({{size,witness.alpha,witness.beta},witness.logLik,witness.logLik});
      out.stop_detail.comparison_tie=true;out.status=BbinomStatus::ProfileTie;return out;
    }
  }
  const auto bound=[&](double l,double m,double h){return bbinom_detail::profile_bound(d,l,m,h);};
  const bool certified=finite_wins?
    bbinom_detail::exclude_outside(intervals,enclosure,best-tie,bound,maxit,exhausted):
    bbinom_detail::certify(intervals,face,-tie,bound,maxit,exhausted);
  if(!certified) {
    if(exhausted)out.stop_detail=discrete_budget(out.stop_detail);
    out.status=finite_wins?BbinomStatus::IterationFailure:BbinomStatus::FaceUnresolved;
    return out;
  }
  if(!finite_wins) {
    out.iterations=iterations;out.status=BbinomStatus::BinomialLimit;return out;
  }
  const double s = enclosure.s;
  const double p = enclosure.p;
  const double value = bbinom_detail::loglik(d, p, 1 - p, s);
  if(!(std::isfinite(p) && p>0 && p<1 && std::isfinite(value))) {
    out.status=BbinomStatus::IterationFailure;return out;
  }
  return finish(p / s, (1 - p) / s, value, iterations, BbinomStatus::Ok);

  }();
  if(stopped.status==BbinomStatus::PointMass) stopped.stop_detail=discrete_point(x[0]);
  if(stopped.status==BbinomStatus::TwoPointLimit) stopped.stop_detail=discrete_two_point(x,0,size);
  if(stopped.status==BbinomStatus::BinomialLimit) {
    double m=0;for(int v:x)m+=v;m/=x.size();const double p=m/size;double ll=0;
    for(int v:x)ll+=special::lgamma(size+1)-special::lgamma(v+1.)-special::lgamma(size-v+1)+v*std::log(p)+(size-v)*std::log1p(-p);
    stopped.stop_detail=discrete_face(StopModel::Binom,{size,p},ll);
  }
  if(stopped.status==BbinomStatus::Unidentified && !x.empty()) {
    double m=0;for(int v:x)m+=v;m/=x.size();
    const double N=std::isnan(size)?0:size;
    stopped.stop_detail=N==0?discrete_point(0):discrete_two_point(x,0,1);
  }
  if(stopped.status==BbinomStatus::SizeBudget || stopped.status==BbinomStatus::ProfileTie || stopped.status==BbinomStatus::FaceUnresolved || stopped.status==BbinomStatus::IterationFailure) {
    stopped.stop_detail.loglik_upper=INFINITY;
    for(const auto& c:stopped.stop_detail.candidates)
      if(std::isnan(stopped.stop_detail.loglik_lower) || c.loglik_lower>stopped.stop_detail.loglik_lower)
        stopped.stop_detail.loglik_lower=c.loglik_lower;
    if(std::isfinite(stopped.size) && std::isfinite(stopped.alpha) &&
       std::isfinite(stopped.beta) && std::isfinite(stopped.logLik))
      stopped.stop_detail.candidates.push_back({{stopped.size,stopped.alpha,stopped.beta},stopped.logLik,stopped.logLik});
  }
  if(work.exhausted){stopped.status=BbinomStatus::SizeBudget;stopped.stop_detail=discrete_budget(stopped.stop_detail);}
  if(stopped.status==BbinomStatus::SizeBudget || stopped.status==BbinomStatus::ProfileTie ||
     stopped.status==BbinomStatus::FaceUnresolved || stopped.status==BbinomStatus::IterationFailure) {
    bool found=false;
    for(const auto& c:stopped.stop_detail.candidates) {
      if(c.model!=StopModel::None || c.coef.size()!=3 || !std::isfinite(c.loglik_lower) ||
         !(c.coef[0]>=0 && c.coef[1]>0 && c.coef[2]>0) || !std::isfinite(c.coef[1]+c.coef[2]))continue;
      if(!found || c.loglik_lower>stopped.logLik) {
        stopped.size=c.coef[0];stopped.alpha=c.coef[1];stopped.beta=c.coef[2];
        stopped.logLik=c.loglik_lower;found=true;
      }
    }
    if(found){stopped.status=BbinomStatus::Ok;stopped.certified=false;}
  }
  return stopped;
}

inline BbinomResult bbinom_mle(ispan x,double size,double tol=1e-10,int maxit=500,
    double alpha_fixed=std::numeric_limits<double>::quiet_NaN(),
    double beta_fixed=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  const bbinom_detail::CellWork work(enumeration_budget(maxit));
  auto out=bbinom_mle_work(x,size,tol,maxit,alpha_fixed,beta_fixed,work);
  if(out.status==BbinomStatus::Ok && !out.certified) {
    if(!bbinom_detail::fallback_accuracy(x,out.size,out.alpha,out.beta,alpha_fixed,beta_fixed,tol,maxit)) {
      out.status=BbinomStatus::IterationFailure;out.accuracy_failure=true;return out;
    }
    // Evaluate only the returned point; certificate witnesses describe their
    // original points and retain their original likelihood bounds.
    if(std::isnan(alpha_fixed)||std::isnan(beta_fixed)) {
      const double total=out.alpha+out.beta;
      local_accuracy::JetSum ll(0);
      for(int v:x) {
        double term=special::lgamma(out.size+1)-special::lgamma(v+1.)-special::lgamma(out.size-v+1);
        for(int j=0;j<v;++j)term+=-std::log1p(out.beta/out.alpha)+std::log1p(j/out.alpha)-std::log1p(j/total);
        for(int j=0;j<out.size-v;++j)term+=-std::log1p(out.alpha/out.beta)+std::log1p(j/out.beta)-std::log1p((j+v)/total);
        ll.add(local_accuracy::Jet(term));
      }
      out.logLik=ll.value().v;
    }
  }
  return out;
}

}  // namespace cppunivariate
