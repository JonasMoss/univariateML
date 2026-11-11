// Where the held-scale Frechet cover spends its box evaluations (TASK-172).
// Regenerates the TASK-170 seeded samples (same seed and draw order as
// ../2026-11-10-gev-held-data/study.cpp), refits the Frechet samples with
// n >= 100 at each iterlim given on the command line, and reruns the cover
// for the first iterlim with a counting wrapper around the box evaluation.
// Columns of each tally: boxes, pruned by value, by nuisance score, by shape
// score, not smooth, surviving to the Taylor test or a split.
// Build and run from the repository root; see the report for commands.
#include "cppunivariate/special/special.hpp"
#include "cppunivariate/distributions/frechet/mle.hpp"
#include "cppunivariate/distributions/gev/dpqr.hpp"
#include <chrono>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <random>
#include <string>
#include <vector>

namespace cu = cppunivariate;
using Box = cu::gev_held_detail::Box;

struct Tally { long boxes = 0, value = 0, yscore = 0, xscore = 0, notok = 0, survive = 0; };

static void reconstruct(int id, std::vector<double> data, cu::FrechetResult r, int maxit) {
  const int n = static_cast<int>(data.size());
  const double origin = *std::min_element(data.begin(), data.end());
  double D = 0, k = 0;
  for (double& v : data) { v -= origin; D = std::max(D, v); if (v == 0) ++k; }
  const cu::span z{data.data(), data.size()};
  const double c = std::asinh(D / 2);
  const double start = std::max({2., 2 * n / c, 2 * std::sqrt(double(n)) / c});
  double upper = start;
  for (int i = 0; i < 100; ++i) {
    const double bound = n * std::log1p(upper) + std::max(-(upper + 1) * c, c - 1 - upper * upper * c * c / 2);
    if (bound < r.logLik - 1e-8 * (n + std::abs(r.logLik))) break;
    upper *= 2;
  }
  const Box domain{{1, upper}, {std::log(k / (2 * n)) - 1e-8, 1e-8}};
  const double incumbent = r.logLik / n, lam = r.lambda, le = std::log(origin - r.mu);
  std::map<int, Tally> by_lambda;   // floor(log2(lambda_lo))
  std::map<int, Tally> by_dist;     // floor(log10 of max relative distance to root)
  long points = 0;
  auto eval = [&](Box b) {
    auto e = cu::frechet_held_detail::evaluate(z, b);
    if (b.x.lo == b.x.hi && b.y.lo == b.y.hi) { ++points; return e; }
    const double dx = std::max({b.x.lo - lam, lam - b.x.hi, 0.}) / lam;
    const double dy = std::max({b.y.lo - le, le - b.y.hi, 0.});
    const double d = std::max(dx, dy);
    const int bin = d > 0 ? static_cast<int>(std::floor(std::log10(d))) : -99;
    for (Tally* t : {&by_lambda[static_cast<int>(std::floor(std::log2(b.x.lo)))], &by_dist[bin]}) {
      ++t->boxes;
      if (e.upper < incumbent - 1e-12) ++t->value;
      else if (!e.ok) ++t->notok;
      else if (e.jet.g[1].lo > 0 || e.jet.g[1].hi < 0) ++t->yscore;
      else if ((e.jet.g[0].lo > 0 && b.x.hi < domain.x.hi) || (e.jet.g[0].hi < 0 && b.x.lo > domain.x.lo)) ++t->xscore;
      else ++t->survive;
    }
    return e;
  };
  auto precise = [](Box b, double t) {
    return b.x.hi - b.x.lo <= t * .25 * (b.x.lo + b.x.hi) / 2 && std::expm1(b.y.hi - b.y.lo) <= t * .25;
  };
  const auto res = cu::gev_held_detail::cover(domain, lam, le, 1e-10, maxit, eval, precise);
  std::printf("  cover maxit=%d: certified=%d contradicted=%d evaluations=%lld points=%ld\n", maxit,
              int(res.certified), int(res.contradicted), static_cast<long long>(res.evaluations), points);
  std::printf("  D=%.4g c=%.4g k=%g tail start=%.4g tail cutoff=%.4g  lambda_hat=%.6g log e_hat=%.6g  log e domain lower=%.4g\n",
              D, c, k, start, upper, lam, le, domain.y.lo);
  std::printf("  boxes by lambda_lo octave: [boxes value yscore xscore notok survive]\n");
  for (auto& kv : by_lambda)
    std::printf("    lambda>=%-6g %7ld %7ld %7ld %7ld %7ld %7ld\n", std::ldexp(1., kv.first), kv.second.boxes,
                kv.second.value, kv.second.yscore, kv.second.xscore, kv.second.notok, kv.second.survive);
  std::printf("  boxes by distance to the root (max of relative lambda gap, log-e gap), decade:\n");
  for (auto& kv : by_dist)
    std::printf("    %-10s %7ld %7ld %7ld %7ld %7ld %7ld\n",
                kv.first == -99 ? "contains" : ("1e" + std::to_string(kv.first)).c_str(), kv.second.boxes,
                kv.second.value, kv.second.yscore, kv.second.xscore, kv.second.notok, kv.second.survive);
  (void)id;
}

int main(int argc, char** argv) {
  std::vector<int> maxits;
  for (int i = 1; i < argc; ++i) maxits.push_back(std::atoi(argv[i]));
  if (maxits.empty()) maxits = {100};
  std::mt19937_64 rng(17020261110ULL);
  std::uniform_real_distribution<double> uniform(0, 1);
  int sample_id = 0;
  for (double shape : {-.9, -.5, 0., .5, 1., 2.})
    for (int n : {3, 10, 50, 100, 500})
      for (int rep = 0; rep < 2; ++rep) {
        ++sample_id;
        for (int i = 0; i < n; ++i) (void)cu::distributions::qgev(uniform(rng), 0, 1, shape, true, false);
        if (shape > 0 || shape == 0) {
          const int id = sample_id++;
          const double lambda = shape > 0 ? 1 / shape : 2;
          std::vector<double> f;
          for (int i = 0; i < n; ++i) f.push_back(std::pow(-std::log(uniform(rng)), -1 / lambda));
          if (n < 100) continue;
          cu::FrechetResult first{};
          bool need = false;
          for (int maxit : maxits) {
            const auto t0 = std::chrono::steady_clock::now();
            const auto r = cu::frechet_mle({f.data(), f.size()}, 1e-10, maxit, NAN, NAN, 1);
            const double ms = std::chrono::duration<double, std::milli>(std::chrono::steady_clock::now() - t0).count();
            std::printf("sample %d gen_lambda=%g n=%d maxit=%d status=%d certified=%d ms=%.1f loglik=%.17g lambda=%.12g mu=%.12g restricted=%u\n",
                        id, lambda, n, maxit, int(r.status), int(r.certified), ms, r.logLik, r.lambda, r.mu, r.restricted);
            std::fflush(stdout);
            if (maxit == maxits.front()) { first = r; need = !r.certified && r.status == cu::FrechetStatus::Ok; }
          }
          if (need || n >= 100) { reconstruct(id, f, first, maxits.front()); std::fflush(stdout); }
        }
      }
}
