// dweibull: golden, score gate, escapes from limit-law data, held
// subsets, scale (n = 1e6; many distinct counts), stable d/p/q/r.

#include "test_support.hpp"

#include "cppunivariate/core/finite_diff.hpp"
#include "cppunivariate/distributions/dweibull/dpqr.hpp"
#include "cppunivariate/distributions/dweibull/mle.hpp"
#include "cppunivariate/distributions/rng_std.hpp"

#include <cmath>
#include <limits>
#include <vector>

namespace {

constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();

double dweibull_loglik(const std::vector<double> &x, double q, double b) {
  double ll = 0;
  for (double v : x) ll += cppunivariate::distributions::ddweibull(v, q, b, true);
  return ll;
}

// Negative log-likelihood in (shape1, shape2) for the score gate.
struct DweibullCost {
  const std::vector<double> *x;
  int dim() const { return 2; }
  double operator()(const double *theta) const {
    return -dweibull_loglik(*x, theta[0], theta[1]);
  }
};

double score_norm(const std::vector<double> &x, double q, double b) {
  DweibullCost cost{&x};
  const double theta[2] = {q, b};
  double g[2];
  cppunivariate::finite_diff_gradient(cost, theta, g);
  return std::sqrt(g[0] * g[0] + g[1] * g[1]);
}

std::vector<double> sample(std::size_t n, double q, double b, unsigned seed) {
  cppunivariate::distributions::StdRng<> rng(seed);
  std::vector<double> x(n);
  for (double &v : x) v = cppunivariate::distributions::rdweibull(rng, q, b);
  return x;
}

cppunivariate::DweibullResult fit(const std::vector<double> &x, double q = kNaN,
                                  double b = kNaN) {
  return cppunivariate::dweibull_mle({x.data(), x.size()}, 1e-12, 200, q, b);
}

std::vector<double> repeat(std::vector<std::pair<double, int>> cells) {
  std::vector<double> x;
  for (const auto &c : cells) x.insert(x.end(), c.second, c.first);
  return x;
}

}  // namespace

TEST_CASE("dweibull: golden and score gate") {
  const auto g = mltest::load_distribution_golden("dweibull");
  const auto r = fit(g.data);
  REQUIRE(r.status == cppunivariate::DweibullStatus::Ok);
  CHECK(r.shape1 == doctest::Approx(g.estimates.at("shape1")).epsilon(mltest::kGolden_dweibull).scale(0));
  CHECK(r.shape2 == doctest::Approx(g.estimates.at("shape2")).epsilon(mltest::kGolden_dweibull).scale(0));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_dweibull).scale(0));
  CHECK(r.logLik == doctest::Approx(dweibull_loglik(g.data, r.shape1, r.shape2)).epsilon(1e-13));
  CHECK(score_norm(g.data, r.shape1, r.shape2) < 1e-3);
}

TEST_CASE("dweibull: held subsets solve their own score equations") {
  const auto g = mltest::load_distribution_golden("dweibull");
  const auto free = fit(g.data);
  // shape2 held: a strictly concave scalar problem in shape1.
  const auto hb = fit(g.data, kNaN, 2.5);
  REQUIRE(hb.status == cppunivariate::DweibullStatus::Ok);
  CHECK(hb.shape2 == 2.5);
  const double h = 1e-6;
  CHECK(std::abs(dweibull_loglik(g.data, hb.shape1 + h, 2.5) -
                 dweibull_loglik(g.data, hb.shape1 - h, 2.5)) / (2 * h) < 1e-3);
  CHECK(hb.logLik < free.logLik);
  // shape1 held: a strictly concave scalar problem in shape2.
  const auto hq = fit(g.data, 0.6);
  REQUIRE(hq.status == cppunivariate::DweibullStatus::Ok);
  CHECK(hq.shape1 == 0.6);
  CHECK(std::abs(dweibull_loglik(g.data, 0.6, hq.shape2 + h) -
                 dweibull_loglik(g.data, 0.6, hq.shape2 - h)) / (2 * h) < 1e-3);
  CHECK(hq.logLik < free.logLik);
  // Holding both evaluates; holding the free optimum reproduces it.
  const auto both = fit(g.data, free.shape1, free.shape2);
  CHECK(both.logLik == doctest::Approx(free.logLik).epsilon(1e-13));
  CHECK(fit(g.data, free.shape1).shape2 == doctest::Approx(free.shape2).epsilon(1e-8));
  CHECK(fit(g.data, kNaN, free.shape2).shape1 == doctest::Approx(free.shape1).epsilon(1e-8));
}

TEST_CASE("dweibull: shape2 = 1 is the attained geometric member") {
  const std::vector<double> x{0, 0, 1, 2, 2, 3, 5, 8};
  double mean = 0;
  for (double v : x) mean += v / x.size();
  const auto r = fit(x, kNaN, 1);
  REQUIRE(r.status == cppunivariate::DweibullStatus::Ok);
  CHECK(r.shape1 == doctest::Approx(mean / (1 + mean)).epsilon(1e-13));
  double geom = 0;
  for (double v : x) geom += std::log(1 / (1 + mean)) + v * std::log(mean / (1 + mean));
  CHECK(r.logLik == doctest::Approx(geom).epsilon(1e-13));
}

TEST_CASE("dweibull: escapes are reported from data of the limit laws") {
  // Point masses.
  CHECK(fit({4, 4, 4, 4}).status == cppunivariate::DweibullStatus::PointMass);
  CHECK(fit({0, 0, 0}).status == cppunivariate::DweibullStatus::PointMass);
  CHECK(fit({0, 0, 0}, kNaN, 2).status == cppunivariate::DweibullStatus::PointMass);
  // Bernoulli (binomial size 1) and shifted adjacent two-point laws.
  cppunivariate::distributions::StdRng<> rng(11);
  std::vector<double> bern, two;
  for (int i = 0; i < 400; ++i) {
    bern.push_back(rng.runif(0, 1) < 0.3 ? 1 : 0);
    two.push_back(rng.runif(0, 1) < 0.7 ? 4 : 3);
  }
  CHECK(fit(bern).status == cppunivariate::DweibullStatus::BernoulliLimit);
  CHECK(fit(two).status == cppunivariate::DweibullStatus::TwoPointLimit);
  // shape1 held: values in {0, 1} send shape2 to infinity; all zeros are flat.
  CHECK(fit(bern, 0.4).status == cppunivariate::DweibullStatus::HeldBernoulliLimit);
  CHECK(fit({1, 1, 1}, 0.4).status == cppunivariate::DweibullStatus::HeldBernoulliLimit);
  const auto flat = fit({0, 0, 0}, 0.4);
  CHECK(flat.status == cppunivariate::DweibullStatus::ShapeUnidentified);
  CHECK(flat.shape2 == 1);
  CHECK(flat.logLik == doctest::Approx(3 * std::log(0.6)).epsilon(1e-14));
  // Held subsets without those faces fit constant and range-one samples.
  CHECK(fit({4, 4, 4}, 0.4).status == cppunivariate::DweibullStatus::Ok);
  CHECK(fit({4, 4, 4}, kNaN, 2).status == cppunivariate::DweibullStatus::Ok);
  CHECK(fit(two, kNaN, 2).status == cppunivariate::DweibullStatus::Ok);
  CHECK(fit(two, 0.9).status == cppunivariate::DweibullStatus::Ok);
  CHECK(fit({0, 2}).status == cppunivariate::DweibullStatus::Ok);
}

TEST_CASE("dweibull: the two-point face is approached from below") {
  // x: 30 threes, 70 fours. Along shape2 = b with t*4^b = h, exp(-h) = 0.7,
  // the log-likelihood rises to the saturated value. Evaluated in
  // alpha = log t, since shape1 = exp(-t) rounds to 1 beyond b ~ 30.
  const auto x = repeat({{3, 30}, {4, 70}});
  const double sup = 30 * std::log(0.3) + 70 * std::log(0.7);
  cppunivariate::dweibull_detail::Problem P;
  REQUIRE(P.tabulate({x.data(), x.size()}));
  P.centre(0);
  double prev = -std::numeric_limits<double>::infinity();
  for (double b = 2; b <= 512; b *= 2) {
    P.set_beta(b);
    const double ll = cppunivariate::dweibull_detail::loglik(
        P, std::log(-std::log(0.7)) - b * std::log(4.0));
    // Forming alpha + beta*log(k) cancels terms of order beta;
    // at beta=512 this introduces roundoff in the saturated plateau.
    CHECK(ll >= prev - 1e-10);
    CHECK(ll <= sup + 1e-10);
    prev = ll;
  }
  CHECK(std::abs(sup - prev) < 1e-10);
}

TEST_CASE("dweibull: invalid input and controls") {
  CHECK(fit({}).status == cppunivariate::DweibullStatus::InvalidData);
  CHECK(fit({0, 1, -1}).status == cppunivariate::DweibullStatus::InvalidData);
  CHECK(fit({0, 1.5, 3}).status == cppunivariate::DweibullStatus::InvalidData);
  CHECK(fit({0, kNaN, 3}).status == cppunivariate::DweibullStatus::InvalidData);
  CHECK(fit({0, 1, 3}, 1.0).status == cppunivariate::DweibullStatus::InvalidControl);
  CHECK(fit({0, 1, 3}, kNaN, 0.0).status == cppunivariate::DweibullStatus::InvalidControl);
  const std::vector<double> x{0, 1, 3};
  CHECK(cppunivariate::dweibull_mle({x.data(), x.size()},
        std::numeric_limits<double>::infinity(), 200).status ==
        cppunivariate::DweibullStatus::InvalidControl);
  CHECK(cppunivariate::dweibull_mle({x.data(), x.size()}, 0, 200).status ==
        cppunivariate::DweibullStatus::InvalidControl);
}

TEST_CASE("dweibull: n = 1e6 with few distinct counts, and many distinct counts") {
  const auto big = sample(1000000, 0.8, 1.5, 7);
  const auto r = fit(big);
  REQUIRE(r.status == cppunivariate::DweibullStatus::Ok);
  CHECK(std::abs(r.shape1 - 0.8) < 0.003);
  CHECK(std::abs(r.shape2 - 1.5) < 0.01);
  // Heavy tail: thousands of distinct counts up to ~1e7.
  const auto heavy = sample(20000, 0.9, 0.3, 8);
  const auto h = fit(heavy);
  REQUIRE(h.status == cppunivariate::DweibullStatus::Ok);
  CHECK(std::abs(h.shape2 - 0.3) < 0.02);
  // The shared sqrt(epsilon) step loses precision when subtracting two
  // sums of 2e4 log masses. Use a central-difference step suited to this
  // sample, retaining the same absolute score gate.
  const double step = 2e-6;
  const double sq = (dweibull_loglik(heavy, h.shape1 + step, h.shape2) -
                     dweibull_loglik(heavy, h.shape1 - step, h.shape2)) / (2 * step);
  const double sb = (dweibull_loglik(heavy, h.shape1, h.shape2 + step) -
                     dweibull_loglik(heavy, h.shape1, h.shape2 - step)) / (2 * step);
  CHECK(std::hypot(sq, sb) < 1e-2);
  for (double dq : {-1e-4, 1e-4}) {
    for (double db : {-1e-4, 0.0, 1e-4}) {
      CHECK(dweibull_loglik(heavy, h.shape1 + dq, h.shape2 + db) < h.logLik);
    }
  }
}

TEST_CASE("dweibull: peaked counts near the double-precision limit of shape1") {
  // Scale 40, shape 8: -log(shape1) ~ 1e-13 is resolved to ~1e-3 by the
  // double nearest 1; shape2 is refitted with that shape1 held.
  const auto peaked = sample(5000, std::exp(-std::pow(40.0, -8.0)), 8, 9);
  const auto r = fit(peaked);
  REQUIRE(r.status == cppunivariate::DweibullStatus::Ok);
  CHECK(r.logLik == doctest::Approx(dweibull_loglik(peaked, r.shape1, r.shape2)).epsilon(1e-12));
  const double h = 1e-6;
  CHECK(std::abs(dweibull_loglik(peaked, r.shape1, r.shape2 + h) -
                 dweibull_loglik(peaked, r.shape1, r.shape2 - h)) / (2 * h) < 1e-3);
  // Shape 12 at scale 40: shape1 rounds to 1 and cannot be returned.
  cppunivariate::distributions::StdRng<> rng(10);
  std::vector<double> sharper;
  for (int i = 0; i < 5000; ++i) {
    sharper.push_back(std::floor(40 * std::pow(-std::log(rng.runif(0, 1)), 1.0 / 12)));
  }
  CHECK(fit(sharper).status == cppunivariate::DweibullStatus::Unrepresentable);
}

TEST_CASE("dweibull dpqr: stable log pmf, floored CDF, generalized inverse") {
  namespace d = cppunivariate::distributions;
  CHECK(d::ddweibull(1000, 0.999, 2, true) == doctest::Approx(-1000.6454341855).epsilon(1e-11));
  CHECK(d::ddweibull(4, 0.7, 1.3, false) ==
        doctest::Approx(std::pow(0.7, std::pow(4.0, 1.3)) - std::pow(0.7, std::pow(5.0, 1.3))));
  CHECK(d::ddweibull(0, 0.7, 1, false) == doctest::Approx(0.3));
  CHECK(d::ddweibull(0.5, 0.7, 1, false) == 0);
  CHECK(d::pdweibull(0.5, 0.7, 1, true, false) == d::pdweibull(0, 0.7, 1, true, false));
  CHECK(d::pdweibull(0, 0.7, 1, true, false) == doctest::Approx(0.3));
  CHECK(d::pdweibull(-0.5, 0.7, 1, true, false) == 0);
  CHECK(d::pdweibull(2.9, 0.7, 1.3, false, true) ==
        doctest::Approx(-(-std::log(0.7)) * std::pow(3.0, 1.3)));
  double cdf = 0;
  for (int k = 0; k <= 30; ++k) {
    cdf += d::ddweibull(k, 0.8, 1.5, false);
    CHECK(cdf == doctest::Approx(d::pdweibull(k, 0.8, 1.5, true, false)).epsilon(1e-12));
  }
  for (double p : {1e-12, 0.001, 0.1, 0.5, 0.9, 0.999}) {
    for (double b : {0.05, 0.7, 3.0}) {
      const double k = d::qdweibull(p, 0.6, b, true, false);
      CHECK(k >= 0);
      CHECK(d::pdweibull(k, 0.6, b, true, false) >= p * (1 - 1e-12));
      // Different supplied scales have different relative bands; each
      // answer must be its own first adjusted crossing, even below 2^53.
      for (bool lower : {false,true}) for (bool log : {false,true}) {
        const double target=log ? std::log(lower ? p : 1-p) : lower ? p : 1-p;
        CHECK(mltest::truth_lattice_quantile(d::qdweibull(target,.6,b,lower,log),target,lower,
            [&](double x) {return d::pdweibull(x,.6,b,lower,log);},false,true,log));
      }
    }
  }
  CHECK(std::isinf(d::qdweibull(1, 0.6, 1, true, false)));
  CHECK(std::isnan(d::ddweibull(1, 1.0, 1, false)));
  const auto small = sample(100000, 0.9, 0.05, 12);
  double minimum = 1;
  for (double v : small) minimum = std::min(minimum, v);
  CHECK(minimum == 0);
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("dweibull: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("dweibull",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::ddweibull(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pdweibull(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qdweibull(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rdweibull(rng, p[0], p[1]); });
}

TEST_CASE("dweibull: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("dweibull");
  const auto x=mltest::as_span(g.data);
  CHECK(cppunivariate::dweibull_mle(x,1e-8,1).status==cppunivariate::DweibullStatus::IterationFailure);
}

// shape2=1 has geometric survival shape1^(k+1).
TEST_CASE("dweibull: geometric member allows adjacent answers at jumps") {
  const auto adjacent=[](double x,double lo) {return x==lo || x==lo+1;};
  CHECK(cppunivariate::distributions::qdweibull(.83,.5,1,true,false)==2);
  CHECK(cppunivariate::distributions::qdweibull(.17,.5,1,false,false)==2);

  CHECK(adjacent(cppunivariate::distributions::qdweibull(-0.6931471805599453,.5,1,true,true),0));
  CHECK(adjacent(cppunivariate::distributions::qdweibull(.5,.5,1,true,false),0));
  CHECK(adjacent(cppunivariate::distributions::qdweibull(.25,.5,1,false,false),1));
  CHECK(adjacent(cppunivariate::distributions::qdweibull(-1.3862943611198906,.5,1,false,true),1));
}

TEST_CASE("dweibull: structured stop payloads") {
  const std::vector<double> x{1,1};
  const auto r=cppunivariate::dweibull_mle({x.data(),x.size()});
  REQUIRE(r.stop_detail.limit_coef.size()==1);
  CHECK(r.stop_detail.limit_model==cppunivariate::StopModel::PointMass);
  CHECK(r.stop_detail.limit_coef[0]==1);
  CHECK(r.stop_detail.limit_loglik==0);
  CHECK(r.stop_detail.candidates.size()==1);
}

TEST_CASE("dweibull: translated adjacent limit retains original support") {
  const std::vector<double> x{2,2,3};
  const auto r=cppunivariate::dweibull_mle({x.data(),x.size()});
  REQUIRE(r.status==cppunivariate::DweibullStatus::TwoPointLimit);
  REQUIRE(r.stop_detail.limit_coef.size()==3);
  CHECK(r.stop_detail.limit_coef[0]==2);
  CHECK(r.stop_detail.limit_coef[1]==3);
  CHECK(r.stop_detail.limit_coef[2]==doctest::Approx(1./3));
  CHECK(r.stop_detail.limit_loglik==doctest::Approx(2*std::log(2./3)+std::log(1./3)));
}

TEST_CASE("dweibull: held and fitted Bernoulli faces") {
  const std::vector<double> x{0,1,1};
  const auto f=cppunivariate::dweibull_mle({x.data(),x.size()});
  REQUIRE(f.status==cppunivariate::DweibullStatus::BernoulliLimit);
  REQUIRE(f.stop_detail.limit_coef.size()==2);
  CHECK(f.stop_detail.limit_coef[1]==doctest::Approx(2./3));
  const auto h=cppunivariate::dweibull_mle({x.data(),x.size()},1e-12,200,.4);
  REQUIRE(h.status==cppunivariate::DweibullStatus::HeldBernoulliLimit);
  CHECK(h.stop_detail.limit_coef[1]==.4);
  CHECK(h.stop_detail.limit_loglik==doctest::Approx(std::log(.6)+2*std::log(.4)));
}

TEST_CASE("dweibull quantiles above 2^53 are minimal representable doubles") {
  for (bool lower : {false,true}) for (bool log : {false,true}) {
    const double u=lower ? .75 : .25;
    const double p=log ? std::log(u) : u;
    const double h=cppunivariate::distributions::qdweibull(p,.5,.01,lower,log);
    REQUIRE(std::isfinite(h));
    REQUIRE(std::abs(h)>9007199254740992.0);
    const double at=cppunivariate::distributions::pdweibull(h,.5,.01,lower,log);
    const double before=cppunivariate::distributions::pdweibull(std::nextafter(h,-INFINITY),.5,.01,lower,log);
    CHECK(cppunivariate::special::discrete_reached(at,p,lower,log)==1);
    CHECK(cppunivariate::special::discrete_reached(before,p,lower,log)==0);
  }
}

TEST_CASE("dweibull: rounded seeds cannot bypass inversion at 2^53") {
  for (double k : {9007199254740990.0,9007199254740992.0,9007199254740994.0}) {
    for (bool lower : {false,true}) for (bool log : {false,true}) {
      const double at=cppunivariate::distributions::pdweibull(k,.5,.05,lower,log);
      const double p=std::nextafter(at,lower ? INFINITY : -INFINITY);
      const double edge=cppunivariate::distributions::pdweibull(9007199254740992.0,.5,.05,lower,log);
      if (lower ? edge>=p : edge<=p) continue;
      const double h=cppunivariate::distributions::qdweibull(p,.5,.05,lower,log);
      REQUIRE(std::isfinite(h));
      CHECK(mltest::truth_lattice_quantile(h,p,lower,[&](double x) {
        return cppunivariate::distributions::pdweibull(x,.5,.05,lower,log);
      },false,true,log));
    }
  }
}

TEST_CASE("dweibull: extreme lattice hazards avoid power overflow") {
  const double h=-std::log(.5)*std::exp(.001*std::log(1e308));
  CHECK(cppunivariate::distributions::pdweibull(1e308,.5,.001,false,true)
        ==doctest::Approx(-h).epsilon(2e-14));
  CHECK(cppunivariate::distributions::pdweibull(1e308,.5,.001,true,false)
        ==doctest::Approx(-std::expm1(-h)).epsilon(2e-14));
  CHECK(cppunivariate::distributions::pdweibull(1e308,.5,2,true,false)==1);
  CHECK(cppunivariate::distributions::pdweibull(1e308,.5,2,false,false)==0);
}

TEST_CASE("dweibull: slow reference discrete quantile round trips on every truth point") {
  mltest::discrete_roundtrips("dweibull",
    [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::pdweibull(x,t[0], t[1],l,g);},
    [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::qdweibull(x,t[0], t[1],l,g);});
}

TEST_CASE("dweibull: large inverse samples retain the unfuzzed log crossing") {
  struct FixedRng {double runif(double,double) {return .5;}} rng;
  const double draw=cppunivariate::distributions::rdweibull(rng,.9,.05);
  const double target=std::log(.5);
  CHECK(draw>9007199254740992.);
  CHECK(cppunivariate::distributions::pdweibull(draw,.9,.05,false,true)<=target);
  CHECK(cppunivariate::distributions::pdweibull(std::nextafter(draw,-INFINITY),.9,.05,false,true)>target);
  CHECK(draw>cppunivariate::distributions::qdweibull(target,.9,.05,false,true));
}
