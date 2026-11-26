#include <algorithm>
#include "test_support.hpp"
#include "cppunivariate/distributions/gamma/mle.hpp"
#include "cppunivariate/distributions/lgamma/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <string>
#include <vector>

TEST_CASE("lgamma (wrapper-only: gamma on log(x))") {
  auto g = mltest::load_distribution_golden("lgamma");
  // Replicate mllgamma_ wrapper: y = log(x); shapelog/ratelog from gamma_mle;
  // logLik recomputed with the log-gamma density.
  std::vector<double> y;
  y.reserve(g.data.size());
  for (double v : g.data) y.push_back(std::log(v));
  auto r = cppunivariate::gamma_mle(mltest::as_span(y), 1e-10, mltest::kFitMaxit);
  const double rate = 1.0 / r.scale;
  CHECK(r.shape == doctest::Approx(g.estimates.at("shapelog")).epsilon(mltest::kGolden_lgamma).scale(0));
  CHECK(rate == doctest::Approx(g.estimates.at("ratelog")).epsilon(mltest::kGolden_lgamma).scale(0));
  // logLik for log-gamma is recomputed by the wrapper; match here.
  const double dn = static_cast<double>(g.data.size());
  double sum_y = 0.0, sum_log_y = 0.0;
  for (double v : g.data) {
    const double ly = std::log(v);
    sum_y += ly;
    sum_log_y += std::log(ly);
  }
  const double L = sum_y / dn;
  const double K = sum_log_y / dn;
  const double logLik =
      dn * (r.shape * std::log(rate) - std::lgamma(r.shape) +
            (r.shape - 1.0) * K - (rate + 1.0) * L);
  CHECK(logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_lgamma).scale(0));
}


// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("lgamma: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("lgamma",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dlgamma(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::plgamma(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qlgamma(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rlgamma(rng, p[0], p[1]); });
}

TEST_CASE("lgamma P2 every held subset") {
  const auto g=mltest::load_distribution_golden("lgamma");
  std::vector<double> x=g.data;
  for(auto &v:x)v=std::log(v);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  // held[1] is ratelog; gamma_mle's 5th argument is a scale (1/rate).
  const std::vector<double> held{g.estimates.at("shapelog"),g.estimates.at("ratelog")};
  { const auto r=cppunivariate::gamma_mle(mltest::as_span(x),1e-10,200,held[0],nan);
    CHECK(r.shape==held[0]);
    CHECK(std::isfinite(r.scale));
    CHECK(std::isfinite(r.logLik));
    double jac=0;for(double v:x)jac+=v;
    CHECK(r.logLik-jac>=g.logLik-1e-6*x.size());
  }
  { const auto r=cppunivariate::gamma_mle(mltest::as_span(x),1e-10,200,nan,1.0/held[1]);
    CHECK(std::isfinite(r.shape));
    CHECK(r.scale==1.0/held[1]);
    CHECK(std::isfinite(r.logLik));
    double jac=0;for(double v:x)jac+=v;
    CHECK(r.logLik-jac>=g.logLik-1e-6*x.size());
  }
  { const auto r=cppunivariate::gamma_mle(mltest::as_span(x),1e-10,200,held[0],1.0/held[1]);
    CHECK(r.shape==held[0]);
    CHECK(r.scale==1.0/held[1]);
    CHECK(std::isfinite(r.logLik));
    double jac=0;for(double v:x)jac+=v;
    CHECK(r.logLik-jac>=g.logLik-1e-6*x.size());
  }
}


// mpmath generator: import mpmath as mp; mp.mp.dps=100
// P=lambda z:mp.gammainc(2,0,z)/mp.gamma(2)
// Q=lambda z:mp.gammainc(2,z,mp.inf)/mp.gamma(2)
// lo=mp.exp(mp.findroot(lambda y:mp.log(P(mp.exp(y)))+800,-mp.mpf(800)/2))
// hi=mp.findroot(lambda z:mp.log(Q(z))+800,800)
// [mp.exp(lo/3),mp.exp(hi/3)]
TEST_CASE("lgamma: logarithmic quantile regression") {
  CHECK(cppunivariate::distributions::qlgamma(-800.0, 2.0, 3.0, true, true) == doctest::Approx(1.0).epsilon(1e-11));
  CHECK(cppunivariate::distributions::qlgamma(-800.0, 2.0, 3.0, false, true) == doctest::Approx(6.038709799216898e+116).epsilon(1e-11));
}

TEST_CASE("lgamma: support endpoints on log input") {
  CHECK(cppunivariate::distributions::qlgamma(cppunivariate::special::kNegInf, 2.0, 3.0, true, true) == 1.0);
  CHECK(cppunivariate::distributions::qlgamma(0.0, 2.0, 3.0, false, true) == 1.0);
  CHECK(cppunivariate::distributions::qlgamma(cppunivariate::special::kNegInf, 2.0, 3.0, false, true) == cppunivariate::special::kPosInf);
  CHECK(cppunivariate::distributions::qlgamma(0.0, 2.0, 3.0, true, true) == cppunivariate::special::kPosInf);
}

// At x=mp.mpf(float(1e100)), evaluate the same mpmath tails and
// the README log density; constants are also in the independent truth table.
TEST_CASE("lgamma: extreme observation log tails and density") {
  CHECK(cppunivariate::distributions::dlgamma(1e100, 2.0, 3.0, true) == doctest::Approx(-913.397609989046).epsilon(1e-11));
  CHECK(cppunivariate::distributions::plgamma(1e100, 2.0, 3.0, true, true) == doctest::Approx(0.0).epsilon(1e-11));
  CHECK(cppunivariate::distributions::plgamma(1e100, 2.0, 3.0, false, true) == doctest::Approx(-684.2362663768691).epsilon(1e-11));
}

#include "cppunivariate/distributions/lgamma/mle.hpp"
TEST_CASE("lgamma core matches previous adapter for every held subset") {
  const auto g=mltest::load_distribution_golden("lgamma");
  std::vector<double> y;
  double jac=0;
  for(double v:g.data) {y.push_back(std::log(v));jac+=y.back();}
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(int mask=0;mask<4;++mask) {
    const double shape=(mask&1) ? g.estimates.at("shapelog") : nan;
    const double rate=(mask&2) ? g.estimates.at("ratelog") : nan;
    const auto old=cppunivariate::gamma_mle(mltest::as_span(y),1e-10,100,shape,(mask&2)?1/rate:nan);
    const auto r=cppunivariate::lgamma_mle(mltest::as_span(g.data),1e-10,100,shape,rate);
    CHECK(r.status==cppunivariate::LgammaStatus::Ok);
    CHECK(r.shapelog==old.shape);
    CHECK(r.ratelog==((mask&2)?rate:1/old.scale));
    CHECK(r.logLik==old.logLik-jac);
    CHECK(r.iterations==old.iterations);
  }
  const auto failed=cppunivariate::lgamma_mle(mltest::as_span(g.data),1e-15,1,nan,2);
  CHECK(failed.status==cppunivariate::LgammaStatus::IterationFailure);
  const std::vector<double> ties{2,2};
  CHECK(cppunivariate::lgamma_mle(mltest::as_span(ties),1e-8,100).status==cppunivariate::LgammaStatus::Degenerate);
}

TEST_CASE("lgamma: TASK-132 empty and invalid delegates preserve non-success") {
  for (const auto& data : {std::vector<double>{},std::vector<double>{NAN,2},std::vector<double>{-1,2}}) {
    const cppunivariate::span x{data.data(),data.size()};
    CHECK(cppunivariate::lgamma_mle(x,1e-8,200).status != cppunivariate::LgammaStatus::Ok);
  }
}

TEST_CASE("lgamma R6 constant shape-rate collapse and held exceptions") {
  const std::vector<double> x(3,2.);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  CHECK(cppunivariate::lgamma_mle(mltest::as_span(x),1e-8,100).status==cppunivariate::LgammaStatus::Degenerate);
  CHECK(cppunivariate::lgamma_mle(mltest::as_span(x),1e-8,100,2).status==cppunivariate::LgammaStatus::Ok);
  CHECK(cppunivariate::lgamma_mle(mltest::as_span(x),1e-8,100,nan,2).status==cppunivariate::LgammaStatus::Ok);
}

// Independent 180/360-digit raw-model references: TASK-101.7.5 evidence.
TEST_CASE("lgamma: raw transform audit case 4435") {
  const std::vector<double> data{1.953632517309603, 1.9536325173115567, 1.9536325173135103, 1.9536325173154638, 1.9536325173193712};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::lgamma_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::LgammaStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shapelog==doctest::Approx(1.515167188134916e+23).epsilon(1e-10).scale(0));
  CHECK(fit.ratelog==doctest::Approx(2.2624888036389032e+23).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(124.99901060523791).epsilon(1e-10).scale(0));
}
TEST_CASE("lgamma: raw transform audit case 4438") {
  const std::vector<double> data{1.953632517309603, 1.9536325173115567, 1.9536325173135103, 1.9536325173154638, 1.9536325173193712};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::lgamma_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::LgammaStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shapelog==doctest::Approx(1.515167188134916e+23).epsilon(1e-10).scale(0));
  CHECK(fit.ratelog==doctest::Approx(2.2624888036389032e+23).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(124.99901060523791).epsilon(1e-10).scale(0));
}
TEST_CASE("lgamma: raw transform audit case 4434") {
  const std::vector<double> data{1.9536324782408598, 1.953632497777185, 1.9536325173135103, 1.9536325368498355, 1.9536325759224857};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::lgamma_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::LgammaStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shapelog==doctest::Approx(1515153157062830.0).epsilon(1e-10).scale(0));
  CHECK(fit.ratelog==doctest::Approx(2262467845304841.0).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(78.94728556929056).epsilon(1e-10).scale(0));
}
TEST_CASE("lgamma: raw transform audit case 4437") {
  const std::vector<double> data{1.9536324782408598, 1.953632497777185, 1.9536325173135103, 1.9536325368498355, 1.9536325759224857};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::lgamma_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::LgammaStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shapelog==doctest::Approx(1515153157062830.0).epsilon(1e-10).scale(0));
  CHECK(fit.ratelog==doctest::Approx(2262467845304841.0).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(78.94728556929056).epsilon(1e-10).scale(0));
}
TEST_CASE("lgamma: raw transform audit case 10557") {
  const std::vector<double> data{1.99999996, 1.99999998, 2.0, 2.00000002, 2.00000006};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::lgamma_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::LgammaStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shapelog==doctest::Approx(1623152109407427.5).epsilon(1e-10).scale(0));
  CHECK(fit.ratelog==doctest::Approx(2341713492093799.5).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(78.83000201013296).epsilon(1e-10).scale(0));
}
TEST_CASE("lgamma: raw transform audit case 4442") {
  const std::vector<double> data{5.262181501175243, 1.9536325173135103, 2.538689273183437, 1.1505091839279815, 1953637.7794950113};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::lgamma_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::LgammaStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shapelog==doctest::Approx(0.5551927127152927).epsilon(1e-10).scale(0));
  CHECK(fit.ratelog==doctest::Approx(0.15519193754309563).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(-28.521381431876165).epsilon(1e-10).scale(0));
}

TEST_CASE("lgamma: archived density seam 4435") {
  const std::vector<double> data{1.953632517309603,1.9536325173115567,1.9536325173135103,1.9536325173154638,1.9536325173193712};
  const std::vector<double> expected{24.682202729864894,25.256572030183143,25.493044961914787,25.391705999939152,24.175484883178914};
  for(std::size_t i=0;i<data.size();++i) {
    const double actual=cppunivariate::distributions::dlgamma(data[i],1.515167188134916e+23,2.262488803638903e+23,true);
    CHECK(std::abs(actual-expected[i])<=1e-10*std::max(1.0,std::abs(expected[i])));
  }
}

TEST_CASE("lgamma: archived density seam 4438") {
  const std::vector<double> data{1.953632517309603,1.9536325173115567,1.9536325173135103,1.9536325173154638,1.9536325173193712};
  const std::vector<double> expected{24.682202729864894,25.256572030183143,25.493044961914787,25.391705999939152,24.175484883178914};
  for(std::size_t i=0;i<data.size();++i) {
    const double actual=cppunivariate::distributions::dlgamma(data[i],1.515167188134916e+23,2.262488803638903e+23,true);
    CHECK(std::abs(actual-expected[i])<=1e-10*std::max(1.0,std::abs(expected[i])));
  }
}

TEST_CASE("lgamma: Jensen gap with one observation near the lower endpoint") {
  // Direct log-likelihood and score at 360 decimal digits (TASK-363 check.py).
  // References below are rounded to 17 significant digits.
  struct Case {
    int power, count;
    double shape, rate, logLik;
  };
  const Case cases[]{
    {2, 10, 0.22497092469473005, 0.17851044057071031, -10.593818115507188},
    {2, 15, 0.31047376389854794, 0.23889011173470186, -28.355971179280004},
    {2, 31, 0.56437570529485507, 0.42024362902043793, -79.977346403618687},
    {2, 127, 1.9599105267647052, 1.4249086979777089, -330.76998344162799},
    {4, 10, 0.22122992463711302, 0.087771011671805567, -31.542867214932794},
    {4, 15, 0.30533220655091550, 0.11746700272391746, -59.761005670569751},
    {4, 31, 0.55493265423846094, 0.20660608730249739, -144.82793677454412},
    {4, 127, 1.9248046969084954, 0.69969289849692487, -596.20532090442535},
    {16, 10, 0.21413252491899154, 0.021238795310814227, -128.88518563442604},
    {16, 15, 0.29558103897181483, 0.028428886607029646, -205.73835593991874},
    {16, 31, 0.53704497036505332, 0.049986588095485338, -446.41056623508185},
    {16, 127, 1.8584583473169509, 0.16889377268490481, -1831.2427930674571},
    {128, 10, 0.20435121844309608, 0.0025335792061892124, -926.43938047500448},
    {128, 15, 0.28214936433493616, 0.0033921290726332146, -1402.0176872192822},
    {128, 31, 0.51245193965559719, 0.0059621925164117774, -2918.5710118542528},
    {128, 127, 1.7675861303204067, 0.020079431059506801, -11958.425753750211},
    {512, 10, 0.19834156681334224, 0.00061476764517213170, -3602.2665786590696},
    {512, 15, 0.27390070205433610, 0.00082323996781207542, -5415.7252262820124},
    {512, 31, 0.49737552592890447, 0.0014466959772523599, -11213.470019811322},
    {512, 127, 1.7120864441254798, 0.0048622413828997028, -45940.298327054293},
    {1023, 10, 0.19547946384932405, 0.00030324435816242281, -7151.3066632008468},
    {1023, 15, 0.26997323016385755, 0.00040611434638532108, -10739.269136991708},
    {1023, 31, 0.49020426764748713, 0.00071361550501826841, -22215.413554924387},
    {1023, 127, 1.6857454726182741, 0.0023960570584745188, -91012.552400230542},
  };
  for (const auto& ref : cases) {
    CAPTURE(ref.power);
    CAPTURE(ref.count);
    std::vector<double> x(ref.count + 1, std::ldexp(1., ref.power));
    x.front() = 1 + std::ldexp(1., -52);
    const auto fit = cppunivariate::lgamma_mle(mltest::as_span(x), 1e-10, 200);
    REQUIRE(fit.status == cppunivariate::LgammaStatus::Ok);
    const auto outcome = cppunivariate::outcome(fit);
    CHECK(outcome.certified);
    CHECK(outcome.uniqueness_known);
    CHECK(outcome.unique);
    CHECK(std::abs(fit.shapelog / ref.shape - 1) <= 1e-10);
    CHECK(std::abs(fit.ratelog / ref.rate - 1) <= 1e-10);
    CHECK(std::abs(fit.logLik - ref.logLik) <= 1e-10 * std::max(1., std::abs(ref.logLik)));
  }
}
