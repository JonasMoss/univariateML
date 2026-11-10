#include <algorithm>
#include "test_support.hpp"
#include "cppunivariate/distributions/llogis/mle.hpp"
#include "cppunivariate/distributions/llogis/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <string>
#include <vector>

TEST_CASE("rllogis samples are positive; CDF round-trip at p=0.7") {
  cppunivariate::distributions::StdRng<> rng(163);
  for (int i = 0; i < 5000; ++i) {
    const double x = cppunivariate::distributions::rllogis(rng, 2.0, 1.0 / 3.0);
    CHECK(x > 0.0);
  }
  const double q =
      cppunivariate::distributions::qllogis(0.7, 2.0, 1.0 / 3.0, true, false);
  const double back =
      cppunivariate::distributions::pllogis(q, 2.0, 1.0 / 3.0, true, false);
  CHECK(std::abs(back - 0.7) < 1e-10);
}

TEST_CASE("llogis") {
  auto g = mltest::load_distribution_golden("llogis");
  auto r = cppunivariate::loglogistic_mle(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.shape == doctest::Approx(g.estimates.at("shape")).epsilon(mltest::kGolden_llogis).scale(0));
  CHECK(r.scale == doctest::Approx(g.estimates.at("scale")).epsilon(mltest::kGolden_llogis).scale(0));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_llogis).scale(0));
}

TEST_CASE("llogis: seeded sample with outlier") {
  // fixture-outlier.json is a static fixture predating the scale
  // parameterisation; its "second" field is the old rate (1/scale).
  auto g = mltest::load_golden(std::string(CPPUNIV_DISTRIBUTIONS_DIR) +
      "/llogis/fixture-outlier.json");
  auto r = cppunivariate::loglogistic_mle(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.shape == doctest::Approx(g.estimates.at("first")).epsilon(1e-4));
  CHECK(1.0 / r.scale == doctest::Approx(g.estimates.at("second")).epsilon(1e-4));
  CHECK(r.logLik >= g.logLik - 1e-6);
}


// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("llogis: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("llogis",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dllogis(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pllogis(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qllogis(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rllogis(rng, p[0], p[1]); });
}

TEST_CASE("llogis P2 every held subset") {
  const auto g=mltest::load_distribution_golden("llogis");
  std::vector<double> x=g.data;
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> held{g.estimates.at("shape"),g.estimates.at("scale")};
  { const auto r=cppunivariate::loglogistic_mle(mltest::as_span(x),1e-10,200,held[0],nan);
    CHECK(r.shape==held[0]);
    CHECK(std::isfinite(r.scale));
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dllogis(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.scale};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[1]));a[1]+=h;b[1]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::loglogistic_mle(mltest::as_span(x),1e-10,200,nan,held[1]);
    CHECK(std::isfinite(r.shape));
    CHECK(r.scale==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dllogis(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.scale};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[0]));a[0]+=h;b[0]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::loglogistic_mle(mltest::as_span(x),1e-10,200,held[0],held[1]);
    CHECK(r.shape==held[0]);
    CHECK(r.scale==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dllogis(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.scale};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
  }
}

TEST_CASE("llogis P2 held boundaries and solver failure") {
  const std::vector<double> x{2,2,2};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  // Held scale 2 is held rate 0.5, which equals every observation's
  // reciprocal 1/2: the unbounded face.
  CHECK_FALSE(std::isfinite(cppunivariate::loglogistic_mle(mltest::as_span(x),1e-10,100,nan,2.0).logLik));
  CHECK(std::isfinite(cppunivariate::loglogistic_mle(mltest::as_span(x),1e-10,100,2,nan).logLik));
  CHECK_FALSE(std::isfinite(cppunivariate::loglogistic_mle(mltest::as_span(x),1e-10,1,nan,1).logLik));
}


// mpmath reproduction: import mpmath as mp; mp.mp.dps=1200
// F=lambda x: 1/(1+x**-4); Q=lambda l: mp.exp((l-mp.log1p(-mp.exp(l)))/4)
// Evaluate log tails at the stated decimal x; log densities follow mp.log(mp.diff(F,x))
// (use -mp.diff(S,x) for survival definitions); Q uses the indicated log tail.
TEST_CASE("llogis: deep tails and log inversion regressions") {
  CHECK(cppunivariate::distributions::pllogis(1e200,4,1,false,true) == doctest::Approx(-800*std::log(10.0)).scale(0.0));
  CHECK(cppunivariate::distributions::dllogis(1e200,4,1,true) == doctest::Approx(std::log(4.0)-1000*std::log(10.0)).scale(0.0));
  CHECK(cppunivariate::distributions::qllogis(-1000,4,1,true,true) == doctest::Approx(std::exp(-250.0)).scale(0.0));
  CHECK(cppunivariate::distributions::qllogis(-INFINITY,4,1,false,true) == INFINITY);
}

TEST_CASE("llogis: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("llogis");
  const auto x=mltest::as_span(g.data);
  CHECK(cppunivariate::loglogistic_mle(x,1e-8,1).status==cppunivariate::LogLogisticStatus::IterationFailure);
}

TEST_CASE("llogis: P3 constant data retain the unbounded diagnostic") {
  const std::vector<double> values{1,1,1};
  CHECK(cppunivariate::loglogistic_mle(mltest::as_span(values),1e-8,100).status==cppunivariate::LogLogisticStatus::Unbounded);
}

TEST_CASE("llogis: README scale validation") {
  cppunivariate::distributions::StdRng<> rng(94);
  for (double scale : {0.0, -1.0, double(INFINITY), double(NAN)}) {
    CHECK(std::isnan(cppunivariate::distributions::dllogis(0.5, 2.0, scale, false)));
    CHECK(std::isnan(cppunivariate::distributions::pllogis(0.5, 2.0, scale, false, true)));
    CHECK(std::isnan(cppunivariate::distributions::qllogis(0.5, 2.0, scale, false, true)));
    CHECK(std::isnan(cppunivariate::distributions::rllogis(rng, 2.0, scale)));
  }
}

TEST_CASE("llogis: finite scale with overflowing reciprocal") {
  const double scale = 1e-320;
  CHECK(std::isfinite(cppunivariate::distributions::dllogis(scale, 2.0, scale, true)));
  CHECK(std::isfinite(cppunivariate::distributions::pllogis(scale, 2.0, scale, false, true)));
  CHECK(cppunivariate::distributions::qllogis(0.5, 2.0, scale, true, false) > 0.0);
}

TEST_CASE("llogis: TASK-132 invalid data never report success") {
  for (const auto& data : {std::vector<double>{},std::vector<double>{NAN,2.0},std::vector<double>{INFINITY,2.0},std::vector<double>{-INFINITY,2.0},std::vector<double>{-1.0,2.0},std::vector<double>{0.0,2.0}}) {
    const cppunivariate::span x{data.data(),data.size()};
    CHECK(cppunivariate::loglogistic_mle(x,1e-8,200).status != cppunivariate::LogLogisticStatus::Ok);
  }
}

TEST_CASE("llogis R6 all-free constant collapse") {
  for (double value : {.5,2.}) {
    const std::vector<double> x(3,value);
    CHECK(cppunivariate::loglogistic_mle(mltest::as_span(x),1e-8,100).status==cppunivariate::LogLogisticStatus::Unbounded);
  }
}

// Independent 180/360-digit raw-model references: TASK-101.7.5 evidence.
TEST_CASE("llogis: raw transform audit case 4525") {
  const std::vector<double> data{1.307038121314793, 1.3070381213161002, 1.3070381213174072, 1.307038121318714, 1.3070381213213282};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::loglogistic_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::LogLogisticStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(977338636898.3333).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(1.3070381213175297).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(126.83443367637379).epsilon(1e-10).scale(0));
}
TEST_CASE("llogis: raw transform audit case 4528") {
  const std::vector<double> data{1.307038121314793, 1.3070381213161002, 1.3070381213174072, 1.307038121318714, 1.3070381213213282};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::loglogistic_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::LogLogisticStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(977338636898.3333).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(1.3070381213175297).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(126.83443367637379).epsilon(1e-10).scale(0));
}
TEST_CASE("llogis: raw transform audit case 4535") {
  const std::vector<double> data{1.4612222342357195, 0.7038909590134532, 0.013073080872494258, 1.3070381213174072, 1307058.4295823444};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::loglogistic_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::LogLogisticStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(0.3040901444416098).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(2.201982518014345).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(-26.21124672480389).epsilon(1e-10).scale(0));
}
TEST_CASE("llogis: raw transform audit case 4536") {
  const std::vector<double> data{1.3070355072411646, 1.3070368142792859, 1.3070381213174072, 1.3070394283555284, 1.30704073539365};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::loglogistic_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::LogLogisticStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(1156676.740617789).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(1.3070381213165507).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(58.66887513411426).epsilon(1e-10).scale(0));
}
TEST_CASE("llogis: raw transform audit case 4533") {
  const std::vector<double> data{1.4612222342357196e-150, 7.038909590134533e-151, 1.3073080872494259e-152, 1.3070381213174072e-150, 7.555950558444047e-153, 1.6454641076172512e-150, 3.634118342449831e-150, 5.611994994187286e-152, 3.3079750606038745e-150, 2.030826493716806e-149, 6.404111626327396e-151, 2.1198704112740274e-152};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::loglogistic_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::LogLogisticStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(0.6905841610868956).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(5.212327592697621e-151).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(4126.668189703903).epsilon(1e-10).scale(0));
}
TEST_CASE("llogis: raw transform audit case 4518") {
  const std::vector<double> data{1.4612222342357195, 0.7038909590134532, 0.013073080872494258, 1.3070381213174072, 0.007555950558444047, 1.6454641076172511, 3.634118342449831, 0.05611994994187286, 3.3079750606038747, 20.308264937168058, 0.6404111626327396, 0.021198704112740274};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::loglogistic_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::LogLogisticStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(0.6905841610868956).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(0.5212327592697621).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(-17.98497768537857).epsilon(1e-10).scale(0));
}
TEST_CASE("llogis: raw transform audit case 4534") {
  const std::vector<double> data{1.4612222342357194e+150, 7.038909590134532e+149, 1.307308087249426e+148, 1.307038121317407e+150, 7.555950558444047e+147, 1.6454641076172512e+150, 3.634118342449831e+150, 5.611994994187286e+148, 3.307975060603874e+150, 2.0308264937168056e+151, 6.404111626327396e+149, 2.1198704112740275e+148};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::loglogistic_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::LogLogisticStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(0.6905841610868956).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(5.212327592697621e+149).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(-4162.638145074661).epsilon(1e-10).scale(0));
}
TEST_CASE("llogis: raw transform audit case 4524") {
  const std::vector<double> data{1.3070380951766447, 1.307038108247026, 1.3070381213174072, 1.3070381343877884, 1.3070381605285508};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::loglogistic_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::LogLogisticStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(97732768.82081644).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(1.3070381225421193).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(80.78268785196853).epsilon(1e-10).scale(0));
}
TEST_CASE("llogis: raw transform audit case 4527") {
  const std::vector<double> data{1.3070380951766447, 1.307038108247026, 1.3070381213174072, 1.3070381343877884, 1.3070381605285508};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::loglogistic_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::LogLogisticStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(97732768.82081644).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(1.3070381225421193).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(80.78268785196853).epsilon(1e-10).scale(0));
}
TEST_CASE("llogis: raw transform audit case 4522") {
  const std::vector<double> data{1.4612222342357195, 1.4612222342357195, 1.4612222342357195, 1.4612222342357195, 0.7038909590134532, 0.013073080872494258};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::loglogistic_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::LogLogisticStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(1.2532425554232094).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(0.9111157554269419).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(-8.10597742931569).epsilon(1e-10).scale(0));
}
