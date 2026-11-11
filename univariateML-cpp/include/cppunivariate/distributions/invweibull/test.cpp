#include <algorithm>
#include "test_support.hpp"
#include "cppunivariate/distributions/invweibull/mle.hpp"
#include "cppunivariate/distributions/invweibull/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <string>
#include <vector>

TEST_CASE("rinvweibull samples are positive; CDF round-trip at p=0.5") {
  cppunivariate::distributions::StdRng<> rng(157);
  const int n = 5000;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rinvweibull(rng, 2.0, 1.0 / 3.0);
    CHECK(x > 0.0);
  }
  const double q =
      cppunivariate::distributions::qinvweibull(0.5, 2.0, 1.0 / 3.0, true, false);
  const double back =
      cppunivariate::distributions::pinvweibull(q, 2.0, 1.0 / 3.0, true, false);
  CHECK(std::abs(back - 0.5) < 1e-10);
}

TEST_CASE("invweibull") {
  auto g = mltest::load_distribution_golden("invweibull");
  auto r =
      cppunivariate::invweibull_mle(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.shape == doctest::Approx(g.estimates.at("shape")).epsilon(mltest::kGolden_invweibull).scale(0));
  CHECK(r.scale == doctest::Approx(g.estimates.at("scale")).epsilon(mltest::kGolden_invweibull).scale(0));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_invweibull).scale(0));
}

TEST_CASE("invweibull: explicit shape start and automatic default") {
  auto g = mltest::load_distribution_golden("invweibull");
  auto a = cppunivariate::invweibull_mle(mltest::as_span(g.data), 1e-12, 200);
  auto b = cppunivariate::invweibull_mle(mltest::as_span(g.data), 1e-12, 200, 1.2 * a.shape);
  CHECK(b.shape == doctest::Approx(a.shape).epsilon(1e-9));
  CHECK(b.logLik == doctest::Approx(a.logLik).epsilon(1e-10));
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("invweibull: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("invweibull",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dinvweibull(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pinvweibull(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qinvweibull(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rinvweibull(rng, p[0], p[1]); });
}

// mpmath reproduction: import mpmath as m; m.mp.dps=100
// F=lambda x:m.exp(-1/x**2); Q=lambda l:1/m.sqrt(-l)
TEST_CASE("invweibull: extreme tail regressions") {
  CHECK(cppunivariate::distributions::pinvweibull(1e300,2,1,false,true) == doctest::Approx(-2*std::log(1e300)).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::qinvweibull(-10000,2,1,true,true) == doctest::Approx(0.01).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::dinvweibull(INFINITY,2,1,true) == -INFINITY);
}


TEST_CASE("invweibull P2 every held subset") {
  const auto g=mltest::load_distribution_golden("invweibull");
  std::vector<double> x=g.data;
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> held{g.estimates.at("shape"),g.estimates.at("scale")};
  { const auto r=cppunivariate::invweibull_mle(mltest::as_span(x),1e-10,200,nan,held[0],nan);
    CHECK(r.shape==held[0]);
    CHECK(std::isfinite(r.scale));
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dinvweibull(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.scale};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[1]));a[1]+=h;b[1]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::invweibull_mle(mltest::as_span(x),1e-10,200,nan,nan,held[1]);
    CHECK(std::isfinite(r.shape));
    CHECK(r.scale==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dinvweibull(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.scale};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[0]));a[0]+=h;b[0]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::invweibull_mle(mltest::as_span(x),1e-10,200,nan,held[0],held[1]);
    CHECK(r.shape==held[0]);
    CHECK(r.scale==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dinvweibull(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.scale};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
  }
}

TEST_CASE("invweibull P2 held boundaries and solver failure") {
  const std::vector<double> x{2,2,2};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  // Held scale 2 is held rate 0.5, which equals every reciprocal
  // observation 1/2: the unbounded face.
  CHECK_FALSE(std::isfinite(cppunivariate::invweibull_mle(mltest::as_span(x),1e-10,100,nan,nan,2.0).logLik));
  CHECK(std::isfinite(cppunivariate::invweibull_mle(mltest::as_span(x),1e-10,100,nan,2,nan).logLik));
}

TEST_CASE("invweibull: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("invweibull");
  const auto x=mltest::as_span(g.data);
  CHECK(cppunivariate::invweibull_mle(x,1e-8,1).status==cppunivariate::InvWeibullStatus::IterationFailure);
}

TEST_CASE("invweibull: README scale validation") {
  cppunivariate::distributions::StdRng<> rng(94);
  for (double scale : {0.0, -1.0, double(INFINITY), double(NAN)}) {
    CHECK(std::isnan(cppunivariate::distributions::dinvweibull(0.5, 2.0, scale, false)));
    CHECK(std::isnan(cppunivariate::distributions::pinvweibull(0.5, 2.0, scale, false, true)));
    CHECK(std::isnan(cppunivariate::distributions::qinvweibull(0.5, 2.0, scale, false, true)));
    CHECK(std::isnan(cppunivariate::distributions::rinvweibull(rng, 2.0, scale)));
  }
}

TEST_CASE("invweibull: finite scale with overflowing reciprocal") {
  const double scale = 1e-320;
  CHECK(std::isfinite(cppunivariate::distributions::dinvweibull(scale, 2.0, scale, true)));
  CHECK(std::isfinite(cppunivariate::distributions::pinvweibull(scale, 2.0, scale, false, true)));
  CHECK(cppunivariate::distributions::qinvweibull(0.5, 2.0, scale, true, false) > 0.0);
}

TEST_CASE("invweibull: TASK-132 invalid data never report success") {
  for (const auto& data : {std::vector<double>{},std::vector<double>{NAN,2.0},std::vector<double>{INFINITY,2.0},std::vector<double>{-INFINITY,2.0},std::vector<double>{-1.0,2.0},std::vector<double>{0.0,2.0}}) {
    const cppunivariate::span x{data.data(),data.size()};
    CHECK(cppunivariate::invweibull_mle(x,1e-8,200).status != cppunivariate::InvWeibullStatus::Ok);
  }
}

TEST_CASE("invweibull: TASK-132 preserves invalid Gumbel controls") {
  const std::vector<double> data{1,2,3};
  CHECK(cppunivariate::invweibull_mle(cppunivariate::span{data.data(),data.size()},1e-8,0).status == cppunivariate::InvWeibullStatus::InvalidInput);
}

// Independent 180/360-digit raw-model references: TASK-101.7.5 evidence.
TEST_CASE("invweibull: raw transform audit case 4219") {
  const std::vector<double> data{0.5071697345584799, 0.5071697345589871, 0.5071697345594942, 0.5071697345600014, 0.5071697345610158};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::invweibull_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::InvWeibullStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(707675622377.0812).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(0.5071697345591792).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(131.91568101929354).epsilon(1e-10).scale(0));
}
TEST_CASE("invweibull: raw transform audit case 4283") {
  const std::vector<double> data{1e-200, 2e-200, 3e-200, 5e-200, 9e-200};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::invweibull_mle(x, 1e-10, 200, NAN, 1.928819454840455, 0.3434414370956311);
  CHECK(fit.status==cppunivariate::InvWeibullStatus::IterationFailure);
}
TEST_CASE("invweibull: raw transform audit case 4217") {
  const std::vector<double> data{0.5070683006125823, 0.5071190175860383, 0.5071697345594942, 0.5072204515329501, 0.507321885479862};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::invweibull_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::InvWeibullStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(7076.606527224669).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(0.5071382319443192).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(39.81219762261764).epsilon(1e-10).scale(0));
}
TEST_CASE("invweibull: raw transform audit case 4220") {
  const std::vector<double> data{0.9998, 0.9999, 1.0, 1.0001, 1.0003};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::invweibull_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::InvWeibullStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(7076.606527223182).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(0.9999378854591896).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(36.41764987636973).epsilon(1e-10).scale(0));
}
TEST_CASE("invweibull: raw transform audit case 4221") {
  const std::vector<double> data{0.99999998, 0.99999999, 1.0, 1.00000001, 1.00000003};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::invweibull_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::InvWeibullStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(70766413.46901025).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(0.9999999937896172).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(82.46937861550163).epsilon(1e-10).scale(0));
}
TEST_CASE("invweibull: raw transform audit case 4222") {
  const std::vector<double> data{0.999999999998, 0.999999999999, 1.0, 1.000000000001, 1.000000000003};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::invweibull_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::InvWeibullStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(707663145134.7852).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(0.999999999999379).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(128.52106495920626).epsilon(1e-10).scale(0));
}
TEST_CASE("invweibull: raw transform audit case 4218") {
  const std::vector<double> data{0.5071697244160995, 0.5071697294877968, 0.5071697345594942, 0.5071697396311916, 0.5071697497745863};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::invweibull_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::InvWeibullStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(70766413.34268498).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(0.5071697314097761).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(85.86392635209116).epsilon(1e-10).scale(0));
}
TEST_CASE("invweibull: raw transform audit case 9338") {
  const std::vector<double> data{0.988503900932562, 0.9927057395157377, 0.9955081820301651, 0.9979146225997783, 1.0001935876214705, 1.0024856651664025, 1.004904185632642, 1.0075787228651987, 1.0107036114154169, 1.0146470464667317, 1.0203382467452637, 1.0320720648857378};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::invweibull_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::InvWeibullStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(105.80607046342593).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(1.00006845636793).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(36.91980806615555).epsilon(1e-10).scale(0));
}
TEST_CASE("invweibull: raw transform audit case 9798") {
  const std::vector<double> data{0.999998843731662, 0.9999992679008999, 0.9999995498064518, 0.9999997912445391, 1.0000000193568888, 1.0000002482581318, 1.0000004892200485, 1.0000007550151475, 1.0000010646738942, 1.000001454082512, 1.000002013420705, 1.0000031568544765};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::invweibull_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::InvWeibullStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(1058060.7046228813).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(1.0000000068454025).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(147.51042142852074).epsilon(1e-10).scale(0));
}
TEST_CASE("invweibull: raw transform audit case 4228") {
  const std::vector<double> data{2.6516987632880034e+149, 2.180658596102709e+149, 9.134933419025255e+149, 4.864424540397452e+149, 5.071697345594942e+149, 6.058163188737759e+149, 6.212659185566571e+149, 1.7898872530714507e+149, 2.8094570154848314e+149, 9.308703872748639e+149, 4.375259238937079e+149, 5.23122126168884e+149};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::invweibull_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::InvWeibullStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(2.0285581651990117).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(3.389075478414657e+149).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(-4144.6620460087115).epsilon(1e-10).scale(0));
}
TEST_CASE("invweibull: raw transform audit case 4212") {
  const std::vector<double> data{0.26516987632880035, 0.21806585961027092, 0.9134933419025255, 0.4864424540397452, 0.5071697345594942, 0.6058163188737758, 0.6212659185566571, 0.17898872530714507, 0.28094570154848314, 0.9308703872748638, 0.4375259238937079, 0.523122126168884};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::invweibull_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::InvWeibullStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(2.0285581651990117).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(0.3389075478414657).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(-0.008878619429328452).epsilon(1e-10).scale(0));
}
TEST_CASE("invweibull: raw transform audit case 4227") {
  const std::vector<double> data{2.6516987632880036e-151, 2.180658596102709e-151, 9.134933419025255e-151, 4.864424540397452e-151, 5.071697345594942e-151, 6.058163188737759e-151, 6.212659185566572e-151, 1.7898872530714506e-151, 2.8094570154848313e-151, 9.308703872748639e-151, 4.375259238937079e-151, 5.23122126168884e-151};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::invweibull_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::InvWeibullStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(2.0285581651990117).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(3.389075478414657e-151).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(4144.644288769853).epsilon(1e-10).scale(0));
}
TEST_CASE("invweibull: raw transform audit case 4214") {
  const std::vector<double> data{0.26516987632880035, 0.21806585961027092, 0.9134933419025255};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::invweibull_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::InvWeibullStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(2.292803066777193).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(0.2810958635274569).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(0.44376226468664065).epsilon(1e-10).scale(0));
}
TEST_CASE("invweibull: raw transform audit case 4226") {
  const std::vector<double> data{1e-200, 2e-200, 3e-200, 5e-200, 9e-200};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::invweibull_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::InvWeibullStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(1.4424748841607793).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(2.1013325956265178e-200).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(2291.098594735031).epsilon(1e-10).scale(0));
}
TEST_CASE("invweibull: raw transform audit case 4225") {
  const std::vector<double> data{1e-17, 2e-17, 3e-17, 5.0000000000000005e-17, 9.000000000000001e-17};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::invweibull_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::InvWeibullStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(1.4424748841607793).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(2.101332595626518e-17).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(184.233234645479).epsilon(1e-10).scale(0));
}
TEST_CASE("invweibull: raw transform audit case 4223") {
  const std::vector<double> data{1e-06, 2e-06, 3e-06, 4.9999999999999996e-06, 9e-06};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::invweibull_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::InvWeibullStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(1.4424748841607793).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(2.101332595626518e-06).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(57.59105453080648).epsilon(1e-10).scale(0));
}
TEST_CASE("invweibull: raw transform audit case 4224") {
  const std::vector<double> data{1e-12, 2e-12, 3e-12, 5e-12, 9e-12};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::invweibull_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::InvWeibullStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(1.4424748841607793).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(2.1013325956265178e-12).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(126.66860732062784).epsilon(1e-10).scale(0));
}

TEST_CASE("invweibull: tiny held power retains the geometric-mean scale limit") {
  const std::vector<double> data{1,4};
  const auto fit=cppunivariate::invweibull_mle(mltest::as_span(data),1e-10,100,NAN,1e-20,NAN);
  REQUIRE(fit.status==cppunivariate::InvWeibullStatus::Ok);
  CHECK(fit.scale==doctest::Approx(2).epsilon(1e-14).scale(0));
}

TEST_CASE("invweibull: finite restored scale despite an overflowing scale multiplier") {
  const std::vector<double> data{1e-300,1e300,1e300,1e300};
  const auto fit=cppunivariate::invweibull_mle(mltest::as_span(data),1e-10,100,NAN,1e-20,NAN);
  REQUIRE(fit.status==cppunivariate::InvWeibullStatus::Ok);
  // The finite-power correction to the geometric mean is below 1e-14.
  CHECK(fit.scale==doctest::Approx(1e150).epsilon(1e-12).scale(0));
}

TEST_CASE("invweibull: ulp-wide scale restoration retains an explicitly uncertified fit") {
  const double eps=std::numeric_limits<double>::epsilon();
  const std::vector<double> data{1,1+eps,1+2*eps,1+3*eps,1+4*eps};
  const auto fit=cppunivariate::invweibull_mle(mltest::as_span(data),1.220703e-4,100);
  REQUIRE(fit.status==cppunivariate::InvWeibullStatus::Ok);
  CHECK_FALSE(fit.certified);
  CHECK(fit.shape>1e14);
  CHECK(std::isfinite(fit.logLik));
  CHECK(fit.logLik>100);
}

TEST_CASE("invweibull: centered d/p/q near the Gumbel limit on exact doubles") {
  // Independent 100-digit mpmath values on the exact binary64 inputs.
  const double rows[][8] = {
    {1000000,0,1000000,1000000.500000125,-1.1065311597244512,-0.60653065969441644,-0.78798373872629013,1000000.3665129878},
    {1000000,0,1000000,1000000.500000125,-1.1065311597244512,-0.60653065969441644,-0.78798373872629013,1000000.3665129878},
    {1000000,0,1000000,1000000.500000125,-1.1065311597244512,-0.60653065969441644,-0.78798373872629013,1000000.3665129878},
    {1000000000,0,1000000000,1000000000.5,-1.1065306601634497,-0.60653065978844978,-0.78798373861354798,1000000000.3665129},
    {1000000000,0,1000000000,1000000000.5,-1.1065306601634497,-0.60653065978844978,-0.78798373861354798,1000000000.3665129},
    {1000000000,0,1000000000,1000000000.5,-1.1065306601634497,-0.60653065978844978,-0.78798373861354798,1000000000.3665129},
    {1000000000000,0,1000000000000,1000000000000.5,-1.1065306597130842,-0.60653065971270925,-0.78798373870435778,1000000000000.3665},
    {1000000000000,0,1000000000000,1000000000000.5,-1.1065306597130842,-0.60653065971270925,-0.78798373870435778,1000000000000.3665},
    {1000000000000,0,1000000000000,1000000000000.5,-1.1065306597130842,-0.60653065971270925,-0.78798373870435778,1000000000000.3665},
    {1000000000000000,0,1000000000000000,1000000000000000.5,-1.1065306597126339,-0.60653065971263354,-0.7879837387044486,1000000000000000.4},
    {1000000000000000,0,1000000000000000,1000000000000000.5,-1.1065306597126339,-0.60653065971263354,-0.7879837387044486,1000000000000000.4},
    {1000000000000000,0,1000000000000000,1000000000000000.5,-1.1065306597126339,-0.60653065971263354,-0.7879837387044486,1000000000000000.4},
  };
  for(const auto& r:rows) {
    CHECK(std::abs(cppunivariate::distributions::dinvweibull(r[3],r[0],r[2],true)-r[4])<4e-15*std::max(1.,std::abs(r[4])));
    for(bool lower:{false,true}) CHECK(std::abs(cppunivariate::distributions::pinvweibull(r[3],r[0],r[2],lower,true)-r[lower ? 5 : 6])<4e-15);
    CHECK(std::abs(cppunivariate::distributions::qinvweibull(.5,r[0],r[2],true,false)-r[7])<4e-15*std::max(1.,std::abs(r[7])));
  }
}

TEST_CASE("invweibull: scaling rescues overflowing and subnormal multipliers") {
  for(const double exponent:{-740.,740.}) {
    const double scale=exponent<0 ? 1e300 : 1e-300;
    const double q=cppunivariate::distributions::qinvweibull(-std::exp(-exponent/2),.5,scale,true,true);
    CHECK(std::isnormal(q));
    CHECK(std::abs(std::log(q)-(std::log(scale)+exponent))<2e-13);
  }
}

TEST_CASE("invweibull: large-shape log tail at a noncentral finite ratio") {
  const double actual=cppunivariate::distributions::pinvweibull(1e308,1e15,5e307,false,true);
  CHECK(std::abs(actual+1e15*std::log(2.))<.25);
}

TEST_CASE("invweibull: extreme scale rescues a multiplier beyond the half-exponent range") {
  const double scale=std::numeric_limits<double>::denorm_min();
  const double q=cppunivariate::distributions::qinvweibull(-1450.,1.,scale,false,true);
  CHECK(std::isnormal(q));
  CHECK(std::abs(std::log(q)-(1450.+std::log(scale)))<2e-13);
}
