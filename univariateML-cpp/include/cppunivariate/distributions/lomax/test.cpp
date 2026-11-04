#include <algorithm>
#include <limits>
// lomax: golden regression, start value, existence condition, d/p/q/r.

#include "test_support.hpp"

#include "cppunivariate/distributions/lomax/dpqr.hpp"
#include "cppunivariate/distributions/lomax/mle.hpp"
#include "cppunivariate/distributions/rng_std.hpp"

#include <cmath>
#include <vector>

TEST_CASE("lomax: golden") {
  auto g = mltest::load_distribution_golden("lomax");
  auto r = cppunivariate::lomax_mle(mltest::as_span(g.data), mltest::kFitTol,
                                    mltest::kFitMaxit);
  CHECK(r.status == cppunivariate::LomaxStatus::Ok);
  CHECK(r.lambda == doctest::Approx(g.estimates.at("lambda")).epsilon(mltest::kTol));
  CHECK(r.kappa == doctest::Approx(g.estimates.at("kappa")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

TEST_CASE("lomax: an explicit start converges to the same fit") {
  auto g = mltest::load_distribution_golden("lomax");
  const auto x = mltest::as_span(g.data);
  auto a = cppunivariate::lomax_mle(x, 1e-12, 200);
  auto b = cppunivariate::lomax_mle(x, 1e-12, 200, 3.0 * a.lambda);
  CHECK(b.status == cppunivariate::LomaxStatus::Ok);
  CHECK(b.lambda == doctest::Approx(a.lambda).epsilon(1e-8));
  CHECK(b.kappa == doctest::Approx(a.kappa).epsilon(1e-8));
}

TEST_CASE("lomax: concentrated sample selects the exponential limit") {
  const std::vector<double> x = {1.0, 1.1, 0.9, 1.05, 0.95};
  auto r = cppunivariate::lomax_mle({x.data(), x.size()}, mltest::kFitTol,
                                    mltest::kFitMaxit);
  CHECK(r.status == cppunivariate::LomaxStatus::ExponentialLimit);
}

TEST_CASE("lomax: rlomax is positive; qlomax and plomax round-trip") {
  cppunivariate::distributions::StdRng<> rng(127);
  for (int i = 0; i < 5000; ++i) {
    CHECK(cppunivariate::distributions::rlomax(rng, 2.0, 3.0) > 0.0);
  }
  const double q = cppunivariate::distributions::qlomax(0.7, 2.0, 3.0, true, false);
  const double back = cppunivariate::distributions::plomax(q, 2.0, 3.0, true, false);
  CHECK(std::abs(back - 0.7) < 1e-10);
}

TEST_CASE("lomax: bounded global profile, scaling and replication") {
  const std::vector<std::vector<double>> cases = {{1,100},{.001,4,6},{.001,1,2},{1,29},{1,1,1,12,12}};
  const std::vector<double> gaps = {.590084,1.338305,.652423,.001924,1.90927e-4};
  const std::vector<double> lambdas = {.772199,1394.08,1039.29,.236987,.0359623};
  const std::vector<double> kappas = {.405536,.161888,.196081,.878708,6.07162};
  for (std::size_t j=0;j<cases.size();++j) {
    const auto &x=cases[j];
    const auto f=cppunivariate::lomax_mle(mltest::as_span(x),1e-12,200);
    REQUIRE(f.status==cppunivariate::LomaxStatus::Ok);
    double m=0; for (double v:x) m+=v/x.size();
    CHECK(f.logLik+x.size()*(std::log(m)+1) == doctest::Approx(gaps[j]).epsilon(5e-5));
    CHECK(f.lambda == doctest::Approx(lambdas[j]).epsilon(5e-6));
    CHECK(f.kappa == doctest::Approx(kappas[j]).epsilon(5e-6));
    for (double scale : {1e-200,1e200}) {
      auto z=x; for (double &v:z) v*=scale;
      auto g=cppunivariate::lomax_mle(mltest::as_span(z),1e-12,200);
      CHECK(g.status==cppunivariate::LomaxStatus::Ok);
      CHECK(g.lambda*scale == doctest::Approx(f.lambda).epsilon(1e-6));
      CHECK(g.kappa == doctest::Approx(f.kappa).epsilon(1e-6));
    }
    std::vector<double> repeated;
    for (int k=0;k<50;++k) repeated.insert(repeated.end(),x.begin(),x.end());
    const auto g=cppunivariate::lomax_mle(mltest::as_span(repeated),1e-12,200);
    CHECK(g.lambda == doctest::Approx(f.lambda).epsilon(1e-6));
    CHECK(g.logLik/50 == doctest::Approx(f.logLik).epsilon(1e-12));
  }
  for (const auto &x : std::vector<std::vector<double>>{{1,10},{1,28},{1,1,1}})
    CHECK(cppunivariate::lomax_mle(mltest::as_span(x),1e-12,200).status==cppunivariate::LomaxStatus::ExponentialLimit);
}

TEST_CASE("lomax: compressed nominations refine against the complete sample") {
  for (const auto &x : std::vector<std::vector<double>>{{1,100},{1,29},{1,1,1,12,12}}) {
    const auto f=cppunivariate::lomax_mle(mltest::as_span(x),1e-12,200);
    std::vector<double> large;
    for (int j=0;j<3000;++j) large.insert(large.end(),x.begin(),x.end());
    const auto g=cppunivariate::lomax_mle(mltest::as_span(large),1e-12,200);
    REQUIRE(g.status==cppunivariate::LomaxStatus::Ok);
    CHECK(g.lambda==doctest::Approx(f.lambda).epsilon(1e-6));
    CHECK(g.kappa==doctest::Approx(f.kappa).epsilon(1e-6));
    CHECK(g.logLik/3000==doctest::Approx(f.logLik).epsilon(1e-10));
  }
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("lomax: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("lomax",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dlomax(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::plomax(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qlomax(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rlomax(rng, p[0], p[1]); });
}

// from mpmath import mp, log, log1p, exp, expm1
// mp.dps=500; v=mp.mpf
// -2*log1p(v('1e200')); expm1(500);
// expm1(-log1p(-v('1e-300'))/2);
// expm1(-log1p(-exp(-1000))/v('1e-200'))/v('1e-200');
// -2*log1p(v('1e400')); expm1(v(745)/v('1e200'))/v('1e-200').
TEST_CASE("lomax: high precision extreme tails") {
  CHECK(cppunivariate::distributions::plomax(1e200,1,2,false,true) == doctest::Approx(-921.0340371976182736072).epsilon(2e-15));
  CHECK(std::abs(cppunivariate::distributions::qlomax(-1000,1,2,false,true)/1.4035922178528374107e217-1) < 2e-13);
  CHECK(std::abs(cppunivariate::distributions::qlomax(1e-300,1,2,true,false)/5e-301-1) < 2e-13);
  CHECK(std::abs(cppunivariate::distributions::qlomax(-1000,1e-200,1e-200,true,true)/5.0759588975494567653e-35-1) < 3e-13);
  CHECK(cppunivariate::distributions::plomax(1e200,1e200,2,false,true) == doctest::Approx(-1842.0680743952365472).epsilon(2e-15));
  CHECK(cppunivariate::distributions::plomax(1e-300,1,2,true,true) == doctest::Approx(std::log(2.0)-300*std::log(10.0)).epsilon(2e-15));
  CHECK(std::isfinite(cppunivariate::distributions::qlomax(-1600,1e100,2,false,true)));
  CHECK(cppunivariate::distributions::qlomax(-1e4,1,2,false,true) == cppunivariate::special::kPosInf);
  CHECK(cppunivariate::distributions::dlomax(0,2,3,false) == doctest::Approx(6));
  CHECK(cppunivariate::distributions::qlomax(-745,1e-200,1e200,false,true) == doctest::Approx(745).epsilon(2e-15));
}

TEST_CASE("lomax: tail flags, endpoints, invalid probabilities") {
  const double lo = 0.0, hi = cppunivariate::special::kPosInf;
  for (bool lower : {false,true}) {
    CHECK(cppunivariate::distributions::qlomax(0,1,2,lower,false) == (lower ? lo : hi));
    CHECK(cppunivariate::distributions::qlomax(1,1,2,lower,false) == (lower ? hi : lo));
    CHECK(cppunivariate::distributions::qlomax(cppunivariate::special::kNegInf,1,2,lower,true) == (lower ? lo : hi));
    CHECK(cppunivariate::distributions::qlomax(0,1,2,lower,true) == (lower ? hi : lo));
    CHECK(std::isnan(cppunivariate::distributions::qlomax(-.1,1,2,lower,false)));
    CHECK(std::isnan(cppunivariate::distributions::qlomax(1.1,1,2,lower,false)));
    CHECK(std::isnan(cppunivariate::distributions::qlomax(.1,1,2,lower,true)));
    CHECK(cppunivariate::distributions::plomax(lo,1,2,lower,false) == (lower ? 0 : 1));
    CHECK(cppunivariate::distributions::plomax(hi,1,2,lower,false) == (lower ? 1 : 0));
    for (bool log_p : {false,true}) {
      for (double lp : {-1e-20,-.01,-1.0,-20.0,-100.0}) {
        const double p = log_p ? lp : std::exp(lp);
        const double q = cppunivariate::distributions::qlomax(p,1,2,lower,log_p);
        if (q > lo && q < hi) {
          const double back = cppunivariate::distributions::plomax(q,1,2,lower,true);
          // Near finite support edges, test rounding by bracketing neighbours.
          const double left = cppunivariate::distributions::plomax(std::nextafter(q,cppunivariate::special::kNegInf),1,2,lower,true);
          const double right = cppunivariate::distributions::plomax(std::nextafter(q,cppunivariate::special::kPosInf),1,2,lower,true);
          const double target = log_p ? p : std::log(p);
          CHECK((std::abs(back-target) < 1e-12 ||
                 (target >= std::min(left,right)-1e-12 && target <= std::max(left,right)+1e-12)));
        }
      }
    }
  }
}

// mpmath: mp.dps=500; v=mp.mpf
// h=v(1e200)*log1p(v(1e-200)*v(1e-200)); log(-expm1(-h)).
TEST_CASE("lomax: double-only rescued product") {
  CHECK(cppunivariate::distributions::plomax(1e-200,1e-200,1e200,true,true) == doctest::Approx(-460.51701859880913687).epsilon(2e-15));
  CHECK(cppunivariate::distributions::plomax(1e300,1e20,.1,false,true) == doctest::Approx(-73.68272297580947).epsilon(2e-15));
}

TEST_CASE("lomax P2 every held subset") {
  const auto g=mltest::load_distribution_golden("lomax");
  std::vector<double> x=g.data;
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> held{g.estimates.at("lambda"),g.estimates.at("kappa")};
  { const auto r=cppunivariate::lomax_mle(mltest::as_span(x),1e-10,200,nan,held[0],nan);
    CHECK(r.lambda==held[0]);
    CHECK(std::isfinite(r.kappa));
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dlomax(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.lambda,r.kappa};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[1]));a[1]+=h;b[1]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::lomax_mle(mltest::as_span(x),1e-10,200,nan,nan,held[1]);
    CHECK(std::isfinite(r.lambda));
    CHECK(r.kappa==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dlomax(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.lambda,r.kappa};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[0]));a[0]+=h;b[0]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::lomax_mle(mltest::as_span(x),1e-10,200,nan,held[0],held[1]);
    CHECK(r.lambda==held[0]);
    CHECK(r.kappa==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dlomax(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.lambda,r.kappa};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
  }
}

TEST_CASE("lomax P2 held boundaries and solver failure") {
  const std::vector<double> x{2,2,2};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  CHECK(std::isfinite(cppunivariate::lomax_mle(mltest::as_span(x),1e-10,100,nan,2,nan).logLik));
  CHECK(std::isfinite(cppunivariate::lomax_mle(mltest::as_span(x),1e-10,100,nan,nan,2).logLik));
  CHECK(cppunivariate::lomax_mle(mltest::as_span(x),1e-10,1,nan,nan,2).status==cppunivariate::LomaxStatus::IterationFailure);
}

TEST_CASE("lomax: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("lomax");
  const auto x=mltest::as_span(g.data);
  CHECK(cppunivariate::lomax_mle(x,1e-8,1).status==cppunivariate::LomaxStatus::IterationFailure);
}

TEST_CASE("lomax: P3 Newton candidate and compressed profile reject exhaustion") {
  const auto g=mltest::load_distribution_golden("lomax");
  CHECK(cppunivariate::lomax_newton_candidate(mltest::as_span(g.data),1e-10,1).status==cppunivariate::LomaxStatus::IterationFailure);
  std::vector<double> large;
  while (large.size()<=4096) large.insert(large.end(),g.data.begin(),g.data.end());
  CHECK(cppunivariate::lomax_mle(mltest::as_span(large),1e-10,1).status==cppunivariate::LomaxStatus::IterationFailure);
  const auto fit=cppunivariate::lomax_mle(mltest::as_span(large),1e-10,100);
  CHECK(fit.status==cppunivariate::LomaxStatus::Ok);
  CHECK(fit.lambda==doctest::Approx(g.estimates.at("lambda")).epsilon(1e-5));
  CHECK(fit.kappa==doctest::Approx(g.estimates.at("kappa")).epsilon(1e-5));
}

TEST_CASE("lomax: verified extreme conditional bracket") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> x{1e300,2e300};
  CHECK(cppunivariate::lomax_mle(mltest::as_span(x),1e-10,100,nan,nan,1e300).status==cppunivariate::LomaxStatus::IterationFailure);
  const std::vector<double> y{1,2};
  const auto r=cppunivariate::lomax_mle(mltest::as_span(y),1e-10,100,nan,nan,1e300);
  REQUIRE(r.status==cppunivariate::LomaxStatus::Ok);
  CHECK(r.lambda==doctest::Approx(2e-300/3).epsilon(1e-9).scale(1e-300));
  CHECK(r.logLik==doctest::Approx(2*(std::log(2.0/3)-1)).epsilon(1e-10));
}

TEST_CASE("lomax: certificate envelopes dominate the complete scalar profile") {
  cppunivariate::lomax_cert_detail::Data d;
  const std::vector<double> y{.02,.02,.8,3.16}; // mean exactly one in decimal
  for(double v:y)d.values.push_back({v,std::log(v),.25});
  d.error=1e-12;
  for(double v:y)d.second+=v*v/y.size();
  for(double a:{-20.,-5.,-1.,1.,5.})for(double width:{.01,.2,2.,10.}) {
    const auto l=cppunivariate::lomax_cert_detail::evaluate(d,a);
    const auto r=cppunivariate::lomax_cert_detail::evaluate(d,a+width);
    const double upper=cppunivariate::lomax_cert_detail::upper(d,l,r);
    const double curvature=cppunivariate::lomax_cert_detail::curvature_upper(d,l,r);
    for(int j=0;j<=20;++j) {
      const auto p=cppunivariate::lomax_cert_detail::evaluate(d,a+width*j/20);
      CHECK(p.h<=upper);
      const double hessian=p.B*p.B/(p.A*p.A)-p.C/p.A-p.C;
      CHECK(hessian>=-1.25);
      CHECK(hessian<=curvature);
    }
  }
}

TEST_CASE("lomax: certificate arithmetic and budget failures never return coefficients") {
  const std::vector<double> extremes{std::numeric_limits<double>::denorm_min(),1e300};
  const auto failed=cppunivariate::lomax_mle(mltest::as_span(extremes),1e-10,100);
  CHECK(failed.status==cppunivariate::LomaxStatus::IterationFailure);
  CHECK(std::isnan(failed.lambda));
  CHECK(std::isnan(failed.kappa));
  const std::vector<double> nonlocal{1,1,40,59};
  const auto fitted=cppunivariate::lomax_mle(mltest::as_span(nonlocal),1e-12,100);
  REQUIRE(fitted.status==cppunivariate::LomaxStatus::Ok);
  CHECK(fitted.logLik> -4*(1+std::log(25.25)));
  CHECK(fitted.iterations>2);
}

TEST_CASE("lomax: certificate checks coefficients after extreme rescaling") {
  const auto g=mltest::load_distribution_golden("lomax");
  for(double scale:{1e-308,1e-309}) {
    auto x=g.data;for(double& v:x)v*=scale;
    const auto r=cppunivariate::lomax_mle(mltest::as_span(x),1e-10,100);
    if(scale==1e-308) {
      REQUIRE(r.status==cppunivariate::LomaxStatus::Ok);
      CHECK(r.lambda>0);
      CHECK(std::isfinite(r.lambda));
      CHECK(std::isfinite(r.kappa));
      CHECK(std::isfinite(r.logLik));
    } else {
      CHECK(r.status==cppunivariate::LomaxStatus::IterationFailure);
      CHECK(std::isnan(r.lambda));
      CHECK(std::isnan(r.kappa));
    }
  }
}

TEST_CASE("lomax: subnormal normalization cannot certify a relative enclosure") {
  const std::vector<double> x{std::numeric_limits<double>::denorm_min(),1};
  const auto r=cppunivariate::lomax_mle(mltest::as_span(x),1e-10,100);
  CHECK(r.status==cppunivariate::LomaxStatus::IterationFailure);
  CHECK(r.certificate_failure==cppunivariate::LomaxCertificateFailure::Arithmetic);
  CHECK(std::isnan(r.lambda));
}

TEST_CASE("lomax: restored log coordinates require adequate relative precision") {
  cppunivariate::lomax_cert_detail::Data d;
  d.error=1e-12;
  for(double v:{1.,100.})d.values.push_back({v/50.5,std::log(v/50.5),.5});
  const double z=std::log(.7721986394822592*50.5);
  const auto a=cppunivariate::lomax_cert_detail::evaluate(d,z-.001);
  const auto b=cppunivariate::lomax_cert_detail::evaluate(d,z+.001);
  REQUIRE(cppunivariate::lomax_cert_detail::isolate(d,a,b,100).valid);
  // A conversion near exp(-740) has only about two significant digits.
  // This tests the restoration component independently of input scaling.
  d.logmean=z+740;
  CHECK_FALSE(cppunivariate::lomax_cert_detail::isolate(d,a,b,100).valid);

  const std::vector<double> x{1,1,1,12,12};
  const auto base=cppunivariate::lomax_mle(mltest::as_span(x),1e-12,100);
  auto large=x;for(double& v:large)v*=1e307;
  const auto r=cppunivariate::lomax_mle(mltest::as_span(large),1e-12,100);
  REQUIRE(r.status==cppunivariate::LomaxStatus::Ok);
  CHECK(r.lambda>0);
  CHECK(r.lambda<std::numeric_limits<double>::min());
  CHECK(r.lambda*1e307==doctest::Approx(base.lambda).epsilon(1e-6));
  CHECK(r.kappa==doctest::Approx(base.kappa).epsilon(1e-6));
}
