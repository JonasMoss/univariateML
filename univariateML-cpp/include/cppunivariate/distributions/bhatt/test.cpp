#include <limits>
#include "test_support.hpp"
#include "cppunivariate/distributions/bhatt/mle.hpp"
#include <vector>
TEST_CASE("bhatt: golden and likelihood") {
  const auto g=mltest::load_distribution_golden("bhatt");
  const auto f=cppunivariate::bhatt_mle(mltest::as_span(g.data));
  REQUIRE(f.status==cppunivariate::BhattStatus::Ok);
  CHECK(f.logLik==doctest::Approx(g.logLik).epsilon(1e-7));
  double ll=0;for(double x:g.data)ll+=cppunivariate::distributions::dbhatt(x,f.mu,f.sigma,f.a,true);
  CHECK(ll==doctest::Approx(f.logLik).epsilon(1e-12));
  std::vector<double> theta={f.mu,f.sigma,f.a};
  double norm2=0;
  for(std::size_t j=0;j<theta.size();++j) {
    const double h=1e-5*(1+std::abs(theta[j]));auto plus=theta,minus=theta;
    plus[j]+=h;minus[j]-=h;double score=0;
    for(double x:g.data)score+=(cppunivariate::distributions::dbhatt(x,plus[0],plus[1],plus[2],true)-cppunivariate::distributions::dbhatt(x,minus[0],minus[1],minus[2],true))/(2*h);
    norm2+=score*score;
  }
  CHECK(std::sqrt(norm2)<1e-3);
}
TEST_CASE("bhatt: direct log tails and quantiles") {
  for(bool lower:{false,true})for(double lp:{-1e-6,-.6931471805599453,-10.,-100.,-600.}) {
    const double q=cppunivariate::distributions::qbhatt(lp,0,1,2,lower,true);
    CHECK(cppunivariate::distributions::pbhatt(q,0,1,2,lower,true)==doctest::Approx(lp).epsilon(1e-10));
  }
  for(double a:{0.,2.})for(bool lower:{false,true})for(double lp:{-1e-20,-1e-100}) {
    const double q=cppunivariate::distributions::qbhatt(lp,0,1,a,lower,true);
    CHECK(cppunivariate::distributions::pbhatt(q,0,1,a,lower,true)/lp==doctest::Approx(1).epsilon(1e-9));
  }
  CHECK(std::isinf(cppunivariate::distributions::qbhatt(0,0,1,2)));
  CHECK(std::isinf(cppunivariate::distributions::qbhatt(1,0,1,2)));
  CHECK(std::isnan(cppunivariate::distributions::dbhatt(0,0,-1,1)));
  CHECK(cppunivariate::distributions::dbhatt(0,0,1,0)==doctest::Approx(.39894228040143267794));
  CHECK(cppunivariate::distributions::pbhatt(0,0,1,0)==doctest::Approx(.5));
  CHECK(cppunivariate::distributions::qbhatt(.5,7,3,0)==doctest::Approx(7));
}
TEST_CASE("bhatt: faces, held subsets and invalid inputs") {
  const std::vector<double> uniform={-1,-.5,0,.5,1},constant={2,2,2};
  CHECK(cppunivariate::bhatt_mle(mltest::as_span(uniform)).status==cppunivariate::BhattStatus::UniformLimit);
  CHECK(cppunivariate::bhatt_mle(mltest::as_span(constant)).status==cppunivariate::BhattStatus::Unbounded);
  CHECK(cppunivariate::bhatt_mle(mltest::as_span(uniform),1e-8,500,1,2).status==cppunivariate::BhattStatus::Ok);
  CHECK(cppunivariate::bhatt_mle(mltest::as_span(uniform),1e-8,500,cppunivariate::special::kNaN,2).status==cppunivariate::BhattStatus::UniformLimit);
  CHECK(cppunivariate::bhatt_mle({}).status==cppunivariate::BhattStatus::InvalidInput);
}

#include "cppunivariate/distributions/bhatt/dpqr.hpp"

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("bhatt: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("bhatt",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dbhatt(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pbhatt(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qbhatt(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rbhatt(rng, p[0], p[1], p[2]); });
}

TEST_CASE("bhatt: every held golden subset preserves coefficients and likelihood") {
  const auto g=mltest::load_distribution_golden("bhatt");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(int mask=1;mask<8;++mask) {
    CAPTURE(mask);
    const auto r=cppunivariate::bhatt_mle(mltest::as_span(g.data),1e-10,1000,((mask & 2) ? g.estimates.at("sigma") : nan),((mask & 4) ? g.estimates.at("a") : nan),((mask & 1) ? g.estimates.at("mu") : nan));
    if(mask & 1)CHECK(r.mu==g.estimates.at("mu"));
    if(mask & 2)CHECK(r.sigma==g.estimates.at("sigma"));
    if(mask & 4)CHECK(r.a==g.estimates.at("a"));
    CHECK(r.status==cppunivariate::BhattStatus::Ok);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*g.data.size());
  }
}


// mpmath generator: import mpmath as mp; mp.mp.dps=100
// N=lambda z: mp.erfc(-z/mp.sqrt(2))/2; H=lambda z: z*N(z)+mp.exp(-z*z/2)/mp.sqrt(2*mp.pi); F=lambda x: (H(x+2)-H(x-2))/4; mp.findroot(lambda x: F(x)-mp.exp(mp.mpf(float(-mp.log(2)))),0)
TEST_CASE("bhatt: logarithmic quantile regression") {
  CHECK(cppunivariate::distributions::qbhatt(-0.6931471805599453, 0.0, 1.0, 2.0, true, true) == doctest::Approx(4.859187962299222e-17).epsilon(1e-11));
  CHECK(cppunivariate::distributions::qbhatt(-0.6931471805599453, 0.0, 1.0, 2.0, false, true) == doctest::Approx(-4.859187962299222e-17).epsilon(1e-11));
}

TEST_CASE("bhatt: support endpoints on log input") {
  CHECK(cppunivariate::distributions::qbhatt(cppunivariate::special::kNegInf, 0.0, 1.0, 2.0, true, true) == cppunivariate::special::kNegInf);
  CHECK(cppunivariate::distributions::qbhatt(0.0, 0.0, 1.0, 2.0, false, true) == cppunivariate::special::kNegInf);
  CHECK(cppunivariate::distributions::qbhatt(cppunivariate::special::kNegInf, 0.0, 1.0, 2.0, false, true) == cppunivariate::special::kPosInf);
  CHECK(cppunivariate::distributions::qbhatt(0.0, 0.0, 1.0, 2.0, true, true) == cppunivariate::special::kPosInf);
}

// At x=mp.mpf(float(1e100)), evaluate the same mpmath tails and
// the README log density; constants are also in the independent truth table.
TEST_CASE("bhatt: extreme observation log tails and density") {
  CHECK(cppunivariate::distributions::dbhatt(1e100, 0.0, 1.0, 2.0, true) == doctest::Approx(-5e+199).epsilon(1e-11));
  CHECK(cppunivariate::distributions::pbhatt(1e100, 0.0, 1.0, 2.0, true, true) == doctest::Approx(0.0).epsilon(1e-11));
  CHECK(cppunivariate::distributions::pbhatt(1e100, 0.0, 1.0, 2.0, false, true) == doctest::Approx(-5e+199).epsilon(1e-11));
}

// import mpmath as mp; mp.mp.dps=100
// a=mp.mpf(float(1e308)); s=mp.mpf(float(1e-308))
// -mp.log(2*a); mp.log(s/(2*a)/mp.sqrt(2*mp.pi))
// The omitted opposite normal endpoint has log mass < -1e1230.
TEST_CASE("bhatt: convolution width stays on the log scale") {
  CHECK(cppunivariate::distributions::dbhatt(0,0,1e-308,1e308,true) == doctest::Approx(-709.889355822726).epsilon(1e-12));
  CHECK(cppunivariate::distributions::pbhatt(1e308,0,1e-308,1e308,false,true) == doctest::Approx(-1420.0045029980968).epsilon(1e-12));
  CHECK(cppunivariate::distributions::qbhatt(0.75,0,1e-308,1e308,true,false) == doctest::Approx(5e307).epsilon(1e-10));
}

// The fitter consumes dbhatt and the original precision of its score helpers.
// Tail repairs must preserve the existing golden fit, without changing mle.hpp.
TEST_CASE("bhatt: central density preserves the golden fit") {
  const auto g=mltest::load_distribution_golden("bhatt");
  const auto r=cppunivariate::bhatt_mle(mltest::as_span(g.data));
#ifdef BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
  CHECK(std::abs((r.mu-g.estimates.at("mu"))/g.estimates.at("mu")) < mltest::kBhattGoldenTol);
  CHECK(std::abs((r.sigma-g.estimates.at("sigma"))/g.estimates.at("sigma")) < mltest::kBhattGoldenTol);
  CHECK(std::abs((r.a-g.estimates.at("a"))/g.estimates.at("a")) < mltest::kBhattGoldenTol);
#else
  CHECK(r.mu == doctest::Approx(g.estimates.at("mu")).epsilon(mltest::kBhattGoldenTol));
  CHECK(r.sigma == doctest::Approx(g.estimates.at("sigma")).epsilon(mltest::kBhattGoldenTol));
  CHECK(r.a == doctest::Approx(g.estimates.at("a")).epsilon(mltest::kBhattGoldenTol));
#endif
}


TEST_CASE("bhatt P3 normal boundary is attained with conditional coefficients") {
 std::vector<double> x(20,0); x.push_back(-10); x.push_back(10);
 const auto f=cppunivariate::bhatt_mle({x.data(),x.size()});
 REQUIRE(f.status==cppunivariate::BhattStatus::Ok); CHECK(f.a==0);
 const auto n=cppunivariate::norm_mle({x.data(),x.size()});
 CHECK(f.logLik==doctest::Approx(n.logLik));
 const auto held=cppunivariate::bhatt_mle({x.data(),x.size()},1e-8,500,4,0,1);
 CHECK(held.status==cppunivariate::BhattStatus::Ok); CHECK(held.mu==1); CHECK(held.sigma==4); CHECK(held.a==0);
}

TEST_CASE("bhatt: free fit is affine equivariant") {
  const auto g=mltest::load_distribution_golden("bhatt");
  const auto original=cppunivariate::bhatt_mle(mltest::as_span(g.data));
  std::vector<double> transformed;for(double x:g.data)transformed.push_back(1.25+2.5*x);
  const auto shifted=cppunivariate::bhatt_mle(mltest::as_span(transformed));
  REQUIRE(original.status==cppunivariate::BhattStatus::Ok);
  REQUIRE(shifted.status==cppunivariate::BhattStatus::Ok);
  CHECK(shifted.mu==doctest::Approx(1.25+2.5*original.mu).epsilon(1e-5));
  CHECK(shifted.sigma==doctest::Approx(2.5*original.sigma).epsilon(1e-5));
  CHECK(shifted.a==doctest::Approx(2.5*original.a).epsilon(1e-5));
  CHECK(shifted.logLik==doctest::Approx(original.logLik-g.data.size()*std::log(2.5)).epsilon(1e-10));
}

TEST_CASE("bhatt: face comparison cannot resolve rounded extreme objectives") {
  const double magnitude=1e20;
  CHECK(cppunivariate::compare_face(-magnitude,-magnitude,2e-8)==cppunivariate::FaceComparison::NumericalTie);
  CHECK(cppunivariate::compare_face(-magnitude,-2*magnitude,2e-8)==cppunivariate::FaceComparison::FirstWins);
  CHECK(cppunivariate::compare_face(-2*magnitude,-magnitude,2e-8)==cppunivariate::FaceComparison::SecondWins);
  CHECK(cppunivariate::compare_face(-10.,-10.,2e-8)==cppunivariate::FaceComparison::FirstWins);
}

TEST_CASE("bhatt: audited huge held-parameter magnitude") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  std::vector<double> x{1,2};
  cppunivariate::span s{x.data(),x.size()};
  const auto r=cppunivariate::bhatt_mle(s,1e-8,500,1e-10,nan,0);
  CHECK((r.status==cppunivariate::BhattStatus::Ok || r.status==cppunivariate::BhattStatus::IterationFailure));
  CHECK(!(r.status==cppunivariate::BhattStatus::Ok && r.a==0));
}
