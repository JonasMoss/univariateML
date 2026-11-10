#include "test_support.hpp"
#include "cppunivariate/distributions/sgomp/mle.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <vector>
TEST_CASE("sgomp tails and inversion") {
  CHECK(cppunivariate::distributions::dsgomp(1000,1,1,true)==doctest::Approx(-1000+std::log(2.0)));
  CHECK(cppunivariate::distributions::psgomp(1000,1,1,false,true)==doctest::Approx(-1000+std::log(2.0)));
  CHECK(cppunivariate::distributions::psgomp(1e-20,1,1,true,true)==doctest::Approx(std::log(1e-20)-1));
  const double extreme=cppunivariate::distributions::qsgomp(-1e300,1,1e308,true,true);
  CHECK(cppunivariate::distributions::psgomp(extreme,1,1e308,true,true)==doctest::Approx(-1e300).epsilon(1e-12));
  for (double eta:{0.,1e-10,1.,100.,1e100}) for (double lp:{-1.,-20.,-700.,-1000.}) for (bool lower:{false,true}) {
    const double q=cppunivariate::distributions::qsgomp(lp,2,eta,lower,true);
    if (q>0) CHECK(cppunivariate::distributions::psgomp(q,2,eta,lower,true)==doctest::Approx(lp).epsilon(1e-10));
  }
}
TEST_CASE("sgomp fitting faces fixed coordinates and collapse") {
  const std::vector<double> x{0,0,0,1,10},c{2,2,2},z{0,0};
  const auto f=cppunivariate::sgomp_mle({x.data(),x.size()});
  REQUIRE(f.status==cppunivariate::SgompStatus::Ok);
  CHECK(f.eta==0);
  CHECK(f.b==doctest::Approx(5./11));
  CHECK(cppunivariate::sgomp_mle({c.data(),c.size()}).status==cppunivariate::SgompStatus::Collapse);
  CHECK(cppunivariate::sgomp_mle({z.data(),z.size()},1e-10,200,1).eta==0);
  CHECK(cppunivariate::sgomp_mle({c.data(),c.size()},1e-10,200,1).status==cppunivariate::SgompStatus::Ok);
  CHECK(cppunivariate::sgomp_mle({c.data(),c.size()},1e-10,200,cppunivariate::sgomp_detail::nan(),2).status==cppunivariate::SgompStatus::Ok);
  CHECK(cppunivariate::sgomp_mle({x.data(),x.size()},1e-10,1).status==cppunivariate::SgompStatus::IterationFailure);
}
TEST_CASE("sgomp golden and independent score") {
  const auto g=mltest::load_distribution_golden("sgomp");
  const auto f=cppunivariate::sgomp_mle({g.data.data(),g.data.size()});
  REQUIRE(f.status==cppunivariate::SgompStatus::Ok);
  CHECK(f.b==doctest::Approx(g.estimates.at("b")).epsilon(mltest::kGolden_sgomp).scale(0));
  CHECK(f.eta==doctest::Approx(g.estimates.at("eta")).epsilon(mltest::kGolden_sgomp).scale(0));
  CHECK(f.logLik==doctest::Approx(g.logLik).epsilon(mltest::kGolden_sgomp).scale(0));
  double sb=0,se=0;
  for (double x:g.data) {
    const double v=std::exp(-f.b*x),a=-std::expm1(-f.b*x);
    sb+=1/f.b-x+f.eta*x*v+f.eta*x*v/(1+f.eta*a);
    se+=-v+a/(1+f.eta*a);
  }
  CHECK(std::abs(sb)<1e-3); CHECK(std::abs(se)<1e-3);
}
TEST_CASE("sgomp standalone sampler uses the same distribution") {
  cppunivariate::distributions::StdRng<> rng(719);
  double mean=0;
  for (int i=0;i<5000;++i) {
    const double x=cppunivariate::distributions::rsgomp(rng,2,3);
    mean+=cppunivariate::distributions::psgomp(x,2,3);
  }
  CHECK(std::abs(mean/5000-.5)<.02);
}

#include "cppunivariate/distributions/sgomp/dpqr.hpp"

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("sgomp: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("sgomp",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dsgomp(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::psgomp(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qsgomp(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rsgomp(rng, p[0], p[1]); });
}

TEST_CASE("sgomp: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("sgomp");
  const auto x=mltest::as_span(g.data);
  CHECK(cppunivariate::sgomp_mle(x,1e-8,1).status==cppunivariate::SgompStatus::IterationFailure);
}

TEST_CASE("sgomp: shape gain survives enormous common log likelihood") {
  for (double large:{1e17,1e100,1e300}) {
    const std::vector<double> x{1e-300,1,large};
    const auto f=cppunivariate::sgomp_mle(mltest::as_span(x),1e-10,200,1);
    REQUIRE(f.status==cppunivariate::SgompStatus::Ok);
    const double e1=std::exp(-1.0),linear=3*e1-e1*e1;
    const double oracle=2*(1-2*e1)/(linear+std::sqrt(linear*linear+4*(1-e1*e1)*(1-2*e1)));
    CHECK(f.eta==doctest::Approx(oracle).epsilon(1e-9));
    double score=0,gain=0;
    for (double v:x) {
      const double e=std::exp(-v),a=-std::expm1(-v);
      score+=a/(1+f.eta*a)-e;
      gain+=std::log1p(f.eta*a)-f.eta*e;
    }
    CHECK(std::abs(score)<1e-10);
    CHECK(gain>0.028);
    const auto face=cppunivariate::sgomp_detail::profile(mltest::as_span(x),1,0,200,1e-10);
    const auto best=cppunivariate::sgomp_detail::profile(mltest::as_span(x),1,f.eta,200,1e-10);
    CHECK(cppunivariate::sgomp_detail::difference(mltest::as_span(x),best,face)==doctest::Approx(gain).epsilon(1e-12));
  }
}

TEST_CASE("sgomp: a positive face score cannot be hidden by the tie margin") {
  const std::vector<double> x{std::log(2.0)+1e-6};
  const auto f=cppunivariate::sgomp_mle(mltest::as_span(x),1e-10,200,1);
  REQUIRE(f.status==cppunivariate::SgompStatus::Ok);
  const double e=std::exp(-x[0]),a=-std::expm1(-x[0]);
  CHECK(f.eta>0);
  CHECK(f.eta==doctest::Approx((a-e)/(a*e)).epsilon(1e-9));
  const double gain=std::log1p(f.eta*a)-f.eta*e;
  CHECK(gain>0);
  CHECK(gain<1e-8);
}

TEST_CASE("sgomp: ranking tie margin never grows with the common likelihood") {
  const std::vector<double> x{1e-300,1,1e300};
  const cppunivariate::sgomp_detail::Candidate a{1,std::log(.2),-1e300,0,true};
  const cppunivariate::sgomp_detail::Candidate b{1,-std::numeric_limits<double>::infinity(),-1e300,0,true};
  CHECK(cppunivariate::sgomp_detail::ranking_difference(mltest::as_span(x),a,b)>.027);
  CHECK(cppunivariate::sgomp_detail::ranking_difference(mltest::as_span(x),b,a)<-.027);
}

TEST_CASE("sgomp certificate: held eta strictly concave rate root") {
  const std::vector<double> x{0,.1,.3,2,9};
  for(double eta:{0.,.01,2.,1e10}) {
    const auto r=cppunivariate::sgomp_mle(mltest::as_span(x),1e-10,200,cppunivariate::sgomp_detail::nan(),eta);
    REQUIRE(r.status==cppunivariate::SgompStatus::Ok);
    double score=0;
    for(double v:x){const double e=std::exp(-r.b*v),a=-std::expm1(-r.b*v);score+=1/r.b-v+eta*v*e*(1+1/(1+eta*a));}
    CHECK(std::abs(score*r.b)<1e-8);
  }
}

TEST_CASE("sgomp certificate: unfinished bracket preserves a numerical witness") {
  const std::vector<double> x{1,1.01};
  const auto r=cppunivariate::sgomp_mle(mltest::as_span(x),1e-10,1);
  CHECK(r.status==cppunivariate::SgompStatus::IterationFailure);
  CHECK_FALSE(r.stop_detail.budget_exhausted);
  REQUIRE(!r.stop_detail.candidates.empty());
  CHECK(std::isfinite(r.stop_detail.candidates.front().loglik_lower));
}

TEST_CASE("sgomp: large replication preserves the held exponential mean") {
  std::vector<double> x(10000000,.1);x.back()=1;
  const long double reference=x.size()/((x.size()-1)*static_cast<long double>(.1)+1);
  const auto r=cppunivariate::sgomp_mle(mltest::as_span(x),1e-10,200,NAN,0);
  REQUIRE(r.status==cppunivariate::SgompStatus::Ok);
  CHECK(std::abs(r.b/reference-1)<=1e-10L);CHECK(r.eta==0);
}
