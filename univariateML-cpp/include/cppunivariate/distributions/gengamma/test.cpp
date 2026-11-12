#include "test_support.hpp"
#include "cppunivariate/distributions/gengamma/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include "cppunivariate/distributions/gengamma/held.hpp"
#include "cppunivariate/distributions/gengamma/held_joint.hpp"
#include "cppunivariate/distributions/gengamma/shape_score.hpp"
#include "cppunivariate/distributions/gengamma/profile_root.hpp"
#include "cppunivariate/distributions/gengamma/local.hpp"
#include "cppunivariate/distributions/gengamma/shape_candidate.hpp"
#include "cppunivariate/distributions/gengamma/shape_scan.hpp"
#include "cppunivariate/distributions/gengamma/free_certificate.hpp"
#include "cppunivariate/distributions/gengamma/face_certificate.hpp"
#include "cppunivariate/distributions/gengamma/zero_certificate.hpp"
#include "cppunivariate/distributions/gengamma/mle.hpp"
#include "cppunivariate/distributions/invtrgamma/mle.hpp"

TEST_CASE("gengamma: normal seam and reflection") {
  for (double w : {-6.,-1.,0.,1.,6.}) {
    const double x=std::exp(w);
    const double center=cppunivariate::distributions::pgengamma(x,0,1,0,true,true);
    for (double q : {1e-8,1e-6,1e-4}) {
      const double positive=cppunivariate::distributions::pgengamma(x,0,1,q,true,true);
      const double negative=cppunivariate::distributions::pgengamma(x,0,1,-q,true,true);
      CHECK(std::abs(positive-center)<q*100);
      CHECK(std::abs(negative-center)<q*100);
      const double reflected=cppunivariate::distributions::pgengamma(1/x,0,1,-q,false,true);
      CHECK(positive==doctest::Approx(reflected).epsilon(1e-11));
    }
  }
  CHECK(cppunivariate::distributions::pgengamma(std::exp(1.),0,1,1e-8,true,false)==
        doctest::Approx(0.841344747278396568).epsilon(2e-12));
}

TEST_CASE("gengamma: seeded sampler matches CDF at fixed landmarks") {
  for (double q : {-1.,-0.01,-1e-8,0.,1e-8,0.01,1.}) {
    cppunivariate::distributions::StdRng<> rng(70191);
    constexpr int n=12000;
    int count[3]={0,0,0};
    const double cut[3]={std::exp(-1.),1.,std::exp(1.)};
    for (int i=0;i<n;++i) {
      const double x=cppunivariate::distributions::rgengamma(rng,0,1,q);
      REQUIRE(std::isfinite(x));
      CHECK(x>0);
      for(int j=0;j<3;++j) if(x<=cut[j]) ++count[j];
    }
    for(int j=0;j<3;++j) {
      const double p=cppunivariate::distributions::pgengamma(cut[j],0,1,q,true,false);
      const double z=std::abs(static_cast<double>(count[j])/n-p);
      CHECK(z<5*std::sqrt(p*(1-p)/n)+1.0/n);
    }
  }
}

TEST_CASE("gengamma: high precision density, tails, quantiles and sampling") {
  mltest::check_dpqr_truth("gengamma",
    [](double x,const std::vector<double> &t,bool log) {
      return cppunivariate::distributions::dgengamma(x,t[0],t[1],t[2],log);
    },
    [](double x,const std::vector<double> &t,bool lower,bool log) {
      return cppunivariate::distributions::pgengamma(x,t[0],t[1],t[2],lower,log);
    },
    [](double p,const std::vector<double> &t,bool lower,bool log) {
      return cppunivariate::distributions::qgengamma(p,t[0],t[1],t[2],lower,log);
    },
    [](auto &rng,const std::vector<double> &t) {
      return cppunivariate::distributions::rgengamma(rng,t[0],t[1],t[2]);
    });
}

TEST_CASE("gengamma: very large signed shape retains log gamma tails") {
  const double q=1e200, mu=-2*std::log(q), sigma=q, loga=mu;
  // These coordinates make X a unit-scale Gamma(q^-2) variate.  At x=1,
  // its upper tail tends to q^-2 * E1(1), with relative error O(q^-2).
  const double log_e1_at_one=std::log(0.21938393439552027368);
  const double upper=cppunivariate::distributions::pgengamma(1,mu,sigma,q,false,true);
  CHECK(upper==doctest::Approx(loga+log_e1_at_one).epsilon(1e-13));
  const double density=cppunivariate::distributions::dgengamma(1,mu,sigma,q,true);
  CHECK(density==doctest::Approx(loga-1).epsilon(1e-13));
  const double x=cppunivariate::distributions::qgengamma(upper,mu,sigma,q,false,true);
  CHECK(x==doctest::Approx(1).epsilon(1e-10));
  const double reflected=cppunivariate::distributions::pgengamma(1,-mu,sigma,-q,true,true);
  CHECK(reflected==doctest::Approx(upper).epsilon(1e-13));
  CHECK(cppunivariate::distributions::qgengamma(upper,-mu,sigma,-q,true,true)==
        doctest::Approx(1).epsilon(1e-10));
  CHECK(cppunivariate::distributions::qgengamma(-10000,0,1e-204,q,true,true)==
        doctest::Approx(std::exp(-1.)).epsilon(1e-11));
}

TEST_CASE("gengamma: numerical transition and subnormal shape") {
  for (double sign : {-1.,1.}) {
    const double q=sign*0.25;
    const double next=std::nextafter(q,sign*1.);
    for (double w : {-6.,0.,1.,6.}) {
      const double x=std::exp(w);
      const double a=cppunivariate::distributions::pgengamma(x,0,1,q,true,true);
      const double b=cppunivariate::distributions::pgengamma(x,0,1,next,true,true);
      CHECK(a==doctest::Approx(b).epsilon(2e-11));
    }
    const double tiny=sign*1e-320;
    const double p=cppunivariate::distributions::pgengamma(std::exp(1.),0,1,tiny,true,false);
    CHECK(p==doctest::Approx(0.8413447460685429486).epsilon(1e-12));
    cppunivariate::distributions::StdRng<> rng(7432);
    CHECK(std::isfinite(cppunivariate::distributions::rgengamma(rng,0,1,tiny)));
  }
}

TEST_CASE("gengamma: huge shape and tiny scale retain standardized tails") {
  const double q=1e200, sigma=1e-200, mu=1e200;
  const double lower=cppunivariate::distributions::pgengamma(1,mu,sigma,q,true,true);
  const double density=cppunivariate::distributions::dgengamma(1,mu,sigma,q,true);
  CHECK(lower==doctest::Approx(-1e200).epsilon(1e-13));
  CHECK(density==doctest::Approx(-1e200).epsilon(1e-13));
}

TEST_CASE("gengamma: large-shape transition") {
  const double q=1e10, next=std::nextafter(q,cppunivariate::special::kPosInf);
  for (double shape : {q,next}) {
    const double mu=-2*std::log(shape), sigma=shape;
    const double upper=cppunivariate::distributions::pgengamma(1,mu,sigma,shape,false,true);
    CHECK(upper==doctest::Approx(mu+std::log(0.21938393439552027368)).epsilon(1e-12));
  }
}

TEST_CASE("gengamma: small gamma shape preserves the upper tail below gamma underflow") {
  const double shape=1e8, mu=-2*std::log(shape), sigma=shape;
  const double x=std::exp(-40.);
  const double lp=cppunivariate::distributions::pgengamma(x,mu,sigma,shape,false,true);
  const double reference=mu+std::log(40.-0.57721566490153286061);
  CHECK(lp==doctest::Approx(reference).epsilon(1e-12));
  CHECK(cppunivariate::distributions::qgengamma(lp,mu,sigma,shape,false,true)==
        doctest::Approx(x).epsilon(1e-9));
}

TEST_CASE("gengamma: overflowed coordinates and exact zero endpoint") {
  const double q=1e8, sigma=1e-200, mu=5e115;
  const double expected=-5e307;
  CHECK(cppunivariate::distributions::pgengamma(1,mu,sigma,q,true,true)==
        doctest::Approx(expected).epsilon(1e-13));
  CHECK(cppunivariate::distributions::dgengamma(1,mu,sigma,q,true)==
        doctest::Approx(expected).epsilon(1e-13));
  const double tiny=std::ldexp(1.,-600), large=std::ldexp(1.,600);
  CHECK(cppunivariate::distributions::dgengamma(0,0,large,tiny,true)==
        cppunivariate::special::kPosInf);
}

TEST_CASE("gengamma: overflowed shape product retains finite log tails") {
  for (double sign : {-1.,1.}) {
    const double Q=sign*1e8, mu=sign*5e307;
    const double expected=-5e299;
    CHECK(cppunivariate::distributions::dgengamma(1,mu,1,Q,true)==
          doctest::Approx(expected).epsilon(1e-13));
    CHECK(cppunivariate::distributions::pgengamma(1,mu,1,Q,Q>0,true)==
          doctest::Approx(expected).epsilon(1e-13));
  }
}

TEST_CASE("gengamma: large-shape inverse crosses the E1 transition") {
  const double Q=1e12, mu=-2*std::log(Q), sigma=Q;
  for (double lx : {-36.2,-36.1,-36.0,-35.9,-35.8}) {
    const double x=std::exp(lx);
    const double lp=cppunivariate::distributions::pgengamma(x,mu,sigma,Q,false,true);
    CHECK(cppunivariate::distributions::qgengamma(lp,mu,sigma,Q,false,true)==
          doctest::Approx(x).epsilon(1e-9));
  }
}

TEST_CASE("gengamma: log-location and log-scale transformation") {
  const double mu=.75, sigma=1.3;
  for (double Q : {-3.,-.01,0.,.01,3.}) {
    for (double w : {-3.,0.,2.}) {
      const double x=std::exp(mu+sigma*w), base=std::exp(w);
      const double d=cppunivariate::distributions::dgengamma(x,mu,sigma,Q,true);
      const double d0=cppunivariate::distributions::dgengamma(base,0,1,Q,true);
      CHECK(d==doctest::Approx(d0+w-std::log(sigma)-std::log(x)).epsilon(1e-11));
      for (bool lower : {false,true}) {
        const double p=cppunivariate::distributions::pgengamma(x,mu,sigma,Q,lower,true);
        const double p0=cppunivariate::distributions::pgengamma(base,0,1,Q,lower,true);
        CHECK(p==doctest::Approx(p0).epsilon(1e-11));
        if (p<0 && std::isfinite(p))
          CHECK(cppunivariate::distributions::qgengamma(p,mu,sigma,Q,lower,true)==
                doctest::Approx(x).epsilon(1e-9));
      }
    }
  }
}

TEST_CASE("gengamma: gamma quantile keeps sub-ulp correction through scaling") {
  for (double sigma : {1e6,std::ldexp(1.,1000)}) {
    const double mu=36*sigma;
    const double correction=0.5*std::exp(-36.)*sigma;
    const double upper=cppunivariate::distributions::qgengamma(-36,mu,sigma,1,true,true);
    if (std::isinf(std::exp(correction))) CHECK(upper==cppunivariate::special::kPosInf);
    else CHECK(upper==doctest::Approx(std::exp(correction)).epsilon(1e-13));
    CHECK(cppunivariate::distributions::qgengamma(-36,-mu,sigma,-1,false,true)==
          doctest::Approx(std::exp(-correction)).epsilon(1e-13));
  }
}

TEST_CASE("gengamma: seeded log moments") {
  constexpr int n=12000;
  constexpr double euler=0.57721566490153286061;
  constexpr double pi2_over_six=1.6449340668482264365;
  for (double Q : {-1.,-.01,0.,.01,1.}) {
    cppunivariate::distributions::StdRng<> rng(20261110);
    double sum=0, sum2=0;
    for (int i=0;i<n;++i) {
      const double v=std::log(cppunivariate::distributions::rgengamma(rng,0,1,Q));
      REQUIRE(std::isfinite(v));
      sum+=v; sum2+=v*v;
    }
    const double mean=std::abs(Q)==1 ? -euler/Q : -Q/2-Q*Q*Q/12;
    const double variance=std::abs(Q)==1 ? pi2_over_six : 1+Q*Q/2;
    CHECK(std::abs(sum/n-mean)<6*std::sqrt(variance/n));
    CHECK(std::abs(sum2/n-(variance+mean*mean))<(std::abs(Q)==1 ? .25 : .12));
  }
}

TEST_CASE("gengamma: conditional location and scale") {
  const std::vector<double> y{-1.4, -0.3, 0.2, 0.6, 1.1, 1.8};
  const cppunivariate::span data{y.data(), y.size()};
  for (double q : {-2., -0.2, -1e-8, 0., 1e-8, 0.2, 2.}) {
    const auto loc = cppunivariate::gengamma_fit_detail::location(data, 1.2, q);
    REQUIRE(loc.status == cppunivariate::gengamma_fit_detail::ConditionalStatus::Ok);
    const auto sc = cppunivariate::gengamma_fit_detail::scale(data, loc.value, q, 1e-12, 100);
    REQUIRE(sc.status == cppunivariate::gengamma_fit_detail::ConditionalStatus::Ok);
    double score_mu = 0, score_sigma = 0;
    for (double v : y) {
      const double w = (v - loc.value) / 1.2;
      const double z = q * w;
      score_mu += w * (z == 0 ? 1 : std::expm1(z) / z);
      const double ws = (v - loc.value) / sc.value;
      const double zs = q * ws;
      score_sigma += ws * ws * (zs == 0 ? 1 : std::expm1(zs) / zs) - 1;
    }
    CHECK(std::abs(score_mu) < 1e-10);
    CHECK(std::abs(score_sigma) < 1e-9);
  }
  const std::vector<double> constant{2., 2., 2.};
  const cppunivariate::span repeated{constant.data(), constant.size()};
  CHECK(cppunivariate::gengamma_fit_detail::scale(repeated, 2., 0.5, 1e-10, 100).status ==
        cppunivariate::gengamma_fit_detail::ConditionalStatus::Unbounded);
  CHECK(cppunivariate::gengamma_fit_detail::location(repeated, 1., -1e-8).value == 2.);
  CHECK(cppunivariate::gengamma_fit_detail::scale(data, 0., 1., 1e-10, 1).status ==
        cppunivariate::gengamma_fit_detail::ConditionalStatus::IterationFailure);
}

TEST_CASE("gengamma: fixed shape joint profile") {
  const std::vector<double> y{-1.4, -0.3, 0.2, 0.6, 1.1, 1.8};
  const cppunivariate::span data{y.data(), y.size()};
  for (double q : {-2., -0.2, -1e-8, 0., 1e-8, 0.2, 2.}) {
    const auto fit = cppunivariate::gengamma_fit_detail::location_scale(data, q, 1e-10, 100);
    REQUIRE(fit.status == cppunivariate::gengamma_fit_detail::ConditionalStatus::Ok);
    double score_mu = 0, score_sigma = 0;
    for (double v : y) {
      const double w = (v - fit.mu) / fit.sigma;
      const double z = q * w;
      const double exprel = z == 0 ? 1 : std::expm1(z) / z;
      score_mu += w * exprel;
      score_sigma += w * w * exprel - 1;
    }
    CHECK(std::abs(score_mu) < 1e-9);
    CHECK(std::abs(score_sigma) < 1e-8);
  }
  const std::vector<double> constant{2., 2., 2.};
  CHECK(cppunivariate::gengamma_fit_detail::location_scale(
      {constant.data(), constant.size()}, -1., 1e-10, 100).status ==
      cppunivariate::gengamma_fit_detail::ConditionalStatus::Unbounded);
}

TEST_CASE("gengamma: shape score is smooth across the lognormal seam") {
  const std::vector<double> y{-1.4, -0.3, 0.2, 0.6, 1.1, 1.8};
  const cppunivariate::span data{y.data(), y.size()};
  const double mu=0.2, sigma=1.3;
  double expected=0;
  for (double v : y) expected -= std::pow((v-mu)/sigma, 3)/(6*y.size());
  const auto seam=cppunivariate::gengamma_fit_detail::shape_score(data,mu,sigma,0.);
  REQUIRE(seam.valid);
  CHECK(seam.value == doctest::Approx(expected).epsilon(1e-13));
  for (double q : {-0.4,-0.1,-1e-8,1e-8,0.1,0.4}) {
    const auto score=cppunivariate::gengamma_fit_detail::shape_score(data,mu,sigma,q);
    REQUIRE(score.valid);
    const double h=1e-5;
    double llplus=0, llminus=0;
    for (double v : y) {
      llplus += cppunivariate::distributions::dgengamma(std::exp(v),mu,sigma,q+h,true);
      llminus += cppunivariate::distributions::dgengamma(std::exp(v),mu,sigma,q-h,true);
    }
    const double fd=(llplus-llminus)/(2*h*y.size());
    CHECK(std::abs(score.value-fd)<2e-8);
  }
}

TEST_CASE("gengamma: conditional brackets reject unrepresentable tolerance") {
  const std::vector<double> y{-1.4, -0.3, 0.2, 0.6, 1.1, 1.8};
  const cppunivariate::span data{y.data(), y.size()};
  CHECK(cppunivariate::gengamma_fit_detail::scale(data, 0.2, 0.3, 1e-30, 100).status ==
        cppunivariate::gengamma_fit_detail::ConditionalStatus::IterationFailure);
  CHECK(cppunivariate::gengamma_fit_detail::location_scale(data, 0.3, 1e-30, 100).status ==
        cppunivariate::gengamma_fit_detail::ConditionalStatus::IterationFailure);
}

TEST_CASE("gengamma: held-Q zero reduces to normal likelihood") {
  const std::vector<double> y{-1., 0., 1., 2.};
  const cppunivariate::span data{y.data(), y.size()};
  const auto joint=cppunivariate::gengamma_fit_detail::location_scale(data,0.,1e-10,100);
  REQUIRE(joint.status == cppunivariate::gengamma_fit_detail::ConditionalStatus::Ok);
  CHECK(joint.mu == doctest::Approx(0.5));
  CHECK(joint.sigma == doctest::Approx(std::sqrt(1.25)).epsilon(1e-14));
  const auto scale=cppunivariate::gengamma_fit_detail::scale(data,0.,0.,1e-10,100);
  REQUIRE(scale.status == cppunivariate::gengamma_fit_detail::ConditionalStatus::Ok);
  CHECK(scale.value == doctest::Approx(std::sqrt(1.5)).epsilon(1e-10));
}

TEST_CASE("gengamma: signed profile encloses a near-seam local maximum") {
  const std::vector<double> reflected{2., 1., 0., -1., -2.1};
  cppunivariate::trgamma_detail::LogData d;
  REQUIRE(cppunivariate::trgamma_detail::standardize({reflected.data(),reflected.size()},d));
  int eval=0;
  double left=1e-5;
  auto a=cppunivariate::trgamma_detail::profile(d,left,eval,100);
  REQUIRE(a.slope>0);
  double right=left;
  cppunivariate::trgamma_detail::ProfilePoint b=a;
  for (int i=0;i<100 && b.slope>0;++i) {
    right*=1.5;
    b=cppunivariate::trgamma_detail::profile(d,right,eval,100);
  }
  REQUIRE(b.slope<0);
  const auto fit=cppunivariate::gengamma_fit_detail::profile_root(d,left,right,1e-10,100);
  REQUIRE(fit.ok);
  CHECK(fit.q>0);
  CHECK(fit.sigma>0);
  CHECK(fit.t>left);
  CHECK(fit.t<right);
  const auto at=cppunivariate::trgamma_detail::profile(d,fit.t,eval,100);
  CHECK(std::abs(at.slope)<1e-8);
}

TEST_CASE("gengamma: local profile refuses invalid or exhausted brackets") {
  const std::vector<double> y{2., 1., 0., -1., -2.1};
  cppunivariate::trgamma_detail::LogData d;
  REQUIRE(cppunivariate::trgamma_detail::standardize({y.data(),y.size()},d));
  CHECK_FALSE(cppunivariate::gengamma_fit_detail::profile_root(d,1.,1.,1e-10,100).ok);
  CHECK_FALSE(cppunivariate::gengamma_fit_detail::profile_root(d,1e-5,1.,1e-10,1).ok);
}

TEST_CASE("gengamma: local information check refines the symmetric seam") {
  const std::vector<double> y{-2., -1., 0., 1., 2.};
  const cppunivariate::span data{y.data(),y.size()};
  const double sd=std::sqrt(2.);
  std::array<double,3> p{0.001,sd*1.001,0.001};
  REQUIRE(cppunivariate::gengamma_fit_detail::polish_local(
      data,0.,sd,p,{true,true,true},1e-10,100));
  CHECK(std::abs(p[0])<1e-10);
  CHECK(p[1]==doctest::Approx(sd).epsilon(1e-10));
  CHECK(std::abs(p[2])<1e-10);
  std::array<double,3> invalid{0.,sd,0.};
  CHECK_FALSE(cppunivariate::gengamma_fit_detail::polish_local(
      data,0.,sd,invalid,{true,true,true},1e-30,100));
}

TEST_CASE("gengamma: held location and scale nominate the seam") {
  const std::vector<double> y{-2., -1., 0., 1., 2.};
  const cppunivariate::span data{y.data(),y.size()};
  const auto left=cppunivariate::gengamma_fit_detail::shape_point(data,-0.5,0.,1.,false,false,1e-10,100);
  const auto right=cppunivariate::gengamma_fit_detail::shape_point(data,0.5,0.,1.,false,false,1e-10,100);
  REQUIRE(left.valid);
  REQUIRE(right.valid);
  REQUIRE(left.score>0);
  REQUIRE(right.score<0);
  const auto fit=cppunivariate::gengamma_fit_detail::shape_candidate(data,left,right,false,false,0.,1.,1e-10,100);
  REQUIRE(fit.valid);
  CHECK(std::abs(fit.q)<1e-10);
  CHECK(fit.mu==0.);
  CHECK(fit.sigma==1.);
}

TEST_CASE("gengamma: shape scan retains a checked symmetric candidate") {
  const std::vector<double> y{-2., -1., 0., 1., 2.};
  const cppunivariate::span data{y.data(),y.size()};
  const auto candidates=cppunivariate::gengamma_fit_detail::shape_candidates(
      data,0.,1.,false,false,0.,1.,1e-3,1.,1e-10,100);
  REQUIRE(candidates.scan_completed);
  REQUIRE_FALSE(candidates.values.empty());
  CHECK(std::abs(candidates.values.front().q)<1e-10);
}

TEST_CASE("gengamma: transformed-gamma side can certify a separated candidate") {
  const auto g=mltest::load_distribution_golden("trgamma");
  const auto parent=cppunivariate::trgamma_mle(mltest::as_span(g.data));
  REQUIRE(parent.status==cppunivariate::TrGammaStatus::Ok);
  std::vector<double> y; y.reserve(g.data.size());
  for(double x:g.data)y.push_back(std::log(x));
  const double q=1/std::sqrt(parent.shape1);
  const double sigma=q/parent.shape2;
  const double mu=parent.log_scale+std::log(parent.shape1)/parent.shape2;
  const cppunivariate::span data{y.data(),y.size()};
  const auto point=cppunivariate::gengamma_fit_detail::shape_point(data,q,mu,sigma,false,false,1e-10,100);
  REQUIRE(point.valid);
  CHECK(point.mean_loglik*y.size()==doctest::Approx(parent.logLik).epsilon(1e-10));
  CHECK(cppunivariate::gengamma_fit_detail::certify_free_candidate(data,point,1e-10,100));
  CHECK_FALSE(cppunivariate::gengamma_fit_detail::certify_free_candidate(data,point,1e-10,1));
}

TEST_CASE("gengamma: strict signed face certificate") {
  const std::vector<double> y{0.,std::log(2.),std::log(3.)};
  std::vector<double> reflected;
  for(double v:y)reflected.push_back(-v);
  const auto power=cppunivariate::gengamma_fit_detail::strict_winning_face({y.data(),y.size()},100);
  const auto pareto=cppunivariate::gengamma_fit_detail::strict_winning_face({reflected.data(),reflected.size()},100);
  CHECK(power.winner==cppunivariate::gengamma_fit_detail::FaceWinner::Power);
  CHECK(pareto.winner==cppunivariate::gengamma_fit_detail::FaceWinner::Pareto);
  const std::vector<double> equal_faces{-1.,1.};
  CHECK(cppunivariate::gengamma_fit_detail::strict_winning_face({equal_faces.data(),equal_faces.size()},100).winner==
        cppunivariate::gengamma_fit_detail::FaceWinner::None);
}
TEST_CASE("gengamma: fixed Q and all held subsets") {
  cppunivariate::distributions::StdRng<> rng(12470);
  std::vector<double> x;
  for(int i=0;i<120;++i)x.push_back(cppunivariate::distributions::rgengamma(rng,0.,1.,0.4));
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(int mask=0;mask<8;++mask) {
    const auto r=cppunivariate::gengamma_mle({x.data(),x.size()},1e-10,100,
        mask&1?0.:nan,mask&2?1.:nan,mask&4?0.4:nan);
    REQUIRE(r.status==cppunivariate::GengammaStatus::Ok);
    if(mask&1)CHECK(r.mu==0.);
    if(mask&2)CHECK(r.sigma==1.);
    if(mask&4)CHECK(r.Q==0.4);
    double ll=0;
    for(double value:x)ll+=cppunivariate::distributions::dgengamma(value,r.mu,r.sigma,r.Q,true);
    CHECK(r.logLik==doctest::Approx(ll).epsilon(1e-10));
  }
}

TEST_CASE("gengamma: parent fits map to signed coordinates") {
  const auto golden=mltest::load_distribution_golden("trgamma");
  const auto p=cppunivariate::trgamma_mle(mltest::as_span(golden.data));
  REQUIRE(p.status==cppunivariate::TrGammaStatus::Ok);
  const auto g=cppunivariate::gengamma_mle(mltest::as_span(golden.data));
  REQUIRE(g.status==cppunivariate::GengammaStatus::Ok);
  const double shape=1/std::sqrt(p.shape1);
  CHECK(g.Q==doctest::Approx(shape).epsilon(2e-9));
  CHECK(g.sigma==doctest::Approx(shape/p.shape2).epsilon(2e-9));
  CHECK(g.mu==doctest::Approx(p.log_scale+std::log(p.shape1)/p.shape2).epsilon(2e-9));
  CHECK(g.logLik==doctest::Approx(p.logLik).epsilon(1e-10));
  std::vector<double> inverse;for(double x:golden.data)inverse.push_back(1/x);
  const auto r=cppunivariate::gengamma_mle({inverse.data(),inverse.size()});
  REQUIRE(r.status==cppunivariate::GengammaStatus::Ok);
  const auto inverse_parent=cppunivariate::invtrgamma_mle({inverse.data(),inverse.size()});
  REQUIRE(inverse_parent.status==cppunivariate::InvTrGammaStatus::Ok);
  CHECK(r.Q==doctest::Approx(-g.Q).epsilon(2e-9));
  CHECK(r.sigma==doctest::Approx(g.sigma).epsilon(2e-9));
  CHECK(r.mu==doctest::Approx(-g.mu).epsilon(2e-9));
  const double inverse_q=-1/std::sqrt(inverse_parent.shape1);
  CHECK(r.Q==doctest::Approx(inverse_q).epsilon(2e-9));
  CHECK(r.sigma==doctest::Approx(-inverse_q/inverse_parent.shape2).epsilon(2e-9));
  CHECK(r.mu==doctest::Approx(inverse_parent.log_scale-std::log(inverse_parent.shape1)/inverse_parent.shape2).epsilon(2e-9));
}

TEST_CASE("gengamma: constant data and strict power and Pareto faces") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> one(5,1.);
  CHECK(cppunivariate::gengamma_mle({one.data(),one.size()}).status==cppunivariate::GengammaStatus::Unbounded);
  CHECK(cppunivariate::gengamma_mle({one.data(),one.size()},1e-10,100,0.,nan,nan).status==cppunivariate::GengammaStatus::Unbounded);
  const auto held=cppunivariate::gengamma_mle({one.data(),one.size()},1e-10,100,nan,1.,nan);
  REQUIRE(held.status==cppunivariate::GengammaStatus::Ok);
  CHECK(held.mu==0.);
  CHECK(held.Q==0.);
  const std::vector<double> x{1.,2.,3.},inverse{1.,.5,1./3.};
  CHECK(cppunivariate::gengamma_mle({x.data(),x.size()}).status==cppunivariate::GengammaStatus::PowerBoundary);
  CHECK(cppunivariate::gengamma_mle({inverse.data(),inverse.size()}).status==cppunivariate::GengammaStatus::ParetoBoundary);
}

TEST_CASE("gengamma: signed seam and zero-skew samples") {
  const int n=101;
  std::vector<double> z;
  double square=0;
  for(int i=0;i<n;++i){const double v=-3.+6.*i/(n-1);z.push_back(v);square+=v*v/n;}
  for(double perturbation:{-1e-4,-1e-8,0.,1e-8,1e-4}) {
    std::vector<double> x;
    for(double v:z)x.push_back(std::exp(v+perturbation*(v*v-square)));
    const auto r=cppunivariate::gengamma_mle({x.data(),x.size()});
    REQUIRE(r.status==cppunivariate::GengammaStatus::Ok);
    if(perturbation==0)CHECK(std::abs(r.Q)<1e-8);
    else {
      CHECK(r.Q*perturbation<0);
      CHECK(std::abs(r.Q)>1e-10);
      CHECK(std::abs(r.Q)<0.01);
    }
  }
}

TEST_CASE("gengamma: log-input accuracy is checked against fitted scale") {
  std::vector<double> stable,ill_conditioned;
  for(double z:{-2.,-1.,0.,1.,2.}) {
    stable.push_back(std::exp(1e-6*z));
    ill_conditioned.push_back(std::exp(100.+1e-10*z));
  }
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto good=cppunivariate::gengamma_mle({stable.data(),stable.size()},1e-10,100,nan,nan,0.);
  const auto bad=cppunivariate::gengamma_mle({ill_conditioned.data(),ill_conditioned.size()},1e-10,100,nan,nan,0.);
  CHECK(good.status==cppunivariate::GengammaStatus::Ok);
  CHECK(bad.status==cppunivariate::GengammaStatus::IterationFailure);
}

TEST_CASE("gengamma: shape-score Stirling switch is continuous") {
  const std::vector<double> y{-1.4,-0.3,0.2,0.6,1.1,1.8};
  const cppunivariate::span data{y.data(),y.size()};
  for(double sign:{-1.,1.}) {
    const double q=sign*0.25;
    const auto a=cppunivariate::gengamma_fit_detail::shape_score(data,0.2,1.3,q);
    const auto b=cppunivariate::gengamma_fit_detail::shape_score(data,0.2,1.3,std::nextafter(q,sign));
    REQUIRE(a.valid);
    REQUIRE(b.valid);
    CHECK(std::abs(a.value-b.value)<1e-11);
  }
}

TEST_CASE("gengamma: shallow seam keeps an uncertified nonzero candidate") {
  const int n=101;
  std::vector<double> z,x;double square=0;
  for(int i=0;i<n;++i){const double v=-3.+6.*i/(n-1);z.push_back(v);square+=v*v/n;}
  for(double v:z)x.push_back(std::exp(v+1e-8*(v*v-square)));
  const auto fit=cppunivariate::gengamma_mle({x.data(),x.size()});
  REQUIRE(fit.status==cppunivariate::GengammaStatus::Ok);
  CHECK(fit.Q<0);
  CHECK(std::abs(fit.Q)>1e-10);
  CHECK_FALSE(fit.certified);
}

TEST_CASE("gengamma: certificate allowance retains a checked candidate") {
  const std::vector<double> y{-2., -1., 0., 1., 2.};
  const auto candidates=cppunivariate::gengamma_fit_detail::shape_candidates(
      {y.data(),y.size()},0.,1.,false,false,0.,1.,1e-3,1.,1e-10,100,1);
  CHECK_FALSE(candidates.scan_completed);
  REQUIRE(candidates.values.size()==1);
  CHECK(std::abs(candidates.values[0].q)<1e-10);
}

TEST_CASE("gengamma: log-location equivariance") {
  const auto golden=mltest::load_distribution_golden("trgamma");
  const auto base=cppunivariate::gengamma_mle(mltest::as_span(golden.data));
  REQUIRE(base.status==cppunivariate::GengammaStatus::Ok);
  std::vector<double> moved;
  for(double x:golden.data)moved.push_back(x*1e50);
  const auto fit=cppunivariate::gengamma_mle({moved.data(),moved.size()});
  REQUIRE(fit.status==cppunivariate::GengammaStatus::Ok);
  CHECK(std::abs(fit.mu-base.mu-std::log(1e50))<1e-8);
  CHECK(fit.sigma==doctest::Approx(base.sigma).epsilon(1e-9));
  CHECK(fit.Q==doctest::Approx(base.Q).epsilon(1e-9));
  CHECK(std::abs(fit.logLik-base.logLik+golden.data.size()*std::log(1e50))<1e-7);
}

TEST_CASE("gengamma: frozen sample matches independent reference") {
  const auto g=mltest::load_distribution_golden("gengamma");
  const auto r=cppunivariate::gengamma_mle(mltest::as_span(g.data));
  REQUIRE(r.status==cppunivariate::GengammaStatus::Ok);
  const double mu=g.estimates.at("mu"),sigma=g.estimates.at("sigma"),q=g.estimates.at("Q");
  CHECK(std::abs(r.mu-mu)<=1e-10*std::max(std::abs(mu),sigma));
  CHECK(std::abs(r.sigma-sigma)<=1e-10*sigma);
  CHECK(std::abs(r.Q-q)<=1e-10*std::max(1.,std::abs(q)));
  CHECK(r.logLik==doctest::Approx(g.logLik).epsilon(1e-10));
}

TEST_CASE("gengamma: exact symmetry can certify the zero member") {
  std::vector<double> x,y;
  for(int k=-5;k<=5;++k){const double v=std::ldexp(1.,k);x.push_back(v);y.push_back(std::log(v));}
  REQUIRE(cppunivariate::gengamma_fit_detail::multiplicatively_symmetric({x.data(),x.size()}));
  const auto certificate=cppunivariate::gengamma_fit_detail::certify_zero(
      {x.data(),x.size()},{y.data(),y.size()},100);
  CHECK(certificate.certified);
  x.back()=std::nextafter(x.back(),std::numeric_limits<double>::infinity());
  CHECK_FALSE(cppunivariate::gengamma_fit_detail::multiplicatively_symmetric({x.data(),x.size()}));
}

TEST_CASE("gengamma: exact multiplicative symmetry returns certified Q zero") {
  std::vector<double> x;
  for(int k=-5;k<=5;++k)x.push_back(std::ldexp(1.,k));
  const auto r=cppunivariate::gengamma_mle({x.data(),x.size()});
  REQUIRE(r.status==cppunivariate::GengammaStatus::Ok);
  CHECK(r.Q==0.);
  CHECK(r.certified);
}

TEST_CASE("gengamma: public accuracy kinds retain held coordinates and a finite witness") {
  const std::vector<double> x{.2,.7,1.,2.,4.};
  for(double tol:{1e-20,1e-14}) {
    const auto fit=cppunivariate::gengamma_mle({x.data(),x.size()},tol,100,NAN,1.,0.);
    REQUIRE(fit.status==cppunivariate::GengammaStatus::IterationFailure);
    CHECK(fit.stop_detail.accuracy_stop==(tol<1e-16?cppunivariate::AccuracyStop::Tolerance:cppunivariate::AccuracyStop::Conditioning));
    REQUIRE(!fit.stop_detail.candidates.empty());
    const auto& c=fit.stop_detail.candidates.back();
    REQUIRE(c.coef.size()==3);CHECK(c.coef[1]==1.);CHECK(c.coef[2]==0.);
    if(tol==1e-14)CHECK(fit.stop_detail.attainable_tol==cppunivariate::coefficient_spacing(c.coef[0],std::max(std::abs(c.coef[0]),1.)));
    double ll=0;for(double v:x)ll+=cppunivariate::distributions::dgengamma(v,c.coef[0],1.,0.,true);
    CHECK(c.loglik_lower==doctest::Approx(ll).epsilon(1e-13));
  }
  const auto held=cppunivariate::gengamma_mle({x.data(),x.size()},1e-30,100,0.,1.,0.);
  CHECK(held.status==cppunivariate::GengammaStatus::Ok);
  const auto joint=cppunivariate::gengamma_mle({x.data(),x.size()},1e-30,100,NAN,NAN,.3);
  CHECK(joint.stop_detail.accuracy_stop==cppunivariate::AccuracyStop::Tolerance);
}

TEST_CASE("gengamma: an all-held log-input rejection has no coefficient conditioning kind") {
  const std::vector<double> x{2.,2.};
  const auto fit=cppunivariate::gengamma_mle({x.data(),x.size()},1e-20,100,std::log(2.),1.,0.);
  CHECK(fit.status==cppunivariate::GengammaStatus::IterationFailure);
  CHECK(fit.stop_detail.accuracy_stop==cppunivariate::AccuracyStop::None);
  CHECK(fit.stop_detail.attainable_tol==0.);
}

TEST_CASE("gengamma: an internal conditional bracket retains public tolerance detail") {
  const std::vector<double> x{1.,1.};
  const auto fit=cppunivariate::gengamma_mle({x.data(),x.size()},1e-10,100,std::numeric_limits<double>::denorm_min());
  CHECK(fit.status==cppunivariate::GengammaStatus::IterationFailure);
  CHECK(fit.stop_detail.accuracy_stop==cppunivariate::AccuracyStop::Tolerance);
  CHECK(fit.stop_detail.attainable_tol>=1.);
  REQUIRE(!fit.stop_detail.candidates.empty());
  const auto& c=fit.stop_detail.candidates.back();
  CHECK(c.coef[0]==std::numeric_limits<double>::denorm_min());
  CHECK(c.coef[1]>0);CHECK(std::isfinite(c.loglik_lower));
}

TEST_CASE("gengamma: transformed tolerance underflow retains the method floor") {
  const std::vector<double> x{.2,.7,1.,2.,4.};
  const auto fit=cppunivariate::gengamma_mle({x.data(),x.size()},std::numeric_limits<double>::denorm_min(),100,NAN,1.,0.);
  CHECK(fit.status==cppunivariate::GengammaStatus::IterationFailure);
  CHECK(fit.stop_detail.accuracy_stop==cppunivariate::AccuracyStop::Tolerance);
  CHECK(fit.stop_detail.attainable_tol>=16*std::numeric_limits<double>::epsilon());
  REQUIRE(!fit.stop_detail.candidates.empty());
  CHECK(std::isfinite(fit.stop_detail.candidates.back().loglik_lower));
}

TEST_CASE("gengamma: conversion rounding preserves a diagnosed local tolerance floor") {
  const std::vector<double> y{-1.,1.};
  const double unit=1.147;
  const double factor=1/unit;
  const double tol=16*std::numeric_limits<double>::epsilon()/factor;
  REQUIRE(tol*factor*.25<4*std::numeric_limits<double>::epsilon());
  std::array<double,3> parameter{0.,1.,0.};
  cppunivariate::StopDetail detail;
  CHECK_FALSE(cppunivariate::gengamma_fit_detail::polish_local(
    {y.data(),y.size()},0.,unit,parameter,{true,true,false},tol,100,0.,detail));
  CHECK(detail.accuracy_stop==cppunivariate::AccuracyStop::Tolerance);
  CHECK(detail.attainable_tol>tol);
  REQUIRE(!detail.candidates.empty());
  CHECK(std::isfinite(detail.candidates.back().loglik_lower));
}
