#include "test_support.hpp"
#include "cppunivariate/cppunivariate.hpp"
#include "cppunivariate/distributions/ged/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <random>
#include <vector>

#define C_APPROX(x) doctest::Approx(x).epsilon(mltest::kTol)
#define C_APPROX_FD(x) doctest::Approx(x).epsilon(mltest::kFdTol)

// rged samples are symmetric around the mean; sample mean ~ 0, sd ~ 1 at
// (mean=0, sd=1), and the distribution lines up with the CDF at 0 (= 0.5).
TEST_CASE("rged samples are symmetric and match the CDF") {
  cppunivariate::distributions::StdRng<> rng(17);
  const int n = 20000;
  double sum = 0.0;
  double sum_sq = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rged(rng, 0.0, 1.0, 1.5);
    sum += x;
    sum_sq += x * x;
  }
  const double mean = sum / n;
  const double var = sum_sq / n - mean * mean;
  CHECK(std::abs(mean) < 0.05);
  CHECK(std::abs(var - 1.0) < 0.1);

  // CDF at 0 should be exactly 0.5 (symmetric distribution).
  const double cdf0 =
      cppunivariate::distributions::pged(0.0, 0.0, 1.0, 1.5, true, false);
  CHECK(std::abs(cdf0 - 0.5) < 1e-12);
}

TEST_CASE("ged (Generalized Error Distribution)") {
  auto g = mltest::load_distribution_golden("ged");
  auto r = cppunivariate::ged_mle(mltest::as_span(g.data), mltest::kFitTol, 200);
  CHECK(r.mu == doctest::Approx(g.estimates.at("mean")).epsilon(mltest::kGolden_ged).scale(0));
  CHECK(r.sigma == doctest::Approx(g.estimates.at("sd")).epsilon(mltest::kGolden_ged).scale(0));
  CHECK(r.nu == doctest::Approx(g.estimates.at("nu")).epsilon(mltest::kGolden_ged).scale(0));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_ged).scale(0));

  cppunivariate::GedCost cost{mltest::as_span(g.data)};
  const double phi_hat[3] = {r.mu, std::log(r.sigma), std::log(r.nu)};
  double grad[3];
  cppunivariate::finite_diff_gradient(cost, phi_hat, grad);
  const double gnorm =
      std::sqrt(grad[0] * grad[0] + grad[1] * grad[1] + grad[2] * grad[2]);
  if (r.nu > 1.0 && r.status == cppunivariate::GedStatus::Ok) CHECK(gnorm < 1e-3);
}

#undef C_APPROX
#undef C_APPROX_FD

TEST_CASE("ged constrained faces and shapes") {
  std::vector<double> uniform, peaked;
  for (int i = 0; i < 40; ++i) uniform.push_back(-1.0 + 2.0*i/39.0);
  for (int i = 0; i < 19; ++i) peaked.push_back(-0.02 + 0.04*i/18.0);
  peaked.push_back(-10.0); peaked.push_back(10.0);
  auto u = cppunivariate::ged_mle(mltest::as_span(uniform), 1e-6, 500);
  CHECK(u.status == cppunivariate::GedStatus::UniformBoundary);
  auto l = cppunivariate::ged_mle(mltest::as_span(peaked), 1e-6, 500);
  CHECK(l.status == cppunivariate::GedStatus::Ok);
  CHECK(l.nu == 1.0);
  CHECK(l.nu >= 1.0);
  cppunivariate::distributions::StdRng<> rng(812);
  for (double shape : {1.5, 3.0}) {
    std::vector<double> x;
    for (int i = 0; i < 500; ++i)
      x.push_back(cppunivariate::distributions::rged(rng, 0.0, 1.0, shape));
    auto r = cppunivariate::ged_mle(mltest::as_span(x), 1e-6, 500);
    CHECK(r.status == cppunivariate::GedStatus::Ok);
    CHECK(r.nu > 1.0);
  }
}

TEST_CASE("ged package dpqr preserve logarithmic tails") {
  for (bool lower : {false, true}) {
    for (double lp : {-1000.0, -100.0, -2.0, -0.1}) {
      const double x = cppunivariate::distributions::qged(lp, 0.3, 1.7, 1.4, lower, true);
      CHECK(cppunivariate::distributions::pged(x, 0.3, 1.7, 1.4, lower, true) == doctest::Approx(lp).epsilon(2e-12));
    }
  }
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("ged: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("ged",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dged(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pged(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qged(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rged(rng, p[0], p[1], p[2]); });
}

TEST_CASE("ged: every held golden subset preserves coefficients and likelihood") {
  const auto g=mltest::load_distribution_golden("ged");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(int mask=1;mask<8;++mask) {
    CAPTURE(mask);
    const auto r=cppunivariate::ged_mle(mltest::as_span(g.data),1e-8,1000,((mask & 1) ? g.estimates.at("mean") : nan),((mask & 2) ? g.estimates.at("sd") : nan),((mask & 4) ? g.estimates.at("nu") : nan));
    if(mask & 1)CHECK(r.mu==g.estimates.at("mean"));
    if(mask & 2)CHECK(r.sigma==g.estimates.at("sd"));
    if(mask & 4)CHECK(r.nu==g.estimates.at("nu"));
    CHECK(r.status==cppunivariate::GedStatus::Ok);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*g.data.size());
  }
}

TEST_CASE("ged: held variance uniform endpoint comparison") {
  const double endpoint=std::sqrt(3.0),nan=std::numeric_limits<double>::quiet_NaN();
  std::vector<double> x;for(int i=0;i<10;++i){x.push_back(-endpoint);x.push_back(endpoint);}
  CHECK(cppunivariate::ged_mle(mltest::as_span(x),1e-8,200,0,1,nan).status==cppunivariate::GedStatus::UniformUnresolved);
  CHECK(cppunivariate::ged_mle(mltest::as_span(x),1e-8,200,nan,1,nan).status==cppunivariate::GedStatus::UniformUnresolved);
}

TEST_CASE("ged: held profiles preserve extreme units") {
  const std::vector<double> x={-.02,-.005,0,.007,.03};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto base=cppunivariate::ged_mle(mltest::as_span(x),1e-8,200,nan,nan,10000);
  for(double unit:{1e-300,1e300}) {
    auto y=x;for(double& v:y)v*=unit;
    const auto fit=cppunivariate::ged_mle(mltest::as_span(y),1e-8,200,nan,nan,10000);
    CHECK(fit.sigma/unit==doctest::Approx(base.sigma).epsilon(1e-8));
    CHECK(fit.logLik+x.size()*std::log(unit)==doctest::Approx(base.logLik).epsilon(1e-8));
  }
}


// Generating mpmath snippet (binary64 inputs retained exactly):
// import mpmath as mp, math; mp.mp.dps=100
// n=mp.mpf(3); s=mp.sqrt(mp.gamma(1/n)/mp.gamma(3/n))
// p=mp.mpf(float(-math.log(2))); mass=mp.expm1(p+mp.log(2))
// f=lambda y: mp.log(mp.gammainc(1/n,0,mp.exp(y))/mp.gamma(1/n))-mp.log(mass)
// y=mp.findroot(f,-110); print(s*mp.exp(y/n))
// g=lambda a: mp.log(mp.gammainc(1/n,a,mp.inf)/mp.gamma(1/n))+800-mp.log(2)
// print(s*mp.findroot(g,800)**(1/n))
TEST_CASE("ged: targeted tail and endpoint regressions") {
  CHECK(cppunivariate::distributions::qged(-0.6931471805599453, 0.0, 1.0, 3.0, true, true) == doctest::Approx(3.389474609308743e-17).epsilon(1e-10));
  CHECK(cppunivariate::distributions::qged(-800.0, 0.0, 1.0, 3.0, false, true) == doctest::Approx(15.155294491505197).epsilon(1e-10));
}


TEST_CASE("ged: P3 restrictions and closed faces") {
 const std::vector<double> x{-100,-2,-.01,0,.01,2,100};const auto data=mltest::as_span(x);
 const auto f=cppunivariate::ged_mle(data,1e-8,1000); REQUIRE(f.status==cppunivariate::GedStatus::Ok);
 CHECK(f.nu==1);CHECK(f.restricted==16u);
 const auto h=cppunivariate::ged_mle(data,1e-8,1000,0,1,1);
 CHECK(h.restricted==0u);
}

TEST_CASE("ged fixed shape inverse-scale concavity examples") {
  const std::vector<double> x{-1.4, -0.1, 0.3, 2.0};
  const auto ll=[&](double u,double b,double nu) {
    double value=0;
    for(double v:x) value+=std::log(b)+cppunivariate::distributions::dged(b*v-u,0,1,nu,true);
    return value;
  };
  for(double nu:{1.0,1.2,2.0,5.0})
    CHECK(ll(0.1,1.2,nu)>=0.5*(ll(-0.4,0.7,nu)+ll(0.6,1.7,nu))-1e-12);
  const auto f=[](double z){return cppunivariate::distributions::dged(z,0,1,0.5,true);};
  CHECK(f(2)<0.5*(f(1)+f(3)));
}

TEST_CASE("ged: face comparison cannot resolve rounded extreme objectives") {
  const double magnitude=1e20;
  CHECK(cppunivariate::compare_face(-magnitude,-magnitude,2e-8)==cppunivariate::FaceComparison::NumericalTie);
  CHECK(cppunivariate::compare_face(-magnitude,-2*magnitude,2e-8)==cppunivariate::FaceComparison::FirstWins);
  CHECK(cppunivariate::compare_face(-2*magnitude,-magnitude,2e-8)==cppunivariate::FaceComparison::SecondWins);
  CHECK(cppunivariate::compare_face(-10.,-10.,2e-8)==cppunivariate::FaceComparison::FirstWins);
}

TEST_CASE("ged: audited huge held-parameter magnitude") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  std::vector<double> x{1,2};
  cppunivariate::span s{x.data(),x.size()};
  const auto r=cppunivariate::ged_mle(s,1e-6,500,0,1e-20,nan);
  CHECK((r.status==cppunivariate::GedStatus::Ok || r.status==cppunivariate::GedStatus::BfgsFailure));
}

TEST_CASE("ged: structured face payloads") {
  std::vector<double> x;for(int i=0;i<40;++i)x.push_back(-1.+2.*i/39.);
  const auto r=cppunivariate::ged_mle(mltest::as_span(x),1e-6,500);
  REQUIRE(r.stop_detail.limit_coef.size()==2);
  CHECK(r.stop_detail.limit_coef[0]==doctest::Approx(-1));CHECK(r.stop_detail.limit_coef[1]==doctest::Approx(1));
  CHECK(r.stop_detail.limit_loglik==doctest::Approx(-40*std::log(2.)));
}

TEST_CASE("ged: free-shape power moment bounds include the exact normal profile") {
  const std::vector<double> x{-3,-1,0,1,4};
  const auto data=mltest::as_span(x);
  const auto m=cppunivariate::ged_shape_detail::moment(data,2,NAN,200);
  REQUIRE(m.valid);
  const double expected=std::sqrt(5.36); // mean=0.2, population variance=5.36
  CHECK(m.log_lower<=std::log(expected));
  CHECK(m.log_upper>=std::log(expected));
  CHECK(m.log_upper-m.log_lower<1e-8);
  // Even a single bisection gives bounds rather than an unchecked minimizer.
  const auto short_run=cppunivariate::ged_shape_detail::moment(data,2,NAN,1);
  REQUIRE(short_run.valid);
  CHECK(short_run.log_lower<=std::log(expected));
  CHECK(short_run.log_upper>=std::log(expected));
  const auto median=cppunivariate::ged_shape_detail::moment(data,1,NAN,1);
  REQUIRE(median.valid);
  CHECK(median.log_lower<=std::log(1.8));
  CHECK(median.log_upper>=std::log(1.8));
}

TEST_CASE("ged: finite shape interval bounds cover all four free-shape subsets") {
  const std::vector<double> x{-3,-1,0,1,4};
  const auto data=mltest::as_span(x);
  for(double mean:{double(NAN),.2})for(double sd:{double(NAN),1.3}) {
    for(double p:{1.,1.2,2.,10.}) {
      const double q=p+.2;
      const auto bound=cppunivariate::ged_shape_detail::interval(data,p,q,mean,sd,200);
      REQUIRE(bound.valid);
      for(double shape:{p,(p+q)/2,q}) {
        const auto r=cppunivariate::ged_detail::conditional(data,shape,mean,sd,200);
        REQUIRE(r.status==cppunivariate::GedStatus::Ok);
        CHECK(r.logLik<=bound.upper);
      }
    }
  }
}

TEST_CASE("ged: endpoint power profile includes unequal endpoint counts") {
  const auto power=[](double p){return cppunivariate::ged_shape_detail::endpoint_power(1,9,p);};
  CHECK(power(1)==doctest::Approx(2));
  CHECK(power(2)==doctest::Approx(3.6));
  CHECK(power(1000000)==doctest::Approx(6).epsilon(1e-6));
  for(double p:{1.,1.1,2.,10.,1000.}) {
    CHECK(power(p)<=power(p*2)+1e-12);
    for(int j=-100;j<=100;++j) {
      const double u=j/100.;
      CHECK(power(p)<=std::pow(std::abs(1+u),p)+9*std::pow(std::abs(1-u),p)+1e-10);
    }
  }
}

TEST_CASE("ged: tail bounds enclose optimized moments and endpoint paths") {
  for(const std::vector<double>& x:{std::vector<double>{-3,-1,0,1,4},
      std::vector<double>{-1,1,1,1,1,1,1,1,1,1}}) {
    const auto data=mltest::as_span(x);
    for(double mean:{double(NAN),0.})for(double sd:{double(NAN),1/std::sqrt(3.),.4,2.}) {
      for(double p:{1.,2.,10.,100.}) {
        const auto bound=cppunivariate::ged_shape_detail::tail(data,p,mean,sd,200);
        REQUIRE(bound.valid);
        for(double shape:{p,p*2,p*10}) {
          const auto r=cppunivariate::ged_detail::conditional(data,shape,mean,sd,200);
          if(std::isfinite(r.logLik))CHECK(r.logLik<=bound.upper);
        }
      }
    }
  }
  const std::vector<double> endpoints{-std::sqrt(3.),std::sqrt(3.)};
  const auto uncertain=cppunivariate::ged_shape_detail::tail(mltest::as_span(endpoints),100,0,1,200);
  REQUIRE(uncertain.valid);
  CHECK(uncertain.endpoint_uncertain);
  // Endpoint comparisons are discontinuous: near-equality drops the penalty.
  CHECK(uncertain.upper>=-2*std::log(2*std::sqrt(3.)));
}

TEST_CASE("ged: constant samples with held sd have an analytic free-shape certificate") {
  const std::vector<double> x{2,2,2};
  for(double mean:{double(NAN),2.}) {
    const auto r=cppunivariate::ged_mle(mltest::as_span(x),1e-6,1,mean,1,NAN);
    REQUIRE(r.status==cppunivariate::GedStatus::Ok);
    CHECK(r.certified);
    CHECK(r.mu==2);
    CHECK(r.sigma==1);
    CHECK(r.nu==1);
    CHECK(r.restricted==16u);
    CHECK(r.logLik==doctest::Approx(-1.5*std::log(2.)));
    for(double shape:{1.01,2.,10.,1000.}) {
      const auto other=cppunivariate::ged_detail::conditional(mltest::as_span(x),shape,mean,1,200);
      CHECK(other.logLik<r.logLik);
    }
  }
}

TEST_CASE("ged: failed free-shape searches retain a witness and global bound") {
  auto g=mltest::load_distribution_golden("ged");
  auto r=cppunivariate::ged_mle(mltest::as_span(g.data),1e-8,1,0.,1.,NAN);
  REQUIRE(r.status==cppunivariate::GedStatus::BfgsFailure);
  CHECK_FALSE(r.certified);
  REQUIRE_FALSE(r.stop_detail.candidates.empty());
  CHECK(r.stop_detail.loglik_lower<=r.stop_detail.loglik_upper);
  CHECK(r.stop_detail.loglik_upper>=g.logLik);
  for(const auto& c:r.stop_detail.candidates) {
    const auto at=cppunivariate::ged_detail::conditional(mltest::as_span(g.data),c.coef[2],c.coef[0],c.coef[1],200);
    CHECK(c.loglik_lower<=at.logLik);
    CHECK(c.loglik_upper>=at.logLik);
  }
}

TEST_CASE("ged: small held scale certifies the Laplace maximum or its median tie") {
  const std::vector<double> odd{-3,-1,0,1,4};
  const auto r=cppunivariate::ged_mle(mltest::as_span(odd),1e-6,1,NAN,.1,NAN);
  REQUIRE(r.status==cppunivariate::GedStatus::Ok);
  CHECK(r.certified);
  CHECK(r.mu==0);
  CHECK(r.nu==1);
  CHECK(r.restricted==16u);
  const std::vector<double> even{-1,1};
  const auto tied=cppunivariate::ged_mle(mltest::as_span(even),1e-6,1,NAN,.1,NAN);
  REQUIRE(tied.status==cppunivariate::GedStatus::Nonunique);
  CHECK(tied.stop_detail.comparison_tie);
  REQUIRE(tied.stop_detail.candidates.size()==2);
  CHECK(tied.stop_detail.candidates[0].coef[0]==-1);
  CHECK(tied.stop_detail.candidates[1].coef[0]==1);
  CHECK(tied.stop_detail.loglik_lower<=tied.stop_detail.loglik_upper);
  CHECK(std::isfinite(tied.stop_detail.loglik_upper));
  for(double mean:{-.8,0.,.7}) {
    const auto held=cppunivariate::ged_mle(mltest::as_span(even),1e-6,1,mean,.1,NAN);
    REQUIRE(held.status==cppunivariate::GedStatus::Ok);
    CHECK(held.certified);
    CHECK(held.mu==mean);
    CHECK(held.nu==1);
    CHECK(held.logLik==doctest::Approx(tied.logLik));
    for(double shape:{1.01,1.1,2.,10.}) {
      const auto other=cppunivariate::ged_detail::conditional(mltest::as_span(even),shape,NAN,.1,200);
      CHECK(other.logLik<tied.logLik);
    }
  }
}

TEST_CASE("ged: rounded held-sd uniform endpoints stop without selecting a face") {
  const std::vector<double> x{-std::sqrt(3.),std::sqrt(3.)};
  for(double mean:{double(NAN),0.}) {
    const auto r=cppunivariate::ged_mle(mltest::as_span(x),1e-6,200,mean,1,NAN);
    REQUIRE(r.status==cppunivariate::GedStatus::UniformUnresolved);
    CHECK(r.stop_detail.limit_coef.empty());
    CHECK_FALSE(r.stop_detail.candidates.empty());
    CHECK(std::isfinite(r.stop_detail.loglik_upper));
    CHECK(r.stop_detail.loglik_upper>=-2*std::log(2*std::sqrt(3.)));
  }
}

TEST_CASE("ged: a held-sd interior uniform height cannot be selected from a capped shape search") {
  const std::vector<double> x{-std::sqrt(3.)*(1-1e-8),std::sqrt(3.)*(1-1e-8)};
  const auto r=cppunivariate::ged_mle(mltest::as_span(x),1e-6,200,0,1,NAN);
  CHECK(r.status==cppunivariate::GedStatus::UniformUnresolved);
  CHECK(r.stop_detail.limit_coef.empty());
  CHECK(r.stop_detail.loglik_upper>=-2*std::log(2*std::sqrt(3.)));
}

TEST_CASE("ged: a signed free-scale tail bound closes uniform-winning tails") {
  std::vector<double> x;for(int j=0;j<40;++j)x.push_back(-1.+2.*j/39.);
  const auto data=mltest::as_span(x);
  for(double mean:{double(NAN),.2}) {
    const auto bound=cppunivariate::ged_shape_detail::tail(data,100,mean,NAN,200);
    REQUIRE(bound.valid);
    const double radius=std::isnan(mean)?1:1.2;
    const double uniform=-40*std::log(2*radius);
    CHECK(bound.upper>=uniform);
    CHECK(bound.upper<uniform+1e-6);
    for(double shape:{100.,1000.,10000.}) {
      const auto finite=cppunivariate::ged_detail::conditional(data,shape,mean,NAN,200);
      CHECK(finite.logLik<=uniform);
    }
  }
}

TEST_CASE("ged: a constant sample away from a held mean has a certified uniform escape") {
  const std::vector<double> x{2,2,2};
  const auto r=cppunivariate::ged_mle(mltest::as_span(x),1e-6,1,0,NAN,NAN);
  REQUIRE(r.status==cppunivariate::GedStatus::UniformBoundary);
  CHECK(r.certified);
  REQUIRE(r.stop_detail.limit_coef.size()==2);
  CHECK(r.stop_detail.limit_coef[0]==-2);
  CHECK(r.stop_detail.limit_coef[1]==2);
  CHECK(r.stop_detail.limit_loglik==doctest::Approx(-3*std::log(4.)));
  for(double shape:{1.,2.,10.,1000.}) {
    const auto finite=cppunivariate::ged_detail::conditional(mltest::as_span(x),shape,0,NAN,200);
    CHECK(finite.logLik<r.stop_detail.limit_loglik);
  }
}

TEST_CASE("ged: free-scale uniform decisions require a global finite-interval certificate") {
  std::vector<double> x;for(int j=0;j<40;++j)x.push_back(-1.+2.*j/39.);
  const auto data=mltest::as_span(x);
  for(double mean:{double(NAN),0.}) {
    const auto r=cppunivariate::ged_mle(data,1e-6,200,mean,NAN,NAN);
    REQUIRE(r.status==cppunivariate::GedStatus::UniformBoundary);
    CHECK(r.certified);
    CHECK(std::isfinite(r.stop_detail.loglik_upper));
    CHECK(r.stop_detail.loglik_lower<=-40*std::log(2.));
    CHECK(r.stop_detail.loglik_upper>=-40*std::log(2.));
  }
  // A deliberately false face proposed over a known finite normal-like fit
  // must exhaust or fail the certificate, never become a selected limit.
  auto g=mltest::load_distribution_golden("ged");
  const auto mm=std::minmax_element(g.data.begin(),g.data.end());
  const double ll=-static_cast<double>(g.data.size())*std::log(*mm.second-*mm.first);
  cppunivariate::GedResult proposal{0,1,2,ll,0,cppunivariate::GedStatus::UniformBoundary};
  proposal.stop_detail=cppunivariate::limit_detail({*mm.first,*mm.second},ll,cppunivariate::StopModel::Unif);
  cppunivariate::ged_uniform_certificate(proposal,mltest::as_span(g.data),NAN,1e-6,1);
  CHECK_FALSE(proposal.certified);
  CHECK(proposal.status==cppunivariate::GedStatus::BfgsFailure);
  CHECK(proposal.stop_detail.limit_coef.empty());
}

TEST_CASE("ged: all four free-shape golden subsets require the global score certificate") {
  const auto g=mltest::load_distribution_golden("ged");
  for(int mask=0;mask<4;++mask) {
    CAPTURE(mask);
    const auto r=cppunivariate::ged_mle(mltest::as_span(g.data),1.220703e-4,200,
      mask&1?g.estimates.at("mean"):NAN,mask&2?g.estimates.at("sd"):NAN,NAN);
    REQUIRE(r.status==cppunivariate::GedStatus::Ok);
    CHECK(r.certified);
    CHECK(r.mu==doctest::Approx(g.estimates.at("mean")).epsilon(1e-5));
    CHECK(r.sigma==doctest::Approx(g.estimates.at("sd")).epsilon(1e-5));
    CHECK(r.nu==doctest::Approx(g.estimates.at("nu")).epsilon(1e-5));
    CHECK(r.stop_detail.loglik_lower<=r.logLik);
    CHECK(r.stop_detail.loglik_upper>=r.logLik);
    CHECK(r.iterations<=cppunivariate::certificate_budget(200));
  }
}

TEST_CASE("ged: score enclosure covers nuisance roots and proves local root isolation") {
  const std::vector<double> x{-1,-.3,0,.2,.9};const auto data=mltest::as_span(x);
  for(double mean:{double(NAN),.1})for(double sd:{double(NAN),.7}) {
    const auto box=cppunivariate::ged_shape_detail::score_box(data,2.9,3.1,mean,sd,200);
    REQUIRE(box.valid);
    for(double p:{2.9,3.,3.1}) {
      const auto r=cppunivariate::ged_detail::conditional(data,p,mean,sd,200);
      CHECK(r.mu>=box.location.lo);CHECK(r.mu<=box.location.hi);
      const auto point=cppunivariate::ged_shape_detail::score_box(data,p,p,mean,sd,200);
      CHECK(point.score.lo>=box.score.lo);CHECK(point.score.hi<=box.score.hi);
    }
  }
  const auto g=mltest::load_distribution_golden("ged");
  const auto box=cppunivariate::ged_shape_detail::score_box(mltest::as_span(g.data),3.016,3.017,NAN,NAN,200);
  REQUIRE(box.valid);CHECK(box.derivative.hi<0);
}

TEST_CASE("ged: globally selected free-shape Laplace median intervals stop with two witnesses") {
  std::vector<double> x;for(int i=0;i<18;++i)x.push_back(-.02+.04*i/17.);
  x.push_back(-10);x.push_back(10);
  const auto r=cppunivariate::ged_mle(mltest::as_span(x),1e-6,200);
  REQUIRE(r.status==cppunivariate::GedStatus::Nonunique);
  REQUIRE(r.stop_detail.candidates.size()>=2);
  CHECK(r.stop_detail.candidates[0].coef[2]==1);
  CHECK(r.stop_detail.candidates[1].coef[2]==1);
  CHECK(r.stop_detail.candidates[0].coef[0]!=r.stop_detail.candidates[1].coef[0]);
}

TEST_CASE("ged: held-shape certificates expose median and knot ties") {
  const std::vector<double> x{-1,1};const auto data=mltest::as_span(x);
  for(double nu:{.5,1.}) {
    const auto r=cppunivariate::ged_mle(data,1e-6,200,NAN,NAN,nu);
    CHECK(r.status==cppunivariate::GedStatus::Nonunique);
    CHECK(r.stop_detail.candidates.size()==2);
  }
  const auto normal=cppunivariate::ged_mle(data,1e-6,200,NAN,NAN,2.);
  REQUIRE(normal.status==cppunivariate::GedStatus::Ok);CHECK(normal.certified);
}

TEST_CASE("ged: numerically overlapping finite maxima keep an uncertified search") {
  // 80-digit raw-likelihood construction: held sd=1, symmetric locations.
  // Exact inner radius 0.3299866080912000371592... makes the two peaks equal.
  std::vector<double> x(8,0.);
  for(int i=0;i<23;++i){x.push_back(-.32998660809120006);x.push_back(.32998660809120006);}
  for(int i=0;i<12;++i){x.push_back(-1.4239025460520294);x.push_back(1.4239025460520294);}
  for(double mean:{0.,double(NAN)}) {
    const auto r=cppunivariate::ged_mle(mltest::as_span(x),1e-6,200,mean,1,NAN);
    REQUIRE(r.status==cppunivariate::GedStatus::Ok);CHECK_FALSE(r.certified);
    REQUIRE(r.stop_detail.candidates.size()>=2);
    auto a=r.stop_detail.candidates[0],b=r.stop_detail.candidates[1];
    if(a.coef[2]>b.coef[2])std::swap(a,b);
    CHECK(a.coef[2]==doctest::Approx(1.108145553391854).epsilon(1e-5));
    CHECK(b.coef[2]==doctest::Approx(25.39783214175462).epsilon(1e-5));
    CHECK(std::abs((a.loglik_lower+a.loglik_upper)-(b.loglik_lower+b.loglik_upper))<1e-5);
  }
}

TEST_CASE("ged: inverse normalization cannot certify an unrepresentable location") {
  const auto g=mltest::load_distribution_golden("ged");
  auto x=g.data;for(double& v:x)v=1e16+10*v;
  const auto data=mltest::as_span(x);
  const auto r=cppunivariate::ged_mle(data,1.220703e-4,200);
  REQUIRE(r.status==cppunivariate::GedStatus::TransformFailure);
  CHECK_FALSE(r.certified);
  REQUIRE_FALSE(r.stop_detail.candidates.empty());
  for(const auto& c:r.stop_detail.candidates) {
    const auto actual=cppunivariate::ged_detail::conditional(data,c.coef[2],c.coef[0],c.coef[1],200);
    CHECK(c.loglik_lower<=actual.logLik);CHECK(c.loglik_upper>=actual.logLik);
  }
  const auto normal=cppunivariate::ged_mle(data,1e-6,200,NAN,NAN,2.);
  CHECK(normal.status==cppunivariate::GedStatus::TransformFailure);
}


TEST_CASE("ged: rounded residual ties cannot select a uniform excluding an observation") {
  const double u=std::ldexp(1.,-53);
  const std::vector<double> x{-1+u,1+2*u};
  const auto r=cppunivariate::ged_mle(mltest::as_span(x),1e-6,200,u,NAN,NAN);
  CHECK(r.status==cppunivariate::GedStatus::TransformFailure);
  CHECK_FALSE(r.certified);CHECK(r.stop_detail.limit_coef.empty());
  CHECK_FALSE(r.stop_detail.candidates.empty());
}

TEST_CASE("ged: the Laplace median interval retains original observation endpoints") {
  std::vector<double> x;for(int i=1;i<=19;++i)x.push_back(i);x.push_back(1e308);
  const auto r=cppunivariate::ged_mle(mltest::as_span(x),1e-6,200);
  REQUIRE(r.status==cppunivariate::GedStatus::Nonunique);
  REQUIRE(r.stop_detail.candidates.size()==2);
  CHECK(r.stop_detail.candidates[0].coef[0]==10);
  CHECK(r.stop_detail.candidates[1].coef[0]==11);
}

TEST_CASE("ged: held-shape arithmetic losses are numerical stops rather than certificates") {
  const double a=1e-320;
  const std::vector<double> tiny{0,0,a};
  const auto r=cppunivariate::ged_mle(mltest::as_span(tiny),1e-6,200,NAN,NAN,.5);
  CHECK(r.status==cppunivariate::GedStatus::TransformFailure);CHECK_FALSE(r.certified);
  const std::vector<double> small{0,0,1e-200};
  const auto q=cppunivariate::ged_mle(mltest::as_span(small),1e-6,200,NAN,NAN,.02);
  CHECK(q.status==cppunivariate::GedStatus::TransformFailure);CHECK(std::isnan(q.sigma));
  const std::vector<double> underflow{1e-100};
  const auto h=cppunivariate::ged_mle(mltest::as_span(underflow),1e-6,200,0,1e300,.02);
  CHECK(h.status==cppunivariate::GedStatus::TransformFailure);CHECK_FALSE(h.certified);
  const std::vector<double> constant(400,0);
  const auto c=cppunivariate::ged_mle(mltest::as_span(constant),1e-6,1,NAN,1,.5);
  CHECK(c.status==cppunivariate::GedStatus::Ok);CHECK(c.certified);
}

TEST_CASE("ged: positive residual kernels survive quotient underflow") {
  // Independent 100-digit mpmath evaluation at the exact binary64 inputs;
  // reproducible checker in 2026-11-08-ged-density-data/check.py.
  for (double x : {-1e-100,1e-100}) {
    CHECK(cppunivariate::distributions::dged(x,0,1e300,.02,true) == doctest::Approx(-612.224579676587918).epsilon(2e-14));
    CHECK(cppunivariate::distributions::dged(x,0,1e300,.02,false) == doctest::Approx(std::exp(-612.224579676587918)).epsilon(2e-12));
    CHECK(cppunivariate::distributions::dged(x,0,1e300,.02,true) < cppunivariate::distributions::dged(0,0,1e300,.02,true)-9e-7);
    // With a smaller held shape, the lost sign affects a representable tail.
    const double small=cppunivariate::distributions::pged(x,0,1e300,.0005,x<0,true);
    CHECK(small == doctest::Approx(-41.209468768459971).epsilon(2e-12));
    CHECK(cppunivariate::distributions::pged(x,0,1e300,.0005,x>0,true) == doctest::Approx(-1.2675208492151555e-18).epsilon(2e-10));
    const double recovered=cppunivariate::distributions::qged(small,0,1e300,.0005,x<0,true);
    CHECK(recovered/x == doctest::Approx(1).epsilon(2e-10));
  }
  // A nonzero subnormal quotient has too few significant bits as well.
  CHECK(cppunivariate::distributions::dged(1e-23,0,1e300,.02,true) == doctest::Approx(-612.224611684520115).epsilon(2e-14));
  CHECK(cppunivariate::distributions::pged(0,0,1e300,.0005,true,false) == .5);
  CHECK(cppunivariate::distributions::dged(INFINITY,0,1e300,.02,true) == -INFINITY);
}

TEST_CASE("ged: inherited location enclosures retain all child-shape minimizers") {
  const std::vector<double> x{-1,-.7,-.7,-.1,0,.2,.8,1};
  const auto y=mltest::as_span(x);
  for(const auto shapes:std::vector<std::pair<double,double>>{{1,1.1},{1.1,1.9},{1.9,2.1},{3,4},{20,30}}) {
    const auto parent=cppunivariate::ged_shape_detail::locations(y,shapes.first,shapes.second,NAN,200);
    for(int j=0;j<=8;++j) {
      const double p=shapes.first+(shapes.second-shapes.first)*j/8;
      const auto child=cppunivariate::ged_shape_detail::locations(y,p,p,NAN,200,parent);
      const auto point=cppunivariate::ged_detail::conditional(y,p,NAN,1,200);
      REQUIRE(point.status==cppunivariate::GedStatus::Ok);
      CHECK(child.lo>=parent.lo);CHECK(child.hi<=parent.hi);
      CHECK(point.mu>=child.lo-2e-12);CHECK(point.mu<=child.hi+2e-12);
      const auto enclosed=cppunivariate::ged_shape_detail::moment(y,p,NAN,200,child.lo,child.hi);
      const auto independent=cppunivariate::ged_shape_detail::moment(y,p,point.mu,200);
      REQUIRE(enclosed.valid);REQUIRE(independent.valid);
      CHECK(enclosed.log_lower<=independent.log_upper);
      CHECK(enclosed.log_upper>=independent.log_lower);
    }
  }
}

TEST_CASE("ged: shared residual powers preserve zero and underflow curvature bounds") {
  // T can underflow while r^(p-2) is still representable. Curvature must
  // evaluate that lower power directly, rather than recovering it from T.
  const std::vector<double> x{-1e-160,0,1e-160};
  const auto box=cppunivariate::ged_shape_detail::score_box(mltest::as_span(x),2.1,2.1,0,1,200);
  REQUIRE(box.valid);
  CHECK(std::isfinite(box.score.lo));CHECK(std::isfinite(box.score.hi));
  const auto h=cppunivariate::ged_shape_detail::residual_hessian(1e-160,1e-160,2.1,2.1);
  CHECK(h.lo>0);CHECK(std::isfinite(h.hi));
  const std::vector<double> tied{-1,0,0,0,1};
  for(double p:{1.,1.001,2.,3.,30.}) {
    const auto b=cppunivariate::ged_shape_detail::score_box(mltest::as_span(tied),p,p,0,1,200);
    REQUIRE(b.valid);CHECK(std::isfinite(b.score.lo+b.score.hi));
  }
}

TEST_CASE("ged: location reuse preserves short-budget numerical stops") {
  const std::vector<double> x{-.8,0,0,0,.9};
  for(int maxit:{1,2})for(double sd:{std::numeric_limits<double>::quiet_NaN(),.1}) {
    const auto r=cppunivariate::ged_mle(mltest::as_span(x),1e-10,maxit,NAN,sd,NAN);
    CHECK(r.status==cppunivariate::GedStatus::BfgsFailure);
    CHECK_FALSE(r.certified);
    CHECK_FALSE(r.stop_detail.budget_exhausted);
  }
}

#include "cppunivariate/distributions/sged/dpqr.hpp"
TEST_CASE("GED extreme shapes preserve inversion and random amplitude") {
  for(double nu:{1e8,1e100,std::numeric_limits<double>::max()}) {
    for(double p:{.01,.25,.5,.75,.99}) {
      const double q=cppunivariate::distributions::qged(p,0,1,nu,true,false);
      CHECK(q==doctest::Approx(std::sqrt(3.)*(2*p-1)).epsilon(5e-7));
      CHECK(cppunivariate::distributions::pged(q,0,1,nu,true,false)==doctest::Approx(p).epsilon(1e-12));
      const double s=cppunivariate::distributions::qsged(p,0,1,nu,2);
      CHECK(s==doctest::Approx(std::sqrt(3.)*(2*p-1)).epsilon(5e-7));
      CHECK(cppunivariate::distributions::psged(s,0,1,nu,2)==doctest::Approx(p).epsilon(1e-12));
    }
    cppunivariate::distributions::StdRng<> rng(163);
    double sum=0,square=0;int below=0;
    for(int i=0;i<10000;++i) {
      const double x=cppunivariate::distributions::rsged(rng,0,1,nu,2);
      REQUIRE(std::isfinite(x));sum+=x;square+=x*x;below+=x<0;
    }
    CHECK(std::abs(sum/10000)<.04);
    CHECK(std::abs(square/10000-1)<.05);
    CHECK(std::abs(below/10000.-.5)<.025);
  }
}

TEST_CASE("GED tiny gamma shapes retain logarithmic upper tails") {
  for(double nu:{1e8,1e100,std::numeric_limits<double>::max()}) {
    const double a=1/nu;
    for(double x:{1.,2.}) {
      const double e1=x==1 ? .21938393439552027368 : .048900510708061119567;
      const double lp=cppunivariate::ged_detail::gamma_power_log(std::log(x)/nu,nu,false);
      CHECK(lp==doctest::Approx(std::log(a)+std::log(e1)).epsilon(1e-8));
    }
    for(double lp:{std::log(a)-2,std::log(a)-1000}) {
      const double w=cppunivariate::ged_detail::gamma_power_logq(lp,nu,false);
      CHECK(std::isfinite(w));
      CHECK(cppunivariate::ged_detail::gamma_power_log(w,nu,false)==doctest::Approx(lp).epsilon(1e-12));
    }
  }
}

TEST_CASE("GED unsupported log-scale overflow is a numerical failure") {
  CHECK(std::isnan(cppunivariate::distributions::pged(1,0,1,4e-306,false,true)));
  CHECK(std::isfinite(cppunivariate::distributions::pged(1,0,1,1e-300,false,true)));
}

TEST_CASE("GED uniform-edge neighbors match high precision") {
  // Independent 360-digit gamma-integral values: tail-extreme-data/edge-oracle.py.
  struct Row {double nu,x,ld,lp;};
  const Row rows[]={
    {100000000,1.732050807568877,-1.8039128164814298,-19.824051526222767},
    {100000000,1.7320508075688772,-1.803912823679201,-19.824051541098797},
    {100000000,1.7320508075688774,-1.8039128308769721,-19.824051555974826},
    {1e+100,1.732050807568877,-1.2424533248940002,-36.913206110018642},
    {1e+100,1.7320508075688772,-1.2424533248940002,-38.080312530773824},
    {1e+100,1.7320508075688774,-std::numeric_limits<double>::infinity(),-std::numeric_limits<double>::infinity()},
    {1.7976931348623157e+308,1.732050807568877,-1.2424533248940002,-36.913206110018642},
    {1.7976931348623157e+308,1.7320508075688772,-1.2424533248940002,-38.080312530773824},
    {1.7976931348623157e+308,1.7320508075688774,-std::numeric_limits<double>::infinity(),-std::numeric_limits<double>::infinity()},
  };
  for(const auto& row:rows) {
    CAPTURE(row.nu);CAPTURE(row.x);
    const double ld=cppunivariate::distributions::dged(row.x,0,1,row.nu,true);
    const double lp=cppunivariate::distributions::pged(row.x,0,1,row.nu,false,true);
    if(std::isinf(row.ld)) {CHECK(ld==row.ld);CHECK(lp==row.lp);}
    else {
      CHECK(ld==doctest::Approx(row.ld).epsilon(1e-12));
      CHECK(lp==doctest::Approx(row.lp).epsilon(1e-12));
      CHECK(cppunivariate::distributions::pged(row.x,0,1,row.nu,false,false)/std::exp(row.lp)==doctest::Approx(1.).epsilon(1e-10));
    }
    CHECK(cppunivariate::distributions::dsged(row.x,0,1,row.nu,1,true)==ld);
    CHECK(cppunivariate::distributions::psged(-row.x,0,1,row.nu,1,true,true)==lp);
  }
}
TEST_CASE("GED compensated physical and skew uniform edges") {
  // Exact-input uniform-limit bounds: dpq-skew-data/edge.py (400 digits).
  const double nu=std::numeric_limits<double>::max(),x=std::sqrt(3.);
  CHECK(cppunivariate::distributions::pged(1+x,1,1,nu,false,true)==doctest::Approx(-38.080312530773824).epsilon(1e-12));
  CHECK(cppunivariate::distributions::dged(1+x,1,1,nu,true)==doctest::Approx(-1.2424533248940002).epsilon(1e-12));
  for(double sign:{-1.,1.}) {
    CHECK(cppunivariate::distributions::psged(sign*x,0,1,nu,2,sign<0,true)==doctest::Approx(-38.080312530773824).epsilon(1e-12));
    CHECK(cppunivariate::distributions::dsged(sign*x,0,1,nu,2,true)==doctest::Approx(-1.2424533248940002).epsilon(1e-12));
  }
}

TEST_CASE("GED subnormal gamma draws report failure") {
  struct SubnormalRng {
    double rgamma(double,double) {return std::numeric_limits<double>::denorm_min();}
    double runif(double a,double b) {return (a+b)/2;}
  } rng;
  for(double nu:{10.,1e8,std::numeric_limits<double>::max()})
    CHECK(std::isnan(cppunivariate::distributions::rged(rng,0,1,nu)));
}

TEST_CASE("GED uncertified fallback rejects a strictly better feasible witness") {
 const auto g=mltest::load_distribution_golden("ged");const auto data=mltest::as_span(g.data);
 auto certificate=cppunivariate::ged_shape_certificate(data,1e-8,200,NAN,NAN);
 certificate.status=cppunivariate::GedStatus::BfgsFailure;certificate.certified=false;
 certificate.stop_detail.loglik_lower=g.logLik+1;
 CHECK(cppunivariate::ged_uncertified(certificate,data,1e-8,200,NAN,NAN,NAN).status==cppunivariate::GedStatus::BfgsFailure);
}

TEST_CASE("GED retains converged searches after held-sd and short-work certificate failure") {
 const std::vector<double> x{.7321666961121761,2.2766042947262384,-.5491691631533211};
 const auto r=cppunivariate::ged_mle(mltest::as_span(x),1e-8,1000,NAN,1.0178559902092952,NAN);
 REQUIRE(r.status==cppunivariate::GedStatus::Ok);CHECK_FALSE(r.certified);
 CHECK(std::isfinite(r.logLik));
 const auto g=mltest::load_distribution_golden("ged");
 const auto short_work=cppunivariate::ged_mle(mltest::as_span(g.data),1e-8,16);
 REQUIRE(short_work.status==cppunivariate::GedStatus::Ok);CHECK_FALSE(short_work.certified);
}

TEST_CASE("GED point-score inclusion failure retains a converged conditional search") {
 const std::vector<double> x{-0.02133959176388924,-0.021339613103523683,-0.021339634443158128,-0.02133965578279257,-0.021339677122427014};
 const auto r=cppunivariate::ged_mle(mltest::as_span(x),1e-8,1000,NAN,NAN,3.016645308327854);
 REQUIRE(r.status==cppunivariate::GedStatus::Ok);CHECK_FALSE(r.certified);
 double ll=0;for(double v:x)ll+=cppunivariate::distributions::dged(v,r.mu,r.sigma,r.nu,true);
 CHECK(r.logLik==doctest::Approx(ll).epsilon(1e-10));
}

TEST_CASE("GED reflection does not prove a tie against an unresolved central knot") {
 const double t=.96295235253631053;const std::vector<double> x{-1,-t,0,t,1};
 const auto r=cppunivariate::ged_mle(mltest::as_span(x),1e-8,1000,NAN,NAN,.5);
 REQUIRE(r.status==cppunivariate::GedStatus::Ok);CHECK_FALSE(r.certified);CHECK_FALSE(r.proved_tie);
}

TEST_CASE("GED held shape and scale retain a converged point-score fallback") {
 const std::vector<double> x{-.02133959176388924,-.021339613103523683,-.021339634443158128,-.02133965578279257,-.021339677122427014};
 const auto r=cppunivariate::ged_mle(mltest::as_span(x),1e-8,1000,NAN,1.0178559902092952,3.016645308327854);
 REQUIRE(r.status==cppunivariate::GedStatus::Ok);CHECK_FALSE(r.certified);
 CHECK(r.sigma==1.0178559902092952);CHECK(r.nu==3.016645308327854);CHECK(std::isfinite(r.logLik));
}

TEST_CASE("GED uncertified knot fit refines scale at the original observation") {
  const std::vector<double> x{-1,-.9629523525363105,0,.9629523525363105,1};
  const auto r=cppunivariate::ged_mle(mltest::as_span(x),1e-10,1000,NAN,NAN,.5);
  REQUIRE(r.status==cppunivariate::GedStatus::Ok);
  CHECK_FALSE(r.certified);
  CHECK(r.mu==x[1]);
  CHECK(r.sigma==doctest::Approx(1.7200920482178039627483).scale(0.).epsilon(1e-10));
  CHECK(r.nu==.5);
}

TEST_CASE("GED held-scale accuracy propagates to SGED unit skew case 31101") {
  const std::vector<double> x{0.7321666961121761,2.2766042947262384,-0.5491691631533211};
  const double sd=1.0178559902092952,mean_truth=0.8637175657864586408862805910629875338342774,nu_truth=27.497336616831684338668525265498056683345282;
  const auto parent=cppunivariate::ged_mle(mltest::as_span(x),1e-10,1000,NAN,sd,NAN);
  const auto child=cppunivariate::sged_mle(mltest::as_span(x),1e-10,1000,NAN,sd,NAN,1.);
  REQUIRE(parent.status==cppunivariate::GedStatus::Ok);
  REQUIRE(child.status==cppunivariate::SgedStatus::Ok);
  CHECK(std::abs(parent.mu-mean_truth)<=1e-10*std::max(std::abs(mean_truth),sd));
  CHECK(std::abs(parent.nu/nu_truth-1)<=1e-10);
  CHECK(parent.sigma==sd);CHECK(child.sigma==sd);CHECK(child.xi==1.);
  CHECK(child.mu==parent.mu);CHECK(child.nu==parent.nu);
  CHECK(child.logLik==parent.logLik);CHECK(child.certified==parent.certified);
}

TEST_CASE("GED tail enclosure cannot certify a false underflowed center") {
 // The center lies outside the uniform edge, but its uncertainty interval
 // also contains radii inside it. Neither requested tail is certified.
 const cppunivariate::ged_detail::Radius r{1e-4,.01,.5493061443340549};
 for(bool lower:{false,true})for(bool log:{false,true})
  CHECK(std::isnan(cppunivariate::ged_detail::probability_precise(r,1e8,-std::log(2.),false,lower,log)));
}

TEST_CASE("GED representable tiny tails retain the relative error gate") {
 CHECK_FALSE(cppunivariate::ged_detail::probability_interval(-701.,-702.,-700.5,false));
 CHECK_FALSE(cppunivariate::ged_detail::probability_interval(-740.,-740.-1e-12,-740.+1e-12,false));
 CHECK(cppunivariate::ged_detail::probability_interval(-750.,-750.-1e-8,-750.+1e-8,false));
 const cppunivariate::ged_detail::Radius r{std::log(680.)/1e8,1e-14,.5493061443340549};
 CHECK(std::isnan(cppunivariate::ged_detail::probability_precise(r,1e8,-std::log(2.),true,true,false)));
 CHECK(std::isnan(cppunivariate::special::skew_probability::bounds(-701.,NAN,-702.,true,true,false)));
 const double aligned=16*std::numeric_limits<double>::denorm_min(),lp=std::log(aligned);
 CHECK(cppunivariate::special::skew_probability::bounds(lp,lp-1e-12,lp+1e-12,true,true,false)==aligned);
}
