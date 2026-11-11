// invtrgamma: golden, score gate, reciprocal relation, mirrored faces, fixed shapes.

#include "test_support.hpp"

#include "cppunivariate/distributions/invgamma/mle.hpp"
#include "cppunivariate/distributions/invtrgamma/dpqr.hpp"
#include "cppunivariate/distributions/invtrgamma/mle.hpp"
#include "cppunivariate/distributions/invweibull/mle.hpp"
#include "cppunivariate/distributions/lnorm/mle.hpp"
#include "cppunivariate/distributions/pareto/mle.hpp"
#include "cppunivariate/distributions/rng_std.hpp"

#include <cmath>
#include <vector>

namespace {
double invtrgamma_loglik(const std::vector<double> &x, double a, double t, double s) {
  double ll = 0.0;
  for (double v : x) ll += cppunivariate::distributions::dinvtrgamma(v, a, t, s, true);
  return ll;
}
}  // namespace

TEST_CASE("invtrgamma: golden, score gate and reciprocal of trgamma") {
  auto g = mltest::load_distribution_golden("invtrgamma");
  auto r = cppunivariate::invtrgamma_mle(mltest::as_span(g.data));
  REQUIRE(r.status == cppunivariate::InvTrGammaStatus::Ok);
  CHECK(r.shape1 == doctest::Approx(g.estimates.at("shape1")).epsilon(mltest::kGolden_invtrgamma).scale(0));
  CHECK(r.shape2 == doctest::Approx(g.estimates.at("shape2")).epsilon(mltest::kGolden_invtrgamma).scale(0));
  CHECK(r.scale == doctest::Approx(g.estimates.at("scale")).epsilon(mltest::kGolden_invtrgamma).scale(0));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_invtrgamma).scale(0));
  CHECK(invtrgamma_loglik(g.data, r.shape1, r.shape2, r.scale) ==
        doctest::Approx(r.logLik).epsilon(1e-12));
  const double h = 1e-5;
  double score[3];
  for (int k = 0; k < 3; ++k) {
    double p[3] = {std::log(r.shape1), std::log(r.shape2), std::log(r.scale)};
    double m[3] = {p[0], p[1], p[2]};
    p[k] += h;
    m[k] -= h;
    score[k] = (invtrgamma_loglik(g.data, std::exp(p[0]), std::exp(p[1]), std::exp(p[2])) -
                invtrgamma_loglik(g.data, std::exp(m[0]), std::exp(m[1]), std::exp(m[2]))) / (2 * h);
  }
  CHECK(std::sqrt(score[0] * score[0] + score[1] * score[1] + score[2] * score[2]) < 1e-3);
  std::vector<double> y;
  double sum_log = 0;
  for (double v : g.data) {
    y.push_back(1 / v);
    sum_log += std::log(v);
  }
  auto f = cppunivariate::trgamma_mle(mltest::as_span(y));
  CHECK(f.shape1 == doctest::Approx(r.shape1).epsilon(1e-8));
  CHECK(f.shape2 == doctest::Approx(r.shape2).epsilon(1e-8));
  CHECK(f.scale == doctest::Approx(1 / r.scale).epsilon(1e-8));
  CHECK(r.logLik == doctest::Approx(f.logLik - 2 * sum_log).epsilon(1e-12));
}

TEST_CASE("invtrgamma: lognormal and Pareto faces") {
  const int n = 400;
  std::vector<double> lognormal, pareto;
  for (int i = 0; i < n; ++i) {
    const double p = (i + 0.5) / n;
    const double z = cppunivariate::special::qnorm(p, 0, 1, true, false);
    // Left-skewed log data: reciprocal of the trgamma lognormal-face sample.
    lognormal.push_back(std::exp(-(z + 0.05 * (z * z - 1))));
    pareto.push_back(2 * std::pow(1 - p, -1 / 1.7));
  }
  auto l = cppunivariate::invtrgamma_mle(mltest::as_span(lognormal));
  CHECK(l.status == cppunivariate::InvTrGammaStatus::LognormalBoundary);
  CHECK(l.logLik == doctest::Approx(cppunivariate::lognormal_mle(mltest::as_span(lognormal)).logLik)
                        .epsilon(1e-14));
  auto p = cppunivariate::invtrgamma_mle(mltest::as_span(pareto));
  const auto pm = cppunivariate::pareto_mle(mltest::as_span(pareto));
  CHECK(p.status == cppunivariate::InvTrGammaStatus::ParetoBoundary);
  CHECK(p.scale == doctest::Approx(pm.min).epsilon(1e-14));
  CHECK(p.logLik == doctest::Approx(pm.logLik).epsilon(1e-12));
}

TEST_CASE("invtrgamma: fixed shapes reduce to inverse gamma and inverse Weibull") {
  auto g = mltest::load_distribution_golden("invtrgamma");
  const auto x = mltest::as_span(g.data);
  auto fg = cppunivariate::invtrgamma_mle(x, 1e-10, 100, std::nan(""), 1.0);
  auto ig = cppunivariate::invgamma_mle(x, 1e-14, 100);
  REQUIRE(fg.status == cppunivariate::InvTrGammaStatus::Ok);
  CHECK(fg.shape1 == doctest::Approx(ig.shape).epsilon(1e-10));
  CHECK(fg.scale == doctest::Approx(ig.scale).epsilon(1e-10));
  CHECK(fg.logLik == doctest::Approx(ig.logLik).epsilon(1e-12));
  auto fw = cppunivariate::invtrgamma_mle(x, 1e-12, 100, 1.0);
  auto iw = cppunivariate::invweibull_mle(x, 1e-12, 100);
  REQUIRE(fw.status == cppunivariate::InvTrGammaStatus::Ok);
  CHECK(fw.shape2 == doctest::Approx(iw.shape).epsilon(1e-9));
  CHECK(fw.scale == doctest::Approx(iw.scale).epsilon(1e-9));
  CHECK(fw.logLik == doctest::Approx(iw.logLik).epsilon(1e-12));
  auto both = cppunivariate::invtrgamma_mle(x, 1e-12, 100, 2.5, 0.8);
  CHECK(both.logLik == doctest::Approx(invtrgamma_loglik(g.data, 2.5, 0.8, both.scale)).epsilon(1e-12));
  CHECK(cppunivariate::invtrgamma_mle(x, 1e-12, 100, 0.0).status ==
        cppunivariate::InvTrGammaStatus::InvalidFixed);
}

TEST_CASE("invtrgamma: dpqr") {
  namespace d = cppunivariate::distributions;
  const double q = d::qinvtrgamma(0.3, 2, 1.5, 0.5, true, false);
  CHECK(d::pinvtrgamma(q, 2, 1.5, 0.5, true, false) == doctest::Approx(0.3).epsilon(1e-12));
  CHECK(d::dinvtrgamma(0.7, 2, 1.5, 0.5, true) ==
        doctest::Approx(d::dtrgamma(1 / 0.7, 2, 1.5, 2, true) - 2 * std::log(0.7)).epsilon(1e-13));
  cppunivariate::distributions::StdRng<> rng(9);
  for (int i = 0; i < 100; ++i) CHECK(d::rinvtrgamma(rng, 2, 1.5, 0.5) > 0);
}

TEST_CASE("invtrgamma: both shapes fixed on constant data and stable density") {
  const std::vector<double> x = {0.5, 0.5};
  const auto r = cppunivariate::invtrgamma_mle(mltest::as_span(x), 1e-10, 100, 3, 2);
  REQUIRE(r.status == cppunivariate::InvTrGammaStatus::Ok);
  CHECK(r.scale == doctest::Approx(std::sqrt(3.0)/2));
  CHECK(r.logLik == doctest::Approx(invtrgamma_loglik(x, 3, 2, r.scale)).epsilon(1e-12));
  const double a = 1e10;
  CHECK(cppunivariate::distributions::dinvtrgamma(1, a, 1, a, true) ==
        doctest::Approx(0.5 * std::log(a) - 0.91893853320467274178 - 1/(12*a)).epsilon(1e-12));
  CHECK(std::isnan(cppunivariate::distributions::dinvtrgamma(std::nan(""), 2, 1, 1, false)));
}

TEST_CASE("invtrgamma: evaluation close to the Pareto face") {
  namespace d = cppunivariate::distributions;
  const double q = d::qinvtrgamma(.1, 1e-6, 1e6, 1, true, false);
  CHECK(q > 1.1);
  CHECK(q < 1.12);
  CHECK(d::pinvtrgamma(q, 1e-6, 1e6, 1, true, false) == doctest::Approx(.1).epsilon(1e-12));
  cppunivariate::distributions::StdRng<> rng(7);
  for (int i=0; i<20; ++i) CHECK(std::isfinite(d::rinvtrgamma(rng, 1e-6, 1e6, 1)));
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("invtrgamma: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("invtrgamma",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dinvtrgamma(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pinvtrgamma(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qinvtrgamma(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rinvtrgamma(rng, p[0], p[1], p[2]); });
}

TEST_CASE("P2 invtrgamma golden holds") {
 auto g=mltest::load_distribution_golden("invtrgamma");
 const double nan=std::numeric_limits<double>::quiet_NaN();
for(int mask=0;mask<4;++mask){double a=mask&1?g.estimates.at("shape1"):nan,b=mask&2?g.estimates.at("shape2"):nan;auto r=cppunivariate::invtrgamma_mle_fixed_scale(mltest::as_span(g.data),g.estimates.at("scale"),a,b);REQUIRE(r.status==cppunivariate::InvTrGammaStatus::Ok);
 CHECK(r.scale==g.estimates.at("scale"));
 if(mask&1)CHECK(r.shape1==a);
 if(mask&2)CHECK(r.shape2==b);
 CHECK(r.logLik>=g.logLik-1e-5);}
}


// mpmath generator: import mpmath as mp; mp.mp.dps=100
// P=lambda z:mp.gammainc(3,0,z)/mp.gamma(3)
// Q=lambda z:mp.gammainc(3,z,mp.inf)/mp.gamma(3)
// lo=mp.exp(mp.findroot(lambda y:mp.log(P(mp.exp(y)))+800,-mp.mpf(800)/3))
// hi=mp.findroot(lambda z:mp.log(Q(z))+800,800)
// [mp.mpf(".5")/hi**(mp.mpf(2)/3),mp.mpf(".5")/lo**(mp.mpf(2)/3)]
TEST_CASE("invtrgamma: logarithmic quantile regression") {
  CHECK(cppunivariate::distributions::qinvtrgamma(-800.0, 3.0, 1.5, 0.5, true, true) == doctest::Approx(0.005741335328336336).epsilon(1e-11));
  CHECK(cppunivariate::distributions::qinvtrgamma(-800.0, 3.0, 1.5, 0.5, false, true) == doctest::Approx(5.419452300248306e+76).epsilon(1e-11));
}

TEST_CASE("invtrgamma: support endpoints on log input") {
  CHECK(cppunivariate::distributions::qinvtrgamma(cppunivariate::special::kNegInf, 3.0, 1.5, 0.5, true, true) == 0.0);
  CHECK(cppunivariate::distributions::qinvtrgamma(0.0, 3.0, 1.5, 0.5, false, true) == 0.0);
  CHECK(cppunivariate::distributions::qinvtrgamma(cppunivariate::special::kNegInf, 3.0, 1.5, 0.5, false, true) == cppunivariate::special::kPosInf);
  CHECK(cppunivariate::distributions::qinvtrgamma(0.0, 3.0, 1.5, 0.5, true, true) == cppunivariate::special::kPosInf);
}

// At x=mp.mpf(float(1e100)), evaluate the same mpmath tails and
// the README log density; constants are also in the independent truth table.
TEST_CASE("invtrgamma: extreme observation log tails and density") {
  CHECK(cppunivariate::distributions::dinvtrgamma(1e100, 3.0, 1.5, 0.5, true) == doctest::Approx(-1269.8286455316966).epsilon(1e-11));
  CHECK(cppunivariate::distributions::pinvtrgamma(1e100, 3.0, 1.5, 0.5, true, true) == doctest::Approx(0.0).epsilon(1e-11));
  CHECK(cppunivariate::distributions::pinvtrgamma(1e100, 3.0, 1.5, 0.5, false, true) == doctest::Approx(-1041.0742136290683).epsilon(1e-11));
}

TEST_CASE("invtrgamma: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("invtrgamma");
  const auto x=mltest::as_span(g.data);
  CHECK(cppunivariate::invtrgamma_mle(x,1e-8,1).status==cppunivariate::InvTrGammaStatus::IterationFailure);
}

TEST_CASE("invtrgamma: structured face payloads") {
  std::vector<double> x,p;
  for(int i=0;i<400;++i) {
    const double u=(i+.5)/400,z=cppunivariate::special::qnorm(u,0,1,true,false);
    x.push_back(std::exp(-(z+.05*(z*z-1))));
    p.push_back(2*std::pow(1-u,-1/1.7));
  }
  const auto n=cppunivariate::invtrgamma_mle(mltest::as_span(x));
  const auto ref=cppunivariate::lognormal_mle(mltest::as_span(x));
  REQUIRE(n.stop_detail.limit_coef.size()==2);
  CHECK(n.stop_detail.limit_coef[0]==doctest::Approx(ref.meanlog));
  CHECK(n.stop_detail.limit_coef[1]==doctest::Approx(ref.sdlog));
  CHECK(n.stop_detail.limit_loglik==doctest::Approx(ref.logLik));
  const auto r=cppunivariate::invtrgamma_mle(mltest::as_span(p));
  REQUIRE(r.stop_detail.limit_coef.size()==2);
  double sum=0;const double endpoint=*std::min_element(p.begin(),p.end());
  for(double v:p)sum+=std::log(v/endpoint);
  CHECK(r.stop_detail.limit_coef[0]==doctest::Approx(p.size()/sum));
  CHECK(r.stop_detail.limit_coef[1]==doctest::Approx(endpoint));
  CHECK(r.stop_detail.limit_loglik==doctest::Approx(r.logLik));
}

TEST_CASE("invtrgamma: exhausted coefficient solve retains a feasible witness") {
  const auto g=mltest::load_distribution_golden("invtrgamma");
  const auto r=cppunivariate::invtrgamma_mle({g.data.data(),g.data.size()},1e-10,5);
  CHECK(r.status==cppunivariate::InvTrGammaStatus::IterationFailure);
  // Five solver iterations cannot isolate the coefficients to the required
  // width. This is a numerical stop before the global certificate cap.
  REQUIRE_FALSE(r.stop_detail.budget_exhausted);
  REQUIRE_FALSE(r.stop_detail.candidates.empty());
  CHECK(std::isfinite(r.stop_detail.loglik_lower));
  CHECK(r.stop_detail.loglik_upper==INFINITY);
  const auto& c=r.stop_detail.candidates.front();
  REQUIRE(c.coef.size()==3);
  double ll=0;
  for(double v:g.data)ll+=cppunivariate::distributions::dinvtrgamma(v,c.coef[0],c.coef[1],c.coef[2],true);
  CHECK(ll>=c.loglik_lower-1e-8);
  CHECK(ll<=c.loglik_upper+1e-8);
  CHECK_FALSE(cppunivariate::invtrgamma_mle({g.data.data(),g.data.size()},1e-10,100).stop_detail.budget_exhausted);
}

TEST_CASE("invtrgamma: reciprocal transport shifts every diagnostic field") {
  cppunivariate::StopDetail d;
  d.loglik_lower=1;d.loglik_upper=2;d.segment_loglik_upper=3;d.limit_loglik=4;
  d.limit_model=cppunivariate::StopModel::Power;d.limit_coef={2,3};
  d.candidates={{{1,2,4},5,6,cppunivariate::StopModel::None},{{2,3},7,8,cppunivariate::StopModel::Power}};
  d.comparison_tie=true;
  const auto r=cppunivariate::invtrgamma_transport_detail(d,-10);
  CHECK(r.loglik_lower==-9);CHECK(r.loglik_upper==-8);
  CHECK(r.segment_loglik_upper==-7);CHECK(r.limit_loglik==-6);
  CHECK(r.comparison_tie);
  CHECK(r.limit_model==cppunivariate::StopModel::Pareto);
  CHECK(r.limit_coef==std::vector<double>{3,.5});
  CHECK(r.candidates[0].coef==std::vector<double>{1,2,.25});
  CHECK(r.candidates[0].loglik_lower==-5);CHECK(r.candidates[0].loglik_upper==-4);
  CHECK(r.candidates[1].model==cppunivariate::StopModel::Pareto);
  CHECK(r.candidates[1].coef==std::vector<double>{3,.5});
  CHECK(r.candidates[1].loglik_lower==-3);CHECK(r.candidates[1].loglik_upper==-2);
}

TEST_CASE("invtrgamma: held endpoint agrees with reciprocal fit") {
  const std::vector<double> x={10,1/.3,1};
  const std::vector<double> y={.1,.3,1};
  const auto r=cppunivariate::invtrgamma_mle_fixed_scale(mltest::as_span(x),1);
  const auto t=cppunivariate::trgamma_mle_fixed_scale(mltest::as_span(y),1);
  REQUIRE(r.status==cppunivariate::InvTrGammaStatus::Ok);
  CHECK(r.shape1==doctest::Approx(t.shape1).epsilon(1e-10));
  CHECK(r.shape2==doctest::Approx(t.shape2).epsilon(1e-10));
  CHECK(r.logLik==doctest::Approx(t.logLik-2*(std::log(10)+std::log(1/.3))).epsilon(1e-11));
  const std::vector<double> tiny={1e-310,1e-305};
  const auto extreme=cppunivariate::invtrgamma_mle_fixed_scale(mltest::as_span(tiny),1e-308,2,1.5);
  CHECK(extreme.status==cppunivariate::InvTrGammaStatus::Ok);
  CHECK(extreme.scale==1e-308);
  CHECK(std::isfinite(extreme.logLik));
}

TEST_CASE("invtrgamma: finite coupled power survives latent log overflow") {
 CHECK(cppunivariate::distributions::qinvtrgamma(.5,1e-309,1e308,1,true,false)==doctest::Approx(1024.0).epsilon(1e-9).scale(0));
 CHECK(cppunivariate::distributions::dinvtrgamma(1,1e-309,1e308,1,true)==doctest::Approx(-3.302585092994043).epsilon(1e-10));
}
