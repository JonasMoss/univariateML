#include <limits>
#include "test_support.hpp"
#include "cppunivariate/distributions/slash/mle.hpp"
#include <vector>
TEST_CASE("slash: golden and likelihood") {
  const auto g=mltest::load_distribution_golden("slash");
  const auto f=cppunivariate::slash_mle(mltest::as_span(g.data));
  REQUIRE(f.status==cppunivariate::SlashStatus::Ok);
  CHECK(f.logLik==doctest::Approx(g.logLik).epsilon(1e-7));
  double ll=0;for(double x:g.data)ll+=cppunivariate::distributions::dslash(x,f.mu,f.sigma,true);
  CHECK(ll==doctest::Approx(f.logLik).epsilon(1e-12));
  std::vector<double> theta={f.mu,f.sigma};
  double norm2=0;
  for(std::size_t j=0;j<theta.size();++j) {
    const double h=1e-5*(1+std::abs(theta[j]));auto plus=theta,minus=theta;
    plus[j]+=h;minus[j]-=h;double score=0;
    for(double x:g.data)score+=(cppunivariate::distributions::dslash(x,plus[0],plus[1],true)-cppunivariate::distributions::dslash(x,minus[0],minus[1],true))/(2*h);
    norm2+=score*score;
  }
  CHECK(std::sqrt(norm2)<1e-3);
}

TEST_CASE("slash: direct log tails and quantiles") {
  for(bool lower:{false,true})for(double lp:{-1e-6,-.6931471805599453,-10.,-100.,-600.}) {
    const double q=cppunivariate::distributions::qslash(lp,0,1,lower,true);
    CHECK(cppunivariate::distributions::pslash(q,0,1,lower,true)==doctest::Approx(lp).epsilon(1e-10));
  }
  CHECK(std::isinf(cppunivariate::distributions::qslash(-1000,0,1,true,true)));
  CHECK(std::isinf(cppunivariate::distributions::qslash(0,0,1)));
  CHECK(std::isinf(cppunivariate::distributions::qslash(1,0,1)));
  CHECK(std::isnan(cppunivariate::distributions::dslash(0,0,-1)));
}
TEST_CASE("slash: multiplicity, held scale and invalid inputs") {
  const std::vector<double> x={0,0,0,1},half={0,0,-1,1},two={0,1};
  CHECK(cppunivariate::slash_mle(mltest::as_span(x)).status==cppunivariate::SlashStatus::Unbounded);
  CHECK(cppunivariate::slash_mle(mltest::as_span(x),1e-9,200,1).status==cppunivariate::SlashStatus::Ok);
  CHECK(cppunivariate::slash_mle(mltest::as_span(half)).status==cppunivariate::SlashStatus::CollapseLimit);
  CHECK(cppunivariate::slash_mle(mltest::as_span(two)).status==cppunivariate::SlashStatus::Ok);
  CHECK(cppunivariate::slash_mle({},1e-9,200).status==cppunivariate::SlashStatus::InvalidInput);
  CHECK(cppunivariate::distributions::dslash(0)==doctest::Approx(cppunivariate::distributions::dslash(1e-10)).epsilon(1e-14));
}

#include "cppunivariate/distributions/slash/dpqr.hpp"

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("slash: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("slash",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dslash(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pslash(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qslash(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rslash(rng, p[0], p[1]); });
}

TEST_CASE("slash: every held golden subset preserves coefficients and likelihood") {
  const auto g=mltest::load_distribution_golden("slash");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(int mask=1;mask<4;++mask) {
    CAPTURE(mask);
    const auto r=cppunivariate::slash_mle(mltest::as_span(g.data),1e-10,1000,((mask & 2) ? g.estimates.at("sigma") : nan),((mask & 1) ? g.estimates.at("mu") : nan));
    if(mask & 1)CHECK(r.mu==g.estimates.at("mu"));
    if(mask & 2)CHECK(r.sigma==g.estimates.at("sigma"));
    CHECK(r.status==cppunivariate::SlashStatus::Ok);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*g.data.size());
  }
}


// mpmath generator: import mpmath as mp; mp.mp.dps=100
// N=lambda z: mp.erfc(-z/mp.sqrt(2))/2; F=lambda x: N(x)+mp.expm1(-x*x/2)/(mp.sqrt(2*mp.pi)*x); mp.findroot(lambda x: F(x)-mp.exp(mp.mpf(float(-mp.log(2)))), (mp.mpf("1e-17"),mp.mpf("1e-16")))
TEST_CASE("slash: logarithmic quantile regression") {
  CHECK(cppunivariate::distributions::qslash(-0.6931471805599453, 0.0, 1.0, true, true) == doctest::Approx(5.812988313780069e-17).epsilon(1e-11));
  CHECK(cppunivariate::distributions::qslash(-0.6931471805599453, 0.0, 1.0, false, true) == doctest::Approx(-5.812988313780069e-17).epsilon(1e-11));
}

TEST_CASE("slash: support endpoints on log input") {
  CHECK(cppunivariate::distributions::qslash(cppunivariate::special::kNegInf, 0.0, 1.0, true, true) == cppunivariate::special::kNegInf);
  CHECK(cppunivariate::distributions::qslash(0.0, 0.0, 1.0, false, true) == cppunivariate::special::kNegInf);
  CHECK(cppunivariate::distributions::qslash(cppunivariate::special::kNegInf, 0.0, 1.0, false, true) == cppunivariate::special::kPosInf);
  CHECK(cppunivariate::distributions::qslash(0.0, 0.0, 1.0, true, true) == cppunivariate::special::kPosInf);
}

// At x=mp.mpf(float(1e100)), evaluate the same mpmath tails and
// the README log density; constants are also in the independent truth table.
TEST_CASE("slash: extreme observation log tails and density") {
  CHECK(cppunivariate::distributions::dslash(1e100, 0.0, 1.0, true) == doctest::Approx(-461.4359571320138).epsilon(1e-11));
  CHECK(cppunivariate::distributions::pslash(1e100, 0.0, 1.0, true, true) == doctest::Approx(0.0).epsilon(1e-11));
  CHECK(cppunivariate::distributions::pslash(1e100, 0.0, 1.0, false, true) == doctest::Approx(-231.17744783260923).epsilon(1e-11));
}

// import mpmath as mp; mp.mp.dps=100
// F=lambda z:mp.erfc(-z/mp.sqrt(2))/2+mp.expm1(-z*z/2)/(mp.sqrt(2*mp.pi)*z)
// float(F(2)); mp.mpf(float(-1e308))+2*mp.mpf(float(1e308))
TEST_CASE("slash: finite affine quantile despite overflowing distance") {
  CHECK(cppunivariate::distributions::qslash(0.8047742111076985,-1e308,1e308,true,false) == doctest::Approx(1e308).epsilon(1e-10));
}

TEST_CASE("slash: P3 failed refinement cannot inherit a grid point status") {
  const std::vector<double> values{-2,-.3,.1,.8,4};
  CHECK(cppunivariate::slash_mle(mltest::as_span(values),1e-9,1).status==cppunivariate::SlashStatus::IterationFailure);
}

TEST_CASE("slash: scale score saturation preserves binary64 subtraction") {
  const double edge=42;
  for(double t:{std::nextafter(edge,0.0),edge,std::nextafter(edge,100.0),43.,50.,100.,700.}) {
    CAPTURE(t);
    CHECK(1-2*t/std::expm1(t)==1.0);
  }
  // Cover the preceding transition as well as the exact saturation edge.
  for(int i=0;i<=10000;++i) {
    const double t=40+i*.0005;
    const double original=1-2*t/std::expm1(t);
    CHECK((t>=42?1:original)==original);
  }
}

TEST_CASE("slash: cached scale logarithm agrees with direct density formula") {
  for(double s:{1e-300,1e-5,1.,1e5,1e300})for(double m:{-1e308,0.,1e308})
    for(double x:{-1e308,-1.,0.,1e-10,1.,1e308}) {
      const double z=cppunivariate::special::scaled_difference(x,m,s),t=(.5*z)*z;
      const double kernel=t<1e-8 ? -std::log(2.)-t/2+t*t/24 : std::log(-std::expm1(-t))-2*cppunivariate::distributions::slash_log_distance(x,m,s);
      const double original=kernel-.91893853320467274178-std::log(s);
      CHECK(cppunivariate::distributions::slash_log_density_cached(x,m,s,std::log(s))==original);
      CHECK(cppunivariate::distributions::dslash(x,m,s,true)==original);
    }
}

TEST_CASE("slash: held-scale certificate covers separated modes") {
  const std::vector<double> x{-100,-100,-99,98,100,100,100};
  const auto r=cppunivariate::slash_mle(mltest::as_span(x),1e-9,200,1.0);
  REQUIRE(r.status==cppunivariate::SlashStatus::Ok);
  for(int i=0;i<=20000;++i) {
    const double m=-100+200.0*i/20000;
    double ll=0;
    for(double v:x)ll+=cppunivariate::distributions::dslash(v,m,1,true);
    CHECK(ll<=r.logLik+1e-9*(1+std::abs(r.logLik))+1e-10);
  }
}

TEST_CASE("slash: held-scale subdivision work limit fails closed") {
  const std::vector<double> x{-2,0,3};
  const cppunivariate::slash_detail::Profile p{x,1};
  const std::vector<double> grid{-2,3};
  const std::vector<double> ll{p(-2).logLik,p(3).logLik};
  auto best=p(-2);
  double lo=0,hi=0;
  CHECK_FALSE(cppunivariate::slash_detail::certify_held(p,grid,ll,0,best,lo,hi,0));
  CHECK(cppunivariate::slash_detail::certify_held(p,grid,ll,0,best,lo,hi));
}

TEST_CASE("slash: held-scale extreme coordinates never return a nonfinite fit") {
  const std::vector<double> x{-1.7e308,1.6e308,1.7e308};
  const auto r=cppunivariate::slash_mle(mltest::as_span(x),1e-9,200,1e307);
  CHECK((r.status==cppunivariate::SlashStatus::IterationFailure ||
         (r.status==cppunivariate::SlashStatus::Ok && std::isfinite(r.mu) && std::isfinite(r.logLik))));
}

TEST_CASE("slash: verified extreme conditional bracket") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  double base=0;
  for(double unit:{1.0,1e100}) {
    const std::vector<double> x{0,1e-300*unit,unit};
    const auto r=cppunivariate::slash_mle(mltest::as_span(x),1e-8,500,nan,0);
    REQUIRE(r.status==cppunivariate::SlashStatus::Ok);
    const double z=(1e-300*unit)/r.sigma,t=z*z/2;
    CHECK(std::expm1(t)==doctest::Approx(2*t).epsilon(1e-9));
    CHECK(r.logLik+3*std::log(unit)>686);
    if(unit==1)base=r.logLik;else CHECK(r.logLik+3*std::log(unit)==doctest::Approx(base).epsilon(1e-10));
  }
  const std::vector<double> limited{0,1e-300,1};
  CHECK(cppunivariate::slash_mle(mltest::as_span(limited),1e-8,1,nan,0).status==cppunivariate::SlashStatus::IterationFailure);
  const std::vector<double> edge{0,std::numeric_limits<double>::denorm_min(),1};
  CHECK(cppunivariate::slash_mle(mltest::as_span(edge),1e-8,500,nan,0).status==cppunivariate::SlashStatus::IterationFailure);
  const std::vector<double> rounded{0,1e-322,1e-22};
  CHECK(cppunivariate::slash_mle(mltest::as_span(rounded),1e-8,500,nan,0).status==cppunivariate::SlashStatus::IterationFailure);
  const double big=std::numeric_limits<double>::max();
  const std::vector<double> opposite{big,-big,0};
  const auto wide=cppunivariate::slash_mle(mltest::as_span(opposite),1e-8,500,nan,big);
  REQUIRE(wide.status==cppunivariate::SlashStatus::Ok);
  CHECK(std::isfinite(wide.logLik));
  CHECK(std::abs(cppunivariate::slash_detail::scale_score(opposite,big,std::log(wide.sigma)))<1e-3);
}

TEST_CASE("slash: free-scale interval bound covers conditional profiles") {
  const std::vector<double> x{-3,-1,0,2,8};
  const cppunivariate::slash_detail::Profile p{x,cppunivariate::special::kNaN,200};
  for(int j=0;j<40;++j) {
    const double a=-3+j*.25,b=a+.25;
    std::vector<double> d;
    for(double v:x)d.push_back(v<a?a-v:(v>b?v-b:0));
    const double upper=cppunivariate::slash_detail::residual_upper(d);
    for(int k=0;k<=10;++k) {
      const auto f=p(a+(b-a)*k/10);
      CHECK(f.status==cppunivariate::SlashStatus::Ok);
      CHECK(f.logLik<=upper+1e-10);
    }
  }
  const std::vector<double> half{0,0,1,2};
  CHECK(std::isfinite(cppunivariate::slash_detail::residual_upper(half)));
}

TEST_CASE("slash: free-scale certificate fails closed on exhausted budget") {
  const std::vector<double> x{-3,-1,0,2,8},grid{-3,0,8};
  const cppunivariate::slash_detail::Profile p{x,cppunivariate::special::kNaN,200};
  std::vector<double> ll;for(double m:grid)ll.push_back(p(m).logLik);
  auto best=p(0);double lo=0,hi=0;
  CHECK_FALSE(cppunivariate::slash_detail::certify_free(p,grid,ll,0,-INFINITY,best,lo,hi,0));
}

TEST_CASE("slash: adjacent represented grid points are bounded before subdivision") {
  const std::vector<double> x{-6,-3,3,5,0,0,-1,5,4,-2,-1,1,4,-1,-4,4,-9,-2,1};
  const auto f=cppunivariate::slash_mle(mltest::as_span(x));
  CHECK(f.status==cppunivariate::SlashStatus::Ok);
  CHECK(std::isfinite(f.logLik));
}

TEST_CASE("slash: exhausted certificate retains its witness and an unclosed upper bound") {
  const std::vector<double> x{-2,0,3},grid{-2,3};
  const cppunivariate::slash_detail::Profile p{x,1};
  const std::vector<double> ll{p(-2).logLik,p(3).logLik};auto best=p(-2);
  double lo=0,hi=0;
  CHECK_FALSE(cppunivariate::slash_detail::certify_held(p,grid,ll,0,best,lo,hi,0));
  REQUIRE(best.budget_exhausted);REQUIRE(best.stop_detail.candidates.size()==1);
  CHECK(best.stop_detail.candidates[0].coef[0]==-2);CHECK(best.stop_detail.candidates[0].coef[1]==1);
  CHECK(best.stop_detail.candidates[0].loglik_lower==best.logLik);
  CHECK(std::isinf(best.stop_detail.loglik_upper));
}

TEST_CASE("slash: cached scale scores preserve the uncached summation") {
  const double big=std::numeric_limits<double>::max();
  const std::vector<double> x{-big,-2,-1e-300,0,1e-300,2,big};
  for(double m:{-big,0.,big}) {
    const cppunivariate::slash_detail::ScaleScore cached{x,m};
    for(double ls:{-740.,-350.,-10.,0.,10.,350.,710.}) {
      double direct=0;
      for(double v:x) {
        const double logt=2*cppunivariate::distributions::slash_log_distance_cached(v,m,ls)-std::log(2.0);
        const double t=std::exp(std::min(logt,4.0));
        direct+=t<1e-7?-1+t-t*t/6:(t>=42?1:1-2*t/std::expm1(t));
      }
      CHECK(cached(ls)==direct);
    }
    CHECK(std::isnan(cached(cppunivariate::special::kNaN)));
  }
  // Both sides of the new shortcut.
  for(double logt:{std::nextafter(4.,0.),4.,std::nextafter(4.,5.)}) {
    const std::vector<double> d{std::sqrt(2*std::exp(logt))};
    const cppunivariate::slash_detail::ScaleScore cached{d,0};
    CHECK(cached(0)==1.);
  }
}
