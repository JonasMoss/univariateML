#include <limits>
#include "test_support.hpp"
#include "cppunivariate/distributions/lst/mle.hpp"
#include "cppunivariate/distributions/lst/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include "cppunivariate/core/finite_diff.hpp"
#include <vector>
#include <cmath>

TEST_CASE("lst: golden and analytic score") {
  auto g = mltest::load_distribution_golden("lst");
  auto r = cppunivariate::lst_mle(mltest::as_span(g.data),mltest::kFitTol,mltest::kFitMaxit);
  CHECK(r.status == cppunivariate::LstStatus::Ok);
  // Preserve the frozen BFGS-era golden tolerance. The certificate also
  // checks the complete coefficient box against independent roots below.
  CHECK(r.mu == doctest::Approx(g.estimates.at("mu")).epsilon(mltest::kFdTol));
  CHECK(r.sigma == doctest::Approx(g.estimates.at("sigma")).epsilon(mltest::kFdTol));
  CHECK(r.df == doctest::Approx(g.estimates.at("df")).epsilon(mltest::kFdTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kFdTol));
  cppunivariate::LstCost cost{mltest::as_span(g.data)};
  double p[3] = {r.mu,std::log(r.sigma),std::log(r.df-1.0)};
  double grad[3], numeric[3];
  cost.gradient(p,grad);
  cppunivariate::finite_diff_gradient(cost,p,numeric);
  for (int i=0;i<3;++i) {
    CHECK(std::abs(grad[i]) < 1e-3);
    CHECK(std::abs(numeric[i]) < 1e-3);
    CHECK(std::abs(grad[i]-numeric[i]) < 1e-4);
  }
}
TEST_CASE("lst: analytic gradients away from optimum") {
  const std::vector<double> x = {-3,-1,0,1,8};
  for (double df : {1.1,2.0,10.0,20000.0}) {
    cppunivariate::LstCost cost{mltest::as_span(x)};
    double p[3] = {0.3,std::log(1.2),std::log(df-1)};
    double a[3], b[3];
    cost.gradient(p,a);
    cppunivariate::finite_diff_gradient(cost,p,b);
    for (int j=0;j<3;++j) CHECK(a[j] == doctest::Approx(b[j]).epsilon(1e-5));
  }
}
TEST_CASE("lst: faces and collapse guard") {
  std::vector<double> x;
  for (int i=0;i<200;++i) x.push_back(cppunivariate::special::qt((i+0.5)/200,0.8,true,false));
  auto c = cppunivariate::lst_mle(mltest::as_span(x),1e-7,300);
  CHECK(c.status == cppunivariate::LstStatus::Ok);
  CHECK(c.df == 1.0);
  for (int i=0;i<200;++i) x[i] = cppunivariate::special::qnorm((i+0.5)/200,0,1,true,false);
  CHECK(cppunivariate::lst_mle(mltest::as_span(x),1e-7,300).status == cppunivariate::LstStatus::NormalBoundary);
  for (const std::vector<double> normal : {std::vector<double>{0,1}, {0,0,1,2}})
    CHECK(cppunivariate::lst_mle(mltest::as_span(normal),1e-7,200).status == cppunivariate::LstStatus::NormalBoundary);
  const std::vector<double> constant{1,1,1,1};
  CHECK(cppunivariate::lst_mle(mltest::as_span(constant),1e-7,200).status == cppunivariate::LstStatus::Unbounded);
}
TEST_CASE("lst: dpqr location-scale convention and tails") {
  cppunivariate::distributions::StdRng<> rng(123);
  for (double df : {0.5,1.0,1.5,3.0,10.0}) {
    for (double p : {0.001,0.2,0.8,0.999}) {
      const double q = cppunivariate::distributions::qlst(p,2,3,df,true,false);
      CHECK(cppunivariate::distributions::plst(q,2,3,df,true,false) == doctest::Approx(p));
      CHECK(cppunivariate::distributions::qlst(std::log(p),2,3,df,false,true) ==
            doctest::Approx(cppunivariate::distributions::qlst(p,2,3,df,false,false)));
      CHECK(cppunivariate::distributions::dlst(q,2,3,df,true) == doctest::Approx(cppunivariate::special::dt((q-2)/3,df,true)-std::log(3.0)));
    }
    CHECK(std::isfinite(cppunivariate::distributions::rlst(rng,2,3,df)));
  }
}

TEST_CASE("lst: fixed df score, ties and validation") {
  const std::vector<double> x = {-5,-2,-1,0,0,1,2,9};
  for (double df : {1.0,2.5,10.0}) {
    auto r = cppunivariate::lst_mle_fixed_df(mltest::as_span(x),df,1e-8,5000);
    REQUIRE(r.status == cppunivariate::LstStatus::Ok);
    CHECK(r.df == df);
    cppunivariate::LstCost cost{mltest::as_span(x),df};
    double p[2] = {r.mu,std::log(r.sigma)}, g[2];
    cost.gradient(p,g);
    CHECK(std::hypot(r.sigma*g[0],g[1]) < 1e-8);
    CHECK(r.logLik == doctest::Approx(-cost(p)).epsilon(1e-12));
  }
  for (double df : {0.0,-1.0,std::numeric_limits<double>::infinity()})
    CHECK(cppunivariate::lst_mle_fixed_df(mltest::as_span(x),df,1e-8,100).status == cppunivariate::LstStatus::InvalidDf);
  const std::vector<double> tied = {0,0,0,1};
  CHECK(cppunivariate::lst_mle_fixed_df(mltest::as_span(tied),2,1e-8,100).status == cppunivariate::LstStatus::Unbounded);
  CHECK(cppunivariate::lst_mle_fixed_df(mltest::as_span(tied),3,1e-8,100).status == cppunivariate::LstStatus::ScaleBoundary);
  CHECK(cppunivariate::lst_mle_fixed_df(mltest::as_span(tied),4,1e-8,5000).status == cppunivariate::LstStatus::Ok);
  CHECK(cppunivariate::lst_mle_fixed_df(mltest::as_span(x),3,1e-12,0).status == cppunivariate::LstStatus::IterationFailure);
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("lst: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("lst",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dlst(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::plst(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qlst(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rlst(rng, p[0], p[1], p[2]); });
}

TEST_CASE("lst: every held golden subset preserves coefficients and likelihood") {
  const auto g=mltest::load_distribution_golden("lst");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(int mask=1;mask<8;++mask) {
    CAPTURE(mask);
    const auto r=cppunivariate::lst_mle(mltest::as_span(g.data),1e-8,1000,((mask & 1) ? g.estimates.at("mu") : nan),((mask & 2) ? g.estimates.at("sigma") : nan),((mask & 4) ? g.estimates.at("df") : nan));
    if(mask & 1)CHECK(r.mu==g.estimates.at("mu"));
    if(mask & 2)CHECK(r.sigma==g.estimates.at("sigma"));
    if(mask & 4)CHECK(r.df==g.estimates.at("df"));
    CHECK(r.status==cppunivariate::LstStatus::Ok);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*g.data.size());
  }
}

TEST_CASE("lst: held-location half ties compare the normal face") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> x={0,1};
  CHECK(cppunivariate::lst_mle(mltest::as_span(x),1e-8,200,0,nan,nan).status==cppunivariate::LstStatus::Unbounded);
}

TEST_CASE("lst: held profiles preserve extreme units") {
  const std::vector<double> x={-2,-.5,0,.7,3};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto base=cppunivariate::lst_mle(mltest::as_span(x),1e-8,200,0,nan,1e20);
  for(double unit:{1e-300,1e300}) {
    auto y=x;for(double& v:y)v*=unit;
    const auto fit=cppunivariate::lst_mle(mltest::as_span(y),1e-8,200,0,nan,1e20);
    CHECK(fit.sigma/unit==doctest::Approx(base.sigma).epsilon(1e-8));
    CHECK(fit.logLik+x.size()*std::log(unit)==doctest::Approx(base.logLik).epsilon(1e-8));
  }
  CHECK(base.sigma==doctest::Approx(std::sqrt(2.748)).epsilon(1e-8));
}


// Generating mpmath snippet (binary64 inputs retained exactly):
// import mpmath as mp; mp.mp.dps=100
// n=mp.mpf(3); x=mp.mpf(1e300)
// print(-mp.log(mp.beta(n/2,mp.mpf('.5')))-mp.log(n)/2-(n+1)/2*mp.log1p(x*x/n))
// ls=lambda y: mp.log(mp.betainc(n/2,mp.mpf('.5'),0,n/(n+mp.exp(2*y)),regularized=True)/2)
// print(mp.exp(mp.findroot(lambda y: ls(y)+800,800/n)))
TEST_CASE("lst: targeted tail and endpoint regressions") {
  CHECK(cppunivariate::distributions::dlst(1e+300, 0.0, 1.0, 3.0, true) == doctest::Approx(-2761.905775865142).epsilon(1e-10));
  CHECK(cppunivariate::distributions::qlst(-800.0, 0.0, 1.0, 3.0, false, true) == doctest::Approx(6.698981179231598e+115).epsilon(1e-10));
}

// import mpmath as mp; mp.mp.dps=100
// n=mp.mpf(3); s=mp.mpf(1e-300); x=mp.mpf(1e300)
// c=(n/2-1)*mp.log(n)-mp.log(mp.beta(n/2,mp.mpf('.5')))
// print(mp.exp(mp.log(s)+(c+2200)/n),c-n*mp.log(x/s))
// The omitted Student tail correction is O((x/s)**-2), below 1e-1200.
TEST_CASE("lst: targeted tail scaling before exponentiation") {
  CHECK(cppunivariate::distributions::qlst(-2200,0,1e-300,3,false,true)
        == doctest::Approx(3.13882410493767e18).epsilon(1e-10));
  CHECK(cppunivariate::distributions::plst(1e300,0,1e-300,3,false,true)
        == doctest::Approx(-4144.5554439502375).epsilon(1e-10));
  CHECK(cppunivariate::distributions::dlst(1e300,0,1e-300,3,true)
        == doctest::Approx(-4834.232359559784).epsilon(1e-10));
}


TEST_CASE("lst: P3 restrictions and closed faces") {
 const std::vector<double> x{-100,-2,-.01,0,.01,2,100};const auto data=mltest::as_span(x);
 const auto f=cppunivariate::lst_mle(data,1e-8,1000); REQUIRE(f.status==cppunivariate::LstStatus::Ok);
 CHECK(f.df==1);CHECK(f.restricted==16u);
 const auto h=cppunivariate::lst_mle(data,1e-8,1000,0,1,.5);
 CHECK(h.status==cppunivariate::LstStatus::Ok);CHECK(h.restricted==0u);
}

TEST_CASE("lst: profile search evaluates each grid point once") {
  const std::vector<double> grid{-1.0,0.0,1.0};
  int grid_calls=0;
  const auto result=cppunivariate::lst_detail::maximize([&](double x) {
    if (x==-1.0 || x==0.0 || x==1.0) ++grid_calls;
    return -x*x;
  },grid,1e-8,0);
  CHECK(result.point==0.0);
  CHECK_FALSE(result.converged);
  CHECK(grid_calls==3);
}

TEST_CASE("lst: verified extreme conditional bracket") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(double unit:{1.0,1e100}) {
    const std::vector<double> x{0,1e-300*unit,unit};
    const auto r=cppunivariate::lst_mle(mltest::as_span(x),1e-10,200,0,nan,1);
    REQUIRE(r.status==cppunivariate::LstStatus::Ok);
    CHECK(r.sigma/unit==doctest::Approx(1e-300).epsilon(1e-9).scale(1e-300));
    CHECK(r.logLik+3*std::log(unit)==doctest::Approx(-std::log(1e-300)-3*std::log(cppunivariate::pi)-std::log(2.0)).epsilon(1e-10));
  }
  const std::vector<double> tiny{0,1e-300,1};
  CHECK(cppunivariate::lst_mle(mltest::as_span(tiny),1e-10,1,0,nan,1).status==cppunivariate::LstStatus::IterationFailure);
  const std::vector<double> edge{0,std::numeric_limits<double>::denorm_min(),1};
  CHECK(cppunivariate::lst_mle(mltest::as_span(edge),1e-10,200,0,nan,1).status==cppunivariate::LstStatus::IterationFailure);
  const double big=std::numeric_limits<double>::max();
  const std::vector<double> opposite{big,-big,0};
  const auto wide=cppunivariate::lst_mle(mltest::as_span(opposite),1e-10,200,big,nan,1);
  REQUIRE(wide.status==cppunivariate::LstStatus::Ok);
  CHECK(wide.sigma/big==doctest::Approx(std::sqrt((std::sqrt(73.0)-5)/6)).epsilon(1e-9));
  CHECK(std::isfinite(wide.logLik));
}

TEST_CASE("lst: face comparison cannot resolve rounded extreme objectives") {
  const double magnitude=1e20;
  CHECK(cppunivariate::compare_face(-magnitude,-magnitude,2e-8)==cppunivariate::FaceComparison::NumericalTie);
  CHECK(cppunivariate::compare_face(-magnitude,-2*magnitude,2e-8)==cppunivariate::FaceComparison::FirstWins);
  CHECK(cppunivariate::compare_face(-2*magnitude,-magnitude,2e-8)==cppunivariate::FaceComparison::SecondWins);
  CHECK(cppunivariate::compare_face(-10.,-10.,2e-8)==cppunivariate::FaceComparison::FirstWins);
}

TEST_CASE("lst: audited huge held-parameter magnitude") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  std::vector<double> x{1,2};
  cppunivariate::span s{x.data(),x.size()};
  const auto r=cppunivariate::lst_mle(s,1e-6,200,0,1e-10,nan);
  CHECK((r.status==cppunivariate::LstStatus::Ok || r.status==cppunivariate::LstStatus::BfgsFailure || r.status==cppunivariate::LstStatus::IterationFailure));
  CHECK(r.status!=cppunivariate::LstStatus::NormalBoundary);
}

TEST_CASE("lst: certificate root boxes, bounded refusals and exact sample faces") {
  const auto g=mltest::load_distribution_golden("lst");
  const auto r=cppunivariate::t_certificate(mltest::as_span(g.data),1);
  REQUIRE(r.status==cppunivariate::TCertificateStatus::Finite);
  CHECK(r.coefficient_lower[0]<0.07455070888521645);
  CHECK(r.coefficient_upper[0]>0.07455070888521645);
  CHECK(r.coefficient_lower[2]<3.0242427094268456);
  CHECK(r.coefficient_upper[2]>3.0242427094268456);
  CHECK(r.profiles<=4096);
  for(int budget:{0,1}) {
    const auto b=cppunivariate::t_certificate(mltest::as_span(g.data),1,budget);
    CHECK(b.status==cppunivariate::TCertificateStatus::Budget);
    CHECK(b.profiles<=budget);
    CHECK(std::isnan(b.mu));
  }
  const std::vector<double> balanced{0,0,1,1},constant{2,2,2},invalid{0,1,INFINITY};
  CHECK(cppunivariate::t_certificate(mltest::as_span(balanced),1).status==cppunivariate::TCertificateStatus::Normal);
  CHECK(cppunivariate::t_certificate(mltest::as_span(constant),1).status==cppunivariate::TCertificateStatus::Unbounded);
  CHECK(cppunivariate::t_certificate(mltest::as_span(invalid),1).status==cppunivariate::TCertificateStatus::InvalidData);
  CHECK_FALSE((cppunivariate::t_cert_detail::I(1)/cppunivariate::t_cert_detail::I(-1,1)).valid());
}

TEST_CASE("lst: certificate cancellation margins at series cutoffs") {
  // Independent 80-digit values, generated by the retained arithmetic checker.
  for(double u:{std::nextafter(.01,0.),.01,std::nextafter(.01,1.)}) {
    const auto remainder=cppunivariate::t_cert_detail::lr(cppunivariate::t_cert_detail::I(u));
    const auto positive=cppunivariate::t_cert_detail::nl(cppunivariate::t_cert_detail::I(u));
    const auto negative=cppunivariate::t_cert_detail::nl(cppunivariate::t_cert_detail::I(-u));
    CHECK(remainder.lo<0.000049669146831917154);
    CHECK(remainder.hi>0.000049669146831917154);
    CHECK(positive.lo<0.000099336653377651115);
    CHECK(positive.hi>0.000099336653377651115);
    CHECK(negative.lo<0.000100670013377905084);
    CHECK(negative.hi>0.000100670013377905084);
  }
}

TEST_CASE("lst: certificate resolves retained competing shape maxima") {
  // Independent 80-digit roots and weighted counterexamples from TASK-140.
  {
    const std::vector<double> support{-3.3753182637169354,-1.2713352565886626,-0.8001089566898665,0.4550011648748676,0.4631607376683885,0.48440591294209223,1.2931658287635732};
    const std::vector<int> counts{1,11,8,11,7,8,9};
    std::vector<double> x;for(std::size_t j=0;j<support.size();++j)x.insert(x.end(),counts[j],support[j]);
    for(double floor:{1.,2.}){
      const auto r=cppunivariate::t_certificate(mltest::as_span(x),floor);
      REQUIRE(r.status==cppunivariate::TCertificateStatus::Finite);
      CHECK(r.profiles<2000);
      CHECK(r.coefficient_lower[0]<0.020343843626779073);
      CHECK(r.coefficient_upper[0]>0.020343843626779073);
      CHECK(r.coefficient_lower[1]<0.9693130976148451);
      CHECK(r.coefficient_upper[1]>0.9693130976148451);
      CHECK(r.coefficient_lower[2]<33.90540690730407);
      CHECK(r.coefficient_upper[2]>33.90540690730407);
    }
  }
  {
    const std::vector<double> support{-2.5267827738984026,-1.3086958612269082,-0.917311177389449,-0.7470612434645413,-0.6822670978323393,0.5175256317384838,0.8692557396694677};
    const std::vector<int> counts{2,1,4,4,1,8,11};
    std::vector<double> x;for(std::size_t j=0;j<support.size();++j)x.insert(x.end(),counts[j],support[j]);
    for(double floor:{1.,2.}){
      const auto r=cppunivariate::t_certificate(mltest::as_span(x),floor);
      REQUIRE(r.status==cppunivariate::TCertificateStatus::Finite);
      CHECK(r.profiles<2000);
      CHECK(r.coefficient_lower[0]<0.01964559936858695);
      CHECK(r.coefficient_upper[0]>0.01964559936858695);
      CHECK(r.coefficient_lower[1]<0.9797749809886654);
      CHECK(r.coefficient_upper[1]>0.9797749809886654);
      CHECK(r.coefficient_lower[2]<49.705241724474675);
      CHECK(r.coefficient_upper[2]>49.705241724474675);
    }
  }
}

TEST_CASE("lst: certificate closes the shallow retained normal sample") {
  const std::vector<double> x{0.050954662455960954,-0.41009991819347347,-0.27514948570271125,1.738894665940981,0.7827929830488856,-0.6094885407586247,0.16676804415486185,2.1605246276102874,-0.2387465089482371,-2.306020779595189,0.7712778390740149,0.868444961637359,1.0920372377409862,-0.6483986076864828,0.46991123089786935,-0.42877220249733083,-0.6711991838272713,-0.02262549573968949,-1.4089791730105745,0.03065718253139144,0.7394838316847435,-0.9188201716268543,-0.9824239413172151,-1.2717118121176605,0.7072827328968989,1.1005705147845657,-0.31552546144261623,0.24877221879696262,-0.7638326991319095,-0.8839285063895818,0.6899451665252806,-0.06974028824316554,-0.497841648238298,0.8381413762133866,-0.26457437687827884,-0.38679889567518655,-0.1278879326472602,-0.06134395969980899,-1.9342834627670904,-0.9650194594318576,0.12782341531106875,-0.5397482053246428,1.393079864160065,-1.8866780642217882,0.48159934707033075,0.37619107597635915,-0.7842379492323549,0.16661327780961455,0.06570257810373901,1.5033378387590548,-1.5584653425173214,0.5575584145186088,-0.7733568166152429,0.17687773844287813,-0.453478962001212,-2.6420087840828708,-2.0359122776319354,0.40101402508509254,2.637322231411576,-1.2594962766931357,0.896216299275718,-1.124759799790428,-0.31742515720096587,-0.3950423506433641,2.0094231968658294,0.6377817892826372,1.362962386207389,0.4899601440673617,0.7184328548359303,0.7179914812225302,0.616123650652679,0.2661075130421014,-0.6585341582181332,-1.012868755256849,-0.5993024660073559,1.0810268064227586,0.5679558382979852,1.3096494218560304,0.8540889501811413,-1.3334550306342379};
  for(double floor:{1.,2.}) {
    const auto r=cppunivariate::t_certificate(mltest::as_span(x),floor);
    REQUIRE(r.status==cppunivariate::TCertificateStatus::Finite);
    CHECK(r.profiles<2000);
    CHECK(r.coefficient_lower[2]<218.257164105);
    CHECK(r.coefficient_upper[2]>218.257164105);
    // A smaller explicit budget refuses, preserving witnesses and no estimate.
    const auto b=cppunivariate::t_certificate(mltest::as_span(x),floor,40);
    CHECK(b.status==cppunivariate::TCertificateStatus::Budget);
    CHECK(b.profiles==40);
    CHECK(std::isnan(b.mu));
    CHECK(std::isfinite(b.stop_detail.loglik_lower));
    CHECK(b.stop_detail.loglik_upper==INFINITY);
  }
}
