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
    if(mask==1||mask==2)CHECK_FALSE(r.certified);
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
  CHECK(r.status==cppunivariate::LstStatus::Ok);
  CHECK(std::isfinite(r.logLik));
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

TEST_CASE("work budgets: unit multipliers widen before multiplication and saturate") {
  CHECK(cppunivariate::solver_budget(1)==1);
  CHECK(cppunivariate::certificate_budget(1)==128);
  CHECK(cppunivariate::enumeration_budget(1)==100000);
  CHECK(cppunivariate::enumeration_budget(100000)==10000000000LL);
  CHECK(cppunivariate::enumeration_budget(2147483647)==10000000000LL);
  CHECK(cppunivariate::certificate_budget(2147483647)==12800000LL);
  CHECK(cppunivariate::solver_budget(0)==0);
}

TEST_CASE("lst and std: iterlim scales certificate budget and preserves witnesses") {
  const std::vector<double> x{0.050954662455960954,-0.41009991819347347,-0.27514948570271125,1.738894665940981,0.7827929830488856,-0.6094885407586247,0.16676804415486185,2.1605246276102874,-0.2387465089482371,-2.306020779595189,0.7712778390740149,0.868444961637359,1.0920372377409862,-0.6483986076864828,0.46991123089786935,-0.42877220249733083,-0.6711991838272713,-0.02262549573968949,-1.4089791730105745,0.03065718253139144,0.7394838316847435,-0.9188201716268543,-0.9824239413172151,-1.2717118121176605,0.7072827328968989,1.1005705147845657,-0.31552546144261623,0.24877221879696262,-0.7638326991319095,-0.8839285063895818,0.6899451665252806,-0.06974028824316554,-0.497841648238298,0.8381413762133866,-0.26457437687827884,-0.38679889567518655,-0.1278879326472602,-0.06134395969980899,-1.9342834627670904,-0.9650194594318576,0.12782341531106875,-0.5397482053246428,1.393079864160065,-1.8866780642217882,0.48159934707033075,0.37619107597635915,-0.7842379492323549,0.16661327780961455,0.06570257810373901,1.5033378387590548,-1.5584653425173214,0.5575584145186088,-0.7733568166152429,0.17687773844287813,-0.453478962001212,-2.6420087840828708,-2.0359122776319354,0.40101402508509254,2.637322231411576,-1.2594962766931357,0.896216299275718,-1.124759799790428,-0.31742515720096587,-0.3950423506433641,2.0094231968658294,0.6377817892826372,1.362962386207389,0.4899601440673617,0.7184328548359303,0.7179914812225302,0.616123650652679,0.2661075130421014,-0.6585341582181332,-1.012868755256849,-0.5993024660073559,1.0810268064227586,0.5679558382979852,1.3096494218560304,0.8540889501811413,-1.3334550306342379};
  for(double floor:{1.,2.}) {
    const auto stop=cppunivariate::t_certificate(mltest::as_span(x),floor,-1,7);
    REQUIRE(stop.status==cppunivariate::TCertificateStatus::Budget);
    CHECK(stop.profiles==896);
    CHECK(stop.stop_detail.budget_exhausted);
    CHECK(std::isfinite(stop.stop_detail.loglik_lower));
    CHECK_FALSE(stop.stop_detail.candidates.empty());
    CHECK(cppunivariate::t_certificate(mltest::as_span(x),floor,-1,20).status==cppunivariate::TCertificateStatus::Finite);
  }
}

TEST_CASE("lst: held-scale certificate isolates competing maxima and retains ties") {
  const std::vector<double> two{-3,3};
  const auto tie=cppunivariate::lst_mle(mltest::as_span(two),1e-7,200,NAN,1,3);
  REQUIRE(tie.status==cppunivariate::LstStatus::CertificateTie);
  CHECK(tie.stop_detail.comparison_tie);
  REQUIRE(tie.stop_detail.candidates.size()==2);
  // For two symmetric observations, maxima are +/-sqrt(a^2-df*sigma^2).
  for(const auto& c:tie.stop_detail.candidates)CHECK(std::abs(c.coef[0])==doctest::Approx(std::sqrt(6.)).epsilon(1e-8));
  const std::vector<double> one{-1,1};
  const auto unique=cppunivariate::lst_mle(mltest::as_span(one),1e-7,200,NAN,1,3);
  REQUIRE(unique.status==cppunivariate::LstStatus::Ok);
  CHECK(std::abs(unique.mu)<1e-8);
  const std::vector<double> asymmetric{-3,-2,0,1,5};
  const auto fit=cppunivariate::lst_mle(mltest::as_span(asymmetric),1e-7,200,NAN,1,3);
  REQUIRE(fit.status==cppunivariate::LstStatus::Ok);
  CHECK(fit.mu==doctest::Approx(-.3210104101280357).epsilon(1e-6));
  const auto budget=cppunivariate::t_location_detail::solve(mltest::as_span(two),1,3,0);
  CHECK(budget.failure==1);
  CHECK(budget.detail.budget_exhausted);
  CHECK(std::isfinite(budget.detail.loglik_lower));
  CHECK_FALSE(budget.detail.candidates.empty());
}

TEST_CASE("lst: held-location scale bracket proves coefficient accuracy and collapse values") {
  const std::vector<double> x{0,1};
  const auto equality=cppunivariate::lst_mle(mltest::as_span(x),1e-7,200,0,NAN,1);
  REQUIRE(equality.status==cppunivariate::LstStatus::ScaleBoundary);
  CHECK(equality.stop_detail.limit_loglik==doctest::Approx(-2*std::log(cppunivariate::pi)));
  const std::vector<double> symmetric{-3,3};
  for(double df:{.25,1.,3.,1e20,1e300}){
    const auto fit=cppunivariate::lst_mle(mltest::as_span(symmetric),1e-7,200,0,NAN,df);
    REQUIRE(fit.status==cppunivariate::LstStatus::Ok);
    CHECK(fit.sigma==doctest::Approx(3).epsilon(1e-9));
  }
  const auto small=cppunivariate::lst_mle(mltest::as_span(symmetric),1e-7,1,0,NAN,3);
  CHECK(small.status!=cppunivariate::LstStatus::Ok);
}

TEST_CASE("lst: fixed-shape joint root inclusion works up to normal-sized shape") {
  const std::vector<double> x{-2,-1,0,1,3};
  for(double df:{1.,3.,1e20,1e300}) {
    const auto fit=cppunivariate::lst_mle(mltest::as_span(x),1e-7,500,NAN,NAN,df);
    REQUIRE(fit.status==cppunivariate::LstStatus::Ok);
    double mu=fit.mu,scale=fit.sigma;
    CHECK(cppunivariate::t_location_detail::joint_inclusion(mltest::as_span(x),df,500,mu,scale));
    CHECK(mu==doctest::Approx(fit.mu).epsilon(1e-8));
    CHECK(scale==doctest::Approx(fit.sigma).epsilon(1e-8));
  }
}

TEST_CASE("lst: non-Cauchy held-location equality has the correct scale-zero value") {
  const std::vector<double> x{0,0,0,1};
  const auto r=cppunivariate::lst_mle(mltest::as_span(x),1e-7,200,0,NAN,3);
  REQUIRE(r.status==cppunivariate::LstStatus::ScaleBoundary);
  const double c=cppunivariate::special::lgamma(2)-cppunivariate::special::lgamma(1.5)-.5*std::log(3*cppunivariate::pi);
  CHECK(r.stop_detail.limit_loglik==doctest::Approx(4*c+2*std::log(3)).epsilon(1e-12));
}

TEST_CASE("lst: subunit joint shape stops with a witness rather than an EM maximum") {
  const std::vector<double> x{-5,-4,-3,-2,-1,1,2,3,4,5};
  const auto r=cppunivariate::lst_mle(mltest::as_span(x),1e-7,200,NAN,NAN,.5);
  REQUIRE(r.status==cppunivariate::LstStatus::NumericalFailure);
  CHECK(std::isnan(r.mu));CHECK(std::isnan(r.sigma));
  CHECK(std::isfinite(r.stop_detail.loglik_lower));
  CHECK(r.stop_detail.loglik_upper==INFINITY);
  CHECK_FALSE(r.stop_detail.candidates.empty());
}

TEST_CASE("lst: held scale has a certified decreasing-shape Cauchy regime") {
  const std::vector<double> x{-100,-2,-.01,0,.01,2,100};
  const auto proof=cppunivariate::t_location_detail::decreasing_shape(mltest::as_span(x),1,200);
  REQUIRE(proof.result==1);
  CHECK(proof.evaluations<cppunivariate::certificate_budget(200));
  const auto r=cppunivariate::lst_mle(mltest::as_span(x),1e-7,200,NAN,1,NAN);
  REQUIRE(r.status==cppunivariate::LstStatus::Ok);
  CHECK(r.df==1);CHECK(r.restricted==16u);CHECK(std::abs(r.mu)<1e-8);
  CHECK(r.iterations<=cppunivariate::certificate_budget(200));
  const auto small=cppunivariate::lst_mle(mltest::as_span(x),1e-7,1,NAN,1,NAN);
  CHECK(small.status!=cppunivariate::LstStatus::Ok);
}

TEST_CASE("lst: subunit joint equality is a numerical refusal, not a selected face") {
  const std::vector<double> x{-1,0,1};
  const auto r=cppunivariate::lst_mle_fixed_df(mltest::as_span(x),.5,1e-7,200);
  REQUIRE(r.status==cppunivariate::LstStatus::NumericalFailure);
  CHECK(std::isfinite(r.stop_detail.loglik_lower));
  CHECK(r.stop_detail.limit_coef.empty());
}

TEST_CASE("lst: fused collapse comparison does not manufacture rational equality") {
  const std::vector<double> x{0,1,2,3};
  const double df=1./3;
  REQUIRE(df*3==1); // Rounded multiplication hides the negative exact exponent.
  REQUIRE(std::fma(df,3.,-1.)<0);
  const auto held=cppunivariate::lst_mle(mltest::as_span(x),1e-7,200,0,NAN,df);
  CHECK(held.status==cppunivariate::LstStatus::Unbounded);
  const auto joint=cppunivariate::lst_mle_fixed_df(mltest::as_span(x),df,1e-7,200);
  CHECK(joint.status==cppunivariate::LstStatus::Unbounded);
}

TEST_CASE("lst: location candidate bounds evaluate the rounded physical coefficient") {
  const double center=1e7;
  const std::vector<double> x{center,center,std::nextafter(center,INFINITY),center+1};
  const auto r=cppunivariate::t_location_detail::solve(mltest::as_span(x),1e-4,3,500);
  REQUIRE(r.failure==0);
  REQUIRE_FALSE(r.detail.candidates.empty());
  for(const auto& candidate:r.detail.candidates){
    const double p[2]={candidate.coef[0],std::log(candidate.coef[1])};
    const double ll=-cppunivariate::LstCost{mltest::as_span(x),3,true}(p);
    CHECK(ll>=candidate.loglik_lower);CHECK(ll<=candidate.loglik_upper);
  }
}

TEST_CASE("lst: joint numerical witnesses retain the represented translated likelihood") {
  const double a=1e7,b=std::nextafter(a,INFINITY),c=std::nextafter(b,INFINITY),d=std::nextafter(c,INFINITY);
  const std::vector<double> x{a,b,c,d};
  const auto r=cppunivariate::lst_mle_fixed_df(mltest::as_span(x),3,1e-7,500);
  REQUIRE(r.status==cppunivariate::LstStatus::NumericalFailure);
  REQUIRE_FALSE(r.stop_detail.candidates.empty());
  for(const auto& candidate:r.stop_detail.candidates){
    const double p[2]={candidate.coef[0],std::log(candidate.coef[1])};
    const double ll=-cppunivariate::LstCost{mltest::as_span(x),3,true}(p);
    CHECK(ll>=candidate.loglik_lower);CHECK(ll<=candidate.loglik_upper);
  }
  CHECK(r.stop_detail.loglik_upper==INFINITY);
}

TEST_CASE("lst: balanced Cauchy midpoint rounding cannot return an endpoint as a maximizer") {
  const double a=1e7,b=std::nextafter(a,INFINITY);
  const std::vector<double> x{a,a,b,b};
  const auto r=cppunivariate::lst_mle_fixed_df(mltest::as_span(x),1,1e-7,200);
  REQUIRE(r.status==cppunivariate::LstStatus::NumericalFailure);
  REQUIRE_FALSE(r.stop_detail.candidates.empty());
  const auto& candidate=r.stop_detail.candidates.front();
  const double p[2]={candidate.coef[0],std::log(candidate.coef[1])};
  const double ll=-cppunivariate::LstCost{mltest::as_span(x),1,true}(p);
  CHECK(ll>=candidate.loglik_lower);CHECK(ll<=candidate.loglik_upper);
  CHECK(r.stop_detail.loglik_upper==INFINITY);
}

TEST_CASE("lst: joint critical shape carries the proved global collapse supremum") {
  const std::vector<double> x{0,0,0,1};
  const auto r=cppunivariate::lst_mle_fixed_df(mltest::as_span(x),3,1e-7,200);
  REQUIRE(r.status==cppunivariate::LstStatus::ScaleBoundary);
  REQUIRE(r.stop_detail.limit_coef.size()==2);
  CHECK(r.stop_detail.limit_coef[0]==0);CHECK(r.stop_detail.limit_coef[1]==3);
  const double c=cppunivariate::special::lgamma(2)-cppunivariate::special::lgamma(1.5)-.5*std::log(3*cppunivariate::pi);
  const double limit=4*c+2*std::log(3);
  CHECK(r.stop_detail.loglik_lower<=limit);CHECK(r.stop_detail.loglik_upper>=limit);
  CHECK(r.stop_detail.limit_model==cppunivariate::StopModel::TCollapsePath);
}

TEST_CASE("lst: representable Cauchy location is projected onto the maximizing curve") {
  const double a=1e7,b=std::nextafter(a,INFINITY),c=std::nextafter(b,INFINITY),d=std::nextafter(c,INFINITY);
  const std::vector<double> x{a,a,d,d};
  const auto r=cppunivariate::lst_mle_fixed_df(mltest::as_span(x),1,1e-7,200);
  REQUIRE(r.status==cppunivariate::LstStatus::Ok);
  CHECK(r.mu>a);CHECK(r.mu<d);
  CHECK(r.sigma/(b-a)==doctest::Approx(std::sqrt(2.)).epsilon(1e-6));
  CHECK(r.logLik==doctest::Approx(-4*(std::log(cppunivariate::pi)+std::log(d-a))).epsilon(1e-12));
}

TEST_CASE("lst: finite data with an unrepresentable range stop as a numerical failure") {
  const std::vector<double> x{-1e308,0,1e308};
  const auto r=cppunivariate::lst_mle_fixed_df(mltest::as_span(x),3,1e-7,200);
  CHECK(r.status==cppunivariate::LstStatus::NumericalFailure);
  CHECK(std::isnan(r.mu));CHECK(std::isnan(r.sigma));
  CHECK(r.stop_detail.loglik_upper==INFINITY);
}

TEST_CASE("lst: held solver caps preserve physical finite witnesses") {
  const std::vector<double> x{-3,-1,.2,1,8};
  const auto r=cppunivariate::lst_mle_fixed_df(mltest::as_span(x),3,1e-7,1);
  REQUIRE(r.status==cppunivariate::LstStatus::IterationFailure);
  REQUIRE_FALSE(r.stop_detail.candidates.empty());
  CHECK(std::isfinite(r.stop_detail.loglik_lower));CHECK(r.stop_detail.loglik_upper==INFINITY);
}

TEST_CASE("lst: held-scale location precision uses empirical spread") {
  const std::vector<double> x{-2,-1,0,1,3};
  const auto r=cppunivariate::lst_mle(mltest::as_span(x),1e-7,500,NAN,1e100,3);
  REQUIRE(r.status==cppunivariate::LstStatus::Ok);
  CHECK(r.mu==doctest::Approx(.2).epsilon(1e-7));
  const double tiny=std::numeric_limits<double>::denorm_min();
  const std::vector<double> subnormal{tiny,2*tiny};
  const auto s=cppunivariate::lst_mle(mltest::as_span(subnormal),1e-7,500,0,NAN,.001);
  CHECK(s.status==cppunivariate::LstStatus::NumericalFailure);
}

TEST_CASE("lst: uncertified free-shape searches still stop on solver exhaustion") {
  const auto g=mltest::load_distribution_golden("lst");
  for(int mask:{1,2,3}) {
    const auto r=cppunivariate::lst_mle(mltest::as_span(g.data),1e-8,1,
      mask&1?g.estimates.at("mu"):NAN,mask&2?g.estimates.at("sigma"):NAN,NAN);
    CHECK(r.status==(mask==2?cppunivariate::LstStatus::CertificateBudget:mask==3?cppunivariate::LstStatus::NumericalFailure:cppunivariate::LstStatus::IterationFailure));
  }
}

TEST_CASE("lst: shape-only certificate agrees with the independent 80-digit root") {
  const auto g=mltest::load_distribution_golden("lst");
  const auto r=cppunivariate::lst_mle(mltest::as_span(g.data),1e-8,200,g.estimates.at("mu"),g.estimates.at("sigma"),NAN);
  REQUIRE(r.status==cppunivariate::LstStatus::Ok);
  CHECK(r.df==doctest::Approx(3.02424271362830351424).epsilon(1e-7));
  CHECK(r.logLik==doctest::Approx(-898.624781104657356037).epsilon(1e-12));
  const std::vector<double> x{-1,1};
  const auto normal=cppunivariate::lst_mle(mltest::as_span(x),1e-8,200,0,1,NAN);
  REQUIRE(normal.status==cppunivariate::LstStatus::NormalBoundary);
  CHECK(normal.stop_detail.limit_model==cppunivariate::StopModel::Norm);
  CHECK(normal.stop_detail.limit_coef==std::vector<double>{0,1});
  const auto stopped=cppunivariate::lst_mle(mltest::as_span(g.data),1e-8,1,g.estimates.at("mu"),g.estimates.at("sigma"),NAN);
  CHECK(stopped.status!=cppunivariate::LstStatus::Ok);
  CHECK(std::isfinite(stopped.stop_detail.loglik_lower));
}

TEST_CASE("lst: subunit joint global uniqueness fails under strict atom inequalities") {
  const std::vector<double> x{-100,-99,0,99,100};
  const double B=10000,C=9801;
  // At location zero the unique scale score root has a=df*s^2 below.
  const auto a=(cppunivariate::t_cert_detail::root(cppunivariate::t_cert_detail::I(4*(B+C)*(B+C)+20*B*C))-2*(B+C))/10;
  const auto central_scale=cppunivariate::t_cert_detail::root(a*2);
  cppunivariate::t_cert_detail::Sum difference;
  for(double v:x){
    const auto left=cppunivariate::t_cert_detail::log1p(cppunivariate::t_cert_detail::sq((cppunivariate::t_cert_detail::I(v)-99.5)/.5)*2);
    const auto right=cppunivariate::t_cert_detail::log1p(cppunivariate::t_cert_detail::sq(cppunivariate::t_cert_detail::I(v)/central_scale)*2);
    difference.add((right-left)*.75);
  }
  const auto gap=difference.get()+cppunivariate::t_cert_detail::log(central_scale/.5)*5;
  REQUIRE(gap.valid());CHECK(gap.lo>.44);CHECK(gap.hi<.45);
  // Strict atoms give existence. This witness beats every fit at zero;
  // reflection therefore forces at least two distinct global maximizers.
  const auto stopped=cppunivariate::lst_mle_fixed_df(mltest::as_span(x),.5,1e-8,200);
  REQUIRE(stopped.status==cppunivariate::LstStatus::NonUniqueSymmetry);
  REQUIRE(stopped.stop_detail.candidates.size()==2);
  for(const auto& point:stopped.stop_detail.candidates){
    const double p[2]={point.coef[0],std::log(point.coef[1])};
    const double ll=-cppunivariate::LstCost{mltest::as_span(x),.5,true}(p);
    CHECK(ll>=point.loglik_lower);CHECK(ll<=point.loglik_upper);
  }
}

TEST_CASE("lst: shape-only holds allow a certified shape below one half") {
  const std::vector<double> x{-100,100};
  const auto r=cppunivariate::lst_mle(mltest::as_span(x),1e-8,200,0,1,NAN);
  REQUIRE(r.status==cppunivariate::LstStatus::Ok);
  CHECK(r.df>0);CHECK(r.df<.5);
  CHECK(r.restricted==0u);
  CHECK(r.logLik> -10000-std::log(2*cppunivariate::pi));
}

TEST_CASE("lst: free-shape held solver stops retain admissible witnesses and unknown upper bounds") {
  const auto g=mltest::load_distribution_golden("lst");
  for(int mask:{1,2,3}) {
    const auto r=cppunivariate::lst_mle(mltest::as_span(g.data),1e-8,1,
      mask&1?g.estimates.at("mu"):NAN,mask&2?g.estimates.at("sigma"):NAN,NAN);
    REQUIRE(r.status!=cppunivariate::LstStatus::Ok);
    CHECK(r.stop_detail.loglik_upper==INFINITY);
    CHECK(std::isfinite(r.stop_detail.loglik_lower));
    CHECK_FALSE(r.stop_detail.candidates.empty());
    for(const auto& candidate:r.stop_detail.candidates) {
      REQUIRE(candidate.coef.size()==3);
      const double point[2]={candidate.coef[0],std::log(candidate.coef[1])};
      const double ll=-cppunivariate::LstCost{mltest::as_span(g.data),candidate.coef[2],true}(point);
      CHECK(ll>=candidate.loglik_lower);CHECK(ll<=candidate.loglik_upper);
    }
  }
}

TEST_CASE("lst: shape-only certificate refuses unjustifiable requested coefficient precision") {
  const auto g=mltest::load_distribution_golden("lst");
  const auto r=cppunivariate::lst_mle(mltest::as_span(g.data),1e-20,200,g.estimates.at("mu"),g.estimates.at("sigma"),NAN);
  CHECK(r.status!=cppunivariate::LstStatus::Ok);
  CHECK(r.stop_detail.loglik_upper==INFINITY);
  CHECK(std::isfinite(r.stop_detail.loglik_lower));
}

TEST_CASE("lst: default budget distinguishes scalar certificates and joint numerical fits") {
  const auto g=mltest::load_distribution_golden("lst");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(int mask=1;mask<=3;++mask) {
    CAPTURE(mask);
    const auto r=cppunivariate::lst_mle(mltest::as_span(g.data),1.220703e-4,200,
      mask&1?g.estimates.at("mu"):nan,
      mask&2?g.estimates.at("sigma"):nan,nan);
    REQUIRE(r.status==cppunivariate::LstStatus::Ok);
    CHECK(r.certified==(mask==3));
    CHECK(r.logLik>=g.logLik-1e-6*g.data.size());
  }
}

TEST_CASE("lst: normal-relative value bounds reject a dominated near-normal point") {
  const std::vector<double> x={-1,2};
  cppunivariate::StopDetail point;
  cppunivariate::t_shape_detail::retain_point(mltest::as_span(x),.2,1.5297058413452655,8886110.5205078721,false,point);
  const auto fit=cppunivariate::norm_mle(mltest::as_span(x),.2,NAN);
  const auto normal=cppunivariate::t_shape_detail::normal_point(mltest::as_span(x),fit.mean,fit.sd);
  REQUIRE(point.candidates.size()==1);
  REQUIRE(normal.candidates.size()==1);
  CHECK(point.candidates[0].loglik_upper-point.loglik_lower<1e-9);
  CHECK(cppunivariate::t_shape_detail::dominated(point,normal));
  CHECK(point.candidates.size()==2);
  const auto r=cppunivariate::lst_mle(mltest::as_span(x),1.220703e-4,200,.2,NAN,NAN);
  CHECK(r.status==cppunivariate::LstStatus::NormalBoundary);
  CHECK(r.certified);
}

TEST_CASE("lst: signed normal profile covers every finite shape at actual holds") {
  std::vector<double> x;
  for(int j=0;j<100;++j)x.push_back(cppunivariate::special::qnorm((j+.5)/100.,0,1,true,false));
  for(int mask:{1,2}){
    const double mu=mask==1?0:NAN,scale=mask==2?1:NAN;
    const auto proof=cppunivariate::t_signed_detail::solve(mltest::as_span(x),mu,scale,false,200);
    REQUIRE(proof.proved);CHECK(proof.coefficient_upper<0);CHECK(proof.cutoff>proof.lower_shape);
    CHECK(proof.evaluations<=cppunivariate::certificate_budget(200));
    const auto result=cppunivariate::lst_mle(mltest::as_span(x),1e-8,200,mu,scale,NAN);
    CHECK(result.status==cppunivariate::LstStatus::NormalBoundary);CHECK(result.certified);
    CHECK(std::isfinite(result.stop_detail.loglik_upper));
  }
}

TEST_CASE("lst: signed face zero and uncertain coefficients make no claim") {
  std::vector<cppunivariate::t_cert_detail::I> q{0,0,0,0,3,3};
  const auto zero=cppunivariate::t_signed_detail::coefficient(q,false);
  CHECK(zero.lo<=0);CHECK(zero.hi>=0);
  q={1,4};
  const auto value=cppunivariate::t_signed_detail::coefficient(q,false);
  CHECK(value.lo<=1.25);CHECK(value.hi>=1.25);
}

TEST_CASE("lst: compact cover refuses normal when its negative leading term hides a finite winner") {
  std::vector<double> x;for(int j=0;j<3;++j){x.push_back(-.001);x.push_back(.001);}
  for(int j=0;j<2;++j){x.push_back(-1);x.push_back(1);}
  const auto proof=cppunivariate::t_signed_detail::solve(mltest::as_span(x),0,NAN,false,200);
  CHECK_FALSE(proof.proved);CHECK(proof.coefficient_upper<0);
  // An independent finite witness disproves the face; the budgeted cover
  // may remain unresolved without locating this narrow scale optimum.
  cppunivariate::StopDetail witness;
  cppunivariate::t_shape_detail::retain_point(mltest::as_span(x),0,.001,.2,false,witness);
  REQUIRE_FALSE(witness.candidates.empty());
  const auto normal=cppunivariate::norm_mle(mltest::as_span(x),0,NAN);
  const auto face=cppunivariate::t_shape_detail::normal_point(mltest::as_span(x),0,normal.sd);
  CHECK(witness.loglik_lower>face.candidates[0].loglik_upper);
  const auto fit=cppunivariate::lst_mle(mltest::as_span(x),1e-8,200,0,NAN,NAN);
  CHECK(fit.status==cppunivariate::LstStatus::Ok);CHECK_FALSE(fit.certified);
}

TEST_CASE("lst: zero normal profile coefficient preserves uncertified legacy search") {
  const double a=std::sqrt(9+4*std::sqrt(6.));
  const std::vector<double> x{-a,a,-1,1,-1,1,-1,1};
  const auto proof=cppunivariate::t_signed_detail::solve(mltest::as_span(x),0,NAN,false,200);
  CHECK_FALSE(proof.proved);CHECK(proof.coefficient_lower<=0);CHECK(proof.coefficient_upper>=0);
  const auto fit=cppunivariate::lst_mle(mltest::as_span(x),1e-8,200,0,NAN,NAN);
  CHECK(fit.status==cppunivariate::LstStatus::Ok);CHECK_FALSE(fit.certified);
}

TEST_CASE("lst: inverse-shape score enclosures retain the normal cancellation") {
  const auto g=cppunivariate::t_signed_detail::scaled_gamma({32,1e6},false);
  REQUIRE(g.valid());CHECK(g.lo>.249);CHECK(g.hi<=.25);
  const auto ratio=cppunivariate::t_signed_detail::remainder_ratio({0,1});
  REQUIRE(ratio.valid());CHECK(ratio.lo<=.1931471805599453094);CHECK(ratio.hi==.5);
}

TEST_CASE("lst: a broad inherited scale bracket contracts by uniform monotone signs") {
  cppunivariate::t_signed_detail::Engine e;
  e.free_scale=true;e.n=2;e.maxit=200;e.q={.4,1.6};
  cppunivariate::t_cert_detail::I p(-5,5);
  CHECK(e.nuisance(.01,p.lo).lo>0);CHECK(e.nuisance(.01,p.hi).hi<0);
  REQUIRE(e.scale_box(100,p,true));
  CHECK(p.hi-p.lo<1e-9);CHECK(p.lo>-1);CHECK(p.hi<1);
}

TEST_CASE("lst: scalar root inclusion refreshes curvature after contraction") {
  const std::vector<double> x{
    0.75720486532275033,0.078164112246018269,-0.29617000018298156,0.050073394207876487,
    1.0344803503783386,-1.8705333801050186,-3.0989867299391776,0.90730333404638741,
    0.86382261359534596,0.96758662859438438,-2.1019173105838753,0.92132097699594229,
    0.036567641718837307,1.4540366392763326,-1.3249887416131778,1.8563850477064809,
    0.1116342606423634,0.82160146027238912,1.052750325400404,-0.26650613879082141,
    -0.056457642305623237,-0.70494265187825933,1.6655563810952159,-0.89923880231305531,
    0.75255399723301819,1.2416742840838393,0.19691343696481184,0.48281862013640198,
    0.47676973303520387,0.16249408360302275,1.2896057451399137,1.2237821902695254,
    -0.49720872015426054,0.22303829922632412,0.97071652876075509,-0.29379211607275513,
    -0.88709293449384896,0.1573121349613944,-0.91759370430874165,-0.87160768908349917,
    -1.0101932449454551,0.27404867073227768,-1.4213256807182235,-0.32019158419304417,
    2.2647456405063782,1.1884078696369769,-0.09363209189861052,0.12587651816843237,
    1.0681883028736228,0.16552218950837555,-1.0151526789594714,0.7577305226233213,
    -1.1747273207870981,-0.37395687570530867,-1.707127708692465,-1.0711494510562174,
    1.5766445114569931,-2.1286971407445163,-2.3761726139159434,-1.213674694998772,
    0.56454955707753385,-0.37330024029843223,-0.60150897220800825,0.42511880543734426
  };
  const auto result=cppunivariate::lst_mle(mltest::as_span(x),1e-10,200,0,1,NAN);
  REQUIRE(result.status==cppunivariate::LstStatus::Ok);CHECK(result.certified);
  CHECK(result.df==doctest::Approx(18.49267086468155611686).epsilon(1e-10));
}

TEST_CASE("lst: positive-series shape remainder avoids the direct subtraction seam") {
  const auto value=cppunivariate::t_shape_detail::shape_remainder(.011);
  REQUIRE(value.valid());
  CHECK(value.hi-value.lo<1e-16);
  CHECK(value.lo>0);
  const auto old=cppunivariate::t_cert_detail::shape_remainder(.011);
  CHECK(value.hi-value.lo<old.hi-old.lo);
}
