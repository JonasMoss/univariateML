#include "test_support.hpp"
#include "cppunivariate/distributions/st/mle.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <vector>
TEST_CASE("st: symmetric Student-t identity and inversion") {
 for(double nu:{1.,4.,20.})for(double x:{-5.,-1.,0.,1.,5.}) {
  CHECK(cppunivariate::distributions::dst(x,0,1,0,nu,true)==doctest::Approx(cppunivariate::distributions::dlst(x,0,1,nu,true)).epsilon(1e-12));
  CHECK(cppunivariate::distributions::pst(x,0,1,0,nu,true,true)==doctest::Approx(cppunivariate::distributions::plst(x,0,1,nu,true,true)).epsilon(1e-12));
 }
 for(double a:{-4.,1.,4.})for(double p:{.01,.2,.5,.9,.99}) {
  double q=cppunivariate::distributions::qst(p,0,1,a,4,true,false);
  CHECK(cppunivariate::distributions::pst(q,0,1,a,4,true,false)==doctest::Approx(p).epsilon(1e-8));
 }
}
TEST_CASE("st: excessive ties and exhausted optimizer") {
 const std::vector<double> x{0,0,0,1,2};
 CHECK(cppunivariate::st_mle(mltest::as_span(x)).status==cppunivariate::StStatus::Unbounded);
 const std::vector<double> y{-3,-1,0,0.4,2,4};
 CHECK(cppunivariate::st_mle(mltest::as_span(y),1e-9,1).status!=cppunivariate::StStatus::Ok);
}

TEST_CASE("st: analytic nuisance scores agree with independent finite differences") {
 const std::vector<double> x{-3,-.2,.1,.8,2.};
 cppunivariate::st_detail::Cost c{mltest::as_span(x),{0,0,0,0},{true,true,true,true}};
 for(double a:{-3.,0.,2.}) {
  const double p[]={.2,std::log(1.3),a,std::log(4.)};double g[4];c.gradient(p,g);
  for(int j=0;j<4;++j) {
   std::vector<double> q(p,p+4);const double h=2e-5;q[j]+=h;const double u=c(q.data());q[j]-=2*h;const double d=c(q.data());
   CHECK(g[j]==doctest::Approx((u-d)/(2*h)).epsilon(1e-5).scale(1));
  }
 }
}

TEST_CASE("st: d/p/q/r against the truth table") {
 mltest::check_dpqr_truth("st",
  [](double x,const std::vector<double>& p,bool log){return cppunivariate::distributions::dst(x,p[0],p[1],p[2],p[3],log);},
  [](double x,const std::vector<double>& p,bool lower,bool log){return cppunivariate::distributions::pst(x,p[0],p[1],p[2],p[3],lower,log);},
  [](double x,const std::vector<double>& p,bool lower,bool log){return cppunivariate::distributions::qst(x,p[0],p[1],p[2],p[3],lower,log);},
  [](cppunivariate::distributions::StdRng<>& rng,const std::vector<double>& p){return cppunivariate::distributions::rst(rng,p[0],p[1],p[2],p[3]);});
}

TEST_CASE("st: central quarter probabilities and log input rounding") {
 for(double a:{-1.,1.}) {
  const bool lower=a>0;
  CHECK(cppunivariate::distributions::pst(0,0,1,a,4,lower,false)==doctest::Approx(.25).epsilon(1e-14));
  CHECK(cppunivariate::distributions::qst(.25,0,1,a,4,lower,false)==0);
  const double q=cppunivariate::distributions::qst(-1.3862943611198906,0,1,a,4,lower,true);
  CHECK((lower?q:-q)>0);
 }
}

TEST_CASE("st: numerical comparisons report limiting faces") {
 const double nan=cppunivariate::special::kNaN;
 const std::vector<double> right{0,.1,.2,.4,3};
 const auto r=cppunivariate::st_mle(mltest::as_span(right),1e-6,200,nan,nan,nan,5);
 CHECK(r.status==cppunivariate::StStatus::RightHalfTBoundary);
 std::vector<double> left;for(double x:right)left.push_back(-x);
 const auto l=cppunivariate::st_mle(mltest::as_span(left),1e-6,200,nan,nan,nan,5);
 CHECK(l.status==cppunivariate::StStatus::LeftHalfTBoundary);
 const std::vector<double> symmetric{-2,-1,0,1,2};
 const auto n=cppunivariate::st_mle(mltest::as_span(symmetric),1e-6,200,nan,nan,0,nan);
 CHECK(n.status==cppunivariate::StStatus::SkewNormalBoundary);
}

TEST_CASE("st: native golden fit") {
 const auto g=mltest::load_distribution_golden("st");
 const auto f=cppunivariate::st_mle(mltest::as_span(g.data));
 REQUIRE(f.status==cppunivariate::StStatus::Ok);
 // The df score uses finite differences and WebAssembly uses binary64
 // intermediates. The tolerance covers measured cross-platform score drift.
 CHECK(f.xi==doctest::Approx(g.estimates.at("xi")).epsilon(1e-5));
 CHECK(f.omega==doctest::Approx(g.estimates.at("omega")).epsilon(1e-5));
 CHECK(f.alpha==doctest::Approx(g.estimates.at("alpha")).epsilon(1e-5));
 CHECK(f.nu==doctest::Approx(g.estimates.at("nu")).epsilon(1e-5));
 CHECK(f.logLik==doctest::Approx(g.logLik).epsilon(1e-10));
}

TEST_CASE("st: active degrees-of-freedom restriction is flagged") {
 const std::vector<double> x{-1000,-10,-1,0,1,10,1000};
 const double nan=cppunivariate::special::kNaN;
 const auto f=cppunivariate::st_mle(mltest::as_span(x),1e-6,200,nan,nan,0,nan);
 REQUIRE(f.status==cppunivariate::StStatus::Ok);
 CHECK(f.nu==1);
 CHECK((f.restricted&64u)!=0);
 double near=0;
 for(double v:x)near+=cppunivariate::distributions::dst(v,f.xi,f.omega,f.alpha,1.0001,true);
 CHECK((near-f.logLik)/1e-4<=1e-3);
}
