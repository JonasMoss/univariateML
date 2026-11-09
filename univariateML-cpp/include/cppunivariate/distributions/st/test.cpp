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

TEST_CASE("st: face comparison cannot resolve rounded extreme objectives") {
  const double magnitude=1e20;
  CHECK(cppunivariate::compare_face(-magnitude,-magnitude,2e-8)==cppunivariate::FaceComparison::NumericalTie);
  CHECK(cppunivariate::compare_face(-magnitude,-2*magnitude,2e-8)==cppunivariate::FaceComparison::FirstWins);
  CHECK(cppunivariate::compare_face(-2*magnitude,-magnitude,2e-8)==cppunivariate::FaceComparison::SecondWins);
  CHECK(cppunivariate::compare_face(-10.,-10.,2e-8)==cppunivariate::FaceComparison::FirstWins);
}

TEST_CASE("st: audited huge held-parameter magnitude") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  std::vector<double> x{1,2};
  cppunivariate::span s{x.data(),x.size()};
  const auto r=cppunivariate::st_mle(s,1e-6,200,0,1e-10,0,nan);
  CHECK((r.status==cppunivariate::StStatus::Ok || r.status==cppunivariate::StStatus::NumericalFailure));
  CHECK(r.status!=cppunivariate::StStatus::SkewNormalBoundary);
}

TEST_CASE("st: structured face payloads") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(double sign:{-1.,1.}) {
    std::vector<double> x{0,.1,.2,.4,3};for(double& v:x)v*=sign;
    const auto r=cppunivariate::st_mle(mltest::as_span(x),1e-6,200,nan,nan,nan,5);
    REQUIRE(r.stop_detail.limit_coef.size()==3);CHECK(r.stop_detail.limit_coef[0]==0);CHECK(r.stop_detail.limit_coef[1]==5);
    CHECK(r.stop_detail.limit_coef[2]>0);CHECK(r.stop_detail.limit_loglik==r.logLik);
  }
  const std::vector<double> x{-2,-1,0,1,2};
  const auto r=cppunivariate::st_mle(mltest::as_span(x),1e-6,200,nan,nan,0,nan);
  REQUIRE(r.stop_detail.limit_coef.size()==3);CHECK(r.stop_detail.limit_coef[2]==0);
  CHECK(r.stop_detail.limit_loglik==r.logLik);
}

#include "../ht/dpqr.hpp"

TEST_CASE("st: half-t comparison ambiguity retains original-unit witnesses") {
  const std::vector<double> x{1,2};
  const auto r=cppunivariate::st_mle(mltest::as_span(x),1e-6,200,0,1e-4,cppunivariate::special::kNaN,1e20);
  REQUIRE(r.status==cppunivariate::StStatus::NumericalFailure);
  REQUIRE(r.stop_detail.comparison_tie);
  REQUIRE(r.stop_detail.candidates.size()==2);
  const auto& face=r.stop_detail.candidates[1];
  REQUIRE(face.model==cppunivariate::StopModel::Ht);
  REQUIRE(face.coef.size()==3);
  CHECK(face.coef[0]==0);CHECK(face.coef[1]==1e20);CHECK(face.coef[2]==1e-4);
  double ll=0;for(double v:x)ll+=cppunivariate::distributions::dht(v,face.coef[0],face.coef[1],face.coef[2],true);
  CHECK(face.loglik_lower==doctest::Approx(ll).epsilon(1e-12));
  const auto& finite=r.stop_detail.candidates[0];
  REQUIRE(finite.coef.size()==4);ll=0;
  for(double v:x)ll+=cppunivariate::distributions::dst(v,finite.coef[0],finite.coef[1],finite.coef[2],finite.coef[3],true);
  CHECK(finite.loglik_lower==doctest::Approx(ll).epsilon(1e-12));
  CHECK(r.stop_detail.loglik_upper==INFINITY);
}

TEST_CASE("st: stopped limiting-family certificates preserve transport and bounds") {
  cppunivariate::StopDetail detail;
  detail.candidates={{{2,5,3},-10,-9,cppunivariate::StopModel::None},{{2,4},-8,-8,cppunivariate::StopModel::Hnorm}};
  detail.loglik_lower=-8;detail.loglik_upper=INFINITY;detail.segment_loglik_upper=-7;
  const auto r=cppunivariate::st_detail::face_transport(detail,10,2,3,cppunivariate::StopModel::Ht,-1,std::log(2.));
  REQUIRE(r.candidates.size()==2);
  CHECK(r.candidates[0].model==cppunivariate::StopModel::ReflectedHt);
  CHECK(r.candidates[0].coef==std::vector<double>{6,5,6});
  CHECK(r.candidates[1].model==cppunivariate::StopModel::ReflectedHnorm);
  CHECK(r.candidates[1].coef==std::vector<double>{6,8});
  CHECK(r.candidates[0].loglik_lower==doctest::Approx(-10-4*std::log(2.)));
  CHECK(r.candidates[0].loglik_upper==doctest::Approx(-9-4*std::log(2.)));
  CHECK(r.loglik_lower==doctest::Approx(-8-4*std::log(2.)));
  CHECK(r.segment_loglik_upper==doctest::Approx(-7-4*std::log(2.)));
  CHECK(r.loglik_upper==INFINITY);
}

TEST_CASE("st: conditional skew certificate and exact endpoint factors") {
 for(double sign:{-1.,1.}) {
  const std::vector<double> x{0,sign,2*sign};
  const auto r=cppunivariate::st_mle(mltest::as_span(x),1e-6,200,0,1,NAN,4);
  CHECK(r.status==(sign>0?cppunivariate::StStatus::RightHalfTBoundary:cppunivariate::StStatus::LeftHalfTBoundary));
  CHECK(r.certified);REQUIRE(r.stop_detail.limit_coef.size()==3);
  double closed=0;for(double v:x)closed+=cppunivariate::distributions::dht(sign*v,0,4,1,true);
  CHECK(r.logLik==doctest::Approx(closed-std::log(2.)).epsilon(1e-13));
 }
 const std::vector<double> flat{2,2,2};
 const auto f=cppunivariate::st_mle(mltest::as_span(flat),1e-6,200,2,1,NAN,.5);
 CHECK(f.status==cppunivariate::StStatus::NumericalFailure);CHECK(f.stop_detail.comparison_tie);
 REQUIRE(f.stop_detail.candidates.size()==2);
 CHECK(f.stop_detail.candidates[0].loglik_lower==f.stop_detail.candidates[1].loglik_lower);
}

TEST_CASE("st: conditional skew searches both signs without concavity") {
 const std::vector<double> x{-3,-1,-.2,.1,.8,2,4};
 const auto r=cppunivariate::st_mle(mltest::as_span(x),1e-6,200,0,1,NAN,4);
 REQUIRE(r.status==cppunivariate::StStatus::Ok);CHECK(r.certified);
 CHECK(r.alpha==doctest::Approx(.16456272666935215).epsilon(1e-8));
 CHECK(r.logLik==doctest::Approx(-16.43971347475502).epsilon(1e-12));
 std::vector<double> reflected;for(double v:x)reflected.push_back(-v);
 const auto f=cppunivariate::st_mle(mltest::as_span(reflected),1e-6,200,0,1,NAN,4);
 REQUIRE(f.status==cppunivariate::StStatus::Ok);CHECK(f.certified);
 CHECK(f.alpha==doctest::Approx(-r.alpha).epsilon(1e-9));
 const std::vector<double> difficult{-1e-12,-1,.1,1,4};
 const auto b=cppunivariate::st_mle(mltest::as_span(difficult),1e-6,1,0,1,NAN,4);
 REQUIRE(b.status==cppunivariate::StStatus::NumericalFailure);CHECK(b.budget_exhausted);
 CHECK_FALSE(b.stop_detail.candidates.empty());
}

TEST_CASE("st: underflowed conditional residuals are not exact ties") {
 const std::vector<double> x{1e-300,2e-300};
 const auto r=cppunivariate::st_mle(mltest::as_span(x),1e-6,200,0,1,NAN,1e300);
 CHECK(r.status==cppunivariate::StStatus::NumericalFailure);
 CHECK_FALSE(r.stop_detail.comparison_tie);CHECK_FALSE(r.certified);
}

TEST_CASE("st: reflected conditional face retains the original endpoint coordinate") {
 for(double sign:{-1.,1.}) {
  const std::vector<double> x{2,2+sign,2+2*sign};
  const auto r=cppunivariate::st_mle(mltest::as_span(x),1e-6,200,2,1,NAN,4);
  REQUIRE(r.certified);REQUIRE(r.stop_detail.limit_coef.size()==3);
  CHECK(r.stop_detail.limit_coef[0]==2);
  CHECK(r.stop_detail.limit_model==(sign>0?cppunivariate::StopModel::Ht:cppunivariate::StopModel::ReflectedHt));
  double closed=0;for(double v:x)closed+=cppunivariate::distributions::dht(sign*(v-2),0,4,1,true);
  CHECK(r.logLik==doctest::Approx(closed-std::log(2.)).epsilon(1e-13));
 }
}

TEST_CASE("st: held-shape location certificates find both global Cauchy maxima") {
 const std::vector<double> x{-2,2};
 const auto r=cppunivariate::st_mle(mltest::as_span(x),1e-6,200,NAN,1,0,1);
 REQUIRE(r.status==cppunivariate::StStatus::NumericalFailure);
 REQUIRE(r.stop_detail.comparison_tie);
 REQUIRE(r.stop_detail.candidates.size()>=2);
 bool left=false,right=false;
 for(const auto& c:r.stop_detail.candidates) {
  if(std::abs(c.coef[0]+std::sqrt(3.))<1e-8)left=true;
  if(std::abs(c.coef[0]-std::sqrt(3.))<1e-8)right=true;
 }
 CHECK(left);CHECK(right);
}

TEST_CASE("st: location certificates preserve reflection and held coefficients") {
 const std::vector<double> x{-3,-1,-.2,.1,.8,2,4};
 for(double a:{-3.,0.,2.})for(double nu:{.5,1.,4.,30.}) {
  const auto r=cppunivariate::st_mle(mltest::as_span(x),1e-6,200,NAN,1,a,nu);
  REQUIRE(r.status==cppunivariate::StStatus::Ok);CHECK(r.certified);
  CHECK(r.omega==1);CHECK(r.alpha==a);CHECK(r.nu==nu);
  std::vector<double> reflected;for(double v:x)reflected.push_back(-v);
  const auto s=cppunivariate::st_mle(mltest::as_span(reflected),1e-6,200,NAN,1,-a,nu);
  REQUIRE(s.status==cppunivariate::StStatus::Ok);CHECK(s.certified);
  CHECK(s.xi==doctest::Approx(-r.xi).epsilon(1e-9).scale(1));
  CHECK(s.logLik==doctest::Approx(r.logLik).epsilon(1e-12));
 }
 const auto b=cppunivariate::st_mle(mltest::as_span(x),1e-6,1,NAN,1,2,4);
 CHECK(b.status==cppunivariate::StStatus::NumericalFailure);CHECK(b.budget_exhausted);
 CHECK_FALSE(b.stop_detail.candidates.empty());
}

TEST_CASE("st: certified symmetric scale and one-sided joint alpha-scale faces") {
 const std::vector<double> x{-3,-1,-.2,.1,.8,2,4};
 const auto r=cppunivariate::st_mle(mltest::as_span(x),1e-6,200,0,NAN,0,4);
 REQUIRE(r.status==cppunivariate::StStatus::Ok);CHECK(r.certified);
 double score=-double(x.size());for(double v:x)score+=5*v*v/(4*r.omega*r.omega+v*v);
 CHECK(std::abs(score)<1e-9);
 for(double sign:{-1.,1.}) {
  const std::vector<double> side{2,2+sign,2+2*sign,2+4*sign};
  const auto f=cppunivariate::st_mle(mltest::as_span(side),1e-6,200,2,NAN,NAN,4);
  REQUIRE(f.status==(sign>0?cppunivariate::StStatus::RightHalfTBoundary:cppunivariate::StStatus::LeftHalfTBoundary));CHECK(f.certified);
  REQUIRE(f.stop_detail.limit_coef.size()==3);CHECK(f.stop_detail.limit_coef[0]==2);
  double closed=0;for(double v:side)closed+=cppunivariate::distributions::dht(sign*(v-2),0,4,f.omega,true);
  CHECK(f.logLik==doctest::Approx(closed-std::log(2.)).epsilon(1e-12));
 }
 const std::vector<double> equality{0,1};
 for(double a:std::vector<double>{0.,NAN}) {
  const auto f=cppunivariate::st_mle(mltest::as_span(equality),1e-6,200,0,NAN,a,1);
  CHECK(f.status==cppunivariate::StStatus::ScaleBoundary);CHECK(f.reason==6);CHECK(f.certified);
  const double ll=-2*std::log(std::acos(-1.))+(std::isnan(a)?std::log(2.):0);
  CHECK(f.logLik==doctest::Approx(ll).epsilon(1e-12));
 }
 const std::vector<double> zeros{0,0};
 CHECK(cppunivariate::st_mle(mltest::as_span(zeros),1e-6,200,0,NAN,NAN,4).status==cppunivariate::StStatus::Unbounded);
}

TEST_CASE("st: constant-data moving-location skew faces tie at the sharp bound") {
 const std::vector<double> x{2,2,2};
 const auto r=cppunivariate::st_mle(mltest::as_span(x),1e-6,200,NAN,1,NAN,4);
 REQUIRE(r.status==cppunivariate::StStatus::NumericalFailure);CHECK(r.certified);
 REQUIRE(r.stop_detail.comparison_tie);REQUIRE(r.stop_detail.candidates.size()==2);
 CHECK(r.stop_detail.candidates[0].model==cppunivariate::StopModel::Ht);
 CHECK(r.stop_detail.candidates[1].model==cppunivariate::StopModel::ReflectedHt);
 const double bound=3*(std::log(2.)+cppunivariate::st_detail::log_t_density(-INFINITY,4));
 CHECK(r.stop_detail.loglik_lower<=bound);CHECK(r.stop_detail.loglik_upper>=bound);
 for(double sign:{-1.,1.}) {
  const double delta=1e-6;double ll=0;
  for(double v:x)ll+=cppunivariate::st_detail::logdensity(v,2-sign*delta,1,sign*1e12,4);
  CHECK(ll<bound);CHECK(bound-ll<1e-9);
 }
}

TEST_CASE("st: connected location components meet independent coefficient accuracy") {
 const std::vector<double> x{-3,-1,-.2,.1,.8,2,4};
 const auto r=cppunivariate::st_mle(mltest::as_span(x),1e-6,200,NAN,1,-3,30);
 REQUIRE(r.status==cppunivariate::StStatus::Ok);CHECK(r.certified);
 // 60-digit independent Student density/CDF score root; check.py records it.
 CHECK(std::abs(r.xi-2.1640278896764464854)<1e-10);
}

TEST_CASE("st: exhausted monotone-scale solve keeps a numerical witness") {
 const std::vector<double> x{0,1,2,4};
 const auto r=cppunivariate::st_mle(mltest::as_span(x),1e-6,1,0,NAN,0,4);
 CHECK(r.status==cppunivariate::StStatus::NumericalFailure);CHECK_FALSE(r.certified);
 CHECK_FALSE(r.budget_exhausted);REQUIRE_FALSE(r.stop_detail.candidates.empty());
 CHECK(r.stop_detail.loglik_upper==INFINITY);
}

TEST_CASE("st: exact symmetric all-nuisance fit delegates the Student certificate") {
 const std::vector<double> normal{-2,-1,0,1,2};
 const auto n=cppunivariate::st_mle(mltest::as_span(normal),1e-6,200,NAN,NAN,0,NAN);
 CHECK(n.status==cppunivariate::StStatus::SkewNormalBoundary);CHECK(n.certified);
 REQUIRE(n.stop_detail.limit_coef.size()==3);CHECK(n.stop_detail.limit_coef[2]==0);
 const std::vector<double> heavy{-1000,-10,-1,0,1,10,1000};
 const auto f=cppunivariate::st_mle(mltest::as_span(heavy),1e-6,200,NAN,NAN,0,NAN);
 REQUIRE(f.status==cppunivariate::StStatus::Ok);CHECK(f.certified);CHECK(f.nu==1);CHECK(f.restricted==64u);
 const auto parent=cppunivariate::lst_mle_certified(mltest::as_span(heavy),1e-6,200);
 CHECK(f.xi==parent.mu);CHECK(f.omega==parent.sigma);CHECK(f.logLik==parent.logLik);
 const auto b=cppunivariate::st_mle(mltest::as_span(heavy),1e-6,1,NAN,NAN,0,NAN);
 CHECK(b.status!=cppunivariate::StStatus::Ok);CHECK(b.budget_exhausted==b.stop_detail.budget_exhausted);
}

TEST_CASE("st: thin-tail log cancellation is enclosed or stops numerically") {
 const auto r=cppunivariate::st_cert_detail::location_term(-1,-1,1e6,1e6);
 // Independent 80-digit beta-integral ratio; old score missed by 9.31e-4.
 const double truth=-1000000.000000999998;
 CHECK(r.lo<=truth);CHECK(r.hi>=truth);
 const auto huge=cppunivariate::st_cert_detail::location_term(-1,-1,1e30,1e20);
 CHECK_FALSE((std::isfinite(huge.lo)&&std::isfinite(huge.hi)));
 const std::vector<double> x{-1000,-10,0,1,2};
 const auto failed=cppunivariate::st_mle(mltest::as_span(x),1e-6,200,NAN,1,1e30,1e20);
 CHECK(failed.status==cppunivariate::StStatus::NumericalFailure);
 CHECK_FALSE(failed.certified);
}

TEST_CASE("st: constant-data free-nu corner has two sharp half-normal faces") {
 const std::vector<double> x{2,2,2};
 const auto r=cppunivariate::st_mle(mltest::as_span(x),1e-6,1,NAN,1,NAN,NAN);
 REQUIRE(r.status==cppunivariate::StStatus::NumericalFailure);CHECK(r.certified);
 REQUIRE(r.stop_detail.comparison_tie);REQUIRE(r.stop_detail.candidates.size()==2);
 CHECK(r.stop_detail.candidates[0].model==cppunivariate::StopModel::Hnorm);
 CHECK(r.stop_detail.candidates[1].model==cppunivariate::StopModel::ReflectedHnorm);
 const double bound=3*(std::log(2.)-.5*std::log(2*std::acos(-1.)));
 CHECK(r.stop_detail.loglik_lower<=bound);CHECK(r.stop_detail.loglik_upper>=bound);
}

TEST_CASE("st: budget witnesses enclose likelihoods at rounded public locations") {
 const double center=1e300;
 const double omega=8*(std::nextafter(center,INFINITY)-center);
 std::vector<double> x;for(double z:{-3.,-1.,-.25,.125,.75,2.,4.})x.push_back(center+omega*z);
 const auto r=cppunivariate::st_mle(mltest::as_span(x),1e-6,1,NAN,omega,-3,.5);
 REQUIRE(r.status==cppunivariate::StStatus::NumericalFailure);
 REQUIRE(r.budget_exhausted);REQUIRE_FALSE(r.stop_detail.candidates.empty());
 for(const auto& c:r.stop_detail.candidates) {
  double ll=0;for(double v:x)ll+=cppunivariate::distributions::dst(v,c.coef[0],c.coef[1],c.coef[2],c.coef[3],true);
  CHECK(c.loglik_lower<=ll);CHECK(c.loglik_upper>=ll);
 }
}
