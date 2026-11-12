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
 // Final chain derivatives include the df dependence of the Student CDF.
 CHECK(f.xi==doctest::Approx(g.estimates.at("xi")).epsilon(mltest::kGolden_st).scale(0));
 CHECK(f.omega==doctest::Approx(g.estimates.at("omega")).epsilon(mltest::kGolden_st).scale(0));
 CHECK(f.alpha==doctest::Approx(g.estimates.at("alpha")).epsilon(mltest::kGolden_st).scale(0));
 CHECK(f.nu==doctest::Approx(g.estimates.at("nu")).epsilon(mltest::kGolden_st).scale(0));
 CHECK(f.logLik==doctest::Approx(g.logLik).epsilon(mltest::kGolden_st).scale(0));
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

TEST_CASE("st: large-shape half-step normalizers agree with 80-digit gamma") {
 const double cases[][2]={
  {128,-0.92089163833791273864067632770102506772614885708154472477273838375275944563787804},
  {1e4,-0.91896353320463107511416306972377240262727041098808251502689215054756904085752284},
  {1e12,-0.9189385332049227417803297364056176398613974736378},
  {1e100,-0.9189385332046727417803297364056176398613974736378},
  {1.7976931348623157e308,-0.9189385332046727417803297364056176398613974736378}};
 for(const auto& c:cases) {
  CAPTURE(c[0]);
  CHECK(std::abs(cppunivariate::st_detail::logmode(c[0])-c[1])<4e-16);
  CHECK(std::abs(cppunivariate::student_detail::logmode(c[0])-c[1])<4e-16);
  CHECK(std::abs(cppunivariate::special::dt(0,c[0],true)-c[1])<4e-16);
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

TEST_CASE("st all-free chain derivatives agree with independent 60-digit derivatives") {
 const std::vector<double> x{-2,-.4,.2,1.3,5};
 const std::array<double,4> p{{-.3,std::log(1.2),.7,std::log(4.)}};
 const auto f=cppunivariate::st_accuracy::objective(x,cppunivariate::local_accuracy::variables(p),200);
 CHECK(f.v==doctest::Approx(12.262070689904155).epsilon(2e-13));
 const std::array<double,4> g{{0.28195485866404624,-3.708308858659771,0.7320607745045672,1.04629232103192}};
 const std::array<double,16> h{{2.46274879560771,0.17034691188685322,1.9481716557640356,-0.19191420768452164,0.17034691188685322,7.53234082940357,-1.4387470598615533,-2.917724009917648,1.9481716557640356,-1.4387470598615533,2.2277679164748614,0.34229453263371784,-0.19191420768452164,-2.917724009917648,0.34229453263371784,0.714684657486697}};
 for(int j=0;j<4;++j)CHECK(f.g[j]==doctest::Approx(g[j]).epsilon(3e-12));
 for(int j=0;j<16;++j)CHECK(f.h[j]==doctest::Approx(h[j]).epsilon(3e-11));
}

TEST_CASE("st all-free audit coefficient accuracy survives affine transforms") {
 {
 const std::vector<double> x{0.11024753757609822,-0.049422603469292115,1.3131208557689356,-0.22599451092730288,-1.3588404835226253,-1.554024854164954,-0.489892608032276,-1.5792660060875787,-3.481155288860334,0.3740887816008062,0.18182592636591727,-0.8722188835346512};
 const auto r=cppunivariate::st_mle({x.data(),x.size()});
 REQUIRE(r.status==cppunivariate::StStatus::Ok);
 CHECK(r.xi==doctest::Approx(0.6277064207169055).epsilon(1e-10));
 CHECK(r.omega==doctest::Approx(1.6458236060539524).epsilon(1e-10));
 CHECK(r.alpha==doctest::Approx(-2.5317284337811823).epsilon(1e-10));
 CHECK(r.nu==doctest::Approx(20.006801958806026).epsilon(1e-10));
 }
 {
 const std::vector<double> x{100000000.11024754,99999999.9505774,100000001.31312086,99999999.77400549,99999998.64115952,99999998.44597514,99999999.5101074,99999998.42073399,99999996.51884471,100000000.37408878,100000000.18182592,99999999.12778112};
 const auto r=cppunivariate::st_mle({x.data(),x.size()});
 REQUIRE(r.status==cppunivariate::StStatus::Ok);
 CHECK(r.xi==doctest::Approx(100000000.62770641).epsilon(1e-10));
 CHECK(r.omega==doctest::Approx(1.6458235942091328).epsilon(1e-10));
 CHECK(r.alpha==doctest::Approx(-2.5317284024605207).epsilon(1e-10));
 CHECK(r.nu==doctest::Approx(20.006800302915664).epsilon(1e-10));
 }
 {
 const std::vector<double> x{1.1024753757609823e+149,-4.9422603469292116e+148,1.3131208557689355e+150,-2.2599451092730287e+149,-1.3588404835226253e+150,-1.554024854164954e+150,-4.89892608032276e+149,-1.5792660060875786e+150,-3.481155288860334e+150,3.740887816008062e+149,1.8182592636591726e+149,-8.722188835346512e+149};
 const auto r=cppunivariate::st_mle({x.data(),x.size()});
 REQUIRE(r.status==cppunivariate::StStatus::Ok);
 CHECK(r.xi==doctest::Approx(6.277064207169055e+149).epsilon(1e-10));
 CHECK(r.omega==doctest::Approx(1.6458236060539525e+150).epsilon(1e-10));
 CHECK(r.alpha==doctest::Approx(-2.5317284337811827).epsilon(1e-10));
 CHECK(r.nu==doctest::Approx(20.006801958806047).epsilon(1e-10));
 }
}

TEST_CASE("st central and tail df derivatives retain the normal limit") {
 const std::vector<double> x{-2,-.4,.2,1.3,5};
 const std::array<double,4> p{{-.3,std::log(1.2),2.5,std::log(999.)}};
 const auto f=cppunivariate::st_accuracy::objective(x,cppunivariate::local_accuracy::variables(p),200);
 CHECK(f.v==doctest::Approx(23.200456341900328).epsilon(2e-13));
 const std::array<double,4> g{{6.472298610031706,-31.23988655894785,5.260596034622365,0.1326930499140267}};
 const std::array<double,16> h{{11.839705046462676,-8.875446451216167,6.577316174806845,0.05441838470092813,-8.875446451216167,70.31430756225537,-9.973609610459619,-0.5533149520261402,6.577316174806845,-9.973609610459619,1.8931075565103426,0.07082243568190795,0.05441838470092813,-0.5533149520261402,0.07082243568190795,-0.12925436377627825}};
 for(int j=0;j<4;++j)CHECK(f.g[j]==doctest::Approx(g[j]).epsilon(2e-11).scale(1));
 for(int j=0;j<16;++j)CHECK(f.h[j]==doctest::Approx(h[j]).epsilon(3e-11).scale(1));
}

TEST_CASE("st near-normal 55-digit coefficient reference is affine equivariant") {
 const std::vector<double> base{-1.77332972795447,-0.155049384757702,0.0961950617620771,1.68127726443543,1.81908213856355,-0.344190323493544,-0.355860277284418,1.33615149008829,1.01402838856633,0.0104396263812416,-0.0546875709288008,-1.30102266267756,1.06225094710892,0.861403676169522,0.638285176791554,-0.294553435210585,-2.71936910019106,0.336090541934782,0.0808372031580432,0.708139794484886,0.610833682064369,0.679401772859537,-0.992293045734106,2.06367970293921,2.41601947005125,0.0379470691405896,0.723086764185947,-0.0023536886500704,1.30495555771382,-0.596096973361479,-0.120497483798528,1.35009075415471,0.797244854232913,-0.38526602348665,0.358907551069067,-0.496276343074521,-0.355417449871236,-1.11780248572976,-0.157866989295123,0.559776018785781,1.44193874150905,-0.552594373287359,0.676013008811131,0.783873417526922,-0.365927220871107,0.148253880036053,-1.41996274955774,-1.25546774660881,-0.0108690689518648,0.00601574199390882,-0.320472896930679,0.228181872934452,-1.85897500734843,-2.69381671509274,1.41673638437109,0.580130248871283,-1.34952548769795,-0.0229715084576313,-1.67659910789834,-0.106477172580448,0.612540574389048,2.35755799219499,-1.37335262773542,-0.200401385855495,1.39620246293799,-1.41349118610765,-0.638845225953054,-1.65697763617303,-0.645380187585517,-0.702586449828615,2.79606891757007,-1.20314916206858,-1.6532913947428,0.979951927235418,-0.512380949251251,-0.920579624337648,0.672356305121155,0.381052253383021,-0.0960690823820381,0.0977487999225795};
 for(double scale:{1.,1e-4,1e4}){auto x=base;double shift=scale==1?0:3*scale;for(double&v:x)v=shift+scale*v;const auto r=cppunivariate::st_mle({x.data(),x.size()});
 REQUIRE(r.status==cppunivariate::StStatus::Ok);
 CHECK(r.xi==doctest::Approx(shift+scale*(-0.17292278716260778)).epsilon(1e-10));
 CHECK(r.omega==doctest::Approx(scale*1.1288051218310524).epsilon(1e-10));
 CHECK(r.alpha==doctest::Approx(0.21393697710543438).epsilon(1e-10));
 CHECK(r.nu==doctest::Approx(1272.6522999546437).epsilon(1e-10));
 }
}

// Independent 70-digit direct-density stationary roots: acc-fb-t-data/oracle.py.
TEST_CASE("st held accuracy stationary reference 7149") {
 const std::vector<double> x{1.1024753757609822e-151,-4.942260346929212e-152,1.3131208557689357e-150,-2.259945109273029e-151,-1.3588404835226254e-150,-1.554024854164954e-150,-4.89892608032276e-151,-1.5792660060875789e-150,-3.481155288860334e-150,3.7408878160080625e-151,1.8182592636591727e-151,-8.722188835346512e-151};
 const auto r=cppunivariate::st_mle({x.data(),x.size()},1e-10,200,-0.036539023191020197,NAN,0.10872382324228061,4.831511253373526);
 REQUIRE(r.status==cppunivariate::StStatus::Ok);
 CHECK(r.xi==-0.036539023191020197);
 CHECK(std::abs(r.omega-(0.035185561363608994))<=1e-10*0.035185561363608994);
 CHECK(r.alpha==0.10872382324228061);
 CHECK(r.nu==4.831511253373526);
}
TEST_CASE("st held accuracy stationary reference 6990") {
 const std::vector<double> x{1.1024753757609823e+149,-4.9422603469292116e+148,1.3131208557689355e+150,-2.2599451092730287e+149,-1.3588404835226253e+150,-1.554024854164954e+150,-4.89892608032276e+149,-1.5792660060875786e+150,-3.481155288860334e+150,3.740887816008062e+149,1.8182592636591726e+149,-8.722188835346512e+149};
 const auto r=cppunivariate::st_mle({x.data(),x.size()},1e-10,200,-0.036539023191020197,0.9203517925594821,NAN,NAN);
 REQUIRE(r.status==cppunivariate::StStatus::Ok);
 CHECK(r.xi==-0.036539023191020197);
 CHECK(r.omega==0.9203517925594821);
 CHECK(std::abs(r.alpha-(-0.3535533905932738))<=1e-10*1.0);
 CHECK(std::abs(r.nu-(1.0))<=1e-10*1.0);
}
TEST_CASE("st held accuracy stationary reference 7077") {
 const std::vector<double> x{0.11024753757609822,-0.049422603469292115,1.3131208557689356,-0.22599451092730288,-1.3588404835226253};
 const auto r=cppunivariate::st_mle({x.data(),x.size()},1e-10,200,-0.036539023191020197,NAN,NAN,4.831511253373526);
 REQUIRE(r.status==cppunivariate::StStatus::Ok);
 CHECK(r.xi==-0.036539023191020197);
 CHECK(std::abs(r.omega-(0.718136992622173))<=1e-10*0.718136992622173);
 CHECK(std::abs(r.alpha-(-0.01707413609590068))<=1e-10*1.0);
 CHECK(r.nu==4.831511253373526);
}

// Seventy-digit knot-constrained or near-restriction references.
TEST_CASE("st corner and near restriction accuracy 33001") {
 const std::vector<double> x{-27.022272533573503,-6.313751514675043,-3.732050807568877,-2.6050890646938014,-1.9626105055051506,-1.539864963814583,-1.2348971565350515,-1.0,-0.8097840331950071,-0.6494075931975106,-0.5095254494944288,-0.3838640350354158,-0.2679491924311227,-0.1583844403245363,-0.0524077792830412,0.0524077792830412,0.1583844403245363,0.2679491924311227,0.3838640350354158,0.5095254494944288,0.6494075931975106,0.8097840331950071,1.0,1.2348971565350515,1.539864963814583,1.9626105055051506,2.6050890646938014,3.732050807568877,6.313751514675043,27.022272533573503};
 const auto r=cppunivariate::st_mle({x.data(),x.size()},1e-10,200,0.0,1.0,0.0,NAN);
 REQUIRE(r.status==cppunivariate::StStatus::Ok);
 CHECK_FALSE(r.certified);
 CHECK(r.xi==0.0);
 CHECK(r.omega==1.0);
 CHECK(r.alpha==0.0);
 CHECK(std::abs(r.nu-(1.0001))<=1e-10*1.0001);
}

// Independent 90-digit Student-CDF df score, seeded skew-normal data.
TEST_CASE("st packed held df axis controls CDF derivative accuracy") {
 const std::vector<double> x{0.5725714950541838,0.916284955489906,0.7718386037225546,1.3612192223616357,0.5214475956010274,-0.13456146966535673,0.40668685912992236,0.550897142665792,1.3142743395060423,0.08048434866837914,0.3587186782959107,0.5081686035334709,1.5582984691781898,0.4833419121314613,0.24441007001992518,1.0144944634218187,0.0886423040341802,1.9925635485709199,0.5351613194036999,0.5444140197875179,0.8647848365819799,0.9817133776611185,0.5118874815313802,1.085125393663662,0.9091671806841171,0.49084372837372686,1.6031605018534654,1.1486367401162405,-0.11263866844367561,1.794720276174376,1.3380753977358142,0.46812601014747757,0.2983063978804885,0.3223420905205501,1.4630060882175044,0.5555516807574074,-0.4197985483512873,0.5884640382251879,0.2539568446449151,0.12158181951726704,0.7829158525814354,1.4455473442395117,0.5502285109503402,-0.19430915103128724,0.447458499566304,-0.6343782222314446,0.6673367280703203,0.6796621525256077,1.944392942351972,2.952812568931398,2.116324941590411,0.8173906781546852,0.2906267872627213,0.26129361706799814,0.5643714313337208,0.4532960543603241,0.4931052967023964,1.0373014474020152,1.3419872088052769,1.472339354324713,0.26891023799402614,1.0144123834207066,1.021072954623052,2.015468807401969};
 const auto r=cppunivariate::st_mle({x.data(),x.size()},1e-10,200,0.0,1.0,2.0,NAN);
 REQUIRE(r.status==cppunivariate::StStatus::Ok);
 CHECK_FALSE(r.certified);
 CHECK(std::isfinite(r.logLik));
 CHECK(std::abs(r.logLik-(-62.05007394537413))<1e-11);
 CHECK(r.xi==0.0); CHECK(r.omega==1.0); CHECK(r.alpha==2.0);
 CHECK(std::abs(r.nu-1000.0000000000064)<=1e-10*1000.0000000000064);
}

TEST_CASE("st narrow angular tails and positive central mass") {
  CHECK(cppunivariate::distributions::pst(-3.644230579974875e-7,0,1,1e8,1e8,true,true)==doctest::Approx(-690.7755278982137).epsilon(1e-10));
  CHECK(cppunivariate::distributions::pst(1.2533141404491138e-6,0,1,1e8,1e8,true,false)/1e-6==doctest::Approx(1.).epsilon(1e-10));
}

TEST_CASE("st large skew preserves a small CDF at ordinary degrees of freedom") {
 const double reference=8.412994158666847e-13;
 CHECK(std::abs(cppunivariate::distributions::pst(1e-12,0,1,1e12,5,true,false)/reference-1)<1e-10);
 CHECK(std::abs(cppunivariate::distributions::qst(reference,0,1,1e12,5,true,false)/1e-12-1)<1e-9);
}

TEST_CASE("st normal face retains a ray whose squared scaled coordinate underflows") {
 const double nu=std::numeric_limits<double>::max();
 CHECK(std::abs(cppunivariate::distributions::pst(-2e-16,0,1,1e8,nu,true,false)/3.183098782049451e-9-1)<1e-10);
 CHECK(std::abs(cppunivariate::distributions::pst(2e-16,0,1,1e8,nu,true,false)/3.1830989416263634e-9-1)<1e-10);
}
