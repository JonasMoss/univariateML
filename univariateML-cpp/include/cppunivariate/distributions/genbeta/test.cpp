#include "test_support.hpp"
#include "cppunivariate/distributions/genbeta/mle.hpp"
#include "cppunivariate/distributions/genbeta/dpqr.hpp"

TEST_CASE("genbeta: golden and held-scale likelihood") {
  const auto g=mltest::load_distribution_golden("genbeta");
  const auto r=cppunivariate::genbeta_mle(mltest::as_span(g.data),1);
  REQUIRE(r.status==cppunivariate::GenbetaStatus::Ok);
  CHECK(r.shape1==doctest::Approx(g.estimates.at("shape1")).epsilon(1e-5));
  CHECK(r.shape2==doctest::Approx(g.estimates.at("shape2")).epsilon(1e-5));
  CHECK(r.shape3==doctest::Approx(g.estimates.at("shape3")).epsilon(1e-5));
  CHECK(r.logLik==doctest::Approx(g.logLik).epsilon(1e-8));
  double ll=0;for(double x:g.data)ll+=cppunivariate::distributions::dgenbeta(x,r.shape1,r.shape2,r.shape3,1,true);
  CHECK(ll==doctest::Approx(r.logLik).epsilon(1e-10));
  for(int j=0;j<3;++j) {
    std::array<double,3> v{{r.shape1,r.shape2,r.shape3}};
    auto value=[&](double h){auto w=v;w[j]*=std::exp(h);double ans=0;for(double x:g.data)ans+=cppunivariate::distributions::dgenbeta(x,w[0],w[1],w[2],1,true);return ans;};
    CHECK(std::abs((value(1e-5)-value(-1e-5))/2e-5)<1e-3);
  }
}
TEST_CASE("genbeta: invalid observations, controls, and attained power ridge") {
  const std::vector<double> x{.1,.2,.3,.4};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto f=cppunivariate::genbeta_mle(mltest::as_span(x),1,1e-7,300,nan,1);
  CHECK(f.status==cppunivariate::GenbetaStatus::Ok);
  CHECK(f.shape3==1);
  CHECK(cppunivariate::genbeta_mle(mltest::as_span(x),1,1e-7,300,1e308,1,1e308).status==cppunivariate::GenbetaStatus::SearchUnresolved);
  CHECK(cppunivariate::genbeta_mle(mltest::as_span(x),1,1e-7,300,1e308,1e308,1).status==cppunivariate::GenbetaStatus::SearchUnresolved);
  for(double s:{0.,-.1,.4})CHECK(cppunivariate::genbeta_mle(mltest::as_span(x),s).status!=cppunivariate::GenbetaStatus::Ok);
  CHECK(cppunivariate::genbeta_mle(mltest::as_span(x),1,0).status==cppunivariate::GenbetaStatus::InvalidData);
  CHECK(cppunivariate::distributions::dgenbeta(1,2,1,3,1,false)==doctest::Approx(6));
  CHECK(cppunivariate::distributions::dgenbeta(0,.5,2,2,1,false)==doctest::Approx(2/std::exp(cppunivariate::special::lbeta(.5,2))));
}
TEST_CASE("genbeta: exact gamma and effective-endpoint power face comparisons") {
  std::vector<double> gamma,power;
  for(int i=0;i<300;++i) {
    const double u=(i+.5)/300;
    gamma.push_back(std::exp(-cppunivariate::special::qgamma(u,2,1.0/3,true,false)));
    power.push_back(.7*u*u);
  }
  CHECK(cppunivariate::genbeta_mle(mltest::as_span(gamma),1).status==cppunivariate::GenbetaStatus::UnitGammaBoundary);
  const auto r=cppunivariate::genbeta_mle(mltest::as_span(power),1);
  CHECK(r.status==cppunivariate::GenbetaStatus::PowerBoundary);
  const auto p=cppunivariate::power_mle(mltest::as_span(power),0);
  CHECK(r.logLik==doctest::Approx(p.logLik).epsilon(1e-10));
}

#include "cppunivariate/distributions/genbeta/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
TEST_CASE("genbeta: probability endpoints and sampler CDF") {
  CHECK(cppunivariate::distributions::qgenbeta(0,20,1000,2,1,true,false)==0);
  CHECK(std::isinf(cppunivariate::distributions::qgenbeta(0,20,1000,2,1,false,false))==false);
  CHECK(cppunivariate::distributions::pgenbeta(-1,20,1000,2,1,true,false)==0);
  CHECK(cppunivariate::distributions::pgenbeta(std::numeric_limits<double>::infinity(),20,1000,2,1,true,false)==1);
  CHECK(std::isnan(cppunivariate::distributions::qgenbeta(.1,20,1000,2,1,true,true)));
  cppunivariate::distributions::StdRng<> rng(7813);
  std::vector<double> samples;
  for(int i=0;i<20000;++i) samples.push_back(cppunivariate::distributions::rgenbeta(rng,20,1000,2,1));
  for(double point:{.05,.1,.2}) {
    int count=0;for(double x:samples) if(x<=point) ++count;
    const double prob=cppunivariate::distributions::pgenbeta(point,20,1000,2,1,true,false);
    CHECK(std::abs(count/20000.-prob)<=5*std::sqrt(prob*(1-prob)/20000.));
  }
}
// from mpmath import mp; mp.dps=100; x=mp.mpf('1e-100'); z=x*x
// float(mp.log(mp.betainc(20,1000,0,z,regularized=True)))
TEST_CASE("genbeta: log beta tails beyond probability underflow") {
  CHECK(cppunivariate::distributions::pgenbeta(1e-100,20,1000,2,1,true,true)==doctest::Approx(-9114.3321059628252).epsilon(2e-14));
  for(double lp:{-27.631021115928547,-69.07755278982137,-230.25850929940458,-690.7755278982137,-800.,-10000.}) for(bool lower:{true,false}) {
    const double q=cppunivariate::distributions::qgenbeta(lp,20,1000,2,1,lower,true);
    REQUIRE(q>0);
    REQUIRE(std::isfinite(q));
    CHECK(cppunivariate::distributions::pgenbeta(q,20,1000,2,1,lower,true)==doctest::Approx(lp).epsilon(2e-11));
  }
}

TEST_CASE("P2 genbeta constant conditional beta fits") {
 std::vector<double> x(5,.4);
 const double nan=std::numeric_limits<double>::quiet_NaN();
 auto a=cppunivariate::genbeta_mle(mltest::as_span(x),1,1e-7,300,nan,3,1);
 REQUIRE(a.status==cppunivariate::GenbetaStatus::Ok);
 CHECK(a.shape2==3);
 CHECK(a.shape3==1);
 auto b=cppunivariate::genbeta_mle(mltest::as_span(x),1,1e-7,300,2,nan,1);
 REQUIRE(b.status==cppunivariate::GenbetaStatus::Ok);
 CHECK(b.shape1==2);
 CHECK(b.shape3==1);
 auto ridge=cppunivariate::genbeta_mle(mltest::as_span(x),1,1e-7,300,nan,1,nan);
 REQUIRE(ridge.status==cppunivariate::GenbetaStatus::Ok);
 CHECK(ridge.shape1*ridge.shape3==doctest::Approx(-1/std::log(.4)));
}


TEST_CASE("genbeta: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("genbeta",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dgenbeta(x, p[0], p[1], p[2], p[3], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pgenbeta(x, p[0], p[1], p[2], p[3], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qgenbeta(x, p[0], p[1], p[2], p[3], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rgenbeta(rng, p[0], p[1], p[2], p[3]); });
}

TEST_CASE("genbeta: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("genbeta");
  const auto x=mltest::as_span(g.data);
  CHECK(cppunivariate::genbeta_mle(x,1,1e-8,1).status==cppunivariate::GenbetaStatus::SearchUnresolved);
}

TEST_CASE("genbeta finite upper density limit preserves log range") {
  // shape2=1: f(scale)=shape1*shape3/scale, from B(a,1)=1/a.
  CHECK(cppunivariate::distributions::dgenbeta(3,1e308,1,2,3,true)==
        doctest::Approx(std::log(1e308)+std::log(2.)-std::log(3.)));
  CHECK(cppunivariate::distributions::dgenbeta(3,2,1,2,3,false)==doctest::Approx(4./3));
}

TEST_CASE("genbeta: face comparison cannot resolve rounded extreme objectives") {
  const double magnitude=1e99;
  CHECK(cppunivariate::compare_face(-magnitude,-magnitude,2e-8)==cppunivariate::FaceComparison::NumericalTie);
  CHECK(cppunivariate::compare_face(-magnitude,-2*magnitude,2e-8)==cppunivariate::FaceComparison::FirstWins);
  CHECK(cppunivariate::compare_face(-2*magnitude,-magnitude,2e-8)==cppunivariate::FaceComparison::SecondWins);
  CHECK(cppunivariate::compare_face(-10.,-10.,2e-8)==cppunivariate::FaceComparison::FirstWins);
}

TEST_CASE("genbeta: audited huge held-parameter magnitude") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  std::vector<double> x{std::exp(-1.),std::exp(-2.)};
  cppunivariate::span s{x.data(),x.size()};
  const auto r=cppunivariate::genbeta_mle(s,1,1e-7,300,nan,1e100,nan);
  CHECK((r.status==cppunivariate::GenbetaStatus::Ok || r.status==cppunivariate::GenbetaStatus::PowerBoundary || r.status==cppunivariate::GenbetaStatus::SearchUnresolved));
  CHECK(r.status!=cppunivariate::GenbetaStatus::UnitGammaBoundary);
}

TEST_CASE("genbeta: structured face payloads") {
  std::vector<double> x,p;
  for(int i=0;i<300;++i) {const double u=(i+.5)/300;
    x.push_back(std::exp(-cppunivariate::special::qgamma(u,2,1./3,true,false)));p.push_back(.7*u*u);}
  const auto g=cppunivariate::genbeta_mle(mltest::as_span(x),1);
  REQUIRE(g.stop_detail.limit_coef.size()==3);
  double ll=0;for(double v:x)ll+=cppunivariate::special::dgamma(-std::log(v),g.stop_detail.limit_coef[0],g.stop_detail.limit_coef[1],true)-std::log(v);
  CHECK(g.stop_detail.limit_loglik==doctest::Approx(ll));CHECK(g.stop_detail.limit_coef[2]==1);
  const auto r=cppunivariate::genbeta_mle(mltest::as_span(p),1);
  REQUIRE(r.stop_detail.limit_coef.size()==2);
  CHECK(r.stop_detail.limit_coef[0]==doctest::Approx(p.back()));
  const auto public_fit=cppunivariate::power_mle(mltest::as_span(p));
  CHECK(r.stop_detail.limit_coef[0]==doctest::Approx(public_fit.alpha));
  CHECK(r.stop_detail.limit_coef[1]==doctest::Approx(public_fit.beta));
  CHECK(r.stop_detail.limit_loglik==doctest::Approx(public_fit.logLik));
}

TEST_CASE("genbeta certificate: held shapes power root includes q below one") {
  const std::vector<double> x{.02,.2,.6,.9};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(double p:{.1,1.,10.})for(double q:{.01,.7,2.,30.}) {
    const auto r=cppunivariate::genbeta_mle(mltest::as_span(x),1,1e-7,200,p,q,nan);
    REQUIRE(r.status==cppunivariate::GenbetaStatus::Ok);
    double score=4;
    for(double v:x){const double z=-r.shape3*std::log(v);score-=p*z;score+=(q-1)*z/std::expm1(z);}
    CHECK(std::abs(score)<1e-9);
  }
}

TEST_CASE("genbeta certificate: tiny held q retains the power score scale") {
  const std::vector<double> x{.02,.2,.6,.9};
  double mean=0;for(double v:x)mean-=std::log(v)/x.size();
  for(double q:{1e-16,1e-100,1e-300}) {
    const auto r=cppunivariate::genbeta_mle(mltest::as_span(x),1,1e-7,200,2,q);
    REQUIRE(r.status==cppunivariate::GenbetaStatus::Ok);
    CHECK(r.shape3/q==doctest::Approx(1/(1.5*mean)).epsilon(1e-12));
    CHECK(std::isfinite(r.logLik));
  }
}

TEST_CASE("genbeta certificate: large held q retains the weighted exponential tail") {
  const std::vector<double> x{.5};
  const double q=std::exp(705.);
  const auto r=cppunivariate::genbeta_mle(mltest::as_span(x),1,1e-7,200,2,q);
  REQUIRE(r.status==cppunivariate::GenbetaStatus::Ok);
  const double z=r.shape3*std::log(2.);
  CHECK(z>700);
  CHECK(std::abs(z-(705-std::log(2-1/z)))<1e-9);
}

TEST_CASE("genbeta certificate: critical p one half retains the quadratic score") {
  const std::vector<double> x{.02,.2,.6,.9};
  double second=0;for(double v:x)second+=std::pow(std::log(v),2)/x.size();
  for(double q:{1e-100,1e-300}) {
    const auto r=cppunivariate::genbeta_mle(mltest::as_span(x),1,1e-7,1000,.5,q);
    REQUIRE(r.status==cppunivariate::GenbetaStatus::Ok);
    CHECK(r.shape3/std::sqrt(q)==doctest::Approx(std::sqrt(12/second)).epsilon(1e-12));
  }
}

TEST_CASE("genbeta certificate: held p one transports the Kumaraswamy certificate") {
  const std::vector<double> u{.02,.2,.6,.9};
  const auto parent=cppunivariate::kumar_mle(mltest::as_span(u),1e-7,300);
  REQUIRE(parent.status==cppunivariate::KumarStatus::Ok);
  for(double scale:{1e-200,1.,7.,1e200}) {
    std::vector<double> x;for(double v:u)x.push_back(scale*v);
    const auto r=cppunivariate::genbeta_mle(mltest::as_span(x),scale,1e-7,300,1);
    REQUIRE(r.status==cppunivariate::GenbetaStatus::Ok);
    CHECK(r.shape1==1);
    CHECK(r.shape2==doctest::Approx(parent.b).epsilon(1e-6));
    CHECK(r.shape3==doctest::Approx(parent.a).epsilon(1e-6));
    CHECK(r.logLik+u.size()*std::log(scale)==doctest::Approx(parent.logLik).epsilon(1e-10));
    double ll=0;for(double v:x)ll+=cppunivariate::distributions::dgenbeta(v,1,r.shape2,r.shape3,scale,true);
    CHECK(ll==doctest::Approx(r.logLik).epsilon(1e-10));
  }
}

TEST_CASE("genbeta certificate: held p one preserves parent stops and witness coordinates") {
  const std::vector<double> u{.02,.2,.6,.9};
  for(int maxit:{1,2,3}) {
    const auto k=cppunivariate::kumar_mle(mltest::as_span(u),1e-7,maxit);
    std::vector<double> x;for(double v:u)x.push_back(7*v);
    const auto r=cppunivariate::genbeta_mle(mltest::as_span(x),7,1e-7,maxit,1);
    CHECK((r.status==cppunivariate::GenbetaStatus::Ok)==(k.status==cppunivariate::KumarStatus::Ok));
    CHECK(r.stop_detail.budget_exhausted==k.stop_detail.budget_exhausted);
    REQUIRE(r.stop_detail.candidates.size()==k.stop_detail.candidates.size());
    for(std::size_t i=0;i<k.stop_detail.candidates.size();++i) {
      const auto& c=r.stop_detail.candidates[i];const auto& p=k.stop_detail.candidates[i];
      REQUIRE(c.coef.size()==4);
      CHECK(c.coef[0]==1);CHECK(c.coef[1]==p.coef[1]);CHECK(c.coef[2]==p.coef[0]);CHECK(c.coef[3]==7);
      CHECK(c.loglik_lower==doctest::Approx(p.loglik_lower-u.size()*std::log(7.)).epsilon(1e-12));
      CHECK(c.loglik_upper==doctest::Approx(p.loglik_upper-u.size()*std::log(7.)).epsilon(1e-12));
    }
  }
}

TEST_CASE("genbeta certificate: unresolvable scalar coefficient inclusion stops") {
  const std::vector<double> x{.02,.2,.6,.9};
  const auto r=cppunivariate::genbeta_mle(mltest::as_span(x),1,1e-20,300,2,.7);
  CHECK(r.status==cppunivariate::GenbetaStatus::SearchUnresolved);
  REQUIRE(r.stop_detail.candidates.size()==1);
  CHECK(r.stop_detail.candidates[0].coef.size()==4);
  CHECK(std::isinf(r.stop_detail.loglik_upper));
}

TEST_CASE("genbeta certificate: parent budget and numerical stops are terminal") {
  const std::vector<double> x{.5,.500000000001};
  const auto short_fit=cppunivariate::genbeta_mle(mltest::as_span(x),1,1e-7,1,1);
  CHECK(short_fit.status==cppunivariate::GenbetaStatus::SearchUnresolved);
  CHECK(short_fit.stop_detail.budget_exhausted);
  const auto r=cppunivariate::genbeta_mle(mltest::as_span(x),1,1e-7,300,1);
  CHECK(r.status==cppunivariate::GenbetaStatus::SearchUnresolved);
  CHECK_FALSE(r.stop_detail.budget_exhausted);
  REQUIRE(r.stop_detail.candidates.size()==1);
  CHECK(r.stop_detail.candidates[0].coef.size()==4);
  CHECK(r.stop_detail.candidates[0].coef[0]==1);
  CHECK(r.stop_detail.candidates[0].coef[3]==1);
}

TEST_CASE("genbeta certificate: held p free q and power has a complete finite search") {
  const auto g=mltest::load_distribution_golden("genbeta");
  double mean=0,jac=0;for(double v:g.data){mean-=std::log(v)/g.data.size();jac-=std::log(v);}
  const double exponential=g.data.size()*(-std::log(mean)-1)+jac;
  for(double p:{.1,.4,.5,2.,10.}) {
    CAPTURE(p);
    const auto r=cppunivariate::genbeta_mle(mltest::as_span(g.data),1,1e-7,300,p);
    CAPTURE(static_cast<int>(r.certificate_failure));
    REQUIRE(r.status==cppunivariate::GenbetaStatus::Ok);
    CHECK(r.shape1==p);
    CHECK(r.logLik>=exponential-1e-8);
    double power_score=1-p*r.shape3*mean,q_score=0;
    for(double v:g.data) {
      const double z=-r.shape3*std::log(v);
      power_score+=(r.shape2-1)*z/std::expm1(z)/g.data.size();
      q_score+=std::log(-std::expm1(-z))/g.data.size();
    }
    q_score+=cppunivariate::special::digamma(p+r.shape2)-cppunivariate::special::digamma(r.shape2);
    CHECK(std::abs(power_score)<1e-9);
    CHECK(std::abs(q_score)<1e-9);
  }
  CHECK(cppunivariate::genbeta_mle(mltest::as_span(g.data),1,1e-7,1,.4).status==cppunivariate::GenbetaStatus::SearchUnresolved);
}

TEST_CASE("genbeta certificate: rounded logarithms do not prove constant-data nonexistence") {
  const double v=std::exp(-700.);
  const std::vector<double> x{v,std::nextafter(v,1.)};
  REQUIRE(x[0]!=x[1]);
  REQUIRE(std::log(x[0])==std::log(x[1]));
  for(double p:{.4,std::numeric_limits<double>::quiet_NaN()}) {
    const auto r=cppunivariate::genbeta_mle(mltest::as_span(x),1,1e-7,300,p);
    CHECK(r.status==cppunivariate::GenbetaStatus::SearchUnresolved);
    CHECK(r.certificate_failure==cppunivariate::GenbetaCertificateFailure::Arithmetic);
  }
}

TEST_CASE("genbeta held q: free p and power certify finite frozen-sample maxima") {
  const auto g=mltest::load_distribution_golden("genbeta");
  for(double q:{.01,.1,.3}) {
    const auto r=cppunivariate::genbeta_mle(mltest::as_span(g.data),1,1e-7,300,NAN,q);
    CAPTURE(q); CAPTURE(static_cast<int>(r.certificate_failure)); CAPTURE(r.iterations);
    REQUIRE(r.status==cppunivariate::GenbetaStatus::Ok);
    CHECK(r.shape2==q);
    double ll=0,sp=0,sa=1;
    for(double x:g.data) {
      const double y=-std::log(x),z=r.shape3*y;
      ll+=cppunivariate::distributions::dgenbeta(x,r.shape1,q,r.shape3,1,true);
      sp-=z/g.data.size();
      sa+=(-r.shape1*z+(q-1)*z/std::expm1(z))/g.data.size();
    }
    sp+=cppunivariate::special::digamma(r.shape1+q)-cppunivariate::special::digamma(r.shape1);
    CHECK(ll==doctest::Approx(r.logLik).epsilon(1e-10));
    CHECK(std::abs(sp*r.shape1)<1e-9);
    CHECK(std::abs(sa)<1e-9);
  }
}

TEST_CASE("genbeta held q: unresolved neighborhoods and precision remain terminal") {
  const auto g=mltest::load_distribution_golden("genbeta");
  const auto s=mltest::as_span(g.data);
  for(int limit:{1,2,300}) {
    const auto r=cppunivariate::genbeta_mle(s,1,1e-7,limit,NAN,.7);
    CHECK(r.status==cppunivariate::GenbetaStatus::SearchUnresolved);
    CHECK(r.status!=cppunivariate::GenbetaStatus::UnitGammaBoundary);
    if(!r.stop_detail.candidates.empty()) {
      CHECK(r.stop_detail.candidates[0].coef.size()==4);
      CHECK(r.stop_detail.candidates[0].coef[1]==.7);
      CHECK(std::isfinite(r.stop_detail.loglik_lower));
      CHECK(std::isinf(r.stop_detail.loglik_upper));
    }
  }
  const auto r=cppunivariate::genbeta_mle(s,1,1e-20,300,NAN,.3);
  CHECK(r.status==cppunivariate::GenbetaStatus::SearchUnresolved);
}

TEST_CASE("genbeta held q: q above one power face has a finite improvement") {
  // q=2, v=(.5,1.5), h=100 and p=1/h: per-observation log likelihood
  // equals -1 + log(1+1/h) + mean(log(1-exp(-h*v))) > -1.
  const std::vector<double> x{std::exp(-.5),std::exp(-1.5)};
  const auto r=cppunivariate::genbeta_mle(mltest::as_span(x),1,1e-7,300,NAN,2);
  CHECK(r.status!=cppunivariate::GenbetaStatus::PowerBoundary);
  double normalized=0;
  for(double v:x)normalized+=(cppunivariate::distributions::dgenbeta(v,.01,2,100,1,true)+std::log(v))/2;
  CHECK(normalized> -1+.009);
}
