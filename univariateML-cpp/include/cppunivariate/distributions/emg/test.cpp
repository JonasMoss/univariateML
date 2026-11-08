#include "test_support.hpp"
#include "cppunivariate/distributions/emg/mle.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
namespace {
cppunivariate::span emg_span(const std::vector<double>& x) {return {x.data(),x.size()};}
}
TEST_CASE("emg stable density and direct tails") {
  CHECK(cppunivariate::distributions::pemg(100,0,1,1,true,true)/(-std::exp(-99.5))==doctest::Approx(1).epsilon(1e-12));
  CHECK(cppunivariate::distributions::pemg(100,0,1,1e-300,false,true)/(-1e-298)==doctest::Approx(1).epsilon(1e-12));
  CHECK(cppunivariate::distributions::demg(0,0,1,100)==doctest::Approx(.398902398135681).epsilon(1e-12));
  CHECK(cppunivariate::distributions::demg(-40,0,1,1,true)==doctest::Approx(-804.6331046018).epsilon(1e-12));
  for(double l:{1e-300,1e-8,1.,100.,1e6})for(double z:{-100.,-40.,-2.,0.,2.,40.,100.}) {
    const double a=cppunivariate::distributions::pemg(z,0,1,l);
    const double b=cppunivariate::distributions::pemg(z,0,1,l,false);
    CHECK(a+b==doctest::Approx(1).epsilon(1e-14));
    CHECK(std::isfinite(cppunivariate::distributions::demg(z,0,1,l,true)));
    CHECK(std::isfinite(cppunivariate::distributions::pemg(z,0,1,l,true,true)));
    CHECK(std::isfinite(cppunivariate::distributions::pemg(z,0,1,l,false,true)));
  }
}
TEST_CASE("emg extreme log quantile round trips") {
  for(double x:{-1e100,-1e150}) {
    const double lp=cppunivariate::distributions::pemg(x,0,1,1,true,true);
    CHECK(std::isfinite(lp));
    CHECK(lp==doctest::Approx(-x*x/2).epsilon(1e-14));
  }
  for(double l:{1e-300,1e-8,1.,100.,1e6})for(bool lower:{false,true})for(double p:{-1000.,-100.,-10.,-.6931471805599453,-.0001,-1e-100}) {
    const double q=cppunivariate::distributions::qemg(p,0,1,l,lower,true);
    CHECK(cppunivariate::distributions::pemg(q,0,1,l,lower,true)/p==doctest::Approx(1).epsilon(2e-8));
  }
}
TEST_CASE("emg faces and fixed parameters") {
  const std::vector<double> x={-2,-1,0,1,2};
  CHECK(cppunivariate::emg_mle(emg_span(x)).status==cppunivariate::EmgStatus::TranslatedExponentialLimit);
  const std::vector<double> c={1,1,1};
  CHECK(cppunivariate::emg_mle(emg_span(c)).status==cppunivariate::EmgStatus::Unbounded);
  CHECK(cppunivariate::emg_mle(emg_span(x),1e-8,500,0,1,1).status==cppunivariate::EmgStatus::Ok);
  CHECK(cppunivariate::emg_mle(emg_span(x),1e-8,500,0,1).status==cppunivariate::EmgStatus::NormalLimit);
  const std::vector<double> tied={0,0,1,2};
  const auto endpoint=cppunivariate::emg_mle(emg_span(tied),1e-8,500,0,cppunivariate::special::kNaN,1);
  CHECK(endpoint.status==cppunivariate::EmgStatus::TranslatedExponentialLimit);
  CHECK(endpoint.logLik==doctest::Approx(-3-2*std::log(2.0)).epsilon(1e-12));
  CHECK(cppunivariate::emg_mle(emg_span(c),1e-8,500,cppunivariate::special::kNaN,cppunivariate::special::kNaN,1).status==cppunivariate::EmgStatus::TranslatedExponentialLimit);
}
TEST_CASE("emg golden") {
  const auto g=mltest::load_distribution_golden("emg");
  const auto r=cppunivariate::emg_mle(emg_span(g.data));
  CHECK(r.status==cppunivariate::EmgStatus::Ok);
  CHECK(r.mu==doctest::Approx(g.estimates.at("mu")).epsilon(1e-6));
  CHECK(r.sigma==doctest::Approx(g.estimates.at("sigma")).epsilon(1e-6));
  CHECK(r.lambda==doctest::Approx(g.estimates.at("lambda")).epsilon(1e-6));
  CHECK(r.logLik==doctest::Approx(g.logLik).epsilon(1e-10));
  double mean=0;for(double x:g.data)mean+=x/g.data.size();
  CHECK(r.mu+1/r.lambda==doctest::Approx(mean).epsilon(1e-6));
  double score2=0;
  const std::vector<double> theta={r.mu,std::log(r.sigma),std::log(r.lambda)};
  for(int j=0;j<3;++j) {
    auto plus=theta,minus=theta;plus[j]+=1e-5;minus[j]-=1e-5;
    double a=0,b=0;
    for(double x:g.data) {
      a+=cppunivariate::distributions::demg(x,plus[0],std::exp(plus[1]),std::exp(plus[2]),true);
      b+=cppunivariate::distributions::demg(x,minus[0],std::exp(minus[1]),std::exp(minus[2]),true);
    }
    score2+=std::pow((a-b)/2e-5,2);
  }
  CHECK(std::sqrt(score2)<1e-3);
}
TEST_CASE("emg portable sampling and argument boundaries") {
  cppunivariate::distributions::StdRng<> rng(313);
  double sum=0,square=0;
  for(int i=0;i<30000;++i) {
    const double x=cppunivariate::distributions::remg(rng,2,1,2);
    sum+=x;square+=x*x;
  }
  CHECK(std::abs(sum/30000-2.5)<.03);
  CHECK(std::abs(square/30000-std::pow(sum/30000,2)-1.25)<.04);
  CHECK(cppunivariate::distributions::qemg(0,0,1,1)==cppunivariate::special::kNegInf);
  CHECK(cppunivariate::distributions::qemg(1,0,1,1)==cppunivariate::special::kPosInf);
  CHECK(cppunivariate::distributions::pemg(cppunivariate::special::kNegInf,0,1,1)==0);
  CHECK(cppunivariate::distributions::pemg(cppunivariate::special::kPosInf,0,1,1)==1);
  CHECK(std::isnan(cppunivariate::distributions::demg(0,0,0,1)));
  const double q=cppunivariate::distributions::qemg(1e-300,0,1,1e-320);
  CHECK(std::isfinite(q));
  CHECK(cppunivariate::distributions::pemg(q,0,1,1e-320,true,true)==doctest::Approx(std::log(1e-300)).epsilon(1e-12));
}

#include "cppunivariate/distributions/emg/dpqr.hpp"

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("emg: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("emg",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::demg(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pemg(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qemg(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::remg(rng, p[0], p[1], p[2]); });
}

TEST_CASE("emg: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("emg");
  const auto x=mltest::as_span(g.data);
  CHECK(cppunivariate::emg_mle(x,1e-8,1).status==cppunivariate::EmgStatus::IterationFailure);
}

TEST_CASE("logconcave fixed-shape normal inner solution") {
  const std::vector<double> x={-2,-1,0,1,2};
  const auto normal=[](CPPUNIV_EXTENDED_REAL z,CPPUNIV_EXTENDED_REAL) {
    return std::array<CPPUNIV_EXTENDED_REAL,3>{{-z*z/2,-z,-1}};
  };
  const auto fit=cppunivariate::logconcave_detail::inner(x,1,normal,100);
  REQUIRE(fit.ok);
  CHECK(fit.u==doctest::Approx(0).epsilon(1e-12));
  CHECK(fit.b==doctest::Approx(1/std::sqrt(2.0)).epsilon(1e-12));
}

TEST_CASE("emg: face comparison cannot resolve rounded extreme objectives") {
  const double magnitude=1e20;
  CHECK(cppunivariate::compare_face(-magnitude,-magnitude,2e-8)==cppunivariate::FaceComparison::NumericalTie);
  CHECK(cppunivariate::compare_face(-magnitude,-2*magnitude,2e-8)==cppunivariate::FaceComparison::FirstWins);
  CHECK(cppunivariate::compare_face(-2*magnitude,-magnitude,2e-8)==cppunivariate::FaceComparison::SecondWins);
  CHECK(cppunivariate::compare_face(-10.,-10.,2e-8)==cppunivariate::FaceComparison::FirstWins);
}

TEST_CASE("emg: audited huge held-parameter magnitude") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  std::vector<double> x{1,2};
  cppunivariate::span s{x.data(),x.size()};
  const auto r=cppunivariate::emg_mle(s,1e-8,500,0,1e-10,nan);
  CHECK((r.status==cppunivariate::EmgStatus::Ok || r.status==cppunivariate::EmgStatus::IterationFailure));
  CHECK(r.status!=cppunivariate::EmgStatus::NormalLimit);
}

TEST_CASE("emg: structured face payloads") {
  const std::vector<double> x{-2,-1,0,1,2};
  const auto e=cppunivariate::emg_mle(mltest::as_span(x));
  REQUIRE(e.stop_detail.limit_coef.size()==2);
  CHECK(e.stop_detail.limit_coef[0]==-2);
  CHECK(e.stop_detail.limit_coef[1]==doctest::Approx(.5));
  CHECK(e.stop_detail.limit_loglik==doctest::Approx(5*(std::log(.5)-1)));
  const auto n=cppunivariate::emg_mle(mltest::as_span(x),1e-8,500,0,1);
  REQUIRE(n.stop_detail.limit_coef.size()==2);
  CHECK(n.stop_detail.limit_coef[0]==0);CHECK(n.stop_detail.limit_coef[1]==1);
  CHECK(n.stop_detail.limit_loglik==doctest::Approx(-2.5*std::log(2*std::acos(-1.))-5));
}

#include "cppunivariate/distributions/emg/shape_bounds.hpp"
TEST_CASE("emg certificate: location root enclosure and coordinate accuracy") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(double sigma:{.01,1.,100.})for(double lambda:{.001,1.,1000.}) {
    std::vector<double> x;
    for(int j=0;j<17;++j)x.push_back(cppunivariate::distributions::qemg((j+.5)/17,2,sigma,lambda));
    const auto c=cppunivariate::emg_detail::location_certificate(mltest::as_span(x),sigma,lambda,1e-10,500);
    REQUIRE(c.ok);
    CHECK(c.lower<=c.mu);CHECK(c.mu<=c.upper);
    CHECK(c.upper-c.lower<=2.1e-10*std::max(sigma,std::abs(c.mu)));
    const auto lo=cppunivariate::emg_detail::location_score(mltest::as_span(x),c.lower,sigma,lambda);
    const auto hi=cppunivariate::emg_detail::location_score(mltest::as_span(x),c.upper,sigma,lambda);
    CHECK(lo[0]>0);CHECK(hi[1]<0);
    CHECK(c.detail.loglik_lower<=c.loglik);CHECK(c.detail.loglik_upper>=c.loglik);
    const auto fit=cppunivariate::emg_mle(mltest::as_span(x),1e-10,500,nan,sigma,lambda);
    CHECK(fit.status==cppunivariate::EmgStatus::Ok);
    CHECK(fit.mu==c.mu);CHECK(fit.sigma==sigma);CHECK(fit.lambda==lambda);
  }
  const std::vector<double> x{-2,-1,0,1,2};
  const auto short_fit=cppunivariate::emg_mle(mltest::as_span(x),1e-10,1,nan,1,1);
  CHECK(short_fit.status==cppunivariate::EmgStatus::IterationFailure);
  CHECK_FALSE(short_fit.stop_detail.candidates.empty());
  const auto too_precise=cppunivariate::emg_mle(mltest::as_span(x),1e-30,500,nan,1,1);
  CHECK(too_precise.status==cppunivariate::EmgStatus::NumericalFailure);
  CHECK_FALSE(too_precise.stop_detail.candidates.empty());
}
TEST_CASE("emg certificate: globally dominated normal faces respect holds") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> x{-2,-1,0,1,2};
  for(double sigma:{nan,3.}) {
    const auto fit=cppunivariate::emg_mle(mltest::as_span(x),1e-10,500,2,sigma);
    REQUIRE(fit.status==cppunivariate::EmgStatus::NormalLimit);
    REQUIRE(fit.stop_detail.limit_coef.size()==2);
    CHECK(fit.stop_detail.limit_coef[0]==2);
    CHECK(fit.stop_detail.limit_coef[1]==doctest::Approx(std::isnan(sigma)?std::sqrt(6.):3.));
  }
  const std::vector<double> constant{4,4,4};
  CHECK(cppunivariate::emg_mle(mltest::as_span(constant),1e-10,500,nan,2).status==cppunivariate::EmgStatus::NormalLimit);
  CHECK(cppunivariate::emg_mle(mltest::as_span(constant),1e-10,500,4).status==cppunivariate::EmgStatus::Unbounded);
  const auto location=cppunivariate::emg_mle(mltest::as_span(constant),1e-10,500,nan,2,1);
  CHECK(location.status==cppunivariate::EmgStatus::Ok);
  CHECK(std::isfinite(location.mu));
}
TEST_CASE("emg certificate: small shape and interval bounds dominate witnesses") {
  const std::vector<double> x{0,0,.1,1,4};
  const auto data=mltest::as_span(x);
  for(double kmax:{.001,.1,1.})for(double fraction:{.01,.5,1.})
    for(double lambda:{.01,.2,1.,10.})for(double mu:{-2.,0.,.1,2.}) {
      const double k=kmax*fraction,sigma=k/lambda;
      double ll=0;for(double v:x)ll+=cppunivariate::distributions::demg(v,mu,sigma,lambda,true);
      CHECK(ll<=cppunivariate::emg_detail::small_shape_upper(data,kmax));
      if(mu<=0) {
        CHECK(ll<=cppunivariate::emg_detail::held_location_small_shape_upper(data,mu,kmax));
        CHECK(ll<=cppunivariate::emg_detail::held_location_small_shape_upper(data,mu,kmax,lambda));
      }
      // The density inequality is pointwise and does not require a nuisance solve.
      double lower_ll=0;for(double v:x)lower_ll+=cppunivariate::distributions::demg(v,mu,sigma,(k/2)/sigma,true);
      CHECK(ll<=cppunivariate::emg_detail::shape_interval_upper(lower_ll,x.size(),k/2,k));
    }
  const double face=5*(std::log(5/5.1)-1)-2*std::log(2.);
  CHECK(cppunivariate::emg_detail::held_location_small_shape_upper(data,0,1e-6)==doctest::Approx(face).epsilon(1e-11));
  const std::vector<double> constant{1,1};
  CHECK(std::isinf(cppunivariate::emg_detail::small_shape_upper(mltest::as_span(constant),.1)));
}

#include "cppunivariate/distributions/emg/fixed_shape_certificate.hpp"
TEST_CASE("emg certificate: fixed shape inner bounds with every physical hold") {
  const std::vector<double> x{-2,-1,0,1,2};
  const double nan=std::numeric_limits<double>::quiet_NaN(),k=1.5;
  for(int mask=0;mask<8;++mask) {
    const double mu=mask&1?0:nan,sigma=mask&2?1:nan,lambda=mask&4?k:nan;
    const auto c=cppunivariate::emg_detail::fixed_shape_certificate(mltest::as_span(x),k,1e-7,500,mu,sigma,lambda);
    REQUIRE(c.ok);
    CHECK(c.b_lower<=c.b);CHECK(c.b<=c.b_upper);
    CHECK(c.loglik_upper-c.loglik_lower<1e-6);
    for(double m:{-2.,-1.,0.,1.,2.})for(double b:{.1,.5,1.,2.}) {
      if((mask&1)&&m!=mu)continue;
      if((mask&2)&&b!=1/sigma)continue;
      if((mask&4)&&b!=lambda/k)continue;
      double ll=0;for(double v:x)ll+=cppunivariate::distributions::demg(v,m,1/b,k*b,true);
      CHECK(ll<=c.loglik_upper);
    }
    // A constrained optimum at any k in this interval is bounded by the
    // relaxed unconstrained profile at the lower endpoint.
    const auto relaxed=cppunivariate::emg_detail::fixed_shape_certificate(mltest::as_span(x),k,1e-7,500);
    REQUIRE(relaxed.ok);
    const double upper=cppunivariate::emg_detail::shape_interval_upper(relaxed.loglik_upper,x.size(),k,k*2);
    CHECK(c.loglik_lower<=upper);
  }
  CHECK_FALSE(cppunivariate::emg_detail::fixed_shape_certificate(mltest::as_span(x),k,1e-7,1).ok);
  CHECK_FALSE(cppunivariate::emg_detail::fixed_shape_certificate(mltest::as_span(x),k,1e-7,500,nan,1,1).ok);
}

TEST_CASE("emg certificate: constant exponential faces distinguish held endpoint") {
  const std::vector<double> x{4,4,4};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(double mu:{nan,4.}) {
    const auto r=cppunivariate::emg_mle(mltest::as_span(x),1e-10,500,mu,nan,2);
    REQUIRE(r.status==cppunivariate::EmgStatus::TranslatedExponentialLimit);
    CHECK(r.logLik==doctest::Approx(std::isnan(mu)?3*std::log(2.):0.));
    REQUIRE(r.stop_detail.limit_coef.size()==2);
    CHECK(r.stop_detail.limit_coef[0]==4);CHECK(r.stop_detail.limit_coef[1]==2);
    CHECK(r.stop_detail.limit_loglik==r.logLik);
  }
}

TEST_CASE("emg certificate: unrepresentable nuisance bounds remain conservative") {
  const std::vector<double> x{1e308,1e308};
  CHECK(cppunivariate::emg_detail::held_location_small_shape_upper(mltest::as_span(x),-1e308,.1)>-2000);
}

TEST_CASE("emg certificate: uniform large shape bounds optimize nuisance coordinates") {
  const std::vector<double> x{-2,-1,0,1,2};
  const auto data=mltest::as_span(x);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(double kmin:{1e4,1e8})for(double mu:{nan,0.})for(double sigma:{nan,1.}) {
    const double upper=cppunivariate::emg_detail::large_shape_upper(data,kmin,-100,500,mu,sigma);
    REQUIRE(std::isfinite(upper));
    for(double k:{kmin,2*kmin})for(double m:{-2.,0.,2.})for(double s:{.1,1.,10.}) {
      if(!std::isnan(mu)&&m!=mu)continue;
      if(!std::isnan(sigma)&&s!=sigma)continue;
      double ll=0;for(double v:x)ll+=cppunivariate::distributions::demg(v,m,s,k/s,true);
      CHECK(ll<=upper);
    }
    if(kmin==1e8) {
      const double sd=std::isnan(sigma)?std::sqrt(2.):sigma;
      const double normal=5*(-.91893853320467274178-std::log(sd)-1/(sd*sd));
      CHECK(upper-normal<1e-4);
    }
  }
  CHECK(cppunivariate::emg_detail::large_shape_upper(data,1e8,-100,500,nan,nan,1)<-90);
  CHECK(std::isinf(cppunivariate::emg_detail::large_shape_upper(data,1e8,-100,0)));
}

TEST_CASE("emg certificate: conditional small shape bounds retain physical holds") {
  const std::vector<double> x{0,0,.1,1,4};const auto data=mltest::as_span(x);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(double mu:{nan,-1.,0.,2.})for(double sigma:{nan,1.})for(double lambda:{nan,.5}) {
    const double upper=cppunivariate::emg_detail::conditional_small_shape_upper(data,.1,mu,sigma,lambda);
    REQUIRE(std::isfinite(upper));
    for(double m:{-1.,0.,2.})for(double k:{.001,.1})for(double rate:{.01,.5}) {
      if(!std::isnan(mu)&&mu!=m)continue;
      if(!std::isnan(lambda)&&lambda!=rate)continue;
      const double s=std::isnan(sigma)?k/rate:sigma;
      if(s*rate>.1)continue;
      double ll=0;for(double v:x)ll+=cppunivariate::distributions::demg(v,m,s,rate,true);
      CHECK(ll<=upper);
    }
  }
  CHECK(cppunivariate::emg_detail::conditional_small_shape_upper(data,1e-8,2)<-80);
  CHECK(cppunivariate::emg_detail::conditional_small_shape_upper(data,1e-8,nan,1)<-80);
}

TEST_CASE("emg certificate: quadratic shape chords retain conditional endpoint profiles") {
  const std::vector<double> x{-2,-1,0,1,2};const auto data=mltest::as_span(x);
  const double nan=std::numeric_limits<double>::quiet_NaN(),lo=.8,hi=1.2;
  for(double mu:{nan,0.})for(double lambda:{nan,.5}) {
    const auto a=cppunivariate::emg_detail::fixed_shape_certificate(data,lo,1e-7,500,mu,nan,lambda);
    const auto b=cppunivariate::emg_detail::fixed_shape_certificate(data,hi,1e-7,500,mu,nan,lambda);
    REQUIRE(a.ok);REQUIRE(b.ok);
    const double upper=std::isnan(lambda)?
      cppunivariate::emg_detail::shape_chord_upper(a.loglik_upper,b.loglik_upper,x.size(),lo,hi):
      cppunivariate::emg_detail::fixed_rate_shape_chord_upper(a.loglik_upper,b.loglik_upper,data,lo,hi,lambda,mu);
    for(double k:{.85,1.,1.15}) {
      const auto c=cppunivariate::emg_detail::fixed_shape_certificate(data,k,1e-7,500,mu,nan,lambda);
      REQUIRE(c.ok);CHECK(c.loglik_lower<=upper);
    }
  }
}

TEST_CASE("emg certificate: exact constant classification avoids variance roundoff") {
  const std::vector<double> tied(7,.1),distinct{0,1e-200};
  CHECK(cppunivariate::emg_mle(mltest::as_span(tied),1e-10,1).status==cppunivariate::EmgStatus::Unbounded);
  CHECK(cppunivariate::emg_mle(mltest::as_span(distinct),1e-10,1).status!=cppunivariate::EmgStatus::Unbounded);
}

TEST_CASE("emg certificate: endpoint multiplicity gives a signed small shape remainder") {
  const std::vector<double> x{0,0,.1,1,4};
  const double face=5*(std::log(5/5.1)-1)-2*std::log(2.);
  const double upper=cppunivariate::emg_detail::held_location_small_shape_upper(mltest::as_span(x),0,.1);
  CHECK(upper>=face);CHECK(upper-face<1e-10);
}

TEST_CASE("emg certificate: subnormal score precision cannot manufacture location accuracy") {
  const std::vector<double> x{0};
  const auto r=cppunivariate::emg_mle(mltest::as_span(x),1e-10,2000,NAN,1,1e-320);
  CHECK(r.status!=cppunivariate::EmgStatus::Ok);
}

TEST_CASE("emg certificate: underflowed conditional normal RMS is numerical failure") {
  const std::vector<double> x{0,1e-200};
  const auto r=cppunivariate::emg_mle(mltest::as_span(x),1e-10,1,1e-200);
  CHECK(r.status!=cppunivariate::EmgStatus::Unbounded);
  CHECK((r.status==cppunivariate::EmgStatus::NormalLimit || r.status==cppunivariate::EmgStatus::NumericalFailure));
}

TEST_CASE("emg certificate: exponential amplification in Mills scores limits coordinate precision") {
  const std::vector<double> x{0};
  const auto r=cppunivariate::emg_mle(mltest::as_span(x),1e-16,2000,NAN,.3,1e-300);
  CHECK(r.status!=cppunivariate::EmgStatus::Ok);
}

TEST_CASE("emg outer certificate: budget exhaustion keeps finite and normal witnesses") {
  const std::vector<double> x{-2,-1,0,1,2};
  const auto r=cppunivariate::emg_mle(emg_span(x),1e-10,2,0,1);
  REQUIRE(r.status==cppunivariate::EmgStatus::BudgetExhausted);
  CHECK(r.stop_detail.budget_exhausted);
  CHECK(r.iterations==cppunivariate::certificate_budget(2));
  REQUIRE(r.stop_detail.candidates.size()>=2);
  CHECK(r.stop_detail.loglik_lower<=r.stop_detail.loglik_upper);
  CHECK(r.stop_detail.candidates[0].model==cppunivariate::StopModel::None);
  CHECK(r.stop_detail.candidates[1].model==cppunivariate::StopModel::Norm);
}

TEST_CASE("emg outer certificate: an unexcluded normal face is unresolved") {
  const std::vector<double> x{-2,-1,0,1,2};
  const auto r=cppunivariate::emg_mle(emg_span(x),1e-10,500,0);
  REQUIRE(r.status==cppunivariate::EmgStatus::Unresolved);
  REQUIRE(r.stop_detail.candidates.size()>=2);
  CHECK(r.stop_detail.candidates[0].model==cppunivariate::StopModel::None);
  CHECK(r.stop_detail.candidates[1].model==cppunivariate::StopModel::Norm);
  CHECK_FALSE(r.stop_detail.budget_exhausted);
  CHECK(std::isinf(r.stop_detail.loglik_upper));
}

TEST_CASE("emg outer certificate: an unrepresentable coefficient tolerance stops numerically") {
  const std::vector<double> x{-2,-1,0,1,2};
  const auto r=cppunivariate::emg_mle(emg_span(x),1e-30,500,0,NAN,1);
  CHECK(r.status==cppunivariate::EmgStatus::NumericalFailure);
  CHECK_FALSE(r.stop_detail.candidates.empty());
  CHECK(r.stop_detail.loglik_lower<=r.stop_detail.loglik_upper);
}

TEST_CASE("emg outer certificate: overflowing face witness bounds stop numerically") {
  const double u=std::sqrt(std::numeric_limits<double>::max())/1.0000000000000002;
  const std::vector<double> x{-u,u};
  const auto r=cppunivariate::emg_mle(emg_span(x),1e-8,500,0,1);
  CHECK(r.status==cppunivariate::EmgStatus::NumericalFailure);
  REQUIRE_FALSE(r.stop_detail.candidates.empty());
  for(const auto& c:r.stop_detail.candidates) {
    CHECK(std::isfinite(c.loglik_lower));CHECK(std::isfinite(c.loglik_upper));
    CHECK(c.loglik_lower<=c.loglik_upper);
  }
}

TEST_CASE("emg certificate: likelihood enclosure covers natural coordinate cancellation") {
  const std::vector<double> x{10000000001.,10000000005.,10000000009.};
  const cppunivariate::emg_detail::CertModel model(emg_span(x),NAN,NAN,NAN);
  const std::array<double,3> p{{7000000000.,.7,1.}};
  const auto e=cppunivariate::emg_detail::cert_evaluate(model,p);
  // 80-digit erfc reference with .7 interpreted as its exact binary64 value;
  // see dev/research/evaluations/2026-11-07-emg-cert-data/input-margin.py.
  const double reference=-11.0383558522123382319101050616522723855630979848364;
  REQUIRE(e.ok);
  CHECK(std::abs(e.ll-reference)<=e.ll_error);
  CHECK(e.ll_error<.01);
}

TEST_CASE("emg outer certificate: tail refinement finds a finite witness above the exponential face") {
  const std::vector<double> x{.01};
  const auto r=cppunivariate::emg_mle(emg_span(x),1e-8,10,0,NAN,1);
  REQUIRE((r.status==cppunivariate::EmgStatus::Ok||r.status==cppunivariate::EmgStatus::BudgetExhausted));
  if(r.status==cppunivariate::EmgStatus::Ok)CHECK(r.logLik>-.01);
  else {
    REQUIRE_FALSE(r.stop_detail.candidates.empty());
    CHECK(r.stop_detail.candidates.front().model==cppunivariate::StopModel::None);
    CHECK(r.stop_detail.candidates.front().loglik_lower>-.01);
    CHECK(r.stop_detail.budget_exhausted);
  }
}

TEST_CASE("emg scaled residuals: normal escape at extreme units") {
  for(double unit:{1e-200,1e200}) {
    const std::vector<double> x={-unit,-2*unit};
    const auto r=cppunivariate::emg_mle(mltest::as_span(x),1e-8,500,0);
    REQUIRE(r.status==cppunivariate::EmgStatus::NormalLimit);
    CHECK(r.sigma/unit==doctest::Approx(std::sqrt(2.5)).epsilon(1e-14));
    CHECK(std::isfinite(r.logLik));
  }
}

TEST_CASE("emg scaled residuals: large shape Gaussian bound commutes with units") {
  const std::vector<double> base={-2,-1,0,1,2};
  const auto data=mltest::as_span(base);
  const double reference=cppunivariate::emg_detail::large_shape_upper(data,1e8,-100,500,0,NAN);
  REQUIRE(std::isfinite(reference));
  for(double unit:{1e-200,1e200}) {
    std::vector<double> x;for(double v:base)x.push_back(v*unit);
    const double shift=-static_cast<double>(base.size())*std::log(unit);
    const double upper=cppunivariate::emg_detail::large_shape_upper(mltest::as_span(x),1e8,-100+shift,500,0,NAN);
    REQUIRE(std::isfinite(upper));
    CHECK(upper-shift==doctest::Approx(reference).epsilon(1e-9));
  }
}
