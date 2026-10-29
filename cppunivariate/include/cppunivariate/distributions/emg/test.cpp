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
