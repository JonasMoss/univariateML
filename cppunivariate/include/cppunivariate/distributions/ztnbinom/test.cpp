#include "test_support.hpp"
#include "cppunivariate/distributions/ztnbinom/mle.hpp"
#include "cppunivariate/distributions/ztnbinom/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <vector>
#include <cmath>
TEST_CASE("ztnbinom: golden, score and distribution") {
 const auto g=mltest::load_distribution_golden("ztnbinom");
 std::vector<int> x;for(double v:g.data)x.push_back(static_cast<int>(v));
 const auto fit=cppunivariate::ztnbinom_mle({x.data(),x.size()});
 REQUIRE(fit.status==cppunivariate::ZtnbinomStatus::Ok);
 CHECK(fit.size==doctest::Approx(g.estimates.at("size")).epsilon(1e-10));
 CHECK(fit.prob==doctest::Approx(g.estimates.at("prob")).epsilon(1e-10));
 CHECK(fit.logLik==doctest::Approx(g.logLik).epsilon(1e-10));
 std::vector<double> p{fit.size,fit.prob};
 auto ll=[&](const std::vector<double> &par){double l=0;for(int v:x)l+=cppunivariate::distributions::dztnbinom(v,par[0],par[1],true);return l;};
 CHECK(ll(p)==doctest::Approx(fit.logLik).epsilon(1e-10));
 double norm2=0;
 {auto a=p,b=p;const double h=1e-5;a[0]+=h;b[0]-=h;const double score=(ll(a)-ll(b))/(2*h);norm2+=score*score;}
 {auto a=p,b=p;const double h=1e-5;a[1]+=h;b[1]-=h;const double score=(ll(a)-ll(b))/(2*h);norm2+=score*score;}
 CHECK(std::sqrt(norm2)<1e-3);
 const auto fixed=cppunivariate::ztnbinom_mle({x.data(),x.size()},1e-10,200,2.5);
 CHECK(fixed.status==cppunivariate::ZtnbinomStatus::Ok);
 CHECK(fixed.size==2.5);
 CHECK(cppunivariate::ztnbinom_mle({x.data(),x.size()},1e-10,200,-1).status==cppunivariate::ZtnbinomStatus::InvalidSize);
 CHECK(cppunivariate::ztnbinom_mle({x.data(),x.size()},1e-10,200,std::numeric_limits<double>::infinity()).status==cppunivariate::ZtnbinomStatus::InvalidSize);
}
TEST_CASE("ztnbinom: attained boundaries and invalid input") {
 std::vector<int> ones{1,1,1},zeros{0,0,0},negative{-1,1};
 CHECK(cppunivariate::ztnbinom_mle({}).status==cppunivariate::ZtnbinomStatus::InvalidInput);
 CHECK(cppunivariate::ztnbinom_mle({negative.data(),negative.size()}).status==cppunivariate::ZtnbinomStatus::InvalidInput);
 const auto one=cppunivariate::ztnbinom_mle({ones.data(),ones.size()});
 CHECK(one.logLik==0);
 CHECK(one.status==cppunivariate::ZtnbinomStatus::Ok);
 CHECK(cppunivariate::ztnbinom_mle({zeros.data(),zeros.size()}).status==cppunivariate::ZtnbinomStatus::InvalidInput);
}
TEST_CASE("ztnbinom: scalar tails and quantile endpoints") {
 const auto g=mltest::load_distribution_golden("ztnbinom");
 CHECK(std::isfinite(cppunivariate::distributions::dztnbinom(10000,g.estimates.at("size"),g.estimates.at("prob"),true)));
 CHECK(cppunivariate::distributions::qztnbinom(0,g.estimates.at("size"),g.estimates.at("prob"))==1);
 CHECK(cppunivariate::distributions::qztnbinom(1,g.estimates.at("size"),g.estimates.at("prob"))==std::numeric_limits<double>::infinity());
}

TEST_CASE("ztnbinom: infinite-size escape and fixed-size removal") {
 std::vector<int> x{1,2,2,3,3,3};
 CHECK(cppunivariate::ztnbinom_mle({x.data(),x.size()}).status==cppunivariate::ZtnbinomStatus::PoissonLimit);
 CHECK(cppunivariate::ztnbinom_mle({x.data(),x.size()},1e-10,200,20).status==cppunivariate::ZtnbinomStatus::Ok);
}

TEST_CASE("ztnbinom: attained logarithmic face") {
 std::vector<int> x{1,1,1,1,1,2,3,10,30};
 const auto fit=cppunivariate::ztnbinom_mle({x.data(),x.size()});
 CHECK(fit.status==cppunivariate::ZtnbinomStatus::Ok);
 CHECK(fit.size==0);
 const auto fixed=cppunivariate::ztnbinom_mle({x.data(),x.size()},1e-10,200,0);
 CHECK(fixed.size==0);
 CHECK(fixed.logLik==doctest::Approx(fit.logLik));
}

TEST_CASE("ztnbinom: quantile inversion and sampling") {
 const auto g=mltest::load_distribution_golden("ztnbinom");
 const double q=cppunivariate::distributions::qztnbinom(.37,g.estimates.at("size"),g.estimates.at("prob"));
 CHECK(cppunivariate::distributions::pztnbinom(q,g.estimates.at("size"),g.estimates.at("prob"))>=.37);
 CHECK(cppunivariate::distributions::pztnbinom(q-1,g.estimates.at("size"),g.estimates.at("prob"))<.37);
 CHECK(cppunivariate::distributions::pztnbinom(q,g.estimates.at("size"),g.estimates.at("prob"),true,false)+cppunivariate::distributions::pztnbinom(q,g.estimates.at("size"),g.estimates.at("prob"),false,false)==doctest::Approx(1));
 cppunivariate::distributions::StdRng<> rng(19);
 for(int j=0;j<20;++j) {const double v=cppunivariate::distributions::rztnbinom(rng,g.estimates.at("size"),g.estimates.at("prob"));CHECK(v>=1);CHECK(v==std::floor(v));}
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("ztnbinom: d/p/q/r against the truth table" * doctest::may_fail()) {
  mltest::check_dpqr_truth("ztnbinom",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dztnbinom(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pztnbinom(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qztnbinom(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rztnbinom(rng, p[0], p[1]); });
}

TEST_CASE("ztnbinom: P2 every hold subset preserves coefficients and golden likelihood") {
 const auto g=mltest::load_distribution_golden("ztnbinom");
 std::vector<int> x;for(double v:g.data)x.push_back(static_cast<int>(v));
 const double nan=std::numeric_limits<double>::quiet_NaN();
 for(unsigned mask=1;mask<4;++mask) {
   const auto r=cppunivariate::ztnbinom_mle({x.data(),x.size()},1e-10,200,(mask & 1)?g.estimates.at("size"):nan,(mask & 2)?g.estimates.at("prob"):nan);
   REQUIRE(std::isfinite(r.logLik));
   CHECK(r.logLik>=g.logLik-1e-6*x.size());
   if(mask & 1)CHECK(r.size==g.estimates.at("size"));
   if(mask & 2)CHECK(r.prob==g.estimates.at("prob"));
 }
}

TEST_CASE("ztnbinom: P2 held-domain and non-existence statuses") {
 const std::vector<int> x{1,2,5},one{1,1};
 const double nan=std::numeric_limits<double>::quiet_NaN();
 CHECK(cppunivariate::ztnbinom_mle({x.data(),x.size()},1e-10,200,nan,1).status==cppunivariate::ZtnbinomStatus::InvalidInput);
 const auto r=cppunivariate::ztnbinom_mle({one.data(),one.size()},1e-10,200,nan,1);
 CHECK(r.prob==1);CHECK(r.logLik==0);
 CHECK(cppunivariate::ztnbinom_mle({x.data(),x.size()},1e-10,1,nan,.4).status==cppunivariate::ZtnbinomStatus::IterationFailure);
}
