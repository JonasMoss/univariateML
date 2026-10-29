#include "test_support.hpp"
#include "cppunivariate/distributions/zinb/mle.hpp"
#include "cppunivariate/distributions/zinb/dpqr.hpp"
#include "cppunivariate/distributions/zip/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <vector>

TEST_CASE("zinb: golden and independent natural parameter scores") {
  const auto g = mltest::load_distribution_golden("zinb");
  std::vector<int> x;
  for (double v : g.data) x.push_back(static_cast<int>(v));
  const auto fit = cppunivariate::zinb_mle({x.data(), x.size()});
  REQUIRE(fit.status == cppunivariate::ZinbStatus::Ok);
  CHECK(fit.size == doctest::Approx(g.estimates.at("size")).epsilon(1e-10));
  CHECK(fit.prob == doctest::Approx(g.estimates.at("prob")).epsilon(1e-10));
  CHECK(fit.pi == doctest::Approx(g.estimates.at("pi")).epsilon(1e-10));
  CHECK(fit.logLik == doctest::Approx(g.logLik).epsilon(1e-10));
  auto ll = [&](const std::vector<double> &p) {
    double out = 0;
    for (int v : x) out += cppunivariate::distributions::dzinb(v,p[0],p[1],p[2],true);
    return out;
  };
  std::vector<double> p{fit.size, fit.prob, fit.pi};
  double norm2 = 0;
  for (int j = 0; j < 3; ++j) {
    auto a=p,b=p; const double h=1e-5;
    a[j]+=h; b[j]-=h;
    const double score=(ll(a)-ll(b))/(2*h); norm2+=score*score;
  }
  CHECK(std::sqrt(norm2) < 1e-3);
  const auto fixed = cppunivariate::zinb_mle({x.data(),x.size()},1e-10,200,2.5);
  CHECK(fixed.status == cppunivariate::ZinbStatus::Ok);
  CHECK(fixed.size == 2.5);
}

TEST_CASE("zinb: generated face and attained NB fits") {
  cppunivariate::distributions::StdRng<> rng(123), poisson_rng(5);
  std::vector<int> zip, pois, nb;
  for (int i=0;i<5000;++i) {
    zip.push_back(static_cast<int>(cppunivariate::distributions::rzip(rng,4,.4)));
    pois.push_back(static_cast<int>(poisson_rng.rpois(4)));
    nb.push_back(static_cast<int>(cppunivariate::distributions::rnbinom(rng,2,.4)));
  }
  CHECK(cppunivariate::zinb_mle({zip.data(),zip.size()}).status == cppunivariate::ZinbStatus::ZipLimit);
  CHECK(cppunivariate::zinb_mle({pois.data(),pois.size()}).status == cppunivariate::ZinbStatus::PoissonLimit);
  const auto fit = cppunivariate::zinb_mle({nb.data(),nb.size()});
  CHECK(fit.status == cppunivariate::ZinbStatus::Ok);
  CHECK(fit.pi == 0);
  CHECK(cppunivariate::zinb_mle({pois.data(),pois.size()},1e-10,200,2).status == cppunivariate::ZinbStatus::Ok);
}

TEST_CASE("zinb: held positive-size degenerate representative and stable density") {
  const std::vector<int> x{0,0,0};
  // The size-zero d/p/q/r seam belongs to TASK-78. Retain coverage of
  // the held positive-size representative here; P3 checks size zero below.
  const auto fit=cppunivariate::zinb_mle({x.data(),x.size()},1e-10,200,1);
  CHECK(fit.logLik == 0);
  CHECK(cppunivariate::distributions::dzinb(0,fit.size,fit.prob,fit.pi,true)==0);
  CHECK(cppunivariate::distributions::pzinb(0,fit.size,fit.prob,fit.pi,true,false)==1);
  CHECK(cppunivariate::distributions::qzinb(1,fit.size,fit.prob,fit.pi,true,false)==0);
  CHECK(std::isfinite(cppunivariate::distributions::dzinb(10000,2,.5,.1,true)));
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("zinb: d/p/q/r against the truth table" * doctest::may_fail()) {
  mltest::check_dpqr_truth("zinb",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dzinb(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pzinb(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qzinb(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rzinb(rng, p[0], p[1], p[2]); });
}

TEST_CASE("zinb: P2 every hold subset preserves coefficients and golden likelihood") {
 const auto g=mltest::load_distribution_golden("zinb");
 std::vector<int> x;for(double v:g.data)x.push_back(static_cast<int>(v));
 const double nan=std::numeric_limits<double>::quiet_NaN();
 for(unsigned mask=1;mask<8;++mask) {
   const auto r=cppunivariate::zinb_mle({x.data(),x.size()},1e-10,200,(mask & 1)?g.estimates.at("size"):nan,(mask & 2)?g.estimates.at("prob"):nan,(mask & 4)?g.estimates.at("pi"):nan);
   REQUIRE(std::isfinite(r.logLik));
   CHECK(r.logLik>=g.logLik-1e-6*x.size());
   if(mask & 1)CHECK(r.size==g.estimates.at("size"));
   if(mask & 2)CHECK(r.prob==g.estimates.at("prob"));
   if(mask & 4)CHECK(r.pi==g.estimates.at("pi"));
 }
}

TEST_CASE("zinb: P2 held-domain and non-existence statuses") {
 const std::vector<int> x{0,1,5},z{0,0};
 const double nan=std::numeric_limits<double>::quiet_NaN();
 CHECK(cppunivariate::zinb_mle({z.data(),z.size()},1e-10,200,nan,.4,.3).status==cppunivariate::ZinbStatus::Ok);
 CHECK(cppunivariate::zinb_mle({x.data(),x.size()},1e-10,200,nan,1,nan).status==cppunivariate::ZinbStatus::InvalidInput);
 CHECK(cppunivariate::zinb_mle({x.data(),x.size()},1e-10,200,nan,nan,1).status==cppunivariate::ZinbStatus::InvalidInput);
 CHECK(cppunivariate::zinb_mle({x.data(),x.size()},1e-10,1,nan,.4,nan).status==cppunivariate::ZinbStatus::IterationFailure);
}

TEST_CASE("zinb: P3 attained size zero preserves holds") {
 const std::vector<int> z{0,0,0};
 const auto r=cppunivariate::zinb_mle({z.data(),z.size()},1e-10,200,0,.4,.2);
 CHECK(r.status==cppunivariate::ZinbStatus::Ok);
 CHECK(r.size==0); CHECK(r.prob==.4); CHECK(r.logLik==0);
}
