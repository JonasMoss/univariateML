#include "test_support.hpp"
#include "cppunivariate/distributions/zipf/mle.hpp"
#include "cppunivariate/distributions/zipf/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <vector>
namespace dist = cppunivariate::distributions;
#define D_APPROX(x) doctest::Approx(x).epsilon(mltest::kTol)
#define D_APPROX_FD(x) doctest::Approx(x).epsilon(mltest::kFdTol)
namespace {
 std::vector<int> as_ints(const std::vector<double> &v) {
   std::vector<int> out;
   for (double x : v) out.push_back(static_cast<int>(x));
   return out;
 }
 cppunivariate::ispan as_ispan(const std::vector<int> &v) { return {v.data(), v.size()}; }
 }
 TEST_CASE("zipf") {
  auto g = mltest::load_distribution_golden("zipf");
  auto v = as_ints(g.data);
  auto r = cppunivariate::zipf_mle(as_ispan(v), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.N == D_APPROX(g.estimates.at("N")));
  CHECK(r.shape == D_APPROX(g.estimates.at("s")));
  CHECK(r.logLik == D_APPROX(g.logLik));
}

TEST_CASE("rzipf samples lie in {1, ..., N} and match the PMF") {
  cppunivariate::distributions::StdRng<> rng(13);
  const int N = 10;
  const double s = 1.5;
  const auto cum = cppunivariate::distributions::zipf_cdf(N, s);
  const int n = 30000;
  std::vector<int> counts(N, 0);
  for (int i = 0; i < n; ++i) {
    const int k = cppunivariate::distributions::rzipf_from_cdf(rng, cum);
    REQUIRE(k >= 1);
    REQUIRE(k <= N);
    counts[k - 1]++;
  }
  for (int k = 1; k <= N; ++k) {
    const double empirical = static_cast<double>(counts[k - 1]) / n;
    const double analytic =
        cppunivariate::distributions::dzipf(k, N, s, /*log=*/false);
    CHECK(std::abs(empirical - analytic) < 0.02);
  }
}

TEST_CASE("zipf: large harmonic sums match direct sums") {
  for (double s : {0.1,1.0,2.5,8.0}) {
    const auto h = cppunivariate::zipf_detail::moments(10000,s);
    double a=0,b=0,c=0;
    for (int k=1;k<=10000;++k) {
      const double l=std::log(static_cast<double>(k)), w=std::pow(static_cast<double>(k),-s);
      a+=w; b+=w*l; c+=w*l*l;
    }
    for (int q : {1500,5000,9998}) {
      double upper=0;
      for (int k=q+1;k<=10000;++k) upper+=std::pow(static_cast<double>(k),-s);
      CHECK(dist::pzipf(q+.75,10000,s,false)==doctest::Approx(upper/a).epsilon(1e-10));
      CHECK(dist::pzipf(q+.75,10000,s,false,true)==doctest::Approx(std::log(upper/a)).epsilon(1e-11));
    }
    CHECK(h.h == doctest::Approx(a).epsilon(1e-11));
    CHECK(h.l == doctest::Approx(b).epsilon(1e-11));
    CHECK(h.l2 == doctest::Approx(c).epsilon(1e-11));
  }
}

TEST_CASE("zipf finite sums, upper log tails and billion-point inversion") {
  for (int N:{1,3,20}) for (double s:{0.0,.1,1.0,5.0}) {
    double den=0; for (int k=1;k<=N;++k) den+=std::pow(k,-s);
    for (int k=0;k<=N;++k) {
      double below=0,above=0;
      for (int j=1;j<=N;++j) {if (j<=k) below+=std::pow(j,-s); else above+=std::pow(j,-s);}
      CHECK(dist::pzipf(k+.5,N,s)==doctest::Approx(below/den).epsilon(1e-12));
      CHECK(dist::pzipf(k+.5,N,s,false)==doctest::Approx(above/den).epsilon(1e-12));
    }
  }
  CHECK(dist::qzipf(.5,2147483647,0)==1073741824);
  CHECK(dist::pzipf(1e9-1,1e9,100,false,true)==doctest::Approx(dist::dzipf(1e9,1e9,100,true)).epsilon(1e-13));
  for (double u:{1e-10,.1,.5,.9,1-1e-10}) {
    const double q=dist::qzipf(u,1e9,.5);
    CHECK(dist::pzipf(q,1e9,.5)>=u);
    CHECK(dist::pzipf(q-1,1e9,.5)<u);
  }
}
