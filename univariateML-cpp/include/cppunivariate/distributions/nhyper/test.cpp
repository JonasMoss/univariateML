#include "test_support.hpp"
#include "cppunivariate/distributions/nhyper/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <vector>
#include <limits>

TEST_CASE("nhyper: exhaustive small urns and independent likelihood grid") {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  for (int N=1;N<=20;++N) for (int r=1;r<=N;++r)
    for (int x=r;x<=N;++x) {
      const std::vector<double> data{static_cast<double>(x)};
      const auto a = cppunivariate::nhyper_mle(mltest::as_span(data),r,N,nan);
      long double best=-1e100L;
      for (int m=r;m<=N-x+r;++m) {
        const long double ll = std::lgamma(static_cast<long double>(x)) - std::lgamma(static_cast<long double>(r)) - std::lgamma(static_cast<long double>(x-r+1)) +
          std::lgamma(static_cast<long double>(N-x+1)) - std::lgamma(static_cast<long double>(m-r+1)) - std::lgamma(static_cast<long double>(N-m-x+r+1)) -
          std::lgamma(static_cast<long double>(N+1)) + std::lgamma(static_cast<long double>(N-m+1)) + std::lgamma(static_cast<long double>(m+1));
        best=std::max(best,ll);
      }
      CHECK(std::fabs(a.logLik-best)<1e-12);
      CHECK(a.m>=r);
      CHECK(a.m<=N-x+r);
    }
}
TEST_CASE("nhyper: golden and large populations") {
  const auto g=mltest::load_distribution_golden("nhyper");
  const auto a=cppunivariate::nhyper_mle(mltest::as_span(g.data),3,50,std::numeric_limits<double>::quiet_NaN());
  CHECK(a.m==g.estimates.at("m"));
  CHECK(a.logLik==doctest::Approx(g.logLik).epsilon(1e-12));
  CHECK(cppunivariate::nhyper_log_pmf(3,1e12,1e12,3)==doctest::Approx(std::log(.125)).epsilon(1e-11));
}
TEST_CASE("nhyper: scalar tails, quantiles, RNG and invalid inputs") {
  namespace d=cppunivariate::distributions;
  for (double x=2;x<=14;++x) {
    const double p=d::pnhyper(x,8,20,2,true,false);
    CHECK(p+d::pnhyper(x,8,20,2,false,false)==doctest::Approx(1).epsilon(1e-13));
    CHECK(d::qnhyper(p,8,20,2,true,false)==x);
  }
  CHECK(d::qnhyper(-1e-100,8,20,2,true,true)==14);
  CHECK(d::qnhyper(-1e-100,8,20,2,false,true)==2);
  CHECK(d::qnhyper(-1000,8,20,2,true,true)==2);
  CHECK(d::qnhyper(-1000,8,20,2,false,true)==14);
  CHECK(d::dnhyper(2,20,20,2,false)==1);
  CHECK(d::qnhyper(.5,20,20,2,true,false)==2);
  CHECK(d::dnhyper(0,0,0,0,false)==1);
  CHECK(std::isnan(d::dnhyper(0,1,2,2,false)));
  CHECK(std::isnan(d::qnhyper(2,1,2,1,true,false)));
  cppunivariate::distributions::StdRng<> rng(42);
  for (int i=0;i<50;++i) {
    const double v=d::rnhyper(rng,8,20,2);
    CHECK(v>=2); CHECK(v<=14);
  }
}

TEST_CASE("nhyper: statuses, held composition and boundary identification") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> x{3,4,5}, zero{0}, low{1}, two{2,2};
  const auto fit=[&](double r,double N,double m) {
    return cppunivariate::nhyper_mle(mltest::as_span(x),r,N,m);
  };
  CHECK(fit(2,20,8).status==cppunivariate::NhyperStatus::Ok);
  CHECK(fit(2,20,19).status==cppunivariate::NhyperStatus::Infeasible);
  CHECK(fit(2,4,nan).status==cppunivariate::NhyperStatus::Infeasible);
  CHECK(fit(-1,20,nan).status==cppunivariate::NhyperStatus::InvalidInput);
  CHECK(fit(2,1,nan).status==cppunivariate::NhyperStatus::InvalidInput);
  CHECK(fit(2,20,1).status==cppunivariate::NhyperStatus::InvalidInput);
  CHECK(fit(2,20,21).status==cppunivariate::NhyperStatus::InvalidInput);
  CHECK(fit(2,9007199254740992.0,nan).status==cppunivariate::NhyperStatus::InvalidInput);
  CHECK(cppunivariate::nhyper_mle(mltest::as_span(zero),0,20,nan).status==cppunivariate::NhyperStatus::Unidentified);
  CHECK(cppunivariate::nhyper_mle(mltest::as_span(zero),0,0,nan).status==cppunivariate::NhyperStatus::Ok);
  CHECK(cppunivariate::nhyper_mle(mltest::as_span(low),0,20,nan).status==cppunivariate::NhyperStatus::Infeasible);
  CHECK(cppunivariate::nhyper_mle(mltest::as_span(two),2,20,nan).m==20);
  const std::vector<double> six{6};
  const auto tied=cppunivariate::nhyper_mle(mltest::as_span(six),2,20,nan);
  CHECK(tied.status==cppunivariate::NhyperStatus::Tie);
  CHECK(tied.m==6); CHECK(tied.tie==7);
  const auto large=fit(2,9007199254740991.0,nan);
  CHECK(std::isfinite(large.logLik));
  CHECK(large.iterations<60);
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("nhyper: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("nhyper",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dnhyper(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pnhyper(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qnhyper(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rnhyper(rng, p[0], p[1], p[2]); });
}

TEST_CASE("nhyper: population size is returned unchanged as a coefficient") {
  const std::vector<double> x{3,4,5};
  const auto a = cppunivariate::nhyper_mle(mltest::as_span(x),2,20,
      std::numeric_limits<double>::quiet_NaN());
  CHECK(a.N == 20);
  CHECK(a.m >= 2);
  const auto b = cppunivariate::nhyper_mle(mltest::as_span(x),2,20,8);
  CHECK(b.m == 8);
  CHECK(b.N == 20);
}

TEST_CASE("nhyper: README total population validation") {
  cppunivariate::distributions::StdRng<> rng(94);
  for (double N : {3.0, 10.5, double(INFINITY), double(NAN)}) {
    CHECK(std::isnan(cppunivariate::distributions::dnhyper(0.5, 4.0, N, 2.0, true)));
    CHECK(std::isnan(cppunivariate::distributions::pnhyper(0.5, 4.0, N, 2.0, false, true)));
    CHECK(std::isnan(cppunivariate::distributions::qnhyper(0.5, 4.0, N, 2.0, false, true)));
    CHECK(std::isnan(cppunivariate::distributions::rnhyper(rng, 4.0, N, 2.0)));
  }
}

TEST_CASE("nhyper: free stopping count agrees with a complete finite grid") {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  for (int N = 1; N <= 20; ++N) for (int m = 1; m <= N; ++m)
    for (int x = 1; x <= N; ++x) {
      const std::vector<double> data{double(x), double(x)};
      const auto fit = cppunivariate::nhyper_mle(mltest::as_span(data), nan, N, m);
      double best = -std::numeric_limits<double>::infinity();
      for (int r = std::max(1, x - (N - m)); r <= std::min(m, x); ++r)
        best = std::max(best, 2 * cppunivariate::nhyper_log_pmf(x, N - m, m, r));
      CHECK(fit.m == m);
      CHECK(fit.N == N);
      CHECK(std::fabs(fit.logLik - best) < 1e-11);
    }
}

TEST_CASE("nhyper: all three new finite grids versus inline enumeration") {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  for (int a=1; a<=6; ++a) for (int b=a; b<=8; ++b) {
    const std::vector<double> x{double(a),double(b),double(b)};
    for (int mode=0; mode<3; ++mode) {
      const int held_m=7, held_N=12, held_r=1;
      const auto f=cppunivariate::nhyper_mle(mltest::as_span(x),mode==2?held_r:nan,mode==0?held_N:nan,mode==0?nan:held_m);
      long double best=-1e100L;
      for (int r=1;r<=a;++r) {
        if(mode==2 && r!=held_r) continue;
        for(int m=r;m<= (mode==0?held_N:held_m);++m) {
          if(mode!=0 && m!=held_m) continue;
          const int lo=mode==0?held_N:m+b-r;
          const int hi=mode==0?held_N:std::max(lo,m*b/r);
          for(int N=lo;N<=hi;++N) {
            if(m>N-b+r) continue;
            long double ll=0;
            for(double v:x) ll+=std::lgamma(v)-std::lgamma(double(r))-std::lgamma(v-r+1)+std::lgamma(double(N)-v+1)-std::lgamma(double(m-r+1))-std::lgamma(double(N-m)-v+r+1)-std::lgamma(double(N+1))+std::lgamma(double(N-m+1))+std::lgamma(double(m+1));
            best=std::max(best,ll);
          }
        }
      }
      CHECK(f.status==cppunivariate::NhyperStatus::Ok);
      CHECK(std::fabs(f.logLik-best)<1e-10);
      CHECK(f.m==std::floor(f.m)); CHECK(f.N==std::floor(f.N)); CHECK(f.r==std::floor(f.r));
    }
  }
  const std::vector<double> x{1,100}, mixed{0,1}, zero{0,0};
  CHECK(cppunivariate::nhyper_mle(mltest::as_span(x),1,nan,1e7).status==cppunivariate::NhyperStatus::NumericalFailure);
  CHECK(cppunivariate::nhyper_mle(mltest::as_span(mixed),nan,nan,7).status==cppunivariate::NhyperStatus::Infeasible);
  const auto z=cppunivariate::nhyper_mle(mltest::as_span(zero),nan,nan,7);
  CHECK(z.N==7); CHECK(z.r==0); CHECK(z.logLik==0);
}

// The third of five successes in ten draws is a symmetric order statistic.
// Reversing the urn sends T to 11-T, so F(5)=1/2 exactly.
TEST_CASE("nhyper: symmetric lattice median allows adjacent log-half answers") {
  const auto adjacent=[](double x,double lo) {return x==lo || x==lo+1;};
  CHECK(cppunivariate::distributions::qnhyper(.31,5,10,3,true,false)==5);
  CHECK(cppunivariate::distributions::qnhyper(.31,5,10,3,false,false)==6);

  CHECK(adjacent(cppunivariate::distributions::qnhyper(.5,5,10,3,true,false),5));
  CHECK(adjacent(cppunivariate::distributions::qnhyper(.5,5,10,3,false,false),5));
  CHECK(adjacent(cppunivariate::distributions::qnhyper(-0.6931471805599453,5,10,3,true,true),5));
  CHECK(adjacent(cppunivariate::distributions::qnhyper(-0.6931471805599453,5,10,3,false,true),5));
}

TEST_CASE("nhyper: certified joint population profiles and unresolved face") {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> x{3,3,3,4,4,4};
  for (double rr : {2.0, nan}) {
    const auto f = cppunivariate::nhyper_mle(mltest::as_span(x),rr,nan,nan);
    REQUIRE(f.status == cppunivariate::NhyperStatus::Ok);
    double best = -INFINITY;
    for (int N=4; N<=300; ++N) for (int r=1; r<=3; ++r) {
      if (!std::isnan(rr) && r!=rr) continue;
      for (int m=r; m<=N-4+r; ++m) {
        double ll=0; for (double v:x) ll+=cppunivariate::nhyper_log_pmf(v,N-m,m,r);
        best=std::max(best,ll);
      }
    }
    CHECK(f.logLik == doctest::Approx(best).epsilon(1e-10));
  }
  const std::vector<double> tie{2,2};
  CHECK(cppunivariate::nhyper_mle(mltest::as_span(tie),2,nan,nan).status == cppunivariate::NhyperStatus::Ok);
  const std::vector<double> spread{1,1,1,1,1,30};
  CHECK(cppunivariate::nhyper_mle(mltest::as_span(spread),1,nan,nan).status == cppunivariate::NhyperStatus::FaceUnresolved);
  CHECK(cppunivariate::nhyper_mle(mltest::as_span(spread),nan,nan,nan).status == cppunivariate::NhyperStatus::FaceUnresolved);
  const std::vector<double> large{1,100001};
  CHECK(cppunivariate::nhyper_mle(mltest::as_span(large),1,nan,nan).status == cppunivariate::NhyperStatus::NumericalFailure);
}
