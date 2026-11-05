#include "test_support.hpp"
#include "cppunivariate/distributions/frechet/dpqr.hpp"
#include "cppunivariate/distributions/frechet/mle.hpp"
TEST_CASE("frechet: held location and free location restrictions") {
 std::vector<double> x{.2,.4,.7,1,2,4,10};
 auto r=cppunivariate::frechet_mle(cppunivariate::span{x.data(),x.size()},1e-10,100,0);
 auto b=cppunivariate::invweibull_mle(cppunivariate::span{x.data(),x.size()},1e-10,100);
 CHECK(r.lambda == doctest::Approx(b.shape));
 CHECK(r.sigma == doctest::Approx(b.scale));
 CHECK(r.logLik == doctest::Approx(b.logLik));
 auto f=cppunivariate::frechet_mle(cppunivariate::span{x.data(),x.size()});
 CHECK(f.status == cppunivariate::FrechetStatus::Ok);
 CHECK(f.lambda>=1);
 std::vector<double> ties{1,1,1,2};
 CHECK(cppunivariate::frechet_mle(cppunivariate::span{ties.data(),ties.size()}).status == cppunivariate::FrechetStatus::InvalidData);
}

TEST_CASE("frechet: scalar density parameter mapping") {
 CHECK(cppunivariate::distributions::dfrechet(1,2,0,1,false) == doctest::Approx(2/std::exp(1.)));
}

#include "cppunivariate/distributions/frechet/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
TEST_CASE("frechet: probability endpoints and sampler CDF") {
  CHECK(cppunivariate::distributions::qfrechet(0,2,0,1,true,false)==0);
  CHECK(std::isinf(cppunivariate::distributions::qfrechet(0,2,0,1,false,false))==true);
  CHECK(cppunivariate::distributions::pfrechet(-1,2,0,1,true,false)==0);
  CHECK(cppunivariate::distributions::pfrechet(std::numeric_limits<double>::infinity(),2,0,1,true,false)==1);
  CHECK(std::isnan(cppunivariate::distributions::qfrechet(.1,2,0,1,true,true)));
  cppunivariate::distributions::StdRng<> rng(7813);
  std::vector<double> samples;
  for(int i=0;i<20000;++i) samples.push_back(cppunivariate::distributions::rfrechet(rng,2,0,1));
  for(double point:{.5,1.,2.}) {
    int count=0;for(double x:samples) if(x<=point) ++count;
    const double prob=cppunivariate::distributions::pfrechet(point,2,0,1,true,false);
    CHECK(std::abs(count/20000.-prob)<=5*std::sqrt(prob*(1-prob)/20000.));
  }
}
// from mpmath import mp; mp.dps=100; float(mp.log(2)-3*mp.log(mp.mpf(".01"))-10000)
TEST_CASE("frechet: independent extreme value") {
 CHECK(cppunivariate::distributions::dfrechet(.01,2,0,1,true)==doctest::Approx(-9985.4913422614754).epsilon(2e-13));
}

TEST_CASE("frechet: ties above the minimum are admissible") {
  const std::vector<double> x{0,1,2,2,2};
  const auto r=cppunivariate::frechet_gev_mle(mltest::as_span(x),1e-10,100,.3);
  CHECK(r.status==cppunivariate::GevStatus::Ok);
}

TEST_CASE("frechet: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("frechet",
      [](double x,const std::vector<double>& p,bool log) { return cppunivariate::distributions::dfrechet(x,p[0],p[1],p[2],log); },
      [](double x,const std::vector<double>& p,bool lower,bool log) { return cppunivariate::distributions::pfrechet(x,p[0],p[1],p[2],lower,log); },
      [](double x,const std::vector<double>& p,bool lower,bool log) { return cppunivariate::distributions::qfrechet(x,p[0],p[1],p[2],lower,log); },
      [](cppunivariate::distributions::StdRng<>& rng,const std::vector<double>& p) { return cppunivariate::distributions::rfrechet(rng,p[0],p[1],p[2]); });
}

// mpmath reproduction: import mpmath as m; m.mp.dps=100
// F=lambda x:m.exp(-1/x**2); Q=lambda l:1/m.sqrt(-l)
TEST_CASE("frechet: extreme tail regressions") {
  CHECK(cppunivariate::distributions::pfrechet(1e300,2,0,1,false,true) == doctest::Approx(-2*std::log(1e300)).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::qfrechet(-10000,2,0,1,true,true) == doctest::Approx(0.01).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::qfrechet(-INFINITY,2,3,1,true,true) == doctest::Approx(3).epsilon(1e-12).scale(1e-300));
}


TEST_CASE("frechet: all new holds and exceptional constant likelihoods") {
 const double nan=std::numeric_limits<double>::quiet_NaN();
 const std::vector<double> x{.2,.4,.7,1,2,4,10};const auto data=mltest::as_span(x);
 for(int mask:{1,4,3,5,6,7}) {
   const double a=mask&1 ? 2 : nan,m=mask&2 ? 0 : nan,s=mask&4 ? 1 : nan;
   const auto f=cppunivariate::frechet_mle(data,1e-10,150,m,a,s);
   CAPTURE(mask);
   REQUIRE(f.status==cppunivariate::FrechetStatus::Ok);
   if(mask&1) CHECK(f.lambda==a);
   if(mask&2) CHECK(f.mu==m);
   if(mask&4) CHECK(f.sigma==s);
   for(int j=0;j<3;++j) if(!(mask&(1<<j))) {
     if(j==0 && f.lambda==1) continue;
     double p[3]={f.lambda,f.mu,f.sigma};const double h=1e-5;
     p[j]+=h;const double plus=cppunivariate::frechet_loglik(data,p[0],p[1],p[2]);
     p[j]-=2*h;const double minus=cppunivariate::frechet_loglik(data,p[0],p[1],p[2]);
     CHECK(std::abs((plus-minus)/(2*h))<1e-3);
   }
 }
 const std::vector<double> one{2};const auto d=mltest::as_span(one);
 CHECK(cppunivariate::frechet_mle(d,1e-10,100,0,nan,2).status==cppunivariate::FrechetStatus::InvalidData);
 CHECK(cppunivariate::frechet_mle(d,1e-10,100,0,nan,1).status==cppunivariate::FrechetStatus::Ok);
 const auto f=cppunivariate::frechet_mle(d,1e-10,100,0,.5);
 CHECK(f.status==cppunivariate::FrechetStatus::Ok);CHECK(f.sigma==2);
 CHECK(cppunivariate::frechet_mle(d,1e-10,100,nan,.5,1).status==cppunivariate::FrechetStatus::Ok);
 CHECK(cppunivariate::frechet_mle(d,1e-10,100,nan,nan,1).status==cppunivariate::FrechetStatus::InvalidData);
 CHECK(cppunivariate::frechet_mle(data,1e-10,1,0,nan,1).status==cppunivariate::FrechetStatus::IterationFailure);
 const std::vector<double> ties{1,1,2,3};
 CHECK(cppunivariate::frechet_mle(mltest::as_span(ties),1e-10,100,nan,1).status==cppunivariate::FrechetStatus::InvalidData);
}

TEST_CASE("frechet: tiny held shape retains the exact geometric-mean scale limit") {
 const std::vector<double> x{1,4};
 const auto f=cppunivariate::frechet_mle(mltest::as_span(x),1e-10,100,0,1e-20);
 CHECK(f.status==cppunivariate::FrechetStatus::Ok);
 CHECK(f.lambda==1e-20);
 CHECK(f.sigma==doctest::Approx(2).epsilon(1e-14));
}

TEST_CASE("frechet: P3 restrictions and closed faces") {
 const std::vector<double> x{.01,.02,.1,1,10,100,1000};
 const auto f=cppunivariate::frechet_mle(mltest::as_span(x),1e-10,200,NAN,NAN,1);
 REQUIRE(f.status==cppunivariate::FrechetStatus::Ok);CHECK(f.lambda==1);CHECK(f.restricted==1u);
 const auto h=cppunivariate::frechet_mle(mltest::as_span(x),1e-10,200,0,1,1);CHECK(h.restricted==0u);
}

// TASK-85/87: with mu, lambda and sigma all free, a minimum tied
// exactly at n/2 used to be rejected outright, conflated with a strict
// majority tie. Mirrors gev/test.cpp's identical fix and its finding:
// the lambda->1 collapse face has a finite but unattained supremum
// (A_free, same closed form as GEV's since Frechet's log-likelihood at
// a point is the GEV log-likelihood at the corresponding reparametrized
// point); in 3000+ random trials for this report, no interior point
// ever beat it, so the check below accepts either outcome rather than
// asserting one.
TEST_CASE("frechet: free fit compares half-minimum ties with the free collapse face") {
  for (auto x : {std::vector<double>{1,1,2,3}, std::vector<double>{0,0,.01,100},
                 std::vector<double>{1,1,1,2,5,20}}) {
    const double m = *std::min_element(x.begin(), x.end());
    double sum_log_d = 0;
    for (double v : x) if (v > m) sum_log_d += std::log(v - m);
    const double a_free = x.size()*std::log(2.0) - static_cast<double>(x.size()) - 2.0*sum_log_d;
    const auto f = cppunivariate::frechet_mle(mltest::as_span(x));
    CAPTURE(m);
    if (f.status == cppunivariate::FrechetStatus::Ok) {
      CHECK(f.logLik > a_free);
      double norm = 0, h = 1e-5;
      for (int j = 0; j < 3; ++j) {
        double p[3] = {f.lambda, f.mu, f.sigma}; p[j] += h;
        const double plus = cppunivariate::frechet_loglik(mltest::as_span(x), p[0], p[1], p[2]);
        p[j] -= 2*h;
        const double minus = cppunivariate::frechet_loglik(mltest::as_span(x), p[0], p[1], p[2]);
        const double score = (plus - minus) / (2*h);
        norm += score*score;
      }
      CHECK(std::sqrt(norm) < 1e-3);
    } else {
      CHECK(f.status == cppunivariate::FrechetStatus::NoMLE);
    }
  }
  // A strict majority still diverges outright, unaffected by this fix.
  const std::vector<double> majority{1,1,1,2};
  CHECK(cppunivariate::frechet_mle(mltest::as_span(majority)).status == cppunivariate::FrechetStatus::InvalidData);
}

TEST_CASE("Frechet held location subsets inherit inverse Weibull") {
 const std::vector<double> x{2,3,4,7,12},y{1,2,3,6,11};
 for(double shape:{double(NAN),2.})for(double scale:{double(NAN),3.}) {
  const auto r=cppunivariate::frechet_mle(mltest::as_span(x),1e-10,1000,1,shape,scale);
  const auto p=cppunivariate::invweibull_mle(mltest::as_span(y),1e-10,1000,NAN,shape,scale);
  CHECK(r.status==cppunivariate::FrechetStatus::Ok);
  CHECK(r.logLik==doctest::Approx(p.logLik));CHECK(r.lambda==doctest::Approx(p.shape));
  CHECK(r.sigma==doctest::Approx(p.scale));
 }
}
