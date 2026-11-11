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

TEST_CASE("frechet: fixed-shape certificates cover profiled scale and monotone location") {
 const std::vector<double> x{0,.2,.5,1,3,20};const auto d=mltest::as_span(x);
 for(double a:{.5,1.,2.,10.})for(double scale:{double(NAN),.7}) {
   const auto f=cppunivariate::frechet_mle(d,1e-11,1000,NAN,a,scale);
   CAPTURE(a);CAPTURE(scale);
   REQUIRE(f.status==cppunivariate::FrechetStatus::Ok);
   // Independent dense feasible competitors, including the old search edges.
   for(int j=-400;j<=400;++j) {
     const double gap=std::exp(j*.1);
     const double s=std::isnan(scale)?cppunivariate::frechet_scale(d,a,-gap):scale;
     CHECK(cppunivariate::frechet_loglik(d,a,-gap,s)<=f.logLik+1e-8);
   }
   std::vector<double> y;for(double v:x)y.push_back(3+v*1e50);
   const auto g=cppunivariate::frechet_mle(mltest::as_span(y),1e-11,1000,NAN,a,std::isnan(scale)?NAN:scale*1e50);
   REQUIRE(g.status==cppunivariate::FrechetStatus::Ok);
   CHECK(g.mu/1e50==doctest::Approx(f.mu).epsilon(1e-8));
   CHECK(g.sigma/1e50==doctest::Approx(f.sigma).epsilon(1e-8));
 }
 const auto b=cppunivariate::frechet_mle(d,1e-10,1,NAN,2);
 CHECK(b.status==cppunivariate::FrechetStatus::IterationFailure);
 CHECK_FALSE(b.stop_detail.budget_exhausted);
 REQUIRE(!b.stop_detail.candidates.empty());
 CHECK(std::isinf(b.stop_detail.loglik_upper));
 const std::vector<double> ties{0,0,1,2};
 const auto c=cppunivariate::frechet_mle(mltest::as_span(ties),1e-10,100,NAN,1);
 CHECK(c.status==cppunivariate::FrechetStatus::InvalidData);
 CHECK(c.stop_detail.loglik_upper==doctest::Approx(4*std::log(2.)-4-2*std::log(2.)));
}

TEST_CASE("frechet: near-collapse exponent is evaluated without a rounded false equality") {
 const std::vector<double> x{0,1,2,3};
 const double a=1./3.;
 // This double lies below the exact rational boundary; its ordinary
 // product by 3 rounds to 1, whereas fma retains the negative exponent.
 REQUIRE(std::fma(a,3.,-1.)<0);
 const auto f=cppunivariate::frechet_mle(mltest::as_span(x),1e-10,100,NAN,a);
 CHECK(f.status==cppunivariate::FrechetStatus::InvalidData);
 CHECK(std::isinf(f.stop_detail.loglik_upper));
}

TEST_CASE("frechet: the double above the collapse ratio is not classified as nonexistence") {
 const std::vector<double> x{0,1,2,3};
 const double a=std::nextafter(1./3.,1.);
 REQUIRE(std::fma(a,3.,-1.)>0);
 const auto f=cppunivariate::frechet_mle(mltest::as_span(x),1e-10,100,NAN,a);
 CHECK(f.status!=cppunivariate::FrechetStatus::InvalidData);
 CHECK(f.status!=cppunivariate::FrechetStatus::NoMLE);
}

TEST_CASE("frechet: unrepresentable stopped witnesses are removed") {
 const std::vector<double> x{-1e308,-.9e308,0};
 const auto f=cppunivariate::frechet_mle(mltest::as_span(x),1e-10,1,NAN,2);
 CHECK(f.status==cppunivariate::FrechetStatus::NumericalFailure);
 CHECK(f.stop_detail.candidates.empty());
}

TEST_CASE("frechet: finite collapse bound survives an overflowing observation range") {
 const std::vector<double> x{-1e308,1e308};
 const auto f=cppunivariate::frechet_mle(mltest::as_span(x),1e-10,100,NAN,1);
 CHECK(f.status==cppunivariate::FrechetStatus::InvalidData);
 CHECK(std::isfinite(f.stop_detail.loglik_upper));
 CHECK(f.stop_detail.loglik_upper==doctest::Approx(-2-2*std::log(1e308)).epsilon(1e-12));
}

TEST_CASE("frechet: overflowing positive-exponent normalization is numerical, not nonexistence") {
 const std::vector<double> x{-1e308,1e308};
 const auto f=cppunivariate::frechet_mle(mltest::as_span(x),1e-10,100,NAN,2);
 CHECK(f.status==cppunivariate::FrechetStatus::NumericalFailure);
}

TEST_CASE("frechet: enormous held powers do not return a rounded-log point fit") {
 const std::vector<double> x{0,.2,.5,1,3,20};
 const auto f=cppunivariate::frechet_mle(mltest::as_span(x),1e-10,100,NAN,1e100);
 CHECK(f.status==cppunivariate::FrechetStatus::NumericalFailure);
}

TEST_CASE("frechet: half-minimum pair bound is the global unattained supremum") {
 for(auto x:{std::vector<double>{0,0,1,2},std::vector<double>{0,0,2,2},std::vector<double>{-1e308,-1e308,0,1e308}}) {
   const auto data=mltest::as_span(x);const auto f=cppunivariate::frechet_mle(data);
   REQUIRE(f.status==cppunivariate::FrechetStatus::NoMLE);
   CHECK(std::isfinite(f.stop_detail.loglik_upper));
   CHECK(f.stop_detail.loglik_lower==f.stop_detail.loglik_upper);
   if(x.back()<1e300) {
     for(double a:{1.,1.1,2.,10.})for(double gap:{1e-5,.1,1.,100.}) {
       const double m=x.front()-gap,s=cppunivariate::frechet_scale(data,a,m);
       CHECK(cppunivariate::frechet_loglik(data,a,m,s)<f.stop_detail.loglik_upper);
     }
   }
 }
}

TEST_CASE("frechet: all-free two-point convex-shape certificate") {
 for(int k:{1,4,8,9}) {
   std::vector<double> x(20,1);std::fill_n(x.begin(),k,0.);
   const auto data=mltest::as_span(x);
   const auto f=cppunivariate::frechet_mle(data);
   CAPTURE(k);
   if(k<9) {
     REQUIRE(f.status==cppunivariate::FrechetStatus::GumbelLimit);
     REQUIRE(f.stop_detail.limit_coef.size()==2);
     CHECK(f.stop_detail.loglik_upper-f.stop_detail.loglik_lower<1e-7);
     const double mu=f.stop_detail.limit_coef[0],sigma=f.stop_detail.limit_coef[1];
     CHECK(cppunivariate::gev_loglik(data,mu,sigma,0)==doctest::Approx(f.stop_detail.limit_loglik).epsilon(1e-12));
     for(double a:{10.,100.,1000.}) {
       const double ll=cppunivariate::frechet_loglik(data,a,mu-a*sigma,a*sigma);
       CHECK(ll<f.stop_detail.loglik_upper);
     }
   } else {
     REQUIRE(f.status==cppunivariate::FrechetStatus::Ok);
     CHECK(f.lambda==1);CHECK(f.restricted==1u);
     const double p=k/20.,h=p*(1-2*p)/(std::sqrt(p*(1-p))+p);
     CHECK(f.mu==doctest::Approx(-h).epsilon(1e-12));
     CHECK(f.sigma==doctest::Approx(h*(1+h)/(p+h)).epsilon(1e-12));
   }
   const double upper=f.status==cppunivariate::FrechetStatus::Ok?f.logLik+1e-8:f.stop_detail.loglik_upper;
   for(double a:{1.,1.01,1.2,2.,5.,20.,100.})for(int j=-100;j<=100;++j) {
     const double m=-std::exp(j*.1),s=cppunivariate::frechet_scale(data,a,m);
     CHECK(cppunivariate::frechet_loglik(data,a,m,s)<=upper);
   }
 }
}

TEST_CASE("frechet: two-point certificate preserves precision and solve stops") {
 std::vector<double> x(20,1);std::fill_n(x.begin(),9,0.);
 CHECK(cppunivariate::frechet_mle(mltest::as_span(x),1e-15).status==cppunivariate::FrechetStatus::NumericalFailure);
 std::fill_n(x.begin(),9,1.);x[0]=0;
 const auto low=cppunivariate::frechet_mle(mltest::as_span(x),1e-10,1);
 CHECK(low.status==cppunivariate::FrechetStatus::IterationFailure);
 CHECK_FALSE(low.stop_detail.budget_exhausted);
 CHECK(std::isinf(low.stop_detail.loglik_upper));
 for(double scale:{1e-100,1.,1e100}) {
   std::vector<double> y(20,3*scale);std::fill_n(y.begin(),9,2*scale);
   const auto f=cppunivariate::frechet_mle(mltest::as_span(y));
   REQUIRE(f.status==cppunivariate::FrechetStatus::Ok);
   CHECK(f.lambda==1);CHECK(f.mu>scale);CHECK(f.sigma>0);
 }
 // Constraints not containing the unconstrained winner use their own route.
 std::vector<double> y(20,1);std::fill_n(y.begin(),9,0.);
 const auto held=cppunivariate::frechet_mle(mltest::as_span(y),1e-10,100,-1);
 REQUIRE(held.status==cppunivariate::FrechetStatus::Ok);CHECK(held.mu== -1);
}

TEST_CASE("frechet: exact public scale hold preserves the certified two-point winner") {
 std::vector<double> x(16,1);std::fill_n(x.begin(),7,0.);
 const auto f=cppunivariate::frechet_mle(mltest::as_span(x),1e-10,100,NAN,NAN,.125);
 REQUIRE(f.status==cppunivariate::FrechetStatus::Ok);
 CHECK(f.sigma==.125);CHECK(f.lambda==1);CHECK(f.restricted==1u);
}

TEST_CASE("frechet: two-point Gumbel witness lost on translation stops numerically") {
 const double m=1e300,M=std::nextafter(m,INFINITY);
 std::vector<double> x(20,M);x[0]=m;
 const auto f=cppunivariate::frechet_mle(mltest::as_span(x));
 CHECK(f.status==cppunivariate::FrechetStatus::NumericalFailure);
 CHECK(f.stop_detail.limit_coef.empty());
}

TEST_CASE("frechet: two-point solver cap retains a finite global upper bound") {
 std::vector<double> x(20,1);x[0]=0;
 const auto f=cppunivariate::frechet_mle(mltest::as_span(x),1e-10,10);
 CHECK(f.status==cppunivariate::FrechetStatus::IterationFailure);
 CHECK(std::isfinite(f.stop_detail.loglik_upper));
 CHECK_FALSE(f.stop_detail.candidates.empty());
}

TEST_CASE("frechet: tight Gumbel root precision is numerical rather than a face tie") {
 std::vector<double> x(20,1);x[0]=0;
 const auto f=cppunivariate::frechet_mle(mltest::as_span(x),1e-15,100);
 CHECK(f.status==cppunivariate::FrechetStatus::NumericalFailure);
 CHECK_FALSE(f.stop_detail.comparison_tie);
}

TEST_CASE("frechet: compatible hold failures preserve only feasible lower witnesses") {
 std::vector<double> x(16,1);std::fill_n(x.begin(),7,0.);
 const auto tight=cppunivariate::frechet_mle(mltest::as_span(x),1e-15,100,NAN,NAN,.125);
 CHECK(tight.status==cppunivariate::FrechetStatus::NumericalFailure);
 x.assign(16,1);x[0]=0;
 const auto low=cppunivariate::frechet_mle(mltest::as_span(x),1e-10,10,NAN,NAN,.875);
 CHECK(low.status==cppunivariate::FrechetStatus::IterationFailure);
 REQUIRE(low.stop_detail.candidates.size()==1);
 CHECK(low.stop_detail.candidates[0].coef[2]==.875);
 CHECK(low.stop_detail.loglik_lower==low.stop_detail.candidates[0].loglik_lower);
 CHECK_FALSE(low.stop_detail.comparison_tie);
}

TEST_CASE("frechet: general all-free cover checks public coefficients and proper Gumbel face") {
 const auto g=mltest::load_distribution_golden("frechet");
 const auto fit=cppunivariate::frechet_mle(mltest::as_span(g.data));
 REQUIRE(fit.status==cppunivariate::FrechetStatus::Ok);
 CHECK(fit.iterations>0);
 CHECK(fit.iterations<=cppunivariate::certificate_budget(100));
 CHECK(fit.lambda==doctest::Approx(g.estimates.at("lambda")).epsilon(mltest::kGolden_frechet).scale(0));
 CHECK(fit.mu==doctest::Approx(g.estimates.at("mu")).epsilon(mltest::kGolden_frechet).scale(0));
 CHECK(fit.sigma==doctest::Approx(g.estimates.at("sigma")).epsilon(mltest::kGolden_frechet).scale(0));
 std::vector<double> x;
 for(int j=0;j<80;++j)x.push_back(-std::log(-std::log((j+.5)/80)));
 const auto face=cppunivariate::frechet_mle(mltest::as_span(x));
 REQUIRE(face.status==cppunivariate::FrechetStatus::GumbelLimit);
 REQUIRE(face.stop_detail.limit_coef.size()==2);
 CHECK(face.stop_detail.limit_coef[1]>0);
 CHECK(std::isfinite(face.stop_detail.loglik_lower));
 CHECK(std::isfinite(face.stop_detail.loglik_upper));
 CHECK(face.stop_detail.loglik_lower<=face.stop_detail.loglik_upper);
 CHECK(face.stop_detail.limit_loglik==doctest::Approx(cppunivariate::gev_loglik(mltest::as_span(x),face.stop_detail.limit_coef[0],face.stop_detail.limit_coef[1],0)).epsilon(1e-12));
 const auto stopped=cppunivariate::frechet_mle(mltest::as_span(g.data),1e-16,100);
 CHECK(stopped.status!=cppunivariate::FrechetStatus::Ok);
 CHECK_FALSE(stopped.stop_detail.comparison_tie);
 for(const auto& c:stopped.stop_detail.candidates) {
   REQUIRE(c.coef.size()==3);
   CHECK(c.coef[0]>=1);CHECK(c.coef[2]>0);
   CHECK(c.loglik_lower==doctest::Approx(cppunivariate::frechet_loglik(mltest::as_span(g.data),c.coef[0],c.coef[1],c.coef[2])).epsilon(1e-10));
 }
}

TEST_CASE("frechet: held-location delegate failures preserve numerical status") {
  const std::vector<double> data{1,2,4};
  const auto fit=cppunivariate::frechet_mle(mltest::as_span(data),1e-10,1,0);
  CHECK(fit.status==cppunivariate::FrechetStatus::IterationFailure);
}

TEST_CASE("frechet: held-location delegate preserves the uncertified restoration flag") {
  const double eps=std::numeric_limits<double>::epsilon();
  const std::vector<double> data{1,1+eps,1+2*eps,1+3*eps,1+4*eps};
  const auto fit=cppunivariate::frechet_mle(mltest::as_span(data),1.220703e-4,100,0);
  REQUIRE(fit.status==cppunivariate::FrechetStatus::Ok);
  CHECK_FALSE(fit.certified);
}

TEST_CASE("frechet: held public scale has a constrained global certificate") {
  std::vector<double> x;
  for(int i=0;i<50;++i)x.push_back(std::pow(-std::log((i+.5)/50),-.5));
  const auto r=cppunivariate::frechet_mle(mltest::as_span(x),1e-10,100,NAN,NAN,1);
  REQUIRE(r.status==cppunivariate::FrechetStatus::Ok);
  REQUIRE(r.certified);
  CHECK(r.sigma==1);
  const double h=1e-5;
  const double ga=(cppunivariate::frechet_loglik(mltest::as_span(x),r.lambda+h,r.mu,1)-cppunivariate::frechet_loglik(mltest::as_span(x),r.lambda-h,r.mu,1))/(2*h);
  const double gm=(cppunivariate::frechet_loglik(mltest::as_span(x),r.lambda,r.mu+h,1)-cppunivariate::frechet_loglik(mltest::as_span(x),r.lambda,r.mu-h,1))/(2*h);
  CHECK(std::abs(ga)<1e-3);CHECK(std::abs(gm)<1e-3);
  const double minimum=x.front();for(double& v:x)v-=minimum;
  auto fallback=r;fallback.mu-=minimum;fallback.certified=false;
  const auto result=cppunivariate::frechet_held_scale_certificate(mltest::as_span(x),1e-25,1,fallback);
  CHECK(result.status==fallback.status);CHECK_FALSE(result.certified);
  CHECK(result.lambda==fallback.lambda);CHECK(result.mu==fallback.mu);
  CHECK_FALSE(result.stop_detail.budget_exhausted);
}

TEST_CASE("frechet: held-scale certificate covers shape-one boundary") {
  std::vector<double> x;
  for(int i=0;i<50;++i)x.push_back(1/-std::log((i+.5)/50));
  const auto r=cppunivariate::frechet_mle(mltest::as_span(x),1e-10,100,NAN,NAN,1);
  REQUIRE(r.status==cppunivariate::FrechetStatus::Ok);
  CHECK(r.certified);CHECK(r.lambda==1);CHECK(r.sigma==1);CHECK((r.restricted&1u)!=0);
}

TEST_CASE("frechet: held-scale profile cover contracts a root at the slab midpoint") {
  std::vector<double> x;
  for(int j=0;j<500;++j)x.push_back(std::pow(-std::log((j+.5)/500),-.5));
  const auto fit=cppunivariate::frechet_mle(mltest::as_span(x),1e-10,100,NAN,NAN,1);
  REQUIRE(fit.status==cppunivariate::FrechetStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.sigma==1);
}

TEST_CASE("frechet: failed all-free coefficient inclusion retains a successful search") {
  const auto g=mltest::load_distribution_golden("frechet");
  std::vector<double> x=g.data;std::sort(x.begin(),x.end());
  const double m=x.front(),d=x.back()-m;for(double& v:x)v=(v-m)/d;
  const auto fit=cppunivariate::gev_joint_certificate(mltest::as_span(x),1e-16,100,true);
  REQUIRE(fit.status==cppunivariate::GevStatus::Ok);
  CHECK_FALSE(fit.certified);
  CHECK(fit.xi>0);
  CHECK_FALSE(fit.stop_detail.budget_exhausted);
}

TEST_CASE("frechet: cancellation in a finite two-point location retains an uncertified fit") {
  const double p=.45,m=p*.1/(std::sqrt(p*(1-p))+p);
  std::vector<double> x(20,m+1);std::fill_n(x.begin(),9,m);
  const auto fit=cppunivariate::frechet_mle(mltest::as_span(x));
  REQUIRE(fit.status==cppunivariate::FrechetStatus::Ok);
  CHECK_FALSE(fit.certified);CHECK(fit.lambda==1);
  CHECK(std::abs(fit.mu)<1e-12);CHECK(std::isfinite(fit.logLik));
}

TEST_CASE("frechet: an unordered Gumbel comparison retains the finite stationary proposal") {
  const double generating_shape=.0041309589552387081;
  std::vector<double> x;
  for(int j=0;j<80;++j)x.push_back(std::expm1(-generating_shape*std::log(-std::log((j+.5)/80)))/generating_shape);
  const auto fit=cppunivariate::frechet_mle(mltest::as_span(x));
  REQUIRE(fit.status==cppunivariate::FrechetStatus::Ok);
  CHECK_FALSE(fit.certified);CHECK(fit.lambda>1e5);CHECK(std::isfinite(fit.logLik));
  const auto profile=cppunivariate::gev_profile(mltest::as_span(x),1/fit.lambda,1e-12,100);
  CHECK(std::abs(profile.score)<1e-3);
}
