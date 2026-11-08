#include "test_support.hpp"
#include "cppunivariate/distributions/cmb/mle.hpp"
#include <vector>
#include <random>
TEST_CASE("cmb golden and moment score") {
  const auto g=mltest::load_distribution_golden("cmb");
  std::vector<int> x; for (double v:g.data) x.push_back(static_cast<int>(v));
  const auto f=cppunivariate::cmb_mle({x.data(),x.size()},10);
  REQUIRE(f.status==cppunivariate::CmbStatus::Ok);
  CHECK(f.prob==doctest::Approx(g.estimates.at("prob")).epsilon(1e-8));
  CHECK(f.nu==doctest::Approx(g.estimates.at("nu")).epsilon(1e-8));
  CHECK(f.logLik==doctest::Approx(g.logLik).epsilon(1e-10));
  const auto t=cppunivariate::cmb_detail::Table(10,std::log(f.prob/(1-f.prob)),f.nu);
  double m=0,h=0; for (int v:x) {m+=v; h+=cppunivariate::cmb_detail::logchoose(10,v);}
  CHECK(std::abs(m/x.size()-t.mean)<1e-8);
  CHECK(std::abs(h/x.size()-t.mh)<1e-8);
}
TEST_CASE("cmb binomial and all finite-family faces") {
  for (int N:{1,5,20}) for (double p:{.01,.3,.99}) {
    double sum=0;
    for (int k=0;k<=N;++k) {
      const double lp=cppunivariate::cmb_detail::logchoose(N,k)+k*std::log(p)+(N-k)*std::log1p(-p);
      const double w=cppunivariate::distributions::dcmb(k,N,p,1);
      CHECK(w==doctest::Approx(std::exp(lp)).epsilon(1e-11)); sum+=w;
    }
    CHECK(sum==doctest::Approx(1).epsilon(1e-12));
  }
  const std::vector<int> a{2,2},b{2,3},c{0,5},d{0,1,4,5};
  CHECK(cppunivariate::cmb_mle({a.data(),a.size()},5).status==cppunivariate::CmbStatus::PointMass);
  CHECK(cppunivariate::cmb_mle({b.data(),b.size()},5).status==cppunivariate::CmbStatus::AdjacentLimit);
  CHECK(cppunivariate::cmb_mle({c.data(),c.size()},5).status==cppunivariate::CmbStatus::EndpointLimit);
  CHECK(cppunivariate::cmb_mle({d.data(),d.size()},5).status==cppunivariate::CmbStatus::Ok);
}

#include "cppunivariate/distributions/cmb/dpqr.hpp"

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("cmb: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("cmb",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dcmb(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pcmb(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qcmb(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rcmb(rng, p[0], p[1], p[2]); });
}

TEST_CASE("CMB P3 endpoint and zero-size members are attained") {
 const std::vector<int> z{0,0}, top{5,5}, bad{0,1};
 const double nan=std::numeric_limits<double>::quiet_NaN();
 for(double nu:{-2.0,0.0,2.0,nan}) {
   auto r=cppunivariate::cmb_mle({z.data(),z.size()},5,1e-10,100,nan,nu);
   CHECK(r.status==cppunivariate::CmbStatus::Ok); CHECK(r.prob==0); CHECK(r.logLik==0);
   r=cppunivariate::cmb_mle({top.data(),top.size()},5,1e-10,100,nan,nu);
   CHECK(r.status==cppunivariate::CmbStatus::Ok); CHECK(r.prob==1);
 }
 CHECK(cppunivariate::cmb_mle({z.data(),z.size()},0).status==cppunivariate::CmbStatus::Ok);
 CHECK(cppunivariate::cmb_mle({bad.data(),bad.size()},5,1e-10,100,0).status==cppunivariate::CmbStatus::InvalidInput);
}

TEST_CASE("cmb: certified free-size enumeration with canonical holds") {
  const std::vector<int> x{0,1,2,2,3,5};
  for(double p : {.2,.6})for(double nu : {-2.,0.,.5,1.5}) {
    const auto f=cppunivariate::cmb_size_mle({x.data(),x.size()},p,nu);
    REQUIRE(f.fit.status==cppunivariate::CmbStatus::Ok);
    CHECK(f.fit.prob==p);CHECK(f.fit.nu==nu);
    double best=-INFINITY;int arg=0;
    for(int N=5;N<=120;++N) {
      const auto candidate=cppunivariate::cmb_mle({x.data(),x.size()},N,1e-10,100,p,nu);
      REQUIRE(candidate.status==cppunivariate::CmbStatus::Ok);
      if(candidate.logLik>best){best=candidate.logLik;arg=N;}
    }
    CHECK(f.size==arg);
    CHECK(f.fit.logLik==doctest::Approx(best).epsilon(1e-12));
  }
}

TEST_CASE("cmb: free-size endpoints, invalid controls and work exhaustion") {
  const std::vector<int> x{0,1},zero{0,0},constant{3,3},expensive{0,100};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  CHECK(cppunivariate::cmb_size_mle({zero.data(),zero.size()},.3,-2).size==0);
  CHECK(cppunivariate::cmb_size_mle({constant.data(),constant.size()},1,-2).size==3);
  CHECK(cppunivariate::cmb_size_mle({x.data(),x.size()},0,1).fit.status==cppunivariate::CmbStatus::InvalidInput);
  CHECK(cppunivariate::cmb_size_mle({x.data(),x.size()},1,1).fit.status==cppunivariate::CmbStatus::InvalidInput);
  CHECK(cppunivariate::cmb_size_mle({x.data(),x.size()},nan,1).fit.status==cppunivariate::CmbStatus::InvalidInput);
  CHECK(cppunivariate::cmb_size_mle({x.data(),x.size()},.3,INFINITY).fit.status==cppunivariate::CmbStatus::InvalidInput);
  CHECK(cppunivariate::cmb_size_mle({x.data(),x.size()},.3,1,1e-10,0).fit.status==cppunivariate::CmbStatus::InvalidInput);
  CHECK(cppunivariate::cmb_size_mle({expensive.data(),expensive.size()},1e-8,1).fit.status==cppunivariate::CmbStatus::IterationFailure);
}

TEST_CASE("cmb attained point sampler ignores zero uniform") {
  // At success probability one the (possibly truncated) law is delta_size.
  struct ZeroRng { double runif(double,double) { return 0; } } rng;
  CHECK(cppunivariate::distributions::rcmb(rng,5,1,2)==5);
}

TEST_CASE("cmb TASK-134 size-cap atoms and binomial submodel") {
  for(double p:{0.,1.}) {
    const double atom=p==0?0:1e7;
    CHECK(cppunivariate::distributions::dcmb(atom,1e7,p,2)==1);
    CHECK(cppunivariate::distributions::pcmb(atom,1e7,p,2)==1);
    CHECK(cppunivariate::distributions::qcmb(.5,1e7,p,2)==atom);
    CHECK(cppunivariate::distributions::qcmb(1,1e7,p,2)==1e7);
    CHECK(cppunivariate::distributions::qcmb(0,1e7,p,2)==0);
  }
  CHECK(cppunivariate::distributions::dcmb(3,10,.3,1)==cppunivariate::distributions::dbinom(3,10,.3,false));
}

TEST_CASE("cmb TASK-134 finite geometric shortcut agrees with enumeration") {
  for(double p:{1e-12,.2,.5,.8,1-1e-12}) {
    const auto t=cppunivariate::cmb_detail::table(20,p,0);
    for(int k=0;k<=20;++k) {
      CHECK(cppunivariate::distributions::dcmb(k,20,p,0,true)==doctest::Approx(t.density(k,true)).epsilon(1e-12));
      for(bool lower:{false,true}) {
        const double actual=cppunivariate::distributions::pcmb(k,20,p,0,lower,true);
        const double expected=t.cdf(k,lower,true);
        if(std::isinf(expected)) CHECK(actual==expected);
        else CHECK(actual==doctest::Approx(expected).epsilon(1e-12));
      }
    }
    CHECK(cppunivariate::distributions::qcmb(.3,20,p,0)==t.quantile(.3,true,false));
  }
}

TEST_CASE("cmb free size with fixed dispersion bounds the full tail") {
  const std::vector<int> x{0,0,1,1,2,2,2,3,3,4};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(double nu:{-1.,0.,.5}) {
    const auto fit=cppunivariate::cmb_size_fixed_nu_mle({x.data(),x.size()},nu);
    REQUIRE(fit.fit.status==cppunivariate::CmbStatus::Ok);
    double best=-INFINITY;int arg=-1;
    for(int N=4;N<=300;++N) {
      const auto f=cppunivariate::cmb_mle({x.data(),x.size()},N,1e-10,100,nan,nu);
      REQUIRE(f.status==cppunivariate::CmbStatus::Ok);
      if(f.logLik>best){best=f.logLik;arg=N;}
    }
    CHECK(fit.size==arg);
    CHECK(fit.fit.logLik==doctest::Approx(best).epsilon(1e-10));
    CHECK(fit.fit.nu==nu);
  }
  const std::vector<int> face{0,2}, huge{0,10000000}, constant{3,3};
  CHECK(cppunivariate::cmb_size_fixed_nu_mle({face.data(),face.size()},1).fit.status==cppunivariate::CmbStatus::FaceUnresolved);
  const std::vector<int> tie{0,0,0,0,0,0,0,0,0,6};
  CHECK(cppunivariate::cmb_size_fixed_nu_mle({tie.data(),tie.size()},0.0030483655389936895).fit.status==cppunivariate::CmbStatus::FaceTie);
  CHECK(cppunivariate::cmb_size_fixed_nu_mle({huge.data(),huge.size()},1,1e-10,1).fit.status==cppunivariate::CmbStatus::IterationFailure);
  CHECK(cppunivariate::cmb_size_fixed_nu_mle({constant.data(),constant.size()},-1).size==3);
}

TEST_CASE("cmb fixed dispersion seeded size grid") {
  std::mt19937 rng(1142026);
  std::binomial_distribution<int> draw(4,.4);
  std::vector<int> x(100);
  for(int& v:x)v=draw(rng);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto fit=cppunivariate::cmb_size_fixed_nu_mle({x.data(),x.size()},.5);
  REQUIRE(fit.fit.status==cppunivariate::CmbStatus::Ok);
  double best=-INFINITY;int arg=0;
  for(int N=*std::max_element(x.begin(),x.end());N<=300;++N) {
    const auto f=cppunivariate::cmb_mle({x.data(),x.size()},N,1e-10,100,nan,.5);
    REQUIRE(f.status==cppunivariate::CmbStatus::Ok);
    if(f.logLik>best){best=f.logLik;arg=N;}
  }
  CHECK(fit.size==arg);
  CHECK(fit.fit.logLik==doctest::Approx(best).epsilon(1e-10));
}

TEST_CASE("cmb free dispersion constant samples") {
  const std::vector<int> z{0,0}, c{3,3}, mixed{1,2};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  auto f=cppunivariate::cmb_size_free_nu_mle({c.data(),c.size()});
  CHECK(f.fit.status==cppunivariate::CmbStatus::Ok);
  CHECK(f.size==3); CHECK(f.fit.prob==1); CHECK(f.fit.logLik==0);
  f=cppunivariate::cmb_size_free_nu_mle({z.data(),z.size()},.4);
  CHECK(f.fit.status==cppunivariate::CmbStatus::Ok); CHECK(f.size==0);
  CHECK(f.fit.prob==.4);
  CHECK(cppunivariate::cmb_size_free_nu_mle({c.data(),c.size()},.4).fit.status==cppunivariate::CmbStatus::PointMass);
  CHECK(cppunivariate::cmb_size_free_nu_mle({c.data(),c.size()},0).fit.status==cppunivariate::CmbStatus::InvalidInput);
  CHECK(cppunivariate::cmb_size_free_nu_mle({mixed.data(),mixed.size()},nan).fit.status==cppunivariate::CmbStatus::AdjacentLimit);
  CHECK(cppunivariate::cmb_size_free_nu_mle({c.data(),c.size()},nan,1e-10,0).fit.status==cppunivariate::CmbStatus::InvalidInput);
}

TEST_CASE("cmb free size and dispersion adjacent samples") {
  const std::vector<int> bernoulli{0,0,1}, adjacent{2,2,3};
  const auto fit=cppunivariate::cmb_size_free_nu_mle({bernoulli.data(),bernoulli.size()});
  CHECK(fit.fit.status==cppunivariate::CmbStatus::FaceTie);
  CHECK(cppunivariate::cmb_size_free_nu_mle({adjacent.data(),adjacent.size()}).fit.status==cppunivariate::CmbStatus::AdjacentLimit);
}

TEST_CASE("cmb free size and dispersion endpoint samples") {
  const std::vector<int> endpoints{0,0,4};
  CHECK(cppunivariate::cmb_size_free_nu_mle({endpoints.data(),endpoints.size()}).fit.status==cppunivariate::CmbStatus::EndpointLimit);
}

TEST_CASE("cmb free size symmetric held probability faces") {
  const std::vector<int> bernoulli{0,1}, adjacent{2,3}, endpoints{0,4};
  const auto fit=cppunivariate::cmb_size_free_nu_mle({bernoulli.data(),bernoulli.size()},.5);
  CHECK(fit.fit.status==cppunivariate::CmbStatus::Ok);
  CHECK(fit.size==1);
  CHECK(fit.fit.prob==.5);
  CHECK(fit.fit.logLik==doctest::Approx(-2*std::log(2.)));
  CHECK(cppunivariate::cmb_size_free_nu_mle({adjacent.data(),adjacent.size()},.5).fit.status==cppunivariate::CmbStatus::AdjacentLimit);
  CHECK(cppunivariate::cmb_size_free_nu_mle({endpoints.data(),endpoints.size()},.5).fit.status==cppunivariate::CmbStatus::EndpointLimit);
}

TEST_CASE("cmb truncated entropy dual bounds a two-coordinate grid") {
  const std::vector<int> x{0,1,2,2,3,4};
  double upper=0;std::size_t work=0;
  REQUIRE(cppunivariate::cmb_detail::truncated_upper({x.data(),x.size()},8,upper,work,1000000));
  for(int ia=-12;ia<=12;++ia)for(int iv=-4;iv<=12;++iv) {
    const double a=ia*.25,nu=iv*.25;
    double peak=-INFINITY;
    for(int k=0;k<=8;++k)peak=std::max(peak,k*a-nu*std::lgamma(k+1.));
    double z=0,ll=0;
    for(int k=0;k<=8;++k)z+=std::exp(k*a-nu*std::lgamma(k+1.)-peak);
    for(int v:x)ll+=v*a-nu*std::lgamma(v+1.)-peak-std::log(z);
    CHECK(ll<=upper+1e-9);
  }
  work=1000000;
  CHECK_FALSE(cppunivariate::cmb_detail::truncated_upper({x.data(),x.size()},8,upper,work,1000000));
}

TEST_CASE("cmb uniform empirical support attains the saturated likelihood") {
  const std::vector<int> x{0,0,1,1,2,2,3,3,4,4};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(double p:{nan,.5}) {
    const auto fit=cppunivariate::cmb_size_free_nu_mle({x.data(),x.size()},p);
    REQUIRE(fit.fit.status==cppunivariate::CmbStatus::Ok);
    CHECK(fit.size==4);
    CHECK(fit.fit.prob==.5);
    CHECK(fit.fit.nu==0);
    CHECK(fit.fit.logLik==doctest::Approx(-10*std::log(5.)));
    for(int N=4;N<=30;++N)for(int i=1;i<10;++i)for(int j=-4;j<=4;++j) {
      const auto f=cppunivariate::cmb_mle({x.data(),x.size()},N,1e-10,100,i/10.,j*.5);
      REQUIRE(f.status==cppunivariate::CmbStatus::Ok);
      CHECK(f.logLik<=fit.fit.logLik+1e-9);
    }
  }
}

TEST_CASE("cmb free dispersion does not return a capped size") {
  const std::vector<int> x{1,10000000};
  CHECK(cppunivariate::cmb_size_free_nu_mle({x.data(),x.size()},NAN,1e-10,1).fit.status==cppunivariate::CmbStatus::IterationFailure);
  CHECK(cppunivariate::cmb_size_free_nu_mle({x.data(),x.size()},.3,1e-10,1).fit.status==cppunivariate::CmbStatus::IterationFailure);
}

TEST_CASE("cmb fixed-size entropy dual certifies moment solves") {
  const std::vector<int> x{0,1,2,2,3,4};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(double p:{nan,.3}) {
    const auto fit=cppunivariate::cmb_mle({x.data(),x.size()},7,1e-10,100,p,nan);
    REQUIRE(fit.status==cppunivariate::CmbStatus::Ok);
    std::size_t work=0;
    const auto candidate=cppunivariate::cmb_detail::size_candidate({x.data(),x.size()},7,p,1e-10,100,work,1000000);
    REQUIRE(candidate.status==cppunivariate::CmbStatus::Ok);
    CHECK(candidate.logLik==doctest::Approx(fit.logLik).epsilon(1e-8));
    double upper=0;
    REQUIRE(cppunivariate::cmb_detail::fixed_profile_upper({x.data(),x.size()},7,candidate,!std::isnan(p),upper));
    CHECK(upper>=candidate.logLik-1e-8);
    CHECK(upper-candidate.logLik<1e-7);
    work=1000000;
    CHECK(cppunivariate::cmb_detail::size_candidate({x.data(),x.size()},7,p,1e-10,100,work,1000000).status==cppunivariate::CmbStatus::IterationFailure);
  }
}

TEST_CASE("cmb adjacent observations with held symmetric probability") {
  const std::vector<int> small{0,0,1}, large{0,1,1,1,1,1,1,1,1,1};
  const auto a=cppunivariate::cmb_size_free_nu_mle({small.data(),small.size()},.5);
  const auto b=cppunivariate::cmb_size_free_nu_mle({large.data(),large.size()},.5);
  REQUIRE(a.fit.status==cppunivariate::CmbStatus::Ok);
  REQUIRE(b.fit.status==cppunivariate::CmbStatus::Ok);
  CHECK(a.size==1);CHECK(b.size==2);
  CHECK(b.fit.nu==doctest::Approx(std::log(18.)/std::log(2.)));
  for(int N=2;N<=80;++N)for(int j=-20;j<=20;++j) {
    const auto f=cppunivariate::cmb_mle({large.data(),large.size()},N,1e-10,100,.5,j*.5);
    REQUIRE(f.status==cppunivariate::CmbStatus::Ok);
    CHECK(f.logLik<=b.fit.logLik+1e-8);
  }
}

TEST_CASE("cmb adjacent held-probability face tie") {
  const std::vector<int> x{1,2};
  // The root equates the size-two profile with the size-three central face.
  const double p=.69830412636804606;
  CHECK(cppunivariate::cmb_size_free_nu_mle({x.data(),x.size()},p).fit.status==cppunivariate::CmbStatus::FaceTie);
}

TEST_CASE("cmb nonuniform free-dispersion fits agree with a size grid") {
  std::vector<int> x{0,0,1,2,3,4};
  std::mt19937 rng(1142026);
  std::shuffle(x.begin(),x.end(),rng);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(double p:{nan,.5}) {
    const auto fit=cppunivariate::cmb_size_free_nu_mle({x.data(),x.size()},p);
    REQUIRE(fit.fit.status==cppunivariate::CmbStatus::Ok);
    double best=-INFINITY;int arg=-1;
    for(int N=4;N<=100;++N) {
      const auto f=cppunivariate::cmb_mle({x.data(),x.size()},N,1e-10,100,p,nan);
      REQUIRE(f.status==cppunivariate::CmbStatus::Ok);
      if(f.logLik>best){best=f.logLik;arg=N;}
    }
    CHECK(fit.size==arg);
    CHECK(fit.fit.logLik==doctest::Approx(best).epsilon(1e-8));
  }
}

TEST_CASE("cmb superlevel probability bounds are conservative") {
  const std::vector<int> x{0,0,1,2,3,4};
  const double B=-10;
  for(int category:{0,4}) {
    const double lower=cppunivariate::cmb_detail::observed_log_lower({x.data(),x.size()},category,B);
    const double upper_log=cppunivariate::cmb_detail::observed_log_upper({x.data(),x.size()},category,B);
    REQUIRE(std::isfinite(lower));
    const double n=double(x.size()),count=category==0?2.:1.,f=count/n;
    double saturated=2*std::log(2./6)+4*std::log(1./6);
    for(int j=1;j<1000;++j) {
      const double p=j/1000.;
      const double upper=saturated+count*std::log(p/f)+(n-count)*(std::log1p(-p)-std::log1p(-f));
      if(upper>=B) {
        CHECK(std::log(p)>=lower-1e-12);
        CHECK(std::log(p)<=upper_log+1e-12);
      }
    }
  }
}

TEST_CASE("cmb fixed-odds solver handles a near-endpoint starting step") {
  const std::vector<int> x{0,0,1,2,3,4};
  std::size_t work=0;
  const auto f=cppunivariate::cmb_detail::size_candidate_rescue({x.data(),x.size()},101,.5,1e-10,100,work,10000000);
  REQUIRE(f.status==cppunivariate::CmbStatus::Ok);
  double upper=0;
  REQUIRE(cppunivariate::cmb_detail::fixed_profile_upper({x.data(),x.size()},101,f,true,upper));
  CHECK(std::abs(upper-f.logLik)<1e-8*(1+std::abs(f.logLik)));
  const auto reference=cppunivariate::cmb_mle({x.data(),x.size()},101,1e-10,100,.5);
  REQUIRE(reference.status==cppunivariate::CmbStatus::Ok);
  CHECK(f.logLik==doctest::Approx(reference.logLik).epsilon(1e-8));
}

TEST_CASE("cmb golden free size is certified and agrees with dense profiles") {
  const auto golden=mltest::load_distribution_golden("cmb");
  std::vector<int> x;for(double v:golden.data)x.push_back(int(v));
  const cppunivariate::ispan data{x.data(),x.size()};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(double prob:{nan,golden.estimates.at("prob")}) {
    const auto fit=cppunivariate::cmb_size_free_nu_mle(data,prob);
    REQUIRE(fit.fit.status==cppunivariate::CmbStatus::Ok);
    CHECK(fit.size==(std::isnan(prob)?16.:10.));
    double best=-INFINITY;
    for(int N=7;N<=300;++N) {
      std::size_t work=0;
      const auto f=cppunivariate::cmb_detail::size_candidate_rescue(data,N,prob,1e-10,100,work,10000000);
      REQUIRE(f.status==cppunivariate::CmbStatus::Ok);
      double upper=0;
      REQUIRE(cppunivariate::cmb_detail::fixed_profile_upper(data,N,f,!std::isnan(prob),upper));
      CHECK(upper-f.logLik<1e-8*(1+std::abs(f.logLik)));
      best=std::max(best,f.logLik);
    }
    CHECK(fit.fit.logLik==doctest::Approx(best).epsilon(1e-10));
  }
}

TEST_CASE("cmb: structured stop payloads") {
  const std::vector<int> x{0,0,5};
  const auto r=cppunivariate::cmb_mle({x.data(),x.size()},5);
  REQUIRE(r.stop_detail.limit_coef.size()==3);
  CHECK(r.stop_detail.limit_coef[2]==doctest::Approx(1./3));
  const auto held=cppunivariate::cmb_mle({x.data(),x.size()},5,1e-10,100,.4);
  const double p=1/(1+std::pow(1.5,5));
  CHECK(held.stop_detail.limit_coef[2]==doctest::Approx(p));
  const std::vector<int> spread{0,2};
  const auto f=cppunivariate::cmb_size_fixed_nu_mle({spread.data(),spread.size()},1);
  REQUIRE(f.fit.status==cppunivariate::CmbStatus::FaceUnresolved);
  REQUIRE(f.fit.stop_detail.limit_coef.size()==2);
  CHECK(f.fit.stop_detail.limit_coef[0]==doctest::Approx(1));
  CHECK(f.fit.stop_detail.candidates.size()>=2);
  CHECK(f.fit.stop_detail.loglik_upper==INFINITY);
  const std::vector<int> tie{0,1};
  const auto t=cppunivariate::cmb_size_free_nu_mle({tie.data(),tie.size()});
  REQUIRE(t.fit.status==cppunivariate::CmbStatus::FaceTie);
  CHECK(t.fit.stop_detail.limit_loglik==doctest::Approx(-2*std::log(2.)));
}

TEST_CASE("cmb: explicit work exhaustion differs from numerical iteration failure") {
  const std::vector<int> x{0,1};
  std::size_t work=0;
  const auto r=cppunivariate::cmb_detail::size_candidate({x.data(),x.size()},1,NAN,1e-10,100,work,0);
  CHECK(r.status==cppunivariate::CmbStatus::IterationFailure);
  CHECK(r.stop_detail.budget_exhausted);
  CHECK(r.stop_detail.loglik_upper==INFINITY);
  CHECK(r.stop_detail.candidates.empty());
}

TEST_CASE("cmb: held endpoint odds do not erase the minority likelihood") {
  const std::vector<int> x{0,1000};
  const auto r=cppunivariate::cmb_mle({x.data(),x.size()},1000,1e-10,100,.9);
  REQUIRE(r.status==cppunivariate::CmbStatus::EndpointLimit);
  const double eta=1000*(std::log(.9)-std::log1p(-.9));
  CHECK(std::isfinite(r.stop_detail.limit_loglik));
  CHECK(r.stop_detail.limit_loglik==doctest::Approx(-eta));
}

TEST_CASE("cmb: fixed-size normalizers share the iterlim cell allowance") {
  const std::vector<int> x{0,1};
  const auto stopped=cppunivariate::cmb_mle({x.data(),x.size()},100001,1e-10,1,.5,1);
  CHECK(stopped.status==cppunivariate::CmbStatus::IterationFailure);
  CHECK(stopped.stop_detail.budget_exhausted);
  CHECK(stopped.stop_detail.candidates.empty());
  CHECK(stopped.stop_detail.loglik_upper==INFINITY);
  const auto fitted=cppunivariate::cmb_mle({x.data(),x.size()},100001,1e-10,2,.5,1);
  CHECK(fitted.status==cppunivariate::CmbStatus::Ok);
  CHECK(std::isfinite(fitted.logLik));
}

TEST_CASE("cmb: fixed-design quantitative concave acceptance") {
  const std::vector<int> x{0,0,1,1,2,2,3,4};
  for(double p:{double(NAN),.3})for(double nu:{double(NAN),1.}) {
    const auto f=cppunivariate::cmb_mle({x.data(),x.size()},8,1e-10,2000,p,nu);
    REQUIRE(f.status==cppunivariate::CmbStatus::Ok);
    if(std::isnan(p) || std::isnan(nu)) {
      CHECK(std::isfinite(f.stop_detail.loglik_upper));
      CHECK(f.stop_detail.loglik_upper>=f.logLik);
      CHECK(f.stop_detail.loglik_upper-f.logLik<=1e-8*(x.size()+std::abs(f.logLik)));
    }
  }
  // Size one cannot identify dispersion: distinct nu have the same law.
  const std::vector<int> bernoulli{0,1,1};
  const auto a=cppunivariate::cmb_mle({bernoulli.data(),bernoulli.size()},1,1e-10,2000,NAN,-20);
  const auto b=cppunivariate::cmb_mle({bernoulli.data(),bernoulli.size()},1,1e-10,2000,NAN,20);
  CHECK(a.logLik==doctest::Approx(b.logLik));
}

TEST_CASE("J6: score half-spaces enclose an independently known flat quadratic maximum") {
  const auto evaluate=[](double u,double v) {
    return cppunivariate::concave_accept_detail::Evaluation{
      -.5*(100*u*u+.01*v*v),-100*u,-.01*v,true};
  };
  const auto accurate=[](const cppunivariate::concave_accept_detail::Bound& b) {
    return std::max({std::abs(b.lo1),std::abs(b.hi1),std::abs(b.lo2),std::abs(b.hi2)})<1e-8;
  };
  const auto b=cppunivariate::concave_accept_detail::certify(0,0,0,.01,0,100,
    2,false,100,evaluate,accurate);
  REQUIRE(b.ok);
  CHECK(b.upper>=0);CHECK(b.upper<1e-8);
  CHECK(b.lo1<=0);CHECK(b.hi1>=0);CHECK(b.lo2<=0);CHECK(b.hi2>=0);
  CHECK(b.evaluations<=300);
  CHECK_FALSE(cppunivariate::concave_accept_detail::certify(1,0,-50,.01,0,100,
    2,false,100,evaluate,accurate).ok);
  CHECK_FALSE(cppunivariate::concave_accept_detail::certify(0,0,0,.01,0,100,
    2,false,1,evaluate,accurate).ok);
}

TEST_CASE("J6: boundary KKT sign holds across the enclosed scalar root") {
  const auto evaluate=[](double u,double v) {
    return cppunivariate::concave_accept_detail::Evaluation{
      -.5*u*u-10*u*v-50.5*v*v-1e-6*v,-u-10*v,-10*u-101*v-1e-6,true};
  };
  const auto accurate=[](const cppunivariate::concave_accept_detail::Bound& b) {
    return std::max(std::abs(b.lo1),std::abs(b.hi1))<1e-8;
  };
  const auto b=cppunivariate::concave_accept_detail::certify(0,0,0,1,0,0,
    1,true,100,evaluate,accurate);
  REQUIRE(b.ok);
  CHECK(b.lo1<=0);CHECK(b.hi1>=0);CHECK(b.evaluations>2);
  CHECK(b.upper>=0);CHECK(b.upper<1e-8);
}

TEST_CASE("CMB CMP child allowance never shrinks when iterlim rises") {
  const std::vector<int> x{0,0,1,1,2,2,2,3,3,4};
  const auto a=cppunivariate::cmb_size_free_nu_mle({x.data(),x.size()},NAN,1e-10,100);
  const auto b=cppunivariate::cmb_size_free_nu_mle({x.data(),x.size()},NAN,1e-10,2000);
  REQUIRE(a.fit.status==cppunivariate::CmbStatus::Ok);
  REQUIRE(b.fit.status==cppunivariate::CmbStatus::Ok);
  CHECK(a.size==b.size);
  CHECK(a.fit.prob==b.fit.prob);
  CHECK(a.fit.nu==b.fit.nu);
  CHECK(a.fit.logLik==b.fit.logLik);
}

#include "cppunivariate/distributions/cmb/dpqr.hpp"
TEST_CASE("cmb: quantile search guards and representability") {
  cppunivariate::cmb_detail::Table t(10,0,1);
  t.lp[3]=NAN;
  t.cumulative();
  CHECK(std::isnan(t.quantile(.5,true,false)));
}
