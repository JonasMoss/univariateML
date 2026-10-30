#include "test_support.hpp"
#include "cppunivariate/distributions/cmb/mle.hpp"
#include <vector>
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
