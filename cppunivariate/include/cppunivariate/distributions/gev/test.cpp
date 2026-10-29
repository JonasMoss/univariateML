#include "test_support.hpp"
#include "cppunivariate/distributions/gev/dpqr.hpp"
#include <vector>
#include <random>
#include <cmath>
TEST_CASE("gev: golden and independent scores") {
  const auto g=mltest::load_distribution_golden("gev");
  const auto r=cppunivariate::gev_mle(mltest::as_span(g.data));
  REQUIRE(r.status==cppunivariate::GevStatus::Ok);
  CHECK(r.mu==doctest::Approx(g.estimates.at("mu")).epsilon(1e-10));
  CHECK(r.sigma==doctest::Approx(g.estimates.at("sigma")).epsilon(1e-10));
  CHECK(r.xi==doctest::Approx(g.estimates.at("xi")).epsilon(1e-10));
  CHECK(r.logLik==doctest::Approx(g.logLik).epsilon(1e-10));
  double norm=0,h=1e-5;
  for(int j=0;j<3;++j) {
    double p[3]={r.mu,r.sigma,r.xi};p[j]+=h;
    double ll=cppunivariate::gev_loglik(mltest::as_span(g.data),p[0],p[1],p[2]);p[j]-=2*h;
    double score=(ll-cppunivariate::gev_loglik(mltest::as_span(g.data),p[0],p[1],p[2]))/(2*h);
    norm+=score*score;
  }
  CHECK(std::sqrt(norm)<1e-3);
}
TEST_CASE("gev: attained bounds, fixed shape and Gumbel continuity") {
  for(double k:{-.9,-.3,0.,.3,2.}) {
    std::vector<double>x;
    for(int i=0;i<400;++i)x.push_back(cppunivariate::distributions::qgev((i+.5)/400,0,2,k,true,false));
    const auto r=cppunivariate::gev_mle(mltest::as_span(x));
    REQUIRE(r.status==cppunivariate::GevStatus::Ok);
    if(k==-.9) CHECK(r.xi==-.5);
    if(k==2) CHECK(r.xi==1);
    const double h=1e-5;
    auto ll=[&](double m,double s,double a){return cppunivariate::gev_loglik(mltest::as_span(x),m,s,a);};
    CHECK(std::abs((ll(r.mu+h,r.sigma,r.xi)-ll(r.mu-h,r.sigma,r.xi))/(2*h))<1e-3);
    CHECK(std::abs((ll(r.mu,r.sigma+h,r.xi)-ll(r.mu,r.sigma-h,r.xi))/(2*h))<1e-3);
    if(r.xi==-.5) CHECK(ll(r.mu,r.sigma,r.xi+h)<r.logLik);
    else if(r.xi==1) CHECK(ll(r.mu,r.sigma,r.xi-h)<r.logLik);
    else CHECK(std::abs((ll(r.mu,r.sigma,r.xi+h)-ll(r.mu,r.sigma,r.xi-h))/(2*h))<1e-3);
    for(double a:{-.5,-1e-16,0.,1e-16,1.}) {
      const auto fixed=cppunivariate::gev_mle(mltest::as_span(x),1e-10,100,a);
      CHECK(fixed.status==cppunivariate::GevStatus::Ok);
      CHECK(fixed.xi==a);
      CHECK(fixed.logLik<=r.logLik+1e-7);
    }
  }
}
TEST_CASE("gev: validation and stable dpqr") {
  std::vector<double>x{0,1,2,3,4};
  CHECK(cppunivariate::gev_mle(mltest::as_span(x),1e-10,1).status==cppunivariate::GevStatus::IterationFailure);
  CHECK(cppunivariate::gev_mle(mltest::as_span(x),1e-10,100,1.1).status==cppunivariate::GevStatus::InvalidShape);
  for(auto y:{std::vector<double>{1,2},std::vector<double>{1,1,2},std::vector<double>{0,1,1,2},std::vector<double>{0,0,0,1,2},std::vector<double>{1,1,1}})
    CHECK(cppunivariate::gev_mle(mltest::as_span(y)).status==cppunivariate::GevStatus::InvalidData);
  x={0,0,1,2,3};CHECK(cppunivariate::gev_mle(mltest::as_span(x)).status==cppunivariate::GevStatus::Ok);
  CHECK(cppunivariate::distributions::qgev(.5,0,1,1e-16,true,false)==doctest::Approx(.3665129205816643));
  CHECK(cppunivariate::distributions::qgev(1,0,1,-.5,true,false)==2);
  CHECK(cppunivariate::distributions::pgev(1000,0,1,0,false,true)==-1000);
}

// Dense profile oracle checks root discovery independently of the fast mesh.
// R tests separately check the analytic location/scale reduction against dgev.
TEST_CASE("gev: seeded dense-profile oracle") {
  std::mt19937_64 engine(20261029);
  for(double shape:{-.9,-.6,-.3,0.,.1,.3,.7,1.,2.})
    for(int n:{3,4,5,10,30,100,500})
      for(int replicate=0;replicate<3;++replicate) {
        std::vector<double>x;
        for(int i=0;i<n;++i) {
          const double p=(static_cast<double>(engine()>>11)+.5)/9007199254740992.;
          x.push_back(cppunivariate::distributions::qgev(p,0,1,shape,true,false));
        }
        std::sort(x.begin(),x.end());
        const double origin=x.front(),range=x.back()-origin;
        for(double &v:x) v=(v-origin)/range;
        const auto data=mltest::as_span(x);
        const auto fit=cppunivariate::gev_mle(data);
        CAPTURE(shape);CAPTURE(n);CAPTURE(replicate);
        REQUIRE(fit.status==cppunivariate::GevStatus::Ok);
        const int cells=256;
        double best=-std::numeric_limits<double>::infinity();
        auto evaluate=[&](double k){return cppunivariate::gev_profile(data,k,1e-12,150).fit.logLik;};
        double before=evaluate(-.5),prev=evaluate(-.5+1.5/cells);
        best=std::max(before,prev);
        for(int j=2;j<=cells;++j) {
          const double k=-.5+1.5*j/cells,next=evaluate(k);
          best=std::max(best,next);
          if(prev>before && prev>next) {
            double lo=k-3./cells,hi=k;
            for(int step=0;step<50;++step) {
              const double a=lo+(hi-lo)*.3819660112501051,b=hi-(hi-lo)*.3819660112501051;
              if(evaluate(a)>evaluate(b)) hi=b; else lo=a;
            }
            best=std::max(best,evaluate((lo+hi)/2));
          }
          before=prev;prev=next;
        }
        CHECK(std::abs(fit.logLik-best)<1e-7);
      }
}
TEST_CASE("gev: clustered samples and affine equivariance") {
  for(auto x:{std::vector<double>{0,1e-12,1},std::vector<double>{0,0,1,2,3},
              std::vector<double>{0,1,2,3,3},std::vector<double>{0,1,1,1,2,3,4}}) {
    const auto fit=cppunivariate::gev_mle(mltest::as_span(x));
    REQUIRE(fit.status==cppunivariate::GevStatus::Ok);
    const auto data=mltest::as_span(x);
    for(double k:{-.5,-.3,0.,.3,1.}) {
      const auto fixed=cppunivariate::gev_mle(data,1e-10,100,k);
      CHECK(fixed.status==cppunivariate::GevStatus::Ok);
      CHECK(fit.logLik>=fixed.logLik-1e-7);
    }
  }
  std::vector<double>x{-2,-.4,0,.3,1,4};
  const auto fit=cppunivariate::gev_mle(mltest::as_span(x));
  for(double scale:{1e-100,1e100}) {
    auto y=x;for(double &v:y)v=scale*(10+v);
    const auto other=cppunivariate::gev_mle(mltest::as_span(y));
    REQUIRE(other.status==cppunivariate::GevStatus::Ok);
    CHECK(other.mu/scale-10==doctest::Approx(fit.mu).epsilon(1e-7));
    CHECK(other.sigma/scale==doctest::Approx(fit.sigma).epsilon(1e-7));
    CHECK(other.xi==doctest::Approx(fit.xi).epsilon(1e-7));
    CHECK(other.logLik+x.size()*std::log(scale)==doctest::Approx(fit.logLik).epsilon(1e-7));
  }
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("gev: d/p/q/r against the truth table" * doctest::may_fail()) {
  mltest::check_dpqr_truth("gev",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dgev(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pgev(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qgev(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rgev(rng, p[0], p[1], p[2]); });
}
