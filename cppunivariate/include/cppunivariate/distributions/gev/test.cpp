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
    if(k==-.9) CHECK(r.xi<-.5);
    if(k==2) CHECK(r.xi==1);
    const double h=r.xi<-.5 ? 1e-7 : 1e-5;
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
  CHECK(cppunivariate::gev_mle(mltest::as_span(x),1e-10,100,1.1).status==cppunivariate::GevStatus::Ok);
  for(auto y:{std::vector<double>{1,2},std::vector<double>{1,1,2},std::vector<double>{0,0,0,1,2},std::vector<double>{1,1,1}})
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
        double before=evaluate(-1),prev=evaluate(-1+2.0/cells);
        best=std::max(before,prev);
        for(int j=2;j<=cells;++j) {
          const double k=-1+2.0*j/cells,next=evaluate(k);
          best=std::max(best,next);
          if(prev>before && prev>next) {
            double lo=k-4./cells,hi=k;
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
TEST_CASE("gev: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("gev",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dgev(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pgev(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qgev(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rgev(rng, p[0], p[1], p[2]); });
}

TEST_CASE("gev: only minimum ties control fixed-shape collapse") {
  for (auto x : {std::vector<double>{0,1,1,2}, std::vector<double>{0,1,2,2,2}, std::vector<double>{0,0,1,2}}) {
    for(double xi:{-.5,0.,.3})
      CHECK(cppunivariate::gev_mle(mltest::as_span(x),1e-10,100,xi).status==cppunivariate::GevStatus::Ok);
  }
  const std::vector<double> x{0,0,0,1};
  CHECK(cppunivariate::gev_mle(mltest::as_span(x),1e-10,100,.5).status==cppunivariate::GevStatus::InvalidData);
  CHECK(cppunivariate::gev_mle(mltest::as_span(x),1e-10,100,0).status==cppunivariate::GevStatus::Ok);
}

TEST_CASE("gev: all new hold subsets preserve held coefficients and scores") {
 const double nan=std::numeric_limits<double>::quiet_NaN();
 const std::vector<double> x{-.7,-.2,0,.4,.8,1.5,3};
 const auto data=mltest::as_span(x);
 for(int mask:{1,2,3,5,6,7}) {
   const double mu=mask&1 ? .1 : nan,sigma=mask&2 ? 1.3 : nan,xi=mask&4 ? .2 : nan;
   const auto f=cppunivariate::gev_mle(data,1e-10,150,xi,mu,sigma);
   CAPTURE(mask);
   REQUIRE(f.status==cppunivariate::GevStatus::Ok);
   if(mask&1) CHECK(f.mu==mu);
   if(mask&2) CHECK(f.sigma==sigma);
   if(mask&4) CHECK(f.xi==xi);
   CHECK(f.logLik==doctest::Approx(cppunivariate::gev_loglik(data,f.mu,f.sigma,f.xi)));
   for(int j=0;j<3;++j) if(!(mask&(1<<j))) {
     if(j==2 && (f.xi==-.5 || f.xi==1)) continue;
     double p[3]={f.mu,f.sigma,f.xi};const double h=1e-5;
     p[j]+=h;const double plus=cppunivariate::gev_loglik(data,p[0],p[1],p[2]);
     p[j]-=2*h;const double minus=cppunivariate::gev_loglik(data,p[0],p[1],p[2]);
     CHECK(std::abs((plus-minus)/(2*h))<1e-3);
   }
 }
 const std::vector<double> one{1};
 const auto f=cppunivariate::gev_mle(mltest::as_span(one),1e-10,100,0,nan,2);
 CHECK(f.status==cppunivariate::GevStatus::Ok);
 CHECK(f.mu==1);
 CHECK(cppunivariate::gev_mle(data,1e-10,1,nan,0,1).status==cppunivariate::GevStatus::IterationFailure);
 CHECK(cppunivariate::gev_mle(data,1e-10,100,.3,0,-1).status==cppunivariate::GevStatus::InvalidData);
}


// Generating mpmath snippet (binary64 inputs retained exactly):
// import mpmath as mp; mp.mp.dps=100
// xi=mp.mpf(0.3)
// for lp in [-800,-10000]:
//     y=-mp.log(-mp.log1p(-mp.exp(lp)))
//     print(mp.expm1(xi*y)/xi)
TEST_CASE("gev: targeted tail and endpoint regressions") {
  CHECK(cppunivariate::distributions::qgev(-800.0, 0.0, 1.0, 0.3, false, true) == doctest::Approx(5.669625878558571e+104).epsilon(1e-10));
  CHECK(cppunivariate::distributions::qgev(-10000.0, 0.0, 1.0, 0.3, false, true) == INFINITY);
}

// Generating mpmath snippet at the exact binary64 endpoint neighbour:
// import mpmath as mp; mp.mp.dps=100
// m,s,a=map(mp.mpf,[0.0, 0.1, 0.3]); x=mp.mpf(-0.3333333333333333)
// y=mp.log1p(a*(x-m)/s)/a
// print(-mp.log(s)-(1+a)*y-mp.exp(-y), -mp.exp(-y))
TEST_CASE("gev: exact binary64 finite-endpoint neighbour") {
  CHECK(cppunivariate::distributions::dgev(-0.3333333333333333, 0.0, 0.1, 0.3, true) == doctest::Approx(-5.827783979826188e+52).epsilon(1e-10));
  CHECK(cppunivariate::distributions::pgev(-0.3333333333333333, 0.0, 0.1, 0.3, true, true) == doctest::Approx(-5.827783979826188e+52).epsilon(1e-10));
}


TEST_CASE("gev: P3 restrictions and closed faces") {
 const std::vector<double> x{0,1,2,3,4};const auto data=mltest::as_span(x);
 const double nan=std::numeric_limits<double>::quiet_NaN();
 const auto h=cppunivariate::gev_mle(data,1e-10,100,-1);
 REQUIRE(h.status==cppunivariate::GevStatus::Ok); CHECK(h.mu==2);CHECK(h.sigma==2);
 CHECK(h.logLik==doctest::Approx(-5*(std::log(2.0)+1)));CHECK(h.restricted==0u);
 const auto s=cppunivariate::gev_mle(data,1e-10,100,-1,nan,3);
 CHECK(s.mu==doctest::Approx(1));CHECK(std::isfinite(s.logLik));
 const auto m=cppunivariate::gev_mle(data,1e-10,100,-1,0,nan);
 CHECK(m.sigma==4);CHECK(std::isfinite(m.logLik));
 const auto f=cppunivariate::gev_mle(data);REQUIRE(f.status==cppunivariate::GevStatus::Ok);
 CHECK(f.xi==-1);CHECK(f.restricted==16u);
}
TEST_CASE("gev: held shapes outside free restrictions retain proper finite laws") {
 const std::vector<double> x{0,1,2,3,4};
 const auto r=cppunivariate::gev_mle(mltest::as_span(x),1e-10,100,-2,10,5);
 CHECK(r.status==cppunivariate::GevStatus::Ok);CHECK(r.xi==-2);
 CHECK(std::isfinite(r.logLik));CHECK(r.restricted==0u);
}

// TASK-85/87: with mu and sigma both free, a minimum tied exactly at
// n/2 used to be rejected outright (InvalidData), the same as a strict
// majority tie -- conservative, since unlike a strict majority this is
// a genuinely data-dependent case: the xi->1 collapse face has a finite
// but unattained supremum (A_free = n*log(2) - n - 2*sum_{x_i>m} log(x_i-m),
// m=min(x); notes/evaluations/2026-10-29-p0-extreme.md), and an interior
// (mu, sigma, xi) point can in principle beat it (the mu-held analogue,
// "gev: only minimum ties control fixed-shape collapse" above, does
// allow both outcomes). In extensive testing for this report (>7000
// samples: every case below, plus random and handcrafted datasets
// spanning several orders of magnitude and shapes, free mu/sigma/xi),
// the interior search never once beat A_free -- every k=n/2 sample
// gave NoMLE, with the found optimum converging to A_free as xi->1 to
// within this search's own approach tolerance. No proof that A_free is
// *always* the free-location supremum at this exact tie fraction was
// found or attempted; this records strong, not conclusive, evidence
// for it. The check below is therefore written to accept either
// outcome (CLAUDE.md's rule against assuming more than is proved),
// not because both are expected to occur in practice.
TEST_CASE("gev: free fit compares half-minimum ties with the free collapse face") {
  for (auto x : {std::vector<double>{0,0,1,2}, std::vector<double>{0,0,.01,100},
                 std::vector<double>{0,0,0,1,2,5}}) {
    const double m = *std::min_element(x.begin(), x.end());
    double sum_log_d = 0;
    for (double v : x) if (v > m) sum_log_d += std::log(v - m);
    const double a_free = x.size()*std::log(2.0) - static_cast<double>(x.size()) - 2.0*sum_log_d;
    const auto f = cppunivariate::gev_mle(mltest::as_span(x));
    CAPTURE(m);
    if (f.status == cppunivariate::GevStatus::Ok) {
      CHECK(f.logLik > a_free);
      // Score check (CLAUDE.md): a genuinely interior maximum has a
      // near-zero free-coordinate gradient, not just a logLik that
      // happens to beat the face.
      double norm = 0, h = 1e-5;
      for (int j = 0; j < 3; ++j) {
        double p[3] = {f.mu, f.sigma, f.xi}; p[j] += h;
        const double plus = cppunivariate::gev_loglik(mltest::as_span(x), p[0], p[1], p[2]);
        p[j] -= 2*h;
        const double minus = cppunivariate::gev_loglik(mltest::as_span(x), p[0], p[1], p[2]);
        const double score = (plus - minus) / (2*h);
        norm += score*score;
      }
      CHECK(std::sqrt(norm) < 1e-3);
    } else {
      CHECK(f.status == cppunivariate::GevStatus::NoMLE);
    }
  }
  // A strict majority (k=3 of n=4) still diverges outright, unaffected.
  const std::vector<double> majority{0,0,0,1};
  CHECK(cppunivariate::gev_mle(mltest::as_span(majority)).status == cppunivariate::GevStatus::InvalidData);
  // Odd n: k=n/2 is never an integer, so the ordinary (strict) test
  // applies throughout; confirm the free fit still succeeds for a
  // nonconstant odd-n sample with fewer than half the points tied.
  const std::vector<double> odd{0,0,1,2,5};
  CHECK(cppunivariate::gev_mle(mltest::as_span(odd)).status == cppunivariate::GevStatus::Ok);
}

TEST_CASE("gev R7a finite endpoint closed form") {
  // xi=-1: GPD is uniform [mu,mu+sigma]; GEV has
  // f(x)=exp((x-mu)/sigma-1)/sigma for x<=mu+sigma.
  for (bool logarithm : {false,true}) {
    CHECK(cppunivariate::distributions::dgev(5,2,3,-1,logarithm) ==
          doctest::Approx(logarithm ? -std::log(3.0) : 1.0/3));
    CHECK(cppunivariate::distributions::dgev(5,2,3,-0.5,logarithm) ==
          doctest::Approx(logarithm ? -std::log(3.0)-std::log(2.0)-0.25 : 1.0/6*std::exp(-0.25)));
    for (bool lower : {false,true}) {
      CHECK(cppunivariate::distributions::pgev(5,2,3,-1,lower,logarithm) ==
            (logarithm ? (lower ? 0 : -INFINITY) : (lower ? 1 : 0)));
      CHECK(cppunivariate::distributions::qgev(logarithm ? (lower ? 0 : -INFINITY) : (lower ? 1 : 0),2,3,-1,lower,logarithm) == 5);
    }
  }
}

TEST_CASE("gev xi minus one is the closed form limit law") {
  // 0.1+0.7 rounds inside the exact binary64 support: the declared endpoint
  // still has F=1 and S=0, including on the log scale.
  for (bool lower : {false,true}) for (bool log : {false,true})
    CHECK(cppunivariate::distributions::pgev(.1+.7,.1,.7,-1,lower,log)==
          (log ? (lower?0:-INFINITY) : (lower?1:0)));

  // GPD xi=-1 is uniform; GEV xi=-1 is mu+sigma-sigma*Exp(1).
  struct HalfRng { double runif(double,double) { return .5; } } rng;
  CHECK(cppunivariate::distributions::rgev(rng,2,3,-1)==doctest::Approx(5+3*std::log(.5)));
  for (double lp : {-10000.,-10.,-1.,-.1}) {
    const double x=cppunivariate::distributions::qgev(lp,2,3,-1,true,true);
    CHECK(x==doctest::Approx(5+3*lp));
    if (lp > -100) CHECK(cppunivariate::distributions::pgev(x,2,3,-1,true,true)==doctest::Approx(lp));
  }
  for (bool log : {false,true}) {
    CHECK(cppunivariate::distributions::dgev(8,2,3,-.5,log)==(log?-INFINITY:0));
    CHECK(cppunivariate::distributions::dgev(3.5,2,3,-2,log)==INFINITY);
  }
}
