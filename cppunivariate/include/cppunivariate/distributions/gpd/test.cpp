#include "test_support.hpp"
#include "cppunivariate/distributions/gpd/dpqr.hpp"
#include <vector>
#include <random>
#include <cmath>

TEST_CASE("gpd: golden") {
  auto g=mltest::load_distribution_golden("gpd");
  auto r=cppunivariate::gpd_mle(mltest::as_span(g.data));
  CHECK(r.status==cppunivariate::GpdStatus::Ok);
  CHECK(r.mu==g.estimates.at("mu"));
  CHECK(r.sigma==doctest::Approx(g.estimates.at("sigma")).epsilon(1e-12));
  CHECK(r.xi==doctest::Approx(g.estimates.at("xi")).epsilon(1e-12));
  CHECK(r.logLik==doctest::Approx(g.logLik).epsilon(1e-12));
}
TEST_CASE("gpd: shape regimes, scores, fixed shape and threshold") {
  for(double xi : {0.3,0.0,-0.3,-0.8}) {
    std::vector<double> y;
    for(int i=0;i<400;++i)
      y.push_back(cppunivariate::distributions::qgpd((i+.5)/400,0,2,xi,true,false));
    auto r=cppunivariate::gpd_mle(mltest::as_span(y));
    REQUIRE(r.status==cppunivariate::GpdStatus::Ok);
    const double h=1e-5;
    auto ll=[&](double s,double k) {return cppunivariate::gpd_loglik(mltest::as_span(y),s,k);};
    CHECK(std::abs((ll(r.sigma+h,r.xi)-ll(r.sigma-h,r.xi))/(2*h))<1e-4);
    if(xi==-.8) {
      CHECK(r.xi==-.5);
      CHECK((ll(r.sigma,r.xi+h)-ll(r.sigma,r.xi))/h<0);
    } else {
      CHECK(r.xi>-.5);
      CHECK(std::abs((ll(r.sigma,r.xi+h)-ll(r.sigma,r.xi-h))/(2*h))<1e-3);
    }
    auto fixed=cppunivariate::gpd_mle(mltest::as_span(y),1e-12,200,0,xi<-.5 ? -.5 : xi);
    CHECK(fixed.status==cppunivariate::GpdStatus::Ok);
    CHECK(fixed.xi==(xi<-.5 ? -.5 : xi));
    for(double &v:y) v+=10;
    auto shifted=cppunivariate::gpd_mle(mltest::as_span(y),1e-12,200,10);
    CHECK(shifted.sigma==doctest::Approx(r.sigma).epsilon(1e-9));
    CHECK(shifted.xi==doctest::Approx(r.xi).epsilon(1e-8));
  }
}
TEST_CASE("gpd: exponential stability and invalid inputs") {
  std::vector<double> y{.2,.7,1,2,4};
  auto zero=cppunivariate::gpd_mle(mltest::as_span(y),1e-12,200,0,0);
  CHECK(zero.sigma==doctest::Approx(1.58));
  for(double xi:{-1e-16,1e-16,-1e-9,1e-9}) {
    auto r=cppunivariate::gpd_mle(mltest::as_span(y),1e-12,200,0,xi);
    CHECK(r.sigma==doctest::Approx(zero.sigma).epsilon(1e-8));
    CHECK(r.logLik==doctest::Approx(zero.logLik).epsilon(1e-8));
    CHECK(cppunivariate::distributions::qgpd(.5,0,1,xi,true,false)==doctest::Approx(std::log(2.)).epsilon(1e-8));
  }
  CHECK(cppunivariate::gpd_mle(mltest::as_span(y),1e-12,200,1).status==cppunivariate::GpdStatus::InvalidData);
  CHECK(cppunivariate::gpd_mle(mltest::as_span(y),1e-12,200,0,-.8).status==cppunivariate::GpdStatus::InvalidShape);
  CHECK(cppunivariate::gpd_mle(mltest::as_span(y),1e-12,1).status==cppunivariate::GpdStatus::IterationFailure);
  CHECK(cppunivariate::distributions::pgpd(1e-20,0,1,0,true,false)==doctest::Approx(1e-20));
  CHECK(cppunivariate::distributions::pgpd(1000,0,1,0,false,true)==-1000);
}

// Frozen exhaustive 4096-cell long-double implementation (pre-optimization).
namespace cppunivariate { namespace gpd_reference {
enum class GpdStatus { Ok, InvalidData, InvalidShape, IterationFailure };
struct GpdResult {
  double mu, sigma, xi, logLik;
  int iterations;
  GpdStatus status;
};
// log1p(xi*z)/xi, including the interior exponential member.
inline double reference_gpd_log_ratio(double z, double xi) {
  const double v = xi*z;
  if (std::abs(v) < 1e-5)
    return z*(1-v/2+v*v/3-v*v*v/4+v*v*v*v/5);
  return std::log1p(v)/xi;
}
inline double reference_gpd_loglik(span y, double sigma, double xi) {
  double ll = -static_cast<double>(y.size())*std::log(sigma);
  for (double v : y) {
    if (!(sigma > 0) || !(1+xi*(v/sigma) > 0))
      return -std::numeric_limits<double>::infinity();
    ll -= (1+xi)*reference_gpd_log_ratio(v/sigma, xi);
  }
  return ll;
}
// For fixed xi > -1 the scale score is strictly decreasing in sigma
// after multiplication by sigma: (1+xi)*mean(y/(sigma+xi*y))-1.
inline GpdResult reference_gpd_scale(span y, double mu, double xi, double tol, int maxit) {
  double m = 0, M = 0;
  for (double v : y) { m += v/static_cast<double>(y.size()); M=std::max(M,v); }
  if (xi == 0) return {mu,m,xi,reference_gpd_loglik(y,m,xi),0,GpdStatus::Ok};
  double lo = std::max(0.0,-xi*M), hi = std::max(M,lo*2);
  auto score = [&](double s) {
    long double v=0;
    for (double z : y) v += (1+static_cast<long double>(xi))*z/(s+static_cast<long double>(xi)*z);
    return v/y.size()-1;
  };
  while (score(hi)>0 && std::isfinite(hi)) hi*=2;
  int it=0;
  for (; it<maxit; ++it) {
    const double mid=lo+(hi-lo)/2;
    if (score(mid)>0) lo=mid; else hi=mid;
    if (hi-lo <= tol*hi) break;
  }
  const double s=lo+(hi-lo)/2;
  return {mu,s,xi,reference_gpd_loglik(y,s,xi),it+1,
          it<maxit ? GpdStatus::Ok : GpdStatus::IterationFailure};
}
inline GpdResult gpd_mle(span x, double tol=1e-12, int maxit=200,
                         double mu=0, double xi=std::numeric_limits<double>::quiet_NaN()) {
  GpdResult bad{mu,0,xi,0,0,GpdStatus::InvalidData};
  if (x.size()==0 || !std::isfinite(mu) || !(tol>0) || maxit<1) return bad;
  std::vector<double> y;
  double M=0, minimum=std::numeric_limits<double>::infinity();
  for (double v : x) {
    const double z=v-mu;
    if (!(z>0) || !std::isfinite(z)) return bad;
    y.push_back(z); M=std::max(M,z); minimum=std::min(minimum,z);
  }
  const span ys{y.data(),y.size()};
  if (!std::isnan(xi)) {
    if (!std::isfinite(xi) || xi<-.5) { bad.status=GpdStatus::InvalidShape; return bad; }
    return reference_gpd_scale(ys,mu,xi,tol,maxit);
  }
  auto best=reference_gpd_scale(ys,mu,0,tol,maxit);
  auto floor=reference_gpd_scale(ys,mu,-.5,tol,maxit);
  if (floor.status!=GpdStatus::Ok) return floor;
  if (floor.logLik>best.logLik) best=floor;
  // Grimshaw profile: differentiating in xi at fixed a=xi/sigma gives
  // xi=L=mean(log1p(a*y)); the remaining score is (1+L)*Q-1,
  // Q=mean(1/(1+a*y)). Dimensionless b=a*max(y), t=log1p(b).
  auto profile = [&](long double t, bool shape) {
    const long double b=std::expm1(t);
    long double L=0,Q=0;
    if (std::abs(b)<1e-3L && !shape) {
      // Divide the score by b^2, removing its spurious double zero.
      long double moments[13] = {};
      for (double z:y) {
        long double power=1, r=static_cast<long double>(z)/M;
        for (int k=1;k<=12;++k) { power*=r; moments[k]+=power/y.size(); }
      }
      long double sum=0,power=1;
      for(int k=2;k<=12;++k) {
        long double c=moments[k];
        for(int j=1;j<=k;++j) c-=moments[j]*moments[k-j]/j;
        // moments[0]=0 above: the L term must also be included.
        c-=moments[k]/k;
        sum+=(k%2 ? -1:1)*c*power; power*=b;
      }
      return sum;
    }
    for(double z:y) { const long double v=b*(static_cast<long double>(z)/M); L+=std::log1p(v)/y.size(); Q+=1/(1+v)/y.size(); }
    return shape ? L : ((1+L)*Q-1)/(b*b);
  };
  // L is monotone; below L=-1/2 no stationary candidate is admissible.
  long double left=-.5L*y.size(), right=0;
  for(int j=0;j<100;++j) { auto mid=(left+right)/2; if(profile(mid,true)<-.5L) left=mid; else right=mid; }
  const long double lower=right;
  long double upper=1;
  // Beyond this bound (1+L)Q < 1 follows from min(y), max(y).
  while (1+upper >= 1+std::expm1(upper)*(static_cast<long double>(minimum)/M)) upper*=2;
  bool root_failure=false;
  auto scan=[&](long double low,long double high) {
    const int cells=4096;
    long double prev=low, fprev=profile(prev,false);
    for(int j=1;j<=cells;++j) {
      long double next=low+(high-low)*j/cells, fnext=profile(next,false);
      if ((fprev<0 && fnext>0)||(fprev>0 && fnext<0)) {
        long double l=prev,h=next,fl=fprev;
        int it=0;
        for(;it<maxit;++it) {
          auto mid=(l+h)/2, fm=profile(mid,false);
          if((fl<0)==(fm<0)) {l=mid;fl=fm;} else h=mid;
          if(h-l<tol*(1+std::abs(mid))) break;
        }
        if(it==maxit) { root_failure=true; return; }
        const auto t=(l+h)/2;
        const double shape=static_cast<double>(profile(t,true));
        const double scale=static_cast<double>(static_cast<long double>(shape)*M/std::expm1(t));
        const double ll=reference_gpd_loglik(ys,scale,shape);
        if(shape>=-.5 && ll>best.logLik) best={mu,scale,shape,ll,it+1,GpdStatus::Ok};
      }
      prev=next; fprev=fnext;
    }
  };
  scan(lower,-1e-12L); scan(1e-12L,upper);
  if (root_failure) best.status=GpdStatus::IterationFailure;
  return best;
}
} // namespace cppunivariate

} // namespace cppunivariate

TEST_CASE("gpd: seeded exhaustive profile regression") {
  std::mt19937_64 engine(20261028);
  for(double xi : {-0.9,-0.6,-0.4,-0.1,0.,0.2,0.5,1.,2.})
    for(int n : {5,10,20,50,200})
      for(int replicate=0;replicate<5;++replicate) {
        std::vector<double> x;
        for(int i=0;i<n;++i) {
          const double u=(static_cast<double>(engine() >> 11)+0.5)/9007199254740992.;
          x.push_back(cppunivariate::distributions::qgpd(u,0,1,xi,true,false));
        }
        const auto fit=cppunivariate::gpd_mle(mltest::as_span(x));
        const auto ref=cppunivariate::gpd_reference::gpd_mle(mltest::as_span(x));
        CAPTURE(xi); CAPTURE(n); CAPTURE(replicate);
        CHECK(fit.status==cppunivariate::GpdStatus::Ok);
        CHECK(std::abs(fit.logLik-ref.logLik)<1e-9);
      }
}

TEST_CASE("gpd: free shape crosses the exponential member") {
  // Tune exponential quantiles to mean(x^2)=2*mean(x)^2, then
  // perturb towards either side. The free-shape root lies in the series.
  auto sample=[](double power) {
    std::vector<double> x;
    for(int i=0;i<400;++i) x.push_back(std::pow(-std::log((i+.5)/400),power));
    return x;
  };
  double lo=.9,hi=1.1;
  for(int j=0;j<50;++j) {
    const double mid=(lo+hi)/2;
    const auto x=sample(mid);
    double m=0,m2=0;
    for(double v:x) {m+=v/x.size();m2+=v*v/x.size();}
    if(m2<2*m*m) lo=mid; else hi=mid;
  }
  for(double delta : {-1e-6,0.,1e-6}) {
    const auto x=sample((lo+hi)/2+delta);
    const auto fit=cppunivariate::gpd_mle(mltest::as_span(x));
    const auto ref=cppunivariate::gpd_reference::gpd_mle(mltest::as_span(x));
    CHECK(fit.status==cppunivariate::GpdStatus::Ok);
    CHECK(std::abs(fit.logLik-ref.logLik)<1e-9);
    CHECK(std::abs(fit.xi/fit.sigma)*x.front()<1e-3);
    if(delta<0) CHECK(fit.xi<0);
    if(delta>0) CHECK(fit.xi>0);
  }
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("gpd: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("gpd",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dgpd(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pgpd(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qgpd(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rgpd(rng, p[0], p[1], p[2]); });
}

TEST_CASE("gpd: held scale shape profile and all held") {
 const std::vector<double> x{.1,.3,.5,1,2,4};const auto data=mltest::as_span(x);
 const double nan=std::numeric_limits<double>::quiet_NaN();
 const auto f=cppunivariate::gpd_mle(data,1e-12,200,0,nan,1);
 REQUIRE(f.status==cppunivariate::GpdStatus::Ok);
 CHECK(f.sigma==1);CHECK(f.mu==0);
 const double h=1e-5;
 CHECK(std::abs((cppunivariate::gpd_loglik(data,1,f.xi+h)-cppunivariate::gpd_loglik(data,1,f.xi-h))/(2*h))<1e-3);
 const auto a=cppunivariate::gpd_mle(data,1e-12,200,0,.3,1);
 CHECK(a.status==cppunivariate::GpdStatus::Ok);CHECK(a.xi==.3);CHECK(a.sigma==1);
 CHECK(cppunivariate::gpd_mle(data,1e-12,200,0,-.5,1).status==cppunivariate::GpdStatus::InvalidData);
 CHECK(cppunivariate::gpd_mle(data,1e-12,1,0,nan,1).status==cppunivariate::GpdStatus::IterationFailure);
 const std::vector<double> one{1};
 CHECK(cppunivariate::gpd_mle(mltest::as_span(one),1e-12,200,0,nan,2).status==cppunivariate::GpdStatus::Ok);
}


// Generating mpmath snippet (binary64 inputs retained exactly):
// import mpmath as mp; mp.mp.dps=100
// xi=mp.mpf(0.3); lp=mp.mpf(-690.7755278982137)
// print(mp.expm1(-xi*mp.log1p(-mp.exp(lp)))/xi)
// print(-mp.log(1))  # log right-hand density at mu, sigma=1
TEST_CASE("gpd: targeted tail and endpoint regressions") {
  CHECK(cppunivariate::distributions::dgpd(0.0, 0.0, 1.0, 0.3, true) == doctest::Approx(0.0).epsilon(1e-10));
  CHECK(cppunivariate::distributions::qgpd(-690.7755278982137, 0.0, 1.0, 0.3, true, true) == doctest::Approx(1.0000000000000237e-300).epsilon(1e-10));
}

// Generating mpmath snippet at the exact binary64 endpoint neighbour:
// import mpmath as mp; mp.mp.dps=100
// m,s,a=map(mp.mpf,[0.0, 1.0, -0.42]); x=mp.mpf(2.380952380952381)
// y=mp.log1p(a*(x-m)/s)/a
// print(-mp.log(s)-(1+a)*y, mp.log(-mp.expm1(-y)))
TEST_CASE("gpd: exact binary64 finite-endpoint neighbour") {
  CHECK(cppunivariate::distributions::dgpd(2.380952380952381, 0.0, 1.0, -0.42, true) == doctest::Approx(-51.95184489919618).epsilon(1e-10));
  CHECK(cppunivariate::distributions::pgpd(2.380952380952381, 0.0, 1.0, -0.42, true, true) == doctest::Approx(-1.2569300112272115e-39).epsilon(1e-10));
}

// import mpmath as mp; mp.mp.dps=100
// x=mp.mpf(float.fromhex('0x0.0000000000001p-1022')); sigma=mp.mpf(0.8359420998453796)
// print(mp.log(x)-mp.log(sigma))  # relative correction is below 1e-320
TEST_CASE("gpd: targeted tail subnormal standardization") {
  CHECK(cppunivariate::distributions::pgpd(std::numeric_limits<double>::denorm_min(),0,0.8359420998453796,0.33160253924642696,true,true)
        == doctest::Approx(-744.2608759945272).epsilon(1e-10));
}
