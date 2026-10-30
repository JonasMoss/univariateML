// Location-scale Student t, constrained to df >= 1.
#pragma once
#include "../../core/multistart.hpp"
#include "../../core/bfgs.hpp"
#include "../../core/span.hpp"
#include "../cauchy/mle.hpp"
#include "../norm/mle.hpp"
#include "../../special/special.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

namespace cppunivariate {
inline constexpr double kLstDfLower = 1.0;

enum class LstStatus { Ok, BfgsFailure, InvalidStart, NoMLE, NormalBoundary, InvalidDf, Unbounded, ScaleBoundary, IterationFailure };
struct LstResult {
  double mu, sigma, df, logLik;
  int iterations;
  LstStatus status; unsigned restricted = 0;
};

// Coordinates are location, log(scale), log(df-1); fixed_df includes Cauchy.
struct LstCost {
  span x;
  double fixed_df = 0.0;
  bool exact_fixed = false;
  int dim() const { return fixed_df > 0.0 ? 2 : 3; }
  double operator()(const double *p) const {
    const double s = std::exp(p[1]);
    const double df = fixed_df > 0.0 ? fixed_df : 1.0 + std::exp(p[2]);
    if (!(s > 0.0) || !std::isfinite(s) || !std::isfinite(df) || (!exact_fixed && df > 1e7))
      return std::numeric_limits<double>::infinity();
    // Gamma ratio expansion avoids cancellation on the normal approach.
    const double c = df > 1e4 ? -0.5 * std::log(2.0 * M_PI) - 0.25 / df :
      cppunivariate::special::lgamma((df + 1.0) / 2.0) -
      cppunivariate::special::lgamma(df / 2.0) - 0.5 * std::log(df * M_PI);
    double f = 0.0;
    for (double v : x) {
      const double z = (v - p[0]) / s;
      f += p[1] - c + 0.5 * (df + 1.0) * std::log1p(z * z / df);
    }
    return f;
  }
  void gradient(const double *p, double *g) const {
    const double s = std::exp(p[1]);
    const double df = fixed_df > 0.0 ? fixed_df : 1.0 + std::exp(p[2]);
    g[0] = g[1] = 0.0;
    if (dim() == 3) g[2] = 0.0;
    const double dc = df > 1e4 ? 0.25 / (df * df) :
      0.5 * (cppunivariate::special::digamma((df + 1.0) / 2.0) -
             cppunivariate::special::digamma(df / 2.0) - 1.0 / df);
    for (double v : x) {
      const double z = (v - p[0]) / s;
      const double r = z * z / (df + z * z);
      g[0] -= (df + 1.0) * z / (s * (df + z * z));
      g[1] += 1.0 - (df + 1.0) * r;
      if (dim() == 3)
        g[2] += (df - 1.0) * (-dc + 0.5 * std::log1p(z * z / df) -
                               0.5 * (df + 1.0) * r / df);
    }
  }
};

// Fixed-df EM: conditional normal weights, followed by their exact weighted
// location/scale M-step. Each update increases the observed likelihood.
// Strict mass inequality gives a regular attained fit for df >= 1.
inline LstResult lst_mle_fixed_df(span x, double df, double tol, int maxit) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  if (!std::isfinite(df) || df <= 0.0)
    return {nan,nan,df,nan,0,LstStatus::InvalidDf};
  const std::size_t n = x.size();
  if (n < 2) return {nan,nan,df,nan,0,LstStatus::Unbounded};
  std::vector<double> y;
  for (double v : x) {
    if (!std::isfinite(v)) return {nan,nan,df,nan,0,LstStatus::InvalidStart};
    y.push_back(v);
  }
  std::sort(y.begin(), y.end());
  std::size_t run = 0, largest = 0;
  for (std::size_t i = 0; i < n; ++i) {
    run = i && y[i] == y[i-1] ? run + 1 : 1;
    largest = std::max(largest, run);
  }
  const double other = df * static_cast<double>(n-largest);
  if (largest > other) return {nan,nan,df,INFINITY,0,LstStatus::Unbounded};
  if (largest == other) {
    if (df == 1.0) {
      const auto r = fit_cauchy(x, tol, maxit);
      if (r.status == CauchyStatus::Ok)
        return {r.mu,r.scale,df,r.logLik,r.iterations,LstStatus::Ok};
    }
    return {nan,nan,df,nan,0,LstStatus::ScaleBoundary};
  }
  const double center = y[n/2];
  // Range works even when the IQR vanishes in an admissible tied sample.
  const double unit = y.back()-y.front();
  if (!(unit > 0.0) || !std::isfinite(unit))
    return {nan,nan,df,nan,0,LstStatus::InvalidStart};
  for (double &v : y) v = (v-center)/unit;
  LstCost cost{{y.data(),y.size()},df,true};
  double mu = 0.0, scale = 0.5;
  for (int it = 0; it <= maxit; ++it) {
    const double p[2] = {mu,std::log(scale)};
    double g[2];
    cost.gradient(p,g);
    // Location score is measured in units of the current scale, making
    // the stopping rule affine invariant; log-scale score is dimensionless.
    const double score = std::hypot(scale*g[0],g[1]);
    if (std::isfinite(score) && score <= tol)
      return {center+unit*mu,unit*scale,df,-cost(p)-n*std::log(unit),it,LstStatus::Ok};
    if (it == maxit) break;
    double sw = 0.0, sx = 0.0;
    for (double v : y) {
      const double z = (v-mu)/scale;
      const double w = (1.0+1.0/df)/(1.0+z*z/df);
      sw += w; sx += w*v;
    }
    const double next = sx/sw;
    double ss = 0.0;
    for (double v : y) {
      const double z = (v-mu)/scale;
      ss += (1.0+1.0/df)/(1.0+z*z/df)*(v-next)*(v-next)/n;
    }
    mu = next; scale = std::sqrt(ss);
    if (!(scale > 0.0) || !std::isfinite(mu) || !std::isfinite(scale)) break;
  }
  return {nan,nan,df,nan,maxit,LstStatus::IterationFailure};
}

inline LstResult lst_mle_impl(span x, double tol, int maxit) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n < 3) return {nan,nan,nan,nan,0,LstStatus::NoMLE};
  std::vector<double> y;
  for (double v : x) {
    if (!std::isfinite(v)) return {nan,nan,nan,nan,0,LstStatus::InvalidStart};
    y.push_back(v);
  }
  std::sort(y.begin(), y.end());
  std::size_t run = 1;
  for (std::size_t i = 1; i < n; ++i) {
    run = y[i] == y[i-1] ? run + 1 : 1;
    if (2 * run >= n) return {nan,nan,nan,nan,0,LstStatus::NoMLE};
  }
  // Robust affine standardisation keeps the optimisation meaningful for
  // narrow clusters and heavy tails whose sample variance is enormous.
  const double center = y[n/2];
  const double unit = y[3*n/4] - y[n/4];
  if (!(unit > 0.0) || !std::isfinite(unit))
    return {nan,nan,nan,nan,0,LstStatus::InvalidStart};
  double mean = 0.0;
  for (double &v : y) { v = (v-center)/unit; mean += v / n; }
  double variance = 0.0;
  for (double v : y) variance += (v-mean)*(v-mean)/n;
  const double normal = -0.5*n*(std::log(2.0*M_PI*variance)+1.0);
  BfgsControl ctl;
  ctl.grad_tol = tol;
  ctl.maxit = maxit;
  BfgsResult best;
  best.f = INFINITY;
  double best_df = 1.0;
  BfgsResult cauchy;
  cauchy.f = INFINITY;
  // Profile grid, multiple nuisance starts, then joint refinement of each
  // grid candidate. Neither EM nor this joint search certifies uniqueness.
  for (double df : {1.0, 1.25, 1.5, 2.0, 3.0, 5.0, 10.0, 30.0, 100.0, 1000.0}) {
    LstCost fixed{{y.data(),y.size()},df};
    BfgsResult profile;
    profile.f = INFINITY;
    for (double mu : {mean, y[n/4], y[n/2], y[3*n/4]}) {
      double s = 1.0;
      for (int j = 0; j < maxit; ++j) {
        double sw = 0.0, sx = 0.0;
        for (double v : y) {
          const double z = (v-mu)/s;
          const double w = (df+1.0)/(df+z*z);
          sw += w; sx += w*v;
        }
        const double next = sx/sw;
        double ss = 0.0;
        for (double v : y) {
          const double z = (v-mu)/s;
          ss += (df+1.0)/(df+z*z)*(v-next)*(v-next)/n;
        }
        mu = next; s = std::sqrt(ss);
      }
      const double p[2] = {mu,std::log(s)};
      auto r = bfgs_minimize(fixed,p,ctl);
      consider_bfgs_candidate(profile,r);
    }
    if (df == 1.0) cauchy = profile;
    if (profile.theta.empty()) continue;
    if (df > 1.0) {
      LstCost joint{{y.data(),y.size()}};
      const double p[3] = {profile.theta[0],profile.theta[1],std::log(df-1.0)};
      auto r = bfgs_minimize(joint,p,ctl);
      if (consider_bfgs_candidate(best,r)) best_df = 1.0+std::exp(r.theta[2]);
    }
  }
  const double tie = 1e-8*n;
  if (std::isfinite(cauchy.f) && cauchy.f <= best.f + tie) { best = cauchy; best_df = 1.0; }
  if (best.theta.empty()) return {nan,nan,nan,nan,0,LstStatus::BfgsFailure};
  // Prefer an attained finite member in numerical ties, except on the
  // asymptotic normal approach (df >= 1e5 has no resolved finite optimum).
  if (normal > -best.f + tie || (best_df >= 1e5 && normal >= -best.f-tie))
    return {center+unit*mean,unit*std::sqrt(variance),INFINITY,
            normal-n*std::log(unit),best.iterations,LstStatus::NormalBoundary};
  return {center+unit*best.theta[0],unit*std::exp(best.theta[1]),best_df,
          -best.f-n*std::log(unit),best.iterations,
          best.status == BfgsStatus::Ok ? LstStatus::Ok : LstStatus::BfgsFailure};
}

namespace lst_detail {
// Refine every grid-local maximum, retaining the endpoints as candidates.
// The grid nominates modes numerically; it is not an interval certificate.
struct Maximum {double point;bool converged;};
template <typename F>
inline Maximum maximize(F f,const std::vector<double>& grid,double tol,int maxit) {
  double best=grid.front(),value=f(best);bool converged=grid.size()==1;
  for(std::size_t j=0;j<grid.size();++j) {
    const double v=f(grid[j]);if(v>value){value=v;best=grid[j];converged=false;}
    if((j&&v<f(grid[j-1]))||(j+1<grid.size()&&v<f(grid[j+1])))continue;
    double lo=grid[j?j-1:0],hi=grid[j+1<grid.size()?j+1:j];
    double a=hi-(hi-lo)*.6180339887498948482,b=lo+(hi-lo)*.6180339887498948482;
    double fa=f(a),fb=f(b);
    for(int it=0;it<maxit&&hi-lo>tol;++it) {
      if(fa>fb){hi=b;b=a;fb=fa;a=hi-(hi-lo)*.6180339887498948482;fa=f(a);}
      else{lo=a;a=b;fa=fb;b=lo+(hi-lo)*.6180339887498948482;fb=f(b);}
    }
    if(best==grid[j]&&hi-lo<=tol)converged=true;
    if(std::max(fa,fb)>value){value=std::max(fa,fb);best=fa>fb?a:b;converged=hi-lo<=tol;}
  }
  return {best,converged};
}
inline LstResult conditional(span x,double df,double mu,double sigma,double tol,int maxit) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  LstResult bad{nan,nan,df,nan,0,LstStatus::InvalidStart};
  if(x.empty()||!(tol>0)||maxit<1||!std::isfinite(df)||!(df>0))return bad;
  std::vector<double> y(x.begin(),x.end());for(double v:y)if(!std::isfinite(v))return bad;std::sort(y.begin(),y.end());
  auto ll=[&](double m,double s){const double p[]={m,std::log(s)};return -LstCost{x,df,true}(p);};
  double m=mu,s=sigma;
  if(std::isnan(s)) {
    std::size_t k=0;double r=0;for(double v:y){k+=v==m;r=std::max(r,std::abs(v-m));}
    if(k>df*(y.size()-k)){bad.status=LstStatus::Unbounded;return bad;}
    // At equality every nonzero residual contributes a strict factor below
    // its collapse limit, so the supremum is never attained at held mu.
    if(k==df*(y.size()-k)){bad.status=LstStatus::ScaleBoundary;return bad;}
    double lo=std::max(std::log(r)-350,std::log(std::numeric_limits<double>::denorm_min())),hi=std::log(r)+2;
    for(int it=0;it<maxit;++it){const double t=lo/2+hi/2;s=std::exp(t);double g=-static_cast<double>(y.size());for(double v:y){const double q=((v-m)/s)/std::sqrt(df),q2=q*q;const double weight=q2<=1?q2/(1+q2):1/(1+1/q2);g+=(df+1)*weight;}if(g>0)lo=t;else hi=t;if(hi-lo<1e-12)break;}
    if(hi-lo>1e-9){bad.status=LstStatus::IterationFailure;return bad;}
    s=std::exp(lo/2+hi/2);
  } else if(std::isnan(m)) {
    std::vector<double> grid=y;
    for(std::size_t j=1;j<y.size();++j)for(int k=1;k<8;++k)grid.push_back(y[j-1]+(y[j]-y[j-1])*k/8);
    for(double v:y)for(double d:{-.5,.5})if(v+d*s>y.front()&&v+d*s<y.back())grid.push_back(v+d*s);
    std::sort(grid.begin(),grid.end());grid.erase(std::unique(grid.begin(),grid.end()),grid.end());
    const auto optimum=maximize([&](double a){return ll(a,s);},grid,1e-10*s,maxit);
    if(!optimum.converged){bad.status=LstStatus::IterationFailure;return bad;}
    m=optimum.point;
  }
  return {m,s,df,ll(m,s),0,LstStatus::Ok};
}
}
inline LstResult lst_mle_impl(span x,double tol,int maxit,double mu,double sigma,double df) {
  if(std::isnan(mu)&&std::isnan(sigma))return std::isnan(df)?lst_mle_impl(x,tol,maxit):lst_mle_fixed_df(x,df,tol,maxit);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  if(!std::isnan(df)) {
    if(!std::isfinite(df)||df<=0)return {nan,nan,df,nan,0,LstStatus::InvalidDf};
    return lst_detail::conditional(x,df,mu,sigma,tol,maxit);
 }
  if(x.empty())return {nan,nan,nan,nan,0,LstStatus::InvalidStart};
  // With free scale and held location the lowest df has the weakest
  // collapse exponent; the equality supremum is the df=1 collapse face.
  const double floor=std::isnan(mu) ? 1.0 : 0.0;
  double collapse=-INFINITY;
  if(std::isnan(sigma)) {
    std::size_t k=0;for(double v:x)k+=v==mu;
    if((floor==0 && k>0) || 2*k>x.size())return {mu,nan,1,INFINITY,0,LstStatus::Unbounded};
    if(2*k==x.size()){collapse=-static_cast<double>(x.size())*std::log(M_PI);for(double v:x)if(v!=mu)collapse-=2*std::log(std::abs(v-mu));}
  }
  auto profile=[&](double t){return lst_detail::conditional(x,floor+std::exp(t),mu,sigma,tol,maxit);};
  std::vector<double> grid;for(double t=-12;t<=16;t+=.5)grid.push_back(t);
  const auto optimum=lst_detail::maximize([&](double a){return profile(a).logLik;},grid,1e-8,maxit);
  if(!optimum.converged)return {nan,nan,nan,nan,maxit,LstStatus::IterationFailure};
  auto best=profile(optimum.point);const auto edge=lst_detail::conditional(x,1,mu,sigma,tol,maxit);
  const double tie=1e-8*x.size();if(floor==1 && edge.status==LstStatus::Ok&&edge.logLik>=best.logLik-tie)best=edge;
  const auto normal=norm_mle(x,mu,sigma);
  if(normal.logLik>best.logLik+tie||(best.df>1e5&&normal.logLik>=best.logLik-tie))return {normal.mean,normal.sd,INFINITY,normal.logLik,0,LstStatus::NormalBoundary};
  if(collapse>=best.logLik-tie)return {mu,0,1,collapse,0,LstStatus::ScaleBoundary};
  return best;
}

inline LstResult lst_mle(span x,double tol,int maxit) {
  auto r=lst_mle_impl(x,tol,maxit);
  if(r.status==LstStatus::Ok && r.df==kLstDfLower) r.restricted|=16u;
  return r;
}
inline LstResult lst_mle(span x,double tol,int maxit,double mu,double sigma,double df) {
  auto r=lst_mle_impl(x,tol,maxit,mu,sigma,df);
  if(r.status==LstStatus::Ok && (std::isnan(df) && std::isnan(mu)) && r.df==kLstDfLower) r.restricted|=16u;
  return r;
}
} // namespace cppunivariate
