// Location-scale Student t, constrained to df >= 1.
#pragma once
#include "../../core/work_budget.hpp"
#include "certificate.hpp"
#include "location_certificate.hpp"
#include "shape_certificate.hpp"
#include "symmetry_certificate.hpp"
#include "signed_face_certificate.hpp"
#include "../../core/face_compare.hpp"
#include "cppunivariate/core/constants.hpp"
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

enum class LstStatus { Ok, BfgsFailure, InvalidStart, NoMLE, NormalBoundary, InvalidDf, Unbounded, ScaleBoundary, IterationFailure, CertificateBudget, CertificateTie, NumericalFailure, NonUniqueSymmetry };
struct LstResult {
  double mu, sigma, df, logLik;
  int iterations;
  LstStatus status; unsigned restricted = 0; StopDetail stop_detail{}; bool certified = true;
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
    const double c = df > 1e4 ? -0.5 * std::log(2.0 * cppunivariate::pi) - 0.25 / df :
      cppunivariate::special::lgamma((df + 1.0) / 2.0) -
      cppunivariate::special::lgamma(df / 2.0) - 0.5 * std::log(df * cppunivariate::pi);
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
  maxit = static_cast<int>(solver_budget(maxit));
  const double nan = std::numeric_limits<double>::quiet_NaN();
  if (!std::isfinite(df) || df <= 0.0)
    return {nan,nan,df,nan,0,LstStatus::InvalidDf};
  const std::size_t n = x.size();
  if(n<2) {
    if(n==0||!std::isfinite(x[0]))return {nan,nan,df,nan,0,LstStatus::InvalidStart};
    LstResult unbounded{nan,nan,df,INFINITY,0,LstStatus::Unbounded};
    unbounded.stop_detail.loglik_lower=INFINITY;unbounded.stop_detail.loglik_upper=INFINITY;return unbounded;
  }
  std::vector<double> y;
  for (double v : x) {
    if (!std::isfinite(v)) return {nan,nan,df,nan,0,LstStatus::InvalidStart};
    y.push_back(v);
  }
  std::sort(y.begin(), y.end());
  std::size_t run = 0, largest = 0;
  double atom=y.front();
  for (std::size_t i = 0; i < n; ++i) {
    run = i && y[i] == y[i-1] ? run + 1 : 1;
    if(run>largest){largest=run;atom=y[i];}
  }
  // One fused subtraction preserves the sign at binary64 held shapes;
  // separately rounding df*(n-k) can manufacture an exact equality.
  const double exponent=std::fma(df,static_cast<double>(n-largest),-static_cast<double>(largest));
  if(exponent<0) {
    LstResult unbounded{nan,nan,df,INFINITY,0,LstStatus::Unbounded};
    unbounded.stop_detail.loglik_lower=INFINITY;unbounded.stop_detail.loglik_upper=INFINITY;return unbounded;
  }
  if (exponent==0 && df>=1) {
    if (df == 1.0) {
      const auto r = fit_cauchy(x, tol, maxit);
      if(r.status==CauchyStatus::Ok) {
        // The balanced two-point Cauchy maximum is a curve. Rounding its
        // midpoint to an endpoint must not turn a non-maximizer into a fit.
        const auto left=t_cert_detail::I(r.mu)-y.front();
        const auto right=t_cert_detail::I(y.back())-r.mu;
        const auto exact_scale=t_cert_detail::root(left)*t_cert_detail::root(right);
        if(left.valid()&&right.valid()&&left.lo>0&&right.lo>0&&
           exact_scale.valid()&&exact_scale.lo>0&&
           exact_scale.hi-exact_scale.lo<=1e-6*exact_scale.mid()) {
          const double scale=exact_scale.mid(),point[2]={r.mu,std::log(scale)};
          const double ll=-LstCost{x,df,true}(point);
          if(std::isfinite(ll))return {r.mu,scale,df,ll,r.iterations,LstStatus::Ok};
        }
        const double point[2]={r.mu,std::log(r.scale)};
        const double ll=-LstCost{x,df,true}(point);
        LstResult failed{NAN,NAN,df,NAN,r.iterations,LstStatus::NumericalFailure};
        failed.stop_detail.loglik_upper=INFINITY;
        if(std::isfinite(ll)) {
          const double margin=256*std::numeric_limits<double>::epsilon()*n*(1+std::abs(ll));
          failed.stop_detail.candidates.push_back({{r.mu,r.scale,df},ll-margin,ll+margin,StopModel::Lst});
          failed.stop_detail.loglik_lower=ll-margin;
        }
        return failed;
      }
    }
    t_cert_detail::Sum logs;
    for(double value:y)if(value!=atom) {
      const double distance=std::abs(value-atom);
      const double ld=std::isfinite(distance)?std::log(distance):
        special::log_sum_exp(std::log(std::abs(value)),std::log(std::abs(atom)));
      const double allowance=t_cert_detail::eps*(1+std::abs(ld));
      logs.add({ld-allowance,ld+allowance});
    }
    const auto normalizer=t_cert_detail::gamma(df,0)-.5*std::log(2*pi);
    const auto limit=t_cert_detail::I(n)*normalizer+
      t_cert_detail::I(.5*(df+1))*static_cast<double>(n-largest)*t_cert_detail::log(t_cert_detail::I(df))-
      t_cert_detail::I(df+1)*logs.get();
    LstResult boundary{nan,nan,df,nan,0,LstStatus::ScaleBoundary};
    boundary.stop_detail.limit_coef={atom,df};boundary.stop_detail.limit_model=StopModel::TCollapsePath;
    if(!limit.valid()){boundary.status=LstStatus::NumericalFailure;boundary.stop_detail.loglik_upper=INFINITY;return boundary;}
    boundary.logLik=limit.mid();boundary.stop_detail.limit_loglik=limit.mid();
    boundary.stop_detail.loglik_lower=limit.lo;boundary.stop_detail.loglik_upper=limit.hi;return boundary;
  }
  const double center = y[n/2];
  // Range works even when the IQR vanishes in an admissible tied sample.
  const double unit = y.back()-y.front();
  if(!(unit>0)||!std::isfinite(unit)) {
    LstResult failed{NAN,NAN,df,NAN,0,LstStatus::NumericalFailure};
    failed.stop_detail.loglik_upper=INFINITY;
    const double m=y.front()/2+y.back()/2,s=y.back()/2-y.front()/2;
    const double point[2]={m,std::log(s)},ll=-LstCost{x,df,true}(point);
    if(std::isfinite(ll)&&std::isfinite(m)&&std::isfinite(s)&&s>0) {
      const double margin=256*std::numeric_limits<double>::epsilon()*n*(1+std::abs(ll));
      failed.stop_detail.candidates.push_back({{m,s,df},ll-margin,ll+margin,StopModel::Lst});
      failed.stop_detail.loglik_lower=ll-margin;
    }
    return failed;
  }
  if(df<1) {
    const auto proof=exponent>0?t_symmetry_detail::subunit(x,{y.data(),y.size()},df,maxit):t_symmetry_detail::Result{};
    if(proof.outcome==1){
      LstResult tied{NAN,NAN,df,NAN,proof.evaluations,LstStatus::NonUniqueSymmetry};
      tied.stop_detail=proof.detail;return tied;
    }
    const double candidate[2]={center,std::log(unit/2)};
    const double ll=-LstCost{x,df,true}(candidate);
    LstResult stopped{NAN,NAN,df,NAN,proof.evaluations,proof.outcome==2?LstStatus::CertificateBudget:LstStatus::NumericalFailure};
    stopped.stop_detail=proof.detail;stopped.stop_detail.loglik_upper=INFINITY;
    if(std::isfinite(ll)) {
      const double margin=256*std::numeric_limits<double>::epsilon()*(1+std::abs(ll))*n;
      stopped.stop_detail.candidates.push_back({{center,unit/2,df},ll-margin,ll+margin,StopModel::Lst});
      stopped.stop_detail.loglik_lower=ll-margin;
    }
    return stopped;
  }
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
    if (std::isfinite(score) && score <= tol) {
      double fitted_mu=center+unit*mu,fitted_scale=unit*scale;
      if(df>=1 && !t_location_detail::joint_inclusion(x,df,maxit,fitted_mu,fitted_scale)) {
        LstResult failed{NAN,NAN,df,NAN,it,LstStatus::NumericalFailure};
        const double physical[2]={center+unit*mu,std::log(unit*scale)};
        const double ll=-LstCost{x,df,true}(physical);
        failed.stop_detail.loglik_upper=INFINITY;
        if(std::isfinite(ll)) {
          const double margin=256*std::numeric_limits<double>::epsilon()*n*(1+std::abs(ll));
          failed.stop_detail.candidates.push_back({{physical[0],unit*scale,df},ll-margin,ll+margin,StopModel::Lst});
          failed.stop_detail.loglik_lower=ll-margin;
        }
        return failed;
      }
      const double refined[2]={(fitted_mu-center)/unit,std::log(fitted_scale/unit)};
      return {fitted_mu,fitted_scale,df,-cost(refined)-n*std::log(unit),it,LstStatus::Ok};
    }
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
  LstResult failed{nan,nan,df,nan,maxit,LstStatus::IterationFailure};
  failed.stop_detail.loglik_upper=INFINITY;
  const double point[2]={center+unit*mu,std::log(unit*scale)},ll=-LstCost{x,df,true}(point);
  if(std::isfinite(ll)&&std::isfinite(point[0])&&std::isfinite(unit*scale)&&unit*scale>0) {
    const double margin=256*std::numeric_limits<double>::epsilon()*n*(1+std::abs(ll));
    failed.stop_detail.candidates.push_back({{point[0],unit*scale,df},ll-margin,ll+margin,StopModel::Lst});
    failed.stop_detail.loglik_lower=ll-margin;
  }
  return failed;
}

inline LstResult lst_mle_impl(span x, double tol, int maxit) {
  maxit = static_cast<int>(solver_budget(maxit));
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
  const double normal = -0.5*n*(std::log(2.0*cppunivariate::pi*variance)+1.0);
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
  if((face_numerical_tie(normal,-best.f,tie) || face_numerical_tie(-best.f,normal,tie))) return {nan,nan,nan,nan,0,LstStatus::BfgsFailure};
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
  maxit = static_cast<int>(solver_budget(maxit));
  // A profile evaluation can itself perform a complete nuisance search.
  // Cache grid values without changing the nominated maxima or refinements.
  std::vector<double> values(grid.size());
  std::vector<bool> evaluated(grid.size(),false);
  auto at=[&](std::size_t j) {
    if(!evaluated[j]) {values[j]=f(grid[j]);evaluated[j]=true;}
    return values[j];
  };
  double best=grid.front(),value=at(0);bool converged=grid.size()==1&&std::isfinite(value);
  if(!std::isfinite(value))value=-INFINITY;
  for(std::size_t j=0;j<grid.size();++j) {
    const double v=at(j);
    // An unresolved conditional solve is not a peak to refine. In
    // particular, NaN must not poison later finite nominations or trigger
    // a complete refinement at every failed grid point.
    if(!std::isfinite(v))continue;
    if(v>value){value=v;best=grid[j];converged=false;}
    if((j&&v<at(j-1))||(j+1<grid.size()&&v<at(j+1)))continue;
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
inline LstResult conditional(span x,double df,double mu,double sigma,double tol,int maxit,bool certify_location=false) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double nan=std::numeric_limits<double>::quiet_NaN();
  LstResult bad{nan,nan,df,nan,0,LstStatus::InvalidStart};
  bad.stop_detail.loglik_upper=INFINITY;
  if(x.empty()||!(tol>0)||maxit<1||!std::isfinite(df)||!(df>0))return bad;
  std::vector<double> y(x.begin(),x.end());for(double v:y)if(!std::isfinite(v))return bad;std::sort(y.begin(),y.end());
  // Shape is fixed throughout this nuisance search; preserve LstCost's
  // per-observation grouping and input-order reduction exactly.
  const double c=df>1e4 ? -0.5*std::log(2.0*cppunivariate::pi)-0.25/df :
    special::lgamma((df+1.0)/2.0)-special::lgamma(df/2.0)-0.5*std::log(df*cppunivariate::pi);
  auto log_distance=[](double v,double m){
    const double d=std::abs(v-m);
    return std::isfinite(d)?std::log(d):special::log_sum_exp(std::log(std::abs(v)),std::log(std::abs(m)));
  };
  auto ll=[&](double m,double s) -> double {
    const double ls=std::log(s);
    // LstCost reconstructs s from log(s); keep that rounding.
    const double represented=std::exp(ls);
    if(!(represented>0) || !std::isfinite(represented))return -INFINITY;
    double f=0;
    for(double v:x) {const double z=(v-m)/represented;const double w=2*(log_distance(v,m)-ls)-std::log(df);const double tail=std::isfinite(z*z/df)?std::log1p(z*z/df):std::max(w,0.0)+std::log1p(std::exp(-std::abs(w)));f+=ls-c+0.5*(df+1.0)*tail;}
    return -f;
  };
  const auto retain=[&](double m,double s){
    const double value=ll(m,s);
    if(std::isfinite(m)&&std::isfinite(s)&&s>0&&std::isfinite(value)) {
      const double margin=256*std::numeric_limits<double>::epsilon()*y.size()*(1+std::abs(value));
      bad.stop_detail.candidates.push_back({{m,s,df},value-margin,value+margin,StopModel::Lst});
      bad.stop_detail.loglik_lower=value-margin;
    }
  };
  double m=mu,s=sigma;
  if(std::isnan(s)) {
    std::size_t k=0;double log_r=-INFINITY;for(double v:y){k+=v==m;log_r=std::max(log_r,log_distance(v,m));}
    const double exponent=std::fma(df,static_cast<double>(y.size()-k),-static_cast<double>(k));
    if(exponent<0){bad.status=LstStatus::Unbounded;bad.stop_detail.loglik_lower=INFINITY;bad.stop_detail.loglik_upper=INFINITY;return bad;}
    // At equality every nonzero residual contributes a strict factor below
    // its collapse limit, so the supremum is never attained at held mu.
    if(exponent==0){
      const double shape_term=.5*(df+1)*(y.size()-k)*std::log(df);
      double limit=y.size()*c+shape_term,magnitude=std::abs(y.size()*c)+std::abs(shape_term);
      for(double v:y)if(v!=m){const double term=(df+1)*log_distance(v,m);limit-=term;magnitude+=std::abs(term);}
      const double margin=256*std::numeric_limits<double>::epsilon()*(1+y.size())*(1+magnitude);
      bad.status=LstStatus::ScaleBoundary;bad.stop_detail.limit_coef={m,df};
      bad.stop_detail.limit_model=StopModel::TCollapsePath;
      bad.stop_detail.limit_loglik=limit;bad.stop_detail.loglik_lower=limit-margin;
      bad.stop_detail.loglik_upper=limit+margin;return bad;
    }
    // The total log-scale score decreases strictly; expand before bisecting.
    // Log weights also retain products of huge df and tiny tail probabilities.
    const double lower=std::log(std::numeric_limits<double>::denorm_min()),upper=std::log(std::numeric_limits<double>::max());
    auto score=[&](double t){double g=-static_cast<double>(y.size());for(double v:y){const double z=2*(log_distance(v,m)-t)-std::log(df);g+=std::exp(std::log1p(df)-std::max(-z,0.0)-std::log1p(std::exp(-std::abs(z))));}return g;};
    double lo=std::max(lower,log_r-350),hi=std::min(upper,log_r+2),step=32;
    for(std::int64_t bracket_pass=0; bracket_pass<solver_budget(maxit) && (score(lo)<=0 && lo>lower); ++bracket_pass){lo=std::max(lower,lo-step);step*=2;}
    step=32;for(std::int64_t bracket_pass=0; bracket_pass<solver_budget(maxit) && (score(hi)>=0 && hi<upper); ++bracket_pass){hi=std::min(upper,hi+step);step*=2;}
    if(!(score(lo)>0 && score(hi)<0)){bad.status=LstStatus::IterationFailure;retain(m,std::exp(lo/2+hi/2));return bad;}
    for(int it=0;it<maxit && hi-lo>1e-12;++it){const double t=lo/2+hi/2;if(score(t)>0)lo=t;else hi=t;}
    const double midpoint=lo/2+hi/2;
    // A practical roundoff margin for log weights and sequential reduction.
    // A positive/negative pair brackets the exact monotone root, conditional
    // on these computed comparisons; a small score alone is insufficient.
    double magnitude=4+std::log1p(df)+y.size();
    for(double v:y)if(v!=m)magnitude=std::max(magnitude,std::abs(2*(log_distance(v,m)-midpoint)-std::log(df)));
    const double margin=256*std::numeric_limits<double>::epsilon()*y.size()*magnitude;
    const double left=std::max(lower,midpoint-5e-9),right=std::min(upper,midpoint+5e-9);
    if(certify_location && !(score(left)>margin&&score(right)<-margin)){
      bad.status=LstStatus::NumericalFailure;
      const double candidate=std::exp(midpoint),value=ll(m,candidate);
      if(std::isfinite(value)) {
        const double allowance=256*std::numeric_limits<double>::epsilon()*y.size()*(1+std::abs(value));
        bad.stop_detail.candidates.push_back({{m,candidate,df},value-allowance,value+allowance,StopModel::Lst});
        bad.stop_detail.loglik_lower=value-allowance;bad.stop_detail.loglik_upper=INFINITY;
      }
      return bad;
    }
    s=std::exp(midpoint);
    if(s>0 && std::isfinite(s) && std::abs(std::log(s)-midpoint)>5e-9){bad.status=LstStatus::NumericalFailure;retain(m,s);return bad;}
    if(!(hi-lo<=1e-10 && s>0 && std::isfinite(s) && std::abs(score(std::log(s)))<1e-3)){bad.status=LstStatus::IterationFailure;retain(m,s);return bad;}

  } else if(std::isnan(m) && certify_location) {
    const auto certificate=t_location_detail::solve(x,s,df,maxit);
    if(certificate.failure){
      bad.status=certificate.failure==1?LstStatus::CertificateBudget:
        certificate.failure==2?LstStatus::CertificateTie:LstStatus::NumericalFailure;
      bad.iterations=certificate.evaluations;bad.stop_detail=certificate.detail;return bad;
    }
    m=certificate.location;
  } else if(std::isnan(m)) {
    std::vector<double> grid=y;
    for(std::size_t j=1;j<y.size();++j)for(int k=1;k<8;++k)grid.push_back(y[j-1]+(y[j]-y[j-1])*k/8);
    for(double v:y)for(double d:{-.5,.5})if(v+d*s>y.front()&&v+d*s<y.back())grid.push_back(v+d*s);
    std::sort(grid.begin(),grid.end());grid.erase(std::unique(grid.begin(),grid.end()),grid.end());
    const auto optimum=maximize([&](double a){return ll(a,s);},grid,1e-10*s,maxit);
    if(!optimum.converged){bad.status=LstStatus::IterationFailure;return bad;}
    m=optimum.point;
  }
  const double value=ll(m,s);
  if(!std::isfinite(value)){bad.status=LstStatus::NumericalFailure;return bad;}
  return {m,s,df,value,0,LstStatus::Ok};
}
}
inline LstResult lst_mle_impl(span x,double tol,int maxit,double mu,double sigma,double df) {
  maxit = static_cast<int>(solver_budget(maxit));
  if(std::isnan(mu)&&std::isnan(sigma))return std::isnan(df)?lst_mle_impl(x,tol,maxit):lst_mle_fixed_df(x,df,tol,maxit);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  if(!std::isnan(df)) {
    if(!std::isfinite(df)||df<=0)return {nan,nan,df,nan,0,LstStatus::InvalidDf};
    return lst_detail::conditional(x,df,mu,sigma,tol,maxit,true);
 }
  if(x.empty())return {nan,nan,nan,nan,0,LstStatus::InvalidStart};
  // With free scale and held location the lowest df has the weakest
  // collapse exponent; the equality supremum is the df=1 collapse face.
  const double floor=std::isnan(mu) ? 1.0 : 0.0;
  if(std::isnan(sigma)) {
    std::size_t k=0;for(double v:x)k+=v==mu;
    if((floor==0 && k>0) || 2*k>x.size())return {mu,nan,1,INFINITY,0,LstStatus::Unbounded};
  }
  auto profile=[&](double t){return lst_detail::conditional(x,floor+std::exp(t),mu,sigma,tol,maxit);};
  std::vector<double> grid;for(double t=-12;t<=16;t+=.5)grid.push_back(t);
  StopDetail nomination;nomination.loglik_upper=INFINITY;
  const auto optimum=lst_detail::maximize([&](double a){
    const auto point=profile(a);t_shape_detail::retain_candidates(point.stop_detail,nomination);
    if(nomination.candidates.empty())t_shape_detail::retain_point(x,
      std::isfinite(point.mu)?point.mu:(std::isnan(mu)?x[0]:mu),
      std::isfinite(point.sigma)&&point.sigma>0?point.sigma:(std::isnan(sigma)?1.:sigma),
      floor+std::exp(a),false,nomination);
    return point.logLik;
  },grid,1e-8,maxit);
  if(!optimum.converged){
    LstResult failed{nan,nan,nan,nan,maxit,LstStatus::IterationFailure};failed.stop_detail=nomination;return failed;
  }
  auto best=profile(optimum.point);const auto edge=lst_detail::conditional(x,1,mu,sigma,tol,maxit);
  // A pointwise face comparison cannot select a global limit model.
  const double tie=1e-8*x.size();
  if(floor==1&&edge.status==LstStatus::Ok&&edge.logLik>=best.logLik-tie){
    StopDetail point,face;
    t_shape_detail::retain_point(x,best.mu,best.sigma,best.df,false,point);
    t_shape_detail::retain_point(x,edge.mu,edge.sigma,1,false,face);
    if(point.candidates.empty()||face.candidates.empty()||!(point.loglik_lower>face.candidates.front().loglik_upper))best=edge;
  }
  if(best.status==LstStatus::Ok) {
    StopDetail point;t_shape_detail::retain_point(x,best.mu,best.sigma,best.df,false,point);
    const auto normal=norm_mle(x,mu,sigma);
    const auto face=t_shape_detail::normal_point(x,normal.mean,normal.sd);
    const bool disproved=!point.candidates.empty()&&!face.candidates.empty()&&
      point.loglik_lower>face.candidates.front().loglik_upper;
    // An unresolved certificate preserves the legacy face classification.
    if(!disproved&&(normal.logLik>best.logLik+tie||(best.df>1e5&&normal.logLik>=best.logLik-tie))) {
      LstResult result{normal.mean,normal.sd,INFINITY,normal.logLik,0,LstStatus::NormalBoundary};
      result.certified=false;result.stop_detail=face;
      result.stop_detail.candidates.insert(result.stop_detail.candidates.begin(),point.candidates.begin(),point.candidates.end());
      result.stop_detail.limit_model=StopModel::Norm;result.stop_detail.limit_coef={normal.mean,normal.sd};
      result.stop_detail.limit_loglik=normal.logLik;result.stop_detail.loglik_upper=INFINITY;
      return result;
    }
    if(t_shape_detail::dominated(point,face)){
      best.status=LstStatus::NumericalFailure;best.stop_detail=point;return best;
    }
  }
  best.certified=false;
  return best;
}

inline LstResult lst_mle_certified(span x,double tol,int maxit,std::int64_t budget=-1) {
  maxit = static_cast<int>(solver_budget(maxit));
  if(!(tol>0)||!std::isfinite(tol))return {NAN,NAN,NAN,NAN,0,LstStatus::InvalidStart};
  if(budget<0)budget=certificate_budget(maxit);
  const auto c=t_certificate(x,1,budget,maxit,std::min(1e-6,std::max(1e-7,tol)));
  LstStatus status=LstStatus::NumericalFailure;
  switch(c.status){
    case TCertificateStatus::Finite:case TCertificateStatus::Cauchy:status=LstStatus::Ok;break;
    case TCertificateStatus::Normal:status=LstStatus::NormalBoundary;break;
    case TCertificateStatus::Collapse:status=LstStatus::ScaleBoundary;break;
    case TCertificateStatus::Unbounded:status=LstStatus::Unbounded;break;
    case TCertificateStatus::Budget:status=LstStatus::CertificateBudget;break;
    case TCertificateStatus::Tie:status=LstStatus::CertificateTie;break;
    case TCertificateStatus::InvalidData:status=LstStatus::InvalidStart;break;
    default:break;
  }
  LstResult r{c.mu,c.scale,c.nu,c.logLik,c.profiles,status};r.stop_detail=c.stop_detail;
  if(status==LstStatus::Ok&&r.df==1)r.restricted=16u;
  return r;
}
inline LstResult lst_mle(span x,double tol,int maxit) {
  maxit = static_cast<int>(solver_budget(maxit));
  auto r=lst_mle_certified(x,tol,maxit);
  if(r.status==LstStatus::Ok && r.df==kLstDfLower) r.restricted|=16u;
  return r;
}
inline LstResult lst_mle(span x,double tol,int maxit,double mu,double sigma,double df) {
  maxit = static_cast<int>(solver_budget(maxit));
  StopDetail face_witness;double normal_coefficient=NAN;
  const auto checked=[&](LstResult result){
    const bool normal=result.status==LstStatus::NormalBoundary;
    if(!normal&&result.status!=LstStatus::Ok)return result;
    if(face_witness.candidates.empty()&&!(normal&&normal_coefficient>0))return result;
    StopDetail point;
    if(normal)point=t_shape_detail::normal_point(x,result.mu,result.sigma);
    else t_shape_detail::retain_point(x,result.mu,result.sigma,result.df,false,point);
    const bool wrong=t_shape_detail::dominated(point,face_witness)||(normal&&normal_coefficient>0);
    if(wrong){result.status=LstStatus::NumericalFailure;result.certified=false;result.stop_detail=point;}
    return result;
  };
  if(std::isnan(mu)&&std::isnan(sigma)&&std::isnan(df))return lst_mle(x,tol,maxit);
  if(std::isnan(df)) {
    if(x.empty()||!(tol>0)||!std::isfinite(tol)||maxit<1 ||
       (!std::isnan(mu)&&!std::isfinite(mu)) ||
       (!std::isnan(sigma)&&(!(sigma>0)||!std::isfinite(sigma))))
      return {NAN,NAN,NAN,NAN,0,LstStatus::InvalidStart};
    for(double v:x)if(!std::isfinite(v))return {NAN,NAN,NAN,NAN,0,LstStatus::InvalidStart};
    if(std::isnan(sigma)&&std::count(x.begin(),x.end(),mu)>0) {
      LstResult unbounded{mu,NAN,NAN,INFINITY,0,LstStatus::Unbounded};
      unbounded.stop_detail.loglik_lower=INFINITY;unbounded.stop_detail.loglik_upper=INFINITY;return unbounded;
    }
    if(!std::isnan(mu)&&!std::isnan(sigma)) {
      const auto c=t_shape_detail::solve(x,mu,sigma,false,maxit,tol);
      const auto status=c.status==0?LstStatus::Ok:c.status==1?LstStatus::NormalBoundary:
        c.status==2?LstStatus::CertificateBudget:c.status==3?LstStatus::CertificateTie:LstStatus::NumericalFailure;
      LstResult result{mu,sigma,c.shape,c.loglik,c.evaluations,status};result.stop_detail=c.detail;
      if(c.status==0||c.status==1||tol<std::numeric_limits<double>::epsilon())return result;
      auto legacy=lst_mle_impl(x,tol,maxit,mu,sigma,df);
      if(legacy.status==LstStatus::Ok||legacy.status==LstStatus::NormalBoundary){
        StopDetail point;
        if(legacy.status==LstStatus::Ok)t_shape_detail::retain_point(x,legacy.mu,legacy.sigma,legacy.df,false,point);
        else point=t_shape_detail::normal_point(x,legacy.mu,legacy.sigma);
        if(t_shape_detail::dominated(point,c.detail)){
          legacy.status=LstStatus::NumericalFailure;legacy.stop_detail=point;return legacy;
        }
        legacy.certified=false;return legacy;
      }
      return result;
    }
    const auto normal_proof=t_signed_detail::solve(x,mu,sigma,false,maxit);
    face_witness=normal_proof.detail;normal_coefficient=normal_proof.coefficient_lower;
    if(normal_proof.proved){
      const auto& c=normal_proof.detail.limit_coef;
      LstResult result{c[0],c[1],INFINITY,normal_proof.detail.limit_loglik,normal_proof.evaluations,LstStatus::NormalBoundary};
      result.stop_detail=normal_proof.detail;return result;
    }
    if(std::isnan(mu)&&!std::isnan(sigma)) {
      const auto shape=t_location_detail::decreasing_shape(x,sigma,maxit);
      if(shape.result==1) {
        const auto location=t_location_detail::solve(x,sigma,1,maxit,certificate_budget(maxit)-shape.evaluations);
        LstResult certified{location.location,sigma,1,NAN,shape.evaluations+location.evaluations,LstStatus::NumericalFailure};
        certified.stop_detail=location.detail;
        if(location.failure==1)certified.status=LstStatus::CertificateBudget;
        else if(location.failure==2)certified.status=LstStatus::CertificateTie;
        else if(location.failure==0) {
          const auto value=lst_detail::conditional(x,1,location.location,sigma,tol,maxit);
          if(value.status==LstStatus::Ok){certified.status=LstStatus::Ok;certified.logLik=value.logLik;certified.restricted=16u;}
        }
        if(certified.status==LstStatus::NumericalFailure||certified.status==LstStatus::CertificateBudget) {
          const auto numerical=lst_mle_impl(x,tol,maxit,mu,sigma,df);
          if(numerical.status==LstStatus::Ok||numerical.status==LstStatus::NormalBoundary)return checked(numerical);
        }
        return certified;
      }
      if(shape.result==2) {
        const auto numerical=lst_mle_impl(x,tol,maxit,mu,sigma,df);
        if(numerical.status==LstStatus::Ok||numerical.status==LstStatus::NormalBoundary)return checked(numerical);
        const auto witness=lst_detail::conditional(x,1,x[0],sigma,tol,maxit);
        LstResult stopped{NAN,NAN,NAN,NAN,shape.evaluations,LstStatus::CertificateBudget};
        stopped.stop_detail.budget_exhausted=true;
        if(std::isfinite(witness.logLik)) {
          const double margin=256*std::numeric_limits<double>::epsilon()*x.size()*(1+std::abs(witness.logLik));
          stopped.stop_detail.candidates.push_back({{x[0],sigma,1},witness.logLik-margin,witness.logLik+margin,StopModel::Lst});
          stopped.stop_detail.loglik_lower=witness.logLik-margin;stopped.stop_detail.loglik_upper=INFINITY;
        }
        return stopped;
      }
    }
    // Remaining two-dimensional constrained profiles keep converged
    // numerical fits explicitly marked uncertified.
  }
  auto r=checked(lst_mle_impl(x,tol,maxit,mu,sigma,df));
  if(std::isnan(df) && r.status==LstStatus::Ok)r.certified=false;
  if(r.status==LstStatus::Ok && (std::isnan(df) && std::isnan(mu)) && r.df==kLstDfLower) r.restricted|=16u;
  return r;
}
} // namespace cppunivariate
