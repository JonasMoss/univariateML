#pragma once
#include "../../core/work_budget.hpp"
#include "dpqr.hpp"
#include "../../core/span.hpp"
#include <vector>
namespace cppunivariate {
enum class SgompStatus { Ok, InvalidInput, Collapse, IterationFailure };
struct SgompResult { double b,eta,logLik; int iterations; SgompStatus status; };
namespace sgomp_detail {
struct Candidate { double b,k,ll; int iterations; bool converged; };
// Compare ell(a)-ell(b) before adding the potentially enormous common
// exponential log likelihood. In particular, equal rates cancel exactly.
inline double difference(span x,const Candidate &a,const Candidate &b) {
  const double relative=(a.b-b.b)/b.b;
  double delta=x.size()*(std::abs(relative)<0.5 ? std::log1p(relative) : std::log(a.b)-std::log(b.b));
  for (double v:x) {
    delta-=(a.b-b.b)*v;
    const double za=a.b*v,zb=b.b*v;
    const double la=std::log(-std::expm1(-za));
    const double lb=std::log(-std::expm1(-zb));
    delta+=(a.k==-inf() ? 0 : softplus(a.k+la))-
           (b.k==-inf() ? 0 : softplus(b.k+lb));
    const double ea=a.k-za,eb=b.k-zb;
    if (ea!=eb) {
      if (ea>eb) delta-=std::exp(ea)*(-std::expm1(eb-ea));
      else delta+=std::exp(eb)*(-std::expm1(ea-eb));
    }
  }
  return delta;
}
// Preserve ordinary-fit ordering on sub-roundoff plateaus, but only when
// the old likelihood spacing itself is below an absolute per-data margin.
// This cannot hide a resolved gain behind an enormous common likelihood.
inline double ranking_difference(span x,const Candidate &a,const Candidate &b) {
  const double delta=difference(x,a,b);
  const double margin=8*std::numeric_limits<double>::epsilon()*x.size();
  const double spacing=std::max(std::nextafter(std::abs(a.ll),inf())-std::abs(a.ll),
                                std::nextafter(std::abs(b.ll),inf())-std::abs(b.ll));
  if (std::abs(delta)<=margin && spacing<=margin) return a.ll-b.ll;
  return delta;
}
inline double face_score(span x,double b) {
  double score=0;
  for (double v:x) score+=-std::expm1(-b*v)-std::exp(-b*v);
  return score;
}
// Strict concavity in eta gives its unique constrained maximizer.
inline Candidate profile(span x,double b,double held_eta,int maxit,double tol) {
  maxit = static_cast<int>(solver_budget(maxit));
  int iterations=0; bool converged=true;
  double lsv=-inf();
  std::vector<double> loga;
  loga.reserve(x.size());
  for (double v:x) {
    const double z=b*v;
    lsv=logadd(lsv,-z);
    loga.push_back(std::log(-std::expm1(-z)));
  }
  double k=held_eta==0 ? -inf() : std::log(held_eta);
  if (std::isnan(held_eta)) {
    if (face_score(x,b)<=0) k=-inf();
    else {
      double lo=-750,hi=std::log(static_cast<double>(x.size()))-lsv;
      converged=false;
      for (int j=0;j<maxit;++j) {
        ++iterations;
        k=(lo+hi)/2;
        double score=-std::exp(k+lsv);
        for (double a:loga) {
          const double t=k+a;
          if (t>=0) score+=1/(1+std::exp(-t));
          else { const double e=std::exp(t);score+=e/(1+e); }
        }
        if (score>0) lo=k; else hi=k;
        if (hi-lo<tol || hi-lo<=8*std::numeric_limits<double>::epsilon()*(1+std::abs(k))) {converged=true;break;}
      }
      k=(lo+hi)/2;
    }
  }
  double ll=x.size()*std::log(b);
  for (double v:x) {
    const double z=b*v,a=-std::expm1(-z);
    ll-=z;
    if (k!=-inf()) ll+= (a>0 ? softplus(k+std::log(a)) : 0)-std::exp(k-z);
  }
  return {b,k,ll,iterations,converged};
}
inline Candidate refine(span x,double lo,double hi,double eta,int maxit,double tol) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double ratio=.6180339887498948482;
  double a=hi-ratio*(hi-lo),b=lo+ratio*(hi-lo);
  auto fa=profile(x,std::exp(a),eta,maxit,tol),fb=profile(x,std::exp(b),eta,maxit,tol);
  int iterations=fa.iterations+fb.iterations;
  bool converged=fa.converged && fb.converged;
  for (int j=0;j<maxit && hi-lo>tol;++j) {
    if (ranking_difference(x,fa,fb)>0) { hi=b;b=a;fb=fa;a=hi-ratio*(hi-lo);fa=profile(x,std::exp(a),eta,maxit,tol);iterations+=1+fa.iterations; }
    else { lo=a;a=b;fa=fb;b=lo+ratio*(hi-lo);fb=profile(x,std::exp(b),eta,maxit,tol);iterations+=1+fb.iterations; }converged=converged && fa.converged && fb.converged;
  }
  auto f=ranking_difference(x,fa,fb)>0 ? fa : fb;
  f.iterations=iterations;f.converged=converged && hi-lo<=tol;return f;
}
}
inline SgompResult sgomp_mle(span x,double tol=1e-10,int maxit=200,
                            double b=sgomp_detail::nan(),double eta=sgomp_detail::nan()) {
  maxit = static_cast<int>(solver_budget(maxit));
  SgompResult out{b,eta,sgomp_detail::nan(),0,SgompStatus::InvalidInput};
  if (x.empty() || !(tol>0) || !std::isfinite(tol) || maxit<1 ||
      (!std::isnan(b) && !(b>0 && std::isfinite(b))) ||
      (!std::isnan(eta) && !(eta>=0 && std::isfinite(eta)))) return out;
  double scale=0,mn=sgomp_detail::inf(),mx=0;
  for (double v:x) { if (!(v>=0) || !std::isfinite(v)) return out; scale=std::max(scale,v);mn=std::min(mn,v);mx=std::max(mx,v); }
  const bool free_b=std::isnan(b),free_eta=std::isnan(eta);
  if (free_b && (mx==0 || (free_eta && mn==mx))) {out.status=SgompStatus::Collapse;return out;}
  if (scale==0) scale=1;
  std::vector<double> y; double sum=0;
  for (double v:x) { y.push_back(v/scale);sum+=v/scale; }
  const span data{y.data(),y.size()};
  const double n=static_cast<double>(x.size());
  auto best=sgomp_detail::profile(data,free_b ? n/sum : b*scale,eta,maxit,tol);
  int iterations=best.iterations;bool converged=best.converged;
  if (free_b) {
    if (!free_eta && eta==0) { /* exact exponential candidate already evaluated */ }
    else {
      if (!free_eta) {
        const auto start=sgomp_detail::profile(data,(1+std::log1p(eta))*n/sum,eta,maxit,tol);
        if (sgomp_detail::ranking_difference(data,start,best)>0) best=start;
      }
      // Likelihood envelopes give a finite search interval. For free eta,
      // ell <= n log b + n log n - b*(sum(y)-n*min(y)).
      // For fixed eta, ell <= n log b - b*sum(y) + n log(1+eta).
      double gap=0;
      for (double v:y) gap+=free_eta ? v-mn/scale : v;
      const double c=free_eta ? n*std::log(n) : n*std::log1p(eta);
      double upper=std::max(2*n/gap,best.b*2);
      for(std::int64_t bracket_pass=0; bracket_pass<solver_budget(maxit) && (n*std::log(upper)+c-upper*gap>=best.ll); ++bracket_pass) upper*=2;
      // For b<=1, sum(v)>=n/e: ell<=n log b+n (also valid
      // for held eta by maximizing over eta). This excludes the low end.
      const double lower=std::min(0.5,std::exp(best.ll/n-2));
      if (!(std::isfinite(upper) && lower>0 && std::isfinite(best.ll)) || n*std::log(upper)+c-upper*gap>=best.ll) {out.status=SgompStatus::IterationFailure;return out;}
      const double lo=std::log(lower),hi=std::log(upper);
      const int steps=std::max(64,static_cast<int>(std::ceil((hi-lo)/.0625)));
      std::vector<sgomp_detail::Candidate> grid;
      for (int j=0;j<=steps;++j) {
        auto f=sgomp_detail::profile(data,std::exp(lo+(hi-lo)*j/steps),eta,maxit,tol);
        iterations+=f.iterations;converged=converged && f.converged;
        grid.push_back(f); if (sgomp_detail::ranking_difference(data,f,best)>0) best=f;
      }
      for (int j=1;j<steps;++j) if (sgomp_detail::ranking_difference(data,grid[j],grid[j-1])>=0 && sgomp_detail::ranking_difference(data,grid[j],grid[j+1])>=0) {
        auto f=sgomp_detail::refine(data,std::log(grid[j-1].b),std::log(grid[j+1].b),eta,maxit,tol);
        iterations+=f.iterations;converged=converged && f.converged;
        if (sgomp_detail::ranking_difference(data,f,best)>0) best=f;
      }
    }
  }
  if (free_eta) {
    const auto face=sgomp_detail::profile(data,free_b ? n/sum : b*scale,0,maxit,tol);
    iterations+=face.iterations;converged=converged && face.converged;
    if (sgomp_detail::face_score(data,face.b)<=0 &&
        sgomp_detail::difference(data,face,best)>=-1e-8*n) best=face;
  }
  out.b=free_b ? best.b/scale : b;out.eta=free_eta ? std::exp(best.k) : eta;out.logLik=best.ll-n*std::log(scale);out.iterations=iterations;
  out.status=std::isfinite(out.b) && std::isfinite(out.eta) && std::isfinite(out.logLik) && converged ? SgompStatus::Ok : SgompStatus::IterationFailure;
  return out;
}
}
