#pragma once
#include "dpqr.hpp"
#include "../../core/span.hpp"
#include <vector>
namespace cppunivariate {
enum class SgompStatus { Ok, InvalidInput, Collapse, IterationFailure };
struct SgompResult { double b,eta,logLik; int iterations; SgompStatus status; };
namespace sgomp_detail {
struct Candidate { double b,k,ll; int iterations; bool converged; };
// Strict concavity in eta gives its unique constrained maximizer.
inline Candidate profile(span x,double b,double held_eta,int maxit,double tol) {
  int iterations=0; bool converged=true;
  double sv=0,lsv=-inf();
  std::vector<double> loga;
  loga.reserve(x.size());
  for (double v:x) {
    const double z=b*v;
    sv+=std::exp(-z);lsv=logadd(lsv,-z);
    loga.push_back(std::log(-std::expm1(-z)));
  }
  double k=held_eta==0 ? -inf() : std::log(held_eta);
  if (std::isnan(held_eta)) {
    if (2*sv>=static_cast<double>(x.size())) k=-inf();
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
  const double ratio=.6180339887498948482;
  double a=hi-ratio*(hi-lo),b=lo+ratio*(hi-lo);
  auto fa=profile(x,std::exp(a),eta,maxit,tol),fb=profile(x,std::exp(b),eta,maxit,tol);
  int iterations=fa.iterations+fb.iterations;
  bool converged=fa.converged && fb.converged;
  for (int j=0;j<maxit && hi-lo>tol;++j) {
    if (fa.ll>fb.ll) { hi=b;b=a;fb=fa;a=hi-ratio*(hi-lo);fa=profile(x,std::exp(a),eta,maxit,tol);iterations+=1+fa.iterations; }
    else { lo=a;a=b;fa=fb;b=lo+ratio*(hi-lo);fb=profile(x,std::exp(b),eta,maxit,tol);iterations+=1+fb.iterations; }converged=converged && fa.converged && fb.converged;
  }
  auto f=fa.ll>fb.ll ? fa : fb;
  f.iterations=iterations;f.converged=converged && hi-lo<=tol;return f;
}
}
inline SgompResult sgomp_mle(span x,double tol=1e-10,int maxit=200,
                            double b=sgomp_detail::nan(),double eta=sgomp_detail::nan()) {
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
        if (start.ll>best.ll) best=start;
      }
      // Likelihood envelopes give a finite search interval. For free eta,
      // ell <= n log b + n log n - b*(sum(y)-n*min(y)).
      // For fixed eta, ell <= n log b - b*sum(y) + n log(1+eta).
      double gap=0;
      for (double v:y) gap+=free_eta ? v-mn/scale : v;
      const double c=free_eta ? n*std::log(n) : n*std::log1p(eta);
      double upper=std::max(2*n/gap,best.b*2);
      while (n*std::log(upper)+c-upper*gap>=best.ll) upper*=2;
      // For b<=1, sum(v)>=n/e: ell<=n log b+n (also valid
      // for held eta by maximizing over eta). This excludes the low end.
      const double lower=std::min(0.5,std::exp(best.ll/n-2));
      if (!(std::isfinite(upper) && lower>0 && std::isfinite(best.ll))) {out.status=SgompStatus::IterationFailure;return out;}
      const double lo=std::log(lower),hi=std::log(upper);
      const int steps=std::max(64,static_cast<int>(std::ceil((hi-lo)/.0625)));
      std::vector<sgomp_detail::Candidate> grid;
      for (int j=0;j<=steps;++j) {
        auto f=sgomp_detail::profile(data,std::exp(lo+(hi-lo)*j/steps),eta,maxit,tol);
        iterations+=f.iterations;converged=converged && f.converged;
        grid.push_back(f); if (f.ll>best.ll) best=f;
      }
      for (int j=1;j<steps;++j) if (grid[j].ll>=grid[j-1].ll && grid[j].ll>=grid[j+1].ll) {
        auto f=sgomp_detail::refine(data,std::log(grid[j-1].b),std::log(grid[j+1].b),eta,maxit,tol);
        iterations+=f.iterations;converged=converged && f.converged;
        if (f.ll>best.ll) best=f;
      }
    }
  }
  if (free_eta) {
    const auto face=sgomp_detail::profile(data,free_b ? n/sum : b*scale,0,maxit,tol);
    iterations+=face.iterations;converged=converged && face.converged;
    if (face.ll>=best.ll-1e-8*n) best=face;
  }
  out.b=free_b ? best.b/scale : b;out.eta=free_eta ? std::exp(best.k) : eta;out.logLik=best.ll-n*std::log(scale);out.iterations=iterations;
  out.status=std::isfinite(out.b) && std::isfinite(out.eta) && std::isfinite(out.logLik) && converged ? SgompStatus::Ok : SgompStatus::IterationFailure;
  return out;
}
}
