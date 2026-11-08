// Exact finite enumeration for the Conway--Maxwell--binomial family.
#pragma once
#include "../../core/precision.hpp"
#include "../binom/dpqr.hpp"
#include "../../special/constants.hpp"
#include "../../special/special.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>
namespace cppunivariate {
namespace cmb_detail {
inline bool valid(double size,double prob,double nu) {
  return std::isfinite(size) && size>=0 && size<=1e7 && size==std::floor(size) &&
    std::isfinite(prob) && prob>=0 && prob<=1 && std::isfinite(nu);
}
// nu=0 is a finite geometric series. Reflect positive log odds so every
// exponential has a nonpositive argument; eta=0 is the uniform limit.
inline double geometric_logmass(double x,double size,double eta) {
  if(x<0 || x>size || x!=std::floor(x)) return special::kNegInf;
  if(eta==0) return -std::log1p(size);
  if(eta>0) {x=size-x;eta=-eta;}
  return x*eta+special::log1mexp(eta)-special::log1mexp((size+1)*eta);
}
inline special::LogTails geometric_tails(double q,double size,double eta) {
  if(q<0) return special::tails_from_log(special::kNegInf,true);
  if(q>=size) return special::tails_from_log(special::kNegInf,false);
  q=std::floor(q);
  if(eta>0) {
    const auto t=geometric_tails(size-q-1,size,-eta);
    return {t.upper,t.lower,t.valid};
  }
  const double z=eta==0 ? std::log1p(size) : special::log1mexp((size+1)*eta);
  const double lo=eta==0 ? std::log1p(q)-z : special::log1mexp((q+1)*eta)-z;
  const double up=eta==0 ? std::log(size-q)-z : (q+1)*eta+special::log1mexp((size-q)*eta)-z;
  return lo<up ? special::tails_from_log(lo,true) : special::tails_from_log(up,false);
}
inline double logchoose(int N,int k) {
  k=std::min(k,N-k);
  return special::lgamma(N+1.0)-special::lgamma(N-k+1.0)-special::lgamma(k+1.0);
}
struct Table {
  std::vector<double> h,lp,lc,uc;
  double logZ=0, mean=0, mh=0, vx=0, vh=0, cov=0;
  Table(int N,double eta,double nu) : h(static_cast<std::size_t>(N)+1),lp(h.size()) {
    CPPUNIV_EXTENDED_REAL peak=-std::numeric_limits<CPPUNIV_EXTENDED_REAL>::infinity();
    for (int k=0;k<=N;++k) {
      h[k]=logchoose(N,k);
      const CPPUNIV_EXTENDED_REAL value=static_cast<CPPUNIV_EXTENDED_REAL>(eta)*k+static_cast<CPPUNIV_EXTENDED_REAL>(nu)*h[k];
      peak=std::max(peak,value);
    }
    double rest=0; bool skipped=false;
    for (int k=0;k<=N;++k) {
      const CPPUNIV_EXTENDED_REAL value=static_cast<CPPUNIV_EXTENDED_REAL>(eta)*k+static_cast<CPPUNIV_EXTENDED_REAL>(nu)*h[k];
      lp[k]=static_cast<double>(value-peak);
      if (!skipped && lp[k]==0) { skipped=true; continue; }
      rest+=std::exp(lp[k]);
    }
    const double logz=std::log1p(rest);
    logZ=static_cast<double>(peak)+logz;
    for (int k=0;k<=N;++k) {
      lp[k]=lp[k]-logz;
      const double w=std::exp(lp[k]); mean+=w*k; mh+=w*h[k];
    }
    for (int k=0;k<=N;++k) {
      const double w=std::exp(lp[k]),dx=k-mean,dh=h[k]-mh;
      vx+=w*dx*dx; vh+=w*dh*dh; cov+=w*dx*dh;
    }
    cumulative();
  }
  static double logadd(double a,double b) {
    if (a==special::kNegInf) return b;
    if (b==special::kNegInf) return a;
    return std::max(a,b)+std::log1p(std::exp(-std::abs(a-b)));
  }
  void cumulative() {
    lc.resize(lp.size()); uc.resize(lp.size());
    // Do not turn a failed normalization into finite cumulative probabilities.
    for (double value : lp) {
      if (!(value<=0)) {
        std::fill(lc.begin(),lc.end(),special::kNaN);
        std::fill(uc.begin(),uc.end(),special::kNaN);
        return;
      }
    }
    double below=special::kNegInf,above=special::kNegInf;
    for (std::size_t k=0;k<lp.size();++k) {
      below=logadd(below,lp[k]); lc[k]=std::min(0.0,below);
      const std::size_t j=lp.size()-1-k;
      uc[j]=std::min(0.0,above); above=logadd(above,lp[j]);
    }
  }
  double density(double x,bool log) const {
    const double v=(x<0 || x>=lp.size() || x!=std::floor(x)) ? special::kNegInf : lp[static_cast<std::size_t>(x)];
    return log ? v : std::exp(v);
  }
  double cdf(double q,bool lower,bool log) const {
    const double k=std::floor(q),N=static_cast<double>(lp.size()-1);
    if (k<0 || k>=N) {
      const bool one=lower ? k>=N : k<0;
      return log ? (one ? 0 : special::kNegInf) : (one ? 1 : 0);
    }
    const std::size_t j=static_cast<std::size_t>(k);
    const double own=lower ? lc[j] : uc[j],other=lower ? uc[j] : lc[j];
    const double v=own<other ? own : std::log(-std::expm1(other));
    return log ? v : std::exp(v);
  }
  double quantile(double p,bool lower,bool log) const {
    if (std::isnan(p) || (log ? p>0 : p<0 || p>1)) return special::kNaN;
    if (p==(log ? special::kNegInf : 0)) return lower ? 0 : lp.size()-1;
    if (p==(log ? 0 : 1)) return lower ? lp.size()-1 : 0;
    int a=-1,b=static_cast<int>(lp.size()-1);
    while (b-a>1) {
      const int mid=a+(b-a)/2;
      const double f=cdf(mid,lower,log);
    if (std::isnan(f)) return std::numeric_limits<double>::quiet_NaN();
      if (lower ? f>=p : f<=p) b=mid; else a=mid;
    }
    return b;
  }
};
inline Table table(int N,double p,double nu) {
  if (p>0 && p<1) return Table(N,std::log(p)-std::log1p(-p),nu);
  Table t(N,0,0);
  for (int k=0;k<=N;++k) t.lp[k]=((p==0 && k==0) || (p==1 && k==N)) ? 0 : special::kNegInf;
  t.cumulative();
  return t;
}
}
namespace distributions {
inline double dcmb(double x,double size,double prob,double nu,bool log=false) {
  if (!cmb_detail::valid(size,prob,nu) || std::isnan(x)) return special::kNaN;
  if(size==0 || prob==0 || prob==1) {
    const double v=x==(prob==1?size:0)?0:special::kNegInf;
    return log?v:std::exp(v);
  }
  if(nu==0 && prob>0 && prob<1) {
    const double v=cmb_detail::geometric_logmass(x,size,std::log(prob)-std::log1p(-prob));
    return log?v:std::exp(v);
  }
  if(nu==1 || size==1) return dbinom(x,size,prob,log);
  return cmb_detail::table(static_cast<int>(size),prob,nu).density(x,log);
}
inline double pcmb(double q,double size,double prob,double nu,bool lower_tail=true,bool log_p=false) {
  if (!cmb_detail::valid(size,prob,nu) || std::isnan(q)) return special::kNaN;
  if(size==0 || prob==0 || prob==1) {
    const bool one=lower_tail ? q>=(prob==1?size:0) : q<(prob==1?size:0);
    return log_p?(one?0:special::kNegInf):(one?1:0);
  }
  if(nu==0 && prob>0 && prob<1)
    return special::tail_probability(cmb_detail::geometric_tails(q,size,std::log(prob)-std::log1p(-prob)),lower_tail,log_p);
  if(nu==1 || size==1) return pbinom(q,size,prob,lower_tail,log_p);
  return cmb_detail::table(static_cast<int>(size),prob,nu).cdf(q,lower_tail,log_p);
}
inline double qcmb(double p,double size,double prob,double nu,bool lower_tail=true,bool log_p=false) {
  if (!cmb_detail::valid(size,prob,nu)) return special::kNaN;
  if(size==0 || prob==0 || prob==1 || nu==1 || size==1)
    return qbinom(p,size,prob,lower_tail,log_p);
  if(nu==0) {
    const double eta=std::log(prob)-std::log1p(-prob);
    return special::discrete_quantile(special::discrete_log_tails(p,lower_tail,log_p),0,size,
        [=](double q){return cmb_detail::geometric_tails(q,size,eta);});
  }
  return cmb_detail::table(static_cast<int>(size),prob,nu).quantile(p,lower_tail,log_p);
}
template <typename Rng>
inline double rcmb(Rng &rng,double size,double prob,double nu) {
  if (!cmb_detail::valid(size,prob,nu)) return special::kNaN;
  if (size==0 || prob==0) return 0;
  if (prob==1) return size;
  return qcmb(rng.runif(0,1),size,prob,nu);
}
}
}
