// Exact finite enumeration for the Conway--Maxwell--binomial family.
#pragma once
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
inline double logchoose(int N,int k) {
  k=std::min(k,N-k);
  return special::lgamma(N+1.0)-special::lgamma(N-k+1.0)-special::lgamma(k+1.0);
}
struct Table {
  std::vector<double> h,lp,lc,uc;
  double logZ=0, mean=0, mh=0, vx=0, vh=0, cov=0;
  Table(int N,double eta,double nu) : h(static_cast<std::size_t>(N)+1),lp(h.size()) {
    long double peak=-std::numeric_limits<long double>::infinity();
    for (int k=0;k<=N;++k) {
      h[k]=logchoose(N,k);
      const long double value=static_cast<long double>(eta)*k+static_cast<long double>(nu)*h[k];
      peak=std::max(peak,value);
    }
    double rest=0; bool skipped=false;
    for (int k=0;k<=N;++k) {
      const long double value=static_cast<long double>(eta)*k+static_cast<long double>(nu)*h[k];
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
  return cmb_detail::table(static_cast<int>(size),prob,nu).density(x,log);
}
inline double pcmb(double q,double size,double prob,double nu,bool lower_tail=true,bool log_p=false) {
  if (!cmb_detail::valid(size,prob,nu) || std::isnan(q)) return special::kNaN;
  return cmb_detail::table(static_cast<int>(size),prob,nu).cdf(q,lower_tail,log_p);
}
inline double qcmb(double p,double size,double prob,double nu,bool lower_tail=true,bool log_p=false) {
  if (!cmb_detail::valid(size,prob,nu)) return special::kNaN;
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
