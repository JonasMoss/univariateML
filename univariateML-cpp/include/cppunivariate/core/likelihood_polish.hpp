#pragma once
#include "precision.hpp"
#include "work_budget.hpp"
#include "../special/special.hpp"
#include <array>
#include <cmath>
#include <limits>

namespace cppunivariate {
namespace accuracy_detail {
struct ScalarSum {
  ExtendedReal value=0,correction=0;
  void add(ExtendedReal term) {const auto y=term-correction,t=value+y;correction=(t-value)-y;value=t;}
};
// Check the rounded public coefficient against its solved local coordinate.
inline bool scale_projection(double value,double unit,double coordinate,double tol,bool inverse=false) {
  if(!(value>0)||!std::isfinite(value))return false;
  const ExtendedReal ratio=inverse?static_cast<ExtendedReal>(value)*unit:static_cast<ExtendedReal>(value)/unit;
  const ExtendedReal actual=ratio>0&&std::isfinite(ratio)?std::log(ratio):
    std::log(static_cast<ExtendedReal>(value))+(inverse?1:-1)*std::log(static_cast<ExtendedReal>(unit));
  return std::abs(actual-coordinate)<=tol/4;
}
inline bool location_projection(double value,double center,double unit,double coordinate,double scale,double tol) {
  if(!std::isfinite(value))return false;
  const ExtendedReal difference=static_cast<ExtendedReal>(value)-center;
  const ExtendedReal actual=std::isfinite(difference)?difference/unit:
    static_cast<ExtendedReal>(value)/unit-static_cast<ExtendedReal>(center)/unit;
  return std::abs(actual-coordinate)*(static_cast<ExtendedReal>(unit)/std::max(std::abs(value),scale))<=tol/4;
}
// Second-order forward derivatives, in at most three local coordinates.
// Constants and held coefficients have zero derivatives. No differencing of
// rounded likelihoods is involved.
struct Jet {
  ExtendedReal v=0;
  std::array<ExtendedReal,3> g{};
  std::array<std::array<ExtendedReal,3>,3> h{};
  Jet()=default;
  Jet(ExtendedReal value):v(value) {}
};
inline Jet variable(double value,int index,bool free) {
  Jet out(value);if(free)out.g[index]=1;return out;
}
inline Jet operator+(const Jet& a,const Jet& b) {
  Jet c(a.v+b.v);for(int i=0;i<3;++i){c.g[i]=a.g[i]+b.g[i];for(int j=0;j<3;++j)c.h[i][j]=a.h[i][j]+b.h[i][j];}return c;
}
inline Jet operator-(const Jet& a) {
  Jet c(-a.v);for(int i=0;i<3;++i){c.g[i]=-a.g[i];for(int j=0;j<3;++j)c.h[i][j]=-a.h[i][j];}return c;
}
inline Jet operator-(const Jet& a,const Jet& b){return a+(-b);}
inline Jet operator*(const Jet& a,const Jet& b) {
  Jet c(a.v*b.v);for(int i=0;i<3;++i){c.g[i]=a.g[i]*b.v+a.v*b.g[i];for(int j=0;j<3;++j)c.h[i][j]=a.h[i][j]*b.v+a.g[i]*b.g[j]+a.g[j]*b.g[i]+a.v*b.h[i][j];}return c;
}
inline Jet compose(const Jet& a,ExtendedReal value,ExtendedReal first,ExtendedReal second) {
  Jet c(value);for(int i=0;i<3;++i){c.g[i]=first*a.g[i];for(int j=0;j<3;++j)c.h[i][j]=first*a.h[i][j]+second*a.g[i]*a.g[j];}return c;
}
inline Jet reciprocal(const Jet& a){return compose(a,1/a.v,-1/(a.v*a.v),2/(a.v*a.v*a.v));}
inline Jet operator/(const Jet& a,const Jet& b){return a*reciprocal(b);}
inline Jet exp(const Jet& a){const ExtendedReal e=std::exp(a.v);return compose(a,e,e,e);}
inline Jet log(const Jet& a){return compose(a,std::log(a.v),1/a.v,-1/(a.v*a.v));}
inline Jet log1p(const Jet& a){const ExtendedReal d=1+a.v;return compose(a,std::log1p(a.v),1/d,-1/(d*d));}
inline Jet expm1(const Jet& a){const ExtendedReal e=std::exp(a.v);return compose(a,std::expm1(a.v),e,e);}
inline Jet log1pmx(const Jet& a) {
  ExtendedReal value=std::log1p(a.v)-a.v;
  if(std::abs(a.v)<.01) {
    ExtendedReal term=-a.v*a.v/2;value=term;
    for(int k=3;k<=16;++k){term*=-a.v*ExtendedReal(k-1)/k;value+=term;}
  }
  const ExtendedReal d=1+a.v;return compose(a,value,-a.v/d,-1/(d*d));
}
inline Jet lgamma(const Jet& a){return compose(a,std::lgamma(a.v),special::digamma(static_cast<double>(a.v)),special::trigamma(static_cast<double>(a.v)));}
inline Jet softplus(const Jet& a){return a.v>0?a+log1p(exp(-a)):log1p(exp(a));}
inline Jet logcdf(const Jet& a) {
  // Continued fraction for the inverse Mills ratio avoids cancellation in
  // its derivative in the negative tail.
  if(a.v < -10) {
    Jet f(0);for(int k=48;k>=1;--k)f=Jet(k)/(-a+f);
    return -Jet(.5)*a*a-Jet(.91893853320467274178)-log(-a+f);
  }
  const ExtendedReal c=std::erfc(-a.v/std::sqrt(ExtendedReal(2)))/2;
  const ExtendedReal r=std::exp(-a.v*a.v/2-ExtendedReal(.91893853320467274178))/c;
  return compose(a,std::log(c),r,-r*(a.v+r));
}

// Compensate the value, score and information components independently.
struct JetSum {
  Jet value,correction;
  void add(const Jet& term) {
    const auto y=term-correction,t=value+y;correction=(t-value)-y;value=t;
  }
};

// Local Newton refinement. Coordinates are log positive parameters and
// naturally scaled locations. Cholesky tests the observed information, and
// the *undamped* Newton correction bounds each coordinate error. Two small
// corrections are required, or a quadratic decrease (allowing rounding)
// together with contraction by at least 4.
// A failed refinement is a numerical accuracy failure, never a certificate
// budget failure. The global search and face comparisons belong to callers.
template<class Objective>
inline bool polish(std::array<double,3>& q,const std::array<bool,3>& free,
                   double tol,int maxit,const Objective& objective) {
  std::array<int,3> indices{};int n=0;for(int i=0;i<3;++i)if(free[i])indices[n++]=i;
  if(n==0)return true;
  if(tol<4*std::numeric_limits<double>::epsilon())return false;
  double previous=INFINITY;
  const auto advance=[&](bool update)->int {
    std::array<Jet,3> z;for(int i=0;i<3;++i)z[i]=variable(q[i],i,free[i]);
    const Jet f=objective(z);
    if(!std::isfinite(f.v))return false;
    std::array<std::array<ExtendedReal,3>,3> L{};
    std::array<ExtendedReal,3> step{},y{};
    for(int i=0;i<n;++i)for(int j=0;j<=i;++j) {
      ExtendedReal v=-f.h[indices[i]][indices[j]];
      for(int k=0;k<j;++k)v-=L[i][k]*L[j][k];
      if(i==j){if(!(v>0)||!std::isfinite(v))return false;L[i][j]=std::sqrt(v);}
      else L[i][j]=v/L[j][j];
    }
    for(int i=0;i<n;++i){ExtendedReal v=f.g[indices[i]];for(int j=0;j<i;++j)v-=L[i][j]*y[j];y[i]=v/L[i][i];}
    for(int i=n-1;i>=0;--i){ExtendedReal v=y[i];for(int j=i+1;j<n;++j)v-=L[j][i]*step[j];step[i]=v/L[i][i];}
    double error=0;for(int i=0;i<n;++i)error=std::max(error,static_cast<double>(std::abs(step[i])));
    if(!std::isfinite(error)||error>.25)return false;
    if(error<=tol*.0625 && std::isfinite(previous) && (previous<=tol*.0625 || (error<=previous*.25 && error<=std::max(16*std::numeric_limits<double>::epsilon(),16*previous*previous))))return true;
    if(!update)return false;
    for(int i=0;i<n;++i)q[indices[i]]+=static_cast<double>(step[i]);
    previous=error;
    return -1;
  };
  for(std::int64_t it=0;it<solver_budget(maxit);++it) {
    const int state=advance(true);
    if(state>=0)return state==1;
  }
  // Validation of the final permitted update is not another Newton step.
  return advance(false)==1;
}
} // namespace accuracy_detail
} // namespace cppunivariate
