// Local coefficient-accuracy check in smooth Prentice coordinates.
#pragma once
#include "../../core/likelihood_polish.hpp"
#include "dpqr.hpp"
#include "shape_score.hpp"
#include <array>
#include <cmath>
#include <limits>
#include <vector>

namespace cppunivariate {
namespace gengamma_fit_detail {

inline accuracy_detail::Jet remainder_jet(const accuracy_detail::Jet &q) {
  const double v=static_cast<double>(q.v);
  const double q2=v*v;
  const double value=distributions::gengamma_detail::remainder(v);
  const double first=shape_remainder_derivative(v);
  double second;
  if (std::abs(v)<=0.25) {
    const double q4=q2*q2;
    second=1.0/6+q4*(-1.0/12+q4*(1.0/14+q4*(-13.0/120+q4*(17.0/66-q4*691.0/780))));
  } else {
    const double a=(1/v)/v;
    const double l=trgamma_detail::log_minus_digamma(a);
    const double dl=trgamma_detail::dlog_minus_digamma(a);
    second=(1-6*a*l-4*a*a*dl)/q2;
  }
  return accuracy_detail::compose(q,value,first,second);
}

inline accuracy_detail::Jet penalty_jet(const accuracy_detail::Jet &w,
                                        const accuracy_detail::Jet &q) {
  const auto z=q*w;
  if (std::abs(z.v)<=0.1) {
    const accuracy_detail::Jet p=accuracy_detail::Jet(0.5)+z*(accuracy_detail::Jet(1.0/6)+
      z*(accuracy_detail::Jet(1.0/24)+z*(accuracy_detail::Jet(1.0/120)+
      z*(accuracy_detail::Jet(1.0/720)+z*(accuracy_detail::Jet(1.0/5040)+
      z*(accuracy_detail::Jet(1.0/40320)+z*(accuracy_detail::Jet(1.0/362880)+
      z*(accuracy_detail::Jet(1.0/3628800)+z*(accuracy_detail::Jet(1.0/39916800)+
      z*(accuracy_detail::Jet(1.0/479001600)+z*accuracy_detail::Jet(1.0/6227020800.0)))))))))));
    return w*w*p;
  }
  return (accuracy_detail::expm1(z)-z)/(q*q);
}

// The initial candidate comes from a separate search. On success the
// existing Cholesky/Newton check establishes local coefficient accuracy;
// it does not certify global optimality or any face comparison.
inline bool polish_local(span y, double center, double unit,
                         std::array<double,3> &parameter,
                         const std::array<bool,3> &free, double tol, int maxit,
                         double data_error, StopDetail& diagnostic) {
  if (y.empty() || !std::isfinite(center) || !(unit>0) || !std::isfinite(unit) ||
      !std::isfinite(parameter[0]) || !(parameter[1]>0) || !std::isfinite(parameter[1]) ||
      !std::isfinite(parameter[2]) || !(tol>0) || !std::isfinite(tol) || maxit<1 ||
      !(data_error>=0) || !std::isfinite(data_error)) return false;
  std::vector<double> normalized;
  normalized.reserve(y.size());
  for (double v:y) {
    const double z=(v-center)/unit;
    if (!std::isfinite(z)) return false;
    normalized.push_back(z);
  }
  std::array<double,3> local{(parameter[0]-center)/unit,
      std::log(parameter[1])-std::log(unit),parameter[2]};
  if (!std::isfinite(local[0]) || !std::isfinite(local[1])) return false;
  const auto objective=[&](const std::array<accuracy_detail::Jet,3> &p) {
    const auto s=accuracy_detail::exp(p[1]);
    accuracy_detail::JetSum sum;
    for (double v:normalized) {
      const auto w=(accuracy_detail::Jet(v)-p[0])/s;
      sum.add(-penalty_jet(w,p[2]));
    }
    return sum.value/accuracy_detail::Jet(normalized.size())-p[1]-remainder_jet(p[2]);
  };
  // A location correction in normalized units must also fit the sigma
  // floor; using the smaller tolerance is conservative for all coordinates.
  const double local_factor=free[0] ? std::min(1.0,parameter[1]/unit) : 1.0;
  const double local_tol=tol*local_factor*0.25;
  StopDetail detail;
  const auto fail=[&](double bound=NAN) {
    const double m=free[0]?center+unit*local[0]:parameter[0];
    const double s=free[1]?unit*std::exp(local[1]):parameter[1];
    const double q=free[2]?local[2]:parameter[2];
    if(!std::isfinite(m) || !(s>0) || !std::isfinite(s) || !std::isfinite(q)) {
      if(diagnostic.candidates.empty())
        diagnostic=conditional_accuracy(y,parameter[0],parameter[1],parameter[2],free[0],0,0);
      return false;
    }
    std::array<accuracy_detail::Jet,3> z;
    for(int i=0;i<3;++i)z[i]=accuracy_detail::variable(local[i],i,free[i]);
    const auto evaluated=objective(z);
    bool finite=std::isfinite(evaluated.v);
    for(int i=0;i<3;++i)if(free[i]) {
      finite=finite && std::isfinite(evaluated.g[i]);
      for(int j=0;j<3;++j)if(free[j])finite=finite && std::isfinite(evaluated.h[i][j]);
    }
    const double ll=conditional_loglik(y,m,s,q);
    finite=finite && std::isfinite(ll);
    if(!finite){detail.accuracy_stop=AccuracyStop::None;detail.conditioning_bound=NAN;bound=NAN;}
    if(!std::isfinite(ll)) {
      if(diagnostic.candidates.empty())
        diagnostic=conditional_accuracy(y,parameter[0],parameter[1],parameter[2],free[0],0,0);
      return false;
    }
    const double conversion=std::max({free[0]?unit/std::max(std::abs(m),s):0.,
      free[1]?1.:0.,free[2]?1/std::max(1.,std::abs(q)):0.});
    if(detail.accuracy_stop==AccuracyStop::Tolerance) {
      // Round the converted floor upward: the rounded local request can be
      // below its floor even when the inverse conversion rounds back to tol.
      detail.attainable_tol=std::nextafter(std::max(tol,
        detail.attainable_tol*(4/local_factor)),INFINITY);
    }
    if(std::isfinite(detail.conditioning_bound))detail.conditioning_bound*=conversion;
    record_accuracy(detail,finite?tol:0,{m,s,q},ll,
      {free[0]?std::max(std::abs(m),s):0,free[1]?s:0,free[2]?std::max(1.,std::abs(q)):0},bound);
    if(!detail.candidates.empty() &&
       (diagnostic.candidates.empty() || ll>diagnostic.candidates.back().loglik_lower))
      diagnostic=detail;
    return false;
  };
  if (!accuracy_detail::polish(local,free,local_tol,maxit,objective,detail))return fail();
  // Bound a practical score-roundoff allowance through the observed
  // information. This rejects ill-conditioned candidates for which a
  // small computed Newton correction cannot support the coefficient goal.
  std::array<accuracy_detail::Jet,3> vars;
  std::array<int,3> indices{};int count=0;
  for(int i=0;i<3;++i) {
    vars[i]=accuracy_detail::variable(local[i],i,free[i]);
    if(free[i])indices[count++]=i;
  }
  const auto final=objective(vars);
  const auto scale_jet=accuracy_detail::exp(vars[1]);
  const auto remainder=remainder_jet(vars[2]);
  const CPPUNIV_EXTENDED_REAL scaled_error=static_cast<CPPUNIV_EXTENDED_REAL>(data_error)/unit;
  if (!std::isfinite(scaled_error)) return false;
  std::array<CPPUNIV_EXTENDED_REAL,3> magnitude{},input_error{};
  for(double v:normalized) {
    const auto w=(accuracy_detail::Jet(v)-vars[0])/scale_jet;
    const auto term=-penalty_jet(w,vars[2])-vars[1]-remainder;
    for(int i=0;i<count;++i)magnitude[i]+=std::abs(term.g[indices[i]])/normalized.size();
    if (scaled_error>0) {
      const auto wv=accuracy_detail::variable(static_cast<double>(w.v),0,true);
      const auto qv=accuracy_detail::variable(local[2],1,true);
      const auto p=penalty_jet(wv,qv);
      const CPPUNIV_EXTENDED_REAL s=scale_jet.v;
      const std::array<CPPUNIV_EXTENDED_REAL,3> sensitivity{
        p.h[0][0]/(s*s),(p.g[0]+w.v*p.h[0][0])/s,-p.h[1][0]/s};
      for(int i=0;i<count;++i)
        input_error[i]+=std::abs(sensitivity[indices[i]])*scaled_error/normalized.size();
    }
  }
  const double eps=std::numeric_limits<double>::epsilon();
  std::array<CPPUNIV_EXTENDED_REAL,3> allowance{};
  for(int i=0;i<count;++i) {
    allowance[i]=64*eps*(1+normalized.size()*eps)*(1+magnitude[i])+input_error[i];
    if(indices[i]==2 && std::abs(local[2])>0.25) {
      const double a=(1/local[2])/local[2];
      const double term=(2/std::abs(local[2]))*(1+std::abs(a*trgamma_detail::log_minus_digamma(a)));
      allowance[i]+=64*eps*term;
    }
    if(!std::isfinite(allowance[i]))return false;
  }
  std::array<std::array<CPPUNIV_EXTENDED_REAL,3>,3> L{};
  for(int i=0;i<count;++i)for(int j=0;j<=i;++j) {
    CPPUNIV_EXTENDED_REAL v=-final.h[indices[i]][indices[j]];
    for(int k=0;k<j;++k)v-=L[i][k]*L[j][k];
    if(i==j) {
      if(!(v>128*eps*(1+std::abs(final.h[indices[i]][indices[i]]))) || !std::isfinite(v))return false;
      L[i][j]=std::sqrt(v);
    } else L[i][j]=v/L[j][j];
  }
  std::array<CPPUNIV_EXTENDED_REAL,3> error{};
  for(int column=0;column<count;++column) {
    std::array<CPPUNIV_EXTENDED_REAL,3> forward{},solution{};
    for(int i=0;i<count;++i) {
      CPPUNIV_EXTENDED_REAL v=i==column ? 1 : 0;
      for(int j=0;j<i;++j)v-=L[i][j]*forward[j];
      forward[i]=v/L[i][i];
    }
    for(int i=count-1;i>=0;--i) {
      CPPUNIV_EXTENDED_REAL v=forward[i];
      for(int j=i+1;j<count;++j)v-=L[j][i]*solution[j];
      solution[i]=v/L[i][i];
    }
    for(int i=0;i<count;++i)error[i]+=std::abs(solution[i])*allowance[column];
  }
  double noise=0;
  for(int i=0;i<count;++i) {
    if(!std::isfinite(error[i]))return false;
    const double scale=indices[i]==0?unit/std::max(std::abs(center+unit*local[0]),unit*std::exp(local[1])):
      indices[i]==2?1/std::max(1.,std::abs(local[2])):1.;
    noise=std::max(noise,4*static_cast<double>(error[i])*scale);
  }
  for(int i=0;i<count;++i)if(!(error[i]<local_tol))return fail(noise);
  const double mu=free[0] ? center+unit*local[0] : parameter[0];
  const double sigma=free[1] ? unit*std::exp(local[1]) : parameter[1];
  const double shape=free[2] ? local[2] : parameter[2];
  if (!std::isfinite(mu) || !(sigma>0) || !std::isfinite(sigma) || !std::isfinite(shape)) return false;
  if (free[0] && local_tol > tol*std::min(1.0,sigma/unit)*0.5) return fail();
  if (free[0] && !accuracy_detail::location_projection(mu,center,unit,local[0],sigma,tol)) return fail();
  if (free[1] && !accuracy_detail::scale_projection(sigma,unit,local[1],tol)) return fail();
  parameter={mu,sigma,shape};
  return true;
}

inline bool polish_local(span y,double center,double unit,std::array<double,3>& parameter,
    const std::array<bool,3>& free,double tol,int maxit,double data_error=0) {
  StopDetail detail;
  return polish_local(y,center,unit,parameter,free,tol,maxit,data_error,detail);
}

}  // namespace gengamma_fit_detail
}  // namespace cppunivariate
