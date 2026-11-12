#pragma once
#include "power_one_accuracy.hpp"
namespace cppunivariate { namespace burr_power_one_detail {
struct PairEvaluation {
  Pair likelihood;
  std::array<Pair,3> score{};
  std::array<std::array<Pair,3>,3> information{};
  std::array<std::array<Pair,3>,3> projection{};
  std::array<double,3> score_magnitude{},score_tail{};
  double information_magnitude=0,information_tail=0,likelihood_magnitude=0;
  bool valid=true;
};
inline Pair square_root(Pair a) {
  const Pair initial(std::sqrt(a.hi));
  return initial+(a-initial*initial)/(Pair(2)*initial);
}
// The objective supplies scores, observed information and the Jacobian to
// public log coefficients. Two small undamped corrections establish local
// contraction. Componentwise arithmetic allowances are propagated through
// inverse information before acceptance; a positive rounded pivot alone is
// insufficient. Modified pivots are proposals only, never acceptance evidence.
template<class Objective>
inline bool pair_polish(std::array<Pair,3>& q,const std::array<bool,3>& free,
    const std::array<int,3>& order,double tol,int maxit,const Objective& objective) {
  std::array<int,3> index{};int n=0;
  for(int j:order)if(free[j])index[n++]=j;
  if(n==0)return true;
  if(!(tol>=4*std::numeric_limits<double>::epsilon()))return false;
  const double eps=std::numeric_limits<double>::epsilon(),rounding=4096*eps*eps;
  double previous=INFINITY;std::int64_t remaining=solver_budget(maxit);
  for(;;) {
    const auto f=objective(q);
    if(!f.valid||!std::isfinite(f.likelihood.hi)||!std::isfinite(f.information_magnitude))return false;
    std::array<std::array<Pair,3>,3> L{};bool positive=true;
    const double allowance=rounding*f.information_magnitude+f.information_tail;
    for(int i=0;i<n;++i)for(int j=0;j<=i;++j) {
      Pair a=f.information[index[i]][index[j]];
      for(int k=0;k<j;++k)a=a-L[i][k]*L[j][k];
      if(i==j) {
        if(!std::isfinite(a.hi))return false;
        if(!(value(a)>allowance)) {
          positive=false;a=Pair(std::abs(value(a))+allowance);
        }
        if(!(sign(a)>0))return false;
        L[i][j]=square_root(a);
      } else L[i][j]=a/L[j][j];
    }
    const auto solve=[&](const std::array<Pair,3>& rhs) {
      std::array<Pair,3> y{},answer{},out{};
      for(int i=0;i<n;++i){Pair a=rhs[index[i]];for(int j=0;j<i;++j)a=a-L[i][j]*y[j];y[i]=a/L[i][i];}
      for(int i=n-1;i>=0;--i){Pair a=y[i];for(int j=i+1;j<n;++j)a=a-L[j][i]*answer[j];answer[i]=a/L[i][i];out[index[i]]=answer[i];}
      return out;
    };
    const auto step=solve(f.score);double error=0;std::array<double,3> noise{};
    for(int k=0;k<n;++k) {
      std::array<Pair,3> rhs{};rhs[index[k]]=Pair(1);const auto column=solve(rhs);
      const double bound=rounding*f.score_magnitude[index[k]]+f.score_tail[index[k]];
      for(int i=0;i<n;++i)noise[index[i]]+=std::abs(value(column[index[i]]))*bound;
    }
    double uncertainty=0;
    for(int i=0;i<3;++i) {
      Pair correction;double propagated=0;
      for(int j=0;j<3;++j){correction=correction+f.projection[i][j]*step[j];propagated+=std::abs(value(f.projection[i][j]))*noise[j];}
      error=std::max(error,std::abs(value(correction)));uncertainty=std::max(uncertainty,propagated);
    }
    if(!std::isfinite(error)||!std::isfinite(uncertainty))return false;
    if(positive&&error<=tol*.0625&&uncertainty<=tol*.25&&std::isfinite(previous)&&
        (previous<=tol*.0625||(error<=previous*.25&&error<=std::max(16*eps*eps,16*previous*previous))))return true;
    if(remaining<=0)return false;
    double fraction=std::min(1.,.125/std::max(error,std::numeric_limits<double>::min()));
    bool moved=false;
    while(remaining>0) {
      auto trial=q;bool changed=false;
      for(int i=0;i<n;++i){trial[index[i]]=q[index[i]]+Pair(fraction)*step[index[i]];changed=changed||sign(trial[index[i]]-q[index[i]])!=0;}
      if(!changed)return false;
      const auto candidate=objective(trial);--remaining;
      const double margin=rounding*(1+f.likelihood_magnitude+candidate.likelihood_magnitude);
      if(candidate.valid&&std::isfinite(candidate.likelihood.hi)&&value(candidate.likelihood-f.likelihood)>=-margin) {
        q=trial;moved=true;previous=positive?error:INFINITY;break;
      }
      fraction*=.5;
    }
    if(!moved)return false;
  }
}
}}
