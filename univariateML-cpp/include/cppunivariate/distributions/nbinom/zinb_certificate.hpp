#pragma once
#include "../../core/work_budget.hpp"
#include "zinb_mle.hpp"
#include "../../core/stop_detail.hpp"
#include <vector>

namespace cppunivariate {
namespace zinb_cert_detail {
inline double zero_logmass(double logzero,double pi) {
  if(pi==0)return logzero;
  const double positive=(1-pi)*(-std::expm1(logzero));
  if(positive<.5)return std::log1p(-positive);
  const double a=std::log(pi),b=std::log1p(-pi)+logzero,top=std::max(a,b);
  return top+std::log1p(std::exp(std::min(a,b)-top));
}
}
// Scalar probability certificate, also used by the public default dispatcher.
// See README.md for the exhaustive stationary-branch argument and margins.
inline ZinbCertificateResult zinb_fixed_size_mle(ispan x,double size,
    double fixed_prob,double fixed_pi,double tol,int maxit) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double nan=std::numeric_limits<double>::quiet_NaN();
  ZinbCertificateResult out{{size,nan,nan,nan,0,ZinbStatus::IterationFailure},{}};
  auto fail=[&](ZinbCertificateFailure reason) {
    out.fit.status=ZinbStatus::IterationFailure;out.failure=reason;
    out.stop_detail.budget_exhausted=reason==ZinbCertificateFailure::Budget;
    out.stop_detail.comparison_tie=reason==ZinbCertificateFailure::ComparisonTie;
    out.stop_detail.loglik_upper=INFINITY;return out;
  };
  const bool fp=!std::isnan(fixed_prob),fi=!std::isnan(fixed_pi);
  if(!(size>=0)||!std::isfinite(size)) {out.fit.status=ZinbStatus::InvalidSize;return out;}
  if(x.size()==0 || !(tol>0) || !std::isfinite(tol) || maxit<1 ||
     (fp && (!(fixed_prob>0 && fixed_prob<=1)||!std::isfinite(fixed_prob))) ||
     (fi && (!(fixed_pi>=0 && fixed_pi<=1)||!std::isfinite(fixed_pi)))) {
    out.fit.status=ZinbStatus::InvalidInput;return out;
  }
  double S=0,k=0;int largest=0;
  std::map<int,double> counts;
  for(int v:x) {
    if(v<0){out.fit.status=ZinbStatus::InvalidInput;return out;}
    S+=v;k+=v>0;largest=std::max(largest,v);++counts[v];
  }
  if(k==0) {
    const double p=fp?fixed_prob:(size>0?1:.5);
    const double pi=fi?fixed_pi:(size==0 || p==1?0:1);
    double ll=0;
    if(size>0 && p<1 && pi<1) {
      ll=double(x.size())*zinb_cert_detail::zero_logmass(size*std::log(p),pi);
    }
    if(!std::isfinite(ll))return fail(ZinbCertificateFailure::Arithmetic);
    out.fit={size,p,pi,ll,0,ZinbStatus::Ok};return out;
  }
  if(size==0 || fixed_prob==1 || fixed_pi==1) {
    out.fit.status=ZinbStatus::InvalidInput;return out;
  }
  if(largest>100000 || S>4503599627370496. || double(x.size())>4503599627370496.)
    return fail(ZinbCertificateFailure::Workload);
  const double n=double(x.size()),n0=n-k,r=size;
  const double eps=std::numeric_limits<double>::epsilon();
  const double margin=4096*eps*(1+n+S)*(1+largest*eps);
  if(!std::isfinite(r*n+S))return fail(ZinbCertificateFailure::Arithmetic);
  auto inflation=[&](double p) {
    if(fi)return fixed_pi;
    const double den=-std::expm1(r*std::log(p)),eta=k/n;
    return den>eta?1-eta/den:0.;
  };
  auto likelihood=[&](double p,double pi) {
    double value=0,correction=0;
    for(const auto& kv:counts) {
      const int v=kv.first;double term;
      if(v==0) {
        term=zinb_cert_detail::zero_logmass(r*std::log(p),pi);
      } else {
        term=std::log1p(-pi)+r*std::log(p)+v*std::log1p(-p);
        for(int j=0;j<v;++j)term+=std::log(r+j)-std::log(double(j)+1);
      }
      const double y=kv.second*term-correction,t=value+y;correction=(t-value)-y;value=t;
    }
    return value;
  };
  struct Root {double lo,hi;};
  std::vector<Root> roots;
  if(fp)roots.push_back({fixed_prob,fixed_prob});
  else if(!fi) {
    // The unconstrained truncated-NB maximizer, unless the zero-share
    // constraint is active, in which case the ordinary-NB maximizer.
    const double pnb=r/(r+S/n);
    const double den=-std::expm1(r*std::log(pnb)),eta=k/n;
    if(std::abs(den-eta)<=4096*eps*(1+n+S)*(den+eta))return fail(ZinbCertificateFailure::Arithmetic);
    if(den<eta) {
      const double delta=8*eps*pnb,lo=pnb-delta,hi=pnb+delta;
      if(!(lo>0 && hi<1))return fail(ZinbCertificateFailure::Arithmetic);
      roots.push_back({lo,hi});
    } else {
      if(S==k)return fail(ZinbCertificateFailure::Arithmetic);
      double lo=0,hi=1;
      for(int j=0;j<maxit;++j) {
        ++out.fit.iterations;
        const double mid=lo+(hi-lo)/2;
        if(!(mid>lo && mid<hi))return fail(ZinbCertificateFailure::Arithmetic);
        const double mu=r*(1-mid)/mid/(-std::expm1(r*std::log(mid)));
        const double pad=4096*eps*(1+mu+S/k);
        if(mu>S/k+pad)lo=mid;
        else if(mu<S/k-pad)hi=mid;
        else {
          // Retain a signed bracket: an uncertain score is never a root.
          if(hi-lo<=1e-8*std::min(lo,1-hi))break;
          const double delta=std::max(hi-lo,1.)*8192*eps;
          const double a=mid-delta,b=mid+delta;
          auto mean_at=[&](double p){return r*(1-p)/p/(-std::expm1(r*std::log(p)));};
          if(a>lo && b<hi && mean_at(a)>S/k+pad && mean_at(b)<S/k-pad){lo=a;hi=b;break;}
          return fail(ZinbCertificateFailure::Arithmetic);
        }
        if(hi-lo<=std::min(tol,1e-10)*std::min(lo,1-hi))break;
      }
      if(hi-lo>1e-8*std::min(lo,1-hi))return fail(ZinbCertificateFailure::Budget);
      roots.push_back({lo,hi});
    }
  } else if(fixed_pi==0 || n0==0) {
    const double p=r/(r+S/n),delta=8*eps*p;
    if(!(p-delta>0 && p+delta<1))return fail(ZinbCertificateFailure::Arithmetic);
    roots.push_back({p-delta,p+delta});
  } else {
    const double u=k*r/(k*r+S),v=n*r/(n*r+S);
    if(!(u>0 && u<v && v<1))return fail(ZinbCertificateFailure::Arithmetic);
    // R'(p) has the sign of -r*p^2+B*p-r*u*v.
    const double B=r*(u+v)-(v-u),D=B*B-4*r*r*u*v;
    const double dpad=4096*eps*(1+B*B+4*r*r*u*v);
    if(!std::isfinite(D) || std::abs(D)<=dpad)return fail(ZinbCertificateFailure::Arithmetic);
    std::vector<double> cuts{u};
    if(D>dpad) {
      const double a=(B-std::sqrt(D))/(2*r),b=(B+std::sqrt(D))/(2*r);
      if(a>u && a<v)cuts.push_back(a);
      if(b>u && b<v)cuts.push_back(b);
    }
    cuts.push_back(v);
    auto h=[&](double p) {
      if(p==u)return std::numeric_limits<double>::infinity();
      if(p==v)return -std::numeric_limits<double>::infinity();
      return r*std::log(p)+std::log(n*r+S)+std::log(v-p)-
        std::log(k*r+S)-std::log(p-u)-std::log(fixed_pi)+std::log1p(-fixed_pi);
    };
    // An uncertain turning-point sign can hide a tangency: stop.
    for(std::size_t j=1;j+1<cuts.size();++j)
      if(!std::isfinite(h(cuts[j])) || std::abs(h(cuts[j]))<=margin)
        return fail(ZinbCertificateFailure::Arithmetic);
    for(std::size_t j=1;j<cuts.size();++j) {
      double lo=cuts[j-1],hi=cuts[j],left=h(lo),right=h(hi);
      if((left>0)==(right>0))continue;
      // Only + to - roots are local maxima. All branches are counted.
      if(left<0)continue;
      bool enclosed=false;
      for(int iter=0;iter<maxit;++iter) {
        ++out.fit.iterations;
        const double mid=lo+(hi-lo)/2,value=h(mid);
        if(!(mid>lo && mid<hi))return fail(ZinbCertificateFailure::Arithmetic);
        if(value>margin)lo=mid;
        else if(value< -margin)hi=mid;
        else {
          const double delta=std::max(hi-lo,1.)*margin;
          const double a=mid-delta,b=mid+delta;
          if(a>lo && b<hi && h(a)>margin && h(b)< -margin){lo=a;hi=b;enclosed=true;break;}
          if(hi-lo<=1e-8*std::min(lo,1-hi)){enclosed=true;break;}
          return fail(ZinbCertificateFailure::Arithmetic);
        }
        if(hi-lo<=std::min(tol,1e-10)*std::min(lo,1-hi)){enclosed=true;break;}
      }
      if(!enclosed || hi-lo>1e-8*std::min(lo,1-hi))return fail(ZinbCertificateFailure::Budget);
      roots.push_back({lo,hi});
    }
  }
  if(roots.empty())return fail(ZinbCertificateFailure::Arithmetic);
  double best_lower=-INFINITY,best_upper=-INFINITY;std::size_t winner=0;
  for(const auto& root:roots) {
    const double p=root.lo+(root.hi-root.lo)/2,pi=inflation(p),ll=likelihood(p,pi);
    if(!fi && pi>0 && std::abs((1-pi)*(-std::expm1(r*std::log(p)))-k/n)>1e-7*(k/n))
      return fail(ZinbCertificateFailure::Arithmetic);
    if(!fi && std::abs(inflation(root.hi)-inflation(root.lo))>1e-8)
      return fail(ZinbCertificateFailure::Arithmetic);
    // For free inflation use its midpoint maximizer as a lower witness.
    // Upper bounds use Lipschitz bounds uniform over inflation (zero score
    // in pi at its interior maximizer, or a fixed attained boundary).
    const double error=(root.hi-root.lo)*(n*r/root.lo+S/(1-root.hi))+
      margin*(1+std::abs(ll));
    if(!std::isfinite(ll)||!std::isfinite(error))return fail(ZinbCertificateFailure::Arithmetic);
    out.stop_detail.candidates.push_back({{r,p,pi},ll-error,ll+error,StopModel::None});
    if(ll-error>best_lower){best_lower=ll-error;best_upper=ll+error;winner=out.stop_detail.candidates.size()-1;}
  }
  out.stop_detail.loglik_lower=best_lower;
  out.stop_detail.loglik_upper=best_upper;
  bool tie=false;
  for(std::size_t j=0;j<out.stop_detail.candidates.size();++j) {
    out.stop_detail.loglik_upper=std::max(out.stop_detail.loglik_upper,out.stop_detail.candidates[j].loglik_upper);
    if(j!=winner && out.stop_detail.candidates[j].loglik_upper>=best_lower)tie=true;
  }
  if(tie)return fail(ZinbCertificateFailure::ComparisonTie);
  const auto& candidate=out.stop_detail.candidates[winner];
  out.fit.prob=candidate.coef[1];out.fit.pi=candidate.coef[2];
  out.fit.logLik=likelihood(out.fit.prob,out.fit.pi);out.fit.status=ZinbStatus::Ok;
  return out;
}

// All stationary size branches at a held probability. No size cap is used.
inline ZinbCertificateResult zinb_fixed_prob_mle(ispan x,double prob,
    double fixed_pi,double tol,int maxit) {
  maxit = static_cast<int>(solver_budget(maxit));
  auto out=zinb_fixed_size_mle(x,1,prob,fixed_pi,tol,maxit);
  if(out.fit.status!=ZinbStatus::Ok)return out;
  double S=0,k=0;int largest=0;
  std::map<int,double> counts;
  for(int v:x){S+=v;k+=v>0;largest=std::max(largest,v);++counts[v];}
  if(k==0){out.fit=zinb_candidate_mle(x,tol,maxit,NAN,prob,fixed_pi);return out;}
  out.fit={NAN,prob,NAN,NAN,0,ZinbStatus::IterationFailure};out.stop_detail={};
  auto fail=[&](ZinbCertificateFailure reason) {
    out.failure=reason;out.fit.status=ZinbStatus::IterationFailure;
    out.stop_detail.budget_exhausted=reason==ZinbCertificateFailure::Budget;
    out.stop_detail.comparison_tie=reason==ZinbCertificateFailure::ComparisonTie;
    out.stop_detail.loglik_upper=INFINITY;return out;
  };
  if(largest>100000 || S>4503599627370496.)return fail(ZinbCertificateFailure::Workload);
  const double n=double(x.size()),n0=n-k,t=-std::log(prob),eps=std::numeric_limits<double>::epsilon();
  const bool fi=!std::isnan(fixed_pi);
  const double allowance=4096*eps*(1+largest*eps);
  struct Score {double value,error,curvature,curvature_error,harmonic,squares,q;};
  auto score=[&](double r) {
    double h=0,h2=0,correction=0,correction2=0;
    for(const auto& kv:counts)for(int j=0;j<kv.first;++j) {
      const double u=1/(r+j),y=kv.second*u-correction,w=h+y;correction=(w-h)-y;h=w;
      const double y2=kv.second*u*u-correction2,w2=h2+y2;correction2=(w2-h2)-y2;h2=w2;
    }
    double W=n,positive=0,q=0;
    if(fi) {
      const double f0=std::exp(-r*t);
      q=fixed_pi==0?1:(1-fixed_pi)*f0/(fixed_pi+(1-fixed_pi)*f0);
      W=k+n0*q;positive=n0*t*t*q*(1-q);
    } else if(-std::expm1(-r*t)>k/n) {
      const double f0=std::exp(-r*t),den=-std::expm1(-r*t);
      W=k/den;positive=k*t*t*f0/(den*den);
    }
    return Score{h-t*W,allowance*(1+h+t*W),-h2+positive,
      allowance*(1+h2+positive),h,h2,q};
  };
  const double lower=k/(2*n*t),upper=2*S/(k*t);
  if(!(lower>0 && upper>lower && std::isfinite(upper)))return fail(ZinbCertificateFailure::Arithmetic);
  const auto first=score(lower),last=score(upper);out.fit.iterations=2;
  if(!(first.value>first.error && last.value< -last.error))return fail(ZinbCertificateFailure::Arithmetic);
  struct Segment {double lo,hi;};
  std::vector<Segment> pending{{lower,upper}},roots;
  const std::int64_t budget=certificate_budget(maxit);
  while(!pending.empty()) {
    if(out.fit.iterations+2>budget)return fail(ZinbCertificateFailure::Budget);
    const auto segment=pending.back();pending.pop_back();
    double lo=segment.lo,hi=segment.hi;
    auto left=score(lo),right=score(hi);out.fit.iterations+=2;
    // Every summand in H decreases, as does the posterior zero weight q.
    // These bounds cover the entire segment before any root is discarded.
    if(fi) {
      const double sl=right.harmonic-t*(k+n0*left.q),su=left.harmonic-t*(k+n0*right.q);
      const double pad=left.error+right.error;
      if(sl>pad || su< -pad)continue;
    }
    bool decreasing=!fi;
    bool increasing=false;
    if(fi) {
      const double qmax=(right.q<=.5 && left.q>=.5)?.25:
        std::max(left.q*(1-left.q),right.q*(1-right.q));
      const double qmin=std::min(left.q*(1-left.q),right.q*(1-right.q));
      const double dpad=left.curvature_error+right.curvature_error;
      decreasing=-right.squares+n0*t*t*qmax< -dpad;
      increasing=-left.squares+n0*t*t*qmin>dpad;
    }
    if(increasing)continue; // Only minima occur on a strictly convex segment.
    if(decreasing) {
      if(left.value< -left.error || right.value>right.error)continue;
      if(!(left.value>left.error && right.value< -right.error))return fail(ZinbCertificateFailure::Arithmetic);
      bool enclosed=false;
      for(int iter=0;iter<maxit;++iter) {
        if(out.fit.iterations>=budget)return fail(ZinbCertificateFailure::Budget);
        const double mid=lo+(hi-lo)/2;const auto s=score(mid);++out.fit.iterations;
        if(!(mid>lo && mid<hi))return fail(ZinbCertificateFailure::Arithmetic);
        if(s.value>s.error)lo=mid;
        else if(s.value< -s.error)hi=mid;
        else {
          // Keep signed endpoints around an uncertain score.
          const double delta=std::max(1.,mid)*1e-9;
          const double a=mid-delta,b=mid+delta;
          if(a>lo && b<hi) {
            if(out.fit.iterations+2>budget)return fail(ZinbCertificateFailure::Budget);
            const auto sa=score(a),sb=score(b);out.fit.iterations+=2;
            if(sa.value>sa.error && sb.value< -sb.error){lo=a;hi=b;enclosed=true;break;}
          }
          if((hi-lo)/lo<5e-8){enclosed=true;break;}
          return fail(ZinbCertificateFailure::Arithmetic);
        }
        if((hi-lo)/lo<=std::min(tol,1e-10)){enclosed=true;break;}
      }
      if(!enclosed || (hi-lo)/lo>5e-8)return fail(ZinbCertificateFailure::Budget);
      roots.push_back({lo,hi});
    } else {
      const double mid=lo+(hi-lo)/2;
      if(!(mid>lo && mid<hi))return fail(ZinbCertificateFailure::Arithmetic);
      pending.push_back({mid,hi});pending.push_back({lo,mid});
    }
  }
  if(roots.empty())return fail(ZinbCertificateFailure::Arithmetic);
  double best_lower=-INFINITY;std::size_t winner=0;
  for(const auto& root:roots) {
    const double r=root.lo+(root.hi-root.lo)/2;
    const auto candidate=zinb_fixed_size_mle(x,r,prob,fixed_pi,tol,maxit);
    if(candidate.fit.status!=ZinbStatus::Ok)return fail(ZinbCertificateFailure::Arithmetic);
    if(!fi) {
      auto pi_at=[&](double rr){const double den=-std::expm1(-rr*t);return den>k/n?1-(k/n)/den:0.;};
      if(std::abs(pi_at(root.hi)-pi_at(root.lo))>1e-8)return fail(ZinbCertificateFailure::Arithmetic);
    }
    const double ll=candidate.fit.logLik,error=(root.hi-root.lo)*(S/root.lo+n*t)+
      allowance*(1+n+S)*(1+std::abs(ll));
    if(!std::isfinite(error))return fail(ZinbCertificateFailure::Arithmetic);
    out.stop_detail.candidates.push_back({{r,prob,candidate.fit.pi},ll-error,ll+error,StopModel::None});
    if(ll-error>best_lower){best_lower=ll-error;winner=out.stop_detail.candidates.size()-1;}
  }
  out.stop_detail.loglik_lower=best_lower;out.stop_detail.loglik_upper=best_lower;
  bool tie=false;
  for(std::size_t j=0;j<out.stop_detail.candidates.size();++j) {
    const auto& c=out.stop_detail.candidates[j];out.stop_detail.loglik_upper=std::max(out.stop_detail.loglik_upper,c.loglik_upper);
    if(j!=winner && c.loglik_upper>=best_lower)tie=true;
  }
  if(tie)return fail(ZinbCertificateFailure::ComparisonTie);
  const auto& c=out.stop_detail.candidates[winner];
  const auto fit=zinb_fixed_size_mle(x,c.coef[0],prob,fixed_pi,tol,maxit);
  out.fit.size=c.coef[0];out.fit.pi=c.coef[2];out.fit.logLik=fit.fit.logLik;out.fit.status=ZinbStatus::Ok;
  return out;
}
} // namespace cppunivariate
