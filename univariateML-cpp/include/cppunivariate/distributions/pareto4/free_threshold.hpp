#pragma once
#include "../../core/span.hpp"
#include "../../core/work_budget.hpp"
#include "../../special/special.hpp"
#include "../beta/mle.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

namespace cppunivariate {
namespace gb2_threshold {

enum class Status { Invalid, Numerical, Unbounded, Unattained, Ok };

struct Result {
  double threshold=NAN, scale=NAN, upper=NAN, logLik=NAN;
  Status status=Status::Invalid;
  bool certified=false;
  int iterations=0;
  double loglik_error=0;
};

struct Bracket {
  double lo=NAN,hi=NAN;
  int iterations=0;
  double mid() const { return lo+(hi-lo)/2; }
  bool valid() const { return std::isfinite(lo) && std::isfinite(hi); }
};

// The caller supplies a signed bracket. A score that rounds to zero
// can contract the bracket only using signs at other points.
template<class Score>
inline Bracket solve(double lo,double hi,double width,int maxit,const Score& score,
                     bool keep_signed=false) {
  if(!(lo<hi) || !(score(lo)>0 && score(hi)<0))return {};
  for(int it=0;it<maxit;++it) {
    if(hi-lo<=width)return {lo,hi,it};
    const double mid=lo+(hi-lo)/2;
    const int sign=score(mid);
    if(sign>0 && mid>lo)lo=mid;
    else if(sign<0 && mid<hi)hi=mid;
    else {
      const double left=lo+(mid-lo)/2,right=mid+(hi-mid)/2;
      const bool move_left=left>lo && score(left)>0;
      const bool move_right=right<hi && score(right)<0;
      if(!move_left && !move_right)return keep_signed?Bracket{lo,hi,it}:Bracket{};
      if(move_left)lo=left;
      if(move_right)hi=right;
    }
  }
  return (hi-lo<=width || keep_signed)?Bracket{lo,hi,maxit}:Bracket{};
}

inline int sign(double score,double magnitude) {
  const double guard=32*std::numeric_limits<double>::epsilon()*magnitude;
  return score>guard?1:score< -guard?-1:0;
}

inline double softplus(double z) {
  return std::max(0.,z)+std::log1p(std::exp(-std::abs(z)));
}
inline double logistic(double z) {
  return z>=0?1/(1+std::exp(-z)):std::exp(z)/(1+std::exp(z));
}

inline double log_excess(double log_t,double log_d) {
  if(log_t==-INFINITY)return log_d;
  if(log_d==-INFINITY)return log_t;
  return std::max(log_t,log_d)+std::log1p(std::exp(-std::abs(log_t-log_d)));
}

inline Result fit(span x,double p,double q,double c,double scale,double tol,int maxit) {
  Result out;
  maxit=static_cast<int>(solver_budget(maxit));
  if(x.empty() || !(tol>0 && std::isfinite(tol)) || maxit<1)return out;
  for(double v:x)if(!std::isfinite(v))return out;
  for(double value:{p,q,c,scale})
    if(!std::isnan(value) && !(value>0 && std::isfinite(value)))return out;
  const bool free_q=std::isnan(q),free_scale=std::isnan(scale);
  const double a_minus_one=std::fma(c,p,-1.);
  const auto nonexistence=[&](Status status,double ll) {
    out.status=status;out.logLik=ll;out.certified=true;return out;
  };
  if(std::isnan(p) || std::isnan(c) || a_minus_one<0 || (free_q && free_scale))
    return nonexistence(Status::Unbounded,INFINITY);
  const double smallest=*std::min_element(x.begin(),x.end());
  const double greatest=*std::max_element(x.begin(),x.end());
  if(smallest==greatest && (free_scale || free_q))
    return nonexistence(Status::Unbounded,INFINITY);
  std::vector<double> gaps;
  double sum_logs=0,positive=0;
  for(double v:x) {
    const double log_d=v==smallest?-INFINITY:special::log_abs_difference(v,smallest);
    gaps.push_back(log_d);
    if(v>smallest) {++positive;sum_logs+=log_d;}
  }
  const double n=x.size(),k=n-positive,b=c*q;
  out.status=Status::Numerical;
  if(free_scale) {
    const double tail_low=std::fma(c,q,-b);
    const double head=std::fma(b,positive,-k);
    const double displacement=std::fma(tail_low,positive,head);
    if(std::isfinite(b) && sign(displacement,std::abs(head)+positive*std::abs(tail_low))<0)
      return nonexistence(Status::Unbounded,INFINITY);
    if(head==0 && tail_low==0) {
      const double log_ratio=a_minus_one==0?0:(std::log(a_minus_one)-std::log1p(b))/c;
      const double log_g=a_minus_one==0?0:
          a_minus_one*log_ratio-(p+q)*softplus(c*log_ratio);
      const double normalizer=n*(std::log(c)-special::lbeta(p,q));
      const double minimum_term=k*log_g,tail_term=(1+b)*sum_logs;
      const double ll=normalizer+minimum_term-tail_term;
      out.loglik_error=128*std::numeric_limits<double>::epsilon()*
          (n+std::abs(normalizer)+std::abs(minimum_term)+std::abs(tail_term));
      return std::isfinite(ll)?nonexistence(Status::Unattained,ll):out;
    }
    if(!std::isfinite(b) || sign(displacement,std::abs(head)+positive*std::abs(tail_low))<=0)
      return out;
  }

  const double log_min=std::log(std::numeric_limits<double>::denorm_min());
  const double log_max=std::log(std::numeric_limits<double>::max());
  const double precision=std::min(tol/8,1e-12);
  const auto scale_at=[&](double t) {
    if(!free_scale)return Bracket{std::log(scale),std::log(scale)};
    const auto score=[&](double log_s) {
      transformed_detail::Sum total;
      double magnitude=0,sensitivity=0;
      for(double d:gaps) {
        if(d==-INFINITY && t==-INFINITY) {total.push(-p);magnitude+=p;continue;}
        const double y=log_excess(t,d),z=c*(y-log_s);
        const double up=logistic(z),down=logistic(-z);
        const double positive_term=q*up,negative_term=p*down;
        total.push(positive_term-negative_term);
        magnitude+=positive_term+negative_term;
        sensitivity+=(positive_term*down+negative_term*up)*c*
            (1+std::abs(y)+std::abs(log_s));
      }
      return sign(total.get(),2*magnitude+sensitivity/8);
    };
    return solve(log_min,log_max,precision,maxit,score);
  };
  const auto tail_at=[&](double t) {
    if(!free_q)return Bracket{std::log(q),std::log(q)};
    double target=0;
    for(double d:gaps)target+=softplus(c*(log_excess(t,d)-std::log(scale)))/n;
    if(!(target>0 && std::isfinite(target)))return Bracket{};
    if(p==1) {
      const double center=-std::log(target);
      const double error=8*std::numeric_limits<double>::epsilon()*(n+std::abs(center));
      return Bracket{center-error,center+error};
    }
    const auto score=[&](double log_q) {
      const double shape=std::exp(log_q);
      const double direct=shape<20 && p>=.01*shape?
          std::abs(special::digamma(shape+p))+std::abs(special::digamma(shape)):0;
      return sign(beta_detail::psi_increment(shape,p)-target,direct+(n+4)*target);
    };
    // At either machine limit the digamma can overflow before attaining
    // its mathematical exterior sign. Bracket on finite values instead.
    double lo=std::min(0.,std::log(p)-std::log(target)),hi=lo;
    for(int it=0;it<maxit && lo>log_min && score(lo)<=0;++it)lo=std::max(log_min,lo-2);
    for(int it=0;it<maxit && hi<log_max && score(hi)>=0;++it)hi=std::min(log_max,hi+2);
    return solve(lo,hi,precision,maxit,score);
  };
  const auto threshold_score=[&](double t,double log_s,double tail) {
    transformed_detail::Sum total;
    double magnitude=0,log_sensitivity=0;
    for(double d:gaps) {
      const double log_y=log_excess(t,d),z=c*(log_y-log_s);
      const double w=std::exp(t-log_y),up=logistic(z),down=logistic(-z);
      const double positive_term=(a_minus_one+1)*down;
      const double negative_term=(c*tail)*up+1;
      total.push(w*(positive_term-negative_term));
      magnitude+=w*(positive_term+negative_term);
      log_sensitivity+=w*(positive_term*up+(negative_term-1)*down)*c*
          (1+std::abs(log_y)+std::abs(log_s));
    }
    return sign(total.get(),magnitude*(2+std::abs(t))+log_sensitivity/8);
  };
  const auto profile_score=[&](double t) {
    const auto s=scale_at(t),shape=tail_at(t);
    if(!s.valid() || !shape.valid())return 0;
    const double q_hi=free_q?std::exp(shape.hi):q,q_lo=free_q?std::exp(shape.lo):q;
    if(threshold_score(t,s.lo,q_hi)>0)return 1;
    if(threshold_score(t,s.hi,q_lo)<0)return -1;
    return 0;
  };
  const auto point=[&](double log_t) {
    Result r=out;
    const auto s=scale_at(log_t),shape=tail_at(log_t);
    if(!s.valid() || !shape.valid() || s.hi-s.lo>tol/2 || shape.hi-shape.lo>tol/2)return r;
    const double eps=std::numeric_limits<double>::epsilon();
    if((free_scale && 8*eps*(1+std::abs(s.mid()))>tol/2) ||
       (free_q && 8*eps*(1+std::abs(shape.mid()))>tol/2))return r;
    r.iterations=s.iterations+shape.iterations;
    r.threshold=log_t==-INFINITY?smallest:smallest-std::exp(log_t);
    r.scale=free_scale?std::exp(s.mid()):scale;
    r.upper=free_q?std::exp(shape.mid()):q;
    if(!(std::isfinite(r.threshold) && r.scale>0 && std::isfinite(r.scale) &&
         r.upper>0 && std::isfinite(r.upper)))return r;
    if((free_scale && std::abs(std::log(r.scale)-s.mid())>tol/4) ||
       (free_q && std::abs(std::log(r.upper)-shape.mid())>tol/4))return r;
    if(log_t!=-INFINITY && (!(r.threshold<smallest) ||
       8*eps*(std::abs(smallest)+std::exp(log_t)*(1+std::abs(log_t)))>
           tol/2*std::max(1.,std::abs(r.threshold))))return r;
    const double log_s=std::log(r.scale);
    double ll=n*(std::log(c)-special::lbeta(p,r.upper)-log_s);
    for(double v:x) {
      if(v==r.threshold) {
        if(a_minus_one!=0)return r;
        continue;
      }
      const double l=special::log_abs_difference(v,r.threshold)-log_s,z=c*l;
      ll+=z>0?-(1+c*r.upper)*l-(p+r.upper)*std::log1p(std::exp(-z)):
                   a_minus_one*l-(p+r.upper)*std::log1p(std::exp(z));
    }
    if(!std::isfinite(ll))return r;
    r.logLik=ll;r.status=Status::Ok;
    return r;
  };
  if(a_minus_one==0 || smallest==greatest) {
    const double mode=a_minus_one==0?-INFINITY:
        std::log(scale)+(std::log(a_minus_one)-std::log1p(b))/c;
    auto r=point(mode);
    r.certified=r.status==Status::Ok;
    return r;
  }

  const double anchor=free_scale?special::log_abs_difference(greatest,smallest):
      std::log(scale)+std::log(a_minus_one)/c;
  if(!std::isfinite(anchor))return out;
  const bool multimodal=free_q;
  double lo=std::clamp(anchor,log_min,log_max),hi=lo;
  if(multimodal) {
    hi=std::min(log_max,hi+std::log(2.));
    const auto q0=tail_at(-INFINITY);
    if(!q0.valid())return out;
    const double q_hi=std::exp(q0.hi);
    double inv_gaps=0;
    for(double d:gaps)if(d!=-INFINITY)inv_gaps+=std::exp(-d);
    const auto exterior_positive=[&](double t) {
      const double first=a_minus_one-(p+q_hi)*c*std::exp(c*(t-std::log(scale)));
      const double rest=(1+c*q_hi)*inv_gaps*std::exp(t);
      return first>0 && sign(k*first-rest,k*std::abs(first)+rest)>0;
    };
    for(int it=0;it<maxit && lo>log_min && !exterior_positive(lo);++it)
      lo=std::max(log_min,lo-std::log(2.));
    if(!exterior_positive(lo))return out;
  } else {
    for(int it=0;it<maxit && lo>log_min && profile_score(lo)<=0;++it)
      lo=std::max(log_min,lo-2);
    for(int it=0;it<maxit && hi<log_max && profile_score(hi)>=0;++it)
      hi=std::min(log_max,hi+2);
  }
  const auto accept=[&](const Bracket& root) {
    if(!root.valid())return out;
    auto r=point(root.mid());
    if(r.status!=Status::Ok)return out;
    const double low_gap=std::exp(root.lo),high_gap=std::exp(root.hi);
    const double delta=(high_gap-low_gap)+8*std::numeric_limits<double>::epsilon()*
        (std::abs(smallest)+high_gap*(1+std::abs(root.hi)));
    if(!(delta<=tol*std::max(1.,std::abs(r.threshold))))return out;
    if(free_scale || free_q) {
      const auto left=free_scale?scale_at(root.lo):tail_at(root.hi);
      const auto right=free_scale?scale_at(root.hi):tail_at(root.lo);
      if(!left.valid() || !right.valid() || !(right.hi-left.lo<=tol/2))return out;
    }
    r.iterations+=root.iterations;
    r.certified=!multimodal;
    return r;
  };
  const double outer_precision=std::min(tol/16,1e-7);
  if(!multimodal)return accept(solve(lo,hi,outer_precision,maxit,profile_score,true));

  // A tiny exterior bound can leave most log-spaced points far from
  // the upper end. Cover both scales before comparing local maxima.
  const double first=std::exp(lo),last=std::exp(hi);
  std::vector<double> mesh;
  for(int i=1;i<maxit;++i) {
    const double fraction=static_cast<double>(i)/maxit;
    mesh.push_back(lo+(hi-lo)*fraction);
    mesh.push_back(std::log(first+(last-first)*fraction));
  }
  mesh.push_back(hi);
  std::sort(mesh.begin(),mesh.end());
  mesh.erase(std::unique(mesh.begin(),mesh.end()),mesh.end());
  // q can vary on the power scale even when the threshold is accurate.
  const double local_precision=std::min(outer_precision,tol/(4*std::max(1.,c)));
  double a=lo;
  int old_sign=profile_score(a);
  for(double z:mesh) {
    const int current=profile_score(z);
    if(old_sign>0 && current<0) {
      const auto candidate=accept(solve(a,z,local_precision,maxit,profile_score,true));
      if(candidate.status==Status::Ok && (out.status!=Status::Ok || candidate.logLik>out.logLik))
        out=candidate;
    }
    if(current!=0) {a=z;old_sign=current;}
  }
  return out;
}

} // namespace gb2_threshold
} // namespace cppunivariate
