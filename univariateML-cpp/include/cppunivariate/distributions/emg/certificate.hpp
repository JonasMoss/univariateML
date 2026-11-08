#pragma once
#include "fixed_shape_certificate.hpp"
#include "normal_tail_certificate.hpp"
#include <map>
#include <optional>
#include <queue>

namespace cppunivariate {
enum class EmgStatus { Ok, NormalLimit, TranslatedExponentialLimit, Unbounded, InvalidInput, IterationFailure, NumericalFailure, BudgetExhausted, Unresolved, Tie };
struct EmgResult { double mu,sigma,lambda,logLik; int iterations; EmgStatus status; StopDetail stop_detail{}; };
namespace emg_detail {
struct CertFace {
  bool valid=false;
  StopModel model=StopModel::None;
  std::vector<double> coef{};
  double ll=-INFINITY,error=0,upper=INFINITY;
  double lower()const{return ll-error;}
  FitCandidate witness()const{return {coef,lower(),ll+error,model};}
};
inline FitCandidate cert_finite_witness(span x,double mu,double sigma,double lambda) {
  CertSum ll,mag;
  const CPPUNIV_EXTENDED_REAL k=static_cast<CPPUNIV_EXTENDED_REAL>(sigma)*lambda,lograte=std::log(static_cast<CPPUNIV_EXTENDED_REAL>(lambda));
  for(double v:x) {
    const CPPUNIV_EXTENDED_REAL z=(static_cast<CPPUNIV_EXTENDED_REAL>(v)-mu)/sigma,term=logterm(z,k);
    ll.add(lograte+term);mag.add(std::abs(lograte)+std::abs(term));
  }
  const double value=static_cast<double>(ll.value),error=static_cast<double>(256*std::numeric_limits<double>::epsilon()*(x.size()+mag.value));
  return {{mu,sigma,lambda},value-error,value+error,StopModel::None};
}
inline bool cert_witness_valid(const FitCandidate& c) {
  return c.coef.size()==3&&std::isfinite(c.coef[0])&&c.coef[1]>0&&c.coef[2]>0&&std::isfinite(c.coef[1])&&std::isfinite(c.coef[2])&&
    std::isfinite(c.loglik_lower)&&std::isfinite(c.loglik_upper)&&c.loglik_lower<=c.loglik_upper;
}
inline CertFace cert_normal_face(span x,double mu,double sigma) {
  CertFace out;out.model=StopModel::Norm;
  CertSum sum,abssum;for(double v:x){sum.add(v);abssum.add(std::abs(v));}
  const CPPUNIV_EXTENDED_REAL mean=std::isnan(mu)?sum.value/x.size():mu;
  const double m=static_cast<double>(mean);
  CPPUNIV_EXTENDED_REAL largest=0;for(double v:x)largest=std::max(largest,std::abs(static_cast<CPPUNIV_EXTENDED_REAL>(v)-m));
  CertSum square;
  if(largest>0)for(double v:x){const CPPUNIV_EXTENDED_REAL d=(static_cast<CPPUNIV_EXTENDED_REAL>(v)-m)/largest;square.add(d*d/x.size());}
  const double s=std::isnan(sigma)?static_cast<double>(largest*std::sqrt(square.value)):sigma;
  if(!(std::isfinite(m)&&s>0&&std::isfinite(s)))return out;
  CertSum ll,mag;
  for(double v:x) {
    const CPPUNIV_EXTENDED_REAL z=(static_cast<CPPUNIV_EXTENDED_REAL>(v)-m)/s,ls=std::log(static_cast<CPPUNIV_EXTENDED_REAL>(s));
    ll.add(logphi(z)-ls);mag.add(z*z/2+std::abs(ls)+1);
  }
  out.coef={m,s};out.ll=static_cast<double>(ll.value);
  out.error=static_cast<double>(256*std::numeric_limits<double>::epsilon()*(x.size()+mag.value));
  const CPPUNIV_EXTENDED_REAL meanerror=std::isnan(mu)?64*std::numeric_limits<double>::epsilon()*abssum.value/x.size()+std::abs(mean-m)+64*std::numeric_limits<double>::denorm_min():0;
  const CPPUNIV_EXTENDED_REAL ratio=meanerror/s;
  CPPUNIV_EXTENDED_REAL correction=0;
  if(std::isnan(sigma))correction=ratio<1?-static_cast<CPPUNIV_EXTENDED_REAL>(x.size())*std::log1p(-ratio*ratio)/2:INFINITY;
  else correction=x.size()*ratio*ratio/2;
  out.upper=static_cast<double>(out.ll+out.error+correction);
  // Relative RMS rounding is uncontrolled at a subnormal scale. Retain the
  // proper normal witness, but do not use its likelihood as a supremum bound.
  if(std::isnan(sigma)&&s<std::numeric_limits<double>::min())out.upper=INFINITY;
  out.valid=std::isfinite(out.ll)&&std::isfinite(out.error)&&std::isfinite(out.lower())&&std::isfinite(out.ll+out.error);return out;
}
inline CertFace cert_exponential_face(span x,double mu,double lambda) {
  CertFace out;out.model=StopModel::TranslatedExp;
  const double xmin=*std::min_element(x.begin(),x.end());
  if(!std::isnan(mu)&&mu>xmin)return out;
  const double m=std::isnan(mu)?xmin:mu;
  CertSum gap;std::size_t ties=0;
  for(double v:x){gap.add(static_cast<CPPUNIV_EXTENDED_REAL>(v)-m);if(!std::isnan(mu)&&v==mu)++ties;}
  const double rate=std::isnan(lambda)?static_cast<double>(x.size()/gap.value):lambda;
  if(!(rate>0&&std::isfinite(rate)))return out;
  const CPPUNIV_EXTENDED_REAL a=x.size()*std::log(static_cast<CPPUNIV_EXTENDED_REAL>(rate)),b=rate*gap.value,c=ties*std::log(CPPUNIV_EXTENDED_LITERAL(2.0,2.0L));
  out.coef={m,rate};out.ll=static_cast<double>(a-b-c);
  out.error=static_cast<double>(256*std::numeric_limits<double>::epsilon()*(x.size()+std::abs(a)+std::abs(b)+c));
  out.upper=out.ll+out.error;
  if(std::isnan(lambda)) {
    const CPPUNIV_EXTENDED_REAL optimum=x.size()*(std::log(static_cast<CPPUNIV_EXTENDED_REAL>(x.size()))-std::log(gap.value)-1)-c;
    out.upper=std::max(out.upper,static_cast<double>(optimum+256*std::numeric_limits<double>::epsilon()*(x.size()+std::abs(optimum))));
  }
  out.valid=std::isfinite(out.ll)&&std::isfinite(out.error)&&std::isfinite(out.lower())&&std::isfinite(out.ll+out.error);return out;
}
struct CertSegment {double lo,hi,left,right,upper;};
struct CertSegmentCompare {bool operator()(const CertSegment& a,const CertSegment& b)const{return a.upper<b.upper;}};
struct OuterCertificate {
  span x;double tol,mu,sigma,lambda;int maxit;
  CertificateWork work;
  CertModel model;
  CertFace normal{},exponential{};
  FitCandidate finite{};
  bool has_finite=false,inner_failure=false;
  double incumbent=-INFINITY,small_upper=INFINITY,large_upper=INFINITY,last_upper=INFINITY;
  bool small_closed=false,large_closed=false;
  std::map<double,FixedShapeCertificate> cache{};
  std::vector<CertWinner> winners{};
  std::priority_queue<CertSegment,std::vector<CertSegment>,CertSegmentCompare> queue{};
  OuterCertificate(span data,double t,int budget,double m,double s,double l):x(data),tol(t),mu(m),sigma(s),lambda(l),maxit(budget),work(budget),model(data,m,s,l) {}
  void record(FitCandidate c) {
    if(!cert_witness_valid(c))return;
    if(!has_finite||c.loglik_lower>finite.loglik_lower){finite=c;has_finite=true;}
    incumbent=std::max(incumbent,c.loglik_lower);
  }
  std::optional<FixedShapeCertificate> profile(double k) {
    const auto found=cache.find(k);if(found!=cache.end())return found->second;
    if(!work.charge())return std::nullopt;
    const auto point=fixed_shape_certificate(x,k,1e-7,maxit,mu,sigma,lambda);
    if(!point.ok) {
      const CertModel slice(x,mu,sigma,lambda,false,k);
      const auto p=slice.initial(k);
      if(slice.domain(p)){double m,s,l;slice.coordinates(p,m,s,l);record(cert_finite_witness(x,std::isnan(mu)?m:mu,std::isnan(sigma)?s:sigma,std::isnan(lambda)?l:lambda));}
      inner_failure=true;return std::nullopt;
    }
    const double m=std::isnan(mu)?point.mu:mu,s=std::isnan(sigma)?1/point.b:sigma,l=std::isnan(lambda)?k*point.b:lambda;
    record(cert_finite_witness(x,m,s,l));
    return cache.emplace(k,point).first->second;
  }
  bool propose(double k,const FixedShapeCertificate& point) {
    if(!work.charge())return false;
    auto p=model.initial(k);
    if(model.location>=0)p[model.location]=model.fixed_rate?lambda*point.mu:point.mu*point.b;
    if(model.scale>=0)p[model.scale]=point.b;
    p[model.shape]=k;
    const auto root=cert_newton(model,p,tol,maxit);
    if(!root.ok)return true;
    double m,s,l;model.coordinates(root.center,m,s,l);
    record(cert_finite_witness(x,std::isnan(mu)?m:mu,std::isnan(sigma)?s:sigma,std::isnan(lambda)?l:lambda));
    const auto winner=cert_winner_region(model,root,maxit);
    if(!winner.ok)return true;
    for(auto& old:winners)if(winner.lo<=old.hi&&winner.hi>=old.lo) {
      old.lo=std::min(old.lo,winner.lo);old.hi=std::max(old.hi,winner.hi);
      if((winner.root.accurate&&!old.root.accurate)||(winner.root.accurate==old.root.accurate&&winner.root.ll_upper<old.root.ll_upper))old.root=winner.root;
      return true;
    }
    winners.push_back(winner);return true;
  }
  double normal_upper()const{return normal.valid?normal.upper:-INFINITY;}
  bool finite_beats_normal()const{return has_finite&&finite.loglik_lower>normal_upper();}
  bool signed_exponential_small(double k) const {
    if(!exponential.valid)return false;
    if(std::isnan(mu))return x.size()*k<=1;
    const double xmin=*std::min_element(x.begin(),x.end());
    if(mu!=xmin)return false;
    std::size_t ties=0;for(double v:x)if(v==mu)++ties;
    return x.size()*k<=ties;
  }
  bool push(double lo,double hi,double left,double right) {
    if(!work.charge())return false;
    double upper;
    if(std::isnan(lambda))upper=std::min(shape_chord_upper(left,right,x.size(),lo,hi),shape_interval_upper(left,x.size(),lo,hi));
    else upper=fixed_rate_shape_chord_upper(left,right,x,lo,hi,lambda,mu);
    if(!std::isfinite(upper)){inner_failure=true;return false;}
    queue.push({lo,hi,left,right,upper});return true;
  }
  bool covered(double lo,double hi) const {
    for(const auto& winner:winners)if(winner.lo<=lo&&winner.hi>=hi)return true;
    return false;
  }
  double split(const CertSegment& segment) const {
    for(const auto& winner:winners) {
      if(winner.lo>segment.lo&&winner.lo<segment.hi)return winner.lo;
      if(winner.hi>segment.lo&&winner.hi<segment.hi)return winner.hi;
    }
    return std::sqrt(segment.lo)*std::sqrt(segment.hi);
  }
  EmgResult stop(EmgStatus status) const {
    EmgResult out={NAN,NAN,NAN,NAN,static_cast<int>(work.used),status};
    auto& detail=out.stop_detail;
    if(has_finite)detail.candidates.push_back(finite);
    if(normal.valid)detail.candidates.push_back(normal.witness());
    if(exponential.valid)detail.candidates.push_back(exponential.witness());
    for(const auto& winner:winners) {
      double m,s,l;model.coordinates(winner.root.center,m,s,l);
      const auto candidate=cert_finite_witness(x,std::isnan(mu)?m:mu,std::isnan(sigma)?s:sigma,std::isnan(lambda)?l:lambda);
      if(cert_witness_valid(candidate))detail.candidates.push_back(candidate);
    }
    detail.loglik_lower=std::isfinite(incumbent)?incumbent:NAN;
    detail.loglik_upper=(!small_closed||!large_closed)?INFINITY:std::max({incumbent,small_upper,large_upper,last_upper});
    if(!queue.empty())detail.loglik_upper=std::max(detail.loglik_upper,queue.top().upper);
    for(const auto& winner:winners)detail.loglik_upper=std::max(detail.loglik_upper,winner.root.ll_upper);
    detail.segment_loglik_upper=last_upper;
    detail.budget_exhausted=status==EmgStatus::BudgetExhausted;
    detail.comparison_tie=status==EmgStatus::Tie;
    return out;
  }
  EmgResult stopped()const {return stop(work.exhausted?EmgStatus::BudgetExhausted:(maxit==1?EmgStatus::IterationFailure:EmgStatus::NumericalFailure));}
  EmgResult run() {
    if(std::isnan(lambda)) {
      normal=cert_normal_face(x,mu,sigma);
      if(!normal.valid){profile(1);return stop(EmgStatus::NumericalFailure);}
      incumbent=std::max(incumbent,normal.lower());
    }
    if(std::isnan(sigma)) {
      exponential=cert_exponential_face(x,mu,lambda);
      if(exponential.valid)incumbent=std::max(incumbent,exponential.lower());
    }
    // With held scale and all observations above a held location, the
    // exponential score supplies a useful proper starting rate. In particular
    // it avoids starting at rate 1/sigma when sigma is tiny. This only selects
    // an incumbent: the same exhaustive shape bounds certify the result.
    double start=1;
    if(!std::isnan(mu)&&!std::isnan(sigma)&&std::isnan(lambda)&&mu<*std::min_element(x.begin(),x.end())) {
      CertSum gap;for(double v:x)gap.add(static_cast<CPPUNIV_EXTENDED_REAL>(v)-mu);
      const double proposed=static_cast<double>(static_cast<CPPUNIV_EXTENDED_REAL>(sigma)*x.size()/gap.value);
      if(proposed>0&&std::isfinite(proposed))start=proposed;
    }
    const auto initial=profile(start);
    if(!initial)return stopped();
    if(has_finite&&(!normal.valid||finite_beats_normal())&&(!exponential.valid||finite.loglik_lower>exponential.upper))
      if(!propose(start,*initial))return stopped();
    double lo=std::min(1.,start)/(x.size()+1.),hi=4;
    for(int it=0;it<solver_budget(maxit);++it,lo*=.5) {
      if(!work.charge())return stopped();
      small_upper=conditional_small_shape_upper(x,lo,mu,sigma,lambda);
      if(small_upper<incumbent){small_closed=true;break;}
      if(signed_exponential_small(lo)){small_closed=true;small_upper=exponential.upper;break;}
      if(!(lo>0&&std::isfinite(lo)))return stop(EmgStatus::NumericalFailure);
      // A held location below the minimum can approach its exponential face
      // from above. Improve the finite incumbent while shrinking this tail;
      // an unsuccessful comparison at k=1 is no reason to discard that mode.
      const auto point=profile(lo);
      if(!point)return stopped();
      if(has_finite&&(!normal.valid||finite_beats_normal())&&
          (!exponential.valid||finite.loglik_lower>exponential.upper))
        if(!propose(lo,*point))return stopped();
      if(small_upper<incumbent){small_closed=true;break;}
    }
    if(!small_closed)return stopped();
    // Enlarge the proved excluded tail before subdividing. Starting at 1/n
    // is useful for finding a tail, but extremely wasteful with held rate:
    // its chord curvature grows as k^-4 near zero.
    for(int it=0;it<solver_budget(maxit);++it) {
      const double next=lo*2;
      if(!(next>lo&&std::isfinite(next)))break;
      if(!work.charge())return stopped();
      const double upper=conditional_small_shape_upper(x,next,mu,sigma,lambda);
      if(upper<incumbent){lo=next;small_upper=upper;continue;}
      if(signed_exponential_small(next)){lo=next;small_upper=exponential.upper;continue;}
      break;
    }
    for(int it=0;it<solver_budget(maxit);++it,hi*=2) {
      if(!work.charge())return stopped();
      large_upper=large_shape_upper(x,hi,incumbent,maxit,mu,sigma,lambda,normal.valid?normal.upper:NAN);
      if(large_upper<=incumbent){large_closed=true;break;}
      if(normal.valid&&normal.upper>=incumbent&&!std::isnan(mu)&&!std::isnan(sigma)) {
        if(normal_fixed_location_scale_tail(x,mu,sigma,hi,work)){large_closed=true;large_upper=normal.upper;break;}
        if(work.exhausted)return stopped();
      } else if(normal.valid&&normal.upper>=incumbent) return stop(EmgStatus::Unresolved);
      if(!(hi>0&&std::isfinite(hi)))return stop(EmgStatus::NumericalFailure);
    }
    if(!large_closed)return normal.valid&&!finite_beats_normal()?stop(EmgStatus::Unresolved):stopped();
    const auto left=profile(lo);if(!left)return stopped();
    const auto right=profile(hi);if(!right)return stopped();
    if(!push(lo,hi,left->loglik_upper,right->loglik_upper))return stopped();
    while(!queue.empty()) {
      const auto segment=queue.top();queue.pop();last_upper=segment.upper;
      if(segment.upper<incumbent||covered(segment.lo,segment.hi))continue;
      if(!work.charge()){queue.push(segment);return stopped();}
      const double middle=split(segment);
      if(!(middle>segment.lo&&middle<segment.hi)){queue.push(segment);return stop(EmgStatus::NumericalFailure);}
      const auto point=profile(middle);
      if(!point){queue.push(segment);return stopped();}
      bool near_winner=false;
      for(const auto& winner:winners)if(middle>=winner.lo&&middle<=winner.hi)near_winner=true;
      if(!near_winner&&(winners.empty()||point->loglik_lower>incumbent||segment.hi-segment.lo<.05*middle)) {
        if(!propose(middle,*point)){queue.push(segment);return stopped();}
      }
      if(covered(segment.lo,segment.hi))continue;
      if(!push(segment.lo,middle,segment.left,point->loglik_upper)||!push(middle,segment.hi,point->loglik_upper,segment.right)) {
        queue.push(segment);return stopped();
      }
    }
    last_upper=-INFINITY;
    std::vector<std::size_t> surviving;
    for(std::size_t i=0;i<winners.size();++i)if(winners[i].root.ll_upper>=incumbent)surviving.push_back(i);
    if(normal.valid&&normal.upper>=incumbent) {
      if(surviving.empty()&&large_closed&&small_closed&&normal.lower()>=incumbent&&(!exponential.valid||exponential.upper<normal.lower())) {
        EmgResult out={normal.coef[0],normal.coef[1],INFINITY,normal.ll,static_cast<int>(work.used),EmgStatus::NormalLimit};
        out.stop_detail=limit_detail(normal.coef,normal.ll,StopModel::Norm);return out;
      }
      return stop(EmgStatus::Unresolved);
    }
    if(exponential.valid&&exponential.upper>=incumbent) {
      if(!surviving.empty())return stop(EmgStatus::Tie);
      if(exponential.lower()<incumbent)return stop(EmgStatus::NumericalFailure);
      EmgResult out={exponential.coef[0],0,exponential.coef[1],exponential.ll,static_cast<int>(work.used),EmgStatus::TranslatedExponentialLimit};
      out.stop_detail=limit_detail(exponential.coef,exponential.ll,StopModel::TranslatedExp);return out;
    }
    if(surviving.size()>1)return stop(EmgStatus::Tie);
    if(surviving.empty())return stop(EmgStatus::NumericalFailure);
    const auto& winner=winners[surviving[0]];
    if(!winner.root.accurate)return stop(EmgStatus::NumericalFailure);
    double m,s,l;model.coordinates(winner.root.center,m,s,l);
    if(!std::isnan(mu))m=mu;
    if(!std::isnan(sigma))s=sigma;
    if(!std::isnan(lambda))l=lambda;
    const auto candidate=cert_finite_witness(x,m,s,l);if(!cert_witness_valid(candidate))return stop(EmgStatus::NumericalFailure);
    EmgResult out={m,s,l,(candidate.loglik_lower+candidate.loglik_upper)/2,static_cast<int>(work.used),EmgStatus::Ok};
    return out;
  }
};
inline EmgResult outer_certificate(span x,double tol,int maxit,double mu,double sigma,double lambda) {
  OuterCertificate certificate(x,tol,maxit,mu,sigma,lambda);return certificate.run();
}
}
}
