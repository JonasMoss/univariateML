#pragma once
#include "certificate.hpp"
#include "../pois/dpqr.hpp"
#include <numeric>
namespace cppunivariate {
namespace binomial_certificate_detail {
// Signed Jensen bound; derivation and the infinite-support compression are in
// the binom-faces report. This is a sufficient certificate, not a criterion.
inline double signed_cutoff(ispan x,int L,double U,double lambda,Work& work,bool& exhausted) {
  exhausted=false;
  if(!(lambda>0) || !std::isfinite(lambda))return INFINITY;
  double empirical=0,mean=0;int mx=L;
  for(int v:x){empirical+=double(v)*(v-1);mean+=v;mx=std::max(mx,v);}
  empirical/=x.size();mean/=x.size();
  double f2=0,f3=0,qmean=0,logz=0;
  if(std::isfinite(U)) {
    if(U>std::numeric_limits<int>::max()){work.numerical=true;return INFINITY;}
    if(U-L+1>enumeration_budget(work.maxit)-work.cells){exhausted=true;return INFINITY;}
    const auto z=mass(std::log(lambda),L,static_cast<int>(U),work);
    if(!z.ok){exhausted=!work.numerical;return INFINITY;}
    logz=z.logz;qmean=z.mean;
    if(!work.charge(static_cast<std::int64_t>(U)-L+1)){exhausted=true;return INFINITY;}
    for(int k=L;;++k) {
      const double q=std::exp(k*std::log(lambda)-special::lgamma(k+1.)-logz);
      f2+=q*double(k)*(k-1);f3+=q*double(k)*(k-1)*(k-2);
      if(k==U)break;
    }
  } else {
    const double logtail=L==0?0:distributions::ppois(L-1,lambda,false,true);
    if(!std::isfinite(logtail))return INFINITY;
    logz=lambda+logtail;
    const auto factorial=[&](int r) {
      const double t=L<=r?0:distributions::ppois(L-r-1,lambda,false,true);
      return std::exp(r*std::log(lambda)+t-logtail);
    };
    qmean=factorial(1);f2=factorial(2);f3=factorial(3);
  }
  const double eps=8192*std::numeric_limits<double>::epsilon();
  if(!std::isfinite(f2+f3+qmean) || std::abs(qmean-mean)>eps*(1+mean)) {
    work.numerical=true;return INFINITY;
  }
  const double gap=empirical-f2-eps*(1+empirical+f2);
  if(!(gap>0))return INFINITY;
  const double remainder=(f3/3+f2/2)*(1+eps)+eps;
  // N >= 8*remainder/gap controls the log(1-j/N) remainder.
  if(std::isfinite(U)) {
    const double end=std::ceil(std::max({double(mx),2*U,8*remainder/gap}));
    if(!(end<4503599627370496.)){work.numerical=true;return INFINITY;}
    return end;
  }
  double K=std::ceil(std::max({double(mx),double(L)+2,2*lambda+2,4*remainder/gap}));
  for(std::int64_t j=0;j<solver_budget(work.maxit);++j) {
    if(++work.evaluations>certificate_budget(work.maxit)){exhausted=true;return INFINITY;}
    if(!(K<2251799813685248.)) {work.numerical=true;return INFINITY;}
    const double r=lambda/(K+1),H=r/(1-r)+r/((K-L)*(1-r)*(1-r));
    const double logq=K*std::log(lambda)-special::lgamma(K+1)-logz;
    const double loglow=L*std::log(lambda)-special::lgamma(L+1.)-logz;
    const double logd=logq+std::log(r)-std::log(K-L)-2*std::log1p(-r);
    // Both bounds decrease for all larger K. Compare in logs to retain
    // astronomically small probabilities without treating underflow as zero.
    const double logdeltaN=logq+std::log1p(H)+std::log(std::log1p(H))+std::log(2*K+1);
    if(logd+eps*(1+std::abs(logd)+std::abs(loglow))<loglow &&
       logdeltaN+eps*(1+std::abs(logdeltaN))<std::log(gap/8))return 2*K;
    K*=2;
  }
  exhausted=true;return INFINITY;
}
inline double signed_cutoff(ispan x,int L,double U,double lambda,Work& work) {
  bool exhausted=false;return signed_cutoff(x,L,U,lambda,work,exhausted);
}
// Exact empirical Poisson proportions on a finite window attain the
// multinomial upper bound. Reduced integer ratios avoid tolerance-based ties.
inline double saturated_poisson_rate(ispan x,int L,int U,Work& work,bool& exhausted,double& loglik) {
  exhausted=false;loglik=NAN;
  if(U-L<2 || static_cast<std::int64_t>(U)-L+1>static_cast<std::int64_t>(x.size()))return NAN;
  if(!work.charge(static_cast<std::int64_t>(U)-L+1+x.size())){exhausted=true;return NAN;}
  std::vector<std::uint64_t> count(static_cast<std::size_t>(U)-L+1,0);
  for(int v:x)++count[static_cast<std::size_t>(v-L)];
  std::uint64_t numerator=0,denominator=0;double entropy=0;
  for(int k=L;k<U;++k) {
    const auto a=count[static_cast<std::size_t>(k-L)+1],b=count[static_cast<std::size_t>(k-L)];
    if(a==0 || b==0 || a>std::numeric_limits<std::uint64_t>::max()/static_cast<std::uint64_t>(k+1))return NAN;
    const std::uint64_t product=a*static_cast<std::uint64_t>(k+1),gcd=std::gcd(product,b);
    const std::uint64_t num=product/gcd,den=b/gcd;
    if(k==L){numerator=num;denominator=den;}
    else if(num!=numerator || den!=denominator)return NAN;
    entropy+=b*std::log(b/double(x.size()));
  }
  loglik=entropy+count.back()*std::log(count.back()/double(x.size()));
  return double(numerator)/double(denominator);
}
enum class FaceCoverage { Face, Finite, Unresolved, Failure };
template<class Evaluate>
inline FaceCoverage cover_face(ispan x,double first,double end,double face,
                               Work& work,Evaluate evaluate) {
  std::vector<std::pair<double,double>> pending{{first,end-1}};
  const double margin=1e-9*x.size()+4096*std::numeric_limits<double>::epsilon()*(1+std::abs(face));
  bool ambiguous=false;
  while(!pending.empty()) {
    if(++work.evaluations>certificate_budget(work.maxit))return FaceCoverage::Failure;
    const auto segment=pending.back();pending.pop_back();
    const double A=segment.first,B=segment.second;
    if(A>B)continue;
    double cost=x.size();for(int v:x)cost+=v;
    if(cost>enumeration_budget(work.maxit)-work.cells || !work.charge(static_cast<std::int64_t>(cost)))return FaceCoverage::Failure;
    const double ll=evaluate(A);
    if(!std::isfinite(ll)){work.numerical=true;return FaceCoverage::Failure;}
    if(ll>face+margin)return FaceCoverage::Finite;
    if(A==B) {if(!(ll+margin<face))ambiguous=true;continue;}
    const double error=block_error(x,A,B,work);
    if(ll+error+margin<face)continue;
    const double mid=std::floor(A+(B-A)/2);
    pending.push_back({mid+1,B});pending.push_back({A,mid});
  }
  return ambiguous?FaceCoverage::Unresolved:FaceCoverage::Face;
}
inline void certified_face(StopDetail& d,std::vector<double> coef,double ll) {
  d.candidates.push_back({coef,ll,ll});
  d.loglik_lower=d.limit_loglik;
  d.loglik_upper=d.limit_loglik+1e-9+4096*std::numeric_limits<double>::epsilon()*(1+std::abs(d.limit_loglik));
  d.comparison_tie=false;d.budget_exhausted=false;
}
} // namespace binomial_certificate_detail
} // namespace cppunivariate
