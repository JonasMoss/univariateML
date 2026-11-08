#pragma once
#include "../ztbinom/face_certificate.hpp"
namespace cppunivariate {
namespace tbinom_detail {
// Truncation endpoints may legitimately be infinite. The generic finite-
// coefficient witness guard cannot represent that family-specific domain.
inline void record_witness(StopDetail& d,std::vector<double> coef,double ll,
    bool budget,bool comparison=false) {
  if(coef.size()!=4 || !std::isfinite(ll) || !std::isfinite(coef[0]) ||
     !std::isfinite(coef[3]) || !(coef[0]>=0 && coef[0]<=1) ||
     !(coef[3]>=0 && coef[3]==std::floor(coef[3])) ||
     std::isnan(coef[1]) || std::isnan(coef[2]) || !(coef[1]<coef[2])) {
    d.loglik_upper=INFINITY;d.budget_exhausted=budget;return;
  }
  d.candidates.push_back({coef,ll,ll});
  d.loglik_lower=std::isnan(d.loglik_lower)?ll:std::max(d.loglik_lower,ll);
  d.loglik_upper=INFINITY;d.budget_exhausted=budget;d.comparison_tie=comparison;
}
inline bool face_product(std::uint64_t a,std::uint64_t b,std::uint64_t& out) {
  if(b>0 && a>std::numeric_limits<std::uint64_t>::max()/b)return false;
  out=a*b;return true;
}
inline bool face_add(std::uint64_t a,std::uint64_t b,std::uint64_t& out) {
  if(a>std::numeric_limits<std::uint64_t>::max()-b)return false;
  out=a+b;return true;
}
// Exact finite-window criterion, conditional only on integer comparisons.
// If a mean-matched Poisson law has rational rate, its scaled probabilities
// and factorial moments can be compared without interpreting a tiny gap as zero.
// Overflow refuses this optional criterion; the ordinary signed route remains.
inline double poisson_window_face(ispan x,int U,
    binomial_certificate_detail::Work& work,bool& exhausted,double& loglik,bool second_order,double& cutoff) {
  exhausted=false;loglik=NAN;
  if(U<2 || x.size()==0)return NAN;
  std::vector<std::uint64_t> factorial{1};
  for(int k=1;k<=U;++k) {
    if(!work.charge(1)){exhausted=true;return NAN;}
    std::uint64_t f=0;if(!face_product(factorial.back(),static_cast<std::uint64_t>(k),f))return NAN;
    factorial.push_back(f);
  }
  // All following loops are bounded by the represented factorial table.
  const std::int64_t width=static_cast<std::int64_t>(U)+1;
  if(!work.charge(static_cast<std::int64_t>(x.size())+4*width*width)){exhausted=true;return NAN;}
  std::vector<std::uint64_t> count(static_cast<std::size_t>(width),0);
  std::uint64_t S=0,F2=0;
  for(int v:x) {
    if(v<0 || v>U)return NAN;
    if(!face_add(S,static_cast<std::uint64_t>(v),S) ||
       !face_add(F2,static_cast<std::uint64_t>(v)*(v-1),F2))return NAN;
    ++count[static_cast<std::size_t>(v)];
  }
  const std::uint64_t n=x.size();std::uint64_t us=0,un=0;
  if(!face_product(static_cast<std::uint64_t>(U),S,us) ||
     !face_product(static_cast<std::uint64_t>(U),n,un) || us<=F2 || un<=S)return NAN;
  std::uint64_t a=us-F2,d=un-S;
  const auto gcd=std::gcd(a,d);a/=gcd;d/=gcd;
  std::vector<std::uint64_t> weight(static_cast<std::size_t>(width),0);
  std::uint64_t Z=0,Q1=0;
  for(int k=0;k<=U;++k) {
    std::uint64_t w=factorial[U]/factorial[k];
    for(int j=0;j<k;++j)if(!face_product(w,a,w))return NAN;
    for(int j=k;j<U;++j)if(!face_product(w,d,w))return NAN;
    weight[k]=w;
    std::uint64_t term=0;
    if(!face_add(Z,w,Z) || !face_product(w,static_cast<std::uint64_t>(k),term) || !face_add(Q1,term,Q1))return NAN;
  }
  std::uint64_t left=0,right=0;
  if(!face_product(S,Z,left) || !face_product(n,Q1,right) || left!=right)return NAN;
  if(second_order) {
    // The integer mean identity above also proves equality of second moments.
    if(!work.charge(width*width+4*width)){exhausted=true;return NAN;}
    const double eps=8192*std::numeric_limits<double>::epsilon();
    const double mu=double(S)/n,abar=-double(F2)/(2*double(n));
    double variance=0,covariance=0;
    for(int i=1;i<=U;++i)for(int j=0;j<i;++j) {
      const double w=(double(weight[i])/Z)*(double(weight[j])/Z),dx=i-j;
      variance+=w*dx*dx;
      covariance+=w*(-double(i)*(i-1)/2+double(j)*(j-1)/2)*dx;
    }
    if(!(variance>0) || !std::isfinite(covariance)){work.numerical=true;return NAN;}
    const double beta=covariance/variance;
    const double pad=eps*(1+double(U)*U+std::abs(abar)+std::abs(beta)*(U+std::abs(mu)+1));
    double D=0,H=0,R=0,pb=0,qb=0;
    for(int k=0;k<=U;++k) {
      const double q=double(weight[k])/Z,a0=-double(k)*(k-1)/2;
      const double b0=-double(k)*(k-1)*(2*double(k)-1)/12;
      const double h=std::abs(a0-abar-beta*(k-mu));
      const double hp=h+pad,hm=std::max(0.,h-pad);
      D+=q*hm*hm;H=std::max(H,hp);
      R+=q*((2./3)*hp*hp*hp+hp*std::abs(b0)+a0*a0);
      pb+=(double(count[k])/n)*b0;qb+=q*b0;
    }
    const double B=pb-qb-.5*D*(1-eps)+eps*(1+std::abs(pb)+std::abs(qb)+D);
    R=R*(1+eps)+eps;
    if(!(B<0))return NAN;
    cutoff=std::ceil(std::max({2.*U,4*H,4*R/(-B)}));
    if(!std::isfinite(cutoff) || cutoff>=4503599627370496.){work.numerical=true;return NAN;}
  } else for(int r=2;r<=U;++r) {
    std::uint64_t empirical=0,poisson=0;
    for(int k=r;k<=U;++k) {
      const std::uint64_t choose=factorial[k]/factorial[r]/factorial[k-r];
      std::uint64_t term=0;
      if(!face_product(count[k],choose,term) || !face_add(empirical,term,empirical) ||
         !face_product(weight[k],choose,term) || !face_add(poisson,term,poisson))return NAN;
    }
    if(!face_product(empirical,Z,left) || !face_product(n,poisson,right) || left<right)return NAN;
  }
  loglik=0;
  for(int k=0;k<=U;++k)if(count[k]>0)loglik+=count[k]*std::log(double(weight[k])/double(Z));
  return double(a)/double(d);
}
// Exact factorial-moment dominance is sufficient at every finite size.
inline double exact_poisson_moment_face(ispan x,int U,
    binomial_certificate_detail::Work& work,bool& exhausted,double& loglik) {
  double cutoff=INFINITY;
  return poisson_window_face(x,U,work,exhausted,loglik,false,cutoff);
}
// Project a(k)=-k(k-1)/2 off constants and X in L2(Q), obtaining h.
// Q_N=Q(1+h/N) preserves mean. Jensen plus its relative entropy gives
// (LL_N-LL_face)/n <= B/N^2+R/N^3, uniformly in probability, where
// B=E_P b-E_Q b-E_Q h^2/2 and b=-sum(j^2)/2. For N>=2U,2|max h|,
// R=(2/3)E|h|^3+E|hb|+E a^2 bounds both Taylor remainders.
// Computed comparisons use padded coefficients and twice the needed cutoff.
inline double second_order_poisson_cutoff(ispan x,int U,
    binomial_certificate_detail::Work& work,bool& exhausted,double& rate,double& loglik) {
  double cutoff=INFINITY;
  rate=poisson_window_face(x,U,work,exhausted,loglik,true,cutoff);
  return std::isfinite(rate)?cutoff:INFINITY;
}
} // namespace tbinom_detail
} // namespace cppunivariate
