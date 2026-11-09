// Included after Data is defined in mle.hpp. No likelihood search is evidence
// for a winning face: this tests a global supporting-polynomial inequality.
#pragma once
namespace cppunivariate {
namespace bnbinom_detail {
struct FacePolynomialProof {
  bool proved=false;
  bool budget_exhausted=false;
  long long evaluations=0;
};
struct FaceCoefficient { double lower,upper; };
inline FacePolynomialProof polynomial_mixture_face(const Data &d,double r,
                                                   bool poisson,int maxit) {
  FacePolynomialProof out;
  const long long cap=certificate_budget(maxit);
  if(d.x.empty() || !(d.n>0) || (!poisson && (!(r>0) || !std::isfinite(r))))return out;
  const int degree=d.x.back();
  if(degree<1)return out;
  // Degree and every de Casteljau coefficient update spend the same bounded
  // certificate-evaluation allowance. Check before allocating a dense vector.
  if(degree>cap){out.budget_exhausted=true;return out;}
  CPPUNIV_EXTENDED_REAL total=0;
  for(std::size_t i=0;i<d.x.size();++i)total+=d.count[i]*d.x[i];
  const double mean=static_cast<double>(total/d.n);
  if(!(mean>0) || !std::isfinite(mean))return out;
  const double q=poisson?mean:mean/(r+mean);
  if(!(q>0) || !std::isfinite(q))return out;
  std::vector<FaceCoefficient> coefficients;
  coefficients.reserve(static_cast<std::size_t>(degree));
  std::size_t index=0;
  CPPUNIV_EXTENDED_REAL count=0,weighted=0;
  double inverse_binomial=1,inverse_error=0;
  for(int k=0;k<degree;++k) {
    if(++out.evaluations>cap){out.budget_exhausted=true;return out;}
    while(index<d.x.size() && d.x[index]<=k) {
      count+=d.count[index];weighted+=d.count[index]*d.x[index];++index;
    }
    const double next=index<d.x.size() && d.x[index]==k+1?d.count[index]:0;
    const CPPUNIV_EXTENDED_REAL positive=weighted+(k+1.)*next;
    const CPPUNIV_EXTENDED_REAL negative=poisson?q*count:q*(weighted+r*count);
    // The exact fitted mean puts the derivative root at y=1. These prefix
    // sums are the exact quotient P(y)/(1-y), evaluated with a cancellation
    // margin that includes the fitted-ratio arithmetic.
    const double value=static_cast<double>(positive-negative);
    const double margin=(1024+16*static_cast<double>(index+1))*std::numeric_limits<double>::epsilon()*
      static_cast<double>(std::abs(positive)+std::abs(negative));
    const double weight_error=(std::abs(value)+margin)*inverse_binomial*inverse_error;
    const double lo=(value-margin)*inverse_binomial-weight_error;
    const double hi=(value+margin)*inverse_binomial+weight_error;
    if(!std::isfinite(lo+hi) || !(inverse_binomial>=std::numeric_limits<double>::min()) ||
       ((value-margin)!=0 && lo==0) || ((value+margin)!=0 && hi==0))return out;
    coefficients.push_back({lo,hi});
    if(k<degree-1) {
      inverse_binomial*=static_cast<double>(k+1)/(degree-1-k);
      inverse_error+=8*std::numeric_limits<double>::epsilon()*(1+inverse_error);
    }
  }
  // Q(y)>0 for y>=0 iff (1-z)^m Q(z/(1-z))>0 on [0,1).
  // Its degree-m Bernstein coefficients are Q_j / choose(m,j).
  // Subdivision preserves coefficient enclosures. Strictly positive lower
  // coefficients prove the inequality on a whole subinterval.
  std::vector<std::vector<FaceCoefficient>> pending;
  pending.push_back(std::move(coefficients));
  while(!pending.empty()) {
    if(++out.evaluations>cap){out.budget_exhausted=true;return out;}
    auto current=std::move(pending.back());pending.pop_back();
    bool positive=true;
    for(const auto &c:current)positive=positive && c.lower>0;
    if(positive)continue;
    // A nonpositive or unresolved endpoint cannot be repaired by subdivision.
    // It rejects this sufficient proof, not the face itself.
    if(!(current.front().lower>0) || !(current.back().lower>0))return out;
    const std::size_t n=current.size();
    std::vector<FaceCoefficient> left(n),right(n);
    left[0]=current[0];right[n-1]=current[n-1];
    for(std::size_t level=1;level<n;++level) {
      for(std::size_t j=0;j<n-level;++j) {
        if(++out.evaluations>cap){out.budget_exhausted=true;return out;}
        const double lo=current[j].lower*.5+current[j+1].lower*.5;
        const double hi=current[j].upper*.5+current[j+1].upper*.5;
        const double error=8*std::numeric_limits<double>::epsilon()*
          (std::abs(current[j].lower)+std::abs(current[j+1].lower)+
           std::abs(current[j].upper)+std::abs(current[j+1].upper));
        current[j]={lo-error,hi+error};
      }
      left[level]=current[0];right[n-1-level]=current[n-1-level];
    }
    pending.push_back(std::move(right));pending.push_back(std::move(left));
  }
  out.proved=true;return out;
}
// Equal positive frequencies at 0,...,4: Poisson(2) uniquely maximizes
// likelihood over compound Poisson laws. Every BNB law is compound Poisson
// (its gamma-product/ratio mixing density is HCM and hence GGC).
// The exact degree-15 nonnegative polynomial certificate and derivation are
// retained in 2026-11-08-bnbinom-allfree-data/compound-poisson-proof.py.
inline bool compound_poisson_uniform_four_face(const Data &d) {
  if(d.x.size()!=5 || d.count.size()!=5 || !(d.count[0]>0))return false;
  for(std::size_t j=0;j<5;++j)
    if(d.x[j]!=static_cast<int>(j) || d.count[j]!=d.count[0])return false;
  return true;
}
} // namespace bnbinom_detail
} // namespace cppunivariate
