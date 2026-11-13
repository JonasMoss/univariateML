#pragma once
#include "../sstd/local_accuracy.hpp"
#include "../binom/ztbinom_accuracy.hpp"
#include "../../core/span.hpp"
#include <vector>
namespace cppunivariate { namespace bbinom_detail {
// A conditional-on-size refinement: the integer search and face comparison
// have already finished. Rational rising-factorial scores avoid log-gamma
// subtraction, including when the other shape is held at a large value.
inline bool fallback_accuracy(ispan x,double size,double& alpha,double& beta,
                              double held_alpha,double held_beta,double tol,int maxit,StopDetail& detail) {
  const bool fa=std::isnan(held_alpha),fb=std::isnan(held_beta);
  if(!fa&&!fb)return true;
  if(!(size>=1 && size<=enumeration_budget(maxit) && size<=std::numeric_limits<int>::max()))return false;
  const int N=static_cast<int>(size);
  std::vector<double> A(N),C(N);
  for(int v:x)if(v<0||v>N)return false;
  // Frequency tails avoid O(n*N) work when the sample is replicated.
  std::vector<double> counts(N+1);for(int v:x)counts[v]+=1;
  // For symmetric histograms at N=2 or N=3, p=1/2 is stationary
  // by reflection and the profile is a binomial likelihood in the total
  // endpoint frequency. Its unique finite shape has this count-ratio form.
  // The p curvature is negative and the mixed derivative vanishes there.
  if(fa&&fb && (N==2||N==3) && counts[0]==counts[N] &&
     (N==2||counts[1]==counts[2])) {
    const double numerator=(N==2?1.:2.)*counts[1];
    const double denominator=N*counts[0]-counts[1];
    if(numerator>0 && denominator>0) {
      const double a=numerator/denominator;
      if(std::isfinite(a) && std::abs(std::fma(-a,denominator,numerator))/denominator<=tol/4*std::max(1.,a)) {
        alpha=beta=a;return true;
      }
    }
  }
  if(N==1 && fa!=fb && counts[0]>0 && counts[1]>0) {
    const auto root=binomial_accuracy::divide(binomial_accuracy::mul(
      binomial_accuracy::Pair(fa?beta:alpha),binomial_accuracy::Pair(fa?counts[1]:counts[0])),
      binomial_accuracy::Pair(fa?counts[0]:counts[1]));
    double point=root.hi+root.lo;
    if(point==0)point=std::numeric_limits<double>::denorm_min();
    const double eps=std::numeric_limits<double>::epsilon();
    const double error=binomial_accuracy::magnitude(binomial_accuracy::add(root,binomial_accuracy::Pair(-point)))+
      128*eps*eps*binomial_accuracy::magnitude(root)+4*std::numeric_limits<double>::denorm_min();
    if(!(point>0&&std::isfinite(point))||error>tol*std::max(1.,point))return false;
    if(fa)alpha=point;else beta=point;
    return true;
  }
  double left=counts[0],right=counts[N];
  for(int j=0;j<N;++j){A[j]=x.size()-left;C[j]=x.size()-right;left+=counts[j+1];right+=counts[N-j-1];}
  // Natural-coordinate scores are rational rising-factorial sums. Evaluate
  // them with two components: a sub-machine tolerance must reject an
  // unrepresentable root, rather than rejecting an exactly represented one.
  double a=alpha,b=beta,previous=INFINITY;
  const double eps=std::numeric_limits<double>::epsilon();
  const double gamma=128*(N+1.)*eps*eps;
  const auto value=[](binomial_accuracy::Pair x){return x.hi+x.lo;};
  const auto magnitude=[](binomial_accuracy::Pair x){return std::abs(x.hi)+std::abs(x.lo);};
  const auto divide=[](binomial_accuracy::Pair x,binomial_accuracy::Pair y) {
    const double leading=x.hi/y.hi;
    const auto residual=binomial_accuracy::add(x,binomial_accuracy::neg(binomial_accuracy::mul(y,binomial_accuracy::Pair(leading))));
    return binomial_accuracy::add(binomial_accuracy::Pair(leading),binomial_accuracy::Pair((residual.hi+residual.lo)/y.hi));
  };
  const auto advance=[&](bool update)->int {
    std::array<binomial_accuracy::Pair,2> g{};
    std::array<binomial_accuracy::Pair,3> information{};
    std::array<double,2> gmass{};std::array<double,3> hmass{};
    const auto total=binomial_accuracy::add(binomial_accuracy::Pair(a),binomial_accuracy::Pair(b));
    for(int j=0;j<N;++j) {
      const auto ia=divide(binomial_accuracy::Pair(1),binomial_accuracy::add(binomial_accuracy::Pair(a),binomial_accuracy::Pair(j)));
      const auto ib=divide(binomial_accuracy::Pair(1),binomial_accuracy::add(binomial_accuracy::Pair(b),binomial_accuracy::Pair(j)));
      const auto it=divide(binomial_accuracy::Pair(1),binomial_accuracy::add(total,binomial_accuracy::Pair(j)));
      const auto ga=binomial_accuracy::mul(binomial_accuracy::Pair(A[j]),ia),gb=binomial_accuracy::mul(binomial_accuracy::Pair(C[j]),ib);
      const auto gt=binomial_accuracy::mul(binomial_accuracy::Pair(double(x.size())),it);
      const auto ha=binomial_accuracy::mul(ga,ia),hb=binomial_accuracy::mul(gb,ib),ht=binomial_accuracy::mul(gt,it);
      g[0]=binomial_accuracy::add(g[0],binomial_accuracy::add(ga,binomial_accuracy::neg(gt)));
      g[1]=binomial_accuracy::add(g[1],binomial_accuracy::add(gb,binomial_accuracy::neg(gt)));
      information[0]=binomial_accuracy::add(information[0],binomial_accuracy::add(ha,binomial_accuracy::neg(ht)));
      information[1]=binomial_accuracy::add(information[1],binomial_accuracy::neg(ht));
      information[2]=binomial_accuracy::add(information[2],binomial_accuracy::add(hb,binomial_accuracy::neg(ht)));
      gmass[0]+=magnitude(ga)+magnitude(gt);gmass[1]+=magnitude(gb)+magnitude(gt);
      hmass[0]+=magnitude(ha)+magnitude(ht);hmass[1]+=magnitude(ht);hmass[2]+=magnitude(hb)+magnitude(ht);
    }
    std::array<binomial_accuracy::Pair,3> inverse{};
    if(fa&&fb) {
      const auto determinant=binomial_accuracy::add(binomial_accuracy::mul(information[0],information[2]),binomial_accuracy::neg(binomial_accuracy::mul(information[1],information[1])));
      if(!(value(information[0])>0 && value(determinant)>0))return 0;
      inverse[0]=divide(information[2],determinant);
      inverse[1]=binomial_accuracy::neg(divide(information[1],determinant));
      inverse[2]=divide(information[0],determinant);
    } else {
      const int i=fa?0:2;
      if(!(value(information[i])>0))return 0;
      inverse[i]=divide(binomial_accuracy::Pair(1),information[i]);
    }
    const auto da=binomial_accuracy::add(binomial_accuracy::mul(inverse[0],g[0]),binomial_accuracy::mul(inverse[1],g[1]));
    const auto db=binomial_accuracy::add(binomial_accuracy::mul(inverse[1],g[0]),binomial_accuracy::mul(inverse[2],g[1]));
    const double aa=magnitude(inverse[0]),ab=magnitude(inverse[1]),bb=magnitude(inverse[2]);
    const double conditioning=gamma*std::max(aa*(hmass[0]+hmass[1])+ab*(hmass[1]+hmass[2]),
                                           ab*(hmass[0]+hmass[1])+bb*(hmass[1]+hmass[2]));
    if(!(conditioning<.125))return 0;
    const double noise_a=gamma*(aa*gmass[0]+ab*gmass[1]);
    const double noise_b=gamma*(ab*gmass[0]+bb*gmass[1]);
    detail.conditioning_bound=32*std::max(noise_a/std::max(1.,a),noise_b/std::max(1.,b));
    const double error=std::max((magnitude(da)+2*noise_a)/std::max(1.,a),
                                (magnitude(db)+2*noise_b)/std::max(1.,b));
    if(!std::isfinite(error)||error>.25)return 0;
    if(error<=tol/16 && std::isfinite(previous) &&
       (previous<=tol/16 || error<=std::max(16*previous*previous,128*eps)))return 1;
    const double next_a=fa?a+value(da):a,next_b=fb?b+value(db):b;
    if(!(next_a>0&&next_b>0&&std::isfinite(next_a+next_b)))return 0;
    // A stagnating rounded correction larger than tolerance cannot be hidden
    // by the uncertified flag. The returned witness remains the search point.
    if(next_a==a&&next_b==b) {
      if(error<=tol && std::isfinite(previous) && error<=std::max(16*previous*previous,128*eps))return 1;
      if(error>tol)return 0;
    }
    if(!update)return 0;
    a=next_a;b=next_b;previous=error;return -1;
  };
  for(std::int64_t it=0;it<solver_budget(maxit);++it) {
    const int state=advance(true);
    if(state>=0){if(state==0)return false;alpha=a;beta=b;return true;}
  }
  if(advance(false)!=1)return false;
  alpha=a;beta=b;return true;
}
inline bool fallback_accuracy(ispan x,double size,double& alpha,double& beta,
                              double held_alpha,double held_beta,double tol,int maxit) {
  StopDetail detail;return fallback_accuracy(x,size,alpha,beta,held_alpha,held_beta,tol,maxit,detail);
}
} }
