#pragma once
#include "../../core/span.hpp"
#include <array>
#include <cmath>
#include <cstdint>
namespace cppunivariate { namespace bhatt_detail {
// Exact sums of binary64 products. The grid starts at 2^-2304; 72 limbs
// cover products, cancellation, and every possible 64-bit sample count.
// No floating product or sum enters the sign comparison.
template<std::size_t N> struct NormalMagnitude {
  std::array<std::uint64_t,N> word{};
  void add(std::size_t i,std::uint64_t value) {
    while(value&&i<N){const auto old=word[i];word[i]+=value;value=word[i]<old?1:0;++i;}
  }
};
inline std::array<std::uint64_t,2> normal_product(std::uint64_t a,std::uint64_t b) {
  const auto al=a&UINT64_C(0xffffffff),ah=a>>32,bl=b&UINT64_C(0xffffffff),bh=b>>32;
  std::uint64_t lo=al*bl,hi=ah*bh;
  for(auto cross:std::array<std::uint64_t,2>{{al*bh,ah*bl}}){const auto old=lo;lo+=cross<<32;hi+=(cross>>32)+(lo<old?1:0);}
  return {{lo,hi}};
}
template<std::size_t N> inline int normal_compare(const NormalMagnitude<N>& a,const NormalMagnitude<N>& b) {
  for(std::size_t i=N;i>0;--i){if(a.word[i-1]>b.word[i-1])return 1;if(a.word[i-1]<b.word[i-1])return -1;}return 0;
}
template<std::size_t N> inline NormalMagnitude<N> normal_difference(const NormalMagnitude<N>& a,const NormalMagnitude<N>& b) {
  NormalMagnitude<N> out;std::uint64_t borrow=0;
  for(std::size_t i=0;i<N;++i){const auto first=a.word[i]-b.word[i];out.word[i]=first-borrow;borrow=(a.word[i]<b.word[i]||first<borrow)?1:0;}return out;
}
struct NormalExactSum {
  NormalMagnitude<72> positive,negative;
  void add(double a,double b,bool subtract=false) {
    if(a==0||b==0)return;
    int ea=0,eb=0;const auto ma=static_cast<std::uint64_t>(std::ldexp(std::frexp(std::abs(a),&ea),53));
    const auto mb=static_cast<std::uint64_t>(std::ldexp(std::frexp(std::abs(b),&eb),53));
    const auto product=normal_product(ma,mb);const int bit=ea+eb-106+2304;
    const std::size_t index=static_cast<std::size_t>(bit/64);const int shift=bit%64;
    auto& target=(std::signbit(a)!=std::signbit(b))!=subtract?negative:positive;
    if(shift==0){target.add(index,product[0]);target.add(index+1,product[1]);}
    else {target.add(index,product[0]<<shift);target.add(index+1,(product[1]<<shift)|(product[0]>>(64-shift)));target.add(index+2,product[1]>>(64-shift));}
  }
  int sign() const {return normal_compare(positive,negative);}
  NormalMagnitude<72> magnitude() const {return sign()<0?normal_difference(negative,positive):normal_difference(positive,negative);}
};
inline NormalExactSum normal_location_sum(span x,double center) {
  NormalExactSum sum;for(double v:x){sum.add(v,1);sum.add(center,1,true);}return sum;
}
inline NormalExactSum normal_variance_sum(span x,double center,double scale) {
  NormalExactSum sum;
  for(double v:x){sum.add(v,v);sum.add(center,center);sum.add(v,center,true);sum.add(v,center,true);sum.add(scale,scale,true);}return sum;
}
inline bool normal_stationary_exact(span x,double center,double scale,bool free_location,bool free_scale) {
  return (!free_location||normal_location_sum(x,center).sign()==0)&&(!free_scale||normal_variance_sum(x,center,scale).sign()==0);
}
inline bool normal_symmetric_exact(double a,double b,double center) {
  NormalExactSum sum;sum.add(a,1);sum.add(b,1);sum.add(center,1,true);sum.add(center,1,true);return sum.sign()==0;
}
inline int normal_width_score_sign(span x,double center,double scale,bool free_location) {
  const auto variance=normal_variance_sum(x,center,scale);const int sign=variance.sign();
  if(!free_location)return sign;
  const auto location=normal_location_sum(x,center);
  if(sign<0)return -1;
  if(sign==0)return location.sign()==0?0:-1;
  // Profile the exact arithmetic mean: n*A-B^2, where
  // A=sum (x-center)^2-n*scale^2 and B=sum (x-center).
  const auto A=variance.magnitude(),B=location.magnitude();NormalMagnitude<144> left,right;
  for(std::size_t i=0;i<72;++i)if(A.word[i]){const auto product=normal_product(A.word[i],static_cast<std::uint64_t>(x.size()));left.add(i+36,product[0]);left.add(i+37,product[1]);}
  for(std::size_t i=0;i<72;++i)if(B.word[i])for(std::size_t j=0;j<72;++j)if(B.word[j]){const auto product=normal_product(B.word[i],B.word[j]);right.add(i+j,product[0]);right.add(i+j+1,product[1]);}
  return normal_compare(left,right);
}
}}
