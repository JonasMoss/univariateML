// Exact signs of squared distances against 3 or 12 times a held sd squared.
#pragma once
#include <algorithm>
#include <array>
#include <cmath>
#include <cstddef>
#include <cstdint>

namespace cppunivariate {
namespace sged_width_detail {

// A finite binary64 value is an integer multiple of 2^-1074. The distance
// between two such values needs at most 2099 bits, including opposite signs.
constexpr std::size_t limbs=66;
struct Distance {
  std::array<std::uint32_t,limbs> digits{};
  std::size_t length=0;
};

inline Distance magnitude(double value) {
  Distance out;
  if(value==0)return out;
  int exponent=0;
  const double fraction=std::frexp(std::abs(value),&exponent);
  auto mantissa=static_cast<std::uint64_t>(std::ldexp(fraction,53));
  int offset=exponent+1021;
  if(offset<0) {
    mantissa>>=-offset;
    offset=0;
  }
  constexpr std::uint64_t mask=0xffffffff;
  while(mantissa>0) {
    const auto index=static_cast<std::size_t>(offset/32);
    const int shift=offset%32;
    out.digits[index]|=static_cast<std::uint32_t>((mantissa&mask)<<shift);
    const auto high=(mantissa&mask)>>(32-shift);
    if(high>0)out.digits[index+1]|=static_cast<std::uint32_t>(high);
    out.length=index+1+(high>0);
    mantissa>>=32;
    offset+=32;
  }
  return out;
}

inline Distance distance(double lo,double hi) {
  const auto a=magnitude(lo),b=magnitude(hi);
  Distance out;
  const bool add=lo<0 && hi>0;
  const auto& largest=hi<=0?a:b;
  const auto& smaller=hi<=0?b:a;
  const std::size_t length=add?std::max(a.length,b.length):largest.length;
  std::uint64_t carry=0;
  for(std::size_t j=0;j<length;++j) {
    const std::uint64_t left=largest.digits[j],right=smaller.digits[j]+carry;
    out.digits[j]=static_cast<std::uint32_t>(add?left+right:left-right);
    carry=add?(left+right)>>32:left<right;
  }
  out.length=length;
  if(add && carry>0)out.digits[out.length++]=static_cast<std::uint32_t>(carry);
  while(out.length>0 && out.digits[out.length-1]==0)--out.length;
  return out;
}

inline std::array<std::uint32_t,2*limbs> square(const Distance& value,unsigned multiplier) {
  std::array<std::uint32_t,2*limbs> result{};
  for(std::size_t i=0;i<value.length;++i) {
    std::uint64_t carry=0;
    for(std::size_t j=0;j<value.length;++j) {
      const auto sum=static_cast<std::uint64_t>(value.digits[i])*value.digits[j]+result[i+j]+carry;
      result[i+j]=static_cast<std::uint32_t>(sum);
      carry=sum>>32;
    }
    result[i+value.length]=static_cast<std::uint32_t>(carry);
  }
  if(multiplier!=1) {
    std::uint64_t carry=0;
    for(auto& digit:result) {
      const auto product=static_cast<std::uint64_t>(digit)*multiplier+carry;
      digit=static_cast<std::uint32_t>(product);
      carry=product>>32;
    }
  }
  return result;
}

// Both inputs are ordered and finite and sd is positive. The coefficients
// 3 and 12 give strict comparisons for binary64. The integer comparison
// retains the low word of the observation difference.
inline int compare(double lo,double hi,double sd,unsigned coefficient) {
  const auto observed=square(distance(lo,hi),1);
  const auto expected=square(magnitude(sd),coefficient);
  for(std::size_t j=observed.size();j>0;--j) {
    if(observed[j-1]<expected[j-1])return -1;
    if(observed[j-1]>expected[j-1])return 1;
  }
  return 0;
}
} // namespace sged_width_detail
} // namespace cppunivariate
