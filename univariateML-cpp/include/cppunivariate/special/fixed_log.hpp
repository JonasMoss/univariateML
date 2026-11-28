#pragma once

#include <array>
#include <cmath>
#include <cstdint>

namespace cppunivariate { namespace special { namespace fixed_log {

// Unsigned base-2^32 fixed point, with 640 fractional bits and 64 integer bits.
// Used only when the three-word logarithms cannot resolve a centered argument.
constexpr int fraction = 20;
constexpr int words = fraction + 2;
typedef std::array<std::uint32_t, words> Number;

inline Number add(Number a, const Number &b) {
  std::uint64_t carry = 0;
  for (int i = 0; i < words; ++i) {
    carry += std::uint64_t(a[i]) + b[i];
    a[i] = static_cast<std::uint32_t>(carry);
    carry >>= 32;
  }
  return a;
}

inline bool less(const Number &a, const Number &b) {
  for (int i = words - 1; i >= 0; --i)
    if (a[i] != b[i]) return a[i] < b[i];
  return false;
}

inline Number subtract(Number a, const Number &b) {
  std::uint64_t borrow = 0;
  for (int i = 0; i < words; ++i) {
    const std::uint64_t rhs = std::uint64_t(b[i]) + borrow;
    borrow = a[i] < rhs;
    a[i] = static_cast<std::uint32_t>(a[i] - rhs);
  }
  return a;
}

inline Number multiply(const Number &a, const Number &b) {
  std::array<std::uint32_t, words + fraction> product{};
  for (int i = 0; i < words; ++i) {
    std::uint64_t carry = 0;
    for (int j = 0; j < words && i+j < words+fraction; ++j) {
      carry += std::uint64_t(a[i]) * b[j] + product[i+j];
      product[i+j] = static_cast<std::uint32_t>(carry);
      carry >>= 32;
    }
    if (i+words < words+fraction) product[i+words] = static_cast<std::uint32_t>(carry);
  }
  Number result{};
  for (int i = 0; i < words; ++i) result[i] = product[i+fraction];
  return result;
}

inline Number divide_small(Number a, std::uint32_t divisor) {
  std::uint64_t remainder = 0;
  for (int i = words - 1; i >= 0; --i) {
    const std::uint64_t value = (remainder << 32) | a[i];
    a[i] = static_cast<std::uint32_t>(value / divisor);
    remainder = value % divisor;
  }
  return a;
}

// 2 atanh(n/d) for 0 <= n/d <= 1/3. The omitted series after 208
// terms is below 2^-650. Each fixed-point operation truncates toward zero.
inline Number twice_atanh(std::uint64_t n, std::uint64_t d) {
  Number ratio{};
  for (int i = 32*fraction - 1; i >= 0; --i) {
    n *= 2;
    if (n >= d) {
      n -= d;
      ratio[i/32] |= std::uint32_t(1) << (i%32);
    }
  }
  const Number square = multiply(ratio, ratio);
  Number power = ratio, sum = ratio;
  for (int k = 1; k < 208 && power != Number{}; ++k) {
    power = multiply(power, square);
    sum = add(sum, divide_small(power, 2*k+1));
  }
  return add(sum, sum);
}

inline Number from_double(double x) {
  Number value{};
  int exponent = 0;
  const double mantissa = std::frexp(x, &exponent);
  const auto significand = static_cast<std::uint64_t>(std::ldexp(mantissa, 53));
  const int offset = 32*fraction + exponent - 53;
  for (int i = 0; i < 53; ++i)
    if ((significand >> i) & 1) value[(offset+i)/32] |= std::uint32_t(1) << ((offset+i)%32);
  return value;
}

// log(large/small), for finite positive large >= small.
inline Number log_ratio(double large, double small) {
  int el = 0, es = 0;
  const auto ml = static_cast<std::uint64_t>(std::ldexp(std::frexp(large, &el), 53));
  const auto ms = static_cast<std::uint64_t>(std::ldexp(std::frexp(small, &es), 53));
  const auto normalized = ml < ms ? 2*ml : ml;
  const int exponent = el-es-(ml < ms);
  const Number fraction_log = twice_atanh(normalized-ms, normalized+ms);
  static const Number log_two = twice_atanh(1, 3);
  return exponent == 0 ? fraction_log :
      add(fraction_log, multiply(log_two, from_double(exponent)));
}

// Here c*log(num/den) is near log(a), with num > den and a > 1.
// Distinct binary64 inputs bound the ratio log below by 2^-54, so c
// fits the integer part even at the largest a. Its lower bound exceeds 2^-4 at the fallback threshold.
inline double centered(double num, double den, double a, double c) {
  const Number power = multiply(from_double(c), log_ratio(num, den));
  const Number center = log_ratio(a, 1);
  const bool negative = less(power, center);
  const Number difference = negative ? subtract(center, power) : subtract(power, center);
  double result = 0;
  for (int i = 0; i < words; ++i)
    result += std::ldexp(double(difference[i]), 32*(i-fraction));
  return negative ? -result : result;
}

} } }
