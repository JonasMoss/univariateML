// Compile-time detectors for optional members on a Cost type.
//
// The Cost concept (declared as a comment in core/bfgs.hpp:3-10) is duck-typed
// at template-instantiation time. These traits let downstream code dispatch on
// "does this Cost provide an analytic gradient / hessian / per-obs log-lik?"
// via `if constexpr` without SFINAE on public APIs.
//
// Required members:                int dim() const;
//                                  double operator()(const double *theta) const;
// Optional via has_gradient:       void gradient(const double *theta, double *g) const;
// Optional via has_hessian:        void hessian(const double *theta, double *H) const;
// Optional via has_log_lik_per_obs:void log_lik_per_obs(const double *theta, double *L) const;

#pragma once

#include <type_traits>
#include <utility>

namespace cppunivariate {
namespace detail {

template <typename, typename = void> struct has_gradient : std::false_type {};
template <typename C>
struct has_gradient<
    C, std::void_t<decltype(std::declval<const C &>().gradient(
           std::declval<const double *>(), std::declval<double *>()))>>
    : std::true_type {};

template <typename, typename = void> struct has_hessian : std::false_type {};
template <typename C>
struct has_hessian<
    C, std::void_t<decltype(std::declval<const C &>().hessian(
           std::declval<const double *>(), std::declval<double *>()))>>
    : std::true_type {};

template <typename, typename = void>
struct has_log_lik_per_obs : std::false_type {};
template <typename C>
struct has_log_lik_per_obs<
    C, std::void_t<decltype(std::declval<const C &>().log_lik_per_obs(
           std::declval<const double *>(), std::declval<double *>()))>>
    : std::true_type {};

}  // namespace detail
}  // namespace cppunivariate
