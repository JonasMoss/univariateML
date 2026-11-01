#include "cppunivariate/core/inference_families.hpp"
#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::List univariateML_inference_cpp(Rcpp::NumericVector x, std::string family,
                                      Rcpp::NumericVector coefficients,
                                      Rcpp::LogicalVector free, bool sandwich) {
  std::array<double, cppunivariate::inference_max_parameters> values{};
  if (coefficients.size() > static_cast<int>(values.size()) || free.size() != coefficients.size())
    Rcpp::stop("invalid inference coefficient dimensions");
  unsigned mask = 0;
  for (int i = 0; i < coefficients.size(); ++i) {
    values[i] = coefficients[i];
    if (free[i]) mask |= 1u << i;
  }
  std::vector<double> data(x.begin(), x.end());
  const auto result = cppunivariate::family_inference(cppunivariate::span{data.data(), data.size()}, family, values, mask);
  const auto covariance = cppunivariate::inference_covariance(result, sandwich ?
      cppunivariate::CovarianceKind::Sandwich : cppunivariate::CovarianceKind::Observed);
  Rcpp::NumericMatrix matrix(covariance.dimension, covariance.dimension);
  for (std::size_t i = 0; i < covariance.dimension; ++i)
    for (std::size_t j = 0; j < covariance.dimension; ++j)
      matrix(i, j) = covariance.matrix[i * covariance.dimension + j];
  return Rcpp::List::create(Rcpp::Named("status") = static_cast<int>(covariance.status),
    Rcpp::Named("matrix") = matrix, Rcpp::Named("condition") = covariance.condition,
    Rcpp::Named("validity") = result.validity_conditions);
}
