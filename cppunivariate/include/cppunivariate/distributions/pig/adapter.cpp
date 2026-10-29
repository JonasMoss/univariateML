#include "as_span.h"
#include "cppunivariate/distributions/pig/mle.hpp"
#include <Rcpp.h>
// [[Rcpp::export]]
Rcpp::List mlpig_(const Rcpp::IntegerVector &x, double reltol=1e-9, int iterlim=200,
 Rcpp::Nullable<double> mean=R_NilValue, Rcpp::Nullable<double> shape=R_NilValue) {
 if ((mean.isNotNull() && (!std::isfinite(Rcpp::as<double>(mean)) || !(Rcpp::as<double>(mean)>0))) ||
     (shape.isNotNull() && (!std::isfinite(Rcpp::as<double>(shape)) || !(Rcpp::as<double>(shape)>=0))))
   Rcpp::stop("Fixed mean must be positive, shape nonnegative, and both finite.");
 const double nan=std::numeric_limits<double>::quiet_NaN();
 auto r=cppunivariate::pig_mle(dist_impl::as_ispan(x),reltol,iterlim,
   mean.isNotNull()?Rcpp::as<double>(mean):nan,
   shape.isNotNull()?Rcpp::as<double>(shape):nan);
 if (r.status==cppunivariate::PigStatus::PoissonLimit) {
   if (mean.isNotNull())
     Rcpp::stop("The PIG likelihood is maximised at the Poisson limit with the supplied mean fixed. Use `mlpois` for an unrestricted Poisson fit.");
   Rcpp::stop("The PIG likelihood is maximised at the Poisson limit. Use `mlpois` instead.");
 }
 if (r.status==cppunivariate::PigStatus::ZeroLimit)
   Rcpp::stop("All-zero data have an unattained point-mass supremum. Use `mlpois` for the zero point mass.");
 if (r.status==cppunivariate::PigStatus::InvalidInput)
   Rcpp::stop("PIG requires nonnegative integer data, positive finite fixed parameters, reltol > 0 and iterlim >= 1.");
 if (r.status==cppunivariate::PigStatus::SearchFailure)
   Rcpp::stop("PIG scalar search did not converge; increase iterlim or inspect the parameter range.");
 Rcpp::CharacterVector fixed;
 if (mean.isNotNull()) fixed.push_back("mean");
 if (shape.isNotNull()) fixed.push_back("shape");
 return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(
   Rcpp::_["mean"]=r.mean,Rcpp::_["shape"]=r.shape),Rcpp::_["logLik"]=r.logLik,
   Rcpp::_["iterations"]=r.iterations,Rcpp::_["fixed"]=fixed);
}
