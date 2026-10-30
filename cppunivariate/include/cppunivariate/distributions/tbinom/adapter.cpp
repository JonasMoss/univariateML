#include "as_span.h"
#include "cppunivariate/distributions/tbinom/mle.hpp"
#include <Rcpp.h>
#include <sstream>
// [[Rcpp::export]]
Rcpp::List mltbinom_(const Rcpp::IntegerVector &x, Rcpp::Nullable<double> a, Rcpp::Nullable<double> b, Rcpp::Nullable<double> size, Rcpp::Nullable<double> prob=R_NilValue) {
  const double held=prob.isNull()?std::numeric_limits<double>::quiet_NaN():Rcpp::as<double>(prob);
  if(prob.isNotNull()&&std::isnan(held))Rcpp::stop("Fixed parameter must not be missing.");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  if(x.size()==0)Rcpp::stop("requires nonempty data");
  const double lower=a.isNotNull()?Rcpp::as<double>(a):double(*std::min_element(x.begin(),x.end()))-1;
  const double upper=b.isNotNull()?Rcpp::as<double>(b):double(*std::max_element(x.begin(),x.end()));
  const double trials=size.isNotNull()?Rcpp::as<double>(size):nan;
  if((a.isNotNull()&&std::isnan(lower))||(b.isNotNull()&&std::isnan(upper)))Rcpp::stop("Held bounds must not be missing.");
  if(size.isNotNull() && !std::isfinite(trials))Rcpp::stop("`size` must be a finite nonnegative integer.");
  auto r=cppunivariate::tbinom_design_mle(dist_impl::as_ispan(x),lower,upper,trials,held);
  if(r.status==cppunivariate::TbinomStatus::LowerPointMass) Rcpp::stop("MLE escapes to a point mass at the lowest support point.");
  if(r.status==cppunivariate::TbinomStatus::UpperPointMass) Rcpp::stop("MLE escapes to a point mass at the highest support point.");
  if(r.status==cppunivariate::TbinomStatus::Unidentified) Rcpp::stop("Parameter is unidentified on singleton truncation support.");
  if(r.status==cppunivariate::TbinomStatus::InvalidInput) Rcpp::stop("Invalid design parameters or observations outside truncation bounds a < x <= b.");
  if(r.status==cppunivariate::TbinomStatus::TpoisLimit) {
    std::ostringstream os;
    os << "The maximum likelihood estimator does not exist: with both `prob` and `size` "
          "free, the likelihood increases toward the truncated Poisson distribution as "
          "`size` grows. Use `mltpois(x, a = " << lower << ", b = " << upper << ")` instead.";
    Rcpp::stop(os.str());
  }
  if(r.status!=cppunivariate::TbinomStatus::Ok) Rcpp::stop("Truncated normalizer cannot be resolved within numerical precision or work budget.");
  Rcpp::CharacterVector fixed=Rcpp::CharacterVector(0);
  if(size.isNotNull())fixed.push_back("size");
  if(a.isNotNull())fixed.push_back("a");
  if(b.isNotNull())fixed.push_back("b");
  if(prob.isNotNull())fixed.push_back("prob");
  return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["prob"]=r.prob, Rcpp::_["a"]=lower, Rcpp::_["b"]=upper, Rcpp::_["size"]=r.size),Rcpp::_["logLik"]=r.logLik,Rcpp::_["iterations"]=r.iterations,
    Rcpp::_["fixed"]=fixed);
}
