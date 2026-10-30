#include <Rcpp.h>
#include "as_span.h"
#include "cppunivariate/distributions/cmp/mle.hpp"
// [[Rcpp::export]]
Rcpp::List mlcmp_(const Rcpp::NumericVector &x,
 Rcpp::Nullable<double> lambda=R_NilValue,Rcpp::Nullable<double> nu=R_NilValue,
 double reltol=1e-10,int iterlim=100) {
 cppunivariate::CmpFitControl ctl; ctl.tol=reltol; ctl.maxit=iterlim;
 if (lambda.isNotNull()) {ctl.fixed_lambda=Rcpp::as<double>(lambda);
  if (!std::isfinite(ctl.fixed_lambda) || ctl.fixed_lambda<0) Rcpp::stop("fixed lambda must be nonnegative and finite.");}
 if (nu.isNotNull()) {ctl.fixed_nu=Rcpp::as<double>(nu);
  if (!std::isfinite(ctl.fixed_nu) || ctl.fixed_nu<0) Rcpp::stop("fixed nu must be nonnegative and finite.");}
 const auto res=cppunivariate::cmp_domain_mle(dist_impl::as_span(x),ctl);
 switch(res.status) {
 case cppunivariate::CmpStatus::Ok: break;
 case cppunivariate::CmpStatus::GeometricLimit: Rcpp::stop("The MLE approaches the geometric face; use mlgeom.");
 case cppunivariate::CmpStatus::TwoPointLimit: Rcpp::stop("The MLE approaches an adjacent two-point law.");
 case cppunivariate::CmpStatus::PointMassLimit: Rcpp::stop("The MLE approaches a point mass.");
 case cppunivariate::CmpStatus::InvalidInput: Rcpp::stop("CMP requires nonnegative integer counts at most 2147483647, valid fixed parameters and positive controls.");
 default: Rcpp::stop("CMP calculation failed to converge or exceeded its work limit.");
 }
 Rcpp::CharacterVector fixed;
 if(lambda.isNotNull()) fixed.push_back("lambda");
 if(nu.isNotNull()) fixed.push_back("nu");
 return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["lambda"]=lambda.isNotNull()?ctl.fixed_lambda:std::exp(res.eta),Rcpp::_["nu"]=res.nu),Rcpp::_["logLik"]=res.logLik,Rcpp::_["iterations"]=res.iterations,Rcpp::_["fixed"]=fixed);
}
