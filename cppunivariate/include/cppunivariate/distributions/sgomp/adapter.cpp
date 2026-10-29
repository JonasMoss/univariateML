#include "as_span.h"
#include "dpqr.h"
#include "cppunivariate/distributions/sgomp/mle.hpp"
#include <Rcpp.h>
// [[Rcpp::export]]
Rcpp::List mlsgomp_(const Rcpp::NumericVector &x,
                    Rcpp::Nullable<double> b=R_NilValue,
                    Rcpp::Nullable<double> eta=R_NilValue,
                    double reltol=1e-10,int iterlim=200) {
  const double bv=b.isNotNull() ? Rcpp::as<double>(b) : cppunivariate::sgomp_detail::nan();
  const double ev=eta.isNotNull() ? Rcpp::as<double>(eta) : cppunivariate::sgomp_detail::nan();
  if (b.isNotNull() && !(bv>0 && std::isfinite(bv))) Rcpp::stop("fixed `b` must be positive and finite.");
  if (eta.isNotNull() && !(ev>=0 && std::isfinite(ev))) Rcpp::stop("fixed `eta` must be nonnegative and finite.");
  const auto f=cppunivariate::sgomp_mle(dist_impl::as_span(x),reltol,iterlim,bv,ev);
  if (f.status==cppunivariate::SgompStatus::Collapse) Rcpp::stop("The shifted Gompertz MLE does not exist: constant data have unbounded likelihood.");
  if (f.status==cppunivariate::SgompStatus::InvalidInput) Rcpp::stop("Shifted Gompertz requires finite nonnegative data, positive reltol and positive iterlim.");
  if (f.status!=cppunivariate::SgompStatus::Ok) Rcpp::stop("Shifted Gompertz MLE failed to converge; increase iterlim.");
  Rcpp::CharacterVector fixed;
  if (b.isNotNull()) fixed.push_back("b");
  if (eta.isNotNull()) fixed.push_back("eta");
  return Rcpp::List::create(Rcpp::_["estimates"]=Rcpp::NumericVector::create(Rcpp::_["b"]=f.b,Rcpp::_["eta"]=f.eta),Rcpp::_["logLik"]=f.logLik,Rcpp::_["iterations"]=f.iterations,Rcpp::_["fixed"]=fixed);
}
// [[Rcpp::export]]
Rcpp::NumericVector dsgomp_(Rcpp::NumericVector x,Rcpp::NumericVector b,Rcpp::NumericVector eta,bool log=false) {
  return dist_impl::dpqr_apply({x,b,eta},[&](const std::vector<double> &a) {return cppunivariate::distributions::dsgomp(a[0],a[1],a[2],log);});
}
// [[Rcpp::export]]
Rcpp::NumericVector psgomp_(Rcpp::NumericVector q,Rcpp::NumericVector b,Rcpp::NumericVector eta,bool lower_tail=true,bool log_p=false) {
  return dist_impl::dpqr_apply({q,b,eta},[&](const std::vector<double> &a) {return cppunivariate::distributions::psgomp(a[0],a[1],a[2],lower_tail,log_p);});
}
// [[Rcpp::export]]
Rcpp::NumericVector qsgomp_(Rcpp::NumericVector p,Rcpp::NumericVector b,Rcpp::NumericVector eta,bool lower_tail=true,bool log_p=false) {
  return dist_impl::dpqr_apply({p,b,eta},[&](const std::vector<double> &a) {return cppunivariate::distributions::qsgomp(a[0],a[1],a[2],lower_tail,log_p);});
}
namespace {
struct SgompRng { double runif(double a,double b) { return R::runif(a,b); } };
}
// [[Rcpp::export]]
Rcpp::NumericVector rsgomp_(int n,Rcpp::NumericVector b,Rcpp::NumericVector eta) {
  SgompRng rng;
  return dist_impl::dpqr_apply({Rcpp::NumericVector(n),b,eta},[&](const std::vector<double> &a) {return cppunivariate::distributions::rsgomp(rng,a[1],a[2]);});
}
