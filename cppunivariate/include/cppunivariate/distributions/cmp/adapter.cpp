#include "as_span.h"
#include "dpqr.h"
#include "cppunivariate/distributions/cmp/mle.hpp"
#include <map>
#include <tuple>
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
namespace {
Rcpp::NumericVector cmp_evaluate(const std::vector<Rcpp::NumericVector> &args,
                               char op,bool lower=true,bool log=false) {
  std::map<std::tuple<double,double>,cppunivariate::cmp_dpqr_detail::Law> cache;
  return dist_impl::dpqr_apply(args,[&](const std::vector<double> &a) {
    if (!cppunivariate::cmp_dpqr_detail::valid(a[1],a[2])) return R_NaN;
    const auto key=std::make_tuple(a[1],a[2]);
    auto found=cache.find(key);
    if (found==cache.end()) found=cache.emplace(key,cppunivariate::cmp_dpqr_detail::Law(a[1],a[2])).first;
    const auto &tab=found->second;
    if (op=='d') return tab.density(a[0],log);
    if (op=='p') return tab.cdf(a[0],lower,log);
    return tab.quantile(op=='r' ? R::unif_rand() : a[0],lower,log);
  },op=='d');
}
}
// [[Rcpp::export]]
Rcpp::NumericVector dcmp_(Rcpp::NumericVector x,Rcpp::NumericVector lambda,Rcpp::NumericVector nu,bool log=false) {
  return cmp_evaluate({x,lambda,nu},'d',true,log);
}
// [[Rcpp::export]]
Rcpp::NumericVector pcmp_(Rcpp::NumericVector q,Rcpp::NumericVector lambda,Rcpp::NumericVector nu,bool lower_tail=true,bool log_p=false) {
  return cmp_evaluate({q,lambda,nu},'p',lower_tail,log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector qcmp_(Rcpp::NumericVector p,Rcpp::NumericVector lambda,Rcpp::NumericVector nu,bool lower_tail=true,bool log_p=false) {
  return cmp_evaluate({p,lambda,nu},'q',lower_tail,log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector rcmp_(int n,Rcpp::NumericVector lambda,Rcpp::NumericVector nu) {
  return cmp_evaluate({Rcpp::NumericVector(n),lambda,nu},'r');
}
