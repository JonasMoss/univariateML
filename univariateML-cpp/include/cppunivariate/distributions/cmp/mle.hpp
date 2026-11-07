#pragma once
#include "../../core/discrete_stop.hpp"
#include "../../special/cmp.hpp"
#include "dpqr.hpp"
#include "../cmb/acceptance.hpp"

namespace cppunivariate {
// Compatibility family entry point; the shared public solver owns closure.
struct CmpDomainResult : CmpResult { StopDetail stop_detail{}; };
inline CmpDomainResult cmp_domain_mle(span x, const CmpFitControl& ctl = {}) {
  CmpFitControl control=ctl;
  control.maxit=static_cast<int>(solver_budget(ctl.maxit));
  CmpDomainResult out; static_cast<CmpResult&>(out)=cmp_mle(x,control);
  if(out.status==CmpStatus::Ok && (!std::isfinite(out.logLik) || !std::isfinite(out.nu) ||
     !std::isfinite(std::exp(out.eta)) || (std::isfinite(out.eta) && std::exp(out.eta)==0))) {
    out.status=CmpStatus::IterationFailure;out.stop_detail.loglik_upper=INFINITY;return out;
  }
  if(out.status==CmpStatus::Ok && std::isfinite(out.eta) &&
     !(std::isfinite(ctl.fixed_lambda) && std::isfinite(ctl.fixed_nu)) &&
     !(ctl.fixed_nu==0)) {
    const bool fl=std::isfinite(ctl.fixed_lambda),fn=std::isfinite(ctl.fixed_nu);
    const bool boundary=out.nu==0 && !fn;
    const double n=double(x.size());
    CPPUNIV_EXTENDED_REAL sum_x=0;for(double k:x)sum_x+=k;
    const CPPUNIV_EXTENDED_REAL empirical_mean=sum_x/n;
    const double eta=fl?std::log(ctl.fixed_lambda):std::log(std::exp(out.eta));
    const std::uint64_t budget=static_cast<std::uint64_t>(enumeration_budget(ctl.maxit));
    std::uint64_t used=out.work;
    bool exhausted=false;
    const auto moment_evaluate=[&](double e,double dispersion) {
      CmpMoments q;
      if(used>=budget) {exhausted=true;q.status=CmpStatus::WorkLimit;return q;}
      const std::uint64_t remaining=budget-used;
      const auto terms=static_cast<std::size_t>(std::min<std::uint64_t>(ctl.max_terms,remaining));
      q=cmp_moments(e,dispersion,2e-16,terms);
      used+=q.work;
      out.work=static_cast<std::size_t>(std::min<std::uint64_t>(used,std::numeric_limits<std::size_t>::max()));
      if(used>budget || (q.status==CmpStatus::WorkLimit && remaining<ctl.max_terms))exhausted=true;
      return q;
    };
    auto moments=moment_evaluate(eta,out.nu);
    if(boundary) {
      double t=0;for(double k:x)t+=std::lgamma(k+1)/n;
      if(moments.status!=CmpStatus::Ok || !(moments.mean_h-t < -1e-12*(1+std::abs(t)))) {
        out.status=CmpStatus::IterationFailure;
        const double margin=1e-9*(n+std::abs(out.logLik));
        out.stop_detail.candidates.push_back({{std::exp(out.eta),out.nu},out.logLik-margin,out.logLik+margin});
        out.stop_detail.loglik_lower=out.logLik-margin;
        out.stop_detail.loglik_upper=INFINITY;
        if(exhausted)out.stop_detail=discrete_budget(out.stop_detail);
        return out;
      }
      if(fl) {
        const double margin=1e-9*(n+std::abs(out.logLik));
        out.stop_detail.loglik_lower=out.logLik-margin;
        out.stop_detail.loglik_upper=out.logLik+margin;
        return out;
      }
    }
    const double slope=(!fl && !fn && !boundary)?double(moments.slope):0;
    const double alpha=eta-slope*out.nu;
    double a=0,b=0,c=0;
    if(fl)a=1/moments.variance_h;
    else if(fn || boundary)a=1/moments.variance;
    else {
      const double det=moments.variance*moments.variance_residual-
        moments.covariance_residual*moments.covariance_residual;
      a=moments.variance_residual/det;b=moments.covariance_residual/det;c=moments.variance/det;
    }
    const auto evaluate=[&](double u,double v) {
      const double dispersion=fl?u:((fn || boundary)?out.nu:v);
      const double e=fl?eta:u+slope*dispersion;
      const auto q=moment_evaluate(e,dispersion);
      if(q.status!=CmpStatus::Ok || !(dispersion>=0))return concave_accept_detail::Evaluation{};
      CPPUNIV_EXTENDED_REAL data=0;
      for(double k:x)data+=cmp_detail::residual(k,q.center,q.slope);
      data/=n;
      const CPPUNIV_EXTENDED_REAL delta=empirical_mean-q.center;
      const double ll=double(delta*(e-dispersion*q.slope)-dispersion*data-q.log_scaled_z);
      const double g1=double(delta-q.offset);
      const double g2=double(q.residual_mean-data+(q.slope-slope)*(q.offset-delta));
      return concave_accept_detail::Evaluation{ll,fl?g2:g1,(!fn && !fl)?g2:0,
        std::isfinite(ll) && q.relative_tail_bound<1e-12};
    };
    const auto accurate=[&](const concave_accept_detail::Bound& z) {
      const double low=fl?z.lo1:z.lo2,high=fl?z.hi1:z.hi2;
      if(!fn && !boundary && std::max(std::abs(low-out.nu),std::abs(high-out.nu))>
         1e-8*(1+out.nu))return false;
      const double l=std::exp(z.lo1+slope*z.lo2),h=std::exp(z.hi1+slope*z.hi2);
      const double lambda=std::exp(out.eta);
      return fl || (std::isfinite(h) && std::max(std::abs(l-lambda),std::abs(h-lambda))<=1e-8*(1+lambda));
    };
    const auto bound=concave_accept_detail::certify(fl?out.nu:alpha,out.nu,out.logLik/n,
      a,b,c,(!fl && !fn && !boundary)?2:1,boundary,ctl.maxit,evaluate,accurate);
    out.stop_detail.loglik_lower=out.logLik-1e-9*(n+std::abs(out.logLik));
    out.stop_detail.loglik_upper=n*bound.upper;
    if(!bound.ok || exhausted) {
      out.status=CmpStatus::IterationFailure;
      if(std::isfinite(std::exp(out.eta)) && std::exp(out.eta)>0)out.stop_detail.candidates.push_back(
        {{std::exp(out.eta),out.nu},out.stop_detail.loglik_lower,out.logLik+1e-9*(n+std::abs(out.logLik))});
      if(exhausted)out.stop_detail=discrete_budget(out.stop_detail);
    }
  }
  if(out.status==CmpStatus::PointMassLimit) out.stop_detail=discrete_point(x[0]);
  if(out.status==CmpStatus::TwoPointLimit) {
    const bool fixed=std::isfinite(ctl.fixed_lambda);
    const double lo=fixed?0:*std::min_element(x.begin(),x.end());
    out.stop_detail=discrete_two_point(x,lo,lo+1,fixed?ctl.fixed_lambda/(1+ctl.fixed_lambda):NAN,fixed?std::log(ctl.fixed_lambda):NAN);
  }
  if(out.status==CmpStatus::GeometricLimit) {
    double m=0;for(double v:x)m+=v;m/=x.size();
    const double p=std::isnan(ctl.fixed_lambda)?1/(1+m):1-ctl.fixed_lambda;
    double ll=0;for(double v:x)ll+=std::log(p)+v*std::log1p(-p);
    out.stop_detail=discrete_face(StopModel::Geom,{p},ll);
  }
  return out;
}
}
