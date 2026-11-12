#pragma once
#include "../../core/likelihood_polish.hpp"
#include "../burr/accuracy.hpp"
#include "../genbeta/accuracy.hpp"
#include "gamma_accuracy.hpp"
// Log-coordinate proposal search, with held coordinates removed.
// It nominates a point after the search score gate. Public returns apply
// accurate_fallback before exposing an estimate.
namespace cppunivariate {
namespace betapr_detail {
struct FallbackCost {
  span x;
  double a,b,s;
  bool unit_shape() const {return a==1 || b==1;}
  int dim() const {return int(std::isnan(a))+int(std::isnan(b))+int(std::isnan(s));}
  std::vector<double> coefficients(const double* t) const {
    int j=0;return {std::isnan(a)?std::exp(t[j++]):a,
      std::isnan(b)?std::exp(t[j++]):b,std::isnan(s)?std::exp(t[j++]):s};
  }
  double evaluate(const std::vector<double>& c,bool search_bounds=true) const {
    if(!(c[0]>0 && c[1]>0 && c[2]>0) || !std::isfinite(c[0]+c[1]+c[2]) || (search_bounds && !unit_shape() && (c[0]>1e6 || c[1]>1e6)))return INFINITY;
    const double lb=betapr_joint_cert::log_beta(c[0],c[1]);
    CPPUNIV_EXTENDED_REAL ll=0;
    for(double v:x) {
      const double z=std::log(v)-std::log(c[2]);
      const double u=std::max(z,0.)+std::log1p(std::exp(-std::abs(z)));
      const double w=std::max(-z,0.)+std::log1p(std::exp(-std::abs(z)));
      ll+=-lb-c[0]*w-c[1]*u-std::log(v);
    }
    return -static_cast<double>(ll);
  }
  double operator()(const double* t) const {return evaluate(coefficients(t));}
  void gradient(const double* t,double* g) const {
    const auto c=coefficients(t);double ga=0,gb=0,gs=0;
    const double da=beta_detail::psi_increment(c[0],c[1]),db=beta_detail::psi_increment(c[1],c[0]);
    for(double v:x) {
      const double z=std::log(v)-std::log(c[2]);
      const double u=std::max(z,0.)+std::log1p(std::exp(-std::abs(z)));
      const double w=std::max(-z,0.)+std::log1p(std::exp(-std::abs(z)));
      ga+=w-da;gb+=u-db;
      const double p=z>=0?1/(1+std::exp(-z)):std::exp(z)/(1+std::exp(z));
      const double q=z>=0?std::exp(-z)/(1+std::exp(-z)):1/(1+std::exp(z));
      gs+=c[0]*q-c[1]*p;
    }
    int j=0;if(std::isnan(a))g[j++]=ga*c[0];if(std::isnan(b))g[j++]=gb*c[1];if(std::isnan(s))g[j++]=gs;
  }
};
inline BetaPrimeResult fallback(BetaPrimeResult certificate,span x,double a,double b,double s,
    double tol,int maxit,double starta=0,double startb=0,double starts=0) {
  maxit=static_cast<int>(solver_budget(maxit));
  if(certificate.status!=BetaPrimeStatus::BfgsFailure || maxit<1 || certificate.search_failed)return certificate;
  // With one shape held and scale free, a constant sample has a proved
  // gamma/inverse-gamma escape, even if its coefficients cannot be encoded.
  if(std::isnan(s) && (std::isnan(a)!=std::isnan(b)) && !x.empty() &&
      std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}))return certificate;
  FallbackCost cost{x,a,b,s};
  if(cost.dim()==0)return certificate;
  double lm=0;for(double v:x){if(!(v>0)||!std::isfinite(v))return certificate;lm+=std::log(v)/x.size();}
  BfgsControl ctl;ctl.maxit=maxit;ctl.grad_tol=tol;ctl.reltol=1e-13;
  BfgsResult best,attempted;best.f=attempted.f=INFINITY;
  const auto propose=[&](double p,double q,double scale) {
    std::vector<double> t;
    if(std::isnan(a))t.push_back(std::log(p));if(std::isnan(b))t.push_back(std::log(q));
    if(std::isnan(s))t.push_back(std::log(scale));
    const auto r=bfgs_minimize(cost,t.data(),ctl);
    double norm=0;for(double v:r.grad)norm=std::hypot(norm,v);
    if(norm<1e-3)consider_bfgs_candidate(best,r,1.,attempted);
  };
  // Unit shapes are exact Lomax reductions. Its existing scalar search
  // supplies a nomination even beyond the two-large-shape arithmetic guard.
  // Here log B(1,k)=-log(k) and the free-shape digamma increment is 1/k.
  if(std::isnan(s) && ((a==1 && std::isnan(b)) || (b==1 && std::isnan(a)))) {
    std::vector<double> y;bool valid=true;double center=-INFINITY;
    for(double v:x){const double w=a==1?std::log(v):-std::log(v);y.push_back(w);center=std::max(center,w);}
    for(double& v:y){v=std::exp(v-center);if(!(v>0)||!std::isfinite(v))valid=false;}
    if(valid) {
      const auto parent=lomax_newton_candidate(span{y.data(),y.size()},tol,maxit);
      const double logscale=a==1?center-std::log(parent.lambda):std::log(parent.lambda)-center;
      const double scale=std::exp(logscale);
      if(parent.status==LomaxStatus::Ok && parent.kappa>0 && std::isfinite(parent.kappa) && scale>0 && std::isfinite(scale))
        propose(a==1?1:parent.kappa,a==1?parent.kappa:1,scale);
    }
  }
  for(double p:{.2,1.,5.,20.,100.})for(double q:{.2,1.,5.,20.,100.}) {
    if((!std::isnan(a) && p!=.2) || (!std::isnan(b) && q!=.2))continue;
    propose(p,q,std::exp(lm-special::digamma(std::isnan(a)?p:a)+special::digamma(std::isnan(b)?q:b)));
  }
  if(starta>0||startb>0||starts>0)propose(starta>0?starta:2,startb>0?startb:2,starts>0?starts:std::exp(lm));
  if(!std::isfinite(best.f) || best.theta.size()!=static_cast<std::size_t>(cost.dim()))return certificate;
  auto c=cost.coefficients(best.theta.data());
  if(!cost.unit_shape() && ((std::isnan(a) && c[0]>=.999e6) || (std::isnan(b) && c[1]>=.999e6)))return certificate;
  for(double v:c)if(!(v>0) || !std::isfinite(v))return certificate;
  std::vector<double> score(cost.dim());cost.gradient(best.theta.data(),score.data());
  double norm=0;for(double v:score)norm=std::hypot(norm,v);
  if(!(norm<1e-3))return certificate;
  const double ll=-cost(best.theta.data());
  const double margin=2048*std::numeric_limits<double>::epsilon()*(x.size()+std::abs(ll));
  if(!std::isfinite(ll) || certificate.stop_detail.loglik_lower>ll+margin)return certificate;
  BetaPrimeResult out{c[0],c[1],c[2],ll,best.iterations,BetaPrimeStatus::Ok};
  out.certified=false;out.stop_detail=certificate.stop_detail;
  out.certificate_failure=certificate.certificate_failure;out.joint_exterior=certificate.joint_exterior;return out;
}
// Public return boundary: a score-gated nomination is not an accurate fit.
inline BetaPrimeResult accurate_fallback(BetaPrimeResult certificate,span x,double a,double b,double s,
    double tol,int maxit,double starta=0,double startb=0,double starts=0) {
  auto out=fallback(certificate,x,a,b,s,tol,maxit,starta,startb,starts);
  if(out.status!=BetaPrimeStatus::Ok||out.certified)return out;
  tol=std::min(tol,1e-10);
  FallbackCost cost{x,a,b,s};std::vector<double> c{out.alpha,out.beta,out.scale};
  const double unit=c[2];
  const bool reflect=c[0]>c[1];
  const double large=reflect?c[0]:c[1],small=reflect?c[1]:c[0];
  const bool rare=std::isnan(s)&&large>32&&small<large*.1;
  const bool compensated=std::isnan(s);
  std::array<double,3> q{{std::log(c[0]),std::log(c[1]),rare?-std::log(large):0}};
  const std::array<bool,3> free{{std::isnan(a),std::isnan(b),std::isnan(s)}};
  const auto objective=[&](const std::array<accuracy_detail::Jet,3>& z) {
    const auto A=free[0]?accuracy_detail::exp(z[0]):accuracy_detail::Jet(c[0]),B=free[1]?accuracy_detail::exp(z[1]):accuracy_detail::Jet(c[1]);
    const auto lb=a==1?-z[1]:b==1?-z[0]:accuracy_detail::lgamma(A)+accuracy_detail::lgamma(B)-accuracy_detail::lgamma(A+B);
    accuracy_detail::JetSum f;
    if(rare) {
      const auto P=reflect?B:A,Q=reflect?A:B;
      const auto normalizer=accuracy_detail::lgamma(P)+genbeta_detail::gamma_ratio_correction(Q,P);
      for(double v:x) {
        const auto h=accuracy_detail::Jet((reflect?-1:1)*burr_excess_log_ratio(v,0,unit))-z[2],w=accuracy_detail::exp(h),r=w/Q;
        f.add(-normalizer+P*h-w-P*accuracy_detail::log1p(r)-Q*accuracy_detail::log1pmx(r));
      }
      return f.value;
    }
    for(double v:x){const auto t=accuracy_detail::Jet(burr_excess_log_ratio(v,0,unit))-z[2];
      f.add(-lb-A*accuracy_detail::softplus(-t)-B*accuracy_detail::softplus(t));}
    return f.value;
  };
  bool accurate=false;
  if(a==1 || b==1) {
    double k=a==1?c[1]:c[0],power=1,scale=c[2];
    accurate=burr_local_accuracy(x,k,power,scale,{{a==1?free[1]:free[0],false,free[2]}},tol,maxit,b==1);
    if(a==1)c[1]=k;else c[0]=k;c[2]=scale;
  } else if(compensated)accurate=burr_power_one_detail::beta_gamma_fit(x,c[0],c[1],c[2],free,tol,maxit,out.logLik);
  else accurate=continuous_fallback_polish(q,free,tol,maxit,objective);
  if(!accurate) {
    certificate.status=BetaPrimeStatus::BfgsFailure;certificate.certificate_failure=LomaxCertificateFailure::Arithmetic;return certificate;
  }
  ExtendedReal coordinate=0;
  if(a!=1&&b!=1&&!compensated){
    if(free[0])c[0]=std::exp(q[0]);if(free[1])c[1]=std::exp(q[1]);
    if(free[2]) {
      const int large_index=reflect?0:1;
      const ExtendedReal large_log=free[large_index]?static_cast<ExtendedReal>(q[large_index]):std::log(static_cast<ExtendedReal>(c[large_index]));
      coordinate=rare?(reflect?-1:1)*(static_cast<ExtendedReal>(q[2])+large_log):q[2];
      c[2]=static_cast<double>(static_cast<ExtendedReal>(unit)*std::exp(coordinate));
    }
  }
  if(a!=1&&b!=1&&!compensated&&free[2]&&!accuracy_detail::scale_projection(c[2],unit,static_cast<double>(coordinate),tol)) {
    certificate.status=BetaPrimeStatus::BfgsFailure;certificate.certificate_failure=LomaxCertificateFailure::Arithmetic;return certificate;
  }
  const double ll=-cost.evaluate(c,false);

  if(!std::isfinite(ll)){certificate.status=BetaPrimeStatus::BfgsFailure;certificate.certificate_failure=LomaxCertificateFailure::Arithmetic;return certificate;}
  out.alpha=c[0];out.beta=c[1];out.scale=c[2];out.logLik=ll;return out;
}

}
}
