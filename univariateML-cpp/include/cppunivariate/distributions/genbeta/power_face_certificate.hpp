#pragma once
// Whole-cell upper envelopes for the winning moving-endpoint power face.
namespace cppunivariate {
namespace genbeta_power_face_cert {
struct R {double h,r,lr;};
inline R stats_sorted(const std::vector<double>& v,double h) {
 const double n=double(v.size()),vm=v.front();
 auto logterm=[](double z){return z>40?-z:std::log(-(z<.6931471805599453?std::log(-std::expm1(-z)):std::log1p(-std::exp(-z))));};
 const double base=logterm(h*vm);genbeta_power_cert::Sum sum;
 const double cutoff=std::log(n)+std::log(4/std::numeric_limits<double>::epsilon());
 for(double w:v) {
  // The sorted omitted tail is at most epsilon/2 of the leading term:
  // R(t)<=2 exp(-t), R(tmin)>=exp(-tmin), with at most n terms.
  const double t=h*w;
  if(t>40&&h*(w-vm)>cutoff*(1+128*std::numeric_limits<double>::epsilon()))break;
  sum.add(std::exp(logterm(t)-base));
 }
 const double lr=base+std::log(double(sum.value/n));return {h,std::exp(lr),lr};
}
inline double gamma_upper(const R& l,const R& h,double mu,int maxit) {
 const double e=std::numeric_limits<double>::epsilon();
 if(!(l.lr< -1024*e*(1+std::abs(l.lr))))return INFINITY;
 const double pad=1024*e*(1+l.h*mu+std::abs(h.lr)+l.r),A=l.h*mu+h.lr-l.r-pad;
 if(!(A>0)||!std::isfinite(A))return INFINITY;
 double lo=(1/(2*A))*(1-256*e),hi=(1/A)*(1+256*e);
 if(!(hi>0)||!std::isfinite(hi))return INFINITY;
 for(std::int64_t it=0;it<solver_budget(maxit);++it){
  const double p=lo+(hi-lo)/2,a=trgamma_detail::log_minus_digamma(p),s=a-A;
  const double err=256*e*(std::abs(a)+A+(p<100?2*std::abs(std::log(p)):0));
  if(s>err)lo=p;else if(s< -err)hi=p;else break;
  if(hi-lo<1e-11*lo)break;
 }
 const double p=lo+(hi-lo)/2,lg=special::lgamma(p),
  val=std::log(h.h)+l.r+p*std::log(p)-p-lg-A*p,
  s=trgamma_detail::log_minus_digamma(p)-A;
 const double err=256*e*(A+std::abs(trgamma_detail::log_minus_digamma(p))+(p<100?2*std::abs(std::log(p)):0));
 const double vp=2048*e*(1+std::abs(std::log(h.h))+l.r+std::abs(p*std::log(p))+p+std::abs(lg)+A*p);
 const double upper=val+std::max(s*(lo-p),s*(hi-p))+err*(hi-lo)+vp;
 return std::isfinite(upper)?upper:INFINITY;
}
inline double beta_upper(const R& l,const R& h,double mu,double c,int maxit) {
 const double e=std::numeric_limits<double>::epsilon(),spad=512*e*(1+h.h*mu+l.r);
 const auto b=genbeta_allfree_bounds::rectangle(l.h*mu-spad,h.h*mu+spad,h.r-spad,l.r+spad,c+spad,maxit);
 if(!b.enclosed)return INFINITY;
 const double p=(b.pl+b.ph)/2,q=(b.ql+b.qh)/2,lb=gb2_detail::logbeta(p,q);
 double ep=0,eq=0;
 const double gp=genbeta_free_p_cert::shape_score(p,q,l.h*mu,ep),
  gq=genbeta_free_p_cert::shape_score(q,p,h.r,eq);
 const double val=std::log(h.h)+l.r-p*l.h*mu-q*h.r-lb;
 const double pad=2048*e*(1+std::abs(std::log(h.h))+l.r+p*l.h*mu+q*h.r+std::abs(lb));
 const double upper=val+std::max(gp*(b.pl-p),gp*(b.ph-p))+std::max(gq*(b.ql-q),gq*(b.qh-q))+
   4*ep*std::max(std::abs(b.pl-p),std::abs(b.ph-p))+
   4*eq*std::max(std::abs(b.ql-q),std::abs(b.qh-q))+pad+(p+q)*spad;
 return std::isfinite(upper)?upper:INFINITY;
}
inline GenbetaResult verify(const std::vector<double>& y,double endpoint,double jac,
    double gamma_shape,int maxit,GenbetaResult out) {
  out.allfree_certified=false;
  const double n=static_cast<double>(y.size()),eps=std::numeric_limits<double>::epsilon();
  genbeta_power_cert::Sum total,logsum,vsum,gaps;
  double ymin=INFINITY;
  for(double z:y){total.add(z);ymin=std::min(ymin,z);}
  const double mean=static_cast<double>(total.value/n);
  for(double z:y)gaps.add(z-ymin);
  const double gap=static_cast<double>(gaps.value/n);
  std::vector<double> v;v.reserve(y.size());double vmin=INFINITY;
  for(double z:y){const double w=z/mean;v.push_back(w);vsum.add(w);logsum.add(std::log(w));vmin=std::min(vmin,w);}
  std::sort(v.begin(),v.end());
  const double mu=static_cast<double>(vsum.value/n),delta=gap/mean,
    c=std::log(mu)-static_cast<double>(logsum.value/n),W=-std::log(delta)-1;
  const double shift=jac-n*std::log(mean),value=n*(-std::log(gap)-1)+jac;
  const double margin=8192*eps*(1+std::abs(W)+1/delta),target=W-margin;
  const double totalpad=n*margin+4096*eps*(1+std::abs(value)+std::abs(shift));
  const FitCandidate face{{endpoint,1/gap},value-totalpad,value+totalpad,StopModel::Power};
  const std::int64_t budget=certificate_budget(maxit);
  std::int64_t work=out.iterations;
  auto stop=[&](GenbetaCertificateFailure reason,bool exhausted) {
    out.status=GenbetaStatus::SearchUnresolved;out.allfree_certified=false;
    out.certificate_failure=exhausted?GenbetaCertificateFailure::Budget:reason;
    out.iterations=static_cast<int>(std::min<std::int64_t>(work,INT_MAX));
    out.stop_detail={};
    if(valid_stop_coefficients(face.model,face.coef)&&std::isfinite(face.loglik_lower+face.loglik_upper)) {
      out.stop_detail.candidates={face};out.stop_detail.loglik_lower=face.loglik_lower;
    }
    out.stop_detail.loglik_upper=INFINITY;out.stop_detail.budget_exhausted=exhausted;
    return out;
  };
  if(work>=budget)return stop(GenbetaCertificateFailure::Budget,true);
  if(!valid_stop_coefficients(face.model,face.coef)||!(mu>0&&delta>0&&c>512*eps*(1+std::abs(c)))||!std::isfinite(value+W+c+margin))
    return stop(GenbetaCertificateFailure::Arithmetic,false);
  // A gamma tangent upper bound, independent of the accuracy of its proposer.
  const double cpad=512*eps*(1+std::abs(c)+std::abs(std::log(mu))),k=gamma_shape;
  if(!(c>cpad&&k>0))return stop(GenbetaCertificateFailure::GammaNeighborhood,false);
  const double kl=1/(2*(c+cpad)),kh=1/(c-cpad),
    base=k>=100?.5*std::log(k)-.5*std::log(6.2831853071795864769)-gamma_detail::stirlerr(k):k*std::log(k)-k-special::lgamma(k),
    gv=base-(k-1)*c-std::log(mu),gs=trgamma_detail::log_minus_digamma(k)-c,
    gpad=2048*eps*(1+std::abs(base)+std::abs((k-1)*c)+std::abs(std::log(k)))*(1+kh+1/k)+cpad*(1+kh+k),
    gu=gv+std::max(gs*(kl-k),gs*(kh-k))+gpad,Q=2/(c-cpad);
  double left=(c-cpad)/(2*mu);bool closed=false;
  for(std::int64_t i=0;i<solver_budget(maxit);++i) {
    if(gu+std::max(1.,Q)*left*mu+margin<target){closed=true;break;}
    left*=.5;if(!(left>0))break;
  }
  if(!closed)return stop(GenbetaCertificateFailure::GammaNeighborhood,false);
  const double C=std::log(n)+.57721566490153286061;
  double right=std::max(std::log(2.)/vmin,(std::log(4*n)+3)/delta);
  closed=false;
  for(std::int64_t i=0;i<solver_budget(maxit);++i) {
    const double D=right*delta-std::log(n)-1,rhs=std::log(right*mu)-C+1,
      upper_rhs=std::log(D)-C;
    // p*(log(p)+C) is convex. Control both endpoints of the entire
    // conditional p interval instead of assuming it decreases there.
    if(right>=1/vmin&&D>delta/vmin*(1+8192*eps)&&rhs>margin&&upper_rhs>margin&&
       2*std::exp(1.)*right*mu*std::exp(-right*vmin)<rhs-margin&&
       2*D*std::exp(-right*vmin)<upper_rhs-margin){closed=true;break;}
    right*=2;if(!std::isfinite(right))break;
  }
  if(!closed)return stop(GenbetaCertificateFailure::PowerNeighborhood,false);
  struct Cell {R l,h;};
  if(work+2>budget)return stop(GenbetaCertificateFailure::Budget,true);
  work+=2;std::vector<Cell> queue{{stats_sorted(v,left),stats_sorted(v,right)}};
  while(!queue.empty()) {
    if(work>=budget)return stop(GenbetaCertificateFailure::Budget,true);
    ++work;const auto box=queue.back();queue.pop_back();
    if(!std::isfinite(box.l.lr+box.h.lr))return stop(GenbetaCertificateFailure::Arithmetic,false);
    double upper=gamma_upper(box.l,box.h,mu,maxit);
    if(!(upper+margin<target))upper=std::min(upper,beta_upper(box.l,box.h,mu,c+cpad,maxit));
    if(upper+margin<target)continue;
    const double mid=box.h.h/box.l.h>4?std::sqrt(box.h.h)*std::sqrt(box.l.h):box.l.h+(box.h.h-box.l.h)/2;
    if(!(mid>box.l.h&&mid<box.h.h))return stop(GenbetaCertificateFailure::Arithmetic,false);
    if(work>=budget)return stop(GenbetaCertificateFailure::Budget,true);
    ++work;const auto point=stats_sorted(v,mid);
    queue.push_back({point,box.h});queue.push_back({box.l,point});
  }
  out.status=GenbetaStatus::PowerBoundary;out.allfree_certified=true;
  out.certificate_failure=GenbetaCertificateFailure::None;
  out.logLik=value;out.iterations=static_cast<int>(std::min<std::int64_t>(work,INT_MAX));
  out.stop_detail=limit_detail(face.coef,value,StopModel::Power);
  out.stop_detail.candidates={face};out.stop_detail.loglik_lower=face.loglik_lower;
  out.stop_detail.loglik_upper=face.loglik_upper;
  return out;
}
} // namespace genbeta_power_face_cert
} // namespace cppunivariate
