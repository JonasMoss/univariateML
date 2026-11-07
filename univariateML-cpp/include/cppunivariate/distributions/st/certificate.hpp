#pragma once
// Included inside cppunivariate, after StResult and st_detail definitions.
namespace st_cert_detail {
inline StResult skew_fit(span data,double xi,double omega,double nu,int maxit) {
 StResult out{xi,omega,NAN,nu,NAN,0,StStatus::NumericalFailure};
 std::vector<double> q;int positive=0,negative=0;double base=0,base_mag=0,minq=INFINITY;
 for(double v:data) {
  const double z=st_detail::standardize(v,xi,omega);
  const double t=(z/std::hypot(std::sqrt(nu),z))*std::sqrt(nu+1);
  if(!std::isfinite(t) || (t==0 && v!=xi))return out;
  q.push_back(t);if(t>0)++positive;if(t<0)++negative;
  if(t!=0)minq=std::min(minq,std::abs(t));
  const double lp=st_detail::logdensity(v,xi,omega,0,nu);base+=lp;base_mag+=std::abs(lp);
 }
 if(!std::isfinite(base))return out;
 const auto candidate=[&](double a,double ll,double error){return FitCandidate{{xi,omega,a,nu},ll-error,ll+error};};
 if(positive+negative==0) {
  out.stop_detail=comparison_detail(candidate(-1,base,skew_scalar_detail::margin(std::abs(base))),candidate(1,base,skew_scalar_detail::margin(std::abs(base))));return out;
 }
 if(positive==0 || negative==0) {
  const double ll=base+(positive+negative)*std::log(2.);
  out.alpha=positive?INFINITY:-INFINITY;out.logLik=ll;
  out.status=positive?StStatus::RightHalfTBoundary:StStatus::LeftHalfTBoundary;
  out.stop_detail=limit_detail({xi,nu,omega},ll,positive?StopModel::Ht:StopModel::ReflectedHt);
  // A computed face value at huge magnitude can lose even the log(2)
  // density gain. Preserve the existing practical likelihood contract.
  if(skew_scalar_detail::margin(base_mag)>1e-8*data.size()) {
   double finite=0;for(double v:data)finite+=st_detail::logdensity(v,xi,omega,positive?8:-8,nu);
   const double e=skew_scalar_detail::margin(base_mag);
   const FitCandidate face{{xi,nu,omega},ll-e,ll+e,positive?StopModel::Ht:StopModel::ReflectedHt};
   out.stop_detail=comparison_detail(candidate(positive?8:-8,finite,e),face);out.status=StStatus::NumericalFailure;return out;
  }
  out.certified=true;return out;
 }
 const double m=nu+1;
 const double logc=st_detail::log_t_density(-INFINITY,m);
 // logc above is t_m(0); log_t_density takes log |argument|.
 const auto bracket=[&](bool right) {
  double largest=-INFINITY;
  for(double t:q)if(right?t>0:t<0)largest=std::max(largest,-m*std::log(std::abs(t)));
  double sum=0;for(double t:q)if(right?t>0:t<0)sum+=std::exp(-m*std::log(std::abs(t))-largest);
  const double logC=std::log(2.)+logc+(m+1)/2*std::log(m)+largest+std::log(sum);
  return std::exp(std::max(.5*std::log(m)-std::log(minq),(std::log(4.)+logC-std::log(m)-std::log(right?negative:positive))/m));
 };
 const double left=-bracket(false),right=bracket(true);
 if(!std::isfinite(left)||!std::isfinite(right))return out;
 const auto logcdf=[&](double h){return student_detail::probability_log(std::log(std::abs(h)),h<0,m,true,true);};
 const auto logpdf=[&](double h){return st_detail::log_t_density(std::log(std::abs(h)),m);};
 const auto value=[&](double a) {
  double ll=base,mag=base_mag;
  for(double t:q){const double v=std::log(2.)+logcdf(a*t);ll+=v;mag+=std::abs(v)+std::log(2.);}
  return skew_scalar_detail::Range{ll,skew_scalar_detail::margin(mag)};
 };
 const auto score=[&](double a,double b) {
  double lo=0,hi=0,mag=0;
  for(double t:q)if(t!=0) {
   const double l=std::min(a*t,b*t),h=std::max(a*t,b*t);
   const double near=l<=0 && h>=0?0:std::min(std::abs(l),std::abs(h));
   const double far=std::max(std::abs(l),std::abs(h));
   const double rl=std::exp(logpdf(far)-logcdf(h)),rh=std::exp(logpdf(near)-logcdf(l));
   const double u=std::min(t*rl,t*rh),v=std::max(t*rl,t*rh);
   lo+=u;hi+=v;mag+=std::max(std::abs(u),std::abs(v));
  }
  const double pad=skew_scalar_detail::margin(mag);
  return skew_scalar_detail::Range{lo-pad,hi+pad};
 };
 auto search=skew_scalar_detail::enumerate(left,right,maxit,score,value);
 out.iterations=search.evaluations;out.budget_exhausted=search.budget;
 out.stop_detail.budget_exhausted=search.budget;out.stop_detail.loglik_upper=INFINITY;
 for(const auto& p:search.points)out.stop_detail.candidates.push_back(candidate(p.x,p.ll,p.error));
 if(!search.ok){
  const auto& w=search.witness;if(std::isfinite(w.ll)&&std::isfinite(w.error))out.stop_detail.candidates.push_back(candidate(w.x,w.ll,w.error));
  for(const auto& c:out.stop_detail.candidates)out.stop_detail.loglik_lower=std::isnan(out.stop_detail.loglik_lower)?c.loglik_lower:std::max(out.stop_detail.loglik_lower,c.loglik_lower);
  return out;
 }
 const auto best=std::max_element(search.points.begin(),search.points.end(),[](const auto& a,const auto& b){return a.ll<b.ll;});
 int competitors=0;out.stop_detail.loglik_lower=best->ll-best->error;out.stop_detail.loglik_upper=-INFINITY;
 for(const auto& p:search.points){out.stop_detail.loglik_upper=std::max(out.stop_detail.loglik_upper,p.ll+p.error);if(p.ll+p.error>=best->ll-best->error)++competitors;}
 if(competitors>1){out.stop_detail.comparison_tie=true;return out;}
 out.alpha=best->x;out.logLik=0;
 for(double v:data)out.logLik+=st_detail::logdensity(v,xi,omega,out.alpha,nu);
 if(!std::isfinite(out.logLik)||std::abs(out.logLik-best->ll)>4*best->error)return out;
 out.status=StStatus::Ok;out.stop_detail={};out.certified=true;return out;
}
} // namespace st_cert_detail
