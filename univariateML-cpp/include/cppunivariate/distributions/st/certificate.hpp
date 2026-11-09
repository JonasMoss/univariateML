#pragma once
// Included inside cppunivariate, after StResult and st_detail definitions.
namespace st_cert_detail {
// Bound the positive weighted Student pdf/CDF ratio before exponentiation.
// Subtracting two large negative log probabilities can lose the small log
// ratio. Include those working magnitudes, not only the final score's size.
inline skew_scalar_detail::Range weighted_ratio(double lp,double lc,double weight,
    double weight_magnitude) {
 if(!std::isfinite(lp)||!std::isfinite(lc)||!std::isfinite(weight)||
     !std::isfinite(weight_magnitude))return {NAN,NAN};
 const double logvalue=lp-lc+weight;
 const double error=skew_scalar_detail::margin(std::abs(lp)+std::abs(lc)+weight_magnitude+std::abs(logvalue));
 if(!std::isfinite(logvalue)||!std::isfinite(error))return {NAN,NAN};
 return {std::exp(logvalue-error),std::exp(logvalue+error)};
}
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
   const double w=std::log(std::abs(t));
   const auto rl=weighted_ratio(logpdf(far),logcdf(h),w,std::abs(w));
   const auto rh=weighted_ratio(logpdf(near),logcdf(l),w,std::abs(w));
   const double u=t>0?rl.lo:-rh.hi,v=t>0?rh.hi:-rl.lo;
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

// Exact alpha=0 is the location-scale Student family with the identical
// nu>=1 fitting restriction. Only the parent's all-free certified route is
// delegated; an estimated alpha close to zero does not qualify.
inline StResult symmetric_joint_fit(span data,double tol,int maxit) {
 const auto r=lst_mle_certified(data,tol,maxit);
 StResult out{r.mu,r.sigma,0,r.df,r.logLik,r.iterations,StStatus::NumericalFailure};
 out.stop_detail=r.stop_detail;
 for(auto& c:out.stop_detail.candidates)if(c.model==StopModel::None)c.model=StopModel::Lst;
 switch(r.status) {
  case LstStatus::Ok:out.status=StStatus::Ok;out.restricted=r.restricted&16u?64u:0u;out.certified=true;break;
  case LstStatus::NormalBoundary:
   out.status=StStatus::SkewNormalBoundary;out.certified=true;
   out.stop_detail.limit_model=StopModel::Sn;out.stop_detail.limit_coef={r.mu,r.sigma,0};break;
  case LstStatus::Unbounded:out.status=StStatus::Unbounded;out.certified=true;break;
  case LstStatus::ScaleBoundary:
   out.status=StStatus::ScaleBoundary;out.reason=6;out.certified=true;out.stop_detail.limit_model=StopModel::TCollapsePath;break;
  case LstStatus::CertificateBudget:out.budget_exhausted=true;out.stop_detail.budget_exhausted=true;break;
  case LstStatus::CertificateTie:out.stop_detail.comparison_tie=true;break;
  default:break;
 }
 return out;
}
// Constant data at held scale: both slant directions have the same sharp
// supremum. Held df gives two half-t faces; free df gives two half-normal
// faces since t_nu(0)<phi(0), with equality only in the normal limit.
inline StResult constant_location_skew(span data,double omega,double nu) {
 StResult out{NAN,omega,NAN,nu,NAN,0,StStatus::NumericalFailure};
 const bool fixed_nu=std::isfinite(nu);
 const double a=fixed_nu?st_detail::log_t_density(-INFINITY,nu):-.5*std::log(2*std::acos(-1.));
 const double b=std::log(omega);
 const double ll=data.size()*(std::log(2.)+a-b);
 const double e=skew_scalar_detail::margin(data.size()*(std::log(2.)+std::abs(a)+std::abs(b)));
 if(!std::isfinite(ll)||!std::isfinite(e))return out;
 const std::vector<double> coef=fixed_nu?std::vector<double>{data[0],nu,omega}:std::vector<double>{data[0],omega};
 out.stop_detail=comparison_detail({coef,ll-e,ll+e,fixed_nu?StopModel::Ht:StopModel::Hnorm},
     {coef,ll-e,ll+e,fixed_nu?StopModel::ReflectedHt:StopModel::ReflectedHnorm});
 out.stop_detail.loglik_lower=ll-e;out.stop_detail.loglik_upper=ll+e;
 out.certified=true;return out;
}
// Location score range in t=(xi-origin)/omega. The Student rational
// term has extrema at +/-sqrt(nu). The skew term is bounded by pdf
// unimodality, CDF monotonicity and monotonicity of hypot(sqrt(nu),z).
inline skew_scalar_detail::Range location_term(double lo,double hi,double alpha,double nu) {
 const double c=std::sqrt(nu),m=nu+1;
 skew_scalar_detail::Range rational;
 const auto add=[&](double z){const double h=std::hypot(c,z);skew_scalar_detail::include(rational,(z/h)/h);};
 add(lo);add(hi);if(lo<=-c && hi>=-c)add(-c);if(lo<=c && hi>=c)add(c);
 double lower=m*rational.lo,upper=m*rational.hi;
 if(alpha!=0) {
  const double near=lo<=0 && hi>=0?0:std::min(std::abs(lo),std::abs(hi));
  const double far=std::max(std::abs(lo),std::abs(hi));
  const double dn=std::hypot(c,near),df=std::hypot(c,far);
  double a=alpha*(lo/std::hypot(c,lo))*std::sqrt(m),b=alpha*(hi/std::hypot(c,hi))*std::sqrt(m);
  if(a>b)std::swap(a,b);
  const double hn=a<=0 && b>=0?0:std::min(std::abs(a),std::abs(b));
  const double hf=std::max(std::abs(a),std::abs(b));
  const auto lcdf=[&](double h){return student_detail::probability_log(std::log(std::abs(h)),h<0,m,true,true);};
  const double la=std::log(std::abs(alpha)),ln=std::log(nu),lm=.5*std::log(m);
  const double lf=3*std::log(df),ll=3*std::log(dn);
  const double common=std::abs(la)+std::abs(ln)+std::abs(lm);
  const auto rl=weighted_ratio(st_detail::log_t_density(std::log(hf),m),lcdf(b),la+ln+lm-lf,common+std::abs(lf));
  const auto rh=weighted_ratio(st_detail::log_t_density(std::log(hn),m),lcdf(a),la+ln+lm-ll,common+std::abs(ll));
  const double sl=alpha>0?rl.lo:-rh.hi,sh=alpha>0?rh.hi:-rl.lo;
  lower-=sh;upper-=sl;
 }
 return {lower,upper};
}
// Preserve the hull of each connected stationary component. A component
// wider than public coefficient accuracy stops rather than merging roots
// using a distance-only threshold. Shared sstd machinery is left unchanged.
template<class Score,class Value>
inline skew_scalar_detail::Search location_search(double left,double right,int maxit,
    double origin,double omega,Score score,Value value) {
 skew_scalar_detail::Search out;
 const double seed=left/2+right/2;const auto v=value(seed);
 if(!std::isfinite(v.lo)||!std::isfinite(v.hi))return out;
 out.witness={seed,v.lo,v.hi};
 struct Cell {double lo,hi;};
 struct Root {Cell cell;skew_scalar_detail::Point point;};
 std::vector<Cell> pending{{left,right}};std::vector<Root> roots;
 while(!pending.empty()) {
  if(out.evaluations>=certificate_budget(maxit)){out.budget=true;return out;}
  ++out.evaluations;const auto c=pending.back();pending.pop_back();
  const auto r=score(c.lo,c.hi);
  if(!std::isfinite(r.lo)||!std::isfinite(r.hi)||r.lo>r.hi)return out;
  if(r.lo>0 || r.hi<0)continue;
  const double mid=c.lo/2+c.hi/2,public_mid=origin+omega*mid;
  if(!std::isfinite(public_mid))return out;
  if(c.hi-c.lo<=2e-13*(1+std::abs(mid)) &&
      (c.hi-c.lo)*omega<=2e-13*(1+std::abs(public_mid))) {
   const auto p=value(mid);const double rem=std::max(std::abs(r.lo),std::abs(r.hi))*(c.hi-c.lo)/2;
   if(!std::isfinite(p.lo)||!std::isfinite(p.hi+rem))return out;
   if(rem<=p.hi){roots.push_back({c,{mid,p.lo,p.hi+rem}});continue;}
  }
  if(mid==c.lo || mid==c.hi)return out;
  pending.push_back({mid,c.hi});pending.push_back({c.lo,mid});
 }
 double end=-INFINITY,begin=0;
 for(const auto& r:roots) {
  if(!out.points.empty() && r.cell.lo==end) {
   end=r.cell.hi;
   if((end-begin)*omega>4e-11*(1+std::abs(origin+omega*(begin/2+end/2))))return out;
   const double upper=std::max(out.points.back().ll+out.points.back().error,r.point.ll+r.point.error);
   if(r.point.ll>out.points.back().ll)out.points.back()=r.point;
   out.points.back().error=std::max(out.points.back().error,upper-out.points.back().ll);
  } else {begin=r.cell.lo;end=r.cell.hi;out.points.push_back(r.point);}
 }
 out.ok=!out.points.empty();return out;
}
inline StResult location_fit(span data,double omega,double alpha,double nu,int maxit) {
 StResult out{NAN,omega,alpha,nu,NAN,0,StStatus::NumericalFailure};
 double low=data[0],high=data[0];for(double x:data){low=std::min(low,x);high=std::max(high,x);}
 const double origin=low/2+high/2;
 std::vector<double> y;
 for(double x:data){const double z=st_detail::standardize(x,origin,omega);if(!std::isfinite(z)||(z==0 && x!=origin))return out;y.push_back(z);}
 std::vector<double> raw(data.begin(),data.end()),ys=y;std::sort(raw.begin(),raw.end());std::sort(ys.begin(),ys.end());
 for(std::size_t i=1;i<raw.size();++i)if(raw[i]!=raw[i-1] && ys[i]==ys[i-1])return out;
 // For alpha>0, g_z>0 on z<0. On z>0, R(h)<=2*t_m(0)
 // and g_z<0 whenever z*hypot(sqrt(nu),z)>2*t_m(0)*alpha*nu/sqrt(m).
 // R^2 is twice this threshold, so every root lies in the stated range.
 const double radius=alpha==0?0:std::exp(.5*(std::log(4.)+st_detail::log_t_density(-INFINITY,nu+1)+std::log(std::abs(alpha))+std::log(nu)-.5*std::log(nu+1)));
 const double left=ys.front()-(alpha>0?radius:0),right=ys.back()+(alpha<0?radius:0);
 if(!std::isfinite(radius)||!std::isfinite(left)||!std::isfinite(right)||(alpha!=0 && (radius==0 || left==right)))return out;
 const auto value=[&](double t){double ll=0,mag=0;for(double z:y){const double v=st_detail::logdensity(z,t,1,alpha,nu)-std::log(omega);ll+=v;mag+=std::abs(v)+std::abs(std::log(omega));}return skew_scalar_detail::Range{ll,skew_scalar_detail::margin(mag)};};
 const auto score=[&](double a,double b){double l=0,h=0,mag=0;for(double z:y){const auto r=location_term(z-b,z-a,alpha,nu);l+=r.lo;h+=r.hi;mag+=std::max(std::abs(r.lo),std::abs(r.hi));}const double e=skew_scalar_detail::margin(mag);return skew_scalar_detail::Range{l-e,h+e};};
 if(left==right) { // All observations coincide and alpha is exactly zero.
  out.xi=data[0];out.logLik=0;for(double x:data)out.logLik+=st_detail::logdensity(x,out.xi,omega,alpha,nu);
  if(std::isfinite(out.logLik)){out.status=StStatus::Ok;out.certified=true;}return out;
 }
 auto search=location_search(left,right,maxit,origin,omega,score,value);
 out.iterations=search.evaluations;out.budget_exhausted=search.budget;out.stop_detail.budget_exhausted=search.budget;out.stop_detail.loglik_upper=INFINITY;
 const auto candidate=[&](const skew_scalar_detail::Point& p) {
  const double location=origin+omega*p.x;
  if(!std::isfinite(location))return FitCandidate{};
  double represented=0,magnitude=0;
  for(double x:data){const double v=st_detail::logdensity(x,location,omega,alpha,nu);represented+=v;magnitude+=std::abs(v)+std::abs(std::log(omega));}
  const double error=p.error+std::abs(represented-p.ll)+skew_scalar_detail::margin(magnitude);
  if(!std::isfinite(represented)||!std::isfinite(error)||
      !std::isfinite(p.ll-error)||!std::isfinite(p.ll+error))return FitCandidate{};
  return FitCandidate{{location,omega,alpha,nu},p.ll-error,p.ll+error};
 };
 const auto append=[&](const skew_scalar_detail::Point& p){const auto c=candidate(p);if(c.coef.size()==4)out.stop_detail.candidates.push_back(c);};
 for(const auto& p:search.points)append(p);
 if(!search.ok) {
  if(std::isfinite(search.witness.ll))append(search.witness);
  if(out.stop_detail.candidates.empty()){out.budget_exhausted=false;out.stop_detail.budget_exhausted=false;}
  for(const auto& c:out.stop_detail.candidates)out.stop_detail.loglik_lower=std::isnan(out.stop_detail.loglik_lower)?c.loglik_lower:std::max(out.stop_detail.loglik_lower,c.loglik_lower);
  return out;
 }
 if(out.stop_detail.candidates.empty())return out;
 const auto best=std::max_element(search.points.begin(),search.points.end(),[](const auto& a,const auto& b){return a.ll<b.ll;});
 int competitors=0;out.stop_detail.loglik_lower=best->ll-best->error;out.stop_detail.loglik_upper=-INFINITY;
 for(const auto& p:search.points){out.stop_detail.loglik_upper=std::max(out.stop_detail.loglik_upper,p.ll+p.error);if(p.ll+p.error>=best->ll-best->error)++competitors;}
 if(competitors>1){out.stop_detail.comparison_tie=true;return out;}
 out.xi=origin+omega*best->x;out.logLik=0;
 if(!std::isfinite(out.xi)||std::abs((out.xi-origin)/omega-best->x)>2e-11*(1+std::abs(best->x)))return out;
 for(double x:data)out.logLik+=st_detail::logdensity(x,out.xi,omega,alpha,nu);
 if(!std::isfinite(out.logLik)||std::abs(out.logLik-best->ll)>4*best->error)return out;
 out.status=StStatus::Ok;out.certified=true;out.stop_detail={};return out;
}

// Exactly symmetric scale likelihood; also the profiled one-sign alpha face.
// Its log-scale score is strictly decreasing unless every residual is zero.
inline StResult symmetric_scale_fit(span data,double xi,double nu,int maxit) {
 StResult out{xi,NAN,0,nu,NAN,0,StStatus::NumericalFailure};
 std::vector<double> logs;std::size_t ties=0;
 for(double x:data) {
  if(x==xi){++ties;continue;}
  const double d=x-xi;if(!std::isfinite(d)||d==0)return out;
  logs.push_back(std::log(std::abs(d)));
 }
 const double n=static_cast<double>(data.size()),m=nu+1;
 const double exponent=std::fma(nu,double(logs.size()),-double(ties));
 if(exponent<0 || logs.empty()){out.status=StStatus::Unbounded;out.certified=true;return out;}
 if(exponent==0) {
  const double a=n*st_detail::log_t_density(-INFINITY,nu),b=.5*m*logs.size()*std::log(nu);
  double ll=a+b,mag=std::abs(a)+std::abs(b);
  for(double d:logs){ll-=m*d;mag+=std::abs(m*d);}
  if(!std::isfinite(ll)||!std::isfinite(mag))return out;
  out.status=StStatus::ScaleBoundary;out.reason=6;out.logLik=ll;out.certified=true;
  const double e=skew_scalar_detail::margin(mag);
  out.stop_detail.loglik_lower=ll-e;out.stop_detail.loglik_upper=ll+e;return out;
 }
 const auto score=[&](double t){double s=-n,mag=n;for(double d:logs){const double a=2*(t-d)+std::log(nu);const double w=a>0?std::exp(-a)/(1+std::exp(-a)):1/(1+std::exp(a));const double v=m*w;s+=v;mag+=std::abs(v);}const double e=skew_scalar_detail::margin(mag);return skew_scalar_detail::Range{s-e,s+e};};
 const auto witness=[&](double t){const double scale=std::exp(t);double ll=0,mag=0;for(double x:data){const double p=st_detail::logdensity(x,xi,scale,0,nu);ll+=p;mag+=std::abs(p);}if(scale>0 && std::isfinite(scale)&&std::isfinite(ll)){const double e=skew_scalar_detail::margin(mag);out.stop_detail.candidates={{{xi,scale,0,nu},ll-e,ll+e}};out.stop_detail.loglik_lower=ll-e;}out.stop_detail.loglik_upper=INFINITY;};
 double left=*std::min_element(logs.begin(),logs.end())-.5*std::log(nu)-1,right=*std::max_element(logs.begin(),logs.end())-.5*std::log(nu)+1;
 bool bracket=false;
 for(std::int64_t i=0;i<solver_budget(maxit);++i){const auto l=score(left),r=score(right);if(l.lo>0 && r.hi<0){bracket=true;break;}if(l.lo<=0)left-=1;if(r.hi>=0)right+=1;}
 if(!bracket){witness(left/2+right/2);return out;}
 bool resolved=false;
 for(std::int64_t i=0;i<solver_budget(maxit);++i) {
  ++out.iterations;const double mid=left/2+right/2;
  if(right-left<=2e-11){resolved=true;break;}
  const auto s=score(mid);
  if(s.lo>0)left=mid;else if(s.hi<0)right=mid;
  else {
   // Keep an enclosing interval even when the point score is sign-uncertain.
   const double delta=8e-12;const auto l=score(mid-delta),r=score(mid+delta);
   if(l.lo>0 && r.hi<0){left=mid-delta;right=mid+delta;resolved=true;}break;
  }
 }
 const double mid=left/2+right/2;witness(mid);
 if(!resolved || out.stop_detail.candidates.empty())return out;
 out.omega=std::exp(mid);out.logLik=0;for(double x:data)out.logLik+=st_detail::logdensity(x,xi,out.omega,0,nu);
 if(!(out.omega>0)||!std::isfinite(out.omega)||!std::isfinite(out.logLik)||std::abs(std::log(out.omega)-mid)>2e-11)return out;
 out.status=StStatus::Ok;out.certified=true;out.stop_detail={};return out;
}
inline StResult one_sign_scale_fit(span data,double xi,double nu,int maxit) {
 int pos=0,neg=0;for(double x:data){if(x>xi)++pos;if(x<xi)++neg;}
 auto out=symmetric_scale_fit(data,xi,nu,maxit);
 if(!out.certified)return out;
 if(out.status==StStatus::ScaleBoundary) {
  const double gain=(pos+neg)*std::log(2.);out.logLik+=gain;
  out.stop_detail.loglik_lower+=gain;out.stop_detail.loglik_upper+=gain;return out;
 }
 if(out.status!=StStatus::Ok)return out;
 if(pos+neg==0 || (pos && neg)){out.status=StStatus::NumericalFailure;out.certified=false;return out;}
 double magnitude=0;for(double x:data)magnitude+=std::abs(st_detail::logdensity(x,xi,out.omega,0,nu))+std::log(2.);
 if(skew_scalar_detail::margin(magnitude)>1e-8*data.size()) {
  double finite=0;for(double x:data)finite+=st_detail::logdensity(x,xi,out.omega,pos?8:-8,nu);
  out.status=StStatus::NumericalFailure;out.certified=false;
  const double e=skew_scalar_detail::margin(magnitude);
  if(std::isfinite(finite))out.stop_detail.candidates={{{xi,out.omega,pos?8.:-8.,nu},finite-e,finite+e}};
  out.stop_detail.loglik_upper=INFINITY;return out;
 }
 out.alpha=pos?INFINITY:-INFINITY;out.logLik+=(pos+neg)*std::log(2.);
 out.status=pos?StStatus::RightHalfTBoundary:StStatus::LeftHalfTBoundary;
 out.stop_detail=limit_detail({xi,nu,out.omega},out.logLik,pos?StopModel::Ht:StopModel::ReflectedHt);return out;
}
} // namespace st_cert_detail
