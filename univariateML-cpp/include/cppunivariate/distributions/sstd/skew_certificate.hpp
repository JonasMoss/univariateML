#pragma once
// Included inside cppunivariate after SstdResult. Fixed moments and df.
namespace sstd_skew_cert {
struct Interval { double lo,hi; };
inline Interval add(Interval a,Interval b){const double l=a.lo+b.lo,h=a.hi+b.hi;return std::isnan(l)||std::isnan(h)?Interval{-INFINITY,INFINITY}:Interval{l,h};}
inline Interval mul(Interval a,Interval b){if(!std::isfinite(a.lo)||!std::isfinite(a.hi)||!std::isfinite(b.lo)||!std::isfinite(b.hi))return {-INFINITY,INFINITY};return {std::min({a.lo*b.lo,a.lo*b.hi,a.hi*b.lo,a.hi*b.hi}),std::max({a.lo*b.lo,a.lo*b.hi,a.hi*b.lo,a.hi*b.hi})};}
inline Interval scale(Interval a,double b){if(b==0)return {0,0};if(!std::isfinite(b)||!std::isfinite(a.lo)||!std::isfinite(a.hi))return {-INFINITY,INFINITY};return b>=0?Interval{a.lo*b,a.hi*b}:Interval{a.hi*b,a.lo*b};}
inline Interval square(Interval a){return {a.lo<=0 && a.hi>=0?0:std::min(a.lo*a.lo,a.hi*a.hi),std::max(a.lo*a.lo,a.hi*a.hi)};}
inline Interval divide(Interval a,Interval b){if(!(b.lo>0))return {-INFINITY,INFINITY};return mul(a,{1/b.hi,1/b.lo});}
struct Bounds {Interval value,score;};
inline double kernel_log(double logw,double nu){
 const double t=2*logw-std::log(nu-2);
 return t>0?t+std::log1p(std::exp(-t)):std::log1p(std::exp(t));
}
// q=xi^2/(1+xi^2), H=(1-m^2)+(4m^2-3)q(1-q).
// f=2 sqrt(H)/sd * t_unit((d sqrt(H)+m(2q-1))/side),
// side=q for a positive numerator, otherwise 1-q.
inline Bounds bounds(span d,double sd,double nu,double m,double l,double h){
 const Interval q{l,h},one{1-h,1-l};
 const auto prod=mul(q,one); // dependency only enlarges the enclosure
 const auto H=add({1-m*m,1-m*m},scale(prod,4*m*m-3));
 if(!(H.lo>0))return {{-INFINITY,INFINITY},{-INFINITY,INFINITY}};
 const Interval root{std::sqrt(H.lo),std::sqrt(H.hi)};
 const auto Hp=scale({1-2*h,1-2*l},4*m*m-3);
 const auto rp=scale(divide(Hp,root),.5);
 Interval val{0,0},score{0,0};double vmag=0,smag=0;
 const double normal=std::log(2/sd)+dens::log_dstd_unit_const(nu),p=.5*(nu+1);
 for(double v:d){
  const auto N=add(scale(root,v),scale({2*l-1,2*h-1},m));
  const auto Np=add(scale(rp,v),{2*m,2*m});
  // A seam-crossing cell uses both branches. At N=0 the kernel score is 0.
  Interval vv{INFINITY,-INFINITY},ss{INFINITY,-INFINITY};
  for(int side=0;side<2;++side){
   if((side==0 && N.hi<0)||(side==1 && N.lo>0))continue;
   const Interval ns=side==0?Interval{std::max(0.,N.lo),N.hi}:Interval{N.lo,std::min(0.,N.hi)};
   const Interval den=side==0?q:one;
   // For a zero denominator, the kernel bound still excludes unsupported tails.
   const double absmin=ns.lo<=0 && ns.hi>=0?0:std::min(std::abs(ns.lo),std::abs(ns.hi));
   const double absmax=std::max(std::abs(ns.lo),std::abs(ns.hi));
   const double logwmin=std::log(absmin)-std::log(den.hi);
   const double logwmax=den.lo>0?std::log(absmax)-std::log(den.lo):INFINITY;
   const Interval b{normal+std::log(root.lo)-p*kernel_log(logwmax,nu),normal+std::log(root.hi)-p*kernel_log(logwmin,nu)};
   vv.lo=std::min(vv.lo,b.lo);vv.hi=std::max(vv.hi,b.hi);
   Interval s{-INFINITY,INFINITY};
   if(den.lo>0){
    // w*w'/(nu-2+w*w) = N*(N'*den-N*den') /
    // [den*((nu-2)*den^2+N^2)]. Avoid enormous w near skew faces.
    const auto numerator=mul(ns,add(mul(Np,den),scale(ns,side==0?-1:1)));
    const auto denominator=mul(den,add(scale(square(den),nu-2),square(ns)));
    s=add(scale(divide(Hp,H),.5),scale(divide(numerator,denominator),-(nu+1)));
   }
   ss.lo=std::min(ss.lo,s.lo);ss.hi=std::max(ss.hi,s.hi);
  }
  val=add(val,vv);score=add(score,ss);
  vmag+=std::max(std::isfinite(vv.lo)?std::abs(vv.lo):0.,std::abs(vv.hi))
       +2*(std::abs(normal)+std::max(std::abs(std::log(root.lo)),std::abs(std::log(root.hi))));
  smag+=std::max(std::abs(ss.lo),std::abs(ss.hi));
 }
 const double ve=skew_scalar_detail::margin(vmag);
 const double se=skew_scalar_detail::margin(smag);
 if(std::isfinite(ve)){val.lo-=ve;val.hi+=ve;}
 if(std::isfinite(se)){score.lo-=se;score.hi+=se;}
 return {val,score};
}
inline SstdResult fit(span x,int maxit,double mean,double sd,double nu){
 SstdResult out{mean,sd,nu,NAN,NAN,0,SstdStatus::CertificateFailure};
 out.stop_detail.loglik_upper=INFINITY;
 // The density seam stabilizes the t normalizer at 128, but still forms
 // the skew moment from a cancelling lgamma difference. Until that settled
 // seam has a stable ratio, do not certify a different numerical family.
 if(nu>=128){
  out.certificate_reason=4;
  double ll=0,mag=0;
  const double normal=dens::log_dstd_unit_const(nu)-std::log(sd);
  for(double v:x){const double d=(v-mean)/sd;
   const double term=.5*(nu+1)*std::log1p(d*d/(nu-2));
   ll+=normal-term;mag+=std::abs(normal)+std::abs(term);
  }
  if(std::isfinite(ll)&&std::isfinite(mag)){
   const double e=skew_scalar_detail::margin(mag);
   out.stop_detail.candidates.push_back({{mean,sd,nu,1},ll-e,ll+e});
   out.stop_detail.loglik_lower=ll-e;
  }
  return out;
 }
 double shift_at_two=0;dens::sstd_sigma(nu,2,shift_at_two);
 const double m=shift_at_two/1.5;
 if(!(m>0 && m<1) || !std::isfinite(m))return out;
 std::vector<double> y;
 for(double v:x){const double d=(v-mean)/sd;if(!std::isfinite(d)||(d==0 && v!=mean))return out;y.push_back(d);}
 std::vector<double> raw(x.begin(),x.end()),normalized=y;
 std::sort(raw.begin(),raw.end());std::sort(normalized.begin(),normalized.end());
 for(std::size_t i=1;i<raw.size();++i)if(raw[i]!=raw[i-1] && normalized[i]==normalized[i-1])return out;
 const span d{y.data(),y.size()};
 struct Candidate {double lo,hi,q,ll,error;bool face;};
 std::vector<Candidate> candidates;
 double incumbent=-INFINITY;
 const auto point=[&](double q){return bounds(d,sd,nu,m,q,q).value;};
 const auto seed=point(.5);
 if(!std::isfinite(seed.lo+seed.hi))return out;
 incumbent=std::max(incumbent,seed.lo);
 out.stop_detail.candidates.push_back({{mean,sd,nu,1},seed.lo,seed.hi});
 // Endpoint contacts require a removable 0/0 extension with the constrained
 // shrinking-side density, not the usual half-t endpoint density.
 for(double q:{0.,1.}){
  const double r=std::sqrt(1-m*m);
  bool supported=true;double ll=0,mag=0;
  for(double v:y){const double N=v*r+m*(2*q-1);
   if(std::abs(N)<=skew_scalar_detail::margin(std::abs(v*r)+m)){out.certificate_reason=3;return out;}
   if((q==0 && N>0)||(q==1 && N<0)){supported=false;break;}
   const double term=std::log(2*r/sd)+dens::log_dstd_unit_const(nu)-.5*(nu+1)*std::log1p(N*N/(nu-2));ll+=term;mag+=std::abs(term);
  }
  if(supported && std::isfinite(ll)){const double e=skew_scalar_detail::margin(mag);candidates.push_back({q,q,q,ll,e,true});incumbent=std::max(incumbent,ll-e);}
 }
 struct Cell{double l,h;};std::vector<Cell> pending{{0,1}};
 while(!pending.empty()){
  if(out.iterations>=certificate_budget(maxit)){out.stop_detail.budget_exhausted=true;out.stop_detail.loglik_lower=incumbent;out.stop_detail.loglik_upper=INFINITY;return out;}
  ++out.iterations;const auto c=pending.back();pending.pop_back();
  const auto b=bounds(d,sd,nu,m,c.l,c.h);
  if(std::isnan(b.value.hi))return out;
  if(b.value.hi<incumbent)continue;
  if((std::isfinite(b.score.lo) && b.score.lo>0) || (std::isfinite(b.score.hi) && b.score.hi<0))continue;
  const double mid=c.l+(c.h-c.l)/2;
  if(!(mid>c.l && mid<c.h))return out;
  const auto v=point(mid);
  if(!std::isfinite(v.lo+v.hi))return out;
  incumbent=std::max(incumbent,v.lo);
  const double rem=std::max(std::abs(b.score.lo),std::abs(b.score.hi))*(c.h-c.l)/2;
  const double err=(v.hi-v.lo)/2;
  if(c.h-c.l<=2e-13*std::min(mid,1-mid) && rem<=err){
   candidates.push_back({c.l,c.h,mid,(v.lo+v.hi)/2,err+rem,false});
  } else {pending.push_back({mid,c.h});pending.push_back({c.l,mid});}
 }
 if(candidates.empty())return out;
 std::sort(candidates.begin(),candidates.end(),[](const auto&a,const auto&b){return a.lo<b.lo;});
 std::vector<Candidate> merged;
 for(const auto& c:candidates){
  if(!merged.empty() && !c.face && !merged.back().face && c.hi-merged.back().lo<=1e-10*std::min(c.q,1-c.q)){
   auto& a=merged.back();const double upper=std::max(a.ll+a.error,c.ll+c.error);a.hi=c.hi;
   if(c.ll>a.ll){a.q=c.q;a.ll=c.ll;}a.error=upper-a.ll;
  }else merged.push_back(c);
 }
 for(auto& c:merged)if(!c.face){
  c.q=c.lo+(c.hi-c.lo)/2;const auto v=point(c.q);
  const double upper=c.ll+c.error;c.ll=(v.lo+v.hi)/2;
  c.error=std::max((v.hi-v.lo)/2,upper-c.ll);
 }
 const auto best=std::max_element(merged.begin(),merged.end(),[](const auto&a,const auto&b){return a.ll-a.error<b.ll-b.error;});
 out.stop_detail.candidates.clear();int competitors=0;out.stop_detail.loglik_lower=best->ll-best->error;double global_upper=-INFINITY;
 for(const auto& c:merged){
  const double xi=std::sqrt(c.q/(1-c.q));
  FitCandidate witness{{mean,sd,nu,xi},c.ll-c.error,c.ll+c.error};
  if(c.face){const double r=std::sqrt(1-m*m),scale=sd/r*std::sqrt((nu-2)/nu),endpoint=mean+(c.q==0?1:-1)*sd*m/r;
   witness={{endpoint,nu,scale},c.ll-c.error,c.ll+c.error,c.q==0?StopModel::ReflectedHt:StopModel::Ht};
   if(!valid_stop_coefficients(witness.model,witness.coef))return out;
   double represented=0;
   for(double v:x){
    const double distance=c.q==0?endpoint-v:v-endpoint;
    if(!(distance>=0) || !std::isfinite(distance))return out;
    const double w=distance/scale;
    represented+=std::log(2/scale)+dens::log_dstd_unit_const(nu)
       +.5*std::log((nu-2)/nu)-.5*(nu+1)*std::log1p(w*w/nu);
   }
   if(!std::isfinite(represented) || std::abs(represented-c.ll)>4*c.error)return out;
  }
  out.stop_detail.candidates.push_back(witness);global_upper=std::max(global_upper,c.ll+c.error);
  if(c.ll+c.error>=best->ll-best->error)++competitors;
 }
 out.stop_detail.loglik_upper=global_upper;
 if(competitors>1){out.stop_detail.comparison_tie=true;return out;}
 out.logLik=best->ll;
 if(best->face){out.status=best->q==0?SstdStatus::LeftHalfTBoundary:SstdStatus::RightHalfTBoundary;out.certified=true;
  out.stop_detail.limit_model=best->q==0?StopModel::ReflectedHt:StopModel::Ht;
  for(const auto& c:out.stop_detail.candidates)if(c.model==out.stop_detail.limit_model){out.stop_detail.limit_coef=c.coef;out.stop_detail.limit_loglik=out.logLik;break;}
  return out;
 }
 out.xi=std::sqrt(best->q/(1-best->q));
 if(!(std::isfinite(out.xi)&&out.xi>0)){out.stop_detail.loglik_upper=INFINITY;return out;}
 double shift=0;const double su=dens::sstd_sigma(nu,out.xi,shift);double ll=0;
 for(double v:x){const double z=(v-mean)/sd*su+shift,w=z/(z>0?out.xi:1/out.xi);ll+=std::log(su/sd)+std::log(2/(out.xi+1/out.xi))+dens::log_dstd_unit_const(nu)-.5*(nu+1)*std::log1p(w*w/(nu-2));}
 if(!std::isfinite(ll)||std::abs(ll-out.logLik)>4*best->error){out.stop_detail.loglik_upper=INFINITY;return out;}
 out.logLik=ll;out.status=SstdStatus::Ok;out.certified=true;out.stop_detail={};return out;
}
} // namespace sstd_skew_cert
