#pragma once
// Included inside cppunivariate after SstdResult. No density changes.
namespace sstd_cert_detail {
// Exact extrema of z/(c^2+z^2) or z(z-shift)/(c^2+z^2).
inline skew_scalar_detail::Range kernel(double lo,double hi,double c,double shift,bool scale) {
 skew_scalar_detail::Range out;
 if(!(c>=std::numeric_limits<double>::min()) || !std::isfinite(c))return {NAN,NAN};
 const auto add=[&](double z) {
  double v;
  if(std::isinf(z))v=scale?1:0;
  else {
   const double h=std::hypot(c,z);
   if(!std::isfinite(h)){skew_scalar_detail::include(out,NAN);return;}
   v=scale?(z/h)*((z-shift)/h):(z/h)/h;
  }
  skew_scalar_detail::include(out,v);
 };
 add(lo);add(hi);
 std::vector<double> roots;
 if(!scale)roots={-c,c};
 else if(shift==0)roots={0};
 else {
  const double h=std::hypot(c,shift);
  if(!std::isfinite(h))return {NAN,NAN};
  // Scaling first avoids c*shift and c*(c+h), which may overflow
  // even though both stationary coordinates are finite.
  const double a=c/h,b=shift/h;
  roots={c*(b/(1+a)),-c*((1+a)/b)};
  for(double r:roots)if(!std::isfinite(r))return {NAN,NAN};
 }
 for(double r:roots)if(r>=lo && r<=hi)add(r);
 return out;
}
inline skew_scalar_detail::Range term(double lo,double hi,double nu,double xi,double shift,bool scale) {
 skew_scalar_detail::Range out;
 if(lo<=0){const auto r=kernel(lo,std::min(0.,hi),std::sqrt(nu-2)/xi,shift,scale);skew_scalar_detail::include(out,r.lo);skew_scalar_detail::include(out,r.hi);}
 if(hi>=0){const auto r=kernel(std::max(0.,lo),hi,std::sqrt(nu-2)*xi,shift,scale);skew_scalar_detail::include(out,r.lo);skew_scalar_detail::include(out,r.hi);}
 return out;
}
// At critical mass the common scale power cancels. Compare every
// positive root of the resulting piecewise rational score with delta=0
// at collapse. A join is smooth for the value and first derivative.
inline SstdResult critical_scale(span x,double tol,int maxit,double mean,
    double unit,double nu,double xi,double su,double shift,double face,double face_error) {
 SstdResult out{mean,NAN,nu,xi,NAN,0,SstdStatus::CertificateFailure};
 out.certificate_reason=1;
 out.stop_detail.limit_loglik=face;
 out.stop_detail.loglik_lower=face-face_error;
 out.stop_detail.loglik_upper=INFINITY;
 std::vector<t_cert_detail::I> offsets;
 double farthest=0;
 const auto spread=t_cert_detail::I(su);
 const auto displacement=t_cert_detail::I(shift);
 const auto left_side=t_cert_detail::I(1)/xi,right_side=t_cert_detail::I(xi);
 const auto degrees=t_cert_detail::I(nu)-2;
 for(double v:x)if(v!=mean) {
  const auto d=(t_cert_detail::I(v)-mean)/unit*spread;
  if(!d.valid() || (d.lo<=0 && d.hi>=0))return out;
  offsets.push_back(d);farthest=std::max(farthest,d.mag());
 }
 const auto evaluate=[&](t_cert_detail::I scale,bool score) {
  t_cert_detail::Sum total;
  for(auto d:offsets) {
   const auto z=d+displacement*scale;
   const auto initial=d.lo>0?right_side:left_side;
   const auto term=[&](t_cert_detail::I residual,t_cert_detail::I side) {
    const auto c2=degrees*t_cert_detail::sq(side);
    const auto denominator=t_cert_detail::sq(residual)+c2*t_cert_detail::sq(scale);
    return score?(c2*scale+displacement*residual)/denominator:
      t_cert_detail::log(denominator/t_cert_detail::sq(d)*t_cert_detail::sq(initial/side));
   };
   if(!z.valid())return t_cert_detail::bad();
   t_cert_detail::I bound;
   if(z.lo>=0)bound=term(z,right_side);
   else if(z.hi<=0)bound=term(z,left_side);
   else {
    const auto left=term({z.lo,0},left_side),right=term({0,z.hi},right_side);
    if(!left.valid() || !right.valid())return t_cert_detail::bad();
    bound={std::min(left.lo,right.lo),std::max(left.hi,right.hi)};
   }
   if(!bound.valid())return t_cert_detail::bad();
   total.add(bound);
  }
  return total.get()*(-.5*(nu+1)*(score?2:1));
 };
 const double upper=2*(farthest/std::abs(shift));
 if(!(upper>0) || !std::isfinite(upper))return out;
 // Beyond upper, shift*z is positive on every side, so the score is negative.
 double lower=std::min(.5,upper/4);
 bool closed=false;
 for(std::int64_t j=0;j<solver_budget(maxit) && lower>0;++j) {
  ++out.iterations;
  const auto collar=evaluate({0,lower},true);
  if(collar.valid() && (collar.lo>0 || collar.hi<0)){closed=true;break;}
  lower/=2;
 }
 if(!closed)return out;
 std::vector<t_cert_detail::I> pending{{lower,upper}},roots;
 while(!pending.empty()) {
  if(out.iterations>=certificate_budget(maxit))return out;
  ++out.iterations;
  const auto box=pending.back();pending.pop_back();
  const auto g=evaluate(box,true);
  if(g.valid() && (g.lo>0 || g.hi<0))continue;
  const double mid=box.mid();
  if(g.valid() && box.hi-box.lo<=tol*mid/8) {
   if(!roots.empty() && roots.back().hi==box.lo)roots.back().hi=box.hi;
   else roots.push_back(box);
  } else {
   if(!(mid>box.lo && mid<box.hi))return out;
   pending.push_back({mid,box.hi});pending.push_back({box.lo,mid});
  }
 }
 double best_lower=0;
 std::size_t best=roots.size();
 std::vector<t_cert_detail::I> comparison;
 for(std::size_t j=0;j<roots.size();++j) {
  if(out.iterations>=certificate_budget(maxit))return out;
  ++out.iterations;
  const auto likelihood=evaluate(roots[j],false);
  if(!likelihood.valid())return out;
  comparison.push_back(likelihood);
  if(likelihood.lo>best_lower){best_lower=likelihood.lo;best=j;}
 }
 for(std::size_t j=0;j<comparison.size();++j)
  if(j!=best && !(comparison[j].hi<best_lower))return out;
 if(best==roots.size()) {
  out.status=SstdStatus::ScaleBoundary;out.certified=true;out.logLik=face;
  out.stop_detail.loglik_upper=face+face_error;return out;
 }
 const auto physical=t_cert_detail::scale_units(roots[best],unit);
 if(!physical.valid() || !(physical.lo>0) || physical.hi-physical.lo>tol*physical.lo)return out;
 out.sigma=physical.mid();
 const auto value=evaluate(t_cert_detail::I(out.sigma)/unit,false);
 if(!value.valid())return out;
 out.logLik=face+value.mid();
 if(!std::isfinite(out.logLik))return out;
 out.status=SstdStatus::Ok;out.certified=true;
 out.stop_detail={};return out;
}

inline SstdResult fit(span x,double tol,int maxit,double mean,double sd,double nu,double xi) {
 const bool hm=std::isfinite(mean),hs=std::isfinite(sd);
 SstdResult out{mean,sd,nu,xi,NAN,0,SstdStatus::CertificateFailure};
 double shift=0;const double su=dens::sstd_sigma(nu,xi,shift);
 if(!(su>0) || !std::isfinite(su+shift))return out;
 double lo=x[0],hi=x[0];for(double v:x){lo=std::min(lo,v);hi=std::max(hi,v);}
 const double origin=hm?mean:lo/2+hi/2;
 double unit=hs?sd:std::max(std::abs(lo-origin),std::abs(hi-origin));
 if(unit==0 && !hs){out.status=SstdStatus::Unbounded;out.certified=true;return out;}
 if(!(unit>0) || !std::isfinite(unit))return out;
 std::vector<double> y;for(double v:x){double z=(v-origin)/unit;if(!std::isfinite(z) || (z==0 && v!=origin))return out;y.push_back(z);}
 std::vector<double> raw(x.begin(),x.end()),normalized=y;
 std::sort(raw.begin(),raw.end());std::sort(normalized.begin(),normalized.end());
 for(std::size_t i=1;i<raw.size();++i)if(raw[i]!=raw[i-1] && normalized[i]==normalized[i-1])return out;
 const double n=static_cast<double>(x.size()),power=nu+1;
 const double normal=std::log(su)+std::log(2/(xi+1/xi))+dens::log_dstd_unit_const(nu)-std::log(unit);
 const auto value=[&](double t) {
  double ll=0,mag=n*(std::abs(std::log(su))+std::abs(std::log(2/(xi+1/xi)))+std::abs(dens::log_dstd_unit_const(nu))+std::abs(std::log(unit))+1);
  for(double v:y) {
   const double z=hs?(v-t)*su+shift:v*su*std::exp(t)+shift;
   const double w=z/(z>0?xi:1/xi);
   const double a=normal+(hs?0:t),b=.5*power*std::log1p(w*w/(nu-2));
   ll+=a-b;mag+=std::abs(a)+std::abs(b);
  }
  return skew_scalar_detail::Range{ll,skew_scalar_detail::margin(mag)};
 };
 const auto score=[&](double a,double b) {
  double lower=hs?0:n,upper=lower,mag=std::abs(lower);
  for(double v:y) {
   double za,zb;
   if(hs){za=(v-b)*su+shift;zb=(v-a)*su+shift;}
   else {za=v==0?shift:v*su*std::exp(a)+shift;zb=v==0?shift:v*su*std::exp(b)+shift;if(za>zb)std::swap(za,zb);}
   const auto r=term(za,zb,nu,xi,shift,!hs);
   const double factor=hs?power*su:-power;
   const double l=std::min(factor*r.lo,factor*r.hi),h=std::max(factor*r.lo,factor*r.hi);
   lower+=l;upper+=h;mag+=std::max(std::abs(l),std::abs(h));
  }
  const double pad=skew_scalar_detail::margin(mag);
  return skew_scalar_detail::Range{lower-pad,upper+pad};
 };
 double left,right;
 if(hs){left=(lo-origin)/unit+shift/su;right=(hi-origin)/unit+shift/su;}
 else {
  std::size_t k=0;for(double v:x)if(v==mean)++k;
  const double exponent=std::fma(nu,double(x.size()-k),-double(k));
  if(exponent<0){out.status=SstdStatus::Unbounded;out.certified=true;return out;}
  if(exponent==0) {
   // Keep the relative accuracy of a small nonzero skew displacement.
   shift=dens::std_abs_moment(nu)*(xi-1)*(1+1/xi);
   if(!std::isfinite(shift))return out;
   // With one off-mean value the critical likelihood ratio is a
   // single piecewise quadratic raised to a negative power (TASK-264).
   const double other=lo!=mean?lo:hi;
   const bool one_distance=std::all_of(x.begin(),x.end(),[&](double v){
    return v==mean || v==other;
   });
   if(xi!=1 && shift==0)return out;
   if(xi!=1 && one_distance && ((other>mean)!=(shift>0))) {
    // The vertex precedes the join crossing. After the crossing the
    // quadratic on the new side is strictly increasing.
    const double side=other>mean?xi:1/xi;
    const double curvature=std::hypot(shift,std::sqrt(nu-2)*side);
    const double ratio=(std::abs(shift)/curvature)*(su/curvature);
    if(tol<64*std::numeric_limits<double>::epsilon())return out;
    out.sigma=unit*ratio; // unit is the single off-mean distance
    if(!(out.sigma>0) || !std::isfinite(out.sigma) ||
       std::abs(out.sigma/unit-ratio)>16*std::numeric_limits<double>::epsilon()*ratio)return out;
    out.logLik=value(-std::log(out.sigma/unit)).lo;
    if(!std::isfinite(out.logLik))return out;
    out.status=SstdStatus::Ok;out.certified=true;
    return out;
   }
   // The finite zero-scale value from the critical density product.
   const double tied_side=shift>0?xi:1/xi;
   const double tied_log=std::log1p(std::pow((shift/tied_side)/std::sqrt(nu-2),2));
   double ll=n*(std::log(su)+std::log(2/(xi+1/xi))+dens::log_dstd_unit_const(nu))-.5*power*k*tied_log;
   double mag=std::abs(ll)+n*(1+std::abs(std::log(su))+std::abs(std::log(xi))+
     std::abs(dens::log_dstd_unit_const(nu))+.5*power*tied_log);
   for(double v:x)if(v!=mean){
    const double side=v>mean?xi:1/xi;
    const double t=power*(std::log(std::sqrt(nu-2))+std::log(side)-std::log(su)-special::log_abs_difference(v,mean));
    ll+=t;mag+=std::abs(t);
   }
   if(!std::isfinite(ll) || !std::isfinite(mag))return out;
   const double e=skew_scalar_detail::margin(mag);
   if(xi!=1 && !one_distance) {
    auto critical=critical_scale(x,tol,maxit,mean,unit,nu,xi,su,shift,ll,e);
    if(critical.status==SstdStatus::CertificateFailure) {
     const auto v=value(0);
     if(std::isfinite(v.lo)&&std::isfinite(v.hi)) {
      critical.stop_detail.candidates.push_back({{mean,unit,nu,xi},v.lo-v.hi,v.lo+v.hi});
      critical.stop_detail.loglik_lower=std::max(critical.stop_detail.loglik_lower,v.lo-v.hi);
     }
    }
    return critical;
   }
   // For unit skew or a single residual value moving away from the
   // join every positive scale loses strictly to collapse.
   out.logLik=ll;out.status=SstdStatus::ScaleBoundary;out.certified=true;
   out.stop_detail.limit_loglik=ll;
   out.stop_detail.loglik_lower=ll-e;out.stop_detail.loglik_upper=ll+e;return out;
  }
  left=-1;right=1;bool bracket=false;
  for(std::int64_t i=0;i<solver_budget(maxit);++i) {
   // Entire half-lines, not just signs at two finite probes.
   const auto l=score(-INFINITY,left),r=score(right,INFINITY);
   if(l.lo>0 && r.hi<0){bracket=true;break;}
   if(!(std::isfinite(l.lo)&&std::isfinite(r.hi)))return out;
   if(l.lo<=0)left-=1;
   if(r.hi>=0)right+=1;
  }
  if(!bracket) {
   out.certificate_reason=2;
   const auto v=value(0);if(std::isfinite(v.lo))out.stop_detail.candidates.push_back({{mean,unit,nu,xi},v.lo-v.hi,v.lo+v.hi});
   out.stop_detail.loglik_upper=INFINITY;return out;
  }
 }
 if(hs && left==right) {
  out.mu=origin+unit*left;out.logLik=value(left).lo;
  if(!std::isfinite(out.mu)||!std::isfinite(out.logLik)||std::abs((out.mu-origin)/unit-left)>1e-9*(1+std::abs(left)))return out;
  out.status=SstdStatus::Ok;out.certified=true;return out;
 }
 auto search=skew_scalar_detail::enumerate(left,right,maxit,score,value);
 out.iterations=search.evaluations;out.stop_detail.budget_exhausted=search.budget;
 const auto coef=[&](double t){return std::vector<double>{hm?mean:origin+unit*t,hs?sd:unit*std::exp(-t),nu,xi};};
 const auto valid=[&](const std::vector<double>& c){return valid_stop_coefficients(StopModel::None,c) && c[1]>0;};
 for(const auto& p:search.points) {
  const auto c=coef(p.x);
  if(!valid(c)){out.stop_detail.budget_exhausted=false;out.stop_detail.loglik_upper=INFINITY;return out;}
  out.stop_detail.candidates.push_back({c,p.ll-p.error,p.ll+p.error});
 }
 if(!search.ok){
  const auto& w=search.witness;
  if(std::isfinite(w.ll)&&std::isfinite(w.error)&&valid(coef(w.x)))out.stop_detail.candidates.push_back({coef(w.x),w.ll-w.error,w.ll+w.error});
  if(out.stop_detail.candidates.empty())out.stop_detail.budget_exhausted=false;
  out.stop_detail.loglik_upper=INFINITY;
  for(const auto& c:out.stop_detail.candidates)out.stop_detail.loglik_lower=std::isnan(out.stop_detail.loglik_lower)?c.loglik_lower:std::max(out.stop_detail.loglik_lower,c.loglik_lower);
  return out;
 }
 const auto best=std::max_element(search.points.begin(),search.points.end(),[](const auto& a,const auto& b){return a.ll<b.ll;});
 out.stop_detail.loglik_lower=best->ll-best->error;out.stop_detail.loglik_upper=-INFINITY;
 int competitors=0;
 for(const auto& p:search.points){out.stop_detail.loglik_upper=std::max(out.stop_detail.loglik_upper,p.ll+p.error);if(p.ll+p.error>=best->ll-best->error)++competitors;}
 if(competitors>1){out.stop_detail.comparison_tie=true;return out;}
 const auto c=coef(best->x);out.mu=c[0];out.sigma=c[1];out.logLik=best->ll;
 if(!std::isfinite(out.mu)||!(out.sigma>0)||!std::isfinite(out.sigma))return out;
 const double recovered=hm?std::log(unit)-std::log(out.sigma):(out.mu-origin)/unit;
 if(std::abs(recovered-best->x)>1e-9*(1+std::abs(best->x)))return out;
 // Re-evaluate at represented public coefficients; reject lossy transport.
 double represented=0;
 for(double v:x){const double z=(v-out.mu)/out.sigma*su+shift,w=z/(z>0?xi:1/xi);represented+=std::log(su/out.sigma)+std::log(2/(xi+1/xi))+dens::log_dstd_unit_const(nu)-.5*power*std::log1p(w*w/(nu-2));}
 if(!std::isfinite(represented)||std::abs(represented-out.logLik)>4*best->error)return out;
 out.logLik=represented;out.status=SstdStatus::Ok;out.certified=true;out.stop_detail={};return out;
}
} // namespace sstd_cert_detail
