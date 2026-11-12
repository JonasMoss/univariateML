#pragma once
#include "mle.hpp"
#include "winner_certificate.hpp"
#include "accuracy.hpp"
#include <map>
#include <queue>

namespace cppunivariate {
namespace bhatt_detail {
inline FitCandidate cert_witness(span x,double mu,double sigma,double a) {
  FitCandidate out;out.coef={mu,sigma,a};
  if(!valid(mu,sigma,a))return out;
  CertSum sum,mag;
  for(double v:x){const double term=distributions::dbhatt(v,mu,sigma,a,true);sum.add(term);mag.add(std::abs(term));}
  const double ll=static_cast<double>(sum.value),e=1024*std::numeric_limits<double>::epsilon()*static_cast<double>(x.size()+mag.value);
  if(std::isfinite(ll)&&std::isfinite(e)){out.loglik_lower=ll-e;out.loglik_upper=ll+e;}
  return out;
}
inline bool cert_valid(const FitCandidate& c) {
  return valid_stop_coefficients(c.model,c.coef)&&std::isfinite(c.loglik_lower)&&std::isfinite(c.loglik_upper)&&c.loglik_lower<=c.loglik_upper;
}
// A limit encoded by endpoints must retain the parent half-width to the
// requested coefficient accuracy. Large locations can otherwise round the
// endpoint difference to a substantially different width.
inline bool uniform_representation_valid(const BhattResult& face,double tol) {
  const auto& c=face.stop_detail.limit_coef;
  if(!valid_stop_coefficients(StopModel::Unif,c)||!(face.a>0&&std::isfinite(face.a)))return false;
  const double span=c[1]-c[0];
  const double half=std::isfinite(span)?span/2:c[1]/2-c[0]/2;
  const double relative=std::max(tol,1024*std::numeric_limits<double>::epsilon());
  return half>0&&std::isfinite(half)&&std::abs(half-face.a)<=relative*std::max(half,face.a);
}
inline BhattResult cert_stop(BhattStatus status,const std::vector<FitCandidate>& witnesses,int iterations=0) {
  BhattResult out={NAN,NAN,NAN,NAN,iterations,status};
  auto& d=out.stop_detail;d.loglik_upper=INFINITY;
  for(const auto& c:witnesses)if(cert_valid(c)){d.candidates.push_back(c);d.loglik_lower=std::isnan(d.loglik_lower)?c.loglik_lower:std::max(d.loglik_lower,c.loglik_lower);}
  d.budget_exhausted=status==BhattStatus::BudgetExhausted;d.comparison_tie=status==BhattStatus::Tie;
  return out;
}
// Conditional normal RMS must be scaled before squaring: the shared
// conditional delegate can underflow distinct tiny residuals to zero.
inline BhattResult normal_member_certificate(span x,double mu,double sigma,double tol) {
  const auto moments=norm_detail::moments(x);
  const double m=std::isnan(mu)?moments.mean:mu;
  if(std::isnan(sigma)&&std::all_of(x.begin(),x.end(),[&](double v){return v==m;}))
    return {m,0,0,INFINITY,0,BhattStatus::Unbounded};
  double scale=std::abs(m);for(double v:x)scale=std::max(scale,std::abs(v));
  double sd=sigma;
  if(std::isnan(sigma)){
    if(std::isnan(mu))sd=moments.sd;
    else{
      CertSum square;
      for(double v:x){
        const CPPUNIV_EXTENDED_REAL difference=static_cast<CPPUNIV_EXTENDED_REAL>(v)-m;
        const CPPUNIV_EXTENDED_REAL d=std::isfinite(difference)?difference/scale:static_cast<CPPUNIV_EXTENDED_REAL>(v)/scale-static_cast<CPPUNIV_EXTENDED_REAL>(m)/scale;
        square.add(d*d);
      }
      sd=static_cast<double>(static_cast<CPPUNIV_EXTENDED_REAL>(scale)*std::sqrt(square.value/x.size()));
    }
  }
  if(!(sd>0&&std::isfinite(sd))||(std::isnan(sigma)&&sd<std::numeric_limits<double>::min()))
    return cert_stop(BhattStatus::NumericalFailure,{cert_witness(x,m,scale,0)});
  const auto witness=cert_witness(x,m,sd,0);
  if(!cert_valid(witness))return cert_stop(BhattStatus::NumericalFailure,{witness});
  if(std::isnan(sigma)&&tol<128*std::numeric_limits<double>::epsilon())return cert_stop(BhattStatus::NumericalFailure,{witness});
  if(std::isnan(mu)&&scale>0){
    CertSum residual,magnitude;
    for(double v:x){
      const CPPUNIV_EXTENDED_REAL difference=static_cast<CPPUNIV_EXTENDED_REAL>(v)-m;
      const CPPUNIV_EXTENDED_REAL d=std::isfinite(difference)?difference/scale:static_cast<CPPUNIV_EXTENDED_REAL>(v)/scale-static_cast<CPPUNIV_EXTENDED_REAL>(m)/scale;
      residual.add(d);magnitude.add(std::abs(d));
    }
    const CPPUNIV_EXTENDED_REAL error=(std::abs(residual.value)+64*std::numeric_limits<double>::epsilon()*magnitude.value)/x.size()+64*std::numeric_limits<double>::denorm_min();
    const CPPUNIV_EXTENDED_REAL target=static_cast<CPPUNIV_EXTENDED_REAL>(tol)*(static_cast<CPPUNIV_EXTENDED_REAL>(sd)/scale);
    if(magnitude.value>0&&!(error<=target))return cert_stop(BhattStatus::NumericalFailure,{witness});
  }
  return {m,sd,0,(witness.loglik_lower/2+witness.loglik_upper/2),0,BhattStatus::Ok};
}
// Error-free difference comparison in original physical coordinates. If the
// rounded difference equals the threshold, its residual decides the side.
inline int cert_difference_compare(double x,double y,double threshold) {
  const double hi=x-y;
  if(!std::isfinite(hi))return hi>0?1:-1;
  const double virtual_y=x-hi;
  const double lo=(x-(hi+virtual_y))+(virtual_y-y);
  if(hi<threshold)return -1;
  if(hi>threshold)return 1;
  return lo<0?-1:(lo>0?1:0);
}
inline int cert_distance_compare(double x,double mu,double a) {
  return x>=mu?cert_difference_compare(x,mu,a):cert_difference_compare(mu,x,a);
}
// 2 means that halving would discard a subnormal data bit: do not decide
// an exact covering equality from those rounded coordinates.
inline int cert_range_compare(double xmin,double xmax,double a) {
  if(a<=std::numeric_limits<double>::max()/2)return cert_difference_compare(xmax,xmin,2*a);
  if(2*(xmin/2)!=xmin||2*(xmax/2)!=xmax)return 2;
  return cert_difference_compare(xmax/2,xmin/2,a);
}
inline BhattResult held_width_face(span x,double mu,double a,int comparison,double tol) {
  const double xmin=*std::min_element(x.begin(),x.end()),xmax=*std::max_element(x.begin(),x.end());
  const double midpoint=std::isnan(mu)?xmin/2+xmax/2:mu;
  CertSum ll;ll.add(-static_cast<CPPUNIV_EXTENDED_REAL>(x.size())*(std::log(CPPUNIV_EXTENDED_LITERAL(2.,2.L))+std::log(static_cast<CPPUNIV_EXTENDED_REAL>(a))));
  if(std::isnan(mu)&&comparison==0){
    std::size_t left=0,right=0;for(double v:x){if(v==xmin)++left;if(v==xmax)++right;}
    const CPPUNIV_EXTENDED_REAL total=static_cast<CPPUNIV_EXTENDED_REAL>(left)+right;
    ll.add(left*std::log(static_cast<CPPUNIV_EXTENDED_REAL>(left)/total));
    ll.add(right*std::log(static_cast<CPPUNIV_EXTENDED_REAL>(right)/total));
  }else if(!std::isnan(mu)){
    for(double v:x)if(cert_distance_compare(v,mu,a)==0)ll.add(-std::log(CPPUNIV_EXTENDED_LITERAL(2.,2.L)));
  }
  const std::vector<double> endpoints=std::isnan(mu)&&comparison==0?std::vector<double>{xmin,xmax}:std::vector<double>{midpoint-a,midpoint+a};
  const double value=static_cast<double>(ll.value);
  if(!valid_stop_coefficients(StopModel::Unif,endpoints)||!std::isfinite(value))
    return cert_stop(BhattStatus::NumericalFailure,{cert_witness(x,midpoint,a,a)});
  BhattResult out={midpoint,0,a,value,0,BhattStatus::UniformLimit};
  out.stop_detail=limit_detail(endpoints,value,StopModel::Unif);
  if(!uniform_representation_valid(out,tol))
    return cert_stop(BhattStatus::NumericalFailure,{cert_witness(x,midpoint,a,a)});
  const double margin=1024*std::numeric_limits<double>::epsilon()*(x.size()+std::abs(value));
  out.stop_detail.candidates.front().loglik_lower-=margin;
  out.stop_detail.candidates.front().loglik_upper+=margin;
  return out;
}
inline BhattResult cert_result(const CertModel& model,const CertRoot& root,int iterations=0) {
  double m,s,a;model.coordinates(root.center,m,s,a);
  // Preserve physical holds exactly, including round-trips through b=1/sigma.
  if(!std::isnan(model.mu))m=model.mu;
  if(!std::isnan(model.sigma))s=model.sigma;
  if(!std::isnan(model.a))a=model.a;
  const auto witness=cert_witness(model.x,m,s,a);
  if(!root.ok||!root.accurate||!cert_valid(witness))return cert_stop(BhattStatus::NumericalFailure,{witness},iterations);
  return {m,s,a,(witness.loglik_lower/2+witness.loglik_upper/2),iterations,BhattStatus::Ok};
}
// Positive held width: a globally concave likelihood in the affine endpoint
// coordinates (u,b). A validated stationary root is the global maximizer.
inline BhattResult held_width_certificate(span x,double tol,int maxit,double mu,double sigma,double a,double origin=0,double unit=1) {
  CertModel model(x,mu,sigma,a);model.origin=origin;model.unit=unit;
  const auto p=model.initial();
  const auto root=cert_newton(model,p,tol,maxit);
  if(root.ok&&root.accurate)return cert_result(model,root);
  double m,s,w;model.coordinates(root.ok?root.center:p,m,s,w);
  return cert_stop(BhattStatus::IterationFailure,{cert_witness(x,m,s,w)});
}
struct ShapeTangent {double intercept=NAN;CertInterval slope{};};
inline CertInterval cert_log(CertInterval x) {
  if(!x.valid()||!(x.lo>0))return {NAN,NAN};
  const double lo=std::log(x.lo),hi=std::log(x.hi);
  const double e=64*std::numeric_limits<double>::epsilon()*(1+std::max(std::abs(lo),std::abs(hi)));
  return {lo-e,hi+e};
}
// F(k)=P(k)+n log k is concave. Supporting tangents bound F from above.
// Their minimum minus n log k is convex on each linear piece; its maximum
// occurs at an endpoint or at the intersection of the two support lines.
inline double shape_tangent_upper(const ShapeTangent& left,const ShapeTangent& right,
    double lo,double hi,std::size_t size) {
  const double n=static_cast<double>(size),ls=left.slope.hi,rs=right.slope.lo;
  const auto line=[&](bool first,CertInterval k){
    return CertInterval(first?left.intercept:right.intercept)+CertInterval(first?ls:rs)*(k-CertInterval(first?lo:hi))-CertInterval(n)*cert_log(k);
  };
  const auto ll=line(true,CertInterval(lo)),lr=line(true,CertInterval(hi));
  const auto rl=line(false,CertInterval(lo)),rr=line(false,CertInterval(hi));
  if(!ll.valid()||!lr.valid()||!rl.valid()||!rr.valid())return INFINITY;
  double upper=std::min(std::max(ll.hi,lr.hi),std::max(rl.hi,rr.hi));
  const CertInterval denominator=CertInterval(ls)-CertInterval(rs);
  if(denominator.valid()&&(denominator.lo>0||denominator.hi<0)){
    const CertInterval crossing=(CertInterval(right.intercept)-CertInterval(left.intercept)+CertInterval(ls)*CertInterval(lo)-CertInterval(rs)*CertInterval(hi))/denominator;
    if(crossing.valid()){
      double sharper=std::max(std::min(ll.hi,rl.hi),std::min(lr.hi,rr.hi));
      if(crossing.hi>lo&&crossing.lo<hi){
        const CertInterval middle(std::max(lo,crossing.lo),std::min(hi,crossing.hi));
        const auto a=line(true,middle),b=line(false,middle);
        if(!a.valid()||!b.valid())return upper;
        sharper=std::max(sharper,std::min(a.hi,b.hi));
      }
      upper=std::min(upper,sharper);
    }
  }
  return upper+256*std::numeric_limits<double>::epsilon()*(n+std::abs(upper));
}
struct CertSegment {double lo,hi,left,right,upper;};
struct CertSegmentCompare {bool operator()(const CertSegment& a,const CertSegment& b)const{return a.upper<b.upper;}};
struct WidthCertificate {
  span x;double tol,mu,sigma;int maxit;
  CertificateWork work;CertModel model;
  FitCandidate normal{},uniform{},finite{};bool has_finite=false;
  double incumbent=-INFINITY,normal_upper=-INFINITY,tail_upper=INFINITY,last_upper=INFINITY;
  std::map<double,CertRoot> cache{};
  std::map<double,ShapeTangent> tangents{};
  std::vector<CertWinner> winners{};
  std::priority_queue<CertSegment,std::vector<CertSegment>,CertSegmentCompare> queue{};
  WidthCertificate(span data,double t,int budget,double m,double s,double origin=0,double unit=1):x(data),tol(t),mu(m),sigma(s),maxit(budget),work(budget),model(data,m,s,NAN,true){model.origin=origin;model.unit=unit;}
  void record(const FitCandidate& c) {
    if(!cert_valid(c))return;
    if(!has_finite||c.loglik_lower>finite.loglik_lower){finite=c;has_finite=true;}
    incumbent=std::max(incumbent,c.loglik_lower);
  }
  bool profile(double k,CertRoot& out) {
    const auto old=cache.find(k);if(old!=cache.end()){out=old->second;return true;}
    if(!work.charge())return false;
    const CertModel slice(x,mu,sigma,NAN,false,k);
    out=cert_newton(slice,slice.initial(k),std::min(tol,1e-9),maxit);
    const bool enclosed_root=out.ok;
    if(!out.ok&&!std::isnan(sigma)){
      // A broad fixed-sigma convolution can be nearly flat in location.
      // It needs an upper profile bound, not an accurate nuisance estimate.
      // The unique location optimum is in the data hull; concavity gives
      // a tangent upper bound over that entire interval.
      const auto p=slice.initial(k);const auto e=cert_evaluate(slice,p);
      if(!e.ok)return false;
      out.center=p;out.ll_lower=e.ll-e.ll_error;out.ll_upper=e.ll+e.ll_error;
      if(slice.location>=0){
        const double b=1/sigma,lo=b*(*std::min_element(x.begin(),x.end())),hi=b*(*std::max_element(x.begin(),x.end()));
        const CertInterval displacement(lo-p[slice.location],hi-p[slice.location]);
        const CertInterval correction=e.gradient[slice.location]*displacement;
        if(!correction.valid())return false;
        out.ll_upper+=std::max(0.,correction.hi);
      }
      out.ok=true;
    }
    if(!out.ok)return false;
    double m,s,a;slice.coordinates(out.center,m,s,a);record(cert_witness(x,m,std::isnan(sigma)?s:sigma,a));
    // Enclose F'(k) at the exact nuisance optimum when its score root is
    // enclosed. The local Hessian row propagates the nuisance enclosure.
    auto p=model.initial(k);for(int i=0;i<slice.dim;++i)p[i]=out.center[i];p[model.shape]=k;
    const auto evaluation=cert_evaluate(model,p);if(!evaluation.ok)return false;
    ShapeTangent tangent;CertInterval derivative=evaluation.gradient[model.shape];
    if(enclosed_root){
      std::array<CertInterval,3> box{};
      for(int i=0;i<model.dim;++i)box[i]=CertInterval(p[i]);
      for(int i=0;i<slice.dim;++i)box[i]=out.enclosure[i];
      const auto hessian=cert_hessian(model,box);
      for(int i=0;i<slice.dim;++i)derivative=derivative+hessian[model.shape][i]*(out.enclosure[i]-CertInterval(p[i]));
      tangent.intercept=out.ll_upper+x.size()*std::log(k);
    }else{
      // A fixed-sigma location fallback still supplies a joint supporting
      // plane. Maximize its nuisance linear term over the data hull.
      tangent.intercept=evaluation.ll+evaluation.ll_error+x.size()*std::log(k);
      if(model.location>=0){
        const CertInterval displacement((*std::min_element(x.begin(),x.end()))/sigma-p[model.location],(*std::max_element(x.begin(),x.end()))/sigma-p[model.location]);
        const auto correction=evaluation.gradient[model.location]*displacement;
        if(!correction.valid())return false;
        tangent.intercept+=std::max(0.,correction.hi);
      }
    }
    tangent.intercept+=256*std::numeric_limits<double>::epsilon()*(x.size()+std::abs(tangent.intercept));
    tangent.slope=derivative+CertInterval(static_cast<double>(x.size()))/CertInterval(k);
    if(!tangent.slope.valid()||!std::isfinite(tangent.intercept))return false;
    tangents.emplace(k,tangent);cache.emplace(k,out);return true;
  }
  bool propose(double k,const CertRoot& inner) {
    if(!work.charge())return false;
    auto p=model.initial(k);
    if(model.location>=0)p[model.location]=inner.center[model.location];
    if(std::isnan(mu)&&model.scale>=0)p[model.scale]=inner.center[model.scale];
    const auto root=cert_newton(model,p,tol,maxit);
    if(!root.ok){
      // An unrestricted finite witness beating both faces still needs an
      // enclosed stationary winner. If none exists yet and this full solve
      // fails, report the numerical failure instead of repeating it across
      // the entire outer subdivision budget.
      if(winners.empty()&&std::isnan(mu)&&std::isnan(sigma)&&has_finite&&
          finite.loglik_lower>normal_upper&&finite.loglik_lower>uniform.loglik_upper)return false;
      return true;
    }
    double m,s,a;model.coordinates(root.center,m,s,a);record(cert_witness(x,m,std::isnan(sigma)?s:sigma,a));
    // No winner can be accepted without its coefficient enclosure. Preserve
    // a successful search as uncertified instead of repeating this failure.
    if(winners.empty()&&!root.accurate)return false;
    auto winner=cert_winner_region(model,root,maxit);
    if(!winner.ok)return true;
    for(auto& old:winners)if(winner.lo<=old.hi&&winner.hi>=old.lo){
      // Overlapping strictly concave profile regions have the same root.
      old.lo=std::min(old.lo,winner.lo);old.hi=std::max(old.hi,winner.hi);
      if((winner.root.accurate&&!old.root.accurate)||
          (winner.root.accurate==old.root.accurate&&winner.root.error<old.root.error))old.root=winner.root;
      return true;
    }
    winners.push_back(winner);return true;
  }
  bool covered(double lo,double hi)const {for(const auto& w:winners)if(w.lo<=lo&&w.hi>=hi)return true;return false;}
  bool push(double lo,double hi,double left,double right) {
    if(!work.charge())return false;
    const auto l=tangents.find(lo),r=tangents.find(hi);
    if(l==tangents.end()||r==tangents.end())return false;
    const double upper=shape_tangent_upper(l->second,r->second,lo,hi,x.size());
    if(!std::isfinite(upper))return false;
    queue.push({lo,hi,left,right,upper});return true;
  }
  BhattResult stop(BhattStatus status)const {
    std::vector<FitCandidate> witnesses{normal};if(cert_valid(uniform))witnesses.push_back(uniform);if(has_finite)witnesses.push_back(finite);
    for(const auto& winner:winners){
      double m,s,a;model.coordinates(winner.root.center,m,s,a);
      if(!std::isnan(mu))m=mu;
      if(!std::isnan(sigma))s=sigma;
      witnesses.push_back(cert_witness(x,m,s,a));
    }
    auto out=cert_stop(status,witnesses,static_cast<int>(work.used));
    out.stop_detail.segment_loglik_upper=last_upper;
    // The unexcluded normal neighbourhood makes +infinity intentional
    // until the small-width bound has been closed.
    return out;
  }
  BhattResult stopped()const {return stop(work.exhausted?BhattStatus::BudgetExhausted:BhattStatus::IterationFailure);}
  BhattResult run() {
    const auto n=norm_mle(x,mu,sigma);
    if(n.status!=NormStatus::Ok)return stop(BhattStatus::NumericalFailure);
    normal=cert_witness(x,n.mean,n.sd,0);
    if(!cert_valid(normal))return stop(BhattStatus::NumericalFailure);
    normal_upper=normal.loglik_upper;incumbent=normal.loglik_lower;
    if(std::isnan(mu)){
      // The normal alternative is a profile maximum, not merely the
      // likelihood at a rounded mean. Bound that mean's lost likelihood.
      CertSum residual,magnitude;
      for(double v:x){const CPPUNIV_EXTENDED_REAL d=static_cast<CPPUNIV_EXTENDED_REAL>(v)-n.mean;residual.add(d);magnitude.add(std::abs(d));}
      const CPPUNIV_EXTENDED_REAL error=(std::abs(residual.value)+64*std::numeric_limits<double>::epsilon()*magnitude.value)/x.size()+64*std::numeric_limits<double>::denorm_min();
      const CPPUNIV_EXTENDED_REAL ratio=error/n.sd;
      const CPPUNIV_EXTENDED_REAL correction=std::isnan(sigma)?(ratio<1?-static_cast<CPPUNIV_EXTENDED_REAL>(x.size())*std::log1p(-ratio*ratio)/2:INFINITY):x.size()*ratio*ratio/2;
      normal_upper+=static_cast<double>(correction);
    }
    if(std::isnan(sigma)&&n.sd<std::numeric_limits<double>::min())normal_upper=INFINITY;
    normal_upper+=256*std::numeric_limits<double>::epsilon()*(x.size()+std::abs(normal_upper));
    if(std::isnan(sigma)){
      const double xmin=*std::min_element(x.begin(),x.end()),xmax=*std::max_element(x.begin(),x.end());
      const double radius=std::isnan(mu)?(xmax-xmin)/2:std::max(std::abs(xmin-mu),std::abs(xmax-mu));
      const double midpoint=std::isnan(mu)?xmin/2+xmax/2:mu;
      const double ll=-static_cast<double>(x.size())*(std::log(2.)+std::log(radius));
      const double margin=1024*std::numeric_limits<double>::epsilon()*(x.size()+std::abs(ll));
      uniform={{midpoint-radius,midpoint+radius},ll-margin,ll+margin,StopModel::Unif};
      if(!cert_valid(uniform))return stop(BhattStatus::NumericalFailure);
      incumbent=std::max(incumbent,uniform.loglik_lower);
    }
    CertSum ss;
    for(double v:x){const CPPUNIV_EXTENDED_REAL z=(static_cast<CPPUNIV_EXTENDED_REAL>(v)-n.mean)/n.sd;ss.add(z*z);}
    double start=static_cast<double>(std::sqrt(std::max(CPPUNIV_EXTENDED_LITERAL(.1,.1L),3*(ss.value/x.size()-1))));
    // The legacy search is a proposer only. No likelihood, convergence flag
    // or face selection from it is used by the acceptance certificate.
    if(std::isnan(sigma)){
      const auto proposed=bhatt_detail::legacy_candidate(x,tol,maxit,sigma,NAN,mu);
      // This outer certificate isolates positive-width roots; it has no
      // global normal-neighbourhood domination proof with all scales free.
      // Do not subdivide merely to rediscover that missing proof. The public
      // policy retains this successful existing search with certified=false.
      if(std::isnan(mu)&&proposed.status==BhattStatus::Ok&&proposed.a==0)
        return stop(BhattStatus::Unresolved);
      if(proposed.sigma>0&&proposed.a>0&&std::isfinite(proposed.sigma)&&std::isfinite(proposed.a))start=proposed.a/proposed.sigma;
    }
    if(!(start>0&&std::isfinite(start)))return stop(BhattStatus::NumericalFailure);
    CertRoot point;
    if(!profile(start,point))return stopped();
    if(!propose(start,point))return stopped();
    double lo=std::min(.25,start),hi=std::max(2.,start);
    bool closed=false;
    for(int it=0;it<solver_budget(maxit);++it,lo*=.5){
      if(!work.charge())return stopped();
      // Hoeffding for T uniform[-k,k]: E exp(zT-T^2/2) <= exp(z^2 k^2/2).
      // Optimize the negative quadratic in held/free mu at the normal mean.
      const double extra=std::isnan(sigma)?-static_cast<double>(x.size())*std::log1p(-lo*lo)/2:static_cast<double>(ss.value)*lo*lo/2;
      tail_upper=normal_upper+extra+128*std::numeric_limits<double>::epsilon()*(x.size()+extra);
      if(tail_upper<incumbent){closed=true;break;}
      // Shrinking k below this point cannot separate the practical margin.
      if(extra<=4*(normal_upper-normal.loglik_lower)&&incumbent<=normal_upper)return stop(BhattStatus::Unresolved);
      if(!profile(lo,point))return stopped();
      if(!propose(lo,point))return stopped();
    }
    if(!closed)return stop(BhattStatus::Unresolved);
    closed=false;
    // For k >= 2n, the universal uniform-face bound holds. If a >= r,
    // use the peak bound. If a < r, retain one extreme's Q(k*(r/a-1));
    // n*log(t)+log Q(k*(t-1)) decreases for t>=1 since R(v)>=sqrt(2/pi).
    if(std::isnan(sigma)){
      // For every K>=hi and a<r, Q(K*(t-1)) <= exp(-K^2*(t-1)^2/2)/2.
      // Since log(t)<=t-1, n*log(t)+log Q <= n^2/(2*hi^2)-log(2).
      // For a>=r use the uniform peak bound. This closes a whole tail
      // earlier when an interior witness already dominates the uniform face.
      for(int it=0;it<solver_budget(maxit);++it,hi*=2){
        if(!work.charge())return stopped();
        if(hi>=2.*x.size()){closed=true;break;}
        const double ratio=static_cast<double>(x.size())/hi;
        const double extra=std::max(0.,ratio*ratio/2-std::log(2.));
        const double upper=uniform.loglik_upper+extra;
        const double margin=256*std::numeric_limits<double>::epsilon()*(x.size()+std::abs(upper)+extra);
        if(std::isfinite(upper)&&upper+margin<incumbent){closed=true;break;}
      }
    }
    for(int it=0;!closed&&it<solver_budget(maxit);++it,hi*=2){
      if(!work.charge())return stopped();
      const double upper=-static_cast<double>(x.size())*(std::log(2.)+std::log(hi)+std::log(sigma));
      const double margin=256*std::numeric_limits<double>::epsilon()*(x.size()+std::abs(upper));
      if(upper+margin<incumbent){closed=true;break;}
      if(!std::isfinite(hi))return stop(BhattStatus::NumericalFailure);
    }
    if(!closed)return stopped();
    CertRoot left,right;
    if(!profile(lo,left)||!profile(hi,right)||!push(lo,hi,left.ll_upper,right.ll_upper))return stopped();
    while(!queue.empty()){
      const auto segment=queue.top();queue.pop();last_upper=segment.upper;
      if(segment.upper<incumbent||covered(segment.lo,segment.hi))continue;
      if(!work.charge()){queue.push(segment);return stopped();}
      double middle=segment.lo+(segment.hi-segment.lo)/2;
      for(const auto& w:winners){if(w.lo>segment.lo&&w.lo<segment.hi){middle=w.lo;break;}if(w.hi>segment.lo&&w.hi<segment.hi){middle=w.hi;break;}}
      if(!(middle>segment.lo&&middle<segment.hi)){queue.push(segment);return stop(BhattStatus::NumericalFailure);}
      if(!profile(middle,point)){queue.push(segment);return stopped();}
      // A full stationary proposal is useful only while this profile point
      // can improve the incumbent. With no enclosed winner yet, repeatedly
      // solving from already dominated points adds no acceptance evidence.
      if(point.ll_upper>=incumbent){if(!propose(middle,point)){queue.push(segment);return stopped();}}
      if(covered(segment.lo,segment.hi))continue;
      if(!push(segment.lo,middle,segment.left,point.ll_upper)||!push(middle,segment.hi,point.ll_upper,segment.right)){queue.push(segment);return stopped();}
    }
    std::vector<std::size_t> surviving;
    for(std::size_t i=0;i<winners.size();++i)if(winners[i].root.ll_upper>=incumbent)surviving.push_back(i);
    if(normal_upper>=incumbent)return stop(BhattStatus::Unresolved);
    if(cert_valid(uniform)&&uniform.loglik_upper>=incumbent){
      if(!surviving.empty())return stop(BhattStatus::Unresolved);
      if(uniform.loglik_lower<incumbent||normal_upper>=uniform.loglik_lower)return stop(BhattStatus::Unresolved);
      const double midpoint=uniform.coef[0]/2+uniform.coef[1]/2,radius=uniform.coef[1]/2-uniform.coef[0]/2;
      BhattResult out={midpoint,0,radius,(uniform.loglik_lower/2+uniform.loglik_upper/2),static_cast<int>(work.used),BhattStatus::UniformLimit};
      out.stop_detail=limit_detail(uniform.coef,out.logLik,StopModel::Unif);return out;
    }
    if(surviving.size()>1)return stop(BhattStatus::Tie);
    if(surviving.empty())return stop(BhattStatus::NumericalFailure);
    return cert_result(model,winners[surviving.front()].root,static_cast<int>(work.used));
  }
};
} // namespace bhatt_detail

namespace bhatt_detail {
inline BhattResult certificate_mle(span x,double tol=1e-8,int maxit=500,double sigma=special::kNaN,double a=special::kNaN,double mu=special::kNaN) {
  const int budget=static_cast<int>(solver_budget(maxit));
  BhattResult bad={NAN,NAN,NAN,NAN,0,BhattStatus::InvalidInput};
  if(x.empty()||!(tol>0&&std::isfinite(tol))||budget<1||(!std::isnan(mu)&&!std::isfinite(mu))||(!std::isnan(sigma)&&!(sigma>0&&std::isfinite(sigma)))||(!std::isnan(a)&&!(a>=0&&std::isfinite(a))))return bad;
  for(double v:x)if(!std::isfinite(v))return bad;
  if(std::isnan(sigma)&&std::isnan(a)&&std::all_of(x.begin(),x.end(),[&](double v){return v==(std::isnan(mu)?x[0]:mu);})){
    bad.status=BhattStatus::Unbounded;return bad;
  }
  if(!std::isnan(mu)&&!std::isnan(sigma)&&!std::isnan(a)){
    const auto witness=bhatt_detail::cert_witness(x,mu,sigma,a);
    if(!bhatt_detail::cert_valid(witness))return bhatt_detail::cert_stop(BhattStatus::NumericalFailure,{witness});
    return {mu,sigma,a,(witness.loglik_lower/2+witness.loglik_upper/2),0,BhattStatus::Ok};
  }
  if(a==0)return bhatt_detail::normal_member_certificate(x,mu,sigma,tol);
  if(std::isnan(a)&&!std::isnan(sigma)){
    // For |(x-mu)/sigma|<=1 the normal member dominates every width.
    // Decide this in original coordinates: rounding a ratio to one can
    // otherwise hide a genuinely positive optimal width of order sqrt(eps).
    int comparison=-1;
    if(std::isnan(mu))comparison=bhatt_detail::cert_difference_compare(*std::max_element(x.begin(),x.end()),*std::min_element(x.begin(),x.end()),sigma);
    else for(double v:x)comparison=std::max(comparison,bhatt_detail::cert_distance_compare(v,mu,sigma));
    if(comparison<=0)return bhatt_detail::normal_member_certificate(x,mu,sigma,tol);
    // The truncated-normal variance domination argument in the dossier
    // proves the normal winner whenever the empirical RMS is at most sigma.
    // For free location, SSE at the rounded mean bounds the minimum SSE
    // from above. Require separation from the practical summation margin.
    const double normal_center=std::isnan(mu)?norm_detail::moments(x).mean:mu;
    CertSum square;bool bounded=std::isfinite(normal_center);
    if(bounded)for(double v:x){
      const CPPUNIV_EXTENDED_REAL z=(static_cast<CPPUNIV_EXTENDED_REAL>(v)-normal_center)/sigma;
      const CPPUNIV_EXTENDED_REAL term=z*z;
      if(!std::isfinite(term)){bounded=false;break;}
      square.add(term);
    }
    const CPPUNIV_EXTENDED_REAL n=static_cast<CPPUNIV_EXTENDED_REAL>(x.size());
    const CPPUNIV_EXTENDED_REAL margin=256*std::numeric_limits<double>::epsilon()*(n+std::abs(square.value));
    if(bounded&&std::isfinite(square.value)&&square.value+margin<n)
      return bhatt_detail::normal_member_certificate(x,mu,sigma,tol);
  }
  if(std::isnan(sigma)&&!std::isnan(a)){
    int comparison=-1;
    if(std::isnan(mu))comparison=bhatt_detail::cert_range_compare(*std::min_element(x.begin(),x.end()),*std::max_element(x.begin(),x.end()),a);
    else for(double v:x)comparison=std::max(comparison,bhatt_detail::cert_distance_compare(v,mu,a));
    if(comparison<=0)return bhatt_detail::held_width_face(x,mu,a,comparison,tol);
    if(comparison==2)return bhatt_detail::cert_stop(BhattStatus::NumericalFailure,{bhatt_detail::cert_witness(x,*std::min_element(x.begin(),x.end())/2+*std::max_element(x.begin(),x.end())/2,a,a)});
  }
  const double xmin=*std::min_element(x.begin(),x.end()),xmax=*std::max_element(x.begin(),x.end());
  const double center=xmin/2+xmax/2;
  double scale=std::abs(xmin/2-xmax/2);
  if(!std::isnan(sigma))scale=std::max(scale,sigma);
  if(!std::isnan(a))scale=std::max(scale,a);
  if(!std::isnan(mu))scale=std::max(scale,static_cast<double>(std::abs(static_cast<CPPUNIV_EXTENDED_REAL>(mu)-center)));
  if(!(scale>0&&std::isfinite(scale)))return bhatt_detail::cert_stop(BhattStatus::NumericalFailure,{bhatt_detail::cert_witness(x,std::isnan(mu)?x[0]:mu,std::isnan(sigma)?std::numeric_limits<double>::min():sigma,std::isnan(a)?0:a)});
  std::vector<double> y; y.reserve(x.size());
  for(double v:x)y.push_back(static_cast<double>((static_cast<CPPUNIV_EXTENDED_REAL>(v)-center)/scale));
  const double m=std::isnan(mu)?mu:static_cast<double>((static_cast<CPPUNIV_EXTENDED_REAL>(mu)-center)/scale);
  const double s=std::isnan(sigma)?sigma:sigma/scale,w=std::isnan(a)?a:a/scale;
  BhattResult out;
  if(std::isnan(a)){bhatt_detail::WidthCertificate certificate({y.data(),y.size()},tol,budget,m,s,center,scale);out=certificate.run();}
  else out=bhatt_detail::held_width_certificate({y.data(),y.size()},tol,budget,m,s,w,center,scale);
  const double shift=x.size()*std::log(scale);
  if(out.status==BhattStatus::UniformLimit){
    out.mu=std::isnan(mu)?center+scale*out.mu:mu;out.a*=scale;out.logLik-=shift;
    // Unrestricted uniform endpoints are the original extrema. Recovering
    // them through a rounded centre/half-width can change their span.
    const std::vector<double> endpoints=std::isnan(mu)&&std::isnan(a)?std::vector<double>{xmin,xmax}:std::vector<double>{out.mu-out.a,out.mu+out.a};
    out.stop_detail=limit_detail(endpoints,out.logLik,StopModel::Unif);
    if(!bhatt_detail::uniform_representation_valid(out,tol))return bhatt_detail::cert_stop(BhattStatus::NumericalFailure,{bhatt_detail::cert_witness(x,std::isnan(mu)?x[0]:mu,scale,0)});
    const double margin=1024*std::numeric_limits<double>::epsilon()*(x.size()+std::abs(out.logLik));
    out.stop_detail.candidates.front().loglik_lower-=margin;out.stop_detail.candidates.front().loglik_upper+=margin;
  }else if(out.status==BhattStatus::Ok){
    const auto normalized=bhatt_detail::cert_witness({y.data(),y.size()},out.mu,out.sigma,out.a);
    out.mu=std::isnan(mu)?std::fma(scale,out.mu,center):mu;
    out.sigma=std::isnan(sigma)?scale*out.sigma:sigma;out.a=std::isnan(a)?scale*out.a:a;
    const auto witness=bhatt_detail::cert_witness(x,out.mu,out.sigma,out.a);
    const double margin=128*std::numeric_limits<double>::epsilon()*(x.size()+std::abs(shift));
    if(!bhatt_detail::cert_valid(witness)||!bhatt_detail::cert_valid(normalized)||
        witness.loglik_lower>normalized.loglik_upper-shift+margin||
        witness.loglik_upper<normalized.loglik_lower-shift-margin)
      return bhatt_detail::cert_stop(BhattStatus::NumericalFailure,{witness});
    out.logLik=(witness.loglik_lower/2+witness.loglik_upper/2);
  }else{
    for(auto& c:out.stop_detail.candidates){
      if(c.model==StopModel::None&&c.coef.size()==3){
        const double cm=std::isnan(mu)?std::fma(scale,c.coef[0],center):mu,cs=std::isnan(sigma)?scale*c.coef[1]:sigma,ca=std::isnan(a)?scale*c.coef[2]:a;
        c=bhatt_detail::cert_witness(x,cm,cs,ca);
      }else{
        if(c.model==StopModel::Unif&&c.coef.size()==2){for(double& v:c.coef)v=std::fma(scale,v,center);}
        c.loglik_lower-=shift;c.loglik_upper-=shift;
      }
    }
    auto& candidates=out.stop_detail.candidates;
    candidates.erase(std::remove_if(candidates.begin(),candidates.end(),[](const FitCandidate& c){return !bhatt_detail::cert_valid(c);}),candidates.end());
    out.stop_detail.loglik_lower=NAN;
    for(const auto& c:candidates)out.stop_detail.loglik_lower=std::isnan(out.stop_detail.loglik_lower)?c.loglik_lower:std::max(out.stop_detail.loglik_lower,c.loglik_lower);
    if(std::isfinite(out.stop_detail.loglik_upper))out.stop_detail.loglik_upper-=shift;
    if(std::isfinite(out.stop_detail.segment_loglik_upper))out.stop_detail.segment_loglik_upper-=shift;
  }
  return out;
}
} // namespace bhatt_detail

inline BhattResult bhatt_mle(span x,double tol=1e-8,int maxit=500,double sigma=special::kNaN,double a=special::kNaN,double mu=special::kNaN) {
  auto certificate=bhatt_detail::certificate_mle(x,tol,maxit,sigma,a,mu);
  if(certificate.status==BhattStatus::Ok||certificate.status==BhattStatus::UniformLimit||
      certificate.status==BhattStatus::NormalLimit||certificate.status==BhattStatus::Unbounded){
    certificate.certified=true;return certificate;
  }
  if(certificate.status==BhattStatus::InvalidInput)return certificate;
  // Inconclusive certification must preserve a successful existing search.
  // The final local refinement supplies coefficient accuracy; certified=false
  // withdraws only the global claim.
  auto searched=bhatt_detail::accurate_candidate(x,tol,maxit,sigma,a,mu);
  if(a==0 && searched.status==BhattStatus::Ok) {
    // A normal coefficient inclusion can fail solely because tol is below
    // its practical margin. Preserve the closed-form search in that case,
    // but reject a physically displaced mean or scale (notably large offsets).
    bhatt_detail::CertSum location,scale_score,magnitude;
    for(double v:x) {
      const CPPUNIV_EXTENDED_REAL z=(static_cast<CPPUNIV_EXTENDED_REAL>(v)-searched.mu)/searched.sigma;
      location.add(z);scale_score.add(z*z-1);magnitude.add(std::abs(z)+z*z);
    }
    const CPPUNIV_EXTENDED_REAL margin=256*std::numeric_limits<double>::epsilon()*(x.size()+magnitude.value);
    const CPPUNIV_EXTENDED_REAL score=std::hypot(std::isnan(mu)?location.value:0,
        std::isnan(sigma)?scale_score.value:0);
    if(!std::isfinite(score) || !std::isfinite(margin) || score>1e-3+margin)return certificate;
  }
  const auto reject_search=[&]() {
    certificate.status=BhattStatus::NumericalFailure;
    certificate.stop_detail.budget_exhausted=false;
    certificate.stop_detail.comparison_tie=false;
    return certificate;
  };
  if(searched.status==BhattStatus::Ok){
    const auto witness=bhatt_detail::cert_witness(x,searched.mu,searched.sigma,searched.a);
    if(!bhatt_detail::cert_valid(witness))return certificate;
    // The existing search worked in normalized coordinates. An inconsistent
    // recovered likelihood proves its physical result wrong; uncertified does
    // not permit reporting a likelihood at different coefficients.
    if(!std::isfinite(searched.logLik)||searched.logLik<witness.loglik_lower||searched.logLik>witness.loglik_upper)return reject_search();
    // For a repeated observation with held centre and sigma, the a^2
    // coefficient is positive exactly when |x-mu|>sigma. Error-free distance
    // comparison can prove this even when ordinary subtraction rounds to sigma.
    if(searched.a==0&&!std::isnan(mu)&&!std::isnan(sigma)&&
        std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];})&&
        bhatt_detail::cert_distance_compare(x[0],mu,sigma)>0)return certificate;
    // A higher finite or reachable-face lower bound refutes this result even
    // when the outer certificate itself has not closed.
    if(certificate.stop_detail.loglik_lower>witness.loglik_upper)return certificate;
    return searched;
  }
  if(searched.status==BhattStatus::UniformLimit&&std::isnan(mu)&&std::isnan(a))
    searched.stop_detail=limit_detail({*std::min_element(x.begin(),x.end()),*std::max_element(x.begin(),x.end())},searched.logLik,StopModel::Unif);
  if(searched.status==BhattStatus::UniformLimit&&
      valid_stop_coefficients(StopModel::Unif,searched.stop_detail.limit_coef)&&std::isfinite(searched.logLik)){
    if(!bhatt_detail::uniform_representation_valid(searched,tol))return reject_search();
    const double margin=1024*std::numeric_limits<double>::epsilon()*(x.size()+std::abs(searched.logLik));
    if(certificate.stop_detail.loglik_lower>searched.logLik+margin)return certificate;
    // A numerical face comparison is not a proof of a limit win. Retain an
    // independently converged finite search instead of emitting that stop.
    auto finite=bhatt_detail::accurate_candidate(x,tol,maxit,sigma,a,mu,true);
    if(finite.status!=BhattStatus::Ok)return certificate;
    const auto witness=bhatt_detail::cert_witness(x,finite.mu,finite.sigma,finite.a);
    if(!bhatt_detail::cert_valid(witness) || !std::isfinite(finite.logLik) ||
        finite.logLik<witness.loglik_lower || finite.logLik>witness.loglik_upper ||
        certificate.stop_detail.loglik_lower>witness.loglik_upper)return certificate;
    return finite;
  }
  if(searched.status==BhattStatus::NumericalFailure)return reject_search();
  if(searched.status==BhattStatus::IterationFailure){searched.stop_detail=certificate.stop_detail;return searched;}
  // The search itself failed, or proposed an invalid/unbounded result not
  // proved by the certificate. Preserve the certificate's physical witnesses.
  return certificate;
}
} // namespace cppunivariate
