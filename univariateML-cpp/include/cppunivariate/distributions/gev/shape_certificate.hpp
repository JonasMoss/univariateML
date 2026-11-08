#pragma once
// Included after GevResult and gev_loglik. Exact-arithmetic interval bounds;
// comparisons allow a practical floating-point margin, not directed rounding.
namespace cppunivariate {
namespace gev_shape_detail {
struct Interval { double lo, hi; };
inline Interval add(Interval a, Interval b) { return {a.lo+b.lo,a.hi+b.hi}; }
inline Interval neg(Interval a) { return {-a.hi,-a.lo}; }
inline Interval mul(Interval a, Interval b) {
  return {std::min({a.lo*b.lo,a.lo*b.hi,a.hi*b.lo,a.hi*b.hi}),
    std::max({a.lo*b.lo,a.lo*b.hi,a.hi*b.lo,a.hi*b.hi})};
}
inline Interval reciprocal(Interval a) { return {1/a.hi,1/a.lo}; }
inline Interval scale(Interval a,double b) { return mul(a,{b,b}); }
struct Jet { Interval v,d,dd; };
inline Jet constant(double v) { return {{v,v},{0,0},{0,0}}; }
inline Jet add(Jet a,Jet b) { return {add(a.v,b.v),add(a.d,b.d),add(a.dd,b.dd)}; }
inline Jet neg(Jet a) { return {neg(a.v),neg(a.d),neg(a.dd)}; }
inline Jet mul(Jet a,Jet b) {
  return {mul(a.v,b.v),add(mul(a.d,b.v),mul(a.v,b.d)),
    add(add(mul(a.dd,b.v),scale(mul(a.d,b.d),2)),mul(a.v,b.dd))};
}
inline Jet scale(Jet a,double b) { return mul(a,constant(b)); }
inline Jet logarithm(Jet a) {
  const auto r=reciprocal(a.v);
  return {{std::log(a.v.lo),std::log(a.v.hi)},mul(a.d,r),
    add(mul(a.dd,r),neg(mul(mul(a.d,a.d),mul(r,r))))};
}
inline Jet inverse(Jet a) {
  const auto r=reciprocal(a.v),r2=mul(r,r);
  return {r,neg(mul(a.d,r2)),add(scale(mul(mul(a.d,a.d),mul(r2,r)),2),neg(mul(a.dd,r2)))};
}
inline Jet exponential(Jet a) {
  const Interval e{std::exp(a.v.lo),std::exp(a.v.hi)};
  return {e,mul(e,a.d),mul(e,add(a.dd,mul(a.d,a.d)))};
}
inline bool finite(Jet a) {
  return std::isfinite(a.v.lo)&&std::isfinite(a.v.hi)&&std::isfinite(a.d.lo)&&
    std::isfinite(a.d.hi)&&std::isfinite(a.dd.lo)&&std::isfinite(a.dd.hi);
}
// log(1+xi*z)/xi, including the removable singularity at xi=0.
inline Jet ratio(Jet xi,double z,int maxit,bool& ok,bool& exhausted) {
  const Jet q=scale(xi,z);
  const double r=std::max(std::abs(q.v.lo),std::abs(q.v.hi));
  if((xi.v.lo>0 || xi.v.hi<0) && r>.5)
    return mul(logarithm(add(constant(1),q)),inverse(xi));
  Jet sum=constant(z),power=constant(1);
  if(z==0)return sum;
  for(int j=1;j<=maxit;++j) {
    power=mul(power,neg(q));sum=add(sum,scale(power,z/(j+1)));
    const double e0=std::abs(z)*std::pow(r,j+1)/((j+2)*(1-r));
    const double e1=z*z*std::pow(r,j)/(1-r);
    const double e2=2*std::abs(z*z*z)*(j+1)*std::pow(r,j-1)/std::pow(1-r,3);
    if(std::max({e0,e1,e2})<2*std::numeric_limits<double>::epsilon()) {
      sum.v=add(sum.v,{-e0,e0});sum.d=add(sum.d,{-e1,e1});sum.dd=add(sum.dd,{-e2,e2});
      return sum;
    }
  }
  exhausted=true;
  ok=false;return sum;
}
inline Jet likelihood(span z,double lo,double hi,int maxit,bool& ok,bool& exhausted) {
  Jet sum=constant(0);const Jet xi{{lo,hi},{1,1},{0,0}};
  for(double v:z) {
    const auto L=ratio(xi,v,maxit,ok,exhausted);
    sum=add(sum,neg(add(mul(add(constant(1),xi),L),exponential(neg(L)))));
  }
  if(!finite(sum))ok=false;
  return sum;
}
inline Jet likelihood(span z,double lo,double hi,int maxit,bool& ok) {
  bool exhausted=false;return likelihood(z,lo,hi,maxit,ok,exhausted);
}
} // namespace gev_shape_detail
// Shape-only certificate. Open support edges are excluded only by explicit
// likelihood envelopes; every remaining segment is covered by derivative bounds.
inline GevResult gev_shape_certificate(span x,double mu,double sigma,double tol,int maxit) {
  bool solver_exhausted=false;
  std::vector<double> z;for(double v:x)z.push_back((v-mu)/sigma);
  const span data{z.data(),z.size()};
  GevResult out{mu,sigma,0,gev_loglik(x,mu,sigma,0),0,GevStatus::NumericalFailure};
  out.stop_detail.loglik_upper=INFINITY;
  if(std::isfinite(out.logLik)) {
    out.stop_detail.loglik_lower=out.logLik;
    out.stop_detail.candidates.push_back({{mu,sigma,0},out.logLik,out.logLik});
  }
  const double jac=x.size()*std::log(sigma);
  const double margin=4096*std::numeric_limits<double>::epsilon()*x.size();
  struct Box { double lo,hi; };
  struct Candidate { double lo,hi,ll,upper; };
  std::vector<Box> pending;
  std::vector<Candidate> candidates;
  auto keep=[&](double lo,double hi) {
    const double mid=lo+(hi-lo)/2,ll=gev_loglik(data,0,1,mid);
    bool ok=true;const auto bounds=gev_shape_detail::likelihood(data,lo,hi,maxit,ok,solver_exhausted);
    if(!ok)return false;
    candidates.push_back({lo,hi,ll,bounds.v.hi});return true;
  };
  bool all_zero=true;for(double v:x)all_zero=all_zero && v==mu;
  if(all_zero) {out.status=GevStatus::Ok;out.stop_detail={};return out;} // every shape maximizes
  bool at_reverse_endpoint=true;
  for(double v:z)at_reverse_endpoint=at_reverse_endpoint && v==1;
  if(at_reverse_endpoint) {
    // h_xi(z)<=1 uniformly on the fitted branch, with equality at
    // xi=-1,z=1. This also avoids differentiating log(0) at that endpoint.
    out.xi=-1;out.logLik=gev_loglik(x,mu,sigma,-1);
    if(std::isfinite(out.logLik) && std::abs(out.logLik+jac)<=1e-7*x.size()) {
      out.status=GevStatus::Ok;out.stop_detail={};
    }
    return out;
  }
  std::int64_t evaluations=0;
  const auto cap=certificate_budget(maxit);
  double lower=-1,upper=1,maxabs=0;
  double left_z=0,right_z=0;
  for(double v:z) {
    if(!std::isfinite(v))return out;
    maxabs=std::max(maxabs,std::abs(v));
    if(v>=1 && -1/v>=lower) {lower=-1/v;left_z=v;}
    if(v<=-1 && -1/v<=upper) {upper=-1/v;right_z=v;}
  }
  const double middle=0;
  if(!keep(middle,middle)) {if(solver_exhausted)out.status=GevStatus::IterationFailure;return out;}
  if(left_z==0 && !keep(lower,lower)) {if(solver_exhausted)out.status=GevStatus::IterationFailure;return out;}
  if(right_z==0 && !keep(upper,upper)) {if(solver_exhausted)out.status=GevStatus::IterationFailure;return out;}
  double incumbent=-INFINITY;for(const auto& c:candidates)incumbent=std::max(incumbent,c.ll);
  // For c=1+xi>=0, c*log(y)-y <= c*log(c)-c, y>0.
  // This convex function of c attains its box maximum at an endpoint.
  auto universal=[](double a,double b) {
    auto f=[](double c){return c==0 ? 0 : c*(std::log(c)-1);};
    return std::max(f(1+a),f(1+b));
  };
  auto tail=[&](double edge,double inner,double extreme,bool left) -> double {
    const double a=left ? edge : inner,b=left ? inner : edge;
    const double t=1+inner*extreme;
    if(!(t>0 && t<1))return INFINITY;
    double term=INFINITY;
    if(left) {
      // xi<0: -(1+1/xi)*log(t)-t^(-1/xi).
      const double c=-(1+1/edge);
      term=c*std::log(t); // drop the nonpositive power term
    } else {
      const double logy=-std::log(t)/edge,c=1+edge;
      if(logy>std::log(c))term=c*logy-std::exp(logy);
    }
    return term+(x.size()-1)*universal(a,b);
  };
  bool covered=true,budget_exhausted=false;
  if(left_z!=0) {
    double inner=lower/2;bool excluded=false,stalled=false;
    while(evaluations<cap) {
      ++evaluations;
      if(tail(lower,inner,left_z,true)<incumbent-margin) {excluded=true;break;}
      const double next=lower+(inner-lower)/2;
      if(next==lower || next==inner) {stalled=true;break;}
      inner=next;
    }
    if(excluded)lower=inner;else {covered=false;budget_exhausted=!stalled && evaluations>=cap;}
  }
  if(covered && right_z!=0) {
    double inner=upper/2;bool excluded=false,stalled=false;
    while(evaluations<cap) {
      ++evaluations;
      if(tail(upper,inner,right_z,false)<incumbent-margin) {excluded=true;break;}
      const double next=upper+(inner-upper)/2;
      if(next==upper || next==inner) {stalled=true;break;}
      inner=next;
    }
    if(excluded)upper=inner;else {covered=false;budget_exhausted=!stalled && evaluations>=cap;}
  }
  // The central box uses a uniformly convergent series; the outer boxes
  // divide by xi only where its interval is separated from zero.
  const double radius=std::min(.5,.25/maxabs);
  if(covered) {
    const double a=std::max(lower,-radius),b=std::min(upper,radius);
    pending.push_back({a,b});
    if(lower<a)pending.push_back({lower,a});
    if(b<upper)pending.push_back({b,upper});
  }
  while(!pending.empty() && evaluations<cap) {
    const Box b=pending.back();pending.pop_back();++evaluations;
    bool ok=true;
    const auto e=gev_shape_detail::likelihood(data,b.lo,b.hi,maxit,ok,solver_exhausted);
    if(!ok) {covered=false;break;}
    double incumbent=-INFINITY;for(const auto& c:candidates)incumbent=std::max(incumbent,c.ll);
    if(e.v.hi<incumbent-margin || e.d.lo>margin || e.d.hi< -margin)continue;
    if(e.dd.hi< -margin) {
      bool endpoints_ok=true;
      const auto l=gev_shape_detail::likelihood(data,b.lo,b.lo,maxit,endpoints_ok,solver_exhausted);
      const auto h=gev_shape_detail::likelihood(data,b.hi,b.hi,maxit,endpoints_ok,solver_exhausted);
      if(!endpoints_ok) {covered=false;break;}
      if(l.d.hi< -margin || h.d.lo>margin)continue;
      if(l.d.lo>margin && h.d.hi< -margin) {
        double lo=b.lo,hi=b.hi;bool solved=false;
        int root_steps=0;
        for(;root_steps<maxit;++root_steps) {
          const double mid=lo+(hi-lo)/2;
          bool point_ok=true;const auto m=gev_shape_detail::likelihood(data,mid,mid,maxit,point_ok,solver_exhausted);
          if(!point_ok)break;
          // Derivative uncertainty gives a root enclosure by strict curvature.
          const double error=std::max(std::abs(m.d.lo),std::abs(m.d.hi))+margin;
          const double radius=(error-margin+32*std::numeric_limits<double>::epsilon()*x.size())/(-e.dd.hi);
          if(2*radius<=tol*std::abs(mid)) {lo=std::max(lo,mid-radius);hi=std::min(hi,mid+radius);solved=true;break;}
          if(m.d.lo>margin)lo=mid;
          else if(m.d.hi< -margin)hi=mid;
          else break;
          if(hi-lo<=tol*std::abs(lo+(hi-lo)/2)) {solved=true;break;}
        }
        if(!solved && root_steps>=maxit)solver_exhausted=true;
        if(!solved || !keep(lo,hi)) {covered=false;break;}
        continue;
      }
    }
    const double mid=b.lo+(b.hi-b.lo)/2;
    if(mid==b.lo || mid==b.hi) {covered=false;break;}
    pending.push_back({mid,b.hi});pending.push_back({b.lo,mid});
  }
  if(covered && !pending.empty() && evaluations>=cap)budget_exhausted=true;
  if(covered && pending.empty() && !candidates.empty()) {
    auto winner=candidates.begin();
    for(auto it=candidates.begin();it!=candidates.end();++it)
      if(it->ll>winner->ll || (it->hi>it->lo && it->ll>=winner->ll-margin &&
          winner->hi>=it->lo && winner->lo<=it->hi))winner=it;
    bool separated=true;
    for(const auto& c:candidates) {
      if(&c==&*winner)continue;
      // Duplicate enclosures touching the same stationary point may coalesce.
      if(c.hi>=winner->lo && c.lo<=winner->hi)continue;
      if(c.upper>=winner->ll-margin) {separated=false;out.stop_detail.comparison_tie=true;}
    }
    if(separated) {
      bool point_ok=true;
      const double shape=winner->lo+(winner->hi-winner->lo)/2;
      const auto point=gev_shape_detail::likelihood(data,shape,shape,maxit,point_ok,solver_exhausted);
      // A point incumbent used for pruning must itself be a domain endpoint
      // or lie in an isolated stationary enclosure before it can be returned.
      if(winner->lo==winner->hi && shape!= -1 && shape!=1 &&
         !(point_ok && point.d.lo==0 && point.d.hi==0))separated=false;
    }
    if(separated) {
      out.xi=winner->lo+(winner->hi-winner->lo)/2;
      out.logLik=gev_loglik(x,mu,sigma,out.xi);
      if(std::isfinite(out.logLik) && std::abs(out.logLik-(winner->ll-jac))<=1e-7*x.size())out.status=GevStatus::Ok;
    }
  }
  out.iterations=static_cast<int>(evaluations);
  if(out.status==GevStatus::Ok)out.stop_detail={};
  if(out.status!=GevStatus::Ok) {
    if(solver_exhausted)out.status=GevStatus::IterationFailure;
    out.stop_detail.loglik_upper=INFINITY;
    out.stop_detail.budget_exhausted=budget_exhausted;
    out.stop_detail.candidates.clear();out.stop_detail.loglik_lower=NAN;
    for(const auto& c:candidates) {
      const double shape=c.lo+(c.hi-c.lo)/2,ll=gev_loglik(x,mu,sigma,shape);
      if(std::isfinite(ll)) {
        out.stop_detail.candidates.push_back({{mu,sigma,shape},ll,ll});
        out.stop_detail.loglik_lower=std::isnan(out.stop_detail.loglik_lower)?ll:std::max(out.stop_detail.loglik_lower,ll);
      }
    }
  }
  return out;
}
} // namespace cppunivariate
