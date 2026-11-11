// Shape-only Student certificates with location and physical scale held.
// std's physical scale is sd, hence a^2=(nu-2)*sd^2. See the job derivation.
#pragma once
#include "certificate.hpp"
#include "../../special/special.hpp"
namespace cppunivariate {
namespace t_shape_detail {
struct Result {
  double shape=NAN,loglik=NAN; int evaluations=0;
  // 0 finite, 1 normal supremum, 2 budget, 3 tie, 4 numerical, 5 collapse.
  int status=4; StopDetail detail{};
};
// Extend the existing normalizer enclosures below .5 by one exact recurrence.
inline t_cert_detail::I gamma(double v,int order) {
  if(!(v>0)||!std::isfinite(v))return t_cert_detail::bad();
  if(v>=.5)return t_cert_detail::gamma(v,order);
  const t_cert_detail::I q(v);
  if(order==0)return t_cert_detail::gamma(v+2,0)-
    (t_cert_detail::log1p(t_cert_detail::I(1)/q)-t_cert_detail::log1p(t_cert_detail::I(2)/q)*.5);
  if(order==3)return t_cert_detail::gamma(v+2,3)+t_cert_detail::I(1)/(q*(q+1)*(q+2));
  return t_cert_detail::gamma(v+2,4)+(q*q*3+q*6+2)/(t_cert_detail::sq(q)*t_cert_detail::sq(q+1)*t_cert_detail::sq(q+2));
}
inline t_cert_detail::I derivative(t_cert_detail::I v,int order) {
  if(!v.valid()||v.lo<=0)return t_cert_detail::bad();
  return {gamma(v.hi,order).lo,gamma(v.lo,order).hi};
}
struct Engine {
  std::vector<t_cert_detail::I> q;
  double mu,scale; bool standardized; int maxit; Result out;
  t_cert_detail::I normal,constant;
  Engine(double m,double s,bool z,int it):mu(m),scale(s),standardized(z),maxit(it){}
  bool charge(){if(out.evaluations>=certificate_budget(maxit)){out.status=2;out.detail.budget_exhausted=true;return false;}++out.evaluations;return true;}
  t_cert_detail::I value(t_cert_detail::I v) const {
    const auto r=standardized?v-2:v;
    if(!r.valid()||r.lo<=0)return t_cert_detail::bad();
    auto c=t_cert_detail::I(gamma(v.lo,0).lo,gamma(v.hi,0).hi);
    bool normal_seam=v.lo>=32;
    if(normal_seam)for(auto z:q)if(z.hi/r.lo>.01){normal_seam=false;break;}
    if(normal_seam) {
      // Move the factor v inside each summand before reduction. Otherwise
      // Sum's absolute rounding margin is amplified by v at the normal face.
      t_cert_detail::Sum squares,relative;
      for(auto z:q) {
        const auto u=z/r;squares.add(z);
        relative.add((v+1)*t_cert_detail::lr(u)*.5-u*(standardized?1.5:.5));
      }
      if(standardized)c=c+t_cert_detail::log1p(t_cert_detail::I(2)/r)*.5;
      return constant-squares.get()*.5+c*q.size()+relative.get();
    }
    t_cert_detail::Sum sum;
    for(auto z:q)sum.add(t_cert_detail::log1p(z/r));
    if(standardized)c=c+(t_cert_detail::log(v)-t_cert_detail::log(r))*.5;
    return constant+c*q.size()-(v+1)*sum.get()*.5;
  }

  // Derivatives with respect to v, with actual held sd when standardized.
  t_cert_detail::I score(t_cert_detail::I v,bool curvature) const {
    const auto r=standardized?v-2:v;
    if(!r.valid()||r.lo<=0)return t_cert_detail::bad();
    auto c=curvature?-t_shape_detail::derivative(v,4):t_shape_detail::derivative(v,3);
    if(standardized)c=c+(curvature?(t_cert_detail::I(1)/v+t_cert_detail::I(1)/r)/(v*r):
      -t_cert_detail::I(1)/(v*r));
    t_cert_detail::Sum sum;
    for(auto z:q){
      auto u=z/r,den=t_cert_detail::I(1)+u;den.lo=std::max(1.,den.lo);
      if(curvature)sum.add(z*(standardized?z-6-u*3:z-2-u)/(r*r*r*2)/den/den);
      else sum.add(-t_cert_detail::shape_remainder(u)*.5+z/(r*r)/den*(standardized?1.5:.5));
    }
    return c*q.size()+sum.get();
  }
  void witness(double v){
    auto ll=value(v);if(!ll.valid())return;
    if(ll.lo>out.detail.loglik_lower){
      out.detail.loglik_lower=ll.lo;out.detail.candidates={{{mu,scale,v},ll.lo,ll.hi,standardized?StopModel::None:StopModel::Lst}};
    }
  }
  // Entire infinite tail in inverse shape h (ordinary) or w=1/(v-2).
  // The signed bound proves a normal supremum on that tail. The unsigned
  // bound excludes it only when a finite witness strictly beats it.
  bool tail(double v,bool& signed_tail) const {
    const double h=1/(standardized?v-2:v),n=q.size();
    t_cert_detail::Sum second,first;
    for(auto z:q){first.add(z);second.add(t_cert_detail::sq(z));}
    auto upper=standardized?t_cert_detail::I(n*h)+second.get()*(h*(1+3*h)/4):second.get()*(h*(1+h)/4);
    if(upper.valid()&&(normal+upper).hi<out.detail.loglik_lower){signed_tail=false;return true;}
    t_cert_detail::I slope;
    if(standardized){
      t_cert_detail::Sum terms;
      for(auto z:q)terms.add(-z*1.5/(t_cert_detail::I(1)+z*h)+t_cert_detail::sq(z)*.25);
      slope=t_cert_detail::I(n)*(t_cert_detail::I(1)-t_cert_detail::I(1)/(t_cert_detail::sq(t_cert_detail::I(1+2*h))*4)+h*h/8)+terms.get();
    }else{
      // dL/dh <= A + h*sum(q^2)/2 + n*h^2/8.
      slope=(second.get()-first.get()*2-n)*.25+second.get()*(h*.5)+n*h*h/8;
    }
    signed_tail=slope.valid()&&slope.hi<0;
    return signed_tail;
  }
};
// A diagnostic point is evaluated at its represented physical coefficients.
// This is a likelihood lower witness, never an optimizer acceptance gate.
inline void retain_point(span x,double mu,double scale,double shape,bool standardized,StopDetail& detail) {
  detail.loglik_upper=INFINITY;
  if(x.empty()||!std::isfinite(mu)||!(scale>0)||!std::isfinite(scale)||
     !std::isfinite(shape)||!(shape>(standardized?2.:0.)))return;
  Engine e(mu,scale,standardized,0);e.q.reserve(x.size());
  for(double v:x){
    if(!std::isfinite(v))return;
    auto q=t_cert_detail::sq((t_cert_detail::I(v)-mu)/scale);
    if(!q.valid())return;
    q.lo=std::max(0.,q.lo);e.q.push_back(q);
  }
  e.constant=t_cert_detail::I(-static_cast<double>(x.size()))*
    (.5*std::log(2*pi)+t_cert_detail::log(t_cert_detail::I(scale)));
  const auto ll=e.value(shape);
  if(ll.valid()&&(detail.candidates.empty()||!std::isfinite(detail.loglik_lower)||ll.lo>detail.loglik_lower)) {
    detail.candidates={{{mu,scale,shape},ll.lo,ll.hi,standardized?StopModel::None:StopModel::Lst}};
    detail.loglik_lower=ll.lo;
  }
}
inline void retain_candidates(const StopDetail& source,StopDetail& target) {
  target.loglik_upper=INFINITY;
  for(const auto& candidate:source.candidates) {
    if(!std::isfinite(candidate.loglik_lower)||!std::isfinite(candidate.loglik_upper)||
       candidate.loglik_lower>candidate.loglik_upper)continue;
    if(target.candidates.empty()||!std::isfinite(target.loglik_lower)||candidate.loglik_lower>target.loglik_lower) {
      target.candidates={candidate};target.loglik_lower=candidate.loglik_lower;
    }
  }
}
// A reachable normal-face point is a lower witness, without a claim that
// this face is the global winner. Strict domination can reject a numerical
// fallback while unresolved comparisons leave it available, uncertified.
inline StopDetail normal_point(span x,double mu,double scale) {
  StopDetail out;out.loglik_upper=INFINITY;
  if(x.empty()||!std::isfinite(mu)||!(scale>0)||!std::isfinite(scale))return out;
  t_cert_detail::Sum squares;
  for(double v:x){
    const auto q=t_cert_detail::sq((t_cert_detail::I(v)-mu)/scale);
    if(!q.valid())return out;
    squares.add(q);
  }
  const auto ll=t_cert_detail::I(-static_cast<double>(x.size()))*
    (.5*std::log(2*pi)+t_cert_detail::log(t_cert_detail::I(scale)))-squares.get()*.5;
  if(ll.valid()) {
    out.loglik_lower=ll.lo;
    out.candidates.push_back({{mu,scale},ll.lo,ll.hi,StopModel::Norm});
  }
  return out;
}
inline bool dominated(StopDetail& point,const StopDetail& alternative) {
  if(point.candidates.empty()||alternative.candidates.empty()||
     !(alternative.loglik_lower>point.candidates.front().loglik_upper))return false;
  point.candidates.insert(point.candidates.end(),alternative.candidates.begin(),alternative.candidates.end());
  point.loglik_lower=alternative.loglik_lower;point.loglik_upper=INFINITY;
  return true;
}
inline Result solve(span x,double mu,double scale,bool standardized,int maxit,double coefficient_tol=1e-7) {
  Engine e(mu,scale,standardized,static_cast<int>(solver_budget(maxit)));
  e.out.detail.loglik_upper=INFINITY;e.out.detail.loglik_lower=-INFINITY;
  if(x.empty()||!std::isfinite(mu)||!(scale>0)||!std::isfinite(scale)||e.maxit<1||
     !(coefficient_tol>0)||!std::isfinite(coefficient_tol))return e.out;
  const double accepted_width=std::min(1e-7,coefficient_tol);
  const double target_width=std::min(1e-10,accepted_width);
  t_cert_detail::Sum squares,logs;std::size_t k=0;
  double minq=INFINITY;
  for(double v:x){
    if(!std::isfinite(v))return e.out;
    auto z=t_cert_detail::sq((t_cert_detail::I(v)-mu)/scale);
    if(!z.valid())return e.out;
    z.lo=std::max(0.,z.lo);e.q.push_back(z);squares.add(z);
    if(v==mu)++k;
    else {if(!(z.lo>0))return e.out;logs.add(t_cert_detail::log(z));minq=std::min(minq,z.lo);}
  }
  const double n=x.size();
  e.constant=t_cert_detail::I(-n)*(.5*std::log(2*pi)+t_cert_detail::log(t_cert_detail::I(scale)));
  e.normal=e.constant-squares.get()*.5;
  if(!e.normal.valid())return e.out;
  // A bounded geometric grid nominates witnesses only. Every interval is
  // independently covered below, including gaps between these proposals.
  for(int j=0;j<e.maxit;++j){
    if(!e.charge())return e.out;
    double v=std::ldexp(1.,j/2)*(j%2?std::sqrt(2.):1.);
    if(standardized)v+=2;
    if(!std::isfinite(v)||v>1e12)break;
    e.witness(v);
    // A declining proposal ends nomination only; the full domain below
    // is still covered. It is never used as a bound on possible modes.
    if(j>1&&e.value(v).hi<e.out.detail.loglik_lower)break;
  }
  if(!std::isfinite(e.out.detail.loglik_lower))return e.out;
  double lower;t_cert_detail::I collapse;bool critical=false;
  if(standardized){
    // At held sd, v down to two is scale collapse. A strict positive
    // exponent gives a uniform upper bound C + (n-3k/2)*log(v-2).
    const double exponent=n-1.5*k;
    if(exponent<0)return e.out; // The public router reports unboundedness.
    critical=exponent==0;
    if(critical){
      collapse=e.constant+gamma(2,0)*n+n*.5*std::log(2.)-logs.get()*1.5;
      const auto D=t_cert_detail::I(5*n/16)-logs.get()*.5;
      const double h=std::min(1.,std::exp(2*(-D.hi-1)/(n-k)));
      lower=2+h;if(!(lower>2)||!collapse.valid())return e.out;
      const auto bound=D+t_cert_detail::log(t_cert_detail::I(lower)-2)*((n-k)*.5);
      if(!bound.valid()||!(bound.hi<0))return e.out;
    }else{
      const auto c=e.constant+n*.5*std::log(3.)-logs.get()*1.5;
      const double h=std::min({1.,minq,std::exp((e.out.detail.loglik_lower-c.hi-1)/exponent)});
      lower=2+h;
      if(!(h>0)||!(lower>2))return e.out;
      // Enclose the actual rounded cutoff, rather than the unrounded h.
      const auto bound=c+t_cert_detail::log(t_cert_detail::I(lower)-2)*exponent;
      if(!bound.valid()||!(bound.hi<e.out.detail.loglik_lower))return e.out;
    }
  }else{
    // Gamma((v+1)/2)/Gamma((v+2)/2) <= sqrt(pi), hence
    // f_v(x;mu,s) <= sqrt(v)/(2s), uniformly in the observation.
    const auto c=t_cert_detail::I(-n)*t_cert_detail::log(t_cert_detail::I(scale)*2);
    lower=std::min(.25,std::exp(2*(e.out.detail.loglik_lower-c.hi-1)/n));
    const auto bound=c+t_cert_detail::log(t_cert_detail::I(lower))*(n*.5);
    if(!(lower>0)||!bound.valid()||!(bound.hi<e.out.detail.loglik_lower))return e.out;
  }
  double upper=standardized?4:2;bool signed_tail=false,closed=false;
  for(int it=0;it<e.maxit;++it){
    if(!e.charge())return e.out;
    if(e.tail(upper,signed_tail)){closed=true;break;}
    upper*=2;if(!std::isfinite(upper)||upper>1e12)break; // Arithmetic refusal.
  }
  if(!closed)return e.out;
  std::vector<t_cert_detail::I> pending{{lower,upper}},roots,regions;
  while(!pending.empty()){
    if(!e.charge())return e.out;
    auto box=pending.back();pending.pop_back();bool covered=false;
    for(auto region:regions)if(box.lo>=region.lo&&box.hi<=region.hi)covered=true;
    if(covered)continue;
    const auto split=[&](){
      const double mid=std::sqrt(box.lo)*std::sqrt(box.hi);
      if(!(mid>box.lo&&mid<box.hi))return false;
      pending.push_back({box.lo,mid});pending.push_back({mid,box.hi});return true;
    };
    auto g=e.score(box,false);if(!g.valid()){if(!split())return e.out;continue;}
    if(g.lo>0||g.hi<0)continue;
    auto h=e.score(box,true);if(!h.valid()){if(!split())return e.out;continue;}
    if(h.lo>0)continue;
    if(h.hi<0){
      const double extra=1e-7*box.mid();
      t_cert_detail::I region(std::max(lower,box.lo-extra),std::min(upper,box.hi+extra));
      auto curvature=e.score(region,true),left=e.score(region.lo,false),right=e.score(region.hi,false);
      if(curvature.valid()&&curvature.hi<0&&left.lo>0&&right.hi<0){
        auto root=region;
        for(int it=0;it<e.maxit;++it){
          if(!e.charge())return e.out;
          const double mid=root.mid();auto gm=e.score(mid,false);if(!gm.valid())return e.out;
          const double error=gm.mag()/(-curvature.hi);
          root.lo=std::max(root.lo,mid-error);root.hi=std::min(root.hi,mid+error);
          if(gm.lo>0)root.lo=std::max(root.lo,mid);
          if(gm.hi<0)root.hi=std::min(root.hi,mid);
          if(root.hi-root.lo<target_width*mid)break;
        }
        if(!root.valid()||root.hi-root.lo>accepted_width*root.mid())return e.out;
        bool duplicate=false;for(auto old:roots)if(root.lo<=old.hi&&root.hi>=old.lo)duplicate=true;
        if(!duplicate){roots.push_back(root);regions.push_back(region);}continue;
      }
    }
    const double mid=std::sqrt(box.lo)*std::sqrt(box.hi);
    if(!(mid>box.lo&&mid<box.hi))return e.out;
    pending.push_back({box.lo,mid});pending.push_back({mid,box.hi});
  }
  std::vector<t_cert_detail::I> values;
  t_cert_detail::I best=e.normal;int winner=-1;
  if(critical&&collapse.lo>best.lo){best=collapse;winner=-2;}
  for(auto root:roots){auto value=e.value(root);if(!value.valid())return e.out;values.push_back(value);if(value.lo>best.lo){best=value;winner=static_cast<int>(values.size())-1;}}
  // A normal candidate is eligible only if its entire punctured tail was
  // proved below it; an unsigned witness-based tail cannot select normal.
  if(winner==-1&&!signed_tail)return e.out;
  bool tie=(winner!=-1&&e.normal.hi>=best.lo)||(critical&&winner!=-2&&collapse.hi>=best.lo);
  for(std::size_t j=0;j<values.size();++j)if(static_cast<int>(j)!=winner&&values[j].hi>=best.lo)tie=true;
  e.out.detail.loglik_upper=std::max(best.hi,e.normal.hi);
  if(critical)e.out.detail.loglik_upper=std::max(e.out.detail.loglik_upper,collapse.hi);
  for(auto value:values)e.out.detail.loglik_upper=std::max(e.out.detail.loglik_upper,value.hi);
  if(!roots.empty()) {
    e.out.detail.candidates.clear();
    for(auto root:roots){
      auto ll=e.value(root.mid());if(!ll.valid())return e.out;
      e.out.detail.candidates.push_back({{mu,scale,root.mid()},ll.lo,ll.hi,standardized?StopModel::None:StopModel::Lst});
    }
  }
  if(tie){e.out.status=3;e.out.detail.comparison_tie=true;return e.out;}
  if(winner==-2){
    e.out.status=5;e.out.loglik=collapse.mid();e.out.detail.limit_model=StopModel::TCollapsePath;
    e.out.detail.limit_coef={mu,2};e.out.detail.limit_loglik=collapse.mid();
    e.out.detail.loglik_lower=collapse.lo;return e.out;
  }
  if(winner==-1){
    e.out.status=1;e.out.loglik=e.normal.mid();e.out.detail.limit_model=StopModel::Norm;
    e.out.detail.limit_coef={mu,scale};e.out.detail.limit_loglik=e.normal.mid();
    e.out.detail.loglik_lower=e.normal.lo;return e.out;
  }
  e.out.shape=roots[winner].mid();auto represented=e.value(e.out.shape);
  if(!represented.valid())return e.out;
  e.out.loglik=represented.mid();e.out.status=0;
  e.out.detail.candidates={{{mu,scale,e.out.shape},represented.lo,represented.hi,standardized?StopModel::None:StopModel::Lst}};
  e.out.detail.loglik_lower=represented.lo;return e.out;
}
} // namespace t_shape_detail
} // namespace cppunivariate
