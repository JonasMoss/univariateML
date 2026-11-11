#pragma once
// Included after GevProfile. Bounds are exact-arithmetic interval expressions,
// conditional on floating-point comparisons with the margins below.
namespace gev_joint_detail {
struct Range { double lo,hi; };
inline Range point(double x) {return {x,x};}
inline Range add(Range a,Range b) {return {a.lo+b.lo,a.hi+b.hi};}
inline Range neg(Range a) {return {-a.hi,-a.lo};}
inline Range sub(Range a,Range b) {return add(a,neg(b));}
inline Range mul(Range a,Range b) {
  if(a.lo>=0) {
    if(b.lo>=0)return {a.lo*b.lo,a.hi*b.hi};
    if(b.hi<=0)return {a.hi*b.lo,a.lo*b.hi};
    return {a.hi*b.lo,a.hi*b.hi};
  }
  if(a.hi<=0) {
    if(b.lo>=0)return {a.lo*b.hi,a.hi*b.lo};
    if(b.hi<=0)return {a.hi*b.hi,a.lo*b.lo};
    return {a.lo*b.hi,a.lo*b.lo};
  }
  if(b.lo>=0)return {a.lo*b.hi,a.hi*b.hi};
  if(b.hi<=0)return {a.hi*b.lo,a.lo*b.lo};
  return {std::min(a.lo*b.hi,a.hi*b.lo),std::max(a.lo*b.lo,a.hi*b.hi)};
}
inline Range scale(Range a,double b) {return b>=0?Range{a.lo*b,a.hi*b}:Range{a.hi*b,a.lo*b};}
inline Range inv(Range a) {return {1/a.hi,1/a.lo};}
inline Range div(Range a,Range b) {return mul(a,inv(b));}
inline Range square(Range a) {
  return {a.lo<=0 && a.hi>=0 ? 0 : std::min(a.lo*a.lo,a.hi*a.hi),
          std::max(a.lo*a.lo,a.hi*a.hi)};
}
inline Range log(Range a) {return {std::log(a.lo),std::log(a.hi)};}
inline Range exp(Range a) {return {std::exp(a.lo),std::exp(a.hi)};}
inline double magnitude(Range a) {return std::max(std::abs(a.lo),std::abs(a.hi));}
inline bool finite(Range a) {return a.lo<=a.hi && std::isfinite(a.lo) && std::isfinite(a.hi);}
inline Range pad(Range a,double error) {return {a.lo-error,a.hi+error};}
// Compensated sums keep the practical margin independent of sample size.
struct Sum {
  double lo=0,hi=0,cl=0,ch=0;
  void add(Range a) {
    const double yl=a.lo-cl,tl=lo+yl;cl=(tl-lo)-yl;lo=tl;
    const double yh=a.hi-ch,th=hi+yh;ch=(th-hi)-yh;hi=th;
  }
  Range value() const {return {lo,hi};}
};
inline double feasible_loglik(span data,double mu,double sigma,double xi) {
  if(!(sigma>0)||!std::isfinite(mu)||!std::isfinite(sigma))return -INFINITY;
  Sum sum;const double logs=std::log(sigma);
  for(double x:data) {
    const double z=(x-mu)/sigma;
    if(xi== -1) {
      // Public coefficients must contain the observations in real arithmetic;
      // the closed face is rounded towards feasible endpoints below.
      if(x>mu+sigma && z>1+4*std::numeric_limits<double>::epsilon())return -INFINITY;
      sum.add(point(z-1-logs));
    } else {
      if(!(1+xi*z>0))return -INFINITY;
      const double L=gev_log_ratio(z,xi);
      sum.add(point(-logs-(1+xi)*L-std::exp(-L)));
    }
  }
  return sum.value().lo;
}
struct Bounds {Range value,x,v,xx,xv,vv; bool ok=false;};
// f=ell/n after profiling the location/scale orbit, minimum pivot at zero.
// v=sigma-xi*mu>0, L=log1p(xi*d/v)/xi, and every L is nonnegative.
inline Bounds evaluate(span data,Range xi,Range v,int maxit) {
  Bounds out{};
  if(!(v.lo>0) || 1+std::min(0.,xi.lo)/v.lo<=0)return out;
  Sum sl,sx,sv,sxx,sxv,svv,se,sex,sev,sexx,sexv,sevv,sex2,sev2,sexev;
  for(double d:data) {
    const Range z=scale(inv(v),d),q=mul(xi,z),t=add(point(1),q);
    if(!(t.lo>0))return out;
    Range L,Lx,Lxx;
    const double radius=magnitude(q);
    if(radius<=.25) {
      // Integral representations show that L, -L_x and L_xx increase
      // with z>=0 and decrease with xi. Evaluate their two common corners;
      // scalar series retain these dependencies instead of expanding boxes.
      auto jet=[&](double Z,double X) {
        const double q=X*Z,r=std::abs(q);
        double l=Z,lx=-Z*Z/2,lxx=2*Z*Z*Z/3,power=1,remainder=r;
        Bounds result{};
        for(int j=1;j<=maxit;++j) {
          power*= -q;
          l+=Z*power/(j+1);
          lx-=Z*Z*power*(j+1.)/(j+2.);
          lxx+=Z*Z*Z*power*(j+1.)*(j+2.)/(j+3.);
          remainder*=r;
          const double e0=Z*remainder/((j+2)*(1-r));
          const double e1=Z*Z*remainder/(1-r);
          const double e2=Z*Z*Z*remainder*((j+3)/(1-r)+r/((1-r)*(1-r)));
          if(std::max({e0,e1,e2})<=4*std::numeric_limits<double>::epsilon()) {
            result.value=pad(point(l),e0);result.x=pad(point(lx),e1);
            result.xx=pad(point(lxx),e2);result.ok=true;break;
          }
        }
        return result;
      };
      const auto low=jet(z.lo,xi.hi),high=jet(z.hi,xi.lo);
      if(!low.ok||!high.ok)return out;
      L={low.value.lo,high.value.hi};
      Lx={high.x.lo,low.x.hi};Lxx={low.xx.lo,high.xx.hi};
    } else {
      if(xi.lo<=0 && xi.hi>=0)return out;
      L=div(log(t),xi);
      const Range a=div(z,t);
      Lx=div(sub(a,L),xi);
      Lxx=div(sub(neg(square(a)),scale(Lx,2)),xi);
    }
    // Exact known signs avoid a spurious exponential factor at zero.
    L.lo=std::max(0.,L.lo);
    const Range Lv=neg(div(z,mul(v,t)));
    const Range Lvv=div(mul(z,add(point(2),q)),mul(square(v),square(t)));
    const Range Lxv=div(square(z),mul(v,square(t)));
    const Range e=exp(neg(L));
    sl.add(L);sx.add(Lx);sv.add(Lv);sxx.add(Lxx);sxv.add(Lxv);svv.add(Lvv);
    se.add(e);sex.add(mul(e,Lx));sev.add(mul(e,Lv));
    sexx.add(mul(e,Lxx));sexv.add(mul(e,Lxv));sevv.add(mul(e,Lvv));
    sex2.add(mul(e,square(Lx)));sev2.add(mul(e,square(Lv)));sexev.add(mul(e,mul(Lx,Lv)));
  }
  const double n=static_cast<double>(data.size());
  Range E=se.value();E.lo=std::max(1.,E.lo);
  const Range X=div(sex.value(),E),V=div(sev.value(),E),one=add(point(1),xi);
  const Range ml=scale(sl.value(),1/n),mx=scale(sx.value(),1/n),mv=scale(sv.value(),1/n);
  out.value=sub(sub(neg(log(v)),log(scale(E,1/n))),add(mul(one,ml),point(1)));
  out.x=sub(sub(X,ml),mul(one,mx));
  out.v=sub(sub(V,inv(v)),mul(one,mv));
  out.xx=sub(add(sub(div(sexx.value(),E),div(sex2.value(),E)),square(X)),
      add(scale(mx,2),mul(one,scale(sxx.value(),1/n))));
  out.vv=sub(add(add(sub(div(sevv.value(),E),div(sev2.value(),E)),square(V)),square(inv(v))),
      mul(one,scale(svv.value(),1/n)));
  out.xv=sub(add(sub(div(sexv.value(),E),div(sexev.value(),E)),mul(X,V)),
      add(mv,mul(one,scale(sxv.value(),1/n))));
  // Arithmetic comparison margin; no claim of directed rounding.
  const double eps=128*std::numeric_limits<double>::epsilon();
  out.value=pad(out.value,eps*(1+magnitude(out.value)+magnitude(ml)));
  out.x=pad(out.x,eps*(1+magnitude(X)+magnitude(ml)+magnitude(mul(one,mx))));
  out.v=pad(out.v,eps*(1+1/v.lo+magnitude(V)+magnitude(mul(one,mv))));
  out.xx=pad(out.xx,eps*(1+magnitude(out.xx)+magnitude(scale(sxx.value(),1/n))));
  out.vv=pad(out.vv,eps*(1+magnitude(out.vv)+1/(v.lo*v.lo)));
  out.xv=pad(out.xv,eps*(1+magnitude(out.xv)+magnitude(mv)));
  out.ok=finite(out.value)&&finite(out.x)&&finite(out.v)&&finite(out.xx)&&finite(out.vv)&&finite(out.xv);
  return out;
}
// Only f_v and f_vv are needed to bracket the unique inner optimum.
// L(z,xi) increases in z and decreases in xi, including its removable zero.
// These corner bounds avoid all shape-derivative jets during sign checks.
inline Bounds nuisance_evaluate(span data,Range xi,Range v,int maxit) {
  Bounds out{};
  if(!(v.lo>0) || 1+std::min(0.,xi.lo)/v.lo<=0)return out;
  Sum se,sev,sevv,sev2,sv,svv;
  const Range iv=inv(v),iv2=square(iv),one=add(point(1),xi);
  auto ratio=[](double z,double x) {
    const double q=x*z;
    return q==0?z:(std::abs(q)<1e-4?z*(std::log1p(q)/q):std::log1p(q)/x);
  };
  for(double d:data) {
    const Range z=scale(iv,d),q=mul(xi,z),t=add(point(1),q);
    if(!(t.lo>0))return out;
    // Retain the shape-jet work cap even though sign checks do not need
    // those jets. Otherwise a small maxit can change an iteration stop into
    // a later numerical stop. Remainders decrease with the series degree.
    const double radius=magnitude(q);
    if(radius<=.25 && d!=0) {
      const double Z=z.hi,r=radius,power=std::pow(r,maxit+1);
      const double e0=Z*power/((maxit+2.)*(1-r));
      const double e1=Z*Z*power/(1-r);
      const double e2=Z*Z*Z*power*((maxit+3.)/(1-r)+r/((1-r)*(1-r)));
      if(!(std::max({e0,e1,e2})<=4*std::numeric_limits<double>::epsilon()))return out;
    }
    const Range L{std::max(0.,ratio(z.lo,xi.hi)),ratio(z.hi,xi.lo)};
    const Range Lv=neg(div(mul(z,iv),t));
    const Range Lvv=div(mul(mul(z,add(point(2),q)),iv2),square(t));
    const Range e=exp(neg(L));
    se.add(e);sev.add(mul(e,Lv));sevv.add(mul(e,Lvv));
    sev2.add(mul(e,square(Lv)));sv.add(Lv);svv.add(Lvv);
  }
  const double n=static_cast<double>(data.size());
  Range E=se.value();E.lo=std::max(1.,E.lo);
  const Range V=div(sev.value(),E),mv=scale(sv.value(),1/n);
  out.v=sub(sub(V,iv),mul(one,mv));
  out.vv=sub(add(add(sub(div(sevv.value(),E),div(sev2.value(),E)),square(V)),iv2),
      mul(one,scale(svv.value(),1/n)));
  const double eps=128*std::numeric_limits<double>::epsilon();
  out.v=pad(out.v,eps*(1+1/v.lo+magnitude(V)+magnitude(mul(one,mv))));
  out.vv=pad(out.vv,eps*(1+magnitude(out.vv)+1/(v.lo*v.lo)));
  out.ok=finite(out.v)&&finite(out.vv);return out;
}
struct Root {Range v; bool ok=false;};
inline Root coordinate(Root r,double xi,bool gap) {
  if(gap)r.v=sub(scale(r.v,-1/xi),point(1));
  return r;
}
// Reflected-Weibull profile, with the upper endpoint gap e as nuisance.
// Normalizing distances by a fixed reference keeps every exponential <=1.
inline Bounds gap_evaluate(span data,Range xi,Range e) {
  Bounds out{};if(!(e.lo>0))return out;
  const Range a=neg(inv(xi));const double ref=1+e.hi;
  Sum sl,sr,sr2,se,sel,ser,sel2,ser2,selr;
  for(double x:data) {
    const Range w=add(e,point(1-x)),l=log(scale(w,1/ref)),r=inv(w),p=exp(mul(a,l));
    sl.add(l);sr.add(r);sr2.add(square(r));se.add(p);
    sel.add(mul(p,l));ser.add(mul(p,r));sel2.add(mul(p,square(l)));
    ser2.add(mul(p,square(r)));selr.add(mul(p,mul(l,r)));
  }
  const double n=static_cast<double>(data.size());
  const Range E=se.value(),L=div(sel.value(),E),R=div(ser.value(),E);
  const Range ml=scale(sl.value(),1/n),mr=scale(sr.value(),1/n),one=sub(a,point(1));
  const Range fa=sub(add(inv(a),ml),L);
  const Range faa=sub(neg(square(inv(a))),sub(div(sel2.value(),E),square(L)));
  const Range fae=sub(sub(mr,R),mul(a,sub(div(selr.value(),E),mul(L,R))));
  out.value=sub(add(sub(log(a),point(std::log(ref))),mul(one,ml)),add(log(scale(E,1/n)),point(1)));
  out.x=mul(square(a),fa);
  out.v=sub(mul(one,mr),mul(a,R));
  out.xx=add(mul(square(square(a)),faa),scale(mul(mul(square(a),a),fa),2));
  out.xv=mul(square(a),fae);
  out.vv=add(neg(add(mul(one,scale(sr2.value(),1/n)),mul(mul(a,one),div(ser2.value(),E)))),mul(square(a),square(R)));
  const double eps=256*std::numeric_limits<double>::epsilon();
  out.value=pad(out.value,eps*(1+magnitude(out.value)+magnitude(ml)));
  out.x=pad(out.x,eps*(1+magnitude(square(a))*(magnitude(ml)+magnitude(L)+1)));
  out.v=pad(out.v,eps*(1+magnitude(mul(one,mr))+magnitude(mul(a,R))));
  out.xx=pad(out.xx,eps*(1+magnitude(out.xx)+magnitude(square(a))));
  out.xv=pad(out.xv,eps*(1+magnitude(out.xv)));
  out.vv=pad(out.vv,eps*(1+magnitude(out.vv)+magnitude(mul(square(a),square(R)))));
  out.ok=finite(out.value)&&finite(out.x)&&finite(out.v)&&finite(out.xx)&&finite(out.xv)&&finite(out.vv);
  return out;
}
inline Bounds bounds(span data,Range xi,Range nuisance,int maxit,bool gap) {
  return gap?gap_evaluate(data,xi,nuisance):evaluate(data,xi,nuisance,maxit);
}
// Signs plus uniqueness of the profiled endpoint gap certify this bracket.
// Newton is only a proposer. Failure to verify signs cannot return Ok.
inline Root inner(span data,double xi,int maxit) {
  const auto proposed=gev_profile(data,xi,1e-13,maxit).fit;
  double center=proposed.sigma-xi*proposed.mu;
  if(!(center>std::max(0.,-xi)) || !std::isfinite(center))center=1+std::max(0.,-xi);
  double lo=center,hi=center;
  bool lower=false,upper=false;
  double step=1e-9*center;
  int steps=0;
  for(;steps<maxit;++steps) {
    lo=std::max(std::nextafter(std::max(0.,-xi),INFINITY),center-step);
    hi=center+step;
    const auto l=nuisance_evaluate(data,point(xi),point(lo),maxit),h=nuisance_evaluate(data,point(xi),point(hi),maxit);
    lower=l.ok && l.v.lo>0;upper=h.ok && h.v.hi<0;
    if(lower&&upper)break;
    step*=2;
  }
  if(!lower || !upper)return {{lo,hi},false};
  for(;steps<maxit;++steps) {
    const double mid=lo+(hi-lo)/2;
    const auto m=nuisance_evaluate(data,point(xi),point(mid),maxit);
    if(!m.ok)return {{lo,hi},false};
    if(m.v.lo>0)lo=mid;
    else if(m.v.hi<0)hi=mid;
    else {
      const auto box=nuisance_evaluate(data,point(xi),{lo,hi},maxit);
      if(box.ok && box.vv.hi<0) {
        const Range next=sub(point(mid),div(m.v,box.vv));
        lo=std::max(lo,next.lo);hi=std::min(hi,next.hi);
        return {{lo,hi},lo<=hi};
      }
      const double a=lo+(hi-lo)/4,b=hi-(hi-lo)/4;
      const auto A=nuisance_evaluate(data,point(xi),point(a),maxit),B=nuisance_evaluate(data,point(xi),point(b),maxit);
      bool progress=false;
      if(A.ok&&A.v.lo>0) {lo=a;progress=true;}
      if(B.ok&&B.v.hi<0) {hi=b;progress=true;}
      if(!progress)return {{lo,hi},false};
    }
    if(hi-lo<=64*std::numeric_limits<double>::epsilon()*mid)return {{lo,hi},true};
  }
  return {{lo,hi},false};
}
// e(a) increases with a on each branch. These enclose all optimal pivot
// scales in a slab, using brackets at its ends (not numerical point roots).
template<class F>
inline Root region(span data,double lo,double hi,int maxit,F& get,bool gap) {
  const auto l=get(lo),h=get(hi);
  if(!l.ok || !h.ok)return {{0,0},false};
  Range v;
  if(gap) {
    v={l.v.lo/(-lo)-1,h.v.hi/(-hi)-1};
  } else if(lo>0 && !(lo>=-.25&&hi<=.25)) {
    const Range eA=scale(h.v,1/hi),eB=scale(l.v,1/lo);
    v={lo*eA.lo,hi*eB.hi};
  } else if(hi<0 && !(lo>=-.25&&hi<=.25)) {
    const Range eA=sub(scale(l.v,-1/lo),point(1)),eB=sub(scale(h.v,-1/hi),point(1));
    v={-hi*(1+eA.lo),-lo*(1+eB.hi)};
  } else {
    // At zero, uniqueness and uniform opposite boundary-score signs give a
    // connected inner-root enclosure. Do not infer this from point signs.
    double a=std::min(l.v.lo,h.v.lo),b=std::max(l.v.hi,h.v.hi);
    const double boundary=std::max(0.,-lo);
    // A rectangle containing either endpoint root below this support floor
    // cannot be evaluated uniformly. Subdivide rather than expanding it.
    if(std::min(l.v.hi,h.v.hi)<=boundary)return {{a,b},false};
    Range corners{NAN,NAN};
    if(lo>0)corners={(lo/hi)*h.v.lo,(hi/lo)*l.v.hi};
    else if(hi<0)corners={(hi/lo)*l.v.lo,(lo/hi)*h.v.hi};
    const bool have_corners=finite(corners);
    double radius=std::max(1e-10*(a+b),b-a);
    bool found=false;
    for(int j=0;j<maxit;++j) {
      // On one-sided slabs the endpoint-gap monotonicity supplies a second
      // proved enclosure. Expansion need not continue beyond that box.
      if(have_corners && a<=corners.lo && b>=corners.hi) {found=true;break;}
      const auto L=nuisance_evaluate(data,{lo,hi},point(a),maxit),H=nuisance_evaluate(data,{lo,hi},point(b),maxit);
      if(L.ok&&H.ok&&L.v.lo>0&&H.v.hi<0) {found=true;break;}
      // Approach the support floor from above; jumping to it can make
      // inverse powers overflow and never establish the lower score sign.
      const double next=a-radius;
      a=next>boundary?next:std::max(std::nextafter(boundary,INFINITY),boundary+(a-boundary)/2);
      b+=radius;
      radius*=2;
    }
    if(!found)return {{a,b},false};
    v={a,b};
    if(have_corners)v={std::max(v.lo,corners.lo),std::min(v.hi,corners.hi)};
  }
  if(!finite(v))return {v,false};
  // Uniform interval Newton contracts this enclosure without assuming that
  // the inner maximizer is monotone in the pivot scale.
  const double center=lo+(hi-lo)/2;
  const auto center_root=coordinate(get(center),center,gap);
  for(int j=0;j<maxit;++j) {
    const auto box=bounds(data,{lo,hi},v,maxit,gap);
    if(!box.ok || !(box.vv.hi<0))break;
    const double mid=v.lo+(v.hi-v.lo)/2;
    const auto m=bounds(data,{lo,hi},point(mid),maxit,gap);
    if(!m.ok)break;
    const Range next=sub(point(mid),div(m.v,box.vv));
    Range narrowed{std::max(v.lo,next.lo),std::min(v.hi,next.hi)};
    if(center_root.ok) {
      const Range variation=mul(neg(div(box.xv,box.vv)),{lo-center,hi-center});
      const Range tube=add(center_root.v,variation);
      narrowed={std::max(narrowed.lo,tube.lo),std::min(narrowed.hi,tube.hi)};
    }
    if(narrowed.lo>narrowed.hi)return {v,false};
    if(narrowed.hi-narrowed.lo>=.75*(v.hi-v.lo))break;
    v=narrowed;
  }
  return {v,true};
}
inline Range curvature(Bounds b) {return sub(b.xx,div(square(b.xv),b.vv));}
// The rectangular minimum-pivot box can cross support on a wide negative
// slab even though every actual inner optimum is feasible. The reflected
// Weibull coordinates retain that feasibility and give a value envelope
// before derivative-based subdivision is needed.
template<class F>
inline double negative_upper(span data,double lo,double hi,int,F& get) {
  const auto l=get(lo),h=get(hi);
  if(!l.ok||!h.ok)return INFINITY;
  const Range a{-1/lo,-1/hi};
  const Range e{l.v.lo/(-lo)-1,h.v.hi/(-hi)-1};
  if(!(e.lo>0&&e.hi>=e.lo))return INFINITY;
  const double ref=1+e.hi;
  Sum logs,powers;
  for(double x:data) {
    const Range u=log({(e.lo+1-x)/ref,(e.hi+1-x)/ref});
    logs.add(u);powers.add(exp(mul(a,u)));
  }
  const double n=static_cast<double>(data.size());
  const Range result=sub(add(sub(log(a),point(std::log(ref))),mul(sub(a,point(1)),scale(logs.value(),1/n))),
      add(log(scale(powers.value(),1/n)),point(1)));
  return result.hi+4096*std::numeric_limits<double>::epsilon()*(1+magnitude(result));
}
// Public coefficients over an inner-root enclosure, using stable expm1.
inline std::pair<Range,Range> coefficients(span data,Range xi,Range v,int maxit) {
  // L value is also obtained from point-profile orbit sums; use the same
  // nonnegative minimum-pivot integrand and its removable-zero series.
  Sum sum;
  for(double d:data) {
    const Range z=scale(inv(v),d),q=mul(xi,z);Range L;
    if(magnitude(q)<=.25) {
      L=z;Range power=point(1);bool done=d==0;
      for(int j=1;!done&&j<=maxit;++j) {
        power=mul(power,neg(q));L=add(L,scale(mul(z,power),1./(j+1)));
        const double r=magnitude(q),err=z.hi*std::pow(r,j+1)/((j+2)*(1-r));
        if(err<4*std::numeric_limits<double>::epsilon()) {L=pad(L,err);done=true;}
      }
      if(!done)return {{NAN,NAN},{NAN,NAN}};
    } else {
      if(xi.lo<=0&&xi.hi>=0)return {{NAN,NAN},{NAN,NAN}};
      L=div(log(add(point(1),q)),xi);
    }
    L.lo=std::max(0.,L.lo);sum.add(exp(neg(L)));
  }
  Range g=neg(log(scale(sum.value(),1./data.size()))),q=mul(xi,g);
  Range scale_out=mul(v,exp(q)),loc;
  // expm1(xi*g)/xi = g*integral_0^1 exp(t*xi*g)dt is monotone
  // in both nonnegative g and xi, also across zero.
  auto ratio=[](double x,double g) {return x==0?g:std::expm1(x*g)/x;};
  loc=mul(v,{ratio(xi.lo,g.lo),ratio(xi.hi,g.hi)});
  const double eps=256*std::numeric_limits<double>::epsilon();
  return {pad(loc,eps*(magnitude(v)+magnitude(loc))),pad(scale_out,eps*magnitude(scale_out))};
}
inline std::pair<Range,Range> public_bounds(span data,Range xi,Range v,int maxit,bool gap) {
  if(!gap)return coefficients(data,xi,v,maxit);
  // Positive-order power means increase in e and a, while s(e,a)-e
  // decreases in e and increases in a (Holder gives partial_e s <= 1).
  // Corner evaluation preserves these dependencies in the public location.
  const Range a=neg(inv(xi));
  auto power_mean=[&](double e,double shape) {
    const double ref=1+e;Sum powers;
    for(double x:data)powers.add(point(std::exp(shape*std::log((e+1-x)/ref))));
    return ref*std::exp(std::log(powers.value().lo/data.size())/shape);
  };
  const Range sigma{power_mean(v.lo,a.lo)/a.hi,power_mean(v.hi,a.hi)/a.lo};
  const Range mu{1+v.lo-power_mean(v.lo,a.hi),1+v.hi-power_mean(v.hi,a.lo)};
  const double eps=256*std::numeric_limits<double>::epsilon();
  return {pad(mu,eps*(1+magnitude(mu)+magnitude(v))),pad(sigma,eps*(1+magnitude(sigma)))};
}
inline bool precise(span data,Range xi,Range nuisance,int maxit,bool gap,double tol,bool frechet) {
  const auto c=public_bounds(data,xi,nuisance,maxit,gap);
  if(!finite(c.first)||!finite(c.second))return false;
  Range location=c.first,scale_out=c.second,shape=xi;
  if(frechet) {
    if(!(xi.lo>0))return false;
    shape=inv(xi);location=neg(div(nuisance,xi));scale_out=div(c.second,xi);
  }
  const double mu=(location.lo+location.hi)/2,sigma=(scale_out.lo+scale_out.hi)/2;
  const double shape_scale=frechet?(shape.lo+shape.hi)/2:1;
  const double fraction=frechet?.5:1.;
  return sigma>0 && finite(location)&&finite(scale_out)&&finite(shape) &&
      shape.hi-shape.lo<=tol*.5*shape_scale &&
      location.hi-location.lo<=fraction*tol*std::max(std::abs(mu),sigma) &&
      scale_out.hi-scale_out.lo<=fraction*tol*sigma;
}
// A one-sided face envelope. Convexity of log mean(w^a) in a gives
// ell(a,e)<=ell(1,e)+(a-1)[n+sum log w-n sum(w log w)/sum w].
// Bounding e<=eA leaves k log(eA), hence excludes a full neighbourhood
// of a=1 without evaluating a derivative at the moving support endpoint.
template<class F>
inline double face_upper(span data,double xi,int,double face,F& get) {
  const auto root=get(xi);
  if(!root.ok)return INFINITY;
  const double e=std::max(0.,root.v.hi/(-xi)-1);
  if(!(e>0))return INFINITY;
  Sum sumd,logs,weighted;std::size_t k=0;
  auto xlog=[](double z) {return z==0?0:z*std::log(z);};
  for(double x:data) {
    const double d=1-x;sumd.add(point(d));
    if(d==0)++k;else logs.add(point(std::log(d+e)));
    double minimum=std::min(xlog(d),xlog(d+e));
    if(d<=std::exp(-1.)&&d+e>=std::exp(-1.))minimum=-std::exp(-1.);
    weighted.add(point(minimum));
  }
  const double n=static_cast<double>(data.size());
  const double w=weighted.value().lo,s=sumd.value().lo;
  const double denominator=w<0?s:s+n*e;
  const double C=n+logs.value().hi+k*std::log(e)-n*w/denominator;
  const double margin=4096*std::numeric_limits<double>::epsilon()*n*(1+std::abs(C));
  return face+std::max(0.,(-1/xi-1)*(C+margin))/n;
}
} // namespace gev_joint_detail

// General all-free GEV, compact fitted shape branch. Every discarded slab
// has a value or profile-score exclusion; every returned interior member is
// isolated with a strictly negative profile curvature and public bounds.
inline GevResult gev_joint_certificate_impl(span data,double tol,int maxit,bool frechet=false) {
  gev_joint_detail::Sum gaps;
  for(double x:data)gaps.add(gev_joint_detail::point(1-x));
  const double face_scale=gaps.value().lo/data.size(),face_location=std::nextafter(1-face_scale,INFINITY);
  GevResult out{face_location,face_scale,-1,gev_joint_detail::feasible_loglik(data,face_location,face_scale,-1),0,GevStatus::Ok};
  GevResult face=out;
  if(256*std::numeric_limits<double>::epsilon()*(1+face_scale)>tol*std::max(std::abs(face_location),face_scale) ||
      256*std::numeric_limits<double>::epsilon()>tol)
    face.status=GevStatus::NumericalFailure;
  out.status=GevStatus::NumericalFailure;
  out.stop_detail.loglik_upper=INFINITY;
  const double n=static_cast<double>(data.size());
  const double margin=4096*std::numeric_limits<double>::epsilon();
  double face_upper_value=-std::log(face_scale)-1+margin*(1+std::abs(std::log(face_scale)));
  struct Candidate {double lo,hi,ll,upper;GevResult fit;};
  struct Slab {double lo,hi;};
  std::vector<Candidate> candidates;
  std::vector<Slab> pending;
  std::int64_t evaluations=0;const auto cap=certificate_budget(maxit);
  std::vector<std::pair<double,gev_joint_detail::Root>> cache;
  auto get=[&](double xi) {
    const auto at=std::lower_bound(cache.begin(),cache.end(),xi,
        [](const auto& a,double x){return a.first<x;});
    if(at!=cache.end()&&at->first==xi)return at->second;
    if(evaluations>=cap)return gev_joint_detail::Root{{0,0},false};
    ++evaluations;
    const auto r=gev_joint_detail::inner(data,xi,maxit);
    cache.insert(at,{xi,r});return r;
  };
  if(frechet) {
    const auto root=get(0);
    if(!root.ok) {out.status=GevStatus::IterationFailure;out.iterations=static_cast<int>(evaluations);return out;}
    const auto c=gev_joint_detail::coefficients(data,{0,0},root.v,maxit);
    const auto b=gev_joint_detail::evaluate(data,{0,0},root.v,maxit);
    if(!b.ok) {out.iterations=static_cast<int>(evaluations);return out;}
    const double mu=(c.first.lo+c.first.hi)/2,sigma=(c.second.lo+c.second.hi)/2;
    face={mu,sigma,0,gev_joint_detail::feasible_loglik(data,mu,sigma,0),0,GevStatus::Ok};
    if(!gev_joint_detail::precise(data,{0,0},root.v,maxit,false,tol,false))face.status=GevStatus::NumericalFailure;
    out=face;out.status=GevStatus::NumericalFailure;out.stop_detail.loglik_upper=INFINITY;
    face_upper_value=b.value.hi;
  }
  double incumbent=out.logLik/n;
  GevProfile proposal=gev_profile(data,0,1e-12,maxit);
  auto witness=[&](const GevResult& f) {
    if(std::isfinite(f.logLik)) {
      out.stop_detail.candidates.push_back({{f.mu,f.sigma,f.xi},f.logLik,f.logLik});
      out.stop_detail.loglik_lower=std::isnan(out.stop_detail.loglik_lower)?f.logLik:
          std::max(out.stop_detail.loglik_lower,f.logLik);
    }
  };
  witness(out);candidates.push_back({face.xi,face.xi,incumbent,face_upper_value,face});
  for(double xi:{-.99,-.9,-.5,0.,.25,.5,1.}) {
    if(frechet && xi<0)continue;
    if(xi!=1) {
      if(evaluations>=cap) {out.stop_detail.budget_exhausted=true;return out;}
      ++evaluations;
      auto f=gev_profile(data,xi,1e-12,maxit).fit;
      f.logLik=gev_joint_detail::feasible_loglik(data,f.mu,f.sigma,xi);
      witness(f);if(std::isfinite(f.logLik))incumbent=std::max(incumbent,f.logLik/n);
      if(f.logLik>proposal.fit.logLik)proposal=gev_profile(data,xi,1e-12,maxit);
      continue;
    }
    auto r=get(xi);
    if(!r.ok) {out.status=GevStatus::IterationFailure;out.iterations=static_cast<int>(evaluations);return out;}
    const double vmid=r.v.lo+(r.v.hi-r.v.lo)/2;
    const auto c=gev_joint_detail::coefficients(data,{xi,xi},frechet?gev_joint_detail::point(vmid):r.v,maxit);
    const double mu=(c.first.lo+c.first.hi)/2,s=(c.second.lo+c.second.hi)/2;
    GevResult f{mu,s,xi,gev_joint_detail::feasible_loglik(data,mu,s,xi),0,GevStatus::Ok};
    if(!gev_joint_detail::precise(data,{xi,xi},r.v,maxit,false,tol,frechet))
      f.status=GevStatus::NumericalFailure;
    witness(f);incumbent=std::max(incumbent,f.logLik/n);
    if(xi==1) {
      const auto b=gev_joint_detail::evaluate(data,{xi,xi},r.v,maxit);
      if(!b.ok) {out.iterations=static_cast<int>(evaluations);return out;}
      candidates.push_back({xi,xi,f.logLik/n,b.value.hi,f});
    }
  }
  // A proposer improves only the feasible lower witness. It cannot remove
  // any slab, supply an inclusion, or turn a failed certificate into Ok.
  for(int j=0;j<maxit && evaluations<cap;++j) {
    if(!(proposal.curvature<0) || !std::isfinite(proposal.score))break;
    const double next=std::max(frechet?0.:-.999999,std::min(1.,proposal.fit.xi-proposal.score/proposal.curvature));
    if(std::abs(next-proposal.fit.xi)<1e-13)break;
    ++evaluations;
    const auto p=gev_profile(data,next,1e-13,maxit);
    auto f=p.fit;f.logLik=gev_joint_detail::feasible_loglik(data,f.mu,f.sigma,next);
    witness(f);if(std::isfinite(f.logLik))incumbent=std::max(incumbent,f.logLik/n);
    if(!(f.logLik>proposal.fit.logLik))break;
    proposal=p;
  }
  double left=-.9375;
  bool face_done=frechet;
  while(!frechet && evaluations<cap) {
    ++evaluations;
    const double upper=gev_joint_detail::face_upper(data,left,maxit,candidates[0].upper,get);
    // Equality is allowed here: the face itself is retained as a candidate.
    if(upper<=candidates[0].upper) {face_done=true;break;}
    if(upper<incumbent-margin) {face_done=true;break;}
    const double next=-1+(left+1)/2;
    if(next==left || next== -1)break;
    left=next;
  }
  if(!face_done) {out.iterations=static_cast<int>(evaluations);out.stop_detail.budget_exhausted=evaluations>=cap;return out;}
  // A central slab can cross zero; its size is only a starting subdivision.
  if(!frechet) {pending.push_back({left,-.25});pending.push_back({-.25,-.001});}
  pending.push_back({frechet?0.:-.001,.001});pending.push_back({.001,.25});pending.push_back({.25,1});
  bool covered=true,solver_exhausted=false;
  while(!pending.empty() && evaluations<cap) {
    const Slab s=pending.back();pending.pop_back();++evaluations;
    const bool gap=s.hi<=-.25;
    // The value-only reflected-Weibull envelope is valid on every strictly
    // negative slab, including pivot boxes that cross rectangular support.
    if(s.hi<0 && gev_joint_detail::negative_upper(data,s.lo,s.hi,maxit,get)<incumbent-margin)continue;
    const auto root=gev_joint_detail::region(data,s.lo,s.hi,maxit,get,gap);
    const auto b=root.ok?gev_joint_detail::bounds(data,{s.lo,s.hi},root.v,maxit,gap):gev_joint_detail::Bounds{};
    out.stop_detail.segment_loglik_upper=b.ok?n*b.value.hi:INFINITY;
    if(b.ok && (b.value.hi<incumbent-margin || b.x.lo>margin || b.x.hi< -margin))continue;
    const double midpoint=s.lo+(s.hi-s.lo)/2;
    gev_joint_detail::Bounds midpoint_bounds{};
    if(b.ok) {
      const double mid=midpoint,w=(s.hi-s.lo)/2;
      const auto r=gev_joint_detail::coordinate(get(mid),mid,gap);
      const auto m=r.ok?gev_joint_detail::bounds(data,{mid,mid},r.v,maxit,gap):gev_joint_detail::Bounds{};
      midpoint_bounds=m;
      if(m.ok) {
        // Taylor about an exact inner optimum: its v score is zero. This
        // retains the nuisance cancellation even before f_vv is negative
        // throughout the slab box.
        const double V=std::max(root.v.hi-r.v.lo,r.v.hi-root.v.lo);
        const double upper=m.value.hi+gev_joint_detail::magnitude(m.x)*w+
            .5*std::max(0.,b.xx.hi)*w*w+gev_joint_detail::magnitude(b.xv)*w*V+
            .5*std::max(0.,b.vv.hi)*V*V;
        if(upper<incumbent-margin)continue;
      }
    }
    if(b.ok && b.vv.hi<0) {
      const auto C=gev_joint_detail::curvature(b);
      if(C.lo>margin)continue; // strict convexity has no interior maximum
      const double radius=(s.hi-s.lo)/2;
      const auto m=midpoint_bounds;
      if(m.ok && m.value.hi+gev_joint_detail::magnitude(m.x)*radius+
          .5*std::max(0.,C.hi)*radius*radius<incumbent-margin)continue;
      if(m.ok) {
        const auto score=gev_joint_detail::add(m.x,gev_joint_detail::mul(C,{-radius,radius}));
        if(score.lo>margin || score.hi< -margin)continue;
      }
    }
    bool isolated=false;
    if(b.ok && b.vv.hi<0 && gev_joint_detail::curvature(b).hi< -margin) {
      const auto l=gev_joint_detail::coordinate(get(s.lo),s.lo,gap),h=gev_joint_detail::coordinate(get(s.hi),s.hi,gap);
      const auto L=gev_joint_detail::bounds(data,{s.lo,s.lo},l.v,maxit,gap),H=gev_joint_detail::bounds(data,{s.hi,s.hi},h.v,maxit,gap);
      if(l.ok&&h.ok&&L.ok&&H.ok&&(L.x.hi< -margin||H.x.lo>margin))continue;
      if(l.ok&&h.ok&&L.ok&&H.ok&&L.x.lo>margin&&H.x.hi< -margin) {
        double lo=s.lo,hi=s.hi;auto vr=root;bool solved=false;
        int root_steps=0;
        for(;root_steps<maxit && evaluations<cap;++root_steps) {
          ++evaluations;
          if(gev_joint_detail::precise(data,{lo,hi},vr.v,maxit,gap,tol,frechet)) {solved=true;break;}
          const auto box=gev_joint_detail::bounds(data,{lo,hi},vr.v,maxit,gap);
          if(!box.ok || !(box.vv.hi<0))break;
          const auto determinant=gev_joint_detail::sub(gev_joint_detail::mul(box.xx,box.vv),gev_joint_detail::square(box.xv));
          if(!(determinant.lo>0))break;
          const double mid=lo+(hi-lo)/2,v=vr.v.lo+(vr.v.hi-vr.v.lo)/2;
          const auto m=gev_joint_detail::bounds(data,{mid,mid},{v,v},maxit,gap);
          if(!m.ok)break;
          const auto dx=gev_joint_detail::div(gev_joint_detail::sub(gev_joint_detail::mul(box.vv,m.x),gev_joint_detail::mul(box.xv,m.v)),determinant);
          const auto dv=gev_joint_detail::div(gev_joint_detail::sub(gev_joint_detail::mul(box.xx,m.v),gev_joint_detail::mul(box.xv,m.x)),determinant);
          const auto X=gev_joint_detail::sub(gev_joint_detail::point(mid),dx),V=gev_joint_detail::sub(gev_joint_detail::point(v),dv);
          const double nl=std::max(lo,X.lo),nh=std::min(hi,X.hi);
          const gev_joint_detail::Range nv{std::max(vr.v.lo,V.lo),std::min(vr.v.hi,V.hi)};
          if(nl>nh || nv.lo>nv.hi)break;
          if(nh-nl>=.99*(hi-lo) && nv.hi-nv.lo>=.99*(vr.v.hi-vr.v.lo))break;
          lo=nl;hi=nh;vr.v=nv;
        }
        if(!solved) {solver_exhausted=root_steps>=maxit;covered=false;break;}
        if(solved) {
          const double xi=lo+(hi-lo)/2,vmid=vr.v.lo+(vr.v.hi-vr.v.lo)/2;
          const auto c=gev_joint_detail::public_bounds(data,frechet?gev_joint_detail::point(xi):gev_joint_detail::Range{lo,hi},
              frechet?gev_joint_detail::point(vmid):vr.v,maxit,gap);
          const double mu=(c.first.lo+c.first.hi)/2,scale=(c.second.lo+c.second.hi)/2;
          GevResult f{mu,scale,xi,gev_joint_detail::feasible_loglik(data,mu,scale,xi),0,GevStatus::Ok};
          const auto bound=gev_joint_detail::bounds(data,{lo,hi},vr.v,maxit,gap);
          if(std::isfinite(f.logLik)&&bound.ok) {
            witness(f);candidates.push_back({lo,hi,f.logLik/n,bound.value.hi,f});
            incumbent=std::max(incumbent,f.logLik/n);isolated=true;
          }
        }
      }
    }
    if(isolated)continue;
    const double mid=s.lo+(s.hi-s.lo)/2;
    if(mid==s.lo || mid==s.hi) {covered=false;break;}
    pending.push_back({s.lo,mid});pending.push_back({mid,s.hi});
  }
  out.iterations=static_cast<int>(evaluations);
  if(solver_exhausted)out.status=GevStatus::IterationFailure;
  if((!covered||!pending.empty()) && evaluations>=cap)out.stop_detail.budget_exhausted=true;
  if(covered && pending.empty()) {
    auto winner=candidates.begin();
    for(auto it=candidates.begin();it!=candidates.end();++it)if(it->ll>winner->ll)winner=it;
    bool separated=true;
    for(const auto& c:candidates)if(&c!=&*winner && !(c.lo<=winner->hi&&c.hi>=winner->lo) && c.upper>=winner->ll-margin)separated=false;
    if(separated && winner->fit.status==GevStatus::Ok) {
      const int work=out.iterations;out=winner->fit;out.iterations=work;out.stop_detail={};out.certified=true;
      if(frechet) {out.stop_detail.loglik_lower=winner->ll*n;out.stop_detail.loglik_upper=winner->upper*n;}
    }
    else if(!separated) {
      out.stop_detail.comparison_tie=true;
      out.stop_detail.candidates.clear();double upper=-INFINITY;
      for(const auto& c:candidates) {
        upper=std::max(upper,c.upper*n);
        if(c.upper>=winner->ll-margin || &c==&*winner)
          out.stop_detail.candidates.push_back({{c.fit.mu,c.fit.sigma,c.fit.xi},c.ll*n,c.upper*n});
      }
      out.stop_detail.loglik_upper=upper+n*margin*(1+std::abs(upper/n));
    }
  }
  return out;
}

// An unresolved certificate does not invalidate a successful numerical search.
// This is the existing scalar search over the same profile, with its own
// convergence test and the R6 total shape-score gate. It supplies no proof
// of global coverage. The open Frechet seam is never a finite fallback.
inline GevResult gev_joint_certificate(span data,double tol,int maxit,bool frechet=false) {
  auto result=gev_joint_certificate_impl(data,tol,maxit,frechet);
  if(result.status==GevStatus::Ok)return result;
  const double lower=frechet?0.:-1.;
  auto objective=[&](double xi) {
    const auto p=gev_profile(data,xi,1e-12,maxit);
    return p.fit.status==GevStatus::Ok?p.fit.logLik:-INFINITY;
  };
  const auto search=extreme_detail::maximize(objective,lower,1,1e-12,maxit);
  auto acceptable=[&](const GevProfile& point,bool reverse_face) {
    if(point.fit.status!=GevStatus::Ok || !std::isfinite(point.fit.logLik) ||
       (frechet&&!(point.fit.xi>0)))return false;
    if(point.fit.xi== -1)return reverse_face;
    if(point.fit.xi==1)return point.score>= -1e-3;
    return std::abs(point.score)<1e-3;
  };
  GevProfile p{{0,0,0,-INFINITY,0,GevStatus::IterationFailure},NAN,NAN};
  bool found=false;
  if(search.ok&&std::isfinite(search.value)) {
    p=gev_profile(data,search.x,1e-12,maxit);found=acceptable(p,true);
  }
  if(!found) {
    // A value-only scan can select the seam when a narrow finite maximum
    // improves its value by less than floating-point resolution. Preserve
    // an existing converged stationary proposal instead of losing it.
    for(const auto& c:result.stop_detail.candidates) {
      if(c.coef.size()!=3 || c.coef[2]<=lower || c.coef[2]>1)continue;
      const auto q=gev_profile(data,c.coef[2],1e-12,maxit);
      if(acceptable(q,false)&&q.curvature<0&&(!found||q.fit.logLik>p.fit.logLik)) {p=q;found=true;}
    }
  }
  if(!found)return result;
  // A strictly better certified feasible witness refutes this search.
  const double margin=4096*std::numeric_limits<double>::epsilon()*(data.size()+std::abs(p.fit.logLik));
  for(const auto& c:result.stop_detail.candidates)
    if(c.loglik_lower>p.fit.logLik+margin) {
      result.status=GevStatus::SearchContradicted;
      result.stop_detail.budget_exhausted=false;result.stop_detail.comparison_tie=false;
      result.stop_detail.candidates.push_back({{p.fit.mu,p.fit.sigma,p.fit.xi},p.fit.logLik-margin,p.fit.logLik+margin});
      return result;
    }
  p.fit.certified=false;p.fit.stop_detail={};
  return p.fit;
}
