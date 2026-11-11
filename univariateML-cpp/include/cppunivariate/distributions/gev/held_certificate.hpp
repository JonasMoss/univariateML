#pragma once
// Included inside cppunivariate, after joint_certificate.hpp.
namespace gev_held_detail {
// The joint cover's interval arithmetic and compensated sums are shared here.
struct Jet {
  gev_joint_detail::Range v,g[2],h[3]; // xx, xy, yy
};
inline Jet constant(double v) {return {{v,v},{{0,0},{0,0}},{{0,0},{0,0},{0,0}}};}
inline Jet variable(gev_joint_detail::Range v,int i) {auto a=constant(0);a.v=v;a.g[i]={1,1};return a;}
inline Jet add(Jet a,Jet b) {
  a.v=gev_joint_detail::add(a.v,b.v);
  for(int i=0;i<2;++i)a.g[i]=gev_joint_detail::add(a.g[i],b.g[i]);
  for(int i=0;i<3;++i)a.h[i]=gev_joint_detail::add(a.h[i],b.h[i]);
  return a;
}
inline Jet scale(Jet a,double b) {
  a.v=gev_joint_detail::scale(a.v,b);
  for(auto& g:a.g)g=gev_joint_detail::scale(g,b);
  for(auto& h:a.h)h=gev_joint_detail::scale(h,b);
  return a;
}
inline Jet mul(Jet a,Jet b) {
  Jet c=constant(0);c.v=gev_joint_detail::mul(a.v,b.v);
  for(int i=0;i<2;++i)c.g[i]=gev_joint_detail::add(gev_joint_detail::mul(a.g[i],b.v),gev_joint_detail::mul(a.v,b.g[i]));
  for(int k=0;k<3;++k) {
    const int i=k==2?1:0,j=k==0?0:1;
    c.h[k]=gev_joint_detail::add(gev_joint_detail::add(gev_joint_detail::mul(a.h[k],b.v),gev_joint_detail::mul(a.v,b.h[k])),
      gev_joint_detail::add(gev_joint_detail::mul(a.g[i],b.g[j]),gev_joint_detail::mul(a.g[j],b.g[i])));
  }return c;
}
inline Jet compose(Jet a,gev_joint_detail::Range v,gev_joint_detail::Range d,gev_joint_detail::Range dd) {
  Jet c=a;c.v=v;
  for(int i=0;i<2;++i)c.g[i]=gev_joint_detail::mul(d,a.g[i]);
  for(int k=0;k<3;++k) {
    const int i=k==2?1:0,j=k==0?0:1;
    c.h[k]=gev_joint_detail::add(gev_joint_detail::mul(d,a.h[k]),gev_joint_detail::mul(dd,gev_joint_detail::mul(a.g[i],a.g[j])));
  }return c;
}
inline Jet logarithm(Jet a) {
  return compose(a,gev_joint_detail::log(a.v),gev_joint_detail::inv(a.v),gev_joint_detail::neg(gev_joint_detail::square(gev_joint_detail::inv(a.v))));
}
inline Jet inverse(Jet a) {
  const auto r=gev_joint_detail::inv(a.v);
  return compose(a,r,gev_joint_detail::neg(gev_joint_detail::square(r)),gev_joint_detail::scale(gev_joint_detail::mul(gev_joint_detail::square(r),r),2));
}
inline Jet exponential(Jet a) {const auto e=gev_joint_detail::exp(a.v);return compose(a,e,e,e);}
inline bool finite(Jet a) {
  if(!gev_joint_detail::finite(a.v))return false;
  for(auto g:a.g)if(!gev_joint_detail::finite(g))return false;
  for(auto h:a.h)if(!gev_joint_detail::finite(h))return false;
  return true;
}
inline Jet margin(Jet a) {
  const double eps=128*std::numeric_limits<double>::epsilon();
  a.v=gev_joint_detail::pad(a.v,eps*(1+gev_joint_detail::magnitude(a.v)));
  for(auto& g:a.g)g=gev_joint_detail::pad(g,eps*(1+gev_joint_detail::magnitude(g)));
  for(auto& h:a.h)h=gev_joint_detail::pad(h,eps*(1+gev_joint_detail::magnitude(h)));
  return a;
}
// Compensate every interval endpoint, as in the joint profile jets.
struct Accumulator {
  gev_joint_detail::Sum value,g[2],h[3];
  void add(Jet a) {
    value.add(a.v);
    for(int i=0;i<2;++i)g[i].add(a.g[i]);
    for(int i=0;i<3;++i)h[i].add(a.h[i]);
  }
  Jet mean(double n) const {
    Jet out=constant(0);out.v=value.value();
    for(int i=0;i<2;++i)out.g[i]=g[i].value();
    for(int i=0;i<3;++i)out.h[i]=h[i].value();
    return scale(out,1/n);
  }
};
struct Box {gev_joint_detail::Range x,y;};
struct Evaluation {Jet jet{};double upper=INFINITY;bool ok=false;};
// Stable two-variable log1p(x*z)/x. Integral corner jets retain correlation
// between powers of x and z, including negative z and the Gumbel seam.
inline Jet ratio(Jet x,Jet z,int maxit,bool& ok) {
  const auto q=mul(x,z);const double r=gev_joint_detail::magnitude(q.v);
  if(r>.25) {
    if(x.v.lo<=0&&x.v.hi>=0) {ok=false;return constant(0);}
    return mul(logarithm(add(constant(1),q)),inverse(x));
  }
  // Scalar shape derivatives have integral representations. They are
  // monotone in x; z derivatives are elementary rational expressions.
  auto scalar=[&](double X,double Z) {
    double l=Z,lx=-Z*Z/2,lxx=2*Z*Z*Z/3,p=1;
    Jet a=constant(0);bool done=Z==0;
    for(int j=1;!done&&j<=maxit;++j) {
      p*= -X*Z;l+=Z*p/(j+1);lx-=Z*Z*p*(j+1.)/(j+2.);
      lxx+=Z*Z*Z*p*(j+1.)*(j+2.)/(j+3.);
      const double R=std::abs(X*Z),power=std::pow(R,j+1);
      const double e0=std::abs(Z)*power/((j+2)*(1-R));
      const double e1=Z*Z*power/(1-R);
      const double e2=std::abs(Z*Z*Z)*power*((j+3)/(1-R)+R/((1-R)*(1-R)));
      if(std::max({e0,e1,e2})<=2*std::numeric_limits<double>::epsilon()) {
        a.v=gev_joint_detail::pad({l,l},e0);a.g[0]=gev_joint_detail::pad({lx,lx},e1);
        a.h[0]=gev_joint_detail::pad({lxx,lxx},e2);done=true;
      }
    }
    if(!done)ok=false;
    return a;
  };
  // L_x=-integral t*z^2/(1+xzt)^2 dt is nonpositive; L_xx has
  // the sign of z. Their extrema in z occur at endpoints or z=0.
  Jet a=scalar(x.v.lo,z.v.lo);
  auto merge=[&](Jet b) {
    a.v={std::min(a.v.lo,b.v.lo),std::max(a.v.hi,b.v.hi)};
    a.g[0]={std::min(a.g[0].lo,b.g[0].lo),std::max(a.g[0].hi,b.g[0].hi)};
    a.h[0]={std::min(a.h[0].lo,b.h[0].lo),std::max(a.h[0].hi,b.h[0].hi)};
  };
  merge(scalar(x.v.lo,z.v.hi));merge(scalar(x.v.hi,z.v.lo));merge(scalar(x.v.hi,z.v.hi));
  if(z.v.lo<=0&&z.v.hi>=0)merge(constant(0));
  const auto t=gev_joint_detail::add({1,1},q.v),iv=gev_joint_detail::inv(t);
  const auto lz=iv,lzz=gev_joint_detail::neg(gev_joint_detail::mul(x.v,gev_joint_detail::square(iv)));
  const auto lxz=gev_joint_detail::neg(gev_joint_detail::mul(z.v,gev_joint_detail::square(iv)));
  Jet out=constant(0);out.v=a.v;
  for(int i=0;i<2;++i)out.g[i]=gev_joint_detail::add(gev_joint_detail::mul(a.g[0],x.g[i]),gev_joint_detail::mul(lz,z.g[i]));
  for(int k=0;k<3;++k) {
    const int i=k==2?1:0,j=k==0?0:1;
    out.h[k]=gev_joint_detail::add(gev_joint_detail::add(gev_joint_detail::mul(a.g[0],x.h[k]),gev_joint_detail::mul(lz,z.h[k])),
      gev_joint_detail::add(gev_joint_detail::mul(a.h[0],gev_joint_detail::mul(x.g[i],x.g[j])),
        gev_joint_detail::add(gev_joint_detail::mul(lzz,gev_joint_detail::mul(z.g[i],z.g[j])),
          gev_joint_detail::mul(lxz,gev_joint_detail::add(gev_joint_detail::mul(x.g[i],z.g[j]),gev_joint_detail::mul(z.g[i],x.g[j]))))));
  }return out;
}
inline Evaluation gev_evaluate(span data,Box b,bool held_mu,double hold,int maxit) {
  if(!held_mu && b.x.lo>=0) {
    // Uniform enclosure of the unique held-scale location root. At zero
    // the Gumbel root is in the same interval by its exponential-sum formula.
    const double m=data[0];double k=0;for(double v:data)if(v==m)++k;
    const double high=m+hold*std::log(2*data.size()/k);
    const double error=128*std::numeric_limits<double>::epsilon()*(1+std::abs(m)+std::abs(high));
    if(b.y.lo!=b.y.hi && (b.y.hi<m-error || b.y.lo>high+error))return {{},-INFINITY,false};
  }
  const Jet xi=variable(b.x,0),u=variable(b.y,1);
  const Jet sigma=held_mu?exponential(u):constant(hold),mu=held_mu?constant(hold):u;
  // Value envelope also covers boxes crossing the moving support. For
  // c=1+xi, c*log(y)-y is convex in c and concave in log(y).
  gev_joint_detail::Sum upper_sum;bool smooth=true;
  for(double d:data) {
    const auto z=mul(add(constant(d),scale(mu,-1)),inverse(sigma));
    const auto t=add(constant(1),mul(xi,z));
    if(t.v.hi<0)return {{},-INFINITY,false};
    double bound=0;
    if((b.x.hi<0||b.x.lo>0)&&t.v.hi>0) {
      double lo=INFINITY,hi=-INFINITY;
      for(double X:{b.x.lo,b.x.hi}) {
        const double a=-std::log(t.v.hi)/X;
        const double b=t.v.lo>0?-std::log(t.v.lo)/X:(X<0?-INFINITY:INFINITY);
        lo=std::min({lo,a,b});hi=std::max({hi,a,b});
      }
      bound=-INFINITY;
      for(double c:{1+b.x.lo,1+b.x.hi}) {
        const double optimum=c==0?-INFINITY:std::log(c);
        const double q=std::max(lo,std::min(hi,optimum));
        const double v=c==0?-std::exp(q):c*q-std::exp(q);
        bound=std::max(bound,v);
      }
    }
    upper_sum.add(gev_joint_detail::point(bound-std::log(sigma.v.lo)));
    smooth=smooth&&t.v.lo>0;
  }
  double upper=upper_sum.value().hi/data.size();
  upper+=512*std::numeric_limits<double>::epsilon()*(1+std::abs(upper));
  if(!held_mu&&b.x.lo>=-1&&b.x.hi<=0&&b.y.lo>data[0]) {
    // At the minimum observation, u=-L>=q/(1+a*q), where
    // q=(mu-min)/sigma and a=-xi. Other standardized densities are <=1.
    // For u>=4, u-exp(u)<=-u*u/4. This also covers xi=0 without
    // forming its enormous exponential or any derivative of it.
    const double q=(b.y.lo-data[0])/hold;
    double u=q/(1-b.x.lo*q);
    if(std::isfinite(u)) {
      u=std::min(u,std::sqrt(std::numeric_limits<double>::max())/2);
      u-=128*std::numeric_limits<double>::epsilon()*(1+std::abs(u));
      if(u>=4) {
        const double tail=-std::log(hold)-(u*u/4)/data.size();
        const double error=512*std::numeric_limits<double>::epsilon()*(1+std::abs(tail));
        upper=std::min(upper,tail+error);
      }
    }
  }
  if(!smooth)return {{},upper,false};
  Accumulator sum;bool ok=true;
  for(double d:data) {
    const Jet z=mul(add(constant(d),scale(mu,-1)),inverse(sigma));
    const auto t=add(constant(1),mul(xi,z));
    if(t.v.hi<0)return {{},-INFINITY,false};
    if(!(t.v.lo>0))return {}; // support face: subdivide; never prune unchecked
    const Jet L=ratio(xi,z,maxit,ok);if(!ok)return {{},upper,false};
    sum.add(scale(add(add(logarithm(sigma),mul(add(constant(1),xi),L)),exponential(scale(L,-1))),-1));
  }
  const Jet result=margin(sum.mean(data.size()));
  return {result,std::isfinite(result.v.hi)?std::min(upper,result.v.hi):upper,finite(result)};
}
inline bool concave(Jet j) {
  return j.h[0].hi<0 && j.h[2].hi<0 &&
    gev_joint_detail::sub(gev_joint_detail::mul(j.h[0],j.h[2]),gev_joint_detail::square(j.h[1])).lo>0;
}
inline bool inside(gev_joint_detail::Range a,gev_joint_detail::Range b) {return a.lo>=b.lo&&a.hi<=b.hi;}
struct Cover {bool certified=false,contradicted=false;Box root{},competitor{};std::int64_t evaluations=0;};
// Krawczyk inclusion gives existence; negative Hessian gives uniqueness.
// Interval Newton then encloses all coefficients. The remaining boxes are
// removed by values, scores, or inclusion in this strictly concave basin.
template<class F,class P>
inline Cover cover(Box domain,double x,double y,double tol,int maxit,F evaluate,P precise,bool profile=false,Box face_basin={},double face_value=NAN,double root_log_fraction=NAN,gev_joint_detail::Range profile_region={-INFINITY,INFINITY},double profile_scale=NAN) {
  Cover out;const auto cap=certificate_budget(maxit);
  auto get=[&](Box b) {if(out.evaluations>=cap)return Evaluation{};++out.evaluations;return evaluate(b);};
  x=std::max(domain.x.lo,std::min(domain.x.hi,x));y=std::max(domain.y.lo,std::min(domain.y.hi,y));
  Box basin{},root{};Evaluation isolated{},witness{};
  if(std::isfinite(face_value)) {
    basin=face_basin;root={{x,x},{y,y}};
    witness.ok=isolated.ok=true;
    witness.jet=isolated.jet=constant(face_value);
    witness.jet=isolated.jet=margin(witness.jet);
  } else {
  // Newton only proposes; all acceptance comes from subsequent inclusions.
  for(int it=0;it<maxit&&out.evaluations<cap;++it) {
    const auto p=get({{x,x},{y,y}});if(!p.ok)break;
    const double a=(p.jet.h[0].lo+p.jet.h[0].hi)/2,b=(p.jet.h[1].lo+p.jet.h[1].hi)/2,c=(p.jet.h[2].lo+p.jet.h[2].hi)/2;
    const double gx=(p.jet.g[0].lo+p.jet.g[0].hi)/2,gy=(p.jet.g[1].lo+p.jet.g[1].hi)/2,det=a*c-b*b;
    if(!(a<0&&c<0&&det>0))break;
    const bool edge=(x==domain.x.lo&&gx<0)||(x==domain.x.hi&&gx>0);
    const double dx=edge?0:(c*gx-b*gy)/det,dy=edge?gy/c:(a*gy-b*gx)/det;
    bool accepted=false;
    double step=1;
    for(int j=0;j<maxit&&out.evaluations<cap;++j) {
      const double nx=std::max(domain.x.lo,std::min(domain.x.hi,x-step*dx)),ny=y-step*dy;
      const auto q=ny>domain.y.lo&&ny<domain.y.hi?get({{nx,nx},{ny,ny}}):Evaluation{};
      if(q.ok&&q.jet.v.lo>=p.jet.v.lo-1e-14) {accepted=true;x=nx;y=ny;break;}step/=2;
    }
    if(!accepted||std::max(std::abs(dx),std::abs(dy))<1e-13)break;
  }
  const double initial=(x==domain.x.lo||x==domain.x.hi)?.02:.000002;
  double rx=initial*(1+std::abs(x)),ry=initial*(1+std::abs(y));
  basin={{std::max(domain.x.lo,x-rx),std::min(domain.x.hi,x+rx)},
            {std::max(domain.y.lo,y-ry),std::min(domain.y.hi,y+ry)}};
  auto B=get(basin);const auto p=get({{x,x},{y,y}});if(!p.ok)return out;
  const bool boundary_proposal=(x==domain.x.lo&&p.jet.g[0].hi<0)||
                               (x==domain.x.hi&&p.jet.g[0].lo>0);
  const auto basin_ok=[&]() {
    if(!B.ok)return false;
    if(boundary_proposal)return
      ((x==domain.x.lo&&B.jet.g[0].hi<0)||(x==domain.x.hi&&B.jet.g[0].lo>0))&&B.jet.h[2].hi<0;
    return concave(B.jet);
  };
  for(int j=0;j<maxit&&out.evaluations<cap&&!basin_ok();++j) {
    rx/=4;ry/=4;
    basin={{std::max(domain.x.lo,x-rx),std::min(domain.x.hi,x+rx)},
           {std::max(domain.y.lo,y-ry),std::min(domain.y.hi,y+ry)}};
    B=get(basin);
  }
  if(!basin_ok())return out;
  const bool left=x==domain.x.lo&&B.jet.g[0].hi<0,right=x==domain.x.hi&&B.jet.g[0].lo>0;
  const bool edge=left||right;
  root=basin;bool included=false;
  if(edge) {
    root.x={x,x};const auto L=get({root.x,{basin.y.lo,basin.y.lo}}),H=get({root.x,{basin.y.hi,basin.y.hi}});
    included=L.ok&&H.ok&&L.jet.g[1].lo>0&&H.jet.g[1].hi<0;
  } else {
    const double a=(p.jet.h[0].lo+p.jet.h[0].hi)/2,b=(p.jet.h[1].lo+p.jet.h[1].hi)/2,c=(p.jet.h[2].lo+p.jet.h[2].hi)/2,det=a*c-b*b;
    const double C[2][2]={{c/det,-b/det},{-b/det,a/det}};
    gev_joint_detail::Range K[2];
    for(int i=0;i<2;++i) {
      K[i]=gev_joint_detail::point(i==0?x:y);
      for(int j=0;j<2;++j)K[i]=gev_joint_detail::sub(K[i],gev_joint_detail::scale(p.jet.g[j],C[i][j]));
      for(int j=0;j<2;++j) {
        auto R=gev_joint_detail::point(i==j?1:0);
        for(int k=0;k<2;++k)R=gev_joint_detail::sub(R,gev_joint_detail::scale(B.jet.h[k+j],C[i][k]));
        K[i]=gev_joint_detail::add(K[i],gev_joint_detail::mul(R,j==0?gev_joint_detail::Range{basin.x.lo-x,basin.x.hi-x}:gev_joint_detail::Range{basin.y.lo-y,basin.y.hi-y}));
      }
    }
    included=K[0].lo>basin.x.lo&&K[0].hi<basin.x.hi&&K[1].lo>basin.y.lo&&K[1].hi<basin.y.hi;
    if(included)root={K[0],K[1]};
  }
  if(!included)return out;
  bool solved=false;
  for(int it=0;it<maxit&&out.evaluations+2<cap;++it) {
    if(precise(root,tol)) {solved=true;break;}
    const auto h=get(root);const double X=(root.x.lo+root.x.hi)/2,Y=(root.y.lo+root.y.hi)/2;
    const auto m=get({{X,X},{Y,Y}});if(!h.ok||!m.ok)break;
    auto nx=root.x,ny=root.y;
    if(edge)ny=gev_joint_detail::sub({Y,Y},gev_joint_detail::div(m.jet.g[1],h.jet.h[2]));
    else {
      const auto det=gev_joint_detail::sub(gev_joint_detail::mul(h.jet.h[0],h.jet.h[2]),gev_joint_detail::square(h.jet.h[1]));
      if(!(det.lo>0))break;
      nx=gev_joint_detail::sub({X,X},gev_joint_detail::div(gev_joint_detail::sub(gev_joint_detail::mul(h.jet.h[2],m.jet.g[0]),gev_joint_detail::mul(h.jet.h[1],m.jet.g[1])),det));
      ny=gev_joint_detail::sub({Y,Y},gev_joint_detail::div(gev_joint_detail::sub(gev_joint_detail::mul(h.jet.h[0],m.jet.g[1]),gev_joint_detail::mul(h.jet.h[1],m.jet.g[0])),det));
    }
    Box next{{std::max(root.x.lo,nx.lo),std::min(root.x.hi,nx.hi)},{std::max(root.y.lo,ny.lo),std::min(root.y.hi,ny.hi)}};
    if(next.x.lo>next.x.hi||next.y.lo>next.y.hi)break;
    root=next;
  }
  if(!solved)return out;
  const double X=(root.x.lo+root.x.hi)/2,Y=(root.y.lo+root.y.hi)/2;
  witness=get({{X,X},{Y,Y}});if(!witness.ok)return out;
  isolated=get(root);if(!isolated.ok)return out;
  }
  const double incumbent=witness.jet.v.lo,slack=512*std::numeric_limits<double>::epsilon()*(1+std::abs(incumbent));
  // Cut along the basin edges so discarded boxes never straddle its boundary.
  std::vector<Box> pending;
  const double xs[4]={domain.x.lo,basin.x.lo,basin.x.hi,domain.x.hi},ys[4]={domain.y.lo,basin.y.lo,basin.y.hi,domain.y.hi};
  for(int i=0;i<3;++i)if(xs[i]<xs[i+1]) {
    if(profile && xs[i]<0 && xs[i+1]>0) {
      pending.push_back({{0,xs[i+1]},domain.y});
      for(int j=0;j<3;++j)if(ys[j]<ys[j+1]&&!(i==1&&j==1))pending.push_back({{xs[i],0},{ys[j],ys[j+1]}});
    } else if(profile && xs[i]>=0)pending.push_back({{xs[i],xs[i+1]},domain.y});
    else for(int j=0;j<3;++j)if(ys[j]<ys[j+1]&&!(i==1&&j==1))
      pending.push_back({{xs[i],xs[i+1]},{ys[j],ys[j+1]}});
  }
  while(!pending.empty()&&out.evaluations<cap) {
    Box b=pending.back();pending.pop_back();
    const bool profiled=profile && b.x.lo>=0;
    if(profiled) {
      b.y.lo=std::max(b.y.lo,profile_region.lo);b.y.hi=std::min(b.y.hi,profile_region.hi);
      if(std::isfinite(profile_scale)&&b.x.lo>0) {
        const double logratio=(profile_region.hi-profile_region.lo)/profile_scale;
        const double high=profile_region.lo-profile_scale*std::expm1(-b.x.lo*logratio)/b.x.lo;
        const double error=128*std::numeric_limits<double>::epsilon()*(1+std::abs(high)+profile_scale);
        b.y.hi=std::min(b.y.hi,high+error);
      }
      if(std::isfinite(root_log_fraction)) {
        const double low=(root_log_fraction-std::log1p(1/b.x.lo))/b.x.lo;
        const double error=128*std::numeric_limits<double>::epsilon()*(1+std::abs(low));
        b.y.lo=std::max(b.y.lo,low-error);
        b.y.hi=std::min(b.y.hi,0.);
      }
      if(b.y.lo>b.y.hi)continue;
      // For each shape there is exactly one inner root, with score positive
      // below and negative above it. Uniform signs discard half-lines even
      // when nuisance curvature is not negative on the initial rectangle.
      for(int it=0;it<maxit&&out.evaluations+2<cap;++it) {
        const double mid=(b.y.lo+b.y.hi)/2;
        const auto m=get({b.x,{mid,mid}});
        auto next=b.y;
        if(m.ok&&m.jet.g[1].lo>0)next.lo=mid;
        else if(m.ok&&m.jet.g[1].hi<0)next.hi=mid;
        else {
          const auto h=get(b);
          if(m.ok&&h.ok&&h.jet.h[2].hi<0) {
            const auto N=gev_joint_detail::sub({mid,mid},gev_joint_detail::div(m.jet.g[1],h.jet.h[2]));
            next={std::max(next.lo,N.lo),std::min(next.hi,N.hi)};
          }
          // The midpoint may itself be a root while curvature on the
          // initial bracket changes sign. Quarter-point signs still give
          // a contraction; splitting shape alone would stagnate here.
          if(next.hi-next.lo>=.9*(b.y.hi-b.y.lo)&&out.evaluations+2<cap) {
            const double l=(b.y.lo+mid)/2,r=(mid+b.y.hi)/2;
            const auto L=get({b.x,{l,l}}),R=get({b.x,{r,r}});
            if(L.ok&&L.jet.g[1].lo>0)next.lo=std::max(next.lo,l);
            if(R.ok&&R.jet.g[1].hi<0)next.hi=std::min(next.hi,r);
          }
        }
        if(next.lo>next.hi) {b.y=next;break;}
        const double old=b.y.hi-b.y.lo;b.y=next;
        if(b.y.hi-b.y.lo>=.9*old)break;
      }
      if(b.y.lo>b.y.hi)continue;
      if(inside(b.x,basin.x)&&inside(b.y,basin.y))continue;
    }
    if(inside(b.x,basin.x)&&inside(b.y,basin.y))continue;
    const auto e=get(b);
    if(e.upper<incumbent-slack)continue;
    if(e.ok) {
      const double lower_margin=4096*std::numeric_limits<double>::epsilon()*
        (1+std::abs(isolated.jet.v.hi)+std::abs(e.jet.v.lo));
      if(e.jet.v.lo>isolated.jet.v.hi+lower_margin) {
        const double mx=(b.x.lo+b.x.hi)/2,my=(b.y.lo+b.y.hi)/2;
        out.contradicted=true;out.root=root;out.competitor={{mx,mx},{my,my}};return out;
      }
      if(e.jet.g[1].lo>0||e.jet.g[1].hi<0)continue;
      if((e.jet.g[0].lo>0&&b.x.hi<domain.x.hi)||(e.jet.g[0].hi<0&&b.x.lo>domain.x.lo))continue;
      // Tangent plus Hessian remainder is much tighter than natural values.
      const double mx=(b.x.lo+b.x.hi)/2,my=(b.y.lo+b.y.hi)/2;
      const auto m=get({{mx,mx},{my,my}});
      // A strict feasible counterexample disproves this candidate. It must
      // never be confused with an incomplete cover of an unrefuted search.
      const double comparison_margin=4096*std::numeric_limits<double>::epsilon()*
        (1+std::abs(isolated.jet.v.hi)+(m.ok?std::abs(m.jet.v.lo):0));
      if(m.ok&&m.jet.v.lo>isolated.jet.v.hi+comparison_margin) {
        out.contradicted=true;out.root=root;out.competitor={{mx,mx},{my,my}};return out;
      }
      const double dx=(b.x.hi-b.x.lo)/2,dy=(b.y.hi-b.y.lo)/2;
      if(m.ok&&m.jet.v.hi+gev_joint_detail::magnitude(m.jet.g[0])*dx+gev_joint_detail::magnitude(m.jet.g[1])*dy+
          .5*std::max(0.,e.jet.h[0].hi)*dx*dx+gev_joint_detail::magnitude(e.jet.h[1])*dx*dy+.5*std::max(0.,e.jet.h[2].hi)*dy*dy<incumbent-slack)continue;
    }
    const bool splitx=profiled || (b.x.hi-b.x.lo)/(domain.x.hi-domain.x.lo)>=(b.y.hi-b.y.lo)/(domain.y.hi-domain.y.lo);
    const double mid=splitx?(b.x.lo+b.x.hi)/2:(b.y.lo+b.y.hi)/2;
    if(splitx) {if(mid==b.x.lo||mid==b.x.hi) {out.root=b;return out;}pending.push_back({{mid,b.x.hi},b.y});pending.push_back({{b.x.lo,mid},b.y});}
    else {if(mid==b.y.lo||mid==b.y.hi) {out.root=b;return out;}pending.push_back({b.x,{mid,b.y.hi}});pending.push_back({b.x,{b.y.lo,mid}});}
  }
  if(pending.empty()) {out.certified=true;out.root=root;}return out;
}
} // namespace gev_held_detail

inline GevResult gev_held_certificate(span data,double mu,double sigma,double tol,int maxit,GevResult fallback,
                                      double public_origin,double public_unit) {
  if(fallback.status!=GevStatus::Ok||!std::isfinite(fallback.logLik)||
     tol<1024*std::numeric_limits<double>::epsilon()||!(public_unit>0)||
     !std::isfinite(public_unit)||!std::isfinite(public_origin))return fallback;
  bool supported=true;
  for(double d:data)if(!gev_public_contains(d,fallback.mu,fallback.sigma,fallback.xi))supported=false;
  const bool hm=!std::isnan(mu);const double n=static_cast<double>(data.size());
  // An exact member is available analytically at an active support corner,
  // even if its rounded stored coordinates fail the strict support predicate.
  double face_scale=NAN,S=0,L=0,H=0,k=0,face_height=NAN;
  if(fallback.xi== -1) {
    const double M=data[data.size()-1],s0=hm?M-mu:sigma;
    if(s0>0&&(hm?fallback.sigma==s0:fallback.mu==M-sigma)) {
      gev_joint_detail::Sum sum_d,sum_log,sum_inverse;
      for(double v:data) {
        const double d=(M-v)/s0;sum_d.add(gev_joint_detail::point(d));
        if(d==0)++k;else {sum_log.add(gev_joint_detail::point(std::log(d)));sum_inverse.add(gev_joint_detail::point(1/d));}
      }
      face_scale=s0;S=sum_d.value().lo;L=sum_log.value().lo;H=sum_inverse.value().hi;
      face_height=-std::log(s0)-S/n;
    }
  }
  if(!supported&&!std::isfinite(face_height))return fallback;
  const double analytic=n*face_height;
  const double level_loglik=std::isfinite(analytic)?analytic-
    512*std::numeric_limits<double>::epsilon()*(n+std::abs(analytic)):fallback.logLik;
  const double level=level_loglik/n;
  gev_held_detail::Box domain{{-1,1},{0,0}};
  if(hm) {
    double logs=0;std::size_t nonzero=0;
    for(double d:data)if(d!=mu) {logs+=std::log(std::abs(d-mu));++nonzero;}
    if(2*nonzero>data.size())domain.y={(level_loglik+2*logs)/(2*nonzero-n),-level};
    else {
      // With data on both sides of the hold, support forces |xi|=O(sigma).
      // A negative residual then has -L>=C/sigma, uniformly in shape.
      const double below=mu-data[0],above=data[data.size()-1]-mu;
      if(!(below>0))return fallback;
      const bool straddles=above>0;
      const double C=straddles?std::min(below,above)/(1+std::min(below,above)/std::max(below,above)):below;
      double t=straddles?std::max(4.,std::sqrt(2*n)):std::max(8.,2*n);bool compact=false;
      for(int it=0;it<maxit;++it,t*=2) {
        const double bound=n*(std::log(t)-std::log(C))-(straddles?t*t/4:t/2);
        if(bound<level_loglik-1e-8*(n+std::abs(level_loglik))) {compact=true;break;}
        if(!std::isfinite(t))break;
      }
      if(!compact)return fallback;
      domain.y={std::log(C)-std::log(t),-level};
    }
  } else {
    const double radius=std::sqrt(sigma*std::exp(-level));
    domain.y={data[0]-radius,data[data.size()-1]+radius};
  }
  if(!gev_joint_detail::finite(domain.y)||!(domain.y.lo<domain.y.hi))return fallback;
  // Strictly outside the level set; these boundaries cannot be maximizers.
  domain.y=gev_joint_detail::pad(domain.y,1e-8*(1+gev_joint_detail::magnitude(domain.y)));
  auto eval=[&](gev_held_detail::Box b) {return gev_held_detail::gev_evaluate(data,b,hm,hm?mu:sigma,maxit);};
  auto precise=[&](gev_held_detail::Box b,double t) {
    const double x=(b.x.lo+b.x.hi)/2,y=(b.y.lo+b.y.hi)/2;
    return b.x.hi-b.x.lo<=t*.25*std::abs(x)&&
      (hm ? std::expm1(b.y.hi-b.y.lo)<=t*.25 :
        public_unit*(b.y.hi-b.y.lo)<=t*.25*std::max(std::abs(public_origin+public_unit*y),public_unit*sigma));
  };
  gev_held_detail::Box face_basin{};double face_value=NAN;
  // No derivative is evaluated at a zero support residual.
  if(std::isfinite(face_height)) {
    const double A=2*n-S;double c=1;
    for(int it=0;it<maxit;++it,c/=2) {
      const double a=1/(1+c);
      if(!(a<1))break;
      const double rate=hm?A/4:a*n/2;
      const double C=S+n/std::exp(1.)+L+k*(std::log(c*k/rate)-1)+(hm?n-S:0);
      if(rate>0&&c*H<(hm?A/4:a*n/2)&&C< -1e-8*(n+std::abs(L)+S)) {
        // Every feasible point here has w>=0. Extend below the support
        // floor instead of leaving roundoff-sized infeasible boxes to cover.
        face_basin={{-1,-a},hm?gev_joint_detail::Range{domain.y.lo,std::min(domain.y.hi,std::log(2*a*face_scale))}:domain.y};
        face_value=face_height;break;
      }
    }
  }
  if(!supported&&!std::isfinite(face_value))return fallback;
  double minimum_count=0;for(double v:data)if(v==data[0])++minimum_count;
  const double location_high=data[0]+sigma*std::log(2*n/minimum_count);
  const double location_margin=128*std::numeric_limits<double>::epsilon()*(1+std::abs(location_high)+sigma);
  const gev_joint_detail::Range location_roots{data[0],location_high+location_margin};
  const auto result=gev_held_detail::cover(domain,fallback.xi,hm?std::log(fallback.sigma):fallback.mu,std::min(tol,.01),maxit,eval,precise,!hm,face_basin,face_value,NAN,location_roots,sigma);
  if(result.contradicted) {
    fallback.xi=result.competitor.x.lo;
    if(hm)fallback.sigma=std::exp(result.competitor.y.lo);
    else fallback.mu=result.competitor.y.lo;
    fallback.logLik=gev_loglik(data,fallback.mu,fallback.sigma,fallback.xi);
    fallback.status=GevStatus::SearchContradicted;return fallback;
  }
  if(!result.certified)return fallback;
  const auto search=fallback;
  fallback.xi=(result.root.x.lo+result.root.x.hi)/2;
  if(hm)fallback.sigma=std::exp((result.root.y.lo+result.root.y.hi)/2);
  else fallback.mu=(result.root.y.lo+result.root.y.hi)/2;
  fallback.logLik=gev_loglik(data,fallback.mu,fallback.sigma,fallback.xi);
  if(std::isfinite(face_value)&&(!std::isfinite(fallback.logLik)||
     std::abs(fallback.logLik-n*face_value)>1e-7*n))return search;
  fallback.certified=true;return fallback;
}
