// Parameter-uniform extension of the alpha-held stationary cover.
#pragma once
namespace cppunivariate {
namespace bnbinom_detail {
struct AlphaIntervalProof {
  bool proved=false,budget_exhausted=false,comparison_tie=false;
  StopDetail stop_detail{};
  double lower=NAN,upper=NAN;
  AlphaBox mode{NAN,NAN,NAN,NAN};
  long long evaluations=0;
  int iterations=0;
};
inline std::array<AlphaHessian,2> alpha_interval_hessian(const Data &d,
    double al,double ah,const AlphaBox &q) {
  AlphaHessian lo{special::trigamma(ah+q.rh)-special::trigamma(q.rl),0,
                 special::trigamma(ah+q.bh)-special::trigamma(q.bl)};
  AlphaHessian hi{special::trigamma(al+q.rl)-special::trigamma(q.rh),0,
                 special::trigamma(al+q.bl)-special::trigamma(q.bh)};
  double mag=std::abs(lo.rr)+std::abs(lo.bb)+std::abs(hi.rr)+std::abs(hi.bb)+
    2*(special::trigamma(q.rl)+special::trigamma(q.bl));
  for(std::size_t i=0;i<d.x.size();++i) {
    const double w=d.count[i]/d.n,x=d.x[i];
    const double tl=special::trigamma(ah+q.rh+q.bh+x),th=special::trigamma(al+q.rl+q.bl+x);
    lo.rr+=w*(special::trigamma(q.rh+x)-th);
    hi.rr+=w*(special::trigamma(q.rl+x)-tl);
    lo.bb+=w*(special::trigamma(q.bh+x)-th);
    hi.bb+=w*(special::trigamma(q.bl+x)-tl);
    lo.rb-=w*th;hi.rb-=w*tl;
    mag+=w*(special::trigamma(q.rl+x)+special::trigamma(q.bl+x)+2*th);
  }
  const double e=512*std::numeric_limits<double>::epsilon()*mag;
  lo.rr-=e;lo.bb-=e;lo.rb-=e;hi.rr+=e;hi.bb+=e;hi.rb+=e;
  return {lo,hi};
}
inline bool alpha_interval_contraction(const Data &d,double al,double ah,
    const std::vector<double> &p,const AlphaBox &q) {
  const auto center=alpha_hessian(d,p[1],p[0],p[2]);
  const double det=center.rr*center.bb-center.rb*center.rb;
  if(!(center.rr<0 && center.bb<0 && det>0))return false;
  const double m00=center.bb/det,m01=-center.rb/det,m11=center.rr/det;
  const auto h=alpha_interval_hessian(d,al,ah,q);
  const auto lo=h[0],hi=h[1];
  if(!(hi.rr<0 && hi.bb<0 && hi.rr*hi.bb>std::max(lo.rb*lo.rb,hi.rb*hi.rb)))return false;
  const auto product=[](double m,double l,double u) {
    return m>=0?std::array<double,2>{m*l,m*u}:std::array<double,2>{m*u,m*l};
  };
  const auto bound=[&](double a,double b,double l0,double u0,double l1,double u1,double identity) {
    const auto v=product(a,l0,u0),w=product(b,l1,u1);
    return std::max(std::abs(identity-v[0]-w[0]),std::abs(identity-v[1]-w[1]));
  };
  const double e00=bound(m00,m01,lo.rr,hi.rr,lo.rb,hi.rb,1);
  const double e01=bound(m00,m01,lo.rb,hi.rb,lo.bb,hi.bb,0);
  const double e10=bound(m01,m11,lo.rr,hi.rr,lo.rb,hi.rb,0);
  const double e11=bound(m01,m11,lo.rb,hi.rb,lo.bb,hi.bb,1);
  // Both coordinate scores increase with alpha. Bound the residual of the
  // same preconditioned mapping for every alpha in the interval.
  const auto rl=scalar_score(d,{p[0],al,p[2]},0),rh=scalar_score(d,{p[0],ah,p[2]},0);
  const auto bl=scalar_score(d,{p[0],al,p[2]},2),bh=scalar_score(d,{p[0],ah,p[2]},2);
  const auto residual=[&](double a,double b) {
    const auto r=product(a,rl.value-rl.margin,rh.value+rh.margin);
    const auto s=product(b,bl.value-bl.margin,bh.value+bh.margin);
    return std::max(std::abs(r[0]+s[0]),std::abs(r[1]+s[1]));
  };
  const double dr=std::min(p[0]-q.rl,q.rh-p[0]),db=std::min(p[2]-q.bl,q.bh-p[2]);
  if(!(dr>0 && db>0))return false;
  const double cr=e00+e01*db/dr,cb=e11+e10*dr/db;
  return std::isfinite(cr+cb) && cr<1 && cb<1 &&
    residual(m00,m01)+cr*dr<dr && residual(m01,m11)+cb*db<db;
}
inline AlphaLikelihood alpha_interval_bound(const Data &d,double al,double ah,const AlphaBox &q) {
  // p0 increases in alpha; the recurrence ratio decreases in alpha.
  auto out=alpha_likelihood_bound(d,al,q);
  const double low=zero(q.rl,al,q.bl),high=zero(q.rl,ah,q.bl);
  out.value+=d.n*(high-low);
  out.margin+=1e-10*d.n*(1+std::abs(low)+std::abs(high));
  return out;
}
inline AlphaIntervalProof alpha_interval_profile(const Data &d,
    const std::vector<double> &proposal,double al,double ah,double tol,int maxit) {
  AlphaIntervalProof out;out.lower=al;out.upper=ah;
  maxit=static_cast<int>(solver_budget(maxit));
  if(maxit<1 || !(tol>0) || !std::isfinite(tol))return out;
  if(!(al>0 && al<=proposal[1] && proposal[1]<=ah) || !std::isfinite(ah))return out;
  const long long cap=certificate_budget(maxit);
  const auto spend=[&]() {return ++out.evaluations<=cap;};
  // The fixed-alpha certificate supplies a global inner mode at the center.
  const Cost c{d,{NAN,proposal[1],NAN},{0,2}};
  const auto fit=alpha_fit(c,tol,maxit);
  out.iterations=fit.iterations;
  if(fit.status!=BnbinomStatus::Ok) {
    out.budget_exhausted=fit.stop_detail.budget_exhausted;
    out.comparison_tie=fit.status==BnbinomStatus::ComparisonTie;
    out.stop_detail=fit.stop_detail;return out;
  }
  std::vector<double> p{fit.size,proposal[1],fit.beta};AlphaBox root;
  if(!alpha_isolate(c,p,root,tol,maxit,out.iterations) ||
     !alpha_interval_contraction(d,al,ah,p,root))return out;
  // The returned finite proposal must lie in the certified inner branch;
  // certifying some other conditional mode would not validate this proposal.
  const double pr=std::min(proposal[0],proposal[2]),pb=std::max(proposal[0],proposal[2]);
  if(pr<root.rl || pr>root.rh || pb<root.bl || pb>root.bh)return out;
  // A single uniformly isolated identified branch must cover every surviving
  // box. Other branches, bifurcations and unresolved seams cause a stop.
  const double low_ll=likelihood(d,{p[0],al,p[2]}),high_ll=likelihood(d,{p[0],ah,p[2]});
  // Pointwise alpha curvature is not assumed. A monotone-factor lower bound
  // combines p0(al) with count denominators(ah), uniformly across the slab.
  const double zl=zero(p[0],al,p[2]),zh=zero(p[0],ah,p[2]);
  const double witness=high_ll+d.n*(zl-zh)-
    1e-9*d.n*(1+std::abs(zl)+std::abs(zh)+std::abs(low_ll/d.n)+std::abs(high_ll/d.n));
  if(!std::isfinite(witness))return out;
  double lower=1;
  while(true) {
    if(!spend()){out.budget_exhausted=true;return out;}
    double upper=0,mag=0;
    for(std::size_t i=0;i<d.x.size();++i)if(d.x[i]>0) {
      const double u=static_cast<double>(increment(lower,d.x[i])),v=special::lgamma(d.x[i]+1.);
      upper+=d.count[i]*std::min(0.,u-v);mag+=d.count[i]*(std::abs(u)+std::abs(v));
    }
    if(upper+1e-10*d.n*(1+std::abs(upper/d.n))+512*std::numeric_limits<double>::epsilon()*mag<witness)break;
    lower*=.5;if(!(lower>0))return out;
  }
  double upper=std::max(1.,2*lower);
  while(true) {
    if(!spend()){out.budget_exhausted=true;return out;}
    const auto s=scalar_score(d,{upper,ah,lower},0);
    if(s.value < -s.margin)break;
    upper*=2;if(!std::isfinite(upper))return out;
  }
  std::vector<AlphaBox> pending{{lower,upper,lower,upper}};
  while(!pending.empty()) {
    if(!spend()){out.budget_exhausted=true;return out;}
    const auto q=pending.back();pending.pop_back();
    if(q.rl>q.bh)continue;
    if(q.rl>=root.rl && q.rh<=root.rh && q.bl>=root.bl && q.bh<=root.bh)continue;
    const auto bound=alpha_interval_bound(d,al,ah,q);
    if(!std::isfinite(bound.value+bound.margin))return out;
    if(bound.value+bound.margin<witness)continue;
    bool excluded=false;
    for(int j:{0,2}) {
      const auto low=scalar_score(d,{q.rl,ah,q.bl},j),high=scalar_score(d,{q.rh,al,q.bh},j);
      if(low.value < -low.margin || high.value>high.margin){excluded=true;break;}
    }
    if(excluded)continue;
    const double wr=std::log(q.rh)-std::log(q.rl),wb=std::log(q.bh)-std::log(q.bl);
    if(wr>=wb) {
      const double mid=std::sqrt(q.rl)*std::sqrt(q.rh);
      if(!(q.rl<mid && mid<q.rh))return out;
      pending.push_back({q.rl,mid,q.bl,q.bh});pending.push_back({mid,q.rh,q.bl,q.bh});
    } else {
      const double mid=std::sqrt(q.bl)*std::sqrt(q.bh);
      if(!(q.bl<mid && mid<q.bh))return out;
      pending.push_back({q.rl,q.rh,q.bl,mid});pending.push_back({q.rl,q.rh,mid,q.bh});
    }
  }
  out.mode=root;out.proved=true;return out;
}
} // namespace bnbinom_detail
} // namespace cppunivariate
