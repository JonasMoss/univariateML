// Included by mle.hpp after the scalar and alpha-held helpers.
#pragma once
namespace cppunivariate {
namespace bnbinom_detail {
// PairBox reuses the four-field rectangle, with (rl,rh) denoting alpha.
inline AlphaLikelihood pair_bound(const Data &d,double r,const AlphaBox &q) {
  CPPUNIV_EXTENDED_REAL value=d.n*zero(r,q.rh,q.bl),mag=std::abs(value);
  for(std::size_t i=0;i<d.x.size();++i) {
    const double x=d.x[i];
    const double u=static_cast<double>(increment(r,x));
    const double v=static_cast<double>(increment(q.bh,x));
    const double w=static_cast<double>(increment(r+q.rl+q.bh,x));
    const double f=special::lgamma(x+1);
    value+=d.count[i]*(u+v-w-f);
    mag+=d.count[i]*(std::abs(u)+std::abs(v)+std::abs(w)+std::abs(f));
  }
  const double ll=static_cast<double>(value);
  return {ll,1e-10*d.n*(1+std::abs(ll/d.n))+512*std::numeric_limits<double>::epsilon()*static_cast<double>(mag)};
}
inline std::array<AlphaHessian,2> pair_hessian_bounds(const Data &d,double r,const AlphaBox &q) {
  // aa = psi1(a+r)-psi1(a) + psi1(a+b)-E psi1(a+b+r+X)
  // ab = psi1(a+b)-E psi1(a+b+r+X)
  // bb = psi1(a+b)-psi1(b)+E[psi1(b+X)-psi1(a+b+r+X)].
  const double sl=q.rl+q.bl,sh=q.rh+q.bh;
  const double al=special::trigamma(q.rh+r)-special::trigamma(q.rl);
  const double ah=special::trigamma(q.rl+r)-special::trigamma(q.rh);
  AlphaHessian lo{al+special::trigamma(sh),special::trigamma(sh),special::trigamma(sh)-special::trigamma(q.bl)};
  AlphaHessian hi{ah+special::trigamma(sl),special::trigamma(sl),special::trigamma(sl)-special::trigamma(q.bh)};
  double mag=std::abs(al)+std::abs(ah)+2*(special::trigamma(q.rl)+special::trigamma(q.bl)+special::trigamma(sl));
  for(std::size_t i=0;i<d.x.size();++i) {
    const double w=d.count[i]/d.n,x=d.x[i];
    const double tl=special::trigamma(sh+r+x),th=special::trigamma(sl+r+x);
    lo.rr-=w*th;hi.rr-=w*tl;lo.rb-=w*th;hi.rb-=w*tl;
    lo.bb+=w*(special::trigamma(q.bh+x)-th);
    hi.bb+=w*(special::trigamma(q.bl+x)-tl);
    mag+=w*(2*th+special::trigamma(q.bl+x));
  }
  const double e=512*std::numeric_limits<double>::epsilon()*mag;
  lo.rr-=e;lo.rb-=e;lo.bb-=e;hi.rr+=e;hi.rb+=e;hi.bb+=e;
  return {lo,hi};
}
inline AlphaHessian pair_hessian(const Data &d,double r,double a,double b) {
  // Midpoint of the interval evaluated at a point; its rounding margin
  // cancels here and remains present in the contraction verification.
  const auto h=pair_hessian_bounds(d,r,{a,a,b,b});
  return {(h[0].rr+h[1].rr)/2,(h[0].rb+h[1].rb)/2,(h[0].bb+h[1].bb)/2};
}
inline double pair_root_error(const Data &d,const std::vector<double> &p,double tol) {
  const double radius=std::min(tol,1e-8)*.25,da=p[1]*radius,db=p[2]*radius;
  const auto h=pair_hessian_bounds(d,p[0],{p[1]-da,p[1]+da,p[2]-db,p[2]+db});
  const auto a=scalar_score(d,p,1),b=scalar_score(d,p,2);
  const double aa=std::max(std::abs(h[0].rr),std::abs(h[1].rr));
  const double ab=std::max(std::abs(h[0].rb),std::abs(h[1].rb));
  const double bb=std::max(std::abs(h[0].bb),std::abs(h[1].bb));
  return pair_bound(d,p[0],{p[1],p[1],p[2],p[2]}).margin+
    d.n*((std::abs(a.value)+a.margin)*da+(std::abs(b.value)+b.margin)*db+.5*(aa*da*da+2*ab*da*db+bb*db*db));
}
inline bool pair_isolate(const Cost &c, std::vector<double> &p, AlphaBox &box,
                          double tol,int maxit,int &iterations) {
  // Newton only proposes a center. The interval contraction below must prove
  // existence, uniqueness and coefficient accuracy independently of this loop.
  for(int it=0;it<maxit;++it) {
    if(iterations<std::numeric_limits<int>::max())++iterations;
    const auto g=scalar_score(c.d,p,1), h=scalar_score(c.d,p,2);
    const auto H=pair_hessian(c.d,p[0],p[1],p[2]);
    const double det=H.rr*H.bb-H.rb*H.rb;
    if(!(H.rr<0 && H.bb<0 && det>0) || !std::isfinite(det))return false;
    const double dr=(H.bb*g.value-H.rb*h.value)/det;
    const double db=(H.rr*h.value-H.rb*g.value)/det;
    if(!std::isfinite(dr) || !std::isfinite(db))return false;
    if(std::max(std::abs(dr)/p[1],std::abs(db)/p[2])<std::min(tol,1e-8)*.01)break;
    if(std::abs(dr)>p[1]*.25 || std::abs(db)>p[2]*.25)return false;
    p[1]-=dr;p[2]-=db;
  }
  const auto g=scalar_score(c.d,p,1), h=scalar_score(c.d,p,2);
  const auto H=pair_hessian(c.d,p[0],p[1],p[2]);
  const double det=H.rr*H.bb-H.rb*H.rb;
  if(!(H.rr<0 && H.bb<0 && det>0))return false;
  const double m00=H.bb/det,m01=-H.rb/det,m11=H.rr/det;
  const auto contraction=[&](double radius,AlphaBox &q) {
    q={p[1]*(1-radius),p[1]*(1+radius),p[2]*(1-radius),p[2]*(1+radius)};
    const auto bounds=pair_hessian_bounds(c.d,p[0],q);
    const auto lo=bounds[0],hi=bounds[1];
    // Strict negative definiteness throughout the box makes its stationary
    // point the unique box maximum, including its evaluated center.
    if(!(hi.rr<0 && hi.bb<0 && hi.rr*hi.bb>std::max(lo.rb*lo.rb,hi.rb*hi.rb)))return false;
    const auto product=[](double m,double l,double u) {return m>=0?std::array<double,2>{m*l,m*u}:std::array<double,2>{m*u,m*l};};
    const auto bound=[&](double a0,double a1,double l0,double u0,double l1,double u1,double identity) {
      const auto v=product(a0,l0,u0),w=product(a1,l1,u1);
      return std::max(std::abs(identity-v[0]-w[0]),std::abs(identity-v[1]-w[1]));
    };
    const double e00=bound(m00,m01,lo.rr,hi.rr,lo.rb,hi.rb,1);
    const double e01=bound(m00,m01,lo.rb,hi.rb,lo.bb,hi.bb,0);
    const double e10=bound(m01,m11,lo.rr,hi.rr,lo.rb,hi.rb,0);
    const double e11=bound(m01,m11,lo.rb,hi.rb,lo.bb,hi.bb,1);
    const double er=std::abs(m00*g.value+m01*h.value)+std::abs(m00)*g.margin+std::abs(m01)*h.margin;
    const double eb=std::abs(m01*g.value+m11*h.value)+std::abs(m01)*g.margin+std::abs(m11)*h.margin;
    // The weighted infinity norm is used both for the mapping and contraction.
    const double cr=e00+e01*p[2]/p[1], cb=e11+e10*p[1]/p[2];
    return std::isfinite(cr+cb+er+eb) && cr<1 && cb<1 &&
      er/p[1]+cr*radius<radius && eb/p[2]+cb*radius<radius;
  };
  AlphaBox small;
  if(!contraction(std::min(tol,1e-8)*.25,small))return false;
  double radius=.01;
  for(int it=0;it<maxit;++it) {
    if(contraction(radius,box))return std::isfinite(likelihood(c.d,p)) &&
      std::isfinite(pair_root_error(c.d,p,tol));
    radius*=.5;
    if(radius<std::min(tol,1e-8)*.25)break;
  }
  return false;
}
// Uniform NB-face remainder on p=a/(a+b) in [pmin,1-qmin].
// |log (z)_c - c log z| <= c(c+2)/(2z), c,z>0.
inline double pair_nb_remainder(const Data &d,double r,double pmin,double qmin) {
  CPPUNIV_EXTENDED_REAL c=0;
  for(std::size_t i=0;i<d.x.size();++i) {
    const double x=d.x[i];
    c+=d.count[i]*(r*(r+2)/(2*pmin)+x*(x+2)/(2*qmin)+(r+x)*(r+x+2)/2);
  }
  const double value=static_cast<double>(c);
  return value*(1+512*std::numeric_limits<double>::epsilon());
}
// A supporting-hyperplane certificate over all mixtures of the face law.
// The derivative of its directional-likelihood polynomial has at most one
// positive root when its nonzero coefficients change sign once, + to -.
inline bool mixture_face(const Data &d,double r,bool poisson=false) {
  double mean=0;for(std::size_t i=0;i<d.x.size();++i)mean+=d.count[i]*d.x[i]/d.n;
  std::map<long long,std::array<double,2>> coefficients;
  for(std::size_t i=0;i<d.x.size();++i) {
    const long long k=d.x[i];const double n=d.count[i];
    const double negative=n*(poisson?mean:(k+r)*mean/(r+mean));
    coefficients[k][0]-=negative;coefficients[k][1]+=std::abs(negative);
    if(k>0) {const double positive=k*n;coefficients[k-1][0]+=positive;coefficients[k-1][1]+=positive;}
  }
  bool positive=false,negative=false;
  for(const auto &entry:coefficients) {
    const double value=entry.second[0],margin=512*std::numeric_limits<double>::epsilon()*entry.second[1];
    if(!std::isfinite(value+margin))return false;
    if(value>margin) {if(negative)return false;positive=true;}
    else if(value < -margin)negative=true;
    else return false;
  }
  return positive && negative;
}
inline BnbinomResult pair_fit(const Cost &c,double tol,int maxit,double face,double prob) {
  BnbinomResult out{NAN,NAN,NAN,NAN,0,BnbinomStatus::NumericalFailure};
  const double r=c.fixed[0],eps=std::numeric_limits<double>::epsilon();
  const long long cap=certificate_budget(maxit);
  long long evaluations=0;
  const double face_error=1e-10*c.d.n*(1+std::abs(face/c.d.n))+512*eps*(1+std::abs(face));
  std::vector<double> best{r,1,1};double best_ll=likelihood(c.d,best);
  std::vector<AlphaRoot> roots;
  double global_upper=0;
  const auto error=[&](const std::vector<double> &p) {return pair_bound(c.d,r,{p[1],p[1],p[2],p[2]}).margin;};
  const auto stop=[&](bool budget,bool tie=false) {
    out.status=budget?BnbinomStatus::IterationFailure:(tie?BnbinomStatus::ComparisonTie:BnbinomStatus::NumericalFailure);
    out.stop_detail.budget_exhausted=budget;out.stop_detail.comparison_tie=tie;
    out.stop_detail= [&]() {auto detail=out.stop_detail;
      detail.limit_model=StopModel::Nbinom;detail.limit_coef={r,prob};detail.limit_loglik=face;
      detail.candidates.push_back({{r,prob},face-face_error,face+face_error,StopModel::Nbinom});
      if(std::isfinite(best_ll) && std::isfinite(error(best)))
        detail.candidates.push_back({best,best_ll-error(best),best_ll+error(best),StopModel::None});
      for(const auto &root:roots)detail.candidates.push_back({root.p,root.ll-root.error,root.ll+root.error,StopModel::None});
      detail.loglik_lower=std::max(face-face_error,best_ll-error(best));
      detail.loglik_upper=global_upper;return detail;}();
    out.logLik=best_ll;return out;
  };
  // Supporting-hyperplane bound over all mixtures, with strict inequality
  // for the nondegenerate beta mixing distribution.
  if(mixture_face(c.d,r)) {
    out.status=BnbinomStatus::NegativeBinomialLimit;out.logLik=face;
    out.stop_detail=discrete_face(StopModel::Nbinom,{r,prob},face);return out;
  }
  const auto add_root=[&](std::vector<double> p) {
    AlphaBox q;
    if(!pair_isolate(c,p,q,tol,maxit,out.iterations))return;
    for(const auto &root:roots)
      if(p[1]*(1-std::min(tol,1e-8)*.25)>root.box.rl && p[1]*(1+std::min(tol,1e-8)*.25)<root.box.rh &&
         p[2]*(1-std::min(tol,1e-8)*.25)>root.box.bl && p[2]*(1+std::min(tol,1e-8)*.25)<root.box.bh)return;
    const double ll=likelihood(c.d,p),e=pair_root_error(c.d,p,tol);
    if(std::isfinite(ll+e))roots.push_back({q,p,ll,e});
    if(ll>best_ll){best=p;best_ll=ll;}
  };
  double mean=0;for(std::size_t i=0;i<c.d.x.size();++i)mean+=c.d.count[i]*c.d.x[i]/c.d.n;
  for(double b:{.15,1.,6.,40.})for(double a:{.3,2.,12.}) {
    if(++evaluations>cap)return stop(true);
    const auto u=optimise(c,{std::log(a*(1+r*b/(1+mean))),std::log(b)},tol,maxit,out.iterations);
    const auto p=c.decode(u);const double ll=likelihood(c.d,p);
    if(std::isfinite(ll) && ll>best_ll){best=p;best_ll=ll;}
    add_root(p);
  }
  if(!std::isfinite(best_ll) || !(best_ll-error(best)>face+face_error))return stop(false);
  const double lower_ll=best_ll-error(best);
  // Exclude small beta independently of alpha, by p_x <= (beta)_x/x!.
  double lower=1;
  while(true) {
    if(++evaluations>cap)return stop(true);
    double upper_ll=0,mag=0;
    for(std::size_t i=0;i<c.d.x.size();++i)if(c.d.x[i]>0) {
      const double u=static_cast<double>(increment(lower,c.d.x[i])),v=special::lgamma(c.d.x[i]+1.);
      upper_ll+=c.d.count[i]*std::min(0.,u-v);mag+=c.d.count[i]*(std::abs(u)+std::abs(v));
    }
    if(upper_ll+1e-10*c.d.n*(1+std::abs(upper_ll/c.d.n))+512*eps*mag<lower_ll)break;
    lower*=.5;if(!(lower>0))return stop(false);
  }
  // Jensen: p_x <= NBcoef(r,x)*p^min(r,1); Pr(X>0)<=max(r,1)*(1-p).
  double pmin=.5,qmin=.5;
  for(int side=0;side<2;++side)while(true) {
    if(++evaluations>cap)return stop(true);
    double upper_ll=0,mag=0;
    const double v=side?qmin:pmin;
    for(std::size_t i=0;i<c.d.x.size();++i) {
      double term=0;
      if(side) {if(c.d.x[i]>0)term=std::min(0.,std::log(std::max(1.,r))+std::log(v));}
      else term=std::min(0.,static_cast<double>(increment(r,c.d.x[i]))-special::lgamma(c.d.x[i]+1.)+std::min(1.,r)*std::log(v));
      upper_ll+=c.d.count[i]*term;mag+=c.d.count[i]*std::abs(term);
    }
    if(upper_ll+1e-10*c.d.n*(1+std::abs(upper_ll/c.d.n))+512*eps*mag<lower_ll)break;
    if(side){qmin*=.5;if(!(qmin>0))return stop(false);}
    else {pmin*=.5;if(!(pmin>0))return stop(false);}
  }
  const double remainder=pair_nb_remainder(c.d,r,pmin,qmin);
  const double upper=2*remainder/(lower_ll-face-face_error);
  const double alower=pmin*lower;
  if(!(upper>lower && alower>0) || !std::isfinite(upper))return stop(false);
  std::vector<AlphaBox> pending{{alower,upper,lower,upper}};
  while(!pending.empty()) {
    if(++evaluations>cap)return stop(true);
    const auto q=pending.back();pending.pop_back();
    // A nonfinite denominator must not turn a box-ratio upper bound into
    // zero and incorrectly discard an unresolved region.
    if(!std::isfinite(q.rh+q.bl) || !std::isfinite(q.bh+q.rl))return stop(false);
    if(q.rh/(q.rh+q.bl)<pmin || q.bh/(q.bh+q.rl)<qmin || q.rl+q.bl>=upper)continue;
    const auto bound=pair_bound(c.d,r,q);
    if(!std::isfinite(bound.value+bound.margin))return stop(false);
    if(bound.value+bound.margin<lower_ll)continue;
    bool covered=false;
    for(const auto &root:roots)if(q.rl>=root.box.rl && q.rh<=root.box.rh && q.bl>=root.box.bl && q.bh<=root.box.bh){covered=true;break;}
    if(covered)continue;
    // Positive mixed derivative and scalar single crossing give the two
    // extremal corners for coordinate-score signs (not score magnitudes).
    const std::vector<double> low{r,q.rl,q.bh},high{r,q.rh,q.bl};
    const auto amin=scalar_score(c.d,high,1),amax=scalar_score(c.d,low,1);
    const auto bmin=scalar_score(c.d,low,2),bmax=scalar_score(c.d,high,2);
    if(amin.value>amin.margin || amax.value < -amax.margin || bmin.value>bmin.margin || bmax.value < -bmax.margin)continue;
    if(!std::isfinite(amin.value+amax.value+bmin.value+bmax.value))return stop(false);
    const double wa=std::log(q.rh)-std::log(q.rl),wb=std::log(q.bh)-std::log(q.bl);
    if(std::max(wa,wb)<.1) {
      add_root({r,std::sqrt(q.rl)*std::sqrt(q.rh),std::sqrt(q.bl)*std::sqrt(q.bh)});
      for(const auto &root:roots)if(q.rl>=root.box.rl && q.rh<=root.box.rh && q.bl>=root.box.bl && q.bh<=root.box.bh){covered=true;break;}
      if(covered)continue;
    }
    const double amid=std::exp((std::log(q.rl)+std::log(q.rh))/2),bmid=std::exp((std::log(q.bl)+std::log(q.bh))/2);
    if(wa>=wb) {
      if(!(amid>q.rl && amid<q.rh))return stop(false);
      pending.push_back({q.rl,amid,q.bl,q.bh});pending.push_back({amid,q.rh,q.bl,q.bh});
    } else {
      if(!(bmid>q.bl && bmid<q.bh))return stop(false);
      pending.push_back({q.rl,q.rh,q.bl,bmid});pending.push_back({q.rl,q.rh,bmid,q.bh});
    }
  }
  if(roots.empty())return stop(false);
  std::size_t winner=0;
  for(std::size_t i=1;i<roots.size();++i)if(roots[i].ll>roots[winner].ll)winner=i;
  for(std::size_t i=0;i<roots.size();++i)if(i!=winner && roots[i].ll+roots[i].error>=roots[winner].ll-roots[winner].error)return stop(false,true);
  global_upper=face+face_error;
  for(const auto &root:roots)global_upper=std::max(global_upper,root.ll+root.error);
  out=stop(false);out.status=BnbinomStatus::Ok;
  const auto &root=roots[winner];out.size=r;out.alpha=root.p[1];out.beta=root.p[2];out.logLik=root.ll;
  out.stop_detail.loglik_lower=root.ll-root.error;out.stop_detail.loglik_upper=root.ll+root.error;
  return out;
}
} // namespace bnbinom_detail
} // namespace cppunivariate
