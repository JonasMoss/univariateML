#pragma once
// Included inside cppunivariate after TlambdaResult and uniform_fit.
namespace tlambda_fit_detail {
struct Obs {double lg=0,psi=0,dpsi=0,dl=0,zdpsi=0,z2dpsi=0;bool ok=true;};
// Stable expansion of -(D D''-2D'^2)/D^4, optionally multiplied
// by |z| or z^2 in log space before an underflow can lose tail curvature.
inline double curvature(double lp,double lq,double ld,double l,double weight=0) {
  const double c=1-l;
  return -c*(l*(std::exp((2*l-4)*lp-4*ld+weight)+std::exp((2*l-4)*lq-4*ld+weight))+
      (2-l)*(std::exp((l-1)*lp+(l-3)*lq-4*ld+weight)+std::exp((l-1)*lq+(l-3)*lp-4*ld+weight))+
      4*c*std::exp((l-2)*(lp+lq)-4*ld+weight));
}
inline Obs eval(double z,double l,double lp,double ld,int maxit) {
  Obs o;
  const double az=std::abs(z),lq=std::log1p(-std::exp(lp));
  const double sign=z<0?-1:1;
  const double u=std::exp((l-1)*lq-ld),v=std::exp((l-1)*lp-ld);
  double positive_psi;
  if(az==0)positive_psi=0;
  else if(l==0)positive_psi=-(1-2*std::exp(lp));
  else if(l<2)positive_psi=-(1-l)*std::exp((l-2)*lp-2*ld)*(-std::expm1((l-2)*(lq-lp)));
  else positive_psi=(1-l)*(std::exp((l-2)*lq-2*ld)-std::exp((l-2)*lp-2*ld));
  o.lg=-ld;o.psi=sign*positive_psi;
  o.dpsi=l==0?-2*std::exp(lp)*(-std::expm1(lp)):curvature(lp,lq,ld,l);
  if(az>1e100 && l!=0) {
    o.zdpsi=sign*curvature(lp,lq,ld,l,std::log(az));
    o.z2dpsi=curvature(lp,lq,ld,l,2*std::log(az));
  } else {o.zdpsi=z*o.dpsi;o.z2dpsi=z*o.zdpsi;}
  const double dl=u*lq+v*lp;
  if(std::max(std::abs(lp),std::abs(lq))*std::abs(l)>.5) {
    // Expand (D_u/D)(Q_lambda/D), so neither D nor 1/p is formed.
    const double cross1=std::exp(l*lq+(l-2)*lp-2*ld),cross2=std::exp((l-2)*lq+l*lp-2*ld);
    const double first=u*u*lq+v*v*lp-cross1*lq-cross2*lp;
    const double second=u*u+v*v-cross1-cross2;
    o.dl=-dl+(l-1)*(first/l-second/(l*l));
  } else {
    double ql=0,pu=lq*lq,pv=lp*lp,fact=2,lk=1;bool converged=false;
    for(int j=0;j<maxit;++j) {
      const int k=j+2;const double term=(k-1)*lk*(pu-pv)/fact;ql+=term;
      if(std::abs(term)<1e-18*std::max(1.,std::abs(ql)) && k>4){converged=true;break;}
      pu*=lq;pv*=lp;fact*=k+1;lk*=l;
    }
    if(!converged){o.ok=false;return o;}
    o.dl=-dl-positive_psi*ql;
  }
  return o;
}
struct Accumulator {double value=0,carry=0;void add(double x){const double y=x-carry,t=value+y;carry=(t-value)-y;value=t;}};
struct Val { double ll=-INFINITY,gu=0,gv=0,huu=0,huv=0,hvv=0,gl=0; bool ok=false; };
inline Val value(const std::vector<double>& x,double A,double B,double l,int maxit){

  Val v{-INFINITY,0,0,0,0,0,0,true};
  const double n=x.size();
  if(!(A>0)){v.ok=false;return v;}
  // Local affine frame: dA=A*u, dB=B*u-v. Thus v is the
  // location correction in fitted-scale units and u is relative inverse scale.
  // Summing its scores/Hessian directly avoids cancellation in J' H J.
  Accumulator ll{n*std::log(A)},ga{n},gb,haa{-n},hab,hbb,gl;
  for(double xi:x){
    const double mu=-B/A,sigma=1/A;
    const double gap=l>0?tlambda_detail::slack(xi,mu,sigma,l):1;
    if(l>0 && !(gap>0)){v.ok=false;return v;}
    const auto root=inverse(std::abs((xi-mu)/sigma),l,l>0?std::log(gap)-std::log(sigma):NAN,maxit);
    if(!root.ok){v.ok=false;return v;}
    const double z=(xi-mu)/sigma;
    const auto o=eval(z,l,root.logp,root.logd,maxit);
    if(!o.ok){v.ok=false;return v;}
    ll.add(o.lg);ga.add(z*o.psi);gb.add(-o.psi);haa.add(o.z2dpsi);hab.add(-o.zdpsi);hbb.add(o.dpsi);gl.add(o.dl);
  }
  v.ll=ll.value;v.gu=ga.value;v.gv=gb.value;v.huu=haa.value;v.huv=hab.value;v.hvv=hbb.value;v.gl=gl.value;
  v.ok=std::isfinite(v.ll+v.gu+v.gv+v.huu+v.huv+v.hvv+v.gl);
  return v;
}
struct Inner {double A=NAN,B=NAN;Val v{};bool ok=false;int it=0;};
inline Inner inner(const std::vector<double>& x,double A,double B,double l,double tol,int maxit,double mu=NAN,double sigma=NAN) {
  const bool hm=std::isfinite(mu),hs=std::isfinite(sigma);
  if(hs)A=1/sigma;
  if(hm)B=-mu*A;
  if(l>0 && !hs) {
    const double centre=-B/A,r=std::max(std::abs(x.front()-centre),std::abs(x.back()-centre));
    A=std::min(A,1/(1.02*l*r));B=-centre*A;
  }
  if(l>0 && hs && !hm) {
    const double lower=x.back()-sigma/l,upper=x.front()+sigma/l;
    if(!(lower<upper))return {};
    double centre=-B/A;
    if(!(centre>lower && centre<upper))centre=lower/2+upper/2;
    B=-centre*A;
  }
  Val v=value(x,A,B,l,maxit);
  if(!v.ok)return {A,B,v,false,0};
  if(hm&&hs)return {A,B,v,true,0};
  double damp=0,last_norm=INFINITY;bool last_full=false;
  for(int it=0;it<maxit;++it) {
    double ga=v.gu,gb=v.gv,aa=-v.huu,ab=-v.huv,bb=-v.hvv;
    if(hm){gb=0;ab=0;bb=1;}
    if(hs){ga=0;aa=1;ab=0;}
    const double scale=std::abs(aa)+std::abs(bb);
    double a=aa+damp,c=bb+damp,det=a*c-ab*ab;
    int guard=0;
    for(;guard<maxit && !(a>0 && c>0 && det>0);++guard) {
      damp=std::max(2*damp,1e-10*scale+1e-300);a=aa+damp;c=bb+damp;det=a*c-ab*ab;
      if(!std::isfinite(damp))return {A,B,v,false,it};
    }
    if(guard==maxit || !std::isfinite(a) || !std::isfinite(c) || !std::isfinite(det))return {A,B,v,false,it};
    double du=(c*ga-ab*gb)/det,dv=(-ab*ga+a*gb)/det;
    if(!std::isfinite(du) || !std::isfinite(dv))return {A,B,v,false,it};
    if(hm)dv=0;
    if(hs)du=0;
    const double dm=dv/(1+du),ds=-du/(1+du);
    // Undamped Newton correction checks each free coefficient. Recompute
    // after every accepted step; a damped or shortened step cannot pass.
    const double target=std::min(tol*.05,1e-12);
    const double norm=std::max(hm?0:std::abs(dm),hs?0:std::abs(ds));
    const bool quadratic=last_full && (norm<=8*last_norm*last_norm || last_norm<=10*target);
    if(damp==0 && quadratic && norm<=target)return {A,B,v,true,it};
    double step=1;bool accepted=false;Val w;
    for(int k=0;k<maxit;++k) {
      const double na=A*(1+step*du),nb=hm?-mu*na:std::fma(B,step*du,B)-step*dv;
      if(na>0){w=value(x,na,nb,l,maxit);if(w.ok && w.ll>=v.ll-8e-15*(std::abs(v.ll)+x.size())){A=na;B=nb;accepted=true;break;}}
      step/=2;if(step==0)break;
    }
    if(!accepted){last_full=false;damp=std::max(10*damp,1e-6*scale);continue;}
    last_full=damp==0 && step==1;last_norm=norm;
    v=w;damp=step==1?damp/10:damp;if(damp<1e-12*scale)damp=0;
  }
  return {A,B,v,false,maxit};
}
inline TlambdaResult profile(span x,double tol,int maxit,double l=NAN,double mu=NAN,double sigma=NAN) {
  maxit=static_cast<int>(solver_budget(maxit));
  TlambdaResult bad{NAN,NAN,l,NAN,0,TlambdaStatus::InvalidData};
  const bool hl=!std::isnan(l),hm=!std::isnan(mu),hs=!std::isnan(sigma);
  if(hl && !std::isfinite(l)){bad.status=TlambdaStatus::InvalidShape;return bad;}
  if(x.empty() || !(tol>0) || !std::isfinite(tol) || maxit<1 || (!hm && !hs && x.size()<3))return bad;
  std::vector<double> y(x.begin(),x.end());for(double v:y)if(!std::isfinite(v))return bad;std::sort(y.begin(),y.end());
  if(!hs && hm && !hl && std::count(y.begin(),y.end(),mu)){bad.status=TlambdaStatus::Unbounded;return bad;}
  if(!hm && !hs) {
    std::size_t run=1;
    for(std::size_t j=1;j<y.size();++j){run=y[j]==y[j-1]?run+1:1;if(hl&&l<0?run*std::abs(l)>=y.size()-run:2*run>=y.size())return bad;}
  }
  const double centre=y.front()/2+y.back()/2;
  const double origin=hm?mu:y[y.size()/2];
  double unit=y.back()/2-y.front()/2;
  if(hm)unit=std::max(unit,std::abs(mu-centre));
  if(hs)unit=std::max(unit,sigma);
  if(!(unit>0) || !std::isfinite(unit))return bad;
  double previous=y.front();
  for(std::size_t i=0;i<y.size();++i){const double raw=y[i];y[i]=(raw-origin)/unit;if(i && raw>previous && !(y[i]>y[i-1]))return bad;previous=raw;}
  const double m=hm?(mu-origin)/unit:NAN,s=hs?sigma/unit:NAN,n=y.size();
  const double med=y[y.size()/2],iqr=y[3*y.size()/4]-y[y.size()/4];
  struct Grid {double l;Inner r;};
  double unresolved_global=-INFINITY;
  auto at=[&](double shape,const Inner& warm) {
    Inner best;best.v.ll=-INFINITY;double unresolved=-INFINITY;
    const double q=-distributions::qtlambda(.25,0,1,shape);
    const double sc=hs?s:std::max(iqr/(2*q),1e-4);
    const auto try_start=[&](double a,double b){const auto r=inner(y,a,b,shape,tol,maxit,m,s);if(r.ok && r.v.ll>best.v.ll)best=r;else if(!r.ok && std::isfinite(r.v.ll)){unresolved=std::max(unresolved,r.v.ll);unresolved_global=std::max(unresolved_global,r.v.ll);}};
    try_start(1/sc,-(hm?m:med)/sc);
    if(shape<0 && !hm){try_start(1/sc,-y[y.size()/4]/sc);try_start(1/sc,-y[3*y.size()/4]/sc);}
    if(warm.ok)try_start(warm.A,warm.B);
    if(unresolved>best.v.ll+1e-10*x.size())best.ok=false;
    return best;
  };
  TlambdaResult out=bad;out.status=TlambdaStatus::IterationFailure;double best=-INFINITY;
  auto take=[&](double shape,const Inner& r,bool polished=false){
    if(r.ok && (r.v.ll>best || (polished && r.v.ll>=best-1e-12*n))){best=r.v.ll;out={-r.B/r.A,1/r.A,shape,best,r.it,TlambdaStatus::Ok};}
  };
  // Uniform face, respecting holds. The original-data support is checked below.
  if(!hl || l==1 || l==2) {
    const double shape=hl?l:1,centre=hm?m:y.front()/2+y.back()/2;
    const double radius=std::max(std::abs(y.front()-centre),std::abs(y.back()-centre));
    const double scale=hs?s:shape*radius;
    if(scale>0 && radius<=scale/shape){best=-n*std::log(2*scale/shape);out={centre,scale,shape,best,0,TlambdaStatus::Ok};}
    if(hl && (l==1 || l==2)) {
      if(out.status!=TlambdaStatus::Ok){bad.status=TlambdaStatus::InvalidSupport;return bad;}
    }
  }
  Inner initial;
  std::vector<double> shapes=hl?std::vector<double>{l}:std::vector<double>{-1,-.75,-.5,-.25,0,.14,.3,.5,.7,.9,.99};
  if(!hl && hs && !hm) {
    const double edge=std::min(1.,2*s/(y.back()-y.front()));
    shapes.erase(std::remove_if(shapes.begin(),shapes.end(),[&](double v){return v>=edge;}),shapes.end());
    for(double fraction:{.25,.5,.75,.9,.99,1-1e-6})shapes.push_back(edge*fraction);
    std::sort(shapes.begin(),shapes.end());
  }
  if(!hl && hm) {
    // |x-mu| f(x) < 1/(2a) gives a joint scale-independent
    // likelihood upper bound. An incumbent makes the lower shape cutoff finite.
    const auto logistic=at(0,{});
    // A known feasible logistic/uniform scale supplies a second start even
    // when the first (very negative) profile points cannot be resolved.
    if(logistic.ok)initial=logistic;
    else if(out.status==TlambdaStatus::Ok)initial={1/out.sigma,-out.mu/out.sigma,{},true,0};
    const double incumbent=logistic.ok?std::max(best,logistic.v.ll):best;
    double logs=0;for(double v:y){if(v==m){bad.status=TlambdaStatus::IterationFailure;return bad;}logs+=std::log(std::abs(v-m));}
    const double cutoff=std::exp((-incumbent-logs)/n)/2;
    if(!std::isfinite(cutoff)){bad.status=TlambdaStatus::IterationFailure;return bad;}
    double a=2;
    for(int k=0;a<cutoff && k<maxit;++k,a*=2)shapes.push_back(-a);
    if(a<cutoff){bad.status=TlambdaStatus::IterationFailure;return bad;}
    shapes.push_back(-std::max(1.,cutoff)*(1+1e-12));std::sort(shapes.begin(),shapes.end());
  }
  std::vector<Grid> grid;Inner warm=initial;
  for(double shape:shapes){auto r=at(shape,warm);grid.push_back({shape,r});if(r.ok)warm=r;if(hl || (!hm && shape==-1 && r.v.gl<=0))take(shape,r);}
  if(!std::any_of(grid.begin(),grid.end(),[](const Grid& point){return point.r.ok;}) && !((l==1 || l==2) && hl)){bad.status=TlambdaStatus::IterationFailure;return bad;}
  if(!hl)for(std::size_t j=1;j<grid.size();++j) {
    auto a=grid[j-1],b=grid[j];if(!(a.r.ok && b.r.ok && a.r.v.gl>=0 && b.r.v.gl<=0 && (a.r.v.gl>0 || b.r.v.gl<0)))continue;
    double fa=a.r.v.gl,fb=b.r.v.gl;int side=0;bool done=false;
    for(int it=0;it<maxit;++it){
      double c=b.l-fb*(b.l-a.l)/(fb-fa);
      if(!(c>a.l && c<b.l))c=a.l+(b.l-a.l)/2;
      auto r=at(c,std::abs(c-a.l)<std::abs(c-b.l)?a.r:b.r);
      if(!r.ok)break;
      const bool nuisance_width=(hm || std::abs(a.r.B/a.r.A-b.r.B/b.r.A)<=tol*.1*std::max(1/r.A,std::abs(r.B/r.A))) &&
          (hs || std::abs(1/a.r.A-1/b.r.A)<=tol*.1/r.A);
      if(b.l-a.l<=std::min(tol*.01,1e-13)*std::max(.001,std::abs(c)) && nuisance_width){take(c,r,true);done=true;break;}
      if(r.v.gl>0){a={c,r};fa=r.v.gl;if(side==-1)fb/=2;side=-1;}
      else {b={c,r};fb=r.v.gl;if(side==1)fa/=2;side=1;}
    }
    if(!done){bad.status=TlambdaStatus::IterationFailure;return bad;}
  }
  if(unresolved_global>best+1e-10*n){bad.status=TlambdaStatus::IterationFailure;return bad;}
  for(const auto& point:grid)if(point.r.ok && point.r.v.ll>best+1e-10*n){bad.status=TlambdaStatus::IterationFailure;return bad;}
  if(out.status!=TlambdaStatus::Ok)return out;
  if(out.lambda==1 && !hm && !hs) {
    auto uniform=tlambda_detail::uniform_fit(*std::min_element(x.begin(),x.end()),*std::max_element(x.begin(),x.end()),n,1,maxit,tol);
    return uniform;
  }
  const double normalized_mu=out.mu;
  out.mu=hm?mu:std::fma(unit,normalized_mu,origin);out.sigma=hs?sigma:unit*out.sigma;
  const double eps=std::numeric_limits<double>::epsilon();
  const double location_rounding=4*eps*std::abs(origin)+4*std::abs(normalized_mu)*(eps*unit);
  if(!hm && location_rounding>tol*.25*std::max(std::abs(out.mu),out.sigma)) {
    out.status=TlambdaStatus::IterationFailure;return out;
  }
  if(out.lambda==1 && !hs)for(int k=0;k<maxit && (tlambda_detail::slack(*std::min_element(x.begin(),x.end()),out.mu,out.sigma,1)<0 || tlambda_detail::slack(*std::max_element(x.begin(),x.end()),out.mu,out.sigma,1)<0);++k)out.sigma=std::nextafter(out.sigma,INFINITY);
  Accumulator ll;for(double v:x)ll.add(logdensity(v,out.mu,out.sigma,out.lambda,maxit));
  out.logLik=ll.value;if(!std::isfinite(ll.value))out.status=TlambdaStatus::IterationFailure;
  return out;
}
} // namespace tlambda_fit_detail
