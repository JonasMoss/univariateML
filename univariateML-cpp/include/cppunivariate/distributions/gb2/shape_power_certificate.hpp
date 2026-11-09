// Included after gb2_detail. One beta shape and scale held: global power
// profile enclosure, including every stationary point and both exteriors.
#pragma once
namespace cppunivariate {
namespace gb2_shape_power_detail {
struct Root { double lo=NAN,hi=NAN; bool ok=false; };
struct Terms {
  double a=0,sm=0,sp=0,em=0,ep=0,tm=0,tp=0,etm=0,etp=0;
  bool ok=false;
};
struct Point {
  double a=0,p=0,ll=-INFINITY,error=INFINITY,ga=0,gp=0,ea=INFINITY,egp=INFINITY;
  bool ok=false;
};
struct Cell { double lo,hi,pl,pu,upper; };
struct Context {
  std::vector<double> z,ez;
  double q,b,n,tol,logs=0,log_error=0,abslogs=0;
  double left=0,right=0,el=0,er=0,absolute=0;
  std::size_t ties=0,negative=0,positive=0;
  bool inverse,ok=true,budget=false,constant=true;
  int maxit;
  std::int64_t evaluations=0;
  Point best;
  double face=-INFINITY,face_error=0;
  bool face_valid=false;
  const double eps=std::numeric_limits<double>::epsilon();
  Context(span x,span lx,double held,double scale,double tolerance,int iterations,bool inv)
    :q(held),b(scale),n(static_cast<double>(x.size())),tol(std::min(tolerance,1e-9)),inverse(inv),maxit(iterations) {
    betapr_cert_detail::Sum sl,sa,se,sleft,sright,sabs;
    const double lb=std::log(b);
    for(std::size_t i=0;i<x.size();++i) {
      const double w=inverse?lb-lx[i]:lx[i]-lb;
      const double e=x[i]==b?0:8*eps*(1+std::abs(lx[i])+std::abs(lb));
      z.push_back(x[i]==b?0:w);ez.push_back(e);
      sl.add(lx[i]);sa.add(std::abs(lx[i]));se.add(8*eps*(1+std::abs(lx[i])));
      sabs.add(std::abs(w)+e);
      constant=constant&&x[i]==x[0];
      if(x[i]==b)++ties;
      else if(inverse?x[i]>b:x[i]<b) {++negative;sleft.add(-w);el+=e;}
      else {++positive;sright.add(w);er+=e;}
    }
    logs=sl.value;abslogs=sa.value;log_error=se.value;
    left=sleft.value;right=sright.value;absolute=sabs.value;
    if(!(n>0)||!std::isfinite(absolute)||!std::isfinite(logs))ok=false;
  }
  bool charge() {
    if(evaluations>=certificate_budget(maxit)){budget=true;ok=false;return false;}
    ++evaluations;return true;
  }
  double difference(double p,double& error) const {
    const double ratio=q/p;
    const double l=std::isfinite(ratio)?std::log1p(ratio):std::log(q)-std::log(p)+std::log1p(p/q);
    const double d=trgamma_detail::log_minus_digamma(p);
    const double s=trgamma_detail::log_minus_digamma(p+q);
    const double v=l+d-s;
    error=128*eps*(std::abs(l)+std::abs(d)+std::abs(s)+(std::isfinite(ratio)?0:std::abs(std::log(q))+std::abs(std::log(p))))+8*std::numeric_limits<double>::denorm_min();
    return v;
  }
  Root root(double s,double e) const {
    Root out;
    if(!(s>e)||!std::isfinite(s)||!std::isfinite(e))return out;
    const double le=s>50?s:std::log(std::expm1(s));
    const double u=-std::log(s),v=std::log(q)-le,m=std::max(u,v);
    const double center=m+std::log(std::exp(u-m)+std::exp(v-m));
    const double lower=std::log(std::numeric_limits<double>::min());
    const double upper=std::log(std::numeric_limits<double>::max())-2*eps;
    double lo=std::max(lower,center-1),hi=std::min(upper,center+1);
    const auto score=[&](double w,double& error) {
      double de=0;const double d=difference(std::exp(w),de);
      error=de+e+16*eps*(std::abs(d)+s);return d-s;
    };
    bool bracket=false;
    for(int i=0;i<maxit;++i) {
      double elow=0,ehigh=0;const double gl=score(lo,elow),gh=score(hi,ehigh);
      if(std::isfinite(gl)&&std::isfinite(gh)&&gl>elow&&gh< -ehigh){bracket=true;break;}
      if(!(gl>elow))lo=std::max(lower,lo-4);
      if(!(gh< -ehigh))hi=std::min(upper,hi+4);
      if(lo==lower&&hi==upper)break;
    }
    if(!bracket)return out;
    const double target=std::min(1e-12,tol/64);
    for(int i=0;i<maxit;++i) {
      if(hi-lo<target)break;
      const double mid=lo/2+hi/2;
      double error=0;const double g=score(mid,error);
      if(!std::isfinite(g)||!std::isfinite(error))return out;
      if(g>error)lo=mid;
      else if(g< -error)hi=mid;
      else {
        const double pl=std::exp(lo),pu=std::exp(hi);
        const double r=1/(1+pu/q);
        const double derivative=(1-128*eps)*std::max(r,(pl/pu)/pu*r*(2-r));
        if(!(derivative>0)||!std::isfinite(derivative))break;
        const double width=(std::abs(g)+error)/derivative;
        const double nl=std::max(lo,mid-width),nh=std::min(hi,mid+width);
        if(!(nh>nl))return out;
        const double old=hi-lo;lo=nl;hi=nh;
        if(hi-lo>.99*old)break;
      }
    }
    const double pad=128*eps*(1+std::abs(lo)+std::abs(hi));
    out.lo=std::exp(lo)*(1-pad);out.hi=std::exp(hi)*(1+pad);
    out.ok=out.lo>0&&std::isfinite(out.hi)&&out.hi>=out.lo;
    return out;
  }
  Terms terms(double a) {
    Terms t;t.a=a;if(!charge()||!(a>0)||!std::isfinite(a))return t;
    betapr_cert_detail::Sum sm,sp,em,ep,tm,tp,etm,etp;
    for(std::size_t i=0;i<z.size();++i) {
      const double v=a*z[i],dv=a*ez[i]+8*eps*std::abs(v);
      if(!std::isfinite(v)||!std::isfinite(dv))return t;
      const double ev=std::exp(-std::abs(v)),lp=std::log1p(ev);
      const double m=std::max(-v,0.)+lp,p=std::max(v,0.)+lp;
      const double minus=v>=0?ev/(1+ev):1/(1+ev),plus=v>=0?1/(1+ev):ev/(1+ev);
      const double absmin=std::max(0.,std::abs(v)-dv),e=absmin==std::abs(v)?ev:std::exp(-absmin);
      const double du=std::min(1.,dv*e/((1+e)*(1+e))+8*std::numeric_limits<double>::denorm_min());
      const double mm=std::min(1.,minus+du+8*eps*minus),mp=std::min(1.,plus+du+8*eps*plus);
      sm.add(m);sp.add(p);tm.add(z[i]*minus);tp.add(z[i]*plus);
      em.add(dv*mm+16*eps*m+8*std::numeric_limits<double>::denorm_min());
      ep.add(dv*mp+16*eps*p+8*std::numeric_limits<double>::denorm_min());
      etm.add(ez[i]*mm+(std::abs(z[i])+ez[i])*du+16*eps*std::abs(z[i]*minus));
      etp.add(ez[i]*mp+(std::abs(z[i])+ez[i])*du+16*eps*std::abs(z[i]*plus));
    }
    t.sm=sm.value;t.sp=sp.value;t.em=em.value;t.ep=ep.value;
    t.tm=tm.value;t.tp=tp.value;t.etm=etm.value;t.etp=etp.value;
    t.ok=std::isfinite(t.sm)&&std::isfinite(t.sp)&&std::isfinite(t.em)&&std::isfinite(t.ep);
    return t;
  }
  double beta_error(double p,double value) const {
    const double l1=std::log1p(q/p),l2=std::log1p(p/q);
    return 128*eps*(1+std::abs(value)+10*(1+std::abs(std::log(p))+std::abs(std::log(q)))+p*l1+q*l2);
  }
  Point point(const Terms& t,double p) const {
    Point v;v.a=t.a;v.p=p;
    if(!t.ok||!(p>0)||!std::isfinite(p+q))return v;
    const double lb=gb2_detail::logbeta(p,q);
    v.ll=n*std::log(t.a)-n*lb-p*t.sm-q*t.sp-logs;
    v.error=log_error+p*t.em+q*t.ep+n*beta_error(p,lb)+128*eps*(1+abslogs+n*(std::abs(std::log(t.a))+std::abs(lb))+p*t.sm+q*t.sp);
    double de=0;const double d=difference(p,de);
    v.ga=n/t.a+p*t.tm-q*t.tp;v.gp=n*d-t.sm;
    v.ea=p*t.etm+q*t.etp+32*eps*(n/t.a+std::abs(p*t.tm)+std::abs(q*t.tp));
    v.egp=n*de+t.em+32*eps*(n*std::abs(d)+t.sm);
    v.ok=std::isfinite(v.ll)&&std::isfinite(v.error)&&std::isfinite(v.ga)&&std::isfinite(v.ea)&&std::isfinite(v.gp)&&std::isfinite(v.egp);
    return v;
  }
  void consider(const Point& p) {
    if(p.ok&&p.ll-p.error>best.ll-best.error)best=p;
  }
  bool cell(double lo,double hi,Cell& out,bool& keep) {
    keep=false;
    const double a=std::exp(std::log(lo)/2+std::log(hi)/2);
    if(!(a>lo&&a<hi)){ok=false;return false;}
    const Terms l=terms(lo),r=terms(hi),c=terms(a);
    if(!l.ok||!r.ok||!c.ok){ok=false;return false;}
    const double ha=std::max(a-lo,hi-a);
    double smin=0;
    if(positive==0)smin=l.sm-l.em;
    else if(negative==0)smin=r.sm-r.em;
    else {
      const double known=(negative+ties)*(std::log(2.)-128*eps);
      smin=std::max(known,c.sm-c.em-(std::abs(c.tm)+c.etm)*ha);
    }
    const double smax=std::max(l.sm+l.em,r.sm+r.em);
    const Root lower=root(smax/n,32*eps*smax/n),upper=root(smin/n,32*eps*std::abs(smin)/n);
    const Root middle=root(c.sm/n,c.em/n);
    if(!lower.ok||!upper.ok||!middle.ok){ok=false;return false;}
    const double pl=lower.lo,pu=upper.hi;
    if(!(pu>=pl)){ok=false;return false;}
    const double p=std::exp(std::log(middle.lo)/2+std::log(middle.hi)/2);
    const Point center=point(c,p);consider(center);
    const Point ll=point(l,pl),lu=point(l,pu),rl=point(r,pl),ru=point(r,pu);
    if(!center.ok||!ll.ok||!lu.ok||!rl.ok||!ru.ok){ok=false;return false;}
    // For fixed p, the a score decreases. For fixed a it is affine in p.
    // Strict one-sign corners exclude every stationary point in the cell.
    const double gl=std::min(rl.ga-rl.ea,ru.ga-ru.ea);
    const double gh=std::max(ll.ga+ll.ea,lu.ga+lu.ea);
    if(gl>0||gh<0)return true;
    double nl=lo,nh=hi,npl=pl,npu=pu;
    const double cross=std::max(std::abs(l.tm)+l.etm,std::abs(r.tm)+r.etm);
    // Verified Schur-complement curvature permits interval Newton only
    // inside cells where the entire power profile is strictly concave.
    if(!charge())return false;
    betapr_cert_detail::Sum wl,wu;
    for(std::size_t i=0;i<z.size();++i) {
      const double zl=std::max(0.,std::abs(z[i])-ez[i])*(1-16*eps);
      const double zu=(std::abs(z[i])+ez[i])*(1+16*eps);
      const double emin=std::exp(-hi*zu*(1+16*eps));
      const double emax=std::exp(-lo*zl*(1-16*eps));
      const double wmin=std::max(0.,emin/((1+emin)*(1+emin))*(1-128*eps)-8*std::numeric_limits<double>::denorm_min());
      const double wmax=emax/((1+emax)*(1+emax))*(1+128*eps)+8*std::numeric_limits<double>::denorm_min();
      wl.add(zl*zl*wmin);wu.add(zu*zu*wmax);
    }
    const double disc=128*eps*(1+n);
    const double haa_min=n/(hi*hi)+(pl+q)*wl.value*(1-disc);
    const double haa_max=n/(lo*lo)+(pu+q)*wu.value*(1+disc);
    const double rq=1/(1+pu/q),rp=1/(1+q/pu),ratio=pl/pu;
    const double denominator=n*ratio*ratio*std::max(q*rp,rq*(2-rq))*(1-2*disc);
    const double numerator=pu*cross*(1+2*disc);
    if(denominator>0&&std::isfinite(denominator)&&std::isfinite(numerator)&&
       std::isfinite(haa_min)&&std::isfinite(haa_max)&&haa_max>0) {
      const double scaled=numerator/std::sqrt(denominator),schur=scaled*scaled;
      const double curvature_upper=-haa_min+schur+128*eps*(1+haa_min+schur);
      if(curvature_upper<0&&std::isfinite(curvature_upper)) {
        const Point ml=point(c,middle.lo),mh=point(c,middle.hi);
        if(ml.ok&&mh.ok) {
          const double glc=std::min(ml.ga-ml.ea,mh.ga-mh.ea),ghc=std::max(ml.ga+ml.ea,mh.ga+mh.ea);
          const double m=-curvature_upper,M=haa_max*(1+128*eps);
          const double d1=glc/m,d2=glc/M,d3=ghc/m,d4=ghc/M;
          const double dl=std::min(std::min(d1,d2),std::min(d3,d4));
          const double dh=std::max(std::max(d1,d2),std::max(d3,d4));
          if(std::isfinite(dl)&&std::isfinite(dh)) {
            const double pad=256*eps*(std::abs(a)+std::abs(dl)+std::abs(dh))+8*std::numeric_limits<double>::denorm_min();
            const double il=std::max(lo,a+dl-pad),ih=std::min(hi,a+dh+pad);
            if(ih<il)return true;
            const double h=std::max(std::abs(il-a),std::abs(ih-a));
            const double snl=std::max(smin,c.sm-c.em-cross*h),snu=std::min(smax,c.sm+c.em+cross*h);
            const Root ilp=root(snu/n,32*eps*std::abs(snu)/n),ihp=root(snl/n,32*eps*std::abs(snl)/n);
            if(ilp.ok&&ihp.ok) {
              nl=il;nh=ih;npl=std::max(pl,ilp.lo);npu=std::min(pu,ihp.hi);
              if(npu<npl){ok=false;return false;}
            }
          }
        }
      }
    }
    const double hnew=std::max(std::abs(nl-a),std::abs(nh-a));
    const double hp=std::max(std::abs(p-npl),std::abs(npu-p));
    // Both diagonal Hessian entries are negative. Only the cross term can
    // increase the supporting-plane upper bound, by <= cross*ha*hp.
    const double bound=center.ll+center.error+(std::abs(center.ga)+center.ea)*hnew+
      (std::abs(center.gp)+center.egp)*hp+cross*hnew*hp;
    if(!std::isfinite(bound)){ok=false;return false;}
    out={nl,nh,npl,npu,bound};keep=true;return true;
  }
  std::vector<double> coefficients(double a,double p) const {
    return inverse?std::vector<double>{p,a,q,b}:std::vector<double>{q,a,p,b};
  }
  Gb2Result result(Gb2Status status,double upper=INFINITY) const {
    Gb2Result r{inverse?NAN:q,NAN,inverse?q:NAN,b,NAN,static_cast<int>(evaluations),status};
    auto& d=r.stop_detail;d.loglik_upper=upper;d.budget_exhausted=status==Gb2Status::CertificateBudget;d.comparison_tie=status==Gb2Status::CertificateTie;
    if(best.ok) {
      r.shape1=inverse?best.p:q;r.shape2=best.a;r.shape3=inverse?q:best.p;r.logLik=best.ll;
      d.candidates.push_back({coefficients(best.a,best.p),best.ll-best.error,best.ll+best.error,StopModel::None});
      d.loglik_lower=best.ll-best.error;
    }
    if(face_valid) {
      const double rate=n/left;
      d.candidates.push_back({inverse?std::vector<double>{rate,b}:std::vector<double>{b,rate},face-face_error,face+face_error,inverse?StopModel::Pareto:StopModel::Power});
      d.loglik_lower=std::isnan(d.loglik_lower)?face-face_error:std::max(d.loglik_lower,face-face_error);
    }
    return r;
  }
};
inline Gb2Result fit(span x,span logs,double q,double p,double b,double tol,int maxit) {
  const bool inverse=std::isnan(q);
  Context c(x,logs,inverse?p:q,b,tol,maxit,inverse);
  if(!c.ok)return c.result(Gb2Status::NumericalFailure);
  // With no left-hand observation, a constant sample concentrates by
  // p -> infinity and a -> infinity (or by a alone at the endpoint).
  if(c.constant&&c.negative==0)return c.result(Gb2Status::InvalidData);
  if(tol<512*c.eps)return c.result(Gb2Status::NumericalFailure);
  const double log2=std::log(2.);
  double C=0,ec=0;
  if(c.q>1) {
    const double dq=special::digamma(c.q),d1=special::digamma(1.);
    C=dq-d1;ec=128*c.eps*(1+std::abs(dq)+std::abs(d1));
    if(!std::isfinite(C))return c.result(Gb2Status::NumericalFailure);
  }
  const double cu=c.n*(C+ec)-c.ties*(log2-128*c.eps);
  const double cl=c.n*(C-ec)-c.ties*(log2+128*c.eps);
  if(c.positive==0) {
    if(c.q<=1||cu<0)return gb2_endpoint_detail::fit(x,q,p,b,tol,inverse);
    if(!(c.left>c.el))return c.result(Gb2Status::NumericalFailure);
    const double rate=c.n/c.left;
    c.face=c.n*std::log(rate)-c.n-c.logs-c.ties*c.q*log2;
    c.face_error=c.log_error+c.n*c.el/(c.left-c.el)+128*c.eps*(1+c.abslogs+c.n*(1+std::abs(std::log(rate)))+c.ties*c.q*log2);
    c.face_valid=std::isfinite(c.face)&&std::isfinite(c.face_error);
    if(!c.face_valid)return c.result(Gb2Status::NumericalFailure);
    // Preserve the unselected face witness on a sign-uncertain seam.
    if(!(cl>0))return c.result(Gb2Status::NumericalFailure);
  }
  double proposal=c.n/c.absolute;
  if(!(proposal>0)||!std::isfinite(proposal))return c.result(Gb2Status::NumericalFailure);
  // Feasible lower witnesses need no optimizer. The power path is enough
  // to beat the endpoint face when its signed first correction is positive.
  for(int i=0;i<maxit;++i) {
    const Terms t=c.terms(proposal);
    if(!t.ok)return c.result(c.budget?Gb2Status::CertificateBudget:Gb2Status::NumericalFailure);
    double pp=1;
    if(c.face_valid)pp=(c.n/c.left)/proposal;
    else {
      const Root r=c.root(t.sm/c.n,t.em/c.n);
      if(!r.ok)return c.result(Gb2Status::NumericalFailure);
      pp=r.lo/2+r.hi/2;
    }
    c.consider(c.point(t,pp));
    if(c.best.ok&&(!c.face_valid||c.best.ll-c.best.error>c.face+c.face_error))break;
    proposal*=2;
    if(!std::isfinite(proposal))break;
  }
  if(!c.best.ok||(c.face_valid&&!(c.best.ll-c.best.error>c.face+c.face_error)))
    return c.result(c.budget?Gb2Status::CertificateBudget:Gb2Status::NumericalFailure);
  const double lower=c.best.ll-c.best.error;
  // For a <= a0, S_-(a)/n >= log(2)/2. Concavity in p supplies the
  // finite upper constant M in L <= n log(a) + n M - sum log(x).
  const double lambda=log2/2;
  const Root small=c.root(lambda,32*c.eps*lambda);
  if(!small.ok)return c.result(Gb2Status::NumericalFailure);
  const double pc=small.lo/2+small.hi/2;
  double de=0;const double dp=c.difference(pc,de),lb=gb2_detail::logbeta(pc,c.q);
  const double F=-lb-pc*lambda;
  const double M=F+(std::abs(dp-lambda)+de+32*c.eps*lambda)*std::max(pc-small.lo,small.hi-pc)+c.beta_error(pc,lb)+128*c.eps*(1+std::abs(lb)+pc*lambda);
  const double a0=c.n*(log2-128*c.eps)/(2*c.absolute);
  double alo=std::min(a0,std::exp((lower+c.logs-c.log_error-c.n*M)/c.n-log2*2));
  if(!(alo>0)||!std::isfinite(alo)||!std::isfinite(M))return c.result(Gb2Status::NumericalFailure);
  const double small_upper=c.n*std::log(alo)+c.n*M-c.logs+c.log_error+128*c.eps*c.n*(1+std::abs(std::log(alo)));
  if(!std::isfinite(small_upper)||!(small_upper<lower))return c.result(Gb2Status::NumericalFailure);
  double ahi=0,tail_upper=INFINITY;
  if(c.negative>0) {
    const double sl=c.left-c.el;
    if(!(sl>0))return c.result(Gb2Status::NumericalFailure);
    if(c.positive==0) {
      const double gap=(c.face+c.face_error-lower)/c.n;
      ahi=std::max(c.best.a*2,4*cu/(sl*(-std::expm1(gap))));
      if(!(ahi>0)||!std::isfinite(ahi))return c.result(Gb2Status::NumericalFailure);
      const double ratio=cu/(ahi*sl);
      if(!(ratio>=0&&ratio<1))return c.result(Gb2Status::NumericalFailure);
      tail_upper=c.face+c.face_error-c.n*std::log1p(-ratio)+128*c.eps*c.n*(1+std::abs(std::log1p(-ratio)));
    } else {
      const double sr=c.right-c.er;
      if(!(sr>0))return c.result(Gb2Status::NumericalFailure);
      const double K=c.n*std::log(2*c.n/sl)-c.n-c.logs+c.log_error;
      const double ek=128*c.eps*(1+std::abs(K)+c.abslogs);
      ahi=std::max(c.best.a*2,std::max(2*c.n*std::max(0.,C+ec)/sl,(K+ek-lower+c.n*log2)/(c.q*sr)));
      tail_upper=K+ek-c.q*ahi*sr;
    }
  } else {
    // Gamma-ratio bound B(p,q) >= Gamma(q)/(p+q)^q. After maximizing
    // p+q, the observed log spread gives n log(a) - q D a + K.
    double D=0,eD=0;
    std::size_t minindex=0;
    for(std::size_t i=1;i<c.z.size();++i)if(c.z[i]<c.z[minindex])minindex=i;
    for(std::size_t i=0;i<c.z.size();++i){D+=c.z[i]-c.z[minindex];eD+=c.ez[i]+c.ez[minindex];}
    D-=eD+128*c.eps*(1+std::abs(D));
    const double lg=special::lgamma(c.q);
    const double K=c.n*c.q*(std::log(2.)+std::log(c.n)+std::log(c.q)-1)-c.n*lg-c.logs;
    const double ek=c.log_error+128*c.eps*(1+std::abs(K)+c.n*std::abs(lg)+c.abslogs);
    if(!(D>0)||!std::isfinite(K)||!std::isfinite(ek))return c.result(Gb2Status::NumericalFailure);
    ahi=std::max(2*c.best.a,2*c.n/(c.q*D));
    for(int i=0;i<maxit;++i) {
      tail_upper=c.n*std::log(ahi)-c.q*D*ahi+K+ek+128*c.eps*c.n*(1+std::abs(std::log(ahi)));
      if(tail_upper<lower)break;
      ahi*=2;if(!std::isfinite(ahi))break;
    }
  }
  if(!(ahi>alo)||!std::isfinite(ahi)||!std::isfinite(tail_upper)||!(tail_upper<lower))return c.result(Gb2Status::NumericalFailure);
  std::vector<Cell> cells;
  Cell first{};bool keep=false;
  if(!c.cell(alo,ahi,first,keep))return c.result(c.budget?Gb2Status::CertificateBudget:Gb2Status::NumericalFailure);
  if(keep)cells.push_back(first);
  while(!cells.empty()) {
    const double best=c.best.ll-c.best.error;
    cells.erase(std::remove_if(cells.begin(),cells.end(),[&](const Cell& v){return v.upper<best;}),cells.end());
    if(cells.empty())return c.result(Gb2Status::NumericalFailure);
    double al=INFINITY,au=0,pl=INFINITY,pu=0,upper=std::max(tail_upper,small_upper),width=-1;
    std::size_t selected=0;
    for(std::size_t i=0;i<cells.size();++i) {
      const auto& v=cells[i];al=std::min(al,v.lo);au=std::max(au,v.hi);pl=std::min(pl,v.pl);pu=std::max(pu,v.pu);upper=std::max(upper,v.upper);
      const double w=std::log1p((v.hi-v.lo)/v.lo);if(w>width){width=w;selected=i;}
    }
    if((au-al)/al<c.tol/8&&(pu-pl)/pl<c.tol/8) {
      const double aa=al/2+au/2,pp=pl/2+pu/2;
      const Point returned=c.point(c.terms(aa),pp);
      if(!returned.ok)return c.result(c.budget?Gb2Status::CertificateBudget:Gb2Status::NumericalFailure);
      Gb2Result out=c.result(Gb2Status::Ok,upper);
      out.shape1=inverse?pp:c.q;out.shape2=aa;out.shape3=inverse?c.q:pp;out.logLik=returned.ll;
      out.stop_detail.candidates={{c.coefficients(aa,pp),returned.ll-returned.error,returned.ll+returned.error,StopModel::None}};
      return out;
    }
    bool narrow=true;
    for(const auto& v:cells)narrow=narrow&&(v.hi-v.lo)/v.lo<c.tol/32&&(v.pu-v.pl)/v.pl<c.tol/32;
    if(narrow) {
      struct Cluster {double al,au,pl,pu,upper;};
      std::vector<Cell> ordered=cells;
      std::sort(ordered.begin(),ordered.end(),[](const Cell& a,const Cell& b){return a.lo<b.lo;});
      std::vector<Cluster> groups;
      for(const auto& v:ordered) {
        if(groups.empty()||v.lo>groups.back().au)groups.push_back({v.lo,v.hi,v.pl,v.pu,v.upper});
        else {auto& g=groups.back();g.au=std::max(g.au,v.hi);g.pl=std::min(g.pl,v.pl);g.pu=std::max(g.pu,v.pu);g.upper=std::max(g.upper,v.upper);}
      }
      bool separated=groups.size()>1;
      for(const auto& g:groups)separated=separated&&(g.au-g.al)/g.al<c.tol/8&&(g.pu-g.pl)/g.pl<c.tol/8;
      if(separated) {
        Gb2Result out=c.result(Gb2Status::CertificateTie,upper);
        out.stop_detail.candidates.clear();
        for(const auto& g:groups) {
          const double aa=g.al/2+g.au/2,pp=g.pl/2+g.pu/2;
          const Point v=c.point(c.terms(aa),pp);
          if(!v.ok)return c.result(c.budget?Gb2Status::CertificateBudget:Gb2Status::NumericalFailure,upper);
          out.stop_detail.candidates.push_back({c.coefficients(aa,pp),v.ll-v.error,std::max(g.upper,v.ll+v.error),StopModel::None});
        }
        out.iterations=static_cast<int>(c.evaluations);return out;
      }
    }
    if(!c.charge())return c.result(Gb2Status::CertificateBudget,upper);
    const Cell v=cells[selected];cells[selected]=cells.back();cells.pop_back();
    const double mid=std::exp(std::log(v.lo)/2+std::log(v.hi)/2);
    if(!(mid>v.lo&&mid<v.hi))return c.result(Gb2Status::NumericalFailure,upper);
    for(int side=0;side<2;++side) {
      Cell child{};bool retain=false;
      if(!c.cell(side?mid:v.lo,side?v.hi:mid,child,retain))return c.result(c.budget?Gb2Status::CertificateBudget:Gb2Status::NumericalFailure,INFINITY);
      if(retain)cells.push_back(child);
    }
  }
  return c.result(Gb2Status::NumericalFailure);
}
} // namespace gb2_shape_power_detail
} // namespace cppunivariate
