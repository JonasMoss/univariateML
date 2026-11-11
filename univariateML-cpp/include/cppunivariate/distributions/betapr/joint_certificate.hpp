// All-free global certificate. Exact-arithmetic proof and FP margins: README.md.
#pragma once
#include "mle.hpp"
#include <array>
#include <map>
#include <queue>
namespace cppunivariate {
namespace betapr_joint_cert {
struct Interval {double lo=0,hi=0;};
inline Interval interval(double x,double e=0){return {x-e,x+e};}
inline Interval add(Interval a,Interval b){const double e=16*std::numeric_limits<double>::epsilon()*(1+std::max(std::abs(a.lo),std::abs(a.hi))+std::max(std::abs(b.lo),std::abs(b.hi)));return {a.lo+b.lo-e,a.hi+b.hi+e};}
inline Interval neg(Interval a){return {-a.hi,-a.lo};}
inline Interval sub(Interval a,Interval b){return add(a,neg(b));}
inline Interval mul(Interval a,Interval b){const double lo=std::min(std::min(a.lo*b.lo,a.lo*b.hi),std::min(a.hi*b.lo,a.hi*b.hi)),hi=std::max(std::max(a.lo*b.lo,a.lo*b.hi),std::max(a.hi*b.lo,a.hi*b.hi));const double e=16*std::numeric_limits<double>::epsilon()*(1+std::max(std::abs(lo),std::abs(hi)));return {lo-e,hi+e};}
inline Interval times(double a,Interval b){return mul(interval(a),b);}
inline double magnitude(Interval a){return std::max(std::abs(a.lo),std::abs(a.hi));}
inline double midpoint(Interval a){return a.lo+(a.hi-a.lo)/2;}
struct Budget {
  std::int64_t used=0,limit=0;bool exhausted=false;
  bool take(){if(used>=limit){exhausted=true;return false;}++used;return true;}
};
// Stirling remainder through B_12; the omitted term at z>=32 is < 1e-21.
inline double remainder(double z) {
  const double v=1/(z*z);
  return (1./12-v*(1./360-v*(1./1260-v*(1./1680-v*(1./1188-v*691./360360)))))/z;
}
inline double log_beta(double p,double q) {
  const double z=std::max(p,q),h=std::min(p,q);
  if(z<32)return special::lbeta(p,q);
  const double r=h/z,l=std::log1p(r);
  if(h>=32)return .5*std::log(2*cppunivariate::pi)-.5*std::log(z+h)-
    (z-.5)*l+(h-.5)*(std::log(h)-std::log(z)-l)+remainder(z)+remainder(h)-remainder(z+h);
  const double lm=r<.001?r*r*(-.5+r*(1./3+r*(-.25+r*(.2+r*(-1./6+r/7))))):l-r;
  return special::lgamma(h)-h*std::log(z)-(h-.5)*l-z*lm-remainder(z+h)+remainder(z);
}
inline double logistic(double t) {return t>=0?1/(1+std::exp(-t)):std::exp(t)/(1+std::exp(t));}
struct Point {
  double p=NAN,q=NAN,t=NAN,te=NAN,ll=NAN,error=NAN,A=NAN,B=NAN,lb=NAN;
  Interval ga{},gb{};bool valid=false;
};
struct Face {
  double a=NAN,la=NAN,V=NAN,M=NAN,ll=-INFINITY,error=INFINITY,upper=INFINITY;
  std::vector<double> coef;StopModel model=StopModel::None;bool accurate=false,root_enclosed=false;
  Interval root;
};
struct Cell {
  double pl,pu,ql,qu,upper=INFINITY;bool fresh=false;
  bool operator<(const Cell& b)const{return upper<b.upper;}
};
struct Patch {
  Cell outer;Interval p,q,t;Point point;double upper=INFINITY;
};
struct Engine {
  span x;int maxit;double tol,n,unit,lm=0,mean=0,mag=0;
  Budget budget;bool arithmetic=false,accuracy_unresolved=false;std::int64_t cells=0;
  std::vector<double> logs,scaled;
  std::map<std::pair<double,double>,Point> cache;
  std::vector<Patch> patches;
  Point best;Face gamma,inv;BetaPrimeJointBounds exterior;
  double incumbent=-INFINITY,last_upper=NAN,anchor_q=NAN,anchor_upper=INFINITY;
  Engine(span data,double tolerance,int iterations):x(data),maxit(iterations),tol(tolerance),n(data.size()),unit(NAN){budget.limit=certificate_budget(maxit);}
  Point point(double p,double q) {
    const auto key=std::make_pair(p,q);const auto found=cache.find(key);
    if(found!=cache.end())return found->second;
    Point out;out.p=p;out.q=q;
    if(!budget.take())return out;
    if(!(p>0&&q>0)||!std::isfinite(p+q)){arithmetic=true;return out;}
    double left=std::log(q)-std::log(p)+logs.front(),right=std::log(q)-std::log(p)+logs.back();
    double t=left+(right-left)/2;bool converged=false;double root_error=INFINITY;
    const double eps=std::numeric_limits<double>::epsilon();
    for(int j=0;j<maxit;++j) {
      betapr_joint_detail::Sum small,Q;
      for(double y:logs){const double u=logistic(y-t),v=logistic(t-y);small.add((p<=q?u:v)/n);Q.add(u*v/n);}
      const double f=p<=q?small.value-p/(p+q):q/(p+q)-small.value;
      const double target=std::min(p,q)/(p+q);
      const double fe=64*eps*(1+std::abs(t)+std::max(std::abs(logs.front()),std::abs(logs.back())))*(small.value+target);
      const double ratio=(std::abs(f)+fe)/(Q.value*(1-128*eps));
      if(std::abs(f)<=4*fe) {
        if(!(ratio<.5)){arithmetic=true;return out;}
        root_error=-std::log1p(-ratio);left=t-root_error;right=t+root_error;converged=true;break;
      }
      if(f>0)left=t;else right=t;
      if(right-left<32*eps*(1+std::abs(t))){converged=true;root_error=ratio<.5?-std::log1p(-ratio):INFINITY;break;}
      double next=t+f/Q.value;
      if(!(next>left&&next<right)||!std::isfinite(next))next=left+(right-left)/2;
      t=next;
    }
    if(!converged){arithmetic=true;return out;}
    const double input_error=64*eps*(1+mag+std::abs(lm)+std::max(std::abs(logs.front()),std::abs(logs.back())));
    out.t=left+(right-left)/2;out.te=right-left+root_error+64*eps*(1+std::abs(out.t))+input_error;
    betapr_joint_detail::Sum A,B;
    for(double y:logs){A.add(betapr_joint_detail::softplus(out.t-y)/n);B.add(betapr_joint_detail::softplus(y-out.t)/n);}
    out.A=A.value;out.B=B.value;out.lb=log_beta(p,q);
    out.ll=-out.lb-p*out.A-q*out.B-mean;
    out.error=1e-19+256*eps*(1+std::abs(out.lb)+p*out.A+q*out.B+std::abs(mean))+(p+q)*out.te*out.te/8+(1+p+q)*input_error;
    const auto a=betapr_cert_detail::increment(q,p),b=betapr_cert_detail::increment(p,q);
    out.ga=interval(a.value-out.A,a.error+64*eps*(1+out.A)+out.te+input_error);
    out.gb=interval(b.value-out.B,b.error+64*eps*(1+out.B)+out.te+input_error);
    out.valid=std::isfinite(out.ll)&&std::isfinite(out.error)&&std::isfinite(out.t)&&std::isfinite(out.te);
    if(!out.valid){arithmetic=true;return out;}
    cache.emplace(key,out);
    if(!best.valid||out.ll-out.error>best.ll-best.error)best=out;
    incumbent=std::max(incumbent,out.ll-out.error);
    return out;
  }
  std::array<Interval,3> hessian(const Cell& c) {
    if(!budget.take())return {};
    const Point l=point(c.pu,c.ql),r=point(c.pl,c.qu);
    if(!l.valid||!r.valid)return {};
    betapr_joint_detail::Sum Qlo,Qhi;
    for(double y:logs){
      const double u=logistic(y-l.t+l.te),v=logistic(y-r.t-r.te);
      const double a=u*(1-u),b=v*(1-v);
      Qlo.add(std::min(a,b)/n);Qhi.add((v<=.5&&u>=.5?.25:std::max(a,b))/n);
    }
    const double eps=std::numeric_limits<double>::epsilon();
    const double qe=128*eps*(1+Qhi.value)+l.te+r.te;
    const double ql=Qlo.value-qe,qh=Qhi.value+qe;
    if(!(ql>0)){arithmetic=true;return {};}
    const double sl=c.pl+c.ql,sh=c.pu+c.qu;
    const double ulo=c.pl/(c.pl+c.qu),uhi=c.pu/(c.pu+c.ql),vlo=1-uhi,vhi=1-ulo;
    Interval aa{special::trigamma(c.pl+c.qu)-special::trigamma(c.pl)+vlo*vlo/(sh*qh),
                special::trigamma(c.pu+c.ql)-special::trigamma(c.pu)+vhi*vhi/(sl*ql)};
    Interval bb{special::trigamma(c.pu+c.ql)-special::trigamma(c.ql)+ulo*ulo/(sh*qh),
                special::trigamma(c.pl+c.qu)-special::trigamma(c.qu)+uhi*uhi/(sl*ql)};
    const double uvlo=std::min(ulo*(1-ulo),uhi*(1-uhi)),uvhi=ulo<=.5&&uhi>=.5?.25:std::max(ulo*(1-ulo),uhi*(1-uhi));
    Interval ab{special::trigamma(sh)-uvhi/(sl*ql),special::trigamma(sl)-uvlo/(sh*qh)};
    const double error=256*eps*(1+special::trigamma(c.pl)+special::trigamma(c.ql)+1/(sl*ql));
    aa.lo-=error;aa.hi+=error;bb.lo-=error;bb.hi+=error;ab.lo-=error;ab.hi+=error;
    return {aa,ab,bb};
  }
  // Safeguarded Newton supplies proposals only. Inclusion below proves roots.
  Point refine(Point p) {
    const double pl=p.p/4,pu=p.p*4,ql=p.q/4,qu=p.q*4;
    for(int j=0;j<maxit&&p.valid&&!budget.exhausted;++j) {
      const auto h=hessian({p.p,p.p,p.q,p.q});if(arithmetic||budget.exhausted)return p;
      const double a=midpoint(h[0])*p.p*p.p,b=midpoint(h[1])*p.p*p.q,c=midpoint(h[2])*p.q*p.q;
      const double det=a*c-b*b;if(!(a<0&&c<0&&det>0))break;
      const double ga=midpoint(p.ga)*p.p,gb=midpoint(p.gb)*p.q;
      const double dp=(c*ga-b*gb)/det,dq=(a*gb-b*ga)/det;
      if(std::max(std::abs(dp),std::abs(dq))<1e-13)break;
      double step=1;bool accepted=false;
      for(int k=0;k<maxit;++k) {
        const double pp=p.p*std::exp(-step*dp),qq=p.q*std::exp(-step*dq);
        if(!(pp>pl&&pp<pu&&qq>ql&&qq<qu))return p;
        if(pp>0&&qq>0&&std::isfinite(pp+qq)) {
          const Point next=point(pp,qq);if(!next.valid)return p;
          if(next.ll+next.error>=p.ll-p.error){p=next;accepted=true;break;}
        }
        step*=.5;
      }
      if(!accepted)break;
    }
    return p;
  }
  bool patch(Point seed) {
    accuracy_unresolved=false;
    if(!seed.valid||arithmetic||budget.exhausted)return false;
    for(const auto& old:patches)if(seed.p>old.outer.pl&&seed.p<old.outer.pu&&seed.q>old.outer.ql&&seed.q<old.outer.qu)return true;
    Point p=refine(seed);if(arithmetic){arithmetic=false;return false;}if(budget.exhausted)return false;
    for(const auto& old:patches)if(p.p>old.p.lo&&p.p<old.p.hi&&p.q>old.q.lo&&p.q<old.q.hi)return true;
    const auto center=hessian({p.p,p.p,p.q,p.q});if(arithmetic){arithmetic=false;return false;}if(budget.exhausted)return false;
    const double A=midpoint(center[0])*p.p*p.p,B=midpoint(center[1])*p.p*p.q,C=midpoint(center[2])*p.q*p.q,det=A*C-B*B;
    if(!(A<0&&C<0&&det>0))return false;
    const double ca=C/det,cb=-B/det,cc=A/det;
    if(!(ca<0&&cc<0&&ca*cc-cb*cb>256*std::numeric_limits<double>::epsilon()*(std::abs(ca*cc)+cb*cb)))return false;
    double radius=.001;
    for(int attempt=0;attempt<maxit;++attempt) {
      if(radius<256*std::numeric_limits<double>::epsilon())return false;
      Cell box{p.p*(1-radius),p.p*(1+radius),p.q*(1-radius),p.q*(1+radius)};
      const auto raw=hessian(box);if(arithmetic){arithmetic=false;return false;}if(budget.exhausted)return false;
      const auto a=times(p.p*p.p,raw[0]),b=times(p.p*p.q,raw[1]),c=times(p.q*p.q,raw[2]);
      const auto r00=sub(interval(1),add(times(ca,a),times(cb,b))),r01=neg(add(times(ca,b),times(cb,c))),
                 r10=neg(add(times(cb,a),times(cc,b))),r11=sub(interval(1),add(times(cb,b),times(cc,c)));
      const double contraction=std::max(magnitude(r00)+magnitude(r01),magnitude(r10)+magnitude(r11));
      if(!(contraction<.5)){radius*=.5;continue;}
      const Interval g0=times(p.p,p.ga),g1=times(p.q,p.gb);
      const Interval correction0=neg(add(times(ca,g0),times(cb,g1))),correction1=neg(add(times(cb,g0),times(cc,g1)));
      const double a00=magnitude(r00),a01=magnitude(r01),a10=magnitude(r10),a11=magnitude(r11);
      const double dd=(1-a00)*(1-a11)-a01*a10,d0=magnitude(correction0),d1=magnitude(correction1);
      if(!(dd>0))return false;
      const double err0=((1-a11)*d0+a01*d1)/dd+128*std::numeric_limits<double>::epsilon(),
                   err1=(a10*d0+(1-a00)*d1)/dd+128*std::numeric_limits<double>::epsilon();
      if(!(err0<radius&&err1<radius)){radius*=.5;continue;}
      if(!(d0+a00*err0+a01*err1<err0&&d1+a10*err0+a11*err1<err1))return false;
      const Interval pr{p.p*(1-err0),p.p*(1+err0)},qr{p.q*(1-err1),p.q*(1+err1)};
      const Point sl=point(pr.hi,qr.lo),su=point(pr.lo,qr.hi);
      if(!sl.valid||!su.valid){arithmetic=false;return false;}
      const Interval tr{sl.t-sl.te,su.t+su.te};
      const double target=std::min(1e-6,tol);
      if(std::max(err0,err1)>target/4||tr.hi-tr.lo>target/2){accuracy_unresolved=true;return false;}
      const double upper=p.ll+p.error+radius*(magnitude(g0)+magnitude(g1));
      patches.push_back({box,pr,qr,tr,p,upper});return true;
    }
    return false;
  }
  double gamma_upper(const Face& f,double lo,double hi) {
    const double a=std::min(hi,std::max(lo,f.a));
    const double m=f.model==StopModel::Gamma?mean:-mean;
    const double val=a*std::log(a)-special::lgamma(a)-a+a*(m-f.la)-m;
    const double score=std::log(a)-special::digamma(a)+m-f.la;
    const double shift=f.model==StopModel::Gamma?0:-2*mean;
    const double e=512*std::numeric_limits<double>::epsilon()*(1+std::abs(val)+std::abs(special::lgamma(a))+a*(1+std::abs(std::log(a))+std::abs(f.la))+hi*(1+std::abs(score))+(1+a)*(1+mag+std::abs(lm)));
    const double upper=val+std::max(score*(lo-a),score*(hi-a))+shift+e;
    return std::isfinite(upper)?upper:INFINITY;
  }
  double completed_cutoff(const Face& f,double hi) {
    if(!f.root_enclosed)return INFINITY;
    const double v=std::max(1.,f.V+1e-10*(1+f.V)),M=f.M*(1+1e-10),u=std::max(hi,f.root.hi);
    const auto A=[&](double p){return p*.5*(p*(v-1)-1);};
    const double leading=std::max(A(f.root.lo),A(f.root.hi))+1e-10*(1+f.root.hi*f.root.hi*v);
    if(!(leading<0))return INFINITY;
    const double C=hi*hi/4+2*hi*hi*hi*M*(1+2*v)+2*hi*hi*hi*v;
    const double B=.5+u*(v-1),remainder=(C+B*B*u*u)*(1+1e-10);
    const double cutoff=std::max(2*hi*M,remainder/(-leading))*(1+1e-10);
    return std::isfinite(cutoff)?cutoff:INFINITY;
  }
  bool tail(const Face& f,double lo,double hi,double other) {
    if(!(other>2*hi*f.M*(1+1e-10)))return false;
    if(other>completed_cutoff(f,hi))return true;
    const double ve=1e-10*(1+f.V),v=std::max(1.,f.V+ve),M=f.M*(1+1e-10);
    const auto leading=[&](double p){return .5*p*(p*(v-1)-1);};
    const double lead=std::max(leading(lo),leading(hi))+1e-10*(1+hi*hi*v);
    const double rem=hi*hi/4+2*hi*hi*hi*M*(1+2*v)+2*hi*hi*hi*v;
    if(lead<0&&rem*(1+1e-10)/other < -lead)return true;
    const double signed_remainder=std::max(0.,lead/other+rem*(1+1e-10)/(other*other));
    if(gamma_upper(f,lo,hi)+signed_remainder<incumbent)return true;
    return gamma_upper(f,lo,hi)+hi*hi*(1+2*v)*(1+1e-10)/other<incumbent;
  }
  bool maximum_impossible(const Cell& c) {
    const auto h=hessian(c);if(arithmetic){arithmetic=false;return false;}if(budget.exhausted)return false;
    const double a=midpoint(h[0]),b=midpoint(h[1]),d=midpoint(h[2]);
    const double lambda=.5*(a+d+std::hypot(a-d,2*b));
    if(!(lambda>0))return false;
    double u=b,v=lambda-a;if(std::abs(u)+std::abs(v)==0){u=lambda==a?1:0;v=1-u;}
    const double norm=std::hypot(u,v);u/=norm;v/=norm;
    return add(add(times(u*u,h[0]),times(2*u*v,h[1])),times(v*v,h[2])).lo>0;
  }
  bool cell(Cell c,std::priority_queue<Cell>& queue) {
    if(!budget.take())return false;
    ++cells;c.fresh=false;
    const auto harmonic=[](double p,double q){return p<=q?p/(1+p/q):q/(1+q/p);};
    if(harmonic(c.pl,c.ql)>=exterior.harmonic_upper||harmonic(c.pu,c.qu)<=exterior.harmonic_lower)return true;
    for(const auto& p:patches)if(c.pl>=p.outer.pl&&c.pu<=p.outer.pu&&c.ql>=p.outer.ql&&c.qu<=p.outer.qu)return true;
    if(tail(gamma,c.pl,c.pu,c.ql)||tail(inv,c.ql,c.qu,c.pl))return true;
    const Point ll=point(c.pl,c.ql),lr=point(c.pl,c.qu),rl=point(c.pu,c.ql),rr=point(c.pu,c.qu);
    if(!ll.valid||!lr.valid||!rl.valid||!rr.valid)return false;
    betapr_joint_detail::Sum Alo,Ahi,Blo,Bhi;
    for(double y:logs){Alo.add(betapr_joint_detail::softplus(rl.t-rl.te-y)/n);Ahi.add(betapr_joint_detail::softplus(lr.t+lr.te-y)/n);Blo.add(betapr_joint_detail::softplus(y-lr.t-lr.te)/n);Bhi.add(betapr_joint_detail::softplus(y-rl.t+rl.te)/n);}
    const auto aa=betapr_cert_detail::increment(c.ql,c.pu),ab=betapr_cert_detail::increment(c.qu,c.pl),ba=betapr_cert_detail::increment(c.pl,c.qu),bb=betapr_cert_detail::increment(c.pu,c.ql);
    const double e=128*std::numeric_limits<double>::epsilon()*(1+Ahi.value+Bhi.value);
    const Interval ga{aa.value-aa.error-Ahi.value-e,ab.value+ab.error-Alo.value+e},
                   gb{ba.value-ba.error-Bhi.value-e,bb.value+bb.error-Blo.value+e};
    if(!(ga.lo<=ga.hi&&gb.lo<=gb.hi)||!std::isfinite(ga.lo+ga.hi+gb.lo+gb.hi)){arithmetic=true;return false;}
    if(ga.lo>0||ga.hi<0||gb.lo>0||gb.hi<0)return true;
    const Point mid=point(std::sqrt(c.pl)*std::sqrt(c.pu),std::sqrt(c.ql)*std::sqrt(c.qu));
    if(!mid.valid)return false;
    const auto da=betapr_cert_detail::increment(mid.q,mid.p),db=betapr_cert_detail::increment(mid.p,mid.q);
    double upper=-INFINITY;
    for(const Point& p:{ll,lr,rl,rr}) {
      const double candidate=p.ll+p.error+p.lb-mid.lb+da.value*(p.p-mid.p)+db.value*(p.q-mid.q);
      const double error=da.error*std::abs(p.p-mid.p)+db.error*std::abs(p.q-mid.q)+
        256*std::numeric_limits<double>::epsilon()*(1+std::abs(p.lb)+std::abs(mid.lb)+std::abs(candidate));
      upper=std::max(upper,candidate+error);
    }
    if(anchor_q>=c.ql&&anchor_q<=c.qu&&std::isfinite(anchor_upper))
      upper=std::min(upper,anchor_upper+std::max(0.,std::max(gb.hi*(c.qu-anchor_q),gb.lo*(c.ql-anchor_q))));
    c.upper=upper;last_upper=upper;
    if(upper<incumbent)return true;
    if(std::min(std::log(c.pu/c.pl),std::log(c.qu/c.ql))<128*std::numeric_limits<double>::epsilon()){arithmetic=true;return false;}
    if(std::max(std::log(c.pu/c.pl),std::log(c.qu/c.ql))<.5) {
      const auto hc=hessian({mid.p,mid.p,mid.q,mid.q});
      if(arithmetic)arithmetic=false;
      else if(!budget.exhausted) {
        const auto hh=hessian(c);
        if(arithmetic)arithmetic=false;
        else if(!budget.exhausted) {
          const double aa=midpoint(hc[0])*mid.p*mid.p,bb=midpoint(hc[1])*mid.p*mid.q,cc=midpoint(hc[2])*mid.q*mid.q,d=aa*cc-bb*bb;
          if(std::isfinite(d)&&std::abs(d)>256*std::numeric_limits<double>::epsilon()*(std::abs(aa*cc)+bb*bb)) {
            const double ia=cc/d,ib=-bb/d,ic=aa/d;
            const auto a=times(mid.p*mid.p,hh[0]),b=times(mid.p*mid.q,hh[1]),h=times(mid.q*mid.q,hh[2]);
            const auto r00=sub(interval(1),add(times(ia,a),times(ib,b))),r01=neg(add(times(ia,b),times(ib,h))),r10=neg(add(times(ib,a),times(ic,b))),r11=sub(interval(1),add(times(ib,b),times(ic,h)));
            const Interval dx{c.pl/mid.p-1,c.pu/mid.p-1},dy{c.ql/mid.q-1,c.qu/mid.q-1};
            auto k0=add(neg(add(times(ia,times(mid.p,mid.ga)),times(ib,times(mid.q,mid.gb)))),add(mul(r00,dx),mul(r01,dy)));
            auto k1=add(neg(add(times(ib,times(mid.p,mid.ga)),times(ic,times(mid.q,mid.gb)))),add(mul(r10,dx),mul(r11,dy)));
            const double ke=128*std::numeric_limits<double>::epsilon()*(1+magnitude(k0)+magnitude(k1));k0.lo-=ke;k0.hi+=ke;k1.lo-=ke;k1.hi+=ke;
            if(k0.hi<dx.lo||k0.lo>dx.hi||k1.hi<dy.lo||k1.lo>dy.hi)return true;
            const double contraction=std::max(magnitude(r00)+magnitude(r01),magnitude(r10)+magnitude(r11));
            const bool negative=ia<0&&ic<0&&ia*ic-ib*ib>256*std::numeric_limits<double>::epsilon()*(std::abs(ia*ic)+ib*ib);
            if(negative&&contraction<.5) {
              for(const auto& old:patches)if(old.p.lo>c.pl&&old.p.hi<c.pu&&old.q.lo>c.ql&&old.q.hi<c.qu)return true;
              if(k0.lo>dx.lo+ke&&k0.hi<dx.hi-ke&&k1.lo>dy.lo+ke&&k1.hi<dy.hi-ke) {
                patch(mid);if(budget.exhausted)return false;
                if(accuracy_unresolved){arithmetic=true;return false;}
                for(const auto& old:patches)if(old.p.lo>c.pl&&old.p.hi<c.pu&&old.q.lo>c.ql&&old.q.hi<c.qu)return true;
              }
            }
            Cell tight{std::max(c.pl,mid.p*(1+k0.lo)),std::min(c.pu,mid.p*(1+k0.hi)),std::max(c.ql,mid.q*(1+k1.lo)),std::min(c.qu,mid.q*(1+k1.hi)),c.upper,true};
            if(tight.pl<tight.pu&&tight.ql<tight.qu&&(tight.pu-tight.pl)/(c.pu-c.pl)*(tight.qu-tight.ql)/(c.qu-c.ql)<.5) {queue.push(tight);return true;}
          }
        }
      }
      if(budget.exhausted)return false;
    }
    if(std::max(std::log(c.pu/c.pl),std::log(c.qu/c.ql))<.1) {
      if(arithmetic||budget.exhausted)return false;
      for(const auto& p:patches)if(c.pl>=p.outer.pl&&c.pu<=p.outer.pu&&c.ql>=p.outer.ql&&c.qu<=p.outer.qu)return true;
      if(maximum_impossible(c))return true;
      if(arithmetic||budget.exhausted)return false;
    }
    queue.push(c);return true;
  }
};
inline BetaPrimeResult fit(span x,double tol,int maxit,double startp,double startq,
    double starts,double reltol,double abstol) {
  maxit=static_cast<int>(solver_budget(maxit));
  BetaPrimeResult out{NAN,NAN,NAN,NAN,0,BetaPrimeStatus::BfgsFailure};
  if(x.size()<2||!(tol>0)||!std::isfinite(tol)||startp<0||startq<0||starts<0||
     !std::isfinite(startp+startq+starts)) {out.status=BetaPrimeStatus::InvalidStart;return out;}
  bool constant=true;betapr_joint_detail::Sum lm,mag;
  for(double v:x){if(!(v>0)||!std::isfinite(v)){out.status=BetaPrimeStatus::InvalidStart;return out;}constant=constant&&v==x[0];lm.add(std::log(v)/x.size());mag.add(std::abs(std::log(v))/x.size());}
  if(constant){out.status=BetaPrimeStatus::InvalidStart;out.stop_detail.loglik_lower=INFINITY;out.stop_detail.loglik_upper=INFINITY;return out;}
  Engine e(x,tol,maxit);e.lm=lm.value;e.mag=mag.value;e.unit=std::exp(e.lm);out.scale=e.unit;
  if(!(e.unit>0)||!std::isfinite(e.unit)){out.certificate_failure=LomaxCertificateFailure::Arithmetic;return out;}
  for(double v:x){e.logs.push_back(std::log(v)-e.lm);e.scaled.push_back(v/e.unit);}
  std::sort(e.logs.begin(),e.logs.end());betapr_joint_detail::Sum cm;for(double y:e.logs)cm.add(y/e.n);e.mean=cm.value;
  if(!e.budget.take()){out.certificate_failure=LomaxCertificateFailure::Budget;out.stop_detail.budget_exhausted=true;return out;}
  betapr_joint_detail::Sum fallback;
  for(double y:e.logs)fallback.add((-betapr_joint_detail::softplus(y)-betapr_joint_detail::softplus(-y)-y)/e.n);
  const double fallback_error=512*std::numeric_limits<double>::epsilon()*(1+std::abs(fallback.value)+e.mag+std::abs(e.lm));
  out.stop_detail.candidates.push_back({{1,1,e.unit},e.n*(fallback.value-e.lm-fallback_error),e.n*(fallback.value-e.lm+fallback_error)});
  out.stop_detail.loglik_lower=e.n*(fallback.value-e.lm-fallback_error);out.stop_detail.loglik_upper=INFINITY;
  const auto g=gamma_mle(x,1e-12,maxit);const auto ig=invgamma_mle(x,1e-12,maxit);
  e.budget.used+=g.iterations+ig.iterations;
  if(g.status!=GammaStatus::Ok||ig.status!=GammaStatus::Ok) {
    out.iterations=static_cast<int>(e.budget.used);out.certificate_failure=maxit<=1?LomaxCertificateFailure::Budget:LomaxCertificateFailure::Arithmetic;
    out.stop_detail.budget_exhausted=maxit<=1;return out;
  }
  const auto make_face=[&](double a,bool inverse) {
    Face f;f.a=a;f.model=inverse?StopModel::InvGamma:StopModel::Gamma;
    if(!e.budget.take())return f;
    const double maxlog=inverse?-e.logs.front():e.logs.back(),m=inverse?-e.mean:e.mean;
    betapr_joint_detail::Sum sum;for(double y:e.logs)sum.add(std::exp((inverse?-y:y)-maxlog)/e.n);
    f.la=maxlog+std::log(sum.value);betapr_joint_detail::Sum V;
    for(double y:e.logs){const double z=std::exp((inverse?-y:y)-f.la);V.add(z*z/e.n);}
    f.V=V.value;f.M=std::exp(maxlog-f.la);
    f.ll=a*std::log(a)-special::lgamma(a)-a+a*(m-f.la)-m-(inverse?2*e.mean:0);
    f.error=512*std::numeric_limits<double>::epsilon()*(1+std::abs(f.ll)+std::abs(special::lgamma(a))+a*(1+std::abs(std::log(a))+std::abs(f.la))+(1+a)*(1+e.mag+std::abs(e.lm)));
    const double logscale=inverse?e.lm+std::log(a)-f.la:e.lm+f.la-std::log(a);
    const double scale=std::exp(logscale);f.coef={a,scale};
    const double se=128*std::numeric_limits<double>::epsilon()*(1+std::abs(std::log(a))+std::abs(m)+std::abs(f.la)+e.mag+std::abs(e.lm));
    double radius=1e-5;
    for(int j=0;j<maxit;++j) {
      if(!e.budget.take())return f;
      const double al=a*(1-radius),au=a*(1+radius);
      const double left=std::log(al)-special::digamma(al)+m-f.la,right=std::log(au)-special::digamma(au)+m-f.la;
      if(!(left>se&&right<-se))break;
      f.root={al,au};f.root_enclosed=true;radius*=.5;
    }
    if(f.root_enclosed) {
      const double distance=std::max(a-f.root.lo,f.root.hi-a);
      const double score=std::log(a)-special::digamma(a)+m-f.la;
      f.upper=f.ll+f.error+(std::abs(score)+se)*distance;
      const double pe=std::max(std::abs(std::log(f.root.lo/a)),std::abs(std::log(f.root.hi/a)));
      const double logerror=128*std::numeric_limits<double>::epsilon()*(1+e.mag+std::abs(e.lm)+std::abs(f.la));
      f.accurate=pe+logerror<std::log1p(std::min(tol,1e-6))&&valid_stop_coefficients(f.model,f.coef)&&
        std::numeric_limits<double>::denorm_min()/scale<std::min(1e-7,tol/4);
    }
    return f;
  };
  e.gamma=make_face(g.shape,false);e.inv=make_face(ig.shape,true);
  e.incumbent=std::max(e.gamma.ll-e.gamma.error,e.inv.ll-e.inv.error);
  e.point(1,1);
  if(maxit<=0||e.budget.used>=e.budget.limit)e.budget.exhausted=true;
  // A metered version of the old proposals. No proposal supplies acceptance.
  if(!e.arithmetic&&!e.budget.exhausted) {
    BetaPrimeLogCost cost{span{e.scaled.data(),e.scaled.size()}};
    BfgsControl ctl;ctl.maxit=maxit;ctl.grad_tol=tol;ctl.reltol=reltol;ctl.step_tol=abstol;
    const auto propose=[&](double p,double q,double t) {
      if(e.budget.exhausted)return;
      cost.evaluations=0;cost.exhausted=false;cost.evaluation_limit=e.budget.limit-e.budget.used;
      const double seed[3]={std::log(p),std::log(q),t};
      const auto r=bfgs_minimize(cost,seed,ctl);e.budget.used+=cost.evaluations;
      if(cost.exhausted){e.budget.exhausted=true;return;}
      if(r.theta.size()==3&&std::isfinite(r.f))e.point(std::exp(r.theta[0]),std::exp(r.theta[1]));
    };
    for(double p:{.2,1.,5.,20.,100.})for(double q:{.2,1.,5.,20.,100.}) {
      if(e.budget.exhausted||e.arithmetic)break;
      propose(p,q,e.mean-special::digamma(p)+special::digamma(q));
    }
    if(startp>0||startq>0||starts>0)propose(startp>0?startp:2,startq>0?startq:2,
      starts>0?std::log(starts)-e.lm:e.mean-special::digamma(startp>0?startp:2)+special::digamma(startq>0?startq:2));
  }
  // Reuse the unchanged q-held global certificate as a conditional anchor.
  // The full cover below retains every possible p branch, including ties.
  if(e.best.valid&&!e.arithmetic&&!e.budget.exhausted&&
     e.best.ll-e.best.error>std::max(e.gamma.ll+e.gamma.error,e.inv.ll+e.inv.error)) {
    if(e.budget.take()) {
      const auto inner_maxit=std::min<std::int64_t>(maxit,(e.budget.limit-e.budget.used)/128);
      if(inner_maxit>0) {
        const double q=e.best.q;
        const auto inner=betapr_mle_holds(x,NAN,q,NAN,tol,static_cast<int>(inner_maxit));
        e.budget.used+=std::max(0,inner.iterations);
        if(std::isfinite(inner.stop_detail.loglik_upper)) {
          e.anchor_q=q;e.anchor_upper=inner.stop_detail.loglik_upper/e.n+e.lm+
            512*std::numeric_limits<double>::epsilon()*(1+e.mag+std::abs(e.lm));
        }
        for(const auto& c:inner.stop_detail.candidates)if(c.model==StopModel::None&&c.coef.size()==3) {
          if(!e.budget.exhausted)e.point(c.coef[0],c.coef[1]);
        }
      }
    }
  }
  if(e.best.valid&&!e.arithmetic&&!e.budget.exhausted) {
    const double s=std::exp(e.lm+e.best.t);
    e.exterior=betapr_joint_exterior_bounds(x,e.best.p,e.best.q,s,maxit,e.n*(e.incumbent-e.lm),e.budget.limit-e.budget.used);
    e.budget.used+=e.exterior.evaluations;
    if(e.exterior.status==BetaPrimeJointBoundStatus::Budget)e.budget.exhausted=true;
    const double gu=e.exterior.gamma_upper/e.n+e.lm,iu=e.exterior.invgamma_upper/e.n+e.lm;
    if(!e.gamma.root_enclosed)e.gamma.upper=gu;
    if(!e.inv.root_enclosed)e.inv.upper=iu;
    e.exterior.gamma_upper=e.n*(e.gamma.upper-e.lm);e.exterior.invgamma_upper=e.n*(e.inv.upper-e.lm);
    if(e.budget.take())e.exterior.gamma_signed_shape2=std::min(e.exterior.gamma_signed_shape2,e.completed_cutoff(e.gamma,2*e.exterior.harmonic_upper));
    if(e.budget.take())e.exterior.invgamma_signed_shape1=std::min(e.exterior.invgamma_signed_shape1,e.completed_cutoff(e.inv,2*e.exterior.harmonic_upper));
    double P=std::min(e.exterior.invgamma_shape1,e.exterior.invgamma_signed_shape1),
           Q=std::min(e.exterior.gamma_shape2,e.exterior.gamma_signed_shape2);
    if(!(std::isfinite(P)&&std::isfinite(Q)&&P>e.exterior.harmonic_lower&&Q>e.exterior.harmonic_lower))e.arithmetic=true;
    const bool face_comparison=std::isfinite(e.gamma.upper)&&std::isfinite(e.inv.upper)&&e.gamma.ll-e.gamma.error<=e.inv.upper&&e.inv.ll-e.inv.error<=e.gamma.upper&&
      e.best.ll-e.best.error<=std::max(e.gamma.upper,e.inv.upper);
    if(face_comparison){out.stop_detail.comparison_tie=true;e.arithmetic=true;}
    if(!e.arithmetic&&!e.budget.exhausted) {
      if(e.best.ll-e.best.error>std::max(e.gamma.upper,e.inv.upper))e.patch(e.best);
      if(e.accuracy_unresolved)e.arithmetic=true;
      std::priority_queue<Cell> queue;
      if(!e.arithmetic&&!e.budget.exhausted)e.cell({e.exterior.harmonic_lower,P,e.exterior.harmonic_lower,Q},queue);
      while(!queue.empty()&&!e.arithmetic&&!e.budget.exhausted) {
        const Cell c=queue.top();queue.pop();
        if(c.upper<e.incumbent)continue;
        if(c.fresh){e.cell(c,queue);continue;}
        if(std::log(c.pu/c.pl)>std::log(c.qu/c.ql)) {
          const double mid=std::sqrt(c.pl)*std::sqrt(c.pu);
          if(!(mid>c.pl&&mid<c.pu)){e.arithmetic=true;break;}
          if(!e.cell({c.pl,mid,c.ql,c.qu},queue))break;
          if(!e.cell({mid,c.pu,c.ql,c.qu},queue))break;
        }else {
          const double mid=std::sqrt(c.ql)*std::sqrt(c.qu);
          if(!(mid>c.ql&&mid<c.qu)){e.arithmetic=true;break;}
          if(!e.cell({c.pl,c.pu,c.ql,mid},queue))break;
          if(!e.cell({c.pl,c.pu,mid,c.qu},queue))break;
        }
      }
    }
  }
  out.joint_exterior=e.exterior;out.iterations=static_cast<int>(e.budget.used);
  out.joint_exterior.total_evaluations=e.budget.used;out.joint_exterior.cover_cells=e.cells;
  out.joint_exterior.profile_evaluations=e.cache.size();out.joint_exterior.retained_maxima=static_cast<int>(e.patches.size());
  out.stop_detail.loglik_upper=INFINITY;
  const auto original=[&](double ll){return e.n*(ll-e.lm);};
  const auto add_face=[&](const Face& f){if(valid_stop_coefficients(f.model,f.coef)&&std::isfinite(f.ll+f.error))out.stop_detail.candidates.push_back({f.coef,original(f.ll-f.error),original(f.ll+f.error),f.model});};
  add_face(e.gamma);add_face(e.inv);
  if(e.best.valid) {
    const double s=std::exp(e.lm+e.best.t);
    if(s>0&&std::isfinite(s))out.stop_detail.candidates.push_back({{e.best.p,e.best.q,s},original(e.best.ll-e.best.error),original(e.best.ll+e.best.error)});
  }
  for(const auto& p:e.patches){const double s=std::exp(e.lm+p.point.t);if(s>0&&std::isfinite(s))out.stop_detail.candidates.push_back({{p.point.p,p.point.q,s},original(p.point.ll-p.point.error),original(p.upper)});}
  out.stop_detail.loglik_lower=original(e.incumbent);
  if(std::isfinite(e.last_upper))out.stop_detail.segment_loglik_upper=original(e.last_upper);
  if(e.budget.exhausted){out.certificate_failure=LomaxCertificateFailure::Budget;out.stop_detail.budget_exhausted=true;return out;}
  if(e.arithmetic||!e.best.valid){out.certificate_failure=LomaxCertificateFailure::Arithmetic;return out;}
  // Every finite maximum lies in a certified, unique-root patch now.
  double winner_lower=e.gamma.ll-e.gamma.error;int winner=-1;
  if(e.inv.ll-e.inv.error>winner_lower){winner_lower=e.inv.ll-e.inv.error;winner=-2;}
  for(std::size_t i=0;i<e.patches.size();++i)if(e.patches[i].point.ll-e.patches[i].point.error>winner_lower){winner_lower=e.patches[i].point.ll-e.patches[i].point.error;winner=static_cast<int>(i);}
  double competitor=-INFINITY,upper=-INFINITY;
  if(winner!=-1)competitor=std::max(competitor,e.gamma.upper);
  if(winner!=-2)competitor=std::max(competitor,e.inv.upper);
  upper=std::max(e.gamma.upper,e.inv.upper);
  for(std::size_t i=0;i<e.patches.size();++i){upper=std::max(upper,e.patches[i].upper);if(winner!=static_cast<int>(i))competitor=std::max(competitor,e.patches[i].upper);}
  out.stop_detail.loglik_upper=original(upper);
  if(!(winner_lower>competitor)){out.stop_detail.comparison_tie=true;return out;}
  if(winner<0) {
    const Face& f=winner==-1?e.gamma:e.inv;
    if(!f.accurate){out.certificate_failure=LomaxCertificateFailure::Arithmetic;return out;}
    out.alpha=winner==-1?f.a:INFINITY;out.beta=winner==-1?INFINITY:f.a;out.scale=winner==-1?INFINITY:0;
    out.logLik=original(f.ll);out.status=winner==-1?BetaPrimeStatus::GammaBoundary:BetaPrimeStatus::InvGammaBoundary;
    out.stop_detail.limit_model=f.model;out.stop_detail.limit_coef=f.coef;out.stop_detail.limit_loglik=out.logLik;
    out.stop_detail.loglik_upper=original(f.upper);out.joint_exterior.global_certified=true;return out;
  }
  const Patch& p=e.patches[static_cast<std::size_t>(winner)];
  const double scale=std::exp(e.lm+p.point.t);
  if(!(scale>0)||!std::isfinite(scale)||std::numeric_limits<double>::denorm_min()/scale>1e-7) {out.certificate_failure=LomaxCertificateFailure::Arithmetic;return out;}
  const double se=128*std::numeric_limits<double>::epsilon()*(1+std::abs(e.lm));
  const double sl=std::exp(e.lm+p.t.lo-se),su=std::exp(e.lm+p.t.hi+se);
  if(!(sl>0)||!std::isfinite(su)||std::max(std::abs(std::log(scale/sl)),std::abs(std::log(su/scale)))>std::log1p(std::min(1e-6,tol))) {out.certificate_failure=LomaxCertificateFailure::Arithmetic;return out;}
  out.joint_exterior.coefficient_lower={p.p.lo,p.q.lo,sl};out.joint_exterior.coefficient_upper={p.p.hi,p.q.hi,su};out.joint_exterior.global_certified=true;
  out.alpha=p.point.p;out.beta=p.point.q;out.scale=scale;out.logLik=original(p.point.ll);out.status=BetaPrimeStatus::Ok;
  out.stop_detail.loglik_lower=original(p.point.ll-p.point.error);return out;
}
} // namespace betapr_joint_cert
inline BetaPrimeResult betapr_mle(span x,double tol,int maxit,double shape1,
    double shape2,double scale,double reltol,double abstol) {
  return betapr_detail::fallback(betapr_joint_cert::fit(x,tol,maxit,shape1,shape2,scale,reltol,abstol),x,NAN,NAN,NAN,tol,maxit,shape1,shape2,scale);
}
} // namespace cppunivariate
