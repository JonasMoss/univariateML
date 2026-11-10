#pragma once
#include "dpqr.hpp"
#include "../../core/span.hpp"
#include "../../core/work_budget.hpp"
#include <array>
#include <vector>
#include <limits>
namespace cppunivariate {
namespace bhatt_detail {
// Same comparison-conditional interval and root construction as EMG, kept
// family-local so the shared logconcave profile and EMG behavior do not change.
struct CertInterval {
  double lo=0,hi=0;
  CertInterval(double x=0):lo(x),hi(x){}
  CertInterval(double l,double h):lo(l),hi(h){}
  bool valid() const {return std::isfinite(lo)&&std::isfinite(hi)&&lo<=hi;}
  double mag() const {return std::max(std::abs(lo),std::abs(hi));}
};
inline CertInterval cert_pad(double lo,double hi) {
  if(!(std::isfinite(lo)&&std::isfinite(hi)&&lo<=hi))return {NAN,NAN};
  const double e=32*std::numeric_limits<double>::epsilon()*std::max(std::abs(lo),std::abs(hi))+
    std::numeric_limits<double>::denorm_min();
  return {lo-e,hi+e};
}
inline CertInterval operator+(CertInterval a,CertInterval b) {return cert_pad(a.lo+b.lo,a.hi+b.hi);}
inline CertInterval operator-(CertInterval a) {return {-a.hi,-a.lo};}
inline CertInterval operator-(CertInterval a,CertInterval b) {return a+-b;}
inline CertInterval operator*(CertInterval a,CertInterval b) {
  const std::array<double,4> v{{a.lo*b.lo,a.lo*b.hi,a.hi*b.lo,a.hi*b.hi}};
  for(double x:v)if(!std::isfinite(x))return {NAN,NAN};
  return cert_pad(*std::min_element(v.begin(),v.end()),*std::max_element(v.begin(),v.end()));
}
inline CertInterval operator/(CertInterval a,CertInterval b) {
  if(!b.valid()||!(b.lo>0||b.hi<0))return {NAN,NAN};
  return a*cert_pad(1/b.hi,1/b.lo);
}
inline CertInterval cert_square(CertInterval x) {
  if(!x.valid())return {NAN,NAN};
  return cert_pad(x.lo<=0&&x.hi>=0?0:std::min(x.lo*x.lo,x.hi*x.hi),std::max(x.lo*x.lo,x.hi*x.hi));
}
struct CertSum {
  CPPUNIV_EXTENDED_REAL value=0,correction=0;
  void add(CPPUNIV_EXTENDED_REAL x) {const CPPUNIV_EXTENDED_REAL y=x-correction,t=value+y;correction=(t-value)-y;value=t;}
};
struct CertIntervalSum {
  CertSum lo{},hi{};
  void add(CertInterval x) {lo.add(x.lo);hi.add(x.hi);}
  CertInterval get() const {return cert_pad(static_cast<double>(lo.value),static_cast<double>(hi.value));}
};

// Positive held a: endpoints b*(x +/- a)-u are affine. Free a with held
// sigma: endpoints b*x-u +/- k are affine. The only nonconcave term in
// the latter case is -n*log(k).
struct CertModel {
  span x; double mu,sigma,a,kfixed;
  double origin=0,unit=1;
  int dim=0,location=-1,scale=-1,shape=-1;
  double bfixed;
  CertModel(span data,double m,double s,double width,bool outer=false,double k=NAN):
    x(data),mu(m),sigma(s),a(width),kfixed(k),bfixed(std::isnan(s)?NAN:1/s) {
    if(std::isnan(mu))location=dim++;
    if(std::isnan(sigma))scale=dim++;
    if(outer)shape=dim++;
  }
  std::array<double,3> initial(double k=1)const {
    CertSum sum,sq;for(double v:x)sum.add(v);
    const double mean=static_cast<double>(sum.value/x.size());
    for(double v:x){const CPPUNIV_EXTENDED_REAL d=static_cast<CPPUNIV_EXTENDED_REAL>(v)-(std::isnan(mu)?mean:mu);sq.add(d*d);}
    double b=bfixed;
    if(scale>=0)b=sq.value>0?static_cast<double>(std::sqrt(x.size()/sq.value))*(std::isnan(a)?std::sqrt(1+k*k/3):1):1;
    std::array<double,3> p{};
    if(location>=0)p[location]=b*mean;
    if(scale>=0)p[scale]=b;
    if(shape>=0)p[shape]=k;
    return p;
  }
  void coordinates(const std::array<double,3>& p,double& m,double& s,double& width)const {
    const double b=scale>=0?p[scale]:bfixed;
    m=location>=0?p[location]/b:mu;s=1/b;
    width=std::isnan(a)?(shape>=0?p[shape]:kfixed)/b:a;
  }
  bool domain(const std::array<double,3>& p)const {
    for(int j=0;j<dim;++j)if(!std::isfinite(p[j]))return false;
    if(scale>=0&&!(p[scale]>0))return false;
    if(shape>=0&&!(p[shape]>0))return false;
    double m,s,w;coordinates(p,m,s,w);
    return std::isfinite(m)&&s>0&&w>0&&std::isfinite(s)&&std::isfinite(w);
  }
  std::array<CertInterval,3> coefficients(const std::array<CertInterval,3>& p)const {
    const CertInterval b=scale>=0?p[scale]:CertInterval(bfixed);
    const CertInterval k=shape>=0?p[shape]:CertInterval(kfixed);
    return {{location>=0?p[location]/b:CertInterval(mu),CertInterval(1)/b,std::isnan(a)?k/b:CertInterval(a)}};
  }
};
inline CPPUNIV_EXTENDED_REAL cert_logmass(CPPUNIV_EXTENDED_REAL z,CPPUNIV_EXTENDED_REAL k) {
  if(!(k>0&&std::isfinite(z)&&std::isfinite(k)))return NAN;
  const CPPUNIV_EXTENDED_REAL lo=z-k,hi=z+k;
  if(!(hi>lo))return NAN;
  return lo>=0?logdiff_extended(logQ_extended(lo),logQ_extended(hi)):
    logdiff_extended(logQ_extended(-hi),logQ_extended(-lo));
}
// Subtracting nearby normal tails amplifies their log-probability error.
// Preserve that conditioning factor rather than treating log(D) as a
// uniformly accurate special function. Wide central intervals lose less
// than 4e-33 when the omitted tails are bounded at +/-12.
inline CPPUNIV_EXTENDED_REAL cert_logmass_error(CPPUNIV_EXTENDED_REAL z,CPPUNIV_EXTENDED_REAL k) {
  const CPPUNIV_EXTENDED_REAL lo=z-k,hi=z+k;
  if(!(hi>lo))return INFINITY;
  if(lo < -12&&hi > 12)return 64*std::numeric_limits<double>::epsilon();
  const CPPUNIV_EXTENDED_REAL left=lo>=0?logQ_extended(lo):logQ_extended(-hi);
  const CPPUNIV_EXTENDED_REAL right=lo>=0?logQ_extended(hi):logQ_extended(-lo);
  const CPPUNIV_EXTENDED_REAL gap=-std::expm1(right-left);
  if(!(gap>0&&std::isfinite(left)&&std::isfinite(right)))return INFINITY;
  return 64*std::numeric_limits<double>::epsilon()*(1+std::abs(left)+std::abs(right))/gap;
}
inline CertInterval cert_exp(CertInterval v) {
  if(!v.valid())return {NAN,NAN};
  return cert_pad(std::exp(v.lo),std::exp(v.hi));
}
inline CertInterval cert_logmass_range(CertInterval z,CertInterval k) {
  if(!z.valid()||!k.valid()||!(k.lo>0))return {NAN,NAN};
  const double nearest=z.lo<=0&&z.hi>=0?0:std::min(std::abs(z.lo),std::abs(z.hi));
  const CPPUNIV_EXTENDED_REAL lower=cert_logmass(z.mag(),k.lo),upper=cert_logmass(nearest,k.hi);
  const CPPUNIV_EXTENDED_REAL common=256*std::numeric_limits<double>::epsilon()*(1+std::max(std::abs(lower),std::abs(upper)));
  const CPPUNIV_EXTENDED_REAL le=common+cert_logmass_error(z.mag(),k.lo),he=common+cert_logmass_error(nearest,k.hi);
  return {static_cast<double>(lower-le),static_cast<double>(upper+he)};
}
inline std::array<std::array<CertInterval,3>,3> cert_hessian(const CertModel& model,const std::array<CertInterval,3>& p) {
  std::array<std::array<CertInterval,3>,3> out{};
  std::array<std::array<CertIntervalSum,3>,3> sums{};
  const CertInterval b=model.scale>=0?p[model.scale]:CertInterval(model.bfixed);
  const CertInterval k=std::isnan(model.a)?(model.shape>=0?p[model.shape]:CertInterval(model.kfixed)):b*CertInterval(model.a);
  const double n=static_cast<double>(model.x.size());
  if(std::isnan(model.a)&&model.scale>=0)sums[model.scale][model.scale].add(-CertInterval(n)/cert_square(b));
  if(model.shape>=0)sums[model.shape][model.shape].add(CertInterval(n)/cert_square(k));
  for(double v:model.x) {
    const CertInterval z=model.location>=0?b*CertInterval(v)-p[model.location]:b*(CertInterval(v)-CertInterval(model.mu));
    const CertInterval lo=z-k,hi=z+k,ld=cert_logmass_range(z,k);
    const CertInterval A=cert_exp(CertInterval(-.91893853320467274178)-CertInterval(.5)*cert_square(lo)-ld);
    const CertInterval B=cert_exp(CertInterval(-.91893853320467274178)-CertInterval(.5)*cert_square(hi)-ld);
    const CertInterval gz=B-A,gk=B+A;
    const CertInterval common=lo*A-hi*B;
    const CertInterval zz=common-cert_square(gz),kk=common-cert_square(gk),zk=-lo*A-hi*B-gz*gk;
    std::array<CertInterval,3> dz{},dk{};
    if(model.location>=0)dz[model.location]=CertInterval(-1);
    if(model.scale>=0){dz[model.scale]=model.location>=0?CertInterval(v):CertInterval(v)-CertInterval(model.mu);if(!std::isnan(model.a))dk[model.scale]=CertInterval(model.a);}
    if(model.shape>=0)dk[model.shape]=CertInterval(1);
    for(int i=0;i<model.dim;++i)for(int j=0;j<model.dim;++j)
      sums[i][j].add(zz*dz[i]*dz[j]+zk*(dz[i]*dk[j]+dk[i]*dz[j])+kk*dk[i]*dk[j]);
  }
  for(int i=0;i<model.dim;++i)for(int j=0;j<model.dim;++j)out[i][j]=sums[i][j].get();
  return out;
}
struct CertEvaluation {
  double ll=NAN,ll_error=INFINITY;
  std::array<CertInterval,3> gradient{};
  std::array<std::array<CertInterval,3>,3> hessian{};
  bool ok=false;
};
inline CertEvaluation cert_evaluate(const CertModel& model,const std::array<double,3>& p) {
  CertEvaluation out;if(!model.domain(p))return out;
  const CPPUNIV_EXTENDED_REAL b=model.scale>=0?p[model.scale]:model.bfixed;
  const CPPUNIV_EXTENDED_REAL k=std::isnan(model.a)?(model.shape>=0?p[model.shape]:model.kfixed):b*model.a;
  const CPPUNIV_EXTENDED_REAL u=model.location>=0?p[model.location]:b*model.mu,n=model.x.size();
  std::array<CertSum,3> sums{},errors{};CertSum ll,mag,input_error,mass_error;
  const CPPUNIV_EXTENDED_REAL normalizer=std::isnan(model.a)?std::log(b)-std::log(2*k):-std::log(2*static_cast<CPPUNIV_EXTENDED_REAL>(model.a));
  for(double v:model.x) {
    const CPPUNIV_EXTENDED_REAL z=model.location>=0?b*v-u:b*(static_cast<CPPUNIV_EXTENDED_REAL>(v)-model.mu),lo=z-k,hi=z+k,ld=cert_logmass(z,k);
    const CPPUNIV_EXTENDED_REAL A=std::exp(logphi_extended(lo)-ld),B=std::exp(logphi_extended(hi)-ld),gz=B-A,gk=B+A;
    ll.add(normalizer+ld);mag.add(std::abs(normalizer)+std::abs(ld));
    const CPPUNIV_EXTENDED_REAL input=1+std::abs(b*v)+std::abs(u)+std::abs(k);
    const CPPUNIV_EXTENDED_REAL masserr=cert_logmass_error(z,k);mass_error.add(masserr);
    const CPPUNIV_EXTENDED_REAL err=(A+B)*masserr+64*std::numeric_limits<double>::epsilon()*
      (A*(1+std::abs(ld)+lo*lo)+B*(1+std::abs(ld)+hi*hi)+input*(A*(1+std::abs(lo))+B*(1+std::abs(hi))))+
      64*std::numeric_limits<double>::denorm_min();
    input_error.add(64*std::numeric_limits<double>::epsilon()*input*(A+B));
    if(model.location>=0){sums[model.location].add(-gz);errors[model.location].add(err);}
    if(model.scale>=0){const CPPUNIV_EXTENDED_REAL d=model.location>=0?v:static_cast<CPPUNIV_EXTENDED_REAL>(v)-model.mu;
      sums[model.scale].add(d*gz+(std::isnan(model.a)?0:static_cast<CPPUNIV_EXTENDED_REAL>(model.a)*gk));
      errors[model.scale].add(err*(std::abs(d)+(std::isnan(model.a)?0:model.a)));}
    if(model.shape>=0){sums[model.shape].add(gk);errors[model.shape].add(err);}
  }
  if(std::isnan(model.a)&&model.scale>=0){sums[model.scale].add(n/b);errors[model.scale].add(64*std::numeric_limits<double>::epsilon()*n/b);}
  if(model.shape>=0){sums[model.shape].add(-n/k);errors[model.shape].add(64*std::numeric_limits<double>::epsilon()*n/k);}
  std::array<CertInterval,3> point{};
  for(int j=0;j<model.dim;++j){const CPPUNIV_EXTENDED_REAL e=errors[j].value+64*std::numeric_limits<double>::epsilon()*std::abs(sums[j].value);
    out.gradient[j]={static_cast<double>(sums[j].value-e),static_cast<double>(sums[j].value+e)};point[j]=CertInterval(p[j]);}
  out.ll=static_cast<double>(ll.value);out.ll_error=static_cast<double>(1024*std::numeric_limits<double>::epsilon()*(n+mag.value)+input_error.value+mass_error.value);
  out.hessian=cert_hessian(model,point);out.ok=std::isfinite(out.ll)&&std::isfinite(out.ll_error);
  for(int i=0;i<model.dim;++i){out.ok=out.ok&&out.gradient[i].valid();for(int j=0;j<model.dim;++j)out.ok=out.ok&&out.hessian[i][j].valid();}
  return out;
}
inline bool cert_inverse(std::array<std::array<double,3>,3> a,int n,std::array<std::array<double,3>,3>& inverse) {
  inverse={};for(int i=0;i<n;++i)inverse[i][i]=1;
  for(int j=0;j<n;++j) {
    int pivot=j;for(int i=j+1;i<n;++i)if(std::abs(a[i][j])>std::abs(a[pivot][j]))pivot=i;
    if(!(std::abs(a[pivot][j])>std::numeric_limits<double>::min()))return false;
    std::swap(a[pivot],a[j]);std::swap(inverse[pivot],inverse[j]);
    const double d=a[j][j];
    for(int k=0;k<n;++k){a[j][k]/=d;inverse[j][k]/=d;}
    for(int i=0;i<n;++i)if(i!=j){const double v=a[i][j];for(int k=0;k<n;++k){a[i][k]-=v*a[j][k];inverse[i][k]-=v*inverse[j][k];}}
  }
  for(int i=0;i<n;++i)for(int j=0;j<n;++j)if(!std::isfinite(inverse[i][j]))return false;
  return true;
}
inline bool cert_negative_definite(const std::array<std::array<CertInterval,3>,3>& h,int n) {
  if(n==0)return true;
  if(!h[0][0].valid()||!(-h[0][0]).valid()||!((-h[0][0]).lo>0))return false;
  if(n==1)return true;
  const CertInterval d2=h[0][0]*h[1][1]-h[0][1]*h[1][0];
  if(!d2.valid()||!(d2.lo>0))return false;
  if(n==2)return true;
  const CertInterval determinant=h[0][0]*(h[1][1]*h[2][2]-h[1][2]*h[2][1])-
    h[0][1]*(h[1][0]*h[2][2]-h[1][2]*h[2][0])+h[0][2]*(h[1][0]*h[2][1]-h[1][1]*h[2][0]);
  return determinant.valid()&&determinant.hi<0;
}
struct CertRoot {
  std::array<double,3> center{},scale{};
  std::array<CertInterval,3> enclosure{},box{};
  CertEvaluation evaluation{};
  double radius=0,error=INFINITY,ll_lower=-INFINITY,ll_upper=INFINITY;
  bool ok=false,accurate=false;
};
inline CertRoot cert_validate_root(const CertModel& model,const std::array<double,3>& p,double tol,int maxit) {
  CertRoot out;out.center=p;out.evaluation=cert_evaluate(model,p);if(!out.evaluation.ok)return out;
  if(model.dim==0){out.ok=out.accurate=true;out.ll_lower=out.evaluation.ll-out.evaluation.ll_error;out.ll_upper=out.evaluation.ll+out.evaluation.ll_error;return out;}
  std::array<std::array<double,3>,3> point{},inverse{};
  for(int i=0;i<model.dim;++i)out.scale[i]=(i==model.scale||i==model.shape)?p[i]:std::max(1.,std::abs(p[i]));
  for(int i=0;i<model.dim;++i)for(int j=0;j<model.dim;++j)point[i][j]=(out.evaluation.hessian[i][j].lo/2+out.evaluation.hessian[i][j].hi/2)*out.scale[i]*out.scale[j];
  if(!cert_inverse(point,model.dim,inverse))return out;
  double initial_displacement=0;
  for(int i=0;i<model.dim;++i) {
    CertInterval d;
    for(int j=0;j<model.dim;++j)d=d-CertInterval(inverse[i][j]*out.scale[j])*out.evaluation.gradient[j];
    initial_displacement=std::max(initial_displacement,d.mag());
  }
  if(!(initial_displacement<1e-3))return out;
  double radius=1e-3;
  for(int it=0;it<solver_budget(maxit);++it,radius*=.5) {
    if(radius<512*std::numeric_limits<double>::epsilon())return out;
    std::array<CertInterval,3> box{};
    for(int i=0;i<model.dim;++i)box[i]=cert_pad(p[i]-radius*out.scale[i],p[i]+radius*out.scale[i]);
    const auto h=cert_hessian(model,box);if(!cert_negative_definite(h,model.dim))continue;
    double q=0,displacement=0;
    for(int i=0;i<model.dim;++i) {
      CertInterval step;double row=0;
      for(int j=0;j<model.dim;++j)step=step-CertInterval(inverse[i][j]*out.scale[j])*out.evaluation.gradient[j];
      displacement=std::max(displacement,step.mag());
      for(int j=0;j<model.dim;++j) {
        CertInterval value(i==j?1.:0.);
        for(int k=0;k<model.dim;++k)value=value-CertInterval(inverse[i][k]*out.scale[k]*out.scale[j])*h[k][j];
        row+=value.mag();
      }
      q=std::max(q,row);
    }
    if(!(q<.5&&std::isfinite(displacement)&&displacement+q*radius<radius))continue;
    out.error=displacement/(1-q);out.radius=radius;out.box=box;
    double gap=0;
    for(int i=0;i<model.dim;++i) {
      const double e=out.scale[i]*out.error;
      out.enclosure[i]=cert_pad(p[i]-e,p[i]+e);gap+=out.evaluation.gradient[i].mag()*
        std::max(std::abs(out.enclosure[i].lo-p[i]),std::abs(out.enclosure[i].hi-p[i]));
    }
    auto coef=model.coefficients(out.enclosure);
    coef[0]=CertInterval(model.origin)+CertInterval(model.unit)*coef[0];
    coef[1]=CertInterval(model.unit)*coef[1];coef[2]=CertInterval(model.unit)*coef[2];
    double mu,sigma,a;model.coordinates(p,mu,sigma,a);
    out.accurate=true;
    const std::array<double,3> center{{std::fma(model.unit,mu,model.origin),model.unit*sigma,model.unit*a}};
    for(int i=0;i<3;++i) {
      if((i==0&&!std::isnan(model.mu))||(i==1&&!std::isnan(model.sigma))||(i==2&&!std::isnan(model.a)))continue;
      const double floor=i==0?coef[1].lo:coef[i].lo;
      const double target=tol*std::max(floor,i==0?(coef[0].lo<=0&&coef[0].hi>=0?0.:std::min(std::abs(coef[0].lo),std::abs(coef[0].hi))):floor);
      out.accurate=out.accurate&&coef[i].valid()&&target>0&&
        std::max(std::abs(center[i]-coef[i].lo),std::abs(coef[i].hi-center[i]))<=target;
    }
    out.ll_lower=out.evaluation.ll-out.evaluation.ll_error;
    out.ll_upper=out.evaluation.ll+out.evaluation.ll_error+gap;out.ok=std::isfinite(out.ll_lower)&&std::isfinite(out.ll_upper);return out;
  }
  return out;
}
inline CertRoot cert_newton(const CertModel& model,std::array<double,3> p,double tol,int maxit) {
  CertRoot last;
  for(int it=0;it<solver_budget(maxit);++it) {
    const auto e=cert_evaluate(model,p);if(!e.ok)return last;
    auto validated=cert_validate_root(model,p,tol,maxit);
    if(validated.ok&&validated.accurate)return validated;
    if(validated.ok)last=validated;
    std::array<std::array<double,3>,3> h{},inverse{};
    for(int i=0;i<model.dim;++i)for(int j=0;j<model.dim;++j)h[i][j]=e.hessian[i][j].lo/2+e.hessian[i][j].hi/2;
    if(!cert_inverse(h,model.dim,inverse))return last;
    std::array<double,3> delta{};
    double ascent=0;
    for(int i=0;i<model.dim;++i) {for(int j=0;j<model.dim;++j)delta[i]-=inverse[i][j]*(e.gradient[j].lo/2+e.gradient[j].hi/2);ascent+=delta[i]*(e.gradient[i].lo/2+e.gradient[i].hi/2);}
    if(!(ascent>0&&std::isfinite(ascent)))return last;
    if(validated.ok){
      // Once the proposed correction is small relative to the enclosure's
      // uncertainty, more point Newton steps do not supply the missing
      // coefficient evidence. Keep the valid (possibly inaccurate) enclosure
      // for profile bounds; public acceptance still requires accurate=true.
      double relative_step=0;
      for(int i=0;i<model.dim;++i)relative_step=std::max(relative_step,std::abs(delta[i])/validated.scale[i]);
      if(relative_step<=validated.error/4)return last;
    }
    double step=1;bool accepted=false;
    for(int backtrack=0;backtrack<solver_budget(maxit);++backtrack,step*=.5) {
      auto next=p;for(int i=0;i<model.dim;++i)next[i]+=step*delta[i];
      bool changed=false;for(int i=0;i<model.dim;++i)changed=changed||next[i]!=p[i];
      if(!changed)return last;
      const auto candidate=cert_evaluate(model,next);
      if(candidate.ok&&candidate.ll>=e.ll-8*std::numeric_limits<double>::epsilon()*(1+std::abs(e.ll))) {p=next;accepted=true;break;}
    }
    if(!accepted)return last;
  }
  return last;
}
}
}
