// Exact-arithmetic certificate conditional on practical binary64 enclosures.
// See README.md and the TASK-140 derivation; no outward rounding is claimed.
#pragma once
#include "../../core/span.hpp"
#include "../../core/stop_detail.hpp"
#include "../../core/constants.hpp"
#include <algorithm>
#include <array>
#include <cmath>
#include <limits>
#include <map>
#include <numeric>
#include <queue>
#include <vector>
namespace cppunivariate {
namespace t_cert_detail {
constexpr double eps=64*std::numeric_limits<double>::epsilon();
struct I {
  double lo=0,hi=0;
  I(double v=0):lo(v),hi(v){}
  I(double l,double h):lo(l),hi(h){}
  bool valid()const{return std::isfinite(lo)&&std::isfinite(hi)&&lo<=hi;}
  double mag()const{return std::max(std::abs(lo),std::abs(hi));}
  double mid()const{return lo/2+hi/2;}
};
inline I bad(){return {NAN,NAN};}
inline I pad(double l,double h){if(!std::isfinite(l)||!std::isfinite(h)||l>h)return bad();double e=eps*std::max(std::abs(l),std::abs(h))+std::numeric_limits<double>::denorm_min();return {l-e,h+e};}
inline I operator+(I a,I b){return pad(a.lo+b.lo,a.hi+b.hi);}
inline I operator-(I a){return {-a.hi,-a.lo};}
inline I operator-(I a,I b){return a+-b;}
inline I operator*(I a,I b){std::array<double,4> p{{a.lo*b.lo,a.lo*b.hi,a.hi*b.lo,a.hi*b.hi}};for(double v:p)if(!std::isfinite(v))return bad();return pad(*std::min_element(p.begin(),p.end()),*std::max_element(p.begin(),p.end()));}
inline I operator/(I a,I b){if(!b.valid()||!(b.lo>0||b.hi<0))return bad();const double lo=1/b.hi,hi=1/b.lo;
  const double error=eps*std::max(std::abs(lo),std::abs(hi))+std::numeric_limits<double>::denorm_min();
  return a*I(lo-error,hi+error);}
inline I sq(I a){if(!a.valid())return bad();return pad(a.lo<=0&&a.hi>=0?0:std::min(a.lo*a.lo,a.hi*a.hi),std::max(a.lo*a.lo,a.hi*a.hi));}
inline I exp(I a){return a.valid()?pad(std::exp(a.lo),std::exp(a.hi)):bad();}
inline I log(I a){return a.valid()&&a.lo>0?pad(std::log(a.lo),std::log(a.hi)):bad();}
inline I log1p(I a){return a.valid()&&a.lo>-1?pad(std::log1p(a.lo),std::log1p(a.hi)):bad();}
inline I scale_units(I a,double unit){
  double lo=a.lo*unit,hi=a.hi*unit,error=eps*std::max(std::abs(lo),std::abs(hi));return {lo-error,hi+error};
}
inline I location_units(I a,double unit,double center){
  I b=scale_units(a,unit);double error=eps*(std::abs(center)+b.mag());return {b.lo+center-error,b.hi+center+error};
}
inline I root(I a){return a.valid()&&a.hi>=0?pad(std::sqrt(std::max(0.,a.lo)),std::sqrt(a.hi)):bad();}
struct Sum {
  std::array<double,64> lower{},upper{};double mag=0;std::size_t n=0;
  void add(I v){
    double lo=v.lo,hi=v.hi;std::size_t count=n;int level=0;
    while(count&1u){lo+=lower[level];hi+=upper[level];count>>=1;++level;}
    lower[level]=lo;upper[level]=hi;mag+=v.mag();++n;
  }
  I get()const{
    double lo=0,hi=0;std::size_t count=n;int level=0;
    while(count){if(count&1u){lo+=lower[level];hi+=upper[level];}count>>=1;++level;}
    // Binary carry sums have at most 2*ceil(log2(n))+2 rounded additions
    // on a path. Enlarge the separately accumulated absolute sum too.
    double error=eps*(2*level+2)*(1+mag*(1+2*eps*n));
    return {lo-error,hi+error};
  }
};
// Alternating Stieltjes remainders of tanh's partial fractions, integrated
// against exp(-v*u). Recurrence shifts v to >=32 (at most 16 steps).
// order 0: normal log-normalizer correction; 1: c'; 2: -c''.
inline I gamma(double v,int order){
  if(!(v>=.5)||!std::isfinite(v))return bad();
  double q=v,shift=0,mag=0;int shifts=0;
  for(int j=0;j<16&&q<32;++j,q+=2){
    ++shifts;
    double d=order==0?std::log1p(1/q)-.5*std::log1p(2/q):
      order==1?1/(q*(q+1)):order==3?1/(q*(q+1)*(q+2)):(2*q+1)/(q*q*(q+1)*(q+1));
    shift+=d;mag+=std::abs(d);
  }
  double z=1/q,z2=z*z,value=0,remainder=0;
  if(order==0){value=-z/4+z*z2/24-z*z2*z2/20+17*z*z2*z2*z2/112;remainder=31*std::pow(z,9)/36;value-=shift;}
  if(order==1){value=z/2+z2/4-std::pow(z,4)/8+std::pow(z,6)/4-17*std::pow(z,8)/16;remainder=31*std::pow(z,10)/4;value+=shift;}
  if(order==2){value=z2/2+std::pow(z,3)/2-std::pow(z,5)/2+1.5*std::pow(z,7)-8.5*std::pow(z,9);remainder=77.5*std::pow(z,11);value+=shift;}
  if(order==3){value=z2/4-std::pow(z,4)/8+std::pow(z,6)/4-17*std::pow(z,8)/16;remainder=31*std::pow(z,10)/4;value+=shift;}
  const double e=eps*(8+2*shifts)*(mag+std::abs(value)+std::numeric_limits<double>::min());
  return {value-remainder-e,value+remainder+e};
}
inline I derivative(I v,int order){if(!v.valid()||v.lo<.5)return bad();return {gamma(v.hi,order).lo,gamma(v.lo,order).hi};}
// Monotone u-log(1+u), avoiding subtraction at the normal seam.
inline double log_remainder(double u){
  if(u<0)return NAN;
  if(u>.01)return u-std::log1p(u);
  double term=u*u,result=term/2;
  for(int k=3;k<=14;++k){term*=-u;result+=term/k;}
  return result;
}
inline I lr(I a){
  if(!a.valid()||a.hi<0)return bad();
  auto at=[](double u){
    u=std::max(0.,u);const double value=log_remainder(u);
    // The direct subtraction loses relative accuracy near its cutoff.
    const double size=u>.01?u+std::abs(std::log1p(u)):std::abs(value);
    const double error=eps*size+std::numeric_limits<double>::denorm_min();
    return I(value-error,value+error);
  };
  return {at(a.lo).lo,at(a.hi).hi};
}
inline double normal_loss(double u){
  if(std::abs(u)>.01)return u+std::expm1(-2*u)/2;
  double term=u*u,result=term;
  for(int k=3;k<=16;++k){term*=(-2*u)/k;result+=term;}
  return result;
}
inline I nl(I a){
  if(!a.valid())return bad();
  auto at=[](double u){
    const double value=normal_loss(u);
    const double size=std::abs(u)>.01?std::abs(u)+std::abs(std::expm1(-2*u))/2:std::abs(value);
    const double error=eps*size+std::numeric_limits<double>::denorm_min();
    return I(value-error,value+error);
  };
  const I left=at(a.lo),right=at(a.hi);
  return {a.lo<=0&&a.hi>=0?0:std::min(left.lo,right.lo),std::max(left.hi,right.hi)};
}
struct Data {
  std::vector<double> y,original;std::vector<I> x;
  double center=0,unit=1,jac=0,atom=0;I mean,var,normal;std::size_t mass=0;
};
inline bool data(span x,Data& d){
  if(x.size()<2)return false;
  d.original.assign(x.begin(),x.end());for(double v:x)if(!std::isfinite(v))return false;
  std::sort(d.original.begin(),d.original.end());
  std::size_t run=0;for(std::size_t j=0;j<x.size();++j){run=j&&d.original[j]==d.original[j-1]?run+1:1;if(run>d.mass){d.mass=run;d.atom=d.original[j];}}
  const double base=d.original[x.size()/2],range=std::max(std::abs(d.original.back()/2-base/2),std::abs(d.original.front()/2-base/2))*2;
  if(!(range>0)||!std::isfinite(range))return false;
  double m=0;for(double v:d.original)m+=((v-base)/range)/x.size();
  d.center=base+range*m;double ss=0;for(double v:d.original){double z=(v-d.center)/range;ss+=z*z/x.size();}
  d.unit=range*std::sqrt(ss);if(!(d.unit>0)||!std::isfinite(d.unit))return false;
  Sum sum;for(double v:d.original){const double value=(v-d.center)/d.unit;
    const double error=eps*(1+std::abs(v/d.unit)+std::abs(d.center/d.unit)+std::abs(value));
    I y(value-error,value+error);if(!y.valid())return false;d.x.push_back(y);d.y.push_back((v-d.center)/d.unit);sum.add(y);}
  const double n=x.size();d.mean=sum.get()/n;Sum variance;
  for(I v:d.x)variance.add(sq(v-d.mean));d.var=variance.get()/n;
  if(!d.var.valid()||d.var.lo<=0)return false;
  d.jac=-n*std::log(d.unit);d.normal=I(-.5*n)*(log(I(2*pi)*d.var)+1)+d.jac;
  return d.normal.valid();
}
// All arrays are fixed-size values, including guarded matrix inversion.
struct System {std::array<I,3> g;std::array<std::array<I,3>,3> h;};
inline System enclosure(const Data& d,const std::array<I,3>& p,double v0){
  I v=p[2]*v0,a2=exp(p[1]*2);Sum rm,rr,mm,mt,tt,ld;
  for(I x:d.x){I r=x-p[0],r2=sq(r),D=r2+a2,D2=sq(D);rm.add(r/D);rr.add(r2/D);mm.add((a2-r2)/D2);mt.add(r*a2/D2);tt.add(a2*r2/D2);ld.add(log1p(r2/a2));}
  System s;const double n=d.y.size();
  s.g={{-(v+1)*rm.get(),I(n)-(v+1)*rr.get(),I(v0)*(-I(n)*derivative(v,1)+ld.get()*.5)}};
  s.h[0]={{(v+1)*mm.get(),I(2)*(v+1)*mt.get(),I(-v0)*rm.get()}};
  s.h[1]={{s.h[0][1],I(2)*(v+1)*tt.get(),I(-v0)*rr.get()}};
  s.h[2]={{s.h[0][2],s.h[1][2],I(n*v0*v0)*derivative(v,2)}};
  return s;
}
inline bool inverse(const System& s,int dim,std::array<std::array<double,3>,3>& C){
  std::array<std::array<double,6>,3> a{};double norm=0;
  for(int i=0;i<dim;++i){for(int j=0;j<dim;++j){if(!s.h[i][j].valid())return false;a[i][j]=s.h[i][j].mid();norm=std::max(norm,std::abs(a[i][j]));}a[i][i+dim]=1;}
  for(int j=0;j<dim;++j){int k=j;for(int i=j+1;i<dim;++i)if(std::abs(a[i][j])>std::abs(a[k][j]))k=i;
    if(!(std::abs(a[k][j])>eps*norm))return false;std::swap(a[k],a[j]);double z=a[j][j];for(int q=0;q<2*dim;++q)a[j][q]/=z;
    for(int i=0;i<dim;++i)if(i!=j){z=a[i][j];for(int q=0;q<2*dim;++q)a[i][q]-=z*a[j][q];}}
  double cnorm=0;for(int i=0;i<dim;++i)for(int j=0;j<dim;++j){C[i][j]=a[i][j+dim];cnorm=std::max(cnorm,std::abs(C[i][j]));}
  if(!std::isfinite(cnorm)||cnorm*norm>1/eps)return false;
  // A strict ||I-C H|| <1 also certifies invertibility of C.
  double defect=0;for(int i=0;i<dim;++i){double row=0;for(int j=0;j<dim;++j){I z;for(int k=0;k<dim;++k)z=z+I(C[i][k])*I(s.h[k][j].mid());row+=(I(i==j?1:0)-z).mag();}defect=std::max(defect,row);}return defect<.1;
}
inline double nuisance_lower(const System& s){
  I a=s.h[0][0],b=s.h[0][1],c=s.h[1][1];
  double mid=.5*(a.mid()+c.mid()-std::hypot(a.mid()-c.mid(),2*b.mid()));
  return mid-std::sqrt(std::pow((a.hi-a.lo)/2,2)+2*std::pow((b.hi-b.lo)/2,2)+std::pow((c.hi-c.lo)/2,2))-eps*(1+a.mag()+c.mag()+2*b.mag());
}
inline bool positive(const System& s,const System& point){
  // Inverse Cholesky congruence, then Frobenius norm bound.
  std::array<std::array<double,3>,3> L{},C{};
  for(int i=0;i<3;++i)for(int j=0;j<=i;++j){double z=point.h[i][j].mid();for(int k=0;k<j;++k)z-=L[i][k]*L[j][k];if(i==j){if(!(z>eps))return false;L[i][j]=std::sqrt(z);}else L[i][j]=z/L[j][j];}
  for(int j=0;j<3;++j)for(int i=0;i<3;++i){double z=i==j?1:0;for(int k=0;k<i;++k)z-=L[i][k]*C[k][j];C[i][j]=z/L[i][i];}
  double sum=0;for(int i=0;i<3;++i)for(int j=0;j<3;++j){I z;for(int k=0;k<3;++k)for(int l=0;l<3;++l)z=z+I(C[i][k])*s.h[k][l]*C[j][l];double err=(z-I(i==j?1:0)).mag();sum+=err*err;}return std::isfinite(sum)&&sum<1-eps;
}
struct Profile {double v=0,m=0,t=0;I delta;bool collapse=false;};
struct Engine {
  Data d;double floor;int budget,maxit;int failure=0; double containedL=0,containedU=0; // 1 budget, 2 numerical, 3 tie
  std::map<double,Profile> cache;
  explicit Engine(Data input,double q,int b,int it):d(input),floor(q),budget(b),maxit(it){}
  bool containment(double v,double& L,double& U){
    if(v>=floor&&containedL>0){L=containedL;U=containedU;return true;}
    I best(0);const double n=d.y.size();
    std::vector<std::size_t> ks{d.mass,d.y.size()-1};for(int j=2;j<=16;++j)ks.push_back(std::max(d.mass,(d.y.size()-1)/j));
    for(std::size_t k:ks){double gap=INFINITY;for(std::size_t j=0;j+k<d.x.size();++j)gap=std::min(gap,(d.x[j+k]-d.x[j]).lo/2);I z=I(gap*gap)*((I(n-k)-I(static_cast<double>(k))/v)/n);if(gap>0&&z.valid()&&z.lo>best.lo)best=z;}
    L=std::sqrt(best.lo);I upper=(d.x.back()-d.x.front())*root(I(1)+I(1)/v);U=upper.hi;if(v==floor&&L>0){containedL=L;containedU=U;}return L>0&&std::isfinite(U);
  }
  I value(double v,double m,double t){
    I scale=exp(I(t)),zmean=I(m)-d.mean;
    I u=I(t)-log(d.var)*.5;
    I delta=-I(d.y.size())*(nl(u)+sq(zmean)*exp(I(-2*t))*.5)+I(d.y.size())*gamma(v,0);
    Sum correction;for(I x:d.x){I z=(x-m)/scale,u2=sq(z)/v;correction.add(I(v)*lr(u2)*.5-log1p(u2)*.5);}
    return delta+correction.get();
  }
  bool nuisance(double v,double& m,double& t){
    // EM is a bounded proposal stage. Newton refinement has guarded solves
    // and backtracking; only the subsequent score/distance bound is evidence.
    m=0;t=0;const double n=d.y.size();
    for(int it=0;it<maxit;++it){double s=std::exp(t),sw=0,sx=0;
      for(double x:d.y){double z=(x-m)/s,w=(1+1/v)/(1+z*z/v);sw+=w;sx+=w*x;}double next=sx/sw,ss=0;
      for(double x:d.y){double z=(x-m)/s,w=(1+1/v)/(1+z*z/v);ss+=w*(x-next)*(x-next)/n;}
      if(!(ss>0)||!std::isfinite(next))return false;
      double nt=std::log(ss)/2;if(std::max(std::abs(nt-t),std::abs(next-m))<1e-11){m=next;t=nt;break;}m=next;t=nt;
    }
    for(int it=0;it<std::min(40,maxit);++it){std::array<I,3> p{{m,t+.5*std::log(v),1}};auto s=enclosure(d,p,v);std::array<std::array<double,3>,3>C{};if(!inverse(s,2,C))return false;
      std::array<double,2> step{};for(int i=0;i<2;++i)for(int j=0;j<2;++j)step[i]+=C[i][j]*s.g[j].mid();
      if(std::max(std::abs(step[0]),std::abs(step[1]))<1e-13)break;
      double f=value(v,m,t).mid(),a=1;bool ok=false;
      for(int k=0;k<24;++k,a*=.5){double mm=m-a*step[0],tt=t-a*step[1];I trial=value(v,mm,tt);if(trial.valid()&&trial.mid()>=f-1e-12){m=mm;t=tt;ok=true;break;}}
      if(!ok)break;
    }return std::isfinite(m)&&std::isfinite(t);
  }
  Profile at(double v){
    auto found=cache.find(v);if(found!=cache.end())return found->second;
    Profile p;p.v=v;p.delta=bad();if(failure)return p;
    if(static_cast<int>(cache.size())>=budget){failure=1;return p;}
    if(!(v>=floor)||v>1e14||!std::isfinite(v)){failure=2;return p;}
    const double n=d.y.size();
    if(v==floor&&d.mass==floor*(n-d.mass)){
      p.collapse=true;double lower=-INFINITY,upper=-INFINITY;
      for(std::size_t j=0;j<d.original.size();){std::size_t k=j+1;while(k<d.original.size()&&d.original[k]==d.original[j])++k;
        if(k-j==d.mass){Sum sum;for(std::size_t i=0;i<d.x.size();++i)if(d.original[i]!=d.original[j])sum.add(log(I(std::abs(d.original[i]-d.original[j]))/d.unit));
          I endpoint=I(n)*(gamma(v,0)-.5*std::log(2*pi))-(v+1)*sum.get()+I((v+1)*(n-d.mass)*.5*std::log(v));
          I delta=endpoint-(d.normal-d.jac);lower=std::max(lower,delta.lo);upper=std::max(upper,delta.hi);p.m=d.y[j];p.t=-INFINITY;}
        j=k;}p.delta={lower,upper};
    }else{
      double L,U;if(!containment(v,L,U)||!nuisance(v,p.m,p.t)){failure=2;return p;}
      I val=value(v,p.m,p.t);I a=root(I(v))*exp(I(p.t));double D=0;
      for(I mm:{d.x.front(),d.x.back()})for(double ss:{L,U}){I aa=root(I(v))*ss;I q=I(1)+(sq(I(p.m)-mm)+sq(a-aa))/(I(2)*a*aa);if(!q.valid()){failure=2;return p;}D=std::max(D,std::acosh(std::max(1.,q.hi)));}
      // Stable nuisance scores in ordinary-scale coordinates (no n*nu
      // cancellation and no absolute-likelihood subtraction).
      Sum g0,g1;I scale=exp(I(p.t));
      for(I x:d.x){I z=(x-p.m)/scale,z2=sq(z),den=I(1)+z2/v;g0.add(-I(1+1/v)*z/(scale*den));g1.add(I(1)-I(1+1/v)*z2/den);}
      I gm=g0.get(),gt=g1.get();I error=root(sq(a*gm)+sq(gt))*(D+eps);
      p.delta={val.lo,val.hi+error.hi};
    }
    if(!p.delta.valid()){failure=2;return p;}cache.emplace(v,p);return p;
  }
};

// Stable ordinary-scale shape score. R(u)=log1p(u)-u/(1+u) is
// O(u^2); c'(v)-1/(2v) is evaluated directly by gamma order 3.
inline I shape_remainder(I u){
  if(!u.valid()||u.hi<0)return bad();
  auto at=[](double q){
    q=std::max(0.,q);double value=0,error=0;
    if(q<=.01){double term=q*q;value=term/2;for(int k=3;k<=14;++k){term*=-q;value+=term*(k-1)/k;}error=eps*std::abs(value)+std::numeric_limits<double>::denorm_min();}
    else{double a=std::log1p(q),b=q/(1+q);value=a-b;error=eps*(std::abs(a)+std::abs(b));}
    return I(value-error,value+error);
  };
  return {at(u.lo).lo,at(u.hi).hi};
}
inline I shape_score(const Data& d,const std::array<double,3>& p,double v0){
  I v=I(p[2])*v0,a2=exp(I(p[1])*2);Sum sum;
  for(I x:d.x){I u=sq(x-p[0])/a2;sum.add(shape_remainder(u)-u/((I(1)+u)*v));}
  return -I(d.y.size())*derivative(v,3)+sum.get()*.5;
}
struct Isolation {
  std::array<double,3> p{};std::array<I,3> box{};
  double v0=0,left=0,right=0;int stage=0;
};
inline bool root_box(const Data& d,Isolation& r,int dim){
  std::array<I,3> point{{r.p[0],r.p[1],r.p[2]}};auto s=enclosure(d,point,r.v0);
  std::array<std::array<double,3>,3> C{};if(!inverse(s,dim,C))return false;
  std::array<double,3> e{};
  I ordinary_shape;if(dim==3)ordinary_shape=shape_score(d,r.p,r.v0);
  for(int i=0;i<dim;++i){I q;
    if(dim==3)q=I(C[i][0])*s.g[0]+(I(C[i][1])-I(C[i][2])/(I(r.p[2])*2))*s.g[1]+I(C[i][2])*I(r.v0)*ordinary_shape;
    else for(int j=0;j<dim;++j)q=q+I(C[i][j])*s.g[j];
    e[i]=q.mag();
  }
  for(double width:{1e-10,3e-10,1e-9,3e-9,1e-8,3e-8,1e-7,2e-7,3e-7,5e-7,1e-6}){
    auto box=point;std::array<double,3> radii{};for(int i=0;i<dim;++i){radii[i]=width*std::max(1.,std::abs(r.p[i]));box[i]={r.p[i]-radii[i],r.p[i]+radii[i]};}
    auto h=enclosure(d,box,r.v0);bool ok=true;
    for(int i=0;i<dim;++i){double sum=0,defect=0;for(int j=0;j<dim;++j){I q;for(int k=0;k<dim;++k)q=q+I(C[i][k])*h.h[k][j];double Q=(I(i==j?1:0)-q).mag();sum+=Q*radii[j];defect+=Q;}if(!(defect<1&&e[i]+sum<radii[i]))ok=false;}
    if(ok){r.box=box;return true;}
  }return false;
}
inline bool isolate(Engine& e,const Profile& proposal,Isolation& r){
  r.stage=1;r.v0=proposal.v;r.p={{proposal.m,proposal.t+.5*std::log(proposal.v),1}};
  // Newton nominations do not certify roots. Finite bounded trials only.
  for(int it=0;it<std::min(60,e.maxit);++it){std::array<I,3> p{{r.p[0],r.p[1],r.p[2]}};auto h=enclosure(e.d,p,r.v0);std::array<std::array<double,3>,3>C{};if(!inverse(h,3,C))return false;
    std::array<double,3> step{};double size=0;for(int i=0;i<3;++i){for(int j=0;j<3;++j)step[i]+=C[i][j]*h.g[j].mid();size=std::max(size,std::abs(step[i]));}
    if(size<1e-12)break;
    double a=1;bool ok=false;I old=e.value(r.v0*r.p[2],r.p[0],r.p[1]-.5*std::log(r.v0*r.p[2]));
    for(int j=0;j<30;++j,a*=.5){auto next=r.p;for(int i=0;i<3;++i)next[i]-=a*step[i];double v=r.v0*next[2];if(!(v>e.floor&&v<1e14))continue;I val=e.value(v,next[0],next[1]-.5*std::log(v));if(val.valid()&&val.mid()>=old.mid()-1e-10){r.p=next;ok=true;break;}}
    if(!ok)return false;
  }
  r.stage=2;
  if(!root_box(e.d,r,3)||r.box[2].lo*r.v0<=e.floor)return false;
  r.stage=3;
  const double massfloor=static_cast<double>(e.d.mass)/(e.d.y.size()-e.d.mass);
  auto H0=enclosure(e.d,{{r.p[0],r.p[1],r.p[2]}},r.v0);
  for(double h:{.01,.003,.001,.0003,.0001,.00003,.00001,.000003,.000001,.0000003,.0000001,.00000003,.00000001})for(double radius:{h,3*h,10*h,30*h}){
    std::array<I,3> box{{{r.p[0]-radius,r.p[0]+radius},{r.p[1]-radius,r.p[1]+radius},{r.p[2]-h,r.p[2]+h}}};
    if(box[2].lo*r.v0<=std::max(e.floor,massfloor))continue;
    bool contains=true;for(int i=0;i<3;++i)if(!(box[i].lo<r.box[i].lo&&r.box[i].hi<box[i].hi))contains=false;if(!contains)continue;
    auto H=enclosure(e.d,box,r.v0);if(!positive(H,H0))continue;double lam=nuisance_lower(H);if(!(lam>0))continue;
    auto centre=enclosure(e.d,{{r.p[0],r.p[1],box[2]}},r.v0);double G=std::hypot(centre.g[0].mag(),centre.g[1].mag());
    if(G<lam*radius){r.left=std::nextafter(box[2].lo*r.v0,INFINITY);r.right=std::nextafter(box[2].hi*r.v0,-INFINITY);r.stage=4;return true;}
  }return false;
}
inline bool endpoint(Engine& e,const Profile& p,Isolation& r){
  if(!p.collapse){r.v0=e.floor;r.p={{p.m,p.t+.5*std::log(e.floor),1}};if(!root_box(e.d,r,2))return false;}
  for(double rel:{.1,.03,.01,.003,.001,.0003,.0001,.00003,.00001}){
    double h=e.floor*rel;auto right=e.at(e.floor+h);if(e.failure)return false;
    I slope=(right.delta-p.delta)/h+I(e.d.y.size())*h/(2*e.floor*e.floor);
    if(slope.valid()&&slope.hi<0){r.left=e.floor;r.right=std::nextafter(e.floor+h,e.floor);return true;}
  }return false;
}
struct Tail {double N=0,upper=0;};
inline bool tail(Engine& e,double witness,Tail& tail){
  const double n=e.d.y.size();Sum fourth,gm,gt;
  // Recenter and rescale with intervals; the tail theorem uses the exact
  // fitted normal coordinates, even though affine constants are binary64.
  std::vector<I> x;for(I y:e.d.x){I z=(y-e.d.mean)/root(e.d.var);x.push_back(z);I z2=sq(z);fourth.add(sq(z2));gm.add(I((z*(I(1)-z2)).mag()));gt.add(I((z2*(I(1)-z2)).mag()));}
  I A=(fourth.get()-3*n)/4;double G=std::hypot(gm.get().hi,gt.get().hi);
  if(A.valid()&&A.hi<0){
    for(double r:{.01,.03,.1})for(int j=0;j<80;++j){double h=std::ldexp(1.,-j-1);if(1/h<=static_cast<double>(e.d.mass)/(n-e.d.mass))continue;Sum mm,tt,cross;bool ok=true;
      for(I y:x){I z=(y-I(-r,r))*exp(I(-r,r)),z2=sq(z);double hi=std::max(0.,z2.hi),lo=std::max(0.,z2.lo);if(h*hi>=1){ok=false;break;}I den=I(1,std::pow(1+h*hi,2));mm.add(I(1-h*hi)/den);tt.add(I(lo)/den);cross.add(z/den);}
      if(!ok)continue;
      double off=(I(2*(1+h))*exp(I(r))*cross.get()).mag();
      I lam=I(std::min((exp(I(-2*r))*mm.get()).lo,2*tt.get().lo))-off;
      if(!lam.valid()||lam.lo<=0)continue;
      I inward=lam*r-I(h*G);I K=fourth.get()/4+I(n*h/24)+I(G*G)/(I(2)*lam);
      I slope=-A/2-I(h)*K;
      if(inward.valid()&&inward.lo>0&&slope.valid()&&slope.lo>0){tail={1/h,0};return true;}
    }
  }
  if(witness>0){double L,U;if(!e.containment(std::max(2.,e.floor+1),L,U))return false;I C=I(3*n/8)*sq(sq((e.d.x.back()-e.d.x.front())/L));if(!C.valid())return false;double N=std::max(1000.,4*C.hi/witness);if(!std::isfinite(N)||N>1e14)return false;tail={N,C.hi/N};return true;}
  e.failure=3;return false;
}
} // namespace t_cert_detail

enum class TCertificateStatus {Finite, Cauchy, Normal, TTwo, Collapse, Unbounded, Budget, Tie, NumericalFailure, InvalidData};
struct TCertificateResult {
  double mu=NAN,scale=NAN,nu=NAN,logLik=NAN;
  std::array<double,2> unresolved_shape{{NAN,NAN}};
  int profiles=0;TCertificateStatus status=TCertificateStatus::NumericalFailure;
  StopDetail stop_detail{};
  // Root enclosure in public ordinary-scale coordinates; no fitted sd at t2.
  std::array<double,3> coefficient_lower{{NAN,NAN,NAN}},coefficient_upper{{NAN,NAN,NAN}};
};
inline TCertificateResult t_certificate(span x,double floor,int budget=4096,int maxit=200,double coefficient_tol=1e-6){
  TCertificateResult out;
  if(!(floor==1||floor==2)||budget<0||maxit<1||!(coefficient_tol>0)||!std::isfinite(coefficient_tol)){out.status=TCertificateStatus::InvalidData;return out;}
  if(budget==0){out.status=TCertificateStatus::Budget;return out;}
  t_cert_detail::Data data;if(!t_cert_detail::data(x,data)){
    bool constant=!x.empty();for(double v:x)constant=constant&&std::isfinite(v)&&v==x[0];
    bool finite=x.size()>=2;for(double v:x)finite=finite&&std::isfinite(v);
    out.status=constant?TCertificateStatus::Unbounded:finite?TCertificateStatus::NumericalFailure:TCertificateStatus::InvalidData;
    if(constant){out.stop_detail.loglik_lower=INFINITY;out.stop_detail.loglik_upper=INFINITY;}return out;}
  // Exact replication leaves every maximum and face unchanged. Reduce the
  // common integer multiplicity before sums, then transport all likelihoods.
  std::size_t divisor=0;
  for(std::size_t j=0;j<data.original.size();){std::size_t k=j+1;while(k<data.original.size()&&data.original[k]==data.original[j])++k;divisor=std::gcd(divisor,k-j);j=k;}
  if(divisor>1){
    std::vector<double> reduced;
    for(std::size_t j=0;j<data.original.size();){std::size_t k=j+1;while(k<data.original.size()&&data.original[k]==data.original[j])++k;reduced.insert(reduced.end(),(k-j)/divisor,data.original[j]);j=k;}
    out=t_certificate({reduced.data(),reduced.size()},floor,budget,maxit,coefficient_tol);
    const double multiple=divisor;out.logLik*=multiple;
    out.stop_detail.limit_loglik*=multiple;out.stop_detail.loglik_lower*=multiple;out.stop_detail.loglik_upper*=multiple;out.stop_detail.segment_loglik_upper*=multiple;
    for(auto& candidate:out.stop_detail.candidates){candidate.loglik_lower*=multiple;candidate.loglik_upper*=multiple;}
    return out;
  }
  t_cert_detail::Engine e(data,floor,budget,maxit);const double n=x.size();
  if(data.mass>floor*(n-data.mass)){out.status=TCertificateStatus::Unbounded;out.stop_detail.loglik_lower=INFINITY;out.stop_detail.loglik_upper=INFINITY;return out;}
  // The two exact sample theorems use exact binary64 equality only. No
  // tolerance can turn an approximate tie into a symbolic face decision.
  std::vector<double> support=data.original;support.erase(std::unique(support.begin(),support.end()),support.end());
  bool balanced=support.size()==2&&2*data.mass==x.size();
  bool critical=false;
  if(floor==2&&support.size()==3&&3*data.mass==2*x.size()){
    // Sterbenz differences are exact when endpoints share a binade. Also
    // require exactly representable subtraction, checked through intervals
    // is insufficient for a theorem; use an error-free TwoSum residual.
    const double a=support[1]-support[0],b=support[2]-support[1];
    auto exact_difference=[](double u,double v){double s=u-v,z=s-u;double err=(u-(s-z))-(v+z);return err==0&&std::isfinite(s);};
    critical=a==b&&a>0&&exact_difference(support[1],support[0])&&exact_difference(support[2],support[1])&&6*std::count(data.original.begin(),data.original.end(),support[0])==static_cast<int>(x.size())&&6*std::count(data.original.begin(),data.original.end(),support[2])==static_cast<int>(x.size());
  }
  auto normal_stop=[&](){out.status=TCertificateStatus::Normal;out.mu=data.center+data.unit*data.mean.mid();out.scale=data.unit*std::sqrt(data.var.mid());out.nu=INFINITY;out.logLik=data.normal.mid();out.stop_detail.limit_coef={out.mu,out.scale};out.stop_detail.limit_loglik=out.logLik;out.stop_detail.loglik_lower=data.normal.lo;out.stop_detail.loglik_upper=data.normal.hi;};
  if(balanced){normal_stop();return out;}
  if(critical){out.status=TCertificateStatus::Collapse;out.mu=support[1];out.scale=0;out.nu=2;auto ll=t_cert_detail::I(-n)*(t_cert_detail::log(t_cert_detail::I(support[2]-support[1]))+std::log(2.));out.logLik=ll.mid();out.stop_detail.limit_coef={out.mu,2};out.stop_detail.limit_loglik=out.logLik;out.stop_detail.loglik_lower=ll.lo;out.stop_detail.loglik_upper=ll.hi;return out;}
  t_cert_detail::Profile best;best.delta={-INFINITY,-INFINITY};
  auto finish_failure=[&](){
    out.stop_detail.candidates.clear();
    out.stop_detail.candidates.push_back({{data.center+data.unit*data.mean.mid(),data.unit*std::sqrt(data.var.mid())},data.normal.lo,data.normal.hi,StopModel::Norm});
    if(best.delta.valid()&&best.collapse)out.stop_detail.candidates.push_back({{data.atom,floor},best.delta.lo+data.normal.lo,best.delta.hi+data.normal.hi,StopModel::TCollapsePath});
    if(best.delta.valid()&&!best.collapse)out.stop_detail.candidates.push_back({{data.center+data.unit*best.m,data.unit*std::exp(best.t),best.v},best.delta.lo+data.normal.lo,best.delta.hi+data.normal.hi,StopModel::Lst});
    out.profiles=e.cache.size();out.status=e.failure==1?TCertificateStatus::Budget:e.failure==3?TCertificateStatus::Tie:TCertificateStatus::NumericalFailure;out.stop_detail.loglik_lower=std::max(data.normal.lo,best.delta.lo+data.normal.lo);out.stop_detail.loglik_upper=INFINITY;return out;};
  for(int j=0;j<=32;++j){double v=floor*std::exp(std::log(1000/floor)*j/32);if(j==0)v=floor;auto p=e.at(v);if(e.failure)return finish_failure();if(p.delta.lo>best.delta.lo)best=p;}
  t_cert_detail::Isolation iso;bool finite=false,boundary=false;
  if(best.delta.lo>0){
    if(best.v==floor)boundary=t_cert_detail::endpoint(e,best,iso);
    if(!boundary&&!e.failure)finite=t_cert_detail::isolate(e,best,iso);
    if(e.failure)return finish_failure();
    if(!(finite||boundary)){e.failure=2;return finish_failure();}
    if(finite){best.v=iso.v0*iso.p[2];best.m=iso.p[0];best.t=iso.p[1]-.5*std::log(best.v);best.delta=e.value(best.v,best.m,best.t);}
  }
  const double witness=std::max(0.,best.delta.lo);t_cert_detail::Tail tail;
  if(!t_cert_detail::tail(e,witness,tail))return finish_failure();
  if(tail.N>1e14){e.failure=2;return finish_failure();}
  e.at(tail.N);if(finite||boundary){e.at(iso.left);e.at(iso.right);}if(e.failure)return finish_failure();
  // Retain witnesses at every guarded exit. Profile bounds are not global
  // upper bounds until every interval and the tail have been closed.
  out.stop_detail.candidates.push_back({{data.center+data.unit*data.mean.mid(),data.unit*std::sqrt(data.var.mid())},data.normal.lo,data.normal.hi,StopModel::Norm});
  struct Segment{double upper,lo,hi;bool operator<(const Segment& b)const{return upper<b.upper;}};
  std::priority_queue<Segment> queue;
  auto push=[&](double lo,double hi){
    if((finite||boundary)&&lo>=iso.left&&hi<=iso.right)return;
    auto a=e.at(lo),b=e.at(hi);if(e.failure)return;
    auto error=t_cert_detail::I(n)*t_cert_detail::sq(t_cert_detail::I(hi)-lo)/(t_cert_detail::I(8)*t_cert_detail::sq(t_cert_detail::I(lo)));
    double upper=std::max(a.delta.hi,b.delta.hi)+error.hi;
    if(!std::isfinite(upper)){e.failure=2;return;}if(upper>=witness)queue.push({upper,lo,hi});
  };
  std::vector<double> knots;for(const auto& p:e.cache)if(p.first<=tail.N)knots.push_back(p.first);
  for(std::size_t j=1;j<knots.size();++j)push(knots[j-1],knots[j]);
  while(!queue.empty()&&!e.failure){auto s=queue.top();queue.pop();double mid=std::sqrt(s.lo)*std::sqrt(s.hi);
    if(!(mid>s.lo&&mid<s.hi)){e.failure=3;break;}e.at(mid);push(s.lo,mid);push(mid,s.hi);
    out.stop_detail.segment_loglik_upper=s.upper+data.normal.hi;out.unresolved_shape={{s.lo,s.hi}};
  }
  if(e.failure)return finish_failure();
  if(tail.upper>witness){e.failure=3;return finish_failure();}
  out.profiles=e.cache.size();
  if(!(finite||boundary)){normal_stop();return out;}
  if(best.collapse){out.status=TCertificateStatus::Collapse;out.mu=data.atom;out.scale=0;out.nu=floor;out.logLik=(best.delta+data.normal).mid();out.stop_detail.limit_coef={out.mu,floor};out.stop_detail.limit_loglik=out.logLik;out.stop_detail.loglik_lower=(best.delta+data.normal).lo;out.stop_detail.loglik_upper=(best.delta+data.normal).hi;return out;}
  // Transform the entire root box, and require coefficient widths before
  // returning any finite result. std's sd width is checked by its caller.
  t_cert_detail::I v=iso.box[2]*iso.v0,m=t_cert_detail::location_units(iso.box[0],data.unit,data.center),s=t_cert_detail::scale_units(t_cert_detail::exp(iso.box[1])/t_cert_detail::root(v),data.unit);
  if(!s.valid()||s.lo<=0){e.failure=2;return finish_failure();}
  std::array<t_cert_detail::I,3> coef{{m,s,v}};
  for(int j=0;j<3;++j){const double width_unit=j==0?data.unit:std::abs(coef[j].mid());if(!coef[j].valid()||coef[j].hi-coef[j].lo>coefficient_tol*width_unit){e.failure=2;return finish_failure();}out.coefficient_lower[j]=coef[j].lo;out.coefficient_upper[j]=coef[j].hi;}
  out.mu=m.mid();out.scale=s.mid();out.nu=v.mid();out.logLik=(best.delta+data.normal).mid();
  out.status=finite?TCertificateStatus::Finite:floor==1?TCertificateStatus::Cauchy:TCertificateStatus::TTwo;
  out.stop_detail.candidates.push_back({{out.mu,out.scale,out.nu},best.delta.lo+data.normal.lo,best.delta.hi+data.normal.hi,StopModel::Lst});
  out.stop_detail.loglik_lower=best.delta.lo+data.normal.lo;out.stop_detail.loglik_upper=INFINITY;
  if(boundary){out.nu=floor;out.stop_detail.limit_coef={out.mu,out.scale,floor};out.stop_detail.limit_loglik=out.logLik;out.stop_detail.loglik_lower=(best.delta+data.normal).lo;out.stop_detail.loglik_upper=(best.delta+data.normal).hi;}
  return out;
}
} // namespace cppunivariate
