// TASK-138 practical enclosures. See the Burr dossier for the arithmetic contract.
#pragma once
#include "../../core/work_budget.hpp"
#include "../../core/stop_detail.hpp"
#include "../../core/span.hpp"
#include <array>
#include <algorithm>
#include <cmath>
#include <limits>
#include <map>
#include <queue>
#include <vector>
namespace cppunivariate {
namespace burr_cert_detail {
constexpr double pad = 64 * std::numeric_limits<double>::epsilon();
struct Interval {
  double lo, hi;
  Interval(double a=0):lo(a),hi(a){}
  Interval(double a,double b):lo(a),hi(b){}
  bool valid() const {return std::isfinite(lo)&&std::isfinite(hi)&&lo<=hi;}
  double mag() const {return std::max(std::abs(lo),std::abs(hi));}
};
inline Interval invalid(){return {NAN,NAN};}
inline Interval padded(double a,double b){
  if(!std::isfinite(a)||!std::isfinite(b)||a>b)return invalid();
  const double e=pad*(1+std::max(std::abs(a),std::abs(b)));
  return {a-e,b+e};
}
inline Interval operator+(Interval a,Interval b){return padded(a.lo+b.lo,a.hi+b.hi);}
inline Interval operator-(Interval a){return {-a.hi,-a.lo};}
inline Interval operator-(Interval a,Interval b){return a+-b;}
inline Interval operator*(Interval a,Interval b){
  const std::array<double,4> v{{a.lo*b.lo,a.lo*b.hi,a.hi*b.lo,a.hi*b.hi}};
  for(double x:v)if(!std::isfinite(x))return invalid();
  return padded(*std::min_element(v.begin(),v.end()),*std::max_element(v.begin(),v.end()));
}
inline Interval operator/(Interval a,Interval b){
  if(!b.valid()||!(b.lo>0||b.hi<0))return invalid();
  return a*padded(1/b.hi,1/b.lo);
}
inline Interval exp(Interval a){return a.valid()?padded(std::exp(a.lo),std::exp(a.hi)):invalid();}
inline Interval log(Interval a){return a.valid()&&a.lo>0?padded(std::log(a.lo),std::log(a.hi)):invalid();}
inline double softplus(double z){return std::max(z,0.)+std::log1p(std::exp(-std::abs(z)));}
inline double sigmoid(double z){return z>=0?1/(1+std::exp(-z)):std::exp(z)/(1+std::exp(z));}
inline Interval sp(Interval a){return a.valid()?padded(softplus(a.lo),softplus(a.hi)):invalid();}
inline Interval sig(Interval a){return a.valid()?padded(sigmoid(a.lo),sigmoid(a.hi)):invalid();}
// The n-term allowance depends on input magnitudes, including cancellation.
struct Sum {
  double lo=0,hi=0,mag=0;std::size_t n=0;
  void add(Interval a){lo+=a.lo;hi+=a.hi;mag+=a.mag();++n;}
  Interval get()const{const double e=pad*(n+1)*(1+mag);return {lo-e,hi+e};}
};
inline double entropy(double p){return p<=0||p>=1?0:-p*std::log(p)-(1-p)*std::log1p(-p);}
inline Interval entropy(Interval a){
  if(!a.valid()||a.lo<0||a.hi>1)return invalid();
  return padded(std::min(entropy(a.lo),entropy(a.hi)),a.lo<=.5&&a.hi>=.5?std::log(2.):std::max(entropy(a.lo),entropy(a.hi)));
}
struct Data {int maxit=120;std::vector<double> y;std::vector<Interval> box;double center=0,unit=1,jacobian=0;};
inline bool data(span x,Data& d){
  if(x.size()<2)return false;
  double total=0;
  for(double v:x){if(!(v>0)||!std::isfinite(v))return false;d.y.push_back(std::log(v));total+=d.y.back();}
  std::sort(d.y.begin(),d.y.end());
  d.center=total/x.size();double variance=0;
  for(double y:d.y)variance+=(y-d.center)*(y-d.center)/x.size();
  d.unit=std::sqrt(variance);if(!(d.unit>0)||!std::isfinite(d.unit))return false;
  d.jacobian=-total-x.size()*std::log(d.unit);
  for(double& y:d.y){d.box.push_back((padded(y,y)-d.center)/d.unit);y=(y-d.center)/d.unit;if(!d.box.back().valid())return false;}
  return std::isfinite(d.jacobian);
}
// Fixed-k strictly concave nuisance problem. Bracket the intercept, then
// the decreasing profile power score. Newton proposals stay inside brackets.
struct Proposal {double k=0,c=0,b=0,value=-INFINITY,score=0;};
inline bool intercept(const std::vector<double>& y,double k,double c,double& b,double& score,double& deriv,int maxit=120){
  maxit = static_cast<int>(solver_budget(maxit));
  double lo=-std::log(k)-c*y.back()-1,hi=-std::log(k)-c*y.front()+1;
  if(!std::isfinite(lo)||!std::isfinite(hi)||!(lo<hi))return false;
  const double n=y.size();b=(lo+hi)/2;
  auto evaluate=[&](double v,double& g,double& h){g=0;h=0;for(double t:y){const double z=c*t+v,p=sigmoid(z);g+=k<1?(k+1)*sigmoid(-z)-k:1-(k+1)*p;h-=(k+1)*p*(1-p);}};
  double gl,hl,gh,hh;evaluate(lo,gl,hl);evaluate(hi,gh,hh);
  if(!(gl>0&&gh<0))return false;
  bool done=false;
  for(int it=0;it<maxit;++it){
    double g,h;evaluate(b,g,h);if(!std::isfinite(g)||!std::isfinite(h))return false;
    if(std::abs(g)<2e-14*n||hi-lo<2e-14*(1+std::abs(b))){done=true;break;}
    if(g>0)lo=b;else hi=b;
    const double trial=b-g/h;b=std::isfinite(trial)&&trial>lo&&trial<hi?trial:(lo+hi)/2;
  }
  if(!done)return false;
  double sw=0,syw=0,syyw=0;score=n/c;
  for(double t:y){double z=c*t+b,p=sigmoid(z),w=(k+1)*p*(1-p);score+=t*(k<1?(k+1)*sigmoid(-z)-k:1-(k+1)*p);sw+=w;syw+=t*w;syyw+=t*t*w;}
  if(!(sw>0))return false;
  deriv=-n/(c*c)-syyw+syw*syw/sw;
  return std::isfinite(score)&&std::isfinite(deriv)&&deriv<0;
}
inline bool profile(const std::vector<double>& y,double k,Proposal& p,int maxit=120){
  maxit = static_cast<int>(solver_budget(maxit));
  if(!(k>0)||!std::isfinite(k))return false;
  double lo=1e-8,hi=1,b=0,g=0,h=0;
  if(!intercept(y,k,lo,b,g,h,maxit)||g<=0)return false;
  bool bracket=false;
  for(int i=0;i<maxit;++i){if(!intercept(y,k,hi,b,g,h,maxit))return false;if(g<0){bracket=true;break;}hi*=2;}
  if(!bracket)return false;
  double c=(lo+hi)/2;bool done=false;
  for(int i=0;i<maxit;++i){
    if(!intercept(y,k,c,b,g,h,maxit))return false;
    if(std::abs(g)<1e-12*y.size()||hi-lo<2e-13*(1+c)){done=true;break;}
    if(g>0)lo=c;else hi=c;
    const double trial=c-g/h;c=std::isfinite(trial)&&trial>lo&&trial<hi?trial:(lo+hi)/2;
  }
  if(!done)return false;
  p={k,c,b,static_cast<double>(y.size())*(std::log(k)+std::log(c)),static_cast<double>(y.size())/k};
  for(double t:y){double z=c*t+b;p.value+=k<1?-k*z-(k+1)*softplus(-z):z-(k+1)*softplus(z);p.score-=softplus(z);}
  return std::isfinite(p.value)&&std::isfinite(p.score);
}
inline Interval primal(const std::vector<Interval>& y,const Proposal& t){
  Sum z,s;for(Interval a:y){const Interval v=t.c*a+t.b;z.add(v);s.add(sp(v));}
  return static_cast<double>(y.size())*log(Interval(t.k))+static_cast<double>(y.size())*log(Interval(t.c))+z.get()-(Interval(t.k)+1)*s.get();
}
struct Node {Proposal p;double lower=0,upper=0;};
inline bool dual(const Data& d,double k,Node& out,int maxit=120){
  maxit = static_cast<int>(solver_budget(maxit));
  Proposal t;if(!profile(d.y,k,t,maxit))return false;
  const std::size_t n=d.y.size();std::vector<double> p(n);std::size_t j=0;
  for(std::size_t i=0;i<n;++i){p[i]=sigmoid(t.c*d.y[i]+t.b);if(p[i]*(1-p[i])>p[j]*(1-p[j]))j=i;}
  Interval last;std::vector<double> q(n);bool feasible=false;
  for(double mix:{0.,1e-6,1e-4,.01,.1}){
    Sum others;for(std::size_t i=0;i<n;++i){q[i]=(1-mix)*p[i]+mix/(k+1);if(i!=j)others.add(Interval(q[i]));}
    last=static_cast<double>(n)/(Interval(k)+1)-others.get();
    if(last.valid()&&last.lo>=0&&last.hi<=1){feasible=true;break;}
  }
  if(!feasible)return false;
  Sum a,h;
  for(std::size_t i=0;i<n;++i){const Interval pi=i==j?last:Interval(q[i]);a.add(((Interval(k)+1)*pi-1)*d.box[i]);h.add(entropy(pi));}
  const Interval A=a.get();if(!A.valid()||A.lo<=0)return false;
  const Interval u=static_cast<double>(n)*log(Interval(k))+static_cast<double>(n)*log(static_cast<double>(n)/A)-static_cast<double>(n)-(Interval(k)+1)*h.get();
  const Interval l=primal(d.box,t);
  if(!u.valid()||!l.valid()||u.hi<l.lo)return false;
  out={t,l.lo,u.hi};return true;
}
// Monotone Weibull power score, with the exact root bracket propagated to
// W, A, and all fitted exponentials. Signed tails compare with this same W.
struct Weibull {Interval c,eta,W,A;std::vector<Interval> v;double proposal=0;};
inline bool weibull_at(const Data& d,Interval c,Interval& g,Interval& value){
  Sum sy,se,sye;for(Interval y:d.box){Interval e=exp(c*y);sy.add(y);se.add(e);sye.add(y*e);}
  const double n=d.y.size();g=n/c+sy.get()-n*sye.get()/se.get();
  value=n*log(c)+c*sy.get()-n*log(se.get()/n)-n;
  return g.valid()&&value.valid();
}
inline bool weibull(const Data& d,Weibull& w){
  const double n=d.y.size();
  auto score=[&](double c){double s=0,sy=0,total=0;for(double y:d.y){const double e=std::exp(c*(y-d.y.back()));s+=e;sy+=y*e;total+=y;}return n/c+total-n*sy/s;};
  double lo=1e-8,hi=1;if(!(score(lo)>0))return false;
  int it=0;while(score(hi)>0&&it++<d.maxit)hi*=2;
  if(!std::isfinite(score(hi))||score(hi)>0)return false;
  for(int i=0;i<d.maxit;++i){const double mid=(lo+hi)/2;if(score(mid)>0)lo=mid;else hi=mid;}
  const double c=(lo+hi)/2;w.proposal=c;double width=1e-10*std::max(c,1.);Interval gl,gh,vl,vh;
  bool bracket=false;
  for(int i=0;i<d.maxit;++i){lo=c-width;hi=c+width;if(lo<=0)return false;
    if(weibull_at(d,Interval(lo),gl,vl)&&weibull_at(d,Interval(hi),gh,vh)&&gl.lo>0&&gh.hi<0){bracket=true;break;}width*=2;}
  if(!bracket)return false;
  w.c={lo,hi};Sum e;for(Interval y:d.box)e.add(exp(w.c*y));w.eta=log(n/e.get());
  Sum squares;for(Interval y:d.box){Interval v=exp(w.c*y+w.eta);if(!v.valid()||v.lo<=0)return false;w.v.push_back(v);squares.add(v*v/2);}
  w.A=squares.get()-n;Interval gc,vc;if(!weibull_at(d,Interval(c),gc,vc))return false;
  w.W={vc.lo,(vl+gl*(Interval(hi)-lo)).hi};return w.W.valid()&&w.A.valid();
}
inline double signed_tail(const Data& d,const Weibull& w){
  if(w.A.hi>=0)return 0;
  const double n=d.y.size();Sum d0,d1,ee;
  for(std::size_t i=0;i<d.y.size();++i){Interval v=w.v[i],y=d.box[i];const double norm=y.mag()+1;d0.add(y*v*(v-1));d1.add(v*(v-1));ee.add(v*v*Interval((v-1).mag())*norm);}
  const double E=ee.get().hi;double best=0;
  for(double fraction:{.005,.02,.1,.3}){
    const double R=fraction*std::min(w.c.lo,1.);std::vector<Interval> low,high;
    for(std::size_t i=0;i<d.y.size();++i){const double norm=d.box[i].mag()+1;low.push_back(exp(log(Interval(w.v[i].lo))-R*norm));high.push_back(exp(log(Interval(w.v[i].hi))+R*norm));}
    double h=1;
    for(int j=0;j<d.maxit;++j,h/=2){
      Sum aa,bb,ee2;
      for(std::size_t i=0;i<d.y.size();++i){Interval weight=low[i]/(1+h*high[i])/(1+h*high[i]);if(!weight.valid())return 0;Interval ww(weight.lo);aa.add(ww*d.box[i]*d.box[i]);bb.add(ww*d.box[i]);ee2.add(ww);}
      Interval a=n/((Interval(w.c.hi)+R)*(Interval(w.c.hi)+R))+aa.get(),b=bb.get(),e=ee2.get(),det=a*e-b*b;
      if(!det.valid()||det.lo<=0||a.lo<=0||e.lo<=0)continue;
      Interval mu=det/(a+e);if(!mu.valid()||mu.lo<=0)continue;
      const double D=d0.get().mag()+d1.get().mag()+h*E;Interval DD=padded(D,D),B=n/Interval(6)+DD*DD/(2*Interval(mu.lo));
      Interval containment=h*DD/(Interval(mu.lo)*R),slope=h*B/(-w.A.hi);
      if(containment.valid()&&slope.valid()&&containment.hi<.5&&slope.hi<.5){best=std::max(best,h);break;}
    }
  }
  return best;
}
// Small fixed-size matrix operations; no external linear algebra dependency.
struct Matrix {std::array<std::array<double,3>,3> a{};};
struct BoxMatrix {std::array<std::array<Interval,3>,3> a{};};
inline bool inverse(Matrix m,Matrix& inv,int n){
  for(int i=0;i<n;++i)inv.a[i][i]=1;
  for(int i=0;i<n;++i){int pivot=i;for(int j=i+1;j<n;++j)if(std::abs(m.a[j][i])>std::abs(m.a[pivot][i]))pivot=j;
    if(!std::isfinite(m.a[pivot][i])||std::abs(m.a[pivot][i])<std::numeric_limits<double>::min())return false;
    std::swap(m.a[i],m.a[pivot]);std::swap(inv.a[i],inv.a[pivot]);const double v=m.a[i][i];
    for(int k=0;k<n;++k){m.a[i][k]/=v;inv.a[i][k]/=v;}
    for(int j=0;j<n;++j)if(j!=i){const double t=m.a[j][i];for(int k=0;k<n;++k){m.a[j][k]-=t*m.a[i][k];inv.a[j][k]-=t*inv.a[i][k];}}
  }
  for(int i=0;i<n;++i)for(int j=0;j<n;++j)if(!std::isfinite(inv.a[i][j]))return false;
  return true;
}
inline void numeric(const std::vector<double>& y,const Proposal& t,std::array<double,3>& g,Matrix& h){
  const double n=y.size();g={{n/t.k,n/t.c,n}};h={};h.a[0][0]=-n/(t.k*t.k);h.a[1][1]=-n/(t.c*t.c);
  for(double v:y){const double z=t.c*v+t.b,p=sigmoid(z),w=p*(1-p);g[0]-=softplus(z);g[1]+=v-(t.k+1)*v*p;g[2]-=(t.k+1)*p;
    h.a[0][1]-=v*p;h.a[0][2]-=p;h.a[1][1]-=(t.k+1)*v*v*w;h.a[1][2]-=(t.k+1)*v*w;h.a[2][2]-=(t.k+1)*w;}
  h.a[1][0]=h.a[0][1];h.a[2][0]=h.a[0][2];h.a[2][1]=h.a[1][2];
}
inline bool score_hessian(const Data& d,const std::array<Interval,3>& t,std::array<Interval,3>& g,BoxMatrix& h){
  const Interval k=t[0],c=t[1],b=t[2];if(!k.valid()||!c.valid()||k.lo<=0||c.lo<=0)return false;
  Sum sy,sy2w,syp,sp0,syw,sw,sps;
  for(Interval y:d.box){const Interval z=c*y+b,p=sig(z),w=p*(1-p);sy.add(y);sy2w.add(y*y*w);syp.add(y*p);sp0.add(p);syw.add(y*w);sw.add(w);sps.add(sp(z));}
  const double n=d.y.size();g={{n/k-sps.get(),n/c+sy.get()-(k+1)*syp.get(),n-(k+1)*sp0.get()}};
  h.a[0][0]=-n/(k*k);h.a[0][1]=-syp.get();h.a[0][2]=-sp0.get();h.a[1][1]=-n/(c*c)-(k+1)*sy2w.get();h.a[1][2]=-(k+1)*syw.get();h.a[2][2]=-(k+1)*sw.get();
  h.a[1][0]=h.a[0][1];h.a[2][0]=h.a[0][2];h.a[2][1]=h.a[1][2];
  for(int i=0;i<3;++i){if(!g[i].valid())return false;for(int j=0;j<3;++j)if(!h.a[i][j].valid())return false;}return true;
}
inline bool contraction(const Matrix& R,const std::array<Interval,3>& g,const BoxMatrix& h,const std::array<double,3>& r,int n,int offset){
  for(int i=0;i<n;++i){Interval eg;double qr=0;
    for(int j=0;j<n;++j)eg=eg+R.a[i][j]*g[j+offset];
    for(int j=0;j<n;++j){Interval e(i==j?1:0);for(int l=0;l<n;++l)e=e+R.a[i][l]*h.a[l+offset][j+offset];if(!e.valid())return false;qr+=e.mag()*r[j+offset];}
    const Interval ratio=(Interval(eg.mag())+padded(qr,qr))/r[i+offset];
    if(!ratio.valid()||ratio.hi>=.9)return false;
  }return true;
}
struct Root {Proposal t;std::array<double,3> radii{};double delta=0;};
inline bool isolate(const Data& d,Proposal t,Root& out){
  // Newton only proposes a center. Neither its residual nor convergence is
  // accepted as a certificate; the interval fixed-point test decides.
  for(int it=0;it<d.maxit;++it){std::array<double,3> g;Matrix h,R;numeric(d.y,t,g,h);if(!inverse(h,R,3))return false;
    std::array<double,3> step{};for(int i=0;i<3;++i)for(int j=0;j<3;++j)step[i]+=R.a[i][j]*g[j];
    if(!std::isfinite(step[0])||!std::isfinite(step[1])||!std::isfinite(step[2]))return false;
    if(std::abs(step[0])<1e-13*t.k&&std::abs(step[1])<1e-13*t.c&&std::abs(step[2])<1e-13*(1+std::abs(t.b)))break;
    if(t.k-step[0]<=0||t.c-step[1]<=0)return false;
    t.k-=step[0];t.c-=step[1];t.b-=step[2];
  }
  std::array<double,3> gn;Matrix hn,R;numeric(d.y,t,gn,hn);if(!inverse(hn,R,3))return false;for(auto& row:R.a)for(double& v:row)v=-v;
  const std::array<double,3> r{{1e-8*t.k,1e-8*t.c,1e-8*std::max(1.,std::abs(t.b))}};
  std::array<Interval,3> center{{t.k,t.c,t.b}},box{{padded(t.k-r[0],t.k+r[0]),padded(t.c-r[1],t.c+r[1]),padded(t.b-r[2],t.b+r[2])}},g;BoxMatrix hc,hb;
  if(!score_hessian(d,center,g,hc))return false;
  const auto gcenter=g;
  if(!score_hessian(d,box,g,hb)||!contraction(R,gcenter,hb,r,3,0))return false;
  // Cholesky of the negative center Hessian; its inverse transpose is an
  // arbitrary congruence proposal, validated by the subsequent enclosure.
  Matrix L;
  for(int i=0;i<3;++i)for(int j=0;j<=i;++j){double v=-hn.a[i][j];for(int k=0;k<j;++k)v-=L.a[i][k]*L.a[j][k];if(i==j){if(!(v>0)||!std::isfinite(v))return false;L.a[i][j]=std::sqrt(v);}else L.a[i][j]=v/L.a[j][j];}
  Matrix Linv,C;if(!inverse(L,Linv,3))return false;for(int i=0;i<3;++i)for(int j=0;j<3;++j)C.a[i][j]=Linv.a[j][i];
  Matrix nuisance,NR;for(int i=0;i<2;++i)for(int j=0;j<2;++j)nuisance.a[i][j]=hn.a[i+1][j+1];
  if(!inverse(nuisance,NR,2))return false;
  for(auto& row:NR.a)for(double& v:row)v=-v;
  for(double fraction:{.03,.01,.003,.001,.0003,.0001,.00003,.00001,.000003,.000001}){
    const double delta=t.k*fraction;std::array<double,3> nr{{0,0,0}};
    for(int i=0;i<2;++i){for(int j=0;j<2;++j)nr[i+1]+=2*std::abs(NR.a[i][j])*std::abs(hn.a[j+1][0])*delta;nr[i+1]+=delta*.001*(i==0?t.c:std::max(1.,std::abs(t.b)));}
    bool contained=false;
    for(int attempt=0;attempt<d.maxit;++attempt){if(nr[1]>=t.c)break;
      std::array<Interval,3> ct{{padded(t.k-delta,t.k+delta),t.c,t.b}},full{{ct[0],padded(t.c-nr[1],t.c+nr[1]),padded(t.b-nr[2],t.b+nr[2])}};
      if(!score_hessian(d,ct,g,hc))break;
      const auto nuisance_g=g;
      if(!score_hessian(d,full,g,hb))break;
      if(contraction(NR,nuisance_g,hb,nr,2,1)){contained=true;break;}nr[1]*=1.6;nr[2]*=1.6;
    }
    if(!contained||r[0]>=delta||r[1]>=nr[1]||r[2]>=nr[2])continue;
    bool positive=true;
    for(int i=0;i<3;++i){Sum row;
      for(int j=0;j<3;++j){Interval e(i==j?-1:0);for(int l=0;l<3;++l)for(int m=0;m<3;++m)e=e-C.a[l][i]*hb.a[l][m]*C.a[m][j];if(!e.valid()){positive=false;break;}row.add(Interval(e.mag()));}
      if(!row.get().valid()||row.get().hi>=.9)positive=false;
    }
    if(positive){
      const Interval scale_box=exp(-padded(t.b-r[2],t.b+r[2])/padded(t.c-r[1],t.c+r[1]));
      if(!scale_box.valid()||scale_box.lo<=0||(scale_box.hi-scale_box.lo)/scale_box.lo>2e-6)return false;
      out={t,r,delta};return true;
    }
  }return false;
}
struct Segment {Node a,b;double upper;bool operator<(const Segment& other)const{return upper<other.upper;}};
inline double upper(const Node& a,const Node& b,double n){const Interval diff=Interval(b.p.k)-a.p.k;const Interval u=std::max(a.upper,b.upper)+n*diff*diff/(8*Interval(a.p.k)*Interval(a.p.k));return u.valid()?u.hi:INFINITY;}
enum class Outcome { Finite, Weibull, Pareto, Tie, Budget, Numerical, Invalid, Unbounded };
struct Result {
  StopDetail stop_detail;
  Outcome outcome=Outcome::Numerical;
  double k=NAN,c=NAN,scale=NAN,logLik=NAN;
  int profiles=0;
  // Diagnostic counts and enclosure summaries do not serve as decisions.
  int roots=0,pruned=0;
  double cutoff=NAN,lower=NAN,remaining=NAN;
};
inline Result fit(span x,std::int64_t budget=-1,int maxit=120) {
  maxit = static_cast<int>(solver_budget(maxit));
  Result result;
  if(budget<0)budget=certificate_budget(maxit);
  if(budget<1){result.outcome=Outcome::Budget;return result;}
  if(x.size()<2){result.outcome=Outcome::Invalid;return result;}
  bool constant=true;
  double minimum=INFINITY;
  for(double v:x){if(!(v>0)||!std::isfinite(v)){result.outcome=Outcome::Invalid;return result;}if(v!=x[0])constant=false;minimum=std::min(minimum,v);}
  if(constant){result.outcome=Outcome::Unbounded;result.logLik=INFINITY;return result;}
  Data d;d.maxit=maxit;if(!data(x,d))return result;
  const double n=x.size();int multiplicity=0;for(double v:x)if(v==minimum)++multiplicity;
  const double k0=std::nextafter(multiplicity/(n-multiplicity),0.);
  Sum distances;for(Interval y:d.box)distances.add(y-d.box.front());
  // The selected minimum is the same exact data logarithm in every term.
  // Dependency loss enlarges the sum and cannot sharpen the Pareto bound.
  const Interval P=n*log(n/distances.get())-n;
  Weibull w;if(!P.valid()||!weibull(d,w))return result;
  double distance=0,e=0;
  for(double y:d.y){distance+=y-d.y.front();e+=std::exp(w.proposal*y);}
  const double wc=w.proposal/d.unit;
  const double ws=std::exp(d.center-std::log(n/e)/wc);
  result.stop_detail.candidates.push_back({{n/distance/d.unit,minimum},P.lo+d.jacobian,P.hi+d.jacobian,StopModel::Pareto});
  result.stop_detail.candidates.push_back({{wc,ws},w.W.lo+d.jacobian,w.W.hi+d.jacobian,StopModel::Weibull});
  result.stop_detail.loglik_lower=std::max(P.lo,w.W.lo)+d.jacobian;
  result.stop_detail.loglik_upper=INFINITY;
  double best=std::max(P.lo,w.W.lo);
  Outcome winner=P.lo>=w.W.lo?Outcome::Pareto:Outcome::Weibull;
  std::map<double,Node> cache;
  auto get=[&](double k,Node& node){
    auto found=cache.find(k);if(found!=cache.end()){node=found->second;return true;}
    if(result.profiles>=budget){result.outcome=Outcome::Budget;return false;}
    ++result.profiles;
    if(!dual(d,k,node,maxit)){result.outcome=Outcome::Numerical;return false;}
    cache.emplace(k,node);
    const Interval value=primal(d.box,node.p);
    if(value.valid()) {
      const double c=node.p.c/d.unit,s=std::exp(d.center-node.p.b/c);
      if(std::isfinite(c)&&std::isfinite(s)&&c>0&&s>0) {
        FitCandidate candidate{{node.p.k,c,s},value.lo+d.jacobian,value.hi+d.jacobian};
        // Keep the best finite witness, not every subdivision evaluation.
        if(result.stop_detail.candidates.size()==2)result.stop_detail.candidates.push_back(candidate);
        else if(candidate.loglik_lower>result.stop_detail.candidates[2].loglik_lower)result.stop_detail.candidates[2]=candidate;
        result.stop_detail.loglik_lower=std::max(result.stop_detail.loglik_lower,candidate.loglik_lower);
      }
    }
    return true;
  };
  Root winning;bool has_root=false;
  // Reconnaissance only proposes roots; it never certifies exhaustiveness.
  // A missed root remains in the global envelope and prevents closure.
  Node previous;
  for(int i=0;i<=40;++i){
    const double k=k0*std::exp(std::log(std::max(10000.,4*n)/k0)*i/40.);
    Node node;if(!get(k,node))return result;
    if(i>0&&previous.p.score>0&&node.p.score<0){
      double lo=previous.p.k,hi=node.p.k;Proposal t=node.p;
      for(int j=0;j<maxit;++j){const double mid=std::sqrt(lo*hi);if(!profile(d.y,mid,t,maxit))return result;if(t.score>0)lo=mid;else hi=mid;if(hi-lo<1e-10*(1+mid))break;}
      Root root;
      // Root neighborhoods use log(x / proposed_scale). This exact affine
      // coordinate change avoids large correlated c,b interval boxes.
      Data original;
      Proposal raw=t;raw.c=t.c/d.unit;
      const double shift=d.center-t.b/raw.c;
      for(double v:x){const double y=std::log(v);original.y.push_back(y-shift);original.box.push_back(padded(y,y)-shift);}
      raw.b=t.b+raw.c*(shift-d.center);
      if(isolate(original,raw,root)){
        root.t.b+=root.t.c*(d.center-shift);root.t.c*=d.unit;
        ++result.roots;const Interval value=primal(d.box,root.t);
        const double c=root.t.c/d.unit,scale=std::exp(d.center-root.t.b/c);
        if(value.valid()&&std::isfinite(scale)&&scale>0)
          result.stop_detail.candidates.push_back({{root.t.k,c,scale},value.lo+d.jacobian,value.hi+d.jacobian});
        if(value.valid()&&value.lo>std::max(P.hi,w.W.hi)&&(!has_root||value.lo>best)){best=value.lo;winning=root;has_root=true;winner=Outcome::Finite;}
      }
    }
    previous=node;
  }
  double K=2*n;const double h=signed_tail(d,w);
  if(h>0)K=1/h;
  else {
    bool tail=false;
    for(int i=0;i<maxit;++i){const Interval error=n*n*(Interval(K)+1)/(2*(Interval(K)+1-n)*(Interval(K)+1-n));
      const Interval bound=w.W+error;
      if(bound.valid()&&bound.hi<best){tail=true;break;}K*=2;if(!std::isfinite(K))return result;}
    if(!tail){result.outcome=w.A.lo<=0&&w.A.hi>=0?Outcome::Tie:Outcome::Budget;return result;}
  }
  result.cutoff=K;result.lower=best;
  // A signed tail is below W. It is dominated by the retained finite/Pareto
  // witness, or excludes finite attainment when Weibull is the winner.
  if(h>0&&winner!=Outcome::Weibull&&!(best>w.W.hi)){result.outcome=Outcome::Tie;return result;}
  const double slab_lo=has_root?winning.t.k-winning.delta:0,slab_hi=has_root?winning.t.k+winning.delta:0;
  if(has_root&&!(k0<slab_lo&&slab_hi<K))return result;
  const int count=std::max(1,static_cast<int>(std::ceil(std::log(K/k0)/.4)));
  if(count>budget){result.outcome=Outcome::Budget;return result;}
  std::vector<double> knots;for(int i=0;i<=count;++i)knots.push_back(i==count?K:k0*std::exp(std::log(K/k0)*i/count));
  if(has_root){knots.push_back(slab_lo);knots.push_back(slab_hi);}
  std::sort(knots.begin(),knots.end());knots.erase(std::unique(knots.begin(),knots.end()),knots.end());
  std::priority_queue<Segment> heap;
  auto push=[&](const Node& a,const Node& b){
    if(has_root&&a.p.k>=slab_lo&&b.p.k<=slab_hi)return;
    const double u=upper(a,b,n);
    if(u<best){++result.pruned;return;}
    heap.push({a,b,u});
  };
  Node a;if(!get(knots.front(),a))return result;
  for(std::size_t i=1;i<knots.size();++i){Node b;if(!get(knots[i],b))return result;push(a,b);a=b;}
  while(!heap.empty()){
    const Segment s=heap.top();heap.pop();result.remaining=s.upper;
    result.stop_detail.segment_loglik_upper=s.upper+d.jacobian;
    const double mid=std::sqrt(s.a.p.k*s.b.p.k);
    if(!(mid>s.a.p.k&&mid<s.b.p.k)){result.outcome=Outcome::Tie;return result;}
    Node b;if(!get(mid,b))return result;push(s.a,b);push(b,s.b);
  }
  if(winner==Outcome::Pareto&&!(P.lo>w.W.hi)){result.outcome=Outcome::Tie;return result;}
  if(winner==Outcome::Weibull&&!(w.W.lo>P.hi)){result.outcome=Outcome::Tie;return result;}
  result.outcome=winner;
  if(winner==Outcome::Finite){result.k=winning.t.k;result.c=winning.t.c/d.unit;result.scale=std::exp(d.center-winning.t.b/result.c);result.logLik=primal(d.box,winning.t).lo+d.jacobian;
    // Return the center's directly evaluated likelihood, after certification.
    double value=n*(std::log(winning.t.k)+std::log(winning.t.c));for(double y:d.y){const double z=winning.t.c*y+winning.t.b;value+=winning.t.k<1?-winning.t.k*z-(winning.t.k+1)*softplus(-z):z-(winning.t.k+1)*softplus(z);}result.logLik=value+d.jacobian;
  }else if(winner==Outcome::Weibull){result.k=INFINITY;result.c=w.proposal/d.unit;double e=0,sy=0;for(double y:d.y){e+=std::exp(w.proposal*y);sy+=y;}const double eta=std::log(n/e);result.scale=std::exp(d.center-eta/result.c);result.logLik=n*std::log(w.proposal)+w.proposal*sy-n*std::log(e/n)-n+d.jacobian;
  }else {result.k=0;result.c=INFINITY;result.scale=minimum;double distance=0;for(double y:d.y)distance+=y-d.y.front();result.logLik=n*std::log(n/distance)-n+d.jacobian;}
  if(!std::isfinite(result.scale)||!(result.scale>0)||!std::isfinite(result.logLik)){result.outcome=Outcome::Numerical;result.k=result.c=result.scale=result.logLik=NAN;}
  return result;
}
} // namespace burr_cert_detail
} // namespace cppunivariate
