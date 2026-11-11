#pragma once
// Second-order forward derivatives for the six all-free accuracy refinements.
// Arithmetic is binary64 in every build, including WASM. Branches select the
// smooth piece containing the candidate; no differencing of likelihoods.
#include "../../special/special.hpp"
#include "../../core/work_budget.hpp"
#include <array>
#include <algorithm>
#include <cmath>
#include <limits>
namespace cppunivariate { namespace local_accuracy {
// Back-transformation must leave enough representable public coefficients.
// A full ulp covers the final rounding plus the checked internal correction.
inline bool represented(double value,double scale,double tol){
 if(!std::isfinite(value)||!(scale>0)||!std::isfinite(scale))return false;
 const double a=std::abs(value),gap=a==std::numeric_limits<double>::max()?a-std::nextafter(a,0.):std::nextafter(a,INFINITY)-a;
 return gap/scale<=tol;
}
struct Jet {
 double v=0,mass=0; std::array<double,4> g{}; std::array<double,16> h{};
 Jet(double value=0):v(value),mass(std::abs(value)){}
 static Jet variable(double value,int j){Jet a(value);a.g[j]=1;return a;}
};
inline Jet operator+(const Jet& a,const Jet& b){Jet c(a.v+b.v);c.mass=a.mass+b.mass;for(int i=0;i<4;++i)c.g[i]=a.g[i]+b.g[i];for(int i=0;i<16;++i)c.h[i]=a.h[i]+b.h[i];return c;}
inline Jet operator-(const Jet& a){Jet c(-a.v);for(int i=0;i<4;++i)c.g[i]=-a.g[i];for(int i=0;i<16;++i)c.h[i]=-a.h[i];return c;}
inline Jet operator-(const Jet& a,const Jet& b){return a+-b;}
inline Jet operator*(const Jet& a,const Jet& b){Jet c(a.v*b.v);for(int i=0;i<4;++i){c.g[i]=a.g[i]*b.v+a.v*b.g[i];for(int j=0;j<4;++j)c.h[4*i+j]=a.h[4*i+j]*b.v+a.v*b.h[4*i+j]+a.g[i]*b.g[j]+a.g[j]*b.g[i];}return c;}
inline Jet chain(const Jet& a,double v,double d,double dd){Jet c(v);for(int i=0;i<4;++i){c.g[i]=d*a.g[i];for(int j=0;j<4;++j)c.h[4*i+j]=d*a.h[4*i+j]+dd*a.g[i]*a.g[j];}return c;}
inline Jet inverse(const Jet& a){return chain(a,1/a.v,-1/(a.v*a.v),2/(a.v*a.v*a.v));}
inline Jet operator/(const Jet& a,const Jet& b){return a*inverse(b);}
inline Jet exp(const Jet& a){double v=std::exp(a.v);return chain(a,v,v,v);}
inline Jet log(const Jet& a){return chain(a,std::log(a.v),1/a.v,-1/(a.v*a.v));}
inline Jet log1p(const Jet& a){return chain(a,std::log1p(a.v),1/(1+a.v),-1/((1+a.v)*(1+a.v)));}
inline Jet sqrt(const Jet& a){double s=std::sqrt(a.v);return chain(a,s,.5/s,-.25/(a.v*s));}
inline Jet lgamma(const Jet& a){return chain(a,special::lgamma(a.v),special::digamma(a.v),special::trigamma(a.v));}
inline Jet abs(const Jet& a){return a.v<0?-a:a;}
inline Jet softplus(const Jet& a){return a.v>0?a+log1p(exp(-a)):log1p(exp(a));}
inline Jet log1pmx(const Jet& x){
 if(x.v>.125)return log1p(x)-x;
 Jet p=-1./24;for(int k=23;k>=2;--k)p=(k%2?1.:-1.)/k+x*p;
 return x*x*p;
}
inline Jet stirling(const Jet& z){auto t=1/z,t2=t*t;return t*(1./12+t2*(-1./360+t2*(1./1260+t2*(-1./1680+t2*(1./1188+t2*(-691./360360+t2/156))))));}
// Recurrence shifts the half-step gamma ratio to the Stirling regime.
// Combine its leading logarithm analytically in the Student limit.
inline Jet half_gamma_correction(const Jet& t){auto t2=t*t;return t*(-1./8+t2*(1./192+t2*(-1./640+t2*(17./14336+t2*(-31./18432+t2*(691./180224-t2*5461./851968))))));}
inline Jet half_gamma_ratio(Jet z){Jet correction=0;while(z.v<16){correction=correction-log1p(.5/z);z=z+1;}return correction+.5*log(z)+half_gamma_correction(1/z);}
inline Jet logbeta(Jet p,Jet q){
 Jet correction=0;
 while(p.v<16){correction=correction+log1p(q/p);p=p+1;}
 while(q.v<16){correction=correction+log1p(p/q);q=q+1;}
 auto s=p+q;
 return correction-(p-.5)*log1p(q/p)-(q-.5)*log1p(p/q)-.5*log(s)+.91893853320467274178+stirling(p)+stirling(q)-stirling(s);
}
inline std::array<Jet,4> variables(const std::array<double,4>& p){std::array<Jet,4> a;for(int j=0;j<4;++j)a[j]=Jet::variable(p[j],j);return a;}
// Compensate every derivative component as well as the likelihood. A tiny
// score formed by subtracting O(n) terms otherwise drifts under replication.
inline void accumulate(double& sum,double& correction,double term){const double next=sum+term;correction+=std::abs(sum)>=std::abs(term)?(sum-next)+term:(term-next)+sum;sum=next;}
struct JetSum {
 Jet sum,correction;
 explicit JetSum(const Jet& first):sum(first){}
 void add(const Jet& term){accumulate(sum.v,correction.v,term.v);sum.mass+=term.mass;for(int j=0;j<4;++j)accumulate(sum.g[j],correction.g[j],term.g[j]);for(int j=0;j<16;++j)accumulate(sum.h[j],correction.h[j],term.h[j]);}
 Jet value()const{auto out=sum;out.v+=correction.v;for(int j=0;j<4;++j)out.g[j]+=correction.g[j];for(int j=0;j<16;++j)out.h[j]+=correction.h[j];return out;}
};
// Cholesky also checks positive definiteness, rather than using a small score
// as a surrogate for invertible observed information.
inline bool step(const Jet& f,int n,std::array<double,4>& s){
 std::array<double,16> l{};
 for(int i=0;i<n;++i)for(int j=0;j<=i;++j){double a=f.h[4*i+j];for(int k=0;k<j;++k)a-=l[4*i+k]*l[4*j+k];if(i==j){if(!(a>0&&std::isfinite(a)))return false;l[4*i+j]=std::sqrt(a);}else l[4*i+j]=a/l[4*j+j];}
 for(int i=0;i<n;++i){double a=f.g[i];for(int j=0;j<i;++j)a-=l[4*i+j]*s[j];s[i]=a/l[4*i+i];}
 for(int i=n-1;i>=0;--i){double a=s[i];for(int j=i+1;j<n;++j)a-=l[4*j+i]*s[j];s[i]=a/l[4*i+i];if(!std::isfinite(s[i]))return false;}return true;
}
// `error` maps the undamped Newton correction to relative errors in PUBLIC
// coefficients, including the location floor and profile back-transform.
// Require a second independently evaluated correction after a full Newton
// step: contraction must be quadratic (or at the binary64 rounding floor).
template<class Objective,class Error>
inline bool polish(std::array<double,4>& p,int n,double tol,int maxit,Objective objective,Error error){
 if(!(tol>=8*std::numeric_limits<double>::epsilon())||!std::isfinite(tol)||maxit<1)return false;
 tol=std::min(tol,1e-10);
 double previous=INFINITY;bool full=false;
 for(std::int64_t it=0;it<solver_budget(maxit);++it){
  const Jet f=objective(variables(p));std::array<double,4> s{};
  if(!std::isfinite(f.v)||!std::isfinite(f.mass)||!step(f,n,s))return false;
  const double e=error(p,s);
  // Score rounding is amplified by inverse information. A plateau below the
  // requested error is legitimate; it need not keep decreasing quadratically
  // below the attainable arithmetic scale. This is a practical FP margin.
  double rounding=1;
  for(int j=0;j<n;++j){Jet column=f;column.g.fill(0);column.g[j]=1;std::array<double,4> response{};
    if(!step(column,n,response))return false;
    rounding+=error(p,response);}
  rounding*=128*std::numeric_limits<double>::epsilon();
  if(full && e<=tol*.125 && (e<=16*previous*previous || e<rounding)) {
   // Return the corrected point, with its own information/step check. This
   // avoids spending the error allowance on an avoidable last correction.
   auto q=p;for(int j=0;j<n;++j)q[j]-=s[j];
   const Jet next=objective(variables(q));std::array<double,4> remaining{};
   if(std::isfinite(next.v)&&std::isfinite(next.mass)&&step(next,n,remaining)&&error(q,remaining)<=tol*.125){
    // In a poorly conditioned basin, check rounding in the projected score,
    // preserving correlations between its coordinates. A small computed
    // correction alone must not hide arithmetic amplified by inverse H.
    double inverse_size=0,hsize=0;
    for(int j=0;j<n;++j){Jet column=next;column.g.fill(0);column.g[j]=1;std::array<double,4> response{};
      if(!step(column,n,response))return false;
      inverse_size+=error(q,response);for(int k=0;k<n;++k)hsize=std::max(hsize,std::abs(next.h[4*j+k]));}
    if(128*std::numeric_limits<double>::epsilon()*(1+hsize*inverse_size)>tol*.125){
      double noise=0;
      for(int j=0;j<n;++j)for(double direction:{-1.,1.}){
        auto nearby=q;nearby[j]+=direction*16*std::numeric_limits<double>::epsilon()*std::max(1.,std::abs(q[j]));
        const double delta=nearby[j]-q[j];const Jet sampled=objective(variables(nearby));Jet residual=next;
        for(int k=0;k<n;++k)residual.g[k]=sampled.g[k]-next.g[k]-next.h[4*k+j]*delta;
        std::array<double,4> response{};if(!std::isfinite(sampled.v)||!step(residual,n,response))return false;
        noise=std::max(noise,error(q,response));
      }
      if(2*noise+error(q,remaining)>tol*.75)return false;
    }
    p=q;return true;
   }
  }
  double damping=1;bool accepted=false;std::array<double,4> q=p;
  for(std::int64_t ls=0;ls<solver_budget(maxit);++ls){for(int j=0;j<n;++j)q[j]=p[j]-damping*s[j];const Jet next=objective(variables(q));
   // Permit only rounding-scale objective changes; the next undamped step,
   // never the line-search displacement, decides accuracy.
   if(std::isfinite(next.v)&&std::isfinite(next.mass)&&next.v<=f.v+64*std::numeric_limits<double>::epsilon()*(1+f.mass)){accepted=true;break;}damping*=.5;}
  if(!accepted)return false;
  previous=e;full=damping==1;p=q;
 }return false;
}
inline double log_error(const std::array<double,4>&,const std::array<double,4>& s){double e=0;for(double x:s)e=std::max(e,std::abs(x));return e;}
// Variance-standardized two-piece t / GED objective in (location, log sd,
// log(nu-offset), log skew). Constants are differentiated before summation.
template<class Sample>
inline Jet skew_objective(const Sample& x,const std::array<Jet,4>& p,bool student){
 const Jet sigma=exp(p[1]),nu=(student?2.:1.)+exp(p[2]),xi=exp(p[3]);
 Jet m,logscale,constant;
 if(student){
  m=exp(nu.v>=33?.5*std::log(2/3.14159265358979323846)+.5*log1p(-1/(nu-1))-half_gamma_correction(2/(nu-1)):.5*log((nu-2)/3.14159265358979323846)-half_gamma_ratio((nu-1)/2));
  constant=nu.v>=32?-.91893853320467274178+.5*log1p(2/(nu-2))+half_gamma_correction(2/nu):half_gamma_ratio(nu/2)-.5*log((nu-2)*3.14159265358979323846);
 }
 else{logscale=.5*(lgamma(1/nu)-lgamma(3/nu));m=exp(lgamma(2/nu)-.5*(lgamma(1/nu)+lgamma(3/nu)));constant=log(nu)-logscale-lgamma(1/nu)-std::log(2.);}
 const Jet m2=m*m,su=sqrt((1-m2)*(xi*xi+1/(xi*xi))+2*m2-1),mu=m*(xi-1/xi);
 const Jet base=p[1]-log(su)+log(xi+1/xi)-std::log(2.)-constant;
 JetSum f(double(x.size())*base);
 for(double v:x){Jet z=(v-p[0])*su/sigma+mu;Jet w=z.v>0?z/xi:z*xi;
  if(student){auto square=w*w,y=square/(nu-2);f.add(y.v<.125?square/2+1.5*log1p(y)+(nu-2)/2*log1pmx(y):(nu+1)/2*log1p(y));}
  else if(w.v!=0)f.add(exp(nu*(log(abs(w))-logscale)));
 }
 return f.value();
}
inline double location_error(const std::array<double,4>& p,const std::array<double,4>& s){return std::max(std::abs(s[0])/std::max(std::abs(p[0]),std::exp(p[1])),std::max(std::abs(s[1]),std::max(std::abs(s[2]),std::abs(s[3]))));}
} }
