#pragma once
#include <cmath>
#include <algorithm>
#include <limits>
namespace st_wedge_detail {
// Composite 16-point Gauss-Legendre, with adaptive subdivision. Numerical accuracy is checked against independent quadrature.
static constexpr double nodes[] = {0.09501250983763744,0.2816035507792589,0.4580167776572274,0.6178762444026438,0.7554044083550030,0.8656312023878318,0.9445750230732326,0.9894009349916499};
static constexpr double weights[] = {0.1894506104550685,0.1826034150449236,0.1691565193950025,0.1495959888165767,0.1246289712555339,0.0951585116824928,0.06225352393864789,0.02715245941175409};
inline double softplus(double x) { return x>0 ? x+std::log1p(std::exp(-x)) : std::log1p(std::exp(x)); }
struct Integrand {
 double theta, logt, nu, base, power;
 double operator()(double u) const {
   double r=theta*std::pow(u,power);
   if(r<=0) return 0;
   double s=std::sin(r);
   double v=-.5*nu*softplus(2*(logt-std::log(s))-std::log(nu));
   return std::exp(v-base)*power*std::pow(u,power-1);
 }
};
inline double panel(const Integrand& f,double l,double r) {
 double mid=(l+r)*.5, half=(r-l)*.5, sum=0;
 for(int i=0;i<8;i++) sum+=weights[i]*(f(mid-half*nodes[i])+f(mid+half*nodes[i]));
 return half*sum;
}
inline double adaptive(const Integrand& f,double l,double r,double whole,double tol,int depth,bool& ok) {
 double mid=(l+r)*.5, a=panel(f,l,mid),b=panel(f,mid,r),v=a+b;
 if(std::abs(v-whole)<tol) return v;
 if(depth<=0) {ok=false;return v;}
 return adaptive(f,l,mid,a,tol*.5,depth-1,ok)+adaptive(f,mid,r,b,tol*.5,depth-1,ok);
}
inline double thin_log(double t,double a,double nu,bool& ok) {
 constexpr double pi=3.1415926535897932384626433832795;
 double theta=std::atan2(1.,a);
 if(t==0) return std::log(theta)-std::log(pi);
 double logt=std::log(t), logtheta=std::log(std::sin(theta));
 double base=-.5*nu*softplus(2*(logt-logtheta)-std::log(nu));
 Integrand f{theta,logt,nu,base,std::max(1., 8./nu)};
 double all=panel(f,0,1);
 if(all==0 || !std::isfinite(base)) {ok=false;return -INFINITY;}
 double val=adaptive(f,0,1,all,1e-12*all,24,ok);
 return base+std::log(theta)+std::log(val)-std::log(pi);
}
}
