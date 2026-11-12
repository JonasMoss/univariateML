#pragma once
#include "special.hpp"
#include <array>
namespace cppunivariate { namespace special { namespace skew_t_angular {
// Integrate the angular wedge relative to its endpoint kernel. Subtracting
// two O(nu) log kernels makes the adaptive comparison follow rounding noise.
// Instead K(r)/K(theta) = (1+h*((sin(theta)/sin(r))^2-1))^(-nu/2),
// h=t^2/(nu*sin(theta)^2+t^2). This also handles narrow angular wedges.
struct Integrand {
 double theta,nu,h,rate;
 double operator()(double v) const {
  const double y=v/rate,theta2=theta*theta;
  const double correction=theta2*std::expm1(-2*y)/3+theta2*theta2*std::expm1(-4*y)/90;
  const double delta=std::expm1(2*y+correction);
  return std::exp(-nu*.5*std::log1p(h*delta)-y)/rate;
 }
};
inline double panel(const Integrand& f,double l,double r) {
 constexpr std::array<double,8> nodes{{.09501250983763744,.2816035507792589,.4580167776572274,.6178762444026438,.755404408355003,.8656312023878318,.9445750230732326,.9894009349916499}};
 constexpr std::array<double,8> weights{{.1894506104550685,.1826034150449236,.1691565193950025,.1495959888165767,.1246289712555339,.0951585116824928,.06225352393864789,.02715245941175409}};
 const double mid=(l+r)*.5,half=(r-l)*.5;double sum=0;
 for(int i=0;i<8;++i)sum+=weights[i]*(f(mid-half*nodes[i])+f(mid+half*nodes[i]));
 return half*sum;
}
inline double adaptive(const Integrand& f,double l,double r,double whole,double tol,int depth,bool& ok) {
 const double mid=(l+r)*.5,a=panel(f,l,mid),b=panel(f,mid,r),v=a+b;
 if(std::abs(v-whole)<=tol)return v;
 if(depth==0){ok=false;return v;}
 return adaptive(f,l,mid,a,tol*.5,depth-1,ok)+adaptive(f,mid,r,b,tol*.5,depth-1,ok);
}
inline double thin_log(double t,double a,double nu,bool& ok,double& error) {
 const double theta=std::atan2(1.,a),s=std::sin(theta);
 if(t==0)return std::log(theta)-std::log(3.14159265358979323846);
 const double eps=std::numeric_limits<double>::epsilon();
 const double scaled=t/(std::sqrt(nu)*s),square=scaled*scaled;
 const double logy=2*(std::log(t)-std::log(s))-std::log(nu);
 const bool normal=std::isnormal(square) && std::isfinite(square);
 const double base=-.5*nu*(normal?std::log1p(square):special::softplus(logy));
 const double h=normal?square/(1+square):std::exp(-special::softplus(-logy));
 error=(normal?8*eps:16*eps*(1+std::abs(logy)))*(.5*nu*h)+4*eps*std::abs(base)+2e-11;

 // r=theta*exp(-v/rate) resolves the endpoint layer even when
 // its angular width is theta/nu. The logarithmic derivative is at
 // least rate=1+nu*h*(1-theta^2). Thus the omitted v>64 mass is
 // <=exp(-64)/rate. log(sinc r) has remainder O(theta^6), <1e-36.
 const double rate=1+nu*h*(1-theta*theta);
 const Integrand f{theta,nu,h,rate};
 const double all=panel(f,0,64);
 if(!(all>0) || !std::isfinite(base)){ok=false;return kNaN;}
 const double integral=adaptive(f,0,64,all,8e-12*all,24,ok);
 if(!(integral>0)){ok=false;return kNaN;}
 return base+std::log(theta)+std::log(integral)-std::log(3.14159265358979323846);
}
} } }
