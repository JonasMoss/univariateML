// Research prototype; not a production d/p/q/r implementation.
#include <algorithm>
#include <cmath>
#include <iomanip>
#include <iostream>
#include <limits>

namespace {
constexpr double half_log_two_pi = 0.91893853320467274178;

double penalty(double w, double q) {
  const double z=q*w;
  if (std::abs(z)<0.1) {
    double term=0.5, sum=term;
    for (int k=3;k<=24;++k) { term*=z/k; sum+=term; }
    return (0.5*w)*w*(2*sum);
  }
  return (std::expm1(z)-z)/(q*q);
}

double remainder(double q) {
  const double s=q*q;
  return s*(1.0/12-s*s/360+s*s*s*s/1260-s*s*s*s*s*s/1680);
}

struct Integrand {
  double w, q, base, rate, direction;
  double operator()(double t) const {
    if (t>=1) return 0;
    const double u=1-t;
    const double v=w+direction*(t/u)/rate;
    const double difference=penalty(v,q)-base;
    if (!std::isfinite(difference)) return 0;
    return std::exp(-difference-2*std::log(u)-std::log(rate));
  }
};

double simpson(const Integrand &f,double a,double b,double fa,double fm,double fb,
               double estimate,double tolerance,int depth,bool &ok) {
  const double m=(a+b)/2, l=(a+m)/2, r=(m+b)/2;
  const double fl=f(l), fr=f(r);
  const double left=(m-a)*(fa+4*fl+fm)/6;
  const double right=(b-m)*(fm+4*fr+fb)/6;
  const double delta=left+right-estimate;
  if (std::abs(delta)<=15*tolerance) return left+right+delta/15;
  if (depth==0) { ok=false; return 0; }
  return simpson(f,a,m,fa,fl,fm,left,tolerance/2,depth-1,ok)+
         simpson(f,m,b,fm,fr,fb,right,tolerance/2,depth-1,ok);
}

double tail(double w,double q) {
  const double p=penalty(w,q);
  const double slope=q==0 ? w : std::expm1(q*w)/q;
  const double rate=std::max(1.0,std::abs(slope));
  const Integrand f{w,q,p,rate,w<0 ? -1.0 : 1.0};
  const double fa=f(0), fm=f(0.5), fb=f(1);
  bool ok=true;
  const double value=simpson(f,0,1,fa,fm,fb,(fa+4*fm+fb)/6,1e-13,25,ok);
  return ok && value>0 ? -half_log_two_pi-remainder(q)-p+std::log(value)
                       : std::numeric_limits<double>::quiet_NaN();
}
}
int main() {
  std::cout<<std::setprecision(17);
  for (double q : {-0.01,-1e-8,0.0,1e-8,0.01})
    for (double w : {-100.,-40.,-6.,-1.,0.,1.,6.,40.,100.})
      std::cout<<q<<' '<<w<<' '<<tail(w,q)<<'\n';
}
