#include "proto.hpp"
#include <cstdio>
int main(){
  // (1) -z psi(z) increasing in z>0, for l in [-6,1)
  int viol1=0,checked1=0; double worst1=0, wl1=0, wz1=0;
  for(double l=-6;l<0.999;l+=0.01){
    if(std::abs(l-1)<1e-9) continue;
    double prev=-1; double zmax=l>0?0.999/l:1e6;
    for(double t=-8;t<std::log(zmax);t+=0.01){
      const double z=std::exp(t); auto o=proto::eval(z,std::abs(l)<1e-12?0:l); if(!o.ok) break;
      const double m=-z*o.psi; ++checked1;
      if(prev>=0 && m<prev-1e-12*(1+prev)){++viol1; if(prev-m>worst1){worst1=prev-m;wl1=l;wz1=z;}}
      prev=m;
    }
  }
  std::printf("(1) -z psi increasing: checked %d, violations %d, worst drop %.3g at l=%.2f z=%.3g\n",checked1,viol1,worst1,wl1,wz1);
  // (2) d2/dl2 log g(z;l) <= 0 : central difference of analytic dl
  int viol2=0,checked2=0; double worst2=0,wl2=0,wz2=0;
  for(double t=-6;t<8;t+=0.05){
    const double z=std::exp(t);
    for(double l=-3;l<0.995;l+=0.005){
      const double h=1e-4;
      if(l+h>0 && z*(l+h)>=0.999) break;
      auto a=proto::eval(z,l-h), b=proto::eval(z,l+h); if(!a.ok||!b.ok) continue;
      const double d2=(b.dl-a.dl)/(2*h); ++checked2;
      if(d2>1e-6){++viol2; if(d2>worst2){worst2=d2;wl2=l;wz2=z;}}
    }
  }
  std::printf("(2) log g concave in lambda: checked %d, violations %d, worst d2 %.3g at l=%.3f z=%.3g\n",checked2,viol2,worst2,wl2,wz2);
}
