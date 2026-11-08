#include "proto.hpp"
#include <cstdio>
int main(){
  // where does log g fail to be concave in lambda? tabulate by lambda band and by lambda*z (endpoint proximity)
  const double bands[]={-3,-2,-1,-0.5,0,0.25,0.5,0.75,1.0};
  for(int b=0;b<8;++b){
    int checked=0,viol=0; double minlz=1e9,maxz=0,minz=1e9;
    for(double t=-6;t<8;t+=0.05){ const double z=std::exp(t);
      for(double l=bands[b];l<bands[b+1];l+=0.005){
        const double h=1e-4; if(l+h>0 && z*(l+h)>=0.999) break;
        auto a=proto::eval(z,l-h), c=proto::eval(z,l+h); if(!a.ok||!c.ok) continue;
        const double d2=(c.dl-a.dl)/(2*h); ++checked;
        if(d2>1e-6){++viol; minlz=std::min(minlz,l*z); minz=std::min(minz,z); maxz=std::max(maxz,z);}
      }}
    std::printf("lambda in [%5.2f,%5.2f): checked %6d violations %6d  z range of violations [%.3g, %.3g] min l*z %.3g\n",bands[b],bands[b+1],checked,viol,minz,maxz,minlz);
  }
}
