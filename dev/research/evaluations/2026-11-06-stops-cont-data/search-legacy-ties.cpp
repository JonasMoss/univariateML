#include "bindings/common/distributions/emg/fit.hpp"
#include "bindings/common/distributions/huber/fit.hpp"
#include <iostream>
#include <iomanip>
int main(){
  std::cout<<std::setprecision(17);
  for(int family:{0,1}){
    double lo=family?1:100,hi=family?8:1e6;bool found=false;
    for(int i=0;i<100;++i){
      const double t=(lo+hi)/2;
      const std::vector<double> x=family?std::vector<double>{0,0,t}:std::vector<double>{1,2};
      const auto r=family?umvl_binding::fit_huber({x.data(),x.size()},{true,0},{true,1}):umvl_binding::fit_emg({x.data(),x.size()},{true,0},{true,t});
      if(r.error_kind=="tie") {std::cout<<(family?"huber":"emg")<<" tie "<<t<<" "<<r.candidates.size()<<"\n";found=true;break;}
      const bool normal=r.error_kind=="limit"&&r.model=="norm";
      if(family?normal:!normal)lo=t;else hi=t;
      if(t==lo&&hi==lo)break;
    }
    if(!found)std::cout<<(family?"huber":"emg")<<" no-trigger "<<lo<<" "<<hi<<"\n";
  }
}
