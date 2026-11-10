#include "cppunivariate/distributions/genbeta/mle.hpp"
#include <iostream>
#include <iomanip>
int main(){std::cout<<std::setprecision(17);
 for(int sample=0;sample<3;++sample){std::vector<double> v=sample==0?std::vector<double>{.1,.3,1.,2.6}:sample==1?std::vector<double>{.5,1.5}:std::vector<double>{.01,.1,.5,3.39};
 double c=0;for(double w:v)c-=std::log(w)/v.size();
 for(double h:{.3,1.,10.,100.,1000.,10000.})for(double width:{.0001,.01,.2}){
  const auto L=cppunivariate::genbeta_power_face_cert::stats_sorted(v,h*(1-width)),H=cppunivariate::genbeta_power_face_cert::stats_sorted(v,h*(1+width));
  const double gu=cppunivariate::genbeta_power_face_cert::gamma_upper(L,H,1,300),
    bu=cppunivariate::genbeta_power_face_cert::beta_upper(L,H,1,c,300),
    pad=512*std::numeric_limits<double>::epsilon()*(1+H.h+L.r);
  const auto b=cppunivariate::genbeta_allfree_bounds::rectangle(L.h-pad,H.h+pad,H.r-pad,L.r+pad,c+pad,300);
  std::cout<<sample<<" "<<L.h<<" "<<H.h<<" "<<gu<<" "<<bu<<" "<<b.pl<<" "<<b.ph<<" "<<b.ql<<" "<<b.qh<<"\n";
 }}}
