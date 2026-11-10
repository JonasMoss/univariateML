#include "cppunivariate/distributions/genbeta/mle.hpp"
#include <iostream>
#include <iomanip>
int main(){std::cout<<std::setprecision(17);
 for(int sample=0;sample<3;++sample){std::vector<double> v=sample==0?std::vector<double>{.1,.3,1.,2.6}:sample==1?std::vector<double>{.5,1.5}:std::vector<double>{.01,.1,.5,3.39};
 double c=0;for(double w:v)c-=std::log(w)/v.size();
 auto at=[&](double h){cppunivariate::genbeta_allfree_cert::Point z;z.h=h;
  for(double w:v){const double e=std::exp(-h*w),den=-std::expm1(-h*w);
   z.r-=std::log(den)/v.size();z.d+=w*e/den/v.size();z.k+=w*w*e/(den*den)/v.size();}return z;};
 for(double h:{.03,.3,1.,3.,10.})for(double width:{.0001,.01,.2}){
 auto L=at(h*(1-width)),H=at(h*(1+width));
 const auto b=cppunivariate::genbeta_allfree_cert::enclose(L,H,c+1e-12,300);
 std::cout<<sample<<" "<<L.h<<" "<<H.h<<" "<<b.shape.enclosed<<" "<<b.shape.pl<<" "<<b.shape.ph<<" "<<b.shape.ql<<" "<<b.shape.qh<<" "<<b.smin<<" "<<b.smax<<" "<<b.curvature<<"\n";}}}
