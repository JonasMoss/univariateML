#include "cppunivariate/distributions/betapr/mle.hpp"
#include "cppunivariate/distributions/ged/mle.hpp"
#include "cppunivariate/distributions/paralogis/mle.hpp"
#include "cppunivariate/distributions/invburr/mle.hpp"
#include "cppunivariate/distributions/pareto4/mle.hpp"
#include <iostream>
int main(){
 std::vector<double>x{.03,.1,.4,.7,1,2,4,12};cppunivariate::span s{x.data(),x.size()};
 for(int it:{16,64,1000}) {
 auto r=cppunivariate::betapr_mle(s,1e-14,it);std::cout<<"betapr "<<it<<" "<<int(r.status)<<" "<<r.certified<<" "<<r.alpha<<" "<<r.beta<<" "<<r.scale<<"\n";
 auto p=cppunivariate::paralogis_mle(s,1e-8,it);std::cout<<"paralogis "<<it<<" "<<int(p.status)<<" "<<p.certified<<"\n";
 }
 x={std::nextafter(1.,0.)};s={x.data(),x.size()};auto p=cppunivariate::paralogis_mle_fixed_scale(s,1,1e-10,1000);std::cout<<"paralogis next "<<int(p.status)<<" "<<p.certified<<" "<<p.shape<<"\n";
 x={-.8,0,0,0,.9};s={x.data(),x.size()};for(int it:{1,2,200})for(double sd:{double(NAN),.1}){auto r=cppunivariate::ged_mle(s,1e-10,it,NAN,sd,NAN);std::cout<<"ged short "<<it<<" "<<sd<<" "<<int(r.status)<<" "<<r.certified<<"\n";}
 x={.5,1,2};s={x.data(),x.size()};auto b=cppunivariate::betapr_mle(s);std::cout<<"betapr symmetric "<<int(b.status)<<" "<<b.certified<<" "<<b.alpha<<" "<<b.beta<<"\n";
}
