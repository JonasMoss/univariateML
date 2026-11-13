#include <cstdio>
#include "cppunivariate/distributions/pois/dpqr.hpp"
#include "cppunivariate/distributions/hyper/dpqr.hpp"
int main() {
 const double e=64*std::numeric_limits<double>::epsilon();
 const double pois=cppunivariate::special::discrete_search(0,INFINITY,[&](double k) {
  return int(cppunivariate::distributions::ppois(k,20,false,true)<=-.17021094871159989*(1-e));
 });
 const double hyper=cppunivariate::special::discrete_search(0,20,[&](double k) {
  return int(cppunivariate::distributions::phyper(k,50,100,20,false,true)<=-8.7930366634649135e-08*(1-e));
 });
 std::printf("pois,%.17g,%.17g,%.17g\n",cppunivariate::distributions::ppois(15,20,false,true),cppunivariate::distributions::qpois(-.17021094871159989,20,false,true),pois);
 std::printf("hyper,%.17g,%.17g,%.17g\n",cppunivariate::distributions::phyper(0,50,100,20,false,true),cppunivariate::distributions::qhyper(-8.7930366634649135e-08,50,100,20,false,true),hyper);
}
