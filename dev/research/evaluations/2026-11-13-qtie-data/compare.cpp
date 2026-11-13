#include <cmath>
#include <cstdio>
#include <random>
#include "cppunivariate/distributions/pois/dpqr.hpp"
#include "cppunivariate/distributions/binom/dpqr.hpp"
#include "cppunivariate/distributions/geom/dpqr.hpp"
#include "cppunivariate/distributions/nbinom/dpqr.hpp"
#include "cppunivariate/distributions/hyper/dpqr.hpp"
template<class P,class Q> void grid(const char* id,int lo,P cdf,Q quantile) {
 std::mt19937_64 rng(215);
 for(bool lower:{true,false}) for(bool log:{false,true}) {
  const auto emit=[&](double p,int k,int ulp) {
   if(!(log ? std::isfinite(p)&&p<0 : p>0&&p<1)) return;
   const double q=quantile(p,lower,log);
   const int at=cppunivariate::special::discrete_reached(cdf(q,lower,log),p,lower,log);
   const int before=cppunivariate::special::discrete_reached(cdf(q-1,lower,log),p,lower,log);
   std::printf("%s,%d,%d,%d,%d,%.17g,%.17g,%d,%d\n",id,lower,log,k,ulp,p,q,at,before);
  };
  for(int k=lo;k<=lo+20;++k) {
   const double p=cdf(k,lower,log);
   emit(p,k,0);
   double above=p,below=p;
   for(int j=1;j<=128;++j) {
    above=std::nextafter(above,INFINITY);below=std::nextafter(below,-INFINITY);
    emit(above,k,j);emit(below,k,-j);
   }
  }
  for(int j=0;j<1000;++j) {
   const double p=std::generate_canonical<double,53>(rng);
   emit(log?std::log(p):p,-1,0);
  }
 }
}
int main() {
 std::puts("family,lower,log,k,ulp,p,cpp,at,before");
 grid("pois",10,[](double k,bool l,bool g){return cppunivariate::distributions::ppois(k,20,l,g);},[](double p,bool l,bool g){return cppunivariate::distributions::qpois(p,20,l,g);});
 grid("binom",35,[](double k,bool l,bool g){return cppunivariate::distributions::pbinom(k,100,.5,l,g);},[](double p,bool l,bool g){return cppunivariate::distributions::qbinom(p,100,.5,l,g);});
 grid("geom",0,[](double k,bool l,bool g){return cppunivariate::distributions::pgeom(k,.25,l,g);},[](double p,bool l,bool g){return cppunivariate::distributions::qgeom(p,.25,l,g);});
 grid("nbinom",0,[](double k,bool l,bool g){return cppunivariate::distributions::pnbinom(k,10,.5,l,g);},[](double p,bool l,bool g){return cppunivariate::distributions::qnbinom(p,10,.5,l,g);});
 grid("hyper",0,[](double k,bool l,bool g){return cppunivariate::distributions::phyper(k,50,100,20,l,g);},[](double p,bool l,bool g){return cppunivariate::distributions::qhyper(p,50,100,20,l,g);});
}
