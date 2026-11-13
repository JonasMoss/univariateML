#include <iostream>
#include <iomanip>
#include <cmath>
#include "cppunivariate/distributions/rng_std.hpp"
#include "cppunivariate/distributions/pois/ztpois_dpqr.hpp"
#include "cppunivariate/distributions/pois/zmpois_dpqr.hpp"
#include "cppunivariate/distributions/pois/zip_dpqr.hpp"
#include "cppunivariate/distributions/pois/tpois_dpqr.hpp"
#include "cppunivariate/distributions/binom/ztbinom_dpqr.hpp"
#include "cppunivariate/distributions/binom/zmbinom_dpqr.hpp"
#include "cppunivariate/distributions/binom/zib_dpqr.hpp"
#include "cppunivariate/distributions/binom/tbinom_dpqr.hpp"
#include "cppunivariate/distributions/nbinom/ztnbinom_dpqr.hpp"
#include "cppunivariate/distributions/nbinom/zmnbinom_dpqr.hpp"
#include "cppunivariate/distributions/nbinom/zinb_dpqr.hpp"
#include "cppunivariate/distributions/geom/ztgeom_dpqr.hpp"
#include "cppunivariate/distributions/geom/zmgeom_dpqr.hpp"
#include "cppunivariate/distributions/lgser/zmlogarithmic_dpqr.hpp"
#include "cppunivariate/distributions/zeta/zipf_dpqr.hpp"
#include "cppunivariate/distributions/pois/dpqr.hpp"
#include "cppunivariate/distributions/binom/dpqr.hpp"
#include "cppunivariate/distributions/nbinom/dpqr.hpp"
#include "cppunivariate/distributions/geom/dpqr.hpp"
#include "cppunivariate/distributions/lgser/dpqr.hpp"
#include "cppunivariate/distributions/zeta/truncation.hpp"
using namespace cppunivariate::distributions;
template<class P,class Q> void probe(const char* id,int point,double centre,P prob,Q quant) {
 for(bool l:{true,false})for(bool g:{false,true}) {
 int n=0,up=0,down=0,bad=0,cross=0;
 const int width=std::string(id).find("-window-")!=std::string::npos?2:100;
 for(int j=-width;j<=width;++j){double k=centre+j,p=prob(k,l,g),prev=prob(k-1,l,g);
 if(!(g?std::isfinite(p)&&p<0:p>0&&p<1)||p==prev)continue;
 ++n;double q=quant(p,l,g);if(!std::isfinite(q)){++bad;continue;}if(q>k)++up;if(q<k)++down;
 if(cppunivariate::special::discrete_reached(prob(q,l,g),p,l,g)!=1 || cppunivariate::special::discrete_reached(prob(q-1,l,g),p,l,g)!=0)++cross;
 }
 std::cout<<id<<","<<point<<","<<l<<","<<g<<","<<n<<","<<up<<","<<down<<","<<bad<<","<<cross<<"\n";
 }}
int main(){StdRng<> rng(215);std::cout<<std::hexfloat;for(int j=0;j<512;++j) std::cout<<"ztpois,0,"<<rztpois(rng,2.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztpois,1,"<<rztpois(rng,2.008760837832246)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztpois,2,"<<rztpois(rng,0.2)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztpois,3,"<<rztpois(rng,20.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztpois,4,"<<rztpois(rng,1.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztpois,5,"<<rztpois(rng,0.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztpois,6,"<<rztpois(rng,1e-08)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztpois,7,"<<rztpois(rng,100000000.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmpois,0,"<<rzmpois(rng,2.0,0.3)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmpois,1,"<<rzmpois(rng,1.9832099451065273,0.306)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmpois,2,"<<rzmpois(rng,0.2,0.3)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmpois,3,"<<rzmpois(rng,20.0,0.3)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmpois,4,"<<rzmpois(rng,2.0,0.03)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmpois,5,"<<rzmpois(rng,2.0,0.93)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmpois,6,"<<rzmpois(rng,1.0,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmpois,7,"<<rzmpois(rng,0.0,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmpois,8,"<<rzmpois(rng,1.0,0.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmpois,9,"<<rzmpois(rng,1.0,1.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmpois,10,"<<rzmpois(rng,1e-08,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmpois,11,"<<rzmpois(rng,1.0,5e-09)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmpois,12,"<<rzmpois(rng,100000000.0,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zip,0,"<<rzip(rng,1.0,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zip,1,"<<rzip(rng,0.927085844817755,0.4671475109221307)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zip,2,"<<rzip(rng,0.1,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zip,3,"<<rzip(rng,10.0,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zip,4,"<<rzip(rng,1.0,0.05)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zip,5,"<<rzip(rng,1.0,0.95)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zip,6,"<<rzip(rng,1.0,0.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zip,7,"<<rzip(rng,1.0,1.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zip,8,"<<rzip(rng,1e-08,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zip,9,"<<rzip(rng,1.0,5e-09)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zip,10,"<<rzip(rng,100000000.0,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"tpois,0,"<<rtpois(rng,3.0,0.0,10.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"tpois,1,"<<rtpois(rng,2.9605593598095785,0.0,10.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"tpois,2,"<<rtpois(rng,0.3,0.0,10.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"tpois,3,"<<rtpois(rng,30.0,0.0,10.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"tpois,4,"<<rtpois(rng,1.0,0.0,10.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"tpois,5,"<<rtpois(rng,0.0,-1.0,0.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"tpois,6,"<<rtpois(rng,1e-08,0.0,10.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"tpois,7,"<<rtpois(rng,100000000.0,0.0,10.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztbinom,0,"<<rztbinom(rng,8.0,0.4)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztbinom,1,"<<rztbinom(rng,8.0,0.40988208060294373)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztbinom,2,"<<rztbinom(rng,8.0,0.04)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztbinom,3,"<<rztbinom(rng,8.0,0.94)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztbinom,4,"<<rztbinom(rng,10.0,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztbinom,5,"<<rztbinom(rng,1.0,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztbinom,6,"<<rztbinom(rng,10.0,0.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztbinom,7,"<<rztbinom(rng,10.0,1.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztbinom,8,"<<rztbinom(rng,10.0,5e-09)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztbinom,9,"<<rztbinom(rng,100000000.0,1e-08)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmbinom,0,"<<rzmbinom(rng,8.0,0.4,0.3)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmbinom,1,"<<rzmbinom(rng,8.0,0.41072177895283224,0.276)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmbinom,2,"<<rzmbinom(rng,8.0,0.04,0.3)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmbinom,3,"<<rzmbinom(rng,8.0,0.94,0.3)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmbinom,4,"<<rzmbinom(rng,8.0,0.4,0.03)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmbinom,5,"<<rzmbinom(rng,8.0,0.4,0.93)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmbinom,6,"<<rzmbinom(rng,10.0,0.5,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmbinom,7,"<<rzmbinom(rng,1.0,0.5,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmbinom,8,"<<rzmbinom(rng,10.0,0.0,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmbinom,9,"<<rzmbinom(rng,10.0,1.0,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmbinom,10,"<<rzmbinom(rng,10.0,0.5,0.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmbinom,11,"<<rzmbinom(rng,10.0,0.5,1.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmbinom,12,"<<rzmbinom(rng,10.0,5e-09,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmbinom,13,"<<rzmbinom(rng,10.0,0.5,5e-09)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmbinom,14,"<<rzmbinom(rng,100000000.0,1e-08,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zib,0,"<<rzib(rng,8.0,0.4,0.4)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zib,1,"<<rzib(rng,8.0,0.4180129724418685,0.3899710850828634)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zib,2,"<<rzib(rng,8.0,0.04,0.4)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zib,3,"<<rzib(rng,8.0,0.94,0.4)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zib,4,"<<rzib(rng,8.0,0.4,0.04)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zib,5,"<<rzib(rng,8.0,0.4,0.94)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zib,6,"<<rzib(rng,10.0,0.5,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zib,7,"<<rzib(rng,0.0,0.5,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zib,8,"<<rzib(rng,10.0,0.0,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zib,9,"<<rzib(rng,10.0,1.0,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zib,10,"<<rzib(rng,10.0,0.5,0.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zib,11,"<<rzib(rng,10.0,0.5,1.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zib,12,"<<rzib(rng,10.0,5e-09,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zib,13,"<<rzib(rng,10.0,0.5,5e-09)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zib,14,"<<rzib(rng,100000000.0,1e-08,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"tbinom,0,"<<rtbinom(rng,0.4,0.0,10.0,12.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"tbinom,1,"<<rtbinom(rng,0.4004693386746923,0.0,10.0,12.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"tbinom,2,"<<rtbinom(rng,0.04,0.0,10.0,12.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"tbinom,3,"<<rtbinom(rng,0.94,0.0,10.0,12.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"tbinom,4,"<<rtbinom(rng,0.5,0.0,10.0,10.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"tbinom,5,"<<rtbinom(rng,0.0,-1.0,10.0,10.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"tbinom,6,"<<rtbinom(rng,1.0,0.0,10.0,10.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"tbinom,7,"<<rtbinom(rng,0.5,-1.0,0.0,0.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"tbinom,8,"<<rtbinom(rng,5e-09,0.0,10.0,10.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"tbinom,9,"<<rtbinom(rng,1e-08,0.0,10.0,100000000.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"tbinom,10,"<<rtbinom(rng,2.2250738585072014e-308,-1.0,1.0,4.49423283715579e+307)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztnbinom,0,"<<rztnbinom(rng,2.0,0.4)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztnbinom,1,"<<rztnbinom(rng,2.1894042903107165,0.4269728320855304)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztnbinom,2,"<<rztnbinom(rng,0.2,0.4)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztnbinom,3,"<<rztnbinom(rng,20.0,0.4)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztnbinom,4,"<<rztnbinom(rng,2.0,0.04)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztnbinom,5,"<<rztnbinom(rng,2.0,0.94)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztnbinom,6,"<<rztnbinom(rng,1.0,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztnbinom,7,"<<rztnbinom(rng,0.0,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztnbinom,8,"<<rztnbinom(rng,1.0,1.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztnbinom,9,"<<rztnbinom(rng,1e-08,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztnbinom,10,"<<rztnbinom(rng,100000000.0,0.9999999900000001)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztnbinom,11,"<<rztnbinom(rng,100000000.0,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztnbinom,12,"<<rztnbinom(rng,1.0,5e-09)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmnbinom,0,"<<rzmnbinom(rng,2.0,0.4,0.3)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmnbinom,1,"<<rzmnbinom(rng,2.326926892246514,0.41499877689089204,0.314)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmnbinom,2,"<<rzmnbinom(rng,0.2,0.4,0.3)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmnbinom,3,"<<rzmnbinom(rng,20.0,0.4,0.3)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmnbinom,4,"<<rzmnbinom(rng,2.0,0.04,0.3)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmnbinom,5,"<<rzmnbinom(rng,2.0,0.94,0.3)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmnbinom,6,"<<rzmnbinom(rng,2.0,0.4,0.03)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmnbinom,7,"<<rzmnbinom(rng,2.0,0.4,0.93)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmnbinom,8,"<<rzmnbinom(rng,1.0,0.5,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmnbinom,9,"<<rzmnbinom(rng,0.0,0.5,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmnbinom,10,"<<rzmnbinom(rng,1.0,1.0,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmnbinom,11,"<<rzmnbinom(rng,1.0,0.5,0.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmnbinom,12,"<<rzmnbinom(rng,1.0,0.5,1.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmnbinom,13,"<<rzmnbinom(rng,1e-08,0.5,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmnbinom,14,"<<rzmnbinom(rng,1.0,0.5,5e-09)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmnbinom,15,"<<rzmnbinom(rng,100000000.0,0.9999999900000001,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmnbinom,16,"<<rzmnbinom(rng,100000000.0,0.5,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmnbinom,17,"<<rzmnbinom(rng,1.0,5e-09,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zinb,0,"<<rzinb(rng,2.0,0.4,0.3)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zinb,1,"<<rzinb(rng,1.5215075359537307,0.34428034505775573,0.2573876163521669)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zinb,2,"<<rzinb(rng,0.2,0.4,0.3)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zinb,3,"<<rzinb(rng,20.0,0.4,0.3)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zinb,4,"<<rzinb(rng,2.0,0.04,0.3)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zinb,5,"<<rzinb(rng,2.0,0.94,0.3)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zinb,6,"<<rzinb(rng,2.0,0.4,0.03)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zinb,7,"<<rzinb(rng,2.0,0.4,0.93)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zinb,8,"<<rzinb(rng,1.0,0.5,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zinb,9,"<<rzinb(rng,0.0,0.5,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zinb,10,"<<rzinb(rng,1.0,1.0,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zinb,11,"<<rzinb(rng,1.0,0.5,0.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zinb,12,"<<rzinb(rng,1.0,0.5,1.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zinb,13,"<<rzinb(rng,1e-08,0.5,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zinb,14,"<<rzinb(rng,1.0,0.5,5e-09)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zinb,15,"<<rzinb(rng,100000000.0,0.9999999900000001,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zinb,16,"<<rzinb(rng,100000000.0,0.5,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zinb,17,"<<rzinb(rng,1.0,5e-09,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztgeom,0,"<<rztgeom(rng,0.4)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztgeom,1,"<<rztgeom(rng,0.41254125412541254)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztgeom,2,"<<rztgeom(rng,0.04)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztgeom,3,"<<rztgeom(rng,0.94)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztgeom,4,"<<rztgeom(rng,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztgeom,5,"<<rztgeom(rng,1.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"ztgeom,6,"<<rztgeom(rng,5e-09)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmgeom,0,"<<rzmgeom(rng,0.4,0.3)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmgeom,1,"<<rzmgeom(rng,0.3892969569779643,0.258)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmgeom,2,"<<rzmgeom(rng,0.04,0.3)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmgeom,3,"<<rzmgeom(rng,0.94,0.3)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmgeom,4,"<<rzmgeom(rng,0.4,0.03)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmgeom,5,"<<rzmgeom(rng,0.4,0.93)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmgeom,6,"<<rzmgeom(rng,0.5,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmgeom,7,"<<rzmgeom(rng,1.0,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmgeom,8,"<<rzmgeom(rng,0.5,0.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmgeom,9,"<<rzmgeom(rng,0.5,1.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmgeom,10,"<<rzmgeom(rng,0.5,5e-09)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmgeom,11,"<<rzmgeom(rng,5e-09,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmlogarithmic,0,"<<rzmlogarithmic(rng,0.4,0.3)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmlogarithmic,1,"<<rzmlogarithmic(rng,0.3852620621412276,0.264)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmlogarithmic,2,"<<rzmlogarithmic(rng,0.04,0.3)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmlogarithmic,3,"<<rzmlogarithmic(rng,0.94,0.3)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmlogarithmic,4,"<<rzmlogarithmic(rng,0.4,0.03)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmlogarithmic,5,"<<rzmlogarithmic(rng,0.4,0.93)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmlogarithmic,6,"<<rzmlogarithmic(rng,0.5,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmlogarithmic,7,"<<rzmlogarithmic(rng,0.0,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmlogarithmic,8,"<<rzmlogarithmic(rng,0.5,0.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmlogarithmic,9,"<<rzmlogarithmic(rng,0.5,1.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmlogarithmic,10,"<<rzmlogarithmic(rng,5e-09,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zmlogarithmic,11,"<<rzmlogarithmic(rng,0.5,5e-09)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zipf,0,"<<rzipf(rng,3.0,0.5)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zipf,1,"<<rzipf(rng,3.0,0.3979626946090187)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zipf,2,"<<rzipf(rng,3.0,0.05)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zipf,3,"<<rzipf(rng,3.0,5.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zipf,4,"<<rzipf(rng,10.0,1.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zipf,5,"<<rzipf(rng,1.0,1.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zipf,6,"<<rzipf(rng,10.0,0.0)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zipf,7,"<<rzipf(rng,10.0,1e-08)<<"\n";
for(int j=0;j<512;++j) std::cout<<"zipf,8,"<<rzipf(rng,10.0,100000000.0)<<"\n";}
