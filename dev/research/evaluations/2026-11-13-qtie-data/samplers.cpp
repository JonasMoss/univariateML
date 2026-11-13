#include <cstdio>
#include <string>
#include <vector>
#include "test_support.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include "cppunivariate/distributions/cmp/dpqr.hpp"
#include "cppunivariate/distributions/cmb/dpqr.hpp"
#include "cppunivariate/distributions/dlaplace/dpqr.hpp"
#include "cppunivariate/distributions/hyper/dpqr.hpp"
#include "cppunivariate/distributions/nhyper/dpqr.hpp"
#include "cppunivariate/distributions/yulesimon/dpqr.hpp"
#include "cppunivariate/distributions/zeta/dpqr.hpp"
#include "cppunivariate/distributions/geom/dpqr.hpp"
#include "cppunivariate/distributions/bbinom/dpqr.hpp"
#include "cppunivariate/distributions/binom/dpqr.hpp"
#include "cppunivariate/distributions/dunif/dpqr.hpp"
#include "cppunivariate/distributions/dweibull/dpqr.hpp"
template<class R> void sample(const std::string &id,R draw) {
 const auto text=mltest::detail::slurp(std::string(CPPUNIV_DISTRIBUTIONS_DIR)+"/"+id+"/truth.json");
 auto params=mltest::truth_matrix(text,"parameters");
 auto selected=mltest::truth_matrix(text,"historical_sampler_points");
 if(id=="dlaplace" || id=="nhyper" || id=="binom") selected.push_back({static_cast<double>(params.size()-1)});
 if(id=="geom") {params.push_back({1e-17});selected.push_back({static_cast<double>(params.size()-1)});}
 if(id=="dunif") {
  params.push_back({0,1e20});selected.push_back({static_cast<double>(params.size()-1)});
  params.push_back({1e308,std::numeric_limits<double>::max()});selected.push_back({static_cast<double>(params.size()-1)});
 }
 if(id=="dweibull") {params.push_back({.9,.05});selected.push_back({static_cast<double>(params.size()-1)});}
 for(const auto &row:selected) {
  const std::size_t i=static_cast<std::size_t>(row[0]);
  cppunivariate::distributions::StdRng<> rng(20261029+i);
  for(int j=0;j<512;++j) std::printf("%s,%zu,%d,%.17g\n",id.c_str(),i,j,draw(rng,params[i]));
 }
}
int main() {
 sample("binom",[](auto &r,const auto &t){return cppunivariate::distributions::rbinom(r,t[0],t[1]);});
 sample("bbinom",[](auto &r,const auto &t){return cppunivariate::distributions::rbbinom(r,t[0],t[1],t[2]);});
 sample("geom",[](auto &r,const auto &t){return cppunivariate::distributions::rgeom(r,t[0]);});
 sample("dunif",[](auto &r,const auto &t){return cppunivariate::distributions::rdunif(r,t[0],t[1]);});
 sample("dweibull",[](auto &r,const auto &t){return cppunivariate::distributions::rdweibull(r,t[0],t[1]);});
 sample("cmp",[](auto &r,const auto &t){return cppunivariate::distributions::rcmp(r,t[0],t[1]);});
 sample("cmb",[](auto &r,const auto &t){return cppunivariate::distributions::rcmb(r,t[0],t[1],t[2]);});
 sample("dlaplace",[](auto &r,const auto &t){return cppunivariate::distributions::rdlaplace(r,t[0],t[1]);});
 sample("hyper",[](auto &r,const auto &t){return cppunivariate::distributions::rhyper(r,t[0],t[1],t[2]);});
 sample("nhyper",[](auto &r,const auto &t){return cppunivariate::distributions::rnhyper(r,t[0],t[1],t[2]);});
 sample("yulesimon",[](auto &r,const auto &t){return cppunivariate::distributions::ryulesimon(r,t[0]);});
 sample("zeta",[](auto &r,const auto &t){return cppunivariate::distributions::rzeta(r,t[0]);});
}
