#include "json_loader.hpp"
#include <iostream>
#include <iomanip>
#include <chrono>
#include <random>
#include "common/distributions/beta/fit.hpp"
#include "common/distributions/bhatt/fit.hpp"
#include "common/distributions/binom/fit.hpp"
#include "common/distributions/dnorm/fit.hpp"
#include "common/distributions/dweibull/fit.hpp"
#include "common/distributions/emg/fit.hpp"
#include "common/distributions/fatigue/fit.hpp"
#include "common/distributions/frechet/fit.hpp"
#include "common/distributions/gamma/fit.hpp"
#include "common/distributions/ged/fit.hpp"
#include "common/distributions/genbeta/fit.hpp"
#include "common/distributions/gev/fit.hpp"
#include "common/distributions/gompertz/fit.hpp"
#include "common/distributions/gumbel/fit.hpp"
#include "common/distributions/huber/fit.hpp"
#include "common/distributions/invgamma/fit.hpp"
#include "common/distributions/invweibull/fit.hpp"
#include "common/distributions/lgamma/fit.hpp"
#include "common/distributions/llogis/fit.hpp"
#include "common/distributions/logis/fit.hpp"
#include "common/distributions/naka/fit.hpp"
#include "common/distributions/nsbeta/fit.hpp"
#include "common/distributions/pareto3/fit.hpp"
#include "common/distributions/pig/fit.hpp"
#include "common/distributions/sged/fit.hpp"
#include "common/distributions/sgomp/fit.hpp"
#include "common/distributions/sstd/fit.hpp"
#include "common/distributions/st/fit.hpp"
#include "common/distributions/tlambda/fit.hpp"
#include "common/distributions/tnorm/fit.hpp"
#include "common/distributions/triang/fit.hpp"
#include "common/distributions/weibull/fit.hpp"
#include "common/distributions/yulesimon/fit.hpp"
#include "common/distributions/zeta/fit.hpp"
#include "common/distributions/zipf/fit.hpp"
#include "common/distributions/zmbinom/fit.hpp"
int main(int argc,char** argv){std::string family=argv[1];int mode=std::stoi(argv[2]);std::mt19937 rng(143);std::cout<<std::setprecision(17);
if(family=="beta"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/beta/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_beta(x,umvl_binding::Held{(mode==3),g.estimates.at("shape1")}, umvl_binding::Held{false,g.estimates.at("shape2")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="bhatt"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/bhatt/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_bhatt(x,umvl_binding::Held{(mode==3),g.estimates.at("mu")}, umvl_binding::Held{false,g.estimates.at("sigma")}, umvl_binding::Held{false,g.estimates.at("a")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="binom"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/binom/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
std::vector<int> integer;for(double v:data)integer.push_back(std::max(0,int(std::round(v))));cppunivariate::ispan x{integer.data(),integer.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_binom(x,umvl_binding::Held{true,g.estimates.at("size")}, umvl_binding::Held{false,g.estimates.at("prob")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="dnorm"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/dnorm/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
for(double& v:data)v=std::round(v);
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_dnorm(x,umvl_binding::Held{(mode==3),g.estimates.at("mean")}, umvl_binding::Held{false,g.estimates.at("sd")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="dweibull"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/dweibull/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
for(double& v:data)v=std::round(v);
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_dweibull(x,umvl_binding::Held{(mode==3),g.estimates.at("shape1")}, umvl_binding::Held{false,g.estimates.at("shape2")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="emg"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/emg/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_emg(x,umvl_binding::Held{(mode==3),g.estimates.at("mu")}, umvl_binding::Held{false,g.estimates.at("sigma")}, umvl_binding::Held{false,g.estimates.at("lambda")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="fatigue"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/fatigue/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_fatigue(x,umvl_binding::Held{(mode==3),g.estimates.at("alpha")}, umvl_binding::Held{false,g.estimates.at("beta")}, umvl_binding::Held{true,g.estimates.at("mu")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="frechet"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/frechet/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_frechet(x,umvl_binding::Held{(mode==3),g.estimates.at("lambda")}, umvl_binding::Held{true,g.estimates.at("mu")}, umvl_binding::Held{false,g.estimates.at("sigma")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="gamma"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/gamma/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_gamma(x,umvl_binding::Held{(mode==3),g.estimates.at("shape")}, umvl_binding::Held{false,g.estimates.at("scale")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="ged"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/ged/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_ged(x,umvl_binding::Held{(mode==3),g.estimates.at("mean")}, umvl_binding::Held{false,g.estimates.at("sd")}, umvl_binding::Held{false,g.estimates.at("nu")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="genbeta"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/genbeta/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_genbeta(x,umvl_binding::Held{(mode==3),g.estimates.at("shape1")}, umvl_binding::Held{false,g.estimates.at("shape2")}, umvl_binding::Held{false,g.estimates.at("shape3")}, umvl_binding::Held{true,g.estimates.at("scale")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="gev"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/gev/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_gev(x,umvl_binding::Held{(mode==3),g.estimates.at("mu")}, umvl_binding::Held{false,g.estimates.at("sigma")}, umvl_binding::Held{false,g.estimates.at("xi")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="gompertz"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/gompertz/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_gompertz(x,umvl_binding::Held{(mode==3),g.estimates.at("a")}, umvl_binding::Held{false,g.estimates.at("b")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="gumbel"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/gumbel/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_gumbel(x,umvl_binding::Held{(mode==3),g.estimates.at("mu")}, umvl_binding::Held{false,g.estimates.at("sigma")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="huber"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/huber/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_huber(x,umvl_binding::Held{(mode==3),g.estimates.at("mu")}, umvl_binding::Held{false,g.estimates.at("sigma")}, umvl_binding::Held{true,g.estimates.at("epsilon")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="invgamma"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/invgamma/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_invgamma(x,umvl_binding::Held{(mode==3),g.estimates.at("alpha")}, umvl_binding::Held{false,g.estimates.at("beta")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="invweibull"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/invweibull/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_invweibull(x,umvl_binding::Held{(mode==3),g.estimates.at("shape")}, umvl_binding::Held{false,g.estimates.at("scale")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="lgamma"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/lgamma/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_lgamma(x,umvl_binding::Held{(mode==3),g.estimates.at("shapelog")}, umvl_binding::Held{false,g.estimates.at("ratelog")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="llogis"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/llogis/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_llogis(x,umvl_binding::Held{(mode==3),g.estimates.at("shape")}, umvl_binding::Held{false,g.estimates.at("scale")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="logis"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/logis/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_logis(x,umvl_binding::Held{(mode==3),g.estimates.at("location")}, umvl_binding::Held{false,g.estimates.at("scale")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="naka"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/naka/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_naka(x,umvl_binding::Held{(mode==3),g.estimates.at("shape")}, umvl_binding::Held{false,g.estimates.at("scale")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="nsbeta"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/nsbeta/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_nsbeta(x,umvl_binding::Held{(mode==3),g.estimates.at("shape1")}, umvl_binding::Held{false,g.estimates.at("shape2")}, umvl_binding::Held{true,g.estimates.at("min")}, umvl_binding::Held{true,g.estimates.at("max")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="pareto3"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/pareto3/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_pareto3(x,umvl_binding::Held{(mode==3),g.estimates.at("shape")}, umvl_binding::Held{false,g.estimates.at("scale")}, umvl_binding::Held{true,g.estimates.at("min")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="pig"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/pig/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
std::vector<int> integer;for(double v:data)integer.push_back(std::max(0,int(std::round(v))));cppunivariate::ispan x{integer.data(),integer.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_pig(x,umvl_binding::Held{(mode==3),g.estimates.at("mean")}, umvl_binding::Held{false,g.estimates.at("shape")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="sged"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/sged/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_sged(x,umvl_binding::Held{(mode==3),g.estimates.at("mean")}, umvl_binding::Held{false,g.estimates.at("sd")}, umvl_binding::Held{false,g.estimates.at("nu")}, umvl_binding::Held{false,g.estimates.at("xi")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="sgomp"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/sgomp/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_sgomp(x,umvl_binding::Held{(mode==3),g.estimates.at("b")}, umvl_binding::Held{false,g.estimates.at("eta")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="sstd"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/sstd/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_sstd(x,umvl_binding::Held{(mode==3),g.estimates.at("mean")}, umvl_binding::Held{false,g.estimates.at("sd")}, umvl_binding::Held{false,g.estimates.at("nu")}, umvl_binding::Held{false,g.estimates.at("xi")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="st"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/st/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_st(x,umvl_binding::Held{(mode==3),g.estimates.at("xi")}, umvl_binding::Held{false,g.estimates.at("omega")}, umvl_binding::Held{false,g.estimates.at("alpha")}, umvl_binding::Held{false,g.estimates.at("nu")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="tlambda"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/tlambda/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_tlambda(x,umvl_binding::Held{(mode==3),g.estimates.at("mu")}, umvl_binding::Held{false,g.estimates.at("sigma")}, umvl_binding::Held{false,g.estimates.at("lambda")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="tnorm"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/tnorm/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_tnorm(x,umvl_binding::Held{(mode==3),g.estimates.at("mean")}, umvl_binding::Held{false,g.estimates.at("sd")}, umvl_binding::Held{true,g.estimates.at("a")}, umvl_binding::Held{true,g.estimates.at("b")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="triang"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/triang/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_triang(x,umvl_binding::Held{(mode==3),g.estimates.at("a")}, umvl_binding::Held{false,g.estimates.at("b")}, umvl_binding::Held{false,g.estimates.at("c")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="weibull"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/weibull/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_weibull(x,umvl_binding::Held{(mode==3),g.estimates.at("shape")}, umvl_binding::Held{false,g.estimates.at("scale")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="yulesimon"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/yulesimon/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
for(double& v:data)v=std::max(1.,std::round(v));
std::vector<int> integer;for(double v:data)integer.push_back(std::max(0,int(std::round(v))));cppunivariate::ispan x{integer.data(),integer.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_yulesimon(x,umvl_binding::Held{(mode==3),g.estimates.at("rho")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="zeta"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/zeta/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
for(double& v:data)v=std::max(1.,std::round(v));
std::vector<int> integer;for(double v:data)integer.push_back(std::max(0,int(std::round(v))));cppunivariate::ispan x{integer.data(),integer.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_zeta(x,umvl_binding::Held{(mode==3),g.estimates.at("s")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="zipf"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/zipf/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
for(double& v:data)v=std::max(1.,std::round(v));
std::vector<int> integer;for(double v:data)integer.push_back(std::max(0,int(std::round(v))));cppunivariate::ispan x{integer.data(),integer.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_zipf(x,umvl_binding::Held{(mode==3),g.estimates.at("N")}, umvl_binding::Held{false,g.estimates.at("s")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="zmbinom"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/zmbinom/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
const double lo=*std::min_element(g.data.begin(),g.data.end()), hi=*std::max_element(g.data.begin(),g.data.end());
if(mode==1)v=std::max(lo,std::min(hi,v*(1+1e-4*std::normal_distribution<double>()(rng))));
if(mode==2 && i%10!=0)v=lo+1e-8*(v-lo);
data.push_back(v);}
std::vector<int> integer;for(double v:data)integer.push_back(std::max(0,int(std::round(v))));cppunivariate::ispan x{integer.data(),integer.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_zmbinom(x,umvl_binding::Held{true,g.estimates.at("size")}, umvl_binding::Held{false,g.estimates.at("prob")}, umvl_binding::Held{false,g.estimates.at("p0")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
return 2;}
