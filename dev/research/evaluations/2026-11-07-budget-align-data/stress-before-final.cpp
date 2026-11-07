#include "json_loader.hpp"
#include <iostream>
#include <iomanip>
#include <chrono>
#include <random>
#include "common/distributions/bbinom/fit.hpp"
#include "common/distributions/betapr/fit.hpp"
#include "common/distributions/bnbinom/fit.hpp"
#include "common/distributions/burr/fit.hpp"
#include "common/distributions/cauchy/fit.hpp"
#include "common/distributions/cmb/fit.hpp"
#include "common/distributions/dgamma/fit.hpp"
#include "common/distributions/foldnorm/fit.hpp"
#include "common/distributions/fpareto/fit.hpp"
#include "common/distributions/gb2/fit.hpp"
#include "common/distributions/gpd/fit.hpp"
#include "common/distributions/invburr/fit.hpp"
#include "common/distributions/invparalogis/fit.hpp"
#include "common/distributions/invpareto/fit.hpp"
#include "common/distributions/kumar/fit.hpp"
#include "common/distributions/lomax/fit.hpp"
#include "common/distributions/lst/fit.hpp"
#include "common/distributions/nbinom/fit.hpp"
#include "common/distributions/paralogis/fit.hpp"
#include "common/distributions/pareto2/fit.hpp"
#include "common/distributions/pareto4/fit.hpp"
#include "common/distributions/slash/fit.hpp"
#include "common/distributions/sn/fit.hpp"
#include "common/distributions/std/fit.hpp"
#include "common/distributions/zinb/fit.hpp"
#include "common/distributions/zmnbinom/fit.hpp"
#include "common/distributions/ztnbinom/fit.hpp"
#include "common/distributions/zib/fit.hpp"
#include "common/distributions/ztbinom/fit.hpp"
int main(int argc,char** argv){std::string family=argv[1];int mode=std::stoi(argv[2]);std::mt19937 rng(143);std::cout<<std::setprecision(17);
if(family=="bbinom"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/bbinom/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
if(mode==1)v*=1+1e-4*std::normal_distribution<double>()(rng);
if(mode==2)v=(i%10==0?1.:1e-8)*v;
data.push_back(v);}
std::vector<int> integer;for(double v:data)integer.push_back(std::max(0,int(std::round(v))));cppunivariate::ispan x{integer.data(),integer.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_bbinom(x,umvl_binding::Held{(true && mode!=3),g.estimates.at("size")}, umvl_binding::Held{(false || mode==3),g.estimates.at("alpha")}, umvl_binding::Held{false,g.estimates.at("beta")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="betapr"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/betapr/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
if(mode==1)v*=1+1e-4*std::normal_distribution<double>()(rng);
if(mode==2)v=(i%10==0?1.:1e-8)*v;
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_betapr(x,umvl_binding::Held{(false || mode==3),(mode==3?1.:g.estimates.at("shape1"))}, umvl_binding::Held{false,g.estimates.at("shape2")}, umvl_binding::Held{(false || mode==3),g.estimates.at("scale")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="bnbinom"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/bnbinom/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
if(mode==1)v*=1+1e-4*std::normal_distribution<double>()(rng);
if(mode==2)v=(i%10==0?1.:1e-8)*v;
data.push_back(v);}
std::vector<int> integer;for(double v:data)integer.push_back(std::max(0,int(std::round(v))));cppunivariate::ispan x{integer.data(),integer.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_bnbinom(x,umvl_binding::Held{false,g.estimates.at("size")}, umvl_binding::Held{false,g.estimates.at("alpha")}, umvl_binding::Held{false,g.estimates.at("beta")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="burr"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/burr/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
if(mode==1)v*=1+1e-4*std::normal_distribution<double>()(rng);
if(mode==2)v=(i%10==0?1.:1e-8)*v;
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_burr(x,umvl_binding::Held{false,g.estimates.at("shape1")}, umvl_binding::Held{false,g.estimates.at("shape2")}, umvl_binding::Held{(false || mode==3),g.estimates.at("scale")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="cauchy"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/cauchy/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
if(mode==1)v*=1+1e-4*std::normal_distribution<double>()(rng);
if(mode==2)v=(i%10==0?1.:1e-8)*v;
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_cauchy(x,umvl_binding::Held{false,g.estimates.at("location")}, umvl_binding::Held{(false || mode==3),g.estimates.at("scale")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="cmb"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/cmb/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
if(mode==1)v*=1+1e-4*std::normal_distribution<double>()(rng);
if(mode==2)v=(i%10==0?1.:1e-8)*v;
data.push_back(v);}
std::vector<int> integer;for(double v:data)integer.push_back(std::max(0,int(std::round(v))));cppunivariate::ispan x{integer.data(),integer.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_cmb(x,umvl_binding::Held{(true && mode!=3),g.estimates.at("size")}, umvl_binding::Held{false,g.estimates.at("prob")}, umvl_binding::Held{(false || mode==3),g.estimates.at("nu")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="dgamma"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/dgamma/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
if(mode==1)v*=1+1e-4*std::normal_distribution<double>()(rng);
if(mode==2)v=(i%10==0?1.:1e-8)*v;
data.push_back(v);}
for(double& v:data)v=std::max(0.,std::round(v));
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_dgamma(x,umvl_binding::Held{false,g.estimates.at("shape")}, umvl_binding::Held{false,g.estimates.at("rate")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="foldnorm"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/foldnorm/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
if(mode==1)v*=1+1e-4*std::normal_distribution<double>()(rng);
if(mode==2)v=(i%10==0?1.:1e-8)*v;
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_foldnorm(x,umvl_binding::Held{(false || mode==3),g.estimates.at("mu")}, umvl_binding::Held{false,g.estimates.at("sigma")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="fpareto"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/fpareto/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
if(mode==1)v*=1+1e-4*std::normal_distribution<double>()(rng);
if(mode==2)v=(i%10==0?1.:1e-8)*v;
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_fpareto(x,umvl_binding::Held{false,g.estimates.at("shape1")}, umvl_binding::Held{false,g.estimates.at("shape2")}, umvl_binding::Held{(false || mode==3),(mode==3?1.:g.estimates.at("shape3"))}, umvl_binding::Held{(false || mode==3),g.estimates.at("scale")}, umvl_binding::Held{true,g.estimates.at("min")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="gb2"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/gb2/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
if(mode==1)v*=1+1e-4*std::normal_distribution<double>()(rng);
if(mode==2)v=(i%10==0?1.:1e-8)*v;
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_gb2(x,umvl_binding::Held{false,g.estimates.at("shape1")}, umvl_binding::Held{false,g.estimates.at("shape2")}, umvl_binding::Held{(false || mode==3),(mode==3?1.:g.estimates.at("shape3"))}, umvl_binding::Held{(false || mode==3),g.estimates.at("scale")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="gpd"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/gpd/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
if(mode==1)v*=1+1e-4*std::normal_distribution<double>()(rng);
if(mode==2)v=(i%10==0?1.:1e-8)*v;
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_gpd(x,umvl_binding::Held{true,g.estimates.at("mu")}, umvl_binding::Held{false,g.estimates.at("sigma")}, umvl_binding::Held{false,g.estimates.at("xi")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="invburr"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/invburr/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
if(mode==1)v*=1+1e-4*std::normal_distribution<double>()(rng);
if(mode==2)v=(i%10==0?1.:1e-8)*v;
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_invburr(x,umvl_binding::Held{false,g.estimates.at("shape1")}, umvl_binding::Held{false,g.estimates.at("shape2")}, umvl_binding::Held{(false || mode==3),g.estimates.at("scale")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="invparalogis"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/invparalogis/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
if(mode==1)v*=1+1e-4*std::normal_distribution<double>()(rng);
if(mode==2)v=(i%10==0?1.:1e-8)*v;
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_invparalogis(x,umvl_binding::Held{false,g.estimates.at("shape")}, umvl_binding::Held{(false || mode==3),g.estimates.at("scale")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="invpareto"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/invpareto/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
if(mode==1)v*=1+1e-4*std::normal_distribution<double>()(rng);
if(mode==2)v=(i%10==0?1.:1e-8)*v;
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_invpareto(x,umvl_binding::Held{false,g.estimates.at("shape")}, umvl_binding::Held{false,g.estimates.at("scale")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="kumar"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/kumar/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
if(mode==1)v*=1+1e-4*std::normal_distribution<double>()(rng);
if(mode==2)v=(i%10==0?1.:1e-8)*v;
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_kumar(x,umvl_binding::Held{false,g.estimates.at("a")}, umvl_binding::Held{(false || mode==3),g.estimates.at("b")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="lomax"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/lomax/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
if(mode==1)v*=1+1e-4*std::normal_distribution<double>()(rng);
if(mode==2)v=(i%10==0?1.:1e-8)*v;
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_lomax(x,umvl_binding::Held{false,g.estimates.at("lambda")}, umvl_binding::Held{false,g.estimates.at("kappa")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="lst"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/lst/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
if(mode==1)v*=1+1e-4*std::normal_distribution<double>()(rng);
if(mode==2)v=(i%10==0?1.:1e-8)*v;
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_lst(x,umvl_binding::Held{false,g.estimates.at("mu")}, umvl_binding::Held{false,g.estimates.at("sigma")}, umvl_binding::Held{false,g.estimates.at("df")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="nbinom"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/nbinom/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
if(mode==1)v*=1+1e-4*std::normal_distribution<double>()(rng);
if(mode==2)v=(i%10==0?1.:1e-8)*v;
data.push_back(v);}
std::vector<int> integer;for(double v:data)integer.push_back(std::max(0,int(std::round(v))));cppunivariate::ispan x{integer.data(),integer.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_nbinom(x,umvl_binding::Held{false,g.estimates.at("size")}, umvl_binding::Held{(false || mode==3),g.estimates.at("prob")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="paralogis"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/paralogis/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
if(mode==1)v*=1+1e-4*std::normal_distribution<double>()(rng);
if(mode==2)v=(i%10==0?1.:1e-8)*v;
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_paralogis(x,umvl_binding::Held{false,g.estimates.at("shape")}, umvl_binding::Held{(false || mode==3),g.estimates.at("scale")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="pareto2"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/pareto2/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
if(mode==1)v*=1+1e-4*std::normal_distribution<double>()(rng);
if(mode==2)v=(i%10==0?1.:1e-8)*v;
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_pareto2(x,umvl_binding::Held{false,g.estimates.at("shape")}, umvl_binding::Held{false,g.estimates.at("scale")}, umvl_binding::Held{true,g.estimates.at("min")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="pareto4"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/pareto4/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
if(mode==1)v*=1+1e-4*std::normal_distribution<double>()(rng);
if(mode==2)v=(i%10==0?1.:1e-8)*v;
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_pareto4(x,umvl_binding::Held{false,g.estimates.at("shape1")}, umvl_binding::Held{false,g.estimates.at("shape2")}, umvl_binding::Held{(false || mode==3),g.estimates.at("scale")}, umvl_binding::Held{true,g.estimates.at("min")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="slash"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/slash/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
if(mode==1)v*=1+1e-4*std::normal_distribution<double>()(rng);
if(mode==2)v=(i%10==0?1.:1e-8)*v;
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_slash(x,umvl_binding::Held{false,g.estimates.at("mu")}, umvl_binding::Held{false,g.estimates.at("sigma")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="sn"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/sn/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
if(mode==1)v*=1+1e-4*std::normal_distribution<double>()(rng);
if(mode==2)v=(i%10==0?1.:1e-8)*v;
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_sn(x,umvl_binding::Held{false,g.estimates.at("xi")}, umvl_binding::Held{(false || mode==3),g.estimates.at("omega")}, umvl_binding::Held{false,g.estimates.at("alpha")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="std"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/std/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
if(mode==1)v*=1+1e-4*std::normal_distribution<double>()(rng);
if(mode==2)v=(i%10==0?1.:1e-8)*v;
data.push_back(v);}
cppunivariate::span x{data.data(),data.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_std(x,umvl_binding::Held{false,g.estimates.at("mean")}, umvl_binding::Held{false,g.estimates.at("sd")}, umvl_binding::Held{false,g.estimates.at("nu")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="zinb"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/zinb/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
if(mode==1)v*=1+1e-4*std::normal_distribution<double>()(rng);
if(mode==2)v=(i%10==0?1.:1e-8)*v;
data.push_back(v);}
if(mode==2){for(int i=0;i<100000;++i){int j=i%10000;data[i]=j<3958?1:j<8021?2:4;}}
if(mode==2){for(int i=0;i<40000;++i)data[i]=0;}
std::vector<int> integer;for(double v:data)integer.push_back(std::max(0,int(std::round(v))));cppunivariate::ispan x{integer.data(),integer.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_zinb(x,umvl_binding::Held{false,g.estimates.at("size")}, umvl_binding::Held{false,g.estimates.at("prob")}, umvl_binding::Held{false,g.estimates.at("pi")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="zmnbinom"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/zmnbinom/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
if(mode==1)v*=1+1e-4*std::normal_distribution<double>()(rng);
if(mode==2)v=(i%10==0?1.:1e-8)*v;
data.push_back(v);}
if(mode==2){for(int i=0;i<100000;++i){int j=i%10000;data[i]=j<3958?1:j<8021?2:4;}}
if(mode==2){for(int i=0;i<40000;++i)data[i]=0;}
std::vector<int> integer;for(double v:data)integer.push_back(std::max(0,int(std::round(v))));cppunivariate::ispan x{integer.data(),integer.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_zmnbinom(x,umvl_binding::Held{false,g.estimates.at("size")}, umvl_binding::Held{false,g.estimates.at("prob")}, umvl_binding::Held{false,g.estimates.at("p0")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="ztnbinom"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/ztnbinom/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
if(mode==1)v*=1+1e-4*std::normal_distribution<double>()(rng);
if(mode==2)v=(i%10==0?1.:1e-8)*v;
data.push_back(v);}
if(mode==2){for(int i=0;i<100000;++i){int j=i%10000;data[i]=j<3958?1:j<8021?2:4;}}
for(double& v:data)v=std::max(1.,std::round(v));
std::vector<int> integer;for(double v:data)integer.push_back(std::max(0,int(std::round(v))));cppunivariate::ispan x{integer.data(),integer.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_ztnbinom(x,umvl_binding::Held{false,g.estimates.at("size")}, umvl_binding::Held{false,g.estimates.at("prob")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="zib"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/zib/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
if(mode==1)v*=1+1e-4*std::normal_distribution<double>()(rng);
if(mode==2)v=(i%10==0?1.:1e-8)*v;
data.push_back(v);}
std::vector<int> integer;for(double v:data)integer.push_back(std::max(0,int(std::round(v))));cppunivariate::ispan x{integer.data(),integer.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_zib(x,umvl_binding::Held{true,g.estimates.at("size")}, umvl_binding::Held{(false || mode==3),(mode==3?1e-6:g.estimates.at("prob"))}, umvl_binding::Held{(false || mode==3),g.estimates.at("pi")});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
if(family=="ztbinom"){
auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/ztbinom/golden.json");
std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];
if(mode==1)v*=1+1e-4*std::normal_distribution<double>()(rng);
if(mode==2)v=(i%10==0?1.:1e-8)*v;
data.push_back(v);}
for(double& v:data)v=std::max(1.,std::round(v));
std::vector<int> integer;for(double v:data)integer.push_back(std::max(0,int(std::round(v))));cppunivariate::ispan x{integer.data(),integer.size()};
auto start=std::chrono::steady_clock::now();
const auto fit=umvl_binding::fit_ztbinom(x,umvl_binding::Held{true,g.estimates.at("size")}, umvl_binding::Held{(false || mode==3),(mode==3?1e-6:g.estimates.at("prob"))});
std::cout<<family<<"\t"<<mode<<"\t"<<data.size()<<"\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\t"<<fit.error_kind<<"\t"<<fit.error<<std::endl;return 0;}
return 2;}
