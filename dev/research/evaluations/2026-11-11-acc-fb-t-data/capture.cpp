#include <fstream>
#include <iostream>
#include <iomanip>
#include "bindings/common/distributions/slash/fit.hpp"
#include "bindings/common/distributions/sstd/fit.hpp"
#include "bindings/common/distributions/sged/fit.hpp"
#include "bindings/common/distributions/st/fit.hpp"
#include "bindings/common/distributions/std/fit.hpp"
#include "bindings/common/distributions/lst/fit.hpp"
int main(int argc,char** argv){std::cout<<std::setprecision(17);std::ifstream in(argc>1?argv[1]:"dev/research/evaluations/2026-11-11-acc-fb-t-data/inputs.txt");const bool defaults=argc>1;int index,n;std::string family;while(in>>index>>family>>n){int count=family=="slash"?2:(family=="lst"||family=="std"?3:4);std::vector<umvl_binding::Held> h(count);for(auto& v:h){std::string token;in>>token;double q=std::stod(token);if(!std::isnan(q))v={true,q,1};}std::vector<double> x(n);for(auto& v:x)in>>v;umvl_binding::FitResult r;
if(family=="slash")r=defaults?umvl_binding::fit_slash({x.data(),x.size()},h[0],h[1]):umvl_binding::fit_slash({x.data(),x.size()},h[0],h[1],1e-10);
if(family=="sstd")r=defaults?umvl_binding::fit_sstd({x.data(),x.size()},h[0],h[1],h[2],h[3]):umvl_binding::fit_sstd({x.data(),x.size()},h[0],h[1],h[2],h[3],1e-10);
if(family=="sged")r=defaults?umvl_binding::fit_sged({x.data(),x.size()},h[0],h[1],h[2],h[3]):umvl_binding::fit_sged({x.data(),x.size()},h[0],h[1],h[2],h[3],1e-10);
if(family=="st")r=defaults?umvl_binding::fit_st({x.data(),x.size()},h[0],h[1],h[2],h[3]):umvl_binding::fit_st({x.data(),x.size()},h[0],h[1],h[2],h[3],1e-10);
if(family=="std")r=defaults?umvl_binding::fit_std({x.data(),x.size()},h[0],h[1],h[2]):umvl_binding::fit_std({x.data(),x.size()},h[0],h[1],h[2],1e-10);
if(family=="lst")r=defaults?umvl_binding::fit_lst({x.data(),x.size()},h[0],h[1],h[2]):umvl_binding::fit_lst({x.data(),x.size()},h[0],h[1],h[2],1e-10);
std::cout<<"{\"index\":"<<index<<",\"error\":\""<<r.error<<"\",\"estimates\":{ ";bool first=true;for(auto v:r.estimates){if(!first)std::cout<<",";first=false;std::cout<<"\""<<v.first<<"\":";if(std::isfinite(v.second))std::cout<<v.second;else std::cout<<"null";}std::cout<<"},\"logLik\":";if(std::isfinite(r.logLik))std::cout<<r.logLik;else std::cout<<"null";std::cout<<",\"iterations\":"<<r.iterations<<",\"certified\":";bool has=false;for(auto v:r.extra)if(v.first=="certified"){std::cout<<v.second;has=true;}if(!has)std::cout<<"null";std::cout<<"}"<<std::endl;}return in.eof()?0:2;}
