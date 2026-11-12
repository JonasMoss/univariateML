#include "cppunivariate/distributions/cmb/mle.hpp"
#include "cppunivariate/distributions/dgamma/mle.hpp"
#include <iostream>
#include <csignal>
#include <cstdlib>
#include <unistd.h>
void finish(int){std::exit(0);}
int main(int argc,char**argv){std::signal(SIGALRM,finish);alarm(12);std::vector<int>x{1000000,1000001,1000003,1000008,1000011};int c=std::atoi(argv[1]);if(c==997){std::vector<double> y(x.begin(),x.end());auto r=cppunivariate::dgamma_mle({y.data(),y.size()});std::cout<<int(r.status)<<std::endl;}else {auto r=c==837?cppunivariate::cmb_size_free_nu_mle({x.data(),x.size()}):cppunivariate::cmb_size_mle({x.data(),x.size()},.2777362750324292,1.683599466457963);std::cout<<int(r.fit.status)<<std::endl;}}
