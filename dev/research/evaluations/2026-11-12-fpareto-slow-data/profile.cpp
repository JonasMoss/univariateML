#include "profile-fpareto.hpp"
#include <iostream>
#include <iomanip>
#include <cstdlib>
#include <csignal>
#include <unistd.h>
int main(int argc,char**argv){std::signal(SIGALRM,[](int){std::exit(0);});alarm(8);int id=argc>1?std::atoi(argv[1]):1599;if(id<0||id==1599){std::vector<double>x{9.341081677852826e-151,4.595475734747349e-150,5.998711847579505e-151,1.056419269543656e-149,7.000746031939794e-151,1.112552433763363e-150,1.3300121914598085e-148,3.057685931487713e-151,9.471857714817827e-151,3.8097695246334955e-150,1.8225442484990885e-150,1.1865419401405472e-151};auto r=cppunivariate::fpareto_mle(cppunivariate::span{x.data(),x.size()},0.0,1e-7,1000,NAN,NAN,NAN,NAN);std::cout<<1599<<" "<<int(r.status)<<" "<<r.certified<<" "<<r.shape1<<" "<<r.shape2<<" "<<r.shape3<<" "<<r.scale<<" "<<r.logLik<<" "<<r.iterations<<std::endl;}
if(id<0||id==1600){std::vector<double>x{9.341081677852825e+149,4.5954757347473486e+150,5.998711847579505e+149,1.056419269543656e+151,7.000746031939793e+149,1.1125524337633629e+150,1.3300121914598085e+152,3.057685931487713e+149,9.471857714817827e+149,3.809769524633495e+150,1.8225442484990886e+150,1.1865419401405473e+149};auto r=cppunivariate::fpareto_mle(cppunivariate::span{x.data(),x.size()},0.0,1e-7,1000,NAN,NAN,NAN,NAN);std::cout<<1600<<" "<<int(r.status)<<" "<<r.certified<<" "<<r.shape1<<" "<<r.shape2<<" "<<r.shape3<<" "<<r.scale<<" "<<r.logLik<<" "<<r.iterations<<std::endl;}
if(id<0||id==10717){std::vector<double>x{1e-200,2e-200,3e-200,5e-200,9e-200};auto r=cppunivariate::fpareto_mle(cppunivariate::span{x.data(),x.size()},0.0,1e-7,1000,NAN,NAN,NAN,NAN);std::cout<<10717<<" "<<int(r.status)<<" "<<r.certified<<" "<<r.shape1<<" "<<r.shape2<<" "<<r.shape3<<" "<<r.scale<<" "<<r.logLik<<" "<<r.iterations<<std::endl;}}
