#include "cppunivariate/distributions/bhatt/mle.hpp"
#include <iostream>
#include <iomanip>
#include <chrono>
int main(){
 const std::vector<double> original{1.5186427188633,-.893644554187554,-1.08058198203889,-.639737926177478,.655034880479549,-2.35672046800855,2.46887599116556,.273853493748292,2.5355294429514,-.299436099194099,-.160890965931292,-.201888161544889,.6721053216777,-.344350831779183,1.51377727236596,.163578425435854,1.41905463032963,.838599300343392,-.0826733918168545,-1.34035410608697,-.343170308885225,.824451373169256,-.404339571078608,.0120556813316649};
 std::cout<<std::setprecision(17);
 for(double unit:{1.,1e-4,1e4}){
  auto x=original;for(double& v:x)v=unit*(v+3);
  auto start=std::chrono::steady_clock::now();auto f=cppunivariate::bhatt_mle({x.data(),x.size()});
  std::cout<<"unit "<<unit<<" status "<<int(f.status)<<" work "<<f.iterations<<" mu "<<f.mu<<" sigma "<<f.sigma<<" a "<<f.a<<" seconds "<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<std::endl;
 }
}
