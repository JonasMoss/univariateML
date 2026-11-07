#include "cppunivariate/distributions/frechet/mle.hpp"
#include <chrono>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <sstream>
int main(){
 std::ifstream in("univariateML-cpp/include/cppunivariate/distributions/frechet/golden.json");
 std::string json((std::istreambuf_iterator<char>(in)),std::istreambuf_iterator<char>());
 auto start=json.find('[',json.find("\"data\"")),end=json.find(']',start);
 std::string array=json.substr(start+1,end-start-1);for(char& c:array)if(c==',')c=' ';
 std::istringstream numbers(array);std::vector<double> golden;double v;while(numbers>>v)golden.push_back(v);
 std::cout<<std::setprecision(17);
 for(const auto& x:{std::vector<double>{.2,.4,.7,1,2,4,10},golden}) {
  cppunivariate::span d{x.data(),x.size()};const int reps=x.size()<10?200:20;
  for(int mode=0;mode<5;++mode){auto t=std::chrono::steady_clock::now();
   for(int i=0;i<reps;++i){
    if(mode<3){auto r=cppunivariate::frechet_mle(d,1e-10,100,NAN,mode==0?NAN:2.,mode==2?1.:NAN);
     if(i==0)std::cout<<"n "<<x.size()<<" mode "<<mode<<" status "<<int(r.status)<<" coef "<<r.lambda<<" "<<r.mu<<" "<<r.sigma<<" ll "<<r.logLik<<"\n";
    }else{auto r=cppunivariate::gev_mle(d,1e-10,100,mode==3?-.3:NAN,NAN,mode==3?2.:NAN);
     if(i==0)std::cout<<"n "<<x.size()<<" mode "<<mode<<" status "<<int(r.status)<<" coef "<<r.mu<<" "<<r.sigma<<" "<<r.xi<<" ll "<<r.logLik<<"\n";
    }
   }
   std::cout<<"microseconds_per_fit "<<std::chrono::duration<double,std::micro>(std::chrono::steady_clock::now()-t).count()/reps<<"\n";
  }
 }
}
