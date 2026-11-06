#include "cppunivariate/distributions/ztnbinom/mle.hpp"
#include "cppunivariate/distributions/zmnbinom/mle.hpp"
#include "cppunivariate/distributions/zinb/mle.hpp"
#include <chrono>
#include <iostream>
#include <iomanip>
#include <string>
int main() {
 std::string family;int n;volatile double sink=0;
 while(std::cin>>family>>n) {
  std::vector<int>x(n),positive;for(auto&v:x){std::cin>>v;if(v>0)positive.push_back(v);}
  for(bool certified:{false,true}) {
   if(family=="zinb" && !certified)continue;
   const auto start=std::chrono::steady_clock::now();
   for(int j=0;j<100;++j) {
    if(family=="ztnbinom") {
     const auto f=certified?cppunivariate::ztnbinom_mle({x.data(),x.size()}):cppunivariate::ztnbinom_candidate_mle({x.data(),x.size()});
     if(f.status!=cppunivariate::ZtnbinomStatus::Ok)return 1;
     sink+=f.logLik;
    } else if(family=="zmnbinom") {
     if(certified) {
      const auto f=cppunivariate::zmnbinom_mle({x.data(),x.size()});
      if(f.status!=cppunivariate::ZmnbinomStatus::Ok)return 2;sink+=f.logLik;
     } else {
      // The historical delegate plus its unchanged zero-indicator factor.
      std::vector<int>p;for(int v:x)if(v>0)p.push_back(v);
      const auto f=cppunivariate::ztnbinom_candidate_mle({p.data(),p.size()});
      const double p0=1-double(p.size())/x.size();
      if(f.status!=cppunivariate::ZtnbinomStatus::Ok)return 3;
      sink+=f.logLik+(x.size()-p.size())*std::log(p0)+p.size()*std::log1p(-p0);
     }
    } else {
     const auto f=cppunivariate::zinb_mle({x.data(),x.size()});
     if(f.status!=cppunivariate::ZinbStatus::Ok)return 4;sink+=f.logLik;
    }
   }
   std::cout<<std::setprecision(10)<<family<<' '<<(family=="zinb"?"unchanged":certified?"certified":"proposer")<<' '<<n<<' '<<std::chrono::duration<double,std::micro>(std::chrono::steady_clock::now()-start).count()/100<<" us/fit\n";
  }
 }
}
