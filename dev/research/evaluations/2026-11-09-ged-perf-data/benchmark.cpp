#include <chrono>
#include <map>
#include <string>
#include <vector>
#include <iostream>
#include <iomanip>
#ifdef GED_PERF_PROFILE
namespace ged_perf {
struct Entry {long long calls=0; double milliseconds=0;};
inline std::map<std::string,Entry> entries;
inline std::map<std::string,long long> counters;
inline std::vector<std::pair<double,double>> cells;
struct Scope {
 std::string name; std::chrono::steady_clock::time_point start;
 Scope(std::string n):name(n),start(std::chrono::steady_clock::now()){++entries[name].calls;}
 ~Scope(){entries[name].milliseconds+=std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count();}
};
}
#endif
#include "cppunivariate/distributions/ged/mle.hpp"
#include "cppunivariate/distributions/ged/dpqr.hpp"
int main(int argc,char** argv){
 std::vector<double> golden;double v;while(std::cin>>v)golden.push_back(v);
 const int reps=argc>1?std::stoi(argv[1]):1;
 std::cout<<std::setprecision(17);
 for(int sample=0;sample<2;++sample){
  std::vector<double>x=golden;
  if(sample){x.clear();for(int j=0;j<10000;++j)x.push_back(cppunivariate::distributions::qged((j+.5)/10000.,0,1,3,true,false));}
  for(int mask=0;mask<4;++mask)for(int rep=0;rep<reps;++rep){
#ifdef GED_PERF_PROFILE
   ged_perf::entries.clear();ged_perf::cells.clear();ged_perf::counters.clear();
#endif
   auto start=std::chrono::steady_clock::now();
   const auto r=cppunivariate::ged_mle({x.data(),x.size()},1e-10,200,mask&1?(sample?0:-.017698887677594307):NAN,mask&2?(sample?1:1.0178559902092952):NAN,NAN);
   const double ms=std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count();
   std::cout<<"fit "<<x.size()<<" "<<mask<<" "<<rep<<" "<<int(r.status)<<" "<<r.certified<<" "<<r.mu<<" "<<r.sigma<<" "<<r.nu<<" "<<r.logLik<<" "<<r.iterations<<" "<<ms<<"\n";
#ifdef GED_PERF_PROFILE
   for(const auto& e:ged_perf::entries)std::cout<<"profile "<<x.size()<<" "<<mask<<" "<<e.first<<" "<<e.second.calls<<" "<<e.second.milliseconds<<"\n";
   for(const auto& c:ged_perf::counters)std::cout<<"counter "<<x.size()<<" "<<mask<<" "<<c.first<<" "<<c.second<<"\n";
   for(const auto& c:ged_perf::cells)std::cout<<"cell "<<x.size()<<" "<<mask<<" "<<c.first<<" "<<c.second<<"\n";
#endif
   std::cout.flush();
  }
 }
}
