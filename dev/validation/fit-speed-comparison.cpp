// Deterministic old/new driver; held estimates frozen 2026-10-31.
// Args: family, mode (0: golden timing, 1: seeded equivalence, 2: TASK-120 size,
// 3: twenty n=1500 golden bootstraps), optional held mask and sample count.
#include <cppunivariate/cppunivariate.hpp>
#include "../../univariateML-cpp/tests/json_loader.hpp"
#include <chrono>
#include <iostream>
#include <iomanip>
#include <random>
#include <string>
int main(int argc,char**argv){
if(argc<3)return 2;
const std::string id=argv[1]; const int mode=std::stoi(argv[2]);
const double nan=std::numeric_limits<double>::quiet_NaN();
std::cout<<std::setprecision(17);
std::mt19937_64 rng(13320261031ULL);std::normal_distribution<double> norm;
if(id=="tlambda"){
const std::vector<double> golden=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/"+id+"/golden.json").data;
const std::vector<double> held_values={0.184478447547003,0.04752470610558923,1.0988151244928166};
const unsigned subsets=1u<<3;
for(int sample=0;sample<(mode==1?(argc>4?std::stoi(argv[4]):620):(mode==3?20:1));++sample){
int n=sample<600?(sample/200==0?8:sample/200==1?16:32):500;
std::vector<double> x=golden;
if(mode==1){x.clear();for(int j=0;j<n;++j){double v=norm(rng);x.push_back(v+(sample%5==0?0.1*v*v:0));}}
if(mode==2){x.clear();for(int j=0;j<1000;++j)x.push_back(golden[j%golden.size()]);}
if(mode==3){x.clear();for(int j=0;j<1500;++j)x.push_back(golden[rng()%golden.size()]);}
for(unsigned mask=0;mask<subsets;++mask){
if(argc>3 && mask!=static_cast<unsigned>(std::stoi(argv[3])))continue;
if((mode==2 || mode==3) && mask!=0)continue;
const auto begin=std::chrono::steady_clock::now();
const auto r=cppunivariate::tlambda_mle({x.data(),x.size()},1e-8,1000,(mask&1?held_values[0]:nan),(mask&2?held_values[1]:nan),(mask&4?held_values[2]:nan));
const double seconds=std::chrono::duration<double>(std::chrono::steady_clock::now()-begin).count();
std::cout<<id<<" "<<sample<<" "<<mask<<" "<<static_cast<int>(r.status)<<" "<<r.lambda<<" "<<r.mu<<" "<<r.sigma<<" "<<r.logLik; if(mode!=1 && mode!=3)std::cout<<" "<<seconds;std::cout<<std::endl;
}
}
}
if(id=="emg"){
const std::vector<double> golden=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/"+id+"/golden.json").data;
const std::vector<double> held_values={0.17576157248036894,1.0518147225573364,1.209185930245896};
const unsigned subsets=1u<<3;
for(int sample=0;sample<(mode==1?(argc>4?std::stoi(argv[4]):620):(mode==3?20:1));++sample){
int n=sample<600?(sample/200==0?8:sample/200==1?16:32):500;
std::vector<double> x=golden;
if(mode==1){x.clear();for(int j=0;j<n;++j){double v=norm(rng);x.push_back(v+(sample%5==0?0.1*v*v:0));}}
if(mode==2){x.clear();for(int j=0;j<1000;++j)x.push_back(golden[j%golden.size()]);}
if(mode==3){x.clear();for(int j=0;j<1500;++j)x.push_back(golden[rng()%golden.size()]);}
for(unsigned mask=0;mask<subsets;++mask){
if(argc>3 && mask!=static_cast<unsigned>(std::stoi(argv[3])))continue;
if((mode==2 || mode==3) && mask!=0)continue;
const auto begin=std::chrono::steady_clock::now();
const auto r=cppunivariate::emg_mle({x.data(),x.size()},1e-8,1000,(mask&1?held_values[0]:nan),(mask&2?held_values[1]:nan),(mask&4?held_values[2]:nan));
const double seconds=std::chrono::duration<double>(std::chrono::steady_clock::now()-begin).count();
std::cout<<id<<" "<<sample<<" "<<mask<<" "<<static_cast<int>(r.status)<<" "<<r.mu<<" "<<r.sigma<<" "<<r.lambda<<" "<<r.logLik; if(mode!=1 && mode!=3)std::cout<<" "<<seconds;std::cout<<std::endl;
}
}
}
if(id=="bhatt"){
const std::vector<double> golden=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/"+id+"/golden.json").data;
const std::vector<double> held_values={1.057897350270679,1.7336058787117143,-0.0038000682197418395};
const unsigned subsets=1u<<3;
for(int sample=0;sample<(mode==1?(argc>4?std::stoi(argv[4]):620):(mode==3?20:1));++sample){
int n=sample<600?(sample/200==0?8:sample/200==1?16:32):500;
std::vector<double> x=golden;
if(mode==1){x.clear();for(int j=0;j<n;++j){double v=norm(rng);x.push_back(v+(sample%5==0?0.1*v*v:0));}}
if(mode==2){x.clear();for(int j=0;j<1000;++j)x.push_back(golden[j%golden.size()]);}
if(mode==3){x.clear();for(int j=0;j<1500;++j)x.push_back(golden[rng()%golden.size()]);}
for(unsigned mask=0;mask<subsets;++mask){
if(argc>3 && mask!=static_cast<unsigned>(std::stoi(argv[3])))continue;
if((mode==2 || mode==3) && mask!=0)continue;
const auto begin=std::chrono::steady_clock::now();
const auto r=cppunivariate::bhatt_mle({x.data(),x.size()},1e-8,1000,(mask&1?held_values[0]:nan),(mask&2?held_values[1]:nan),(mask&4?held_values[2]:nan));
const double seconds=std::chrono::duration<double>(std::chrono::steady_clock::now()-begin).count();
std::cout<<id<<" "<<sample<<" "<<mask<<" "<<static_cast<int>(r.status)<<" "<<r.sigma<<" "<<r.a<<" "<<r.mu<<" "<<r.logLik; if(mode!=1 && mode!=3)std::cout<<" "<<seconds;std::cout<<std::endl;
}
}
}
if(id=="sstd"){
const std::vector<double> golden=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/"+id+"/golden.json").data;
const std::vector<double> held_values={0.012734271748561354,1.0184483076867987,2.9622019441967624,1.9114750836827714};
const unsigned subsets=1u<<4;
for(int sample=0;sample<(mode==1?(argc>4?std::stoi(argv[4]):620):(mode==3?20:1));++sample){
int n=sample<600?(sample/200==0?8:sample/200==1?16:32):500;
std::vector<double> x=golden;
if(mode==1){x.clear();for(int j=0;j<n;++j){double v=norm(rng);x.push_back(v+(sample%5==0?0.1*v*v:0));}}
if(mode==2){x.clear();for(int j=0;j<1000;++j)x.push_back(golden[j%golden.size()]);}
if(mode==3){x.clear();for(int j=0;j<1500;++j)x.push_back(golden[rng()%golden.size()]);}
for(unsigned mask=0;mask<subsets;++mask){
if(argc>3 && mask!=static_cast<unsigned>(std::stoi(argv[3])))continue;
if((mode==2 || mode==3) && mask!=0)continue;
const auto begin=std::chrono::steady_clock::now();
const auto r=cppunivariate::sstd_mle({x.data(),x.size()},1e-8,1000,(mask&1?held_values[0]:nan),(mask&2?held_values[1]:nan),(mask&4?held_values[2]:nan),(mask&8?held_values[3]:nan));
const double seconds=std::chrono::duration<double>(std::chrono::steady_clock::now()-begin).count();
std::cout<<id<<" "<<sample<<" "<<mask<<" "<<static_cast<int>(r.status)<<" "<<r.mu<<" "<<r.sigma<<" "<<r.nu<<" "<<r.xi<<" "<<r.logLik; if(mode!=1 && mode!=3)std::cout<<" "<<seconds;std::cout<<std::endl;
}
}
}
if(id=="sged"){
const std::vector<double> golden=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/"+id+"/golden.json").data;
const std::vector<double> held_values={-0.032496865700421936,0.9390299202739361,3.4208324500711944,2.5890673965717057};
const unsigned subsets=1u<<4;
for(int sample=0;sample<(mode==1?(argc>4?std::stoi(argv[4]):620):(mode==3?20:1));++sample){
int n=sample<600?(sample/200==0?8:sample/200==1?16:32):500;
std::vector<double> x=golden;
if(mode==1){x.clear();for(int j=0;j<n;++j){double v=norm(rng);x.push_back(v+(sample%5==0?0.1*v*v:0));}}
if(mode==2){x.clear();for(int j=0;j<1000;++j)x.push_back(golden[j%golden.size()]);}
if(mode==3){x.clear();for(int j=0;j<1500;++j)x.push_back(golden[rng()%golden.size()]);}
for(unsigned mask=0;mask<subsets;++mask){
if(argc>3 && mask!=static_cast<unsigned>(std::stoi(argv[3])))continue;
if((mode==2 || mode==3) && mask!=0)continue;
const auto begin=std::chrono::steady_clock::now();
const auto r=cppunivariate::sged_mle({x.data(),x.size()},1e-8,1000,(mask&1?held_values[0]:nan),(mask&2?held_values[1]:nan),(mask&4?held_values[2]:nan),(mask&8?held_values[3]:nan));
const double seconds=std::chrono::duration<double>(std::chrono::steady_clock::now()-begin).count();
std::cout<<id<<" "<<sample<<" "<<mask<<" "<<static_cast<int>(r.status)<<" "<<r.mu<<" "<<r.sigma<<" "<<r.nu<<" "<<r.xi<<" "<<r.logLik; if(mode!=1 && mode!=3)std::cout<<" "<<seconds;std::cout<<std::endl;
}
}
}
if(id=="slash"){
const std::vector<double> golden=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/"+id+"/golden.json").data;
const std::vector<double> held_values={1.1437689913361664,-0.010630182705389644};
const unsigned subsets=1u<<2;
for(int sample=0;sample<(mode==1?(argc>4?std::stoi(argv[4]):620):(mode==3?20:1));++sample){
int n=sample<600?(sample/200==0?8:sample/200==1?16:32):500;
std::vector<double> x=golden;
if(mode==1){x.clear();for(int j=0;j<n;++j){double v=norm(rng);x.push_back(v+(sample%5==0?0.1*v*v:0));}}
if(mode==2){x.clear();for(int j=0;j<1000;++j)x.push_back(golden[j%golden.size()]);}
if(mode==3){x.clear();for(int j=0;j<1500;++j)x.push_back(golden[rng()%golden.size()]);}
for(unsigned mask=0;mask<subsets;++mask){
if(argc>3 && mask!=static_cast<unsigned>(std::stoi(argv[3])))continue;
if((mode==2 || mode==3) && mask!=0)continue;
const auto begin=std::chrono::steady_clock::now();
const auto r=cppunivariate::slash_mle({x.data(),x.size()},1e-8,1000,(mask&1?held_values[0]:nan),(mask&2?held_values[1]:nan));
const double seconds=std::chrono::duration<double>(std::chrono::steady_clock::now()-begin).count();
std::cout<<id<<" "<<sample<<" "<<mask<<" "<<static_cast<int>(r.status)<<" "<<r.sigma<<" "<<r.mu<<" "<<r.logLik; if(mode!=1 && mode!=3)std::cout<<" "<<seconds;std::cout<<std::endl;
}
}
}
if(id=="fpareto"){
const std::vector<double> golden=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/"+id+"/golden.json").data;
const std::vector<double> held_values={0,0.3,1.9802932530684678,0.4,1.0087842921157315};
const unsigned subsets=1u<<5;
for(int sample=0;sample<(mode==1?(argc>4?std::stoi(argv[4]):620):(mode==3?20:1));++sample){
int n=sample<600?(sample/200==0?8:sample/200==1?16:32):500;
std::vector<double> x=golden;
if(mode==1){x.clear();for(int j=0;j<n;++j){double v=norm(rng);x.push_back(std::exp(v));}}
if(mode==2){x.clear();for(int j=0;j<1000;++j)x.push_back(golden[j%golden.size()]);}
if(mode==3){x.clear();for(int j=0;j<1500;++j)x.push_back(golden[rng()%golden.size()]);}
for(unsigned mask=0;mask<subsets;++mask){
if(argc>3 && mask!=static_cast<unsigned>(std::stoi(argv[3])))continue;
if(!(mask&1))continue;
if((mode==2 || mode==3) && mask!=0)continue;
const auto begin=std::chrono::steady_clock::now();
const auto r=cppunivariate::fpareto_mle({x.data(),x.size()},(mask&1?held_values[0]:nan),1e-8,1000,(mask&2?held_values[1]:nan),(mask&4?held_values[2]:nan),(mask&8?held_values[3]:nan),(mask&16?held_values[4]:nan));
const double seconds=std::chrono::duration<double>(std::chrono::steady_clock::now()-begin).count();
std::cout<<id<<" "<<sample<<" "<<mask<<" "<<static_cast<int>(r.status)<<" "<<r.min<<" "<<r.shape1<<" "<<r.shape2<<" "<<r.shape3<<" "<<r.scale<<" "<<r.logLik; if(mode!=1 && mode!=3)std::cout<<" "<<seconds;std::cout<<std::endl;
}
}
}
if(id=="gb2"){
const std::vector<double> golden=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/"+id+"/golden.json").data;
const std::vector<double> held_values={0.5710330324328029,1.123220989970799,0.7116483765738807,1.0079365599415882};
const unsigned subsets=1u<<4;
for(int sample=0;sample<(mode==1?(argc>4?std::stoi(argv[4]):620):(mode==3?20:1));++sample){
int n=sample<600?(sample/200==0?8:sample/200==1?16:32):500;
std::vector<double> x=golden;
if(mode==1){x.clear();for(int j=0;j<n;++j){double v=norm(rng);x.push_back(std::exp(v));}}
if(mode==2){x.clear();for(int j=0;j<1000000;++j)x.push_back(golden[j%golden.size()]);}
if(mode==3){x.clear();for(int j=0;j<1500;++j)x.push_back(golden[rng()%golden.size()]);}
for(unsigned mask=0;mask<subsets;++mask){
if(argc>3 && mask!=static_cast<unsigned>(std::stoi(argv[3])))continue;
if((mode==2 || mode==3) && mask!=0)continue;
const auto begin=std::chrono::steady_clock::now();
const auto r=cppunivariate::gb2_mle({x.data(),x.size()},1e-8,1000,(mask&1?held_values[0]:nan),(mask&2?held_values[1]:nan),(mask&4?held_values[2]:nan),(mask&8?held_values[3]:nan));
const double seconds=std::chrono::duration<double>(std::chrono::steady_clock::now()-begin).count();
std::cout<<id<<" "<<sample<<" "<<mask<<" "<<static_cast<int>(r.status)<<" "<<r.shape1<<" "<<r.shape2<<" "<<r.shape3<<" "<<r.scale<<" "<<r.logLik; if(mode!=1 && mode!=3)std::cout<<" "<<seconds;std::cout<<std::endl;
}
}
}
if(id=="std"){
const std::vector<double> golden=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/"+id+"/golden.json").data;
const std::vector<double> held_values={-0.0062603717426506856,0.8395065634734545,3.977653882598804};
const unsigned subsets=1u<<3;
for(int sample=0;sample<(mode==1?(argc>4?std::stoi(argv[4]):620):(mode==3?20:1));++sample){
int n=sample<600?(sample/200==0?8:sample/200==1?16:32):500;
std::vector<double> x=golden;
if(mode==1){x.clear();for(int j=0;j<n;++j){double v=norm(rng);x.push_back(v+(sample%5==0?0.1*v*v:0));}}
if(mode==2){x.clear();for(int j=0;j<1000;++j)x.push_back(golden[j%golden.size()]);}
if(mode==3){x.clear();for(int j=0;j<1500;++j)x.push_back(golden[rng()%golden.size()]);}
for(unsigned mask=0;mask<subsets;++mask){
if(argc>3 && mask!=static_cast<unsigned>(std::stoi(argv[3])))continue;
if((mode==2 || mode==3) && mask!=0)continue;
const auto begin=std::chrono::steady_clock::now();
const auto r=cppunivariate::student_t_mle({x.data(),x.size()},1e-8,1000,(mask&1?held_values[0]:nan),(mask&2?held_values[1]:nan),(mask&4?held_values[2]:nan));
const double seconds=std::chrono::duration<double>(std::chrono::steady_clock::now()-begin).count();
std::cout<<id<<" "<<sample<<" "<<mask<<" "<<static_cast<int>(r.status)<<" "<<r.mu<<" "<<r.sigma<<" "<<r.nu<<" "<<r.logLik; if(mode!=1 && mode!=3)std::cout<<" "<<seconds;std::cout<<std::endl;
}
}
}
if(id=="lst"){
const std::vector<double> golden=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/"+id+"/golden.json").data;
const std::vector<double> held_values={0.0745507089520688,1.0270947160838733,3.024242725580454};
const unsigned subsets=1u<<3;
for(int sample=0;sample<(mode==1?(argc>4?std::stoi(argv[4]):620):(mode==3?20:1));++sample){
int n=sample<600?(sample/200==0?8:sample/200==1?16:32):500;
std::vector<double> x=golden;
if(mode==1){x.clear();for(int j=0;j<n;++j){double v=norm(rng);x.push_back(v+(sample%5==0?0.1*v*v:0));}}
if(mode==2){x.clear();for(int j=0;j<1000;++j)x.push_back(golden[j%golden.size()]);}
if(mode==3){x.clear();for(int j=0;j<1500;++j)x.push_back(golden[rng()%golden.size()]);}
for(unsigned mask=0;mask<subsets;++mask){
if(argc>3 && mask!=static_cast<unsigned>(std::stoi(argv[3])))continue;
if((mode==2 || mode==3) && mask!=0)continue;
const auto begin=std::chrono::steady_clock::now();
const auto r=cppunivariate::lst_mle({x.data(),x.size()},1e-8,1000,(mask&1?held_values[0]:nan),(mask&2?held_values[1]:nan),(mask&4?held_values[2]:nan));
const double seconds=std::chrono::duration<double>(std::chrono::steady_clock::now()-begin).count();
std::cout<<id<<" "<<sample<<" "<<mask<<" "<<static_cast<int>(r.status)<<" "<<r.mu<<" "<<r.sigma<<" "<<r.df<<" "<<r.logLik; if(mode!=1 && mode!=3)std::cout<<" "<<seconds;std::cout<<std::endl;
}
}
}
}
