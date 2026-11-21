// Replay the frozen TASK-180 archive and every hold subset.
#include "cppunivariate/distributions/expweibull/mle.hpp"
#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

int main(int argc,char** argv) {
  if(argc!=2)return 2;
  std::ifstream input(argv[1]);
  std::string line;
  if(!std::getline(input,line))return 2;
  std::vector<std::pair<std::string,std::vector<double>>> samples;
  while(std::getline(input,line)) {
    std::istringstream row(line);
    std::string name,index,value;
    if(!std::getline(row,name,',') || !std::getline(row,index,',') ||
        !std::getline(row,value))return 2;
    if(samples.empty() || samples.back().first!=name)samples.emplace_back(name,std::vector<double>{});
    samples.back().second.push_back(std::strtod(value.c_str(),nullptr));
  }
  std::cout<<"sample,mask,status,kind,reason,certified,scale,shape,exponent,logLik,model\n"
    <<std::setprecision(17);
  for(const auto& sample:samples) {
    const cppunivariate::span x{sample.second.data(),sample.second.size()};
    for(int mask=0;mask<8;++mask) {
      const auto r=cppunivariate::expweibull_mle(x,1e-10,100,
        mask&1?2:NAN,mask&2?1.5:NAN,mask&4?2.5:NAN);
      const auto o=cppunivariate::outcome(r);
      const auto m=r.stop_detail.limit_model;
      const char* model=m==cppunivariate::StopModel::InvWeibull?"invweibull":
        m==cppunivariate::StopModel::Power?"power":"";
      std::cout<<sample.first<<','<<mask<<','<<static_cast<int>(r.status)<<','
        <<cppunivariate::outcome_kind_name(o.kind)<<','<<cppunivariate::outcome_reason_name(o.reason)<<','
        <<o.certified<<','<<r.scale<<','<<r.shape<<','<<r.exponent<<','<<r.logLik<<','<<model<<'\n';
    }
  }
}
