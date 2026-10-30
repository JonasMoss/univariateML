// Standalone seeded frequency experiment, not part of the header-only library.
// Compile/run under /tmp/umvl-cpu/run; output checkpoints after every cell.
#include "cppunivariate/distributions/sn/mle.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
int main() {
  const std::string path="/tmp/umvl-codex/sn/simulation.csv";
  std::vector<std::string> done;
  std::ifstream previous(path);
  std::string line;
  std::getline(previous,line);
  while(std::getline(previous,line)) {
    std::istringstream row(line); std::string a,n;
    std::getline(row,a,','); std::getline(row,n,','); done.push_back(a+","+n);
  }
  previous.close();
  std::ofstream out(path,std::ios::app);
  if(done.empty()) out<<"alpha,n,seed,reps,right,left,failed\n";
  for(int alpha:{0,1,3,5,10}) for(int n:{20,50,100,500}) {
    const std::string key=std::to_string(alpha)+","+std::to_string(n);
    if(std::find(done.begin(),done.end(),key)!=done.end()) continue;
    const int seed=20261029+10000*alpha+n;
    cppunivariate::distributions::StdRng<> rng(seed);
    int right=0,left=0,failed=0;
    for(int i=0;i<100;++i) {
      std::vector<double> x;
      for(int j=0;j<n;++j) x.push_back(cppunivariate::distributions::rsn(rng,0,1,alpha));
      const auto f=cppunivariate::sn_mle({x.data(),x.size()},1e-9,200);
      if(f.status==cppunivariate::SnStatus::RightHalfNormalBoundary) ++right;
      else if(f.status==cppunivariate::SnStatus::LeftHalfNormalBoundary) ++left;
      else if(f.status!=cppunivariate::SnStatus::Ok) ++failed;
    }
    const std::string record=key+","+std::to_string(seed)+",100,"+std::to_string(right)+","+std::to_string(left)+","+std::to_string(failed);
    out<<record<<std::endl;
    std::cout<<record<<std::endl;
  }
}
