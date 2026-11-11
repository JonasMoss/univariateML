#include "cppunivariate/distributions/std/mle.hpp"
#include <fstream>
#include <iomanip>
#include <iostream>
#include <map>
#include <sstream>
#include <string>
std::vector<std::string> fields(const std::string& line){
  std::vector<std::string> out;std::istringstream stream(line);std::string field;
  while(std::getline(stream,field,','))out.push_back(field);return out;
}
int main(){
  const std::string folder="dev/research/evaluations/2026-11-08-std-free-data/";
#ifdef UMVL_DIAGNOSTIC_BEFORE
  const std::string fit_file="seeded-coverage-before-refinement.csv";
#else
  const std::string fit_file="seeded-coverage.csv";
#endif
  std::ifstream input(folder+"seeded-samples.csv"),fits(folder+fit_file);
  if(!input||!fits)return 2;
  std::map<std::string,std::vector<double>> samples;std::string line;
  std::getline(input,line);
  while(std::getline(input,line)){
    const auto f=fields(line);if(f.size()!=67)return 3;
    auto& x=samples[f[0]+","+f[1]+","+f[2]];
    for(std::size_t j=3;j<f.size();++j)x.push_back(std::stod(f[j]));
  }
  std::cout<<std::setprecision(17)<<"family,source,seed,held_mask,category,proof_status,budget_exhausted,evaluations,c1_lower,c1_upper,cutoff,lower_shape,witnesses\n";
  std::getline(fits,line);
  while(std::getline(fits,line)){
    const auto f=fields(line);if(f.size()!=13)return 4;
    if(f[6]!="uncertified_face"&&!(f[6]=="uncertified_finite"&&f[3]=="3"))continue;
    const auto& x=samples.at(f[0]+","+f[1]+","+f[2]);
    const bool variance=f[0]=="std";const int mask=std::stoi(f[3]);
    std::cout<<f[0]<<","<<f[1]<<","<<f[2]<<","<<mask<<","<<f[6]<<",";
    if(mask==3){
      const auto p=cppunivariate::t_shape_detail::solve({x.data(),x.size()},0,1,variance,200,1e-10);
      std::cout<<p.status<<","<<p.detail.budget_exhausted<<","<<p.evaluations<<",nan,nan,nan,nan,"<<p.detail.candidates.size()<<"\n";
    }else{
      const auto p=cppunivariate::t_signed_detail::solve({x.data(),x.size()},mask&1?0:NAN,mask&2?1:NAN,variance,200);
      std::cout<<p.proved<<","<<p.budget_exhausted<<","<<p.evaluations<<","<<p.coefficient_lower<<","<<p.coefficient_upper<<","<<p.cutoff<<","<<p.lower_shape<<","<<p.detail.candidates.size()<<"\n";
    }
  }
  return std::cout?0:5;
}
