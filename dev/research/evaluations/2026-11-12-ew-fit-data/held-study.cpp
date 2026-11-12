// Reproduce from repository root with the slot-locked command in the report.
#include "cppunivariate/distributions/expweibull/mle.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <chrono>
#include <iomanip>
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <sstream>
#include <string>
#include <vector>
std::string status(cppunivariate::ExpweibullStatus s) {
  switch(s) {
    case cppunivariate::ExpweibullStatus::Ok:return "Ok";
    case cppunivariate::ExpweibullStatus::Unbounded:return "Unbounded";
    case cppunivariate::ExpweibullStatus::PowerBoundary:return "PowerBoundary";
    case cppunivariate::ExpweibullStatus::PowerBoundaryTied:return "PowerBoundaryTied";
    case cppunivariate::ExpweibullStatus::AccuracyFailure:return "AccuracyFailure";
    case cppunivariate::ExpweibullStatus::IterationFailure:return "IterationFailure";
    default:return "Invalid";
  }
}
int main(int argc,char** argv) {
  const int budget=argc>3?std::atoi(argv[3]):100;
  if(budget<1)return 2;
  std::vector<std::pair<std::string,std::vector<double>>> cases;
  for(double c:{.2,1.,2.5,20.}) {
    std::vector<double> x;for(int i=0;i<80;++i)x.push_back(cppunivariate::distributions::qexpweibull((i+.5)/80,2,1.5,c,true,false));
    cases.push_back({"quantile_c"+std::to_string(c),x});
  }
  cases.push_back({"two_point",{1,1,2,2,2}});
  cases.push_back({"below_endpoint",{.1,.4,.8,1.2,1.8}});
  cases.push_back({"endpoint_ties",{.4,.8,2,2,2}});
  cases.push_back({"constant_below",{1,1,1,1}});
  cases.push_back({"constant_at",{2,2,2,2}});
  cases.push_back({"constant_above",{3,3,3,3}});
  std::vector<double> close;for(double y:{0.,1e-6,2e-6,3e-6,5e-6})close.push_back(std::exp(y));
  cases.push_back({"near_constant",close});
  cases.push_back({"wide",{std::exp(-15.),std::exp(-5.),1,std::exp(5.),std::exp(15.)}});
  if(argc>1 && std::string(argv[1])=="archive") {
    cppunivariate::distributions::StdRng<> rng(18020261112);
    for(double c:{.05,.2,1.,5.,50.})for(double b:{.3,1.5,8.}) {
      std::vector<double> x;
      for(int i=0;i<40;++i)x.push_back(cppunivariate::distributions::rexpweibull(rng,2,b,c));
      cases.push_back({"random_b"+std::to_string(b)+"_c"+std::to_string(c),x});
    }
    for(double delta:{1e-3,1e-6,1e-9})for(int multiplicity:{1,3,8}) {
      std::vector<double> x{std::exp(-delta),std::exp(-.3*delta)};
      for(int i=0;i<multiplicity;++i)x.push_back(1);
      cases.push_back({"ties_"+std::to_string(delta)+"_"+std::to_string(multiplicity),x});
    }
    for(double ratio:{1e-100,1e-6,1.,1e6,1e100}) {
      std::vector<double> x{ratio*.4,ratio*.8,ratio*1.3,ratio*2,ratio*2};
      cases.push_back({"rescaled_"+std::to_string(std::log10(ratio)),x});
    }
  }
  const bool replay=argc>2 && std::string(argv[1])=="replay";
  if(replay) {
    cases.clear();std::ifstream input(argv[2]);std::string line;
    if(!std::getline(input,line))return 2;
    while(std::getline(input,line)) {
      std::istringstream fields(line);std::string name,index,value;
      if(!std::getline(fields,name,',') || !std::getline(fields,index,',') || !std::getline(fields,value))return 2;
      if(cases.empty() || cases.back().first!=name)cases.push_back({name,{}});
      cases.back().second.push_back(std::strtod(value.c_str(),nullptr));
    }
  }
  if(!replay && argc>2 && std::string(argv[2])!="-") {
    std::ofstream samples(argv[2]);samples<<"sample,index,x\n"<<std::setprecision(17);
    for(const auto& entry:cases)for(std::size_t i=0;i<entry.second.size();++i)samples<<entry.first<<","<<i<<","<<entry.second[i]<<"\n";
  }
  std::cout<<"sample,mask,iterlim,status,certified,scale,shape,exponent,logLik,ms,iterations,seam_error\n"<<std::setprecision(17);
  for(const auto& entry:cases)for(int mask=0;mask<8;++mask) {
    const auto begin=std::chrono::steady_clock::now();
    const auto r=cppunivariate::expweibull_mle({entry.second.data(),entry.second.size()},1e-10,budget,mask&1?2:NAN,mask&2?1.5:NAN,mask&4?2.5:NAN);
    const double ms=std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-begin).count();
    double seam=NAN;
    if(r.status==cppunivariate::ExpweibullStatus::Ok) {
      double ll=0;for(double v:entry.second)ll+=cppunivariate::distributions::dexpweibull(v,r.scale,r.shape,r.exponent,true);
      seam=ll-r.logLik;
    }
    std::cout<<entry.first<<","<<mask<<","<<budget<<","<<status(r.status)<<","<<r.certified<<","<<r.scale<<","<<r.shape<<","<<r.exponent<<","<<r.logLik<<","<<ms<<","<<r.iterations<<","<<seam<<"\n";
  }
}
