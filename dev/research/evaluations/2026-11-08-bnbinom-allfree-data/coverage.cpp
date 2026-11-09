#include "cppunivariate/distributions/bnbinom/mle.hpp"
#include "cppunivariate/distributions/nbinom/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <chrono>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <set>
#include <string>

// Fixed design: 216 BNB, 36 NB and 18 Poisson samples. All fits use the
// production defaults tol=1e-8, iterlim=600; generation is outside timing.
int main(int argc,char **argv) {
  if(argc!=2)return 2;
  const std::string directory="build/scratch/bnbinom-allfree/coverage/";
  std::set<int> complete;
  std::ifstream previous(directory+"coverage.csv");std::string line;
  std::getline(previous,line);
  while(std::getline(previous,line))if(!line.empty())complete.insert(std::stoi(line.substr(0,line.find(','))));
  std::ofstream csv(directory+"coverage.csv",std::ios::app),samples(directory+"samples.jsonl",std::ios::app);
  csv<<std::setprecision(17);
  if(complete.empty())csv<<"id,machine,source,size,alpha,beta,prob,lambda,n,seed,status,certified,profile_closed,category,fit_ms,fit_size,fit_alpha,fit_beta,loglik,max_x\n";
  int id=0;
  const auto run=[&](const std::string &source,double r,double a,double b,double prob,double lambda,int n) {
    const int current=id++;if(complete.count(current))return true;
    const unsigned seed=11654000u+static_cast<unsigned>(current);
    cppunivariate::distributions::StdRng<> rng(seed);std::vector<int> x;int max_x=0;
    for(int j=0;j<n;++j) {
      const double value=source=="bnbinom"?cppunivariate::distributions::rbnbinom(rng,r,a,b):
        source=="nbinom"?cppunivariate::distributions::rnbinom(rng,r,prob):rng.rpois(lambda);
      if(!(std::isfinite(value) && value>=0 && value<=std::numeric_limits<int>::max()))return false;
      x.push_back(static_cast<int>(value));max_x=std::max(max_x,x.back());
    }
    const auto start=std::chrono::steady_clock::now();
    const auto fit=cppunivariate::bnbinom_mle({x.data(),x.size()});
    const double ms=std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count();
    const bool finite=fit.status==cppunivariate::BnbinomStatus::Ok;
    const bool face=fit.status==cppunivariate::BnbinomStatus::NegativeBinomialLimit ||
      fit.status==cppunivariate::BnbinomStatus::PoissonLimit || fit.status==cppunivariate::BnbinomStatus::PointMass;
    if(finite && !(std::isfinite(fit.size+fit.alpha+fit.beta+fit.logLik) && fit.size>0 && fit.alpha>0 && fit.beta>0))return false;
    if(finite && fit.certified && !std::isfinite(fit.global_interval[0]))return false;
    const std::string category=finite&&fit.certified?"certified_finite":face&&fit.certified?"proved_face":
      (finite||face)?"uncertified_legacy":"search_failure";
    samples<<"{\"id\":"<<current<<",\"x\":[";
    for(std::size_t j=0;j<x.size();++j){if(j)samples<<',';samples<<x[j];}
    samples<<"]}\n";samples.flush();
    csv<<current<<','<<argv[1]<<','<<source<<','<<r<<','<<a<<','<<b<<','<<prob<<','<<lambda<<','<<n<<','<<seed<<','
      <<static_cast<int>(fit.status)<<','<<fit.certified<<','<<std::isfinite(fit.profile_alpha[0])<<','<<category<<','<<ms<<','<<fit.size<<','<<fit.alpha<<','<<fit.beta<<','<<fit.logLik<<','<<max_x<<'\n';csv.flush();
    std::cerr<<current+1<<"/270 "<<source<<" n="<<n<<' '<<category<<' '<<ms<<" ms\n";
    return true;
  };
  for(double r:{.5,1.,3.})for(double a:{1.5,3.,8.})for(double b:{.5,2.,8.})for(int n:{30,200,1000})for(int repeat=0;repeat<2;++repeat)
    if(!run("bnbinom",r,a,b,NAN,NAN,n))return 1;
  for(double r:{.5,2.,10.})for(double p:{.2,.7})for(int n:{30,200,1000})for(int repeat=0;repeat<2;++repeat)
    if(!run("nbinom",r,NAN,NAN,p,NAN,n))return 1;
  for(double lambda:{.3,2.,10.})for(int n:{30,200,1000})for(int repeat=0;repeat<2;++repeat)
    if(!run("pois",NAN,NAN,NAN,NAN,lambda,n))return 1;
  // Append the infinite-mean stratum, preserving all original row IDs/seeds.
  for(double r:{.5,1.,3.})for(double b:{.5,2.,8.})for(int n:{30,200,1000})for(int repeat=0;repeat<2;++repeat)
    if(!run("bnbinom",r,.75,b,NAN,NAN,n))return 1;
  return id==270?0:1;
}
