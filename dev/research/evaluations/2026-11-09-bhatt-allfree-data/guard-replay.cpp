// Replay the final, rejection-only physical consistency guards on recorded
// census decisions, regenerating the independently seeded samples.
#include "cppunivariate/distributions/bhatt/mle.hpp"
#include <fstream>
#include <iostream>
#include <random>
#include <sstream>
#include <string>
#include <vector>
int main(){
  std::ifstream input("dev/research/evaluations/2026-11-09-bhatt-allfree-data/review-census.csv");
  std::string line;std::getline(input,line);int rows=0,finite=0,faces=0,errors=0,changed=0;
  std::cout<<"row,status,certified,consistent,sample_likelihood_verified\n";
  while(std::getline(input,line)){
    if(line.empty())continue;
    std::stringstream stream(line);std::vector<std::string> c;std::string cell;
    while(std::getline(stream,cell,','))c.push_back(cell);
    if(c.size()!=15)return 2;
    const int row=std::stoi(c[0]),n=std::stoi(c[3]),status=std::stoi(c[6]),certified=std::stoi(c[7]);
    const double a=std::stod(c[2]),mu=std::stod(c[9]),sigma=std::stod(c[10]),width=std::stod(c[11]),ll=std::stod(c[12]);
    int model=c[1]=="normal"?5:c[1]=="uniform"?6:0;
    if(c[1]=="bhatt"){const std::vector<double> shapes{.1,.5,1.,3.,10.};model=static_cast<int>(std::find(shapes.begin(),shapes.end(),a)-shapes.begin());}
    std::mt19937 engine(static_cast<unsigned>(std::stoul(c[5])));
    std::normal_distribution<double> normal(0,1);std::uniform_real_distribution<double> uniform(-a,a);
    std::vector<double> x;for(int i=0;i<n;++i)x.push_back(model==6?uniform(engine):(model==5?normal(engine):normal(engine)+uniform(engine)));
    bool consistent=true,verified=true;
    if(status==0){
      ++finite;
      const auto witness=cppunivariate::bhatt_detail::cert_witness({x.data(),x.size()},mu,sigma,width);
      consistent=cppunivariate::bhatt_detail::cert_valid(witness)&&std::isfinite(ll)&&ll>=witness.loglik_lower&&ll<=witness.loglik_upper;
      verified=consistent;
    }else if(status==2){
      ++faces;
      const double lo=*std::min_element(x.begin(),x.end()),hi=*std::max_element(x.begin(),x.end());
      const double radius=hi/2-lo/2,oracle=-n*(std::log(2.)+std::log(radius));
      const double margin=1024*std::numeric_limits<double>::epsilon()*(n+std::abs(oracle));
      verified=std::abs(ll-oracle)<=margin&&std::abs(width-radius)<=1e-8*radius;
      cppunivariate::BhattResult face{mu,0,width,ll,0,cppunivariate::BhattStatus::UniformLimit};
      face.stop_detail=cppunivariate::limit_detail({lo,hi},ll,cppunivariate::StopModel::Unif);
      consistent=cppunivariate::bhatt_detail::uniform_representation_valid(face,1e-8);

    }else{
      ++errors;
      consistent=std::stoi(c[13])!=0||std::stoi(c[14])!=0;
    }
    if(!consistent||!verified)++changed;
    std::cout<<row<<','<<status<<','<<certified<<','<<consistent<<','<<verified<<'\n';++rows;
  }
  std::ofstream summary("dev/research/evaluations/2026-11-09-bhatt-allfree-data/guard-replay-summary.json");
  summary<<"{\"rows\":"<<rows<<",\"finite\":"<<finite<<",\"uniform_faces\":"<<faces<<",\"other_stops\":"<<errors<<",\"guard_or_regeneration_failures\":"<<changed<<"}\n";
  return rows==350&&changed==0?0:1;
}
