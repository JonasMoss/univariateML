#include "cppunivariate/distributions/bhatt/mle.hpp"
#include "json_loader.hpp"
#include <chrono>
#include <ctime>
#include <iomanip>
#include <iostream>
#include <random>
#include <string>
int main(int argc,char** argv){
  std::cout<<std::setprecision(17);
  if(argc>1&&std::string(argv[1])=="timing"){
    const auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/bhatt/golden.json");
    std::cout<<"repeat,search_seconds,total_seconds,certified,mu,sigma,a,loglik,search_cpu_seconds,total_cpu_seconds\n";
    for(int j=0;j<3;++j){
      auto start=std::chrono::steady_clock::now();
      const auto start_cpu=std::clock();
      const auto old=cppunivariate::bhatt_detail::legacy_candidate(mltest::as_span(g.data));
      auto now=std::chrono::steady_clock::now();
      const auto search_cpu=std::clock();
      const auto f=cppunivariate::bhatt_mle(mltest::as_span(g.data));
      auto end=std::chrono::steady_clock::now();
      const auto end_cpu=std::clock();
      std::cout<<j<<','<<std::chrono::duration<double>(now-start).count()<<','<<std::chrono::duration<double>(end-now).count()<<','<<f.certified<<','<<f.mu<<','<<f.sigma<<','<<f.a<<','<<f.logLik<<','<<double(search_cpu-start_cpu)/CLOCKS_PER_SEC<<','<<double(end_cpu-search_cpu)/CLOCKS_PER_SEC<<std::endl;
      if(old.status!=cppunivariate::BhattStatus::Ok||f.status!=cppunivariate::BhattStatus::Ok||!f.certified)return 1;
    }
    return 0;
  }
  const int resume=argc>1?std::stoi(argv[1]):0;
  std::cout<<"row,model,a,n,rep,seed,status,certified,seconds,mu,sigma,fitted_a,loglik,search_status,refuted\n";
  int row=0;
  for(int model=0;model<7;++model)for(int n:{10,30,100,300,1000})for(int rep=0;rep<10;++rep,++row){
    if(row<resume)continue;
    const double a=model<5?std::vector<double>{.1,.5,1.,3.,10.}[model]:(model==5?0.:1.);
    const unsigned seed=11658000u+static_cast<unsigned>(row);
    std::mt19937 engine(seed);std::normal_distribution<double> normal(0,1);std::uniform_real_distribution<double> uniform(-a,a);
    std::vector<double> x;for(int i=0;i<n;++i)x.push_back(model==6?uniform(engine):(model==5?normal(engine):normal(engine)+uniform(engine)));
    const auto start=std::chrono::steady_clock::now();
    const auto f=cppunivariate::bhatt_mle(mltest::as_span(x));
    const double seconds=std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count();
    int search_status=-1;bool refuted=false;
    if(f.status!=cppunivariate::BhattStatus::Ok&&f.status!=cppunivariate::BhattStatus::UniformLimit){
      const auto existing=cppunivariate::bhatt_detail::legacy_candidate(mltest::as_span(x));
      search_status=int(existing.status);
      if(existing.status==cppunivariate::BhattStatus::Ok){
        const auto witness=cppunivariate::bhatt_detail::cert_witness(mltest::as_span(x),existing.mu,existing.sigma,existing.a);
        refuted=f.stop_detail.loglik_lower>witness.loglik_upper||existing.logLik<witness.loglik_lower||existing.logLik>witness.loglik_upper;
        if(cppunivariate::bhatt_detail::cert_valid(witness)&&!refuted)return 2;
      }else if(existing.status==cppunivariate::BhattStatus::UniformLimit){
        const double margin=1024*std::numeric_limits<double>::epsilon()*(x.size()+std::abs(existing.logLik));
        refuted=f.stop_detail.loglik_lower>existing.logLik+margin||!cppunivariate::bhatt_detail::uniform_representation_valid(existing,1e-8);
        if(!refuted)return 3;
      }
    }
    std::cout<<row<<','<<(model<5?"bhatt":model==5?"normal":"uniform")<<','<<a<<','<<n<<','<<rep<<','<<seed<<','<<int(f.status)<<','<<f.certified<<','<<seconds<<','<<f.mu<<','<<f.sigma<<','<<f.a<<','<<f.logLik<<','<<search_status<<','<<refuted<<std::endl;
  }
}
