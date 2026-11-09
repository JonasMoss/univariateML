#include "cppunivariate/distributions/emg/mle.hpp"
#include "json_loader.hpp"
#include <iostream>
#include <iomanip>
int main() {
  std::cout<<std::unitbuf<<std::setprecision(17)<<"case,status,candidates,lower,upper,budget,tie\n";
  auto run=[](const char* name,const std::vector<double>& x,double tol,int maxit,double mu=NAN,double sigma=NAN,double lambda=NAN) {
    const auto r=cppunivariate::emg_mle({x.data(),x.size()},tol,maxit,mu,sigma,lambda);
    std::cout<<name<<','<<int(r.status)<<','<<r.stop_detail.candidates.size()<<','<<r.stop_detail.loglik_lower<<','<<r.stop_detail.loglik_upper<<','<<r.stop_detail.budget_exhausted<<','<<r.stop_detail.comparison_tie<<'\n';
  };
  const std::vector<double> a{-2,-1,0,1,2},c(7,.1),endpoint{0,0,1,2};
  run("exponential-face",a,1e-8,500);
  run("signed-normal-face",a,1e-8,500,0,1);
  run("unresolved-normal-face",a,1e-8,500,0);
  run("dominated-normal-face",a,1e-8,500,2);
  run("endpoint-multiplicity",endpoint,1e-8,500,0,NAN,1);
  run("constant-unbounded",c,1e-8,500);
  run("constant-normal-face",c,1e-8,500,NAN,1);
  run("constant-exponential-face",c,1e-8,500,NAN,NAN,1);
  run("constant-held-endpoint",c,1e-8,500,.1,NAN,1);
  run("budget-witness",a,1e-8,2,0,1);
  run("invalid-rate",a,1e-8,500,NAN,NAN,0);
  const auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/emg/golden.json");
  run("solver-failure",g.data,1e-10,1);
  run("unrepresentable-tolerance",g.data,1e-30,500);
}
