#include "cppunivariate/distributions/sn/mle.hpp"
#include <iostream>
#include <iomanip>

int main() {
  std::vector<double> x{0.11024753757609822, -0.049422603469292115,
    1.3131208557689356, -0.22599451092730288, 1000001.3131208557};
  const double origin=x.front();
  const double unit=x.back()-x[3];
  for (auto& value:x) value=(value-origin)/unit;
  const double omega=0.9203517925594821/unit,alpha=0.10872382324228061;
  const double scale=(x.back()-x[3])/2, midpoint=x[3]+scale;
  std::vector<double> y;
  for(double value:x) y.push_back((value-midpoint)/scale);
  double mean=0,variance=0;
  for(double value:y) mean+=value/y.size();
  for(double value:y) variance+=(value-mean)*(value-mean)/y.size();
  const double sd=std::sqrt(variance);
  const double center=midpoint+scale*mean, spread=scale*sd;
  for(double& value:y) value=(value-mean)/sd;
  auto inner=cppunivariate::sn_detail::inner(y,alpha,NAN,omega/spread,1e-10,200);
  const auto fit=cppunivariate::sn_accuracy_search({x.data(),x.size()},1e-10,200,NAN,omega,alpha);
  const double location=center+spread*inner.h/inner.eta;
  double ll=0;
  for(double value:x) ll+=cppunivariate::distributions::dsn(value,location,omega,alpha,true);
  std::cout<<std::setprecision(17)<<"sn status="<<static_cast<int>(fit.status)
           <<" witnesses="<<fit.stop_detail.candidates.size()
           <<" inner accepted="<<inner.converged
           <<" physical logLik="<<ll
           <<" normalized logLik="<<inner.ll-x.size()*std::log(spread)
           <<" gap="<<ll-(inner.ll-x.size()*std::log(spread))<<"\n";
}
