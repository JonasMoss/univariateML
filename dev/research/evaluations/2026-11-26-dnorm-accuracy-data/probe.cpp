#include "cppunivariate/distributions/dnorm/mle.hpp"
#include <iomanip>
#include <iostream>
int main() {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double tiny=std::numeric_limits<double>::denorm_min();
  const double k=0x1p51;
  auto fit=[](const char* id,const std::vector<double>& x,double m,double s) {
    const auto r=cppunivariate::dnorm_mle({x.data(),x.size()},1e-10,200,m,s);
    const auto o=cppunivariate::outcome(r);
    std::cout<<id<<" "<<std::setprecision(17)<<r.mean<<" "<<r.sd<<" "<<r.logLik<<" "<<r.iterations<<" "
             <<cppunivariate::outcome_kind_name(o.kind)<<" "<<o.certified<<"\n";
  };
  fit("constant52",{0},-0x1p-52,nan);
  fit("constant60",{0},-0x1p-60,nan);
  fit("constant100",{0},-0x1p-100,nan);
  fit("constant1074",{0},-tiny,nan);
  fit("adjacent52",{-1,0},-0x1p-52,nan);
  fit("adjacent60",{-1,0},-0x1p-60,nan);
  fit("adjacent100",{-1,0},-0x1p-100,nan);
  fit("adjacent1074",{-1,0},-tiny,nan);
  fit("adjacent_unbalanced",{-1,-1,0},-0x1p-100,nan);
  fit("constant_sd",{0},nan,tiny);
  fit("symmetric_sd",{-1,0},nan,tiny);
  fit("unrepresentable_sd",{-1,-1,0},nan,tiny);
  fit("adjacent_sd",{-1,-1,0},nan,0x1p-100);
  fit("both",{0},0.5,tiny);
  fit("both_edge",{0},0,tiny);
  fit("joint_limit",{-1,0},nan,nan);
  fit("joint",{-1,-1,1},nan,nan);
  fit("joint_shifted",{k,k,k+2},nan,nan);
  fit("held_large",{-1,0},1e308,nan);
  fit("scale_large",{-1,-1,1},nan,1e308);
}
