#include "cppunivariate/distributions/cmb/mle.hpp"
#include "json_loader.hpp"
#include <iostream>
#include <iomanip>
#include <chrono>
int main() {
 const auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/cmb/golden.json");
 std::vector<int> x;for(double v:g.data)x.push_back(int(v));
 std::cout<<std::setprecision(17);
 for(int subset=0;subset<4;++subset) {
 const double p=(subset&1)?g.estimates.at("prob"):NAN;
 const double nu=(subset&2)?g.estimates.at("nu"):NAN;
 const auto start=std::chrono::steady_clock::now();
 const auto r=(subset&2)?((subset&1)?cppunivariate::cmb_size_mle({x.data(),x.size()},p,nu):cppunivariate::cmb_size_fixed_nu_mle({x.data(),x.size()},nu)):cppunivariate::cmb_size_free_nu_mle({x.data(),x.size()},p);
 std::cout<<subset<<' '<<int(r.fit.status)<<' '<<r.size<<' '<<r.fit.prob<<' '<<r.fit.nu<<' '<<r.fit.logLik<<' '<<r.fit.stop_detail.budget_exhausted<<' '<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<'\n';
 }
 const auto f=cppunivariate::cmb_mle({x.data(),x.size()},10);
 std::cout<<"golden "<<int(f.status)<<' '<<f.prob<<' '<<f.nu<<' '<<f.logLik<<'\n';
}
