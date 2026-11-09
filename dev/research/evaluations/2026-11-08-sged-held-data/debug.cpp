#include <iostream>
#include <iomanip>
#include "cppunivariate/distributions/sged/mle.hpp"
#include "json_loader.hpp"
namespace cppunivariate {
#include "held-debug.hpp"
}
int main(){std::cerr<<std::setprecision(17);auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/sged/golden.json");
const auto r=cppunivariate::sged_held_moment_debug::fit({g.data.data(),g.data.size()},1e-6,1000,g.estimates.at("mean"),g.estimates.at("sd"),g.estimates.at("nu"));std::cerr<<int(r.status)<<" "<<r.xi<<"\n";}
