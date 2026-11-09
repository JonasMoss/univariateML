#include "bindings/common/distributions/gev/fit.hpp"
#include "bindings/common/distributions/frechet/fit.hpp"
#include <cassert>
int main() {
 const std::vector<double> g{0,2,2,2};
 const auto a=umvl_binding::fit_gev({g.data(),g.size()},{true,1.5},{true,.5},{},1e-15,100);
 assert(a.error_kind=="numerical");assert(std::isfinite(a.loglik_upper));
 std::vector<double> f(16,1);std::fill_n(f.begin(),7,0.);
 const auto b=umvl_binding::fit_frechet({f.data(),f.size()},{},{},{true,.125},1e-15,100);
 assert(b.error_kind=="numerical");
 f.assign(16,1);f[0]=0;
 const auto c=umvl_binding::fit_frechet({f.data(),f.size()},{},{},{true,.875},1e-10,10);
 assert(c.error_kind=="numerical");assert(c.candidates.size()==1);
 assert(c.loglik_lower==c.candidates[0].loglik_lower);
 bool scale=false;for(const auto& v:c.candidates[0].estimates)if(v.first=="sigma")scale=v.second==.875;
 assert(scale);
}
