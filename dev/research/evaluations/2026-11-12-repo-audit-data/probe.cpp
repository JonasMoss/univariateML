#include "cppunivariate/distributions/norm/mle.hpp"
#include "cppunivariate/distributions/exp/mle.hpp"
#include "cppunivariate/distributions/gamma/mle.hpp"
#include "cppunivariate/distributions/weibull/mle.hpp"
#include "cppunivariate/distributions/lomax/mle.hpp"
#include "cppunivariate/distributions/cauchy/mle.hpp"
#include "cppunivariate/distributions/logis/mle.hpp"
#include "cppunivariate/distributions/lnorm/mle.hpp"
#include "cppunivariate/distributions/laplace/mle.hpp"
#include "cppunivariate/distributions/gumbel/mle.hpp"
#include <cstdio>
#include <vector>
using namespace cppunivariate;
static span S(const std::vector<double>& v){return span{v.data(),v.size()};}
int main(){
  const double nan=std::numeric_limits<double>::quiet_NaN();
  std::vector<std::pair<const char*,std::vector<double>>> sets={
    {"const3",{2,2,2}},{"near-const",{1,1,1+2.220446049250313e-16}},{"two-far",{1e-300,1e300}},
    {"huge",{1e308,1.5e308,1.7e308}},{"tiny",{4.9e-324,9.9e-324,1.5e-323}},{"one",{3.5}},
    {"1,100",{1,100}},{"ties",{1,1,1,1,2,3}},{"spread",{1e-8,1,1e8}},{"ord",{0.5,1.2,2.3,0.7,3.1,1.9}}};
  for(auto& [name,v]:sets){
    std::printf("--- %s\n",name);
    {auto r=norm_mle(S(v));std::printf("norm    st=%d mean=%.17g sd=%.17g ll=%.17g\n",(int)r.status,r.mean,r.sd,r.logLik);}
    {auto r=exp_mle(S(v));std::printf("exp     st=%d rate=%.17g ll=%.17g\n",(int)r.status,r.rate,r.logLik);}
    {auto r=gamma_mle(S(v),1e-10,100);std::printf("gamma   st=%d shape=%.17g scale=%.17g ll=%.17g it=%d\n",(int)r.status,r.shape,r.scale,r.logLik,r.iterations);}
    {auto r=weibull_mle(S(v),1e-10,100,nan,nan,nan);std::printf("weibull st=%d shape=%.17g scale=%.17g ll=%.17g it=%d\n",(int)r.status,r.shape,r.scale,r.logLik,r.iterations);}
    {auto r=lomax_mle(S(v),1e-10,100,nan,nan,nan);std::printf("lomax   st=%d lambda=%.17g kappa=%.17g ll=%.17g it=%d cf=%d\n",(int)r.status,r.lambda,r.kappa,r.logLik,r.iterations,(int)r.certificate_failure);}
    {auto r=fit_cauchy(S(v),1e-10,100);std::printf("cauchy  st=%d mu=%.17g scale=%.17g ll=%.17g it=%d\n",(int)r.status,r.mu,r.scale,r.logLik,r.iterations);}
    {auto r=logistic_mle(S(v),1e-10,100);std::printf("logis   st=%d mu=%.17g scale=%.17g ll=%.17g it=%d\n",(int)r.status,r.mu,r.scale,r.logLik,r.iterations);}
    {auto r=lognormal_mle(S(v));std::printf("lnorm   st=%d meanlog=%.17g sdlog=%.17g ll=%.17g cert=%d\n",(int)r.status,r.meanlog,r.sdlog,r.logLik,(int)r.certified);}
    {auto r=laplace_mle(S(v));std::printf("laplace st=%d mu=%.17g sigma=%.17g ll=%.17g\n",(int)r.status,r.mu,r.sigma,r.logLik);}
  }
  // tol semantics: does a loose tol loosen anything?
  std::vector<double> o={0.5,1.2,2.3,0.7,3.1,1.9,0.2,4.4};
  for(double tol:{1e-2,1e-6,1e-10,1e-14}){auto r=gamma_mle(S(o),tol,100);auto w=weibull_mle(S(o),tol,100,nan,nan,nan);auto l=logistic_mle(S(o),tol,100);
    std::printf("tol=%g gamma st=%d shape=%.17g it=%d | weibull st=%d shape=%.17g it=%d | logis st=%d scale=%.17g it=%d\n",tol,(int)r.status,r.shape,r.iterations,(int)w.status,w.shape,w.iterations,(int)l.status,l.scale,l.iterations);}
  for(int it:{1,2,5}){auto r=gamma_mle(S(o),1e-10,it);auto w=weibull_mle(S(o),1e-10,it,nan,nan,nan);auto l=logistic_mle(S(o),1e-10,it);
    std::printf("maxit=%d gamma st=%d shape=%.17g | weibull st=%d shape=%.17g | logis st=%d scale=%.17g\n",it,(int)r.status,r.shape,(int)w.status,w.shape,(int)l.status,l.scale);}
}
