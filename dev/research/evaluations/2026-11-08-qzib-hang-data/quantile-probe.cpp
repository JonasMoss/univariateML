#include <iostream>
#include <cppunivariate/distributions/dnorm/dpqr.hpp>
#include <cppunivariate/distributions/dgamma/dpqr.hpp>
#include <cppunivariate/distributions/zib/dpqr.hpp>
#include <cppunivariate/distributions/dlaplace/dpqr.hpp>
#include <cppunivariate/distributions/skellam/dpqr.hpp>
#include <cppunivariate/distributions/zeta/dpqr.hpp>
#include <cppunivariate/distributions/yulesimon/dpqr.hpp>
#include <cppunivariate/distributions/cmp/dpqr.hpp>
#include <cppunivariate/distributions/cmb/dpqr.hpp>
#include <cppunivariate/distributions/bbinom/dpqr.hpp>
#include <cppunivariate/distributions/tbinom/dpqr.hpp>
#include <cppunivariate/distributions/tpois/dpqr.hpp>
#include <cppunivariate/distributions/nhyper/dpqr.hpp>
int main(){
std::cout.precision(17);
#define P(x) std::cout << #x << " = " << (x) << std::endl
P(cppunivariate::distributions::qzib(.5,1e308,.9,0));
P(cppunivariate::distributions::qbinom(.5,1e308,.9,true,false));
P(cppunivariate::distributions::qpois(.5,1e308,true,false));
P(cppunivariate::distributions::qnbinom(.5,1e308,.1,true,false));
P(cppunivariate::distributions::qdlaplace(.25,1e308,.5));
P(cppunivariate::distributions::qskellam(.5,1e308,1e308));
P(cppunivariate::distributions::qzeta(.9,1.001));
P(cppunivariate::distributions::qyulesimon(.9,.001));
P(cppunivariate::distributions::qcmp(.5,.9999999999999999,0));
P(cppunivariate::distributions::qcmb(.5,10,.4,1e308));
P(cppunivariate::distributions::qbbinom(.5,10,1e308,1e308,true,false));
P(cppunivariate::distributions::qtbinom(.5,.9,-INFINITY,INFINITY,1e308));
P(cppunivariate::distributions::qtpois(.5,1e308,-INFINITY,INFINITY));
P(cppunivariate::distributions::qzib(.5,10,1,.5));
P(cppunivariate::distributions::qzib(-0.69314718055994530942,10,1,.5,true,true));
P(cppunivariate::special::binomial_quantile(.5,1e308,.9,true));
P(cppunivariate::special::poisson_quantile(.5,1e308,true));
P(cppunivariate::special::negative_binomial_quantile(.5,1e308,.1,true));
P(cppunivariate::special::log_gamma_ratio(300,1,1e308));
P(cppunivariate::distributions::qbnbinom(.5,1,1e308,1,true,false));
P(cppunivariate::distributions::qdnorm(.5,1e308,1));
P(cppunivariate::distributions::qdgamma(.5,2,1e-300));
}
