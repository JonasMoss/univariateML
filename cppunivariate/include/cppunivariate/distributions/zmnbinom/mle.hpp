#pragma once
#include "../zmpois/profile.hpp"
namespace cppunivariate {
// Status and result types retain the shared factorised likelihood contract.
typedef zero_detail::ModifiedStatus ZmnbinomStatus;
typedef zero_detail::ModifiedResult ZmnbinomResult;
inline ZmnbinomResult zmnbinom_mle(ispan x, double tol=1e-10,int maxit=200,double fixed_size=std::numeric_limits<double>::quiet_NaN()) {
 return zero_detail::modified_mle(x,2,tol,maxit,fixed_size);
}
}
