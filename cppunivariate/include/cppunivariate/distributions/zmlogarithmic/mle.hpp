#pragma once
#include "../zmpois/profile.hpp"
namespace cppunivariate {
// Status and result types retain the shared factorised likelihood contract.
typedef zero_detail::ModifiedStatus ZmlogarithmicStatus;
typedef zero_detail::ModifiedResult ZmlogarithmicResult;
inline ZmlogarithmicResult zmlogarithmic_mle(ispan x, double tol=1e-10,int maxit=200) {
 return zero_detail::modified_mle(x,4,tol,maxit,std::numeric_limits<double>::quiet_NaN());
}
}
