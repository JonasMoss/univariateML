#pragma once
#include "../gb2/transport_accuracy.hpp"
namespace cppunivariate {
inline bool fpareto_transport_accuracy(span x,double shift,Gb2Result& r,
    const std::array<bool,4>& free,double tol,int maxit) {
  return gb2_transport_accuracy(x,shift,r,free,tol,maxit);
}
}
