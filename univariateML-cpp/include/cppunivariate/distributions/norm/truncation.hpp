#pragma once
#include "../../core/truncation.hpp"
#include "../tnorm/mle.hpp"
namespace cppunivariate {
struct NormTruncationTraits {};
// Joint concavity in (mean/sd^2, -1/(2 sd^2)); the existing solver also
// compares the exponential/uniform face and supports every coefficient hold.
template<> struct truncated<NormTruncationTraits> {
  static TnormResult fit(span x,double a,double b,double mean=NAN,double sd=NAN,double tol=1e-10,int maxit=200) {
    return tnorm_mle(x,a,b,tol,maxit,mean,sd);
  }
};
}
