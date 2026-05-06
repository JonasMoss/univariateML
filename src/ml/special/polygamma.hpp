// Polygamma family without Rmath.
//
// `dpsifn` is the Bell Labs / Amos algorithm 610 routine, ported verbatim
// from R's polygamma.c. It computes the scaled sequence
//
//   ans[k] = (-1)^{n+k+1} / (n+k)! * psi^{(n+k)}(x),  k = 0, ..., m-1
//
// for kode == 1, m >= 1 (the only case our callers use; kode == 2 path is
// also kept for upstream parity).
//
// `psigamma_deriv(x, n)` is the convenience wrapper: returns psi^{(n)}(x)
// directly. digamma/trigamma/tetragamma in special.hpp delegate to this.

#pragma once

namespace mlcore {
namespace special {

void dpsifn(double x, int n, int kode, int m, double *ans, int *nz, int *ierr);

double psigamma_deriv(double x, int n);

}  // namespace special
}  // namespace mlcore
