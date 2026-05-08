// Hand-rolled Cholesky factorisation, solve, and inverse for small symmetric
// positive-definite matrices. Row-major storage. Capped at kFiniteDiffMaxDim
// to keep stack usage bounded; all current estimators are at most 4-D so the
// cap is comfortably above what we need.
//
// `cholesky_factor` overwrites the lower triangle of A with L (where A = L L^T)
// and zeros the strict upper triangle. Returns false if a non-positive pivot
// is encountered (matrix not PD; A may be partially modified).
//
// `cholesky_solve` solves L L^T x = b in place on x.
//
// `invert_symmetric_pd` factors then inverts in place, producing A^{-1} as a
// dense symmetric matrix in A. Returns false if A is not PD; on failure A is
// left in a partially modified state.

#pragma once

#include <cmath>
#include <cstddef>

namespace cppunivariate {

inline bool cholesky_factor(double *A, int n) {
  for (int i = 0; i < n; ++i) {
    double *Ai = A + static_cast<std::size_t>(i) * n;
    for (int j = 0; j <= i; ++j) {
      double *Aj = A + static_cast<std::size_t>(j) * n;
      double sum = Ai[j];
      for (int k = 0; k < j; ++k) sum -= Ai[k] * Aj[k];
      if (i == j) {
        if (!(sum > 0.0)) return false;
        Ai[i] = std::sqrt(sum);
      } else {
        Ai[j] = sum / Aj[j];
      }
    }
    // Zero the strict upper triangle.
    for (int j = i + 1; j < n; ++j) Ai[j] = 0.0;
  }
  return true;
}

inline void cholesky_solve(const double *L, double *x, int n) {
  // Forward: L y = b, solve for y in place.
  for (int i = 0; i < n; ++i) {
    const double *Li = L + static_cast<std::size_t>(i) * n;
    double sum = x[i];
    for (int k = 0; k < i; ++k) sum -= Li[k] * x[k];
    x[i] = sum / Li[i];
  }
  // Back: L^T x = y, solve for x in place.
  for (int i = n - 1; i >= 0; --i) {
    double sum = x[i];
    for (int k = i + 1; k < n; ++k) {
      const double *Lk = L + static_cast<std::size_t>(k) * n;
      sum -= Lk[i] * x[k];
    }
    const double *Li = L + static_cast<std::size_t>(i) * n;
    x[i] = sum / Li[i];
  }
}

inline bool invert_symmetric_pd(double *A, int n) {
  if (!cholesky_factor(A, n)) return false;
  // Solve A X = I by columns; X overwrites A in place after we copy L out.
  // Stack workspace for L (read-only during the solve).
  // Cap matches kFiniteDiffMaxDim in finite_diff.hpp.
  constexpr int kMax = 32;
  double L[kMax * kMax];
  const std::size_t nn = static_cast<std::size_t>(n) * n;
  for (std::size_t k = 0; k < nn; ++k) L[k] = A[k];
  // Build columns of A^{-1} = (L L^T)^{-1} into the rows of A first
  // (row-major), then transpose-mirror is unnecessary because A^{-1} is
  // symmetric.
  double e[kMax];
  for (int j = 0; j < n; ++j) {
    for (int i = 0; i < n; ++i) e[i] = (i == j) ? 1.0 : 0.0;
    cholesky_solve(L, e, n);
    // Write column j of A^{-1} into row j of A. Then we'll symmetrise.
    double *Aj = A + static_cast<std::size_t>(j) * n;
    for (int i = 0; i < n; ++i) Aj[i] = e[i];
  }
  // The matrix is now A^{-1} stored such that A[j*n + i] == (A^{-1})[i,j].
  // Since A^{-1} is symmetric, this also equals (A^{-1})[j,i]; transpose to
  // canonical row-major.
  for (int i = 0; i < n; ++i) {
    for (int j = i + 1; j < n; ++j) {
      double *Aij = A + static_cast<std::size_t>(i) * n + j;
      double *Aji = A + static_cast<std::size_t>(j) * n + i;
      const double tmp = *Aij;
      *Aij = *Aji;
      *Aji = tmp;
    }
  }
  return true;
}

}  // namespace cppunivariate
