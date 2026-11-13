// Round trip q(p(k)) == k for discrete families: how often does the exact
// comparison F(q) >= p lose a tie? (TASK-215)
#include <cmath>
#include <cstdio>
#include "cppunivariate/distributions/pois/dpqr.hpp"
#include "cppunivariate/distributions/binom/dpqr.hpp"
#include "cppunivariate/distributions/geom/dpqr.hpp"
#include "cppunivariate/distributions/nbinom/dpqr.hpp"
#include "cppunivariate/distributions/hyper/dpqr.hpp"
namespace d = cppunivariate::distributions;
struct Count { long total=0, up=0, down=0, other=0; };
template <class P, class Q> void trip(Count& c, int lo, int hi, P p, Q q) {
  for (int k = lo; k <= hi; ++k) {
    const double F = p(k);
    if (!(F > 0) || !(F < 0.999)) continue;
    if (k > lo && p(k - 1) == F) continue;   // numerically flat: k is not identifiable
    const double back = q(F);
    ++c.total;
    if (back == k) continue;
    if (back == k + 1) ++c.up; else if (back < k) ++c.down; else ++c.other;
  }
}
static void show(const char* name, const Count& c) {
  std::printf("%-8s cases %7ld  q(p(k)) = k+1: %5ld (%.2f%%)  below k: %ld  other: %ld\n", name, c.total, c.up, 100.0 * c.up / c.total, c.down, c.other);
}
int main() {
  Count pois, binom, geom, nbinom, hyper, hyper_upper;
  for (double lambda : {0.1, 0.5, 1.0, 2.5, 7.0, 20.0, 55.5, 130.0})
    trip(pois, 0, 300, [&](int k) { return d::ppois(k, lambda, true, false); }, [&](double p) { return d::qpois(p, lambda, true, false); });
  for (double size : {1.0, 2.0, 5.0, 10.0, 31.0, 100.0})
    for (double prob : {0.01, 0.1, 0.25, 0.3, 0.5, 0.75, 0.9})
      trip(binom, 0, int(size) - 1, [&](int k) { return d::pbinom(k, size, prob, true, false); }, [&](double p) { return d::qbinom(p, size, prob, true, false); });
  for (double prob : {0.01, 0.1, 0.25, 0.3, 0.5, 0.75, 0.9})
    trip(geom, 0, 300, [&](int k) { return d::pgeom(k, prob, true, false); }, [&](double p) { return d::qgeom(p, prob, true, false); });
  for (double size : {0.5, 1.0, 2.0, 5.5, 20.0})
    for (double prob : {0.05, 0.25, 0.3, 0.5, 0.75, 0.9})
      trip(nbinom, 0, 300, [&](int k) { return d::pnbinom(k, size, prob, true, false); }, [&](double p) { return d::qnbinom(p, size, prob, true, false); });
  for (int N : {10, 20, 50, 200})
    for (int m : {1, N / 5, N / 2, N - 3})
      for (int k : {1, N / 4, N / 2, N - 2}) {
        if (m < 1 || k < 1) continue;
        trip(hyper, 0, k, [&](int x) { return d::phyper(x, m, N, k, true, false); }, [&](double p) { return d::qhyper(p, m, N, k, true, false); });
      }
  show("pois", pois); show("binom", binom); show("geom", geom); show("nbinom", nbinom); show("hyper", hyper);
  std::printf("qhyper(0.5; m=5, N=10, k=5) = %g (exact 2)\n", d::qhyper(0.5, 5, 10, 5, true, false));
  std::printf("qbinom(0.5; 1, 0.5) = %g (exact 0)   qbinom(0.75; 2, 0.5) = %g (exact 1)\n", d::qbinom(0.5, 1, 0.5, true, false), d::qbinom(0.75, 2, 0.5, true, false));
}
