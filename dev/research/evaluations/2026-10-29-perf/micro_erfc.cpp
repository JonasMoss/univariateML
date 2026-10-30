#include <cmath>
#include <chrono>
#include <cstdio>
#include <vector>
int main() {
  const int N = 2000000;
  std::vector<double> xs(N);
  for (int i = 0; i < N; ++i) xs[i] = -3.0 + 6.0 * i / N;
  volatile double sink_d = 0;
  volatile long double sink_l = 0;

  auto t0 = std::chrono::steady_clock::now();
  double accd = 0;
  for (int i = 0; i < N; ++i) accd += std::erfc(xs[i]);
  auto t1 = std::chrono::steady_clock::now();
  long double accl = 0;
  for (int i = 0; i < N; ++i) accl += std::erfc((long double)xs[i]);
  auto t2 = std::chrono::steady_clock::now();
  double accd2 = 0;
  for (int i = 0; i < N; ++i) accd2 += std::exp(xs[i]) + std::log1p(xs[i]*0.1);
  auto t3 = std::chrono::steady_clock::now();

  sink_d = accd; sink_l = accl; sink_d = accd2;
  std::printf("double erfc: %.4f s (%.2f ns/call)\n", std::chrono::duration<double>(t1-t0).count(), std::chrono::duration<double>(t1-t0).count()/N*1e9);
  std::printf("long double erfc: %.4f s (%.2f ns/call)\n", std::chrono::duration<double>(t2-t1).count(), std::chrono::duration<double>(t2-t1).count()/N*1e9);
  std::printf("exp+log1p double: %.4f s (%.2f ns/call pair)\n", std::chrono::duration<double>(t3-t2).count(), std::chrono::duration<double>(t3-t2).count()/N*1e9);
  return (int)(sink_d+sink_l);
}
