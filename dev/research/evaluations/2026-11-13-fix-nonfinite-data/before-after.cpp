// TASK-214.3 defect cases on the core fitters (no adapters); API common to
// the baseline bbe1b13a and the fix. Built and run by before-after.py.
#include "cppunivariate/distributions/gamma/mle.hpp"
#include "cppunivariate/distributions/norm/mle.hpp"
#include "cppunivariate/distributions/lnorm/mle.hpp"
#include "cppunivariate/distributions/logis/mle.hpp"
#include "cppunivariate/distributions/weibull/mle.hpp"
#include "cppunivariate/distributions/gumbel/mle.hpp"
#include "cppunivariate/distributions/gompertz/mle.hpp"
#include "cppunivariate/distributions/cauchy/mle.hpp"
#include <cstdio>
#include <vector>
namespace cu = cppunivariate;
static cu::span S(const std::vector<double>& v) { return cu::span{v.data(), v.size()}; }
static const char* kind(const cu::FitOutcome& o) { return cu::outcome_kind_name(o.kind); }
static const char* why(const cu::FitOutcome& o) { return cu::outcome_reason_name(o.reason); }
int main() {
  const double nan = NAN;
  const double D = 4.9406564584124654e-324;
  {  // 1 gamma subnormal, all free
    std::vector<double> x{D, 2 * D, 3 * D};
    auto r = cu::gamma_mle(S(x), 1.220703e-4, 100, nan, nan);
    std::printf("1 gamma subnormal: st=%d %s/%s shape=%.17g scale=%a ll=%.17g\n", (int)r.status,
                kind(cu::outcome(r)), why(cu::outcome(r)), r.shape, r.scale, r.logLik);
  }
  {  // 2 norm near-constant
    std::vector<double> x{1, 1, 1 + 0x1p-52};
    auto r = cu::norm_mle(S(x));
    std::printf("2 norm near-const: st=%d mean=%.17g sd=%.17g (MLE %.17g) ll=%.17g\n", (int)r.status, r.mean,
                r.sd, 0x1p-52 * std::sqrt(2.0) / 3, r.logLik);
    auto l = cu::lognormal_mle(S(std::vector<double>{std::exp(1.0), std::exp(1.0), std::nextafter(std::exp(1.0), 9.0)}));
    std::printf("  lnorm analogue: st=%d sdlog=%.17g cert=%d\n", (int)l.status, l.sdlog, (int)l.certified);
  }
  {  // 3 logis wide, tol 1e-10; constant data
    std::vector<double> x{1e-8, 1, 1e8};
    auto r = cu::logistic_mle(S(x), 1e-10, 100);
    std::printf("3 logis wide tol1e-10: st=%d %s mu=%.17g s=%.17g ll=%.17g it=%d\n", (int)r.status,
                kind(cu::outcome(r)), r.mu, r.scale, r.logLik, r.iterations);
    auto d = cu::logistic_mle(S(x), 1.220703e-4, 100);
    std::printf("  logis wide default tol: st=%d mu=%.17g s=%.17g ll=%.17g it=%d\n", (int)d.status, d.mu, d.scale,
                d.logLik, d.iterations);
    std::vector<double> c{2, 2, 2};
    auto k = cu::logistic_mle(S(c), 1e-10, 100);
    std::printf("  logis constant: st=%d %s/%s\n", (int)k.status, kind(cu::outcome(k)), why(cu::outcome(k)));
    auto k1 = cu::logistic_mle(S(std::vector<double>{2}), 1e-10, 100);
    std::printf("  logis one: st=%d %s/%s\n", (int)k1.status, kind(cu::outcome(k1)), why(cu::outcome(k1)));
    auto kh = cu::logistic_mle(S(c), 1e-10, 100, 2.0, nan);
    std::printf("  logis constant at held location: st=%d %s/%s\n", (int)kh.status, kind(cu::outcome(kh)),
                why(cu::outcome(kh)));
  }
  {  // 4 weibull shape held at denorm_min
    std::vector<double> x{0.5, 1, 2, 4};
    auto r = cu::weibull_mle(S(x), 1e-10, 100, nan, D, nan);
    std::printf("4 weibull shape=denorm: st=%d %s scale=%.17g (sqrt2 %.17g) ll=%.17g\n", (int)r.status,
                kind(cu::outcome(r)), r.scale, std::sqrt(2.0), r.logLik);
  }
  {  // 5 weibull archive 8443, both held
    std::vector<double> x;
    for (const char* h : {"0x1.4d91d33b60d64p+500", "0x1.56ed2f1ffbd0dp+500", "0x1.7fae0540168e1p+499",
                          "0x1.15ada6d2f5b03p+499", "0x1.716897c08000fp+499", "0x1.43932c103ad40p+499",
                          "0x1.c41da698e91b3p+499", "0x1.2b09ec555a47ep+500", "0x1.755a8917d40afp+500",
                          "0x1.bf79f040d7eacp+498", "0x1.7e81ff53c34b1p+498", "0x1.8c351a591e363p+500"})
      x.push_back(std::strtod(h, nullptr));
    auto r = cu::weibull_mle(S(x), 1e-10, 100, nan, 2.9155677520153174, 3.961112786792183);
    std::printf("5 weibull 8443 held: st=%d %s/%s ll=%.17g\n", (int)r.status, kind(cu::outcome(r)),
                why(cu::outcome(r)), r.logLik);
  }
  {  // 6 gumbel archive 3008, both held
    std::vector<double> x;
    for (const char* h : {"0x1.6b51ab798eca6p+499", "-0x1.92c1ac345ade0p+494", "-0x1.5b83e24bb8ee8p+498",
                          "0x1.0a95335440d9fp+500", "0x1.2dccd5b1619dap+500", "0x1.88bb91b93e758p+500",
                          "0x1.b3aa95091712ep+499", "-0x1.8dfeed445558ep+499", "0x1.81af676c8cfdap+499",
                          "0x1.d783fe789fa5dp+497", "0x1.62e0db47c3f33p+500", "0x1.7c1201c5ee206p+501"})
      x.push_back(std::strtod(h, nullptr));
    auto r = cu::fit_gumbel_held(S(x), 1e-10, 100, 2.964923020698754, 3.021078660513882);
    std::printf("6 gumbel 3008 held: st=%d %s/%s ll=%.17g\n", (int)r.status, kind(cu::outcome(r)),
                why(cu::outcome(r)), r.logLik);
  }
  {  // 7 gompertz subnormal
    std::vector<double> x{D, 2 * D, 3 * D};
    auto r = cu::gompertz_mle(S(x), 1.220703e-4, 100, nan, nan);
    std::printf("7 gompertz subnormal: st=%d %s/%s a=%g b=%g ll=%.17g\n", (int)r.status, kind(cu::outcome(r)),
                why(cu::outcome(r)), r.a, r.b, r.logLik);
  }
  {  // 8 cauchy empty
    std::vector<double> x;
    auto r = cu::fit_cauchy(S(x), 1.220703e-4, 100, nan, nan);
    std::printf("8 cauchy empty: st=%d %s/%s\n", (int)r.status, kind(cu::outcome(r)), why(cu::outcome(r)));
    auto r1 = cu::fit_cauchy(S(std::vector<double>{3}), 1.220703e-4, 100, nan, nan);
    std::printf("  cauchy one: st=%d %s/%s\n", (int)r1.status, kind(cu::outcome(r1)), why(cu::outcome(r1)));
    auto rn = cu::fit_cauchy(S(std::vector<double>{NAN}), 1.220703e-4, 100, nan, nan);
    std::printf("  cauchy one NaN: st=%d %s/%s\n", (int)rn.status, kind(cu::outcome(rn)), why(cu::outcome(rn)));
  }
  {  // 9 gamma huge
    std::vector<double> x{2.1929206633396517e+307, 4.316903372290887e+307, 7.156392554778722e+307,
                          8.806638121904167e+307, 1.1769861760077501e+308, 1.7000000000000001e+308};
    auto r = cu::gamma_mle(S(x), 1.220703e-4, 100, nan, nan);
    std::printf("9 gamma huge: st=%d %s shape=%a scale=%a ll=%a\n", (int)r.status, kind(cu::outcome(r)), r.shape,
                r.scale, r.logLik);
  }
  {  // logis held-scale archive 4719 (offset 1e8)
    std::vector<double> x;
    for (const char* h : {"0x1.7d783f609d127p+26", "0x1.7d7840110d012p+26", "0x1.7d7840000db82p+26",
                          "0x1.7d783fc2b056ap+26", "0x1.7d7840351a593p+26", "0x1.7d783f61e5240p+26",
                          "0x1.7d7840acdca3bp+26", "0x1.7d78407da7befp+26", "0x1.7d783fce71df4p+26",
                          "0x1.7d783fdce8cc5p+26", "0x1.7d78402f7065cp+26", "0x1.7d78407729152p+26"})
      x.push_back(std::strtod(h, nullptr));
    auto r = cu::logistic_mle(S(x), 1.220703e-4, 100, nan, 1.0081430589945095);
    std::printf("L4719 logis held scale: st=%d mu=%.17g ll=%.17g it=%d\n", (int)r.status, r.mu, r.logLik,
                r.iterations);
  }
  {  // logis held-scale archive 4718 (spread 1e150): must not be Ok
    std::vector<double> x;
    for (const char* h : {"-0x1.8588474a3c164p+499", "0x1.4d5ec9a539d1fp+496", "0x1.0c3bdee89a5a3p+488",
                          "-0x1.2baecb9b165f4p+498", "0x1.038fca903d996p+498", "-0x1.82667ef7aeb8ap+499",
                          "0x1.a677329ce2dc6p+499", "0x1.3318554c44384p+499", "-0x1.e47100bd6e2b8p+497",
                          "-0x1.5709f3c33f45cp+497", "0x1.cfc127e5d264cp+497", "0x1.2338de5d01383p+499"})
      x.push_back(std::strtod(h, nullptr));
    auto r = cu::logistic_mle(S(x), 1.220703e-4, 100, nan, 1.0081430589945095);
    std::printf("L4718 logis held scale: st=%d %s mu=%.17g it=%d\n", (int)r.status, kind(cu::outcome(r)), r.mu,
                r.iterations);
  }
}
