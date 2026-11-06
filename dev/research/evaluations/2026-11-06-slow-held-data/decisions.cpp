// Supplemental status, iteration, restriction and structured-stop equivalence.
// Run from the worktree root; build against either include tree.
#include <cppunivariate/distributions/tlambda/mle.hpp>
#include <cppunivariate/distributions/emg/mle.hpp>
#include <cppunivariate/distributions/bhatt/mle.hpp>
#include <iostream>
#include <iomanip>
#include <random>
void detail(const cppunivariate::StopDetail& d) {
  std::cout << ' ' << int(d.limit_model) << ' ' << d.limit_loglik
    << ' ' << d.loglik_lower << ' ' << d.loglik_upper << ' ' << d.segment_loglik_upper
    << ' ' << d.budget_exhausted << ' ' << d.comparison_tie << ' ' << d.limit_coef.size();
  for(double v:d.limit_coef) std::cout << ' ' << v;
  std::cout << ' ' << d.candidates.size();
  for(const auto& c:d.candidates) {
    std::cout << ' ' << int(c.model) << ' ' << c.loglik_lower << ' ' << c.loglik_upper << ' ' << c.coef.size();
    for(double v:c.coef) std::cout << ' ' << v;
  }
}
int main() {
  std::cout << std::setprecision(17);
  std::mt19937_64 rng(13320261106ULL);
  std::normal_distribution<double> normal;
  std::vector<std::vector<double>> samples{{0,0,0,0},{-1,0,0,1},{-10,0,0,0,0,10},{1,2},
    {-1e10,0,1e10},{-1e-10,0,1e-10},{-2,-1,0,1,2}};
  for(int n:{8,32,500}) for(int j=0;j<3;++j) {
    std::vector<double> x; for(int i=0;i<n;++i)x.push_back(normal(rng)); samples.push_back(x);
  }
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(std::size_t i=0;i<samples.size();++i) for(int budget:{1,1000}) for(unsigned mask=0;mask<8;++mask) {
    const cppunivariate::span x{samples[i].data(),samples[i].size()};
    const auto t=cppunivariate::tlambda_mle(x,1e-8,budget,mask&1?.184478447547003:nan,mask&2?.04752470610558923:nan,mask&4?1.0988151244928166:nan);
    std::cout << "tlambda " << i << ' ' << budget << ' ' << mask << ' ' << int(t.status) << ' ' << t.iterations << ' ' << t.restricted << ' ' << t.mu << ' ' << t.sigma << ' ' << t.lambda << ' ' << t.logLik << '\n';
    const auto e=cppunivariate::emg_mle(x,1e-8,budget,mask&1?.17576157248036894:nan,mask&2?1.0518147225573364:nan,mask&4?1.209185930245896:nan);
    std::cout << "emg " << i << ' ' << budget << ' ' << mask << ' ' << int(e.status) << ' ' << e.iterations << ' ' << e.mu << ' ' << e.sigma << ' ' << e.lambda << ' ' << e.logLik; detail(e.stop_detail); std::cout << '\n';
    const auto b=cppunivariate::bhatt_mle(x,1e-8,budget,mask&1?1.057897350270679:nan,mask&2?1.7336058787117143:nan,mask&4?-.0038000682197418395:nan);
    std::cout << "bhatt " << i << ' ' << budget << ' ' << mask << ' ' << int(b.status) << ' ' << b.iterations << ' ' << b.mu << ' ' << b.sigma << ' ' << b.a << ' ' << b.logLik; detail(b.stop_detail); std::cout << '\n';
  }
}
