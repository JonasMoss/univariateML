// Compile with CPPUNIVARIATE_LOGCONCAVE_TEST_HOOK; returns nonzero on failure.
#include <cppunivariate/cppunivariate.hpp>
#include "../../univariateML-cpp/tests/json_loader.hpp"
int main() {
  const auto emg=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/emg/golden.json");
  const auto bhatt=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/bhatt/golden.json");
  auto count=cppunivariate::logconcave_detail::fallback_count;
  cppunivariate::emg_mle({emg.data.data(),emg.data.size()},1e-8,1);
  if(cppunivariate::logconcave_detail::fallback_count!=count+1 || cppunivariate::logconcave_detail::fallback_reason!=cppunivariate::logconcave_detail::FallbackReason::InnerFailure)return 1;
  count=cppunivariate::logconcave_detail::fallback_count;
  cppunivariate::bhatt_mle({bhatt.data.data(),bhatt.data.size()},1e-8,1);
  if(cppunivariate::logconcave_detail::fallback_count!=count+1 || cppunivariate::logconcave_detail::fallback_reason!=cppunivariate::logconcave_detail::FallbackReason::InnerFailure)return 2;
  count=cppunivariate::logconcave_detail::fallback_count;
  cppunivariate::bhatt_mle({bhatt.data.data(),bhatt.data.size()},1e-8,1,1,1,1);
  return cppunivariate::logconcave_detail::fallback_count==count?0:3;
}
