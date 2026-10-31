// Scores and information for independent observations, in README coordinates.
// No covariance, inversion, optimization or asymptotic claim is made here.
// For these four fixed-support interior models, local finite covariance of
// exponential-family sufficient statistics gives DQM and full-rank model
// information, also for each nonempty free subset. This does not prove a
// fitted estimator's asymptotics or justify misspecification inference.
// Sum(s_i s_i^T), never outer(sum(s_i)), is the OPG. H is minus the
// log-likelihood Hessian, not an optimizer approximation. Fisher is n*I_1.
// Scores transform by the parameter Jacobian. At arbitrary points observed
// information additionally needs minus score times the coordinate Hessian.
// See dev/roadmap/2026-10-30-inference-and-dqm.md in the development repo.
#pragma once
#include "span.hpp"
#include <algorithm>
#include <array>
#include <string>
#include <vector>
#include <cmath>
#include <limits>

namespace cppunivariate {
enum class InferenceStatus { Ok, InvalidParameter, InvalidObservation,
  EmptySample, InvalidSubset, NumericalFailure, Singular, IllConditioned };
enum class InferenceCoordinates { Natural };
enum class InferenceNormalization { Total };

// Active entries occupy [0, dimension), matrices are packed row-major p*p.
// free_indices maps these entries to parameter_names in README order.
// mask bit i means coefficient i is free. Held coefficients are known constants;
// selecting a target while fitting nuisance requires the full free matrix.
// Zero mask is supported (empty vectors/matrices), but data must still be valid.
// On non-Ok overall status numerical fields are unusable. Matrix statuses are
// independent diagnostics: singular/ill-conditioned J or H does not invalidate
// scores. H may be indefinite at an arbitrary point; Ok does not mean positive.
// Singular means a zero diagonal (PSD matrices) or exactly zero scaled
// determinant; IllConditioned means |scaled determinant| <= 64*epsilon.
// No pseudoinverse is ever formed. Nonfinite arithmetic is NumericalFailure.
// Finite precision can diagnose numerical rank loss even in full-rank models.
struct InferenceResult {
  InferenceStatus status = InferenceStatus::Ok;
  InferenceStatus opg_status = InferenceStatus::Ok;
  InferenceStatus observed_status = InferenceStatus::Ok;
  InferenceStatus fisher_status = InferenceStatus::Ok;
  std::array<std::string, 2> parameter_names{};
  std::array<std::size_t, 2> free_indices{};
  std::size_t parameter_count = 0, dimension = 0, observations = 0;
  unsigned free_mask = 0;
  InferenceCoordinates coordinates = InferenceCoordinates::Natural;
  InferenceNormalization normalization = InferenceNormalization::Total;
  std::string validity_conditions;
  std::array<double, 2> score{};
  std::array<double, 4> opg{}, observed{}, fisher{};
};
namespace inference_detail {
inline InferenceStatus matrix_status(const std::array<double,4>& m,
                                      std::size_t p, bool psd) {
  if (p == 0) return InferenceStatus::Ok;
  double scale = 0;
  for (std::size_t i=0; i<p*p; ++i) {
    if (!std::isfinite(m[i])) return InferenceStatus::NumericalFailure;
    scale = std::max(scale, std::abs(m[i]));
  }
  if (scale == 0 || (psd && (m[0] <= 0 || (p==2 && m[3] <= 0))))
    return InferenceStatus::Singular;
  if (p==1) return InferenceStatus::Ok;
  const double det = (m[0]/scale)*(m[3]/scale)-(m[1]/scale)*(m[2]/scale);
  if (det == 0) return InferenceStatus::Singular;
  if (std::abs(det) <= 64*std::numeric_limits<double>::epsilon())
    return InferenceStatus::IllConditioned;
  return InferenceStatus::Ok;
}
// emit(row) receives one packed score row per observation, in input order.
// It may copy into caller-owned storage or perform additional accumulation;
// it must not throw. No n*p storage is allocated internally. Invalid input
// can leave a prefix emitted: only consume output after overall status Ok.
template<class Data, class Model, class Emit>
inline InferenceResult accumulate(Data x, const Model& model, unsigned mask, Emit emit) {
  InferenceResult r;
  r.parameter_count=Model::count; r.parameter_names=model.names();
  r.free_mask=mask; r.observations=x.size();
  r.validity_conditions=model.conditions();
  if (mask >= (1u<<Model::count)) {r.status=InferenceStatus::InvalidSubset; return r;}
  for (std::size_t j=0;j<Model::count;++j)
    if(mask & (1u<<j)) r.free_indices[r.dimension++]=j;
  if (!model.valid()) {r.status=InferenceStatus::InvalidParameter; return r;}
  if (x.empty()) {r.status=InferenceStatus::EmptySample; return r;}
  const auto f=model.fisher();
  std::vector<double> row(r.dimension);
  for (auto v:x) {
    if (!model.support(v)) {r.status=InferenceStatus::InvalidObservation; return r;}
    std::array<double,2> s{};
    std::array<double,4> h{};
    model.observation(v,s,h);
    for(std::size_t j=0;j<r.dimension;++j) {
      const auto a=r.free_indices[j]; row[j]=s[a];
      if (!std::isfinite(row[j])) {r.status=InferenceStatus::NumericalFailure; return r;}
      r.score[j]+=s[a];
      for(std::size_t k=0;k<r.dimension;++k) {
        const auto b=r.free_indices[k], dst=j*r.dimension+k;
        r.opg[dst]+=s[a]*s[b]; r.observed[dst]+=h[a*Model::count+b];
        r.fisher[dst]+=f[a*Model::count+b];
      }
    }
    emit(row);
  }
  for(std::size_t j=0;j<r.dimension;++j)
    if(!std::isfinite(r.score[j])) r.status=InferenceStatus::NumericalFailure;
  r.opg_status=matrix_status(r.opg,r.dimension,true);
  r.observed_status=matrix_status(r.observed,r.dimension,false);
  r.fisher_status=matrix_status(r.fisher,r.dimension,true);
  if(r.opg_status==InferenceStatus::NumericalFailure ||
     r.observed_status==InferenceStatus::NumericalFailure ||
     r.fisher_status==InferenceStatus::NumericalFailure) r.status=InferenceStatus::NumericalFailure;
  return r;
}
struct Discard { void operator()(const std::vector<double>&) const {} };
} // namespace inference_detail
} // namespace cppunivariate
