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
#include <type_traits>

namespace cppunivariate {
enum class InferenceStatus { Ok, InvalidParameter, InvalidObservation,
  EmptySample, InvalidSubset, NumericalFailure, Singular, IllConditioned, DimensionExceeded };
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
// See matrix_status for eigenvalue-based numerical thresholds.
// No pseudoinverse is ever formed. Nonfinite arithmetic is NumericalFailure.
// Finite precision can diagnose numerical rank loss even in full-rank models.
inline constexpr std::size_t inference_max_parameters = 8;
struct InferenceResult {
  InferenceStatus status = InferenceStatus::Ok;
  InferenceStatus opg_status = InferenceStatus::Ok;
  InferenceStatus observed_status = InferenceStatus::Ok;
  InferenceStatus fisher_status = InferenceStatus::Ok;
  std::array<std::string, inference_max_parameters> parameter_names{};
  std::array<std::size_t, inference_max_parameters> free_indices{};
  std::size_t parameter_count = 0, dimension = 0, observations = 0;
  double opg_condition = 1, observed_condition = 1, fisher_condition = 1;
  unsigned free_mask = 0;
  InferenceCoordinates coordinates = InferenceCoordinates::Natural;
  InferenceNormalization normalization = InferenceNormalization::Total;
  std::string validity_conditions;
  std::array<double, inference_max_parameters> score{};
  std::array<double, inference_max_parameters*inference_max_parameters> opg{}, observed{}, fisher{};
};
namespace inference_detail {
// Symmetric Jacobi eigenvalues of max-entry-scaled free blocks.
// Condition is max |eigenvalue| / min |eigenvalue| in README units.
// Numerical singularity: min/max <= 64 epsilon; ill conditioning: <= sqrt(epsilon).
template<std::size_t N>
inline InferenceStatus matrix_status(const std::array<double,N>& m,
                                      std::size_t p, bool psd, double& condition) {
  condition=1;
  if(p==0) return InferenceStatus::Ok;
  double scale=0;
  for(std::size_t i=0;i<p*p;++i) {
    if(!std::isfinite(m[i])) {condition=INFINITY; return InferenceStatus::NumericalFailure;}
    scale=std::max(scale,std::abs(m[i]));
  }
  if(scale==0) {condition=INFINITY; return InferenceStatus::Singular;}
  std::array<double,N> a{};
  for(std::size_t i=0;i<p*p;++i) a[i]=m[i]/scale;
  bool converged=false;
  for(std::size_t iter=0;iter<128*p*p;++iter) {
    std::size_t u=0,v=0; double largest=0;
    for(std::size_t j=0;j<p;++j) for(std::size_t k=j+1;k<p;++k)
      if(std::abs(a[j*p+k])>largest) {largest=std::abs(a[j*p+k]);u=j;v=k;}
    if(largest<=std::numeric_limits<double>::epsilon()) {converged=true;break;}
    const double angle=.5*std::atan2(2*a[u*p+v],a[v*p+v]-a[u*p+u]);
    const double c=std::cos(angle),s=std::sin(angle);
    const double au=a[u*p+u],av=a[v*p+v],off=a[u*p+v];
    for(std::size_t k=0;k<p;++k) if(k!=u && k!=v) {
      const double x=a[k*p+u],y=a[k*p+v];
      a[k*p+u]=a[u*p+k]=c*x-s*y;
      a[k*p+v]=a[v*p+k]=s*x+c*y;
    }
    a[u*p+u]=c*c*au-2*c*s*off+s*s*av;
    a[v*p+v]=s*s*au+2*c*s*off+c*c*av;
    a[u*p+v]=a[v*p+u]=0;
  }
  if(!converged) {condition=INFINITY; return InferenceStatus::NumericalFailure;}
  double lo=INFINITY,hi=0;
  for(std::size_t j=0;j<p;++j) {
    if(psd && a[j*p+j]<-64*std::numeric_limits<double>::epsilon())
      return InferenceStatus::NumericalFailure;
    lo=std::min(lo,std::abs(a[j*p+j])); hi=std::max(hi,std::abs(a[j*p+j]));
  }
  condition=lo==0 ? INFINITY : hi/lo;
  if(lo<=64*std::numeric_limits<double>::epsilon()*hi) return InferenceStatus::Singular;
  if(lo<=std::sqrt(std::numeric_limits<double>::epsilon())*hi) return InferenceStatus::IllConditioned;
  return InferenceStatus::Ok;
}
struct Discard { template<class Row> void operator()(const Row&) const {} };
// emit(row) receives one packed score row per observation, in input order.
// It may copy into caller-owned storage or perform additional accumulation;
// it must not throw. No n*p storage is allocated internally. Invalid input
// can leave a prefix emitted: only consume output after overall status Ok.
template<class Data, class Model, class Emit>
inline InferenceResult accumulate(Data x, const Model& model, unsigned mask, Emit emit) {
  InferenceResult r;
  r.parameter_count=Model::count;
  if constexpr(Model::count > inference_max_parameters) {
    r.status=InferenceStatus::DimensionExceeded; return r;
  }
  else {
  const auto names=model.names();
  for(std::size_t j=0;j<Model::count;++j) r.parameter_names[j]=names[j];
  r.free_mask=mask; r.observations=x.size();
  r.validity_conditions=model.conditions();
  if (mask >= (1u<<Model::count)) {r.status=InferenceStatus::InvalidSubset; return r;}
  for (std::size_t j=0;j<Model::count;++j)
    if(mask & (1u<<j)) r.free_indices[r.dimension++]=j;
  if (!model.valid()) {r.status=InferenceStatus::InvalidParameter; return r;}
  if (x.empty()) {r.status=InferenceStatus::EmptySample; return r;}
  const auto f=model.fisher();
  // Preserve legacy vector callbacks; the default path has stack-only rows.
  std::conditional_t<std::is_same<Emit,Discard>::value,
    std::array<double,inference_max_parameters>,std::vector<double>> row{};
  if constexpr(!std::is_same<Emit,Discard>::value) row.resize(r.dimension);
  for (auto v:x) {
    if (!model.support(v)) {r.status=InferenceStatus::InvalidObservation; return r;}
    std::array<double, (Model::count<2 ? 2 : Model::count)> s{};
    std::array<double, (Model::count<2 ? 4 : Model::count*Model::count)> h{};
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
  r.opg_status=matrix_status(r.opg,r.dimension,true,r.opg_condition);
  r.observed_status=matrix_status(r.observed,r.dimension,false,r.observed_condition);
  r.fisher_status=matrix_status(r.fisher,r.dimension,true,r.fisher_condition);
  if(r.opg_status==InferenceStatus::NumericalFailure ||
     r.observed_status==InferenceStatus::NumericalFailure ||
     r.fisher_status==InferenceStatus::NumericalFailure) r.status=InferenceStatus::NumericalFailure;
  return r;
  }
}

} // namespace inference_detail
} // namespace cppunivariate
