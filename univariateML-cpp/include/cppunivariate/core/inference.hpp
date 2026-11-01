// Scores and information for independent observations, in README coordinates.
// Covariance extensions below require a successful MLE supplied by the caller.
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
  EmptySample, InvalidSubset, NumericalFailure, Singular, IllConditioned, DimensionExceeded, Indefinite,
  FamilyUnavailable, RegimeUnavailable, BoundaryOrLimit };
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

namespace cppunivariate {
// All matrices use the free-coordinate order of InferenceResult. No n factor.
enum class CovarianceKind { Fisher, Observed, Sandwich };
struct CovarianceResult {
  InferenceStatus status=InferenceStatus::Ok;
  std::size_t dimension=0;
  double condition=1;
  std::array<double,inference_max_parameters*inference_max_parameters> matrix{};
};
// Reject negative pivots separately from rank/conditioning diagnostics.
inline CovarianceResult inference_covariance(const InferenceResult& r,
    CovarianceKind kind=CovarianceKind::Fisher) {
  CovarianceResult out; out.dimension=r.dimension;
  if(r.status!=InferenceStatus::Ok) {out.status=r.status;return out;}
  const auto p=r.dimension;
  if(p>inference_max_parameters) {out.status=InferenceStatus::DimensionExceeded;return out;}
  const auto& a=kind==CovarianceKind::Fisher?r.fisher:r.observed;
  std::array<double,inference_max_parameters*inference_max_parameters> l{},inv{};
  double scale=0;
  for(std::size_t i=0;i<p*p;++i) {
    if(!std::isfinite(a[i])) {out.status=InferenceStatus::NumericalFailure;return out;}
    scale=std::max(scale,std::abs(a[i]));
  }
  if(p && scale==0) {out.status=InferenceStatus::Singular;return out;}
  for(std::size_t i=0;i<p;++i) for(std::size_t j=0;j<=i;++j) {
    if(std::abs(a[i*p+j]-a[j*p+i])>64*std::numeric_limits<double>::epsilon()*scale) {
      out.status=InferenceStatus::NumericalFailure;return out;
    }
    double v=a[i*p+j]/scale;
    for(std::size_t k=0;k<j;++k) v-=l[i*p+k]*l[j*p+k];
    if(i==j) {
      if(v<=0) {out.status=v<0?InferenceStatus::Indefinite:InferenceStatus::Singular;return out;}
      l[i*p+j]=std::sqrt(v);
    } else l[i*p+j]=v/l[j*p+j];
  }
  out.status=inference_detail::matrix_status(a,p,false,out.condition);
  if(out.status!=InferenceStatus::Ok) return out;
  for(std::size_t col=0;col<p;++col) {
    std::array<double,inference_max_parameters> y{},z{};
    for(std::size_t i=0;i<p;++i) {
      double v=i==col?1:0;
      for(std::size_t k=0;k<i;++k) v-=l[i*p+k]*y[k];
      y[i]=v/l[i*p+i];
    }
    for(std::size_t ii=p;ii>0;--ii) {
      const auto i=ii-1; double v=y[i];
      for(std::size_t k=i+1;k<p;++k) v-=l[k*p+i]*z[k];
      z[i]=v/l[i*p+i];inv[i*p+col]=z[i]/scale;
    }
  }
  for(std::size_t i=0;i<p;++i) if(!(inv[i*p+i]>0 && std::isfinite(inv[i*p+i]))) {
    out.status=InferenceStatus::NumericalFailure;return out;
  }
  out.matrix=inv;
  if(kind==CovarianceKind::Sandwich) {
    // Singular empirical meat is allowed; no inversion of J is needed.
    double condition=1;
    const auto s=inference_detail::matrix_status(r.opg,p,true,condition);
    if(s==InferenceStatus::NumericalFailure) {out.status=s;return out;}
    out.matrix.fill(0);
    for(std::size_t i=0;i<p;++i) for(std::size_t j=0;j<p;++j)
      for(std::size_t k=0;k<p;++k) for(std::size_t h=0;h<p;++h)
        out.matrix[i*p+j]+=inv[i*p+k]*r.opg[k*p+h]*inv[j*p+h];
  }
  for(std::size_t i=0;i<p*p;++i) if(!std::isfinite(out.matrix[i])) out.status=InferenceStatus::NumericalFailure;
  return out;
}
struct ContrastResult {
  InferenceStatus status=InferenceStatus::Ok;
  double variance=NAN, standard_error=NAN;
};
inline ContrastResult inference_delta(const CovarianceResult& c,
    const std::array<double,inference_max_parameters>& gradient) {
  ContrastResult r; r.status=c.status;
  if(r.status!=InferenceStatus::Ok) return r;
  if(c.dimension>inference_max_parameters) {r.status=InferenceStatus::DimensionExceeded;return r;}
  double v=0;
  for(std::size_t i=0;i<c.dimension;++i) for(std::size_t j=0;j<c.dimension;++j)
    v+=gradient[i]*c.matrix[i*c.dimension+j]*gradient[j];
  if(!(v>0 && std::isfinite(v))) {r.status=InferenceStatus::NumericalFailure;return r;}
  r.variance=v;r.standard_error=std::sqrt(v);return r;
}
struct ScalarTestResult {
  InferenceStatus status=InferenceStatus::Ok;
  double statistic=NAN;
};
inline ScalarTestResult inference_wald(double estimate,double null_value,const ContrastResult& c) {
  if(c.status!=InferenceStatus::Ok) return {c.status,NAN};
  const double z=(estimate-null_value)/c.standard_error;
  return std::isfinite(z)?ScalarTestResult{InferenceStatus::Ok,z*z}:
    ScalarTestResult{InferenceStatus::NumericalFailure,NAN};
}
// At a constrained null MLE with nuisance score zero: S' F^-1 S is Rao.
// The caller must fit nuisance under the null and retain the FULL free block.
inline ScalarTestResult inference_rao(const InferenceResult& null_fit) {
  const auto c=inference_covariance(null_fit);
  if(c.status!=InferenceStatus::Ok) return {c.status,NAN};
  double v=0;
  for(std::size_t i=0;i<c.dimension;++i) for(std::size_t j=0;j<c.dimension;++j)
    v+=null_fit.score[i]*c.matrix[i*c.dimension+j]*null_fit.score[j];
  if(!(v>=0 && std::isfinite(v))) return {InferenceStatus::NumericalFailure,NAN};
  return {InferenceStatus::Ok,v};
}
// Log likelihoods must be from successful unrestricted/constrained maximizers.
inline ScalarTestResult inference_lr(double unrestricted,double constrained) {
  const double v=2*(unrestricted-constrained);
  if(!std::isfinite(v) || v < -64*std::numeric_limits<double>::epsilon()*(1+std::abs(unrestricted)))
    return {InferenceStatus::NumericalFailure,NAN};
  return {InferenceStatus::Ok,std::max(0.0,v)};
}
} // namespace cppunivariate

namespace cppunivariate {
struct InferenceInterval {
  InferenceStatus status=InferenceStatus::Ok;
  double lower=NAN,upper=NAN;
};
// Critical values are supplied explicitly (e.g. z_.975 or chi-square_1,.95).
inline InferenceInterval inference_wald_interval(double estimate,const ContrastResult& c,double critical) {
  if(c.status!=InferenceStatus::Ok) return {c.status,NAN,NAN};
  if(!(critical>0 && std::isfinite(critical) && std::isfinite(estimate)))
    return {InferenceStatus::InvalidParameter,NAN,NAN};
  const double width=critical*c.standard_error;
  if(!std::isfinite(width) || !std::isfinite(estimate-width) || !std::isfinite(estimate+width))
    return {InferenceStatus::NumericalFailure,NAN,NAN};
  return {InferenceStatus::Ok,estimate-width,estimate+width};
}
struct TargetInferenceResult {
  InferenceStatus status=InferenceStatus::Ok;
  double estimate=NAN,null_value=NAN;
  ContrastResult uncertainty{InferenceStatus::NumericalFailure,NAN,NAN};
  ScalarTestResult wald{InferenceStatus::NumericalFailure,NAN};
  ScalarTestResult score{InferenceStatus::NumericalFailure,NAN};
  ScalarTestResult likelihood_ratio{InferenceStatus::NumericalFailure,NAN};
};
namespace inference_detail {
// The caller establishes a continuous strictly decreasing derivative on (0,inf).
// Positive and negative signs certify a unique global maximum. No capped iterate.
template<class Derivative>
inline ScalarTestResult positive_concave_root(Derivative derivative) {
  double lo=1,hi=1,fl=derivative(lo),fh=fl;
  for(int i=0;i<1000 && std::isfinite(fl) && fl<=0;++i) {lo*=.5;fl=derivative(lo);}
  for(int i=0;i<1000 && std::isfinite(fh) && fh>=0;++i) {hi*=2;fh=derivative(hi);}
  if(!(fl>0 && fh<0 && std::isfinite(fl) && std::isfinite(fh)))
    return {InferenceStatus::NumericalFailure,NAN};
  double l=std::log(lo),h=std::log(hi);
  for(int i=0;i<160 && h-l>1e-12;++i) {
    const double m=(l+h)/2, f=derivative(std::exp(m));
    if(!std::isfinite(f)) return {InferenceStatus::NumericalFailure,NAN};
    if(f>0) l=m;else h=m;
  }
  if(h-l>1e-12) return {InferenceStatus::NumericalFailure,NAN};
  return {InferenceStatus::Ok,std::exp((l+h)/2)};
}
inline TargetInferenceResult target_result(double estimate,double null_value,
    const std::array<double,inference_max_parameters>& gradient,
    const InferenceResult& fitted,const InferenceResult& constrained,
    double fitted_loglik,double constrained_loglik,CovarianceKind kind) {
  TargetInferenceResult r;r.status=InferenceStatus::Ok;r.estimate=estimate;r.null_value=null_value;
  r.uncertainty=inference_delta(inference_covariance(fitted,kind),gradient);
  r.wald=inference_wald(estimate,null_value,r.uncertainty);
  r.score=inference_rao(constrained);
  r.likelihood_ratio=inference_lr(fitted_loglik,constrained_loglik);
  // Each procedure retains its own diagnostic; overall reports any failure.
  for(auto s:{r.wald.status,r.score.status,r.likelihood_ratio.status})
    if(s!=InferenceStatus::Ok) r.status=s;
  return r;
}
// LR superlevel sets for the pilot targets are intervals: the normal/gamma
// mean profile derivative has sign(xbar-m); gamma shape profiles are concave;
// beta's concave likelihood has convex superlevel sets whose a/(a+b) image
// is an interval. Caller supplies log-coordinate or logit-coordinate target.
// This inversion is ONLY for those certified unimodal profiles, not arbitrary
// score statistics. Endpoints require sign brackets and finite evaluations.
template<class Statistic,class Transform>
inline InferenceInterval profile_lr_interval(double center,Statistic statistic,
    Transform transform,double critical) {
  if(!(critical>0 && std::isfinite(critical) && std::isfinite(center)))
    return {InferenceStatus::InvalidParameter,NAN,NAN};
  const auto middle=statistic(transform(center));
  if(middle.status!=InferenceStatus::Ok || middle.statistic>critical)
    return {InferenceStatus::NumericalFailure,NAN,NAN};
  InferenceInterval out;
  for(int side:{-1,1}) {
    double inside=center,outside=center,step=1;bool bracket=false;
    for(int iter=0;iter<12;++iter) {
      outside=center+side*step;const double t=transform(outside);
      if(!std::isfinite(t)) break;
      const auto v=statistic(t);
      if(v.status!=InferenceStatus::Ok) return {v.status,NAN,NAN};
      if(v.statistic>critical) {bracket=true;break;}
      step*=2;
    }
    if(!bracket) return {InferenceStatus::NumericalFailure,NAN,NAN};
    for(int iter=0;iter<160 && std::abs(outside-inside)>1e-10*(1+std::abs(center));++iter) {
      const double mid=(inside+outside)/2;const auto v=statistic(transform(mid));
      if(v.status!=InferenceStatus::Ok) return {v.status,NAN,NAN};
      if(v.statistic>critical) outside=mid;else inside=mid;
    }
    if(std::abs(outside-inside)>1e-10*(1+std::abs(center))) return {InferenceStatus::NumericalFailure,NAN,NAN};
    if(side<0) out.lower=transform((inside+outside)/2);else out.upper=transform((inside+outside)/2);
  }
  return out;
}
} // namespace inference_detail
} // namespace cppunivariate

// Exact and nonsmooth procedures: see inference.md for theory and hypotheses.
namespace cppunivariate {
namespace inference_detail {
inline bool valid_alpha(double alpha) { return alpha>0 && alpha<1; }
inline InferenceStatus finite_sample(span x) {
  if(x.empty()) return InferenceStatus::EmptySample;
  for(double v:x) if(!std::isfinite(v)) return InferenceStatus::InvalidObservation;
  return InferenceStatus::Ok;
}
// Finite endpoints only. Infinite bounds are reserved for exact sign intervals.
inline InferenceInterval finite_interval(double lo,double hi) {
  if(!std::isfinite(lo) || !std::isfinite(hi) || lo>hi)
    return {InferenceStatus::NumericalFailure,NAN,NAN};
  return {InferenceStatus::Ok,lo,hi};
}
inline InferenceInterval positive_interval(double lo,double hi) {
  if(!(lo>0 && hi>lo)) return {InferenceStatus::NumericalFailure,NAN,NAN};
  return finite_interval(lo,hi);
}
} // namespace inference_detail
} // namespace cppunivariate
