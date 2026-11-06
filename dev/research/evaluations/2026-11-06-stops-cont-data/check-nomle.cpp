// Compile and run independently of the standalone C++ library tests.
#include <cassert>
#include "bindings/common/distributions/sn/fit.hpp"
#include "bindings/common/distributions/ged/fit.hpp"
#include "bindings/common/distributions/genbeta/fit.hpp"
#include "bindings/common/distributions/trgamma/fit.hpp"
#include "bindings/common/distributions/invtrgamma/fit.hpp"
#include "bindings/common/distributions/gamma/fit.hpp"
#include "bindings/common/distributions/rayleigh/fit.hpp"
#include "bindings/common/distributions/sged/fit.hpp"
#include "bindings/common/distributions/unif/fit.hpp"
#include "bindings/common/distributions/fatigue/fit.hpp"
#include "bindings/common/distributions/naka/fit.hpp"
#include "bindings/common/distributions/power/fit.hpp"
#include "bindings/common/distributions/invgauss/fit.hpp"
#include "bindings/common/distributions/st/fit.hpp"
#include "bindings/common/distributions/bhatt/fit.hpp"
#include "bindings/common/distributions/sgomp/fit.hpp"
#include "bindings/common/distributions/weibull/fit.hpp"
#include "bindings/common/distributions/gev/fit.hpp"
#include "bindings/common/distributions/logis/fit.hpp"
#include "bindings/common/distributions/llogis/fit.hpp"
#include "bindings/common/distributions/foldnorm/fit.hpp"
#include "bindings/common/distributions/tnorm/fit.hpp"
#include "bindings/common/distributions/exp/fit.hpp"
#include "bindings/common/distributions/logitnorm/fit.hpp"
#include "bindings/common/distributions/invgamma/fit.hpp"
#include "bindings/common/distributions/hnorm/fit.hpp"
#include "bindings/common/distributions/emg/fit.hpp"
#include "bindings/common/distributions/lnorm/fit.hpp"
#include "bindings/common/distributions/norm/fit.hpp"
#include "bindings/common/distributions/triang/fit.hpp"
#include "bindings/common/distributions/hcauchy/fit.hpp"
#include "bindings/common/distributions/ht/fit.hpp"
#include "bindings/common/distributions/pareto/fit.hpp"
#include "bindings/common/distributions/slash/fit.hpp"
#include "bindings/common/distributions/tlambda/fit.hpp"
#include "bindings/common/distributions/gumbel/fit.hpp"
#include "bindings/common/distributions/snorm/fit.hpp"
#include "bindings/common/distributions/laplace/fit.hpp"
#include "bindings/common/distributions/beta/fit.hpp"
#include "bindings/common/distributions/gompertz/fit.hpp"
#include "bindings/common/distributions/huber/fit.hpp"
#include "bindings/common/distributions/nsbeta/fit.hpp"
#include "bindings/common/distributions/invweibull/fit.hpp"
#include "bindings/common/distributions/lgamma/fit.hpp"
int main() {
  { // gamma
  const std::vector<double> x{1,1,1};
  const auto r=umvl_binding::fit_gamma(cppunivariate::span{x.data(),x.size()});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // rayleigh
  const std::vector<double> x{0,0,0};
  const auto r=umvl_binding::fit_rayleigh(cppunivariate::span{x.data(),x.size()});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // sged
  const std::vector<double> x{1,1,1};
  const auto r=umvl_binding::fit_sged(cppunivariate::span{x.data(),x.size()});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // unif
  const std::vector<double> x{1,1,1};
  const auto r=umvl_binding::fit_unif(cppunivariate::span{x.data(),x.size()});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // fatigue
  const std::vector<double> x{1,1,1};
  const auto r=umvl_binding::fit_fatigue(cppunivariate::span{x.data(),x.size()},{},{},{true,0});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // naka
  const std::vector<double> x{1,1,1};
  const auto r=umvl_binding::fit_naka(cppunivariate::span{x.data(),x.size()});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // power
  const std::vector<double> x{1,1,1};
  const auto r=umvl_binding::fit_power(cppunivariate::span{x.data(),x.size()});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // invgauss
  const std::vector<double> x{1,1,1};
  const auto r=umvl_binding::fit_invgauss(cppunivariate::span{x.data(),x.size()});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // st
  const std::vector<double> x{1,1,1};
  const auto r=umvl_binding::fit_st(cppunivariate::span{x.data(),x.size()});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // bhatt
  const std::vector<double> x{1,1,1};
  const auto r=umvl_binding::fit_bhatt(cppunivariate::span{x.data(),x.size()});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // sgomp
  const std::vector<double> x{1,1,1};
  const auto r=umvl_binding::fit_sgomp(cppunivariate::span{x.data(),x.size()});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // weibull
  const std::vector<double> x{1,1,1};
  const auto r=umvl_binding::fit_weibull(cppunivariate::span{x.data(),x.size()});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // gev
  const std::vector<double> x{1,1,1};
  const auto r=umvl_binding::fit_gev(cppunivariate::span{x.data(),x.size()},{true,1});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // logis
  const std::vector<double> x{1,1,1};
  const auto r=umvl_binding::fit_logis(cppunivariate::span{x.data(),x.size()});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // llogis
  const std::vector<double> x{1,1,1};
  const auto r=umvl_binding::fit_llogis(cppunivariate::span{x.data(),x.size()});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // foldnorm
  const std::vector<double> x{1,1,1};
  const auto r=umvl_binding::fit_foldnorm(cppunivariate::span{x.data(),x.size()});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // tnorm
  const std::vector<double> x{1,1,1};
  const auto r=umvl_binding::fit_tnorm(cppunivariate::span{x.data(),x.size()},{},{},{true,-INFINITY},{true,INFINITY});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // exp
  const std::vector<double> x{0,0,0};
  const auto r=umvl_binding::fit_exp(cppunivariate::span{x.data(),x.size()});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // logitnorm
  const std::vector<double> x{0.5,0.5,0.5};
  const auto r=umvl_binding::fit_logitnorm(cppunivariate::span{x.data(),x.size()});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // invgamma
  const std::vector<double> x{1,1,1};
  const auto r=umvl_binding::fit_invgamma(cppunivariate::span{x.data(),x.size()});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // hnorm
  const std::vector<double> x{1,1,1};
  const auto r=umvl_binding::fit_hnorm(cppunivariate::span{x.data(),x.size()},{true,1});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // emg
  const std::vector<double> x{1,1,1};
  const auto r=umvl_binding::fit_emg(cppunivariate::span{x.data(),x.size()});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // lnorm
  const std::vector<double> x{1,1,1};
  const auto r=umvl_binding::fit_lnorm(cppunivariate::span{x.data(),x.size()});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // norm
  const std::vector<double> x{1,1,1};
  const auto r=umvl_binding::fit_norm(cppunivariate::span{x.data(),x.size()});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // triang
  const std::vector<double> x{1,1,1};
  const auto r=umvl_binding::fit_triang(cppunivariate::span{x.data(),x.size()});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // hcauchy
  const std::vector<double> x{1,1,1};
  const auto r=umvl_binding::fit_hcauchy(cppunivariate::span{x.data(),x.size()},{true,1});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // ht
  const std::vector<double> x{1,1,1};
  const auto r=umvl_binding::fit_ht(cppunivariate::span{x.data(),x.size()},{true,1});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // pareto
  const std::vector<double> x{1,1,1};
  const auto r=umvl_binding::fit_pareto(cppunivariate::span{x.data(),x.size()});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // slash
  const std::vector<double> x{1,1,1};
  const auto r=umvl_binding::fit_slash(cppunivariate::span{x.data(),x.size()});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // tlambda
  const std::vector<double> x{1,1,1};
  const auto r=umvl_binding::fit_tlambda(cppunivariate::span{x.data(),x.size()});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // gumbel
  const std::vector<double> x{1,1,1};
  const auto r=umvl_binding::fit_gumbel(cppunivariate::span{x.data(),x.size()});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // snorm
  const std::vector<double> x{1,1,1};
  const auto r=umvl_binding::fit_snorm(cppunivariate::span{x.data(),x.size()});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // laplace
  const std::vector<double> x{1,1,1};
  const auto r=umvl_binding::fit_laplace(cppunivariate::span{x.data(),x.size()});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // beta
  const std::vector<double> x{0.5,0.5,0.5};
  const auto r=umvl_binding::fit_beta(cppunivariate::span{x.data(),x.size()});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // gompertz
  const std::vector<double> x{1,1,1};
  const auto r=umvl_binding::fit_gompertz(cppunivariate::span{x.data(),x.size()});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // huber
  const std::vector<double> x{1,1,1};
  const auto r=umvl_binding::fit_huber(cppunivariate::span{x.data(),x.size()});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // nsbeta
  const std::vector<double> x{0.5,0.5,0.5};
  const auto r=umvl_binding::fit_nsbeta(cppunivariate::span{x.data(),x.size()},{},{},{true,0},{true,1});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // invweibull
  const std::vector<double> x{1,1,1};
  const auto r=umvl_binding::fit_invweibull(cppunivariate::span{x.data(),x.size()});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  { // lgamma
  const std::vector<double> x{2,2,2};
  const auto r=umvl_binding::fit_lgamma(cppunivariate::span{x.data(),x.size()});
  assert(r.error_kind=="nomle");assert(!r.error.empty());
  assert(r.candidates.empty());assert(r.limit_estimates.empty());
  }
  {
    const std::vector<double> x{1,1,1};
    const auto r=umvl_binding::fit_ged(cppunivariate::span{x.data(),x.size()});
    assert(r.error_kind=="nomle");assert(r.limit_estimates.empty());assert(r.candidates.empty());
  }
  {
    const std::vector<double> x{1,1,1};
    const auto r=umvl_binding::fit_genbeta(cppunivariate::span{x.data(),x.size()},{},{},{},{true,2});
    assert(r.error_kind=="nomle");assert(r.limit_estimates.empty());assert(r.candidates.empty());
  }
  {
    const std::vector<double> x{1,1,1};
    const auto r=umvl_binding::fit_trgamma(cppunivariate::span{x.data(),x.size()});
    assert(r.error_kind=="nomle");assert(r.limit_estimates.empty());assert(r.candidates.empty());
  }
  {
    const std::vector<double> x{1,1,1};
    const auto r=umvl_binding::fit_invtrgamma(cppunivariate::span{x.data(),x.size()});
    assert(r.error_kind=="nomle");assert(r.limit_estimates.empty());assert(r.candidates.empty());
  }
  {
    const std::vector<double> x{0,0,0,1,2};
    const auto r=umvl_binding::fit_gev({x.data(),x.size()});
    assert(r.error_kind=="nomle");assert(r.limit_estimates.empty());
  }
  {
    const std::vector<double> x{1,1,1};
    const cppunivariate::span data{x.data(),x.size()};
    assert(umvl_binding::fit_gamma(data,{},{},0,100).error_kind.empty());
    assert(umvl_binding::fit_fatigue(data,{},{},{true,0},0,100).error_kind.empty());
    assert(umvl_binding::fit_llogis(data,{},{},0,100).error_kind.empty());
  }
  {
    const std::vector<double> x{1,1,1};
    assert(umvl_binding::fit_sn({x.data(),x.size()}).error_kind=="nomle");
  }
  {
    const std::vector<double> infinite{INFINITY,INFINITY,INFINITY},negative{-1,-1,-1},mixed{0,INFINITY};
    assert(umvl_binding::fit_gompertz({infinite.data(),infinite.size()}).error_kind.empty());
    assert(umvl_binding::fit_gompertz({negative.data(),negative.size()}).error_kind.empty());
    assert(umvl_binding::fit_rayleigh({mixed.data(),mixed.size()}).error_kind.empty());
  }
  {
    const std::vector<double> x{-1e308,1e308};
    const auto r=umvl_binding::fit_emg({x.data(),x.size()});
    assert(r.error_kind.empty());assert(!r.error.empty());
  }
  {
    const std::vector<double> x{1e308,std::nextafter(1e308,INFINITY)};
    const cppunivariate::span data{x.data(),x.size()};
    assert(umvl_binding::fit_weibull(data).error_kind!="nomle");
    assert(umvl_binding::fit_invweibull(data).error_kind!="nomle");
    assert(umvl_binding::fit_lnorm(data).error_kind!="nomle");
    assert(umvl_binding::fit_fatigue(data,{},{},{true,0}).error_kind!="nomle");
  }
  {
    const std::vector<double> x{1e-200,std::nextafter(1e-200,INFINITY)};
    assert(umvl_binding::fit_genbeta({x.data(),x.size()},{},{},{},{true,1e100}).error_kind!="nomle");
  }
}
