#include "test_support.hpp"
#include "cppunivariate/distributions/tbinom/mle.hpp"
#include <vector>
TEST_CASE("tbinom: unique mean solve and support errors") {
  const std::vector<int> x={1,2,3,4};
  auto f=cppunivariate::tbinom_mle({x.data(),x.size()},0,10,12);
  CHECK(f.status==cppunivariate::truncated_detail::Status::Ok);
  CHECK(std::isfinite(f.logLik));
  CHECK(cppunivariate::tbinom_mle({x.data(),x.size()},-INFINITY,INFINITY,1e308).status==cppunivariate::truncated_detail::Status::NumericalFailure);
  auto z=cppunivariate::tbinom_mle({x.data(),x.size()},2,10,12);
  CHECK(z.status==cppunivariate::truncated_detail::Status::InvalidInput);
  const std::vector<int> constant={1,1};
  CHECK(cppunivariate::tbinom_mle({constant.data(),constant.size()},0,10,12).status==cppunivariate::truncated_detail::Status::LowerPointMass);
}

#include "cppunivariate/distributions/tbinom/dpqr.hpp"
TEST_CASE("tbinom: density normalization and log-tail quantiles") {
  double total=0;
  for(int x=0;x<=200;++x)total+=cppunivariate::distributions::dtbinom(x, .4, 0, 10, 12);
  CHECK(total==doctest::Approx(1).epsilon(1e-12));
  for(double lp : {-1.,-10.,-100.}) {
    const double q=cppunivariate::distributions::qtbinom(lp, .4, 0, 10, 12, false, true);
    CHECK(cppunivariate::distributions::ptbinom(q, .4, 0, 10, 12, false, true)<=lp);
    CHECK(cppunivariate::distributions::ptbinom(q-1, .4, 0, 10, 12, false, true)>lp);
  }
}

TEST_CASE("tbinom: golden regression") {
  auto g=mltest::load_distribution_golden("tbinom");std::vector<int>x;
  for(double v:g.data)x.push_back(static_cast<int>(v));
  auto f=cppunivariate::tbinom_mle({x.data(),x.size()},0,10,12);
  CHECK(f.logLik==doctest::Approx(g.logLik).epsilon(mltest::kGolden_tbinom).scale(0));
  CHECK(f.parameter==doctest::Approx(g.estimates.at("prob")).epsilon(mltest::kGolden_tbinom).scale(0));
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("tbinom: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("tbinom",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dtbinom(x, p[0], p[1], p[2], p[3], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::ptbinom(x, p[0], p[1], p[2], p[3], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qtbinom(x, p[0], p[1], p[2], p[3], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rtbinom(rng, p[0], p[1], p[2], p[3]); });
}

TEST_CASE("tbinom: free bounds and fixed-probability integer tail") {
 const double nan=std::numeric_limits<double>::quiet_NaN();
 const std::vector<int> x{1,2,2,3,4},constant{3,3};
 const auto f=cppunivariate::tbinom_design_mle({x.data(),x.size()},nan,nan,8);
 CHECK(f.a==0);CHECK(f.b==4);CHECK(f.size==8);
 CHECK(f.status==cppunivariate::TbinomStatus::Ok);
 const auto h=cppunivariate::tbinom_design_mle({x.data(),x.size()},nan,nan,nan,.4);
 REQUIRE(h.status==cppunivariate::TbinomStatus::Ok);
 for(int k=4;k<100;++k)CHECK(h.logLik>=cppunivariate::tbinom_mle({x.data(),x.size()},0,4,k,.4).logLik-1e-9);
 const auto c=cppunivariate::tbinom_design_mle({constant.data(),constant.size()},nan,nan,8);
 CHECK(c.prob==.5);CHECK(c.logLik==0);
 CHECK(cppunivariate::tbinom_design_mle({constant.data(),constant.size()},-1,3,nan,.4).status==cppunivariate::TbinomStatus::UpperPointMass);
}

#include "cppunivariate/distributions/rng_std.hpp"
#include <algorithm>

// Free probability AND free size: the four previously not-implemented
// `holds:` subsets ({}, {a}, {b}, {a, b}), implemented from
// notes/evaluations/2026-10-29-p0b.md section 6 (tbinom) and
// notes/evaluations/2026-10-29-p2b-binom-report.md.

TEST_CASE("tbinom: free probability and free size, a constant sample attains likelihood one") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<int> c3{5,5,5};
  auto f=cppunivariate::tbinom_design_mle({c3.data(),c3.size()},nan,nan,nan);
  CHECK(f.status==cppunivariate::TbinomStatus::Ok);
  CHECK(f.prob==1);CHECK(f.size==5);CHECK(f.logLik==0);
  // A held lower bound that still admits size = max(x) gives the same fit.
  f=cppunivariate::tbinom_design_mle({c3.data(),c3.size()},0,nan,nan);
  CHECK(f.status==cppunivariate::TbinomStatus::Ok);
  CHECK(f.prob==1);CHECK(f.logLik==0);
  // An infeasible held lower bound (excludes the data) stops.
  CHECK(cppunivariate::tbinom_design_mle({c3.data(),c3.size()},5,nan,nan).status==cppunivariate::TbinomStatus::InvalidInput);
}

TEST_CASE("tbinom: free probability and free size attains on regular samples, all four bound subsets") {
  cppunivariate::distributions::StdRng<> rng(44);
  std::vector<int> x;
  for (int i=0;i<60;++i)
    x.push_back(static_cast<int>(cppunivariate::distributions::rtbinom(rng,.4,0,10,12)));
  const int xmin=*std::min_element(x.begin(),x.end()), xmax=*std::max_element(x.begin(),x.end());
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double held_a[]={nan,0,nan,0}, held_b[]={nan,nan,10,10};
  for (int i=0;i<4;++i) {
    const double a=held_a[i], b=held_b[i];
    const auto f=cppunivariate::tbinom_design_mle({x.data(),x.size()},a,b,nan);
    REQUIRE(f.status==cppunivariate::TbinomStatus::Ok);
    CHECK(f.prob>0);CHECK(f.prob<1);
    CHECK(f.size>=xmax);
    // Independent oracle: the already-certified fixed-size profile over
    // prob (truncated_detail::fit via tbinom_mle), brute-forced over a
    // size range well past the attained optimum.
    const double aa=std::isnan(a)?double(xmin)-1:a, bb=std::isnan(b)?double(xmax):b;
    double best=-std::numeric_limits<double>::infinity();
    for (int k=xmax;k<=xmax+2000;++k) {
      auto r=cppunivariate::tbinom_mle({x.data(),x.size()},aa,bb,k);
      if (r.status==cppunivariate::truncated_detail::Status::Ok && r.logLik>best) best=r.logLik;
    }
    CHECK(f.logLik>=best-1e-6*x.size());
    // The fixed-size profile at the fitter's own chosen size agrees exactly.
    auto at_size=cppunivariate::tbinom_mle({x.data(),x.size()},aa,bb,f.size);
    CHECK(at_size.logLik==doctest::Approx(f.logLik).epsilon(1e-9));
  }
}

TEST_CASE("tbinom: free probability and free size escapes to the truncated Poisson face on thin samples") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<int> thin{2,3,4};
  // held {}: a, b both estimated (a = min(x) - 1 = 1, b = max(x) = 4).
  auto f=cppunivariate::tbinom_design_mle({thin.data(),thin.size()},nan,nan,nan);
  CHECK(f.status==cppunivariate::TbinomStatus::TpoisLimit);
  auto tp=cppunivariate::truncated_detail::fit({thin.data(),thin.size()},1,4,-1);
  REQUIRE(tp.status==cppunivariate::truncated_detail::Status::Ok);
  CHECK(f.logLik<=tp.logLik+1e-6);
  // A wider lower conditioning window changes existence: the same thin
  // sample has an attained finite fit, strictly better than its new face.
  f=cppunivariate::tbinom_design_mle({thin.data(),thin.size()},0,nan,nan);
  CHECK(f.status==cppunivariate::TbinomStatus::Ok);
  tp=cppunivariate::truncated_detail::fit({thin.data(),thin.size()},0,4,-1);
  REQUIRE(tp.status==cppunivariate::truncated_detail::Status::Ok);
  CHECK(f.logLik>tp.logLik+1e-3);
  CHECK(cppunivariate::tbinom_design_mle({thin.data(),thin.size()},0,5,nan).status==cppunivariate::TbinomStatus::Ok);
  // Infeasible bounds (excludes an observation) stop instead of searching.
  CHECK(cppunivariate::tbinom_design_mle({thin.data(),thin.size()},3,nan,nan).status==cppunivariate::TbinomStatus::InvalidInput);
}

TEST_CASE("tbinom: Poisson-scale profile stays accurate past the inherited cap") {
  const std::vector<int> x{1,1,2,3,5,7};
  const auto face=cppunivariate::tbinom_detail::profile({x.data(),x.size()},1,10,-1);
  REQUIRE(face.status==cppunivariate::truncated_detail::Status::Ok);
  for(double N : {1e8,1e10,1e12,1e14}) {
    const auto f=cppunivariate::tbinom_detail::profile({x.data(),x.size()},1,10,N);
    REQUIRE(f.status==cppunivariate::truncated_detail::Status::Ok);
    CHECK(f.logLik==doctest::Approx(face.logLik).epsilon(1e-7));
    CHECK(N*f.parameter==doctest::Approx(face.parameter).epsilon(1e-7));
  }
}

TEST_CASE("tbinom: two-point saturated profile is attained, not a Poisson escape") {
  const std::vector<int> x{2,2,3,3};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto f=cppunivariate::tbinom_design_mle({x.data(),x.size()},1,3,nan);
  REQUIRE(f.status==cppunivariate::TbinomStatus::Ok);
  CHECK(f.size==3);
  CHECK(f.logLik==doctest::Approx(-4*std::log(2.)).epsilon(1e-12));
  const std::vector<int> huge{2000001,2000002};
  CHECK(cppunivariate::tbinom_design_mle({huge.data(),huge.size()},2000000,2000005,nan).status==cppunivariate::TbinomStatus::Ok);
  CHECK(cppunivariate::tbinom_design_mle({huge.data(),huge.size()},2000000,2000005,nan).certified);
}

TEST_CASE("tbinom attained point sampler ignores zero uniform") {
  // At success probability one the (possibly truncated) law is delta_size.
  struct ZeroRng { double runif(double,double) { return 0; } } rng;
  CHECK(cppunivariate::distributions::rtbinom(rng,1,-1,6,5)==5);
}

TEST_CASE("tbinom: finite window tail certificate enumerates the remaining sizes") {
  const std::vector<int> x{7,7,7,8,8,5,7,5,5,6,7,5,7,7,7,6};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto f=cppunivariate::tbinom_design_mle({x.data(),x.size()},nan,nan,nan);
  REQUIRE(f.status==cppunivariate::TbinomStatus::Ok);
  CHECK(f.size==8);
  const auto face=cppunivariate::tbinom_detail::profile({x.data(),x.size()},5,8,-1);
  REQUIRE(face.status==cppunivariate::truncated_detail::Status::Ok);
  CHECK(f.logLik>face.logLik+1e-9*x.size());
  // Independent enumeration through the numerical tail cutoff 327.
  for(int size=8;size<=327;++size) {
    const auto g=cppunivariate::tbinom_mle({x.data(),x.size()},4,8,size);
    REQUIRE(g.status==cppunivariate::truncated_detail::Status::Ok);
    CHECK(g.logLik<=f.logLik+1e-8);
  }
}

TEST_CASE("tbinom: expensive certificate preserves the numerical fit") {
  std::vector<int> x(101,100);x[0]=0;
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto f=cppunivariate::tbinom_design_mle({x.data(),x.size()},-1,2000,nan);
  REQUIRE(f.status==cppunivariate::TbinomStatus::Ok);
  CHECK(f.size==3350);
  CHECK(f.logLik==doctest::Approx(-421.73150333619617).epsilon(1e-10));
}

TEST_CASE("tbinom: structured stop payloads") {
  const std::vector<int> x{2,3,4};
  const auto r=cppunivariate::tbinom_design_mle({x.data(),x.size()},NAN,NAN,NAN);
  REQUIRE(r.stop_detail.limit_model==cppunivariate::StopModel::Tpois);
  REQUIRE(r.stop_detail.limit_coef.size()==3);
  CHECK(r.stop_detail.limit_coef[1]==1);
  CHECK(r.stop_detail.limit_coef[2]==4);
  const auto f=cppunivariate::truncated_detail::fit({x.data(),x.size()},1,4,-1);
  CHECK(r.stop_detail.limit_coef[0]==doctest::Approx(f.parameter));
  CHECK(r.stop_detail.limit_loglik==doctest::Approx(f.logLik));
}

TEST_CASE("tbinom: mandatory finite and infinite retained-window certificates") {
  const std::vector<int>x{1,2,2,3,3,3,4,4,5,5};
  for(double b : {10.,double(INFINITY)}) {
    const auto f=cppunivariate::tbinom_design_mle({x.data(),x.size()},0,b,NAN);
    REQUIRE(f.status==cppunivariate::TbinomStatus::Ok);
    for(int N=5;N<100;++N) {
      const auto g=cppunivariate::tbinom_mle({x.data(),x.size()},0,b,N);
      CHECK(g.logLik<=f.logLik+1e-8);
    }
  }
}

TEST_CASE("tbinom: equivalent real bounds disprove coefficient uniqueness") {
  const std::vector<int>x{1,2};
  const auto a=cppunivariate::tbinom_design_mle({x.data(),x.size()},0,2,3);
  const auto b=cppunivariate::tbinom_design_mle({x.data(),x.size()},.5,2.5,3);
  CHECK(a.logLik==doctest::Approx(b.logLik));
  CHECK(a.prob==doctest::Approx(b.prob));
}

TEST_CASE("tbinom: a very large finite upper window needs only a small retained support") {
  const std::vector<int>x{1,2,2,3,3,3,4,4,5,5};
  const auto f=cppunivariate::tbinom_design_mle({x.data(),x.size()},0,1e300,NAN);
  REQUIRE(f.status==cppunivariate::TbinomStatus::Ok);
  CHECK(f.size==6);
}

TEST_CASE("tbinom: signed face bounds cover finite and infinite upper windows") {
  const std::vector<int>finite{2,3,4},infinite{1,1,1,2,3,7,12};
  const auto a=cppunivariate::tbinom_design_mle({finite.data(),finite.size()},NAN,NAN,NAN);
  REQUIRE(a.status==cppunivariate::TbinomStatus::TpoisLimit);
  CHECK_FALSE(a.stop_detail.comparison_tie);
  CHECK(std::isfinite(a.stop_detail.loglik_upper));
  const auto b=cppunivariate::tbinom_design_mle({infinite.data(),infinite.size()},0,INFINITY,NAN);
  REQUIRE(b.status==cppunivariate::TbinomStatus::TpoisLimit);
  CHECK_FALSE(b.stop_detail.comparison_tie);
  CHECK(std::isfinite(b.stop_detail.loglik_upper));
}

TEST_CASE("tbinom: zero first-order gap can have an exact saturated-face certificate") {
  // Exactly the Poisson(rate=2) probabilities on {0,1,2}: (.2,.4,.4).
  // The sufficient first-order bound must refuse, rather than infer a sign.
  const std::vector<int>x{0,1,1,2,2};
  cppunivariate::binomial_certificate_detail::Work work{0,0,512};
  CHECK_FALSE(std::isfinite(cppunivariate::binomial_certificate_detail::signed_cutoff({x.data(),x.size()},0,2,2,work)));
  CHECK_FALSE(work.numerical);
  const auto f=cppunivariate::tbinom_design_mle({x.data(),x.size()},-1,2,NAN);
  CHECK_FALSE(f.stop_detail.comparison_tie);
  CHECK(std::isfinite(f.stop_detail.loglik_upper));
  CHECK(f.stop_detail.limit_coef[0]==2);
}

TEST_CASE("tbinom: exact saturated-face test rejects nearly matching proportions") {
  const std::vector<int>exact{0,1,1,2,2},near{0,1,1,2,2,2};
  cppunivariate::binomial_certificate_detail::Work work{0,0,512};bool budget=false;double ll=NAN;
  CHECK(cppunivariate::binomial_certificate_detail::saturated_poisson_rate({exact.data(),exact.size()},0,2,work,budget,ll)==2);
  CHECK(ll==doctest::Approx(std::log(.2)+4*std::log(.4)));
  CHECK_FALSE(std::isfinite(cppunivariate::binomial_certificate_detail::saturated_poisson_rate({near.data(),near.size()},0,2,work,budget,ll)));
  CHECK_FALSE(budget);
}

TEST_CASE("tbinom: nonsaturated first-order equality has exact factorial-moment exclusion") {
  // Same first two moments as Poisson(3) conditioned on {0,1,2,3},
  // but different proportions: (1,9,6,10) versus (2,6,9,9).
  std::vector<int>x{0};x.insert(x.end(),9,1);x.insert(x.end(),6,2);x.insert(x.end(),10,3);
  const auto f=cppunivariate::tbinom_design_mle({x.data(),x.size()},-1,3,NAN);
  CHECK_FALSE(f.stop_detail.comparison_tie);
  CHECK(std::isfinite(f.stop_detail.loglik_upper));
}

TEST_CASE("tbinom: exact factorial-moment face test has checked integer arithmetic") {
  std::vector<int>x{0};x.insert(x.end(),9,1);x.insert(x.end(),6,2);x.insert(x.end(),10,3);
  cppunivariate::binomial_certificate_detail::Work work{0,0,512};bool budget=false;double ll=NAN;
  CHECK(cppunivariate::tbinom_detail::exact_poisson_moment_face({x.data(),x.size()},3,work,budget,ll)==3);
  CHECK(std::isfinite(ll));CHECK_FALSE(budget);
  x.push_back(3);
  CHECK_FALSE(std::isfinite(cppunivariate::tbinom_detail::exact_poisson_moment_face({x.data(),x.size()},3,work,budget,ll)));
  // Factorial overflow refuses this sufficient criterion without wrapping.
  CHECK_FALSE(std::isfinite(cppunivariate::tbinom_detail::exact_poisson_moment_face({x.data(),x.size()},30,work,budget,ll)));
  CHECK_FALSE(budget);
}

TEST_CASE("tbinom: mixed higher factorial moments have a signed second-order tail") {
  std::vector<int>x(50,0);x.insert(x.end(),41,1);x.insert(x.end(),33,2);x.insert(x.end(),3,3);x.insert(x.end(),3,4);
  const auto f=cppunivariate::tbinom_design_mle({x.data(),x.size()},-1,4,NAN);
  CHECK_FALSE(f.stop_detail.comparison_tie);
  CHECK(std::isfinite(f.stop_detail.loglik_upper));
  CHECK(f.stop_detail.limit_coef[0]==1);
}

TEST_CASE("tbinom: unresolved witnesses preserve proper infinite truncation bounds") {
  auto d=cppunivariate::discrete_face(cppunivariate::StopModel::Tpois,{2,0,INFINITY},-10);
  cppunivariate::tbinom_detail::record_witness(d,{.1,0,INFINITY,20},-11,false,true);
  REQUIRE(d.candidates.size()==2);
  CHECK(d.candidates.back().coef[2]==INFINITY);
  CHECK(d.comparison_tie);CHECK(d.loglik_upper==INFINITY);
}

TEST_CASE("tbinom: second-order certificate refuses a positive coefficient") {
  std::vector<int>x(104,0);x.insert(x.end(),608,3);x.insert(x.end(),112,4);
  cppunivariate::binomial_certificate_detail::Work work{0,0,512};
  bool budget=false;double rate=NAN,ll=NAN;
  CHECK_FALSE(std::isfinite(cppunivariate::tbinom_detail::second_order_poisson_cutoff({x.data(),x.size()},4,work,budget,rate,ll)));
  CHECK_FALSE(budget);CHECK_FALSE(work.numerical);
}

#include "cppunivariate/distributions/tbinom/dpqr.hpp"
TEST_CASE("tbinom: quantile search guards and representability") {
  CHECK(std::isnan(cppunivariate::distributions::qtbinom(.5,.9,-INFINITY,INFINITY,1e308)));
}

TEST_CASE("tbinom: checked rational third-order cancellation and overflow") {
  const auto zero=cppunivariate::tbinom_detail::face_minus(
    cppunivariate::tbinom_detail::FaceRational(1,3),
    cppunivariate::tbinom_detail::face_plus(cppunivariate::tbinom_detail::FaceRational(1,6),cppunivariate::tbinom_detail::FaceRational(1,6)));
  CHECK(zero.ok);CHECK(zero.n==0);
  const auto overflow=cppunivariate::tbinom_detail::face_multiply(
    cppunivariate::tbinom_detail::FaceRational(INT64_MAX),cppunivariate::tbinom_detail::FaceRational(2));
  CHECK_FALSE(overflow.ok);
  // These rational weights have rate 6 on {0,...,4}. Both leading
  // coefficients vanish exactly; the third is -226332657/36450495095.
  const std::vector<std::uint64_t> count{21251,11790,0,244158,168771},weight{24,144,432,864,1296};
  cppunivariate::binomial_certificate_detail::Work work{0,0,512};bool budget=false;
  const double end=cppunivariate::tbinom_detail::third_order_window_cutoff(count,weight,445970,2760,work,budget);
  CHECK(std::isfinite(end));CHECK(end>=26593);CHECK(end<=26594);
  CHECK_FALSE(budget);CHECK_FALSE(work.numerical);
  // A one-cell change is not an exact second-order zero.
  auto near=count;++near[0];
  CHECK_FALSE(std::isfinite(cppunivariate::tbinom_detail::third_order_window_cutoff(near,weight,445971,2760,work,budget)));
}

TEST_CASE("tbinom: positive third-order boundary cannot become a face certificate") {
  const std::vector<std::uint64_t> count{242187,60672,121344,1488980,406274},weight{24,96,192,256,256};
  cppunivariate::binomial_certificate_detail::Work work{0,0,512};bool budget=false;
  cppunivariate::tbinom_detail::FaceRational m,aa,beta;
  CHECK(cppunivariate::tbinom_detail::zero_second_window(count,weight,2319457,824,m,aa,beta));
  CHECK_FALSE(std::isfinite(cppunivariate::tbinom_detail::third_order_window_cutoff(count,weight,2319457,824,work,budget)));
  CHECK_FALSE(budget);CHECK_FALSE(work.numerical);
  cppunivariate::binomial_certificate_detail::Work tiny{cppunivariate::enumeration_budget(1),0,1};
  CHECK_FALSE(std::isfinite(cppunivariate::tbinom_detail::third_order_window_cutoff(count,weight,2319457,824,tiny,budget)));
  CHECK(budget);
}

TEST_CASE("tbinom: exact zero first and second coefficients certify finite and face winners") {
  for(const auto& count : {std::vector<int>{242187,60672,121344,1488980,406274},std::vector<int>{21251,11790,0,244158,168771}}) {
    std::vector<int>x;for(int k=0;k<5;++k)x.insert(x.end(),count[k],k);
    const auto f=cppunivariate::tbinom_design_mle({x.data(),x.size()},-1,4,NAN);
    if(count[0]!=242187) {
      REQUIRE(f.status==cppunivariate::TbinomStatus::TpoisLimit);
      CHECK(std::isfinite(f.stop_detail.loglik_upper));
      CHECK_FALSE(f.stop_detail.comparison_tie);
      CHECK(f.stop_detail.limit_coef[0]==6);
      continue;
    }
    REQUIRE(f.status==cppunivariate::TbinomStatus::Ok);
    CHECK(f.size==7);
    // Independent 80-digit root of the finite-window binomial mean equation.
    CHECK(f.prob==doctest::Approx(.4471565933571736339834998905046751).epsilon(1e-10));
    CHECK(std::abs(f.logLik+3379063.919604254421050967086)<1e-7);
    cppunivariate::binomial_certificate_detail::Work work{0,0,512};
    std::vector<std::uint64_t> weights(count.begin(),count.end());
    const auto g=cppunivariate::tbinom_detail::compact_profile(weights,f.size,work);
    REQUIRE(g.ok);CHECK(f.prob==doctest::Approx(g.prob).epsilon(1e-10));
    for(int N=4;N<=24;++N) {
      const auto h=cppunivariate::tbinom_detail::compact_profile(weights,N,work);
      REQUIRE(h.ok);CHECK(h.ll<=f.logLik+1e-8);
    }
    // Exhaustion never accepts the initial witness as a maximizer.
    auto witness=f;cppunivariate::binomial_certificate_detail::Work tiny{cppunivariate::enumeration_budget(1),0,1};
    CHECK(cppunivariate::tbinom_detail::compact_boundary_certificate({x.data(),x.size()},4,witness,tiny));
    CHECK(witness.status!=cppunivariate::TbinomStatus::Ok);
    CHECK(witness.stop_detail.budget_exhausted);
    CHECK(witness.stop_detail.loglik_upper==INFINITY);
    // Exhaustion during prefix coverage preserves both the face and the
    // finite incumbent, plus the strongest evaluated finite competitor.
    witness=f;
    cppunivariate::binomial_certificate_detail::Work middle{
      cppunivariate::enumeration_budget(512)-static_cast<std::int64_t>(x.size())-100000,0,512};
    CHECK(cppunivariate::tbinom_detail::compact_boundary_certificate({x.data(),x.size()},4,witness,middle));
    CHECK(middle.evaluations>1);
    CHECK(witness.status!=cppunivariate::TbinomStatus::Ok);
    CHECK(witness.stop_detail.budget_exhausted);
    CHECK(witness.stop_detail.loglik_upper==INFINITY);
    CHECK(witness.stop_detail.limit_model==cppunivariate::StopModel::Tpois);
    CHECK(witness.stop_detail.candidates.size()>=3);
  }
}

TEST_CASE("tbinom: histogram certificate profiles sizes below the held upper bound") {
  cppunivariate::binomial_certificate_detail::Work work{0,0,512};
  const std::vector<std::uint64_t> count{1,1,1,1,0};
  const auto f=cppunivariate::tbinom_detail::compact_profile(count,3,work);
  REQUIRE(f.ok);
  CHECK(f.prob==doctest::Approx(.5).epsilon(1e-10));
  const double exact=2*std::log(.125)+2*std::log(.375);
  CHECK(std::abs(f.ll-exact)<1e-12);
  CHECK(f.upper>=exact-1e-12);
}

TEST_CASE("tbinom: an unrepresentable certificate window retains the finite profile") {
  const std::vector<int> x{1,1,1,1,1,12};
  const auto f=cppunivariate::tbinom_design_mle({x.data(),x.size()},0,1e100,NAN);
  REQUIRE(f.status==cppunivariate::TbinomStatus::Ok);
  CHECK_FALSE(f.certified);CHECK(std::isfinite(f.logLik));CHECK(f.b==1e100);
}

TEST_CASE("tbinom: anchored kernels cancel exactly the common conditioning factor") {
  for(double size:{20.,100.,-1.})for(int v=8;v<=12;++v) {
    const double difference=cppunivariate::tbinom_detail::kernel(v,.3,size)-cppunivariate::tbinom_detail::kernel(8,.3,size);
    CHECK(cppunivariate::tbinom_detail::anchored_kernel(v,.3,size,8)==doctest::Approx(difference).epsilon(1e-12));
  }
  const std::vector<int> x{1000000,1000001,1000003,1000008,1000011};
  const auto f=cppunivariate::tbinom_design_mle({x.data(),x.size()},999999,1000011,NAN);
  REQUIRE(f.status==cppunivariate::TbinomStatus::Ok);
  CHECK(std::isfinite(f.logLik));
}

TEST_CASE("tbinom: large held-probability search returns its budgeted finite prefix") {
  const std::vector<int>x{1000000,1000001,1000003,1000008,1000011};
  const auto f=cppunivariate::tbinom_design_mle({x.data(),x.size()},0,1000011,NAN,.4);
  REQUIRE(f.status==cppunivariate::TbinomStatus::Ok);CHECK_FALSE(f.certified);
  CHECK(f.stop_detail.budget_exhausted);CHECK(f.prob==.4);CHECK(std::isfinite(f.logLik));
}


TEST_CASE("tbinom: uncertified coefficients meet the local accuracy contract") {
  {
    const std::vector<int> x{1000000,1000001,1000003,1000008,1000011};
    const auto f=cppunivariate::tbinom_design_mle({x.data(),x.size()},NAN,NAN,NAN);
    REQUIRE(f.status==cppunivariate::TbinomStatus::Ok);
    CHECK_FALSE(f.certified);
    // The uncertified size search lands on different integers across compilers
    // (GCC 15: 1000011; GCC 13 and WASM: 11224310574). Check the local accuracy
    // at the returned size against its 80-digit reference.
    CHECK(f.a==999999.0);
    CHECK(f.b==1000011.0);
    const bool small=f.size==1000011.0, large=f.size==11224310574.0;
    REQUIRE((small || large));
    const double ref=small ? 0.9999933471248125 : 8.2524620502977025e-05;
    CHECK(std::abs(f.prob-ref)<=1e-10*ref);
  }
}
