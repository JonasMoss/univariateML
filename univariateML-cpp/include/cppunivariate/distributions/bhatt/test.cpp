#include <limits>
#include "test_support.hpp"
#include "cppunivariate/distributions/bhatt/mle.hpp"
#include <vector>
TEST_CASE("bhatt: golden and likelihood") {
  const auto g=mltest::load_distribution_golden("bhatt");
  const auto f=cppunivariate::bhatt_mle(mltest::as_span(g.data));
  REQUIRE(f.status==cppunivariate::BhattStatus::Ok);
  CHECK(f.logLik==doctest::Approx(g.logLik).epsilon(1e-7));
  double ll=0;for(double x:g.data)ll+=cppunivariate::distributions::dbhatt(x,f.mu,f.sigma,f.a,true);
  CHECK(ll==doctest::Approx(f.logLik).epsilon(1e-12));
  std::vector<double> theta={f.mu,f.sigma,f.a};
  double norm2=0;
  for(std::size_t j=0;j<theta.size();++j) {
    const double h=1e-5*(1+std::abs(theta[j]));auto plus=theta,minus=theta;
    plus[j]+=h;minus[j]-=h;double score=0;
    for(double x:g.data)score+=(cppunivariate::distributions::dbhatt(x,plus[0],plus[1],plus[2],true)-cppunivariate::distributions::dbhatt(x,minus[0],minus[1],minus[2],true))/(2*h);
    norm2+=score*score;
  }
  CHECK(std::sqrt(norm2)<1e-3);
}
TEST_CASE("bhatt: direct log tails and quantiles") {
  for(bool lower:{false,true})for(double lp:{-1e-6,-.6931471805599453,-10.,-100.,-600.}) {
    const double q=cppunivariate::distributions::qbhatt(lp,0,1,2,lower,true);
    CHECK(cppunivariate::distributions::pbhatt(q,0,1,2,lower,true)==doctest::Approx(lp).epsilon(1e-10));
  }
  for(double a:{0.,2.})for(bool lower:{false,true})for(double lp:{-1e-20,-1e-100}) {
    const double q=cppunivariate::distributions::qbhatt(lp,0,1,a,lower,true);
    CHECK(cppunivariate::distributions::pbhatt(q,0,1,a,lower,true)/lp==doctest::Approx(1).epsilon(1e-9));
  }
  CHECK(std::isinf(cppunivariate::distributions::qbhatt(0,0,1,2)));
  CHECK(std::isinf(cppunivariate::distributions::qbhatt(1,0,1,2)));
  CHECK(std::isnan(cppunivariate::distributions::dbhatt(0,0,-1,1)));
  CHECK(cppunivariate::distributions::dbhatt(0,0,1,0)==doctest::Approx(.39894228040143267794));
  CHECK(cppunivariate::distributions::pbhatt(0,0,1,0)==doctest::Approx(.5));
  CHECK(cppunivariate::distributions::qbhatt(.5,7,3,0)==doctest::Approx(7));
}
TEST_CASE("bhatt: faces, held subsets and invalid inputs") {
  const std::vector<double> uniform={-1,-.5,0,.5,1},constant={2,2,2};
  CHECK(cppunivariate::bhatt_mle(mltest::as_span(uniform)).status==cppunivariate::BhattStatus::UniformLimit);
  CHECK(cppunivariate::bhatt_mle(mltest::as_span(constant)).status==cppunivariate::BhattStatus::Unbounded);
  CHECK(cppunivariate::bhatt_mle(mltest::as_span(uniform),1e-8,500,1,2).status==cppunivariate::BhattStatus::Ok);
  CHECK(cppunivariate::bhatt_mle(mltest::as_span(uniform),1e-8,500,cppunivariate::special::kNaN,2).status==cppunivariate::BhattStatus::UniformLimit);
  CHECK(cppunivariate::bhatt_mle({}).status==cppunivariate::BhattStatus::InvalidInput);
}

#include "cppunivariate/distributions/bhatt/dpqr.hpp"

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("bhatt: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("bhatt",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dbhatt(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pbhatt(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qbhatt(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rbhatt(rng, p[0], p[1], p[2]); });
}

TEST_CASE("bhatt: every held golden subset preserves coefficients and likelihood") {
  const auto g=mltest::load_distribution_golden("bhatt");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(int mask=1;mask<8;++mask) {
    CAPTURE(mask);
    const auto r=cppunivariate::bhatt_mle(mltest::as_span(g.data),1e-10,1000,((mask & 2) ? g.estimates.at("sigma") : nan),((mask & 4) ? g.estimates.at("a") : nan),((mask & 1) ? g.estimates.at("mu") : nan));
    if(mask & 1)CHECK(r.mu==g.estimates.at("mu"));
    if(mask & 2)CHECK(r.sigma==g.estimates.at("sigma"));
    if(mask & 4)CHECK(r.a==g.estimates.at("a"));
    CHECK(r.status==cppunivariate::BhattStatus::Ok);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*g.data.size());
  }
}


// mpmath generator: import mpmath as mp; mp.mp.dps=100
// N=lambda z: mp.erfc(-z/mp.sqrt(2))/2; H=lambda z: z*N(z)+mp.exp(-z*z/2)/mp.sqrt(2*mp.pi); F=lambda x: (H(x+2)-H(x-2))/4; mp.findroot(lambda x: F(x)-mp.exp(mp.mpf(float(-mp.log(2)))),0)
TEST_CASE("bhatt: logarithmic quantile regression") {
  CHECK(cppunivariate::distributions::qbhatt(-0.6931471805599453, 0.0, 1.0, 2.0, true, true) == doctest::Approx(4.859187962299222e-17).epsilon(1e-11));
  CHECK(cppunivariate::distributions::qbhatt(-0.6931471805599453, 0.0, 1.0, 2.0, false, true) == doctest::Approx(-4.859187962299222e-17).epsilon(1e-11));
}

TEST_CASE("bhatt: support endpoints on log input") {
  CHECK(cppunivariate::distributions::qbhatt(cppunivariate::special::kNegInf, 0.0, 1.0, 2.0, true, true) == cppunivariate::special::kNegInf);
  CHECK(cppunivariate::distributions::qbhatt(0.0, 0.0, 1.0, 2.0, false, true) == cppunivariate::special::kNegInf);
  CHECK(cppunivariate::distributions::qbhatt(cppunivariate::special::kNegInf, 0.0, 1.0, 2.0, false, true) == cppunivariate::special::kPosInf);
  CHECK(cppunivariate::distributions::qbhatt(0.0, 0.0, 1.0, 2.0, true, true) == cppunivariate::special::kPosInf);
}

// At x=mp.mpf(float(1e100)), evaluate the same mpmath tails and
// the README log density; constants are also in the independent truth table.
TEST_CASE("bhatt: extreme observation log tails and density") {
  CHECK(cppunivariate::distributions::dbhatt(1e100, 0.0, 1.0, 2.0, true) == doctest::Approx(-5e+199).epsilon(1e-11));
  CHECK(cppunivariate::distributions::pbhatt(1e100, 0.0, 1.0, 2.0, true, true) == doctest::Approx(0.0).epsilon(1e-11));
  CHECK(cppunivariate::distributions::pbhatt(1e100, 0.0, 1.0, 2.0, false, true) == doctest::Approx(-5e+199).epsilon(1e-11));
}

// import mpmath as mp; mp.mp.dps=100
// a=mp.mpf(float(1e308)); s=mp.mpf(float(1e-308))
// -mp.log(2*a); mp.log(s/(2*a)/mp.sqrt(2*mp.pi))
// The omitted opposite normal endpoint has log mass < -1e1230.
TEST_CASE("bhatt: convolution width stays on the log scale") {
  CHECK(cppunivariate::distributions::dbhatt(0,0,1e-308,1e308,true) == doctest::Approx(-709.889355822726).epsilon(1e-12));
  CHECK(cppunivariate::distributions::pbhatt(1e308,0,1e-308,1e308,false,true) == doctest::Approx(-1420.0045029980968).epsilon(1e-12));
  CHECK(cppunivariate::distributions::qbhatt(0.75,0,1e-308,1e308,true,false) == doctest::Approx(5e307).epsilon(1e-10));
}

// The fitter consumes dbhatt and the original precision of its score helpers.
// Tail repairs must preserve the existing golden fit, without changing mle.hpp.
TEST_CASE("bhatt: central density preserves the golden fit") {
  const auto g=mltest::load_distribution_golden("bhatt");
  const auto r=cppunivariate::bhatt_mle(mltest::as_span(g.data));
#ifdef BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
  CHECK(std::abs((r.mu-g.estimates.at("mu"))/g.estimates.at("mu")) < mltest::kBhattGoldenTol);
  CHECK(std::abs((r.sigma-g.estimates.at("sigma"))/g.estimates.at("sigma")) < mltest::kBhattGoldenTol);
  CHECK(std::abs((r.a-g.estimates.at("a"))/g.estimates.at("a")) < mltest::kBhattGoldenTol);
#else
  CHECK(r.mu == doctest::Approx(g.estimates.at("mu")).epsilon(mltest::kBhattGoldenTol));
  CHECK(r.sigma == doctest::Approx(g.estimates.at("sigma")).epsilon(mltest::kBhattGoldenTol));
  CHECK(r.a == doctest::Approx(g.estimates.at("a")).epsilon(mltest::kBhattGoldenTol));
#endif
}


TEST_CASE("bhatt P3 normal boundary is attained with conditional coefficients") {
 std::vector<double> x(20,0); x.push_back(-10); x.push_back(10);
 const auto f=cppunivariate::bhatt_mle({x.data(),x.size()});
 REQUIRE(f.status==cppunivariate::BhattStatus::Ok); CHECK(f.a==0);
 const auto n=cppunivariate::norm_mle({x.data(),x.size()});
 CHECK(f.logLik==doctest::Approx(n.logLik));
 const auto held=cppunivariate::bhatt_mle({x.data(),x.size()},1e-8,500,4,0,1);
 CHECK(held.status==cppunivariate::BhattStatus::Ok); CHECK(held.mu==1); CHECK(held.sigma==4); CHECK(held.a==0);
}

TEST_CASE("bhatt: free fit is affine equivariant") {
  const auto g=mltest::load_distribution_golden("bhatt");
  const auto original=cppunivariate::bhatt_mle(mltest::as_span(g.data));
  std::vector<double> transformed;for(double x:g.data)transformed.push_back(1.25+2.5*x);
  const auto shifted=cppunivariate::bhatt_mle(mltest::as_span(transformed));
  REQUIRE(original.status==cppunivariate::BhattStatus::Ok);
  REQUIRE(shifted.status==cppunivariate::BhattStatus::Ok);
  CHECK(shifted.mu==doctest::Approx(1.25+2.5*original.mu).epsilon(1e-5));
  CHECK(shifted.sigma==doctest::Approx(2.5*original.sigma).epsilon(1e-5));
  CHECK(shifted.a==doctest::Approx(2.5*original.a).epsilon(1e-5));
  CHECK(shifted.logLik==doctest::Approx(original.logLik-g.data.size()*std::log(2.5)).epsilon(1e-10));
}

TEST_CASE("bhatt: face comparison cannot resolve rounded extreme objectives") {
  const double magnitude=1e20;
  CHECK(cppunivariate::compare_face(-magnitude,-magnitude,2e-8)==cppunivariate::FaceComparison::NumericalTie);
  CHECK(cppunivariate::compare_face(-magnitude,-2*magnitude,2e-8)==cppunivariate::FaceComparison::FirstWins);
  CHECK(cppunivariate::compare_face(-2*magnitude,-magnitude,2e-8)==cppunivariate::FaceComparison::SecondWins);
  CHECK(cppunivariate::compare_face(-10.,-10.,2e-8)==cppunivariate::FaceComparison::FirstWins);
}

TEST_CASE("bhatt: audited huge held-parameter magnitude") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  std::vector<double> x{1,2};
  cppunivariate::span s{x.data(),x.size()};
  const auto r=cppunivariate::bhatt_mle(s,1e-8,500,1e-10,nan,0);
  CHECK((r.status==cppunivariate::BhattStatus::Ok || r.status==cppunivariate::BhattStatus::IterationFailure));
  CHECK(!(r.status==cppunivariate::BhattStatus::Ok && r.a==0));
}

TEST_CASE("bhatt: structured face payloads") {
  const std::vector<double> x{-1,-.5,0,.5,1};
  for(double width:{static_cast<double>(NAN),2.}) {
    const auto r=cppunivariate::bhatt_mle(mltest::as_span(x),1e-8,500,NAN,width);
    REQUIRE(r.stop_detail.limit_coef.size()==2);
    const double a=std::isnan(width)?1:width;
    CHECK(r.stop_detail.limit_coef[0]==-a);CHECK(r.stop_detail.limit_coef[1]==a);
    CHECK(r.stop_detail.limit_loglik==doctest::Approx(-5*std::log(2*a)));
  }
}

TEST_CASE("bhatt certificate: held width gives a concave endpoint root") {
  const std::vector<double> x{-3,-2,-1,0,.5,1,2,3};
  for(double mu:{static_cast<double>(NAN),.2}) {
    const auto f=cppunivariate::bhatt_mle(mltest::as_span(x),1e-10,500,NAN,.5,mu);
    REQUIRE(f.status==cppunivariate::BhattStatus::Ok);
    CHECK(f.a==.5);if(!std::isnan(mu))CHECK(f.mu==mu);
    // Independent direct density comparisons, including opposite ends of
    // the scale domain. The proof is concavity plus the enclosed root.
    for(double m:{-2.,0.,.2,2.})for(double s:{.01,.2,1.,3.,10.}){
      if(!std::isnan(mu)&&m!=mu)continue;
      double ll=0;for(double v:x)ll+=cppunivariate::distributions::dbhatt(v,m,s,.5,true);
      CHECK(ll<=f.logLik+1e-9);
    }
  }
}

TEST_CASE("bhatt certificate: shape curvature and endpoint derivatives") {
  const std::vector<double> x{-2,-.5,.1,1,3};
  for(double mu:{static_cast<double>(NAN),.2})for(bool held_width:{false,true}){
    const cppunivariate::bhatt_detail::CertModel model(mltest::as_span(x),mu,held_width?NAN:1.3,held_width?.7:NAN,!held_width);
    auto p=model.initial(1.2);if(model.location>=0)p[model.location]=.15;
    const auto e=cppunivariate::bhatt_detail::cert_evaluate(model,p);
    REQUIRE(e.ok);
    for(int j=0;j<model.dim;++j){
      const double h=1e-5;auto a=p,b=p;a[j]-=h;b[j]+=h;
      const auto left=cppunivariate::bhatt_detail::cert_evaluate(model,a),right=cppunivariate::bhatt_detail::cert_evaluate(model,b);
      const double g=(right.ll-left.ll)/(2*h);
      CHECK(g==doctest::Approx((e.gradient[j].lo+e.gradient[j].hi)/2).epsilon(1e-7));
      for(int i=0;i<model.dim;++i){
        const double derivative=((right.gradient[i].lo+right.gradient[i].hi)-(left.gradient[i].lo+left.gradient[i].hi))/(4*h);
        CHECK(derivative==doctest::Approx((e.hessian[i][j].lo+e.hessian[i][j].hi)/2).epsilon(1e-7));
      }
    }
  }
}

TEST_CASE("bhatt certificate: quantitative location and finite precision stops") {
  const std::vector<double> x{-3,-1,0,1,2};
  const auto f=cppunivariate::bhatt_mle(mltest::as_span(x),1e-10,500,1,2);
  REQUIRE(f.status==cppunivariate::BhattStatus::Ok);
  const auto impossible=cppunivariate::bhatt_mle(mltest::as_span(x),1e-30,500,1,2);
  CHECK(impossible.status!=cppunivariate::BhattStatus::Ok);
  REQUIRE(!impossible.stop_detail.candidates.empty());
  CHECK(std::isfinite(impossible.stop_detail.loglik_lower));
  CHECK(impossible.stop_detail.loglik_lower<=impossible.stop_detail.loglik_upper);
  const auto shortfit=cppunivariate::bhatt_mle(mltest::as_span(x),1e-10,1,1,2);
  CHECK(shortfit.status==cppunivariate::BhattStatus::IterationFailure);
}

TEST_CASE("bhatt certificate: endpoint multiplicities and conditional uniform faces") {
  const std::vector<double> x{-1,-1,1};
  const auto free=cppunivariate::bhatt_mle(mltest::as_span(x),1e-10,500,NAN,1);
  REQUIRE(free.status==cppunivariate::BhattStatus::UniformLimit);
  CHECK(free.logLik==doctest::Approx(-3*std::log(2.)+2*std::log(2./3)+std::log(1./3)).epsilon(1e-12));
  const auto held=cppunivariate::bhatt_mle(mltest::as_span(x),1e-10,500,NAN,1,0);
  REQUIRE(held.status==cppunivariate::BhattStatus::UniformLimit);
  CHECK(held.logLik==doctest::Approx(-6*std::log(2.)).epsilon(1e-12));
  CHECK(held.stop_detail.limit_coef==std::vector<double>{-1,1});
}

TEST_CASE("bhatt certificate: pointwise normal winner and input status preservation") {
  const std::vector<double> x{-.2,0,.3};
  for(double mu:{static_cast<double>(NAN),0.}){
    const auto f=cppunivariate::bhatt_mle(mltest::as_span(x),1e-10,500,1,NAN,mu);
    REQUIRE(f.status==cppunivariate::BhattStatus::Ok);CHECK(f.a==0);
    const auto n=cppunivariate::norm_mle(mltest::as_span(x),mu,1);
    CHECK(f.mu==n.mean);CHECK(f.logLik==doctest::Approx(n.logLik).epsilon(1e-12));
  }
  const std::vector<double> bad{1,INFINITY};
  CHECK(cppunivariate::bhatt_mle(mltest::as_span(bad),1e-8,500,1,0).status==cppunivariate::BhattStatus::InvalidInput);
  CHECK(cppunivariate::bhatt_mle(mltest::as_span(x),1e-8,0,1,0,0).status==cppunivariate::BhattStatus::InvalidInput);
}

TEST_CASE("bhatt certificate: frozen all-free proposal fails quantitative acceptance") {
  const auto g=mltest::load_distribution_golden("bhatt");
  const cppunivariate::bhatt_detail::CertModel model(mltest::as_span(g.data),NAN,NAN,NAN,true);
  std::array<double,3> p{};
  p[model.location]=g.estimates.at("mu")/g.estimates.at("sigma");
  p[model.scale]=1/g.estimates.at("sigma");
  p[model.shape]=g.estimates.at("a")/g.estimates.at("sigma");
  const auto checked=cppunivariate::bhatt_detail::cert_validate_root(model,p,1e-8,500);
  CHECK(!checked.accurate);
  // Independent mpmath 70-digit score root, computed from the binary64
  // sample exactly. This is a local stationary-point witness, not a proof
  // that the all-free search has excluded every competitor.
  const auto root=cppunivariate::bhatt_detail::cert_newton(model,p,1e-10,500);
  REQUIRE(root.ok);REQUIRE(root.accurate);
  double m,s,a;model.coordinates(root.center,m,s,a);
  CHECK(m==doctest::Approx(-0.00380006892159174971312956250794).epsilon(1e-8));
  CHECK(s==doctest::Approx(1.05789733329049518933051976791).epsilon(1e-10));
  CHECK(a==doctest::Approx(1.73360592386775098991319797969).epsilon(1e-10));
}

TEST_CASE("bhatt certificate: entire large-shape uniform neighbourhood bound") {
  const std::vector<double> x{-2,-1,0,.5,3};
  for(double mu:{0.,.2,4.}){
    double radius=0;for(double v:x)radius=std::max(radius,std::abs(v-mu));
    const double face=-static_cast<double>(x.size())*std::log(2*radius);
    for(double factor:{.1,.5,.9,1.,1.1,2.})for(double k:{2.*x.size(),4.*x.size(),100.*x.size()}){
      const double a=radius*factor,sigma=a/k;double ll=0;
      for(double v:x)ll+=cppunivariate::distributions::dbhatt(v,mu,sigma,a,true);
      CHECK(ll<=face+1e-11);
    }
  }
}

TEST_CASE("bhatt certificate: physical covering equality retains subtraction residuals") {
  CHECK(cppunivariate::bhatt_detail::cert_difference_compare(1.,-std::numeric_limits<double>::denorm_min(),1.)==1);
  const std::vector<double> exact{1e16,1e16+2};
  const auto face=cppunivariate::bhatt_mle(mltest::as_span(exact),1e-10,500,NAN,1);
  REQUIRE(face.status==cppunivariate::BhattStatus::UniformLimit);
  REQUIRE(face.stop_detail.limit_coef.size()==2);
  CHECK(face.stop_detail.limit_coef[0]==exact[0]);CHECK(face.stop_detail.limit_coef[1]==exact[1]);
  CHECK(face.logLik==doctest::Approx(-4*std::log(2.)).epsilon(1e-12));
  const std::vector<double> outside{-1,std::nextafter(1.,INFINITY)};
  CHECK(cppunivariate::bhatt_detail::cert_range_compare(outside[0],outside[1],1.)==1);
  const std::vector<double> collapsed{1e300,1e300};
  const auto stopped=cppunivariate::bhatt_mle(mltest::as_span(collapsed),1e-10,500,NAN,1);
  CHECK(stopped.status==cppunivariate::BhattStatus::NumericalFailure);
  REQUIRE(!stopped.stop_detail.candidates.empty());
}

TEST_CASE("bhatt certificate: supporting tangents bound an unseen width peak") {
  // An upper bound on P'' cannot give an upper chord bound. At k=90 and
  // k=110 the old positive-curvature chord expression misses this peak.
  const std::vector<double> x{10};
  cppunivariate::bhatt_detail::WidthCertificate certificate(mltest::as_span(x),1e-10,500,0,.1);
  cppunivariate::bhatt_detail::CertRoot left,right;
  REQUIRE(certificate.profile(90,left));REQUIRE(certificate.profile(110,right));
  REQUIRE(certificate.push(90,110,left.ll_upper,right.ll_upper));
  const double peak=cppunivariate::distributions::dbhatt(10,0,.1,10.25,true);
  const double invalid_chord=std::max(left.ll_upper,right.ll_upper)+std::pow(20./90,2)/8;
  CHECK(peak>invalid_chord);
  CHECK(certificate.queue.top().upper>=peak);
  for(int i=0;i<=100;++i){
    const double k=90+i*.2;
    const double ll=cppunivariate::distributions::dbhatt(10,0,.1,.1*k,true);
    CHECK(certificate.queue.top().upper>=ll);
  }
}

TEST_CASE("bhatt certificate: conditional normal RMS never fabricates collapse") {
  const std::vector<double> x{1e-200,2e-200};
  const auto normal=cppunivariate::bhatt_mle(mltest::as_span(x),1e-10,500,NAN,0,0);
  REQUIRE(normal.status==cppunivariate::BhattStatus::Ok);
  CHECK(normal.sigma/1e-200==doctest::Approx(std::sqrt(2.5)).epsilon(1e-12));
  CHECK(normal.mu==0);CHECK(normal.a==0);CHECK(std::isfinite(normal.logLik));
  const std::vector<double> tiny{0,std::numeric_limits<double>::denorm_min()};
  const auto stopped=cppunivariate::bhatt_mle(mltest::as_span(tiny),1e-10,500,NAN,0,0);
  CHECK(stopped.status==cppunivariate::BhattStatus::NumericalFailure);
  const std::vector<double> ties{1e-200,1e-200};
  CHECK(cppunivariate::bhatt_mle(mltest::as_span(ties),1e-10,500,NAN,0,1e-200).status==cppunivariate::BhattStatus::Unbounded);
}

TEST_CASE("bhatt certificate: normal member rejects an unrepresentable location root") {
  const std::vector<double> x{1e16,1e16+2};
  const auto stopped=cppunivariate::bhatt_mle(mltest::as_span(x),1e-10,500,NAN,0);
  CHECK(stopped.status==cppunivariate::BhattStatus::NumericalFailure);
  REQUIRE(!stopped.stop_detail.candidates.empty());
  CHECK(stopped.stop_detail.loglik_lower<=stopped.stop_detail.loglik_upper);
}

TEST_CASE("bhatt certificate: narrow normal intervals include cancellation conditioning") {
  for(double z:{0.,.5,2.5,-2.5})for(double k:{.1,.001,.000001}){
    const auto mass=cppunivariate::bhatt_detail::cert_logmass_range({z,z},{k,k});
    REQUIRE(mass.valid());
    const double central=static_cast<double>(cppunivariate::bhatt_detail::cert_logmass(z,k));
    CHECK(mass.lo<=central);CHECK(mass.hi>=central);
    CHECK(cppunivariate::bhatt_detail::cert_logmass_error(z,k)>0);
  }
}

TEST_CASE("bhatt certificate: a finite likelihood midpoint does not overflow") {
  const std::vector<double> x{1e154,1e154};
  const auto point=cppunivariate::bhatt_mle(mltest::as_span(x),1e-10,500,1,0,0);
  REQUIRE(point.status==cppunivariate::BhattStatus::Ok);
  CHECK(std::isfinite(point.logLik));
  CHECK(point.logLik==doctest::Approx(-1e308).epsilon(1e-14));
}

TEST_CASE("bhatt certificate: normal domination cannot round an exterior distance to sigma") {
  const double mu=-std::numeric_limits<double>::epsilon()/4;
  CHECK(cppunivariate::bhatt_detail::cert_distance_compare(1.,mu,1.)==1);
  const std::vector<double> x{1};
  const auto f=cppunivariate::bhatt_mle(mltest::as_span(x),1e-10,500,1,NAN,mu);
  // The a^2 term of log f at zero is positive for the exact input distance
  // 1+eps/4. Returning the normal member as a proved winner is incorrect.
  CHECK(!(f.status==cppunivariate::BhattStatus::Ok&&f.a==0));
}

TEST_CASE("bhatt certificate: physical coefficient accuracy survives a location shift") {
  const std::vector<double> x{.48,.49,.5,.51,.52};
  cppunivariate::bhatt_detail::CertModel model(mltest::as_span(x),NAN,.01,.005);
  model.origin=-.5;
  auto p=model.initial();p[model.location]=(.5+1e-11)/.01;
  const auto root=cppunivariate::bhatt_detail::cert_validate_root(model,p,1e-10,500);
  REQUIRE(root.ok);CHECK(!root.accurate);
}

TEST_CASE("bhatt certificate: the normal profile upper includes rounded-mean loss") {
  const std::vector<double> x{1e16,1e16+2};
  cppunivariate::bhatt_detail::WidthCertificate certificate(mltest::as_span(x),1e-10,1,NAN,1);
  certificate.run();
  const double maximum=-1-std::log(2*std::acos(-1.));
  CHECK(certificate.normal.loglik_upper<maximum-.5);
  CHECK(certificate.normal_upper>=maximum);
}

TEST_CASE("bhatt certificate: stopped witnesses are evaluated after physical recovery") {
  const std::vector<double> x{1e16,1e16+2};
  const auto f=cppunivariate::bhatt_mle(mltest::as_span(x),1e-10,100,1);
  REQUIRE(f.status!=cppunivariate::BhattStatus::Ok);
  REQUIRE(!f.stop_detail.candidates.empty());
  for(const auto& c:f.stop_detail.candidates){
    if(c.model!=cppunivariate::StopModel::None)continue;
    REQUIRE(c.coef.size()==3);
    double ll=0;for(double v:x)ll+=cppunivariate::distributions::dbhatt(v,c.coef[0],c.coef[1],c.coef[2],true);
    CHECK(c.loglik_lower<=ll);CHECK(c.loglik_upper>=ll);
  }
}

TEST_CASE("bhatt certificate: default representability checks do not invent nonexistence") {
  const std::vector<double> x{0,std::numeric_limits<double>::denorm_min()};
  const auto f=cppunivariate::bhatt_mle(mltest::as_span(x));
  CHECK(f.status==cppunivariate::BhattStatus::NumericalFailure);
  REQUIRE(!f.stop_detail.candidates.empty());
}
