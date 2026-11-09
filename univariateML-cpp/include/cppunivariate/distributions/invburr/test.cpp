#include "test_support.hpp"
#include "cppunivariate/cppunivariate.hpp"
#include <vector>
#include <cmath>
#include <string>
namespace {
#include "../burr/oracle-fixtures.inc"
}
TEST_CASE("invburr three parameter golden and reciprocal fit") {
 auto g = mltest::load_distribution_golden("invburr");
 auto r = cppunivariate::invburr_mle(mltest::as_span(g.data),1e-8,1000);
 REQUIRE(r.status == cppunivariate::InvBurrStatus::Ok);
 // Same reasoning as burr/test.cpp: compare at the fit's own gradtol
 // (1e-8, adapter.cpp's default), not a bit-exactness that a
 // multi-D finite-difference BFGS fit never promised.
 CHECK(r.shape1 == doctest::Approx(g.estimates.at("shape1")).epsilon(mltest::kGolden_invburr).scale(0));
 CHECK(r.shape2 == doctest::Approx(g.estimates.at("shape2")).epsilon(mltest::kGolden_invburr).scale(0));
 CHECK(r.scale == doctest::Approx(g.estimates.at("scale")).epsilon(mltest::kGolden_invburr).scale(0));
 CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_invburr).scale(0));
 // Independent full actuar multistart oracle on this ordinary sample.
 CHECK(r.logLik == doctest::Approx(-625.40647696836618).epsilon(1e-8));
 // Independent full likelihood scores, including the freely fitted scale.
 double g0=-static_cast<double>(g.data.size()), g1=g0, g2=0;
 for (double x : g.data) {
   const double z = r.shape2 * (std::log(r.scale)-std::log(x));
   const double sp = std::max(z,0.0)+std::log1p(std::exp(-std::abs(z)));
   const double sigmoid = z>=0 ? 1/(1+std::exp(-z)) : std::exp(z)/(1+std::exp(z));
   g0 += r.shape1*sp;
   g1 += -z+(r.shape1+1)*z*sigmoid;
   g2 += r.shape2*(1-(r.shape1+1)*sigmoid);
 }
 CHECK(std::sqrt(g0*g0+g1*g1+g2*g2)<1e-3);
 const double factor = .001; // All Burr values exceed 1; all inverse values are below 1.
 std::vector<double> reciprocal, scaled;
 double jacobian = 0;
 for (double x : g.data) { reciprocal.push_back(1/x); scaled.push_back(factor*x); jacobian += 2*std::log(x); }
 auto inv = cppunivariate::burr_mle(mltest::as_span(reciprocal),1e-8,1000);
 CHECK(inv.shape1 == doctest::Approx(r.shape1).epsilon(1e-5));
 CHECK(inv.shape2 == doctest::Approx(r.shape2).epsilon(1e-5));
 CHECK(inv.scale == doctest::Approx(1/r.scale).epsilon(1e-5));
 CHECK(inv.logLik == doctest::Approx(r.logLik+jacobian).epsilon(1e-8));
 auto s = cppunivariate::invburr_mle(mltest::as_span(scaled),1e-8,1000);
 CHECK(s.shape1 == doctest::Approx(r.shape1).epsilon(1e-4));
 CHECK(s.scale == doctest::Approx(factor*r.scale).epsilon(1e-4));
 CHECK(s.logLik == doctest::Approx(r.logLik-g.data.size()*std::log(factor)).epsilon(1e-8));
}
TEST_CASE("invburr limits and invalid inputs") {
 std::vector<double> w,p;
 for (int i=1;i<=200;++i) {
   double u=(i-0.5)/200.;
   w.push_back(std::pow(-std::log(1-u),0.5));
   p.push_back(std::pow(1-u,-0.5));
 }
 for (double &v : w) v=1/v; for (double &v : p) v=1/v;
 auto a=cppunivariate::invburr_mle(mltest::as_span(w),1e-8,1000);
 auto b=cppunivariate::invburr_mle(mltest::as_span(p),1e-8,1000);
 CHECK(a.status == cppunivariate::InvBurrStatus::InvWeibullBoundary);
 CHECK(b.status == cppunivariate::InvBurrStatus::PowerBoundary);
 const std::vector<double> bad = {0,1};
 CHECK(cppunivariate::invburr_mle(mltest::as_span(bad),1e-8,1000).status == cppunivariate::InvBurrStatus::InvalidInit);
}

TEST_CASE("invburr n20 actuar multistart oracle fixtures") {
 for (const auto &fixture : burr_oracle_fixtures) {
   std::vector<double> x = fixture.x;
   double jacobian=0;
   for (double &v : x) { jacobian+=2*std::log(v); v=1/v; }
   auto fit=cppunivariate::invburr_mle(mltest::as_span(x),1e-8,1000);
   CHECK(fit.logLik == doctest::Approx(fixture.logLik+jacobian).epsilon(1e-8));
   if (fixture.face=="interior") CHECK(fit.status==cppunivariate::InvBurrStatus::Ok);
   if (fixture.face=="weibull") CHECK(fit.status==cppunivariate::InvBurrStatus::InvWeibullBoundary);
   if (fixture.face=="pareto") CHECK(fit.status==cppunivariate::InvBurrStatus::PowerBoundary);
 }
}

TEST_CASE("invburr fixed scale historical regression and equivariance") {
 const auto golden = mltest::load_golden(std::string(CPPUNIV_DISTRIBUTIONS_DIR) + "/invburr/fixed-scale-golden.json");
 const auto &x = golden.data;
 auto r = cppunivariate::invburr_mle_fixed_scale(mltest::as_span(x),1.0);
 REQUIRE(r.status == cppunivariate::InvBurrStatus::Ok);
 CHECK(std::abs(r.shape1 - 1.028600546046916) < 1e-10);
 CHECK(std::abs(r.shape2 - 2.1411632236775726) < 1e-10);
 CHECK(std::abs(r.logLik - -626.1759641045593) < 1e-10);
 auto y=x; for (double &v : y) v*=3.0;
 auto s = cppunivariate::invburr_mle_fixed_scale(mltest::as_span(y),3.0);
 CHECK(s.scale == 3.0);
 CHECK(s.shape1 == doctest::Approx(r.shape1).epsilon(1e-10));
 CHECK(s.shape2 == doctest::Approx(r.shape2).epsilon(1e-10));
 CHECK(s.logLik == doctest::Approx(r.logLik-x.size()*std::log(3.0)).epsilon(1e-10));
 const std::vector<double> escape = { 1,2,3 };
 CHECK(cppunivariate::invburr_mle_fixed_scale(mltest::as_span(escape),3).status == cppunivariate::InvBurrStatus::AllLeOne);
 for (double bad : {0.0,-1.0,std::numeric_limits<double>::infinity()})
   CHECK(cppunivariate::invburr_mle_fixed_scale(mltest::as_span(x),bad).status == cppunivariate::InvBurrStatus::InvalidInit);
}

TEST_CASE("invburr fixed scale profile fallback start") {
 std::vector<double> x = {.5,2,3,4,5};
 for (double &v : x) v = 1.0/v;
 auto r = cppunivariate::invburr_mle_fixed_scale(mltest::as_span(x),1.0,1e-10);
 REQUIRE(r.status == cppunivariate::InvBurrStatus::Ok);
 CHECK(std::isfinite(r.logLik));
 double theta[2] = {r.shape1,r.shape2}, score[2];
 cppunivariate::InvBurrCost cost{mltest::as_span(x)};
 cost.gradient(theta,score);
 CHECK(std::hypot(score[0],score[1]) < 1e-8);
}

TEST_CASE("inverse burr: held-scale endpoint spike and nonconstant escape differ") {
  const std::vector<double> spike={1,1,1,1}, mixed={1,1/1.5,.5,1/3.0};
  const auto a=cppunivariate::invburr_mle_fixed_scale(mltest::as_span(spike),1);
  CHECK(a.status==cppunivariate::InvBurrStatus::Unbounded);
  CHECK(a.logLik==INFINITY);
  CHECK(cppunivariate::invburr_mle_fixed_scale(mltest::as_span(mixed),1).status==cppunivariate::InvBurrStatus::AllLeOne);
}

#include "cppunivariate/distributions/invburr/dpqr.hpp"

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("invburr: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("invburr",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dinvburr(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pinvburr(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qinvburr(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rinvburr(rng, p[0], p[1], p[2]); });
}

TEST_CASE("P2 invburr golden holds") {
 auto g=mltest::load_distribution_golden("invburr");
 const double nan=std::numeric_limits<double>::quiet_NaN();
for(int mask=1;mask<8;++mask){
 double a=mask&1?g.estimates.at("shape1"):nan,b=mask&2?g.estimates.at("shape2"):nan,s=mask&4?g.estimates.at("scale"):nan;
 auto r=cppunivariate::invburr_mle_holds(mltest::as_span(g.data),a,b,s);
 REQUIRE(r.status==cppunivariate::InvBurrStatus::Ok);
 if(mask&1)CHECK(r.shape1==a);
 if(mask&2)CHECK(r.shape2==b);
 if(mask&4)CHECK(r.scale==s);
 CHECK(r.logLik>=g.logLik-1e-5);
 }
}


// mpmath reproduction: import mpmath as mp; mp.mp.dps=1200
// F=lambda x: (1+x**-4)**-2; Q=lambda l: mp.expm1(-mp.log1p(-mp.exp(l))/2)**mp.mpf("-.25")
// Evaluate log tails at the stated decimal x; log densities follow mp.log(mp.diff(F,x))
// (use -mp.diff(S,x) for survival definitions); Q uses the indicated log tail.
TEST_CASE("invburr: deep tails and log inversion regressions") {
  CHECK(cppunivariate::distributions::pinvburr(1e100,2,4,1,false,true) == doctest::Approx(std::log(2.0)-400*std::log(10.0)).scale(0.0));
  CHECK(cppunivariate::distributions::dinvburr(1e-100,2,4,1,true) == doctest::Approx(std::log(8.0)-700*std::log(10.0)).scale(0.0));
  CHECK(cppunivariate::distributions::qinvburr(-1000,2,4,1,false,true) == doctest::Approx(std::exp((1000+std::log(2.0))/4)).scale(0.0));
  CHECK(cppunivariate::distributions::qinvburr(-INFINITY,2,4,1,false,true) == INFINITY);
  CHECK(cppunivariate::distributions::dinvburr(0,2,.5,3,true) == doctest::Approx(-std::log(3.0)).scale(0.0));
  // mp.exp(-(-mp.log(mp.exp(-1))/mp.mpf('1e-310'))/mp.mpf('1e308'))
  CHECK(cppunivariate::distributions::qinvburr(-1,1e-310,1e308,1,true,true) == doctest::Approx(std::exp(-100.0)).scale(0.0).epsilon(1e-10));

  // mp.log((1+mp.mpf('1e100')**mp.mpf('1e308'))**(-mp.mpf('1e-308')))
  CHECK(cppunivariate::distributions::pinvburr(1e-100,1e-308,1e308,1,true,true) == doctest::Approx(-100*std::log(10.0)).scale(0.0).epsilon(1e-10));

}

TEST_CASE("invburr: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("invburr");
  const auto x=mltest::as_span(g.data);
  CHECK(cppunivariate::invburr_mle(x,1e-8,1).status==cppunivariate::InvBurrStatus::IterationFailure);
}

TEST_CASE("invburr: default multistart retains the seed-3 stationary fit") {
  // R: set.seed(3); x <- 1/rburr(200,shape1=1,shape2=2,scale=3).
  // Frozen binary64 sample, independent of future sampler changes.
  const std::vector<double> x={
    0.741688014891710989,0.162742108055142692,0.421345936271497579,0.477406422281652754,
    0.270975976283957976,0.269680819300228858,0.883398020579089671,0.515797384896728439,
    0.285048184671344684,0.254915696066296771,0.325416717038104431,0.330000715896499042,
    0.311365309722428429,0.297121095935807156,0.130034441837987153,0.151012245012127816,
    0.941199094950118775,0.216303187275769998,0.112655175739447638,0.534877258719839821,
    0.613014846709047045,2.671496240204530803,0.866220804105916176,1.038626769080067680,
    0.598280641709360794,0.171265534124322327,0.272317757874774136,0.104733924537986633,
    0.295213953323138989,0.189522230124077595,0.426526838820007637,0.431913973600150769,
    0.735777235194668200,0.366061458302545717,0.564678863553262578,0.468310985329061402,
    0.117436582063485104,0.662638788458287276,0.284128480853972465,0.651170962959894517,
    0.532581943417100856,0.173784547577237797,0.728750863156907847,0.289076732276919268,
    0.392290165639576249,0.551423539172993582,1.487592034017871434,0.981069949664726937,
    0.492656559642663305,0.166332712865110949,0.611067314054500765,0.640734825528104346,
    0.124775209118210439,0.027536424385910564,0.143173341071141602,0.104548862569710105,
    0.353070195265534403,0.619673969273430658,0.870748647760348549,0.534942361492520058,
    0.158230118167905054,1.348134342593400703,0.165191766206665480,0.976368571021297926,
    0.183923600122491204,0.503402441567145420,0.182545052498615523,0.307246889616340058,
    0.442167174241136551,1.043719997629775964,0.187442831980658753,0.186896117299941544,
    0.109087075054625809,0.062266214638520577,0.323312798021659686,0.301827806204955806,
    0.753362322111301497,0.750958393937787738,0.173757879541962201,0.191878681235889831,
    0.174852633714452893,0.242263091059690377,0.427495575454651222,3.585889308271696940,
    0.072079586163397713,0.146226866554974855,0.639920834997773968,0.336876458717515592,
    0.252041315496103702,0.097563994100169804,3.057769125233842367,0.551732119416882938,
    0.379448598738340248,0.151141337008857374,0.128313589786602567,0.575709173121018436,
    0.481087986051465188,0.501712188371929990,0.701305522751076160,0.228676655119025130,
    0.184061520468890100,0.227545216838765080,0.648219493855853313,0.212028647993133007,
    0.269171218641144627,0.463842286399996528,1.608634405856435912,0.406761545728254625,
    1.137671989965797659,0.494352657536277662,0.480301332035108219,1.143122504102237702,
    0.792808886437902705,0.786030857470783051,0.102622452606346254,0.217047978680133230,
    0.155883599741443279,0.236287763137149248,0.634706003167317734,0.358939201469187008,
    0.209493051220972420,0.155014970247514233,0.155781111261889454,0.666782205770261149,
    0.271522146372089490,0.383908074812956746,0.251320807013481196,0.391693801139025077,
    0.123167222714885324,0.485100959183527802,0.491195076695315314,0.337646513332478915,
    0.466841475028485609,0.622594259082339452,0.150728408050614215,0.308584817114471832,
    1.007302688182690753,0.950140825342854933,0.287247548759594462,0.308902595416320869,
    0.341770082472803438,0.486773542143898341,0.228777825543668462,0.133609956262175811,
    0.141601042858313514,0.551914255421627975,0.269111949401623807,0.150346261088014660,
    0.276917555426248352,0.669093682422713720,0.180816696945818395,0.881412551739943928,
    0.118255234653588656,0.498064954178169927,0.244990764320998561,0.558873583898594317,
    0.759350672637270852,0.970569558161090029,0.377825976291026266,1.665334037588084293,
    0.069205188156543349,0.328211259002569256,0.442227702302615877,0.123395364371068281,
    0.190153389395935957,0.187569143798548915,0.117426158825775645,0.710615421981034112,
    0.173677342930034923,0.303113862205005580,0.335040485197193427,0.954101321242205702,
    0.279752050717956224,0.112756598433020966,0.693470281898009411,0.313915996540867381,
    0.233965965256680564,0.175518113652330693,0.693424949994049467,0.291649341197408762,
    0.673828005990476098,0.500332088439181755,0.760684627788774992,1.009451251928883631,
    0.565935772808707771,0.462757160169343651,0.137798832749407213,0.886257024703988927,
    0.456317163341905141,0.481248139898206917,0.352649827390784432,0.139370211139346611,
    0.483663519609697001,0.296574805578218248,0.215252674682348183,0.416234925216194318,
    0.153927364519519244,0.107033598943880870,0.394040132434863632,0.535675515212924225,
  };
  const auto r=cppunivariate::invburr_mle(mltest::as_span(x),1e-8,1000);
  REQUIRE(r.status==cppunivariate::InvBurrStatus::Ok);
  CHECK(r.shape1==doctest::Approx(1.1998579109829286).epsilon(1e-6));
  CHECK(r.shape2==doctest::Approx(2.1538589870062492).epsilon(1e-6));
  CHECK(r.scale==doctest::Approx(1/3.2485471451035055).epsilon(1e-6));
  CHECK(r.logLik==doctest::Approx(-20.609810876161703).epsilon(1e-10));
  std::vector<double> logs;
  double center=0;
  for(double v:x){logs.push_back(std::log(1/v));center+=logs.back()/x.size();}
  for(double& v:logs)v-=center;
  cppunivariate::BurrProfileCost cost{mltest::as_span(logs)};
  cppunivariate::finite_diff_cost<cppunivariate::BurrProfileCost> fd{cost};
  const double theta[]={std::log(r.shape2),std::log(1/r.scale)-center};
  double g[2];fd.gradient(theta,g);
  CHECK(x.size()*std::hypot(g[0],g[1])<1e-3);
}

TEST_CASE("invburr: constant data off a held scale collapse only with both shapes free") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for (double scale : {0.3,1.0,3.0}) {
    std::vector<double> x(3,2.0*scale);
    const auto data=mltest::as_span(x);
    const auto r=cppunivariate::invburr_mle_fixed_scale(data,scale);
    CHECK(r.status==cppunivariate::InvBurrStatus::Unbounded);
    CHECK(r.logLik==INFINITY);
    CHECK(r.iterations==0);
    CHECK(r.scale==scale);
    CHECK(cppunivariate::invburr_mle_holds(data,nan,nan,scale).status==cppunivariate::InvBurrStatus::Unbounded);
    // Holding k removes the off-scale concentrating path; holding c
    // also leaves an ordinary finite fit (including the all-held case).
    for (int mask : {1,2,3}) {
      const auto h=cppunivariate::invburr_mle_holds(data,mask&1?2.0:nan,mask&2?3.0:nan,scale);
      CHECK(h.status==cppunivariate::InvBurrStatus::Ok);
      CHECK(std::isfinite(h.logLik));
    }
    x.back()=std::nextafter(x.back(),INFINITY);
    CHECK(cppunivariate::invburr_mle_fixed_scale(mltest::as_span(x),scale).status!=cppunivariate::InvBurrStatus::Unbounded);
  }
  std::vector<double> invalid{2.0,2.0,INFINITY};
  CHECK(cppunivariate::invburr_mle_fixed_scale(mltest::as_span(invalid),1).status==cppunivariate::InvBurrStatus::InvalidInit);
}

namespace {
#include "../burr/certificate-fixtures.inc"
}
TEST_CASE("invburr: transformed certificate workloads and guarded outcomes") {
  for(const auto& f:burr_certificate_fixtures) {
    CAPTURE(f.name);
    std::vector<double> x=f.x;
    double jac=0; for(double& v:x){v=1/v;jac-=2*std::log(v);}
    auto y=x;for(double& v:y)v=1/v;
    const auto r=cppunivariate::invburr_mle(mltest::as_span(x),1e-8,1000);
    const auto b=cppunivariate::burr_mle(mltest::as_span(y),1e-8,1000);
    cppunivariate::InvBurrStatus expected=cppunivariate::InvBurrStatus::Ok;
    switch(b.status){
      case cppunivariate::BurrStatus::Ok:break;
      case cppunivariate::BurrStatus::WeibullBoundary:expected=cppunivariate::InvBurrStatus::InvWeibullBoundary;break;
      case cppunivariate::BurrStatus::ParetoBoundary:expected=cppunivariate::InvBurrStatus::PowerBoundary;break;
      case cppunivariate::BurrStatus::CertificateTie:expected=cppunivariate::InvBurrStatus::CertificateTie;break;
      case cppunivariate::BurrStatus::CertificateBudget:expected=cppunivariate::InvBurrStatus::CertificateBudget;break;
      default:expected=cppunivariate::InvBurrStatus::NumericalFailure;break;
    }
    CHECK(r.status==expected);
    const auto h=cppunivariate::invburr_mle_holds(mltest::as_span(x),NAN,NAN,NAN,1e-8,1000);
    CHECK(h.status==r.status);
    if(b.status==cppunivariate::BurrStatus::Ok || b.status==cppunivariate::BurrStatus::WeibullBoundary || b.status==cppunivariate::BurrStatus::ParetoBoundary) {
      CHECK(r.shape1==b.shape1);
      CHECK(r.shape2==b.shape2);
      CHECK(r.scale==(b.status==cppunivariate::BurrStatus::ParetoBoundary ? *std::max_element(x.begin(),x.end()) : 1/b.scale));
      CHECK(r.logLik==doctest::Approx(b.logLik+jac).epsilon(1e-11));
      if(b.status==cppunivariate::BurrStatus::Ok) {
        double ll=0;
        for(double v:x) ll+=cppunivariate::distributions::dinvburr(v,r.shape1,r.shape2,r.scale,true);
        CHECK(ll==doctest::Approx(r.logLik).epsilon(1e-10));
      }
    } else {
      CHECK(std::isnan(r.shape1));CHECK(std::isnan(r.shape2));
      CHECK(std::isnan(r.scale));CHECK(std::isnan(r.logLik));
    }
  }
}

TEST_CASE("invburr: certified entrypoints validate controls and reciprocal domain") {
  const std::vector<double> x={.2,.5,1,2,4};
  for(double tol:std::vector<double>{0.,-1.,INFINITY,NAN}) {
    CHECK(cppunivariate::invburr_mle(mltest::as_span(x),tol,1000).status==cppunivariate::InvBurrStatus::InvalidInit);
    CHECK(cppunivariate::invburr_mle_holds(mltest::as_span(x),NAN,NAN,NAN,tol,1000).status==cppunivariate::InvBurrStatus::InvalidInit);
  }
  for(double start:std::vector<double>{0.,-1.,INFINITY}) {
    CHECK(cppunivariate::invburr_mle(mltest::as_span(x),1e-8,1000,start).status==cppunivariate::InvBurrStatus::InvalidInit);
    CHECK(cppunivariate::invburr_mle(mltest::as_span(x),1e-8,1000,NAN,start).status==cppunivariate::InvBurrStatus::InvalidInit);
    CHECK(cppunivariate::invburr_mle(mltest::as_span(x),1e-8,1000,NAN,NAN,start).status==cppunivariate::InvBurrStatus::InvalidInit);
  }
  for(const std::vector<double>& bad:std::vector<std::vector<double>>{{},{1},{0,1},{1,INFINITY},{1,NAN}}) {
    CHECK(cppunivariate::invburr_mle(mltest::as_span(bad),1e-8,1000).status==cppunivariate::InvBurrStatus::InvalidInit);
    CHECK(cppunivariate::invburr_mle_holds(mltest::as_span(bad),NAN,NAN,NAN).status==cppunivariate::InvBurrStatus::InvalidInit);
  }
}

TEST_CASE("invburr: reciprocal overflow and artificial ties stop numerically") {
  const std::vector<double> tiny={1,std::numeric_limits<double>::denorm_min()};
  CHECK(cppunivariate::invburr_mle(mltest::as_span(tiny),1e-8,1000).status==cppunivariate::InvBurrStatus::NumericalFailure);
  double v=1.5;bool found=false;
  for(int i=0;i<1024&&!found;++i) {
    const double next=std::nextafter(v,INFINITY);
    if(1/v==1/next) {
      found=true;
      const std::vector<double> x={v,next,3};
      const auto r=cppunivariate::invburr_mle(mltest::as_span(x),1e-8,1000);
      CHECK(r.status==cppunivariate::InvBurrStatus::NumericalFailure);
      CHECK(std::isnan(r.logLik));CHECK(std::isnan(r.shape1));
    }
    v=next;
  }
  CHECK(found);
}

TEST_CASE("invburr: structured reciprocal face coefficients and likelihood") {
  const std::vector<double> x{1,1,1,.5,.25,.125};
  const auto r=cppunivariate::invburr_mle(mltest::as_span(x),1e-8,1000);
  REQUIRE(r.status==cppunivariate::InvBurrStatus::PowerBoundary);
  REQUIRE(r.stop_detail.limit_coef.size()==2);
  CHECK(r.stop_detail.limit_coef[0]==1);
  CHECK(r.stop_detail.limit_coef[1]==doctest::Approx(x.size()/(std::log(2.)+std::log(4.)+std::log(8.))));
  CHECK(r.stop_detail.limit_loglik==doctest::Approx(r.logLik));
  for(const auto& c:r.stop_detail.candidates) {
    CHECK(c.loglik_lower<=c.loglik_upper);
    if(c.model==cppunivariate::StopModel::Power)CHECK(c.coef==r.stop_detail.limit_coef);
  }
}

TEST_CASE("Inverse Burr held-scale direct and delegated certificates agree") {
 const std::vector<double> x{.2,.4,1,2,4};
 const auto a=cppunivariate::invburr_mle_fixed_scale(mltest::as_span(x),2,1e-9);
 const auto b=cppunivariate::invburr_mle_holds(mltest::as_span(x),NAN,NAN,2);
 REQUIRE(a.status==cppunivariate::InvBurrStatus::Ok);
 CHECK(a.shape1==b.shape1);CHECK(a.shape2==b.shape2);CHECK(a.logLik==b.logLik);
 CHECK(a.stop_detail.loglik_lower<=a.logLik);CHECK(a.stop_detail.loglik_upper>=a.logLik);
 const auto budget=cppunivariate::invburr_mle_fixed_scale(mltest::as_span(x),2,1e-9,1);
 CHECK(budget.status==cppunivariate::InvBurrStatus::Ok);
 CHECK(std::isfinite(budget.stop_detail.loglik_lower));
 const std::vector<double> face{1,1,.5,.25};
 const auto p=cppunivariate::invburr_mle_fixed_scale(mltest::as_span(face),1);
 CHECK(p.status==cppunivariate::InvBurrStatus::AllLeOne);
 REQUIRE(p.stop_detail.limit_coef.size()==2);
 CHECK(p.stop_detail.limit_coef[0]==1);
 CHECK(cppunivariate::invburr_mle_fixed_scale(mltest::as_span(x),NAN).status==cppunivariate::InvBurrStatus::InvalidInit);
}

TEST_CASE("Inverse Burr rejects reciprocal rounding onto the held scale") {
 double scale=1.5;bool found=false;
 for(int i=0;i<1024&&!found;++i) {
   const double next=std::nextafter(scale,INFINITY);
   if(1/scale==1/next) {
     found=true;
     const std::vector<double> x{next,.5,1};
     CHECK(cppunivariate::invburr_mle_fixed_scale(mltest::as_span(x),scale).status==cppunivariate::InvBurrStatus::NumericalFailure);
   }
   scale=next;
 }
 CHECK(found);
}
