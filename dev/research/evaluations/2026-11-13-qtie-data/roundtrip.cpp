#include <cstdio>
#include <cmath>
#include <vector>
#include <string>
#include "test_support.hpp"
#include "cppunivariate/distributions/bbinom/dpqr.hpp"
#include "cppunivariate/distributions/binom/dpqr.hpp"
#include "cppunivariate/distributions/bnbinom/dpqr.hpp"
#include "cppunivariate/distributions/cmb/dpqr.hpp"
#include "cppunivariate/distributions/cmp/dpqr.hpp"
#include "cppunivariate/distributions/dgamma/dpqr.hpp"
#include "cppunivariate/distributions/dlaplace/dpqr.hpp"
#include "cppunivariate/distributions/dnorm/dpqr.hpp"
#include "cppunivariate/distributions/dunif/dpqr.hpp"
#include "cppunivariate/distributions/dweibull/dpqr.hpp"
#include "cppunivariate/distributions/geom/dpqr.hpp"
#include "cppunivariate/distributions/hyper/dpqr.hpp"
#include "cppunivariate/distributions/lgser/dpqr.hpp"
#include "cppunivariate/distributions/nbinom/dpqr.hpp"
#include "cppunivariate/distributions/nhyper/dpqr.hpp"
#include "cppunivariate/distributions/pig/dpqr.hpp"
#include "cppunivariate/distributions/pois/dpqr.hpp"
#include "cppunivariate/distributions/skellam/dpqr.hpp"
#include "cppunivariate/distributions/yulesimon/dpqr.hpp"
#include "cppunivariate/distributions/zeta/dpqr.hpp"
int main() { std::setvbuf(stdout,nullptr,_IOLBF,0);
mltest::discrete_roundtrips("bbinom",
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::pbbinom(x,t[0], t[1], t[2],l,g);},
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::qbbinom(x,t[0], t[1], t[2],l,g);},true);
mltest::discrete_roundtrips("binom",
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::pbinom(x,t[0], t[1],l,g);},
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::qbinom(x,t[0], t[1],l,g);},true);
mltest::discrete_roundtrips("bnbinom",
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::pbnbinom(x,t[0], t[1], t[2],l,g);},
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::qbnbinom(x,t[0], t[1], t[2],l,g);},true);
mltest::discrete_roundtrips("cmb",
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::pcmb(x,t[0], t[1], t[2],l,g);},
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::qcmb(x,t[0], t[1], t[2],l,g);},true);
mltest::discrete_roundtrips("cmp",
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::pcmp(x,t[0], t[1],l,g);},
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::qcmp(x,t[0], t[1],l,g);},true);
mltest::discrete_roundtrips("dgamma",
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::pdgamma(x,t[0], t[1],l,g);},
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::qdgamma(x,t[0], t[1],l,g);},true);
mltest::discrete_roundtrips("dlaplace",
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::pdlaplace(x,t[0], t[1],l,g);},
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::qdlaplace(x,t[0], t[1],l,g);},true);
mltest::discrete_roundtrips("dnorm",
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::pdnorm(x,t[0], t[1],l,g);},
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::qdnorm(x,t[0], t[1],l,g);},true);
mltest::discrete_roundtrips("dunif",
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::pdunif(x,t[0], t[1],l,g);},
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::qdunif(x,t[0], t[1],l,g);},true);
mltest::discrete_roundtrips("dweibull",
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::pdweibull(x,t[0], t[1],l,g);},
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::qdweibull(x,t[0], t[1],l,g);},true);
mltest::discrete_roundtrips("geom",
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::pgeom(x,t[0],l,g);},
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::qgeom(x,t[0],l,g);},true);
mltest::discrete_roundtrips("hyper",
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::phyper(x,t[0], t[1], t[2],l,g);},
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::qhyper(x,t[0], t[1], t[2],l,g);},true);
mltest::discrete_roundtrips("lgser",
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::plgser(x,t[0],l,g);},
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::qlgser(x,t[0],l,g);},true);
mltest::discrete_roundtrips("nbinom",
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::pnbinom(x,t[0], t[1],l,g);},
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::qnbinom(x,t[0], t[1],l,g);},true);
mltest::discrete_roundtrips("nhyper",
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::pnhyper(x,t[0], t[1], t[2],l,g);},
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::qnhyper(x,t[0], t[1], t[2],l,g);},true);
mltest::discrete_roundtrips("pig",
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::ppig(x,t[0], t[1],l,g);},
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::qpig(x,t[0], t[1],l,g);},true);
mltest::discrete_roundtrips("pois",
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::ppois(x,t[0],l,g);},
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::qpois(x,t[0],l,g);},true);
mltest::discrete_roundtrips("skellam",
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::pskellam(x,t[0], t[1],l,g);},
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::qskellam(x,t[0], t[1],l,g);},true);
mltest::discrete_roundtrips("yulesimon",
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::pyulesimon(x,t[0],l,g);},
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::qyulesimon(x,t[0],l,g);},true);
mltest::discrete_roundtrips("zeta",
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::pzeta(x,t[0],l,g);},
 [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::qzeta(x,t[0],l,g);},true);
}
