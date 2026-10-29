#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"
#include "cppunivariate/special/cmp.hpp"
#include <chrono>
#include <iostream>
#include <vector>

TEST_CASE("CMP analytic Poisson and geometric submodels") {
    for (double lambda : {1e-12, .001, .5, 1., 2., 1000.}) {
        const auto v=cppunivariate::cmp_moments(std::log(lambda),1);
        REQUIRE(v.status == cppunivariate::CmpStatus::Ok);
        CHECK(v.log_z == doctest::Approx(lambda).epsilon(2e-14));
        CHECK(v.mean == doctest::Approx(lambda).epsilon(2e-14));
        CHECK(v.variance == doctest::Approx(lambda).epsilon(2e-14));
    }
    for (double lambda : {.001,.5,.9,.99}) {
        const auto v=cppunivariate::cmp_moments(std::log(lambda),0);
        REQUIRE(v.status == cppunivariate::CmpStatus::Ok);
        CHECK(v.log_z == doctest::Approx(-std::log1p(-lambda)).epsilon(2e-14));
        CHECK(v.mean == doctest::Approx(lambda/(1-lambda)).epsilon(2e-14));
        CHECK(v.variance == doctest::Approx(lambda/((1-lambda)*(1-lambda))).epsilon(2e-14));
        CHECK(v.relative_tail_bound <= 4e-16);
    }
}

TEST_CASE("CMP high precision grid") {
    const double refs[][8] = {
#include "cmp_reference.inc"
    };
    double worst=0;
    const auto start=std::chrono::steady_clock::now();
    for (const auto& row : refs) {
        const auto call_start=std::chrono::steady_clock::now();
        const auto v=cppunivariate::cmp_moments(row[0],row[1]);
        const double micros=std::chrono::duration<double,std::micro>(std::chrono::steady_clock::now()-call_start).count();
        INFO("eta=" << row[0] << " nu=" << row[1] << " work=" << v.work);
        REQUIRE(v.status == cppunivariate::CmpStatus::Ok);
        const double actual[]={v.log_z,v.mean,v.variance,v.mean_h,v.variance_h,v.covariance};
        std::cout << "CMP_REF " << row[0] << " " << row[1] << " " << static_cast<int>(v.regime) << " " << micros;
        for (int j=0;j<6;++j) {
            const double rel=std::abs((actual[j]-row[j+2])/row[j+2]);
            worst=std::max(worst,rel);
            std::cout << " " << rel;
            CHECK(rel < 3e-15);
        }
        std::cout << std::endl;
    }
    std::cout << "CMP grid: " << sizeof(refs)/sizeof(refs[0]) << " calls, "
              << std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()
              << " s; maximum relative error " << worst << '\n';
}

TEST_CASE("CMP statuses and fixed parameter paths") {
    std::vector<double> empty, zero(4,0), constant(4,3), adjacent{3,4,4,3};
    std::vector<double> geometric{0,0,0,0,0,0,0,0,1,9};
    CHECK(cppunivariate::cmp_mle({empty.data(),empty.size()}).status == cppunivariate::CmpStatus::InvalidInput);
    for (const auto& x : {zero,constant})
        CHECK(cppunivariate::cmp_mle({x.data(),x.size()}).status == cppunivariate::CmpStatus::PointMassLimit);
    CHECK(cppunivariate::cmp_mle({adjacent.data(),adjacent.size()}).status == cppunivariate::CmpStatus::TwoPointLimit);
    CHECK(cppunivariate::cmp_mle({geometric.data(),geometric.size()}).status == cppunivariate::CmpStatus::GeometricLimit);
    for (double x : {-1., .5, cppunivariate::special::kPosInf}) {
        std::vector<double> bad{x};
        CHECK(cppunivariate::cmp_mle({bad.data(),bad.size()}).status == cppunivariate::CmpStatus::InvalidInput);
    }
    CHECK(cppunivariate::cmp_moments(0,0).status == cppunivariate::CmpStatus::InvalidInput);
    CHECK(cppunivariate::cmp_moments(0,-1).status == cppunivariate::CmpStatus::InvalidInput);
    CHECK(cppunivariate::cmp_moments(100,.1).status == cppunivariate::CmpStatus::WorkLimit);
    CHECK(cppunivariate::cmp_moments(10,1,2e-16,2).status == cppunivariate::CmpStatus::WorkLimit);
    std::vector<double> x{0,1,1,2,2,2,3,3,4,5};
    cppunivariate::CmpFitControl ctl;
    ctl.fixed_nu=1;
    const auto p=cppunivariate::cmp_mle({x.data(),x.size()},ctl);
    REQUIRE(p.status == cppunivariate::CmpStatus::Ok);
    CHECK(std::exp(p.eta) == doctest::Approx(2.3).epsilon(1e-9));
    ctl.fixed_nu=0;
    const auto g=cppunivariate::cmp_mle({x.data(),x.size()},ctl);
    REQUIRE(g.status == cppunivariate::CmpStatus::Ok);
    CHECK(std::exp(g.eta) == doctest::Approx(2.3/3.3).epsilon(2e-14));
    ctl.fixed_nu=cppunivariate::special::kNaN;
    ctl.fixed_lambda=std::exp(p.eta);
    const auto f=cppunivariate::cmp_mle({x.data(),x.size()},ctl);
    REQUIRE(f.status == cppunivariate::CmpStatus::Ok);
    CHECK(cppunivariate::cmp_moments(f.eta,f.nu).mean_h == doctest::Approx(f.mean_h).epsilon(1e-9));
    ctl.fixed_lambda=.5;
    CHECK(cppunivariate::cmp_mle({geometric.data(),geometric.size()},ctl).status == cppunivariate::CmpStatus::GeometricLimit);
    ctl.fixed_lambda=2;
    std::vector<double> bernoulli{0,1,1};
    CHECK(cppunivariate::cmp_mle({bernoulli.data(),bernoulli.size()},ctl).status == cppunivariate::CmpStatus::TwoPointLimit);
    ctl={}; ctl.maxit=1;
    CHECK(cppunivariate::cmp_mle({x.data(),x.size()},ctl).status == cppunivariate::CmpStatus::IterationFailure);
}

// An independent, deliberately slow coordinate search of the full likelihood.
// No analytic scores, profiling or covariance calculations.
TEST_CASE("CMP likelihood agrees with independent coordinate optimizer") {
    for (const std::vector<double>& x : {std::vector<double>{1,2,3,3,3,4,4,4,5,6},
                                       std::vector<double>{0,0,1,1,1,2,3,4,6,8}}) {
        const auto fit=cppunivariate::cmp_mle({x.data(),x.size()});
        REQUIRE(fit.status == cppunivariate::CmpStatus::Ok);
        CHECK((x.front() == 1 ? fit.nu > 1 : fit.nu < 1));
        double e=0,n=1,step=1;
        const auto ll=[&](double eta,double nu) {
            if (nu <= 0) return -cppunivariate::special::kPosInf;
            const auto v=cppunivariate::cmp_moments(eta,nu);
            if (v.status != cppunivariate::CmpStatus::Ok) return -cppunivariate::special::kPosInf;
            double z=-static_cast<double>(x.size())*v.log_z;
            for (double k : x) z+=k*eta-nu*std::lgamma(k+1);
            return z;
        };
        double best=ll(e,n);
        for (int i=0;i<1000 && step>1e-7;++i) {
            bool improved=false;
            for (int axis : {0,1}) for (int direction : {-1,1}) {
                const double ee=e+(axis == 0 ? direction*step : 0);
                const double nn=n+(axis == 1 ? direction*step : 0);
                const double v=ll(ee,nn);
                if (v>best) { e=ee; n=nn; best=v; improved=true; }
            }
            if (!improved) step*=.5;
        }
        CHECK(fit.logLik == doctest::Approx(best).epsilon(1e-9));
        CHECK(fit.eta == doctest::Approx(e).epsilon(2e-4));
        CHECK(fit.nu == doctest::Approx(n).epsilon(2e-4));
    }
}

TEST_CASE("CMP near 32-bit counts and million observation preprocessing") {
    const auto large=cppunivariate::cmp_moments(std::log(2147480000.),1);
    REQUIRE(large.status == cppunivariate::CmpStatus::Ok);
    CHECK(large.mean == doctest::Approx(2147480000.).epsilon(2e-14));
    CHECK(large.variance == doctest::Approx(2147480000.).epsilon(2e-11));
    std::vector<double> x(1000000);
    for (std::size_t i=0;i<x.size();++i) x[i]=static_cast<double>(i%7);
    const auto start=std::chrono::steady_clock::now();
    const auto fit=cppunivariate::cmp_mle({x.data(),x.size()});
    REQUIRE(fit.status == cppunivariate::CmpStatus::Ok);
    std::cout << "CMP million-observation fit: "
              << std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()
              << " s, " << fit.work << " summands\n";
}

TEST_CASE("CMP Poisson recovery and high count fitting") {
    std::vector<double> poisson;
    double prob=std::exp(-3.0);
    for (int k=0;k<20;++k) {
        poisson.insert(poisson.end(),static_cast<std::size_t>(std::round(100000*prob)),k);
        prob*=3.0/(k+1);
    }
    const auto p=cppunivariate::cmp_mle({poisson.data(),poisson.size()});
    REQUIRE(p.status == cppunivariate::CmpStatus::Ok);
    CHECK(p.nu == doctest::Approx(1).epsilon(.002));
    CHECK(std::exp(p.eta) == doctest::Approx(3).epsilon(.002));
    const std::vector<double> x{2147300000.,2147350000.,2147400000.,2147450000.};
    cppunivariate::CmpFitControl ctl; ctl.fixed_nu=1;
    const auto f=cppunivariate::cmp_mle({x.data(),x.size()},ctl);
    REQUIRE(f.status == cppunivariate::CmpStatus::Ok);
    CHECK(std::exp(f.eta) == doctest::Approx(2147375000.).epsilon(2e-14));
    const auto j=cppunivariate::cmp_mle({x.data(),x.size()});
    REQUIRE(j.status == cppunivariate::CmpStatus::Ok);
    CHECK(j.nu > .5);
    CHECK(j.nu < 1);
    CHECK(std::isfinite(j.logLik));
    const auto q=cppunivariate::cmp_moments(j.eta,j.nu);
    CHECK(q.mean == doctest::Approx(j.mean).epsilon(2e-14));
}

TEST_CASE("CMP near two-point and point-mass moment limits") {
    for (double eta : {-1000., 0., 1., 100.}) {
        const auto v=cppunivariate::cmp_moments(eta,1e6);
        REQUIRE(v.status == cppunivariate::CmpStatus::Ok);
        const double p=eta >= 0 ? 1/(1+std::exp(-eta)) : std::exp(eta)/(1+std::exp(eta));
        CHECK(v.mean == doctest::Approx(p).epsilon(2e-14));
        CHECK(v.mean_h == 0);
    }
    const auto v=cppunivariate::cmp_moments(1e6*std::log(4.)+.2,1e6);
    REQUIRE(v.status == cppunivariate::CmpStatus::Ok);
    CHECK(v.mean == doctest::Approx(3+1/(1+std::exp(-.2))).epsilon(1e-11));
}

TEST_CASE("CMP geometric tie tolerance and narrow high-count data") {
    std::vector<double> geometric;
    for (int k=0;k<25;++k)
        geometric.insert(geometric.end(),static_cast<std::size_t>(std::round(1000000*std::pow(.5,k+1))),k);
    CHECK(cppunivariate::cmp_mle({geometric.data(),geometric.size()}).status == cppunivariate::CmpStatus::GeometricLimit);
    const std::vector<double> x{2147483645.,2147483646.,2147483647.};
    const auto f=cppunivariate::cmp_mle({x.data(),x.size()});
    INFO("nu=" << f.nu << " eta=" << f.eta << " iterations=" << f.iterations << " work=" << f.work);
    REQUIRE(f.status == cppunivariate::CmpStatus::Ok);
    const auto m=cppunivariate::cmp_moments(f.eta,f.nu);
    CHECK(m.variance > .5);
    CHECK(m.variance < .8);
    CHECK(std::isfinite(f.logLik));
}
