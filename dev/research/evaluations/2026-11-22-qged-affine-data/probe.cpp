#include "cppunivariate/distributions/ged/dpqr.hpp"
#include <iomanip>
#include <iostream>

int main() {
    namespace cu = cppunivariate;
    namespace dist = cu::distributions;
    namespace ged = cu::ged_detail;
    std::cout << std::setprecision(17);
    char operation;
    double p, mean, sd, nu;
    bool lower, log;
    while (std::cin >> operation >> p >> mean >> sd >> nu >> lower >> log) {
        const double lp=dist::fs::logprob(p,log);
        const double h=log?(p+std::log(2.))+2.3190468138462995584e-17:0.;
        const bool central=log?std::abs(h)<1e-8:std::abs(p-.5)<1e-8;
        const double mass=log?std::expm1(h):2*(p-.5);
        const bool small=lp<=-std::log(2.);
        const double target=central?std::log(std::abs(mass)):
            (small?lp:dist::fs::log1mexp(lp))+std::log(2.);
        const double w=nu==1
            ? std::log(central ? -std::log1p(-std::abs(mass)) : -target)
            : ged::gamma_power_logq(target,nu,central);
        const bool negative=central?(mass>0)!=lower:small==lower;
        const double fast=ged::quantile_double::affine(w,mean,sd,nu,negative,p,log);
        const double q=dist::qged(p,mean,sd,nu,lower,log);
        if (std::isfinite(fast) && q!=fast) return 1;
        std::cout << q << ' ' << dist::pged(q,mean,sd,nu,lower,log)
                  << ' ' << int(std::isfinite(fast)) << '\n';
    }
}
