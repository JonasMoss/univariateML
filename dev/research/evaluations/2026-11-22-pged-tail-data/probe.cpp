#include "cppunivariate/distributions/ged/dpqr.hpp"
#include <ctime>
#include <iomanip>
#include <iostream>
#include <vector>

#ifndef ALTERNATIVE_RADIUS
#define ALTERNATIVE_RADIUS 0
#endif

namespace cu = cppunivariate;
namespace ged = cppunivariate::ged_detail;
namespace dist = cppunivariate::distributions;

#if ALTERNATIVE_RADIUS
// Comparison only: an error-free physical difference and compensated
// quotient against a cached two-word GED scale, as in SGED's log(v/s).
double alternative(double x, double mean, double sd, double nu, bool lower, bool log) {
    if (!(nu >= 1) || !std::isfinite(x) || x == mean)
        return dist::pged(x, mean, sd, nu, lower, log);
    const double ld = cu::special::log_abs_difference(x, mean);
    const double ls = dist::fs::ged_logscale(nu), lsd = std::log(sd);
    const double w = ld - lsd - ls;
    const double h = nu * ged::fast_radius_error(ld, lsd, ls, w);
    if (nu < 1e6 && h * (1 + std::exp(nu*w+h)) <= ged::fast_tail_budget) {
        const double small = -std::log(2.) + ged::gamma_power_log(w, nu, false);
        const double selected = ((x < mean) == lower) ? small : dist::fs::log1mexp(small);
        return log ? selected : std::exp(selected);
    }
    const double dx = x - mean;
    if (!std::isnormal(dx) || !std::isnormal(sd) || nu > 1e6)
        return dist::pged(x, mean, sd, nu, lower, log);
    int exponent;
    const double s = std::frexp(sd, &exponent);
    const double numerator = std::scalbn(std::abs(dx), -exponent);
    const double residual = cu::special::addition_residual(x, -mean, dx);
    const double low = std::scalbn(dx < 0 ? -residual : residual, -exponent);
    static thread_local double last = 0;
    static thread_local cu::special::three_word::Number scale;
    if (nu != last) {
        scale = cu::special::three_word::exponential(ged::logscale_precise(nu));
        last = nu;
    }
    const double denominator = s * scale.high;
    const double dlo = std::fma(s, scale.high, -denominator) + s * scale.low;
    const double q = numerator / denominator;
    if (!(q >= .5 && q <= 2))
        return dist::pged(x, mean, sd, nu, lower, log);
    const double correction = (std::fma(-q, denominator, numerator) + low - q*dlo) / denominator;
    const double radius = std::log1p((q-1) + correction);
    const double eps = std::numeric_limits<double>::epsilon();
    const ged::Radius r{radius, 8*eps*std::abs(radius) + 32*eps*eps, ls};
    return ged::probability_precise(r, nu, -std::log(2.), x < mean, lower, log, true);
}

#else
#define alternative dist::pged
#endif

int main(int argc, char**) {
    std::cout << std::setprecision(17);
    char operation;
    double x, mean, sd, nu;
    bool lower, log;
    if (argc > 1) {
        struct Input {double x, mean, sd, nu; bool lower, log;};
        std::vector<Input> inputs;
        while (std::cin >> operation >> x >> mean >> sd >> nu >> lower >> log)
            inputs.push_back({x, mean, sd, nu, lower, log});
        double sum = 0;
        const std::clock_t start = std::clock();
        for (const auto &i : inputs)
            sum += ALTERNATIVE_RADIUS ? alternative(i.x, i.mean, i.sd, i.nu, i.lower, i.log)
                                      : dist::pged(i.x, i.mean, i.sd, i.nu, i.lower, i.log);
        std::cout << static_cast<double>(std::clock()-start) / CLOCKS_PER_SEC
                  << ' ' << sum << '\n';
        return 0;
    }
    while (std::cin >> operation >> x >> mean >> sd >> nu >> lower >> log) {
        const auto probability = [&] (double at) {
            return ALTERNATIVE_RADIUS ? alternative(at, mean, sd, nu, lower, log)
                                      : dist::pged(at, mean, sd, nu, lower, log);
        };
        if (operation == 'q') {
            const double q = dist::qged(x, mean, sd, nu, lower, log);
            std::cout << q << ' ' << probability(q) << '\n';
        } else {
            std::cout << (operation == 'd' ? dist::dged(x, mean, sd, nu, log) : probability(x)) << '\n';
        }
    }
}
