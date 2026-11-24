#include "cppunivariate/distributions/sged/dpqr.hpp"
#include <iomanip>
#include <iostream>

int main() {
    std::cout << std::setprecision(17);
    char operation;
    double x, mean, sd, nu;
    bool lower, log;
    while (std::cin >> operation >> x >> mean >> sd >> nu >> lower >> log) {
        if (operation != 'p') return 1;
        std::cout << cppunivariate::distributions::psged(x, mean, sd, nu, 1, lower, log)
                  << '\n';
    }
}
