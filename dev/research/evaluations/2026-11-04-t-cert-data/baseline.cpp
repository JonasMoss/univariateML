// Research driver only. Printing and stream input stay outside the library.
#include "cppunivariate/distributions/lst/mle.hpp"
#include "cppunivariate/distributions/std/mle.hpp"
#include <chrono>
#include <iomanip>
#include <iostream>
#include <vector>
#include <cmath>
int main() {
    std::size_t n;
    std::cout << std::setprecision(17);
    while(std::cin >> n) {
        std::vector<double> x(n);
        for(double& v:x) std::cin >> v;
        cppunivariate::span data{x.data(),x.size()};
        for(int family=0;family<2;++family) {
            double mu=0,scale=0,df=0,ll=0; int status=0;
            double elapsed=0;
            for(int rep=0;rep<4;++rep) {
                const auto start=std::chrono::steady_clock::now();
                if(family==0) {
                    const auto r=cppunivariate::lst_mle(data,1.220703e-4,200);
                    mu=r.mu;scale=r.sigma;df=r.df;ll=r.logLik;status=static_cast<int>(r.status);
                } else {
                    const auto r=cppunivariate::student_t_mle(data,1.220703e-4,200);
                    mu=r.mu;scale=r.sigma;df=r.nu;ll=r.logLik;status=static_cast<int>(r.status);
                }
                if(rep)elapsed+=std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()/3;
            }
            std::cout << status << ' ' << mu << ' ' << scale << ' ' << df << ' ' << ll << ' ' << elapsed << '\n';
        }
    }
}
