// Deterministic held-subset census for gengamma; timings are elapsed milliseconds.
#include "cppunivariate/distributions/gengamma/mle.hpp"
#include "cppunivariate/distributions/gengamma/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <chrono>
#include <cmath>
#include <iomanip>
#include <iostream>
#include <limits>
#include <string>
#include <vector>

std::string status_name(cppunivariate::GengammaStatus s) {
  switch(s) {
    case cppunivariate::GengammaStatus::Ok:return "Ok";
    case cppunivariate::GengammaStatus::InvalidData:return "InvalidData";
    case cppunivariate::GengammaStatus::InvalidFixed:return "InvalidFixed";
    case cppunivariate::GengammaStatus::Unbounded:return "Unbounded";
    case cppunivariate::GengammaStatus::IterationFailure:return "IterationFailure";
    case cppunivariate::GengammaStatus::PowerBoundary:return "PowerBoundary";
    case cppunivariate::GengammaStatus::ParetoBoundary:return "ParetoBoundary";
  }
  return "unknown";
}
int main() {
  struct Case {std::string name;std::vector<double> x;};
  std::vector<Case> cases;
  for(double q:{-0.6,0.,0.6}) {
    cppunivariate::distributions::StdRng<> rng(7317);
    Case c{q<0?"negative":q>0?"positive":"normal",{}};
    for(int i=0;i<80;++i)c.x.push_back(cppunivariate::distributions::rgengamma(rng,0.,1.,q));
    cases.push_back(c);
  }
  for(double direction:{-1.,1.}) {
    Case c{direction<0 ? "seam-Q-positive" : "seam-Q-negative",{}};
    const int n=81;
    double square=0;
    for(int i=0;i<n;++i){const double z=-3.+6.*i/(n-1);square+=z*z/n;}
    for(int i=0;i<n;++i){const double z=-3.+6.*i/(n-1);c.x.push_back(std::exp(z+direction*1e-8*(z*z-square)));}
    cases.push_back(c);
  }
  cases.push_back({"power",{1.,2.,3.}});
  cases.push_back({"pareto",{1.,0.5,1./3.}});
  cases.push_back({"two-point",{1.,2.}});
  cases.push_back({"constant",std::vector<double>(8,1.)});
  const double nan=std::numeric_limits<double>::quiet_NaN();
  std::cout << "case,n,held_mask,status,certified,milliseconds,mu,sigma,Q\n";
  for(const auto& c:cases) for(int mask=0;mask<8;++mask) {
    const auto start=std::chrono::steady_clock::now();
    const auto r=cppunivariate::gengamma_mle({c.x.data(),c.x.size()},1e-10,100,
        mask&1?0.:nan,mask&2?1.:nan,mask&4?0.5:nan);
    const double ms=std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count();
    std::cout << c.name << ',' << c.x.size() << ',' << mask << ',' << status_name(r.status) << ','
              << r.certified << ',' << std::setprecision(8) << ms << ',' << std::setprecision(17)
              << r.mu << ',' << r.sigma << ',' << r.Q << '\n';
  }
}
