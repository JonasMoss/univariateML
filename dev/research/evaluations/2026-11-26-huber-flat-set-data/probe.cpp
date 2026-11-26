#include "cppunivariate/distributions/huber/mle.hpp"
#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <string>
int main(int argc, char** argv) {
#ifdef HUBER_EXACT_PROBE
  if (argc==2 && std::string(argv[1])=="gap") {
    std::string a,b,c,s;
    while (std::cin >> a >> b >> c >> s)
      std::cout << cppunivariate::huber_gap_sign(std::strtod(a.c_str(),nullptr),std::strtod(b.c_str(),nullptr),
          std::strtod(c.c_str(),nullptr),std::strtod(s.c_str(),nullptr)) << '\n';
    return 0;
  }
#endif
  for (double a : {9., 10., std::nextafter(1.345,0.), 1.345, std::nextafter(1.345,2.)})
    for (double c : {1.345, .5, 1., 9., NAN+0.})
      for (double s : {1., NAN+0.})
        for (double m : {NAN+0.,0.}) {
          double x[]={-10,-a,a,10};
          auto r=cppunivariate::huber_mle({x,4},1e-10,200,c,m,s);
          auto o=cppunivariate::outcome(r);
          std::cout<<std::setprecision(17)<<a<<' '<<c<<' '<<s<<' '<<m<<" -> "
            <<int(r.status)<<' '<<o.certified<<' '<<o.uniqueness_known<<' '<<o.unique
            <<' '<<r.mu<<' '<<r.sigma<<' '<<r.epsilon<<' '<<r.logLik<<' '
            <<a-r.sigma*r.epsilon<<'\n';
        }
}
