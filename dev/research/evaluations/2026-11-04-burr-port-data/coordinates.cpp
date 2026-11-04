// Reproduce why log-coordinate choice matters to rectangular enclosures.
#include "cppunivariate/distributions/burr/certificate.hpp"
#include <iostream>
#include <iomanip>
int main(){
  std::size_t n;double k,c,scale;
  while(std::cin>>n>>k>>c>>scale){
    std::vector<double>x(n);for(double&v:x)std::cin>>v;
    cppunivariate::burr_cert_detail::Data normalized;
    if(!cppunivariate::burr_cert_detail::data(cppunivariate::span{x.data(),x.size()},normalized))return 1;
    for(int mode=0;mode<3;++mode){
      cppunivariate::burr_cert_detail::Data d;
      cppunivariate::burr_cert_detail::Proposal t;
      if(mode==0){d=normalized;t.k=k;t.c=c*d.unit;t.b=c*(d.center-std::log(scale));}
      else {const double shift=mode==1?0:std::log(scale);t.k=k;t.c=c;t.b=c*(shift-std::log(scale));
        for(double v:x){const double y=std::log(v);d.y.push_back(y-shift);d.box.push_back(cppunivariate::burr_cert_detail::padded(y,y)-shift);}}
      cppunivariate::burr_cert_detail::Root root;
      const bool ok=cppunivariate::burr_cert_detail::isolate(d,t,root);
      std::cout<<std::setprecision(17)<<mode<<' '<<ok<<' '<<root.delta<<' '<<(ok?root.delta/root.t.k:0)<<'\n';
    }
  }
}
