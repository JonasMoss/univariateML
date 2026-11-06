#include <cppunivariate/distributions/zinb/global_certificate.hpp>
#include <iostream>
#include <iomanip>
int main(){int n,mask;double pi;while(std::cin>>n>>mask>>pi){
 std::vector<int>x(n),positive;for(int&v:x){std::cin>>v;if(v>0)positive.push_back(v);}
 const double held=(mask&4)?pi:NAN;
 const auto valid=cppunivariate::zinb_fixed_size_mle({x.data(),x.size()},1,NAN,held,1e-10,200);
 const auto old=cppunivariate::zinb_candidate_mle({x.data(),x.size()},1e-10,200,NAN,NAN,held);
 const auto zt=cppunivariate::ztnbinom_mle({positive.data(),positive.size()});
 const auto nb=cppunivariate::nbinom_mle({x.data(),x.size()},1e-10,200);
 cppunivariate::zinb_global_detail::Data d;d.n=n;d.pi=held;
 for(int v:x){++d.counts[v];d.S+=v;d.k+=v>0;d.M=std::max(d.M,v);}d.n0=d.n-d.k;d.z=d.n0/d.n;
 for(const auto&kv:d.counts)d.factorial-=kv.second*cppunivariate::special::lgamma(kv.first+1.);
 const auto face=cppunivariate::zinb_global_detail::zip_upper(d,200);
 std::cout<<std::setprecision(17)<<"validation "<<int(valid.fit.status)<<' '<<int(valid.failure)<<" proposal "<<int(old.status)<<' '<<old.size<<' '<<old.prob<<' '<<old.pi<<" parents "<<int(zt.status)<<' '<<int(zt.certificate_failure)<<' '<<zt.size<<' '<<zt.prob<<' '<<int(nb.status)<<" face "<<face.ok<<' '<<face.lambda<<' '<<face.lower<<' '<<face.upper;
 if(old.size>0 && old.prob>0 && old.prob<1){double a=std::log(old.size),b=std::log(-old.size*std::log(old.prob));const auto p=d.at(a,b);
  std::cout<<" gradient "<<p.ga<<' '<<p.gb<<' '<<p.ge;
  for(double radius:{.02,.00125,.000078125,.0000048828125}){const auto c=d.curvature(a-radius,a+radius,b-radius,b+radius);const double det=c.aa.hi*c.bb.hi-c.ab.hi*c.ab.hi,lambda=det/(-c.aa.lo-c.bb.lo);std::cout<<" radius "<<radius<<' '<<lambda<<' '<<(std::hypot(p.ga,p.gb)+2*p.ge)/lambda;}
 }
 std::cout<<'\n';
}}
