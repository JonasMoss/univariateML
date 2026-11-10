#include "cppunivariate/distributions/frechet/mle.hpp"
#include "cppunivariate/distributions/pareto3/mle.hpp"
#include "cppunivariate/distributions/fpareto/mle.hpp"
#include "cppunivariate/distributions/lnorm/mle.hpp"
#include "cppunivariate/distributions/invgamma/mle.hpp"
#include "cppunivariate/distributions/naka/mle.hpp"
#include "cppunivariate/distributions/lgamma/mle.hpp"
#include <iostream>
#include <iomanip>
void record(const char* family,const char* sample,int status,bool certified,double power,double k,double scale,double ll){
  std::cout<<family<<','<<sample<<','<<status<<','<<certified<<','<<power<<','<<k<<','<<scale<<','<<ll<<'\n';
}
int main(){std::cout<<std::setprecision(17);const double e=std::numeric_limits<double>::epsilon();
 std::vector<double>x{1,1+e,1+2*e,1+3*e,1+4*e},y{1,1+e,1+2*e,1+4*e,1+7*e};
 const cppunivariate::span s{x.data(),x.size()},t{y.data(),y.size()};
 const auto w=cppunivariate::invweibull_mle(s,1.220703e-4,100);
 record("invweibull","symmetric",int(w.status),w.certified,w.shape,1,w.scale,w.logLik);
 const auto f=cppunivariate::frechet_mle(s,1.220703e-4,100,0);
 record("frechet","symmetric",int(f.status),f.certified,f.lambda,1,f.sigma,f.logLik);
 const auto l=cppunivariate::loglogistic_mle(t,1e-10,100);
 record("llogis","asymmetric",int(l.status),l.certified,l.shape,1,l.scale,l.logLik);
 const auto p=cppunivariate::pareto3_mle(t,0,1e-10,100);
 record("pareto3","asymmetric",int(p.status),p.certified,p.shape,1,p.scale,p.logLik);
 const auto b=cppunivariate::invburr_mle_holds(s,.855,NAN,NAN,1e-10,100);
 record("invburr","symmetric",int(b.status),b.certified,b.shape2,b.shape1,b.scale,b.logLik);
 const auto g=cppunivariate::gb2_mle(s,1e-10,100,1,NAN,.855,NAN);
 record("gb2","symmetric",int(g.status),g.certified,g.shape2,g.shape3,g.scale,g.logLik);
 const auto fp=cppunivariate::fpareto_mle(s,0,1e-10,100,1,NAN,.855,NAN);
 record("fpareto","symmetric",int(fp.status),fp.certified,fp.shape2,fp.shape3,fp.scale,fp.logLik);
 for(const auto& v:std::vector<std::vector<double>>{{1,4},{1e-300,1e300,1e300,1e300}}){
  const auto r=cppunivariate::invweibull_mle({v.data(),v.size()},1e-10,100,NAN,1e-20,NAN);
  record("invweibull",v.size()==2?"tiny-power":"wide-tiny-power",int(r.status),r.certified,r.shape,1,r.scale,r.logLik);
 }
 for(double unit:{1e-150,1e150}) {
  std::vector<double> v;for(double d:{-2.,-1.,0.,1.,3.})v.push_back(unit*(1+d*1e-12));
  const auto r=cppunivariate::lognormal_mle({v.data(),v.size()});
  record("lnorm",unit<1?"normal-small":"normal-large",int(r.status),r.certified,r.meanlog,1,r.sdlog,r.logLik);
 }
 const auto ig=cppunivariate::invgamma_mle(t,1e-10,100);
 record("invgamma","asymmetric",int(ig.status),ig.certified,ig.shape,1,ig.scale,ig.logLik);
 const auto nk=cppunivariate::naka_mle(t,1e-10,100);
 record("naka","asymmetric",int(nk.status),nk.certified,nk.shape,1,nk.scale,nk.logLik);
 std::vector<double> z;for(double v:y)z.push_back(2*v);
 const auto lg=cppunivariate::lgamma_mle({z.data(),z.size()},1e-10,100);
 record("lgamma","log-asymmetric",int(lg.status),lg.certified,lg.shapelog,1,lg.ratelog,lg.logLik);

 const std::vector<double> held{2,2,2};
 const auto normal=cppunivariate::lognormal_mle({held.data(),held.size()},std::log(2.),NAN);
 record("lnorm","normal-held",int(normal.status),normal.certified,normal.meanlog,1,normal.sdlog,normal.logLik);

 const std::vector<double> large_square{1.5e154,1,1,1,1};
 const auto ns=cppunivariate::naka_mle({large_square.data(),large_square.size()},1e-10,100);
 record("naka","square-reference",int(ns.status),ns.certified,ns.shape,1,ns.scale,ns.logLik);
 const std::vector<double> tiny_inverse{1e-309,1,1,1,1};
 const auto ir=cppunivariate::invgamma_mle({tiny_inverse.data(),tiny_inverse.size()},1e-10,100);
 record("invgamma","reciprocal-reference",int(ir.status),ir.certified,ir.shape,1,ir.scale,ir.logLik);
 std::vector<double> broad(100001,1); broad[0]=1e150;
 const auto nb=cppunivariate::naka_mle({broad.data(),broad.size()},1e-10,100);
 record("naka","broad-normalization",int(nb.status),nb.certified,nb.shape,1,nb.scale,nb.logLik);

}
