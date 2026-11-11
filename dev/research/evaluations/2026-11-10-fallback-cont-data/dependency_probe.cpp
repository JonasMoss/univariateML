#include "cppunivariate/distributions/gb2/mle.hpp"
#include "cppunivariate/distributions/sged/mle.hpp"
#include "cppunivariate/distributions/invparalogis/mle.hpp"
#include "cppunivariate/distributions/invpareto/mle.hpp"
#include "cppunivariate/distributions/pareto2/mle.hpp"
#include "cppunivariate/distributions/pareto3/mle.hpp"
#include "cppunivariate/distributions/trgamma/mle.hpp"
#include "cppunivariate/distributions/fpareto/mle.hpp"
#include <iostream>
#include <iomanip>
template<class T> auto flag(const T& r,int)->decltype(int(r.certified)){return int(r.certified);}
template<class T> int flag(const T&,long){return -1;}
template<class T> void show(const char* name,const T& r){std::cout<<name<<'\t'<<int(r.status)<<'\t'<<flag(r,0)<<'\t'<<std::setprecision(17)<<r.logLik<<'\n';}
int main(){
 const std::vector<double> x{std::exp(-1.1),std::exp(-1.)};cppunivariate::span d{x.data(),x.size()};
 show("burr",cppunivariate::burr_mle_fixed_scale(d,1));
 show("gb2-unit-shape",cppunivariate::gb2_mle(d,1e-8,1000,NAN,NAN,1,1));
 show("fpareto-unit-shape",cppunivariate::fpareto_mle(d,0,1e-8,1000,NAN,NAN,1,1));
 const std::vector<double> y{.7321666961121761,2.2766042947262384,-.5491691631533211};cppunivariate::span e{y.data(),y.size()};
 show("ged",cppunivariate::ged_mle(e,1e-8,1000,NAN,1.0178559902092952,NAN));
 show("sged-held-xi",cppunivariate::sged_mle(e,1e-8,1000,NAN,1.0178559902092952,NAN,1));
 const std::vector<double> z{.9998,.9999,1,1.0001,1.0003};std::vector<double> iz;for(double v:z)iz.push_back(1/v);
 show("paralogis",cppunivariate::paralogis_mle({z.data(),z.size()},1e-8,1000));
 show("invparalogis",cppunivariate::invparalogis_mle({iz.data(),iz.size()},1e-8,1000));
 const std::vector<double> a{.03,.1,.4,.7,1,2,4,12};std::vector<double> ia;for(double v:a)ia.push_back(1/v);cppunivariate::span s{a.data(),a.size()};
 show("lomax",cppunivariate::lomax_mle(s,1e-8,1000));
 show("invpareto",cppunivariate::invpareto_mle({ia.data(),ia.size()},1e-8,1000));
 show("pareto2",cppunivariate::pareto2_mle(s,0,1e-8,1000));
 show("trgamma",cppunivariate::trgamma_mle(s,1e-8,1000,NAN,1));
 std::vector<double> crossing{1.9901008294052028e-5,2.209857117039297e-5,.0002450722434289249,12.288342947780086,942.976056905759,16.239512580216314,2.898690421966857,1.2684036923492177,400970.63293690106*std::exp(5.646438395028704)};
 for(double& v:crossing)v=std::sqrt(v);
 show("gb2-rounded-power",cppunivariate::gb2_mle({crossing.data(),crossing.size()},1e-7,1000,NAN,2,.22690393135372058,NAN));
 show("fpareto-rounded-power",cppunivariate::fpareto_mle({crossing.data(),crossing.size()},0,1e-7,1000,NAN,2,.22690393135372058,NAN));
 for(double center:{1.,1000.,1e100})for(int n:{2,3,4,5}){
  std::vector<double> v;double value=center;for(int j=0;j<n;++j){v.push_back(value);value=std::nextafter(value,INFINITY);}
  auto parent=cppunivariate::loglogistic_mle({v.data(),v.size()},1e-10,1000);
  auto child=cppunivariate::pareto3_mle({v.data(),v.size()},0,1e-10,1000);
  if(parent.status==cppunivariate::LogLogisticStatus::Ok&&!parent.certified){std::cout<<"pareto3-case\t"<<center<<'\t'<<n<<'\n';show("llogis",parent);show("pareto3",child);}
 }
}
