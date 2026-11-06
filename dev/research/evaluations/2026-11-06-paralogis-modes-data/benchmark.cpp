// Run from repository root; build command is recorded in the report.
#include "cppunivariate/distributions/invparalogis/mle.hpp"
#include "cppunivariate/distributions/paralogis/dpqr.hpp"
#include "json_loader.hpp"
#include <iostream>
#include <iomanip>
#include <chrono>
int main(){
  std::cout<<std::setprecision(17);
  std::cout<<"case\tfixed\tstatus\tcertified\tbudget\tboxes\tshape\tscale\tlogLik\tms\tproposal_ms\tcertificate_ms\tgolden_shape_relative_error\tgolden_scale_relative_error\n";
  int failed=0;
  auto check=[&](const std::string& name,const std::vector<double>& x,double scale,double ga,double gs){
    const bool fixed=std::isfinite(scale);
    const auto start=std::chrono::steady_clock::now();
    const cppunivariate::span data{x.data(),x.size()};
    auto r=fixed?cppunivariate::paralogis_shape_proposal(data,scale,1e-10,1000):cppunivariate::paralogis_mle_impl(data,1.220703e-4,1000,NAN,false);
    const auto proposed=std::chrono::steady_clock::now();
    auto c=cppunivariate::paralogis_cert_detail::certify(data,r.shape,r.scale,fixed);
    const double ms=std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count();
    std::cout<<name<<'\t'<<fixed<<'\t'<<int(r.status)<<'\t'<<c.ok<<'\t'<<c.budget<<'\t'<<c.boxes<<'\t'<<c.shape<<'\t'<<c.scale<<'\t'<<c.logLik<<'\t'<<ms<<'\t'<<std::chrono::duration<double,std::milli>(proposed-start).count()<<'\t'<<ms-std::chrono::duration<double,std::milli>(proposed-start).count()<<'\t'<<std::abs(c.shape/ga-1)<<'\t'<<std::abs(c.scale/gs-1)<<'\n';
    if(!c.ok||r.status!=cppunivariate::ParalogisStatus::Ok)++failed;
  };
  for(const std::string id:{"paralogis","invparalogis"}){
    auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/"+id+"/golden.json");
    double scale=g.estimates.at("scale");
    if(id=="invparalogis"){for(double& v:g.data)v=1/v;scale=1/scale;}
    check(id+"-golden",g.data,NAN,g.estimates.at("shape"),scale);
    check(id+"-golden-held-scale",g.data,scale,NAN,NAN);
  }
  for(double shape:{.3,.8,1.,3.,10.}){
    std::vector<double> x;
    for(int i=0;i<30;++i)x.push_back(cppunivariate::distributions::qparalogis((i+.5)/30,shape,2.7,true,false));
    check("quantiles-"+std::to_string(shape),x,NAN,NAN,NAN);
    check("quantiles-held-"+std::to_string(shape),x,2.7,NAN,NAN);
  }
  for(double v:{.01,1.,100.})check("single-"+std::to_string(v),{v},1,NAN,NAN);
  check("constant-at-scale",{3,3,3},3,2/std::log(2.),3);
  check("near-tie",{1.,1.00001,1.00002},1,NAN,NAN);
  for(int n:{500,10000,100000}) {
    std::vector<double> distinct;
    for(int i=0;i<n;++i) distinct.push_back(cppunivariate::distributions::qparalogis((i+.5)/n,1,1,true,false));
    check("distinct-"+std::to_string(n),distinct,NAN,NAN,NAN);
  }
  std::vector<double> large;
  for(int i=0;i<1000;++i)for(int j=0;j<100;++j)
    large.push_back(cppunivariate::distributions::qparalogis((i+.5)/1000,1,1,true,false));
  check("replication-100000",large,NAN,NAN,NAN);
  return failed?1:0;
}
