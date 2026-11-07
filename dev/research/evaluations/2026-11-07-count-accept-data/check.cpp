#include <cppunivariate/distributions/tnorm/mle.hpp>
#include <cppunivariate/distributions/cmb/mle.hpp>
#include <cppunivariate/distributions/cmp/mle.hpp>
#include "json_loader.hpp"
#include <iostream>
#include <iomanip>
int main() {
  std::cout<<std::setprecision(17);
  const std::string root="univariateML-cpp/include/cppunivariate/distributions/";
  int failures=0;
  for(const std::string family:{"tnorm","cmb","cmp"}) {
    const auto g=mltest::load_golden(root+family+"/golden.json");
    std::vector<int> counts;for(double k:g.data)counts.push_back(int(k));
    for(int mask=0;mask<4;++mask) {
      double p=0,q=0,ll=0;bool ok=false;
      if(family=="tnorm") {
        auto f=cppunivariate::tnorm_mle({g.data.data(),g.data.size()},-INFINITY,INFINITY,
          1e-12,100,mask&1?g.estimates.at("mean"):NAN,mask&2?g.estimates.at("sd"):NAN);
        ok=f.status==cppunivariate::TnormStatus::Ok;p=f.mean;q=f.sd;ll=f.logLik;
      } else if(family=="cmb") {
        auto f=cppunivariate::cmb_mle({counts.data(),counts.size()},10,1e-10,2000,
          mask&1?g.estimates.at("prob"):NAN,mask&2?g.estimates.at("nu"):NAN);
        ok=f.status==cppunivariate::CmbStatus::Ok;p=f.prob;q=f.nu;ll=f.logLik;
      } else {
        cppunivariate::CmpFitControl c;
        if(mask&1)c.fixed_lambda=g.estimates.at("lambda");if(mask&2)c.fixed_nu=g.estimates.at("nu");
        auto f=cppunivariate::cmp_domain_mle({g.data.data(),g.data.size()},c);
        ok=f.status==cppunivariate::CmpStatus::Ok;p=std::exp(f.eta);q=f.nu;ll=f.logLik;
      }
      const double expected1=g.estimates.at(family=="tnorm"?"mean":family=="cmb"?"prob":"lambda");
      const double expected2=g.estimates.at(family=="tnorm"?"sd":"nu");
      const double e1=std::abs(p-expected1)/(1+std::abs(expected1)),e2=std::abs(q-expected2)/(1+std::abs(expected2));
      const double ell=std::abs(ll-g.logLik)/(1+std::abs(g.logLik));
      failures+=!ok || e1>1e-8 || e2>1e-8 || ell>1e-10;
      std::cout<<family<<" mask="<<mask<<" ok="<<ok<<" coef_errors="<<e1<<","<<e2<<" loglik_error="<<ell<<"\n";
    }
  }
  const std::vector<int> x{0,0,1,2,3,4};
  int refinements=0;double max_delta=0;
  for(int N=4;N<=100;++N) {
    std::uint64_t work=0;
    const auto raw=cppunivariate::cmb_mle_work({x.data(),x.size()},N,1e-10,100,NAN,NAN,work,cppunivariate::enumeration_budget(100));
    const auto f=cppunivariate::cmb_mle({x.data(),x.size()},N,1e-10,100);
    failures+=f.status!=cppunivariate::CmbStatus::Ok;
    if(f.nu!=raw.nu || f.prob!=raw.prob)++refinements;
    max_delta=std::max(max_delta,std::abs(f.nu-raw.nu));
  }
  std::cout<<"CMB 97-size scan: refined="<<refinements<<" max_absolute_nu_change="<<max_delta<<"\nfailures="<<failures<<"\n";
  return failures?1:0;
}
