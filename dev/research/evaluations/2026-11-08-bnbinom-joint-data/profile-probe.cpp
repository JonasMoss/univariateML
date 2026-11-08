// Quantify the missing all-free profile certificate; not a fitting route.
#include "cppunivariate/distributions/bnbinom/mle.hpp"
#include <fstream>
#include <iostream>
#include <iomanip>
int main(int argc,char **argv) {
  if(argc!=2)return 1;
  std::ifstream input(argv[1]);std::vector<int> x;int value;
  std::map<int,double> counts;
  while(input>>value){x.push_back(value);counts[value]+=1;}
  cppunivariate::bnbinom_detail::Data d;
  d.n=x.size();double mean=0,moment=0,positive=0;
  for(const auto &kv:counts){d.x.push_back(kv.first);d.count.push_back(kv.second);mean+=kv.first*kv.second/d.n;moment+=kv.second*kv.first*(kv.first-1.);if(kv.first>0)positive+=kv.second;}
  if(x.empty())return 2;
  const std::vector<double> p{1.0672234787479615,2.1419791546447171,5.3647338351087397};
  const double witness=cppunivariate::bnbinom_detail::likelihood(d,p);
  const double L=witness-cppunivariate::bnbinom_detail::alpha_likelihood_bound(d,p[1],{p[0],p[0],p[2],p[2]}).margin;
  const auto nb=cppunivariate::nbinom_mle({x.data(),x.size()},1e-10,600);
  const auto po=cppunivariate::pois_mle({x.data(),x.size()});
  if(nb.status!=cppunivariate::NbinomStatus::Ok || !(L>nb.logLik && L>po.logLik))return 3;
  double lower=1;
  for(int step=0;step<600;++step) {
    double bound=0;
    for(std::size_t i=0;i<d.x.size();++i)if(d.x[i]>0)
      bound+=d.count[i]*std::min(0.,static_cast<double>(cppunivariate::bnbinom_detail::increment(lower,d.x[i]))-cppunivariate::special::lgamma(d.x[i]+1.));
    if(bound+1e-6<L)break;
    lower*=.5;
  }
  double M=std::max(1.,2*mean);
  for(int step=0;step<600;++step) {
    double bound=-d.n*M;
    for(std::size_t i=0;i<d.x.size();++i)
      bound+=d.count[i]*(static_cast<double>(cppunivariate::bnbinom_detail::increment(M,d.x[i]))-cppunivariate::special::lgamma(d.x[i]+1.));
    if(bound+1e-6<L)break;
    M*=2;
  }
  const double A=4*d.n*M*M+.5*moment;
  const double U=std::max(4*M,4*A/(L-po.logLik));
  // Stronger CDF bound: P_r(X<=x|Q)<=P_delta(X<=x|Q)
  // <= (delta+1)_x/x!*Q^delta, delta=min(lower,1).
  const double delta=std::min(lower,1.);
  double logp=-std::log(2.);
  for(int step=0;step<100000;++step) {
    double bound=0;
    for(std::size_t i=0;i<d.x.size();++i)
      bound+=d.count[i]*std::min(0.,static_cast<double>(cppunivariate::bnbinom_detail::increment(delta+1,d.x[i]))-cppunivariate::special::lgamma(d.x[i]+1.)+delta*logp);
    if(bound+1e-6<L)break;
    logp-=std::log(2.);
  }
  const double logq=L/positive-std::log(std::max(U,1.))-1;
  const double C=cppunivariate::bnbinom_detail::pair_nb_remainder(d,U,std::exp(logp),std::exp(logq));
  const double S=2*C/(L-nb.logLik-1e-6);
  std::cout<<std::setprecision(17)<<"{\n\"witness\":"<<witness<<",\n\"nb_face\":"<<nb.logLik<<",\n\"poisson_face\":"<<po.logLik<<",\n\"r_lower\":"<<lower<<",\n\"m_upper\":"<<M<<",\n\"canonical_r_upper\":"<<U<<",\n\"log_p_lower\":"<<logp<<",\n\"log_q_lower\":"<<logq<<",\n\"sum_shapes_upper\":"<<S<<",\n\"conditional_alpha_probes\":[\n";
  bool first=true;
  for(double r:{lower,1.,U})for(double b:{U,S}) {
    cppunivariate::bnbinom_detail::Cost cost{d,{r,NAN,b},{1}};
    const auto f=cppunivariate::bnbinom_detail::scalar_fit(cost,1e-8,600);
    if(!first)std::cout<<",\n";first=false;
    std::cout<<"{\"r\":"<<r<<",\"beta\":"<<b<<",\"status\":"<<static_cast<int>(f.status)<<",\"has_estimate\":"<<(f.status==cppunivariate::BnbinomStatus::Ok?"true":"false")<<"}";
  }
  std::cout<<"\n]}\n";
}
