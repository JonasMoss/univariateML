#include <cppunivariate/distributions/zinb/global_certificate.hpp>
#include <cppunivariate/distributions/rng_std.hpp>
#include <cppunivariate/distributions/zip/dpqr.hpp>
#include <cppunivariate/distributions/nbinom/dpqr.hpp>
#include <iomanip>
#include <iostream>
int main(){
  std::cout<<std::setprecision(17);
  int mode,n,mask,maxit;double r,p,pi;
  while(std::cin>>mode>>n>>mask>>r>>p>>pi>>maxit){
    std::vector<int>x(n);for(int&v:x)std::cin>>v;
    if(mode==2) {
      cppunivariate::distributions::StdRng<> rng(123),prng(5);
      std::vector<int> zip,pois,nb;
      for(int i=0;i<5000;++i){zip.push_back(int(cppunivariate::distributions::rzip(rng,4,.4)));pois.push_back(int(prng.rpois(4)));nb.push_back(int(cppunivariate::distributions::rnbinom(rng,2,.4)));}
      x=p==0?zip:(p==1?pois:nb);
    }
    if(mode==0 || mode==2 || mode==3){
      const auto z=cppunivariate::zinb_certified_mle({x.data(),x.size()},1e-10,maxit,(mask&1)?r:NAN,(mask&2)?p:NAN,(mask&4)?pi:NAN);
      std::cout<<int(z.fit.status)<<' '<<int(z.failure)<<' '<<z.fit.iterations<<' '<<z.fit.size<<' '<<z.fit.prob<<' '<<z.fit.pi<<' '<<z.fit.logLik<<' '<<z.stop_detail.loglik_lower<<' '<<z.stop_detail.loglik_upper<<' '<<z.stop_detail.candidates.size()<<' '<<int(z.stop_detail.limit_model)<<' '<<z.stop_detail.limit_loglik;
      if(mode==2){std::cout<<' '<<x.size();for(int v:x)std::cout<<' '<<v;}
      if(mode==3){const auto old=cppunivariate::zinb_candidate_mle({x.data(),x.size()});std::cout<<' '<<int(old.status)<<' '<<old.size<<' '<<old.prob<<' '<<old.pi<<' '<<old.logLik;}
      std::cout<<'\n';
    }else{
      double al,ah,bl,bh;std::cin>>al>>ah>>bl>>bh;
      cppunivariate::zinb_global_detail::Data d;d.pi=(mask&4)?pi:NAN;d.n=n;
      for(int v:x){++d.counts[v];d.S+=v;d.k+=v>0;d.M=std::max(d.M,v);}
      d.n0=d.n-d.k;d.z=d.n0/d.n;
      for(const auto& kv:d.counts)d.factorial-=kv.second*cppunivariate::special::lgamma(kv.first+1.);
      const auto a=d.at((al+ah)/2,(bl+bh)/2);const auto c=d.curvature(al,ah,bl,bh);const auto f=cppunivariate::zinb_global_detail::zip_upper(d,maxit);
      std::cout<<a.ll<<' '<<a.error<<' '<<a.ga<<' '<<a.gb<<' '<<a.ge<<' '<<a.aa<<' '<<a.ab<<' '<<a.bb<<' '<<c.aa.lo<<' '<<c.aa.hi<<' '<<c.ab.lo<<' '<<c.ab.hi<<' '<<c.bb.lo<<' '<<c.bb.hi<<' '<<f.ok<<' '<<f.lambda<<' '<<f.pi<<' '<<f.lower<<' '<<f.upper<<'\n';
    }
  }
}
