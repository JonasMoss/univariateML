// Reproduce from the repository root; commands and seed in the final report.
#include "cppunivariate/special/special.hpp"
#include "cppunivariate/distributions/frechet/mle.hpp"
#include "cppunivariate/distributions/gev/dpqr.hpp"
#include <chrono>
#include <iomanip>
#include <iostream>
#include <random>
#include <fstream>
#include <string>

// Reconstruct selected incomplete covers from the returned search estimate.
// This diagnoses the next missing premise; it is not production tracing.
template<class F,class P>
void diagnose(std::ostream& out,int sample_id,const std::string& family,double shape,int n,int rep,
              const std::string& hold,cppunivariate::gev_held_detail::Box domain,
              double x,double y,F evaluate,P precise) {
  int precision_calls=0;bool isolated=false;
  cppunivariate::gev_held_detail::Box last{};
  auto checked=[&](cppunivariate::gev_held_detail::Box b,double tol) {
    ++precision_calls;last=b;const bool ok=precise(b,tol);isolated=isolated||ok;return ok;
  };
  const auto r=cppunivariate::gev_held_detail::cover(domain,x,y,1e-10,100,evaluate,checked);
  const std::string phase=r.certified?"reconstructed_closure":(r.contradicted?"counterexample":
    (r.evaluations>=cppunivariate::certificate_budget(100)?(isolated?"global_budget":"local_budget"):
     (isolated?"global_stagnation":(precision_calls?"coefficient_inclusion":"local_inclusion"))));
  out<<sample_id<<','<<family<<','<<shape<<','<<n<<','<<rep<<','<<hold<<','<<phase<<','<<r.evaluations<<','
     <<precision_calls<<','<<last.x.hi-last.x.lo<<','<<last.y.hi-last.y.lo<<std::endl;
}
void diagnose_gev(std::ostream& out,int sample_id,std::vector<double> data,double shape,int n,int rep,
                  bool held_scale,cppunivariate::GevResult r) {
  std::sort(data.begin(),data.end());const double origin=data.front();
  const double unit=held_scale?1:std::max(data.back()-origin,std::abs(origin));
  for(double& v:data)v=(v-origin)/unit;
  const cppunivariate::span z{data.data(),data.size()};
  const double mu=-origin/unit,level=r.logLik/n+std::log(unit);
  r.mu=(r.mu-origin)/unit;r.sigma/=unit;
  cppunivariate::gev_held_detail::Box domain{{-1,1},{0,0}};
  if(!held_scale) {
    double logs=0,j=0;for(double d:data)if(d!=mu) {logs+=std::log(std::abs(d-mu));++j;}
    if(2*j<=n)return;
    domain.y={(n*level+2*logs)/(2*j-n),-level};
  } else {
    const double radius=std::sqrt(std::exp(-level));domain.y={-radius,data.back()+radius};
  }
  domain.y=cppunivariate::gev_joint_detail::pad(domain.y,1e-8*(1+cppunivariate::gev_joint_detail::magnitude(domain.y)));
  auto eval=[&](cppunivariate::gev_held_detail::Box b) {return cppunivariate::gev_held_detail::gev_evaluate(z,b,!held_scale,held_scale?1:mu,100);};
  auto precise=[&](cppunivariate::gev_held_detail::Box b,double t) {
    return b.x.hi-b.x.lo<=t*.25*std::abs((b.x.lo+b.x.hi)/2)&&
      (!held_scale?std::expm1(b.y.hi-b.y.lo)<=t*.25:unit*(b.y.hi-b.y.lo)<=t*.25*std::max(unit,std::abs(origin+unit*(b.y.lo+b.y.hi)/2)));
  };
  diagnose(out,sample_id,"gev",shape,n,rep,held_scale?"sigma":"mu",domain,r.xi,
           held_scale?r.mu:std::log(r.sigma),eval,precise);
}
void diagnose_frechet(std::ostream& out,int sample_id,std::vector<double> data,double shape,int n,int rep,
                      cppunivariate::FrechetResult r) {
  const double origin=*std::min_element(data.begin(),data.end());
  double D=0,k=0;for(double& v:data) {v-=origin;D=std::max(D,v);if(v==0)++k;}
  const cppunivariate::span z{data.data(),data.size()};const double c=std::asinh(D/2);
  double upper=std::max({2.,2*n/c,2*std::sqrt(double(n))/c});
  for(int i=0;i<100;++i) {
    const double bound=n*std::log1p(upper)+std::max(-(upper+1)*c,c-1-upper*upper*c*c/2);
    if(bound<r.logLik-1e-8*(n+std::abs(r.logLik)))break;
    upper*=2;
  }
  const cppunivariate::gev_held_detail::Box domain{{1,upper},{std::log(k/(2*n))-1e-8,1e-8}};
  auto eval=[&](cppunivariate::gev_held_detail::Box b) {return cppunivariate::frechet_held_detail::evaluate(z,b);};
  auto precise=[](cppunivariate::gev_held_detail::Box b,double t) {
    return b.x.hi-b.x.lo<=t*.25*(b.x.lo+b.x.hi)/2&&std::expm1(b.y.hi-b.y.lo)<=t*.25;
  };
  diagnose(out,sample_id,"frechet",shape,n,rep,"sigma",domain,r.lambda,std::log(origin-r.mu),eval,precise);
}

int main(int argc,char** argv) {
  const int maxit=argc>1?std::stoi(argv[1]):100;
  std::ofstream roots(argc>3?argv[3]:(argc>2?argv[2]:"build/scratch/combination-unused-roots.jsonl"));
  roots<<std::setprecision(17);
  auto record=[&](int id,const char* family,const char* hold,const std::vector<double>& x,int status,bool certified,std::vector<double> coef) {
    roots<<"{\"sample_id\":"<<id<<",\"family\":\""<<family<<"\",\"hold\":\""<<hold<<"\",\"status\":"<<status<<",\"certified\":"<<certified<<",\"coef\":[";
    for(std::size_t j=0;j<coef.size();++j)roots<<(j?",":"")<<coef[j];
    roots<<"],\"data\":[";for(std::size_t j=0;j<x.size();++j)roots<<(j?",":"")<<x[j];roots<<"]}\n";
  };
  std::ofstream diagnostics("build/scratch/combination-unused-diagnostics.csv");
  diagnostics<<"sample_id,family,shape,n,rep,hold,phase,evaluations,precision_calls,shape_width,nuisance_width\n"<<std::setprecision(17);
  std::mt19937_64 rng(17020261110ULL);
  std::uniform_real_distribution<double> uniform(0,1);
  std::cout<<"sample_id,family,shape,n,rep,hold,status,certified,milliseconds,loglik\n"<<std::setprecision(17);
  int sample_id=0;
  for(double shape:{-.9,-.5,0.,.5,1.,2.})for(int n:{3,10,50,100,500})for(int rep=0;rep<2;++rep) {
    const int gev_sample=sample_id++;
    std::vector<double> x;for(int i=0;i<n;++i)x.push_back(cppunivariate::distributions::qgev(uniform(rng),0,1,shape,true,false));
    for(int hold=0;hold<2;++hold) {
      const auto start=std::chrono::steady_clock::now();
      const auto r=cppunivariate::gev_mle({x.data(),x.size()},1e-10,maxit,NAN,hold?NAN:0,hold?1:NAN);
      const double ms=std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count();
      std::cout<<gev_sample<<",gev,"<<shape<<','<<n<<','<<rep<<','<<(hold?"sigma":"mu")<<','<<int(r.status)<<','<<r.certified<<','<<ms<<','<<r.logLik<<std::endl;
      record(gev_sample,"gev",hold?"sigma":"mu",x,int(r.status),r.certified,{r.mu,r.sigma,r.xi});
      if(false&&maxit==100&&!r.certified&&r.status==cppunivariate::GevStatus::Ok&&rep==0&&(n==3||n==500))diagnose_gev(diagnostics,gev_sample,x,shape,n,rep,hold,r);
    }
    // Positive GEV shapes correspond to Frechet lambda=1/xi. Also sample
    // lambda=2 at xi=0 for a regular held-scale comparison across all n.
    if(shape>0||shape==0) {
      const int frechet_sample=sample_id++;
      const double lambda=shape>0?1/shape:2;
      std::vector<double> f;for(int i=0;i<n;++i)f.push_back(std::pow(-std::log(uniform(rng)),-1/lambda));
      const auto start=std::chrono::steady_clock::now();
      const auto r=cppunivariate::frechet_mle({f.data(),f.size()},1e-10,maxit,NAN,NAN,1);
      const double ms=std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count();
      std::cout<<frechet_sample<<",frechet,"<<lambda<<','<<n<<','<<rep<<",sigma,"<<int(r.status)<<','<<r.certified<<','<<ms<<','<<r.logLik<<std::endl;
      record(frechet_sample,"frechet","sigma",f,int(r.status),r.certified,{r.lambda,r.mu,r.sigma});
      if(false&&maxit==100&&!r.certified&&r.status==cppunivariate::FrechetStatus::Ok&&rep==0&&(n==3||n==500))diagnose_frechet(diagnostics,frechet_sample,f,lambda,n,rep,r);
    }
  }
}
