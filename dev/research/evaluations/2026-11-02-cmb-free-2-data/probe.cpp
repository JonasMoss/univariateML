#include "cppunivariate/distributions/cmb/mle.hpp"
#include "../../../../univariateML-cpp/tests/json_loader.hpp"
#include <cstdio>
#include <limits>
#include <random>
#include <vector>
static void trace_held(const std::vector<int>& x,int N,double p) {
  double target=0;for(int v:x)target+=cppunivariate::cmb_detail::logchoose(N,v);
  target/=x.size();
  const double eta=std::log(p)-std::log1p(-p);
  double nu=1;
  for(int it=0;it<100;++it) {
    const cppunivariate::cmb_detail::Table tab(N,eta,nu);
    double ll=0;for(int v:x)ll+=tab.lp[v];
    const double g=target-tab.mh;
    if(it<5 || it>94)std::printf("trace N=%d it=%d nu=%.12g g=%.12g vh=%.12g\n",N,it,nu,g,tab.vh);
    if(std::abs(g)<=1e-10*(1+std::abs(target)))return;
    if(!(tab.vh>0))return;
    const double dv=std::max(-4.,std::min(4.,g/tab.vh));
    bool accepted=false;
    for(double step=1;step>1e-12;step*=.5) {
      const cppunivariate::cmb_detail::Table next(N,eta,nu+step*dv);
      double next_ll=0;for(int v:x)next_ll+=next.lp[v];
      if(std::isfinite(next_ll) && next_ll>=ll-1e-12*(1+std::abs(ll))) {nu+=step*dv;accepted=true;break;}
    }
    if(!accepted)return;
  }
}
int main() {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  double lo=.6,hi=.8;
  for(int i=0;i<80;++i){const double p=(lo+hi)/2;const double f=4*(1-p)*(p*p+(1-p)*(1-p))-p;if(f>0)lo=p;else hi=p;}
  const double p=(lo+hi)/2;const std::vector<int> pair{1,2};
  const auto tie=cppunivariate::cmb_size_free_nu_mle({pair.data(),pair.size()},p);
  std::printf("tie p=%.17g status=%d size=%g ll=%g\n",p,int(tie.fit.status),tie.size,tie.fit.logLik);
  std::fflush(stdout);
  trace_held(std::vector<int>{0,0,1,2,3,4},101,.5);
  std::fflush(stdout);
  for(const std::vector<int> x : {std::vector<int>{0,0,1,2,3,4}, {0,1,2,2,3,4}}) {
    for(double p:{nan,.3,.5}) {
      std::size_t work=0;
      double best=-INFINITY;
      for(int N=4;N<=36;++N) {
        const auto f=cppunivariate::cmb_detail::size_candidate({x.data(),x.size()},N,p,1e-10,100,work,10000000);
        double upper=0;
        const bool checked=f.status==cppunivariate::CmbStatus::Ok && cppunivariate::cmb_detail::fixed_profile_upper({x.data(),x.size()},N,f,!std::isnan(p),upper);
        if(!checked || std::abs(upper-f.logLik)>1e-8*(1+std::abs(f.logLik))) {
          std::printf("candidate fail p=%g N=%d status=%d checked=%d gap=%g work=%zu\n",p,N,int(f.status),int(checked),upper-f.logLik,work);
          if(!std::isnan(p)) {
            const auto old=cppunivariate::cmb_mle({x.data(),x.size()},N,1e-10,100,p,nan);
            double old_upper=0;
            const bool old_ok=old.status==cppunivariate::CmbStatus::Ok && cppunivariate::cmb_detail::fixed_profile_upper({x.data(),x.size()},N,old,true,old_upper);
            std::printf("rescue status=%d upper_ok=%d gap=%g\n",int(old.status),int(old_ok),old_upper-old.logLik);
          }
          break;
        }
        best=std::max(best,f.logLik);
      }
      std::printf("initial p=%g work=%zu best=%.12g\n",p,work,best);
      for(int K:{8,16,32}) {
        double upper=0;
        const bool ok=cppunivariate::cmb_detail::truncated_upper({x.data(),x.size()},K,upper,work,10000000);
        std::printf("truncated K=%d ok=%d upper=%.12g work=%zu\n",K,int(ok),upper,work);
      }
      const double B=best-1e-8*(1+std::abs(best));
      std::printf("log-lower endpoints: %.12g %.12g\n",cppunivariate::cmb_detail::observed_log_lower({x.data(),x.size()},0,B),cppunivariate::cmb_detail::observed_log_lower({x.data(),x.size()},4,B));
      std::fflush(stdout);
      auto r=cppunivariate::cmb_size_free_nu_mle({x.data(),x.size()},p);
      std::printf("max=%d p=%g status=%d size=%g fitted_p=%g nu=%g ll=%g\n",x.back(),p,int(r.fit.status),r.size,r.fit.prob,r.fit.nu,r.fit.logLik);
      std::fflush(stdout);
    }
  }
  std::mt19937 rng(1142026);
  std::uniform_int_distribution<int> draw(0,4);
  std::vector<int> random_sample(100);
  for(int& v:random_sample)v=draw(rng);
  for(double probability:{nan,.5}) {
    const auto fit=cppunivariate::cmb_size_free_nu_mle({random_sample.data(),random_sample.size()},probability);
    std::printf("random p=%g status=%d size=%g ll=%g\n",probability,int(fit.fit.status),fit.size,fit.fit.logLik);
    std::fflush(stdout);
  }
  const auto golden=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/cmb/golden.json");
  std::vector<int> data;for(double v:golden.data)data.push_back(int(v));
  for(double probability:{nan,golden.estimates.at("prob")}) {
    const int max_value=*std::max_element(data.begin(),data.end());
    std::size_t work=0;bool initial_ok=true;double initial_best=-INFINITY;
    for(int N=max_value;N<=max_value+32;++N) {
      const auto candidate=cppunivariate::cmb_detail::size_candidate_rescue({data.data(),data.size()},N,probability,1e-10,100,work,10000000);
      double upper=0;
      const bool checked=candidate.status==cppunivariate::CmbStatus::Ok &&
        cppunivariate::cmb_detail::fixed_profile_upper({data.data(),data.size()},N,candidate,!std::isnan(probability),upper);
      if(!checked || std::abs(upper-candidate.logLik)>1e-8*(1+std::abs(candidate.logLik))) {
        std::printf("golden initial failure p=%g N=%d status=%d checked=%d work=%zu\n",probability,N,int(candidate.status),int(checked),work);
        initial_ok=false;break;
      }
      initial_best=std::max(initial_best,candidate.logLik);
    }
    std::printf("golden initial p=%g ok=%d work=%zu best=%.12g\n",probability,int(initial_ok),work,initial_best);
    if(initial_ok)for(int K:{8,16,32,64}) {
      double upper=0;
      const bool checked=cppunivariate::cmb_detail::truncated_upper({data.data(),data.size()},K,upper,work,10000000);
      std::printf("golden truncated p=%g K=%d ok=%d upper=%.12g work=%zu\n",probability,K,int(checked),upper,work);
    }
    const auto fit=cppunivariate::cmb_size_free_nu_mle({data.data(),data.size()},probability);
    std::printf("golden p=%g status=%d size=%g ll=%g\n",probability,int(fit.fit.status),fit.size,fit.fit.logLik);
    std::fflush(stdout);
  }
}
