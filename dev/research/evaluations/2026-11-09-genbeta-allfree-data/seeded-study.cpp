#include HEADER
#include <atomic>
#include <chrono>
#include <cstdint>
#include <cstring>
#include <iomanip>
#include <iostream>
#include <mutex>
#include <random>
#include <set>
#include <sstream>
#include <thread>
struct StudyCase {
  int index,model,n,seed;
  double p,q,a;
  std::vector<double> x;
  std::uint32_t hash;
};
int main() {
  int count=0;std::cin>>count;std::set<int> completed;
  for(int i=0;i<count;++i){int index=0;std::cin>>index;completed.insert(index);}
  std::vector<StudyCase> cases;
  for(int model=0;model<2;++model)for(int n:{50,300})
    for(double p:(model==0?std::vector<double>{.3,1.,3.}:std::vector<double>{.5,1.,2.,5.}))
      for(double q:(model==0?std::vector<double>{.3,2.,5.}:std::vector<double>{1.}))
        for(double a:(model==0?std::vector<double>{.7,3.}:std::vector<double>{1.}))
          for(int seed=0;seed<5;++seed) {
            std::mt19937_64 rng(1165700+seed);
            std::gamma_distribution<double> gp(p,1),gq(q,1);
            std::vector<double>x;
            while(int(x.size())<n) {
              const double z=gp(rng),w=model==0?gq(rng):1;
              const double logx=model==0?-std::log1p(w/z)/a:-z/p;
              const double value=std::exp(logx);
              if(value>0&&value<1)x.push_back(value);
            }
            std::uint32_t hash=2166136261u;
            for(double v:x) {
              std::uint64_t bits=0;std::memcpy(&bits,&v,sizeof(v));
              for(unsigned j=0;j<8;++j){hash^=std::uint32_t((bits>>(8*j))&255);hash*=16777619u;}
            }
            cases.push_back({int(cases.size()),model,n,seed,p,q,a,std::move(x),hash});
          }
  std::atomic<std::size_t> next{0};std::mutex output;
  std::vector<std::thread> workers;
  for(int worker=0;worker<4;++worker)workers.emplace_back([&]() {
    for(;;) {
      const auto index=next.fetch_add(1);
      if(index>=cases.size())return;
      const auto& c=cases[index];if(completed.count(c.index))continue;
      const auto start=std::chrono::steady_clock::now();
      const auto r=cppunivariate::genbeta_mle({c.x.data(),c.x.size()},1);
      const double ms=std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count();
#ifdef LEGACY
      const bool certified=false;
#else
      const bool certified=r.allfree_certified;
#endif
      std::ostringstream row;row<<std::setprecision(17);
      row<<c.index<<' '<<c.model<<' '<<c.n<<' '<<c.p<<' '<<c.q<<' '<<c.a<<' '<<c.seed<<' '
        <<int(r.status)<<' '<<certified<<' '<<int(r.certificate_failure)<<' '<<r.iterations<<' '<<ms<<' '
        <<r.logLik<<' '<<r.shape1<<' '<<r.shape2<<' '<<r.shape3<<' '<<c.hash<<'\n';
      const std::lock_guard<std::mutex> lock(output);std::cout<<row.str()<<std::flush;
    }
  });
  for(auto& worker:workers)worker.join();
}
