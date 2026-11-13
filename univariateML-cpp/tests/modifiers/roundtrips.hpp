#pragma once
#include <cstdio>
// TASK-215: raw equality is required unless the explicitly specified fuzz
// crosses the preceding jump. Still require the minimal adjusted crossing.
namespace mltest {
template <typename P, typename Q>
inline void modifier_roundtrips(const std::string &id, P probability, Q quantile,
                                bool report=false,int half_width=100) {
  const std::string source=detail::slurp(fixture_directory(id)+"/truth.json");
  const auto params=truth_matrix(source,"parameters");
  const auto qs=truth_matrix(source,"quantiles");
  for(bool lower:{true,false}) for(bool log:{false,true}) {
    long count=0,up=0,down=0,invalid=0;
    for(std::size_t point=0;point<params.size();++point) {
      double centre=0;
      for(const auto &r:qs) if(r[0]==point && r[1]==1 && r[2]==0 && r[3]==0.5 && std::isfinite(r[4])) centre=std::floor(r[4]);
      // Unit support points cannot be represented beyond 2^53. Use a 201
      // point window at zero there; the existing truth lattice tests cover
      // the bulk on the representable-double lattice separately.
      if(std::abs(centre)>9007199254740890.0) centre=0;
      for(int j=-half_width;j<=half_width;++j) {
        const double k=centre+j, p=probability(k,params[point],lower,log);
        if(!(log ? std::isfinite(p)&&p<0 : p>0&&p<1)) continue;
        const double before=probability(k-1,params[point],lower,log);
        if(p==before) continue;
        ++count;
        const double q=quantile(p,params[point],lower,log);
        if(!std::isfinite(q)) ++invalid;
        else if(q>k) ++up;
        else if(q<k) ++down;
        if(!report) {
          INFO(id << " point=" << point << " k=" << k << " lower=" << lower << " log=" << log << " p=" << p << " q=" << q);
          CHECK(q<=k);
          CHECK(std::isfinite(q));
          CHECK(cppunivariate::special::discrete_reached(probability(q,params[point],lower,log),p,lower,log)==1);
          CHECK(cppunivariate::special::discrete_reached(probability(q-1,params[point],lower,log),p,lower,log)==0);
          if(q==k) continue;
          const bool covered=cppunivariate::special::discrete_reached(before,p,lower,log)==1;
          CHECK(covered);
        }
      }
    }
    if(report) std::printf("%s,%d,%d,%ld,%ld,%ld,%ld\n",id.c_str(),lower,log,count,up,down,invalid);
  }
}
}
