#include "cppunivariate/distributions/bnbinom/mle.hpp"
#include <iostream>
int main(){std::vector<int>x;
  for(auto pair:std::array<std::array<int,2>,7>{{{{969,3182}},{{970,1816}},{{1001,1}},{{1003,1}},{{1004,1}},{{1032,1785}},{{1033,3215}}}})
    x.insert(x.end(),pair[1],pair[0]);
  const auto n=cppunivariate::nbinom_mle({x.data(),x.size()},1e-10,600);
  const auto b=cppunivariate::bnbinom_mle({x.data(),x.size()});
  std::cout<<"nb_status "<<static_cast<int>(n.status)<<" nb_candidates "<<n.stop_detail.candidates.size()<<" bnb_status "<<static_cast<int>(b.status)<<'\n';
  for(const auto &c:b.stop_detail.candidates)std::cout<<"candidate_model "<<static_cast<int>(c.model)<<" coefficient_count "<<c.coef.size()<<'\n';
}
