#include "cppunivariate/distributions/emg/shape_bounds.hpp"
#include <iostream>
#include <iomanip>
#include <vector>
#include <array>
int main() {
  std::vector<std::array<double,2>> ranges;
  const double threshold=std::exp(.005);
  for(double scale:{1e-200,1.,1e200})for(double ratio:{1.,std::nextafter(1.,INFINITY),1.+1e-12,1.001,std::nextafter(threshold,0.),threshold,std::nextafter(threshold,INFINITY),1.01,2.,100.})ranges.push_back({{scale,scale*ratio}});
  ranges.push_back({{1e-300,1e300}});ranges.push_back({{1,1e150}});
  ranges.push_back({{1e-300,1e-150}});ranges.push_back({{1e150,1e300}});
  ranges.push_back({{std::numeric_limits<double>::denorm_min(),std::numeric_limits<double>::min()}});
  std::cout<<std::setprecision(17)<<"lo,hi,n,upper\n";
  for(const auto& range:ranges)for(std::size_t n:{1,7,500,10000})
    std::cout<<range[0]<<','<<range[1]<<','<<n<<','<<cppunivariate::emg_detail::fixed_rate_precision_chord_upper(-10,-11,n,range[0],range[1])<<'\n';
}
