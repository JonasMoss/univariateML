#include "cppunivariate/distributions/ged/mle.hpp"
#include <iostream>
#include <iomanip>
#include <sstream>
int main(){std::string line;while(std::getline(std::cin,line)){std::istringstream in(line);int n;double p,q,m,s,v;std::string a,b;in>>n>>p>>q>>a>>b;m=std::stod(a);s=std::stod(b);std::vector<double>x;for(int i=0;i<n;++i){in>>v;x.push_back(v);}const auto r=cppunivariate::ged_shape_detail::score_box({x.data(),x.size()},p,q,m,s,200);std::cout<<std::setprecision(17)<<r.valid<<" "<<r.location.lo<<" "<<r.location.hi<<" "<<r.score.lo<<" "<<r.score.hi<<" "<<r.logscale.lo<<" "<<r.logscale.hi<<" "<<r.derivative.lo<<" "<<r.derivative.hi<<"\n";}}
