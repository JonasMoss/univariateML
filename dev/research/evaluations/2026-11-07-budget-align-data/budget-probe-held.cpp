#include "cppunivariate/cppunivariate.hpp"
#include <iostream>
int main(){std::vector<double> support{-3.3753182637169354,-1.2713352565886626,-.8001089566898665,.4550011648748676,.4631607376683885,.48440591294209223,1.2931658287635732};std::vector<int>counts{1,11,8,11,7,8,9};std::vector<double>x;for(unsigned i=0;i<support.size();++i)x.insert(x.end(),counts[i],support[i]);
for(int it:{1,2,3,4,5,6,7,8,10,20}){auto a=cppunivariate::lst_mle({x.data(),x.size()},1e-6,it);auto b=cppunivariate::student_t_mle({x.data(),x.size()},1e-6,it);std::cout<<it<<" "<<int(a.status)<<" "<<int(b.status)<<" "<<a.iterations<<" "<<b.iterations<<std::endl;}}
