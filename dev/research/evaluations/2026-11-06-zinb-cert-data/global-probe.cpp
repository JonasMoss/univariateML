#include <cppunivariate/distributions/zinb/global_certificate.hpp>
#include <iostream>
#include <iomanip>
int main(){int n;std::cin>>n;std::vector<int>x(n);for(int&v:x)std::cin>>v;for(double pi:std::vector<double>{NAN,.25738761635216689,.7}){auto z=cppunivariate::zinb_joint_certificate({x.data(),x.size()},pi);std::cout<<std::setprecision(17)<<int(z.fit.status)<<" "<<int(z.failure)<<" "<<z.fit.iterations<<" "<<z.fit.size<<" "<<z.fit.prob<<" "<<z.fit.pi<<" "<<z.stop_detail.loglik_lower<<" "<<z.stop_detail.segment_loglik_upper<<"\n";}}
