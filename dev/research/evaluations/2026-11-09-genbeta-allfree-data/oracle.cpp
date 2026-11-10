#include "cppunivariate/distributions/genbeta/mle.hpp"
#include <iostream>
#include <iomanip>
int main(){std::cout<<std::setprecision(17);
 for(double h:{.02,.1,1.,5.,20.})for(double q:{.1,.7,2.,10.}){
 auto r=cppunivariate::genbeta_free_p_cert::inner_range(h*.9,h*1.1,q*.8,q*1.2,300);
 std::cout<<"I "<<h<<" "<<q<<" "<<r.enclosed<<" "<<r.lower<<" "<<r.upper<<"\n";}
 for(int sample=0;sample<3;++sample){std::vector<double> y=sample==0?std::vector<double>{.1,.3,1.,2.6}:sample==1?std::vector<double>{.5,1.5}:std::vector<double>{.01,.1,.5,3.39};
 for(double h:{.02,.1,1.,5.,20.}){auto proposal=cppunivariate::genbeta_detail::profile(y,h,NAN,NAN,300);
 auto r=cppunivariate::genbeta_allfree_bounds::conditional(y,h,proposal.a,proposal.b,0,300);
 std::cout<<"C "<<sample<<" "<<h<<" "<<r.enclosed<<" "<<r.shape1_lower<<" "<<r.shape1_upper<<" "<<r.shape2_lower<<" "<<r.shape2_upper<<" "<<r.loglik_lower<<" "<<r.loglik_upper<<"\n";
 // Deliberately nonstationary point: the tangent must still be an upper bound.
 auto b=cppunivariate::genbeta_allfree_bounds::conditional(y,h,proposal.a*1.1,proposal.b*.9,0,300);
 std::cout<<"B "<<sample<<" "<<h<<" "<<b.enclosed<<" "<<b.loglik_upper<<"\n";}
 auto g=cppunivariate::gamma_mle({y.data(),y.size()},1e-10,300);
 auto t=cppunivariate::genbeta_allfree_bounds::tails(y,1,0,g.shape,1.,300);
 std::cout<<"S "<<sample<<" "<<t.gamma_loglik_upper<<" "<<t.small_quadratic_domain<<" "<<t.small_quadratic_coefficient<<"\n";
 std::cout<<"Q "<<sample<<" "<<t.ridge_shape2_lower<<" "<<t.ridge_shape2_upper<<" "<<t.ridge_loglik_upper<<"\n";
 std::cout<<"T "<<sample<<" "<<t.small_cutoff<<" "<<t.small_upper<<" "<<t.large_cutoff<<" "<<t.large_upper<<"\n";}}
