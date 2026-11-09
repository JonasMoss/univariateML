#include "cppunivariate/distributions/ztbinom/mle.hpp"
#include "cppunivariate/distributions/zmbinom/mle.hpp"
#include "cppunivariate/distributions/zib/mle.hpp"
#include "cppunivariate/distributions/tbinom/mle.hpp"
#include <chrono>
#include <iostream>
#include <vector>
template<class F> void time(const char* name,F f) {
 const auto start=std::chrono::steady_clock::now();
 for(int i=0;i<3;++i)f();
 std::cout<<name<<" "<<std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count()/3<<" ms\n";
}
int main(){
 const std::vector<int> positive{1,1,1,2,2,8},mixed{0,0,0,1,1,1,2,2,8},finite{1,2,2,3,3,3,4,4,5,5},thin{2,3,4};
 const cppunivariate::ispan p{positive.data(),positive.size()},m{mixed.data(),mixed.size()},x{finite.data(),finite.size()},t{thin.data(),thin.size()};
 time("ztbinom face",[&]{volatile auto f=cppunivariate::ztbinom_mle(p);(void)f;});
 time("zmbinom face",[&]{volatile auto f=cppunivariate::zmbinom_mle(m);(void)f;});
 time("zib face",[&]{volatile auto f=cppunivariate::zib_mle(m);(void)f;});
 time("tbinom face",[&]{volatile auto f=cppunivariate::tbinom_design_mle(t,NAN,NAN,NAN);(void)f;});
 time("ztbinom finite",[&]{volatile auto f=cppunivariate::ztbinom_mle(x);(void)f;});
 time("tbinom finite",[&]{volatile auto f=cppunivariate::tbinom_design_mle(x,0,10,NAN);(void)f;});
 const std::vector<int> inclusive{0,1,1,2,2,3,3,4,4,4};
 const cppunivariate::ispan z{inclusive.data(),inclusive.size()};
 time("tbinom zero-inclusive",[&]{volatile auto f=cppunivariate::tbinom_design_mle(z,-1,4,NAN);(void)f;});
}
