#include "cppunivariate/distributions/ztbinom/mle.hpp"
#include "cppunivariate/distributions/zmbinom/mle.hpp"
#include "cppunivariate/distributions/zib/mle.hpp"
#include "cppunivariate/distributions/tbinom/mle.hpp"
#include <iomanip>
#include <iostream>
#include <vector>
int main(){
 std::cout<<std::setprecision(17);
 for(const std::vector<int> x : {std::vector<int>{1,1,1,2,3,7,12},std::vector<int>{1,1,1,2,2,8},std::vector<int>{1,2,2,3,3,3,4,4,5,5}}){
  const cppunivariate::ispan s{x.data(),x.size()};
  const auto f=cppunivariate::ztbinom_mle(s);
  cppunivariate::binomial_certificate_detail::Work w{0,0,200};
  const auto face=cppunivariate::ztpois_mle(s);
  const double end=cppunivariate::binomial_certificate_detail::signed_cutoff(s,1,INFINITY,face.lambda,w);
  std::cout<<"ztbinom max="<<x.back()<<" status="<<int(f.status)<<" tie="<<f.stop_detail.comparison_tie<<" upper="<<f.stop_detail.loglik_upper<<" ll="<<f.logLik<<" face="<<face.logLik<<" signed_end="<<end<<"\n";
 }
 for(const std::vector<int> x : {std::vector<int>{1,2,3},std::vector<int>{2,3,4},std::vector<int>{0,0,0,4,8}}){
  const auto f=cppunivariate::tbinom_design_mle({x.data(),x.size()},NAN,NAN,NAN);
  std::cout<<"tbinom max="<<x.back()<<" status="<<int(f.status)<<" tie="<<f.stop_detail.comparison_tie<<" upper="<<f.stop_detail.loglik_upper<<" face="<<f.stop_detail.limit_loglik<<"\n";
 }
 const std::vector<int>x{0,0,0,0,0,0,0,0,1,1,1,2,3,7,12};
 const cppunivariate::ispan s{x.data(),x.size()};
 for(double pi : {double(NAN),0.,.2}) {
  const auto f=cppunivariate::zib_mle(s,1e-10,200,NAN,NAN,pi);
  std::cout<<"zib pi="<<pi<<" status="<<int(f.status)<<" tie="<<f.stop_detail.comparison_tie<<" upper="<<f.stop_detail.loglik_upper<<"\n";
 }
 const auto f=cppunivariate::zmbinom_mle(s,1e-10,200,NAN,NAN,.2);
 std::cout<<"zmbinom status="<<int(f.status)<<" tie="<<f.stop_detail.comparison_tie<<" upper="<<f.stop_detail.loglik_upper<<"\n";
 const std::vector<int>open_zib{0,0,0,0,0,1,1,2,2,4};
 const auto oz=cppunivariate::zib_mle({open_zib.data(),open_zib.size()},1e-10,200,NAN,NAN,.2);
 std::cout<<"open zib status="<<int(oz.status)<<" tie="<<oz.stop_detail.comparison_tie<<" upper="<<oz.stop_detail.loglik_upper<<" rate="<<(oz.stop_detail.limit_coef.empty()?NAN:oz.stop_detail.limit_coef[0])<<"\n";
 std::vector<int>open_tbinom{0};open_tbinom.insert(open_tbinom.end(),9,1);open_tbinom.insert(open_tbinom.end(),6,2);open_tbinom.insert(open_tbinom.end(),10,3);
 const auto ot=cppunivariate::tbinom_design_mle({open_tbinom.data(),open_tbinom.size()},-1,3,NAN);
 std::cout<<"exact moment tbinom status="<<int(ot.status)<<" tie="<<ot.stop_detail.comparison_tie<<" upper="<<ot.stop_detail.loglik_upper<<"\n";

 const double q=std::exp(-1.6),pi=.75*q/(.25+.75*q);
 const auto eq=cppunivariate::zib_mle({open_zib.data(),open_zib.size()},1e-10,200,NAN,NAN,pi);
 std::cout<<"near-zero local coefficient zib pi="<<pi<<" status="<<int(eq.status)<<" tie="<<eq.stop_detail.comparison_tie<<" upper="<<eq.stop_detail.loglik_upper<<"\n";

 const std::vector<int>boundary{0,1,1,1,2,4};
 const auto zb=cppunivariate::zib_mle({boundary.data(),boundary.size()});
 std::cout<<"free ZIP boundary status="<<int(zb.status)<<" tie="<<zb.stop_detail.comparison_tie<<" upper="<<zb.stop_detail.loglik_upper<<" pi="<<(zb.stop_detail.limit_coef.size()<2?NAN:zb.stop_detail.limit_coef[1])<<"\n";

 std::vector<int>mixed(50,0);mixed.insert(mixed.end(),41,1);mixed.insert(mixed.end(),33,2);mixed.insert(mixed.end(),3,3);mixed.insert(mixed.end(),3,4);
 const auto mt=cppunivariate::tbinom_design_mle({mixed.data(),mixed.size()},-1,4,NAN);
 std::cout<<"mixed higher moments tbinom status="<<int(mt.status)<<" tie="<<mt.stop_detail.comparison_tie<<" upper="<<mt.stop_detail.loglik_upper<<"\n";

 std::vector<int>tied(198,0);tied.push_back(1);tied.push_back(19);
 const auto tf=cppunivariate::zib_mle({tied.data(),tied.size()},1e-10,200,NAN,NAN,.7254971052291191);
 int modes=0;for(const auto& c:tf.stop_detail.candidates)if(c.model==cppunivariate::StopModel::Zip)++modes;
 std::cout<<"winning tied ZIP status="<<int(tf.status)<<" comparison="<<tf.stop_detail.comparison_tie<<" upper="<<tf.stop_detail.loglik_upper<<" face_modes="<<modes<<"\n";
 std::vector<int>finite_tie(99,0);finite_tie.push_back(10);
 const auto ff=cppunivariate::zib_mle({finite_tie.data(),finite_tie.size()},1e-10,200,NAN,NAN,.7254971052291191);
 std::cout<<"finite winner above tied ZIP status="<<int(ff.status)<<" size="<<ff.size<<" ll="<<ff.logLik<<"\n";

 const std::vector<int>equal_dispersion{0,1,1,1,2,3,4,4};
 const auto ed=cppunivariate::zib_mle({equal_dispersion.data(),equal_dispersion.size()});
 std::cout<<"noninflated equality face status="<<int(ed.status)<<" comparison="<<ed.stop_detail.comparison_tie<<" upper="<<ed.stop_detail.loglik_upper<<" pi="<<(ed.stop_detail.limit_coef.size()<2?NAN:ed.stop_detail.limit_coef[1])<<"\n";

}
