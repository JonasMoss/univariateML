#include "cppunivariate/distributions/paralogis/mle.hpp"
#include <iostream>
#include <iomanip>
int main(){
  const std::vector<double> x{std::nextafter(1.,0.)};
  const auto r=cppunivariate::paralogis_mle_fixed_scale({x.data(),x.size()},1);
  const cppunivariate::span data{x.data(),x.size()};
  const double alternative=cppunivariate::paralogis_competing_shape(data,1,r.shape);
  const auto c=cppunivariate::paralogis_cert_detail::certify(data,alternative,1,true);
  const double l=std::log(x[0]);
  const double e=cppunivariate::burr_cert_detail::pad*2*std::abs(l)+std::numeric_limits<double>::denorm_min();
  const cppunivariate::paralogis_cert_detail::Data d({{l-e,l+e}},true);
  std::array<cppunivariate::burr_cert_detail::Interval,3> g;
  cppunivariate::burr_cert_detail::BoxMatrix h;
  cppunivariate::burr_cert_detail::Interval ll;
  cppunivariate::paralogis_cert_detail::derivatives(d,alternative,0,g,h,ll);
  std::cout<<std::setprecision(17)<<"x\tstatus\tshape\tlogLik\talternative\tcertificate_ok\tcertificate_shape\tboxes\tbudget\tpoint_log_axis_lower\tminimum_representable_lower_tail\n"
    <<x[0]<<'\t'<<int(r.status)<<'\t'<<r.shape<<'\t'<<r.logLik<<'\t'<<alternative<<'\t'<<c.ok<<'\t'<<c.shape<<'\t'<<c.boxes<<'\t'<<c.budget<<'\t'<<ll.lo<<'\t'<<2*std::log(std::numeric_limits<double>::denorm_min())<<'\n';
  return r.status==cppunivariate::ParalogisStatus::Ok?1:0;
}
