// Global nonuniqueness proofs from existence, exact data symmetry and strict
// likelihood exclusion of the symmetry center. Candidates are witnesses,
// not claimed coefficient enclosures for the unknown maximizing points.
#pragma once
#include "shape_certificate.hpp"
namespace cppunivariate {
namespace t_symmetry_detail {
struct Result {int outcome=0,evaluations=0;StopDetail detail{};}; // 1 proof, 2 budget
inline void combine(Result& out,const StopDetail& left,const StopDetail& right) {
  out.outcome=1;out.detail.candidates={left.candidates.front(),right.candidates.front()};
  out.detail.loglik_lower=std::max(left.loglik_lower,right.loglik_lower);
  out.detail.loglik_upper=INFINITY;out.detail.comparison_tie=true;
}
inline Result held_sd(span x,double sd) {
  Result out;out.detail.loglik_upper=INFINITY;
  if(x.size()<2||!(sd>0)||!std::isfinite(sd))return out;
  const double first=x[0];double second=NAN;std::size_t mass=0;
  for(double v:x){
    if(!std::isfinite(v))return out;
    if(v==first)++mass;
    else if(std::isnan(second))second=v;
    else if(v!=second)return out;
  }
  if(!std::isfinite(second)||mass!=x.size()-mass)return out;
  const double low=std::min(first,second),high=std::max(first,second);
  const auto half=std::isfinite(high-low)?(t_cert_detail::I(high)-low)*.5:
    t_cert_detail::I(high)*.5-t_cert_detail::I(low)*.5;
  const auto separation=half/sd;
  // For z>15, z^6>9(1+4z^2)^2, using 4+1/z^2<=5.
  // Variance one bounds every centered density at z by 3/(2*z^3).
  // A nu=3 off-center witness beats that bound, including the normal face.
  if(!separation.valid()||!(separation.lo>15))return out;
  StopDetail left,right;
  t_shape_detail::retain_point(x,low,sd,3,true,left);++out.evaluations;
  t_shape_detail::retain_point(x,high,sd,3,true,right);++out.evaluations;
  if(left.candidates.empty()||right.candidates.empty())return out;
  combine(out,left,right);return out;
}
// TwoSum verifies equality of the exact binary64 input sums, rather than
// treating a rounded equality as exact symmetry. No fast-math is assumed.
inline bool symmetric(span sorted,double center) {
  const double twice=2*center;if(!std::isfinite(twice))return false;
  for(std::size_t i=0;i<(sorted.size()+1)/2;++i){
    const double a=sorted[i],b=sorted[sorted.size()-1-i],sum=a+b;
    if(!std::isfinite(sum)||sum!=twice)return false;
    const double tail=sum-a;
    const double error=(a-(sum-tail))+(b-tail);
    if(!std::isfinite(tail)||error!=0)return false;
  }
  return true;
}
inline Result subunit(span x,span sorted,double df,int maxit,double witness_mu=NAN,double witness_scale=NAN) {
  Result out;out.detail.loglik_upper=INFINITY;
  maxit=static_cast<int>(solver_budget(maxit));
  if(x.size()<3||sorted.size()!=x.size()||!(df>0&&df<1)||maxit<1)return out;
  const double center=sorted[0]/2+sorted[sorted.size()-1]/2;
  if(!std::isfinite(center)||!symmetric(sorted,center))return out;
  const double unit=sorted[sorted.size()-1]-sorted[0];
  if(!(unit>0)||!std::isfinite(unit))return out;
  std::size_t mass=0,run=0,zeros=0;
  for(std::size_t j=0;j<sorted.size();++j){
    if(!std::isfinite(sorted[j]))return out;
    run=j&&sorted[j]==sorted[j-1]?run+1:1;mass=std::max(mass,run);zeros+=sorted[j]==center;
  }
  if(!(std::fma(df,static_cast<double>(x.size()-mass),-static_cast<double>(mass))>0))return out;
  const auto charge=[&](){
    if(out.evaluations>=certificate_budget(maxit)){out.outcome=2;out.detail.budget_exhausted=true;return false;}
    ++out.evaluations;return true;
  };
  std::vector<t_cert_detail::I> q;t_cert_detail::Sum squares;double least=INFINITY;
  for(double v:x){
    auto z=t_cert_detail::sq((t_cert_detail::I(v)-center)/unit);
    if(!z.valid())return out;
    z.lo=std::max(0.,z.lo);q.push_back(z);squares.add(z);
    if(v!=center){if(!(z.lo>0))return out;least=std::min(least,z.lo);}
  }
  const double n=x.size();
  const auto exponent=t_cert_detail::I(df)*(n-zeros)-zeros;
  const auto lower=t_cert_detail::root(t_cert_detail::I(least)*exponent/(n*df));
  const auto upper=t_cert_detail::root((t_cert_detail::I(1)+t_cert_detail::I(1)/df)*squares.get()/n);
  if(!exponent.valid()||exponent.lo<=0||!lower.valid()||lower.lo<=0||!upper.valid()||upper.hi<=0)return out;
  const auto log_lower=t_cert_detail::log(lower),log_upper=t_cert_detail::log(upper);
  if(!log_lower.valid()||!log_upper.valid())return out;
  double lo=log_lower.lo,hi=log_upper.hi;
  if(!(lo<hi))return out;
  for(int it=0;it<maxit&&hi-lo>1e-10;++it){
    if(!charge())return out;
    const double mid=lo/2+hi/2;
    if(!(mid>lo&&mid<hi))break;
    const auto scale2=t_cert_detail::exp(t_cert_detail::I(2*mid));
    t_cert_detail::Sum fractions;
    for(auto z:q){
      auto f=z/(t_cert_detail::I(df)*scale2+z);
      if(!f.valid())return out;
      f.lo=std::max(0.,f.lo);f.hi=std::min(1.,f.hi);fractions.add(f);
    }
    const auto score=t_cert_detail::I(df+1)*fractions.get()-n;
    if(!score.valid())return out;
    if(score.lo>0)lo=mid;else if(score.hi<0)hi=mid;else break;
  }
  // The exact monotone scale root remains inside this log-scale interval.
  if(!charge())return out;
  const t_cert_detail::I log_scale(lo,hi);const auto scale2=t_cert_detail::exp(log_scale*2);
  t_shape_detail::Engine profile(center,unit,false,maxit);
  profile.constant=t_cert_detail::I(-n)*(.5*std::log(2*pi)+t_cert_detail::log(t_cert_detail::I(unit))+log_scale);
  for(auto z:q)profile.q.push_back(z/scale2);
  const auto centered=profile.value(df);
  if(!centered.valid())return out;
  if(!charge())return out;
  t_shape_detail::retain_point(x,center,unit*std::exp(log_scale.mid()),df,false,out.detail);
  const std::uint64_t length=sorted.size()-1;
  const std::uint64_t count=std::min<std::uint64_t>(length,solver_budget(maxit));
  for(std::uint64_t pass=0;pass<count;++pass){
    // A bounded set nominates witnesses only. Exclusion of the entire
    // centered profile and exact reflection supply the global tie proof.
    const std::size_t j=static_cast<std::size_t>(pass*(length/count)+(pass*(length%count))/count);
    const std::size_t mirror=sorted.size()-2-j;
    const double left_mu=sorted[j]/2+sorted[j+1]/2;
    const double right_mu=sorted[mirror]/2+sorted[mirror+1]/2;
    const double left_scale=(sorted[j+1]-sorted[j])/2;
    const double right_scale=(sorted[mirror+1]-sorted[mirror])/2;
    if(!(left_mu<center&&right_mu>center&&left_scale>0&&right_scale>0))continue;
    StopDetail left,right;
    if(!charge())return out;
    t_shape_detail::retain_point(x,left_mu,left_scale,df,false,left);
    if(left.candidates.empty()||!(left.loglik_lower>centered.hi))continue;
    if(!charge())return out;
    t_shape_detail::retain_point(x,right_mu,right_scale,df,false,right);
    if(right.candidates.empty()||!(right.loglik_lower>centered.hi))continue;
    combine(out,left,right);return out;
  }
  // The joint search can nominate a narrower witness than adjacent-atom
  // midpoints. The centered-profile upper bound and exact input symmetry
  // still supply the proof; the optimizer supplies no global claim.
  if(std::isfinite(witness_mu)&&witness_mu!=center&&witness_scale>0&&std::isfinite(witness_scale)){
    StopDetail left,right;
    if(!charge())return out;
    t_shape_detail::retain_point(x,witness_mu,witness_scale,df,false,left);
    if(!left.candidates.empty()&&left.loglik_lower>centered.hi){
      if(!charge())return out;
      t_shape_detail::retain_point(x,center+(center-witness_mu),witness_scale,df,false,right);
      if(!right.candidates.empty()&&right.loglik_lower>centered.hi){combine(out,left,right);return out;}
    }
  }
  return out;
}
} // namespace t_symmetry_detail
} // namespace cppunivariate
