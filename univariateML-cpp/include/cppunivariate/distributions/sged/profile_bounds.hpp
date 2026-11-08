#pragma once
// SGED-local interval arithmetic. Bounds are conditional on computed
// comparisons with practical floating-point margins, not directed rounding.
namespace sged_profile {
struct Range {double lo,hi;};
inline Range add(Range a,Range b){return {a.lo+b.lo,a.hi+b.hi};}
inline Range neg(Range a){return {-a.hi,-a.lo};}
inline Range mul(Range a,Range b){return {std::min({a.lo*b.lo,a.lo*b.hi,a.hi*b.lo,a.hi*b.hi}),std::max({a.lo*b.lo,a.lo*b.hi,a.hi*b.lo,a.hi*b.hi})};}
inline Range square(Range a){return {a.lo<=0 && a.hi>=0?0:std::min(a.lo*a.lo,a.hi*a.hi),std::max(a.lo*a.lo,a.hi*a.hi)};}
inline Range divide(Range a,Range b){if(!(b.lo>0))return {-INFINITY,INFINITY};return mul(a,{1/b.hi,1/b.lo});}
inline Range logrange(Range a){return a.lo>0?Range{std::log(a.lo),std::log(a.hi)}:Range{-INFINITY,INFINITY};}
inline Range exp_range(Range a){return {std::exp(a.lo),std::exp(a.hi)};}
inline double margin(double magnitude,double n){return 4096*std::numeric_limits<double>::epsilon()*(1+magnitude)*(1+n*std::numeric_limits<double>::epsilon());}
struct Sum {double value=0,error=0;void add(double v){const double y=v-error,t=value+y;error=(t-value)-y;value=t;}};
inline Range padded(double lo,double hi,double n,double conditioning=1) {
  const double pad=margin(std::max(std::abs(lo),std::abs(hi)),n)*conditioning;
  return {lo-pad,hi+pad};
}
inline Range power(double a,double b,double p,double q) {
  return {a==0?0:std::min(std::pow(a,p),std::pow(a,q)),std::max(std::pow(b,p),std::pow(b,q))};
}
inline Range powerlog(double a,double b,double p,double q) {
  const auto f=[](double r,double v){return r==0?0:std::pow(r,v)*std::log(r);};
  Range out{std::min({f(a,p),f(a,q),f(b,p),f(b,q)}),std::max({f(a,p),f(a,q),f(b,p),f(b,q)})};
  const double c=std::exp(-1/p);if(a<=c && c<=b)out.lo=std::min(out.lo,-1/(std::exp(1.)*p));return out;
}
inline Range powerlog2(double a,double b,double p,double q) {
  const auto f=[](double r,double v){return r==0?0:std::pow(r,v)*std::log(r)*std::log(r);};
  Range out{std::min({f(a,p),f(a,q),f(b,p),f(b,q)}),std::max({f(a,p),f(a,q),f(b,p),f(b,q)})};
  if(a<=1 && b>=1)out.lo=0;
  const double c=std::exp(-2/p);if(a<=c && c<=b)out.hi=std::max(out.hi,4/(std::exp(2.)*p*p));return out;
}
inline Range hessian_power(double a,double b,double p,double q) {
  if(a>0)return {std::min({std::pow(a,p-2),std::pow(a,q-2),std::pow(b,p-2),std::pow(b,q-2)}),std::max({std::pow(a,p-2),std::pow(a,q-2),std::pow(b,p-2),std::pow(b,q-2)})};
  return {q>2?0:(b>0?std::min(std::pow(b,p-2),std::pow(b,q-2)):0),p<2?INFINITY:std::max({1.,std::pow(b,p-2),std::pow(b,q-2)})};
}
// k=G(2/p)/G(1/p), v=G(3/p)/G(1/p). Ratios are NOT monotone
// over p>=1; a digamma derivative enclosure in t=1/p bounds them.
inline Range moment_ratio(double p,double q,double order) {
  const double t=(1/p+1/q)/2;
  const double value=-std::log(order)+special::lgamma(1+order*t)-special::lgamma(1+t);
  const Range d{order*special::digamma(1+order/q)-special::digamma(1+1/p),order*special::digamma(1+order/p)-special::digamma(1+1/q)};
  const double spread=std::max(std::abs(d.lo),std::abs(d.hi))*(1/p-1/q)/2+margin(std::abs(value),1)*(1+q);
  return {std::exp(value-spread),std::exp(value+spread)};
}
struct PublicBox {Range mean{NAN,NAN},logsd{NAN,NAN},logxi{NAN,NAN};bool valid=false;};
inline PublicBox public_box(Range c,Range a,Range b,double p,double q) {
  PublicBox out;
  if(!(a.lo>0 && b.lo>0) || !std::isfinite(a.hi+b.hi))return out;
  const auto k=moment_ratio(p,q,2),v=moment_ratio(p,q,3);
  const auto diff=add(b,neg(a));
  const auto variance=add(mul(add(v,neg(square(k))),square(diff)),mul(v,mul(a,b)));
  if(!(variance.lo>0) || !std::isfinite(variance.hi))return out;
  out.mean=add(c,mul(k,diff));out.logsd=mul({.5,.5},logrange(variance));
  out.logxi=mul({.5,.5},add(logrange(b),neg(logrange(a))));
  out.valid=std::isfinite(out.mean.lo+out.mean.hi+out.logsd.lo+out.logsd.hi+out.logxi.lo+out.logxi.hi);return out;
}
struct System {
  span original;std::vector<double> values;double origin=NAN,unit=NAN,n=0,offset=NAN;bool valid=false;
  explicit System(span x):original(x),n(static_cast<double>(x.size())) {
    const auto mm=std::minmax_element(x.begin(),x.end());origin=*mm.first/2+*mm.second/2;
    unit=std::max(std::abs(*mm.first-origin),std::abs(*mm.second-origin));
    if(!(unit>=std::numeric_limits<double>::min()) || !std::isfinite(unit))return;
    std::vector<double> sorted(x.begin(),x.end());std::sort(sorted.begin(),sorted.end());
    for(double v:sorted)values.push_back((v-origin)/unit);
    for(std::size_t j=1;j<values.size();++j)if(sorted[j]!=sorted[j-1] && values[j]==values[j-1])return;
    offset=n*std::log(unit);valid=std::isfinite(offset);
  }
  span data()const{return {values.data(),values.size()};}
  double actual(double mean,double sd,double p,double xi)const {
    const double phi[4]={mean,std::log(sd),std::log(p),std::log(xi)};return -SgedCost{original}(phi);
  }
  SgedResult recover(double c,double a,double b,double p,double ideal)const {
    SgedResult out{NAN,NAN,p,NAN,NAN,0,SgedStatus::CertificateFailure};
    if(!(a>0 && b>0) || !std::isfinite(c+a+b+p+ideal))return out;
    const double k=std::exp(special::lgamma(1+2/p)-special::lgamma(1+1/p))/2;
    const double v=std::exp(special::lgamma(1+3/p)-special::lgamma(1+1/p))/3;
    const double scale=std::max(a,b),d=(b-a)/scale;
    const double sd=scale*std::sqrt((v-k*k)*d*d+v*(a/scale)*(b/scale));
    const double m=std::fma(k,b-a,c);
    out.mu=std::fma(unit,m,origin);out.sigma=unit*sd;out.xi=std::exp((std::log(b)-std::log(a))/2);
    if(!(out.sigma>0 && out.xi>0) || !std::isfinite(out.mu+out.sigma+out.xi))return out;
    if(std::abs((out.mu-origin)/unit-m)>2e-8 || std::abs(std::log(out.sigma/unit)-std::log(sd))>2e-8)return out;
    if(256*std::numeric_limits<double>::epsilon()*(std::abs(origin)+std::abs(unit*m))>2e-7*(1+std::abs(out.mu)))return out;
    out.logLik=actual(out.mu,out.sigma,p,out.xi);
    const double expected=ideal-offset;
    const double allowance=margin(std::abs(expected)+std::abs(out.logLik)+n*(5+std::abs(std::log(out.sigma))),n)*(1+p);
    if(!std::isfinite(out.logLik) || std::abs(out.logLik-expected)>allowance)return out;
    out.status=SgedStatus::Ok;return out;
  }
  double original_upper(double upper,double p)const {
    if(!std::isfinite(upper))return INFINITY;
    return upper-offset+margin(std::abs(upper)+std::abs(offset)+n*10,n)*(1+p);
  }
  bool narrow(const PublicBox& box)const {
    if(!box.valid)return false;
    const double mid=box.mean.lo/2+box.mean.hi/2;
    const double original_mid=std::fma(unit,mid,origin);
    const double error=256*std::numeric_limits<double>::epsilon()*(std::abs(origin)+std::abs(unit*mid));
    return unit*(box.mean.hi-box.mean.lo)+2*error<=2e-7*(1+std::abs(original_mid)) && box.logsd.hi-box.logsd.lo<=2e-7 && box.logxi.hi-box.logxi.lo<=2e-7;
  }
  bool compatible(PublicBox box,const SgedResult& r)const {
    if(!box.valid || !(r.sigma>0 && r.xi>0))return false;
    const double mean=(r.mu-origin)/unit,sd=std::log(r.sigma/unit),xi=std::log(r.xi);
    if(!std::isfinite(mean+sd+xi))return false;
    box.mean={std::min(box.mean.lo,mean),std::max(box.mean.hi,mean)};
    box.logsd={std::min(box.logsd.lo,sd),std::max(box.logsd.hi,sd)};
    box.logxi={std::min(box.logxi.lo,xi),std::max(box.logxi.hi,xi)};
    return narrow(box);
  }
};
inline FitCandidate witness(const SgedResult& r,double n) {
  const double pad=margin(std::abs(r.logLik)+n*(5+std::abs(std::log(r.sigma))),n)*(1+r.nu);
  return {{r.mu,r.sigma,r.nu,r.xi},r.logLik-pad,r.logLik+pad};
}
} // namespace sged_profile
