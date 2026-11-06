#pragma once
#include "../../core/discrete_stop.hpp"
#include "../../core/span.hpp"
#include "dpqr.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>
namespace cppunivariate {
enum class DnormStatus { Ok, InvalidData, InvalidControl, PointMass, TwoPointLimit, ScaleLimit, IterationFailure };
struct DnormResult { double mean, sd, logLik; int iterations; DnormStatus status;   StopDetail stop_detail{};
};
namespace dnorm_detail {
struct Cells {
  std::vector<double> x, w;
  double n=0, centre=0, variance=0;
  bool init(span data) {
    if (data.size()==0) return false;
    std::vector<double> v(data.begin(),data.end());
    for (double z:v) if (!std::isfinite(z) || z!=std::floor(z) || std::abs(z)>4503599627370495.0) return false;
    std::sort(v.begin(),v.end()); n=static_cast<double>(v.size());
    for (std::size_t i=0;i<v.size();) {
      std::size_t j=i+1; while (j<v.size() && v[j]==v[i]) ++j;
      x.push_back(v[i]); w.push_back(static_cast<double>(j-i)); i=j;
    }
    // Centring avoids summation overflow and loss of location precision.
    const double offset=x.front();
    for (std::size_t j=0;j<x.size();++j) centre+=w[j]*(x[j]-offset)/n;
    centre+=offset+0.5;
    for (std::size_t j=0;j<x.size();++j) variance+=w[j]*(x[j]+0.5-centre)*(x[j]+0.5-centre)/n;
    return true;
  }
};
// Negative log likelihood and analytic score/information in A=(mu-c)/sd,
// B=1/sd. With mean held A=(mean-c)*B; with sd held B is constant.
inline double evaluate(const Cells &p,double A,double B,bool hm,bool hs,double held,
                       std::vector<double> &g,std::vector<double> &H) {
  const int d=hm || hs ? 1 : 2;
  g.assign(d,0); H.assign(d*d,0); double f=0;
  for (std::size_t j=0;j<p.x.size();++j) {
    const double l=p.x[j]-p.centre, r=l+1;
    const double a=l*B-A,b=r*B-A;
    const double m=l+0.5,z=m*B-A;
    if (B*(1+std::abs(z))<1e-5) {
      // Known width B avoids cancellation in b-a. Differentiate the
      // narrow-interval expansion directly instead of subtracting huge
      // endpoint density/probability ratios.
      const double c=((z*B)*(z*B)-B*B)/24,den=1+c;
      const double ca=-z*B*B/12,cb=(z*m*B*B+(z*z-1)*B)/12;
      const double ga=z+ca/den,gb=1/B-z*m+cb/den;
      const double haa=-1+B*B/(12*den)-ca*ca/(den*den);
      const double hab=m-(m*B*B+2*z*B)/(12*den)-ca*cb/(den*den);
      const double hbb=-1/(B*B)-m*m+(m*m*B*B+4*z*m*B+z*z-1)/(12*den)-cb*cb/(den*den);
      f-=p.w[j]*(std::log(B)-0.5*z*z-0.91893853320467274178+std::log1p(c));
      if (hm) {
        g[0]-=p.w[j]*(held*ga+gb);
        H[0]-=p.w[j]*(held*held*haa+2*held*hab+hbb);
      } else if (hs) { g[0]-=p.w[j]*ga; H[0]-=p.w[j]*haa; }
      else {
        g[0]-=p.w[j]*ga; g[1]-=p.w[j]*gb;
        H[0]-=p.w[j]*haa; H[1]-=p.w[j]*hab;
        H[2]-=p.w[j]*hab; H[3]-=p.w[j]*hbb;
      }
      continue;
    }
    const double lp=distributions::dnorm_interval(a,b);
    if (!std::isfinite(lp)) return std::numeric_limits<double>::infinity();
    f-=p.w[j]*lp;
    const double fa=std::exp(-0.5*a*a-0.91893853320467274178-lp);
    const double fb=std::exp(-0.5*b*b-0.91893853320467274178-lp);
    const std::vector<double> va=hm ? std::vector<double>{l-held} : hs ? std::vector<double>{-1} : std::vector<double>{-1,l};
    const std::vector<double> vb=hm ? std::vector<double>{r-held} : hs ? std::vector<double>{-1} : std::vector<double>{-1,r};
    std::vector<double> s(d);
    for (int k=0;k<d;++k) { s[k]=fb*vb[k]-fa*va[k]; g[k]-=p.w[j]*s[k]; }
    for (int k=0;k<d;++k) for (int m=0;m<d;++m)
      H[k*d+m]+=p.w[j]*(s[k]*s[m]+b*fb*vb[k]*vb[m]-a*fa*va[k]*va[m]);
  }
  return f;
}
}
inline DnormResult dnorm_mle(span x,double tol=1e-9,int maxit=200,
    double mean=std::numeric_limits<double>::quiet_NaN(),double sd=std::numeric_limits<double>::quiet_NaN()) {
  auto stopped = [&]() -> DnormResult {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  DnormResult out{nan,nan,nan,0,DnormStatus::InvalidData};
  const bool hm=!std::isnan(mean), hs=!std::isnan(sd);
  if (!(tol>0) || !std::isfinite(tol) || maxit<1 || (hm && !std::isfinite(mean)) || (hs && (!(sd>0) || !std::isfinite(sd)))) { out.status=DnormStatus::InvalidControl; return out; }
  dnorm_detail::Cells p; if (!p.init(x)) return out;
  if (!hm && !hs && p.x.back()-p.x.front()<2) {
    out.status=p.x.size()==1 ? DnormStatus::PointMass : DnormStatus::TwoPointLimit; return out;
  }
  if (hm && !hs && p.x.back()<=mean && p.x.front()+1>=mean) { out.status=DnormStatus::ScaleLimit; return out; }
  if (hm && hs) {
    double ll=0; for (std::size_t j=0;j<p.x.size();++j) ll+=p.w[j]*distributions::ddnorm(p.x[j],mean,sd,true);
    return {mean,sd,ll,0,DnormStatus::Ok};
  }
  if (!hm) mean=p.centre;
  if (!hs) sd=std::hypot(std::sqrt(std::max(0.1,p.variance)),hm ? mean-p.centre:0.0);
  double B=1/sd,A=(mean-p.centre)*B;
  const double held=mean-p.centre;
  std::vector<double> g,H;
  double f=dnorm_detail::evaluate(p,A,B,hm,hs,held,g,H);
  for (int it=0;it<maxit;++it) {
    out.iterations=it+1;
    const int d=static_cast<int>(g.size()); std::vector<double> step(d);
    if (d==1) step[0]=-g[0]/H[0];
    else {
      const double det=H[0]*H[3]-H[1]*H[2];
      if (!(det>0)) break;
      step[0]=(-H[3]*g[0]+H[1]*g[1])/det;
      step[1]=(H[2]*g[0]-H[0]*g[1])/det;
    }
    double slope=0; for (int k=0;k<d;++k) slope+=g[k]*step[k];
    if (!std::isfinite(slope) || slope>0) break;
    if (-slope <= tol*tol*p.n) { out.status=DnormStatus::Ok; break; }
    bool accepted=false;
    for (double t=1;t>1e-12;t*=0.5) {
      double nA=A,nB=B;
      if (hm) { nB+=t*step[0]; nA=held*nB; }
      else { nA+=t*step[0]; if (!hs) nB+=t*step[1]; }
      if (!(nB>0)) continue;
      std::vector<double> ng,nH;
      double nf=dnorm_detail::evaluate(p,nA,nB,hm,hs,held,ng,nH);
      if (nf<=f+1e-4*t*slope+1e-14*p.n) {
        A=nA; B=nB; f=nf; g=ng; H=nH; accepted=true; break;
      }
    }
    if (!accepted) break;
  }
  // Permit rounding-limited Newton convergence only with a small score.
  double norm=0; for (double z:g) norm+=z*z;
  if (std::sqrt(norm)<1e-6*p.n) out.status=DnormStatus::Ok;
  if (out.status!=DnormStatus::Ok) out.status=DnormStatus::IterationFailure;
  out.mean=hm ? mean : p.centre+A/B; out.sd=hs ? sd : 1/B;
  out.logLik=0; for (std::size_t j=0;j<p.x.size();++j) out.logLik+=p.w[j]*distributions::ddnorm(p.x[j],out.mean,out.sd,true);
  return out;

  }();
  if(stopped.status==DnormStatus::PointMass) stopped.stop_detail=discrete_point(x[0]);
  if(stopped.status==DnormStatus::TwoPointLimit) {
    const double lo=*std::min_element(x.begin(),x.end());
    stopped.stop_detail=discrete_two_point(x,lo,lo+1);
  }
  if(stopped.status==DnormStatus::ScaleLimit) {
    const double lo=std::floor(mean);
    stopped.stop_detail=mean==lo?discrete_two_point(x,lo-1,lo,0.5):discrete_point(lo);
  }
  return stopped;
}
}
