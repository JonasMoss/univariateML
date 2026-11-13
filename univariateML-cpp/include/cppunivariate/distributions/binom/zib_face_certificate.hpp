#pragma once
#include "ztbinom_face_certificate.hpp"
namespace cppunivariate {
namespace zib_fit_detail {
// Uniform in inflation. At a large-size probability maximum, u=N*p is
// in [S/n,S/k], k=#positive. See the report for the normalized-pmf bound.
inline double signed_zip_cutoff(ispan x,binomial_certificate_detail::Work& work,bool& exhausted) {
  exhausted=false;
  if(!work.charge(static_cast<std::int64_t>(x.size()))){exhausted=true;return INFINITY;}
  double S=0,F2=0,k=0;int mx=0;
  for(int v:x){S+=v;F2+=double(v)*(v-1);k+=v>0;mx=std::max(mx,v);}
  if(!(k>0))return INFINITY;
  const double pm=S/k,eps=8192*std::numeric_limits<double>::epsilon();
  const double gap=F2-S*pm-eps*(1+F2+S*pm);
  if(!(gap>0))return INFINITY;
  const double C=(S*pm*pm+(x.size()-k)*std::pow(pm,4)/32)*(1+eps)+eps;
  const double end=std::ceil(std::max({double(mx)+1,2*pm,4*C/gap}));
  if(!(end<4503599627370496.)){work.numerical=true;return INFINITY;}
  return end;
}
// Localize the necessary rate interval when the inflation-uniform gap is
// too loose. The certified ZIP supremum controls boxes with negative A;
// tangent/chord bounds exclude the other boxes by a strict face-value gap.
inline double localized_zip_cutoff(ispan x,double pi,double face,
    binomial_certificate_detail::Work& work,bool& exhausted) {
  exhausted=false;
  if(!(pi>0 && pi<1))return INFINITY;
  if(!work.charge(static_cast<std::int64_t>(x.size()))){exhausted=true;return INFINITY;}
  double S=0,F2=0,k=0,factorial=0;int mx=0;
  for(int v:x){S+=v;F2+=double(v)*(v-1);k+=v>0;mx=std::max(mx,v);if(v>0)factorial+=special::lgamma(v+1.);}
  if(!(k>0 && k<x.size()))return INFINITY;
  const double n0=x.size()-k,pm=S/k,m=S/x.size();
  const double eps=8192*std::numeric_limits<double>::epsilon();
  const double C=(S*pm*pm+n0*std::pow(pm,4)/32)*(1+eps)+eps;
  const double globalA=std::max(0.,(S*pm-F2)/2)+eps*(1+F2+S*pm);
  const double constant=k*std::log1p(-pi)-factorial;
  const double margin=1e-9*x.size()+eps*(1+std::abs(face));
  const auto zero=[&](double u) {
    const double a=std::log(pi),b=std::log1p(-pi)-u;
    return std::max(a,b)+std::log1p(std::exp(-std::abs(a-b)));
  };
  std::vector<std::pair<double,double>> pending{{m,pm}};
  double end=std::max(double(mx)+1,2*pm);
  while(!pending.empty()) {
    if(++work.evaluations>certificate_budget(work.maxit)){exhausted=true;return INFINITY;}
    const auto box=pending.back();pending.pop_back();
    const double a=box.first,b=box.second,t=a+(b-a)/2;
    const double wlo=std::exp(std::log1p(-pi)-b-zero(b));
    const double A=-F2/2+b*S-k*b*b/2-n0*a*a*wlo/2+eps*(1+F2+S*pm+n0*pm*pm);
    if(A<0) {
      end=std::max(end,2*C/(-A));
    } else {
      // Concave positive-count tangent plus the convex zero-term chord.
      // Their sum is affine, so its upper bound is at an endpoint.
      const double base=S*std::log(t)-k*t+constant,slope=S/t-k;
      const double raw=std::max(base+slope*(a-t)+n0*zero(a),base+slope*(b-t)+n0*zero(b));
      const double upper=raw+eps*(1+std::abs(raw)+S+n0);
      const double gap=face-upper;
      if(gap>margin) {
        end=std::max({end,4*globalA/gap,std::sqrt(4*C/gap)});
      } else {
        const double ll=base+n0*zero(t);
        const double w=std::exp(std::log1p(-pi)-t-zero(t));
        const double At=-F2/2+t*S-(k+n0*w)*t*t/2;
        if(ll>face+margin){work.numerical=true;return INFINITY;}
        // A near-winning point without a negative first-order margin needs
        // a higher-order or equality argument. Do not subdivide to a cap.
        if(ll>=face-margin && At>=-eps*(1+F2+S*pm))return INFINITY;
        if(t==a || t==b){work.numerical=true;return INFINITY;}
        pending.push_back({a,t});pending.push_back({t,b});
      }
    }
    if(!(end<4503599627370496.)){work.numerical=true;return INFINITY;}
  }
  return std::ceil(end);
}
// Free inflation with a noninflated ZIP face. The caller supplies the
// ordinary binomial Poisson-escape certificate. If b < -log(z), then
// q_N(u) >= exp(-b-b*b/N) > z for sufficiently large N, forcing pi=0.
inline double boundary_zip_cutoff(ispan x,double face,
    binomial_certificate_detail::Work& work,bool& exhausted) {
  exhausted=false;
  if(!work.charge(static_cast<std::int64_t>(x.size()))){exhausted=true;return INFINITY;}
  double S=0,F2=0,k=0,factorial=0;int mx=0;
  for(int v:x){S+=v;F2+=double(v)*(v-1);k+=v>0;mx=std::max(mx,v);if(v>0)factorial+=special::lgamma(v+1.);}
  const double n=x.size(),n0=n-k;
  if(!(k>0 && n0>0))return INFINITY;
  const double m=S/n,v=S/k,z=n0/n,threshold=-std::log(z);
  const double eps=8192*std::numeric_limits<double>::epsilon();
  if(!(threshold>m+eps*(1+threshold+m)))return INFINITY;
  const double C=(S*v*v+n0*std::pow(v,4)/32)*(1+eps)+eps;
  const double globalA=std::max(0.,(S*v-F2)/2)+eps*(1+F2+S*v);
  const double margin=1e-9*n+eps*(1+std::abs(face));
  std::vector<std::pair<double,double>> pending{{m,v}};
  double end=std::max(double(mx)+1,2*v);
  while(!pending.empty()) {
    if(++work.evaluations>certificate_budget(work.maxit)){exhausted=true;return INFINITY;}
    const auto box=pending.back();pending.pop_back();
    const double a=box.first,b=box.second;
    if(b<threshold-eps*(1+threshold+b)) {
      end=std::max(end,2*b*b/(threshold-b));
    } else {
      // Positive conditional Poisson likelihood is concave in log rate.
      // The zero-indicator term is bounded over q >= exp(-b).
      const double lo=std::log(a),hi=std::log(b),eta=lo+(hi-lo)/2,u=std::exp(eta);
      const double normalizer=u+std::log(-std::expm1(-u));
      const double gradient=S-k*u/(-std::expm1(-u));
      const double zero=b<threshold?-n0*b+k*std::log(-std::expm1(-b)):
        n0*std::log(z)+k*std::log(k/n);
      const double raw=S*eta-k*normalizer-factorial+std::abs(gradient)*(hi-lo)/2+zero;
      const double upper=raw+eps*(1+std::abs(raw)+S+n0);
      const double gap=face-upper;
      if(gap>margin)end=std::max({end,4*globalA/gap,std::sqrt(4*C/gap)});
      else {
        if(u==a || u==b){work.numerical=true;return INFINITY;}
        pending.push_back({a,u});pending.push_back({u,b});
      }
    }
    if(!(end<4503599627370496.)){work.numerical=true;return INFINITY;}
  }
  return std::ceil(end);
}
} // namespace zib_fit_detail
} // namespace cppunivariate
