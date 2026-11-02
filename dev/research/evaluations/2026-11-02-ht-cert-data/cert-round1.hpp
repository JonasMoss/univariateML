// Included by mle.hpp inside namespace cppunivariate.
namespace ht_detail {
struct HtInterval { double lo, hi; };

// log(1+w)-w/(1+w), evaluated without cancellation for small w.
inline double score_h(double z, bool upper) {
  if (z < -4) {
    const double w=std::exp(z);
    const double u=w*w*(0.5+w*(-2.0/3+w*(0.75+w*(-0.8+w*5.0/6))));
    return upper ? u : std::max(0.0,u-(6.0/7)*w*w*w*w*w*w*w);
  }
  return softplus(z)-logistic(z);
}

// Bounds on all scale roots in an interval of shape. The contribution to
// the scale score is monotone in shape, with a sign depending on distance.
inline bool scale_box(const std::vector<double>& logs, double n, double a,
                      double b, double &lo, double &hi) {
  if (logs.empty() || !std::isfinite(a) || !std::isfinite(b) || !(a>0)) return false;
  const double la=std::log(a), lb=std::log(b);
  const double mn=*std::min_element(logs.begin(),logs.end());
  const double mx=*std::max_element(logs.begin(),logs.end());
  auto score=[&](double u, bool upper) {
    double v=-n;
    for(double d:logs) {
      const double x=(a+1)*logistic(2*(d-u)-la);
      const double y=(b+1)*logistic(2*(d-u)-lb);
      v+=upper?std::max(x,y):std::min(x,y);
    }
    return v;
  };
  const double p=static_cast<double>(logs.size()), k=n-p;
  const double al=p-k/a, bl=p-k/b;
  if (!(al>0) || !(bl>0)) return false;
  // The extreme-distance root bracket holds uniformly over [a,b].
  const double l=mn+0.5*std::log(al/n);
  const double h=mx+0.5*std::log(bl/n);
  double left=l,right=h;
  for(int i=0;i<100;++i) {
    const double mid=left+(right-left)/2;
    if(mid==left || mid==right) break;
    if(score(mid,false)>0) left=mid; else right=mid;
  }
  lo=left;
  left=l; right=h;
  for(int i=0;i<100;++i) {
    const double mid=left+(right-left)/2;
    if(mid==left || mid==right) break;
    if(score(mid,true)>0) left=mid; else right=mid;
  }
  hi=right;
  return std::isfinite(lo)&&std::isfinite(hi)&&lo<=hi;
}

// A bounded-interval certificate, conditional on floating point comparisons.
// It keeps the legacy grid/refinement as an initializer, never as a proof.
inline HtResult certified_search(span x, double held_sigma, bool free_mu,
                                 const HtResult& initial, int budget=100000) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const bool fs=!std::isnan(held_sigma);
  const double n=static_cast<double>(x.size()), eps=1e-8*n;
  std::vector<double> logs;
  double sumlog=0;
  for(double d:x) if(d>0) {logs.push_back(std::log(d)); sumlog+=std::log(d);}
  const double p=static_cast<double>(logs.size()), k=n-p;
  if(logs.empty()) return initial;
  const bool tie=free_mu&&!fs&&2*k==n;
  double normal_ls=fs?std::log(held_sigma):0;
  const double normal_ll=normal(x,normal_ls,fs);
  if(!std::isfinite(normal_ll)||!std::isfinite(normal_ls)) return {nan,nan,nan,0,HtStatus::IterationFailure};
  double face=-inf;
  if(tie) face=n*std::log(2/std::acos(-1.0))-2*sumlog;
  double best=-inf, bt=0, bu=0;
  if(initial.status==HtStatus::Ok && initial.nu>0 && initial.sigma>0 && std::isfinite(initial.logLik)) {
    best=initial.logLik; bt=std::log(initial.nu); bu=std::log(initial.sigma);
  }
  const double fixed_u=fs?std::log(held_sigma):0;
  bool failed=false;
  auto evaluate=[&](double t, double& error) {
    const double nu=std::exp(t);
    double l=fixed_u, h=fixed_u;
    if(!std::isfinite(nu) || !(nu>0) || (!fs && !scale_box(logs,n,nu,nu,l,h))) {
      failed=true; error=inf; return -inf;
    }
    const double u=l+(h-l)/2;
    double ll=n*(std::log(2.0)+log_mode(nu)-u);
    double magnitude=std::abs(ll);
    for(double d:logs) {
      const double term=0.5*(nu+1)*softplus(2*(d-u)-t);
      ll-=term; magnitude+=std::abs(term);
    }
    // The score is decreasing; the endpoint score bounds the missed gain.
    double sl=-n, sh=-n;
    if(!fs) for(double d:logs) {
      sl+=(nu+1)*logistic(2*(d-l)-t);
      sh+=(nu+1)*logistic(2*(d-h)-t);
    }
    const double rounding=(n+32)*std::numeric_limits<double>::epsilon()*(n+magnitude);
    error=(fs?0:std::max(std::abs(sl),std::abs(sh))*(h-l)/2)+rounding;
    if(rounding>eps/16) {failed=true; return -inf;}
    if(!std::isfinite(ll)||!std::isfinite(error)) {failed=true; return -inf;}
    // Preserve the initializer against changes at roundoff scale.
    if(ll>best+1e-12*n) {best=ll; bt=t; bu=u;}
    return ll;
  };
  double e=0;
  if(!std::isfinite(best)) evaluate(tie ? std::log1p(0.1) : 0,e);
  else {
    const double previous=best;
    const double checked=evaluate(bt,e);
    if(std::abs(checked-previous)>eps/16) failed=true;
  }
  if(failed) return {nan,nan,nan,0,HtStatus::IterationFailure};
  auto incumbent=[&]() {return std::max(normal_ll,std::max(face,best));};
  double lower=0;
  if(!free_mu) {
    // Uniform small-shape bound, including held-scale zero observations.
    const double target=incumbent()-1;
    lower=std::min(-4.0,2*(target+sumlog+(fs?k*fixed_u:0))/n-std::log(2/std::acos(-1.0))-1);
  } else if(tie) {
    double delta=0.5;
    const double lm=*std::max_element(logs.begin(),logs.end());
    for(int i=0;i<100;++i) {
      const double bound=face+n/2*std::log1p(delta)+p*(2+delta)/2*std::log1p(delta)
        +delta*std::abs(sumlog)+p*delta*std::max(lm,0.0);
      if(bound<=incumbent()+eps/4) break;
      delta*=0.5;
    }
    lower=std::log1p(delta);
    if(!(std::exp(lower)>1)) return {nan,nan,nan,0,HtStatus::IterationFailure};
  }
  // Uniform normal-face tail bound. A lower scale bound can use any
  // subset of c positive distances, rather than just the sample minimum.
  const double aa=64;
  double s0=fixed_u;
  if(!fs) {
    std::vector<double> sorted=logs;
    std::sort(sorted.begin(),sorted.end());
    const std::size_t c=static_cast<std::size_t>(std::ceil(0.75*p));
    if(c==0 || (aa+1)*c<=n) return {nan,nan,nan,0,HtStatus::IterationFailure};
    s0=sorted[sorted.size()-c]+0.5*std::log(((aa+1)*c-n)/(aa*n));
  }
  double mx=-inf;
  for(double d:logs) mx=std::max(mx,4*(d-s0));
  double sum=0; for(double d:logs) sum+=std::exp(4*(d-s0)-mx);
  const double logc=std::log((1+1/aa)/4)+mx+std::log(sum);
  const double upper=std::max(std::log(aa),logc-std::log(eps/4));
  if(!std::isfinite(lower)||!std::isfinite(upper)||lower < -700 || upper>700 || !(upper>lower))
    return {nan,nan,nan,0,HtStatus::IterationFailure};
  std::vector<HtInterval> work;
  const int count=static_cast<int>(std::ceil((upper-lower)/0.125));
  for(int i=0;i<count;++i) work.push_back({lower+(upper-lower)*i/count, lower+(upper-lower)*(i+1)/count});
  int used=0;
  while(!work.empty()) {
    if(++used>budget) return {nan,nan,nan,used,HtStatus::IterationFailure};
    const HtInterval q=work.back(); work.pop_back();
    const double mid=q.lo+(q.hi-q.lo)/2;
    if(mid==q.lo||mid==q.hi) return {nan,nan,nan,used,HtStatus::IterationFailure};
    const double v=evaluate(mid,e);
    if(failed) return {nan,nan,nan,used,HtStatus::IterationFailure};
    const double a=std::exp(q.lo),b=std::exp(q.hi);
    double ul=fixed_u,uh=fixed_u;
    if(!fs && !scale_box(logs,n,a,b,ul,uh)) return {nan,nan,nan,used,HtStatus::IterationFailure};
    double low,high;
    if(a>=32) {
      low=1/(4*b)-1/(8*a*a*a);
      high=1/(4*a)-1/(8*b*b*b)+1/(4*a*a*a*a*a);
    } else {
      low=a/2*std::max(0.0,special::digamma((a+1)/2)-special::digamma(b/2))-0.5;
      high=b/2*(special::digamma((b+1)/2)-special::digamma(a/2))-0.5;
    }
    low*=n; high*=n;
    double score_magnitude=std::max(std::abs(low),std::abs(high));
    for(double d:logs) {
      const double zlo=2*(d-uh)-q.hi, zhi=2*(d-ul)-q.lo;
      const double rl=logistic(zlo), rh=logistic(zhi);
      const double bu=b*score_h(zhi,true), al=a*score_h(zlo,false);
      const double l=(rl-bu)/2, h=(rh-al)/2;
      low+=l; high+=h;
      score_magnitude+=std::max(rl+std::abs(bu),rh+std::abs(al))/2;
    }
    const double score_rounding=(n+32)*std::numeric_limits<double>::epsilon()*(n+score_magnitude);
    // A one-sided score enclosure puts the interval maximum at an endpoint.
    // Check that endpoint directly instead of repeatedly bisecting a
    // monotone stretch, particularly near the normal face.
    if(low>score_rounding || high < -score_rounding) {
      double endpoint_error=0;
      const double endpoint=evaluate(low>score_rounding?q.hi:q.lo,endpoint_error);
      if(failed) return {nan,nan,nan,used,HtStatus::IterationFailure};
      if(endpoint+endpoint_error<=incumbent()+eps/4) continue;
    }
    const double bound=v+e+(std::max(std::max(0.0,-low),std::max(0.0,high))+score_rounding)*(q.hi-q.lo)/2;
    if(!std::isfinite(bound)) return {nan,nan,nan,used,HtStatus::IterationFailure};
    if(bound>incumbent()+eps/4) {work.push_back({q.lo,mid}); work.push_back({mid,q.hi});}
  }
  if(normal_ll>=std::max(face,best)-eps/4)
    return {inf,std::exp(normal_ls),normal_ll,used,HtStatus::NormalLimit};
  if(tie&&face>=best-eps/4) return {1,0,nan,used,HtStatus::ScaleBoundary};
  const double sigma=fs?held_sigma:std::exp(bu);
  if(!std::isfinite(best)||!std::isfinite(sigma)||!(sigma>0)) return {nan,nan,nan,used,HtStatus::IterationFailure};
  HtResult out{std::exp(bt),sigma,best,used,HtStatus::Ok};
  if(free_mu&&!tie&&bt==0) out.restricted|=4u;
  return out;
}
} // namespace ht_detail
