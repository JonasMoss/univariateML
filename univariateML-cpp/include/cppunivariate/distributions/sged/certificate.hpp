#pragma once
// Held-shape certificates. Included inside cppunivariate after SgedResult.
// Proof, arithmetic margin and scope are in README.md.
namespace sged_cert_detail {
struct Sum {
  double value=0,correction=0;
  void add(double v) {const double y=v-correction,t=value+y;correction=(t-value)-y;value=t;}
};
inline SgedResult scale_fit(span x,int maxit,double mean,double nu,double xi);
inline SgedResult fit(span x, double /*tol*/, int maxit, double mean,
    double sd, double nu, double xi) {
  SgedResult out{mean,sd,nu,xi,NAN,0,SgedStatus::CertificateFailure};
  const bool hm=std::isfinite(mean), hs=std::isfinite(sd);
  if(hm && !hs && xi!=1) return scale_fit(x,maxit,mean,nu,xi);
  const double n=static_cast<double>(x.size());
  const double eps=std::numeric_limits<double>::epsilon();
  double shift=0;
  const double su=distributions::fs::sged_sigma(nu,xi,shift), lambda=dens::ged_lambda(nu);
  const double normal=dens::ged_g(nu,lambda);
  if(!(su>0 && lambda>0 && normal>0) || !std::isfinite(su+lambda+normal+shift))return out;
  // The fixed shapes and sd translate the mode but do not change its minimizer.
  double lo=x[0],hi=x[0];
  for(double v:x){lo=std::min(lo,v);hi=std::max(hi,v);}
  const double origin=hm?mean:lo/2+hi/2;
  double unit=std::max(std::abs(lo-origin),std::abs(hi-origin));
  if(!std::isfinite(unit))return out;
  if(unit==0) {
    if(!hs){out.status=SgedStatus::Unbounded;return out;}
    unit=sd;
  }
  std::vector<double> raw(x.begin(),x.end());std::sort(raw.begin(),raw.end());
  std::vector<double> y;
  for(double v:raw){const double z=(v-origin)/unit;if(!std::isfinite(z))return out;y.push_back(z);}
  for(std::size_t i=1;i<y.size();++i)if(y[i]==y[i-1] && raw[i]!=raw[i-1])return out;
  const double logxi=std::log(xi);
  const double lognormal=std::log(su)+std::log(2/(xi+1/xi))+distributions::fs::ged_logdensity(0,nu);
  if(!std::isfinite(lognormal))return out;
  auto result_at=[&](double c) {
    SgedResult r=out;
    double mode=hm?mean:origin+unit*c;
    if(!hm) {
      const auto knot=std::lower_bound(y.begin(),y.end(),c);
      if(knot!=y.end() && *knot==c)mode=raw[static_cast<std::size_t>(knot-y.begin())];
    }
    r.mu=mode;
    if(!hs) {
      // Profile ordinary scale: sigma^nu=nu su^nu Q/(2 n lambda^nu).
      bool distinct=false;for(double v:x)if(v!=mode)distinct=true;
      if(!distinct){r.status=SgedStatus::Unbounded;return r;}
      double largest=-INFINITY;
      for(double v:x)if(v!=mode)largest=std::max(largest,
          nu*(std::log(std::abs(v-mode))-(v>mode?logxi:-logxi)));
      if(!std::isfinite(largest))return r;
      double moment=0;
      for(double v:x)if(v!=mode)moment+=std::exp(
          nu*(std::log(std::abs(v-mode))-(v>mode?logxi:-logxi))-largest)/n;
      r.sigma=std::exp(std::log(su)-std::log(lambda)+(std::log(nu/2)+std::log(moment)+largest)/nu);
    }
    if(!hm) {
      r.mu=mode+r.sigma*shift/su;
      const double roundoff=32*eps*(std::abs(origin)+std::abs(unit*c)+std::abs(r.sigma*shift/su));
      if(roundoff>1e-7*(1+std::abs(r.mu)))return r;
      const double recovered=(r.mu-origin-r.sigma*shift/su)/unit;
      if(!std::isfinite(recovered) || std::abs(recovered-c)>1e-9)return r;
    }
    if(!std::isfinite(r.mu) || !(r.sigma>0) || !std::isfinite(r.sigma))return r;
    double penalty=0,ideal_penalty=0;
    for(double v:x) {
      const double z=(v-r.mu)/r.sigma*su+shift;
      const double term=z==0?0:std::exp(nu*(std::log(std::abs(z))-std::log(lambda)-(z>0?logxi:-logxi))-std::log(2.));
      penalty+=term;
      const double ideal=(v-mode)/r.sigma*su+(hm?shift:0);
      ideal_penalty+=ideal==0?0:std::exp(nu*(std::log(std::abs(ideal))-std::log(lambda)-(ideal>0?logxi:-logxi))-std::log(2.));
    }
    r.logLik=n*(lognormal-std::log(r.sigma))-penalty;
    if(std::isfinite(r.logLik) && std::abs(penalty-ideal_penalty)<=(nu<1?1e-7:1e-10)*(n+std::abs(r.logLik)))r.status=SgedStatus::Ok;
    return r;
  };
  auto witness=[&](const SgedResult& r) {
    const double pad=(nu<1?1e-7:1e-10)*(n+std::abs(r.logLik));
    return FitCandidate{{r.mu,r.sigma,nu,xi},r.logLik-pad,r.logLik+pad};
  };
  auto stopped=[&](SgedResult r,bool budget,bool tie,const std::vector<FitCandidate>& candidates) {
    r.status=SgedStatus::CertificateFailure;r.certified=false;
    r.stop_detail.budget_exhausted=budget;r.stop_detail.comparison_tie=tie;
    r.stop_detail.candidates=candidates;r.stop_detail.loglik_upper=INFINITY;
    for(const auto& v:candidates)r.stop_detail.loglik_lower=std::isnan(r.stop_detail.loglik_lower)?v.loglik_lower:std::max(r.stop_detail.loglik_lower,v.loglik_lower);
    return r;
  };
  if(hm) {
    auto r=result_at(0);
    if(r.status==SgedStatus::Ok)r.certified=true;
    return r;
  }
  double a=y.front(),b=y.back(),c=a;
  if(a==b) c=a;
  else if(nu==1) {
    // Weighted median: left slope xi, right slope -1/xi.
    const double wleft=std::exp(logxi-std::abs(logxi)),wright=std::exp(-logxi-std::abs(logxi));
    if(!(wleft>0 && wright>0))return out;
    for(std::size_t i=0;i<y.size();) {
      std::size_t j=i+1;while(j<y.size() && y[j]==y[i])++j;
      const double slope=j*wleft-(y.size()-j)*wright;
      const double pad=512*eps*(j*wleft+(y.size()-j)*wright);
      if(slope>pad){c=y[i];break;}
      if(std::abs(slope)<=pad && j<y.size()) {
        auto r=result_at(y[i]),s=result_at(y[j]);
        if(r.status!=SgedStatus::Ok || s.status!=SgedStatus::Ok)return out;
        return stopped(r,false,true,{witness(r),witness(s)});
      }
      i=j;
    }
  } else if(nu<1) {
    // Strict concavity between distinct knots excludes all intervening points.
    std::vector<FitCandidate> candidates;
    SgedResult best=out;best.logLik=-INFINITY;
    const long long budget=128LL*std::min(maxit,100000);
    long long work=0;
    for(std::size_t i=0;i<y.size();) {
      if(work>=budget)return stopped(best,true,false,candidates);
      auto r=result_at(y[i]);++work;
      if(r.status!=SgedStatus::Ok)return out;
      const auto v=witness(r);
      if(r.logLik>best.logLik){best=r;}
      candidates.push_back(v);
      const double lower=witness(best).loglik_lower;
      candidates.erase(std::remove_if(candidates.begin(),candidates.end(),[&](const FitCandidate& z){return z.loglik_upper<lower;}),candidates.end());
      const double knot=y[i];while(i<y.size() && y[i]==knot)++i;
    }
    best.iterations=static_cast<int>(work);
    if(candidates.size()!=1)return stopped(best,false,true,candidates);
    best.certified=true;return best;
  } else {
    // A positive sum of strictly convex powers has exactly one minimum.
    // Log-rescaling the signed score avoids powers overflowing/underflowing.
    auto sign=[&](double t) {
      double largest=-INFINITY;
      for(double v:y)if(v!=t)largest=std::max(largest,(nu-1)*std::log(std::abs(t-v))+(t>v?nu*logxi:-nu*logxi));
      Sum left,right;
      for(double v:y)if(v!=t) {
        const double value=std::exp((nu-1)*std::log(std::abs(t-v))+(t>v?nu*logxi:-nu*logxi)-largest);
        if(t>v)left.add(value);else right.add(value);
      }
      const double pad=512*eps*(1+nu+nu*std::abs(logxi))*(left.value+right.value)*(1+n*eps);
      return left.value-right.value>pad?1:(right.value-left.value>pad?-1:0);
    };
    // Bound the public coefficients too: a small normalized mode interval
    // alone is insufficient when the recovered mean nearly cancels.
    auto coordinates_closed=[&](double left,double right) {
      const auto r=result_at(left+(right-left)/2);
      if(r.status!=SgedStatus::Ok)return false;
      double slo=sd,shi=sd;
      if(!hs) {
        auto power=[&](double v,double t) {
          return v==t?-INFINITY:nu*(std::log(std::abs(v-t))+(v<t?logxi:-logxi));
        };
        std::vector<double> lower,upper;
        double lm=-INFINITY,um=-INFINITY;
        for(double v:y) {
          const double l=power(v,left),h=power(v,right);
          lower.push_back(v>=left && v<=right?-INFINITY:std::min(l,h));
          upper.push_back(std::max(l,h));
          lm=std::max(lm,lower.back());um=std::max(um,upper.back());
        }
        if(!std::isfinite(lm) || !std::isfinite(um))return false;
        double ls=0,us=0;
        for(double v:lower)ls+=std::exp(v-lm)/n;
        for(double v:upper)us+=std::exp(v-um)/n;
        const double base=std::log(unit)+std::log(su)-std::log(lambda);
        slo=std::exp(base+(std::log(nu/2)+lm+std::log(ls))/nu);
        shi=std::exp(base+(std::log(nu/2)+um+std::log(us))/nu);
        if(!(slo>0) || !std::isfinite(shi) || shi-slo>1e-7*(1+std::abs(r.sigma)))return false;
      }
      const double mlo=origin+unit*left+std::min(slo*shift/su,shi*shift/su);
      const double mhi=origin+unit*right+std::max(slo*shift/su,shi*shift/su);
      return std::isfinite(mlo) && std::isfinite(mhi) && mhi-mlo<=1e-7*(1+std::abs(r.mu));
    };
    const double width=1e-9;
    bool closed=false;
    for(int k=0;k<std::min(maxit,100000);++k) {
      out.iterations=k+1;
      const double mid=a+(b-a)/2;
      const int s=sign(mid);
      if(s>0)b=mid;
      else if(s<0)a=mid;
      else {
        const double l=std::max(a,mid-width/4),r=std::min(b,mid+width/4);
        if(sign(l)<0 && sign(r)>0){a=l;b=r;}
        else return out;
      }
      if(b-a<=width && coordinates_closed(a,b)){closed=true;break;}
    }
    c=a+(b-a)/2;
    auto r=result_at(c);
    if(r.status!=SgedStatus::Ok)return out;
    if(!closed)return stopped(r,false,false,{witness(r)});
    // Certify the stored location too; cancellation must not lose the enclosure.
    const double recovered=(r.mu-origin-r.sigma*shift/su)/unit;
    if(!std::isfinite(recovered) || std::abs(recovered-c)>width)return out;
  }
  auto r=result_at(c);
  if(r.status==SgedStatus::Ok)r.certified=true;
  return r;
}
// With held mean, nu>=1 gives a strictly convex objective in inverse sd.
inline SgedResult scale_fit(span x,int maxit,double mean,double nu,double xi) {
  SgedResult out{mean,NAN,nu,xi,NAN,0,SgedStatus::CertificateFailure};
  if(nu<1)return out;
  const int cap=std::min(maxit,100000);
  double unit=0,shift=0;
  for(double v:x)unit=std::max(unit,std::abs(v-mean));
  if(!std::isfinite(unit))return out;
  if(unit==0){out.status=SgedStatus::Unbounded;return out;}
  const double su=distributions::fs::sged_sigma(nu,xi,shift),lambda=dens::ged_lambda(nu);
  if(!(su>0 && lambda>0) || !std::isfinite(su+lambda+shift))return out;
  const double logxi=std::log(xi),n=static_cast<double>(x.size());
  std::vector<double> y;for(double v:x)y.push_back((v-mean)/unit);
  // Sign of t F'(t), t=unit/sd; the -n/t term is retained exactly
  // in log-rescaled signed sums. Only F'(t), not F(log t), is monotone.
  auto sign=[&](double lt) {
    const double t=std::exp(lt);
    if(!(t>0) || !std::isfinite(t))return 0;
    struct Term { double magnitude;bool positive; };
    std::vector<Term> terms;
    double largest=std::log(n);
    for(double d:y)if(d!=0) {
      const double z=d*su*t+shift;
      if(!std::isfinite(z) || (z==0 && nu==1))return 0;
      if(z==0)continue;
      const double v=std::log(nu/2)+std::log(std::abs(d))+std::log(su)+lt+
          (nu-1)*std::log(std::abs(z))-nu*(std::log(lambda)+(z>0?logxi:-logxi));
      if(std::isnan(v) || v==INFINITY)return 0;
      terms.push_back({v,(d>0)==(z>0)});largest=std::max(largest,v);
    }
    Sum positive,negative;negative.add(std::exp(std::log(n)-largest));
    for(const auto& v:terms) {
      const double term=std::exp(v.magnitude-largest);
      if(v.positive)positive.add(term);else negative.add(term);
    }
    const double pad=512*std::numeric_limits<double>::epsilon()*
        (1+nu*(1+std::abs(logxi)+std::abs(lt)))*(positive.value+negative.value)*
        (1+n*std::numeric_limits<double>::epsilon());
    return positive.value-negative.value>pad?1:(negative.value-positive.value>pad?-1:0);
  };
  int work=1;
  double a=0,b=0;
  int sa=sign(a),sb=sa;
  if(sa==0)return out;
  while(!(sa<0 && sb>0)) {
    if(work>=cap)return out;
    if(sa>0){a-=1;sa=sign(a);if(sa==0)return out;}
    else {b+=1;sb=sign(b);if(sb==0)return out;}
    ++work;
  }
  const double width=1e-9;
  while(b-a>width) {
    if(work>=cap)return out;
    ++work;
    const double mid=a+(b-a)/2;
    const int s=sign(mid);
    if(s>0)b=mid;
    else if(s<0)a=mid;
    else {
      const double l=std::max(a,mid-width/4),r=std::min(b,mid+width/4);
      if(sign(l)<0 && sign(r)>0){a=l;b=r;}else return out;
    }
  }
  const double ls=std::log(unit)-(a+(b-a)/2),sd=std::exp(ls);
  if(!(sd>0) || !std::isfinite(sd) || std::abs(std::log(sd)-ls)>1e-8)return out;
  // Evaluate only after enclosing the unique root; recursive call has sd held.
  auto r=fit(x,1e-8,maxit,mean,sd,nu,xi);
  r.iterations=work;
  return r;
}
} // namespace sged_cert_detail
