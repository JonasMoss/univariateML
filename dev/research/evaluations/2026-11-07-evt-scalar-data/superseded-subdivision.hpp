#pragma once
// Included after the public result and likelihood/scale definitions in mle.hpp.
namespace cppunivariate {
namespace frechet_certificate_detail {
struct Point { double u, ll, score; double error = 0; };
inline Point point(span g,double a,double u) {
  const double e=std::exp(u),n=static_cast<double>(g.size());
  double b=0,c=0,A=0,logs=0;
  for(double v:g) {
    const double l=std::log(v+e),r=e/(v+e),p=std::pow(r,a);
    b+=p;A+=p*r;c+=r;logs+=l;
  }
  return {u,n*std::log(a)-n-n*std::log(b/n)+n*a*u-(a+1)*logs,
          n*a*A/b-(a+1)*c};
}
// Range of r^p(1-r), including its single turning point.
inline std::pair<double,double> power_range(double l,double h,double p) {
  auto f=[&](double r){return std::pow(r,p)*(1-r);};
  const double fl=f(l),fh=f(h),mode=p/(p+1);
  return {std::min(fl,fh),std::max(std::max(fl,fh),l<=mode && mode<=h ? f(mode) : 0.)};
}
inline std::pair<double,double> curvature(span g,double a,double l,double h) {
  const double el=std::exp(l),eh=std::exp(h),n=static_cast<double>(g.size());
  double bl=0,bh=0,al=0,ah=0,pl=0,ph=0,ql=0,qh=0,cl=0,ch=0;
  for(double v:g) {
    const double rl=el/(v+el),rh=eh/(v+eh);
    bl+=std::pow(rl,a);bh+=std::pow(rh,a);
    al+=std::pow(rl,a+1);ah+=std::pow(rh,a+1);
    const auto p=power_range(rl,rh,a+1),q=power_range(rl,rh,a),c=power_range(rl,rh,1);
    pl+=p.first;ph+=p.second;ql+=q.first;qh+=q.second;cl+=c.first;ch+=c.second;
  }
  return {n*a*((a+1)*pl/bh-a*ah*qh/(bl*bl))-(a+1)*ch,
          n*a*((a+1)*ph/bl-a*al*ql/(bh*bh))-(a+1)*cl};
}
}
inline FrechetResult frechet_fixed_shape_certificate(span g,double a,bool held_scale,
                                                       double tol,int maxit) {
  FrechetResult out{a,NAN,NAN,NAN,0,FrechetStatus::NumericalFailure};
  const double n=static_cast<double>(g.size());
  const double margin=256*std::numeric_limits<double>::epsilon()*n*(1+a)*(1+a);
  auto finish=[&](double u) {
    const double e=std::exp(u);
    out.mu=-e;out.sigma=held_scale ? 1 : frechet_scale(g,a,-e);
    out.logLik=frechet_loglik(g,a,out.mu,out.sigma);
    if(!(e>0) || !(out.sigma>0) || !std::isfinite(out.sigma) || !std::isfinite(out.logLik)) out.status=FrechetStatus::NumericalFailure;
    return out;
  };
  auto stopped=[&](bool budget) {
    out.status=budget ? FrechetStatus::Budget : FrechetStatus::NumericalFailure;
    out.stop_detail.budget_exhausted=budget;
    out.stop_detail.loglik_upper=INFINITY;
    if(std::isfinite(out.logLik)) {
      out.stop_detail.loglik_lower=out.logLik-margin;
      out.stop_detail.candidates.push_back({{a,out.mu,out.sigma},out.logLik-margin,out.logLik+margin});
    }
    return out;
  };
  if(held_scale) {
    // log R is strictly decreasing in gap; normalization uses sigma=1.
    auto score=[&](double u) {
      double A=0,B=0;const double e=std::exp(u);
      for(double v:g){const double r=e/(v+e);A+=std::pow(r,a+1);B+=r;}
      return -a*u+std::log(A/B)-std::log1p(1/a);
    };
    double l=-1,h=1;
    int j=0;
    for(;j<maxit && !(score(l)>0 && score(h)<0);++j) {
      if(!(score(l)>0))l*=2;
      if(!(score(h)<0))h*=2;
      if(!std::isfinite(score(l)) || !std::isfinite(score(h)))return stopped(false);
    }
    if(j==maxit)return stopped(true);
    for(j=0;j<maxit;++j) {
      const double m=l+(h-l)/2,s=score(m);
      if(!std::isfinite(s))return stopped(false);
      if(s>0)l=m;else h=m;
      if(h-l<=tol*(1+std::abs(m))) {out.status=FrechetStatus::Ok;out.iterations=j+1;return finish((l+h)/2);}
    }
    return stopped(true);
  }
  std::size_t k=0;double sumlogs=0,R=0;
  for(double v:g){if(v==0)++k;else sumlogs+=std::log(v);R=std::max(R,v);}
  const double exponent=std::fma(a,static_cast<double>(g.size()-k),-static_cast<double>(k));
  if(!(exponent>0)){out.status=FrechetStatus::NoMLE;return out;}
  const double C=n*std::log(n*a)-n-n*std::log(static_cast<double>(k))-(a+1)*sumlogs;
  auto best=frechet_certificate_detail::point(g,a,0);
  if(!std::isfinite(best.ll))return stopped(false);
  out.status=FrechetStatus::Ok;finish(0);
  double l=-1,h=1;
  int j=0;
  auto upper=[&](double u){return n*std::log(a)-n-n*u+n*a*std::log1p(R*std::exp(-u));};
  for(;j<maxit && !(C+exponent*l<best.ll-margin && upper(h)<best.ll-margin);++j) {
    if(!(C+exponent*l<best.ll-margin))l*=2;
    if(!(upper(h)<best.ll-margin))h*=2;
  }
  if(j==maxit)return stopped(true);
  if(!(std::exp(l)>0) || !std::isfinite(std::exp(h)))return stopped(false);
  struct Segment {double l,h;};std::vector<Segment> pending{{l,h}};
  std::vector<frechet_certificate_detail::Point> roots;
  std::int64_t work=0;
  while(!pending.empty()) {
    if(++work>certificate_budget(maxit))return stopped(true);
    const auto seg=pending.back();pending.pop_back();
    const double mid=seg.l+(seg.h-seg.l)/2,half=(seg.h-seg.l)/2;
    const auto p=frechet_certificate_detail::point(g,a,mid);
    if(!std::isfinite(p.ll) || !std::isfinite(p.score))return stopped(false);
    if(p.ll>best.ll){best=p;finish(p.u);}
    // Uniform |ell''(u)| bound; permits pruning without a root assumption.
    const double M=n*a*(2*a+1)+(a+1)*n/4;
    if(p.ll+std::abs(p.score)*half+M*half*half/2<best.ll-margin)continue;
    if(std::abs(p.score)>M*half+margin)continue;
    const auto cv=frechet_certificate_detail::curvature(g,a,seg.l,seg.h);
    if(cv.first>margin)continue; // strictly convex: no interior maximum
    if(cv.second < -margin) {
      const auto left=frechet_certificate_detail::point(g,a,seg.l),right=frechet_certificate_detail::point(g,a,seg.h);
      if(left.score < -margin || right.score > margin)continue;
      if(!(left.score>=0 && right.score<=0))return stopped(false);
      double lo=seg.l,hi=seg.h;
      for(j=0;j<maxit;++j) {
        const double u=lo+(hi-lo)/2;
        if(frechet_certificate_detail::point(g,a,u).score>0)lo=u;else hi=u;
        if(hi-lo<=tol*(1+std::abs(u)))break;
      }
      if(j==maxit)return stopped(true);
      auto root=frechet_certificate_detail::point(g,a,(lo+hi)/2);
      root.error=std::abs(root.score)*(hi-lo)/2+M*(hi-lo)*(hi-lo)/8;
      bool duplicate=false;
      for(const auto& r:roots)if(std::abs(r.u-root.u)<=4*tol*(1+std::abs(root.u)))duplicate=true;
      if(!duplicate)roots.push_back(root);
      if(root.ll>=best.ll-margin){best=root;finish(root.u);}
      continue;
    }
    if(mid==seg.l || mid==seg.h)return stopped(false);
    pending.push_back({seg.l,mid});pending.push_back({mid,seg.h});
  }
  if(roots.empty())return stopped(false);
  best=*std::max_element(roots.begin(),roots.end(),[](const auto& p,const auto& q){return p.ll<q.ll;});
  finish(best.u);
  for(const auto& r:roots)if(std::abs(r.u-best.u)>4*tol*(1+std::abs(best.u)) && r.ll+r.error>=best.ll-margin) {
    out.status=FrechetStatus::Tie;out.stop_detail.comparison_tie=true;
    out.stop_detail.loglik_lower=best.ll-margin;out.stop_detail.loglik_upper=best.ll+best.error+margin;
    for(const auto& q:roots)out.stop_detail.loglik_upper=std::max(out.stop_detail.loglik_upper,q.ll+q.error+margin);
    for(const auto& q:roots)if(q.ll+q.error>=best.ll-margin) {
      const double e=std::exp(q.u),s=frechet_scale(g,a,-e);
      out.stop_detail.candidates.push_back({{a,-e,s},q.ll-margin,q.ll+margin});
    }
    return out;
  }
  out.status=FrechetStatus::Ok;out.iterations=static_cast<int>(work);return out;
}
} // namespace cppunivariate
