#pragma once
// Recover the selected half-GED's base coordinates without rerunning a shape
// search. For held mean and free scale, u=1/a gives the strictly convex cost
// -n log u + sum(m + side*(x-mean)*u)^nu when nu>=1. For nu<1,
// u^2 times its second derivative is strictly decreasing: the score is
// unimodal. Compare its first root with the finite support endpoint.
inline void sged_face_payload(span x, double mean, double sd, SgedResult& r,int maxit=200) {
  const bool hm=std::isfinite(mean),hs=std::isfinite(sd);
  const double n=static_cast<double>(x.size());
  const auto mm=std::minmax_element(x.begin(),x.end());
  if(r.status==SgedStatus::UniformBoundary) {
    const double width=hs?std::sqrt(12.)*sd:hm?2*std::max(mean-*mm.first,*mm.second-mean):*mm.second-*mm.first;
    const double c=hm?mean:*mm.first/2+*mm.second/2;
    r.stop_detail=limit_detail({c-width/2,c+width/2},r.logLik,StopModel::Unif);return;
  }
  if(r.status!=SgedStatus::RightHalfGedBoundary && r.status!=SgedStatus::LeftHalfGedBoundary)return;
  const double side=r.status==SgedStatus::RightHalfGedBoundary?1:-1,nu=r.nu;
  const double m=std::exp(special::ged_log_gamma_ratio(nu,2));
  const double v=std::exp(special::ged_log_gamma_ratio(nu,3));
  double c=side>0?*mm.first:*mm.second,a=NAN;
  bool endpoint_solution=false;
  if(hs)a=sd/std::sqrt(v-m*m);
  else if(!hm) {
    const double range=*mm.second-*mm.first;
    double moment=0;for(double z:x)moment+=std::pow(std::abs(z-c)/range,nu)/n;
    a=range*std::exp((std::log(nu)+std::log(moment))/nu);
  } else {
    double cap=INFINITY;
    for(double z:x)if(side*(z-mean)<0)cap=std::min(cap,m/std::abs(z-mean));
    const auto score=[&](double u) {
      double s=-n/u;
      for(double z:x) {
        const double d=side*(z-mean),w=std::max(0.,m+d*u);
        s+=nu*d*std::pow(w,nu-1);
      }
      return s;
    };
    const auto root=[&](double hi) {
      double lo=0;
      for(int j=0;j<solver_budget(maxit);++j){const double mid=lo/2+hi/2;if(score(mid)>0)hi=mid;else lo=mid;}
      return lo/2+hi/2;
    };
    double u=cap;
    if(nu<1 && std::isfinite(cap)) {
      // u^2 F'' = n - nu*(1-nu)*sum((d*u)^2*(m+d*u)^(nu-2)).
      // Each summand is strictly increasing, even for negative d, because
      // its derivative has the sign of 2*m+nu*d*u > 0 on the support.
      double lo=0,hi=cap;
      for(int j=0;j<solver_budget(maxit);++j) {
        const double mid=lo/2+hi/2;double curvature=n;
        for(double z:x){const double d=side*(z-mean),du=d*mid,w=std::max(0.,m+du);curvature-=nu*(1-nu)*du*du*std::pow(w,nu-2);}
        if(std::isnan(curvature))return;
        if(curvature>0)lo=mid;else hi=mid;
      }
      const double peak=lo/2+hi/2,maximum=score(peak);
      double magnitude=n/peak;
      for(double z:x){const double d=side*(z-mean),w=std::max(0.,m+d*peak);magnitude+=std::abs(nu*d*std::pow(w,nu-1));}
      const double margin=128*std::numeric_limits<double>::epsilon()*magnitude;
      if(!std::isfinite(maximum) || !std::isfinite(margin) || std::abs(maximum)<=margin)return;
      if(maximum>0) {
        const double interior=root(peak);
        const auto candidate=[&](double inverse_scale) {
          const double base_scale=1/inverse_scale;
          double endpoint=inverse_scale==cap?(side>0?*mm.first:*mm.second):mean-side*m*base_scale;
          if(!std::isfinite(endpoint))return FitCandidate{};
          endpoint=side>0?std::min(endpoint,*mm.first):std::max(endpoint,*mm.second);
          double value=n*(std::log(nu)-special::lgamma(1/nu)+std::log(inverse_scale));
          for(double z:x)value-=std::pow(std::max(0.,side*(z-endpoint)/base_scale),nu);
          return FitCandidate{{endpoint,base_scale,nu},value,value,side>0?StopModel::HalfGed:StopModel::ReflectedHalfGed};
        };
        const auto first=candidate(interior),last=candidate(cap);
        if(!std::isfinite(first.loglik_lower)||!std::isfinite(last.loglik_lower))return;
        const auto comparison=std::abs(first.loglik_lower-last.loglik_lower)<=1e-7*n?FaceComparison::NumericalTie:compare_face(first.loglik_lower,last.loglik_lower,0);
        if(comparison==FaceComparison::NumericalTie) {
          if(std::abs(std::max(first.loglik_lower,last.loglik_lower)-r.logLik)>1e-7*n)return;
          r.stop_detail=comparison_detail(first,last);return;
        }
        if(comparison==FaceComparison::FirstWins)u=interior;
      }
    } else {
      // With no finite endpoint the cost tends to infinity. For nu<1 the
      // unimodal score approaches zero from above, so its first root is unique.
      const double unit=std::max(std::abs(*mm.first-mean),std::abs(*mm.second-mean));
      double hi=std::isfinite(cap)?cap:1/unit;
      for(int j=0;j<solver_budget(maxit) && score(hi)<0 && !std::isfinite(cap);++j)hi*=2;
      if(!(hi>0 && std::isfinite(hi)) || std::isnan(score(hi)))return;
      u=hi;
      if(score(hi)>0)u=root(hi);
      else if(!std::isfinite(cap))return;
    }
    endpoint_solution=std::isfinite(cap)&&u==cap;
    a=1/u;
  }
  if(hm)c=endpoint_solution?(side>0?*mm.first:*mm.second):mean-side*m*a;
  if(!(a>0 && std::isfinite(a) && std::isfinite(c)))return;
  // Round a closed support endpoint outward; the mean identity can differ
  // only by the same practical floating-point margin checked below.
  const double boundary=side>0?*mm.first:*mm.second;
  const double violation=side*(c-boundary);
  if(violation>1e-10*(1+std::abs(boundary)))return;
  if(violation>0)c=boundary;
  double ll=n*(std::log(nu)-special::lgamma(1/nu)-std::log(a));
  for(double z:x) {
    double w=side*(z-c)/a;
    if(w < -1e-10*(1+std::abs(z/a)))return;
    if(hm&&hs&&std::abs(z-c)<1e-14*(1+std::abs(c)))w=m*v/(2*(v-m*m));
    ll-=std::pow(std::max(0.,w),nu);
  }
  // The existing search's face value must agree; otherwise keep its ordinary
  // unresolved error rather than manufacture a fitted face.
  if(!std::isfinite(ll) || std::abs(ll-r.logLik)>1e-7*n)return;
  r.stop_detail=limit_detail({c,a,nu},r.logLik,side>0?StopModel::HalfGed:StopModel::ReflectedHalfGed);
  // Retain the original comparison value while enclosing the independently
  // evaluated likelihood at the recovered coordinates as well.
  r.stop_detail.candidates[0].loglik_lower=std::min(ll,r.logLik);
  r.stop_detail.candidates[0].loglik_upper=std::max(ll,r.logLik);
}
