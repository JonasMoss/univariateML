#pragma once
// Included inside cppunivariate, after FatigueResult. Rational-score inclusion
// avoids expanding an n+2 degree polynomial in the monomial basis.
namespace fatigue_certificate {
struct Interval { double lo,hi; Interval(double x):lo(x),hi(x){} Interval(double l,double h):lo(l),hi(h){} };
inline Interval pad(double l,double h) {
  const double e=16*std::numeric_limits<double>::epsilon()*std::max(std::abs(l),std::abs(h));
  return {l-e,h+e};
}
inline Interval operator+(Interval a,Interval b){return pad(a.lo+b.lo,a.hi+b.hi);}
inline Interval operator-(Interval a,Interval b){return pad(a.lo-b.hi,a.hi-b.lo);}
inline Interval operator*(Interval a,Interval b){return pad(std::min({a.lo*b.lo,a.lo*b.hi,a.hi*b.lo,a.hi*b.hi}),std::max({a.lo*b.lo,a.lo*b.hi,a.hi*b.lo,a.hi*b.hi}));}
inline Interval operator/(Interval a,Interval b){if(b.lo<=0&&b.hi>=0)return {-INFINITY,INFINITY};return a*Interval(1/b.hi,1/b.lo);}
inline FatigueResult fit(span x,double tol,int maxit,double alpha,double beta) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  FatigueResult failure{nan,nan,nan,0,FatigueStatus::SearchFailure};
  if(x.empty()||!(tol>0)||!std::isfinite(tol)||maxit<1) {failure.status=FatigueStatus::InvalidInput;return failure;}
  tol=std::min(tol,1e-10);
  const bool fa=std::isfinite(alpha),fb=std::isfinite(beta);
  double unit=0;
  for(double v:x){if(!(v>0)||!std::isfinite(v)){failure.status=FatigueStatus::InvalidInput;return failure;}unit=std::max(unit,v);}
  std::vector<double> y;double mn=1;CPPUNIV_EXTENDED_REAL total=0,total_inv=0;
  for(double v:x){const double t=v/unit;if(!(t>0)||!std::isfinite(1/t))return failure;y.push_back(t);mn=std::min(mn,t);total+=t;total_inv+=CPPUNIV_EXTENDED_LITERAL(1.0,1.0L)/t;}
  const double n=static_cast<double>(x.size()),A=static_cast<double>(total/n),inv=static_cast<double>(total_inv/n),H=1/inv;
  const auto value=[&](double b){
    const double physical_beta=fb?beta:b*unit;
    double ss=0;
    if(!fa)for(double v:x){const double z=(v-physical_beta)/std::sqrt(v)/std::sqrt(physical_beta);ss+=z*z/n;}
    const double a=fa?alpha:std::sqrt(ss);
    double ll=0;
    for(double v:y)ll+=std::log(v+b)-std::log(2*a)-.5*std::log(b)-1.5*std::log(v)-.5*std::log(2*std::acos(-1.0));
    for(double v:x){const double z=(v-physical_beta)/std::sqrt(v)/std::sqrt(physical_beta)/a;ll-=(.5*z)*z;}
    FatigueResult result{a,physical_beta,ll-n*std::log(unit),0,FatigueStatus::Ok};
    if(!(result.alpha>0)||!(result.beta>0)||!std::isfinite(result.alpha)||!std::isfinite(result.beta)||!std::isfinite(result.logLik))result.status=FatigueStatus::SearchFailure;
    if(!fb&&std::abs((physical_beta/unit-b)/b)>2e-13)result.status=FatigueStatus::SearchFailure;
    return result;
  };
  if(fb){auto r=value(beta/unit);r.beta=beta;if(!fa&&r.alpha==0)r.status=FatigueStatus::Unbounded;else if(!std::isfinite(r.logLik))return failure;return r;}
  if(!fa&&mn==1){failure.status=FatigueStatus::Unbounded;return failure;}
  double lo=H,hi=A;
  if(fa){double r=1;if(alpha>2){r=std::pow(alpha/2+std::sqrt((alpha/2-1)*(alpha/2+1)),2);}lo=mn/r;hi=r;}
  if(!(lo>0)||!std::isfinite(hi))return failure;
  // A/H can round together before the induced alpha accuracy is resolved.
  // Do not turn a tiny beta bracket into an unchecked alpha estimate.
  if(!fa&&std::abs(hi-lo)<=128*std::numeric_limits<double>::epsilon()*hi) {
    const auto witness=value(A);
    if(witness.status==FatigueStatus::Ok)failure.stop_detail.candidates={{{witness.alpha,witness.beta},witness.logLik,witness.logLik,StopModel::None}};
    return failure;
  }
  if(lo==hi)return value(lo);
  if(fa&&mn==1) {
    const auto first=value(lo),second=value(hi);
    if(first.status!=FatigueStatus::Ok||second.status!=FatigueStatus::Ok)return failure;
    failure.stop_detail=comparison_detail({{first.alpha,first.beta},first.logLik,first.logLik,StopModel::None},{{second.alpha,second.beta},second.logLik,second.logLik,StopModel::None});
    return failure;
  }
  const auto witness=value(std::sqrt(lo)*std::sqrt(hi));
  if(witness.status==FatigueStatus::Ok) {
    failure.stop_detail.candidates={{{witness.alpha,witness.beta},witness.logLik,witness.logLik,StopModel::None}};
    failure.stop_detail.loglik_lower=witness.logLik;failure.stop_detail.loglik_upper=INFINITY;
  }
  const auto bounds=[&](double l,double h,bool derivative){
    Interval b(l,h),s1(0),s2(0);
    for(double v:y){Interval t=Interval(1)/(Interval(v)+b);s1=s1+t;s2=s2+t*t;}
    if(fa){const double aa=alpha*alpha;
      if(derivative)return Interval(4*aa)*b*s1-Interval(2*aa)*b*b*s2-Interval(n*aa)-Interval(2*n*inv)*b;
      return Interval(2*aa)*b*b*s1-Interval(n*aa)*b+Interval(n*A)-Interval(n*inv)*b*b;
    }
    Interval xi=Interval(n)/s1;
    if(derivative)return Interval(2)*b-Interval(2*H)-xi+(Interval(H)-b)*Interval(n)*s2/(s1*s1);
    return b*b-Interval(2*H)*b+Interval(H*A)+(Interval(H)-b)*xi;
  };
  const auto score=[&](double b){double s=0;for(double v:y)s+=1/(v+b);return fa?2*alpha*alpha*b*b*s-n*alpha*alpha*b+n*A-n*inv*b*b:b*b-2*H*b+H*A+(H-b)*n/s;};
  std::vector<Interval> pending{{lo,hi}};
  std::vector<FatigueResult> candidates;
  std::int64_t used=0;
  while(!pending.empty()) {
    const auto q=pending.back();pending.pop_back();
    if(++used>certificate_budget(maxit)){failure.stop_detail.budget_exhausted=true;break;}
    const auto s=bounds(q.lo,q.hi,false);
    if(s.lo>0||s.hi<0)continue;
    const auto d=bounds(q.lo,q.hi,true);
    if(d.lo>0||d.hi<0){
      double l=q.lo,h=q.hi,sl=score(l),sh=score(h);
      if(sl*sh>0)continue;
      if(d.lo>0)continue; // A minimum cannot be a global maximum.
      bool converged=false;
      for(std::int64_t it=0;it<solver_budget(maxit);++it){double m=std::sqrt(l)*std::sqrt(h);if(score(m)>0)l=m;else h=m;if(h-l<=.01*tol*h) {
        const auto left=value(l),right=value(h);
        if(fa || std::abs(left.alpha-right.alpha)<=.01*tol*std::min(left.alpha,right.alpha)) {converged=true;break;}
      }}
      if(!converged)return failure;
      const auto root=value(std::sqrt(l)*std::sqrt(h));
      if(root.status!=FatigueStatus::Ok)return failure;
      candidates.push_back(root);continue;
    }
    const double m=std::sqrt(q.lo)*std::sqrt(q.hi);
    if(!(m>q.lo&&m<q.hi))return failure;
    pending.push_back({q.lo,m});pending.push_back({m,q.hi});
  }
  if(candidates.empty()) {
    if(failure.stop_detail.budget_exhausted){const auto r=value(std::sqrt(lo)*std::sqrt(hi));if(std::isfinite(r.logLik))failure.stop_detail.candidates.push_back({{r.alpha,r.beta},r.logLik,r.logLik,StopModel::None});}
    return failure;
  }
  std::sort(candidates.begin(),candidates.end(),[](const FatigueResult&a,const FatigueResult&b){return a.logLik>b.logLik;});
  const auto best=candidates[0];
  failure.stop_detail.candidates.clear();
  for(const auto& r:candidates)failure.stop_detail.candidates.push_back({{r.alpha,r.beta},r.logLik,r.logLik,StopModel::None});
  failure.stop_detail.loglik_lower=best.logLik;failure.stop_detail.loglik_upper=INFINITY;
  if(failure.stop_detail.budget_exhausted)return failure;
  for(std::size_t i=1;i<candidates.size();++i)if(std::abs(std::log(candidates[i].beta/best.beta))>1e-9&&best.logLik-candidates[i].logLik<=1e-10*n){failure.stop_detail.comparison_tie=true;return failure;}
  auto result=best;result.iterations=static_cast<int>(used);return result;
}
}
