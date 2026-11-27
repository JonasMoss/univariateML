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
// Samples so concentrated that the harmonic and arithmetic means round together (relative spread
// below about 1e-7), both coefficients free. With c = min(x), l_i = log1p((x_i - c) / c) (exact
// subtraction), beta = c exp(u) and e_i = l_i - u, the log likelihood is
//   -sum log x_i + sum log cosh(e_i / 2) - n log(alpha) - n log(2 pi) / 2 - sum 2 sinh(e_i / 2)^2 / alpha^2,
// alpha^2 = sum 4 sinh(e_i / 2)^2 / n at the maximum, and the profile score in u is
//   -sum tanh(e_i / 2) / 2 + n sum sinh(e_i) / sum 4 sinh(e_i / 2)^2,
// positive at u = min l and negative at u = max l. Its root is bisected, beta is rounded once
// (fma(c, expm1(u), c)) and alpha stays the maximizer's, not the best alpha at the rounded beta;
// logLik is evaluated at the returned coefficients. The bound is numerical: uncertified.
inline bool offset_fit(span x,FatigueResult& out) {
  const std::size_t n=x.size();
  if(n<2)return false;
  double mn=INFINITY,mx=-INFINITY;
  for(double v:x){if(!(v>0)||!std::isfinite(v))return false;mn=std::min(mn,v);mx=std::max(mx,v);}
  if(!(mx>mn)||!(mx<=2*mn))return false;
  std::vector<double> l;double sum_log_x=0;
  for(double v:x){l.push_back(std::log1p((v-mn)/mn));sum_log_x+=std::log(v);}
  const auto logcosh=[](double h){return std::abs(h)<1e-3?h*h*(.5-h*h*(1./12-h*h/45)):std::log(std::cosh(h));};
  const auto s_total=[&](double u){double s=0;for(double v:l){const double h=std::sinh((v-u)/2);s+=4*h*h;}return s;};
  const auto score=[&](double u){
    double t=0,sh=0;
    for(double v:l){const double e=v-u;t+=std::tanh(e/2);sh+=std::sinh(e);}
    return -t/2+static_cast<double>(n)*sh/s_total(u);
  };
  double lo=0,hi=*std::max_element(l.begin(),l.end());
  if(!(score(lo)>0)||!(score(hi)<0))return false;
  for(int i=0;i<2000;++i){const double m=lo+(hi-lo)/2;if(!(m>lo&&m<hi))break;if(score(m)>0)lo=m;else hi=m;}
  const double u=lo+(hi-lo)/2;
  const double alpha=std::sqrt(s_total(u)/static_cast<double>(n));
  const double beta=std::fma(mn,std::expm1(u),mn);
  if(!(alpha>0)||!(beta>0)||!std::isfinite(alpha)||!std::isfinite(beta))return false;
  const double ur=std::log1p((beta-mn)/mn);
  double ll=-sum_log_x-static_cast<double>(n)*(std::log(alpha)+.5*std::log(2*std::acos(-1.0)));
  for(double v:l){const double e=v-ur,h=std::sinh(e/2);ll+=logcosh(e/2)-2*h*h/(alpha*alpha);}
  if(!std::isfinite(ll))return false;
  out=FatigueResult{alpha,beta,ll,0,FatigueStatus::Ok};
  out.certified=false;
  out.stop_detail.candidates={{{alpha,beta},ll,ll,StopModel::None}};
  out.stop_detail.loglik_lower=ll;out.stop_detail.loglik_upper=INFINITY;
  return true;
}
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
  if(fb){auto r=value(beta/unit);r.beta=beta;r.singleton_proved=true;if(!fa&&r.alpha==0)r.status=FatigueStatus::Unbounded;else if(!std::isfinite(r.logLik))return failure;return r;}
  if(!fa&&mn==1){failure.status=FatigueStatus::Unbounded;return failure;}
  double lo=H,hi=A;
  if(fa){double r=1;if(alpha>2){r=std::pow(alpha/2+std::sqrt((alpha/2-1)*(alpha/2+1)),2);}lo=mn/r;hi=r;}
  if(!(lo>0)||!std::isfinite(hi))return failure;
  // A/H can round together before the induced alpha accuracy is resolved.
  // Do not turn a tiny beta bracket into an unchecked alpha estimate.
  if(!fa&&std::abs(hi-lo)<=128*std::numeric_limits<double>::epsilon()*hi) {
    {FatigueResult solved=failure;if(offset_fit(x,solved))return solved;}
    const auto witness=value(A);
    if(witness.status==FatigueStatus::Ok)failure.stop_detail.candidates={{{witness.alpha,witness.beta},witness.logLik,witness.logLik,StopModel::None}};
    return failure;
  }
  if (!fa) {
    // The profiled scale score has exactly one zero in (H, (H+A)/2).
    // Enlarge the analytic bracket for rounding in the input ratios
    // and moments, then retain only sign-confirmed updates.
    const double round = (16 + 4*n)*std::numeric_limits<double>::epsilon();
    const Interval harmonic(H*(1-round), H*(1+round));
    const Interval arithmetic(A*(1-round), A*(1+round));
    const auto sign = [&](double b) {
      CPPUNIV_EXTENDED_REAL reciprocal = 0;
      for (double v : y) reciprocal += 1/(static_cast<CPPUNIV_EXTENDED_REAL>(v)+b);
      const double xi = n/static_cast<double>(reciprocal);
      const auto delta = Interval(b)-harmonic;
      return harmonic*(arithmetic-harmonic) - delta*(Interval(xi*(1-round),xi*(1+round))-delta);
    };
    double lower = harmonic.lo, upper = (harmonic.hi+arithmetic.hi)/2;
    if (lower > 0 && sign(lower).lo > 0 && sign(upper).hi < 0) {
      const auto enclosed = [&](const FatigueResult& r) {
        if (r.status != FatigueStatus::Ok) return false;
        const double beta_lo = lower*unit*(1-round), beta_hi = upper*unit*(1+round);
        if (!(beta_lo > 0 && std::isfinite(beta_hi) && std::max(r.beta-beta_lo,beta_hi-r.beta) <= tol*beta_lo))
          return false;
        CPPUNIV_EXTENDED_REAL ss_lo = 0, ss_hi = 0;
        const double root_n = std::sqrt(n);
        for (double v : x) {
          // Include the rounding in the public threshold subtraction,
          // where present. The signed RMS residual increases in v
          // and decreases in beta.
          const double v_lo = v*(1-4*std::numeric_limits<double>::epsilon());
          const double v_hi = v*(1+4*std::numeric_limits<double>::epsilon());
          if (!(v_lo > 0 && std::isfinite(v_hi))) return false;
          const double dl = (v_lo-beta_hi)/std::sqrt(v_lo)/std::sqrt(beta_hi);
          const double dh = (v_hi-beta_lo)/std::sqrt(v_hi)/std::sqrt(beta_lo);
          const double small = std::max({0.,dl,-dh})/root_n;
          const double large = std::max(-dl,dh)/root_n;
          ss_lo += small*small;
          ss_hi += large*large;
        }
        const double a_lo = std::sqrt(static_cast<double>(ss_lo))*(1-round);
        const double a_hi = std::sqrt(static_cast<double>(ss_hi))*(1+round);
        return a_lo > 0 && std::max(r.alpha-a_lo,a_hi-r.alpha) <= tol*a_lo;
      };
      for (std::int64_t it=0; it<solver_budget(maxit); ++it) {
        const double mid = std::sqrt(lower)*std::sqrt(upper);
        const auto at = sign(mid);
        const bool ambiguous = at.lo <= 0 && at.hi >= 0;
        const double old_lower = lower, old_upper = upper;
        if (at.lo > 0) lower = mid;
        else if (at.hi < 0) upper = mid;
        else {
          const double left = std::sqrt(lower)*std::sqrt(mid);
          const double right = std::sqrt(mid)*std::sqrt(upper);
          if (sign(left).lo > 0) lower = left;
          if (sign(right).hi < 0) upper = right;
        }
        if (upper-lower <= .01*tol*upper || ambiguous) {
          auto joint = value(std::sqrt(lower)*std::sqrt(upper));
          if (enclosed(joint)) {
            joint.iterations = static_cast<int>(it+1);
            joint.singleton_proved = true;
            return joint;
          }
        }
        if (lower == old_lower && upper == old_upper) break;
      }
    }
  }
  if(lo==hi) { auto r=value(lo); r.singleton_proved=fa && alpha<=2 && std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}); return r; }
  if(fa&&mn==1) {
    const auto first=value(lo),second=value(hi);
    if(first.status!=FatigueStatus::Ok||second.status!=FatigueStatus::Ok)return failure;
    failure.stop_detail=comparison_detail({{first.alpha,first.beta},first.logLik,first.logLik,StopModel::None},{{second.alpha,second.beta},second.logLik,second.logLik,StopModel::None});
    failure.proved_tie = true;
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
  auto result=best;result.certified=fa;result.singleton_proved=fa && candidates.size()==1;result.iterations=static_cast<int>(used);return result;
}
}
