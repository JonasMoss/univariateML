#pragma once

namespace cppunivariate { namespace dnorm_detail {

inline DnormResult at_pair(const Cells& p, double mean, double sd, int iterations) {
  if (!std::isfinite(mean) || !std::isfinite(sd) || !(sd>0))
    return {mean,sd,NAN,iterations,DnormStatus::IterationFailure};
  double ll=0;
  for (std::size_t j=0; j<p.x.size(); ++j)
    ll+=p.w[j]*distributions::ddnorm(p.x[j],mean,sd,true);
  if (std::isnan(ll))
    return {mean,sd,ll,iterations,DnormStatus::IterationFailure,{},OutcomeReason::Evaluation};
  return {mean,sd,ll,iterations,DnormStatus::Ok};
}

inline double constant_scale(double delta) {
  // Never form an overflowing reciprocal or an overflowing 2*delta.
  if (delta < 1) return std::sqrt((0.5+delta)/(std::log1p(delta)-std::log(delta)));
  const double reciprocal=1/delta;
  return delta*std::sqrt((1+0.5*reciprocal)/(std::log1p(reciprocal)/reciprocal));
}

inline double log_add(double a, double b) {
  const double hi=std::max(a,b), lo=std::min(a,b);
  return std::isfinite(hi) ? hi+std::log1p(std::exp(lo-hi)) : hi;
}

// Sign of the log likelihood's inverse-scale derivative. Reflect the
// histogram so the mean is just inside the left cell.
inline int adjacent_scale_sign(double log_sd, double delta, double left, double right) {
  const double log_delta=std::log(delta);
  const double log_t=log_delta-log_sd;
  const double t=std::exp(log_t), B=std::exp(-log_sd);
  const double a=(1-delta)*B, b=(1+delta)*B;
  const double lp=distributions::dnorm_interval(-a,t);
  const double rp=distributions::dnorm_interval(t,b);
  if (!std::isfinite(lp) || !std::isfinite(rp)) return 0;
  const double wleft=std::log(left), wright=std::log(right);
  const double outer=log_add(wleft+rp+std::log1p(-delta)-0.5*a*a,
                             wright+lp+std::log1p(delta)-0.5*b*b);

  // For balanced cells, p_left-p_right is the integral of
  // phi(u)-phi(u-B) from -t to t. Integrate the fourth-order Taylor
  // polynomial and keep the factor t in logs, even when t underflows.
  double log_difference, error=0;
  if (left==right && t<1e-3) {
    const double t2=t*t;
    double factor=1-t2/6+t2*t2/40;
    if (B<12) {
      const double b2=B*B;
      factor-=std::exp(-b2/2)*(1+(b2-1)*t2/6+(b2*b2-6*b2+3)*t2*t2/120);
    }
    log_difference=wleft+log_t+0.5*std::log(2/std::acos(-1.0))+std::log(factor);
    error=1e-13;
  } else {
    const double x=wright+lp, y=wleft+rp;
    if (y-x > score_margin*(1+std::abs(x)+std::abs(y))) return 1;
    if (!(x-y > score_margin*(1+std::abs(x)+std::abs(y)))) return 0;
    log_difference=x+special::fs_log1mexp(y-x);
    // Comparing logs magnifies probability error when the seam terms cancel.
    error=score_margin*(1+std::abs(x)+std::abs(y))/(-std::expm1(y-x));
  }
  const double seam=log_delta-0.5*t*t+log_difference;
  if (outer==-INFINITY) return -1;
  if (std::abs(outer-seam)<=error+score_margin*(1+std::abs(outer)+std::abs(seam))) return 0;
  return outer>seam ? 1 : -1;
}

inline DnormResult adjacent_scale(const Cells& p, double mean, double delta,
                                  double tol, int maxit) {
  const bool left_side=mean<p.x.back();
  const double left=p.w[left_side ? 0 : 1], right=p.w[left_side ? 1 : 0];
  const auto sign=[&](double log_sd) { return adjacent_scale_sign(log_sd,delta,left,right); };
  double lo=std::log(delta)-std::log(12.0), hi=0;
  if (sign(lo)!=-1 || sign(hi)!=1) return {mean,NAN,NAN,0,DnormStatus::IterationFailure};
  int iteration=0;
  for (; iteration<maxit; ++iteration) {
    const double mid=lo+(hi-lo)/2;
    const double scale=std::exp(mid);
    const double rounding=4*std::numeric_limits<double>::epsilon()*(1+std::abs(mid));
    const double radius=(hi-lo)/2+rounding;
    const double relative=std::expm1(radius)+(std::nextafter(scale,INFINITY)-scale)/scale;
    if (scale>0 && relative<=tol*(1-relative))
      return at_pair(p,mean,scale,iteration+1);

    const int side=sign(mid);
    if (side<0) lo=mid;
    else if (side>0) hi=mid;
    else {
      const double probe=std::min((hi-lo)/4,tol/8);
      if (sign(mid-probe)==-1 && sign(mid+probe)==1) {
        lo=mid-probe;
        hi=mid+probe;
      } else break;
    }
  }
  return {mean,std::exp(lo+(hi-lo)/2),NAN,iteration,DnormStatus::IterationFailure};
}

inline bool symmetric(const Cells& p) {
  const double sum=p.x.front()+p.x.back();
  for (std::size_t j=0; j<p.x.size()/2; ++j) {
    const std::size_t k=p.x.size()-1-j;
    if (p.x[j]+p.x[k]!=sum || p.w[j]!=p.w[k]) return false;
  }
  return p.x.size()%2==0 || 2*p.x[p.x.size()/2]==sum;
}

template <typename Sign>
inline DnormResult seam_location(const Cells& p, double seam, double sd, double tol,
                                 int maxit, Sign sign) {
  double lo=-12, hi=12;
  int it=0;
  for (; it<maxit; ++it) {
    const double q=lo+(hi-lo)/2;
    const double mean=std::fma(q,sd,seam);
    // Express product rounding in q units. This also sees the loss of
    // all fractional bits when sd is subnormal.
    const double represented_shift=(mean-seam)/sd;
    const double error_q=(hi-lo)/2+std::abs(represented_shift-q)+
        score_margin*(1+std::abs(represented_shift));
    if (error_q<=std::max(tol,tol*std::abs(mean)/sd-tol*error_q))
      return at_pair(p,mean,sd,it+1);
    // Fused reconstruction is the nearest representable location to the
    // bracket centre. Even that point can be too far from every root in
    // the bracket, so increasing the iteration limit cannot help.
    const double nearest=std::abs(represented_shift-q);
    const double uncertainty=(hi-lo)/2+score_margin*(1+std::abs(represented_shift));
    if (nearest-uncertainty>tol*std::max(1.0,std::abs(mean)/sd+nearest+uncertainty) &&
        std::fma(lo,sd,seam)==mean && std::fma(hi,sd,seam)==mean)
      return {mean,sd,NAN,it+1,DnormStatus::IterationFailure,{},OutcomeReason::Tolerance};
    const int side=sign(q);
    if (side<0) lo=q;
    else if (side>0) hi=q;
    else {
      const double probe=std::min((hi-lo)/4,tol/8);
      if (sign(q-probe)==-1 && sign(q+probe)==1) {
        lo=q-probe;
        hi=q+probe;
      } else break;
    }
    if (lo==hi) break;
  }
  return {seam,sd,NAN,it,DnormStatus::IterationFailure};
}

inline DnormResult adjacent_location(const Cells& p, double sd, double tol, int maxit) {
  const double target=std::log(p.w[0])-std::log(p.w[1]);
  // For |q|<=12 and sd<=1/40 the omitted outer tails and their
  // densities are <= exp(-392). This is negligible even at the
  // smallest inner tail on the bracket, Phi(-12).
  const auto sign=[&](double q) {
    const double left=special::fs_normal_p(-q,true,true);
    const double right=special::fs_normal_p(q,true,true);
    const double difference=right-left+target;
    return std::abs(difference)>score_margin*(1+std::abs(left)+std::abs(right)+std::abs(target))
        ? (difference>0 ? 1 : -1) : 0;
  };
  return seam_location(p,p.x.back(),sd,tol,maxit,sign);
}

// The remote cells contribute their projected edge minus mean, with a
// conditional inward displacement at most sd^2 / distance. This follows
// by bounding the normal excess's survival function by an exponential.
inline bool balanced_at_origin(const Cells& p, double& remote, double& left, double& right) {
  if (p.n>=0x1p52) return false;
  constexpr auto cap=std::numeric_limits<std::uint64_t>::max();
  std::uint64_t positive=0, negative=0;
  for (std::size_t j=0; j<p.x.size(); ++j) {
    const double cell=p.x[j], count=p.w[j];
    if (cell==-1) left=count;
    else if (cell==0) right=count;
    else {
      const auto edge=static_cast<std::uint64_t>(cell<0 ? -(cell+1) : cell);
      const auto multiplicity=static_cast<std::uint64_t>(count);
      auto& total=cell<0 ? negative : positive;
      if (multiplicity>(cap-total)/edge) return false;
      total+=multiplicity*edge;
      remote+=count;
    }
  }
  return positive==negative && remote>0;
}

inline DnormResult projected_origin(const Cells& p, double remote, double left,
                                    double right, double sd, double tol, int maxit) {
  // In q=mean/sd, remote projections sum to -remote*q. The two cells
  // at zero have half-line scores with exponentially small truncation
  // error on [-12,12]. Remote edges stay at least 1/2 from the trial mean.
  const auto sign=[&](double q) {
    const double lp=-0.5*q*q-0.91893853320467274178;
    const double log_left=special::fs_normal_p(-q,true,true);
    const double log_right=special::fs_normal_p(q,true,true);
    const double a=left*std::exp(lp-log_left), b=right*std::exp(lp-log_right);
    const double projected=-remote*q;
    const double score=projected+(b-a);
    const double error=p.n*(2*sd+1e-130)+score_margin*
        (std::abs(projected)+a*(1+std::abs(lp)+std::abs(log_left))+
         b*(1+std::abs(lp)+std::abs(log_right)));
    return score>error ? 1 : score<-error ? -1 : 0;
  };
  if (sign(-12)!=1 || sign(12)!=-1)
    return {0,sd,NAN,0,DnormStatus::IterationFailure};
  return seam_location(p,0,sd,tol,maxit,[&](double q) { return -sign(q); });
}

// At tiny fixed scales the conditional normal mean in each observed cell
// is within sd of the projection of the public location onto that cell.
// The projected score stays finite even when standardized squares overflow.
inline DnormResult projected_location(const Cells& p, double sd, double tol, int maxit) {
  double remote=0, left=0, right=0;
  if (balanced_at_origin(p,remote,left,right)) {
    const auto origin=projected_origin(p,remote,left,right,sd,tol,maxit);
    if (origin.status==DnormStatus::Ok || origin.failure_reason==OutcomeReason::Tolerance)
      return origin;
  }
  const auto sign=[&](double mean) {
    double score=0, error=p.n*sd;
    for (std::size_t j=0; j<p.x.size(); ++j) {
      const double residual=std::clamp(mean,p.x[j],p.x[j]+1)-mean;
      const double term=p.w[j]*residual;
      score+=term;
      error+=score_margin*std::abs(term);
    }
    return score>error ? 1 : score<-error ? -1 : 0;
  };
  double lo=p.x.front()+0.5, hi=p.x.back()+0.5;
  for (int it=0; it<maxit; ++it) {
    const double mid=lo+(hi-lo)/2;
    const double radius=std::max(mid-lo,hi-mid);
    if (enclosed(mid,sd,radius,0,tol)) return at_pair(p,mid,sd,it+1);
    const int side=sign(mid);
    if (side>0) lo=mid;
    else if (side<0) hi=mid;
    else {
      const double radius_goal=tol*std::max(std::abs(mid),sd)/4;
      const double probe=std::min(radius/2,radius_goal);
      if (sign(mid-probe)==1 && sign(mid+probe)==-1) {
        lo=mid-probe;
        hi=mid+probe;
      } else break;
    }
    if (lo==hi) break;
  }
  return {lo+(hi-lo)/2,sd,NAN,maxit,DnormStatus::IterationFailure};
}

}}  // namespace cppunivariate::dnorm_detail
