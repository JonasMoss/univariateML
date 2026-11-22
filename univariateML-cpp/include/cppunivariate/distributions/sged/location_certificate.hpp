#pragma once
// Included inside sged_cert_detail, after Sum.
struct LocationResidual {
  double log_distance;
  bool left;
};

inline LocationResidual location_residual(double observation, double anchor, double offset) {
  const double difference=anchor-observation;
  if(std::isfinite(difference)) {
    const double residual=difference+offset;
    if(std::isfinite(residual))return {std::log(std::abs(residual)),residual>0};
  }
  const double half=(anchor*.5-observation*.5)+offset*.5;
  return {std::log(std::abs(half))+std::log(2.),half>0};
}

inline double location_midpoint(double lo,double hi) {
  // Geometric steps reach even subnormal displacements within the work cap.
  if(lo<0 && hi>0)return 0;
  if(hi<=0 && lo<0)return -location_midpoint(-hi,-lo);
  if(lo>=0 && lo<hi/4) {
    const double near=lo==0?std::numeric_limits<double>::denorm_min():lo;
    const double mid=std::sqrt(near)*std::sqrt(hi);
    if(mid>lo && mid<hi)return mid;
  }
  return lo+(hi-lo)/2;
}

inline SgedResult location_fit(span x,double tol,int maxit,double sd,double nu,double xi) {
  SgedResult out{NAN,sd,nu,xi,NAN,0,SgedStatus::CertificateFailure};
  const double eps=std::numeric_limits<double>::epsilon();
  const double tiny=std::numeric_limits<double>::denorm_min();
  const double eta=std::min(tol*.5,.5);
  const double logxi=std::log(xi),logsd=std::log(sd),power=nu-1;
  const double moment=dens::ged_abs_moment(nu);
  // Divide the standardized skew moments by max(xi, 1/xi) first.
  const double t=xi>1?1/xi:xi, t2=t*t, m2=moment*moment;
  const double sigma=std::sqrt((1-m2)*(1+t2*t2)+(2*m2-1)*t2);
  const double asymmetry=(xi>1?(xi-1)/xi:xi-1)*(1+t);
  const double shift=sd*(moment*asymmetry/sigma);
  const double logscale=std::log(sigma)-dens::ged_logscale(nu);
  const double lognormal=logscale-logsd-std::log1p(t2)-special::lgamma1p(1/nu);
  const double logthin=2*std::abs(logxi);
  if(!std::isfinite(shift) || !std::isfinite(lognormal))return out;

  std::vector<double> knots(x.begin(),x.end());
  std::sort(knots.begin(),knots.end());
  const auto split=std::upper_bound(knots.begin(),knots.end(),knots.front());
  const double left_count=static_cast<double>(split-knots.begin());
  knots.erase(std::unique(knots.begin(),knots.end()),knots.end());
  const double normal=x.size()*lognormal;

  auto public_mean=[&](double anchor,double offset) {
    const double mu=anchor+(offset+shift);
    return std::isfinite(mu)?mu:(anchor+shift)+offset;
  };
  auto public_loglik=[&](double mu) {
    Sum ll;
    for(double v:x)ll.add(distributions::dsged(v,mu,sd,nu,xi,true));
    return ll.value;
  };
  auto stopped=[&](double anchor,double offset) {
    const double mu=public_mean(anchor,offset),ll=public_loglik(mu);
    if(std::isfinite(ll)) {
      const double pad=1e-10*(x.size()+std::abs(ll));
      out.mu=mu;
      out.logLik=ll;
      out.stop_detail.candidates.push_back({{mu,sd,nu,xi},ll-pad,ll+pad});
      out.stop_detail.loglik_lower=ll-pad;
      out.stop_detail.loglik_upper=INFINITY;
    }
    return out;
  };
  auto accept=[&](double anchor,double lo,double hi,double representative) {
    const double mu=public_mean(anchor,representative);
    if(!std::isfinite(mu))return false;
    const double scale=std::max(std::abs(mu),sd);
    const double roundoff=8*eps*std::abs(anchor)+8*eps*std::abs(representative)+
        64*eps*std::abs(shift)+4*tiny;
    const double width=std::max(representative-lo,hi-representative);
    if(!std::isfinite(width) || width>std::max(eta*scale,roundoff))return false;
    const double radius=width+roundoff;
    // If |mu - mu*| <= eta*max(|mu|,sd), the scale at mu* is at
    // least (1-eta) times the scale at mu.
    const bool accurate=radius<=eta*scale;
    if(!accurate && roundoff<=eta*scale)return false;

    // Bound the likelihood gap with the minimum penalty of each observation
    // over the root bracket. The representative is a feasible lower bound.
    Sum minimum,penalty;
    double evaluation_error=0;
    for(double v:x) {
      auto cost=[&](double offset) {
        const auto r=location_residual(v,anchor,offset);
        const double side=r.left==(xi>1)?logthin:0;
        const double lr=(r.log_distance-logsd)+logscale+side;
        const double value=std::exp(nu*lr);
        if(value>0 && std::isfinite(value))
          evaluation_error+=value*(16*eps*nu)*(1+std::abs(r.log_distance)+
              std::abs(logsd)+std::abs(logscale)+side);
        return value;
      };
      const auto left=location_residual(v,anchor,lo);
      const auto right=location_residual(v,anchor,hi);
      if(left.left==right.left)
        minimum.add(cost(left.left?lo:hi));
      penalty.add(cost(representative));
    }
    const double ideal=normal-penalty.value;
    const double margin=1e-10*std::max(1.,std::abs(ideal));
    const double loss_bound=std::max(0.,penalty.value-minimum.value)+
        evaluation_error+16*eps*(std::abs(normal)+penalty.value);
    if(!std::isfinite(ideal) || !(loss_bound<=margin))return false;
    const double ll=public_loglik(mu);
    if(!std::isfinite(ll) || std::abs(ll-ideal)>margin-loss_bound)return false;
    out.mu=mu;
    out.logLik=ll;
    if(!accurate) {
      out.accuracy_failure=true;
      out.stop_detail.accuracy_stop=AccuracyStop::Tolerance;
      out.stop_detail.attainable_tol=2*(radius/scale);
      record_accuracy(out,tol,{mu,sd,nu,xi},{scale,0,0,0});
      return false;
    }
    out.status=SgedStatus::Ok;
    out.certified=true;
    return true;
  };

  if(knots.size()==1) {
    accept(knots.front(),0,0,0);
    return out;
  }
  if(knots.size()==2) {
    const double logcounts=std::log(left_count/(x.size()-left_count));
    const double skew=2*logxi*(nu/power), counts=logcounts/power;
    const double odds=skew+counts;
    const bool from_left=odds>=0;
    const double anchor=from_left?knots.front():knots.back();
    const double gap=knots.back()-knots.front();
    const double half=knots.back()*.5-knots.front()*.5;
    const double loggap=special::log_abs_difference(knots.back(),knots.front());
    if(!std::isfinite(odds))return out;
    const double signed_odds=std::abs(odds);
    auto distance=[&](double k) {
      const double ratio=std::exp(-std::abs(k));
      const double fraction=(k>=0?ratio:1)/(1+ratio);
      if(k<0 || std::isnormal(ratio))
        return std::isfinite(gap)?gap*fraction:half*(2*fraction);
      return std::exp(loggap-k-std::log1p(ratio));
    };
    // The log odds error moves the logistic fraction monotonically.
    const double odds_error=16*eps*(1+std::abs(skew)+std::abs(counts));
    const double near=distance(signed_odds+odds_error);
    const double far=distance(signed_odds-odds_error);
    const double pad=16*eps*(1+std::abs(loggap))*far+2*tiny;
    const double l=std::max(0.,near-pad),h=far+pad;
    const double offset=distance(signed_odds);
    accept(anchor,from_left?l:-h,from_left?h:-l,from_left?offset:-offset);
    return out;
  }

  auto sign=[&](double anchor,double offset) {
    if(out.iterations>=maxit)return 0;
    ++out.iterations;
    double largest=-INFINITY,log_magnitude=0;
    auto term=[&](const LocationResidual& r) {
      return power*r.log_distance+(r.left?nu*logxi:-nu*logxi);
    };
    for(double v:x) {
      const auto r=location_residual(v,anchor,offset);
      if(r.log_distance==-INFINITY)continue;
      const double weight=term(r);
      if(!std::isfinite(weight))return 0;
      largest=std::max(largest,weight);
      log_magnitude=std::max(log_magnitude,std::abs(r.log_distance));
    }
    if(!std::isfinite(largest))return 0;
    Sum positive,negative;
    for(double v:x) {
      const auto r=location_residual(v,anchor,offset);
      if(r.log_distance==-INFINITY)continue;
      (r.left?positive:negative).add(std::exp(term(r)-largest));
    }
    const double pad=32*eps*(1+power*log_magnitude+nu*std::abs(logxi))*
        (positive.value+negative.value);
    const double score=positive.value-negative.value;
    return score>pad?1:score<-pad?-1:0;
  };
  auto half_gap=[](double a,double b) {
    const double gap=b-a;
    return std::isfinite(gap)?gap*.5:b*.5-a*.5;
  };

  // Locate the adjacent observations on either side of the root before
  // choosing its nearer anchor. A sign indistinguishable from zero still
  // requires explicit bracketing on both sides.
  std::size_t left=0,right=knots.size()-1;
  double anchor=0,lo=0,hi=0;
  while(right-left>1) {
    const auto middle=left+(right-left)/2;
    if(out.iterations>=maxit)return stopped(knots[middle],0);
    const int s=sign(knots[middle],0);
    if(s<0)left=middle;
    else if(s>0)right=middle;
    else {
      anchor=knots[middle];
      lo=-half_gap(knots[middle-1],anchor);
      hi=half_gap(anchor,knots[middle+1]);
      if(sign(anchor,lo)>=0 || sign(anchor,hi)<=0)return stopped(anchor,0);
      break;
    }
  }
  if(right-left==1) {
    const double a=knots[left],b=knots[right],half=half_gap(a,b);
    const int s=sign(a,half);
    if(s>0){anchor=a;hi=half;}
    else if(s<0 && sign(b,-half)<0){anchor=b;lo=-half;}
    else {
      anchor=a;
      lo=half*.5;
      hi=half+lo;
      if(sign(anchor,lo)>=0 || sign(anchor,hi)<=0)return stopped(anchor,half);
    }
  }
  while(lo<hi) {
    const double mid=location_midpoint(lo,hi);
    if(accept(anchor,lo,hi,mid) || out.accuracy_failure)return out;
    if(out.iterations>=maxit || !(mid>lo && mid<hi))return stopped(anchor,mid);
    const int s=sign(anchor,mid);
    if(s<0)lo=mid;
    else if(s>0)hi=mid;
    else {
      const double l=lo+(mid-lo)/2,h=mid+(hi-mid)/2;
      const bool lower=l>lo && sign(anchor,l)<0;
      const bool upper=h<hi && sign(anchor,h)>0;
      if(!lower && !upper)return stopped(anchor,mid);
      if(lower)lo=l;
      if(upper)hi=h;
    }
  }
  return out;
}
