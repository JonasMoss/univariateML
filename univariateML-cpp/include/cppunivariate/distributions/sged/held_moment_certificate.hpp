#pragma once
// Fixed nu, both public moments held, free skew. Included after profile_bounds.
namespace sged_held_moment {
struct Box {double lo,hi,upper;sged_profile::Range score;bool valid;};
struct Region {double lo,hi,upper;};
struct Face {FitCandidate candidate;double value=NAN;};
struct Profile {
  span x; double mean,sd,p,n,k,v,D,constant;
  bool valid=false;
  Profile(span data,double m,double s,double shape):x(data),mean(m),sd(s),p(shape),n(data.size()) {
    k=std::exp(special::lgamma(1+2/p)-special::lgamma(1+1/p))/2;
    v=std::exp(special::lgamma(1+3/p)-special::lgamma(1+1/p))/3;
    D=3*v-4*k*k;
    constant=n*(std::log(p)-special::lgamma(1/p)-std::log(2.)-std::log(sd));
    valid=std::isfinite(k+v+D+constant) && k>0 && v>0 && v+D>0;
    // Cancellation or poorly represented moments cannot authorize comparisons.
    valid=valid && std::min(v,v+D)>sged_profile::margin(v+4*k*k,n)*(1+1/p);
  }
  double allowance(double value)const {
    return sged_profile::margin(std::abs(value)+std::abs(constant)+n*(10+std::abs(std::log(sd))),n)*(1+p+1/p);
  }
  double value(double z)const {
    if(!(z>-1 && z<1))return -INFINITY;
    const double w=std::sqrt(v+D*z*z);
    sged_profile::Sum penalty;
    for(double t:x){const double d=(t-mean)/sd,q=d*w/2+k*z;
      const double r=2*std::abs(q)/(q<0?1-z:1+z);penalty.add(std::pow(r,p));}
    return constant+n*std::log(w)-penalty.value;
  }
  SgedResult point(double z)const {
    SgedResult out{mean,sd,p,NAN,NAN,0,SgedStatus::CertificateFailure};
    if(!(z>-1 && z<1))return out;
    out.xi=std::exp(std::atanh(z));
    const double ideal=value(z);
    const double phi[4]={mean,std::log(sd),std::log(p),std::log(out.xi)};
    out.logLik=-SgedCost{x}(phi);
    const double pad=allowance(out.logLik);
    if(std::isfinite(pad) && std::isfinite(out.logLik-pad) && std::isfinite(out.logLik+pad) && std::isfinite(ideal+out.logLik+out.xi) && out.xi>0 &&
       std::abs(out.logLik-ideal)<=allowance(out.logLik))out.status=SgedStatus::Ok;
    return out;
  }
  Face face(int side,double target)const {
    Face out;
    const double r=std::sqrt(v-k*k),b=sd/r,c=std::fma(-side*k,b,mean);
    if(!(b>0) || !std::isfinite(b+c))return out;
    const double mean_error=128*std::numeric_limits<double>::epsilon()*(std::abs(c)+std::abs(k*b))*(1+p+1/p);
    if(mean_error>target*(1+std::abs(mean)) || 64*std::numeric_limits<double>::epsilon()*(1+p+1/p)>target)return out;
    sged_profile::Sum actual,ideal;
    for(double t:x) {
      const double stored=side*(t-c)/b,exact=side*(t-mean)/b+k;
      // Contact penalties need a one-sided analysis, so no closed-density
      // endpoint substitution is permitted here.
      const double support_margin=128*std::numeric_limits<double>::epsilon()*
          (1+std::abs((t-mean)/b)+std::abs(k))*(1+p+1/p);
      if(!(stored>support_margin && exact>support_margin) || !std::isfinite(stored+exact))return out;
      actual.add(std::pow(stored,p));ideal.add(std::pow(exact,p));
    }
    const double normalizer=n*(std::log(p)-special::lgamma(1/p)-std::log(b));
    out.value=normalizer-actual.value;
    const double expected=normalizer-ideal.value;
    if(!std::isfinite(out.value+expected) || std::abs(out.value-expected)>allowance(out.value))return Face{};
    const double pad=allowance(out.value)+std::abs(out.value-expected);
    if(!std::isfinite(pad) || !std::isfinite(out.value-pad) || !std::isfinite(out.value+pad))return Face{};
    out.candidate={{c,b,p},out.value-pad,out.value+pad,side>0?StopModel::HalfGed:StopModel::ReflectedHalfGed};
    return out;
  }
  Box box(double lo,double hi)const {
    const sged_profile::Range z{lo,hi};
    const auto w2=sged_profile::add({v,v},sged_profile::mul({D,D},sged_profile::square(z)));
    if(!(w2.lo>0))return {lo,hi,INFINITY,{-INFINITY,INFINITY},false};
    const sged_profile::Range w{std::sqrt(w2.lo),std::sqrt(w2.hi)};
    auto score=sged_profile::mul({n*D,n*D},sged_profile::divide(z,w2));
    sged_profile::Sum penalty;
    bool smooth=true;
    for(double t:x) {
      const double d=(t-mean)/sd;
      if(!std::isfinite(d))return {lo,hi,INFINITY,{-INFINITY,INFINITY},false};
      auto q=sged_profile::add(sged_profile::mul({d/2,d/2},w),sged_profile::mul({k,k},z));
      const double qpad=64*std::numeric_limits<double>::epsilon()*
          (1+std::abs(d)*w.hi/2+std::abs(k)*std::max(std::abs(lo),std::abs(hi)))*(1+p+1/p);
      q={q.lo-qpad,q.hi+qpad};
      double lower=0;
      if(q.lo>0)lower=2*q.lo/(1+hi);
      else if(q.hi<0)lower=-2*q.hi/(1-lo);
      penalty.add(std::pow(lower,p));
      if(q.lo<=0 && q.hi>=0){smooth=false;continue;}
      if(!smooth)continue;
      const double sign=q.lo>0?1:-1;
      const auto den=sged_profile::add({1,1},sged_profile::mul({sign,sign},z));
      const auto num=sged_profile::mul({2*sign,2*sign},q);
      const auto r=sged_profile::divide(num,den);
      if(!(r.lo>0) || !std::isfinite(r.hi)){smooth=false;continue;}
      const auto qp=sged_profile::add({k,k},sged_profile::mul({d*D/2,d*D/2},sged_profile::divide(z,w)));
      const auto rp=sged_profile::divide(sged_profile::add(sged_profile::mul({2*sign,2*sign},sged_profile::mul(qp,den)),sged_profile::neg(sged_profile::mul({sign,sign},num))),sged_profile::square(den));
      const sged_profile::Range power{std::min(std::pow(r.lo,p-1),std::pow(r.hi,p-1)),std::max(std::pow(r.lo,p-1),std::pow(r.hi,p-1))};
      score=sged_profile::add(score,sged_profile::neg(sged_profile::mul({p,p},sged_profile::mul(power,rp))));
    }
    const double upper=constant+n*std::log(w.hi)-penalty.value;
    if(!smooth || !std::isfinite(score.lo+score.hi))score={-INFINITY,INFINITY};
    else score=sged_profile::padded(score.lo,score.hi,n,1+p+1/p);
    const double padded_upper=upper+allowance(upper);
    return {lo,hi,padded_upper,score,std::isfinite(padded_upper)};
  }
};
inline SgedResult fit(span x,double tol,int maxit,double mean,double sd,double p) {
  const Profile profile(x,mean,sd,p);
  // Binding gradtol is still a legacy score control; retain golden accuracy.
  const double target=std::min(tol,2e-7);
  SgedResult out{mean,sd,p,NAN,NAN,0,SgedStatus::CertificateFailure};
  out.stop_detail.loglik_upper=INFINITY;
  if(!profile.valid)return out;
  std::int64_t work=0;const auto cap=certificate_budget(maxit);
  SgedResult best=profile.point(0);++work;
  if(best.status!=SgedStatus::Ok)return out;
  std::vector<Face> faces;
  const auto lower=[&]() {
    double value=best.logLik-profile.allowance(best.logLik);
    for(const auto& face:faces)value=std::max(value,face.candidate.loglik_lower);
    return value;
  };
  const auto stop=[&](bool budget) {
    auto r=best;r.status=SgedStatus::CertificateFailure;r.certified=false;r.iterations=static_cast<int>(work);
    r.stop_detail.budget_exhausted=budget;
    const double pad=profile.allowance(best.logLik);
    r.stop_detail.candidates.push_back({{mean,sd,p,best.xi},best.logLik-pad,best.logLik+pad});
    for(const auto& face:faces)r.stop_detail.candidates.push_back(face.candidate);
    r.stop_detail.loglik_lower=lower();r.stop_detail.loglik_upper=INFINITY;return r;
  };
  for(int side:{-1,1}) {
    if(work>=cap)return stop(true);
    const auto face=profile.face(side,target);++work;
    if(!face.candidate.coef.empty())faces.push_back(face);
  }
  const auto has_face=[&](StopModel model) {
    for(const auto& face:faces)if(face.candidate.model==model)return true;
    return false;
  };
  std::vector<Region> pending{{-1,1,INFINITY}},roots;
  double cell_goal=target/4;
  for(;;) {
  std::size_t cursor=0;
  while(cursor<pending.size()) {
    if(work+2>cap)return stop(true);
    const auto cell=pending[cursor++];
    const auto b=profile.box(cell.lo,cell.hi);++work;
    if(!b.valid)return stop(false);
    if(b.upper<lower())continue;
    // At an endpoint a definite score can discard the cell only when its
    // winning endpoint has a recovered strict-support face witness.
    if(b.score.lo>0 && (cell.hi<1 || has_face(StopModel::HalfGed)))continue;
    if(b.score.hi<0 && (cell.lo>-1 || has_face(StopModel::ReflectedHalfGed)))continue;
    const double mid=cell.lo/2+cell.hi/2;
    if(!(mid>cell.lo && mid<cell.hi))return stop(false);
    auto candidate=profile.point(mid);++work;
    if(candidate.status!=SgedStatus::Ok)return stop(false);
    if(candidate.logLik>best.logLik)best=candidate;
    if(cell.lo>-1 && cell.hi<1 && std::atanh(cell.hi)-std::atanh(cell.lo)<=cell_goal) {
      double upper=b.upper;
      if(std::isfinite(b.score.lo+b.score.hi))upper=std::min(upper,candidate.logLik+profile.allowance(candidate.logLik)+std::max(std::abs(b.score.lo),std::abs(b.score.hi))*(cell.hi-cell.lo));
      roots.push_back({cell.lo,cell.hi,upper});continue;
    }
    pending.push_back({mid,cell.hi,INFINITY});pending.push_back({cell.lo,mid,INFINITY});
  }
  const double threshold=lower();
  double lo=INFINITY,hi=-INFINITY,upper=best.logLik+profile.allowance(best.logLik);
  std::vector<Face> winning_faces;
  for(const auto& face:faces) {
    upper=std::max(upper,face.candidate.loglik_upper);
    if(face.candidate.loglik_upper>=threshold)winning_faces.push_back(face);
  }
  std::vector<Region> winners;
  for(const auto& root:roots)if(root.upper>=threshold) {
    lo=std::min(lo,root.lo);hi=std::max(hi,root.hi);upper=std::max(upper,root.upper);winners.push_back(root);
  }
  if(winners.empty() && winning_faces.size()==1) {
    const auto& face=winning_faces[0];
    out.status=face.candidate.model==StopModel::HalfGed?SgedStatus::RightHalfGedBoundary:SgedStatus::LeftHalfGedBoundary;
    out.logLik=face.value;out.iterations=static_cast<int>(work);out.certified=true;
    out.stop_detail=limit_detail(face.candidate.coef,face.value,face.candidate.model);
    out.stop_detail.candidates={face.candidate};out.stop_detail.loglik_lower=threshold;out.stop_detail.loglik_upper=upper;return out;
  }
  if(winners.empty() && winning_faces.empty())return stop(false);
  if(!winning_faces.empty() || std::atanh(hi)-std::atanh(lo)>target) {
    // Adjacent cells describe one unresolved region, not distinct maxima.
    std::sort(winners.begin(),winners.end(),[](const Region& a,const Region& b){return a.lo<b.lo;});
    std::vector<Region> groups;
    for(const auto& root:winners) {
      if(!groups.empty() && root.lo<=groups.back().hi) {
        groups.back().hi=std::max(groups.back().hi,root.hi);
        groups.back().upper=std::max(groups.back().upper,root.upper);
      } else groups.push_back(root);
    }
    if(groups.size()+winning_faces.size()<2) {
      // Interval dependency can leave a connected UNION wider than its cells.
      // Revisit only this surviving cover, without loosening the target.
      cell_goal/=2;
      if(!(cell_goal>0))return stop(false);
      pending=winners;roots.clear();continue;
    }
    std::vector<FitCandidate> candidates;
    for(const auto& group:groups) {
      if(work>=cap)return stop(true);
      const auto witness=profile.point(group.lo/2+group.hi/2);++work;
      if(witness.status!=SgedStatus::Ok)return stop(false);
      const double pad=profile.allowance(witness.logLik);
      candidates.push_back({{mean,sd,p,witness.xi},witness.logLik-pad,witness.logLik+pad});
    }
    for(const auto& face:winning_faces)candidates.push_back(face.candidate);
    auto r=stop(false);r.stop_detail.comparison_tie=true;r.stop_detail.candidates=std::move(candidates);
    r.stop_detail.loglik_upper=upper;return r;
  }
  if(work>=cap)return stop(true);
  auto representative=profile.point(lo/2+hi/2);++work;
  if(representative.status!=SgedStatus::Ok || representative.logLik<best.logLik-2*profile.allowance(best.logLik))return stop(false);
  const double returned_logxi=std::log(representative.xi);
  if(std::max(std::atanh(hi),returned_logxi)-std::min(std::atanh(lo),returned_logxi)>target)return stop(false);
  best=representative;best.certified=true;best.iterations=static_cast<int>(work);
  best.stop_detail.loglik_lower=best.logLik-profile.allowance(best.logLik);best.stop_detail.loglik_upper=upper;return best;
  }
}
} // namespace sged_held_moment
