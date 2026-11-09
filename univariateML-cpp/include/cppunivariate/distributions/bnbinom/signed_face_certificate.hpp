// Included after joint_certificate.hpp. Sufficient global face proof; failure
// leaves the existing score-checked search and its uncertified result intact.
#pragma once
namespace cppunivariate {
namespace bnbinom_detail {
struct SignedFaceProof {
  bool proved=false,budget_exhausted=false;
  long long evaluations=0;
  double size_upper=NAN,concentration_upper=NAN;
  std::array<double,6> unresolved_box{};
};
// F(z,h)=log Gamma(z+h)-log Gamma(z)-h log z.
// |F(z,h)-h(h-1)/(2z)| <= H(h)/z^2 for every z,h>0.
inline double signed_gamma_remainder(double h) {
  return h*h*h/6+h*h/4+h/12;
}
struct SignedNbBound { double coefficient, remainder; };
inline SignedNbBound signed_nb_bound(const Data &d,double rl,double rh,
                                     double pl,double ph) {
  const double ql=1-ph,qh=1-pl;
  const double quadratic=std::max(rl*(rl-1),rh*(rh-1));
  const double first=quadratic*(quadratic>=0?qh/pl:ql/ph);
  CPPUNIV_EXTENDED_REAL c=d.n*first,rem=0,mag=std::abs(c);
  for(std::size_t i=0;i<d.x.size();++i) {
    const double x=d.x[i],term=x*(x-1)*ph/ql-2*rl*x;
    c+=d.count[i]*term;
    mag+=d.count[i]*(x*(x-1)*ph/ql+2*rl*x);
    rem+=d.count[i]*(signed_gamma_remainder(rh)/(pl*pl)+
      signed_gamma_remainder(x)/(ql*ql)+signed_gamma_remainder(rh+x));
  }
  const double e=4096*std::numeric_limits<double>::epsilon();
  return {static_cast<double>(c/2+e*mag),static_cast<double>(rem)*(1+e)};
}
inline AlphaLikelihood signed_nb_likelihood_bound(const Data &d,double rl,double rh,
                                                  double pl,double ph) {
  CPPUNIV_EXTENDED_REAL total=0,mag=0;
  for(std::size_t i=0;i<d.x.size();++i) {
    const double x=d.x[i];
    const CPPUNIV_EXTENDED_REAL a=increment(rh,d.x[i]),b=special::lgamma(x+1),
      c=rl*std::log(ph),e=x*std::log1p(-pl);
    total+=d.count[i]*(a-b+c+e);
    mag+=d.count[i]*(std::abs(a)+std::abs(b)+std::abs(c)+std::abs(e));
  }
  const double value=static_cast<double>(total);
  return {value,joint_error(d,value,static_cast<double>(mag))};
}
// For h=1/r+1/b, lambda=rb/a and h<=hmax,
// ell <= ell_Pois(lambda)+B(lambda)*h+R(lambda)*h^2.
inline double signed_poisson_remainder(const Data &d,double lambda,double hmax=1) {
  CPPUNIV_EXTENDED_REAL sum=0;
  for(std::size_t i=0;i<d.x.size();++i) {
    const double x=d.x[i];
    sum+=d.count[i]*x*std::pow(1+std::max(0.,x-1)*hmax/4,2);
  }
  return static_cast<double>(sum)*lambda*lambda*.5*
    (1+4096*std::numeric_limits<double>::epsilon());
}
struct SignedFaceBox {double rl,rh,vl,vh,tl,th;};
inline SignedFaceProof signed_face_cover(const Data &d,double nb_size,
                                         double face_lower,double poisson_upper,int maxit) {
  SignedFaceProof out;
  const long long cap=certificate_budget(maxit);
  const auto spend=[&]() {if(++out.evaluations<=cap)return true;out.budget_exhausted=true;return false;};
  if(!(d.n>0) || !std::isfinite(face_lower+poisson_upper) || maxit<1)return out;
  double mean=0,f2=0,positive=0,factorial=0;
  for(std::size_t i=0;i<d.x.size();++i) {
    const double x=d.x[i],w=d.count[i];mean+=w*x/d.n;f2+=w*x*(x-1);
    factorial+=w*special::lgamma(x+1);if(x>0)positive+=w;
  }
  if(!(mean>0 && positive>0))return out;
  const bool poisson=!std::isfinite(nb_size);
  // Small r or b cannot reach even the attained face witness.
  double lower=1;
  while(true) {
    if(!spend())return out;
    double value=0,mag=0;
    for(std::size_t i=0;i<d.x.size();++i)if(d.x[i]>0) {
      const double a=static_cast<double>(increment(lower,d.x[i])),b=special::lgamma(d.x[i]+1.);
      value+=d.count[i]*std::min(0.,a-b);mag+=d.count[i]*(std::abs(a)+std::abs(b));
    }
    if(value+joint_error(d,value,mag)<face_lower)break;
    lower*=.5;if(!(lower>0))return out;
  }
  double M=std::max(1.,2*mean);
  while(true) {
    if(!spend())return out;
    double value=-d.n*M,mag=d.n*M;
    for(std::size_t i=0;i<d.x.size();++i) {
      const double a=static_cast<double>(increment(M,d.x[i])),b=special::lgamma(d.x[i]+1.);
      value+=d.count[i]*(a-b);mag+=d.count[i]*(std::abs(a)+std::abs(b));
    }
    if(value+joint_error(d,value,mag)<face_lower)break;
    M*=2;if(!std::isfinite(M))return out;
  }
  double U=4*M;
  if(!poisson) {
    const double gap=face_lower-poisson_upper;
    if(!(gap>0))return out;
    U=std::max(U,4*(4*d.n*M*M+f2/2)/gap);
  } else {
    // B(mean)=n*(variance-mean)/2 must be strictly negative. No conclusion
    // follows from a zero or unresolved dispersion sign.
    const double B0=(f2-d.n*mean*mean)/2;
    const double margin=4096*std::numeric_limits<double>::epsilon()*(f2+d.n*mean*mean);
    if(!(B0 < -margin))return out;
    std::vector<std::array<double,2>> pending{{{0,2*M}}};
    while(!pending.empty()) {
      if(!spend())return out;
      const auto q=pending.back();pending.pop_back();
      const double B=B0+margin+d.n*.5*std::max(std::pow(q[0]-mean,2),std::pow(q[1]-mean,2));
      const double R=signed_poisson_remainder(d,q[1]);
      double h=0;
      if(B<0)h=std::min(1.,-B/(2*(1+R)));
      else {
        const double value=-d.n*q[0]+d.n*mean*std::log(q[1])-factorial;
        const double gap=face_lower-value-joint_error(d,value,std::abs(value)+2*factorial);
        if(gap>0)h=std::min(1.,gap/(4*(1+std::abs(B)+R)));
      }
      if(h>0 && std::isfinite(h)) {U=std::max(U,2/h);continue;}
      const double mid=q[0]+(q[1]-q[0])/2;
      if(!(q[0]<mid && mid<q[1]))return out;
      pending.push_back({q[0],mid});pending.push_back({mid,q[1]});
    }
  }
  if(!(U>lower) || !std::isfinite(U))return out;
  out.size_upper=U;
  // Uniform Jensen/Markov exclusions for p=a/(a+b) at bounded size.
  double pmin=.5,qmin=.5;
  const double delta=std::min(lower,1.);
  for(int side=0;side<2;++side)while(true) {
    if(!spend())return out;
    double value=0,mag=0;
    const double v=side?qmin:pmin;
    for(std::size_t i=0;i<d.x.size();++i) {
      double term=0;
      if(side) {if(d.x[i]>0)term=std::min(0.,std::log(std::max(U,1.))+std::log(v));}
      else term=std::min(0.,static_cast<double>(increment(delta+1,d.x[i]))-
        special::lgamma(d.x[i]+1.)+delta*std::log(v));
      value+=d.count[i]*term;mag+=d.count[i]*std::abs(term);
    }
    if(value+joint_error(d,value,mag)<face_lower)break;
    if(side)qmin*=.5;else pmin*=.5;
    if(!(pmin>0 && qmin>0))return out;
  }
  // v=logit(p), t=1/(a+b) compactifies both NB labellings after r<=b.
  const double vl=std::log(pmin)-std::log1p(-pmin),vh=std::log1p(-qmin)-std::log(qmin);
  std::vector<SignedFaceBox> pending{{lower,U,vl,vh,0,1/lower}};
  const auto probability=[](double v) {return v>=0?1/(1+std::exp(-v)):std::exp(v)/(1+std::exp(v));};
  while(!pending.empty()) {
    if(!spend())return out;
    auto q=pending.back();pending.pop_back();
    out.unresolved_box={{q.rl,q.rh,q.vl,q.vh,q.tl,q.th}};
    const double pl=probability(q.vl),ph=probability(q.vh),ql=1-ph,qh=1-pl;
    if(!(pl>0 && ql>0))return out;
    q.th=std::min(q.th,qh/q.rl);
    if(q.th<q.tl)continue;
    double boundary_upper=0,boundary_mag=0;
    const double exponent=std::min(q.rl,1.);
    for(std::size_t i=0;i<d.x.size();++i) {
      const double a=static_cast<double>(increment(exponent+1,d.x[i]))-special::lgamma(d.x[i]+1.);
      const double term=std::min(0.,a+exponent*std::log(ph));
      boundary_upper+=d.count[i]*term;boundary_mag+=d.count[i]*(std::abs(a)+std::abs(exponent*std::log(ph)));
    }
    if(boundary_upper+joint_error(d,boundary_upper,boundary_mag)<face_lower)continue;
    const double q_upper=positive*std::min(0.,std::log(std::max(q.rh,1.))+std::log(qh));
    if(q_upper+joint_error(d,q_upper,std::abs(q_upper))<face_lower)continue;
    const double lamlo=q.rl*ql/ph,lamhi=q.rh*qh/pl;
    const double hmax=std::min(2/q.rl,1/q.rl+q.th/ql),hmin=1/q.rh+q.tl/qh;
    const auto record_tail=[&]() {
      if(q.tl==0 && q.th>0)out.concentration_upper=std::max(
        std::isfinite(out.concentration_upper)?out.concentration_upper:0.,1/q.th);
    };
    if(std::isfinite(lamhi+hmax) && hmax>0) {
      const double B=(f2-d.n*mean*mean+d.n*std::max(std::pow(lamlo-mean,2),std::pow(lamhi-mean,2)))/2;
      const double Bmargin=4096*std::numeric_limits<double>::epsilon()*(1+f2+d.n*mean*mean+std::abs(B));
      const double R=signed_poisson_remainder(d,lamhi,hmax),BU=B+Bmargin;
      if(poisson && std::isfinite(R) && BU+R*hmax<0){record_tail();continue;}
      const double lam=std::max(lamlo,std::min(lamhi,mean));
      const double pll=-d.n*lam+d.n*mean*std::log(lam)-factorial;
      const double upper=pll+(BU>=0?BU*hmax:BU*hmin)+R*hmax*hmax;
      if(std::isfinite(upper) && upper+joint_error(d,upper,std::abs(pll)+std::abs(BU*hmax)+R*hmax*hmax)<face_lower){record_tail();continue;}
    }
    const auto signed_bound=signed_nb_bound(d,q.rl,q.rh,pl,ph);
    if(std::isfinite(signed_bound.coefficient+signed_bound.remainder) &&
       signed_bound.coefficient+signed_bound.remainder*q.th<0) {
      record_tail();
      continue;
    }
    auto nb=signed_nb_likelihood_bound(d,q.rl,q.rh,pl,ph);
    // The delegated NB proof supplies the unique size maximum (or a
    // monotone Poisson profile). Optimize p analytically before bounding r.
    double profile_r=nb_size;
    if(poisson || q.rh<nb_size*(1-1e-6))profile_r=q.rh;
    else if(q.rl>nb_size*(1+1e-6))profile_r=q.rl;
    const auto profile=joint_nb_profile_bound(d,profile_r);
    const double optlo=q.rl/(q.rl+mean),opthi=q.rh/(q.rh+mean);
    const double distance=std::max(0.,std::max(pl-opthi,optlo-ph));
    const double profile_upper=profile.value+profile.margin-2*d.n*(q.rl+mean)*distance*distance;
    if(profile_upper<nb.value+nb.margin)nb={profile_upper,0};
    const double remainder=pair_nb_remainder(d,q.rh,pl,ql)*q.th;
    if(std::isfinite(nb.value+nb.margin+remainder) && nb.value+nb.margin+remainder<face_lower){record_tail();continue;}
    if(q.tl>0) {
      JointBox box{{q.rl,pl/q.th,ql/q.th},{q.rh,ph/q.tl,qh/q.tl}};
      if(!std::isfinite(box.hi[0]+box.hi[1]+box.hi[2]))return out;
      const auto bound=alpha_interval_bound(d,box.lo[1],box.hi[1],{box.lo[0],box.hi[0],box.lo[2],box.hi[2]});
      if(std::isfinite(bound.value+bound.margin) && bound.value+bound.margin<face_lower)continue;
      bool excluded=false;
      for(int j=0;j<3;++j) {
        std::vector<double> l{box.lo[0],box.hi[1],box.lo[2]},h{box.hi[0],box.lo[1],box.hi[2]};
        if(j==1)std::swap(l,h);
        const auto a=scalar_score(d,l,j),b=scalar_score(d,h,j);
        if(a.value < -a.margin || b.value>b.margin){excluded=true;break;}
      }
      if(excluded)continue;
      if(joint_reduce(d,box.lo,box.hi))continue;
      q.rl=std::max(q.rl,box.lo[0]);q.rh=std::min(q.rh,box.hi[0]);
      q.vl=std::max(q.vl,std::log(box.lo[1])-std::log(box.hi[2]));
      q.vh=std::min(q.vh,std::log(box.hi[1])-std::log(box.lo[2]));
      q.tl=std::max(q.tl,1/(box.hi[1]+box.hi[2]));
      q.th=std::min(q.th,1/(box.lo[1]+box.lo[2]));
      if(q.rl>q.rh || q.vl>q.vh || q.tl>q.th)continue;
      std::array<double,3> center{},radius{};
      for(int j=0;j<3;++j) {
        center[j]=std::sqrt(box.lo[j])*std::sqrt(box.hi[j]);
        radius[j]=std::max(center[j]-box.lo[j],box.hi[j]-center[j]);
      }
      const std::vector<double> point{center[0],center[1],center[2]};
      const double ll=likelihood(d,point);
      const auto point_bound=alpha_likelihood_bound(d,center[1],{center[0],center[0],center[2],center[2]});
      double taylor=ll+point_bound.margin;
      const auto hessian=joint_hessian_bounds(d,box.lo,box.hi);
      for(int i=0;i<3;++i) {
        const auto score=scalar_score(d,point,i);
        taylor+=d.n*(std::abs(score.value)+score.margin)*radius[i];
        for(int j=0;j<3;++j)taylor+=d.n*.5*std::max(std::abs(hessian[0].v[i][j]),
          std::abs(hessian[1].v[i][j]))*radius[i]*radius[j];
      }
      if(std::isfinite(taylor) && taylor+joint_error(d,taylor,std::abs(taylor))<face_lower)continue;
    }
    // Resolve r and logit(p) before shrinking the concentration near t=0.
    // Elsewhere balance their widths with the relative concentration width.
    const double wr=std::log(q.rh/q.rl),wv=q.vh-q.vl;
    const bool tail_resolvable=std::isfinite(signed_bound.coefficient+signed_bound.remainder) &&
      (signed_bound.coefficient<0 || nb.value+nb.margin<face_lower);
    const double wt=q.tl>0?std::log(q.th/q.tl):
      (tail_resolvable?std::max(wr,wv)+1:0);
    SignedFaceBox other=q;
    if(wr>=wv && wr>=wt) {
      const double mid=std::sqrt(q.rl)*std::sqrt(q.rh);
      if(!(q.rl<mid && mid<q.rh))return out;
      other.rl=mid;q.rh=mid;
    } else if(wv>=wt) {
      const double mid=q.vl+(q.vh-q.vl)/2;
      if(!(q.vl<mid && mid<q.vh))return out;
      other.vl=mid;q.vh=mid;
    } else {
      const double mid=q.tl+(q.th-q.tl)/2;
      if(!(q.tl<mid && mid<q.th))return out;
      other.tl=mid;q.th=mid;
    }
    pending.push_back(other);pending.push_back(q);
  }
  out.proved=true;return out;
}
} // namespace bnbinom_detail
} // namespace cppunivariate
