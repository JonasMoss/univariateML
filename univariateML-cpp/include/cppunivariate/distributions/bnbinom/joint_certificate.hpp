// Global finite-superlevel cover; included after the scalar and pair certificates.
#pragma once
namespace cppunivariate {
namespace bnbinom_detail {
struct JointMatrix {std::array<std::array<double,3>,3> v{};};
inline std::array<JointMatrix,2> joint_hessian_bounds(const cppunivariate::bnbinom_detail::Data &d,const std::array<double,3>&l,const std::array<double,3>&u) {
  JointMatrix lo,hi;
  const double arl=cppunivariate::special::trigamma(u[1]+u[0]),arh=cppunivariate::special::trigamma(l[1]+l[0]);
  const double abl=cppunivariate::special::trigamma(u[1]+u[2]),abh=cppunivariate::special::trigamma(l[1]+l[2]);
  lo.v[0][0]=arl-cppunivariate::special::trigamma(l[0]);hi.v[0][0]=arh-cppunivariate::special::trigamma(u[0]);
  lo.v[1][1]=arl+abl-cppunivariate::special::trigamma(l[1]);hi.v[1][1]=arh+abh-cppunivariate::special::trigamma(u[1]);
  lo.v[2][2]=abl-cppunivariate::special::trigamma(l[2]);hi.v[2][2]=abh-cppunivariate::special::trigamma(u[2]);
  lo.v[0][1]=arl;hi.v[0][1]=arh;lo.v[1][2]=abl;hi.v[1][2]=abh;
  double mag=0;for(int j=0;j<3;++j)mag+=8*cppunivariate::special::trigamma(l[j]);
  for(std::size_t k=0;k<d.x.size();++k) {
    const double w=d.count[k]/d.n,x=d.x[k];
    const double tl=cppunivariate::special::trigamma(u[0]+u[1]+u[2]+x),th=cppunivariate::special::trigamma(l[0]+l[1]+l[2]+x);
    for(int i=0;i<3;++i)for(int j=i;j<3;++j){lo.v[i][j]-=w*th;hi.v[i][j]-=w*tl;}
    lo.v[0][0]+=w*cppunivariate::special::trigamma(u[0]+x);hi.v[0][0]+=w*cppunivariate::special::trigamma(l[0]+x);
    lo.v[2][2]+=w*cppunivariate::special::trigamma(u[2]+x);hi.v[2][2]+=w*cppunivariate::special::trigamma(l[2]+x);
    mag+=w*(8*th+cppunivariate::special::trigamma(l[0]+x)+cppunivariate::special::trigamma(l[2]+x));
  }
  const double e=1024*std::numeric_limits<double>::epsilon()*mag;
  for(int i=0;i<3;++i)for(int j=i;j<3;++j){lo.v[i][j]-=e;hi.v[i][j]+=e;lo.v[j][i]=lo.v[i][j];hi.v[j][i]=hi.v[i][j];}
  return {lo,hi};
}
inline JointMatrix joint_inverse(const JointMatrix &h) {
  JointMatrix m;const auto a=h.v;
  const double det=a[0][0]*(a[1][1]*a[2][2]-a[1][2]*a[2][1])-a[0][1]*(a[1][0]*a[2][2]-a[1][2]*a[2][0])+a[0][2]*(a[1][0]*a[2][1]-a[1][1]*a[2][0]);
  for(int i=0;i<3;++i)for(int j=0;j<3;++j){const int u=(j+1)%3,v=(j+2)%3,s=(i+1)%3,t=(i+2)%3;m.v[i][j]=(a[u][s]*a[v][t]-a[u][t]*a[v][s])/det;}
  return m;
}
inline bool joint_reduce(const cppunivariate::bnbinom_detail::Data &d,std::array<double,3>&l,std::array<double,3>&u) {
  std::array<double,3> center{};for(int j=0;j<3;++j)center[j]=std::sqrt(l[j])*std::sqrt(u[j]);
  const auto bounds=joint_hessian_bounds(d,l,u),point=joint_hessian_bounds(d,center,center);
  JointMatrix h;for(int i=0;i<3;++i)for(int j=0;j<3;++j)h.v[i][j]=(point[0].v[i][j]+point[1].v[i][j])/2;
  const auto m=joint_inverse(h);
  std::array<cppunivariate::bnbinom_detail::ScalarScore,3> g{};
  for(int j=0;j<3;++j)g[j]=cppunivariate::bnbinom_detail::scalar_score(d,{center[0],center[1],center[2]},j);
  for(int i=0;i<3;++i) {
    double nl=center[i],nu=center[i],magnitude=std::abs(center[i]);
    for(int k=0;k<3;++k) {
      const double a=m.v[i][k]*(g[k].value-g[k].margin),b=m.v[i][k]*(g[k].value+g[k].margin);
      nl-=std::max(a,b);nu-=std::min(a,b);magnitude+=std::max(std::abs(a),std::abs(b));
    }
    for(int j=0;j<3;++j) {
      double el=i==j?1:0,eu=el,emag=1;
      for(int k=0;k<3;++k){const double a=m.v[i][k]*bounds[0].v[k][j],b=m.v[i][k]*bounds[1].v[k][j];el-=std::max(a,b);eu-=std::min(a,b);emag+=std::max(std::abs(a),std::abs(b));}
      const double error=64*std::numeric_limits<double>::epsilon()*emag;el-=error;eu+=error;
      const std::array<double,4> products{{el*(l[j]-center[j]),el*(u[j]-center[j]),eu*(l[j]-center[j]),eu*(u[j]-center[j])}};
      nl+=*std::min_element(products.begin(),products.end());nu+=*std::max_element(products.begin(),products.end());
      for(double v:products)magnitude+=std::abs(v);
    }
    const double margin=4096*std::numeric_limits<double>::epsilon()*(1+magnitude);
    if(std::isfinite(nl+nu+margin)) {
      if(nu+margin<l[i] || nl-margin>u[i])return true;
      l[i]=std::max(l[i],nl-margin);u[i]=std::min(u[i],nu+margin);
    }
  }
  return false;
}

struct JointBox { std::array<double,3> lo,hi; };
struct JointProof {
  bool proved=false,budget_exhausted=false;
  std::array<double,3> coef{{NAN,NAN,NAN}};
  // An attained finite witness survives even when the global cover fails.
  // This is not an estimate or a coefficient enclosure.
  std::array<double,3> witness_coef{{NAN,NAN,NAN}};
  double loglik_lower=NAN,loglik_upper=NAN;
  long long evaluations=0;
  int iterations=0;
};
inline double joint_error(const Data &d,double value,double magnitude) {
  return 1e-10*d.n*(1+std::abs(value/d.n))+
    1024*std::numeric_limits<double>::epsilon()*magnitude;
}
// The NB certificate encloses its size root within relative 5e-7.
// Re-evaluate the mean-profile likelihood stably and include the root-width
// error, rather than treating the returned finite coefficient as exact.
inline AlphaLikelihood joint_nb_profile_bound(const Data &d,double size) {
  CPPUNIV_EXTENDED_REAL total=0,correction=0;
  for(std::size_t i=0;i<d.x.size();++i) {
    const CPPUNIV_EXTENDED_REAL y=static_cast<CPPUNIV_EXTENDED_REAL>(d.count[i])*d.x[i]-correction;
    const CPPUNIV_EXTENDED_REAL t=total+y;correction=(t-total)-y;total=t;
  }
  const CPPUNIV_EXTENDED_REAL mean=total/d.n,r=size;
  CPPUNIV_EXTENDED_REAL lp=-std::log1p(mean/r),lq=-std::log1p(r/mean);
  if(!std::isfinite(lp))lp=std::log(r)-std::log(mean)-std::log1p(r/mean);
  if(!std::isfinite(lq))lq=std::log(mean)-std::log(r)-std::log1p(mean/r);
  CPPUNIV_EXTENDED_REAL value=(r*lp)*d.n,mag=std::abs(value);
  for(std::size_t i=0;i<d.x.size();++i) {
    const CPPUNIV_EXTENDED_REAL u=increment(r,d.x[i]),v=special::lgamma(d.x[i]+1.),w=d.x[i]*lq;
    value+=d.count[i]*(u-v+w);mag+=d.count[i]*(std::abs(u)+std::abs(v)+std::abs(w));
  }
  const double ll=static_cast<double>(value),rho=1e-6;
  // Profile curvature: |ell''(r)|/n <= 2*mean/r^2. Taylor about the
  // certified root bounds its height error by n*mean*(rho/(1-rho))^2.
  const double root_error=static_cast<double>(d.n*mean)*std::pow(rho/(1-rho),2);
  return {ll,joint_error(d,ll,static_cast<double>(mag))+root_error};
}
inline JointProof joint_cover(const Data &d,std::vector<double> proposal,
    double nb_face,double po_face,double tol,int maxit,double nb_size=NAN) {
  JointProof out;maxit=static_cast<int>(solver_budget(maxit));
  if(maxit<1 || !(tol>0) || !std::isfinite(tol))return out;
  const long long cap=certificate_budget(maxit);
  const auto spend=[&]() {if(++out.evaluations<=cap)return true;out.budget_exhausted=true;return false;};
  const auto step=[&]() {if(out.iterations<std::numeric_limits<int>::max())++out.iterations;};
  const auto point_error=[&](const std::array<double,3> &p) {
    return alpha_likelihood_bound(d,p[1],{p[0],p[0],p[2],p[2]}).margin;
  };
  std::array<double,3> p{{proposal[0],proposal[1],proposal[2]}};
  if(p[0]>p[2])std::swap(p[0],p[2]);
  const double initial_ll=likelihood(d,{p[0],p[1],p[2]});
  double L=initial_ll-point_error(p);
  out.witness_coef=p;out.loglik_lower=L;
  // Newton proposes a center only. Existence and coefficient accuracy come
  // from compactness and the completed cover, independently of convergence.
  for(int it=0;it<maxit;++it) {
    step();const auto hp=joint_hessian_bounds(d,p,p);JointMatrix h;
    for(int i=0;i<3;++i)for(int j=0;j<3;++j)h.v[i][j]=(hp[0].v[i][j]+hp[1].v[i][j])/2;
    const auto inv=joint_inverse(h);std::array<double,3> change{};
    for(int j=0;j<3;++j) {
      const auto g=scalar_score(d,{p[0],p[1],p[2]},j);
      for(int i=0;i<3;++i)change[i]+=inv.v[i][j]*g.value;
    }
    double relative=0;
    for(int i=0;i<3;++i) {
      if(!std::isfinite(change[i]) || std::abs(change[i])>p[i]*.25)return out;
      relative=std::max(relative,std::abs(change[i])/p[i]);
    }
    if(relative<std::min(tol,1e-8)*.01)break;
    for(int i=0;i<3;++i)p[i]-=change[i];
    if(p[0]>p[2])std::swap(p[0],p[2]);
  }
  const double ll=likelihood(d,{p[0],p[1],p[2]});
  const double refined_lower=ll-point_error(p);
  L=std::max(L,refined_lower);
  if(refined_lower>=L)out.witness_coef=p;
  out.loglik_lower=L;
  double nb_error=joint_error(d,nb_face,std::abs(nb_face))+1e-9*d.n*(1+std::abs(nb_face/d.n));
  if(std::isfinite(nb_size) && nb_size>0) {
    if(!spend())return out;
    const auto profile=joint_nb_profile_bound(d,nb_size);
    if(!std::isfinite(profile.value+profile.margin))return out;
    nb_face=std::max(nb_face,profile.value);nb_error+=profile.margin;
  }
  const double po_error=joint_error(d,po_face,std::abs(po_face))+1e-9*d.n*(1+std::abs(po_face/d.n));
  if(!std::isfinite(L) || !(L>nb_face+nb_error && L>po_face+po_error))return out;
  double mean=0,moment=0,positive=0;
  for(std::size_t i=0;i<d.x.size();++i) {
    mean+=d.count[i]*d.x[i]/d.n;
    moment+=d.count[i]*d.x[i]*(d.x[i]-1.);
    if(d.x[i]>0)positive+=d.count[i];
  }
  double lower=1;
  while(true) {
    if(!spend())return out;
    double value=0,mag=0;
    for(std::size_t i=0;i<d.x.size();++i)if(d.x[i]>0) {
      const double u=static_cast<double>(increment(lower,d.x[i])),v=special::lgamma(d.x[i]+1.);
      value+=d.count[i]*std::min(0.,u-v);mag+=d.count[i]*(std::abs(u)+std::abs(v));
    }
    if(value+joint_error(d,value,mag)<L)break;
    lower*=.5;if(!(lower>0))return out;
  }
  double M=std::max(1.,2*mean);
  while(true) {
    if(!spend())return out;
    double value=-d.n*M,mag=d.n*M;
    for(std::size_t i=0;i<d.x.size();++i) {
      const double u=static_cast<double>(increment(M,d.x[i])),v=special::lgamma(d.x[i]+1.);
      value+=d.count[i]*(u-v);mag+=d.count[i]*(std::abs(u)+std::abs(v));
    }
    if(value+joint_error(d,value,mag)<L)break;
    M*=2;if(!std::isfinite(M))return out;
  }
  const double A=4*d.n*M*M+.5*moment;
  const double U=std::max(4*M,4*A/(L-po_face-po_error));
  if(!(U>lower) || !std::isfinite(U))return out;
  const double delta=std::min(lower,1.);
  double logp=-std::log(2.);
  while(true) {
    if(!spend())return out;
    double value=0,mag=0;
    for(std::size_t i=0;i<d.x.size();++i) {
      const double u=static_cast<double>(increment(delta+1,d.x[i])),v=special::lgamma(d.x[i]+1.);
      const double term=std::min(0.,u-v+delta*logp);
      value+=d.count[i]*term;mag+=d.count[i]*(std::abs(u)+std::abs(v)+std::abs(delta*logp));
    }
    if(value+joint_error(d,value,mag)<L)break;
    logp-=std::log(2.);if(!std::isfinite(logp))return out;
  }
  const double pmin=std::exp(logp),qmin=std::exp(L/positive-std::log(std::max(U,1.))-1);
  if(!(pmin>0 && qmin>0))return out;
  const double S=2*pair_nb_remainder(d,U,pmin,qmin)/(L-nb_face-nb_error);
  const double alower=pmin*lower,radius=std::min(tol,1e-8)*.25;
  if(!(S>lower && alower>0) || !std::isfinite(S))return out;
  std::array<double,3> target_lo{},target_hi{};
  for(int j=0;j<3;++j) {
    target_lo[j]=p[j]*(1-radius);target_hi[j]=p[j]*(1+radius);
    if(!(target_lo[j]<p[j] && p[j]<target_hi[j]))return out;
  }
  std::vector<JointBox> pending{JointBox{{lower,alower,lower},{U,S,S}}};
  double global_upper=-INFINITY;bool retained=false;
  while(!pending.empty()) {
    if(!spend())return out;
    JointBox q=pending.back();pending.pop_back();
    if(q.lo[0]>q.hi[2])continue;
    if(!std::isfinite(q.hi[1]+q.lo[2]) || !std::isfinite(q.hi[2]+q.lo[1]) ||
       !std::isfinite(q.lo[1]+q.lo[2]) || !std::isfinite(q.hi[1]+q.lo[0]+q.lo[2]))return out;
    if(q.hi[1]/(q.hi[1]+q.lo[2])<pmin || q.hi[2]/(q.hi[2]+q.lo[1])<qmin || q.lo[1]+q.lo[2]>=S)continue;
    const double local_p=q.lo[1]/(q.lo[1]+q.hi[2]),local_q=q.lo[2]/(q.hi[1]+q.lo[2]);
    const double remainder=pair_nb_remainder(d,q.hi[0],local_p,local_q)/(q.lo[1]+q.lo[2]);
    if(nb_face+nb_error+remainder<L)continue;
    const double min_m=q.lo[0]*(q.lo[2]/(q.hi[1]+q.lo[0]+q.lo[2]));
    if(min_m>=M)continue;
    // The unique alpha mode is increasing in both other shapes. These
    // signed brackets enclose it uniformly, including on the diagonal.
    for(int it=0;it<maxit;++it) {
      step();const double mid=std::sqrt(q.lo[1])*std::sqrt(q.hi[1]);
      if(!(q.lo[1]<mid && mid<q.hi[1]))break;
      const auto l=scalar_score(d,{q.lo[0],mid,q.lo[2]},1),h=scalar_score(d,{q.hi[0],mid,q.hi[2]},1);
      if(l.value>l.margin)q.lo[1]=mid;
      else if(h.value < -h.margin)q.hi[1]=mid;
      else break;
    }
    bool excluded=false;
    for(int j=0;j<3;++j) {
      std::vector<double> l{q.lo[0],q.hi[1],q.lo[2]},h{q.hi[0],q.lo[1],q.hi[2]};
      if(j==1)std::swap(l,h);
      const auto a=scalar_score(d,l,j),b=scalar_score(d,h,j);
      if(a.value < -a.margin || b.value>b.margin){excluded=true;break;}
    }
    if(excluded)continue;
    const auto bound=alpha_interval_bound(d,q.lo[1],q.hi[1],{q.lo[0],q.hi[0],q.lo[2],q.hi[2]});
    if(!std::isfinite(bound.value+bound.margin))return out;
    if(bound.value+bound.margin<L)continue;
    double width=0;int split=0;
    for(int j=0;j<3;++j) {
      const double w=std::log(q.hi[j])-std::log(q.lo[j]);
      if(w>width){width=w;split=j;}
    }
    if(width<.5) {
      if(joint_reduce(d,q.lo,q.hi))continue;
      width=0;
      for(int j=0;j<3;++j) {
        const double w=std::log(q.hi[j])-std::log(q.lo[j]);
        if(w>width){width=w;split=j;}
      }
    }
    bool inside=true;
    for(int j=0;j<3;++j)inside=inside && q.lo[j]>=target_lo[j] && q.hi[j]<=target_hi[j];
    if(inside) {retained=true;global_upper=std::max(global_upper,bound.value+bound.margin);continue;}
    const double mid=std::sqrt(q.lo[split])*std::sqrt(q.hi[split]);
    if(!(q.lo[split]<mid && mid<q.hi[split]))return out;
    JointBox other=q;other.lo[split]=mid;q.hi[split]=mid;
    pending.push_back(other);pending.push_back(q);
  }
  // A compact nonempty superlevel has a global maximizer. Every such point
  // is stationary and survives the exclusions. Closure puts it in the tiny
  // target box (or its swap), proving coefficient accuracy without assuming
  // uniqueness or accepting a Newton endpoint as an estimate.
  if(!retained || !(global_upper>=L))return out;
  out.coef=p;out.proved=true;out.loglik_lower=L;out.loglik_upper=std::min(0.,global_upper);
  return out;
}
} // namespace bnbinom_detail
} // namespace cppunivariate
