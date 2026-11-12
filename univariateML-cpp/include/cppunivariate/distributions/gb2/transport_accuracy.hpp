#pragma once
namespace cppunivariate {
inline bool gb2_transport_accuracy(span x,double shift,Gb2Result& r,
    const std::array<bool,4>& free,double tol,int maxit,bool check_power=false) {
  tol=std::min(tol,1e-10);
  bool exact=true;
  for(double v:x){const double y=v-shift,b=y-v,e=(v-(y-b))+(-shift-b);exact=exact&&e==0;}
  if(exact&&!check_power)return true;
  if(r.shape3==1||r.shape1==1) {
    double k=r.shape3==1?r.shape1:r.shape3,b=r.shape2,s=r.scale;
    const bool inverse=r.shape1==1&&r.shape3!=1;
    if(!burr_local_accuracy(x,k,b,s,{{inverse?free[2]:free[0],free[1],free[3]}},tol,maxit,inverse,shift))return false;
    const double initial=inverse?r.shape3:r.shape1;
    if(std::abs(k/initial-1)>tol*.125||std::abs(b/r.shape2-1)>tol*.125||std::abs(s/r.scale-1)>tol*.125) {
      if(inverse)r.shape3=k;else r.shape1=k;r.shape2=b;r.scale=s;
      r.logLik=burr_local_loglik(x,k,b,s,inverse,shift);
    }
    return std::isfinite(r.logLik);
  }
  if(!free[1]&&free[3]) {
    double p=r.shape3,q=r.shape1,s=r.scale,ll=NAN;
    if(!burr_power_one_detail::beta_gamma_fit(x,p,q,s,{{free[2],free[0],true}},tol,maxit,ll,r.shape2,shift))return false;
    if(std::max({std::abs(p/r.shape3-1),std::abs(q/r.shape1-1),std::abs(s/r.scale-1)})>tol*.125) {
      r.shape3=p;r.shape1=q;r.scale=s;r.logLik=ll;
    }
    return true;
  }
  const double unit=r.scale;
  const std::array<double,4> base{{r.shape1,r.shape2,r.shape3,r.scale}};
  const bool reflect=r.shape3>r.shape1;
  const double large=reflect?r.shape3:r.shape1,small=reflect?r.shape1:r.shape3;
  const bool rare=!free[1]&&free[3]&&large>32&&small<large*.1;
  std::array<int,4> index{};int dim=0;
  std::array<double,4> v{};
  for(int i=0;i<4;++i)if(free[i]){index[i]=dim;v[dim++]=i==3?(rare?-std::log(large):0):std::log(base[i]);}
  const auto objective=[&](const std::array<local_accuracy::Jet,4>& z) {
    const auto q=free[0]?local_accuracy::exp(z[index[0]]):local_accuracy::Jet(base[0]);
    const auto a=free[1]?local_accuracy::exp(z[index[1]]):local_accuracy::Jet(base[1]);
    const auto p=free[2]?local_accuracy::exp(z[index[2]]):local_accuracy::Jet(base[2]);
    const auto scale=free[3]?z[index[3]]:local_accuracy::Jet(0);
    const auto P=reflect?q:p,Q=reflect?p:q;
    local_accuracy::JetSum f(x.size()*((rare?gb2_accuracy::rare_normalizer(P,Q):local_accuracy::logbeta(p,q))-local_accuracy::log(a)));
    for(double value:x){const double l=static_cast<double>(burr_excess_log_ratio(value,shift,unit));
      const auto constant=local_accuracy::Jet(static_cast<double>(burr_excess_log_ratio(value,shift,1)));
      if(rare) {
        const auto h=(reflect?-1:1)*a*l-scale;
        if(h.v<std::log(std::numeric_limits<double>::max())-1) {
          const auto w=local_accuracy::exp(h),ratio=w/Q;
          if(ratio.v<.125)f.add(constant-P*h+w+P*local_accuracy::log1p(ratio)+Q*local_accuracy::log1pmx(ratio));
          else f.add(constant-P*h+(P+Q)*local_accuracy::log1p(ratio));
        } else {const auto t=h-local_accuracy::log(Q);f.add(constant-P*local_accuracy::log(Q)+P*local_accuracy::softplus(-t)+Q*local_accuracy::softplus(t));}
      } else {const auto t=a*(l-scale);f.add(constant+p*local_accuracy::softplus(-t)+q*local_accuracy::softplus(t));}
    }
    return f.value();
  };
  const auto error=[&](const std::array<double,4>&,const std::array<double,4>& step){
    double e=0;for(int i=0;i<3;++i)if(free[i])e=std::max(e,std::abs(step[index[i]]));
    if(free[3]){const int j=reflect?2:0;const double d=rare?(step[index[3]]+(free[j]?step[index[j]]:0))/base[1]:step[index[3]];e=std::max(e,std::abs(d));}
    return e;
  };
  if(dim&&!local_accuracy::polish(v,dim,tol,maxit,objective,error))return false;
  std::array<double,4> solved=base;double change=0;
  for(int i=0;i<3;++i)if(free[i])solved[i]=std::exp(v[index[i]]);
  const double coordinate=free[3]?(rare?(reflect?-1:1)*(v[index[3]]+std::log(reflect?solved[2]:solved[0]))/base[1]:v[index[3]]):0;
  if(free[3])solved[3]=unit*std::exp(coordinate);
  for(int i=0;i<4;++i)if(free[i])change=std::max(change,std::abs(solved[i]/base[i]-1));
  if(free[3]&&!accuracy_detail::scale_projection(solved[3],unit,coordinate,tol))return false;
  if(change>tol*.125){r.shape1=solved[0];r.shape2=solved[1];r.shape3=solved[2];r.scale=solved[3];}
  const std::array<double,4> returned{{r.shape1,r.shape2,r.shape3,r.scale}};
  for(int i=0;i<3;++i)if(free[i])v[index[i]]=std::log(returned[i]);
  if(free[3])v[index[3]]=rare?(reflect?-1:1)*base[1]*std::log(r.scale/unit)-std::log(reflect?r.shape3:r.shape1):std::log(r.scale/unit);
  r.logLik=-objective(local_accuracy::variables(v)).v;
  return std::isfinite(r.logLik);
}
}
