#pragma once
// Included inside cppunivariate, after SgedResult. At nu=1 the density is
// exp(-|x-c|/side)/(a+b), with mean=c+b-a and sd^2=a^2+b^2.
// Internally the sides are divided by the GED standardization lambda, so
// held subunit powers do not overflow gamma moments. Their cusp dominates
// every smooth mode perturbation; only the tangent coordinate needs solving.
// A density corner is an exact data knot, not a smooth stationary root.
namespace sged_local_accuracy {
struct Sides {local_accuracy::Jet a,b,mean,sd,xi;};
inline Sides sides(const local_accuracy::Jet& q,const local_accuracy::Jet& c,
                   const local_accuracy::Jet& d,double mean,double sd,double xi){
 const bool hm=std::isfinite(mean),hs=std::isfinite(sd),hx=std::isfinite(xi);
 const auto K=local_accuracy::exp(local_accuracy::lgamma(2/d)-.5*(local_accuracy::lgamma(1/d)+local_accuracy::lgamma(3/d)));
 const local_accuracy::Jet G=1;
 local_accuracy::Jet a,b;
 if(hm&&hs&&!hx){
  const auto delta=K.v==0&&(mean-c).v==0?local_accuracy::Jet(0):(mean-c)/K;
  const auto total=local_accuracy::sqrt((4*sd*sd+(4*K*K-3*G)*delta*delta)/G);
  const auto radius=local_accuracy::sqrt(G-K*K);
  const auto product=(sd-radius*delta)*(sd+radius*delta);
  if(delta.v>=0){b=(total+delta)/2;a=2*product/(G*(total+delta));}
  else{a=(total-delta)/2;b=2*product/(G*(total-delta));}
 }else if(hm&&!hx){
  const auto delta=K.v==0&&(mean-c).v==0?local_accuracy::Jet(0):(mean-c)/K;
  if(delta.v>=0){a=local_accuracy::exp(q);b=a+delta;}
  else{b=local_accuracy::exp(q);a=b-delta;}
 }
 else {
  const auto ratio=hx?local_accuracy::Jet(xi*xi):local_accuracy::exp(2*q);
  if(hm){a=(mean-c)/(K*(ratio-1));b=ratio*a;}
  else if(hs){a=sd/local_accuracy::sqrt(G*(1+ratio*ratio-ratio)-K*K*(ratio-1)*(ratio-1));b=ratio*a;}
  else{a=local_accuracy::exp(q);b=ratio*a;}
 }
 const auto delta=b-a;
 return {a,b,c+K*delta,local_accuracy::sqrt(G*(a*a+b*b-a*b)-K*K*delta*delta),local_accuracy::sqrt(b/a)};
}
inline local_accuracy::Jet cost(const std::vector<double>& x,const Sides& p,
                                const local_accuracy::Jet& c,const local_accuracy::Jet& d,
                                bool right=false,bool shape_derivative=false){
 if(!(p.a.v>0&&p.b.v>0))return local_accuracy::Jet(NAN);
 const auto loglambda=.5*(local_accuracy::lgamma(1/d)-local_accuracy::lgamma(3/d));
 local_accuracy::JetSum sum(x.size()*(local_accuracy::log(p.a+p.b)+loglambda+local_accuracy::lgamma(1/d)-local_accuracy::log(d)));
 for(double v:x){
  if(d.v==1&&!shape_derivative){
   if(v<c.v||(v==c.v&&right))sum.add((c-v)/(local_accuracy::exp(loglambda)*p.a));
   else sum.add((v-c)/(local_accuracy::exp(loglambda)*p.b));
  }else{
   if(v==c.v)continue; // Fixed data knot along tangent and shape directions.
   const auto distance=v<c.v?c-v:v-c,side=v<c.v?p.a:p.b;
   sum.add(local_accuracy::exp(d*(local_accuracy::log(distance)-local_accuracy::log(side)-loglambda)));
  }
 }
 return sum.value();
}
// Rescue only contacts nominated by the existing search. Positive tangent
// information and a contracting Newton step control the free public
// coefficients; strict left/right signs certify the remaining corner.
inline bool corner(SgedResult& result,span x,double mean,double sd,double xi,
                   bool held_shape,double tol,int maxit){
 tol=std::min(tol,1e-10);
 const bool hm=std::isfinite(mean),hs=std::isfinite(sd),hx=std::isfinite(xi);
 if(!(result.nu>0&&result.nu<=1)||(hm&&hs&&hx)||(!hm&&!hs&&!hx))return false;
 const double origin=result.mu,unit=result.sigma;
 if(!(unit>0&&std::isfinite(unit)))return false;
 const double m=hm?(mean-origin)/unit:NAN,s=hs?sd/unit:NAN;
 const double ratio=result.xi*result.xi;
 const double power=result.nu;
 const double moment=std::exp(special::lgamma(2/power)-.5*(special::lgamma(1/power)+special::lgamma(3/power)));
 const double a0=1/std::sqrt(1+ratio*ratio-ratio-moment*moment*(ratio-1)*(ratio-1)),b0=ratio*a0,mode=moment*(a0-b0);
 if(!(a0>0&&b0>0&&std::isfinite(mode)))return false;
 std::vector<double> y;for(double v:x)y.push_back((v-origin)/unit);
 const bool tangent=(hm&&!hs&&!hx)||(!hm&&hs&&!hx)||(!hm&&!hs&&hx);
 const double start=(hm&&!hs&&!hx)||(!hm&&!hs&&hx)?std::log(a0):std::log(result.xi);
 const double projection=8*std::numeric_limits<double>::epsilon()*std::abs(origin/unit);
 SgedResult best=result;double best_value=INFINITY;bool found=false;
 std::int64_t work=0;
 for(std::size_t knot=0;knot<y.size()&&work<certificate_budget(maxit);++knot){
  const double c=y[knot];if(std::abs(c-mode)>std::max(projection,1e-5*std::max(1.,std::abs(mode))))continue;
  bool duplicate=false;for(std::size_t j=0;j<knot;++j)if(y[j]==c){duplicate=true;break;}if(duplicate)continue;
  ++work;const double initial=hm&&!hs&&!hx&&m-c<0?std::log(b0):start;
  std::array<double,4> q{{initial,0,0,0}};
  const auto objective=[&](const std::array<local_accuracy::Jet,4>& z){return cost(y,sides(z[0],c,power,m,s,xi),c,power);};
  const auto error=[&](const std::array<double,4>& z,const std::array<double,4>& step){
   const auto p=sides(local_accuracy::Jet::variable(z[0],0),c,power,m,s,xi);
   double e=0;
   if(!hm)e=std::max(e,std::abs(p.mean.g[0]*step[0])/std::max(std::abs(origin/unit+p.mean.v),p.sd.v));
   if(!hs)e=std::max(e,std::abs(p.sd.g[0]*step[0])/p.sd.v);
   if(!hx)e=std::max(e,std::abs(p.xi.g[0]*step[0])/p.xi.v);
   return e;
  };
  if(tangent&&!local_accuracy::polish(q,1,tol,maxit,objective,error))continue;
  const auto p=sides(q[0],c,power,m,s,xi);
  if(!(p.a.v>0&&p.b.v>0&&p.sd.v>0))continue;
  if(power==1){
   const auto mode_jet=local_accuracy::Jet::variable(c,1);
   const auto varied=sides(q[0],mode_jet,power,m,s,xi);
   const auto left=cost(y,varied,mode_jet,power,false),right=cost(y,varied,mode_jet,power,true);
   const double lambda=std::sqrt(.5);
   const double margin=128*std::numeric_limits<double>::epsilon()*y.size()*(1+1/(lambda*p.a.v)+1/(lambda*p.b.v));
   if(!(left.g[1]<-margin&&right.g[1]>margin))continue;
  } // For 0<nu<1, the positive |mode-knot|^nu cusp proves both signs.

  if(!held_shape){
   const auto shape=local_accuracy::Jet::variable(1,2);
   const auto jet=cost(y,sides(q[0],c,shape,m,s,xi),c,shape,false,true);
   const double pad=128*std::numeric_limits<double>::epsilon()*(1+jet.mass);
   if(!(std::isfinite(jet.g[2])&&jet.g[2]>pad))continue;
  }
  SgedResult candidate=result;
  candidate.mu=hm?mean:origin+unit*p.mean.v;
  candidate.sigma=hs?sd:unit*p.sd.v;
  candidate.xi=hx?xi:p.xi.v;
  if((!hm&&!local_accuracy::represented(candidate.mu,std::max(std::abs(candidate.mu),candidate.sigma),tol))||
     (!hs&&!local_accuracy::represented(candidate.sigma,candidate.sigma,tol))||
     (!hx&&!local_accuracy::represented(candidate.xi,candidate.xi,tol)))continue;
  const std::array<double,4> public_point{{(candidate.mu-origin)/unit,std::log(candidate.sigma/unit),0,std::log(candidate.xi)}};
  const double value=local_accuracy::skew_objective(y,local_accuracy::variables(public_point),false,power).v;
  if(std::isfinite(value)&&value<best_value){candidate.logLik=-value-x.size()*std::log(unit);best=candidate;best_value=value;found=true;}
 }
 if(found)result=best;
 return found;
}
} // namespace sged_local_accuracy
