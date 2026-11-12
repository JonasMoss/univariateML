#pragma once
#include "../burr/pair_polish.hpp"
#include <vector>
namespace cppunivariate { namespace burr_power_one_detail {
inline Pair gamma_bernoulli(int j) {
  const double num[]={1,-1,1,-1,5,-691,7,-3617};
  const double den[]={6,30,42,30,66,2730,6,510};
  return Pair(num[j])/Pair(den[j]);
}
inline Pair log_one_plus(Pair x) {return value(x)<.5?log_increment(x):logarithm(Pair(1)+x);}
inline Pair gamma_remainder(Pair x) {
  const Pair inverse=Pair(1)/x,square=inverse*inverse;Pair power=inverse,sum;
  for(int j=0;j<8;++j){sum=sum+gamma_bernoulli(j)*power/Pair((2*j+2)*(2*j+1));power=power*square;}
  return sum;
}
inline Pair gamma_psi(Pair x,bool derivative=false) {
  const int n=std::max(0,static_cast<int>(std::ceil(32-value(x))));const Pair z=x+Pair(n),inverse=Pair(1)/z,square=inverse*inverse;
  Pair sum=derivative?inverse+square/Pair(2):logarithm(z)-inverse/Pair(2),power=derivative?inverse*square:square;
  for(int j=0;j<8;++j){sum=sum+(derivative?gamma_bernoulli(j)*power:-gamma_bernoulli(j)*power/Pair(2*j+2));power=power*square;}
  for(int j=0;j<n;++j){const Pair r=Pair(1)/(x+Pair(j));sum=sum+(derivative?r*r:-r);}
  return sum;
}
inline Pair gamma_increment(Pair q,Pair p,bool derivative=false) {
  const int n=std::max(0,static_cast<int>(std::ceil(32-value(q))));const Pair z=q+Pair(n),a=Pair(1)/z,b=Pair(1)/(z+p),a2=a*a,b2=b*b;
  Pair sum=derivative?-p*a*b+(b2-a2)/Pair(2):log_one_plus(p*a)+p*a*b/Pair(2);
  Pair ap=derivative?a*a2:a2,bp=derivative?b*b2:b2;
  for(int j=0;j<8;++j){sum=sum+(derivative?gamma_bernoulli(j)*(bp-ap):gamma_bernoulli(j)*(ap-bp)/Pair(2*j+2));ap=ap*a2;bp=bp*b2;}
  for(int j=0;j<n;++j){const Pair u=Pair(1)/(q+Pair(j)),v=Pair(1)/(q+p+Pair(j));sum=sum+(derivative?v*v-u*u:p*u*v);}
  return sum;
}
inline Pair gamma_log(Pair x) {
  const int n=std::max(0,static_cast<int>(std::ceil(32-value(x))));const Pair z=x+Pair(n);
  Pair c(.91893853320467274178032973640561764);c.lo=-3.8782941580672414496e-17;
  Pair sum=(z-Pair(.5))*logarithm(z)-z+c+gamma_remainder(z);
  for(int j=0;j<n;++j)sum=sum-logarithm(x+Pair(j));return sum;
}
inline Pair log_minus_increment(Pair x) {
  if(value(x)>.25)return log_one_plus(x)-x;
  Pair power=x*x,sum;for(int j=2;j<=64;++j){sum=sum+Pair(j%2?1:-1)*power/Pair(j);power=power*x;}return sum;
}
inline Pair gamma_correction(Pair q,Pair p) {
  const int n=std::max(0,static_cast<int>(std::ceil(32-value(q))));const Pair z=q+Pair(n),r=p/z;
  Pair sum=z*log_minus_increment(r)+(p-Pair(.5))*log_one_plus(r)+gamma_remainder(z+p)-gamma_remainder(z);
  if(n){sum=sum+p*log_ratio(z,q);for(int j=0;j<n;++j)sum=sum-log_one_plus(p/(q+Pair(j)));}
  return sum;
}
// Analytic Newton in (log p, log q, D), with log s=(D+log q)/a.
// The small Gamma departure is evaluated from raw binary64 observations in
// two-double arithmetic. Stirling remainders after B16 are bounded before
// inverse-information propagation, including for a held literal shape.
inline bool beta_gamma_fit(span x,double& alpha,double& beta,double& scale,
    const std::array<bool,3>& free,double tol,int maxit,double& loglik,double power,double shift,StopDetail& detail) {
  const bool reflect=alpha>beta;const double small=reflect?beta:alpha,large=reflect?alpha:beta;
  if(!(free[2]&&small>0&&large>0&&power>0))return false;
  const double sign_y=reflect?-1:1,unit=scale;
  const std::array<bool,3> active{{reflect?free[1]:free[0],reflect?free[0]:free[1],true}};
  std::array<Pair,3> state{{logarithm(Pair(small)),logarithm(Pair(large)),-logarithm(Pair(large))}};
  std::vector<Pair> raw;for(double v:x){const Pair excess=Pair(v)-Pair(shift);if(!(excess.hi>0))return false;raw.push_back(Pair(sign_y*power)*log_ratio(excess,Pair(unit)));}
  const auto objective=[&](const std::array<Pair,3>& z) {
    PairEvaluation f;const Pair p=active[0]?exponential(z[0]):Pair(small),q=active[1]?exponential(z[1]):Pair(large);
    if(!(value(p)>0&&value(q)>0&&std::isfinite(value(p)+value(q)))){f.valid=false;return f;}
    const Pair normal=gamma_correction(q,p)-gamma_log(p),inc=gamma_increment(q,p),di=gamma_increment(q,p,true),psi=gamma_psi(p),tri=gamma_psi(p,true),sumtri=gamma_psi(p+q,true),sumpsi=gamma_psi(p+q)-logarithm(q);
    const Pair gamma_score=q*inc-p;
    // Stieltjes first-omitted-term bounds, including differentiated
    // increments and log-coordinate chain factors, are covered by 4096/z^18.
    const double nq=std::max(32.,value(q)),np=std::max(32.,value(p));
    const double tail=4096*(std::pow(nq,-18)+std::pow(np,-18))*std::max(1.,value(p)*value(p));
    const double large_shape_tail=4096*std::pow(nq,-18)*std::max(1.,value(p)*value(p));
    for(const Pair l:raw){const Pair h=l-z[2],t=h-logarithm(q);
      const bool upper=sign(t)>0;const Pair e=exponential(upper?-t:t),den=Pair(1)+e;
      const Pair B=upper?Pair(1)/den:e/den,complement=upper?e/den:Pair(1)/den;
      const Pair G=(upper?t:Pair(0))+log_one_plus(e),r=upper?Pair(INFINITY):e;
      const Pair w=upper?Pair(0):q*r,M=q*B-p*complement;
      Pair C,term(1);if(value(r)<.125)for(int j=0;j<64;++j){C=C+Pair(j%2?-1:1)*Pair(j+1)*term/Pair(j+2);term=term*r;}
      const Pair departure=value(r)<.125?w*r*C:q*(G-B);
      const Pair g0=p*(sumpsi-psi+h-G),g1=gamma_score+p*B-departure;
      const Pair ll=normal+p*h-(p+q)*G;
      f.likelihood=f.likelihood+ll;f.likelihood_magnitude+=std::abs(value(normal))+std::abs(value(p*h))+std::abs(value((p+q)*G));
      f.score[0]=f.score[0]+g0;f.score[1]=f.score[1]+g1;f.score[2]=f.score[2]+M;
      const Pair H00=p*p*(tri-sumtri)-g0,H02=p*complement,H01=-p*q*sumtri+H02;
      const Pair H22=(p+q)*B*complement,H12=B*(p*complement-q*B);
      const Pair H11=-q*q*di-p-gamma_score+q*G-Pair(2)*q*B+H22;
      f.information[0][0]=f.information[0][0]+H00;f.information[0][1]=f.information[0][1]+H01;f.information[0][2]=f.information[0][2]+H02;
      f.information[1][1]=f.information[1][1]+H11;f.information[1][2]=f.information[1][2]+H12;f.information[2][2]=f.information[2][2]+H22;
      const double mag=1+value(p)+std::abs(value(q*B))+std::abs(value(q*G))+std::abs(value(q*q*di))+std::abs(value(p*h))+std::abs(value(p*p*tri));
      f.information_magnitude+=mag;f.score_magnitude[0]+=mag;
      f.score_magnitude[1]+=1+std::abs(value(q*inc))+value(p)+std::abs(value(p*B))+std::abs(value(departure));
      f.score_magnitude[2]+=1+std::abs(value(q*B))+std::abs(value(p*complement));
      // Only the small-shape score contains psi(p). The rate score is
      // elementary, and the large-shape score uses the increment at q.
      f.score_tail[0]+=tail;f.score_tail[1]+=large_shape_tail;f.information_tail+=tail;
    }
    for(int i=0;i<3;++i)for(int j=0;j<i;++j)f.information[i][j]=f.information[j][i];
    f.projection[0][0]=Pair(active[0]?1:0);f.projection[1][1]=Pair(active[1]?1:0);
    f.projection[2][1]=Pair(sign_y)/Pair(power);f.projection[2][2]=Pair(sign_y)/Pair(power);
    return f;
  };
  if(!pair_polish(state,active,{{0,2,1}},tol,maxit,objective,detail))return false;
  const Pair pp=active[0]?exponential(state[0]):Pair(small),qq=active[1]?exponential(state[1]):Pair(large);
  const Pair logs=logarithm(Pair(unit))+(Pair(sign_y)/Pair(power))*(state[2]+(active[1]?state[1]:logarithm(Pair(large))));
  const double ss=value(exponential(logs));if(!(ss>0)||!std::isfinite(ss)||std::abs(value(logarithm(Pair(ss))-logs))>tol*.25)return false;
  const double ap=value(reflect?qq:pp),bp=value(reflect?pp:qq);
  if(!(ap>0&&bp>0)||!std::isfinite(ap+bp))return false;
  Pair ll=objective(state).likelihood;
  for(double v:x)ll=ll+logarithm(Pair(power))-logarithm(Pair(v)-Pair(shift));
  if(!std::isfinite(ll.hi))return false;
  alpha=ap;beta=bp;scale=ss;loglik=value(ll);return true;
}
inline bool beta_gamma_fit(span x,double& alpha,double& beta,double& scale,
    const std::array<bool,3>& free,double tol,int maxit,double& loglik,double power=1,double shift=0) {
  StopDetail detail;return beta_gamma_fit(x,alpha,beta,scale,free,tol,maxit,loglik,power,shift,detail);
}

}}
