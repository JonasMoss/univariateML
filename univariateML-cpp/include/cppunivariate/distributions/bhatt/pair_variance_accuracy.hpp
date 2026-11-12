#pragma once
#include "cppunivariate/distributions/burr/pair_polish.hpp"
namespace cppunivariate { namespace bhatt_detail {
struct PairJet {
  burr_power_one_detail::Pair v;
  std::array<burr_power_one_detail::Pair,3> g{};
  std::array<std::array<burr_power_one_detail::Pair,3>,3> h{};
  PairJet()=default;
  PairJet(double x):v(x){}
  PairJet(burr_power_one_detail::Pair x):v(x){}
};
inline PairJet operator+(const PairJet& a,const PairJet& b) {
  PairJet r(a.v+b.v);for(int i=0;i<3;++i){r.g[i]=a.g[i]+b.g[i];for(int j=0;j<3;++j)r.h[i][j]=a.h[i][j]+b.h[i][j];}return r;
}
inline PairJet operator-(const PairJet& a){PairJet r(-a.v);for(int i=0;i<3;++i){r.g[i]=-a.g[i];for(int j=0;j<3;++j)r.h[i][j]=-a.h[i][j];}return r;}
inline PairJet operator-(const PairJet& a,const PairJet& b){return a+(-b);}
inline PairJet operator*(const PairJet& a,const PairJet& b){PairJet r(a.v*b.v);for(int i=0;i<3;++i){r.g[i]=a.g[i]*b.v+a.v*b.g[i];for(int j=0;j<3;++j)r.h[i][j]=a.h[i][j]*b.v+a.g[i]*b.g[j]+a.g[j]*b.g[i]+a.v*b.h[i][j];}return r;}
inline PairJet pair_compose(const PairJet& a,burr_power_one_detail::Pair v,burr_power_one_detail::Pair first,burr_power_one_detail::Pair second){PairJet r(v);for(int i=0;i<3;++i){r.g[i]=first*a.g[i];for(int j=0;j<3;++j)r.h[i][j]=first*a.h[i][j]+second*a.g[i]*a.g[j];}return r;}
inline PairJet pair_exp(const PairJet& a){const auto v=burr_power_one_detail::exponential(a.v);return pair_compose(a,v,v,v);}
inline PairJet pair_log1p(const PairJet& a){const auto d=burr_power_one_detail::Pair(1)+a.v,inv=burr_power_one_detail::Pair(1)/d;return pair_compose(a,burr_power_one_detail::log_increment(a.v),inv,-inv*inv);}
inline PairJet pair_hermite_remainder(const PairJet& z,const PairJet& eta,bool matching_variance=true){
  constexpr std::array<double,33> hi{{1.0,0.0,-0.005555555555555556,0.0003527336860670194,-1.1022927689594357e-05,1.7814832629647445e-07,2.379117605455054e-10,-1.0332167886547663e-10,3.4481655490827335e-12,-7.397732743549895e-14,1.2014248185055961e-15,-1.541362555669139e-17,1.542914361069754e-19,-1.0769770881153958e-21,1.8769827092263714e-24,9.099689282740327e-26,-1.9209199724493857e-27,2.5940218663200634e-29,-2.811882411447734e-31,2.608820019798352e-33,-2.1248151989786644e-35,1.5323836949369032e-37,-9.746305904951356e-40,5.3481399114516135e-42,-2.3689343587750833e-44,6.424361837998374e-47,1.7804620519898854e-49,-4.4816324130964525e-51,4.434707242565657e-53,-3.383006663395076e-55,2.2277340455522795e-57,-1.3190066281484694e-59,7.155370222115456e-62}};
  constexpr std::array<double,33> lo{{0.0,0.0,2.1202175817494308e-19,-2.3787433907794843e-20,7.433573096185888e-22,-1.0302677837766485e-23,1.0985801126363962e-26,-1.669049192089924e-27,-2.1132119275647523e-29,-1.398464061081825e-30,9.354609684479294e-32,-1.2647131132515341e-33,1.0595967142880885e-35,-2.0429666008064887e-38,-1.448181465935077e-40,3.1723688796239454e-42,1.302935728245779e-43,-2.7157391110280842e-45,1.719766593582945e-47,-1.3665969119167296e-49,6.908869203294155e-52,-1.0536316286435271e-54,-5.074798528969352e-56,1.3989720724643069e-58,-1.9516193088064592e-60,3.9045486883876986e-64,-2.115641601457785e-66,2.4943000671611387e-67,-2.5742097153189644e-69,-2.9901885758782947e-71,-1.0831984205511639e-73,-1.2081228945274429e-76,3.8720964721860737e-78}};
  const auto u=z*eta,q=eta*eta;PairJet previous(1),current=u,sum;
  burr_power_one_detail::Pair plain_coefficient(1);
  for(int n=2;n<=64;++n){const auto next=u*current-PairJet(n-1)*q*previous;
    if(n%2==0){plain_coefficient=plain_coefficient/(burr_power_one_detail::Pair(n)*burr_power_one_detail::Pair(n+1));
      if(!matching_variance)sum=sum+PairJet(plain_coefficient)*next;
      else if(n>=4){auto c=burr_power_one_detail::Pair(hi[n/2]);c.lo=lo[n/2];sum=sum+PairJet(c)*next;}}
    previous=current;current=next;}
  return pair_log1p(sum);
}
// Free Gaussian scale: C2*H4*eta^4+C3*H6*eta^6 nominates
// eta^2=10.5*H4/H6, with information scaled by eta_seed^3.
// Held Gaussian scale: the quadratic and quartic width terms instead
// nominate -H2/(12*Q), with information scaled by eta_seed^2.
// Native coefficient projections and propagated arithmetic errors decide
// acceptance in both cases; these moment equations supply starts only.
inline bool pair_variance_accuracy(BhattResult& r,span x,double held_mu,double tol,int maxit,bool sigma_held=false) {
  const double sample_count=static_cast<double>(x.size());
  const double unit=sigma_held?r.sigma:std::hypot(r.sigma,r.a/std::sqrt(3.));
  if(!(unit>0&&r.a>=0)||tol<4*std::numeric_limits<double>::epsilon())return false;
  const auto unit_pair=burr_power_one_detail::Pair(unit);
  burr_power_one_detail::Pair origin(r.mu),offset;
  if(std::isnan(held_mu)){for(double v:x)offset=offset+(burr_power_one_detail::Pair(v)-origin)/unit_pair;origin=origin+unit_pair*offset/burr_power_one_detail::Pair(double(sample_count));}
  const double center=burr_power_one_detail::value(origin);
  burr_power_one_detail::Pair second,fourth,sixth;
  for(double v:x){const auto z=(burr_power_one_detail::Pair(v)-origin)/unit_pair,z2=z*z;second=second+z2;fourth=fourth+z2*z2;sixth=sixth+z2*z2*z2;}
  const auto n=burr_power_one_detail::Pair(double(sample_count)),variance=second/n;
  const auto h4=fourth/(variance*variance)-burr_power_one_detail::Pair(3)*n;
  const auto h6=sixth/(variance*variance*variance)-burr_power_one_detail::Pair(15)*fourth/(variance*variance)+burr_power_one_detail::Pair(30)*n;
  const auto h2=second-n,held_fourth=fourth-burr_power_one_detail::Pair(6)*second+burr_power_one_detail::Pair(3)*n,held_square=fourth-burr_power_one_detail::Pair(2)*second+n;
  const auto quartic=held_fourth/burr_power_one_detail::Pair(120)-held_square/burr_power_one_detail::Pair(72);
  if(!sigma_held&&r.a==0&&burr_power_one_detail::value(h4)>8192*sample_count*(sample_count+std::abs(burr_power_one_detail::value(fourth/(variance*variance))))*std::numeric_limits<double>::epsilon()*std::numeric_limits<double>::epsilon()) {
    if(std::isnan(held_mu))r.mu=center;
    r.sigma=burr_power_one_detail::value(unit_pair*burr_power_one_detail::square_root(variance));
    r.logLik=-sample_count*(std::log(r.sigma)+1.41893853320467274178);return true;
  }
  if(sigma_held&&r.a==0&&burr_power_one_detail::value(h2)<-8192*sample_count*(sample_count+std::abs(burr_power_one_detail::value(second)))*std::numeric_limits<double>::epsilon()*std::numeric_limits<double>::epsilon()) {
    if(std::isnan(held_mu))r.mu=center;
    r.logLik=-sample_count*(std::log(unit)+.91893853320467274178)-.5*burr_power_one_detail::value(second);return true;
  }
  double eta=r.a/unit,conditioning_eta=eta;
  if(sigma_held&&burr_power_one_detail::sign(h2)>0&&burr_power_one_detail::sign(quartic)<0) {
    const double seed=burr_power_one_detail::value(-h2/(burr_power_one_detail::Pair(12)*quartic));
    if(seed>0&&seed<.01){conditioning_eta=std::sqrt(seed);if(!(eta>0))eta=conditioning_eta;}
  }
  if(!sigma_held&&burr_power_one_detail::sign(h4)<0&&burr_power_one_detail::sign(h6)<0){
    const double seed=burr_power_one_detail::value(burr_power_one_detail::Pair(10.5)*h4/h6);
    if(seed>0&&seed<.01){conditioning_eta=std::sqrt(seed);if(!(eta>0))eta=conditioning_eta;}
  }
  const double coordinate_unit=conditioning_eta*conditioning_eta*(sigma_held?1:conditioning_eta);
  if(!(coordinate_unit>0&&eta<=.25))return false;
  std::array<burr_power_one_detail::Pair,3> q{{burr_power_one_detail::Pair(0),sigma_held?burr_power_one_detail::Pair(0):burr_power_one_detail::logarithm(variance)/burr_power_one_detail::Pair(2),burr_power_one_detail::logarithm(burr_power_one_detail::Pair(eta))*burr_power_one_detail::Pair(coordinate_unit)}};
  const std::array<bool,3> free{{std::isnan(held_mu),!sigma_held,true}};
  const auto objective=[&](const std::array<burr_power_one_detail::Pair,3>& t){
    std::array<PairJet,3> w;
    for(int i=0;i<3;++i){w[i]=PairJet(t[i]);if(free[i])w[i].g[i]=burr_power_one_detail::Pair(1);}
    const auto inverse_scale=pair_exp(-w[1]),eta_value=pair_exp(w[2]*PairJet(burr_power_one_detail::Pair(1)/burr_power_one_detail::Pair(coordinate_unit)));
    burr_power_one_detail::PairEvaluation f;
    const double et=burr_power_one_detail::value(eta_value.v);
    if(!(et>0&&et<=.25)){f.valid=false;return f;}
    double maximum_z=0;
    for(double value:x){const auto z=(PairJet((burr_power_one_detail::Pair(value)-origin)/unit_pair)-w[0])*inverse_scale;
      const double az=std::abs(burr_power_one_detail::value(z.v));maximum_z=std::max(maximum_z,az);
      if(et*(1+az)>1){f.valid=false;return f;}
      const auto term=-w[1]-z*z*PairJet(.5)+pair_hermite_remainder(z,eta_value,!sigma_held);
      f.likelihood=f.likelihood+term.v;f.likelihood_magnitude+=std::abs(burr_power_one_detail::value(term.v));
      for(int i=0;i<3;++i){f.score[i]=f.score[i]+term.g[i];f.score_magnitude[i]+=std::abs(burr_power_one_detail::value(term.g[i]));
        for(int j=0;j<3;++j){f.information[i][j]=f.information[i][j]-term.h[i][j];f.information_magnitude+=std::abs(burr_power_one_detail::value(term.h[i][j]));}}
      const double magnitude=4096*std::pow(et*(1+az),sigma_held?2:4);
      f.score_magnitude[2]+=magnitude/coordinate_unit;
      f.information_magnitude+=magnitude/(coordinate_unit*coordinate_unit);
    }
    const double S=unit*burr_power_one_detail::value(burr_power_one_detail::exponential(t[1]));
    const double width=S*et,sigma=sigma_held?unit:S*std::sqrt(1-et*et/3),location=burr_power_one_detail::value(origin+unit_pair*t[0]);
    f.projection[0][0]=burr_power_one_detail::Pair(unit/std::max(std::abs(location),sigma));
    if(!sigma_held){f.projection[1][1]=burr_power_one_detail::Pair(1);f.projection[1][2]=burr_power_one_detail::Pair(-et*et/((3-et*et)*coordinate_unit));}
    f.projection[2][1]=burr_power_one_detail::Pair(width/std::max(1.,width));f.projection[2][2]=f.projection[2][1]/burr_power_one_detail::Pair(coordinate_unit);
    const double radius=std::min(.25,1/(1+maximum_z)),ratio=et/(3*radius);
    const double tail=512*std::pow(ratio,66)/(1-ratio)*sample_count;
    for(int i=0;i<3;++i)f.score_tail[i]=tail*128/coordinate_unit;
    f.information_tail=tail*16384/(coordinate_unit*coordinate_unit);
    return f;
  };
  if(!burr_power_one_detail::pair_polish(q,free,{{0,1,2}},tol,maxit,objective,r.stop_detail))return false;
  const auto S=unit_pair*burr_power_one_detail::exponential(q[1]);
  const auto E=burr_power_one_detail::exponential(q[2]/burr_power_one_detail::Pair(coordinate_unit));
  const auto sigma=sigma_held?unit_pair:S*burr_power_one_detail::square_root(burr_power_one_detail::Pair(1)-E*E/burr_power_one_detail::Pair(3));
  if(free[0])r.mu=burr_power_one_detail::value(origin+unit_pair*q[0]);
  if(!sigma_held)r.sigma=burr_power_one_detail::value(sigma);r.a=burr_power_one_detail::value(S*E);
  const auto last=objective(q);r.logLik=burr_power_one_detail::value(last.likelihood)-sample_count*(std::log(unit)+.91893853320467274178);
  return r.sigma>0&&r.a>0&&std::isfinite(r.mu+r.sigma+r.a+r.logLik);
}
}}
