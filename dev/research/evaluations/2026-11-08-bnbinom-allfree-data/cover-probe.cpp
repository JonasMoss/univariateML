// Feasibility probe only: conditional stationary-cover exclusions; no estimates.
#include "cppunivariate/distributions/bnbinom/mle.hpp"
#include <fstream>
#include <iostream>
#include <iomanip>
#include "joint-bounds.hpp"
struct Box {std::array<double,3> lo,hi;};
int main(int argc,char **argv) {
  if(argc<2 || argc>6)return 1;
  const int maxit=argc>2?std::stoi(argv[2]):600;
  const bool diagonal_only=argc>3 && std::string(argv[3])=="diagonal";
  const bool precondition=argc<5 || std::string(argv[4])!="plain";
  std::ifstream input(argv[1]);std::vector<int> x;std::map<int,double> counts;int v;
  while(input>>v){x.push_back(v);counts[v]++;}
  cppunivariate::bnbinom_detail::Data d;d.n=x.size();
  double mean=0,moment=0,positive=0;
  for(const auto &p:counts){d.x.push_back(p.first);d.count.push_back(p.second);mean+=p.first*p.second/d.n;moment+=p.second*p.first*(p.first-1.);if(p.first>0)positive+=p.second;}
  const auto fit=cppunivariate::bnbinom_mle({x.data(),x.size()});
  const auto nb=cppunivariate::nbinom_mle({x.data(),x.size()},1e-10,600);
  const auto po=cppunivariate::pois_mle({x.data(),x.size()});
  if(fit.status!=cppunivariate::BnbinomStatus::Ok || nb.status!=cppunivariate::NbinomStatus::Ok || po.status!=cppunivariate::PoisStatus::Ok)return 2;
  const double L=fit.logLik-1e-6;
  if(!(L>nb.logLik && L>po.logLik))return 2;
  bool found=false;
  double lower=1,M=std::max(1.,2*mean);
  for(int step=0;step<maxit;++step){double h=0;for(const auto &p:counts)if(p.first>0)h+=p.second*std::min(0.,static_cast<double>(cppunivariate::bnbinom_detail::increment(lower,p.first))-cppunivariate::special::lgamma(p.first+1.));if(h+1e-6<L){found=true;break;}lower*=.5;}
  if(!found)return 3;found=false;
  for(int step=0;step<maxit;++step){double h=-d.n*M;for(const auto &p:counts)h+=p.second*(static_cast<double>(cppunivariate::bnbinom_detail::increment(M,p.first))-cppunivariate::special::lgamma(p.first+1.));if(h+1e-6<L){found=true;break;}M*=2;}
  if(!found)return 3;found=false;
  const double A=4*d.n*M*M+.5*moment,U=std::max(4*M,4*A/(L-po.logLik));
  const double delta=std::min(lower,1.);
  double logp=-std::log(2.);
  for(int step=0;step<maxit;++step){double h=0;for(const auto &p:counts)h+=p.second*std::min(0.,static_cast<double>(cppunivariate::bnbinom_detail::increment(delta+1,p.first))-cppunivariate::special::lgamma(p.first+1.)+delta*logp);if(h+1e-6<L){found=true;break;}logp-=std::log(2.);}
  if(!found)return 3;
  const double qmin=std::exp(L/positive-std::log(std::max(U,1.))-1),pmin=std::exp(logp);
  const double S=2*cppunivariate::bnbinom_detail::pair_nb_remainder(d,U,pmin,qmin)/(L-nb.logLik-1e-6);
  std::vector<Box> pending{{{lower,pmin*lower,lower},{U,S,S}}};
  long long boxes=0,excluded=0,near=0,unresolved=0,scores=0;
  if(diagonal_only)pending={{{lower,pmin*lower,lower},{U,S,U}}};
  const long long cap=cppunivariate::certificate_budget(maxit);
  std::array<double,3> root{fit.size,fit.alpha,fit.beta};
  const double radius=argc>5?std::stod(argv[5]):.01;
  if(argc>5)for(int step=0;step<maxit;++step) {
    const auto hp=bnb_probe::hessian(d,root,root);bnb_probe::Matrix h;
    for(int i=0;i<3;++i)for(int j=0;j<3;++j)h.v[i][j]=(hp[0].v[i][j]+hp[1].v[i][j])/2;
    const auto inv=bnb_probe::inverse(h);std::array<double,3> change{};
    for(int i=0;i<3;++i)for(int j=0;j<3;++j)change[i]+=inv.v[i][j]*cppunivariate::bnbinom_detail::scalar_score(d,{root[0],root[1],root[2]},j).value;
    double relative=0;for(int i=0;i<3;++i)relative=std::max(relative,std::abs(change[i])/root[i]);
    if(relative<radius*.01)break;
    for(int i=0;i<3;++i)root[i]-=change[i];
  }
  while(!pending.empty() && boxes<cap) {
    Box b=pending.back();pending.pop_back();++boxes;
    if(b.lo[0]>b.hi[2] || b.hi[1]/(b.hi[1]+b.lo[2])<pmin || b.hi[2]/(b.hi[2]+b.lo[1])<qmin || b.lo[1]+b.lo[2]>=S){++excluded;continue;}
    // Make the face exclusion local to each box, rather than using only
    // the very loose global ratio cutoffs in the initial superlevel box.
    const double local_p=b.lo[1]/(b.lo[1]+b.hi[2]);
    const double local_q=b.lo[2]/(b.hi[1]+b.lo[2]);
    const double remainder=cppunivariate::bnbinom_detail::pair_nb_remainder(d,b.hi[0],local_p,local_q)/(b.lo[1]+b.lo[2]);
    if(nb.logLik+1e-6+remainder<L){++excluded;continue;}
    const double min_m=b.lo[0]*b.lo[2]/(b.hi[1]+b.lo[0]+b.lo[2]);
    if(min_m>=M){++excluded;continue;}
    // Alpha mode increases with r and beta. Coarse signed brackets enclose
    // it uniformly over the whole r/b rectangle, without tiny endpoint solves.
    for(int step=0;step<maxit;++step) {
      const double mid=std::sqrt(b.lo[1])*std::sqrt(b.hi[1]);
      if(!(b.lo[1]<mid && mid<b.hi[1]))break;
      const auto l=cppunivariate::bnbinom_detail::scalar_score(d,{b.lo[0],mid,b.lo[2]},1);
      const auto h=cppunivariate::bnbinom_detail::scalar_score(d,{b.hi[0],mid,b.hi[2]},1);scores+=2;
      if(l.value>l.margin)b.lo[1]=mid;
      else if(h.value < -h.margin)b.hi[1]=mid;
      else break;
    }
    bool reject=false;
    for(int j=0;j<3;++j) {
      std::vector<double> low{b.lo[0],b.hi[1],b.lo[2]},high{b.hi[0],b.lo[1],b.hi[2]};
      if(j==1)std::swap(low,high);
      const auto a=cppunivariate::bnbinom_detail::scalar_score(d,low,j),z=cppunivariate::bnbinom_detail::scalar_score(d,high,j);scores+=2;
      if(a.value < -a.margin || z.value>z.margin){reject=true;break;}
    }
    if(reject){++excluded;continue;}
    const auto bound=cppunivariate::bnbinom_detail::alpha_likelihood_bound(d,b.lo[1],{b.lo[0],b.hi[0],b.lo[2],b.hi[2]});
    // Adjust only p0 upward to alpha-high; the count ratio decreases in alpha.
    const double upper=bound.value+bound.margin+d.n*(cppunivariate::bnbinom_detail::zero(b.lo[0],b.hi[1],b.lo[2])-cppunivariate::bnbinom_detail::zero(b.lo[0],b.lo[1],b.lo[2]));
    if(upper+1e-6<L){++excluded;continue;}
    double logwidth=0;for(int j=0;j<3;++j)logwidth=std::max(logwidth,std::log(b.hi[j]/b.lo[j]));
    if(precondition && !diagonal_only && logwidth<.5 && bnb_probe::excluded(d,b.lo,b.hi)){++excluded;continue;}
    bool close=true;for(int j=0;j<3;++j)close=close && b.lo[j]>root[j]*(1-radius) && b.hi[j]<root[j]*(1+radius);
    if(close && !diagonal_only){++near;continue;} // Not a proof of root isolation: diagnostic target.
    int split=0;double width=0;
    for(int j=0;j<(diagonal_only?2:3);++j){double w=std::log(b.hi[j])-std::log(b.lo[j]);if(w>width){width=w;split=j;}}
    const double mid=std::sqrt(b.lo[split])*std::sqrt(b.hi[split]);
    if(!(b.lo[split]<mid && mid<b.hi[split])){++unresolved;continue;}
    Box other=b;other.lo[split]=mid;b.hi[split]=mid;
    if(diagonal_only && split==0){other.lo[2]=mid;b.hi[2]=mid;}
    pending.push_back(other);pending.push_back(b);
  }
  std::cout<<std::setprecision(17)<<"{\"lower\":"<<lower<<",\"r_upper\":"<<U<<",\"sum_upper\":"<<S<<",\"radius\":"<<radius<<",\"root\":["<<root[0]<<','<<root[1]<<','<<root[2]<<"],\"boxes\":"<<boxes<<",\"score_evaluations\":"<<scores<<",\"excluded\":"<<excluded<<",\"near_root\":"<<near<<",\"unresolved\":"<<unresolved<<",\"pending\":"<<pending.size()<<",\"pending_boxes\":[";
  bool first=true;for(const auto &b:pending){if(!first)std::cout<<',';first=false;std::cout<<"{\"lower\":["<<b.lo[0]<<','<<b.lo[1]<<','<<b.lo[2]<<"],\"upper\":["<<b.hi[0]<<','<<b.hi[1]<<','<<b.hi[2]<<"]}";}std::cout<<"]}\n";
}
