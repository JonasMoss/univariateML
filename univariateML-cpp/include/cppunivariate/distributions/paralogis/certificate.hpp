// Compact-domain stationary-point exclusion certificate; see README.md.
#pragma once
#include "../../core/work_budget.hpp"
#include "../burr/certificate.hpp"
namespace cppunivariate {
namespace paralogis_cert_detail {
// Reuse the practical enclosures and small matrices of the Burr certificate.
// This does not call the unconstrained Burr fitter.
// Relative operation margins retain tiny scores on the held-scale tail.
inline burr_cert_detail::Interval relative(double lo,double hi) {
  if(!std::isfinite(lo)||!std::isfinite(hi))return burr_cert_detail::invalid();
  const double e=burr_cert_detail::pad*std::max(std::abs(lo),std::abs(hi))+std::numeric_limits<double>::denorm_min();
  // Rounded monotone-function endpoints can reverse by one ulp near
  // zero (notably softplus). Apply their error margins before ordering.
  const burr_cert_detail::Interval result(lo-e,hi+e);
  return result.valid()?result:burr_cert_detail::invalid();
}
inline burr_cert_detail::Interval product(burr_cert_detail::Interval a,burr_cert_detail::Interval b) {
  const std::array<double,4> v{{a.lo*b.lo,a.lo*b.hi,a.hi*b.lo,a.hi*b.hi}};
  for(double value:v)if(!std::isfinite(value))return burr_cert_detail::invalid();
  return relative(*std::min_element(v.begin(),v.end()),*std::max_element(v.begin(),v.end()));
}
inline burr_cert_detail::Interval difference(burr_cert_detail::Interval a,burr_cert_detail::Interval b) {
  return relative(a.lo-b.hi,a.hi-b.lo);
}
struct RelativeSum {
  double lo=0,hi=0,mag=0,n=0;
  void add(burr_cert_detail::Interval v){lo+=v.lo;hi+=v.hi;mag+=v.mag();++n;}
  burr_cert_detail::Interval get()const {
    const double e=burr_cert_detail::pad*(n+1)*mag+(n+1)*std::numeric_limits<double>::denorm_min();return {lo-e,hi+e};
  }
};
struct Group { burr_cert_detail::Interval value; double count; };
struct Data {
  std::vector<Group> exact;
  std::array<std::vector<Group>,3> bins;
  burr_cert_detail::Interval sum;
  double n=0; bool held=false;
  explicit Data(std::vector<burr_cert_detail::Interval> y,bool fixed=false) {
    held=fixed;
    std::sort(y.begin(),y.end(),[](const burr_cert_detail::Interval& a,
        const burr_cert_detail::Interval& b){return a.lo<b.lo;});
    burr_cert_detail::Sum total;
    for(auto v:y) {
      total.add(v); ++n;
      if(!exact.empty()&&exact.back().value.lo==v.lo&&exact.back().value.hi==v.hi)
        ++exact.back().count;
      else exact.push_back({v,1});
    }
    sum=total.get();
    for(std::size_t level=0;level<bins.size();++level) {
      const std::size_t target=std::size_t(128)<<(3*level);
      const std::size_t width=std::max(std::size_t(1),(exact.size()+target-1)/target);
      for(std::size_t i=0;i<exact.size();i+=width) {
        Group g=exact[i];
        for(std::size_t j=i+1;j<std::min(i+width,exact.size());++j) {
          g.value.hi=std::max(g.value.hi,exact[j].value.hi);g.count+=exact[j].count;
        }
        bins[level].push_back(g);
      }
    }
  }
};
inline bool derivatives(const Data& data,
    burr_cert_detail::Interval a, burr_cert_detail::Interval b,
    std::array<burr_cert_detail::Interval,3>& g,
    burr_cert_detail::BoxMatrix& h, burr_cert_detail::Interval& ll,
    int level=-1) {
  burr_cert_detail::Sum sp,syp,sw,syw,syyw,prob;
  for(const auto& group:level>=0?data.bins[level]:data.exact) {
    const auto v=group.value;
    const auto z=a*v+b,p=burr_cert_detail::sig(z),w=p*(1-p);
    const double m=group.count;
    sp.add(m*burr_cert_detail::sp(z));syp.add(m*(v*p));prob.add(m*p);
    sw.add(m*w);syw.add(m*(v*w));syyw.add(m*(v*v*w));
  }
  const double n=data.n;
  ll=2*n*burr_cert_detail::log(a)+a*data.sum+n*b-(a+1)*sp.get();
  g[0]=2*n/a+data.sum-sp.get()-(a+1)*syp.get();
  g[1]=n-(a+1)*prob.get();
  h.a[0][0]=-2*n/(a*a)-2*syp.get()-(a+1)*syyw.get();
  h.a[0][1]=h.a[1][0]=-prob.get()-(a+1)*syw.get();
  h.a[1][1]=-(a+1)*sw.get();
  if(data.held) {
    RelativeSum score,curvature;
    score.add(relative(2*n/a.hi,2*n/a.lo));
    curvature.add(relative(-2*n/(a.lo*a.lo),-2*n/(a.hi*a.hi)));
    for(const auto& group:level>=0?data.bins[level]:data.exact) {
      const auto v=group.value,z=product(a,v);
      const auto p=relative(burr_cert_detail::sigmoid(z.lo),burr_cert_detail::sigmoid(z.hi));
      const auto soft=relative(burr_cert_detail::softplus(z.lo),burr_cert_detail::softplus(z.hi));
      const auto yp=product(v,p);
      const auto term=product(relative(a.lo+1,a.hi+1),yp);
      score.add(product(group.count,difference(difference(v,soft),term)));
      const auto w=product(p,relative(1-p.hi,1-p.lo));
      const auto second=product(relative(a.lo+1,a.hi+1),product(product(v,v),w));
      curvature.add(product(group.count,difference(product(-2,yp),second)));
    }
    g[0]=score.get();h.a[0][0]=curvature.get();
  }
  return a.lo>0&&ll.valid()&&g[0].valid()&&g[1].valid()
    &&h.a[0][0].valid()&&h.a[0][1].valid()&&h.a[1][1].valid();
}
struct Result {bool ok=false,budget=false;int boxes=0;double shape=NAN,scale=NAN,logLik=NAN;};
inline Result certify(span x,double a,double scale,bool fixed,std::int64_t budget=-1,int maxit=100) {
  maxit = static_cast<int>(solver_budget(maxit));
  if(budget<0)budget=certificate_budget(maxit);
  Result out;
  if(!(a>0)||!(scale>0)||!std::isfinite(a)||!std::isfinite(scale))return out;
  // Center at the proposal's scale: the intercept root is near zero, and
  // converting the root box back to scale does not lose correlated digits.
  const double center=std::log(scale);
  double jac=0;
  std::vector<burr_cert_detail::Interval> y;
  for(double v:x){const double l=std::log(v);jac-=l;
    // Relative log rounding and subtraction cancellation allowance. An
    // absolute unit-sized pad here obscures roots at large held shapes.
    const double e=burr_cert_detail::pad*(std::abs(l)+std::abs(center)+std::abs(l-center))+std::numeric_limits<double>::denorm_min();
    y.push_back({l-center-e,l-center+e});}
  const Data data(y,fixed);
  double b=0;
  const int dim=fixed?1:2;
  // Newton steps propose a center only. Success below requires an enclosed
  // fixed-point map, regardless of the proposal's optimizer status.
  for(int it=0;it<maxit;++it){
    std::array<burr_cert_detail::Interval,3> g;
    burr_cert_detail::BoxMatrix h; burr_cert_detail::Interval ll;
    if(!derivatives(data,a,b,g,h,ll))return out;
    burr_cert_detail::Matrix hn,R;
    for(int i=0;i<dim;++i)for(int j=0;j<dim;++j)hn.a[i][j]=(h.a[i][j].lo+h.a[i][j].hi)/2;
    if(!burr_cert_detail::inverse(hn,R,dim))return out;
    std::array<double,2> step{};
    for(int i=0;i<dim;++i)for(int j=0;j<dim;++j)step[i]+=R.a[i][j]*(g[j].lo+g[j].hi)/2;
    if(!std::isfinite(step[0])||!std::isfinite(step[1])||a-step[0]<=0)return out;
    a-=step[0];b-=step[1];
    if(std::abs(step[0])<1e-13*a&&std::abs(step[1])<1e-13*a)break;
  }
  std::array<burr_cert_detail::Interval,3> g;
  burr_cert_detail::BoxMatrix hc,hb; burr_cert_detail::Interval ll;
  if(!derivatives(data,a,b,g,hc,ll))return out;
  const auto gc=g;
  burr_cert_detail::Matrix hn,R;
  for(int i=0;i<dim;++i)for(int j=0;j<dim;++j)hn.a[i][j]=(hc.a[i][j].lo+hc.a[i][j].hi)/2;
  if(!burr_cert_detail::inverse(hn,R,dim))return out;
  for(auto& row:R.a)for(double& v:row)v=-v;
  // Root radii imply relative shape error <=1e-8 and relative scale
  // error <=~2e-8. Ambiguous arithmetic stops, never enlarges this target.
  const std::array<double,3> radii{{1e-8*a,1e-8*a,0}};
  const auto ab=burr_cert_detail::padded(a-radii[0],a+radii[0]);
  const auto bb=fixed?burr_cert_detail::Interval(0):burr_cert_detail::padded(b-radii[1],b+radii[1]);
  if(!derivatives(data,ab,bb,g,hb,ll)||
      !burr_cert_detail::contraction(R,gc,hb,radii,dim,0))return out;
  const auto sb=burr_cert_detail::exp(center-bb/ab);
  if(!fixed&&(!sb.valid()||sb.lo<=0||(sb.hi-sb.lo)/sb.lo>1e-6))return out;
  out.shape=a;out.scale=fixed?scale:std::exp(center-b/a);
  // Direct stable evaluation at the certified center (not a bound).
  out.logLik=2*x.size()*std::log(a)+jac;
  for(double v:x){const double z=a*(std::log(v)-center)+b;
    out.logLik+=z-(a+1)*burr_cert_detail::softplus(z);}
  if(!std::isfinite(out.shape)||!std::isfinite(out.scale)||!(out.scale>0)||!std::isfinite(out.logLik))return out;
  // A larger contraction box contains exactly the same root and permits
  // removal of its neighborhood from the exhaustive stationary search.
  std::array<double,3> neighborhood{{0,0,0}};
  bool local=false;
  for(double f:{.03,.01,.003,.001,.0003,.0001}){
    neighborhood={{f*a,f*a,0}};
    if(derivatives(data,burr_cert_detail::padded(a-neighborhood[0],a+neighborhood[0]),
        fixed?burr_cert_detail::Interval(0):burr_cert_detail::padded(b-neighborhood[1],b+neighborhood[1]),g,hb,ll)
        &&burr_cert_detail::contraction(R,gc,hb,neighborhood,dim,0)){local=true;break;}
  }
  if(!local)return out;
  // A feasible center gives a lower bound, independent of root accuracy.
  if(!derivatives(data,a,b,g,hc,ll))return out;
  const double best=ll.lo;
  double ymin=INFINITY,ymax=-INFINITY;
  for(auto v:y){ymin=std::min(ymin,v.lo);ymax=std::max(ymax,v.hi);}
  double lo=std::min(1.,a/2),hi=std::max(1.,2*a);
  const double n=x.size();
  // All tail decisions carry the same practical enclosure margin.
  auto tail=[&](double v,bool upper){
    burr_cert_detail::Interval z(v);
    if(!upper)return ((fixed?2:1)*n*burr_cert_detail::log(z)).hi;
    if(fixed){
      double c=0;for(auto t:y)c=std::max(c,std::max(t.lo>0?t.lo:0.,t.hi<0?-t.hi:0.));
      if(c==0){ // Exact x == scale: log-axis likelihood known analytically.
        bool equal=true;for(double t:x)if(t!=scale)equal=false;
        if(!equal)return double(INFINITY);
        c=n*std::log(2.);
      }
      if(v<2*n/c)return double(INFINITY);
      return (2*n*burr_cert_detail::log(z)-c*z).hi;
    }
    // Use a lower bound on the separation of the extreme exact logs.
    double minhi=INFINITY,maxlo=-INFINITY;
    for(auto t:y){minhi=std::min(minhi,t.hi);maxlo=std::max(maxlo,t.lo);}
    const double delta=maxlo-minhi;
    if(!(delta>0)||v<5*n/(3*delta))return double(INFINITY);
    return (n*(burr_cert_detail::log(z)+.6*std::log(4.))-.6*delta*z).hi;
  };
  bool lower=false,upper=false;
  for(int i=0;i<maxit;++i){if(tail(lo,false)<best){lower=true;break;}lo/=2;if(!(lo>0))break;}
  for(int i=0;i<maxit;++i){if(tail(hi,true)<best){upper=true;break;}hi*=2;if(!std::isfinite(hi))break;}
  if(!lower||!upper)return out;
  struct Box {double al,ah,bl,bh;};
  auto range=[&](double al,double ah){
    const burr_cert_detail::Interval av(al,ah);
    const auto v=-burr_cert_detail::log(av)-av*burr_cert_detail::Interval(ymin,ymax);
    return Box{al,ah,fixed?0:v.lo,fixed?0:v.hi};
  };
  // Keep initial shape aspect ratios bounded: a very broad interval
  // product a*a can lose its positive lower bound under practical padding.
  std::vector<Box> pending;
  for(double left=lo;left<hi;){
    const double right=std::min(hi,2*left);
    if(!(right>left))return out;
    pending.push_back(range(left,right));left=right;
  }
  while(!pending.empty()){
    if(out.boxes++>=budget){out.budget=true;return out;}
    Box box=pending.back();pending.pop_back();
    if(box.al>a-neighborhood[0]&&box.ah<a+neighborhood[0]&&
       (fixed||(box.bl>b-neighborhood[1]&&box.bh<b+neighborhood[1])))continue;
    // A coarse bin encloses every observation it replaces. It may only
    // exclude; inconclusive bins fall back to the exact grouped sample.
    bool excluded=false;
    for(int level=0;level<2;++level) {
      if(data.bins[level].size()>=data.exact.size())break;
      if(derivatives(data,{box.al,box.ah},{box.bl,box.bh},g,hb,ll,level) &&
          (ll.hi<best||g[0].lo>0||g[0].hi<0||(!fixed&&(g[1].lo>0||g[1].hi<0)))) {
        excluded=true;break;
      }
    }
    if(excluded)continue;
    // Fine bins also give valid Hessian and midpoint bounds for the
    // mean-value map. Restore individual groups on small unresolved boxes.
    const int level=(box.ah-box.al>1e-4*box.al||
        (!fixed&&box.bh-box.bl>1e-4*(1+box.al)))?2:-1;
    if(!derivatives(data,{box.al,box.ah},{box.bl,box.bh},g,hb,ll,level))return out;
    if(ll.hi<best||g[0].lo>0||g[0].hi<0||(!fixed&&(g[1].lo>0||g[1].hi<0)))continue;
    // Every root in this box also lies in this mean-value fixed-point
    // image. Intersection is an exclusion step, not an optimizer step.
    const std::array<double,2> midpoint{{(box.al+box.ah)/2,(box.bl+box.bh)/2}};
    std::array<burr_cert_detail::Interval,3> gm;
    burr_cert_detail::BoxMatrix hm;burr_cert_detail::Interval lm;
    if(!derivatives(data,midpoint[0],midpoint[1],gm,hm,lm,level))return out;
    burr_cert_detail::Matrix Hm,Rm;
    for(int i=0;i<dim;++i)for(int j=0;j<dim;++j)Hm.a[i][j]=(hm.a[i][j].lo+hm.a[i][j].hi)/2;
    if(burr_cert_detail::inverse(Hm,Rm,dim)){
      for(auto& row:Rm.a)for(double& v:row)v=-v;
      const std::array<burr_cert_detail::Interval,2> offset{{
        {box.al-midpoint[0],box.ah-midpoint[0]},{box.bl-midpoint[1],box.bh-midpoint[1]}}};
      std::array<burr_cert_detail::Interval,2> image;
      bool valid=true,empty=false;
      for(int i=0;i<dim;++i){
        image[i]=midpoint[i];
        for(int j=0;j<dim;++j)image[i]=image[i]+Rm.a[i][j]*gm[j];
        for(int j=0;j<dim;++j){
          burr_cert_detail::Interval e(i==j?1:0);
          for(int k=0;k<dim;++k)e=e+Rm.a[i][k]*hb.a[k][j];
          image[i]=image[i]+e*offset[j];
        }
        if(!image[i].valid())valid=false;
      }
      Box clipped=box;
      if(valid){
        clipped.al=std::max(box.al,image[0].lo);clipped.ah=std::min(box.ah,image[0].hi);
        if(!fixed){clipped.bl=std::max(box.bl,image[1].lo);clipped.bh=std::min(box.bh,image[1].hi);}
        empty=clipped.al>clipped.ah||clipped.bl>clipped.bh;
        if(empty)continue;
        if(clipped.ah-clipped.al<.7*(box.ah-box.al)||(!fixed&&clipped.bh-clipped.bl<.7*(box.bh-box.bl))){
          pending.push_back(clipped);continue;
        }
      }
    }
    // Natural intercept bounds are re-applied on every shape split.
    if(fixed||std::log(box.ah/box.al)> (box.bh-box.bl)/(1+box.ah*(ymax-ymin))){
      const double mid=std::sqrt(box.al)*std::sqrt(box.ah);
      if(!(mid>box.al&&mid<box.ah))return out;
      for(auto half:{range(box.al,mid),range(mid,box.ah)}){
        half.bl=std::max(half.bl,box.bl);half.bh=std::min(half.bh,box.bh);
        if(half.bl<=half.bh)pending.push_back(half);
      }
    }else{
      const double mid=box.bl+(box.bh-box.bl)/2;
      if(!(mid>box.bl&&mid<box.bh))return out;
      pending.push_back({box.al,box.ah,box.bl,mid});
      pending.push_back({box.al,box.ah,mid,box.bh});
    }
  }
  out.ok=true;
  return out;
}
inline StopDetail witness(span x,double shape,double scale) {
  StopDetail d;
  if(!(shape>0)||!(scale>0)||!std::isfinite(shape)||!std::isfinite(scale))return d;
  std::vector<burr_cert_detail::Interval> y;
  burr_cert_detail::Sum jac;
  for(double v:x){if(!(v>0)||!std::isfinite(v))return d;
    const auto l=burr_cert_detail::padded(std::log(v),std::log(v));
    y.push_back(l-std::log(scale));jac.add(-l);}
  std::array<burr_cert_detail::Interval,3> g;
  burr_cert_detail::BoxMatrix h; burr_cert_detail::Interval ll;
  const Data data(y);
  if(derivatives(data,shape,0,g,h,ll)){
    ll=ll+jac.get();
    d.candidates.push_back({{shape,scale},ll.lo,ll.hi});
    d.loglik_lower=ll.lo;d.loglik_upper=INFINITY;
  }
  return d;
}
} // namespace paralogis_cert_detail
} // namespace cppunivariate
