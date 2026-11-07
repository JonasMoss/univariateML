#pragma once
// Included within sged_cert_detail. Compact-domain certificate for held mean
// and asymmetric subunit shapes; see README.md for bounds and arithmetic scope.
inline SgedResult subunit_scale_fit(span x,int maxit,double mean,double nu,double xi) {
  SgedResult out{mean,NAN,nu,xi,NAN,0,SgedStatus::CertificateFailure};
  double unit=0,shift=0;
  for(double v:x)unit=std::max(unit,std::abs(v-mean));
  if(!std::isfinite(unit))return out;
  if(unit==0){out.status=SgedStatus::Unbounded;return out;}
  const double su=distributions::fs::sged_sigma(nu,xi,shift),lambda=dens::ged_lambda(nu);
  if(!(su>0 && lambda>0) || !std::isfinite(su+lambda+shift) || shift==0)return out;
  const double n=static_cast<double>(x.size()),eps=std::numeric_limits<double>::epsilon();
  const double k=std::exp(nu*(std::log(std::abs(shift))-std::log(lambda))-std::log(2.));
  const double wx=nu*std::log(xi),constant=n*(std::log(std::abs(shift))-std::log(unit)+
      std::log(2/(xi+1/xi))+distributions::fs::ged_logdensity(0,nu));
  if(!(k>0) || !std::isfinite(k+constant+std::exp(std::abs(wx))))return out;
  struct Term {double b,knot,raw;};
  std::vector<Term> terms;
  std::vector<double> knots;
  for(double v:x) {
    const double b=(v-mean)/unit*(shift>0?1:-1);
    if(b==0 && v!=mean)return out;
    const double knot=b<0?-1/b:INFINITY;
    terms.push_back({b,knot,v});
    if(std::isfinite(knot))knots.push_back(knot);
  }
  std::sort(terms.begin(),terms.end(),[](const Term& a,const Term& b){return a.b<b.b;});
  for(std::size_t j=1;j<terms.size();++j) {
    if(terms[j].b==terms[j-1].b && terms[j].raw!=terms[j-1].raw)return out;
    if(std::isfinite(terms[j].knot) && terms[j].knot==terms[j-1].knot && terms[j].b!=terms[j-1].b)return out;
  }
  std::sort(knots.begin(),knots.end());
  knots.erase(std::unique(knots.begin(),knots.end()),knots.end());
  const auto residual=[](const Term& a,double r) {
    // The algebraic knot is exact in these coordinates; cancellation in the
    // public density is separately checked before a candidate can be returned.
    return r==a.knot?0:std::fma(a.b,r,1.);
  };
  const auto weight=[&](double z){return k*std::exp((z>0)==(shift>0)?-wx:wx);};
  std::int64_t work=0;
  const auto cap=certificate_budget(maxit)-12; // reserve public-coordinate witness evaluations
  double initial_cost=NAN;
  bool incumbent_recovered=false;
  double best=INFINITY,best_r=NAN;
  struct Point {double r,cost,p;};
  struct Box {Point l,h;};
  struct Enclosure {double l,h,lower,cost;};
  std::vector<Enclosure> possible;
  const auto pad=[&](double v){return 4096*eps*(1+n+std::abs(v))*(1+std::abs(wx)+1/nu);};
  const auto evaluate=[&](double r) {
    ++work;
    Sum p;
    for(const auto& a:terms) {
      const double z=residual(a,r);
      p.add(z==0?0:weight(z)*std::pow(std::abs(z),nu));
    }
    const double cost=p.value-n*std::log(r);
    if(std::isfinite(cost) && cost<best){best=cost;best_r=r;}
    return Point{r,cost,p.value};
  };
  const auto public_value=[&](double sd) {
    SgedResult r=out;r.sigma=sd;
    if(!(sd>0) || !std::isfinite(sd))return r;
    Sum ll;
    for(double v:x) {
      const double z=(v-mean)/sd*su+shift;
      ll.add(std::log(2/(xi+1/xi))+distributions::fs::ged_logdensity(z/(z<0?1/xi:xi),nu)-std::log(sd)+std::log(su));
    }
    r.logLik=ll.value;
    if(std::isfinite(r.logLik))r.status=SgedStatus::Ok;
    return r;
  };
  const auto finish=[&](bool budget,bool tie) {
    SgedResult r=out;
    r.iterations=static_cast<int>(std::min<std::int64_t>(work,2147483647));
    r.stop_detail.budget_exhausted=budget;r.stop_detail.comparison_tie=tie;
    r.stop_detail.loglik_upper=INFINITY;
    for(int attempt=0;attempt<2;++attempt)if(std::isfinite(attempt==0?best_r:initial_cost)) {
      const double rr=attempt==0?best_r:.5,cost=attempt==0?best:initial_cost;
      const double base=std::exp(std::log(unit)+std::log(su)-std::log(std::abs(shift))-std::log(rr));
      std::vector<double> scales{base};
      if(attempt==0)for(const auto& a:terms)if(a.knot==rr) {
        scales.push_back((a.raw-mean)/(-shift)*su);
        scales.push_back((a.raw-mean)*su/(-shift));break;
      }
      SgedResult evaluated=out;
      evaluated.logLik=-INFINITY;
      for(double scale:scales)for(int direction:{-1,0,1}) {
        ++work;
        const double sd=direction==0?scale:std::nextafter(scale,direction>0?INFINITY:0.);
        if(!(sd>0) || !std::isfinite(sd))continue;
        const auto v=public_value(sd);
        if(v.status==SgedStatus::Ok && v.logLik>evaluated.logLik)evaluated=v;
      }
      const double ideal=constant-cost;
      if(evaluated.status==SgedStatus::Ok && std::abs(evaluated.logLik-ideal)<=1e-7*(n+std::abs(ideal))) {
        r.sigma=evaluated.sigma;r.logLik=evaluated.logLik;
        const double allowance=1e-7*(n+std::abs(ideal))+pad(cost);
        r.stop_detail.candidates.push_back({{mean,r.sigma,nu,xi},std::min(ideal,r.logLik)-allowance,std::max(ideal,r.logLik)+allowance});
        r.stop_detail.loglik_lower=r.stop_detail.candidates.back().loglik_lower;
        incumbent_recovered=attempt==0;break;
      }
    }
    r.iterations=static_cast<int>(std::min<std::int64_t>(work,2147483647));
    if(r.stop_detail.candidates.empty())r.stop_detail.budget_exhausted=false;
    return r;
  };
  const Point initial=evaluate(.5);
  initial_cost=initial.cost;
  if(!std::isfinite(initial.cost))return finish(false,false);
  // F(r)>=-n log r. For r>=2, a term with |b|=1 has
  // |1+b r|>=r/2, hence F(r)>=C r^nu-n log r.
  const double lowlog=-best/n-1;
  double highlog=std::max(std::log(2.),(std::log(n)-std::log(nu)-std::log(k)+std::abs(wx)+nu*std::log(2.))/nu+1);
  const double logc=std::log(k)-std::abs(wx)-nu*std::log(2.);
  while(true) {
    if(work>=cap)return finish(true,false);
    ++work;
    const double lower=std::exp(logc+nu*highlog)-n*highlog;
    if(lower>best+pad(best))break;
    highlog+=1;
    if(!std::isfinite(highlog))return finish(false,false);
  }
  const double low=std::exp(lowlog),high=std::exp(highlog);
  if(!(low>0 && high>low) || !std::isfinite(high))return finish(false,false);
  std::vector<double> cuts{low};
  for(double v:knots)if(v>low && v<high)cuts.push_back(v);
  cuts.push_back(high);
  // Every initial cell needs its two endpoints. Decline an unfundable
  // partition before spending O(n) work on each of thousands of knots.
  if(static_cast<std::int64_t>(cuts.size())+work>cap)return finish(true,false);
  std::vector<Point> points;
  for(double v:cuts) {
    if(work>=cap)return finish(true,false);
    const auto p=evaluate(v);
    if(!std::isfinite(p.cost+p.p))return finish(false,false);
    points.push_back(p);
    possible.push_back({v,v,p.cost-pad(p.cost),p.cost});
  }
  std::vector<Box> pending;
  for(std::size_t j=1;j<points.size();++j)pending.push_back({points[j-1],points[j]});
  while(!pending.empty()) {
    if(work>=cap)return finish(true,false);
    ++work;
    const Box box=pending.back();pending.pop_back();
    const double l=box.l.r,h=box.h.r;
    // Concave penalty on a sign cell lies above its chord. Minimize the
    // chord plus -n log r analytically to get the cell's cost lower bound.
    const double slope=(box.h.p-box.l.p)/(h-l);
    const double t=slope>0?std::max(l,std::min(h,n/slope)):h;
    const double lower=box.l.p+slope*(t-l)-n*std::log(t)-pad(box.l.cost)-pad(box.h.cost);
    if(!std::isfinite(lower))return finish(false,false);
    if(lower>best+pad(best))continue;
    // P' decreases on the open cell. Endpoint one-sided infinities are
    // intentional; at a left cusp it is +Inf, at a right cusp -Inf.
    struct Derivative {double value,magnitude;};
    const auto derivative=[&](double r,bool left_endpoint) {
      Sum p;double magnitude=0;
      for(const auto& a:terms)if(a.b!=0) {
        const double z=residual(a,r);
        if(z==0)return Derivative{left_endpoint?INFINITY:-INFINITY,INFINITY};
        const double v=nu*a.b*(z>0?1:-1)*weight(z)*std::pow(std::abs(z),nu-1);
        p.add(v);magnitude+=std::abs(v);
      }
      return Derivative{p.value,magnitude};
    };
    const auto dl=derivative(l,true),dh=derivative(h,false);
    const double conditioning=4096*eps*(1+std::abs(wx)+1/nu);
    if((std::isfinite(dh.magnitude) && dh.value-n/l>conditioning*(dh.magnitude+n/l)) ||
       (std::isfinite(dl.magnitude) && dl.value-n/h < -conditioning*(dl.magnitude+n/h)))continue;
    if(std::log(h)-std::log(l)<=1e-10) {
      possible.push_back({l,h,lower,box.l.cost});continue;
    }
    if(work>=cap)return finish(true,false);
    const double mid=std::exp(std::log(l)+(std::log(h)-std::log(l))/2);
    if(!(mid>l && mid<h))return finish(false,false);
    const auto p=evaluate(mid);
    if(!std::isfinite(p.cost+p.p))return finish(false,false);
    pending.push_back({box.l,p});pending.push_back({p,box.h});
  }
  possible.erase(std::remove_if(possible.begin(),possible.end(),[&](const Enclosure& a){return a.lower>best+pad(best);}),possible.end());
  // Point witnesses need not enclose stationary points. The surviving boxes
  // and points together enclose every global minimizer, including cusp knots.
  double rlo=best_r,rhi=best_r;
  for(const auto& a:possible){rlo=std::min(rlo,a.l);rhi=std::max(rhi,a.h);}
  auto r=finish(false,false);
  if(r.stop_detail.candidates.empty() || !incumbent_recovered)return r;
  const double logfactor=std::log(unit)+std::log(su)-std::log(std::abs(shift));
  const double slo=std::exp(logfactor-std::log(rhi)),shi=std::exp(logfactor-std::log(rlo));
  if(!(slo>0) || !std::isfinite(shi))return r;
  double global_lower=best-pad(best);
  for(const auto& a:possible)global_lower=std::min(global_lower,a.lower);
  r.stop_detail.loglik_upper=constant-global_lower+1e-7*(n+std::abs(constant-best));
  if(std::log(rhi)-std::log(rlo)>1e-7 ||
     std::max(shi,r.sigma)-std::min(slo,r.sigma)>1e-7*(1+std::abs(r.sigma))) {
    r.stop_detail.comparison_tie=true;
    // Preserve separated competing witnesses, without turning their diagnostic
    // coordinates into estimates. The bound covers the entire root box.
    for(const auto& a:possible) {
      const double sd=std::exp(logfactor-std::log(a.l));
      bool duplicate=false;
      for(const auto& c:r.stop_detail.candidates)if(std::abs(std::log(sd/c.coef[1]))<1e-7)duplicate=true;
      if(duplicate || !(sd>0) || !std::isfinite(sd))continue;
      if(work>=certificate_budget(maxit)){r.stop_detail.budget_exhausted=true;r.stop_detail.comparison_tie=false;return r;}
      ++work;
      const auto evaluated=public_value(sd);
      const double ideal=constant-a.cost,allowance=1e-7*(n+std::abs(ideal))+pad(a.cost);
      if(evaluated.status!=SgedStatus::Ok || std::abs(evaluated.logLik-ideal)>allowance) {
        r.stop_detail.comparison_tie=false;return r;
      }
      r.stop_detail.candidates.push_back({{mean,sd,nu,xi},std::min(ideal,evaluated.logLik)-allowance,
          std::max(constant-a.lower,evaluated.logLik)+allowance});
    }
    r.iterations=static_cast<int>(work);return r;
  }
  r.status=SgedStatus::Ok;r.certified=true;r.stop_detail={};return r;
}
