#pragma once
// Included after ged_detail::conditional, inside namespace cppunivariate.
namespace ged_shape_detail {
struct Range { double lo,hi; };
inline Range add(Range a,Range b){return {a.lo+b.lo,a.hi+b.hi};}
inline Range neg(Range a){return {-a.hi,-a.lo};}
inline Range mul(Range a,Range b){return {std::min({a.lo*b.lo,a.lo*b.hi,a.hi*b.lo,a.hi*b.hi}),std::max({a.lo*b.lo,a.lo*b.hi,a.hi*b.lo,a.hi*b.hi})};}
inline Range square(Range a){return {a.lo<=0 && a.hi>=0?0:std::min(a.lo*a.lo,a.hi*a.hi),std::max(a.lo*a.lo,a.hi*a.hi)};}
inline Range divide(Range a,Range b){if(!(b.lo>0))return {-INFINITY,INFINITY};return mul(a,{1/b.hi,1/b.lo});}
inline Range power(double a,double b,double p,double q) {
  const double lo=a==0?0:std::min(std::pow(a,p),std::pow(a,q));
  return {lo,std::max(std::pow(b,p),std::pow(b,q))};
}
// Joint range of r^v log r, including its removable zero at r=0.
inline Range powerlog(double a,double b,double p,double q) {
  const auto f=[](double r,double v){return r==0?0:std::pow(r,v)*std::log(r);};
  Range out{std::min({f(a,p),f(a,q),f(b,p),f(b,q)}),std::max({f(a,p),f(a,q),f(b,p),f(b,q)})};
  const double c=std::exp(-1/p);
  if(a<=c && c<=b)out.lo=std::min(out.lo,-1/(std::exp(1.)*p));
  return out;
}
inline Range powerlog2(double a,double b,double p,double q) {
  const auto f=[](double r,double v){return r==0?0:std::pow(r,v)*std::log(r)*std::log(r);};
  Range out{std::min({f(a,p),f(a,q),f(b,p),f(b,q)}),std::max({f(a,p),f(a,q),f(b,p),f(b,q)})};
  if(a<=1 && b>=1)out.lo=0;
  const double c=std::exp(-2/p);
  if(a<=c && c<=b)out.hi=std::max(out.hi,4/(std::exp(2.)*p*p));
  return out;
}
inline Range residual_hessian(double a,double b,double p,double q) {
  if(a>0)return {std::min({std::pow(a,p-2),std::pow(a,q-2),std::pow(b,p-2),std::pow(b,q-2)}),std::max({std::pow(a,p-2),std::pow(a,q-2),std::pow(b,p-2),std::pow(b,q-2)})};
  return {q>2?0:(b>0?std::min(std::pow(b,p-2),std::pow(b,q-2)):0),p<2?INFINITY:std::max({1.,std::pow(b,p-2),std::pow(b,q-2)})};
}
// Endpoint powers/logs shared by the value, score and curvature moments.
// Keep powers in libm (rather than dividing by residuals) so a zero or an
// underflowed high power cannot erase a representable lower power.
struct ResidualPowers {
  double a,b,la,lb,ap,aq,bp,bq;
  ResidualPowers(double lower,double upper,double p,double q):
    a(lower),b(upper),la(a==0?0:std::log(a)),lb(b==a?la:(b==0?0:std::log(b))),
    ap(a==0?0:std::pow(a,p)),aq(p==q?ap:(a==0?0:std::pow(a,q))),
    bp(b==a?ap:std::pow(b,p)),bq(b==a?aq:(p==q?bp:std::pow(b,q))) {}
  Range value() const {return {std::min(ap,aq),std::max(bp,bq)};}
  Range logmoment(double p,int degree) const {
    const double u=ap*la,v=aq*la,w=bp*lb,z=bq*lb;
    if(degree==1) {
      Range out{std::min({u,v,w,z}),std::max({u,v,w,z})};
      const double c=std::exp(-1/p);
      if(a<=c && c<=b)out.lo=std::min(out.lo,-1/(std::exp(1.)*p));
      return out;
    }
    Range out{std::min({u*la,v*la,w*lb,z*lb}),std::max({u*la,v*la,w*lb,z*lb})};
    if(a<=1 && b>=1)out.lo=0;
    const double c=std::exp(-2/p);
    if(a<=c && c<=b)out.hi=std::max(out.hi,4/(std::exp(2.)*p*p));
    return out;
  }
};
struct ShapeBox {
  Range location{NAN,NAN},score{-INFINITY,INFINITY},logscale{-INFINITY,INFINITY};
  Range derivative{-INFINITY,INFINITY};
  bool valid=false;
};
// y and location are normalized once by the public certificate. All interval
// comparisons carry the practical final margin; this is not interval libm.
inline Range location_score(span y,double m,double p,double q) {
  Sum lower,upper,absolute;
  for(double v:y) {
    if(v==m) {if(p==1){lower.add(-1);upper.add(1);absolute.add(1);}continue;}
    auto r=power(std::abs(v-m),std::abs(v-m),p-1,q-1);
    if(v>m)r=neg(r);
    lower.add(r.lo);upper.add(r.hi);absolute.add(std::max(std::abs(r.lo),std::abs(r.hi)));
  }
  const double pad=margin(absolute.value,y.size())*(1+q);
  return {lower.value-pad,upper.value+pad};
}
inline Range locations(span y,double p,double q,double held,int maxit,Range enclosure={NAN,NAN}) {
  if(std::isfinite(held))return {held,held};
  const auto mm=std::minmax_element(y.begin(),y.end());
  const double tolerance=std::max(2e-13,std::min(1e-4,(q-p)*.02));
  double a=*mm.first,b=*mm.second;
  // A parent shape interval encloses the location for every child shape.
  // Short budgets retain the cold-start path, including its stop semantics.
  if(solver_budget(maxit)>=std::numeric_limits<double>::digits && std::isfinite(enclosure.lo) && std::isfinite(enclosure.hi) && enclosure.lo<=enclosure.hi) {
    a=std::max(a,enclosure.lo);b=std::min(b,enclosure.hi);
  }
  for(std::int64_t k=0;k<solver_budget(maxit) && b-a>tolerance;++k) {
    const double c=a/2+b/2;
    const auto g=location_score(y,c,p,q);
    double next_a=a,next_b=b;
    if(g.hi<0)next_a=c;
    if(g.lo>0)next_b=c;
    if(p>1) {
      Sum derivative;
      for(double v:y) {
        const double d=v<a?a-v:(v>b?v-b:0);
        const double e=std::max(std::abs(v-a),std::abs(v-b));
        double lower=0;
        if(d>0)lower=std::min({std::pow(d,p-2),std::pow(d,q-2),std::pow(e,p-2),std::pow(e,q-2)});
        else if(q<=2 && e>0)lower=std::min(std::pow(e,p-2),std::pow(e,q-2));
        derivative.add(lower);
      }
      const double lower=(p-1)*std::max(0.,derivative.value-margin(derivative.value,y.size())*(1+q));
      if(lower>0 && std::isfinite(lower)) {
        next_a=std::max(next_a,c-std::max(0.,g.hi)/lower);
        next_b=std::min(next_b,c+std::max(0.,-g.lo)/lower);
      }
    }
    if(next_a>next_b)return {a,b};
    if(next_b-next_a<.9*(b-a)){a=next_a;b=next_b;continue;}
    if(p>1) {
      // A symmetric sample can make the central score ambiguous while the
      // whole-location Hessian lower bound is zero. Probe both sides before
      // accepting that wide enclosure.
      const double left=a/2+c/2,right=c/2+b/2;
      if(location_score(y,left,p,q).hi<0)next_a=std::max(next_a,left);
      if(location_score(y,right,p,q).lo>0)next_b=std::min(next_b,right);
      if(next_b-next_a<.9*(b-a)){a=next_a;b=next_b;continue;}
      return {next_a,next_b};
    }
    // At p=1 the subgradient may have a median interval. Search its two
    // enclosing endpoints independently instead of dividing by p-1.
    double left=a,right=b;
    for(std::int64_t j=0;j<solver_budget(maxit) && right-left>tolerance;++j) {
      const double mid=left/2+right/2;
      if(location_score(y,mid,p,q).hi<0)left=mid;else right=mid;
    }
    const double lower=left;left=a;right=b;
    for(std::int64_t j=0;j<solver_budget(maxit) && right-left>tolerance;++j) {
      const double mid=left/2+right/2;
      if(location_score(y,mid,p,q).lo>0)right=mid;else left=mid;
    }
    return {lower,right};
  }
  return {a,b};
}
inline ShapeBox score_box(span y,double p,double q,double mean,double sd,int maxit,Range enclosure={NAN,NAN}) {
  ShapeBox out;out.location=locations(y,p,q,mean,maxit,enclosure);
  if(!(p>=1 && q>=p) || !std::isfinite(q+out.location.lo+out.location.hi))return out;
  const Range psi1{special::digamma(1+1/q),special::digamma(1+1/p)};
  Range logb{0,0},B{0,0};
  if(std::isfinite(sd)) {
    const Range psi3{special::digamma(1+3/q),special::digamma(1+3/p)};
    B=add(mul({1.5,1.5},psi3),mul({-.5,-.5},psi1));
    const double c=p/2+q/2,t=1/c;
    const double at=std::log(sd)+log3/2+(special::lgamma(1+t)-special::lgamma(1+3*t))/2;
    const double spread=std::max(std::abs(B.lo),std::abs(B.hi))*std::max(1/p-t,t-1/q);
    const double pad=margin(std::abs(at)+spread,y.size())*(1+q);
    logb={at-spread-pad,at+spread+pad};out.logscale={std::log(sd)-pad,std::log(sd)+pad};
  }
  Sum lowerT,upperT,lowerU,upperU,lowerV,upperV,lowerH,upperH,lowerG,upperG;
  bool finiteH=true;
  std::vector<ResidualPowers> residuals;residuals.reserve(y.size());
  const double inverse_upper_scale=std::exp(-logb.hi),inverse_lower_scale=std::exp(-logb.lo);
  for(double v:y) {
    const double a=v<out.location.lo?out.location.lo-v:(v>out.location.hi?v-out.location.hi:0);
    const double b=std::max(std::abs(v-out.location.lo),std::abs(v-out.location.hi));
    const double ra=a*inverse_upper_scale,rb=b*inverse_lower_scale;
    if(!std::isfinite(rb))return out;
    residuals.emplace_back(ra,rb,p,q);
    const auto t=residuals.back().value(),u=residuals.back().logmoment(p,1);
    lowerT.add(t.lo);upperT.add(t.hi);lowerU.add(u.lo);upperU.add(u.hi);

  }
  const double n=static_cast<double>(y.size());
  const double pad=margin(upperT.value+std::max(std::abs(lowerU.value),std::abs(upperU.value)),n)*(1+q);
  const Range T{std::max(0.,lowerT.value-pad),upperT.value+pad},U{lowerU.value-pad,upperU.value+pad};
  if(std::isnan(sd)) {
    if(!(T.lo>0) || !std::isfinite(T.hi))return out;
    const Range D=add(mul({p,q},divide(U,T)),{-std::log(T.hi/n),-std::log(T.lo/n)});
    out.score=add(add(psi1,{std::log(p),std::log(q)}),neg(D));
    // sigma = R_p * p^(1/p) / sqrt(Gamma(1/p)/Gamma(3/p)).
    const Range lr=divide({std::log(T.lo/n),std::log(T.hi/n)},{p,q});
    const double c=p/2+q/2,t=1/c;
    const double at=log3/2+(special::lgamma(1+t)-special::lgamma(1+3*t))/2;
    const Range psi3{special::digamma(1+3/q),special::digamma(1+3/p)};
    const Range bd=add(mul({1.5,1.5},psi3),mul({-.5,-.5},psi1));
    const double spread=std::max(std::abs(bd.lo),std::abs(bd.hi))*(1/p-1/q);
    out.logscale=add(add(lr,divide({std::log(p),std::log(q)},{p,q})),{-at-spread,-at+spread});
  } else {
    // p^2 ell'/n = psi(1+1/p)-B + p B mean(z^p)
    //                       -p^2 mean(z^p log z), z=|x-m|/b(p).
    out.score=add(add(psi1,neg(B)),add(mul(mul({p,q},B),{T.lo/n,T.hi/n}),neg(mul({p*p,q*q},{U.lo/n,U.hi/n}))));
  }
  if(p>1 && T.lo>0 && out.score.lo<=0 && out.score.hi>=0) {
    for(std::size_t j=0;j<y.size();++j) {
      const double v=y[j];
      const auto& cached=residuals[j];
      const double ra=cached.a,rb=cached.b;
      const auto v2=cached.logmoment(p,2),hh=residual_hessian(ra,rb,p,q);
      auto gg=powerlog(ra,rb,p-1,q-1);
      if(v>out.location.hi)gg=neg(gg);
      else if(v>=out.location.lo){const double size=std::max(std::abs(gg.lo),std::abs(gg.hi));gg={-size,size};}
      lowerV.add(v2.lo);upperV.add(v2.hi);lowerH.add(hh.lo);
      if(std::isfinite(hh.hi))upperH.add(hh.hi);else finiteH=false;
      lowerG.add(gg.lo);upperG.add(gg.hi);
    }

    const double padding=margin(upperV.value+upperH.value+std::max(std::abs(lowerG.value),std::abs(upperG.value)),n)*(1+q);
    const Range V{std::max(0.,lowerV.value-padding),upperV.value+padding};
    const Range H{std::max(0.,lowerH.value-padding),finiteH?upperH.value+padding:INFINITY};
    const Range G=std::isfinite(mean)?Range{0,0}:Range{lowerG.value-padding,upperG.value+padding};
    const Range invp{1/q,1/p},invp2{1/(q*q),1/(p*p)};
    const Range tri1{special::trigamma(1+1/p),special::trigamma(1+1/q)};
    if(H.lo>0) {
      if(std::isnan(sd)) {
        const auto ratio=divide(U,T);
        auto variance=add(divide(V,T),neg(square(ratio)));variance.lo=std::max(0.,variance.lo);variance.hi=std::max(0.,variance.hi);
        const auto correction=divide(mul({p*p,q*q},square(G)),mul(mul({p-1,q-1},T),H));
        out.derivative=add(add(invp,neg(mul(tri1,invp2))),add(neg(mul({p,q},variance)),correction));
      } else {
        const Range tri3{special::trigamma(1+3/p),special::trigamma(1+3/q)};
        const auto E=mul({.5,.5},add(tri1,mul({-9,-9},tri3)));
        auto curvature=add(neg(mul(add(tri1,E),square(invp2))),neg(mul({2,2},mul(add(psi1,neg(B)),mul(invp,invp2)))));
        const auto beta=mul(B,invp);
        auto kernel=add(add(V,neg(mul({2,2},mul(beta,U)))),mul(add(square(beta),neg(mul(E,mul(invp,invp2)))),T));
        curvature=add(curvature,neg(mul({1/n,1/n},kernel)));
        curvature=add(curvature,divide(mul({p/n,q/n},square(G)),mul({p-1,q-1},H)));
        out.derivative=add(mul({p*p,q*q},curvature),mul({2,2},mul(invp,out.score)));
      }
      const double derivative_pad=margin(std::max(std::abs(out.derivative.lo),std::abs(out.derivative.hi)),n)*(1+q);
      out.derivative.lo-=derivative_pad;out.derivative.hi+=derivative_pad;
    }
  }
  const double error=margin(std::max(std::abs(out.score.lo),std::abs(out.score.hi)),n)*(1+q);
  out.score.lo-=error;out.score.hi+=error;
  out.valid=std::isfinite(out.score.lo+out.score.hi);return out;
}
} // namespace ged_shape_detail

// All finite free-shape successes pass here, independent of any proposer.
inline GedResult ged_shape_certificate(span x,double tol,int maxit,double mean,double sd) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  GedResult failed{nan,nan,nan,nan,0,GedStatus::BfgsFailure};
  if(!(tol>0 && std::isfinite(tol)) || maxit<1 || !ged_shape_detail::input(x,mean,sd,1)) {
    failed.status=GedStatus::InvalidStart;return failed;
  }
  if(std::isfinite(sd)) {
    const auto moment=ged_shape_detail::moment(x,1,mean,maxit);
    const double threshold=2.25-ged_shape_detail::euler-ged_shape_detail::log2/2;
    if(moment.valid && moment.log_lower-std::log(sd)>threshold+
        ged_shape_detail::margin(std::abs(std::log(sd))+std::abs(moment.log_lower),x.size()))
      return ged_mle_impl(x,tol,maxit,mean,sd,NAN);
  }
  const auto mm=std::minmax_element(x.begin(),x.end());
  if(*mm.first==*mm.second && std::isnan(sd) && (std::isnan(mean) || mean==x[0]))
    return {x[0],0,nan,INFINITY,0,GedStatus::InvalidStart};
  if(*mm.first==*mm.second && (std::isnan(sd) || std::isnan(mean) || mean==x[0])) return ged_mle_impl(x,tol,maxit,mean,sd,NAN);
  const double origin=std::isfinite(mean)?mean:*mm.first/2+*mm.second/2;
  const double unit=std::max(std::abs(*mm.first-origin),std::abs(*mm.second-origin));
  if(!(unit>=std::numeric_limits<double>::min()) || !std::isfinite(unit))return failed;
  std::vector<double> normalized;normalized.reserve(x.size());
  for(double v:x)normalized.push_back((v-origin)/unit);
  const span y{normalized.data(),normalized.size()};
  const double m=std::isfinite(mean)?0:nan,s=std::isfinite(sd)?sd/unit:nan;
  if(std::isfinite(sd) && !(s>0 && std::isfinite(s)))return failed;
  const double n=static_cast<double>(x.size()),offset=n*std::log(unit);
  const auto cap=certificate_budget(maxit);std::int64_t work=std::isfinite(sd)?2:1;
  const auto evaluate=[&](double p){return ged_detail::conditional(y,p,m,s,maxit);};
  auto best=evaluate(1.);std::vector<GedResult> roots;
  if(best.status!=GedStatus::Ok || !std::isfinite(best.logLik))return failed;
  const auto actual_loglik=[&](double location,double scale,double shape) {
    if(!std::isfinite(location) || !(scale>0 && std::isfinite(scale)) || !std::isfinite(shape))return nan;
    return ged_detail::conditional_loglik(x,location,scale,shape);
  };
  const auto restore=[&](GedResult r) {
    r.mu=std::isfinite(mean)?mean:origin+unit*r.mu;
    r.sigma=std::isfinite(sd)?sd:unit*r.sigma;
    r.logLik=actual_loglik(r.mu,r.sigma,r.nu);return r;
  };
  // Cache an original-coordinate witness before searching. A stop never
  // attaches the optimized normalized likelihood to rounded coefficients.
  auto witness=restore(best);
  if(!std::isfinite(witness.logLik)) {
    witness={std::isfinite(mean)?mean:origin,std::isfinite(sd)?sd:unit,2,nan,0,GedStatus::Ok};
    witness.logLik=actual_loglik(witness.mu,witness.sigma,2);
  }
  if(!std::isfinite(witness.logLik)) {
    failed.status=GedStatus::TransformFailure;failed.search_failed=true;failed.stop_detail.loglik_upper=INFINITY;return failed;
  }
  const auto transform_valid=[&](const GedResult& normalized_result,const GedResult& original_result) {
    if(!std::isfinite(original_result.mu) || !(original_result.sigma>0 && std::isfinite(original_result.sigma)) ||
        !std::isfinite(original_result.logLik))return false;
    if(std::isnan(mean) && std::abs((original_result.mu-origin)/unit-normalized_result.mu)>2e-8)return false;
    if(std::isnan(sd) && std::abs(std::log(original_result.sigma/unit)-std::log(normalized_result.sigma))>2e-8)return false;
    const double expected=normalized_result.logLik-offset;
    const double pad=ged_shape_detail::likelihood_margin(std::abs(original_result.logLik)+std::abs(expected),n,original_result.sigma)*(1+normalized_result.nu);
    return std::abs(original_result.logLik-expected)<=pad;
  };
  auto stop=[&](bool budget,double upper) {
    auto r=restore(best);
    if(!std::isfinite(r.logLik))r=witness;
    r.status=GedStatus::BfgsFailure;r.certified=false;
    r.stop_detail={};r.stop_detail.budget_exhausted=budget;
    const double pad=ged_shape_detail::likelihood_margin(std::abs(r.logLik),n)*(1+best.nu);
    r.stop_detail.candidates.push_back({{r.mu,r.sigma,r.nu},r.logLik-pad,r.logLik+pad});
    r.stop_detail.loglik_lower=r.logLik-pad;
    r.stop_detail.loglik_upper=std::isfinite(upper)?upper-offset+
      ged_shape_detail::likelihood_margin(std::abs(upper)+std::abs(offset),n)*(1+best.nu):INFINITY;
    return r;
  };
  const auto transform_stop=[&]() {
    auto r=stop(false,INFINITY);r.status=GedStatus::TransformFailure;r.search_failed=true;return r;
  };
  // Cheap witnesses only. Completeness comes from the score and tail cover.
  for(double p:{2.,4.}) {
    if(work>=cap)return stop(true,INFINITY);
    ++work;
    const auto r=evaluate(p);if(r.status==GedStatus::Ok && r.logLik>best.logLik)best=r;
  }
  double cutoff=2;
  const double face=std::isnan(sd)?-n*ged_shape_detail::log2:-INFINITY;
  double tail_upper=INFINITY;
  bool tail_face=false;
  for(;;) {
    if(work>=cap)return stop(true,INFINITY);
    ++work;
    const auto bound=ged_shape_detail::tail(y,cutoff,m,s,maxit);
    const double pad=ged_shape_detail::likelihood_margin(std::abs(best.logLik),n)*(1+cutoff);
    if(bound.valid && bound.upper<best.logLik-pad){tail_upper=bound.upper;break;}
    if(std::isnan(sd) && cutoff>=n) {tail_upper=face;tail_face=true;break;}
    if(!std::isfinite(cutoff*2)) {
      if(work+2>cap)return stop(true,INFINITY);
      work+=2;
      const auto interior=ged_shape_detail::interval(y,1,2,m,s,maxit);
      const auto beyond=ged_shape_detail::tail(y,2,m,s,maxit);
      auto r=stop(false,interior.valid && beyond.valid?std::max(interior.upper,beyond.upper):INFINITY);
      if(std::isfinite(sd))r.status=GedStatus::UniformUnresolved;
      return r;
    }
    cutoff*=2;
  }
  struct Segment {double lo,hi;ged_shape_detail::Range location{NAN,NAN};};
  std::vector<Segment> pending{{1,cutoff}},retained;
  double cover_upper=tail_upper;
  while(!pending.empty()) {
    if(work>=cap)return stop(true,INFINITY);
    const auto cell=pending.back();pending.pop_back();++work;
    const auto box=ged_shape_detail::score_box(y,cell.lo,cell.hi,m,s,maxit,cell.location);
    // A definite sign excludes stationary points, not the shape-one endpoint.
    if(box.valid && (box.score.lo>0 || box.score.hi<0))continue;
    if(box.valid && box.derivative.hi<0 && cell.lo>1) {
      if(work+2>cap)return stop(true,INFINITY);
      work+=2;
      const auto left=ged_shape_detail::score_box(y,cell.lo,cell.lo,m,s,maxit,box.location);
      const auto right=ged_shape_detail::score_box(y,cell.hi,cell.hi,m,s,maxit,box.location);
      if((left.valid && left.score.hi<0) || (right.valid && right.score.lo>0))continue;
      double a=cell.lo,b=cell.hi;
      bool isolated=false;
      for(std::int64_t k=0;k<solver_budget(maxit);++k) {
        if(work>=cap)return stop(true,INFINITY);
        ++work;
        const double c=a/2+b/2;
        const auto point=ged_shape_detail::score_box(y,c,c,m,s,maxit,box.location);
        if(!point.valid)break;
        if(point.score.lo>0)a=c;
        else if(point.score.hi<0)b=c;
        else {
          a=std::max(a,c-std::max(0.,-point.score.lo)/(-box.derivative.hi));
          b=std::min(b,c+std::max(0.,point.score.hi)/(-box.derivative.hi));
        }
        if(b-a<=2e-8*(1+c)) {isolated=true;break;}
      }
      if(isolated){retained.push_back({a,b});continue;}
      return stop(false,INFINITY);
    }
    const auto bound=ged_shape_detail::interval(y,cell.lo,cell.hi,m,s,maxit,box.location.lo,box.location.hi);
    const double incumbent=tail_face?std::max(best.logLik,face):best.logLik;
    const double pad=ged_shape_detail::likelihood_margin(std::abs(incumbent),n)*(1+cell.hi);
    if(bound.valid && bound.upper<incumbent-pad)continue;
    const double mid=cell.lo/2+cell.hi/2;
    if(mid==cell.lo || mid==cell.hi)return stop(false,INFINITY);
    if(box.valid && cell.hi-cell.lo<=2e-10*(1+mid) &&
        box.location.hi-box.location.lo<=2e-9 && box.logscale.hi-box.logscale.lo<=2e-9) {
      retained.push_back(cell);continue;
    }
    // A single certificate evaluation also supplies a feasible witness.
    const auto r=evaluate(mid);
    if(r.status==GedStatus::Ok && std::isfinite(r.logLik) && r.logLik>best.logLik)best=r;
    pending.push_back({mid,cell.hi,box.location});pending.push_back({cell.lo,mid,box.location});
  }
  std::sort(retained.begin(),retained.end(),[](const Segment&a,const Segment&b){return a.lo<b.lo;});
  std::vector<Segment> clusters;
  for(const auto& cell:retained) {
    if(!clusters.empty() && cell.lo<=clusters.back().hi)clusters.back().hi=cell.hi;
    else clusters.push_back(cell);
  }
  roots.push_back(evaluate(1.));
  std::vector<double> uppers;
  uppers.push_back(roots.back().logLik+ged_shape_detail::likelihood_margin(std::abs(roots.back().logLik),n)*2);
  for(const auto& cluster:clusters) {
    if(work>=cap)return stop(true,INFINITY);
    ++work;
    const auto box=ged_shape_detail::score_box(y,cluster.lo,cluster.hi,m,s,maxit);
    if(!box.valid || cluster.hi-cluster.lo>2e-7*(1+cluster.lo) ||
        box.location.hi-box.location.lo>2e-7 || box.logscale.hi-box.logscale.lo>2e-7)return stop(false,INFINITY);
    double a=cluster.lo,b=cluster.hi;
    // Refine the representative; the cluster already encloses every possible
    // maximizer and acceptance never relies on this bisection being unimodal.
    for(std::int64_t k=0;k<solver_budget(maxit) && b-a>2e-12*(1+a);++k) {
      if(work>=cap)return stop(true,INFINITY);
      ++work;
      const double c=a/2+b/2;const auto sc=ged_shape_detail::score_box(y,c,c,m,s,maxit,box.location);
      if(!sc.valid)break;
      if(sc.score.lo>0)a=c;else if(sc.score.hi<0)b=c;else break;
    }
    const auto r=evaluate(a/2+b/2);
    if(r.status!=GedStatus::Ok || !std::isfinite(r.logLik))return stop(false,INFINITY);
    const auto bound=ged_shape_detail::interval(y,cluster.lo,cluster.hi,m,s,maxit,box.location.lo,box.location.hi);
    if(!bound.valid)return stop(false,INFINITY);
    roots.push_back(r);uppers.push_back(bound.upper);
  }
  std::size_t winner=0;
  for(std::size_t j=0;j<roots.size();++j)if(roots[j].logLik>roots[winner].logLik)winner=j;
  best=roots[winner];const double tie=1e-8*n;
  const double pad=ged_shape_detail::likelihood_margin(std::abs(best.logLik),n)*(1+best.nu);
  cover_upper=std::max(cover_upper,uppers[winner]);
  if(!tail_face && tail_upper>=best.logLik-pad)return stop(false,cover_upper);
  if(tail_face && face>=best.logLik-pad-tie) {
    double finite_upper=-INFINITY;for(double u:uppers)finite_upper=std::max(finite_upper,u);
    auto r=restore(best);r.status=GedStatus::UniformBoundary;r.certified=true;
    std::vector<double> endpoints;
    if(std::isfinite(mean)) {
      double far=x[0];for(double v:x)if(std::abs(v-mean)>std::abs(far-mean))far=v;
      const double other=mean+(mean-far);endpoints={std::min(far,other),std::max(far,other)};
    } else endpoints={*mm.first,*mm.second};
    // Rounded residual ties do not certify the original uniform support.
    if(!std::isfinite(endpoints[0]) || !std::isfinite(endpoints[1]) ||
        !std::all_of(x.begin(),x.end(),[&](double v){return endpoints[0]<=v && v<=endpoints[1];}))return transform_stop();
    if(!(finite_upper<face-ged_shape_detail::likelihood_margin(std::abs(face),n)*(1+cutoff))) {
      const auto finite=restore(best);
      if(!transform_valid(best,finite))return transform_stop();
      r.status=GedStatus::Nonunique;r.certified=false;
      r.stop_detail=comparison_detail({endpoints,face-offset-pad,face-offset+pad,StopModel::Unif},
        {{finite.mu,finite.sigma,finite.nu},finite.logLik-pad,finite.logLik+pad});
      r.stop_detail.loglik_upper=std::max(face,finite_upper)-offset;return r;
    }
    r.logLik=face-offset;r.stop_detail=limit_detail(endpoints,r.logLik,StopModel::Unif);
    if(r.stop_detail.limit_coef.empty())return stop(false,INFINITY);
    r.stop_detail.loglik_lower=r.logLik-pad;r.stop_detail.loglik_upper=r.logLik+pad;return r;
  }
  const auto original_best=restore(best);
  if(!transform_valid(best,original_best))return transform_stop();
  // A root cluster touching one is the same attained endpoint candidate.
  for(std::size_t j=0;j<roots.size();++j)if(j!=winner) {
    if((j==0 && winner>0 && clusters[winner-1].lo==1) ||
        (winner==0 && j>0 && clusters[j-1].lo==1))continue;
    cover_upper=std::max(cover_upper,uppers[j]);
    if(uppers[j]>=best.logLik-pad-tie) {
      auto r=stop(false,cover_upper);r.status=GedStatus::Nonunique;
      const auto a=original_best,b=restore(roots[j]);
      if(!transform_valid(roots[j],b))return transform_stop();
      r.stop_detail=comparison_detail({{a.mu,a.sigma,a.nu},a.logLik-pad,a.logLik+pad},
        {{b.mu,b.sigma,b.nu},b.logLik-pad,b.logLik+pad});return r;
    }
  }
  if(best.nu==1 && std::isnan(mean)) {
    // Normalization can merge distinct observations. The original median
    // interval remains a continuum of exact Laplace maximizers.
    std::vector<double> sorted(x.begin(),x.end());std::sort(sorted.begin(),sorted.end());
    const double lo=sorted[(sorted.size()-1)/2],hi=sorted[sorted.size()/2];
    if(lo<hi) {
      const auto normalized_a=ged_detail::conditional(y,1,(lo-origin)/unit,s,maxit);
      const auto normalized_b=ged_detail::conditional(y,1,(hi-origin)/unit,s,maxit);
      auto a=restore(normalized_a),b=restore(normalized_b);
      a.mu=lo;a.logLik=actual_loglik(a.mu,a.sigma,1);
      b.mu=hi;b.logLik=actual_loglik(b.mu,b.sigma,1);
      if(!transform_valid(normalized_a,a) || !transform_valid(normalized_b,b))return transform_stop();
      const double tied_pad=ged_shape_detail::likelihood_margin(std::max(std::abs(a.logLik),std::abs(b.logLik))+std::abs(offset),n)*2;
      auto r=stop(false,cover_upper);r.status=GedStatus::Nonunique;r.proved_tie=true;
      r.stop_detail=comparison_detail({{a.mu,a.sigma,1},a.logLik-tied_pad,a.logLik+tied_pad},
        {{b.mu,b.sigma,1},b.logLik-tied_pad,b.logLik+tied_pad});
      r.stop_detail.loglik_upper=cover_upper-offset+tied_pad;return r;
    }
  }
  auto r=original_best;
  const double original_pad=ged_shape_detail::likelihood_margin(std::abs(r.logLik)+std::abs(offset),n,r.sigma)*(1+best.nu);
  r.certified=true;r.status=GedStatus::Ok;
  r.stop_detail.loglik_lower=r.logLik-original_pad;r.stop_detail.loglik_upper=cover_upper-offset+original_pad;
  r.iterations=static_cast<int>(work);return r;
}

// Fixed positive shape: strict convexity above one, the median face at one,
// and exhaustive observation-knot minimization below one. This wrapper only
// certifies the existing conditional solution and makes its ties explicit.
inline GedResult ged_conditional_certificate(span x,double tol,int maxit,double mean,double sd,double nu) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  if(!(tol>0 && std::isfinite(tol)) || maxit<1 || !ged_shape_detail::input(x,mean,sd,1))return {nan,nan,nu,nan,0,GedStatus::InvalidStart};
  if(!(nu>0 && std::isfinite(nu)))return {nan,nan,nu,nan,0,GedStatus::InvalidShape};
  for(double v:x)if(!std::isfinite(v))return {nan,nan,nu,nan,0,GedStatus::InvalidStart};
  const auto n=static_cast<std::int64_t>(x.size());
  const bool constant=std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];});
  if(constant && std::isnan(sd) && (std::isnan(mean) || mean==x[0]))
    return {x[0],0,nu,INFINITY,0,GedStatus::InvalidStart};
  // A numerical loss is not a scale-collapse proof. In particular, never let
  // an underflowed computed scale reach the binding's sigma==0 shortcut.
  auto numerical=[&]() {
    GedResult r{nan,nan,nu,nan,0,GedStatus::TransformFailure};
    r.search_failed=true;
    r.stop_detail.loglik_upper=INFINITY;return r;
  };
  const double lambda=dens::ged_lambda(nu);
  if(!(lambda>=std::numeric_limits<double>::min() && std::isfinite(lambda)))return numerical();
  if(ged_shape_detail::margin(1,x.size())*(1+nu)>1)return numerical();
  if(nu<1 && (std::isnan(mean) || std::isnan(sd)) && !constant) {
    std::vector<double> sorted(x.begin(),x.end());std::sort(sorted.begin(),sorted.end());
    double radius=0,gap=INFINITY;
    if(std::isnan(mean)) {
      radius=sorted.back()-sorted.front();
      for(std::size_t j=1;j<sorted.size();++j)if(sorted[j]>sorted[j-1])gap=std::min(gap,sorted[j]-sorted[j-1]);
    } else for(double v:x) {
      const double d=std::abs(v-mean);radius=std::max(radius,d);if(d>0)gap=std::min(gap,d);
    }
    if(!std::isfinite(radius) || !(gap/radius>=std::numeric_limits<double>::min()))return numerical();
  }
  auto reliable_point=[&](const GedResult& r) {
    if(!(r.sigma>0 && std::isfinite(r.sigma) && std::isfinite(r.logLik)))return false;
    if(nu<1)for(double v:x)if(v!=r.mu) {
      const double z=std::abs(v-r.mu)/r.sigma;
      if(!(z>=std::numeric_limits<double>::min() && std::isfinite(z) &&
           z/lambda>=std::numeric_limits<double>::min() && std::isfinite(z/lambda)))return false;
    }
    return true;
  };
  if(nu<1 && std::isnan(mean) && !constant && n>enumeration_budget(maxit)/(2*n)) {
    auto r=ged_detail::conditional(x,nu,x[0],sd,maxit);
    if(!reliable_point(r))return numerical();
    r.status=GedStatus::BfgsFailure;
    r.stop_detail.budget_exhausted=true;r.stop_detail.loglik_upper=INFINITY;
    if(std::isfinite(r.logLik)) {
      const double pad=ged_shape_detail::likelihood_margin(std::abs(r.logLik),x.size(),r.sigma,nu);
      r.stop_detail.loglik_lower=r.logLik-pad;
      r.stop_detail.candidates.push_back({{r.mu,r.sigma,r.nu},r.logLik-pad,r.logLik+pad});
    }
    return r;
  }
  auto r=ged_detail::conditional(x,nu,constant && std::isnan(mean)?x[0]:mean,sd,maxit);
  if(r.status!=GedStatus::Ok) {
    if(r.sigma==0)return numerical();
    return r;
  }
  // Fractional powers amplify residuals lost to underflow in the density.
  if(!reliable_point(r))return numerical();
  if(std::isnan(sd)) {
    double radius=0;for(double v:x)radius=std::max(radius,std::abs(v-r.mu));
    ged_shape_detail::Sum powers;
    for(double v:x)powers.add(std::pow(std::abs(v-r.mu)/radius,nu));
    const double a=std::log(radius),b=(std::log(nu)+std::log(powers.value)-std::log(double(n)))/nu;
    const double c=dens::ged_logscale(nu);
    const double error=std::abs(std::log(r.sigma)-(a+b-c));
    const double pad=ged_shape_detail::margin(std::abs(a)+std::abs(b)+std::abs(c),x.size())*(1+1/nu);
    if(!std::isfinite(error) || pad>2e-8 || error>2e-8+pad || nu*(error+pad)>1)return numerical();
  }
  if(!std::isfinite(r.mu+r.logLik) || !(r.sigma>0 && std::isfinite(r.sigma)))return numerical();
  if(std::isnan(mean) && nu>1) {
    double radius=0;for(double v:x)radius=std::max(radius,std::abs(v-r.mu));
    if(radius>0 && std::isfinite(radius)) {
      std::vector<double> residuals;residuals.reserve(x.size());
      for(double v:x)residuals.push_back((v-r.mu)/radius);
      const auto score=ged_shape_detail::location_score({residuals.data(),residuals.size()},0,nu,nu);
      if(!(score.lo<=0 && score.hi>=0)) {
        r.status=GedStatus::TransformFailure;r.stop_detail.loglik_upper=INFINITY;
        const double pad=ged_shape_detail::likelihood_margin(std::abs(r.logLik),x.size(),r.sigma,nu)*(1+nu);
        r.stop_detail.loglik_lower=r.logLik-pad;
        r.stop_detail.candidates.push_back({{r.mu,r.sigma,nu},r.logLik-pad,r.logLik+pad});return r;
      }
    }
  }
  if(std::isnan(mean) && nu<=1) {
    auto sorted=std::vector<double>(x.begin(),x.end());std::sort(sorted.begin(),sorted.end());
    double other=nan;
    if(nu==1) {
      if(sorted[(sorted.size()-1)/2]<sorted[sorted.size()/2])other=sorted[(sorted.size()-1)/2];
    } else if(sorted.front()<sorted.back()) {
      const double radius=sorted.back()-sorted.front();
      if(!std::isfinite(radius)){r.status=GedStatus::BfgsFailure;return r;}
      ged_shape_detail::Sum optimum;
      for(double v:x)optimum.add(std::pow(std::abs(v-r.mu)/radius,nu));
      for(std::size_t j=0;j<sorted.size();++j)if(sorted[j]!=r.mu && (!j || sorted[j]!=sorted[j-1])) {
        ged_shape_detail::Sum candidate;
        for(double v:x)candidate.add(std::pow(std::abs(v-sorted[j])/radius,nu));
        if(std::abs(candidate.value-optimum.value)<=ged_shape_detail::margin(candidate.value+optimum.value,x.size())*(1+nu)) {other=sorted[j];break;}
      }
    }
    if(std::isfinite(other)) {
      const auto b=ged_detail::conditional(x,nu,other,sd,maxit);
      const double pad=ged_shape_detail::likelihood_margin(std::max(std::abs(r.logLik),std::abs(b.logLik)),x.size(),r.sigma,nu)*(1+nu);
      r.status=GedStatus::Nonunique;
      r.proved_tie=nu==1;
      if(nu<1) {
        // Error-free TwoSum distinguishes exact reflection from rounded
        // equal sums, including when extended intermediates are doubles.
        const auto sum=[](double a,double b) {
          const double hi=a+b,z=hi-a;
          return std::array<double,2>{{hi,(a-(hi-z))+(b-z)}};
        };
        const auto center=sum(r.mu,other);
        r.proved_tie=std::isfinite(center[0]);
        for(std::size_t j=0;j<sorted.size();++j) {
          const auto pair=sum(sorted[j],sorted[sorted.size()-1-j]);
          if(pair!=center)r.proved_tie=false;
        }
        // Reflection pairs every noncentral minimizer. A central knot
        // could instead be the unique winner, so exclude it separately.
        if(r.proved_tie)for(double c:sorted)if(sum(c,c)==center) {
          ged_shape_detail::Sum central;
          const double radius=sorted.back()-sorted.front();
          for(double v:x)central.add(std::pow(std::abs(v-c)/radius,nu));
          ged_shape_detail::Sum selected;
          for(double v:x)selected.add(std::pow(std::abs(v-r.mu)/radius,nu));
          if(!(central.value>selected.value+ged_shape_detail::margin(central.value+selected.value,x.size())*(1+nu)))r.proved_tie=false;
          break;
        }
      }
      r.stop_detail=comparison_detail({{r.mu,r.sigma,nu},r.logLik-pad,r.logLik+pad},{{b.mu,b.sigma,nu},b.logLik-pad,b.logLik+pad});
      if(std::abs(r.logLik-b.logLik)>1e-8*x.size()+2*pad)return numerical();
      r.stop_detail.loglik_upper=nu==1?std::max(r.logLik,b.logLik)+pad:INFINITY;return r;
    }
  }
  r.certified=true;return r;
}
