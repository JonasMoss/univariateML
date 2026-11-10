#pragma once
#include <climits>
namespace cppunivariate {
namespace genbeta_allfree_cert {
struct Point {
  double h=0,r=0,d=0,k=0,lower=-INFINITY,upper=INFINITY,slo=-INFINITY,shi=INFINITY;
  genbeta_allfree_bounds::ShapeBox shape{};
  bool ok=false;
};
struct CellBounds {
  genbeta_allfree_bounds::ShapeBox shape{};
  double smin=-INFINITY,smax=INFINITY,curvature=INFINITY;
};
inline genbeta_allfree_bounds::ShapeBox shape_cell(const Point& L,const Point& H,double c,int maxit) {
  const double pad=256*std::numeric_limits<double>::epsilon()*(1+L.r+H.h);
  return genbeta_allfree_bounds::rectangle(L.h-pad,H.h+pad,H.r-pad,L.r+pad,c,maxit);
}
inline CellBounds enclose(const Point& L,const Point& H,double c,int maxit) {
  CellBounds out;out.shape=shape_cell(L,H,c,maxit);
  const auto& b=out.shape;
  const double eps=std::numeric_limits<double>::epsilon();
  if(!b.enclosed)return out;
  const double spad=2048*eps*(1/L.h+b.ph+(1+b.qh)*L.d);
  out.smin=1/H.h-b.ph+(b.ql-1)*(b.ql>=1?H.d:L.d)-spad;
  out.smax=1/L.h-b.pl+(b.qh-1)*(b.qh>=1?L.d:H.d)+spad;
  // Conditional beta information J=[A,-T;-T,B]. The profile curvature
  // is -1/h^2-(q-1)K + (B-2TD+AD^2)/det(J).
  const double tlo=special::trigamma(b.ph+b.qh),thi=special::trigamma(b.pl+b.ql),
    al=special::trigamma(b.ph)-special::trigamma(b.ph+b.ql),
    ah=special::trigamma(b.pl)-special::trigamma(b.pl+b.qh),
    bl=special::trigamma(b.qh)-special::trigamma(b.qh+b.pl),
    bh=special::trigamma(b.ql)-special::trigamma(b.ql+b.ph);
  const double jpad=512*eps*(1+ah+bh+thi+special::trigamma(b.pl)+special::trigamma(b.ql));
  const double detpad=1024*eps*((al+jpad)*(bl+jpad)+(thi+jpad)*(thi+jpad));
  const double det=(al-jpad)*(bl-jpad)-(thi+jpad)*(thi+jpad)-detpad;
  if(al>jpad&&bl>jpad&&det>0) {
    const double npad=2048*eps*(bh+jpad+(ah+jpad)*L.d*L.d+2*(tlo+jpad)*H.d);
    const double numerator=std::max(0.,bh+jpad+(ah+jpad)*L.d*L.d-2*std::max(0.,tlo-jpad)*H.d)+npad;
    out.curvature=-1/(H.h*H.h)-(b.ql-1)*(b.ql>=1?H.k:L.k)+numerator/det;
    out.curvature+=4096*eps*(1/(L.h*L.h)+(1+b.qh)*L.k+numerator/det);
  }
  return out;
}
// The old mesh supplies a feasible proposal only. Every acceptance below is
// based on independently covered tails, cells, root signs and coefficients.
inline GenbetaResult verify(const std::vector<double>& y,double mean,double jac,
    double tol,int maxit,GenbetaResult out) {
  out.allfree_certified=false;
  const double n=static_cast<double>(y.size()),eps=std::numeric_limits<double>::epsilon();
  const double shift=jac-n*std::log(mean),target=std::min(tol,.1)*.25,
    proposal_radius=std::min(tol*.5,.25);
  const auto proposed=std::array<double,3>{{out.shape1,out.shape2,out.shape3*mean}};
  const std::int64_t budget=certificate_budget(maxit);
  std::int64_t work=out.iterations;
  GenbetaCertificateFailure reason=GenbetaCertificateFailure::Arithmetic;
  std::vector<double> v;genbeta_power_cert::Sum logs;
  for(double z:y){const double w=z/mean;v.push_back(w);logs.add(std::log(w));}
  const double c=-static_cast<double>(logs.value/n),cpad=512*eps*(1+std::abs(c));
  std::vector<FitCandidate> roots;
  const double original=out.logLik;
  auto stop=[&](bool exhausted) {
    out.status=GenbetaStatus::SearchUnresolved;
    out.certificate_failure=exhausted?GenbetaCertificateFailure::Budget:reason;
    out.iterations=static_cast<int>(std::min<std::int64_t>(work,INT_MAX));
    out.stop_detail.budget_exhausted=exhausted;
    out.stop_detail.candidates=roots;
    const double pad=2048*eps*(1+std::abs(original)+std::abs(jac));
    const double lower=out.conditional_bounds.enclosed?out.conditional_bounds.loglik_lower:original-pad,
      upper=out.conditional_bounds.enclosed?out.conditional_bounds.loglik_upper:original+pad;
    out.stop_detail.candidates.push_back({{proposed[0],proposed[1],out.shape3,out.scale},lower,upper,StopModel::None});
    out.stop_detail.loglik_lower=lower;out.stop_detail.loglik_upper=INFINITY;
    return out;
  };
  if(!(out.tail_bounds.small_excluded&&out.tail_bounds.large_excluded)) {
    reason=out.tail_bounds.small_excluded?GenbetaCertificateFailure::PowerNeighborhood:
      GenbetaCertificateFailure::GammaNeighborhood;return stop(false);
  }
  double witness=(out.conditional_bounds.loglik_lower-shift)/n;
  const double gamma_upper=(out.tail_bounds.gamma_loglik_upper-shift)/n,
    gamma_domain=out.tail_bounds.small_power_domain,gamma_slope=out.tail_bounds.small_power_slope;
  auto gamma_envelope=[&](double h) {
    double upper=h<=gamma_domain?gamma_upper+gamma_slope*h:INFINITY;
    if(h<=out.tail_bounds.small_quadratic_domain)
      upper=std::min(upper,gamma_upper+out.tail_bounds.small_quadratic_coefficient*h*h);
    return upper;
  };
  auto evaluate=[&](double h) {
    Point z;z.h=h;
    if(++work>budget||!(h>0)||!std::isfinite(h))return z;
    const auto p=genbeta_detail::profile(v,h,NAN,NAN,maxit);
    // The local solve proposes only a tangent point. Its failure is never
    // accepted as an estimate; a feasible exponential tangent can still
    // support independent rectangle coverage and coefficient inclusion.
    const double pp=p.ok?p.a:1/h,qq=p.ok?p.b:1;
    auto b=genbeta_allfree_bounds::conditional(v,h,pp,qq,0,maxit);
    if(b.enclosed&&!p.ok) {
      // A failed local proposer does not force an exponential tangent far
      // from the beta optimum: the validated rectangle supplies a new point.
      const double pc=b.shape1_lower+(b.shape1_upper-b.shape1_lower)/2,
        qc=b.shape2_lower+(b.shape2_upper-b.shape2_lower)/2;
      const auto refined=genbeta_allfree_bounds::conditional(v,h,pc,qc,0,maxit);
      if(refined.enclosed&&refined.loglik_upper<b.loglik_upper)b=refined;
    }
    if(!b.enclosed&&!std::isfinite(gamma_envelope(h))){reason=GenbetaCertificateFailure::ShapeSigns;return z;}
    genbeta_power_cert::Sum r,d,k;
    for(double w:v) {
      const double t=h*w,e=std::exp(-t),den=-std::expm1(-t);
      r.add(-(t<.6931471805599453?std::log(den):std::log1p(-e)));
      d.add(w*e/den);k.add(w*w*e/(den*den));
    }
    z.r=static_cast<double>(r.value/n);z.d=static_cast<double>(d.value/n);z.k=static_cast<double>(k.value/n);
    z.upper=gamma_envelope(h);
    if(b.enclosed) {
      z.shape={b.shape1_lower,b.shape1_upper,b.shape2_lower,b.shape2_upper,true};
      z.lower=b.loglik_lower/n;z.upper=std::min(z.upper,b.loglik_upper/n);
      const double pad=1024*eps*(1/h+z.shape.ph+(1+z.shape.qh)*z.d);
      z.slo=1/h-z.shape.ph+(z.shape.ql-1)*z.d-pad;
      z.shi=1/h-z.shape.pl+(z.shape.qh-1)*z.d+pad;
    }
    z.ok=std::isfinite(z.r+z.d+z.k+z.upper)&&
      (!b.enclosed||std::isfinite(z.lower+z.slo+z.shi));
    if(z.ok&&std::isfinite(z.lower))witness=std::max(witness,z.lower);
    return z;
  };
  struct Cell {Point l,h;};
  Point l=evaluate(out.tail_bounds.small_cutoff);if(!l.ok)return stop(work>budget);
  Point h=evaluate(out.tail_bounds.large_cutoff);if(!h.ok)return stop(work>budget);
  std::vector<Cell> queue{{l,h}};
  std::vector<genbeta_allfree_bounds::ShapeBox> root_shapes;
  std::vector<std::array<double,2>> root_powers;
  while(!queue.empty()) {
    if(work>=budget)return stop(true);
    ++work;const auto cell=queue.back();queue.pop_back();
    const auto& L=cell.l;const auto& H=cell.h;
    const double gamma_cell=gamma_envelope(H.h);
    if(gamma_cell+2048*eps*(1+std::abs(gamma_cell))<witness)continue;
    const auto bounds=enclose(L,H,c+cpad,maxit);
    const auto& b=bounds.shape;
    if(b.enclosed) {
      const double envelope=std::min(L.upper+std::max(0.,bounds.smax)*(H.h-L.h),
        H.upper+std::max(0.,-bounds.smin)*(H.h-L.h));
      if(envelope+2048*eps*(1+std::abs(envelope))<witness)continue;
      if(bounds.smin>0||bounds.smax<0)continue;
    }
    // In a strictly concave cell the score decreases throughout. Signed
    // endpoint scores can exclude the whole cell even when the rectangular
    // score enclosure is loose near an almost-gamma ridge.
    if(bounds.curvature<0&&(L.shi<0||H.slo>0))continue;
    if(bounds.curvature<0&&L.slo>0&&H.shi<0) {
      Point a=L,z=H;bool converged=false;
      genbeta_allfree_bounds::ShapeBox inclusion;
      for(std::int64_t i=0;i<solver_budget(maxit);++i) {
        if(work>=budget)return stop(true);
        const double center=a.h+(z.h-a.h)/2;
        auto m=evaluate(center);if(!m.ok)return stop(work>budget);
        if(m.slo>0)a=m;
        else if(m.shi<0)z=m;
        else {
          // A zero within the allowance is not a root bracket. Independently
          // validate nearby endpoint signs before accepting any inclusion.
          const double radius=std::min(target*.25,(z.h-a.h)/(4*center));
          auto left=evaluate(center*(1-radius)),right=evaluate(center*(1+radius));
          if(!left.ok||!right.ok)return stop(work>budget);
          if(!(left.h>=L.h&&right.h<=H.h&&left.slo>0&&right.shi<0)) {
            reason=GenbetaCertificateFailure::RootSigns;return stop(false);
          }
          a=left;z=right;
        }
        inclusion=shape_cell(a,z,c+cpad,maxit);
        if(inclusion.enclosed&&(z.h-a.h)/a.h<target&&
            (inclusion.ph-inclusion.pl)/inclusion.pl<target&&
            (inclusion.qh-inclusion.ql)/inclusion.ql<target){converged=true;break;}
      }
      if(!converged){reason=GenbetaCertificateFailure::CoefficientInclusion;return stop(false);}
      if(++work>budget)return stop(true);
      const double pc=(inclusion.pl+inclusion.ph)/2,qc=(inclusion.ql+inclusion.qh)/2,
        hc=a.h+(z.h-a.h)/2;
      const auto candidate=genbeta_allfree_bounds::conditional(v,hc,pc,qc,0,maxit);
      if(!candidate.enclosed){reason=GenbetaCertificateFailure::Arithmetic;return stop(false);}
      const double lower=candidate.loglik_lower/n,
        upper=std::min(a.upper+std::max(0.,a.shi)*(z.h-a.h),
          z.upper+std::max(0.,-z.slo)*(z.h-a.h));
      if(!(upper>=lower)){reason=GenbetaCertificateFailure::Arithmetic;return stop(false);}
      roots.push_back({{pc,qc,hc/mean,out.scale},n*lower+shift,n*upper+shift,StopModel::None});
      root_shapes.push_back(inclusion);root_powers.push_back({{a.h,z.h}});
      continue;
    }
    const double mid=H.h/L.h>4?std::sqrt(L.h)*std::sqrt(H.h):L.h+(H.h-L.h)/2;
    if(!(mid>L.h&&mid<H.h)){reason=GenbetaCertificateFailure::Arithmetic;return stop(false);}
    auto m=evaluate(mid);if(!m.ok)return stop(work>budget);
    queue.push_back({m,H});queue.push_back({L,m});
  }
  if(roots.empty())return stop(false);
  std::size_t best=0;
  for(std::size_t i=1;i<roots.size();++i)if(roots[i].loglik_lower>roots[best].loglik_lower)best=i;
  for(std::size_t i=0;i<roots.size();++i)if(i!=best&&roots[i].loglik_upper>=roots[best].loglik_lower) {
    auto result=stop(false);result.stop_detail.comparison_tie=true;return result;
  }
  const auto& b=root_shapes[best];const auto& power=root_powers[best];
  if(!(b.pl>=proposed[0]*(1-proposal_radius)&&b.ph<=proposed[0]*(1+proposal_radius)&&
       b.ql>=proposed[1]*(1-proposal_radius)&&b.qh<=proposed[1]*(1+proposal_radius)&&
       power[0]>=proposed[2]*(1-proposal_radius)&&power[1]<=proposed[2]*(1+proposal_radius))) {
    // The mesh is a proposer. If its coefficients miss the certified box,
    // return the already enclosed root midpoint after evaluating that member.
    const auto& coef=roots[best].coef;
    if(++work>budget)return stop(true);
    const auto checked=genbeta_allfree_bounds::conditional(y,coef[2],coef[0],coef[1],jac,maxit);
    if(!checked.enclosed){reason=GenbetaCertificateFailure::Arithmetic;return stop(false);}
    out.shape1=coef[0];out.shape2=coef[1];out.shape3=coef[2];out.logLik=checked.loglik_value;
    out.conditional_bounds=checked;
  }
  out.iterations=static_cast<int>(std::min<std::int64_t>(work,INT_MAX));
  out.certificate_failure=GenbetaCertificateFailure::None;
  out.allfree_certified=true;
  return out;
}
} // namespace genbeta_allfree_cert
} // namespace cppunivariate
