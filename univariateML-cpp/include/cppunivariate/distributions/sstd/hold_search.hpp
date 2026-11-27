// Conditional Fernandez--Steel fits. Used only by sstd and sged.
#pragma once
#include "../../core/work_budget.hpp"
#include "../snorm/mle.hpp"
#include "../../core/bfgs.hpp"
#include "../../core/finite_diff.hpp"
#include "../../core/tie_collapse.hpp"

namespace cppunivariate {
namespace skew_hold_detail {
constexpr double inf = std::numeric_limits<double>::infinity();
constexpr double pi = 3.14159265358979323846;

// Ordinary t scale, or exp(-|z|^nu) scale for GED. Working in these
// coordinates avoids cancellation between sd -> infinity and nu -> 2.
inline double moment(bool student, double nu) {
  if (student) {
    return dens::std_abs_moment(nu)*std::sqrt(nu/(nu-2));
  }
  return std::exp(special::ged_log_gamma_ratio(nu,2));
}
inline double variance(bool student, double nu) {
  return student ? nu/(nu-2) : std::exp(special::ged_log_gamma_ratio(nu,3));
}
inline double logmode(bool student, double nu) {
  if (!student) return nu>=1 ? -std::log(2.0)-special::lgamma1p(1/nu) : std::log(nu)-std::log(2.0)-std::lgamma(1/nu);
  return special::log_t_mode(nu);
}
inline double kernel(bool student, double nu, double w) {
  if(student) return -0.5*(nu+1)*std::log1p(w*w/nu);
  return -std::pow(std::abs(w),nu);
}
inline double density_cost(span x, bool student, double c, double a, double b, double nu) {
  if(!(a>0 && b>0 && std::isfinite(a+b) && std::isfinite(c))) return inf;
  double f=x.size()*(std::log((a+b)/2)-logmode(student,nu));
  for(double v:x) f-=kernel(student,nu,(v-c)/(v<c ? a : b));
  return std::isfinite(f) ? f : inf;
}

// Finite local brackets only: impossible support faces must not trigger
// golden searches in every all-infinite grid interval.
template <typename Objective>
inline double scan(Objective objective, double lo, double hi, int cells,int maxit=200) {
  std::vector<double> values;
  for(int i=0;i<=cells;++i) values.push_back(objective(lo+(hi-lo)*i/cells));
  int best=0;
  for(int i=1;i<=cells;++i) if(values[i]<values[best]) best=i;
  double arg=lo+(hi-lo)*best/cells,value=values[best];
  for(int i=1;i<cells;++i) if(std::isfinite(values[i]) && values[i]<=values[i-1] && values[i]<=values[i+1]) {
    const double t=snorm_detail::minimize(objective,lo+(hi-lo)*(i-1)/cells,lo+(hi-lo)*(i+1)/cells,maxit);
    if(!std::isfinite(t))return NAN;
    const double f=objective(t);
    if(f<value) { value=f; arg=t; }
  }
  return arg;
}

struct Fit { std::vector<double> theta; double f=inf; bool ok=false; int iterations=0; };
// Optimizer interface uses the core's Cost concept; ownership stays in vectors.
template <typename Objective> struct Cost {
  Objective objective;
  int dimensions;
  int dim() const { return dimensions; }
  double operator()(const double *p) const {
    std::vector<double> v;
    for(int i=0;i<dimensions;++i) v.push_back(p[i]);
    return objective(v);
  }
};
template <typename Objective>
inline Fit solve(Objective objective, const std::vector<std::vector<double>> &starts,
                 const std::vector<double> &lower, const std::vector<double> &upper,
                 double tol, int maxit) {
  maxit = static_cast<int>(solver_budget(maxit));
  Fit best;
  if(starts.empty()) return best;
  const int d=static_cast<int>(lower.size());
  if(d==0) return {starts.front(),objective(starts.front()),true,0};
  auto bounded=[&](const std::vector<double> &v) {
    for(int i=0;i<d;++i) if(!std::isfinite(v[i]) || v[i]<lower[i] || v[i]>upper[i]) return inf;
    return objective(v);
  };
  if(d==1) {
    auto scalar=[&](double t) { return bounded(std::vector<double>{t}); };
    const double t=scan(scalar,lower[0],upper[0],256);
    return {{t},scalar(t),std::isfinite(scalar(t)) && t>lower[0]+1e-7 && t<upper[0]-1e-7,0};
  }
  Cost<decltype(bounded)> cost{bounded,d};
  finite_diff_cost<Cost<decltype(bounded)>> fd{cost};
  BfgsControl ctl; ctl.grad_tol=tol; ctl.maxit=maxit;
  for(const auto &start:starts) {
    auto r=bfgs_minimize(fd,start.data(),ctl);
    if(!std::isfinite(r.f) || r.theta.size()!=lower.size()) continue;
    double norm=0;
    std::vector<double> score(d);
    fd.gradient(r.theta.data(),score.data());
    for(double g:score) norm+=g*g;
    const bool ok=r.status==BfgsStatus::Ok || (std::isfinite(norm) && norm<1e-6);
    if(r.f<best.f) best={r.theta,r.f,ok,r.iterations};
  }
  // Independent simplex rescue for nondifferentiable Laplace kinks.
  // Accept only after both simplex diameter and objective spread converge.
  if(!best.ok && !best.theta.empty() && maxit>1) {
    std::vector<std::vector<double>> simplex(d+1,best.theta);
    for(int j=0;j<d;++j) simplex[j+1][j]+=0.05;
    std::vector<double> values(d+1);
    for(int j=0;j<=d;++j) values[j]=bounded(simplex[j]);
    for(int it=0;it<maxit*5;++it) {
      for(int j=0;j<=d;++j) for(int k=j+1;k<=d;++k) if(values[k]<values[j]) {
        std::swap(values[j],values[k]); std::swap(simplex[j],simplex[k]);
      }
      double diameter=0;
      for(int j=1;j<=d;++j) for(int k=0;k<d;++k)
        diameter=std::max(diameter,std::abs(simplex[j][k]-simplex[0][k]));
      if(diameter<1e-7 && values[d]-values[0]<1e-9) {
        best={simplex[0],values[0],true,it}; break;
      }
      std::vector<double> centre(d,0),reflected(d),expanded(d),contracted(d);
      for(int j=0;j<d;++j) for(int k=0;k<d;++k) centre[k]+=simplex[j][k]/d;
      for(int k=0;k<d;++k) reflected[k]=2*centre[k]-simplex[d][k];
      const double fr=bounded(reflected);
      if(fr<values[0]) {
        for(int k=0;k<d;++k) expanded[k]=centre[k]+2*(reflected[k]-centre[k]);
        const double fe=bounded(expanded);
        simplex[d]=fe<fr ? expanded : reflected; values[d]=std::min(fe,fr);
      } else if(fr<values[d-1]) { simplex[d]=reflected; values[d]=fr; }
      else {
        for(int k=0;k<d;++k) contracted[k]=centre[k]+0.5*((fr<values[d] ? reflected[k] : simplex[d][k])-centre[k]);
        const double fc=bounded(contracted);
        if(fc<std::min(fr,values[d])) { simplex[d]=contracted; values[d]=fc; }
        else for(int j=1;j<=d;++j) {
          for(int k=0;k<d;++k) simplex[j][k]=0.5*(simplex[0][k]+simplex[j][k]);
          values[j]=bounded(simplex[j]);
        }
      }
    }
  }
  return best;
}

struct Result {
  double mean=0,sd=1,nu=3,xi=1,f=inf;
  // 0 interior; 1 failure; 2 invalid; 3 collapse; 4 equality/tail guard;
  // 5 normal; 6 uniform; 7/8 half; 9 df2.
  int status=2,iterations=0;
  StopDetail face_detail{};
};

inline Result fit(span x, bool student, double fm, double fs, double fn, double fx,
                  double tol, int maxit, bool inside_uniform=false) {
  maxit = static_cast<int>(solver_budget(maxit));
  const bool hm=!std::isnan(fm), hs=!std::isnan(fs), hn=!std::isnan(fn), hx=!std::isnan(fx);
  Result out;
  if(x.empty() || !(tol>0 && std::isfinite(tol)) || maxit<1 ||
     (hm && !std::isfinite(fm)) || (hs && !(fs>0 && std::isfinite(fs))) ||
     (hn && !(fn>(student ? 2 : 0) && std::isfinite(fn))) ||
     (hx && !(fx>0 && std::isfinite(fx)))) return out;
  double lo=inf,hi=-inf;
  for(double v:x) { if(!std::isfinite(v)) return out; lo=std::min(lo,v); hi=std::max(hi,v); }
  std::size_t k=0;
  if(hm) { for(double v:x) if(v==fm) ++k; } else k=collapse_multiplicity(x);
  // With held sd, nu -> 2 still shrinks the effective t scale. P0b's
  // table incorrectly excludes this path (its own experiment exhibits it).
  double critical_loglik=NAN;
  if(student && (!hs || !hn)) {
    const double exponent=std::fma(hn?fn:2.,static_cast<double>(x.size()-k),-static_cast<double>(k));
    if(exponent<0){out.status=3;return out;}
    if(exponent==0) {
      if(!(hm && hs && hx && !hn)){out.status=4;return out;}
      // At the held-mean t2 collapse the tied residual lies on the
      // broad side. Its dimensionless displacement is 1-small/large.
      const double log_xi=std::log(fx),ratio=-std::expm1(-2*std::abs(log_xi));
      const double log_left=-special::log_sum_exp(0,2*log_xi);
      const double log_right=-special::log_sum_exp(0,-2*log_xi);
      critical_loglik=-1.5*k*std::log1p(ratio*ratio);
      for(double v:x)if(v!=fm)
        critical_loglik+=3*((v<fm?log_left:log_right)-special::log_abs_difference(v,fm));
      if(!std::isfinite(critical_loglik)){out.status=4;return out;}
    }
  }
  if(!student && !hs && lo==hi && (!hm || fm==lo)) { out.status=3; return out; }
  const double origin=hm ? fm : lo+(hi-lo)/2;
  const double unit=hs ? fs : std::max(hi-lo,std::max(std::abs(lo-origin),std::abs(hi-origin)));
  if(!(unit>0 && std::isfinite(unit))) return out;
  std::vector<double> y;
  for(double v:x) y.push_back((v-origin)/unit);
  const span z{y.data(),y.size()};
  const double yl=(lo-origin)/unit,yh=(hi-origin)/unit;
  const double n=static_cast<double>(x.size()), tie=1e-8*n;
  if(hn && hx && !hm && !hs) {
    const double m=moment(student,fn),v=variance(student,fn),q=fx-1/fx;
    double best_scale=1;
    auto profile=[&](double c) {
      auto at=[&](double ls) { const double t=std::exp(ls); return density_cost(z,student,c,t/fx,t*fx,fn); };
      double ls=0;
      if(!student) {
        double largest=0;
        for(double a:y) largest=std::max(largest,std::abs((a-c)/(a<c ? 1/fx : fx)));
        double sum=0;
        for(double a:y) sum+=std::pow(std::abs((a-c)/(a<c ? 1/fx : fx))/largest,fn)/n;
        ls=std::log(largest)+(std::log(fn)+std::log(sum))/fn;
      } else {
        double left=-32,right=32;
        for(int j=0;j<maxit;++j) {
          const double mid=(left+right)/2;
          // Once the midpoint rounds to an endpoint, the remaining
          // updates cannot change the represented final midpoint or scale.
          if(mid==left || mid==right)break;
          const double scale=std::exp(mid);
          double score=-n;
          for(double a:y) {
            const double w=(a-c)/(scale*(a<c ? 1/fx : fx));
            score+=(fn+1)*w*w/(fn+w*w);
          }
          if(score>0) left=mid; else right=mid;
        }
        ls=(left+right)/2;
      }
      best_scale=std::exp(ls);
      return at(ls);
    };
    double c=scan(profile,yl,yh,256),f=profile(c);
    // Refine every observation interval, including modes at ties.
    auto ordered=y; std::sort(ordered.begin(),ordered.end());
    for(std::size_t j=0;j<ordered.size();++j) {
      const double a=ordered[j];
      if(profile(a)<f) { c=a; f=profile(a); }
      if(j && a>ordered[j-1]) {
        const double t=snorm_detail::minimize(profile,ordered[j-1],a);
        if(profile(t)<f) { c=t; f=profile(t); }
      }
    }
    f=profile(c);
    return {origin+unit*(c+best_scale*m*q),unit*best_scale*std::sqrt(v+(v-m*m)*q*q),fn,fx,
            f+n*std::log(unit),std::isfinite(f) ? 0 : 1,0};
  }

  const std::vector<bool> held{hm,hs,hn,hx};
  const std::vector<double> fixed{0,hs ? fs/unit : 1,fn,fx};
  std::vector<int> free;
  for(int i=0;i<4;++i) if(!held[i]) free.push_back(i);
  auto unpack=[&](const std::vector<double> &t, double shape) {
    auto p=fixed;
    for(std::size_t j=0;j<free.size();++j) {
      const int i=free[j];
      p[i]=i==0 ? t[j] : i==2 ? (student ? 2+std::exp(t[j]) : 1+std::exp(t[j])) : std::exp(t[j]);
    }
    if(shape>0) p[2]=shape;
    return p;
  };
  auto objective=[&](const std::vector<double> &t, double shape) {
    const auto p=unpack(t,shape);
    const double m=moment(student,p[2]),v=variance(student,p[2]);
    const double q=p[3]-1/p[3];
    const double scale=p[1]/std::sqrt(v+(v-m*m)*q*q);
    return density_cost(z,student,p[0]-scale*m*q,scale/p[3],scale*p[3],p[2]);
  };
  std::vector<double> lower,upper;
  for(int i:free) {
    lower.push_back(i==0 ? yl-10 : i==2 ? -20 : -18);
    upper.push_back(i==0 ? yh+10 : i==2 ? 11 : 18);
  }
  std::vector<std::vector<double>> starts;
  for(double shape:student ? std::vector<double>{2.3,4,12,100} : std::vector<double>{1.2,2,4,12})
    for(double skew:std::vector<double>{0.4,1,2.5}) {
      std::vector<double> start;
      for(int i:free) start.push_back(i==0 ? (yl+yh)/2 : i==1 ? -0.7 : i==2 ? std::log(shape-(student ? 2 : 1)) : std::log(skew));
      starts.push_back(start);
    }
  auto interior=solve([&](const std::vector<double> &t) { return objective(t,0); },starts,lower,upper,tol,maxit);
  if(!interior.theta.empty() || free.empty()) {
    auto p=unpack(interior.theta,0);
    out={p[0],p[1],p[2],p[3],interior.f,interior.ok ? 0 : 1,interior.iterations};
  }
  if(std::isfinite(critical_loglik)) {
    const auto profile=[&](double t) { return objective(std::vector<double>{t},0); };
    const double t=scan(profile,lower.front(),upper.front(),maxit,maxit);
    const double f=profile(t);
    if(std::isfinite(f) && (out.status!=0 || f<out.f)) {
      const auto p=unpack(std::vector<double>{t},0);
      // Nomination only. The public fitter still requires its observed-
      // information and coefficient-accuracy check on the free shape.
      out={p[0],p[1],p[2],p[3],f,0,0};
    }
  }
  if(!student && !hn) {
    // The attained Laplace restriction is solved separately, without a
    // vanishing log(nu-1) score disguising a nonstationary free fit.
    const auto pos=std::find(free.begin(),free.end(),2)-free.begin();
    auto lf=free; lf.erase(lf.begin()+pos);
    auto ls=starts; auto ll=lower; auto lu=upper;
    for(auto &v:ls) v.erase(v.begin()+pos);
    ll.erase(ll.begin()+pos); lu.erase(lu.begin()+pos);
    auto lap=[&](const std::vector<double> &t) {
      auto a=t; a.insert(a.begin()+pos,0); return objective(a,1);
    };
    auto r=solve(lap,ls,ll,lu,tol,maxit);
    if(r.f<=out.f+tie && r.ok) {
      auto a=r.theta; a.insert(a.begin()+pos,0); auto p=unpack(a,1);
      out={p[0],p[1],1,p[3],r.f,0,r.iterations};
    }
  }
  if(!student && !hn) {
    // Conditional Laplace profiles: the location is a weighted sample
    // quantile, and with fixed skew the scale is explicit. Checking the
    // observation kinks also prevents smooth BFGS termination at a worse
    // Laplace cusp from hiding the best attained restriction member.
    auto laplace=[&](double c,double a,double b) {
      const double f=density_cost(z,false,c,a,b,1);
      if(std::isfinite(f) && f<=out.f+tie)
        out={hm ? 0 : c+b-a,hs ? fs/unit : std::hypot(a,b),1,hx ? fx : std::sqrt(b/a),f,0,0};
      return f;
    };
    if(!hm) {
      auto sorted=y; std::sort(sorted.begin(),sorted.end());
      auto profile=[&](double lx) {
        const double xi=hx ? fx : std::exp(lx),ratio=xi*xi;
        const std::size_t index=std::min(sorted.size()-1,
          static_cast<std::size_t>(std::max(0.0,std::ceil(n/(1+ratio))-1)));
        const double c=sorted[index];
        double a=hs ? fs/unit/std::hypot(1.0,ratio) : 0;
        if(!hs) {
          for(double v:y) a+=(v<c ? c-v : (v-c)/ratio)/n;
        }
        return laplace(c,a,ratio*a);
      };
      if(hx) profile(std::log(fx));
      else { const double t=scan(profile,-18,18,256); profile(t); }
    } else if(!hs && !hx) {
      for(double c:y) {
        const double d=-c;
        auto profile=[&](double lt) {
          const double a=std::exp(lt)+std::max(0.0,-d),b=std::exp(lt)+std::max(0.0,d);
          return laplace(c,a,b);
        };
        const double t=scan(profile,-24,24,96); profile(t);
      }
    } else if(hs && !hx) {
      const double sd=fs/unit;
      for(double c:y) if(std::abs(c)<sd) {
        const double d=-c,t=std::sqrt(2*sd*sd-d*d);
        laplace(c,(t-d)/2,(t+d)/2);
      }
    } else if(!hs && hx && fx!=1) {
      for(double c:y) {
        const double a=-c/(fx*fx-1);
        if(a>0) laplace(c,a,a*fx*fx);
      }
    }
  }
  if(hm && hn && !hs && !hx) {
    // Fixed mean and shape: profile scale before scanning skew. In the
    // log-concave GED range the objective is strictly convex in inverse
    // scale, so this scale minimizer is unique. Student-t and sub-one GED
    // shapes retain a scanned scale profile instead.
    double selected_scale=1;
    auto profile=[&](double lx) {
      auto at=[&](double ls) { return objective(std::vector<double>{ls,lx},0); };
      const double ls=student || fn<1 ? scan(at,-24,24,128,maxit) : snorm_detail::minimize(at,-24,24,maxit);
      selected_scale=std::exp(ls);
      return at(ls);
    };
    const double lx=scan(profile,-18,18,180,maxit),f=profile(lx);
    if(f<out.f) out={0,selected_scale,fn,std::exp(lx),f,
      std::abs(lx)>17.99 || std::abs(std::log(selected_scale))>23.99 ? 1 : 0,0};
  }
  auto hits_guard=[&](const Result &r) {
    for(std::size_t j=0;j<free.size();++j) {
      const int i=free[j];
      if(i==2 && !student && r.nu==1) continue;
      const double t=i==0 ? r.mean : i==1 ? std::log(r.sd) :
        i==2 ? std::log(r.nu-(student ? 2 : 1)) : std::log(r.xi);
      if(!std::isfinite(t) || t<lower[j]+1e-5 || t>upper[j]-1e-5) return true;
    }
    return false;
  };
  // Independently solved closure faces remain competitors even when an
  // interior restart fails. A failed finite estimate keeps status 1 and
  // is never returned unless the free-score or rescue gate passed.
  double finite_witness=inf;
  if(student) {
    double mean=0;for(double a:y)mean+=a/n;
    if(hm)mean=0;
    double variance=0;for(double a:y)variance+=(a-mean)*(a-mean)/n;
    const double sd=hs?fs/unit:std::sqrt(variance),nu=hn?fn:4,xi=hx?fx:1;
    const double m=moment(true,nu),v=skew_hold_detail::variance(true,nu),q=xi-1/xi;
    const double base=sd/std::sqrt(v+(v-m*m)*q*q);
    // A feasible member is a lower likelihood bound, never a replacement
    // estimate. It can refute a face after an unfinished finite search.
    finite_witness=density_cost(z,true,mean-base*m*q,base/xi,base*xi,nu);
  }
  // In held-sd units, strictly interior SGED data admit a finite
  // shape whose likelihood exceeds the uniform height.
  const double uniform_cost=n*.5*std::log(12.);
  const auto uniform_margin=[&](double f) {
    return 128*std::numeric_limits<double>::epsilon()*(n+std::abs(f)+uniform_cost);
  };
  const auto below_uniform=[&](double f) {
    return inside_uniform && f>uniform_cost+uniform_margin(f);
  };
  auto accept=[&](double f,int status,double nu) {
    if((status==7 || status==8) && below_uniform(f))return false;
    if(out.status==1 && interior.iterations>=maxit) {
      if(!student)return false;
      const double margin=1024*std::numeric_limits<double>::epsilon()*(n+std::abs(f)+std::abs(finite_witness));
      if(std::isfinite(finite_witness) && finite_witness<f-margin)return false;
    }
    if(std::isfinite(f) && f<=out.f+tie) { out.f=f; out.status=status; out.nu=nu;out.face_detail={};return true; }
    return false;
  };
  if(!hn && student) {
    const auto r=snorm_mle(z,tol,maxit,hm ? 0 : fm,hs ? fs/unit : fs,fx);
    if(r.status==SnormStatus::Ok && accept(-r.logLik,5,inf))out.face_detail=limit_detail({origin+unit*r.mu,unit*r.sigma,r.xi},r.logLik-n*std::log(unit));
    if(r.status==SnormStatus::RightHalfNormalBoundary && accept(-r.logLik,7,inf)) {
      out.face_detail=r.stop_detail;
      if(out.face_detail.limit_coef.size()==2) {out.face_detail.limit_coef[0]=hm?origin+unit*out.face_detail.limit_coef[0]:lo;out.face_detail.limit_coef[1]*=unit;out.face_detail.limit_loglik-=n*std::log(unit);}
    }
    if(r.status==SnormStatus::LeftHalfNormalBoundary && accept(-r.logLik,8,inf)) {
      out.face_detail=r.stop_detail;
      if(out.face_detail.limit_coef.size()==2) {out.face_detail.limit_coef[0]=hm?origin+unit*out.face_detail.limit_coef[0]:hi;out.face_detail.limit_coef[1]*=unit;out.face_detail.limit_loglik-=n*std::log(unit);}
    }
  }
  if(!hn && !student && !hs) {
    // A fixed sd precludes a winning SGED uniform limit for binary64
    // data. Even an exact difference of dyadics cannot equal sqrt(3)*sd
    // (held mean) or sqrt(12)*sd (free mean). Strictly interior data have
    // a superior finite shape; exterior data have zero limiting likelihood.
    const double width=hm ? 2*std::max(-yl,yh) : yh-yl;
    if(width>0)accept(n*std::log(width),6,inf);
  }
  if(!hx) for(int side:std::vector<int>{1,-1}) {
    // Half family in ordinary base coordinates: mean=c+side*m*t,
    // sd=t*sqrt(v-m^2). Endpoint is free only when mean is free.
    StopDetail selected_half;
    auto half=[&](double nu) {
      selected_half={};double best_half=inf;
      const double m=moment(student,nu),v=variance(student,nu);
      const double forced=hs ? fs/unit/std::sqrt(v-m*m) : 0;
      const double endpoint=side==1 ? yl : yh;
      const double minscale=hm ? std::max(0.0,-side*endpoint/m) : 0;
      auto at=[&](double lt) {
        const double t=hs ? forced : std::exp(lt);
        const double c=hm ? -side*m*t : endpoint;
        if(!(t>0) || t<minscale || (side==1 ? c>yl+1e-13 : c<yh-1e-13)) return inf;
        double f=n*(std::log(t)-std::log(2.0)-logmode(student,nu));
        for(double a:y) {
          double w=side*(a-c)/t;
          // With both moments pinned the finite-family mode approaches
          // the endpoint from inside; ties retain a shrinking-side kernel.
          if(hm && hs && std::abs(a-c)<1e-14*(1+std::abs(c)))
            w=m*v/(2*(v-m*m));
          f-=kernel(student,nu,w);
        }
        if(std::isfinite(f) && f<best_half) {
          best_half=f;
          const double physical_endpoint=hm?origin+unit*c:(side==1?lo:hi);
          selected_half=limit_detail(student?std::vector<double>{physical_endpoint,nu,unit*t}:std::vector<double>{physical_endpoint,unit*t,nu},-f-n*std::log(unit),
            student?(side==1?StopModel::Ht:StopModel::ReflectedHt):(side==1?StopModel::HalfGed:StopModel::ReflectedHalfGed));
        }
        return std::isfinite(f) ? f : inf;
      };
      if(hs) return at(0);
      if(!hm) {
        // Free endpoint is the appropriate sample extreme. Its scale has
        // an exact GED profile, or the unique monotone t scale-score root.
        double ls=0;
        if(!student) {
          const double range=yh-yl;
          double sum=0;
          for(double a:y) sum+=std::pow(std::abs(a-endpoint)/range,nu)/n;
          ls=std::log(range)+(std::log(nu)+std::log(sum))/nu;
        } else {
          double l=-32,r=32;
          for(int j=0;j<maxit;++j) {
            const double mid=(l+r)/2,t=std::exp(mid);
            double score=-n;
            for(double a:y) {
              const double w=(a-endpoint)/t;
              score+=(nu+1)*w*w/(nu+w*w);
            }
            if(score>0) l=mid; else r=mid;
          }
          ls=(l+r)/2;
        }
        return at(ls);
      }
      double left=minscale>0 ? std::log(minscale) : -24;
      if(left>24) return inf;
      // The support constraint is closed; include its endpoint explicitly.
      const double t=scan(at,left,24,96,maxit);
      return std::min(at(left),at(t));
    };
    if(hn) {if(accept(half(fn),side==1 ? 7 : 8,fn))out.face_detail=selected_half;}
    else {
      auto profile=[&](double t) { return half(student ? 2+std::exp(t) : std::exp(t)); };
      const double t=scan(profile,student ? -18 : 0,11,96,maxit);
      if(accept(profile(t),side==1 ? 7 : 8,student ? 2+std::exp(t) : std::exp(t)))out.face_detail=selected_half;
    }
  }
  if(student && !hn && !hs) {
    // At df=2 use ordinary t scales. A held mean constrains the MODE
    // c=mean - sqrt(2)*s*(xi-1/xi), rather than c=mean.
    std::vector<int> coords;
    if(!hm) coords.push_back(0);
    coords.push_back(1);
    if(!hx) coords.push_back(3);
    std::vector<double> low,up;
    for(int i:coords) { low.push_back(i==0 ? yl-10 : -18); up.push_back(i==0 ? yh+10 : 18); }
    std::vector<std::vector<double>> seeds;
    for(double skew:std::vector<double>{0.4,1,2.5}) {
      std::vector<double> t;
      for(int i:coords) t.push_back(i==0 ? (yl+yh)/2 : i==1 ? -1 : std::log(skew));
      seeds.push_back(t);
    }
    auto face=[&](const std::vector<double> &t) {
      double mean=0,s=1,xi=fx;
      for(std::size_t j=0;j<coords.size();++j) {
        if(coords[j]==0) mean=t[j]; else if(coords[j]==1) s=std::exp(t[j]); else xi=std::exp(t[j]);
      }
      return density_cost(z,true,mean-std::sqrt(2.0)*s*(xi-1/xi),s/xi,s*xi,2);
    };
    auto r=solve(face,seeds,low,up,tol,maxit);
    if(r.ok && accept(r.f,9,2)) {
      double mean=0,scale=1,xi=fx;
      for(std::size_t j=0;j<coords.size();++j) {
        if(coords[j]==0)mean=r.theta[j];else if(coords[j]==1)scale=std::exp(r.theta[j]);else xi=std::exp(r.theta[j]);
      }
      out.face_detail=limit_detail({origin+unit*(mean-std::sqrt(2.)*scale*(xi-1/xi)),unit*scale,xi,2},-r.f-n*std::log(unit));
    }
  }
  if(std::isfinite(critical_loglik)) {
    const double offset=n*std::log(unit),candidate=-out.f-offset;
    const double margin=1024*std::numeric_limits<double>::epsilon()*
      (n+std::abs(offset)+std::abs(critical_loglik)+
       (std::isfinite(candidate)?std::abs(candidate):0));
    if(out.status==1 || !std::isfinite(candidate) || !(candidate>critical_loglik+margin)) {
      out.status=4;out.f=-critical_loglik-offset;out.face_detail={};
      out.face_detail.limit_loglik=critical_loglik;
      out.face_detail.loglik_lower=critical_loglik-margin;
      out.face_detail.loglik_upper=INFINITY;
    }
  }
  if(out.status==0 && hits_guard(out))out.status=1;
  if(out.status==0 && below_uniform(out.f)) {
    out.status=10;
    out.face_detail.loglik_lower=-uniform_cost-n*std::log(unit)-uniform_margin(out.f);
    out.face_detail.loglik_upper=INFINITY;
  }
  out.mean=hm ? fm : origin+unit*out.mean;
  out.sd=hs ? fs : unit*out.sd;
  if(hn) out.nu=fn;
  if(hx) out.xi=fx;
  out.f+=n*std::log(unit);
  return out;
}
} // namespace skew_hold_detail
} // namespace cppunivariate
