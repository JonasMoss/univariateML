#pragma once
#include "../../core/span.hpp"
#include "../../core/work_budget.hpp"
#include <algorithm>
#include <array>
#include <cmath>
#include <limits>
#include <map>
#include <numeric>
#include <vector>

namespace cppunivariate { namespace skellam_detail {

struct ProfileInterval {
  double lo, hi;
  ProfileInterval(double x=0) : lo(x), hi(x) {}
  ProfileInterval(double a, double b) : lo(a), hi(b) {}
};

inline ProfileInterval profile_round(double lo, double hi) {
  return {std::nextafter(lo,-INFINITY),std::nextafter(hi,INFINITY)};
}
inline ProfileInterval operator+(ProfileInterval a, ProfileInterval b) {
  return profile_round(a.lo+b.lo,a.hi+b.hi);
}
inline ProfileInterval operator-(ProfileInterval a, ProfileInterval b) {
  return profile_round(a.lo-b.hi,a.hi-b.lo);
}
inline ProfileInterval operator*(ProfileInterval a, ProfileInterval b) {
  const auto bounds=std::minmax({a.lo*b.lo,a.lo*b.hi,a.hi*b.lo,a.hi*b.hi});
  return profile_round(bounds.first,bounds.second);
}
inline ProfileInterval operator/(ProfileInterval a, ProfileInterval b) {
  return a*profile_round(1/b.hi,1/b.lo); // denominators are positive
}
inline ProfileInterval profile_hypot(ProfileInterval a, ProfileInterval b) {
  // Both arguments are nonnegative.
  const double lo=std::hypot(a.lo,b.lo), hi=std::hypot(a.hi,b.hi);
  constexpr double pad=4*std::numeric_limits<double>::epsilon();
  return profile_round(lo*(1-pad),hi*(1+pad));
}
inline ProfileInterval profile_z(ProfileInterval t, ProfileInterval d) {
  const auto u=t*(t+d);
  return profile_round(2*std::sqrt(std::max(0.,u.lo)),2*std::sqrt(u.hi));
}

struct ProfileAtom {
  double order, weight;
};

inline ProfileInterval face_score(const std::vector<ProfileAtom>& atoms, double sum, double n) {
  // Exact, bounded rational addition. Overflow leaves the equality undecided.
  constexpr auto cap=std::numeric_limits<std::uint64_t>::max();
  const ProfileInterval unknown{-INFINITY,INFINITY};
  std::uint64_t numerator=0, denominator=1;
  for (auto atom:atoms) {
    const auto k=static_cast<std::uint64_t>(atom.order)+1;
    const auto count=static_cast<std::uint64_t>(atom.weight);
    const auto g=std::gcd(k,denominator);
    if (denominator>cap/(k/g) || numerator>cap/(k/g) || count>cap/(denominator/g))
      return unknown;
    const auto term=count*(denominator/g);
    numerator*=k/g;
    if (numerator>cap-term) return unknown;
    numerator+=term;
    denominator*=k/g;
    const auto reduce=std::gcd(numerator,denominator);
    numerator/=reduce;
    denominator/=reduce;
  }
  std::array<std::uint64_t,2> left{{static_cast<std::uint64_t>(sum),numerator}};
  std::array<std::uint64_t,3> right{{static_cast<std::uint64_t>(n),
                                     static_cast<std::uint64_t>(n),denominator}};
  for (auto& a:left) {
    for (auto& b:right) {
      const auto g=std::gcd(a,b);
      a/=g;
      b/=g;
    }
  }
  if (left[0]>cap/left[1] || right[0]>cap/right[1] ||
      right[0]*right[1]>cap/right[2]) return unknown;
  const auto a=left[0]*left[1], b=right[0]*right[1]*right[2];
  if(a==b) return 0;
  const double difference=a>b ? static_cast<double>(a-b) : -static_cast<double>(b-a);
  const double score=difference/static_cast<double>(b);
  return profile_round(std::nextafter(score,-INFINITY),std::nextafter(score,INFINITY));
}

// Enclose F'_k(u)/F_k(u) with the positive Bessel continued fraction.
inline ProfileInterval profile_a(double k, double u, int maxit) {
  int depth=std::min(maxit,16+static_cast<int>(4*std::sqrt(u)));
  constexpr double eps=std::numeric_limits<double>::epsilon();
  for (;;) {
    const double order=k+depth;
    ProfileInterval r{0,(4*ProfileInterval(u)/(2*(order+1))).hi};
    for (int j=depth-1;j>=1;--j)
      r=4*ProfileInterval(u)/(ProfileInterval(2*(k+j+1))+r);
    const auto bounds=2/(ProfileInterval(2*(k+1))+r);
    if (depth==maxit || bounds.hi-bounds.lo<=32*eps*bounds.hi) return bounds;
    depth=std::min(maxit,2*depth);
  }
}

struct ProfileDerivatives { ProfileInterval score, curvature; };

inline ProfileDerivatives profile_series(const std::vector<ProfileAtom>& atoms,
    ProfileInterval t, ProfileInterval p, ProfileInterval m, ProfileInterval d,
    double n, int maxit, ProfileInterval g0) {
  const auto u=t*(t+d), h=2*t+d, a=t+d;
  const bool centered=std::isfinite(g0.lo) && t.hi<1;
  ProfileInterval first, second, face_increment;
  for (const auto atom:atoms) {
    const auto bounds=[&](double k) {
      return ProfileInterval(profile_a(k,u.hi,maxit).lo,
                             profile_a(k,std::max(0.,u.lo),maxit).hi);
    };
    const auto ak=bounds(atom.order), next=bounds(atom.order+1);
    first=first+atom.weight*ak;
    auto bk=ak*(next-ak);
    bk.hi=std::min(0.,bk.hi);
    second=second+atom.weight*bk;
    if (centered) face_increment=face_increment+
        atom.weight*(2*t*ak-d*u*ak*next/(atom.order+1));
  }
  auto score=centered ? g0+face_increment/n-t/a : -2+h*first/n;
  auto curvature=(2*first+h*h*second)/n;
  if (p.hi>0 && !centered) score=score+p/a;
  if (p.hi>0) curvature=curvature-p/a/a;
  if (m.hi>0 && t.lo>0) {
    score=score+m/t;
    curvature=curvature-m/t/t;
  }
  return {score,curvature};
}

inline ProfileDerivatives profile_riccati(const std::vector<ProfileAtom>& atoms,
    ProfileInterval z, ProfileInterval d, double n) {
  const auto h=profile_hypot(z,d);
  ProfileInterval score, curvature;
  for (const auto atom:atoms) {
    const double k=atom.order;
    const auto r=profile_hypot(z,k), delta=(k-d)*(k+d)/(r+h);
    const auto ratio=k/z, squared=ratio*ratio, q=1/(1+squared);
    const auto remainder=squared/(1+squared);
    const auto b=q*(0.5-0.625*q), derivative=0.5-1.25*q;
    const double error=(5/z/z).hi;
    const double derivative_error=(10*r/z/z).hi;
    const auto e=(2*q*remainder*derivative-2*q*b)/r+b/r*b/r;
    score=score+atom.weight*(delta-0.5*q+b/r+ProfileInterval(-error,error));
    curvature=curvature+atom.weight*(0-delta*(z/r)*(z/h)-q*remainder+
        (2*q*remainder*derivative-q*b)/r-e+
        ProfileInterval(-derivative_error,derivative_error));
  }
  return {score/n,curvature/n};
}

template <typename Value>
inline bool certify_profile(span x, double mean, double tol, int maxit,
    Value value, double& winner, double& loglik, int& iterations, bool& face_refuted) {
  double positive=0, negative=0;
  std::map<double,double> counts;
  // Integer totals and order increments must remain exactly representable.
  for (double observed:x) {
    const double k=mean<0 ? -observed : observed;
    if (std::abs(k)>=0x1p51) return false;
    positive+=std::max(k,0.);
    negative+=std::max(-k,0.);
    if (positive+negative>=0x1p52) return false;
    ++counts[std::abs(k)];
  }
  const double n=static_cast<double>(x.size());
  if (n>=0x1p52 || positive<negative) return false;
  const double difference=positive-negative;
  const auto average=[n](double total) {
    return total==0 ? ProfileInterval(0) : profile_round(total/n,total/n);
  };
  const auto p=average(positive), m=average(negative), d=average(difference);
  std::vector<ProfileAtom> atoms;
  ProfileInterval second_moment;
  for (const auto& pair:counts) {
    atoms.push_back({pair.first,pair.second});
    second_moment=second_moment+(ProfileInterval(pair.second)/n)*pair.first*pair.first;
  }

  const bool face=negative==0;
  const auto g0=face ? face_score(atoms,difference,n) : ProfileInterval{-INFINITY,INFINITY};
  const bool stationary=g0.lo==0 && g0.hi==0;
  face_refuted=g0.lo>0;
  const double abs_mean=std::abs(mean);
  const auto to_t=[abs_mean](double z) {
    return z*(z/(2*(std::hypot(z,abs_mean)+abs_mean)));
  };
  const double transition=to_t(40);
  // The Riccati upper score is negative from this tail threshold onwards.
  const double tail=to_t(2*std::max({40.,2*counts.rbegin()->first,2*second_moment.hi+2}));
  const bool one_magnitude=atoms.size()==1 && difference==0;

  std::int64_t checked=0;
  const auto derivative=[&](ProfileInterval t, bool large) {
    if (++checked>certificate_budget(maxit)) return ProfileDerivatives{{-INFINITY,INFINITY},{-INFINITY,INFINITY}};
    if (large) return profile_riccati(atoms,profile_z(t,d),d,n);
    if (t.lo==0 && !face) {
      const double lower=t.hi==0 ? INFINITY : (m/t.hi-2).lo;
      return ProfileDerivatives{{lower,INFINITY},{-INFINITY,INFINITY}};
    }
    if (t.hi==0 && stationary) return ProfileDerivatives{0,{-INFINITY,INFINITY}};
    return profile_series(atoms,t,p,m,d,n,maxit,g0);
  };

  struct Cell { double lo,hi; bool large; };
  std::vector<Cell> pending{{transition,tail,true},{0,transition,false}};
  std::vector<ProfileInterval> peaks;
  while (!pending.empty()) {
    if (checked>=certificate_budget(maxit)) return false;
    const auto cell=pending.back();
    pending.pop_back();
    const auto bounds=derivative({cell.lo,cell.hi},cell.large);
    if (bounds.score.lo>0 || bounds.score.hi<0) continue;
    const auto low=derivative(cell.lo,cell.large).score;
    const auto high=derivative(cell.hi,cell.large).score;
    if (cell.lo==0 && stationary) {
      if (bounds.curvature.lo>0) face_refuted=true;
      if (face_refuted || bounds.curvature.hi<0) continue;
    }
    if (bounds.curvature.hi<0) {
      if (low.hi<0 || high.lo>0) continue;
      if (low.lo>0 && high.hi<0) {
        double lo=cell.lo, hi=cell.hi;
        bool refined=false;
        for (int it=0;it<maxit;++it) {
          const double mid=lo+(hi-lo)/2;
          const double radius=std::max(mid-lo,hi-mid);
          const double eps=std::numeric_limits<double>::epsilon();
          if (radius+8*eps*mid<=tol*lo &&
              radius+8*eps*(mid+abs_mean)<=tol*(lo+d.lo)) {
            peaks.emplace_back(lo,hi);
            iterations=std::max(iterations,it);
            refined=true;
            break;
          }
          const auto sign=derivative(mid,cell.large).score;
          if (sign.lo>0) lo=mid;
          else if (sign.hi<0) hi=mid;
          else {
            const double probe=std::min(radius/2,tol*mid/4);
            if (probe<=0 || derivative(mid-probe,cell.large).score.lo<=0 ||
                derivative(mid+probe,cell.large).score.hi>=0) break;
            lo=mid-probe;
            hi=mid+probe;
          }
        }
        if (!refined) return false;
        if (one_magnitude) pending.clear(); // the mean-zero singleton theorem
        continue;
      }
    }
    if (bounds.curvature.lo>0 && low.hi<0 && high.lo>0) continue;
    // An asymmetric split avoids placing common roots at dyadic cell edges.
    const double mid=cell.lo>0 && cell.hi/cell.lo>16
      ? std::sqrt(cell.lo)*std::sqrt(cell.hi) : cell.lo+0.47*(cell.hi-cell.lo);
    if (!(cell.lo<mid && mid<cell.hi)) return false;
    pending.push_back({mid,cell.hi,cell.large});
    pending.push_back({cell.lo,mid,cell.large});
  }

  struct Candidate { double t, lo, hi; };
  std::vector<Candidate> candidates;
  const auto profile_log=[](ProfileInterval positive) {
    const double lo=std::log(positive.lo), hi=std::log(positive.hi);
    constexpr double eps=4*std::numeric_limits<double>::epsilon();
    return profile_round(lo-eps*std::abs(lo),hi+eps*std::abs(hi));
  };
  const bool competing=peaks.size()+static_cast<std::size_t>(face && !face_refuted)>1;
  const auto compare_value=[&](ProfileInterval t) {
    if (!competing) return ProfileInterval(0);
    // Drop the common factorials and the constant -d. This also avoids
    // cancellation between the absolute likelihoods on a feasible face.
    auto loglik=-2*t;
    if (p.hi>0) loglik=loglik+p*profile_log(t+d);
    if (m.hi>0) loglik=loglik+m*profile_log(t);
    if (t.hi==0) return loglik;
    const auto u=t*(t+d);
    for (const auto atom:atoms) {
      ProfileInterval term=1, total=1;
      bool closed=false;
      for (double j=1;checked<certificate_budget(maxit);++j) {
        ++checked;
        term=term*u/(j*(ProfileInterval(j)+atom.order));
        total=total+term;
        if (!std::isfinite(total.hi)) break;
        const auto ratio=u/((j+1)*(ProfileInterval(j+1)+atom.order));
        if (ratio.hi<0.5 && term.hi<total.lo*1e-16) {
          total.hi=(total+term*ratio/(1-ratio)).hi;
          closed=true;
          break;
        }
      }
      if (!closed) return ProfileInterval{-INFINITY,INFINITY};
      loglik=loglik+atom.weight*profile_log(total)/n;
    }
    return loglik;
  };
  if (face && !face_refuted) {
    const auto bounds=compare_value(0);
    candidates.push_back({0,bounds.lo,bounds.hi});
  }
  for (auto peak:peaks) {
    const double mid=peak.lo+(peak.hi-peak.lo)/2;
    auto bounds=compare_value(mid);
    if (competing) {
      // Bound the peak value from an attainable midpoint and the
      // universal per-observation curvature ceiling of two in t.
      const auto radius=profile_round(0,std::max(mid-peak.lo,peak.hi-mid));
      const bool large=mid>=transition;
      const auto score=derivative(mid,large).score;
      const auto scale=large ? (1/ProfileInterval(mid)+1/(mid+d))/2 : ProfileInterval(1);
      const double magnitude=std::max(std::abs(score.lo),std::abs(score.hi));
      bounds.hi=(bounds+radius*scale*magnitude+radius*radius).hi;
    }
    candidates.push_back({mid,bounds.lo,bounds.hi});
  }
  if (candidates.empty()) return false;
  const auto best=std::max_element(candidates.begin(),candidates.end(),
    [](Candidate a,Candidate b) { return a.lo<b.lo; });
  for (auto it=candidates.begin();it!=candidates.end();++it) {
    if (!std::isfinite(it->lo) || !std::isfinite(it->hi)) return false;
    if (it!=best && !(it->hi<best->lo)) return false;
  }
  winner=best->t;
  loglik=value(winner);
  return std::isfinite(loglik);
}

}}  // namespace cppunivariate::skellam_detail
