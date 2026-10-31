// Contract, retained choices, evidence and known numerical limitations:
// dev/research/evaluations/2026-10-31-step7-routines.md (repository root).
#pragma once
#include "../core/precision.hpp"

#include "../core/span.hpp"
#include "special.hpp"
#include <algorithm>
#include <cmath>
#include <cstddef>
#include <vector>

namespace cppunivariate {

// Ok certifies truncation/quadrature remainders, not floating-point roundoff.
enum class CmpStatus {
    Ok, InvalidInput, WorkLimit, IterationFailure,
    GeometricLimit, TwoPointLimit, PointMassLimit
};
enum class CmpRegime { Direct, Geometric, EulerMaclaurin };
struct CmpMoments {
    double log_z = special::kNaN;
    double mean = special::kNaN, variance = special::kNaN;
    double mean_h = special::kNaN, variance_h = special::kNaN;
    double covariance = special::kNaN;
    // h - slope * X: avoids cancellation in profile information.
    CPPUNIV_EXTENDED_REAL center = 0, offset = 0, residual_mean = 0, log_scaled_z = 0;
    CPPUNIV_EXTENDED_REAL slope = 0;
    double variance_residual = special::kNaN;
    double covariance_residual = special::kNaN;
    double relative_tail_bound = special::kPosInf;
    std::size_t work = 0;
    CmpStatus status = CmpStatus::InvalidInput;
    CmpRegime regime = CmpRegime::Direct;
};

namespace cmp_detail {
inline CPPUNIV_EXTENDED_REAL stirling_correction(CPPUNIV_EXTENDED_REAL k) {
    const CPPUNIV_EXTENDED_REAL v=1/k, v2=v*v;
    return v*(CPPUNIV_EXTENDED_LITERAL(1.,1.L)/12+v2*(-CPPUNIV_EXTENDED_LITERAL(1.,1.L)/360+v2*(CPPUNIV_EXTENDED_LITERAL(1.,1.L)/1260+v2*(-CPPUNIV_EXTENDED_LITERAL(1.,1.L)/1680))));
}
// h(k)-h(c)-a*(k-c), stable also for nearby counts near 2^31.
inline CPPUNIV_EXTENDED_REAL residual(CPPUNIV_EXTENDED_REAL k, CPPUNIV_EXTENDED_REAL c, CPPUNIV_EXTENDED_REAL a) {
    const CPPUNIV_EXTENDED_REAL d=k-c;
    if (d == 0) return 0;
    if (std::abs(d) < 1024 && k == std::floor(k) && c == std::floor(c)) {
        CPPUNIV_EXTENDED_REAL r=0;
        const CPPUNIV_EXTENDED_REAL baseline=std::log(c+1)-a;
        if (d > 0) for (CPPUNIV_EXTENDED_REAL j=c+1;j<=k;++j)
            r+=std::log1p((j-(c+1))/(c+1))+baseline;
        else for (CPPUNIV_EXTENDED_REAL j=k+1;j<=c;++j)
            r-=std::log1p((j-(c+1))/(c+1))+baseline;
        return r;
    }
    if (k >= 32 && c >= 32) {
        const CPPUNIV_EXTENDED_REAL u=d/c, l=std::log1p(u);
        CPPUNIV_EXTENDED_REAL lm=l-u;
        if (std::abs(u) < CPPUNIV_EXTENDED_LITERAL(.01,.01L)) {
            lm=0; CPPUNIV_EXTENDED_REAL term=u*u;
            for (int j=2;j<=16;++j) { lm+=(j%2 ? 1 : -1)*term/j; term*=u; }
        }
        return c*lm+d*l+CPPUNIV_EXTENDED_LITERAL(.5,.5L)*l+d*(std::log(c)-a)+
            stirling_correction(k)-stirling_correction(c);
    }
    return std::lgamma(k+1)-std::lgamma(c+1)-a*d;
}
struct Sums {
    CPPUNIV_EXTENDED_REAL z = 1, extra = 0, d = 0, r = 0, dd = 0, rr = 0, dr = 0;
    CPPUNIV_EXTENDED_REAL cz=0, ce=0, cd=0, cr=0, cdd=0, crr=0, cdr=0;
    static void compensated(CPPUNIV_EXTENDED_REAL& sum, CPPUNIV_EXTENDED_REAL& c,
                            CPPUNIV_EXTENDED_REAL value) {
        const CPPUNIV_EXTENDED_REAL y=value-c, t=sum+y;
        c=(t-sum)-y; sum=t;
    }
    void add(CPPUNIV_EXTENDED_REAL lw, CPPUNIV_EXTENDED_REAL x, CPPUNIV_EXTENDED_REAL h,
             CPPUNIV_EXTENDED_REAL lw_correction = 0) {
        const CPPUNIV_EXTENDED_REAL w = std::exp(lw)*std::exp(-lw_correction);
        add_weight(w,x,h);
    }
    void add_weight(CPPUNIV_EXTENDED_REAL w, CPPUNIV_EXTENDED_REAL x, CPPUNIV_EXTENDED_REAL h) {
        compensated(z,cz,w); compensated(extra,ce,w);
        compensated(d,cd,w*x); compensated(r,cr,w*h);
        compensated(dd,cdd,w*x*x); compensated(rr,crr,w*h*h); compensated(dr,cdr,w*x*h);
    }
};
// Sum j^p q^j, j >= 1, p=0,...,4. The omitted weights are <= w q^j.
inline CPPUNIV_EXTENDED_REAL tail_bound(CPPUNIV_EXTENDED_REAL lw, CPPUNIV_EXTENDED_REAL lr,
                              CPPUNIV_EXTENDED_REAL d, CPPUNIV_EXTENDED_REAL r,
                              CPPUNIV_EXTENDED_REAL c, const Sums& s, CPPUNIV_EXTENDED_REAL tol,
                              CPPUNIV_EXTENDED_REAL quadratic = 1) {
    if (!(lr < 0)) return special::kPosInf;
    const CPPUNIV_EXTENDED_REAL q = std::exp(lr), v = -std::expm1(lr);
    const CPPUNIV_EXTENDED_REAL s0=q/v, s1=q/(v*v), s2=q*(1+q)/(v*v*v);
    const CPPUNIV_EXTENDED_REAL s3=q*(1+4*q+q*q)/std::pow(v,4);
    const CPPUNIV_EXTENDED_REAL s4=q*(1+11*q+11*q*q+q*q*q)/std::pow(v,5);
    d=std::abs(d); r=std::abs(r);
    const CPPUNIV_EXTENDED_REAL bd=d*d*s0+2*d*s1+s2;
    // |residual increment| <= c*j + quadratic*j^2.
    const CPPUNIV_EXTENDED_REAL br=r*r*s0+2*r*c*s1+(c*c+2*r*quadratic)*s2+2*c*quadratic*s3+quadratic*quadratic*s4;
    const CPPUNIV_EXTENDED_REAL tiny=std::numeric_limits<CPPUNIV_EXTENDED_REAL>::min();
    if (!std::isfinite(s0) || !std::isfinite(bd) || !std::isfinite(br))
        return special::kPosInf;
    const CPPUNIV_EXTENDED_REAL scale=std::max({s0/std::max(s.extra,tiny),
        bd/std::max(s.dd,tiny), br/std::max(s.rr,tiny)});
    const CPPUNIV_EXTENDED_REAL b=std::exp(lw+std::log(scale));
    return b/tol;
}
// Logarithmic form of the same positive geometric tail bounds. This avoids
// overflow in individual powers when their normalized ratios are finite.
inline CPPUNIV_EXTENDED_REAL tail_bound_log(
    CPPUNIV_EXTENDED_REAL lw, CPPUNIV_EXTENDED_REAL lr,
    CPPUNIV_EXTENDED_REAL d, CPPUNIV_EXTENDED_REAL r,
    CPPUNIV_EXTENDED_REAL c, CPPUNIV_EXTENDED_REAL quadratic,
    CPPUNIV_EXTENDED_REAL log_extra, CPPUNIV_EXTENDED_REAL log_dd,
    CPPUNIV_EXTENDED_REAL log_rr, CPPUNIV_EXTENDED_REAL tol) {
    if (!(lr < 0)) return special::kPosInf;
    const CPPUNIV_EXTENDED_REAL q=std::exp(lr), lv=std::log(-std::expm1(lr));
    const CPPUNIV_EXTENDED_REAL l0=lr-lv, l1=lr-2*lv,
      l2=lr+std::log1p(q)-3*lv, l3=lr+std::log1p(4*q+q*q)-4*lv,
      l4=lr+std::log1p(11*q+11*q*q+q*q*q)-5*lv;
    const auto logv=[](CPPUNIV_EXTENDED_REAL x) { return x>0 ? std::log(x) : -std::numeric_limits<CPPUNIV_EXTENDED_REAL>::infinity(); };
    const auto add=[](CPPUNIV_EXTENDED_REAL a,CPPUNIV_EXTENDED_REAL b) {
      if(a<b) std::swap(a,b);
      if(!std::isfinite(a)) return a;
      return a+std::log1p(std::exp(b-a));
    };
    d=std::abs(d); r=std::abs(r);
    const CPPUNIV_EXTENDED_REAL bd=add(add(2*logv(d)+l0,logv(2*d)+l1),l2);
    const CPPUNIV_EXTENDED_REAL l2c=std::log(static_cast<CPPUNIV_EXTENDED_REAL>(2));
    const CPPUNIV_EXTENDED_REAL lr0=logv(r), lc=logv(c), lq=logv(quadratic);
    const CPPUNIV_EXTENDED_REAL br=add(add(add(add(2*lr0+l0,l2c+lr0+lc+l1),
      add(2*lc,l2c+lr0+lq)+l2),l2c+lc+lq+l3),2*lq+l4);
    return std::exp(lw+std::max({l0-log_extra,bd-log_dd,br-log_rr})-std::log(tol));
}
// Euler--Maclaurin on a smooth saddle window, with composite 16-point Gauss
// integration. All derivative bounds are absolute Taylor coefficients.
// The tails and EM/quadrature remainders are bounded separately from roundoff.
inline CmpMoments euler_maclaurin(double eta, double nu, double tol) {
    CmpMoments out; out.regime=CmpRegime::EulerMaclaurin; out.status=CmpStatus::WorkLimit;
    if (!(nu > 0)) return out;
    const CPPUNIV_EXTENDED_REAL n=nu, lm=static_cast<CPPUNIV_EXTENDED_REAL>(eta)/n;
    CPPUNIV_EXTENDED_REAL z=std::exp(lm);
#ifdef BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
    // Recover the quotient's rounding error before exponentiation.
    z*=std::exp(std::fma(-lm,n,static_cast<CPPUNIV_EXTENDED_REAL>(eta))/n);
#endif
    const CPPUNIV_EXTENDED_REAL sigma=std::sqrt(z/n), radius=16, pi=std::acos(-CPPUNIV_EXTENDED_LITERAL(1.,1.L));
    if (!(z > CPPUNIV_EXTENDED_LITERAL(1e6,1e6L) && sigma >= CPPUNIV_EXTENDED_LITERAL(1e3,1e3L) && radius*sigma < z/2) || !std::isfinite(z)) return out;
    // Stirling through z^-7 has remainder <= 1/(1188 z^9).
    const auto res=[&](CPPUNIV_EXTENDED_REAL y) {
        const CPPUNIV_EXTENDED_REAL d=sigma*y, u=d/z, l=std::log1p(u);
        CPPUNIV_EXTENDED_REAL lm1=l-u;
        if (std::abs(u) < CPPUNIV_EXTENDED_LITERAL(.01,.01L)) {
            lm1=0; CPPUNIV_EXTENDED_REAL term=u*u;
            for (int j=2;j<=32;++j) { lm1+=(j%2 ? 1 : -1)*term/j; term*=u; }
        }
        return z*lm1+d*l+CPPUNIV_EXTENDED_LITERAL(.5,.5L)*l+stirling_correction(z+d)-stirling_correction(z);
    };
    const CPPUNIV_EXTENDED_REAL nodes[]={CPPUNIV_EXTENDED_LITERAL(.095012509837637440185,.095012509837637440185L),CPPUNIV_EXTENDED_LITERAL(.281603550779258913230,.281603550779258913230L),
        CPPUNIV_EXTENDED_LITERAL(.458016777657227386342,.458016777657227386342L),CPPUNIV_EXTENDED_LITERAL(.617876244402643748447,.617876244402643748447L),CPPUNIV_EXTENDED_LITERAL(.755404408355003033895,.755404408355003033895L),
        CPPUNIV_EXTENDED_LITERAL(.865631202387831743880,.865631202387831743880L),CPPUNIV_EXTENDED_LITERAL(.944575023073232576078,.944575023073232576078L),CPPUNIV_EXTENDED_LITERAL(.989400934991649932596,.989400934991649932596L)};
    const CPPUNIV_EXTENDED_REAL weights[]={CPPUNIV_EXTENDED_LITERAL(.189450610455068496285,.189450610455068496285L),CPPUNIV_EXTENDED_LITERAL(.182603415044923588867,.182603415044923588867L),
        CPPUNIV_EXTENDED_LITERAL(.169156519395002538189,.169156519395002538189L),CPPUNIV_EXTENDED_LITERAL(.149595988816576732081,.149595988816576732081L),CPPUNIV_EXTENDED_LITERAL(.124628971255533872052,.124628971255533872052L),
        CPPUNIV_EXTENDED_LITERAL(.095158511682492784810,.095158511682492784810L),CPPUNIV_EXTENDED_LITERAL(.062253523938647892863,.062253523938647892863L),CPPUNIV_EXTENDED_LITERAL(.027152459411754094852,.027152459411754094852L)};
    std::vector<CPPUNIV_EXTENDED_REAL> sums(6,0), corrections(6,0);
    const int panels=256;
    const CPPUNIV_EXTENDED_REAL step=2*radius/panels;
    for (int panel=0;panel<panels;++panel) {
        const CPPUNIV_EXTENDED_REAL mid=-radius+(panel+CPPUNIV_EXTENDED_LITERAL(.5,.5L))*step;
        for (int j=0;j<8;++j) for (int sign : {-1,1}) {
            const CPPUNIV_EXTENDED_REAL y=mid+sign*step*nodes[j]/2, r=res(y);
            const CPPUNIV_EXTENDED_REAL w=std::exp(-n*r)*weights[j]*step/2;
            const CPPUNIV_EXTENDED_REAL values[]={w,w*y,w*r,w*y*y,w*r*r,w*y*r};
            for (int k=0;k<6;++k) Sums::compensated(sums[k],corrections[k],values[k]);
        }
    }
    out.work=panels*16;
    // |d^j log Gamma(x+1)| <= (j-2)! [x^(1-j)+(j-1)x^-j], j>=2,
    // from the positive polygamma series and its integral bound.
    const CPPUNIV_EXTENDED_REAL lower=z-radius*sigma;
    std::vector<CPPUNIV_EXTENDED_REAL> rbound(33,0), ell(33,0), wbound(33,0), ybound(33,0);
    rbound[0]=std::max(std::abs(res(-radius)),std::abs(res(radius)))+sigma/lower;
    rbound[1]=sigma*std::max(std::abs(std::log1p(-radius*sigma/z)),std::abs(std::log1p((radius*sigma+1)/z)));
    ell[1]=n*rbound[1]; wbound[0]=std::exp(n/z); ybound[0]=radius; ybound[1]=1;
    CPPUNIV_EXTENDED_REAL power=sigma*sigma/lower;
    for (int j=2;j<=32;++j) {
        rbound[j]=power*(1+(j-1)/lower)/(j*(j-1));
        ell[j]=n*rbound[j]; power*=sigma/lower;
    }
    for (int j=1;j<=32;++j)
        for (int i=1;i<=j;++i) wbound[j]+=i*ell[i]*wbound[j-i]/j;
    const auto product=[](const std::vector<CPPUNIV_EXTENDED_REAL>& a,const std::vector<CPPUNIV_EXTENDED_REAL>& b) {
        std::vector<CPPUNIV_EXTENDED_REAL> r(33,0);
        for (int j=0;j<=32;++j) for (int i=0;i<=j;++i) r[j]+=a[i]*b[j-i];
        return r;
    };
    const std::vector<std::vector<CPPUNIV_EXTENDED_REAL>> bounds={wbound,product(wbound,ybound),
        product(wbound,rbound),product(wbound,product(ybound,ybound)),
        product(wbound,product(rbound,rbound)),product(wbound,product(ybound,rbound))};
    const CPPUNIV_EXTENDED_REAL endpoint=std::exp(-n*res(-radius))+std::exp(-n*res(radius));
    // For |u|<.01, the log1p remainder after u^32 contributes
    // less than 1e-60 to nu*residual across this window.
    const CPPUNIV_EXTENDED_REAL stirling_error=2*n/(1188*std::pow(lower,9))+CPPUNIV_EXTENDED_LITERAL(1e-60,1e-60L);
    CPPUNIV_EXTENDED_REAL worst=0;
    for (int p=0;p<6;++p) {
        const auto& b=bounds[p];
        CPPUNIV_EXTENDED_REAL factorial=1, em=endpoint*b[0], sigmapower=1;
        for (int j=1;j<=16;++j) {
            factorial*=j; sigmapower/=sigma;
            if (j < 16) em+=endpoint*4/std::pow(2*pi,j+1)*factorial*b[j]*sigmapower;
        }
        em+=4/std::pow(2*pi,16)*factorial*b[16]*2*radius*sigma*sigmapower;
        // Gauss-Legendre remainder: h^33 (16!)^4 / [33 (32!)^3] sup |g^(32)|.
        const CPPUNIV_EXTENDED_REAL gauss=panels*std::pow(step,33)*
            std::exp(4*std::lgamma(CPPUNIV_EXTENDED_LITERAL(17.,17.L))-2*std::lgamma(CPPUNIV_EXTENDED_LITERAL(33.,33.L)))/33*b[32];
        const CPPUNIV_EXTENDED_REAL error=em/sigma+gauss+stirling_error*2*radius*b[0];
        // Odd moments may vanish; certify against the corresponding scale.
        const CPPUNIV_EXTENDED_REAL scale=p == 1 ? std::sqrt(sums[0]*sums[3]) :
            p == 5 ? std::sqrt(sums[3]*sums[4]) : std::abs(sums[p]);
        if (!std::isfinite(error) || !std::isfinite(scale) || !(scale > 0)) return out;
        worst=std::max(worst,error/scale);
    }
    cmp_detail::Sums ts;
    ts.z=sigma*sums[0]; ts.extra=ts.z;
    ts.dd=sigma*sigma*sigma*sums[3]; ts.rr=sigma*sums[4];
    for (int sign : {-1,1}) {
        const CPPUNIV_EXTENDED_REAL y=sign*radius, d=sigma*y, r=res(y);
        const CPPUNIV_EXTENDED_REAL lr=sign > 0 ? -n*std::log1p(d/z) : n*std::log1p(d/z);
        // Include a possibly fractional first interval before the geometric tail.
        const CPPUNIV_EXTENDED_REAL dt=std::abs(d)+1, rt=(dt*dt+dt)/z+lm+2;
        if (!std::isfinite(ts.dd) || -lr < 1e-70) {
            const CPPUNIV_EXTENDED_REAL ls=std::log(sigma);
            const CPPUNIV_EXTENDED_REAL le=ls+std::log(sums[0]), ld=3*ls+std::log(sums[3]),
                lh=ls+std::log(sums[4]);
            worst+=tail_bound_log(-n*r,lr,dt,rt,(2*dt+1)/z,1/z,le,ld,lh,tol)*tol;
            worst+=std::exp(-n*r+std::max({-le,2*std::log(dt)-ld,2*std::log(rt)-lh}));
        } else {
            worst+=tail_bound(-n*r,lr,dt,rt,(2*dt+1)/z,ts,tol,1/z)*tol;
            worst+=std::exp(-n*r)*std::max({1/ts.extra,dt*dt/ts.dd,rt*rt/ts.rr});
        }
    }
    if (!(worst < tol)) return out;
    const CPPUNIV_EXTENDED_REAL md=sigma*sums[1]/sums[0], mr=sums[2]/sums[0];
    const CPPUNIV_EXTENDED_REAL vd=sigma*sigma*(sums[3]/sums[0]-std::pow(sums[1]/sums[0],2));
    const CPPUNIV_EXTENDED_REAL vr=sums[4]/sums[0]-mr*mr;
    const CPPUNIV_EXTENDED_REAL cr=sigma*(sums[5]/sums[0]-sums[1]/sums[0]*mr);
    const CPPUNIV_EXTENDED_REAL sc=stirling_correction(z);
    out.log_scaled_z=std::log(sigma)+std::log(sums[0]);
    out.log_z=static_cast<double>(n*z-n*CPPUNIV_EXTENDED_LITERAL(.5,.5L)*(std::log(2*pi)+lm)-n*sc+out.log_scaled_z);
    out.center=z; out.slope=lm; out.offset=md; out.residual_mean=mr;
    out.mean=static_cast<double>(z+md); out.mean_h=static_cast<double>(std::lgamma(z+1)+lm*md+mr);
    out.variance=static_cast<double>(vd); out.variance_h=static_cast<double>(lm*lm*vd+2*lm*cr+vr);
    out.covariance=static_cast<double>(lm*vd+cr);
    out.variance_residual=static_cast<double>(vr); out.covariance_residual=static_cast<double>(cr);
    out.relative_tail_bound=static_cast<double>(worst);
    if (!std::isfinite(out.log_z) || !std::isfinite(out.mean) || !std::isfinite(out.variance) ||
        !std::isfinite(out.mean_h) || !std::isfinite(out.variance_h) || !std::isfinite(out.covariance)) return out;
    out.status=CmpStatus::Ok;
    return out;
}

}

// max_terms bounds total work, independently of the mode. Both tails are
// bounded geometrically; one need not enumerate billions of lower indices.
inline CmpMoments cmp_moments(double eta, double nu, double tol = 2e-16,
                              std::size_t max_terms = 1000000) {
    CmpMoments out;
    if (!std::isfinite(eta) || !std::isfinite(nu) || nu < 0 ||
        !(tol > 0 && tol < 1) || max_terms < 2 || (nu == 0 && eta >= 0)) return out;
    if (nu == 0) {
        out.regime=CmpRegime::Geometric;
        out.log_z=-std::log(-std::expm1(eta));
        out.mean=1/std::expm1(-eta);
        out.variance=out.mean*(1+out.mean);
    }
    const CPPUNIV_EXTENDED_REAL e=eta, n=nu;
    const CPPUNIV_EXTENDED_REAL lm=nu == 0 ? -special::kPosInf : e/n;
    std::size_t preliminary_work=0;
    if (max_terms >= 4096 && nu > 0 && lm > std::log(CPPUNIV_EXTENDED_LITERAL(1e6,1e6L)) && lm-std::log(nu) >= std::log(CPPUNIV_EXTENDED_LITERAL(1e6,1e6L))) {
        CmpMoments smooth=cmp_detail::euler_maclaurin(eta,nu,tol);
        if (smooth.status == CmpStatus::Ok) return smooth;
        if (lm > std::log(CPPUNIV_EXTENDED_LITERAL(9e15,9e15L))) return smooth;
        preliminary_work=smooth.work;
    }
    if (lm > std::log(CPPUNIV_EXTENDED_LITERAL(9e15,9e15L))) { out.status=CmpStatus::WorkLimit; return out; }
    const CPPUNIV_EXTENDED_REAL mode=lm > 0 ? std::floor(std::exp(lm)) : 0;
    const CPPUNIV_EXTENDED_REAL h0=std::lgamma(mode+1), a=mode <= 1 ? 0 : std::log(mode+1);
    cmp_detail::Sums s;
    out.work=1+preliminary_work;
    CPPUNIV_EXTENDED_REAL worst=0;
    for (int direction : {-1,1}) {
        CPPUNIV_EXTENDED_REAL k=mode, lw=0, lw_correction=0, d=0, r=0;
        bool done=false;
        while (out.work < max_terms) {
            if (direction < 0 && k == 0) { done=true; break; }
            const CPPUNIV_EXTENDED_REAL index=direction > 0 ? k+1 : k;
            const CPPUNIV_EXTENDED_REAL dr=mode <= 1 ? std::log(index) :
                std::log1p((index-(mode+1))/(mode+1));
            CPPUNIV_EXTENDED_REAL increment=direction > 0 ? (e-n*a)-n*dr : (n*a-e)+n*dr;
#ifdef BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
            if (mode<=1 && index==2) {
                // Retain the low part of log(2) and the log-weight sum.
                const double log2_lo=2.3190468138462995584e-17;
                const double product=n*dr;
                const double product_error=std::fma(n,dr,-product)+n*log2_lo;
                cmp_detail::Sums::compensated(lw,lw_correction,direction*e);
                cmp_detail::Sums::compensated(lw,lw_correction,-direction*product);
                cmp_detail::Sums::compensated(lw,lw_correction,-direction*product_error);
            } else
#endif
            cmp_detail::Sums::compensated(lw,lw_correction,increment);
            k += direction; d += direction; r += direction*dr;
#ifdef BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
            if (mode<=1 && k==2 && direction>0 && std::abs(e)<300 && n<1000)
                s.add_weight(std::exp((2-mode)*e)*std::exp2(-n),d,r);
            else
#endif
                s.add(lw,d,r,lw_correction);
            ++out.work;
            if (direction < 0 && k == 0) { done=true; break; }
            const CPPUNIV_EXTENDED_REAL next_index=direction > 0 ? k+1 : k;
            const CPPUNIV_EXTENDED_REAL next_dr=mode <= 1 ? std::log(next_index) :
                std::log1p((next_index-(mode+1))/(mode+1));
            const CPPUNIV_EXTENDED_REAL lr=direction > 0 ? (e-n*a)-n*next_dr : (n*a-e)+n*next_dr;
            const CPPUNIV_EXTENDED_REAL c=direction > 0 ? std::abs(next_dr) : a+std::log(mode+1);
            const CPPUNIV_EXTENDED_REAL bound=cmp_detail::tail_bound(lw,lr,d,r,c,s,tol);
            if (bound <= 1) { worst += bound*tol; done=true; break; }
        }
        if (!done) {
            out.status=CmpStatus::WorkLimit; return out;
        }
    }
    const CPPUNIV_EXTENDED_REAL md=s.d/s.z, mr=s.r/s.z;
    const CPPUNIV_EXTENDED_REAL vd=std::max(CPPUNIV_EXTENDED_LITERAL(0.,0.L),s.dd/s.z-md*md);
    const CPPUNIV_EXTENDED_REAL vr=std::max(CPPUNIV_EXTENDED_LITERAL(0.,0.L),s.rr/s.z-mr*mr), cr=s.dr/s.z-md*mr;
    out.log_z=static_cast<double>(mode*e-n*h0+std::log1p(s.extra));
    out.mean=static_cast<double>(mode+md);
    out.variance=static_cast<double>(vd);
    out.mean_h=static_cast<double>(h0+a*md+mr);
    out.variance_h=static_cast<double>(a*a*vd+2*a*cr+vr);
    out.covariance=static_cast<double>(a*vd+cr);
    out.slope=a;
    out.center=mode; out.offset=md; out.residual_mean=mr;
    out.log_scaled_z=std::log1p(s.extra);
    out.variance_residual=static_cast<double>(vr);
    out.covariance_residual=static_cast<double>(cr);
    out.relative_tail_bound=static_cast<double>(worst);
    out.status=CmpStatus::Ok;
    if (nu == 0) {
        out.regime=CmpRegime::Geometric;
        out.log_z=-std::log(-std::expm1(eta));
        out.mean=1/std::expm1(-eta);
        out.variance=out.mean*(1+out.mean);
    }
    return out;
}

struct CmpFitControl {
    double tol = 1e-10;
    int maxit = 100;
    double fixed_nu = special::kNaN;
    double fixed_lambda = special::kNaN;
    std::size_t max_terms = 1000000;
};
struct CmpResult {
    double eta = special::kNaN, nu = special::kNaN;
    double logLik = special::kNaN;
    double mean = special::kNaN, mean_h = special::kNaN;
    int iterations = 0;
    std::size_t work = 0;
    CmpStatus status = CmpStatus::InvalidInput;
};

namespace cmp_detail {
inline CmpMoments mean_match(double m, double nu, double& eta,
                             const CmpFitControl& ctl, std::size_t& work) {
    double lo=-special::kPosInf, hi=special::kPosInf;
    eta=nu*std::log(m+0.5);
    if (nu == 0) eta=-std::log1p(1/m);
    CmpMoments v;
    for (int i=0;i<ctl.maxit;++i) {
        v=cmp_moments(eta,nu,2e-16,ctl.max_terms); work+=v.work;
        if (v.status != CmpStatus::Ok) return v;
        const CPPUNIV_EXTENDED_REAL err=(v.center-m)+v.offset;
        const double resolution=2*std::numeric_limits<double>::epsilon()*
            std::max(1.0,std::abs(eta))*v.variance;
        if (std::abs(err) <= resolution+5e-16*m+
            ctl.tol*std::max(1.0,std::sqrt(v.variance))) return v;
        if (err < 0) lo=eta; else hi=eta;
        double next=eta-err/v.variance;
        if (!(next > lo && next < hi) || !std::isfinite(next)) {
            if (std::isfinite(lo) && std::isfinite(hi)) next=(lo+hi)/2;
            else next=eta+(err < 0 ? 1 : -1)*std::max(1.0,std::abs(eta)*0.5);
        }
        eta=next;
    }
    v.status=CmpStatus::IterationFailure; return v;
}
// Jensen on conditional geometric blocks gives a lower bound for E h.
// This cheaply proves interior membership for high-count, concentrated data.
inline CPPUNIV_EXTENDED_REAL geometric_lower(CPPUNIV_EXTENDED_REAL m) {
    const CPPUNIV_EXTENDED_REAL rate=std::log1p(1/m);
    const CPPUNIV_EXTENDED_REAL step=std::max(CPPUNIV_EXTENDED_LITERAL(1.,1.L),std::floor(m/32));
    CPPUNIV_EXTENDED_REAL bound=0;
    for (int j=0;j<1024;++j) {
        const CPPUNIV_EXTENDED_REAL lo=j*step;
        const CPPUNIV_EXTENDED_REAL prob=std::exp(-rate*lo)*(-std::expm1(-rate*step));
        const CPPUNIV_EXTENDED_REAL conditional=lo+m-step/std::expm1(rate*step);
        bound+=prob*std::lgamma(conditional+1);
    }
    bound+=std::exp(-rate*1024*step)*std::lgamma(1024*step+m+1);
    return bound;
}
}

inline CmpResult cmp_mle(span x, const CmpFitControl& ctl = {}) {
    CmpResult out;
    const bool fn=std::isfinite(ctl.fixed_nu), fl=std::isfinite(ctl.fixed_lambda);
    if (x.empty() || !(ctl.tol > 0 && ctl.tol < 1) || ctl.maxit < 1 ||
        (!std::isnan(ctl.fixed_nu) && !fn) ||
        (!std::isnan(ctl.fixed_lambda) && !fl) ||
        (fn && ctl.fixed_nu < 0) || (fl && ctl.fixed_lambda < 0) || ctl.max_terms<2 ||
        (fl && fn && ctl.fixed_nu==0 && ctl.fixed_lambda>=1)) return out;
    CPPUNIV_EXTENDED_REAL sx=0, sh=0;
    double xmin=special::kPosInf, xmax=0;
    for (double v : x) {
        if (!std::isfinite(v) || v < 0 || v != std::floor(v) || v > 2147483647.0) return out;
        sx+=v; sh+=std::lgamma(static_cast<CPPUNIV_EXTENDED_REAL>(v)+1);
        xmin=std::min(xmin,v); xmax=std::max(xmax,v);
    }
    if (fl && ctl.fixed_lambda==0 && xmax>0) return out;
    if (xmax==0 && (!fl || ctl.fixed_lambda==0)) {
        out.eta=special::kNegInf; out.nu=fn?ctl.fixed_nu:1;
        out.logLik=0; out.mean=0; out.mean_h=0; out.status=CmpStatus::Ok;
        return out;
    }
    const CPPUNIV_EXTENDED_REAL m=sx/x.size(), t=sh/x.size();
    const CPPUNIV_EXTENDED_REAL pivot=std::floor(m), adata=std::log(pivot+1);
    CPPUNIV_EXTENDED_REAL data_residual=0;
    for (double k : x) data_residual+=cmp_detail::residual(k,pivot,adata);
    data_residual/=x.size();
    out.mean=static_cast<double>(m); out.mean_h=static_cast<double>(t);
    if (!fn && !fl && xmax-xmin <= 1) {
        out.status=xmax == xmin ? CmpStatus::PointMassLimit : CmpStatus::TwoPointLimit;
        const CPPUNIV_EXTENDED_REAL p=m-xmin;
        out.logLik=static_cast<double>(xmax == xmin ? 0 : x.size()*
            (p*std::log(p)+(1-p)*std::log1p(-p)));
        return out;
    }
    if (m == 0 && !fl) { out.status=CmpStatus::PointMassLimit; out.logLik=0; return out; }
    if (fn && ctl.fixed_nu == 0) {
        const double eg=fl ? std::log(ctl.fixed_lambda) : -std::log1p(1/static_cast<double>(m));
        if (!(eg < 0)) return out;
        out.eta=eg; out.nu=0; out.status=CmpStatus::Ok;
        out.logLik=static_cast<double>(sx*eg+x.size()*std::log(-std::expm1(eg)));
        return out;
    }
    if (fl && !fn && t == 0) {
        out.eta=std::log(ctl.fixed_lambda); out.nu=special::kPosInf;
        out.status=CmpStatus::TwoPointLimit;
        out.logLik=static_cast<double>(sx*out.eta-x.size()*
            (out.eta > 0 ? out.eta+std::log1p(std::exp(-out.eta)) : std::log1p(std::exp(out.eta))));
        return out;
    }
    double eta=fl ? std::log(ctl.fixed_lambda) : 0;
    double nu=fn ? ctl.fixed_nu : 1;
    CmpMoments v;
    // Existence before solving the interior: geometric endpoint, eta<0.
    double geometric_ll=special::kNaN, geometric_eta=special::kNaN;
    if (!fn && (!fl || eta < 0)) {
        geometric_eta=fl ? eta : -std::log1p(1/static_cast<double>(m));
        geometric_ll=static_cast<double>(sx*geometric_eta+
            x.size()*std::log(-std::expm1(geometric_eta)));
    }
    if (!fn && (!fl || eta < 0) &&
        !(!fl && m > 1000 && t < cmp_detail::geometric_lower(m))) {
        const double eg=fl ? eta : -std::log1p(1/static_cast<double>(m));
        v=cmp_moments(eg,0,2e-16,ctl.max_terms); out.work+=v.work;
        if (v.status != CmpStatus::Ok) { out.status=v.status; return out; }
        geometric_eta=eg; geometric_ll=static_cast<double>(sx*eg-x.size()*v.log_z);
        if (t >= v.mean_h) {
            out.eta=eg; out.nu=0; out.status=CmpStatus::Ok;
            out.logLik=static_cast<double>(sx*eg-x.size()*v.log_z); return out;
        }
    }
    double lo=0, hi=special::kPosInf;
    for (int i=0;i<ctl.maxit;++i) {
        out.iterations=i+1;
        if (fl) { v=cmp_moments(eta,nu,2e-16,ctl.max_terms); out.work+=v.work; }
        else v=cmp_detail::mean_match(static_cast<double>(m),nu,eta,ctl,out.work);
        if (v.status != CmpStatus::Ok) { out.status=v.status; return out; }
        out.eta=eta; out.nu=nu;
        const CPPUNIV_EXTENDED_REAL amode=v.slope;
        const CPPUNIV_EXTENDED_REAL data_at_mode=data_residual+cmp_detail::residual(pivot,v.center,amode)+
            (adata-amode)*(m-pivot);
        out.logLik=static_cast<double>(x.size()*((m-v.center)*(eta-nu*amode)-
            nu*data_at_mode-v.log_scaled_z));
        const CPPUNIV_EXTENDED_REAL mean_error=(v.center-m)+v.offset;
        // Project out the small remaining mean-solving error. This avoids
        // amplifying binary64 eta resolution by log(m) at large counts.
        const CPPUNIV_EXTENDED_REAL linear=fl ? amode : -v.covariance_residual/v.variance;
        const double score=static_cast<double>(v.residual_mean-data_at_mode+linear*mean_error);
        if (fn || std::abs(score) <= ctl.tol*std::max(1e-12, std::sqrt(v.variance_residual))) {
            out.status=v.status;
            if (!fn && std::isfinite(geometric_ll) &&
                out.logLik-geometric_ll <= 1e-8*x.size()) {
                out.eta=geometric_eta; out.nu=0; out.logLik=geometric_ll;
                out.status=CmpStatus::Ok;
            }
            return out;
        }
        if (score > 0) lo=nu; else hi=nu;
        const double info=fl ? v.variance_h : v.variance_residual-
            v.covariance_residual*v.covariance_residual/v.variance;
        double next=nu+score/info;
        if (!(next > lo && next < hi) || !std::isfinite(next))
            next=std::isfinite(hi) ? (lo+hi)/2 : nu*2;
        nu=next;
    }
    out.status=CmpStatus::IterationFailure; return out;
}

} // namespace cppunivariate
