// Conditional shape/scale certificate; proof and arithmetic contract in README.md.
#pragma once
#include <queue>
namespace cppunivariate {
namespace betapr_cert_detail {
struct Sum {
  double value=0,correction=0;
  void add(double x){const double y=x-correction,t=value+y;correction=(t-value)-y;value=t;}
};
struct Difference {double value,error;};
inline Difference increment(double a,double b) {
  const double eps=std::numeric_limits<double>::epsilon();
  if(b<32) {
    const double p=special::digamma(a+b),q=special::digamma(b);
    return {p-q,64*eps*(1+std::abs(p)+std::abs(q))};
  }
  const double l=std::log1p(a/b),inv=1/b;
  const double inv2=inv*inv,inv4=inv2*inv2,inv6=inv4*inv2,inv8=inv4*inv4;
  const double value=l+.5*(a/b)/(a+b)-inv2*std::expm1(-2*l)/12+
    inv4*std::expm1(-4*l)/120-inv6*std::expm1(-6*l)/252+
    inv8*std::expm1(-8*l)/240;
  // Alternating digamma asymptotics bound each omitted remainder by
  // |B_10|/(10*t^10); differencing the two remainders is conservative.
  const double remainder=inv8*inv2*(1+std::exp(-10*l))/132;
  return {value,remainder+64*eps*std::abs(value)};
}
struct Point { double b=NAN,s=NAN,ll=NAN,d=NAN,error=NAN,scale_error=NAN; bool valid=false; };
struct Segment {
  Point l,r; double upper,curvature_upper=INFINITY;
  bool operator<(const Segment& other) const {return upper<other.upper;}
};
inline BetaPrimeResult fit(span x,double a,double tol,int maxit) {
  maxit=static_cast<int>(solver_budget(maxit));
  (void)tol;
  BetaPrimeResult out{a,NAN,NAN,NAN,0,BetaPrimeStatus::BfgsFailure};
  out.certificate_failure=LomaxCertificateFailure::Arithmetic;
  if(x.empty()||!(a>0)||!std::isfinite(a))return out;
  double mean=0,lm=0,xmin=INFINITY,xmax=0,second=0;
  bool constant=true;Sum logsum;
  for(double v:x) {
    if(!(v>0)||!std::isfinite(v)){out.status=BetaPrimeStatus::InvalidStart;return out;}
    logsum.add(std::log(v)/x.size());
    xmin=std::min(xmin,v);xmax=std::max(xmax,v);constant=constant&&v==x[0];
  }
  Sum scaled_mean;
  for(double v:x)scaled_mean.add((v/xmax)/x.size());
  mean=xmax*scaled_mean.value;lm=logsum.value;
  if(!(mean>0)||!std::isfinite(mean)||std::numeric_limits<double>::denorm_min()/mean>1e-7)return out;
  // Normalize by the arithmetic mean; all following likelihoods are per datum.
  std::vector<double> y;for(double v:x){y.push_back(v/mean);second+=(v/mean)*(v/mean)/x.size();}
  xmin/=mean;xmax/=mean;lm-=std::log(mean);
  const double n=x.size(), face=a*std::log(a)-special::lgamma(a)+(a-1)*lm-a;
  const double face_error=512*std::numeric_limits<double>::epsilon()*(1+std::abs(face)+std::abs(special::lgamma(a))+a);
  const double face_ll=n*(face-std::log(mean));
  out.stop_detail=limit_detail({a,mean/a},face_ll,StopModel::Gamma);
  for(auto& c:out.stop_detail.candidates){c.loglik_lower-=n*face_error;c.loglik_upper+=n*face_error;}
  out.stop_detail.loglik_lower=face_ll-n*face_error;
  out.stop_detail.limit_coef.clear();out.stop_detail.limit_loglik=NAN;out.stop_detail.limit_model=StopModel::None;
  out.stop_detail.loglik_upper=INFINITY;
  if(!std::isfinite(face)||!(mean/a>0)||!std::isfinite(mean/a)||!(xmin>0)||!std::isfinite(second)||face_error>1e-7||std::numeric_limits<double>::denorm_min()/(mean/a)>1e-7)return out;
  if(constant) {out.status=BetaPrimeStatus::GammaBoundary;out.logLik=face_ll;
    out.stop_detail.limit_coef={a,mean/a};out.stop_detail.limit_loglik=face_ll;out.stop_detail.limit_model=StopModel::Gamma;
    out.stop_detail.loglik_upper=out.logLik+n*face_error;out.certificate_failure=LomaxCertificateFailure::None;return out;}
  std::int64_t evaluations=0;
  bool budget=false,arithmetic=false;
  const auto point=[&](double b) {
    Point p;p.b=b;
    if(evaluations>=certificate_budget(maxit)){budget=true;return p;}++evaluations;
    // The scale score is strictly decreasing. Its root lies in
    // [b*xmin/a,b*xmax/a]; bisection is in log scale.
    double l=std::log(b)+std::log(xmin)-std::log(a),r=std::log(b)+std::log(xmax)-std::log(a);
    bool converged=false;
    for(int j=0;j<maxit;++j) {
      const double m=(l+r)/2,s=std::exp(m);
      Sum q;for(double v:y)q.add(v/(s+v)/n);
      const double score=(a+b)*q.value-a;
      if(score>0)l=m;else r=m;
      if(r-l<8*std::numeric_limits<double>::epsilon()*(1+std::abs(m))){converged=true;break;}
    }
    if(!converged)return p;
    p.s=std::exp((l+r)/2);
    p.scale_error=r-l+8*std::numeric_limits<double>::epsilon()*(1+std::abs((l+r)/2));
    Sum logs_sum;for(double v:y)logs_sum.add(std::log1p(v/p.s)/n);
    const double logs=logs_sum.value;
    const CPPUNIV_EXTENDED_REAL aa=a,bb=b;
    const double lb=static_cast<double>(std::lgamma(aa)+std::lgamma(bb)-std::lgamma(aa+bb));
    p.ll=(a-1)*lm-a*std::log(p.s)-(a+b)*logs-lb;
    p.d=increment(a,b).value-logs;
    p.error=512*std::numeric_limits<double>::epsilon()*(1+std::abs(p.ll)+std::abs(lb)+a+b);
    p.valid=p.s>0&&std::isfinite(p.s)&&std::isfinite(p.ll)&&std::isfinite(p.d)&&p.error<1e-7;
    return p;
  };
  Point best;
  const auto consider=[&](const Point& p){if(p.valid&&(!best.valid||p.ll>best.ll))best=p;};
  // A positive derivative on (0,L] follows from trigamma(t)>1/t^2
  // and monotonicity of b*log(1+c/b).
  double lower=1;
  bool left=false;
  for(int j=0;j<maxit;++j) {
    double bound=a/(a+lower);
    for(double v:y)bound-=lower*std::log1p(a*v/(lower*xmin))/n;
    if(bound>1e-10){left=true;break;}lower*=.5;
  }
  if(!left)return out;
  Point l=point(lower);consider(l);if(!l.valid)goto stopped;
  {
    std::priority_queue<Segment> queue;
    // The profile derivative is enclosed using monotonicity of scale(b),
    // digamma(a+b)-digamma(b), and log(1+x/s).
    const auto segment=[&](Point p,Point q) {
      if(evaluations>=certificate_budget(maxit)){budget=true;return Segment{p,q,INFINITY};}
      ++evaluations;
      const auto gl=increment(a,p.b),gr=increment(a,q.b);
      const double dl=gr.value,du=gl.value;
      Sum logs_l,logs_r;
      for(double v:y){logs_l.add(std::log1p(v/p.s)/n);logs_r.add(std::log1p(v/q.s)/n);}
      double low=dl-logs_l.value,high=du-logs_r.value;
      // The log-mean perturbation is weighted by mean q=a/(a+b).
      const double e=gl.error+gr.error+128*std::numeric_limits<double>::epsilon()*
        (std::abs(dl)+std::abs(du)+logs_l.value+logs_r.value)+
        2*a*(p.scale_error/(a+p.b)+q.scale_error/(a+q.b));
      low-=e;high+=e;
      // Schur complement of the (b,log scale) Hessian encloses D'.
      double curvature_upper=INFINITY;
      Sum qmin,qmax;
      for(double v:y) {
        const double u=v/(p.s+v),w=v/(q.s+v),cu=u*(1-u),cw=w*(1-w);
        qmin.add(std::min(cu,cw)/n);
        qmax.add((w<=.5&&u>=.5?.25:std::max(cu,cw))/n);
      }
      const double qe=128*std::numeric_limits<double>::epsilon()*(qmin.value+qmax.value)+
        2*a*(p.scale_error/(a+p.b)+q.scale_error/(a+q.b));
      if(qmin.value>qe) {
        const double pl=special::trigamma(a+p.b),ql=special::trigamma(p.b),
                     pr=special::trigamma(a+q.b),qr=special::trigamma(q.b);
        const double pe=128*std::numeric_limits<double>::epsilon()*(pl+ql+pr+qr);
        const double hl=pl-ql-pe+(a/(a+q.b))*(a/(a+q.b))/(a+q.b)/(qmax.value+qe),
                     hh=pr-qr+pe+(a/(a+p.b))*(a/(a+p.b))/(a+p.b)/(qmin.value-qe);
        const double width=q.b-p.b,dleft=gl.value-logs_l.value,dright=gr.value-logs_r.value;
        if(std::isfinite(hl)&&std::isfinite(hh)&&hl<=hh) {
          curvature_upper=hh;
          low=std::max(low,std::max(dleft-e+std::min(0.,hl)*width,
                                   dright-e-std::max(0.,hh)*width));
          high=std::min(high,std::min(dleft+e+std::max(0.,hh)*width,
                                     dright+e-std::min(0.,hl)*width));
        }
      }
      if(low>high||!std::isfinite(low)||!std::isfinite(high)){arithmetic=true;return Segment{p,q,INFINITY};}
      // Integrating either derivative bound gives independent upper bounds.
      const double u=(low>0||high<0)?-INFINITY:std::min(p.ll+p.error+std::max(0.,high)*(q.b-p.b),
                             q.ll+q.error+std::max(0.,-low)*(q.b-p.b));
      return Segment{p,q,u,curvature_upper};
    };
    consider(l);
    double upper=std::max(1.,lower*2);
    bool right=false;
    const double leading=a*.5*(a*(second-1)-1);
    const double remainder=a*a/4+2*a*a*a*xmax*(1+2*second)+2*a*a*a*second;
    const double leading_error=1e-10*(1+a*a*second);
    const bool negative_leading=leading < -leading_error;
    Point r;
    for(int j=0;j<maxit;++j) {
      r=point(upper);if(!r.valid)goto stopped;consider(r);
      queue.push(segment(l,r));if(budget||arithmetic)goto stopped;
      // For b>=B>2*a*xmax, r=b/s<=2*a. logGamma increment
      // <=a*log(b+a), and log(1+z)>=z-z^2/2, hence
      // profile(b)<=gamma_max+(a^2+2*a^2*E[y^2])/B.
      const double tail=face+face_error+std::min((a*a+2*a*a*second)/upper,
        std::max(0.,(leading+leading_error)/upper+remainder*(1+1e-10)/upper/upper));
      if(upper>2*a*xmax && (best.ll-best.error>tail+1e-10 ||
          (negative_leading && remainder*(1+1e-10)/upper < -(leading+leading_error)/2))) {right=true;break;}
      l=r;upper*=2;if(!std::isfinite(upper))goto stopped;
    }
    // A negative leading 1/b coefficient and the uniform 1/b^2 remainder
    // can instead certify that the entire tail lies below its gamma limit.
    if(!right)goto stopped;
    double possible_lo=INFINITY,possible_hi=0,scale_lo=INFINITY,scale_hi=0,global_upper=std::max(face+face_error,best.ll+best.error);
    Point retained,left_root,right_root;
    while(!queue.empty()) {
      const Segment seg=queue.top();queue.pop();
      if(std::isfinite(seg.upper))out.stop_detail.segment_loglik_upper=n*(seg.upper-std::log(mean));
      if(seg.upper<std::max(face-face_error,best.ll-best.error))continue;
      if(seg.r.b-seg.l.b<1e-10*seg.l.b && seg.r.s-seg.l.s<1e-10*seg.l.s) {
        global_upper=std::max(global_upper,seg.upper);
        const double witness=n*(seg.l.ll-std::log(mean));
        if(seg.l.s*mean>0 && std::isfinite(seg.l.s*mean))
          out.stop_detail.candidates.push_back({{a,seg.l.b,seg.l.s*mean},witness-n*seg.l.error,witness+n*seg.l.error});
        if(seg.l.b<possible_lo)left_root=seg.l;
        if(seg.r.b>possible_hi)right_root=seg.r;
        possible_lo=std::min(possible_lo,seg.l.b);possible_hi=std::max(possible_hi,seg.r.b);
        scale_lo=std::min(scale_lo,seg.l.s);scale_hi=std::max(scale_hi,seg.r.s);
        consider(seg.l);consider(seg.r);retained=best;continue;
      }
      Point mid=point(std::sqrt(seg.l.b)*std::sqrt(seg.r.b));
      if(!mid.valid)goto stopped;
      consider(mid);
      queue.push(segment(seg.l,mid));if(budget||arithmetic)goto stopped;
      queue.push(segment(mid,seg.r));if(budget||arithmetic)goto stopped;
    }
    out.stop_detail.loglik_upper=n*(global_upper-std::log(mean));
    if(possible_lo==INFINITY) {
      if(best.ll-best.error>face+face_error)goto stopped;
      out.status=BetaPrimeStatus::GammaBoundary;out.logLik=face_ll;
      out.stop_detail.limit_coef={a,mean/a};out.stop_detail.limit_loglik=face_ll;out.stop_detail.limit_model=StopModel::Gamma;
      out.stop_detail.loglik_upper=n*(face+face_error-std::log(mean));
      out.certificate_failure=LomaxCertificateFailure::None;
      out.iterations=static_cast<int>(evaluations);return out;
    }
    if(!retained.valid||!(possible_hi>=possible_lo)||possible_hi-possible_lo>1e-6*possible_lo||scale_hi-scale_lo>1e-6*scale_lo) {
      out.stop_detail.comparison_tie=true;goto stopped;
    }
    const auto peak=segment(left_root,right_root);
    if(budget||arithmetic)goto stopped;
    if(!(peak.curvature_upper<0)){out.stop_detail.comparison_tie=true;goto stopped;}
    best=point(std::sqrt(possible_lo)*std::sqrt(possible_hi));
    if(!best.valid)goto stopped;
    if(best.ll-best.error<=face+face_error) {out.stop_detail.comparison_tie=true;goto stopped;}
    out.beta=best.b;out.scale=best.s*mean;out.logLik=n*(best.ll-std::log(mean));
    if(!(out.scale>0)||!std::isfinite(out.scale)||std::abs(out.scale/mean/best.s-1)>1e-6)goto stopped;
    out.status=BetaPrimeStatus::Ok;out.certificate_failure=LomaxCertificateFailure::None;
    out.stop_detail.loglik_lower=out.logLik-n*best.error;
    out.stop_detail.loglik_upper=n*(global_upper-std::log(mean));
    out.stop_detail.candidates.push_back({{a,out.beta,out.scale},out.logLik-n*best.error,out.logLik+n*best.error});
    out.iterations=static_cast<int>(evaluations);return out;
  }
 stopped:
  if(best.valid && best.s*mean>0 && std::isfinite(best.s*mean)) {
    const double ll=n*(best.ll-std::log(mean));
    out.stop_detail.candidates.push_back({{a,best.b,best.s*mean},ll-n*best.error,ll+n*best.error});
    out.stop_detail.loglik_lower=std::max(out.stop_detail.loglik_lower,ll-n*best.error);
  }
  if(budget){out.certificate_failure=LomaxCertificateFailure::Budget;out.stop_detail.budget_exhausted=true;}
  out.iterations=static_cast<int>(evaluations);return out;
}
} // namespace betapr_cert_detail
} // namespace cppunivariate
