// Included after gb2_detail. Held beta shapes: a local curvature ball certifies
// the unique global maximum of a strictly concave log likelihood.
#pragma once
namespace cppunivariate {
namespace gb2_concave_detail {
inline Gb2Result fit(const trgamma_detail::LogData& d,double q,double a,
                     double p,double b,double tol,int maxit) {
  const bool ha=!std::isnan(a),hb=!std::isnan(b);
  Gb2Result out{q,a,p,b,NAN,0,Gb2Status::NumericalFailure};
  const double offset=hb?(d.mean-std::log(b))/d.sd:0;
  double A=ha?a*d.sd:std::sqrt(special::trigamma(p)+special::trigamma(q));
  double c=hb?0:special::digamma(q)-special::digamma(p);
  const double eps=std::numeric_limits<double>::epsilon();
  const double target=std::min(tol,1e-9);
  for(int it=0;it<maxit;++it) {
    ++out.iterations;
    const auto e=gb2_detail::evaluate(d,A,c,p,q,hb,offset);
    const double logb=hb?std::log(b):d.mean+d.sd*c/A;
    out.shape2=ha?a:A/d.sd;out.scale=hb?b:std::exp(logb);
    out.logLik=e.ll-d.n*std::log(d.sd)-d.sum;
    if(!(A>0)||!std::isfinite(e.ll)||!std::isfinite(out.logLik)||
       !(out.scale>0)||!std::isfinite(out.scale)||
       !(out.shape2>0)||!std::isfinite(out.shape2))return out;
    // Euclidean ball radius implies relative power error and log-scale error
    // below target/2, including the nonlinear c/A transformation.
    const double radius=target/(4*(1+1/A+d.sd/A+d.sd*std::abs(c)/(A*A)));
    double gram00=0,gram01=0,gram11=0,score_scale=1,ll_scale=1;
    double gradient_error=0,curvature_error=0,input_error=0;
    for(double z:d.u) {
      const double v=hb?z+offset:z;
      const double t=A*v-c;
      // Input logarithms, centering, division and the coefficient round trip.
      // These allowances grow with power; a large A must not hide log loss.
      const double dv=8*eps*(1+std::abs(d.mean)+d.sd*std::abs(z)+std::abs(logb))/d.sd;
      const double dt=A*dv+16*eps*(1+std::abs(A*v)+std::abs(c));
      const double du=std::min(1.,dt/4);
      gradient_error+=(ha?0:(p+q)*(dv+(std::abs(v)+dv)*du))+(hb?0:(p+q)*du);
      input_error+=(p+q)*dt+dv*d.sd;
      const double bound=std::abs(t)+dt+radius*(std::abs(v)+dv+(hb?0:1));
      const double ew=std::exp(-bound),w=ew/((1+ew)*(1+ew));
      gram00+=w*v*v;gram01-=w*v;gram11+=w;
      if(!ha)curvature_error+=(p+q)*w*(2*std::hypot(v,hb?0.:1.)*dv+dv*dv);
      ll_scale+=(p+q)*(1+std::abs(t));
      score_scale+=1+(p+q)*(1+std::abs(v));
    }
    const double k=p+q;
    const double h00=d.n/((A+radius)*(A+radius))+k*gram00;
    const double h01=k*gram01,h11=k*gram11;
    // det/trace is a lower bound on the smaller eigenvalue; subtract a
    // practical margin for accumulated products and cancellation.
    double m=ha?h11:hb?h00:(h00*h11-h01*h01-128*eps*(h00*h11+h01*h01))/(h00+h11);
    m=m*(1-128*eps*(1+d.n))-curvature_error;
    const double ga=ha?0:e.ga,gc=hb?0:e.gc;
    const double margin=8*eps*(score_scale+d.n/A);
    const double g=std::hypot(ga,gc)+margin+gradient_error;
    const double error=input_error+128*eps*(ll_scale+std::abs(e.ll)+std::abs(d.sum)+d.n*(1+std::abs(std::log(d.sd))+std::abs(std::log(A))+std::abs(gb2_detail::logbeta(p,q))));
    if(!std::isfinite(error))return out;
    out.stop_detail.candidates={{{q,out.shape2,p,out.scale},out.logLik-error,out.logLik+error,StopModel::None}};
    const bool representable=hb || std::nextafter(out.scale,INFINITY)-out.scale<target*out.scale/4;
    if(representable && radius>0 && radius<A/2 && m>0 && std::isfinite(m) && g<m*radius/2 &&
       128*eps*(1+std::abs(logb)+std::abs(d.mean)+d.sd*(1+std::abs(c))/A)<target/4) {
      out.status=Gb2Status::Ok;
      out.stop_detail.loglik_lower=out.logLik-error;
      out.stop_detail.loglik_upper=out.logLik+error+g*radius;
      return out;
    }
    double da=0,dc=0;
    if(ha&&hb)return out;
    if(ha)dc=-e.gc/e.hcc;
    else if(hb)da=-e.ga/e.haa;
    else {
      const double det=e.haa*e.hcc-e.hac*e.hac;
      if(!(det>0)||!std::isfinite(det))return out;
      da=(-e.hcc*e.ga+e.hac*e.gc)/det;
      dc=(e.hac*e.ga-e.haa*e.gc)/det;
    }
    bool accepted=false;
    double step=1;
    for(int ls=0;ls<maxit;++ls) {
      const double nextA=A+step*da,nextc=c+step*dc;
      if(nextA==A&&nextc==c)break;
      if(nextA>0) {
        const auto next=gb2_detail::evaluate(d,nextA,nextc,p,q,hb,offset);
        if(std::isfinite(next.ll)&&next.ll>=e.ll-32*eps*(1+std::abs(e.ll))) {
          A=nextA;c=nextc;accepted=true;break;
        }
      }
      step*=.5;
    }
    if(!accepted)return out;
  }
  out.status=Gb2Status::NumericalFailure;
  out.stop_detail.loglik_upper=INFINITY;
  return out;
}
} // namespace gb2_concave_detail
} // namespace cppunivariate
