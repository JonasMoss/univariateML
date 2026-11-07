#pragma once
// Included inside cppunivariate after SnormResult. See README for the proof.
namespace snorm_certificate {
// Bernstein arithmetic avoids converting high-degree elimination polynomials
// to the ill-conditioned monomial basis on [0,1].
struct Sum {
  CPPUNIV_EXTENDED_REAL value=0,correction=0;
  void add(CPPUNIV_EXTENDED_REAL v){const auto y=v-correction,t=value+y;correction=(t-value)-y;value=t;}
};
struct Poly { std::vector<CPPUNIV_EXTENDED_REAL> b; };
inline Poly constant(CPPUNIV_EXTENDED_REAL c) { return {{c}}; }
inline Poly elevate(Poly p,std::size_t degree) {
  while(p.b.size()<=degree) {
    const std::size_t n=p.b.size(); std::vector<CPPUNIV_EXTENDED_REAL> q(n+1);
    q[0]=p.b[0];q[n]=p.b[n-1];
    for(std::size_t i=1;i<n;++i)q[i]=(i*p.b[i-1]+(n-i)*p.b[i])/n;
    p.b=q;
  }
  return p;
}
inline Poly add(Poly a,Poly b,CPPUNIV_EXTENDED_REAL factor=1) {
  const auto degree=std::max(a.b.size(),b.b.size())-1;
  a=elevate(a,degree);b=elevate(b,degree);
  for(std::size_t i=0;i<a.b.size();++i)a.b[i]+=factor*b.b[i];return a;
}
inline Poly times(Poly a,CPPUNIV_EXTENDED_REAL c) {for(auto& v:a.b)v*=c;return a;}
inline CPPUNIV_EXTENDED_REAL choose(std::size_t n,std::size_t k) {
  CPPUNIV_EXTENDED_REAL c=1;for(std::size_t i=1;i<=k;++i)c=c*(n+1-i)/i;return c;
}
inline Poly mul(const Poly& a,const Poly& b) {
  const auto n=a.b.size()-1,m=b.b.size()-1;Poly p{std::vector<CPPUNIV_EXTENDED_REAL>(n+m+1,0)};
  for(std::size_t i=0;i<=n;++i)for(std::size_t j=0;j<=m;++j)
    p.b[i+j]+=a.b[i]*b.b[j]*choose(n,i)*choose(m,j)/choose(n+m,i+j);
  return p;
}
inline Poly derivative(const Poly& a) {
  const auto n=a.b.size()-1;if(n==0)return constant(0);
  Poly p{std::vector<CPPUNIV_EXTENDED_REAL>(n)};
  for(std::size_t i=0;i<n;++i)p.b[i]=n*(a.b[i+1]-a.b[i]);return p;
}
inline CPPUNIV_EXTENDED_REAL value(Poly p,CPPUNIV_EXTENDED_REAL t) {
  for(std::size_t n=p.b.size()-1;n>0;--n)for(std::size_t i=0;i<n;++i)p.b[i]=(1-t)*p.b[i]+t*p.b[i+1];return p.b[0];
}
// Remove only analytically known endpoint factors, never fitted small roots.
// Bernstein division by u or (1-u) has no subtractive recurrence.
inline bool deflate(Poly& p,int count,bool right) {
  for(int pass=0;pass<count;++pass) {
    if(p.b.size()<2)return false;
    CPPUNIV_EXTENDED_REAL norm=0;for(auto v:p.b)norm=std::max(norm,std::abs(v));
    const auto residual=right?p.b.back():p.b.front();
    if(std::abs(residual)>8192*std::numeric_limits<double>::epsilon()*p.b.size()*norm)return false;
    const auto n=p.b.size()-1;Poly q{std::vector<CPPUNIV_EXTENDED_REAL>(n)};
    for(std::size_t i=0;i<n;++i)q.b[i]=right?n*p.b[i]/(n-i):n*p.b[i+1]/(i+1);
    p=q;
  }
  return true;
}
struct Interval {CPPUNIV_EXTENDED_REAL lo,hi;};
inline Interval sum(Interval a,Interval b){return {a.lo+b.lo,a.hi+b.hi};}
inline Interval product(Interval a,Interval b){const auto p=a.lo*b.lo,q=a.lo*b.hi,r=a.hi*b.lo,s=a.hi*b.hi;return {std::min(std::min(p,q),std::min(r,s)),std::max(std::max(p,q),std::max(r,s))};}
inline Interval scaled(Interval a,CPPUNIV_EXTENDED_REAL c){return c>=0?Interval{c*a.lo,c*a.hi}:Interval{c*a.hi,c*a.lo};}
inline Interval quotient(Interval a,Interval b){if(b.lo<=0 && b.hi>=0)return {-INFINITY,INFINITY};return product(a,{1/b.hi,1/b.lo});}
inline Interval square(Interval a){return {a.lo<=0 && a.hi>=0?0:std::min(a.lo*a.lo,a.hi*a.hi),std::max(a.lo*a.lo,a.hi*a.hi)};}
inline Interval radical(Interval a){return {std::sqrt(a.lo),std::sqrt(a.hi)};}
inline Poly portion(Poly p,CPPUNIV_EXTENDED_REAL t,bool left) {
  Poly q=p;const auto n=p.b.size()-1;
  for(std::size_t j=n;j>0;--j){for(std::size_t i=0;i<j;++i)p.b[i]=(1-t)*p.b[i]+t*p.b[i+1];if(left)q.b[n-j+1]=p.b[0];else q.b[j-1]=p.b[j-1];}
  return q;
}
inline Interval range(Poly p,double lo,double hi) {
  if(hi<1)p=portion(p,hi,true);
  if(lo>0)p=portion(p,lo/hi,false);
  return {*std::min_element(p.b.begin(),p.b.end()),*std::max_element(p.b.begin(),p.b.end())};
}
inline double upper_double(CPPUNIV_EXTENDED_REAL upper) {
  if(!std::isfinite(upper))return INFINITY;
  if(upper<-std::numeric_limits<double>::max())return -std::numeric_limits<double>::max();
  return static_cast<double>(upper);
}
struct Segment {Poly p;double lo,hi;int depth;};
inline bool isolate(const Poly& p,int maxit,std::int64_t& work,std::vector<double>& points,bool& budget,
    std::vector<Interval>& uncertain,bool& covered) {
  covered=false;bool resolved=true;
  std::vector<Segment> queue{{p,0,1,0}};
  CPPUNIV_EXTENDED_REAL norm=0;for(auto v:p.b)norm=std::max(norm,std::abs(v));
  const auto sign_margin=8*std::numeric_limits<CPPUNIV_EXTENDED_REAL>::epsilon()*p.b.size()*norm;
  const auto uncertainty_width=std::sqrt(8*std::numeric_limits<CPPUNIV_EXTENDED_REAL>::epsilon()*p.b.size());
  while(!queue.empty()) {
    if(++work>enumeration_budget(maxit)){budget=true;return false;}
    auto s=queue.back();queue.pop_back();
    bool positive=true,negative=true;
    for(auto v:s.p.b){positive=positive && v>=0;negative=negative && v<=0;}
    // A root exactly on a subdivision boundary belongs to both adjacent
    // Bernstein hulls and must be retained before sign exclusion.
    if(s.p.b.front()==0 && s.lo>0 && s.lo<1)points.push_back(s.lo);
    if(s.p.b.back()==0 && s.hi>0 && s.hi<1)points.push_back(s.hi);
    if(positive && negative)return false; // An arithmetic zero segment is unresolved.
    if(positive || negative) {
      // A small sign-definite hull can hide an even root after rounding.
      // Monotone pieces are excluded normally; an unresolved turning point
      // stops rather than silently declaring the polynomial root-free.
      bool rising=false,falling=false;CPPUNIV_EXTENDED_REAL smallest=INFINITY;
      for(auto v:s.p.b)smallest=std::min(smallest,std::abs(v));
      for(std::size_t i=1;i<s.p.b.size();++i){rising=rising || s.p.b[i]>s.p.b[i-1];falling=falling || s.p.b[i]<s.p.b[i-1];}
      if(!(rising && falling && smallest<=sign_margin))continue;
      if(s.hi-s.lo<=uncertainty_width){uncertain.push_back({s.lo,s.hi});resolved=false;continue;}
      // A wider hull may merely have a small value at an endpoint and a
      // well-separated turning point. Subdivision distinguishes those.

    }
    if(s.hi-s.lo<=2e-14){points.push_back((s.lo+s.hi)/2);continue;}
    if(s.depth>=solver_budget(maxit))return false;
    auto q=s.p;Poly left=q,right=q;
    const auto n=q.b.size()-1;
    for(std::size_t j=n;j>0;--j){for(std::size_t i=0;i<j;++i)q.b[i]=(q.b[i]+q.b[i+1])/2;left.b[n-j+1]=q.b[0];right.b[j-1]=q.b[j-1];}
    const double mid=(s.lo+s.hi)/2;
    queue.push_back({right,mid,s.hi,s.depth+1});queue.push_back({left,s.lo,mid,s.depth+1});
  }
  covered=true;return resolved;
}
inline bool isolate(const Poly& p,int maxit,std::int64_t& work,std::vector<double>& points,bool& budget) {
  std::vector<Interval> uncertain;bool covered=false;
  return isolate(p,maxit,work,points,budget,uncertain,covered);
}
inline SnormResult fit(span x,int maxit,double mean,double sd,double xi) {
  const bool hm=!std::isnan(mean),hs=!std::isnan(sd),hx=!std::isnan(xi);
  SnormResult out{mean,sd,xi,NAN,0,SnormStatus::InvalidStart};
  if(maxit<=0){out.status=SnormStatus::BfgsFailure;out.stop_detail.budget_exhausted=true;return out;}
  if(x.empty() || (hm && !std::isfinite(mean)) || (hs && !(sd>0 && std::isfinite(sd))) || (hx && !(xi>0 && std::isfinite(xi))))return out;
  std::vector<double> raw(x.begin(),x.end());for(double v:raw)if(!std::isfinite(v))return out;
  std::sort(raw.begin(),raw.end());
  if(!hs && raw.front()==raw.back() && (!hm || mean==raw.front())){out.status=SnormStatus::Unbounded;return out;}
  out.status=SnormStatus::BfgsFailure;
  const double origin=hm?mean:raw.front()/2+raw.back()/2;
  double unit=hs?sd:std::max(std::abs(raw.front()-origin),std::abs(raw.back()-origin));
  if(!(unit>0 && std::isfinite(unit)))return out;
  std::vector<CPPUNIV_EXTENDED_REAL> y;
  for(double v:raw){const auto z=(static_cast<CPPUNIV_EXTENDED_REAL>(v)-origin)/unit;if(!std::isfinite(z))return out;y.push_back(z);}
  for(std::size_t i=1;i<y.size();++i)if(y[i]==y[i-1] && raw[i]!=raw[i-1])return out;
  const CPPUNIV_EXTENDED_REAL n=y.size(),m=std::sqrt(CPPUNIV_EXTENDED_LITERAL(2.,2.L)/cppunivariate::pi),h2=1-m*m,d=3-4*m*m;
  const double eps=std::numeric_limits<double>::epsilon();
  SnormResult best=out;best.logLik=-INFINITY;
  std::vector<FitCandidate> candidates,finite;
  bool arithmetic_failure=false;
  FitCandidate endpoint_witness,best_candidate;
  std::vector<double> uncertain_face_upper;
  auto consider=[&](CPPUNIV_EXTENDED_REAL c,CPPUNIV_EXTENDED_REAL sigma,CPPUNIV_EXTENDED_REAL a,CPPUNIV_EXTENDED_REAL b,double lower=NAN,double upper=NAN,bool diagnostic=false) {
    if(!(a>0 && b>0 && sigma>0) || !std::isfinite(c+sigma+a+b)){arithmetic_failure=true;return;}
    const double mu=hm?mean:static_cast<double>(origin+unit*(c+m*(b-a)));
    const double sig=hs?sd:static_cast<double>(unit*sigma),skew=hx?xi:static_cast<double>(std::sqrt(b/a));
    if(!std::isfinite(mu) || !(sig>0 && std::isfinite(sig)) || !(skew>0 && std::isfinite(skew))){arithmetic_failure=true;return;}
    CPPUNIV_EXTENDED_REAL penalty=0;
    for(auto v:y){const auto z=(v-c)/(v<c?a:b);penalty+=z*z/2;}
    const double ll=static_cast<double>(n*(std::log(m)-std::log(a+b)-std::log(unit))-penalty);
    // Certify that conversion back to public moment coordinates preserves
    // the evaluated objective; cancellation or overflow is terminal.
    double actual=0,shift=0,error=0;
    const double su=dens::snorm_sigma(skew,shift);
    if(!(su>0) || !std::isfinite(su+shift)){arithmetic_failure=true;return;}
    for(double v:raw) {
      const double linear=(v-mu)/sig*su,z=linear+shift;
      const double dz=16*eps*(1+std::abs(linear)+std::abs(shift));
      const double side=std::abs(z)<=dz?std::min(skew,1/skew):(z>0?skew:1/skew);
      const double standardized=z/side,dr=dz/side+16*eps*std::abs(standardized);
      error+=std::abs(standardized)*dr+dr*dr/2;
      actual+=dens::log_dsnorm_core(z,skew)-std::log(sig)+std::log(su);
    }
    error+=16*eps*static_cast<double>(n)*(1+std::abs(std::log(sig))+std::abs(std::log(su))+std::abs(std::log(skew)));
    if(!std::isfinite(ll) || !std::isfinite(actual) || !std::isfinite(error) ||
       error>1e-10*(static_cast<double>(n)+std::abs(ll)) ||
       std::abs(ll-actual)>1e-10*(1+std::abs(ll))){arithmetic_failure=true;return;}
    const double pad=1e-9*(static_cast<double>(n)+std::abs(ll));
    if(diagnostic) {
      if(endpoint_witness.coef.empty() || ll>endpoint_witness.loglik_upper)endpoint_witness={{mu,sig,skew},std::min(ll,actual)-pad,std::max(ll,actual)+pad};
      return;
    }
    lower=std::isnan(lower)?std::min(ll,actual):std::min(lower,std::min(ll,actual));
    upper=std::isnan(upper)?std::max(ll,actual):std::max(upper,std::max(ll,actual));
    if(upper>=best.logLik-pad)finite.push_back({{mu,sig,skew},lower-pad,upper+pad});
    if(ll>best.logLik){best={mu,sig,skew,ll,0,SnormStatus::Ok};best_candidate={{mu,sig,skew},lower-pad,upper+pad};}
  };
  auto stopped=[&](bool budget) {
    auto r=best;r.status=SnormStatus::BfgsFailure;r.stop_detail.budget_exhausted=budget;
    if(!best_candidate.coef.empty())r.stop_detail.candidates.push_back(best_candidate);
    if(!endpoint_witness.coef.empty())r.stop_detail.candidates.push_back(endpoint_witness);
    for(const auto& face:candidates)r.stop_detail.candidates.push_back(face);
    r.stop_detail.loglik_lower=best_candidate.coef.empty()?-INFINITY:best_candidate.loglik_lower;r.stop_detail.loglik_upper=INFINITY;
    for(const auto& candidate:r.stop_detail.candidates)r.stop_detail.loglik_lower=std::max(r.stop_detail.loglik_lower,candidate.loglik_lower);
    if(!budget)for(const auto& face:candidates) {
      const double margin=1e-8*static_cast<double>(n);
      for(const auto& witness:{best_candidate,endpoint_witness})if(!witness.coef.empty() &&
          face.loglik_lower>=witness.loglik_lower-margin && face.loglik_lower<=witness.loglik_upper+margin) {
        r.stop_detail=comparison_detail(witness,face);return r;
      }
    }
    return r;
  };
  // For held xi all scores reduce to weighted linear/quadratic equations.
  if(hx) {
    const CPPUNIV_EXTENDED_REAL q=xi-1/static_cast<CPPUNIV_EXTENDED_REAL>(xi),su=std::sqrt(1+h2*q*q);
    const CPPUNIV_EXTENDED_REAL A=1/(su*xi),B=xi/su,W=q/su;
    if(!(A>0 && B>0) || !std::isfinite(A+B+W))return out;
    CPPUNIV_EXTENDED_REAL sl=0,ssl=0,sr=0,ssr=0;
    std::vector<CPPUNIV_EXTENDED_REAL> right(y.size()+1,0),right2(y.size()+1,0),left_shift(y.size()+1,0),right_shift(y.size()+1,0);
    Sum ls,rs;
    for(std::size_t k=0;k<y.size();++k){ls.add(y[k]-y.front());left_shift[k+1]=ls.value;}
    for(std::size_t k=y.size();k-->0;){right[k]=right[k+1]+y[k];right2[k]=right2[k+1]+y[k]*y[k];rs.add(y.back()-y[k]);right_shift[k]=rs.value;}
    for(std::size_t k=0;k<=y.size();++k) {
      if(static_cast<std::int64_t>(k)>=enumeration_budget(maxit))return stopped(true);
      if(k>0){const auto v=y[k-1];sl+=v;ssl+=v*v;}sr=right[k];ssr=right2[k];
      if(k>0 && k<y.size() && y[k]==y[k-1])continue;
      CPPUNIV_EXTENDED_REAL sigma=hs?1:0,c=0;
      if(hm) {
        if(!hs) {
          const auto T=(sl!=0?sl/(A*A):0)+(sr!=0?sr/(B*B):0);
          const auto V=(ssl>0?ssl/(A*A):0)+(ssr>0?ssr/(B*B):0);
          if(!(V>0) || !std::isfinite(T+V))return stopped(false);
          const auto E=m*W*T,disc=std::sqrt(E*E+4*n*V);
          const auto u=E>0?2*n/(disc+E):(disc-E)/(2*V);sigma=1/u;
        }
        c=-m*sigma*W;
      } else {
        const auto lm=k>0?left_shift[k]/k:0,rm=k<y.size()?right_shift[k]/(n-k):0;
        const auto gap=std::max(CPPUNIV_EXTENDED_LITERAL(0.,0.L),y.back()-y.front()-lm-rm);
        const auto wl=k>0?k/(A*A):0,wr=k<y.size()?(n-k)/(B*B):0;
        if(k==0)c=y.back()-rm;
        else if(k==y.size())c=y.front()+lm;
        else if(wl>=wr)c=y.front()+(lm+wr/(wl+wr)*gap);
        else c=y.back()-(rm+wl/(wl+wr)*gap);
      }
      if((k==0 || c>=y[k-1]) && (k==y.size() || c<=y[k])) {
        CPPUNIV_EXTENDED_REAL target=NAN;
        if(!hm) {
          const auto lm=k>0?left_shift[k]/k:0,rm=k<y.size()?right_shift[k]/(n-k):0;
          Sum vl,vr;
          for(std::size_t i=0;i<k;++i){const auto z=(y[i]-y.front())-lm;vl.add(z*z);}
          for(std::size_t i=k;i<y.size();++i){const auto z=(y.back()-y[i])-rm;vr.add(z*z);}
          CPPUNIV_EXTENDED_REAL qmin=0;
          if(vl.value>0)qmin+=vl.value/(A*A);
          if(vr.value>0)qmin+=vr.value/(B*B);
          if(k>0 && k<y.size()) {
            const auto gap=std::max(CPPUNIV_EXTENDED_LITERAL(0.,0.L),y.back()-y.front()-lm-rm);
            qmin+=k*(n-k)*gap*gap/(k*B*B+(n-k)*A*A);
          }
          if(!hs)sigma=std::sqrt(qmin/n);
          if(!(sigma>0) || !std::isfinite(sigma+qmin))return stopped(false);
          target=n*(std::log(m)-std::log(sigma)-std::log(A+B)-std::log(unit))-qmin/(2*sigma*sigma);
        }
        consider(c,sigma,sigma*A,sigma*B);
        if(arithmetic_failure)return stopped(false);
        if(!hm && (!std::isfinite(best.logLik) || std::abs(best.logLik-target)>1e-10*(n+std::abs(target))))return stopped(false);
      }
    }
    if(arithmetic_failure || best.status!=SnormStatus::Ok)return stopped(false);
    return best;
  }
  // Rational parametrization of the held-variance ellipse. t in [-T,T]
  // covers all positive side scales once, including the symmetric member.
  const auto T=1/(1+std::sqrt(1+d));
  const Poly t{{-T,T}},one=constant(1),t2=mul(t,t),D=add(one,times(t2,d));
  Poly A=add(add(one,times(t,-2)),times(t2,-d)),B=add(add(one,times(t,2)),times(t2,-d));
  A.b.back()=0;B.b.front()=0; // Exact endpoint identities, not tolerance clipping.
  const Poly S=add(A,B),W=times(t,4),AB=mul(A,B),V=mul(AB,AB),DS=mul(D,S);
  const Poly lognum=add(mul(derivative(S),D),mul(S,derivative(D)),-1);
  const Poly A2=mul(A,A),B2=mul(B,B),AB3=mul(V,AB);
  // Finite endpoint-approach witnesses expose objective rounding at very
  // large held-moment magnitudes. They are diagnostics, never tail guards.
  if(hm && hs && n*(1+y.front()*y.front()+y.back()*y.back())>1e6)for(double u:{1e-16,1.-1e-16}) {
    const auto a=value(A,u)/value(D,u),b=value(B,u)/value(D,u),w=value(W,u)/value(D,u);
    const auto c=-m*w;
    if((u<.5 && c>=y.back()) || (u>.5 && c<=y.front()))consider(c,1,a,b,NAN,NAN,true);
  }
  if(arithmetic_failure)return stopped(false);
  // Exact constrained half-normal profiles. When both moments are held,
  // endpoint observations retain the shrinking-side Gaussian residual.
  for(int side:{-1,1}) {
    CPPUNIV_EXTENDED_REAL b=hs?1/std::sqrt(h2):0,c=hm?0:(side==1?y.front():y.back()),sum=0,ss=0;
    for(auto v:y){sum+=v-c;ss+=(v-c)*(v-c);}
    bool at_extreme=!hm;
    if(!hs) {
      const auto linear=hm?side*m*sum:0;
      b=(linear+std::sqrt(linear*linear+4*n*ss))/(2*n);
      if(hm) {
        const auto bound=side==1?-y.front()/m:y.back()/m;
        if(bound>=b){b=bound;at_extreme=true;}
      }
    }
    if(!(b>0 && std::isfinite(b))){uncertain_face_upper.push_back(INFINITY);continue;}
    if(hm)c=at_extreme?(side==1?y.front():y.back()):-side*m*b;
    const auto gap=side==1?y.front()-c:c-y.back();
    const auto pad=128*eps*(1+std::abs(c)+std::abs(y.front())+std::abs(y.back()));
    if(gap < -pad)continue;
    CPPUNIV_EXTENDED_REAL ll=n*(std::log(m)-std::log(b)-std::log(unit));
    for(auto v:y){ll-=(v-c)*(v-c)/(2*b*b);if(hm && hs && std::abs(v-c)<=pad)ll-=m*m/(8*h2*h2);}
    const auto model=side==1?StopModel::Hnorm:StopModel::ReflectedHnorm;
    const double value_ll=static_cast<double>(ll);
    const double endpoint=at_extreme?(side==1?raw.front():raw.back()):static_cast<double>(origin+unit*c);
    const double halfscale=static_cast<double>(unit*b);
    if(hm && hs && std::abs(gap)<=pad) {
      // The contact factor is discontinuous in the held moments: a strictly
      // interior endpoint has no shrinking-side penalty, while an infeasible
      // face has likelihood -infinity. Do not force a contact decision.
      const auto blo=b*(1-128*eps),bhi=b*(1+128*eps);
      CPPUNIV_EXTENDED_REAL upper=n*(std::log(m)-std::log(blo)-std::log(unit));
      for(auto v:y) {
        const auto distance=std::max(CPPUNIV_EXTENDED_LITERAL(0.,0.L),std::abs(v-c)-pad);
        upper-=distance*distance/(2*bhi*bhi);
      }
      uncertain_face_upper.push_back(upper_double(upper+1e-9*(n+std::abs(upper))));
      continue;
    }
    bool recovered=std::isfinite(endpoint) && halfscale>0 && std::isfinite(halfscale) && std::isfinite(value_ll);
    if(!at_extreme && 128*eps*(std::abs(origin)+std::abs(unit*c))>1e-7*(1+std::abs(endpoint)))recovered=false;
    double evaluated=0;
    if(recovered) {
      evaluated=static_cast<double>(n)*(std::log(static_cast<double>(m))-std::log(halfscale));
      for(double v:raw) {
        const double distance=side*(v-endpoint)/halfscale;
        if(!(distance>=0 && std::isfinite(distance))){recovered=false;break;}
        evaluated-=distance*distance/2;
      }
      if(!std::isfinite(evaluated) || std::abs(evaluated-value_ll)>1e-10*(static_cast<double>(n)+std::abs(value_ll)))recovered=false;
    }
    if(!recovered) {
      const auto upper=ll+1e-9*(n+std::abs(ll));
      const double bound=upper_double(upper);
      uncertain_face_upper.push_back(bound);continue;
    }
    candidates.push_back({{endpoint,halfscale},value_ll,value_ll,model});
  }
  // At a constant sample, free location places the mode on that value.
  // a+b >= sd/h gives the sharp density bound m*h/sd on both faces.
  if(hs && !hm && raw.front()==raw.back() && candidates.size()==2) {
    auto r=stopped(false);r.stop_detail=comparison_detail(candidates[0],candidates[1]);
    r.stop_detail.loglik_upper=std::max(candidates[0].loglik_upper,candidates[1].loglik_upper);return r;
  }
  // With a held mean away from a constant sample, f(x)<=m^2/|x-mean|.
  // Equality is approached only by the half-normal ending at that sample.
  if(hm && !hs && raw.front()==raw.back()) {
    const auto model=raw.front()<mean?StopModel::Hnorm:StopModel::ReflectedHnorm;
    const auto target=n*(2*std::log(m)-std::log(std::abs(y.front()))-std::log(unit));
    for(const auto& face:candidates)if(face.model==model && std::abs(face.loglik_lower-target)<=1e-10*(n+std::abs(target))) {
      auto r=out;r.status=model==StopModel::Hnorm?SnormStatus::RightHalfNormalBoundary:SnormStatus::LeftHalfNormalBoundary;
      r.logLik=face.loglik_lower;r.stop_detail=limit_detail(face.coef,face.loglik_lower,model);
      r.stop_detail.loglik_lower=face.loglik_lower;r.stop_detail.loglik_upper=upper_double(target+1e-9*(n+std::abs(target)));return r;
    }
    return stopped(false);
  }
  CPPUNIV_EXTENDED_REAL sl=0,ssl=0,sr=0,ssr=0;for(auto v:y){sr+=v;ssr+=v*v;}
  // Right sums are formed directly to protect endpoint cells.
  std::vector<CPPUNIV_EXTENDED_REAL> rs(y.size()+1,0),rss(y.size()+1,0);
  for(std::size_t k=y.size();k-->0;){rs[k]=rs[k+1]+y[k];rss[k]=rss[k+1]+y[k]*y[k];}
  std::int64_t work=0,cells=0;
  std::vector<double> unresolved_upper;
  for(std::size_t k=0;k<=y.size();++k) {
    if(k>0){sl+=y[k-1];ssl+=y[k-1]*y[k-1];}sr=rs[k];ssr=rss[k];
    if(k>0 && k<y.size() && y[k]==y[k-1])continue;
    if(cells++>=certificate_budget(maxit))return stopped(true);
    const Poly H=add(times(B2,k),times(A2,n-k)),J=add(times(B2,sl),times(A2,sr)),K=add(times(B2,ssl),times(A2,ssr));
    Poly equation;Poly CN,EN,FN;
    if(hs && !hm) {
      const Poly N=mul(mul(D,D),add(mul(K,H),mul(J,J),-1)),den=mul(V,H);
      equation=add(times(mul(lognum,mul(den,den)),2*n),mul(DS,add(mul(derivative(N),den),mul(N,derivative(den)),-1)));
    } else {
      CN=mul(mul(D,D),K);EN=times(mul(mul(W,D),J),m);FN=times(mul(mul(W,W),H),m*m);
      auto numerator_derivative=[&](const Poly& p){return add(mul(derivative(p),AB),times(mul(p,derivative(AB)),2),-1);};
      const Poly cN=numerator_derivative(CN),eN=numerator_derivative(EN),fN=numerator_derivative(FN);
      const Poly H0=add(times(mul(lognum,AB3),2*n),mul(DS,fN));
      if(hs)equation=add(H0,mul(DS,add(cN,times(eN,2))));
      else {
        const Poly H1=times(mul(DS,eN),2),H2=mul(DS,cN);
        const Poly L=add(mul(CN,H1),mul(EN,H2),-1),Q=add(mul(CN,H0),times(mul(V,H2),n));
        equation=add(add(mul(CN,mul(Q,Q)),mul(EN,mul(Q,L)),-1),times(mul(V,mul(L,L)),n),-1);
      }
    }
    // Clearing side-scale denominators creates endpoint factors. Their
    // orders follow by substituting A=0 or B=0 in the two score equations;
    // a constant side has k*sum(y^2)=sum(y)^2, and a zero side also has sum(y)=0.
    const bool lc=k>0 && y.front()==y[k-1],rc=k<y.size() && y[k]==y.back();
    int left=0,right=0;
    if(hs && !hm){left=k==0?8:(lc?4:1);right=k==y.size()?8:(rc?4:1);}
    else if(hs){left=k==0?3:0;right=k==y.size()?3:0;}
    else {left=k==0?12:(lc?(y.front()==0?6:3):0);right=k==y.size()?12:(rc?(y.back()==0?6:3):0);}
    // A vanishes at u=1, B at u=0 in the ellipse parametrization.
    if(!deflate(equation,left,true) || !deflate(equation,right,false))return stopped(false);
    CPPUNIV_EXTENDED_REAL largest=0;for(auto v:equation.b){if(!std::isfinite(v))return stopped(false);largest=std::max(largest,std::abs(v));}
    if(!(largest>0 && std::isfinite(largest)))return stopped(false);
    equation=times(equation,1/largest);
    std::vector<double> points;bool budget=false,covered=false;std::vector<Interval> uncertain;
    if(!isolate(equation,maxit,work,points,budget,uncertain,covered) && !covered)return stopped(budget);
    // A relaxed location/scale fit bounds every conditional likelihood on
    // this shape interval. Nonnegative within/between-group sums avoid the
    // removable endpoint poles of the constrained profile.
    for(const auto& box:uncertain) {
      const double lo=static_cast<double>(box.lo),hi=static_cast<double>(box.hi);
      const auto ar=range(A,lo,hi),br=range(B,lo,hi),srng=range(S,lo,hi);
      const auto allowance=8*eps*(1+n);
      CPPUNIV_EXTENDED_REAL qlower=0;
      if(k>0) {
        const auto centred=sl*sl/k;
        qlower+=std::max(CPPUNIV_EXTENDED_LITERAL(0.,0.L),ssl-centred-allowance*(ssl+centred))/(ar.hi*ar.hi);
      }
      if(k<y.size()) {
        const auto centred=sr*sr/(n-k);
        qlower+=std::max(CPPUNIV_EXTENDED_LITERAL(0.,0.L),ssr-centred-allowance*(ssr+centred))/(br.hi*br.hi);
      }
      if(k>0 && k<y.size()) {
        const auto magnitude=std::max(std::abs(y.front()),std::abs(y[k-1]))+std::max(std::abs(y[k]),std::abs(y.back()));
        const auto gap=std::max(CPPUNIV_EXTENDED_LITERAL(0.,0.L),sr/(n-k)-sl/k-allowance*magnitude);
        qlower+=k*(n-k)*gap*gap/(k*br.hi*br.hi+(n-k)*ar.hi*ar.hi);
      }
      double upper=INFINITY;
      if(qlower>0 && std::isfinite(qlower) && srng.lo>0) {
        const auto ll=n*(std::log(m)-std::log(srng.lo)+(std::log(n)-std::log(qlower))/2-CPPUNIV_EXTENDED_LITERAL(.5,.5L))-n*std::log(unit);
        if(std::isfinite(ll))upper=upper_double(ll+allowance*(n+std::abs(ll)));
      }
      unresolved_upper.push_back(upper);
    }
    // Knots need not be added: the squared-residual likelihood is C1 at
    // every moving join, so a finite optimum there also has zero score.
    for(double u:points) {
      const auto a=value(A,u)/value(D,u),b=value(B,u)/value(D,u),w=value(W,u)/value(D,u);
      CPPUNIV_EXTENDED_REAL sigma=1,c=0;
      if(hm) {
        if(!hs) {
          const auto cv=value(CN,u)/value(V,u),ev=value(EN,u)/value(V,u);
          if(!(cv>0))continue;
          const auto disc=std::sqrt(ev*ev+4*n*cv),inv=ev>0?2*n/(disc+ev):(disc-ev)/(2*cv);sigma=1/inv;
        }
        c=-m*sigma*w;
      } else c=value(J,u)/value(H,u);
      {
        // A narrow algebraic parameter interval must also enclose narrow
        // public coefficients, especially near either half-normal endpoint.
        const double ulo=std::max(0.,u-2e-14),uhi=std::min(1.,u+2e-14);
        const auto di=range(D,ulo,uhi),ai=quotient(range(A,ulo,uhi),di),bi=quotient(range(B,ulo,uhi),di),wi=quotient(range(W,ulo,uhi),di);
        Interval si{1,1},ci{0,0};
        if(hm) {
          if(!hs) {
            const auto vi=range(V,ulo,uhi),cc=quotient(range(CN,ulo,uhi),vi),ee=quotient(range(EN,ulo,uhi),vi);
            const auto disc=radical(sum(square(ee),scaled(cc,4*n)));
            si=ee.lo>0?scaled(sum(disc,ee),1/(2*n)):quotient(scaled(cc,2),sum(disc,scaled(ee,-1)));
          }
          ci=scaled(product(si,wi),-m);
        } else ci=quotient(range(J,ulo,uhi),range(H,ulo,uhi));
        if(!std::isfinite(ci.lo+ci.hi))return stopped(false);
        if((k>0 && ci.hi<y[k-1]) || (k<y.size() && ci.lo>y[k]))continue;
        if((k>0 && c<y[k-1]-1e-9) || (k<y.size() && c>y[k]+1e-9))return stopped(false);
        const auto mi=hm?Interval{mean,mean}:sum({origin,origin},scaled(sum(ci,scaled(product(si,wi),m)),unit));
        const auto sdi=hs?Interval{sd,sd}:scaled(si,unit),xi=radical(quotient(bi,ai));
        for(const auto& interval:{mi,sdi,xi})if(!std::isfinite(interval.lo+interval.hi) || interval.hi-interval.lo>1e-7*(1+std::min(std::abs(interval.lo),std::abs(interval.hi))))return stopped(false);
        const auto asi=product(si,ai),bsi=product(si,bi),total=sum(asi,bsi);
        if(!(asi.lo>0 && bsi.lo>0 && total.lo>0))return stopped(false);
        Interval loss{0,0};
        for(std::size_t i=0;i<y.size();++i) {
          const auto residual=sum({y[i],y[i]},scaled(ci,-1));
          auto side=i<k?asi:bsi;
          if(residual.lo<0 && residual.hi>0)side={std::min(asi.lo,bsi.lo),std::max(asi.hi,bsi.hi)};
          loss=sum(loss,scaled(square(quotient(residual,side)),CPPUNIV_EXTENDED_LITERAL(.5,.5L)));
        }
        const Interval likelihood={n*(std::log(m)-std::log(total.hi)-std::log(unit))-loss.hi,
                                   n*(std::log(m)-std::log(total.lo)-std::log(unit))-loss.lo};
        if(!std::isfinite(likelihood.lo+likelihood.hi) || likelihood.hi-likelihood.lo>1e-8*(n+std::abs(likelihood.lo)))return stopped(false);
        consider(c,sigma,sigma*a,sigma*b,static_cast<double>(likelihood.lo),static_cast<double>(likelihood.hi));
      }
    }
  }
  if(arithmetic_failure)return stopped(false);
  if(!uncertain_face_upper.empty()) {
    double lower=best_candidate.coef.empty()?-INFINITY:best_candidate.loglik_lower;
    for(const auto& face:candidates)lower=std::max(lower,face.loglik_lower);
    if(!endpoint_witness.coef.empty())lower=std::max(lower,endpoint_witness.loglik_lower);
    for(double upper:uncertain_face_upper)if(!std::isfinite(lower) || upper>=lower-1e-9*(static_cast<double>(n)+std::abs(lower)))return stopped(false);
  }
  if(!unresolved_upper.empty()) {
    double lower=best_candidate.coef.empty()?-INFINITY:best_candidate.loglik_lower;
    for(const auto& face:candidates)lower=std::max(lower,face.loglik_lower);
    if(!endpoint_witness.coef.empty())lower=std::max(lower,endpoint_witness.loglik_lower);
    for(double upper:unresolved_upper)if(!std::isfinite(lower) || upper>=lower-1e-9*(static_cast<double>(n)+std::abs(lower))) {
      auto r=stopped(false);r.stop_detail.segment_loglik_upper=upper;return r;
    }
  }
  if(!std::isfinite(best.logLik) && candidates.empty())return stopped(false);
  if(!endpoint_witness.coef.empty()) {
    double upper=-INFINITY;
    for(const auto& candidate:finite)upper=std::max(upper,candidate.loglik_upper);
    for(const auto& candidate:candidates)upper=std::max(upper,candidate.loglik_upper);
    if(endpoint_witness.loglik_lower>upper+1e-8*static_cast<double>(n)) {
      auto r=stopped(false);r.stop_detail.candidates.push_back(endpoint_witness);
      r.stop_detail.loglik_lower=std::max(r.stop_detail.loglik_lower,endpoint_witness.loglik_lower);return r;
    }
  }
  // Distinct finite stationary modes are compared below by their objective;
  // near-face ambiguity must stop rather than select a capped skewness.
  for(const auto& face:candidates) {
    const double margin=1e-8*static_cast<double>(n);
    for(const auto& witness:{best_candidate,endpoint_witness})if(!witness.coef.empty() && face.loglik_lower>=witness.loglik_lower-margin && face.loglik_lower<=witness.loglik_upper+margin) {
      auto r=stopped(false);r.stop_detail=comparison_detail(witness,face);return r;
    }
    FitCandidate reference{{best.mu,best.sigma,best.xi},best.logLik,best.logLik};
    if(!best.stop_detail.candidates.empty())reference=best.stop_detail.candidates.front();
    if(!endpoint_witness.coef.empty() && endpoint_witness.loglik_lower>reference.loglik_lower)reference=endpoint_witness;
    if(face_numerical_tie(face.loglik_lower,reference.loglik_lower,margin)) {
      auto r=stopped(false);r.stop_detail=comparison_detail(reference,face);return r;
    }
    if(face.loglik_lower>=best.logLik-margin) {
      for(const auto& candidate:finite)if(candidate.loglik_upper>face.loglik_lower+margin) {
        auto r=stopped(false);r.stop_detail=comparison_detail(candidate,face);return r;
      }
      best.logLik=face.loglik_lower;best.status=face.model==StopModel::Hnorm?SnormStatus::RightHalfNormalBoundary:SnormStatus::LeftHalfNormalBoundary;
      best.stop_detail=limit_detail(face.coef,face.loglik_lower,face.model);
    }
  }
  // Report distinct equal closure maximizers, including opposite faces.
  const double tiepad=1e-9*(static_cast<double>(n)+std::abs(best.logLik));
  std::vector<FitCandidate> tied;
  for(const auto& candidate:candidates)if(candidate.loglik_upper>=best.logLik-tiepad)tied.push_back(candidate);
  if(best.status==SnormStatus::Ok) {
    for(const auto& candidate:finite)if(candidate.loglik_upper>=best.logLik-tiepad) {
      bool duplicate=false;
      for(const auto& previous:tied)if(previous.model==StopModel::None) {
        double distance=0;for(std::size_t j=0;j<3;++j)distance=std::max(distance,std::abs(candidate.coef[j]-previous.coef[j])/(1+std::abs(previous.coef[j])));
        if(distance<1e-6)duplicate=true;
      }
      if(!duplicate)tied.push_back(candidate);
    }
  }
  if(tied.size()>1){auto r=stopped(false);r.stop_detail.comparison_tie=true;r.stop_detail.candidates=tied;return r;}
  best.iterations=static_cast<int>(std::min(work,static_cast<std::int64_t>(2147483647)));return best;
}
} // namespace snorm_certificate
