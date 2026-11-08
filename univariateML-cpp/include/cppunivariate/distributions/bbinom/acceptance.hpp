// Included inside bbinom_detail after the likelihood helpers.
// Conditional exact-arithmetic certificate; margins are practical, not directed rounding.
struct Enclosure {
  double lo=0, hi=0; // a region containing exactly one stationary maximum
  double p=0, s=0, shape=0;
  double alpha_lo=0, alpha_hi=0, beta_lo=0, beta_hi=0;
};

struct Scores {
  long double p=0, s=0, pp=0, ps=0, ss=0, ep=0, es=0;
};
inline Scores scores(const Data& d,long double p,long double s) {
  Scores z;
  if(!d.work.consume(d.size)){z.p=z.s=NAN;return z;}
  long double JA=0,JC=0,remainder=0,remainder_scale=0;
  for(int j=0;j<d.size;++j) {
    const long double a=p+j*s, b=1-p+j*s, t=1+j*s;
    const long double A=d.A[j]/a, C=d.C[j]/b, T=d.n/t;
    z.p+=A-C; z.s+=j*(A+C-T);
    z.pp+=A/a+C/b; z.ps+=j*(C/b-A/a);
    z.ss+=j*static_cast<long double>(j)*(T/t-A/a-C/b);
    z.ep+=A+C;z.es+=j*(A+C+T);
    JA+=j*static_cast<long double>(d.A[j]);JC+=j*static_cast<long double>(d.C[j]);
    const long double ra=A/p,rc=C/(1-p);
    remainder+=j*static_cast<long double>(j)*(ra+rc-T);
    remainder_scale+=j*static_cast<long double>(j)*(ra+rc+T);
  }
  // Subtract the face score symbolically, rather than cancelling O(hN^2)
  // terms to obtain an O(s) residual near the binomial face.
  const long double h=d.n,N=d.size,S=d.total,p0=S/(h*N),q0=1-p0;
  const long double na=N*h*d.squares,nb=(N-1)*S*S,nc=N*h*S;
  const long double numerator=na-nb-nc;
  const long double face=numerator/(2*h*N*p0*q0);
  const long double shift=(p-p0)*(-JA/(p*p0)+JC/((1-p)*q0));
  const long double stable=face+shift-s*remainder;
  const long double eps=std::numeric_limits<long double>::epsilon();
  const bool exact=na+nb+nc<std::ldexp(1.0L,std::numeric_limits<long double>::digits-2);
  const long double face_error=exact?0:64*eps*(na+nb+nc)/(2*h*N*p0*q0);
  const long double stable_error=face_error+32*eps*(std::abs(face)+
    std::abs(p-p0)*(JA/(p*p0)+JC/((1-p)*q0))+s*remainder_scale)+
    8*eps*std::min(p0,q0)*std::abs(-JA/(p*p0)+JC/((1-p)*q0));
  z.es*=32*eps;
  if(std::isfinite(stable)&&stable_error<z.es){z.s=stable;z.es=stable_error;}
  z.ep*=32*std::numeric_limits<long double>::epsilon();
  return z;
}

// Bracket every profiled mean over [sl,sh]. Each score bound is decreasing
// in p. The limiting endpoint scores have opposite signs (A0,C0 > 0).
inline bool mean_box(const Data& d,long double sl,long double sh,
                     long double& pl,long double& ph) {
  auto root=[&](bool lower,long double& answer) {
    long double l=0,h=1;
    for(int it=0;it<d.maxit;++it) {
      if(!d.work.consume(d.size))return false;
      const long double p=(l+h)/2;
      long double g=0,scale=0;
      for(int j=0;j<d.size;++j) {
        const long double A=d.A[j]/(p+j*(lower?sh:sl));
        const long double C=d.C[j]/(1-p+j*(lower?sl:sh));
        g+=A-C;scale+=A+C;
      }
      const long double e=32*std::numeric_limits<long double>::epsilon()*scale;
      if(g>e)l=p;else if(g< -e)h=p;else {
        // The score slope has magnitude at least A0+C0 on (0,1).
        const long double radius=(std::abs(g)+e)/(d.A[0]+d.C[0]);
        l=std::max(l,p-radius);h=std::min(h,p+radius);
        answer=lower?l:h;return true;
      }
      if(h-l<64*std::numeric_limits<long double>::epsilon()*std::min(p,1-p)) {
        answer=lower?l:h;return true;
      }
    }
    return false;
  };
  return root(true,pl)&&root(false,ph)&&pl>0&&ph<1&&pl<=ph;
}

// Upper Schur-curvature bound and bounds on -l_pp and |l_ps|.
inline bool curvature(const Data& d,long double pl,long double ph,
                      long double sl,long double sh,long double& K,
                      long double& M,long double& cross) {
  if(!d.work.consume(d.size))return false;
  long double ss=0,cl=0,ch=0,scale=0;M=0;
  for(int j=0;j<d.size;++j) {
    const long double al=d.A[j]/((ph+j*sh)*(ph+j*sh));
    const long double ah=d.A[j]/((pl+j*sl)*(pl+j*sl));
    const long double bl=d.C[j]/((1-pl+j*sh)*(1-pl+j*sh));
    const long double bh=d.C[j]/((1-ph+j*sl)*(1-ph+j*sl));
    const long double t=d.n/((1+j*sl)*(1+j*sl));
    M+=al+bl;ss+=j*static_cast<long double>(j)*(t-al-bl);
    cl+=j*(bl-ah);ch+=j*(bh-al);
    scale+=j*static_cast<long double>(j)*(t+ah+bh);
  }
  cross=std::max(std::abs(cl),std::abs(ch));
  const long double error=128*std::numeric_limits<long double>::epsilon()*(scale+cross*cross/M);
  K=-(ss+cross*cross/M)-error;
  return std::isfinite(K)&&K>0&&M>0;
}

inline bool profile_enclosure(const Data& d,double proposed,double tol,Enclosure& box) {
  long double s=proposed;
  // Choose a macroscopic region for strict global exclusion. Shrinking the
  // region is permitted only until its Schur curvature is strictly negative.
  long double sl=0,sh=0,pl=0,ph=0,K=0,M=0,cross=0;
  long double radius=s/8;
  bool curved=false;
  for(int it=0;it<d.maxit;++it,radius/=2) {
    sl=s-radius;sh=s+radius;
    if(!(sl<s&&s<sh))return false;
    if(!mean_box(d,sl,sh,pl,ph))return false;
    if(curvature(d,pl,ph,sl,sh,K,M,cross)){curved=true;break;}
  }
  if(!curved)return false;
  box.lo=static_cast<double>(sl);box.hi=static_cast<double>(sh);
  // In this region L'' <= -K. A score residual therefore encloses its
  // unique root; p'(s)=-l_ps/l_pp transports the root enclosure to p.
  for(int it=0;it<d.maxit;++it) {
    long double l,h;
    if(!mean_box(d,s,s,l,h))return false;
    const long double p=(l+h)/2;
    const Scores z=scores(d,p,s);
    if(!std::isfinite(z.s))return false;
    const long double ds=(std::abs(z.s)+z.es+cross*(h-l)/2)/K;
    const long double dp=(h-l)/2+cross/M*ds;
    if(s-ds>sl&&s+ds<sh&&p-dp>pl&&p+dp<ph) {
      const long double al=(p-dp)/(s+ds),ah=(p+dp)/(s-ds);
      const long double bl=(1-p-dp)/(s+ds),bh=(1-p+dp)/(s-ds);
      const double a=static_cast<double>(p/s),b=static_cast<double>((1-p)/s);
      const long double rounding=8*std::numeric_limits<double>::epsilon();
      if(ah-al+rounding*a<=tol*a && bh-bl+rounding*b<=tol*b) {
        box.p=static_cast<double>(p);box.s=static_cast<double>(s);
        box.alpha_lo=static_cast<double>(al);box.alpha_hi=static_cast<double>(ah);
        box.beta_lo=static_cast<double>(bl);box.beta_hi=static_cast<double>(bh);
        return true;
      }
    }
    const long double H=z.ss+z.ps*z.ps/z.pp;
    const long double next=s-z.s/H;
    if(!(H<0&&next>sl&&next<sh)||next==s)return false;
    s=next;
  }
  return false;
}

inline bool shape_enclosure(const Data& d,double a,double proposed,double tol,Enclosure& box) {
  long double b=proposed,sl=0,sh=0,K=0,radius=b/8;
  bool curved=false;
  for(int it=0;it<d.maxit;++it,radius/=2) {
    if(!d.work.consume(d.size))return false;
    sl=b-radius;sh=b+radius;K=0;long double scale=0;
    if(!(sl<b&&b<sh))return false;
    for(int j=0;j<d.size;++j) {
      const long double A=d.C[j]/((sh+j)*(sh+j));
      const long double B=d.n/((a+sl+j)*(a+sl+j));
      K+=A-B;scale+=A+B;
    }
    K-=128*std::numeric_limits<long double>::epsilon()*scale;
    if(K>0){curved=true;break;}
  }
  if(!curved)return false;
  box.lo=static_cast<double>(sl);box.hi=static_cast<double>(sh);
  for(int it=0;it<d.maxit;++it) {
    if(!d.work.consume(d.size))return false;
    long double g=0,H=0,scale=0;
    for(int j=0;j<d.size;++j) {
      const long double A=d.C[j]/(b+j),B=d.n/(a+b+j);
      g+=A-B;H+=B/(a+b+j)-A/(b+j);scale+=A+B;
    }
    const long double db=(std::abs(g)+32*std::numeric_limits<long double>::epsilon()*scale)/K;
    if(b-db>sl&&b+db<sh&&2*db+8*std::numeric_limits<double>::epsilon()*b<=tol*b) {
      box.shape=static_cast<double>(b);box.beta_lo=static_cast<double>(b-db);box.beta_hi=static_cast<double>(b+db);return true;
    }
    const long double next=b-g/H;
    if(!(H<0&&next>sl&&next<sh)||next==b)return false;
    b=next;
  }
  return false;
}

// Cover the whole compact search domain. Intervals in the proved strictly
// concave region contain only its enclosed root; all other intervals must
// be strictly worse than a feasible incumbent. Never discard a positive gap.
template<typename Bound>
inline bool exclude_outside(const std::vector<double>& grid,const Enclosure& box,
                            double best,Bound bound,int maxit,bool& exhausted) {
  std::vector<double> left,right;
  for(double v:grid) {
    if(v<box.lo)left.push_back(v);
    if(v>box.hi)right.push_back(v);
  }
  if(!left.empty())left.push_back(box.lo);
  if(!right.empty())right.insert(right.begin(),box.hi);
  // One evaluation ledger is shared by both sides.
  std::int64_t evaluations=0;
  auto checked=[&](double a,double m,double b) -> double {
    if(++evaluations>certificate_budget(maxit)){exhausted=true;return NAN;}
    return bound(a,m,b);
  };
  bool cap=false;
  const bool ok=certify(left,best,0,checked,maxit,cap)&&certify(right,best,0,checked,maxit,cap);
  exhausted=exhausted||cap;return ok;
}
