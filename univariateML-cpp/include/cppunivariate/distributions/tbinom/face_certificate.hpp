#pragma once
#include "../ztbinom/face_certificate.hpp"
namespace cppunivariate {
namespace tbinom_detail {
// Truncation endpoints may legitimately be infinite. The generic finite-
// coefficient witness guard cannot represent that family-specific domain.
inline void record_witness(StopDetail& d,std::vector<double> coef,double ll,
    bool budget,bool comparison=false) {
  if(coef.size()!=4 || !std::isfinite(ll) || !std::isfinite(coef[0]) ||
     !std::isfinite(coef[3]) || !(coef[0]>=0 && coef[0]<=1) ||
     !(coef[3]>=0 && coef[3]==std::floor(coef[3])) ||
     std::isnan(coef[1]) || std::isnan(coef[2]) || !(coef[1]<coef[2])) {
    d.loglik_upper=INFINITY;d.budget_exhausted=budget;return;
  }
  d.candidates.push_back({coef,ll,ll});
  d.loglik_lower=std::isnan(d.loglik_lower)?ll:std::max(d.loglik_lower,ll);
  d.loglik_upper=INFINITY;d.budget_exhausted=budget;d.comparison_tie=comparison;
}
inline bool face_product(std::uint64_t a,std::uint64_t b,std::uint64_t& out) {
  if(b>0 && a>std::numeric_limits<std::uint64_t>::max()/b)return false;
  out=a*b;return true;
}
inline bool face_add(std::uint64_t a,std::uint64_t b,std::uint64_t& out) {
  if(a>std::numeric_limits<std::uint64_t>::max()-b)return false;
  out=a+b;return true;
}
// Small checked rationals for exact equality tests. Overflow declines the
// optional higher-order certificate; it cannot turn a small coefficient to zero.
struct FaceRational {
  std::int64_t n=0,d=1;
  bool ok=true;
  FaceRational(std::int64_t v=0):n(v) {}
  FaceRational(std::uint64_t v,std::uint64_t w) {
    const auto g=std::gcd(v,w);v/=g;w/=g;
    if(v>std::uint64_t(INT64_MAX) || w>std::uint64_t(INT64_MAX)){ok=false;return;}
    n=static_cast<std::int64_t>(v);d=static_cast<std::int64_t>(w);
  }
};
inline FaceRational face_multiply(FaceRational a,FaceRational b) {
  FaceRational r;
  if(!a.ok || !b.ok){r.ok=false;return r;}
  const auto g=std::gcd(std::abs(a.n),b.d),h=std::gcd(std::abs(b.n),a.d);
  a.n/=g;b.d/=g;b.n/=h;a.d/=h;
  if((b.n!=0 && std::abs(a.n)>INT64_MAX/std::abs(b.n)) || a.d>INT64_MAX/b.d){r.ok=false;return r;}
  r.n=a.n*b.n;r.d=a.d*b.d;return r;
}
inline FaceRational face_plus(FaceRational a,FaceRational b) {
  FaceRational r;
  if(!a.ok || !b.ok){r.ok=false;return r;}
  const auto g=std::gcd(a.d,b.d),ad=a.d/g,bd=b.d/g;
  if(std::abs(a.n)>INT64_MAX/bd || std::abs(b.n)>INT64_MAX/ad || ad>INT64_MAX/b.d){r.ok=false;return r;}
  const auto x=a.n*bd,y=b.n*ad;
  if((y>0 && x>INT64_MAX-y) || (y<0 && x<-INT64_MAX-y)){r.ok=false;return r;}
  r.n=x+y;r.d=ad*b.d;
  const auto h=std::gcd(std::abs(r.n),r.d);r.n/=h;r.d/=h;return r;
}
inline FaceRational face_minus(FaceRational a,FaceRational b) {b.n=-b.n;return face_plus(a,b);}
inline FaceRational face_divide(FaceRational a,FaceRational b) {
  if(!b.ok || b.n==0){a.ok=false;return a;}
  if(b.n<0){b.n=-b.n;a.n=-a.n;}
  std::swap(b.n,b.d);return face_multiply(a,b);
}
inline bool zero_second_window(const std::vector<std::uint64_t>& count,
    const std::vector<std::uint64_t>& weight,std::uint64_t n,std::uint64_t Z,
    FaceRational& m,FaceRational& aa,FaceRational& beta) {
  const int U=static_cast<int>(count.size())-1;
  FaceRational bb,pb,a2,ka,k2;
  for(int k=0;k<=U;++k) {
    const FaceRational q(weight[k],Z),p(count[k],n),a(-std::int64_t(k)*(k-1)/2);
    const auto b=face_divide(FaceRational(-std::int64_t(k)*(k-1)*(2*k-1)),FaceRational(12));
    m=face_plus(m,face_multiply(q,FaceRational(k)));
    aa=face_plus(aa,face_multiply(q,a));bb=face_plus(bb,face_multiply(q,b));pb=face_plus(pb,face_multiply(p,b));
    a2=face_plus(a2,face_multiply(q,face_multiply(a,a)));
    ka=face_plus(ka,face_multiply(q,face_multiply(FaceRational(k),a)));
    k2=face_plus(k2,face_multiply(q,FaceRational(std::int64_t(k)*k)));
  }
  const auto variance=face_minus(k2,face_multiply(m,m));
  const auto covariance=face_minus(ka,face_multiply(m,aa));
  beta=face_divide(covariance,variance);
  const auto D=face_minus(face_minus(a2,face_multiply(aa,aa)),face_multiply(covariance,beta));
  const auto B=face_minus(face_minus(pb,bb),face_divide(D,FaceRational(2)));
  return B.ok && B.n==0 && beta.ok && m.ok && aa.ok;
}
// With exact first- and second-order cancellation, the same mean-preserving
// Q_N=Q(1+h/N) gives a third-order bound C/N^3+R/N^4. See the job report.
inline double third_order_window_cutoff(const std::vector<std::uint64_t>& count,
    const std::vector<std::uint64_t>& weight,std::uint64_t n,std::uint64_t Z,
    binomial_certificate_detail::Work& work,bool& exhausted) {
  const int U=static_cast<int>(count.size())-1;
  if(!work.charge((16+static_cast<std::int64_t>(count.size()))*static_cast<std::int64_t>(count.size()))){exhausted=true;return INFINITY;}
  FaceRational m,aa,beta;
  if(!zero_second_window(count,weight,n,Z,m,aa,beta))return INFINITY;
  const double mu=double(m.n)/m.d,abar=double(aa.n)/aa.d,slope=double(beta.n)/beta.d;
  const double eps=8192*std::numeric_limits<double>::epsilon();
  double C=0,R=0,H=0,scale=1;
  for(int k=0;k<=U;++k) {
    const double q=double(weight[k])/Z,p=double(count[k])/n;
    const double a=-double(k)*(k-1)/2,b=-double(k)*(k-1)*(2*double(k)-1)/12;
    const double c=-double(k)*k*(k-1)*(k-1)/12;
    const double h=a-abar-slope*(k-mu),hp=std::abs(h)+eps*(1+std::abs(a)+std::abs(abar)+std::abs(slope)*(k+mu));
    C+=(p-q)*c-q*(h*b+h*h*h/6);
    scale+=(p+q)*std::abs(c)+q*(std::abs(h*b)+std::abs(h*h*h)/6);
    double fourth=0;for(int j=0;j<k;++j)fourth+=double(j)*j*j*j;
    R+=q*((2./3)*std::pow(hp,4)+hp*std::abs(c)+.75*fourth);H=std::max(H,hp);
  }
  C+=eps*scale;R=R*(1+eps)+eps;
  if(!(C<0))return INFINITY;
  const double end=std::ceil(std::max({2.*U,4*H,4*R/(-C)}));
  if(!(end<4503599627370496.)){work.numerical=true;return INFINITY;}
  return end;
}
// Evaluate N^3 times the mean-preserving Jensen bound after its exact
// first two zero coefficients have been removed. Degree-eight series with
// geometric remainders avoid subtracting O(1/N^2) terms near the face.
inline double scaled_third_jensen(const std::vector<std::uint64_t>& count,
    const std::vector<std::uint64_t>& weight,std::uint64_t n,std::uint64_t Z,
    const FaceRational& m,const FaceRational& aa,const FaceRational& beta,
    double N,binomial_certificate_detail::Work& work) {
  const int U=static_cast<int>(count.size())-1;
  if(N<2*U)return INFINITY;
  if(!work.charge(16*static_cast<std::int64_t>(count.size())*static_cast<std::int64_t>(count.size())))return INFINITY;
  const double mu=double(m.n)/m.d,abar=double(aa.n)/aa.d,slope=double(beta.n)/beta.d;
  const double eps=8192*std::numeric_limits<double>::epsilon();
  double value=0,error=0,scale=1;
  for(int k=0;k<=U;++k) {
    const double q=double(weight[k])/Z,p=double(count[k])/n;
    const double a=-double(k)*(k-1)/2,b=-double(k)*(k-1)*(2.*k-1)/12;
    const double h=a-abar-slope*(k-mu),z=h/N;
    if(std::abs(z)>.5)return INFINITY;
    double entropy=0,power=h*h*h;
    for(int r=3;r<=8;++r){entropy+=(r%2?-1.:1.)*power/(r*(r-1));power*=z;}
    const double er=std::abs(h*h*h)*std::pow(std::abs(z),6)/(72*(1-std::abs(z)));
    double logrest=0,lr=0;
    for(int j=1;j<k;++j) {
      const double y=j/N;double term=double(j)*j*j;
      for(int r=3;r<=8;++r){logrest-=term/r;term*=y;}
      lr+=double(j)*j*j*std::pow(y,6)/(9*(1-y));
    }
    const double coefficient=p-q-q*z;
    value+=q*entropy+coefficient*logrest-q*h*b;
    error+=q*er+std::abs(coefficient)*lr;
    scale+=q*std::abs(entropy)+std::abs(coefficient*logrest)+q*std::abs(h*b);
  }
  return value+error+eps*scale;
}
inline bool rational_window_data(ispan x,int U,
    binomial_certificate_detail::Work& work,bool& exhausted,
    std::vector<std::uint64_t>& count,std::vector<std::uint64_t>& weight,
    std::uint64_t& Z,std::uint64_t& a,std::uint64_t& d) {
  if(U<2 || x.size()==0)return false;
  std::vector<std::uint64_t> factorial{1};
  for(int k=1;k<=U;++k) {
    if(!work.charge(1)){exhausted=true;return false;}
    std::uint64_t f=0;if(!face_product(factorial.back(),static_cast<std::uint64_t>(k),f))return false;
    factorial.push_back(f);
  }
  // All following loops are bounded by the represented factorial table.
  const std::int64_t width=static_cast<std::int64_t>(U)+1;
  if(!work.charge(static_cast<std::int64_t>(x.size())+4*width*width)){exhausted=true;return false;}
  count.assign(static_cast<std::size_t>(width),0);
  std::uint64_t S=0,F2=0;
  for(int v:x) {
    if(v<0 || v>U)return false;
    if(!face_add(S,static_cast<std::uint64_t>(v),S) ||
       !face_add(F2,static_cast<std::uint64_t>(v)*(v-1),F2))return false;
    ++count[static_cast<std::size_t>(v)];
  }
  const std::uint64_t n=x.size();std::uint64_t us=0,un=0;
  if(!face_product(static_cast<std::uint64_t>(U),S,us) ||
     !face_product(static_cast<std::uint64_t>(U),n,un) || us<=F2 || un<=S)return false;
  a=us-F2;d=un-S;
  const auto gcd=std::gcd(a,d);a/=gcd;d/=gcd;
  weight.assign(static_cast<std::size_t>(width),0);
  Z=0;std::uint64_t Q1=0;
  for(int k=0;k<=U;++k) {
    std::uint64_t w=factorial[U]/factorial[k];
    for(int j=0;j<k;++j)if(!face_product(w,a,w))return false;
    for(int j=k;j<U;++j)if(!face_product(w,d,w))return false;
    weight[k]=w;
    std::uint64_t term=0;
    if(!face_add(Z,w,Z) || !face_product(w,static_cast<std::uint64_t>(k),term) || !face_add(Q1,term,Q1))return false;
  }
  std::uint64_t left=0,right=0;
  if(!face_product(S,Z,left) || !face_product(n,Q1,right) || left!=right)return false;
  return true;
}
// Exact finite-window criterion, conditional only on integer comparisons.
// If a mean-matched Poisson law has rational rate, its scaled probabilities
// and factorial moments can be compared without interpreting a tiny gap as zero.
// Overflow refuses this optional criterion; the ordinary signed route remains.
inline double poisson_window_face(ispan x,int U,
    binomial_certificate_detail::Work& work,bool& exhausted,double& loglik,bool second_order,double& cutoff) {
  exhausted=false;loglik=NAN;
  std::vector<std::uint64_t> count,weight;
  std::uint64_t Z=0,a=0,d=0;
  if(!rational_window_data(x,U,work,exhausted,count,weight,Z,a,d))return NAN;
  const std::int64_t width=static_cast<std::int64_t>(U)+1;
  const std::uint64_t n=x.size();
  if(!work.charge(width)){exhausted=true;return NAN;}
  std::vector<std::uint64_t> factorial(static_cast<std::size_t>(width),1);
  for(int k=1;k<=U;++k)factorial[k]=factorial[k-1]*k; // checked by rational_window_data
  std::uint64_t S=0,F2=0,left=0,right=0;
  for(int k=0;k<=U;++k){S+=count[k]*k;F2+=count[k]*std::uint64_t(k)*(k-1);}
  if(second_order) {
    // The integer mean identity above also proves equality of second moments.
    if(!work.charge(width*width+4*width)){exhausted=true;return NAN;}
    const double eps=8192*std::numeric_limits<double>::epsilon();
    const double mu=double(S)/n,abar=-double(F2)/(2*double(n));
    double variance=0,covariance=0;
    for(int i=1;i<=U;++i)for(int j=0;j<i;++j) {
      const double w=(double(weight[i])/Z)*(double(weight[j])/Z),dx=i-j;
      variance+=w*dx*dx;
      covariance+=w*(-double(i)*(i-1)/2+double(j)*(j-1)/2)*dx;
    }
    if(!(variance>0) || !std::isfinite(covariance)){work.numerical=true;return NAN;}
    const double beta=covariance/variance;
    const double pad=eps*(1+double(U)*U+std::abs(abar)+std::abs(beta)*(U+std::abs(mu)+1));
    double D=0,H=0,R=0,pb=0,qb=0;
    for(int k=0;k<=U;++k) {
      const double q=double(weight[k])/Z,a0=-double(k)*(k-1)/2;
      const double b0=-double(k)*(k-1)*(2*double(k)-1)/12;
      const double h=std::abs(a0-abar-beta*(k-mu));
      const double hp=h+pad,hm=std::max(0.,h-pad);
      D+=q*hm*hm;H=std::max(H,hp);
      R+=q*((2./3)*hp*hp*hp+hp*std::abs(b0)+a0*a0);
      pb+=(double(count[k])/n)*b0;qb+=q*b0;
    }
    const double B=pb-qb-.5*D*(1-eps)+eps*(1+std::abs(pb)+std::abs(qb)+D);
    R=R*(1+eps)+eps;
    if(!(B<0)) {
      cutoff=third_order_window_cutoff(count,weight,n,Z,work,exhausted);
      if(!std::isfinite(cutoff))return NAN;
    } else {
      cutoff=std::ceil(std::max({2.*U,4*H,4*R/(-B)}));
      if(!std::isfinite(cutoff) || cutoff>=4503599627370496.){work.numerical=true;return NAN;}
    }
  } else for(int r=2;r<=U;++r) {
    std::uint64_t empirical=0,poisson=0;
    for(int k=r;k<=U;++k) {
      const std::uint64_t choose=factorial[k]/factorial[r]/factorial[k-r];
      std::uint64_t term=0;
      if(!face_product(count[k],choose,term) || !face_add(empirical,term,empirical) ||
         !face_product(weight[k],choose,term) || !face_add(poisson,term,poisson))return NAN;
    }
    if(!face_product(empirical,Z,left) || !face_product(n,poisson,right) || left<right)return NAN;
  }
  loglik=0;
  for(int k=0;k<=U;++k)if(count[k]>0)loglik+=count[k]*std::log(double(weight[k])/double(Z));
  return double(a)/double(d);
}
// Exact factorial-moment dominance is sufficient at every finite size.
inline double exact_poisson_moment_face(ispan x,int U,
    binomial_certificate_detail::Work& work,bool& exhausted,double& loglik) {
  double cutoff=INFINITY;
  return poisson_window_face(x,U,work,exhausted,loglik,false,cutoff);
}
// Project a(k)=-k(k-1)/2 off constants and X in L2(Q), obtaining h.
// Q_N=Q(1+h/N) preserves mean. Jensen plus its relative entropy gives
// (LL_N-LL_face)/n <= B/N^2+R/N^3, uniformly in probability, where
// B=E_P b-E_Q b-E_Q h^2/2 and b=-sum(j^2)/2. For N>=2U,2|max h|,
// R=(2/3)E|h|^3+E|hb|+E a^2 bounds both Taylor remainders.
// Computed comparisons use padded coefficients and twice the needed cutoff.
inline double second_order_poisson_cutoff(ispan x,int U,
    binomial_certificate_detail::Work& work,bool& exhausted,double& rate,double& loglik) {
  double cutoff=INFINITY;
  rate=poisson_window_face(x,U,work,exhausted,loglik,true,cutoff);
  return std::isfinite(rate)?cutoff:INFINITY;
}
} // namespace tbinom_detail
} // namespace cppunivariate
