#pragma once
#include "../sstd/local_accuracy.hpp"
namespace cppunivariate { namespace st_accuracy {
// Stable log Gamma(z+1/2)/Gamma(z), including its derivatives. Shift to
// the Stirling regime before differentiating the cancellation-free series.
inline local_accuracy::Jet half_ratio(local_accuracy::Jet z){
 local_accuracy::Jet correction=0;
 while(z.v<16){correction=correction+local_accuracy::log(z)-local_accuracy::log(z+.5);z=z+1;}
 auto t=1/z,t2=t*t;
 return correction+.5*local_accuracy::log(z)+t*(-1./8+t2*(1./192+t2*(-1./640+t2*(17./14336+t2*(-31./18432+t2*(691./180224-t2*5461./851968))))));
}
inline local_accuracy::Jet mode(const local_accuracy::Jet& df){
 if(df.v<32)return half_ratio(df/2)-.5*local_accuracy::log(df*3.14159265358979323846);
 auto t=2/df,t2=t*t;
 return -.91893853320467274178+t*(-1./8+t2*(1./192+t2*(-1./640+t2*(17./14336+t2*(-31./18432+t2*(691./180224-t2*5461./851968))))));
}
inline local_accuracy::Jet kernel(const local_accuracy::Jet& df,const local_accuracy::Jet& square){
 auto y=square/df;
 return y.v<.125?square/2+.5*local_accuracy::log1p(y)+df/2*local_accuracy::log1pmx(y):(df+1)/2*local_accuracy::log1p(y);
}
// Natural df derivatives lose one additional power of df compared with a
// log-df coordinate. Follow the actual jet after held-coordinate packing.
inline double df_axis_scale(const local_accuracy::Jet& df,int i){
 return df.g[i]!=0&&df.h[4*i+i]==0?df.v:1.;
}
// Packed held searches still need df-weighted derivative convergence. At a
// normal face, differentiating Lentz multipliers near one leaves O(eps)
// absolute derivative noise. Permit that stated arithmetic floor, then let
// the final projected-score/Newton check decide coefficient accuracy. Keep
// the original all-free axis-3 comparison unchanged.
inline double fraction_margin(const local_accuracy::Jet& df){
 const bool packed=df.g[0]!=0||df.g[1]!=0||df.g[2]!=0;
 return packed?std::max(4e-15,8*std::numeric_limits<double>::epsilon()*df.v):4e-15;
}
// Central Student probability fraction. Multiplying df/2 by x=h²/(df+h²)
// algebraically first removes the leading df-score cancellation at a normal
// face. The value and BOTH derivative orders must converge.
inline local_accuracy::Jet central_fraction(local_accuracy::Jet square,local_accuracy::Jet df,int maxit){
 const double a=.5;auto v=square/(1+square/df);
 local_accuracy::Jet c=1,d=1-(.5+a/df)*v/(a+1);d=1/d;auto h=d;
 for(std::int64_t m=1;m<=solver_budget(maxit);++m){
  auto aa=double(m)*(.5-double(m)/df)*v/((a+2.*m-1)*(a+2.*m));
  d=1/(1+aa*d);c=1+aa/c;h=h*d*c;
  aa=-(a+double(m))*(.5+(a+double(m))/df)*v/((a+2.*m)*(a+2.*m+1));
  d=1/(1+aa*d);c=1+aa/c;auto delta=d*c;h=h*delta;
  double err=std::abs(delta.v-1);
  for(int i=0;i<4;++i){
    const double si=1+std::abs(square.g[i]);
    err=std::max(err,std::abs(delta.g[i])*(df.g[i]!=0?df.v*df_axis_scale(df,i):1)/si);
    for(int j=0;j<4;++j){const double scale=si*(1+std::abs(square.g[j]))+std::abs(square.h[4*i+j]);err=std::max(err,std::abs(delta.h[4*i+j])*((df.g[i]!=0||df.g[j]!=0||df.h[4*i+j]!=0)?df.v*df_axis_scale(df,i)*df_axis_scale(df,j):1)/scale);}
  }
  if(err<fraction_margin(df))return h;
 }return local_accuracy::Jet(NAN);
}
// Upper beta fraction with a=df/2, b=1/2. Scale each EVEN Lentz state
// as D=d/a, C=c*a and the fraction as H=h/a. Odd deficits are kept
// divided by 1/a. All recurrences then have a finite normal limit, including
// their df derivatives: no subtraction of two leading O(1) df scores.
// The df jet identifies its derivative axes after held-coordinate packing.
inline local_accuracy::Jet tail_fraction(local_accuracy::Jet square,local_accuracy::Jet df,int maxit){
 auto u=2/df,y=square/df;
 auto D=(1+u)*(1+y)/(.5+(1+u)*square/2);
 local_accuracy::Jet C=1,H=D;
 for(std::int64_t m=1;m<=solver_budget(maxit);++m){
  auto K=double(m)*(.5-double(m))/((1+(2.*m-1)*u)*(1+2.*m*u)*(1+y));
  auto ds=-K*D/(1+u*K*D),cs=m==1?u*K:K/C;
  auto odd=1+u*(ds+cs)+u*u*ds*cs;
  auto B=(1+2.*m*u)*(1+(2.*m+1)*u);
  auto G=(2.*m+.5+u*(3.*m*m+1.5*m)+B*square/2)/(B*(1+y));
  D=1/(-ds+G*(1+u*ds));C=(cs+G)/(1+u*cs);
  auto delta=odd*D*C;H=H*delta;
  double err=std::abs(delta.v-1);
  for(int i=0;i<4;++i){
    const double si=1+std::abs(square.g[i]);
    err=std::max(err,std::abs(delta.g[i])*(df.g[i]!=0?df.v*df_axis_scale(df,i):1)/si);
    for(int j=0;j<4;++j){const double scale=si*(1+std::abs(square.g[j]))+std::abs(square.h[4*i+j]);err=std::max(err,std::abs(delta.h[4*i+j])*((df.g[i]!=0||df.g[j]!=0||df.h[4*i+j]!=0)?df.v*df_axis_scale(df,i)*df_axis_scale(df,j):1)/scale);}
  }
  if(err<fraction_margin(df))return H;
 }return local_accuracy::Jet(NAN);
}
inline local_accuracy::Jet logcdf(local_accuracy::Jet h,local_accuracy::Jet df,int maxit){
 // Near zero, use the central incomplete beta with x=h²/(df+h²).
 // This avoids subtracting h² from 1 in both value and derivative.
 if(std::abs(h.v)<1e-6){
  auto density=local_accuracy::exp(mode(df));
  return local_accuracy::log(.5+density*(h-(df+1)/(6*df)*h*h*h));
 }
 if(std::abs(h.v)<=2){
  auto front=local_accuracy::exp(mode(df)+local_accuracy::log(local_accuracy::abs(h))-kernel(df,h*h));
  auto central=2*front*central_fraction(h*h,df,maxit);
  return local_accuracy::log(h.v>0?.5*(1+central):.5*(1-central));
 }
 auto logtail=mode(df)+local_accuracy::log(local_accuracy::abs(h))-kernel(df,h*h)+
   local_accuracy::log(tail_fraction(h*h,df,maxit))-std::log(2.);
 return h.v<0?logtail:local_accuracy::log1p(-local_accuracy::exp(logtail));
}
template<class Sample>
inline local_accuracy::Jet objective(const Sample& x,const std::array<local_accuracy::Jet,4>& p,int maxit,bool restriction=false,double held_nu=NAN,bool raw_shape=false){
 auto omega=local_accuracy::exp(p[1]),nu=std::isfinite(held_nu)?local_accuracy::Jet(held_nu):raw_shape?p[3]:restriction?local_accuracy::Jet::variable(1,3):1+local_accuracy::exp(p[3]);
 if(raw_shape&&nu.v<1)return local_accuracy::Jet(NAN);
 auto constant=p[1]-mode(nu)-std::log(2.);
 local_accuracy::JetSum f(double(x.size())*constant);
 for(double v:x){auto z=(v-p[0])/omega;auto square=z*z;auto ratio=square.v<.125*nu.v?local_accuracy::sqrt(1+(1-square)/(nu+square)):local_accuracy::sqrt((nu+1)/(nu+square));auto h=std::abs(z.v)>1e70?(z.v>0?1.:-1.)*p[2]*local_accuracy::sqrt((nu+1)/(1+nu/square)):p[2]*z*ratio;
 auto term=std::abs(z.v)>1e70?(nu+1)/2*local_accuracy::softplus(2*local_accuracy::log(local_accuracy::abs(z))-local_accuracy::log(nu)):kernel(nu,square);
 f.add(term-logcdf(h,nu+1,maxit));}return f.value();
}
} }
