#pragma once
#include "winner_certificate.hpp"

namespace cppunivariate {
namespace emg_detail {
// Exact integer polynomial arithmetic, bounded below 2^52 so each operation
// is represented exactly in binary64. Overflow or nonintegral standardized
// observations simply leaves the normal tail unresolved.
inline bool cert_integer(double x) {return std::isfinite(x)&&std::abs(x)<4503599627370496.&&x==std::trunc(x);}
inline bool cert_poly_product(const std::vector<double>& a,const std::vector<double>& b,
    std::vector<double>& out,CertificateWork& work) {
  out.assign(a.size()+b.size()-1,0);
  for(std::size_t i=0;i<a.size();++i)for(std::size_t j=0;j<b.size();++j) {
    if(!work.charge())return false;
    const double product=a[i]*b[j],sum=out[i+j]+product;
    if(!cert_integer(product)||!cert_integer(sum))return false;
    out[i+j]=sum;
  }
  return true;
}
inline bool normal_fixed_location_scale_tail(span x,double mu,double sigma,double kmin,CertificateWork& work) {
  int exponent=0;
  if(!(kmin>0&&std::isfinite(kmin))||std::frexp(sigma,&exponent)!=.5)return false;
  std::vector<double> numerator{1},denominator{1};
  for(double value:x) {
    const double difference=value-mu,bvirtual=value-difference,avirtual=difference+bvirtual;
    const double error=(value-avirtual)+(bvirtual-mu);
    const double z=difference/sigma;
    if(error!=0||!cert_integer(z)||std::scalbn(z,exponent-1)!=difference||!(kmin>z))return false;
    std::array<std::vector<double>,6> powers; powers[0]={1};
    for(int j=1;j<=5;++j)if(!cert_poly_product(powers[j-1],{1,-z},powers[j],work))return false;
    auto n=powers[4],d=powers[5];
    for(int j=0;j<=2;++j) {const double term=9*powers[2][j],sum=n[j+2]+term;if(!cert_integer(term)||!cert_integer(sum))return false;n[j+2]=sum;}
    if(!cert_integer(n[4]+8))return false;
    n[4]+=8;
    for(int j=0;j<=3;++j) {const double term=10*powers[3][j],sum=d[j+2]+term;if(!cert_integer(term)||!cert_integer(sum))return false;d[j+2]=sum;}
    for(int j=0;j<=1;++j) {const double term=15*powers[1][j],sum=d[j+4]+term;if(!cert_integer(term)||!cert_integer(sum))return false;d[j+4]=sum;}
    std::vector<double> next;
    if(!cert_poly_product(numerator,n,next,work))return false;
    numerator=next;
    if(!cert_poly_product(denominator,d,next,work))return false;
    denominator=next;
  }
  numerator.resize(denominator.size(),0);
  std::size_t first=denominator.size();std::vector<double> difference(denominator.size());
  for(std::size_t i=0;i<difference.size();++i) {
    if(!work.charge())return false;
    difference[i]=denominator[i]-numerator[i];if(!cert_integer(difference[i]))return false;
    if(first==difference.size()&&difference[i]!=0)first=i;
  }
  if(first==difference.size())return true;
  if(!(difference[first]>0))return false;
  CPPUNIV_EXTENDED_REAL remainder=0,power=1;
  for(std::size_t j=first+1;j<difference.size();++j) {
    if(!work.charge())return false;
    power/=kmin;if(difference[j]<0)remainder+=-difference[j]*power;
  }
  const CPPUNIV_EXTENDED_REAL margin=128*std::numeric_limits<double>::epsilon()*(difference[first]+remainder);
  // M(t) is strictly below its fourth upper convergent:
  // (t^4+9t^2+8)/(t^5+10t^3+15t), t>0. The exact polynomial
  // comparison proves product_i k M(k-z_i)<1 on the whole k>=kmin tail.
  return difference[first]>remainder+margin;
}
}
}
