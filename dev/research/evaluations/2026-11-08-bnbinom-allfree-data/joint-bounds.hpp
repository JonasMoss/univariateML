#pragma once
#include "cppunivariate/distributions/bnbinom/mle.hpp"
namespace bnb_probe {
struct Matrix {std::array<std::array<double,3>,3> v{};};
inline std::array<Matrix,2> hessian(const cppunivariate::bnbinom_detail::Data &d,const std::array<double,3>&l,const std::array<double,3>&u) {
  Matrix lo,hi;
  const double arl=cppunivariate::special::trigamma(u[1]+u[0]),arh=cppunivariate::special::trigamma(l[1]+l[0]);
  const double abl=cppunivariate::special::trigamma(u[1]+u[2]),abh=cppunivariate::special::trigamma(l[1]+l[2]);
  lo.v[0][0]=arl-cppunivariate::special::trigamma(l[0]);hi.v[0][0]=arh-cppunivariate::special::trigamma(u[0]);
  lo.v[1][1]=arl+abl-cppunivariate::special::trigamma(l[1]);hi.v[1][1]=arh+abh-cppunivariate::special::trigamma(u[1]);
  lo.v[2][2]=abl-cppunivariate::special::trigamma(l[2]);hi.v[2][2]=abh-cppunivariate::special::trigamma(u[2]);
  lo.v[0][1]=arl;hi.v[0][1]=arh;lo.v[1][2]=abl;hi.v[1][2]=abh;
  double mag=0;for(int j=0;j<3;++j)mag+=8*cppunivariate::special::trigamma(l[j]);
  for(std::size_t k=0;k<d.x.size();++k) {
    const double w=d.count[k]/d.n,x=d.x[k];
    const double tl=cppunivariate::special::trigamma(u[0]+u[1]+u[2]+x),th=cppunivariate::special::trigamma(l[0]+l[1]+l[2]+x);
    for(int i=0;i<3;++i)for(int j=i;j<3;++j){lo.v[i][j]-=w*th;hi.v[i][j]-=w*tl;}
    lo.v[0][0]+=w*cppunivariate::special::trigamma(u[0]+x);hi.v[0][0]+=w*cppunivariate::special::trigamma(l[0]+x);
    lo.v[2][2]+=w*cppunivariate::special::trigamma(u[2]+x);hi.v[2][2]+=w*cppunivariate::special::trigamma(l[2]+x);
    mag+=w*(8*th+cppunivariate::special::trigamma(l[0]+x)+cppunivariate::special::trigamma(l[2]+x));
  }
  const double e=1024*std::numeric_limits<double>::epsilon()*mag;
  for(int i=0;i<3;++i)for(int j=i;j<3;++j){lo.v[i][j]-=e;hi.v[i][j]+=e;lo.v[j][i]=lo.v[i][j];hi.v[j][i]=hi.v[i][j];}
  return {lo,hi};
}
inline Matrix inverse(const Matrix &h) {
  Matrix m;const auto a=h.v;
  const double det=a[0][0]*(a[1][1]*a[2][2]-a[1][2]*a[2][1])-a[0][1]*(a[1][0]*a[2][2]-a[1][2]*a[2][0])+a[0][2]*(a[1][0]*a[2][1]-a[1][1]*a[2][0]);
  for(int i=0;i<3;++i)for(int j=0;j<3;++j){const int u=(j+1)%3,v=(j+2)%3,s=(i+1)%3,t=(i+2)%3;m.v[i][j]=(a[u][s]*a[v][t]-a[u][t]*a[v][s])/det;}
  return m;
}
inline bool excluded(const cppunivariate::bnbinom_detail::Data &d,const std::array<double,3>&l,const std::array<double,3>&u) {
  std::array<double,3> center{};for(int j=0;j<3;++j)center[j]=std::sqrt(l[j])*std::sqrt(u[j]);
  const auto bounds=hessian(d,l,u),point=hessian(d,center,center);
  Matrix h;for(int i=0;i<3;++i)for(int j=0;j<3;++j)h.v[i][j]=(point[0].v[i][j]+point[1].v[i][j])/2;
  const auto m=inverse(h);
  std::array<cppunivariate::bnbinom_detail::ScalarScore,3> g{};
  for(int j=0;j<3;++j)g[j]=cppunivariate::bnbinom_detail::scalar_score(d,{center[0],center[1],center[2]},j);
  for(int i=0;i<3;++i) {
    double nl=center[i],nu=center[i],magnitude=std::abs(center[i]);
    for(int k=0;k<3;++k) {
      const double a=m.v[i][k]*(g[k].value-g[k].margin),b=m.v[i][k]*(g[k].value+g[k].margin);
      nl-=std::max(a,b);nu-=std::min(a,b);magnitude+=std::max(std::abs(a),std::abs(b));
    }
    for(int j=0;j<3;++j) {
      double el=i==j?1:0,eu=el,emag=1;
      for(int k=0;k<3;++k){const double a=m.v[i][k]*bounds[0].v[k][j],b=m.v[i][k]*bounds[1].v[k][j];el-=std::max(a,b);eu-=std::min(a,b);emag+=std::max(std::abs(a),std::abs(b));}
      const double error=64*std::numeric_limits<double>::epsilon()*emag;el-=error;eu+=error;
      const std::array<double,4> products{{el*(l[j]-center[j]),el*(u[j]-center[j]),eu*(l[j]-center[j]),eu*(u[j]-center[j])}};
      nl+=*std::min_element(products.begin(),products.end());nu+=*std::max_element(products.begin(),products.end());
      for(double v:products)magnitude+=std::abs(v);
    }
    const double margin=4096*std::numeric_limits<double>::epsilon()*(1+magnitude);
    if(std::isfinite(nl+nu) && (nu+margin<l[i] || nl-margin>u[i]))return true;
  }
  return false;
}
}
