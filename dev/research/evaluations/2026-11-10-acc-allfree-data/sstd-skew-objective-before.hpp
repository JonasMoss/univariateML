#pragma once
#include "cppunivariate/distributions/sstd/local_accuracy.hpp"
namespace cppunivariate { namespace local_accuracy {
template<class Sample>
inline Jet uncompensated_skew_objective(const Sample& x,const std::array<Jet,4>& p,bool student){
 const Jet sigma=exp(p[1]),nu=(student?2.:1.)+exp(p[2]),xi=exp(p[3]);
 Jet m,logscale,constant;
 if(student){m=exp(lgamma((nu-1)/2)-lgamma(nu/2))*sqrt((nu-2)/3.14159265358979323846);constant=lgamma((nu+1)/2)-lgamma(nu/2)-.5*log((nu-2)*3.14159265358979323846);}
 else{logscale=.5*(lgamma(1/nu)-lgamma(3/nu));m=exp(lgamma(2/nu)-.5*(lgamma(1/nu)+lgamma(3/nu)));constant=log(nu)-logscale-lgamma(1/nu)-std::log(2.);}
 const Jet m2=m*m,su=sqrt((1-m2)*(xi*xi+1/(xi*xi))+2*m2-1),mu=m*(xi-1/xi);
 const Jet base=p[1]-log(su)+log(xi+1/xi)-std::log(2.)-constant;
 Jet f=double(x.size())*base;
 for(double v:x){Jet z=(v-p[0])*su/sigma+mu;Jet w=z.v>0?z/xi:z*xi;if(student)f=f+(nu+1)/2*log1p(w*w/(nu-2));else if(w.v!=0)f=f+exp(nu*(log(abs(w))-logscale));}
 return f;
}
} }
