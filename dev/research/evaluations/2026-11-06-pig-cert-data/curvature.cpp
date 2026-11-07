// Search for failure of log-pmf concavity in w; diagnostic, not a proof.
#include <cmath>
#include <iostream>
#include <iomanip>
int main() {
 double largest=-1e300,bmu=0,bw=0;int bk=0;
 for(int a=-20;a<=20;++a) for(int b=1;b<100;++b) {
  const double mu=std::exp(a/2.),w=b/100.;
  const double z=mu*(1-w*w)/(2*w),y=1/z;
  double lprev=0,lcur=0,eprev=0,ecur=0,vprev=0,vcur=0;
  for(int k=1;k<=300;++k) {
   if(k>=2) {
    double la=std::log((2.*k-3)*y)+lcur,lb=lprev,top=std::max(la,lb);
    double ln=top+std::log(std::exp(la-top)+std::exp(lb-top));
    double p=std::exp(la-ln),en=p*(ecur+1)+(1-p)*eprev;
    double vn=p*(vcur+std::pow(ecur+1-en,2))+(1-p)*(vprev+std::pow(eprev-en,2));
    lprev=lcur;lcur=ln;eprev=ecur;ecur=en;vprev=vcur;vcur=vn;
   }
   double second=-4*k*w+2*ecur*(3+w*w)+(vcur-ecur)*std::pow(1+w*w,2)/(w*w);
   if(second>largest){largest=second;bmu=mu;bw=w;bk=k;}
  }
 }
 std::cout<<std::setprecision(17)<<"largest_scaled_second="<<largest<<",mu="<<bmu<<",w="<<bw<<",k="<<bk<<"\n";
}
