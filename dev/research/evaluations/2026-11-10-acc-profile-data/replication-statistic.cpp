#include <iostream>
#include <iomanip>
#include <cmath>
int main(){std::cout<<std::setprecision(18);for(double x:{.5,.1,.999999,.123456789}){const double c=-std::log(x);double s=0;for(int j=0;j<10000000;++j)s+=c;const long double exact=-std::log((long double)x),mean=s/10000000;const auto root=[](long double v){return (2-v+std::sqrt(v*v+4))/(2*v);};std::cout<<x<<" "<<(double)((mean-exact)/exact)<<" "<<(double)((root(mean)-root(exact))/root(exact))<<"\n";}}
