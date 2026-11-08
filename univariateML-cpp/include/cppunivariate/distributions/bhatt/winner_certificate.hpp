#pragma once
#include "certificate_math.hpp"

namespace cppunivariate {
namespace bhatt_detail {
struct CertWinner {
  CertRoot root{};
  double lo=NAN,hi=NAN;
  bool ok=false;
};
// For every k in the returned interval, a nuisance Krawczyk inclusion puts
// the unique (globally concave) nuisance optimum inside the negative-definite
// full-Hessian box. The profiled likelihood is strictly concave there and
// its unique maximizer is the enclosed full score root.
inline CertWinner cert_winner_region(const CertModel& model,const CertRoot& root,int maxit) {
  CertWinner out;out.root=root;if(!root.ok||model.shape<0)return out;
  const int shape=model.shape,nuisance=shape;
  if(nuisance==0) {out.lo=root.box[shape].lo;out.hi=root.box[shape].hi;out.ok=true;return out;}
  std::array<std::array<double,3>,3> point{},inverse{};
  for(int i=0;i<nuisance;++i)for(int j=0;j<nuisance;++j)
    point[i][j]=(root.evaluation.hessian[i][j].lo/2+root.evaluation.hessian[i][j].hi/2)*root.scale[i]*root.scale[j];
  if(!cert_inverse(point,nuisance,inverse))return out;
  double radius=root.radius,width=radius*root.scale[shape];
  for(int it=0;it<solver_budget(maxit);++it) {
    if(radius<=4*root.error||radius<512*std::numeric_limits<double>::epsilon())return out;
    out.lo=root.center[shape]-width;out.hi=root.center[shape]+width;
    if(!(out.lo>0&&out.lo<root.enclosure[shape].lo&&out.hi>root.enclosure[shape].hi))return out;
    auto box=root.box;
    for(int i=0;i<nuisance;++i)box[i]=cert_pad(root.center[i]-radius*root.scale[i],root.center[i]+radius*root.scale[i]);
    box[shape]=CertInterval(out.lo,out.hi);
    const auto h=cert_hessian(model,box);
    if(!cert_negative_definite(h,model.dim)){radius*=.5;width=std::min(width,radius*root.scale[shape]);continue;}
    const CertInterval dk(out.lo-root.center[shape],out.hi-root.center[shape]);
    bool included=true;double contraction=0;
    for(int i=0;i<nuisance;++i) {
      CertInterval displacement;double row=0;
      for(int j=0;j<nuisance;++j) {
        const CertInterval g=root.evaluation.gradient[j]+h[j][shape]*dk;
        displacement=displacement-CertInterval(inverse[i][j]*root.scale[j])*g;
      }
      for(int j=0;j<nuisance;++j) {
        CertInterval residual(i==j?1.:0.);
        for(int k=0;k<nuisance;++k)residual=residual-CertInterval(inverse[i][k]*root.scale[k]*root.scale[j])*h[k][j];
        row+=residual.mag();
      }
      contraction=std::max(contraction,row);
      included=included&&std::isfinite(row)&&displacement.valid()&&row<1&&displacement.mag()+row*radius<radius;
    }
    if(included){out.ok=true;return out;}
    if(contraction>=.9){radius*=.5;width=std::min(width,radius*root.scale[shape]);}
    else width*=.5;
  }
  return out;
}
struct CertificateWork {
  std::int64_t used=0,cap=0;
  bool exhausted=false;
  explicit CertificateWork(int maxit):cap(certificate_budget(maxit)){}
  bool charge() {if(used>=cap){exhausted=true;return false;}++used;return true;}
};
}
}
