#!/usr/bin/env python3
"""Independent reproduction of the settled large-shape density defect.
The package value was recorded by diagnose-oracle.R before its oracle used
stats::dt. This records a needed change outside this job's authorized seam.
"""
import json
import mpmath as mp
mp.mp.dps=80
x=[mp.mpf(-1),mp.mpf(2)]
mu,sigma,nu=map(mp.mpf,[.2,1.5307277909393948,97432810384.283554])
true=sum(mp.loggamma((nu+1)/2)-mp.loggamma(nu/2)-mp.log(nu*mp.pi)/2-mp.log(sigma)-(nu+1)*mp.log1p(((v-mu)/sigma)**2/nu)/2 for v in x)
recorded=mp.mpf(-3.6877369241232691)
assert recorded-true>mp.mpf('.0002')
print(json.dumps({'precision_dps':80,'data':[-1,2],'mu':str(mu),'sigma':str(sigma),'nu':str(nu),'recorded_package_loglik':str(recorded),'mp_loglik':str(true),'false_gain':str(recorded-true),'scope':'Existing lst/tails.hpp logmode -> special::lbeta cancellation; left unchanged as instructed.'},indent=2))
