#!/usr/bin/env python3
import json
from pathlib import Path
import mpmath as mp
mp.mp.dps=80
b=mp.mpf(float(1.4239025460520294));k,l,z=23,12,4;n=k+l+z

def ll(p,a):
    lb=(mp.loggamma(1/p)-mp.loggamma(3/p))/2
    return -mp.log(2)-lb-mp.loggamma(1+1/p)-(k*mp.exp(p*(mp.log(a)-lb))+l*mp.exp(p*(mp.log(b)-lb)))/n
p1,p2,a=mp.findroot(lambda p,q,a:(mp.diff(lambda v:ll(v,a),p),mp.diff(lambda v:ll(v,a),q),ll(p,a)-ll(q,a)),(mp.mpf('1.4'),mp.mpf(25),mp.mpf('.3')),tol=mp.mpf('1e-70'),maxsteps=100)
assert p1>1 and p2>p1
assert mp.diff(lambda v:ll(v,a),p1,2)<0 and mp.diff(lambda v:ll(v,a),p2,2)<0
out={'precision':mp.mp.dps,'held_mean':0,'held_sd':1,'radii':[float(a),float(b)],'counts_each_sign':[k,l],'zeros':2*z,'roots':[str(p1),str(p2)],'loglik_per_observation':str(ll(p1,a)),'inner_radius_exact_tie':str(a),'rounded_radius_likelihood_difference':str(ll(p1,mp.mpf(float(a)))-ll(p2,mp.mpf(float(a))))}
print(json.dumps(out,indent=2))
