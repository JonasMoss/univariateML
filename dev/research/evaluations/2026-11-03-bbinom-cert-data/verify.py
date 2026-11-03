#!/usr/bin/env python3
"""Re-evaluate retained map candidates in C++ and mixture witnesses at 70 dps.

Run after compiling the *existing* map-data/check.cpp to
build/scratch/bbinom-cert-map-check. Does not modify the previous study.
"""
import csv
import json
import math
from pathlib import Path
import subprocess
import mpmath as mp

HERE = Path(__file__).resolve().parent
MAP = HERE.parent / '2026-11-02-bbinom-map-data'

def read(base, name):
    with (base/name).open() as stream:
        return list(csv.DictReader(stream))

mp.mp.dps = 70
samples = read(MAP, 'samples.csv')
profiles = read(MAP, 'certified.csv')
ext = read(MAP, 'extended.csv')
requests = []
for sample, profile in zip(samples, profiles):
    requests.append((sample['data'], profile))
for profile in ext:
    requests.append((samples[int(profile['index'])]['data'], profile))
for profile in read(MAP, 'profile-grid.csv'):
    # Recheck all 1,006 selected profile points, including the (1,1,5)
    # counterexample, both OPZ samples and the one-count perturbation.
    requests.append((profile['data'].replace('-', ' '), profile))
lines = [f"{len(x.split())} {r['N']} {x}" for x,r in requests]
result = subprocess.run(['build/scratch/bbinom-cert-map-check'],
                        input='\n'.join(lines)+'\n',text=True,capture_output=True,check=True)
output = result.stdout.splitlines()
assert len(output) == len(requests)
max_diff = 0.
for line,(data,old) in zip(output,requests):
    n,status,ll,a,b = line.split()
    assert n == old['N'] and int(status) == int(old['status'])
    assert int(status) in (0,3,4,5,6)
    diff = abs(float(ll)-float(old['loglik']))
    assert diff < 1e-7, (data, n, diff)
    max_diff = max(max_diff,diff)
# The negative-slope binomial-face counterexample was in the targeted
# search, rather than the seven retained profile grids.
target = subprocess.run(['build/scratch/bbinom-cert-map-check'],
                        input='3 7 3 4 7\n',text=True,capture_output=True,check=True)
n,status,ll,a,b = target.stdout.split()
assert n == '7' and status == '0'
assert abs(float(ll)+5.65458630) < 1e-7
mu = mp.mpf(14)/3
binom13 = sum(mp.loggamma(14)-mp.loggamma(v+1)-mp.loggamma(14-v)
              +v*mp.log(mu/13)+(13-v)*mp.log1p(-mu/13) for v in (3,4,7))
assert mp.mpf(ll) > binom13+mp.mpf('.12')
target_loglik = ll
faces = read(MAP,'faces.csv')
witnesses = read(HERE,'mixture-witnesses.csv')
errors = []
for row in witnesses:
    x = list(map(int,row['data'].split()))
    mu = mp.mpf(sum(x))/len(x)
    if row['index'].isdigit():
        face = faces[int(row['index'])]
        a = mp.mpf(face['nb_a']) if face['nb_a'] else mp.inf
    else:
        # These seven are diagnostic examples; reconstruct their shape
        # from the public point likelihood by recomputing the score root.
        var = sum((mp.mpf(v)-mu)**2 for v in x)/len(x)
        if var > mu:
            def score(a):
                return sum(sum(1/(a+j) for j in range(v)) for v in x)-len(x)*mp.log1p(mu/a)
            lo,hi = mp.mpf('1e-12'),mp.mpf(1)
            while score(hi) > 0:
                hi *= 2
            for _ in range(260):
                mid = (lo+hi)/2
                if score(mid)>0: lo=mid
                else: hi=mid
            a = (lo+hi)/2
        else:
            a = mp.inf
    def logq(v):
        if a == mp.inf:
            return v*mp.log(mu)-mu-mp.loggamma(v+1)
        return (sum(mp.log1p(j/a) for j in range(v))+v*mp.log(mu)
                -(a+v)*mp.log1p(mu/a)-mp.loggamma(v+1))
    w,lam = mp.mpf(row['weight']),mp.mpf(row['lambda_'])
    ll = sum(mp.log((1-w)*mp.exp(logq(v))+w*mp.exp(v*mp.log(lam)-lam-mp.loggamma(v+1))) for v in x)
    error = abs(ll-mp.mpf(row['mixture_loglik']))
    assert error < mp.mpf('2e-7')
    errors.append(float(error))
    if row['index'].isdigit():
        assert ll > mp.mpf(row['candidate_loglik']) + mp.mpf('0.0006')
summary = dict(cpp_profile_calls=len(output),candidate_calls=len(samples)+len(ext),
               selected_grid_calls=len(output)-len(samples)-len(ext),
               maximum_cpp_map_loglik_difference=max_diff,
               high_precision_witnesses=len(witnesses),
               maximum_witness_loglik_difference=max(errors),
               extra_slope_counterexample_calls=1,
               extra_slope_counterexample_loglik=target_loglik,
               extra_slope_counterexample_binomial_loglik=str(binom13))
(HERE/'verification.json').write_text(json.dumps(summary,indent=2)+'\n')
print(json.dumps(summary,indent=2),flush=True)
