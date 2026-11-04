#!/usr/bin/env python3
"""One compact figure, computed afresh from independent conditional fits."""
from pathlib import Path
import json
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from research import conditional,ll,moments
OUT=Path(__file__).resolve().parent
fig,axs=plt.subplots(1,3,figsize=(12,3.3),layout='constrained')
x=np.array([0.,1.,10.]);w=np.array([9.,6.,5.]);nus=np.geomspace(1,1000,180);normal=moments(x,w)['normal']
ps=[conditional(x,w,nu) for nu in nus]
axs[0].plot(1/nus,[p['ll']-normal for p in ps]);axs[0].axhline(0,color='0.5',lw=.8);axs[0].set(xlabel='1 / ν',ylabel='Profile log likelihood − normal',title='Negative normal slope; finite winner')
r=json.loads((OUT/'fixtures.json').read_text())['two-interior-maxima'];x=np.array(r['x']);w=np.array(r['weights']);nus=np.geomspace(1,1000,200);ps=[conditional(x,w,nu) for nu in nus]
axs[1].semilogx(nus,[p['ll']-r['moments']['normal'] for p in ps]);axs[1].axhline(0,color='0.5',lw=.8)
for p in r['roots']:
    axs[1].plot(p['nu'],p['ll']-r['moments']['normal'],'o',ms=4)
axs[1].set(xlabel='ν',ylabel='Profile log likelihood − normal',title='Two interior stationary maxima')
zoom=axs[1].inset_axes([.08,.30,.43,.40]);nz=np.linspace(1,1.08,55)
zoom.plot(nz,[conditional(x,w,nu)['ll']-r['moments']['normal'] for nu in nz])
zoom.set_title('Near ν = 1',fontsize=8);zoom.tick_params(labelsize=6)
x=np.arange(-2,3,dtype=float);w=np.ones(5);mus=np.linspace(-2.3,2.3,1800);b=.05;nu=3;s=b/np.sqrt(nu)
y=[ll(x,w,mu,s,nu) for mu in mus];axs[2].plot(mus,y)
axs[2].set(xlabel='Location μ (ν = 3, scale held)',ylabel='Log likelihood',title='Exactly five held-scale modes')
for ax in axs:ax.grid(alpha=.2)
fig.savefig(OUT/'profiles.png',dpi=125)
print('Wrote profiles.png')
