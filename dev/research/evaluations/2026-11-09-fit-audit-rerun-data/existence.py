"""Sufficient finite-MLE conditions, not inference from a finite start point.
All arguments concern the restricted held model. Unlisted cases remain open.
"""
import math
from tnorm_existence import criterion as tnorm_criterion

def proof(f,c):
 x=c['data'];h=c['held']
 if f=='tnorm':
  attained,reason=tnorm_criterion(c)
  return reason if attained is True else ''
 positive=min(x)>0;nonconstant=max(x)>min(x)
 if f in ['gamma','invgamma'] and positive and (nonconstant or h):return 'positive nonconstant sample or a held coefficient: unique gamma score/scale solution (family dossier existence)'
 if f=='naka' and positive and (nonconstant or 'shape' in h or 'scale' in h and any(v*v!=h['scale'] for v in x)):return 'Nakagami profiled shape score has a positive Jensen/deviance statistic, or shape is held; its scale solution is analytic'
 if f=='beta' and all(0<v<1 for v in x) and (nonconstant or h):return 'beta dossier: strictly concave two-shape likelihood for nonconstant interior observations; one-held-shape score has a finite root'
 if f=='betapr' and 'scale' in h and positive and (nonconstant or 'shape1' in h or 'shape2' in h):return 'held-scale beta-prime is a bijective beta transformation; its interior shape MLE is finite'
 if f=='nsbeta' and 'min' in h and 'max' in h and all(h['min']<v<h['max'] for v in x) and (nonconstant or 'shape1' in h or 'shape2' in h):return 'held-endpoint nonstandard beta is a bijective beta transformation with a finite shape solution'
 if f=='lgamma' and min(x)>1 and (nonconstant or h):return 'log-gamma is a parameter-independent log transform of gamma, preserving finite-MLE attainment'
 if f=='kumar' and all(0<v<1 for v in x) and (nonconstant or h):return 'Kumaraswamy dossier: the profiled interior likelihood has a finite maximum unless both shapes are free on constant data'
 if f=='frechet' and 'mu' in h and min(x)>h['mu'] and (nonconstant or 'lambda' in h or 'sigma' in h and any(v-h['mu']!=h['sigma'] for v in x)):return 'fixed-threshold Frechet is inverse Weibull; its shape/scale attainment criterion applies'
 if f=='foldnorm' and min(x)>=0 and (nonconstant or 'sigma' in h or 'mu' in h and any(v!=h['mu'] for v in x)):return 'fixed-shape Gaussian tails and the included half-normal boundary: likelihood is coercive away from constant-data scale collapse'
 if f in ['ged','sged','snorm','huber','sn']:
  fixed_shape=(f=='ged' and 'nu' in h) or (f=='sged' and {'nu','xi'}<=h.keys()) or (f=='snorm' and 'xi' in h) or (f=='huber' and 'epsilon' in h) or (f=='sn' and 'alpha' in h)
  loc={'ged':'mean','sged':'mean','snorm':'mean','huber':'mu','sn':'xi'}[f];scale={'ged':'sd','sged':'sd','snorm':'sd','huber':'sigma','sn':'omega'}[f]
  if fixed_shape and (scale in h or nonconstant or loc in h and any(v!=h[loc] for v in x)):return 'held finite shape leaves exponential-power/Gaussian/Laplace tails: scale-zero likelihood vanishes away from constant data; all other parameter escapes are coercive'
 if f in ['lst','std','ht','sstd','st']:
  nu_name='df' if f=='lst' else 'nu';loc='mean' if f in ['std','sstd'] else 'xi' if f=='st' else 'mu';scale='sd' if f in ['std','sstd'] else 'omega' if f=='st' else 'sigma'
  shapes_held=nu_name in h and (f!='sstd' or 'xi' in h) and (f!='st' or 'alpha' in h)
  if shapes_held:
   import collections
   if f=='ht':
    center=h.get(loc,min(x))
    if min(x)<center:return ''
    k=sum(v==center for v in x)
   else:k=sum(v==h[loc] for v in x) if loc in h else max(collections.Counter(x).values())
   if scale in h or h[nu_name]*(len(x)-k)>k:return 'fixed finite Student shape(s): polynomial-tail scale-collapse exponent nu*(n-k)-k is positive, or scale is held; likelihood is coercive on the remaining location/scale model'
 if f=='invgauss' and positive and (nonconstant or 'shape' in h or 'mean' in h and any(v!=h['mean'] for v in x)):return 'inverse-Gaussian profile has positive quadratic reciprocal residual, or shape is held (dossier existence)'
 if f in ['laplace','logis','gumbel']:
  locname='location' if f=='logis' else 'mu';scalename='scale' if f=='logis' else 'sigma'
  if scalename in h or nonconstant or locname in h and any(v!=h[locname] for v in x):return 'location-scale dossier existence theorem; collapse condition excluded (median/strictly concave profile)'
 if f=='llogis' and positive and (nonconstant or 'shape' in h or 'scale' in h and any(v!=h['scale'] for v in x)):return 'bijective log transform gives the strictly concave logistic profile; collapse excluded'
 if f=='cauchy':
  import collections
  if 'scale' in h:return 'positive scale held: continuous coercive location likelihood attains a maximum'
  if 'location' in h:
   if 2*sum(v==h['location'] for v in x)<len(x):return 'Cauchy fixed-location scale theorem: fewer than half the observations equal the held location'
  else:
   multiplicities=list(collections.Counter(x).values())
   if 2*max(multiplicities)<len(x) or len(multiplicities)==2 and multiplicities[0]==multiplicities[1]:return 'Cauchy dossier existence theorem: sub-half ties, or two equally frequent distinct values'
 if f in ['hnorm','hcauchy']:
  mu=h.get('mu',min(x))
  if min(x)>=mu:
   if 'sigma' in h:return 'held positive half-family scale: endpoint likelihood is maximized at the sample minimum'
   k=sum(v==mu for v in x)
   if f=='hnorm' and k<len(x) or f=='hcauchy' and 2*k<len(x):return 'half-family endpoint/scale profile existence criterion from its dossier'
 if f=='gpd' and 'mu' in h and min(x)>h['mu'] and ('xi' not in h or h['xi']>=-1):return 'strictly positive threshold excesses and xi >= -1: finite maximum exists (GPD dossier existence)'
 if f=='triang' and nonconstant:
  width=max(x)-min(x);a=h.get('a',min(min(x),h.get('c',min(x)))-width);b=h.get('b',max(max(x),h.get('c',max(x)))+width);mode=h.get('c',(a+b)/2)
  if a<min(x) and b>max(x) and a<=mode<=b:return 'positive triangular candidate plus nonzero data range: compactness and upper-semicontinuity prove attainment (dossier)'
 if f=='skellam':
  if h.get('mu1')==0 and max(x)>0 or h.get('mu2')==0 and min(x)<0:return ''
  return 'Skellam dossier: continuity in the closed nonnegative rate quadrant and Poisson maximal-mass bounds at infinity give finite attainment'
 if f=='dnorm':
  if 'sd' in h or 'mean' not in h and max(x)-min(x)>=2 or 'mean' in h and any(not(v<=h['mean']<=v+1) for v in x):return 'grouped-normal dossier: affine-section concavity; zero-scale cell-collapse condition excluded'
 if f=='cmp' and min(x)>=0:
  if 'nu' in h or h.get('lambda')==0 and max(x)==0:return 'CMP monotone rate mean equation (zero rate is included)'
  if 'lambda' in h:
   if max(x)>1:return 'CMP fixed-positive-rate dispersion mean map: positive mean log-factorial gives a finite root'
  elif max(x)==0 or max(x)-min(x)>=2:return 'CMP dossier convex-support criterion: empirical sufficient statistic is not on an adjacent-count face, or all-zero rate is included'
 if f=='nbinom' and min(x)>=0 and ('size' in h or 0<h.get('prob',0)<1):return 'negative-binomial held-coordinate likelihood is coercive with a finite score solution or an included zero-count endpoint'
 if f in ['zeta','yulesimon','lgser'] and min(x)>=1 and max(x)>1:return 'strictly monotone one-parameter exponential/Stieltjes score has a finite root when an observation exceeds one (dossier)'
 if f=='zip' and min(x)>=0 and (h.get('pi')!=1 or max(x)==0) and (max(x)>0 or 'lambda' in h or 'pi' not in h or h.get('pi')==1):return 'ZIP dossier existence: positive counts exclude the zero-rate escape; all-zero samples attain pi=1 when inflation is free'
 if f=='zib' and min(x)>=0 and ('size' in h and max(x)<=h['size'] or 0<h.get('prob',0)<1) and (h.get('pi')!=1 or max(x)==0):return 'ZIB dossier: fixed-size compactness, or fixed interior probability makes masses vanish as integer size grows'
 if f in ['ztbinom','zmbinom'] and min(x)>=(1 if f=='ztbinom' else 0) and 0<h.get('prob',0)<1:
  if 'size' in h and max(x)>h['size']:return ''
  if f=='zmbinom' and (h.get('p0')==0 and min(x)==0 or h.get('p0')==1 and max(x)>0):return ''
  return 'held interior binomial probability: positive observed masses vanish with size, so the integer maximum is attained (dossier)'
 if f in ['norm','lnorm','logitnorm']:
  if f=='lnorm' and not positive or f=='logitnorm' and not all(0<v<1 for v in x):return ''
  y=x if f=='norm' else [math.log(v) for v in x] if f=='lnorm' else [math.log(v)-math.log1p(-v) for v in x]
  locname={'norm':'mean','lnorm':'meanlog','logitnorm':'mu'}[f];sdname={'norm':'sd','lnorm':'sdlog','logitnorm':'sigma'}[f]
  loc=h.get(locname)
  if sdname in h or loc is None and nonconstant or loc is not None and any(v!=loc for v in y):return 'normal quadratic likelihood after the stated bijective transformation; positive residual sum of squares or held scale'
 if f=='exp' and min(x)>=0 and sum(x)>0:return 'strictly concave rate likelihood with positive data sum'
 if f=='pois' and min(x)>=0:return 'Poisson mean MLE, including the proper zero-rate point mass'
 if f in ['weibull','invweibull'] and positive:
  if nonconstant or 'shape' in h or 'scale' in h and any(v!=h['scale'] for v in x):return 'monotone Weibull shape score (reciprocate for inverse Weibull) and analytic positive scale; collapse condition excluded'
 if f in ['invexp','rayleigh'] and positive:return 'strict one-coordinate likelihood with a positive finite sufficient statistic and analytic finite scale MLE'
 if f in ['geom','ztgeom'] and min(x)>=(1 if f=='ztgeom' else 0) and 'prob' not in h:return 'geometric mean equation gives a finite probability, including its proper atomic endpoint'
 if f=='ztpois' and min(x)>=1 and 'lambda' not in h:return 'strictly increasing truncated-Poisson mean map; mean one is its proper zero-rate endpoint'
 if f in ['binom','ztbinom','zmbinom'] and 'size' in h and h['size']>=1 and 'prob' not in h:
  lower=0 if f in ['binom','zmbinom'] else 1
  if min(x)>=lower and max(x)<=h['size']:
   if f=='zmbinom' and ('p0' in h and (h['p0']==0 and min(x)==0 or h['p0']==1 and max(x)>0)):return ''
   return 'fixed finite support: monotone (possibly positive-truncated) binomial mean equation, with included proper endpoint members'
 if f=='dunif':
  if ('min' not in h or h['min']<=min(x)) and ('max' not in h or h['max']>=max(x)):return 'finite integer interval; free bounds are observed extrema and interval cardinality stays positive'
 if f=='unif':
  lo=h.get('min',min(x));hi=h.get('max',max(x))
  if lo<=min(x) and hi>=max(x) and lo<hi:return 'uniform likelihood decreases with positive interval width; free endpoints are sample extrema'
 if f in ['pareto','power'] and positive:
  endpoint='b' if f=='pareto' else 'alpha';shape='a' if f=='pareto' else 'beta';extreme=min(x) if f=='pareto' else max(x)
  b=h.get(endpoint,extreme);compatible=b<=min(x) if f=='pareto' else b>=max(x)
  if compatible and (shape in h or any(v!=b for v in x)):return 'monotone endpoint profile and strictly concave shape log likelihood with positive log-ratio sum'
 if f=='fatigue' and 'mu' in h and min(x)>h['mu']:
  y=[v-h['mu'] for v in x]
  if 'alpha' in h or 'beta' in h and any(v!=h['beta'] for v in y) or nonconstant:return 'fixed-threshold Birnbaum–Saunders profile: finite positive scale, and nonzero shape residual (family dossier existence)'
 if f in ['zmgeom','zmpois','zmlogarithmic'] and min(x)>=0:
  p0=h.get('p0');pname='lambda' if f=='zmpois' else 'prob'
  if p0==0 and min(x)==0 or p0==1 and max(x)>0:return ''
  if pname not in h:return 'zero mass is a binomial proportion; positive-count mean map has a finite root or an included one-point endpoint'
 return ''


def nonexistence(f,c):
 x=c['data'];h=c['held']
 if f=='tnorm':
  attained,reason=tnorm_criterion(c)
  return reason if attained is False else ''
 same=max(x)==min(x)
 if f=='cauchy' and 'scale' not in h:
  import collections
  if 'location' in h:
   if 2*sum(v==h['location'] for v in x)>=len(x):return 'Cauchy held-location tie criterion: finite MLE absent'
  else:
   counts=list(collections.Counter(x).values())
   if 2*max(counts)>=len(x) and not(len(counts)==2 and counts[0]==counts[1]):return 'Cauchy atom multiplicity at least half, excluding the equally split two-point attained case'
 if f=='hcauchy' and 'sigma' not in h and min(x)>=h.get('mu',min(x)) and 2*sum(v==h.get('mu',min(x)) for v in x)>=len(x):return 'half-Cauchy dossier: half ties give an unattained zero-scale supremum; more ties give unbounded likelihood'
 if f=='cmp' and 'nu' not in h:
  if 'lambda' in h and h['lambda']>0 and max(x)<=1:return 'fixed-positive-rate CMP dispersion tends to infinity when mean log-factorial is zero'
  if not h and max(x)>0 and max(x)-min(x)<=1:return 'CMP sufficient statistic is on a positive point/adjacent-count convex-support face'
 if f in ['zeta','yulesimon','lgser'] and not h and min(x)==max(x)==1:return 'all-one sample has an unattained parameter-limit supremum (family dossier)'
 if f=='dnorm' and 'sd' not in h:
  if not h and max(x)-min(x)<=1 or 'mean' in h and all(v<=h['mean']<=v+1 for v in x):return 'grouped normal: all observed cell closures admit the zero-scale latent limit; no positive-scale MLE'
 if f=='zip' and 'lambda' not in h and 'pi' in h and h['pi']<1 and max(x)==0:return 'ZIP held inflation below one: all-zero supremum needs excluded zero rate'
 if same:
  if f in ['gamma','invgamma','weibull','invweibull','llogis'] and not h and min(x)>0:return 'constant positive data: unbounded concentration likelihood (family dossier)'
  if f in ['norm','lnorm','logitnorm','laplace','logis','gumbel','hnorm']:
   scale={'norm':'sd','lnorm':'sdlog','logitnorm':'sigma','laplace':'sigma','logis':'scale','gumbel':'sigma','hnorm':'sigma'}[f]
   location={'norm':'mean','lnorm':'meanlog','logitnorm':'mu','laplace':'mu','logis':'location','gumbel':'mu','hnorm':'mu'}[f]
   y=math.log(x[0]) if f=='lnorm' and x[0]>0 else math.log(x[0])-math.log1p(-x[0]) if f=='logitnorm' and 0<x[0]<1 else x[0]
   if scale not in h and (location not in h or h[location]==y):return 'constant data at a free/held fitted location: unbounded zero-scale likelihood (dossier)'
 return ''
