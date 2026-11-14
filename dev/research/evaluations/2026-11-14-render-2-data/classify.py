#!/usr/bin/env python3
"""Strict, disjoint classification of step-5a snapshot changes, second pass.

Usage: classify.py BASELINE NEW [--output FILE]. NEW may be a snapshot directory.
Extends ../2026-11-13-render-data/classify.py (first pass). Former tie stops
are classified by their new outcome: an estimate with a point log likelihood,
a limit stop, or an unresolved stop. Each keeps its candidates and comparison
bounds; the chosen candidate must have a lower bound within 8 ulps of the
largest (or be the first listed, for proved non-uniqueness). An estimate's
log likelihood is a point value: the chosen candidate's equal bounds, or a
value inside its bounds when the binding evaluates it (cauchy, ged). A limit's
log likelihood is the chosen face's point value or NaN, never a bound.
Two first-pass leftovers are named categories here: derived metadata on an
existing classed stop, and the foldnorm numerical-to-tolerance refinement.
binom and lgamma invalid samples move to their invalid-input status.
Every other payload field must be bit-identical. No fit arithmetic is done.
"""
import argparse,collections,gzip,json,math
from pathlib import Path

SENTENCE=' No improvement above the likelihood margin was found.'
ADVICE=' The best candidate is the '
BETAPR_LIMIT='Beta-prime global certificate cannot separate the finite profile from the gamma and inverse-gamma limits; no verified MLE.'
def load(path):
 p=Path(path)
 if p.is_dir():p=p/'snapshot.jsonl.gz'
 with gzip.open(p,'rt') as f:return {(r['family'],r['source'],r['case']):r for r in map(json.loads,f)}
def number(v):return float(v) if v in ('nan','inf','-inf') else float.fromhex(v)
def lower(c):
 v=number(c['loglik_lower']);return -math.inf if math.isnan(v) else v
def equal_lower(a,b):
 if a==b:return True
 if not (math.isfinite(a) and math.isfinite(b)):return False
 return abs(a-b)<=8*2.0**-52*max(abs(a),abs(b))
def expected_holds(old):
 expected=set(old['held'])
 if '+' in old['family']:
  expected-={'a','b','N'}
  if old['family'].startswith('lgser'):expected={'theta' if k=='prob' else k for k in expected}
 return expected
def same_candidates(a,b):
 """Values and bounds kept; a family candidate may gain its own names (tnorm, sged)."""
 if len(a)!=len(b):return False
 for x,y in zip(a,b):
  if x==y:continue
  if x['model']!=y['model'] or x['loglik_lower']!=y['loglik_lower'] or x['loglik_upper']!=y['loglik_upper'] or x['model']!='':return False
  # Snapshot keys are sorted: compare values, every old value kept.
  if collections.Counter(x['estimates'].values())-collections.Counter(y['estimates'].values()):return False
 return True
def chosen_index(new_res,match):
 cands=new_res['candidates'];top=max(lower(c) for c in cands)
 hits=[i for i,c in enumerate(cands) if match(c)]
 assert hits,'chosen candidate not among the candidates'
 if new_res.get('unique') is False:
  assert hits[0]==0,'non-unique estimate is not the first listed maximizer'
  return 0
 good=[i for i in hits if equal_lower(lower(cands[i]),top)]
 assert good,'chosen candidate does not have the largest lower bound'
 return good[0]
def check(old,new):
 a=dict(old['result']);b=dict(new['result']);o=b.get('outcome')
 assert {k:v for k,v in old.items() if k!='result'}=={k:v for k,v in new.items() if k!='result'},'case input changed'
 if not o:
  assert a==b,'unstructured termination changed'
  return 'identical timeout record' if a.get('kind')=='timeout' else None
 assert b.pop('reason')==o['reason'],'reason disagrees with outcome'
 if o['kind'] in ('estimate','limit'):assert b.pop('certified')==o['certified'],'certified disagrees'
 unique=b.pop('unique') if o['kind']=='estimate' else None
 if o['kind']=='estimate':assert unique==o['unique'],'unique disagrees'
 ae=a['extra'].copy();be=b['extra'].copy()
 ae.pop('certified',None)
 assert ae==be,'non-certification extra changed'
 removed=a['extra']!=b['extra'];a['extra']=b['extra']
 old_outcome=a['outcome']
 category=None
 if a['kind']=='tie':
  assert a['candidates'],'tie without candidates'
  assert same_candidates(a['candidates'],b['candidates']),'comparison candidates or bounds changed'
  for k in ('loglik_lower','loglik_upper','segment_loglik_upper','attainable_tol'):assert a[k]==b[k],'comparison bound %s changed'%k
  a['candidates']=b['candidates'];a['outcome']=o;a['core_status']=b['core_status']
  if b['kind']=='ok':
   assert o['kind']=='estimate','estimate kind without estimate outcome'
   held=expected_holds(old)
   def match(c):
    if c['model']!='':return False
    for n,v in b['estimates'].items():
     if n in old['held'] and n in held:continue
     if c['estimates'].get(n)!=v:return False
    return True
   i=chosen_index(dict(b,unique=unique),match);c=b['candidates'][i]
   for n,v in old['held'].items():
    if n in b['estimates']:assert number(b['estimates'][n])==v,'held coefficient changed'
   assert set(b['fixed'])==held,'comparison estimate lost requested holds'
   ll=number(b['logLik']);lo=number(c['loglik_lower']);hi=number(c['loglik_upper'])
   assert math.isfinite(ll),'estimate without a point log likelihood'
   if lo==hi:assert ll==lo,'log likelihood differs from the candidate point value'
   else:assert old['family'] in ('cauchy','ged') and lo<=ll<=hi,'bound reported as log likelihood'
   assert b['message']=='','estimate remains error'
   for k in ('kind','estimates','logLik','message','fixed'):a[k]=b[k]
   category='tie stop to estimate (point log likelihood)'
  elif b['kind']=='limit':
   assert o['kind']=='limit','limit kind without limit outcome'
   i=chosen_index(b,lambda c:c['model']==b['model'] and c['estimates']==b['limit_estimates']);c=b['candidates'][i]
   assert c['model']!='','limit names a family candidate'
   lim=b['limit_loglik']
   assert (lim==c['loglik_lower'] if c['loglik_lower']==c['loglik_upper'] else lim=='nan'),'limit log likelihood is a bound'
   head=BETAPR_LIMIT if old['family']=='betapr' else a['message']
   m=b['message']
   assert m.startswith(head),'limit message lost the comparison diagnostic'
   rest=m[len(head):]
   if not o['certified']:
    assert rest.endswith(SENTENCE),'uncertified limit without margin sentence';rest=rest[:-len(SENTENCE)]
   assert rest=='' or (rest.startswith(ADVICE) and rest.count('.')>=1),'unexpected limit message text'
   for k in ('kind','model','limit_estimates','limit_loglik','message'):a[k]=b[k]
   category='tie stop to limit stop'
  elif b['kind']=='unresolved':
   assert o['kind']=='failure' and o['reason']=='unresolved','unresolved kind without unresolved outcome'
   a['kind']=b['kind']
   category='tie stop to unresolved stop'
  else:raise AssertionError('tie became '+b['kind'])
 elif old['family'] in ('binom','lgamma') and a['kind']=='error' and b['kind']=='invalid' and old_outcome!=o:
  assert old_outcome=={'kind':'failure','reason':'no-convergence','certified':False,'unique':True},'not a former no-convergence status'
  assert o=={'kind':'failure','reason':'invalid-input','certified':False,'unique':True},'not invalid input'
  for k in ('kind','message','outcome','core_status'):a[k]=b[k]
  category='invalid-input status (binom, lgamma)'
 else:
  assert old_outcome==o,'outcome changed'
  if a['kind']=='error':
   expected={'invalid-input':'invalid','invalid-argument':'invalid','unsupported':'unsupported','no-convergence':'numerical','evaluation':'numerical'}
   assert b['kind']==expected[o['reason']],'wrong formerly plain error class'
   assert a['message']==b['message'],'plain error message changed'
   a['kind']=b['kind'];category='plain error to classed stop'
  elif a['kind']=='limit' and not o['certified']:
   assert b['message']==a['message']+SENTENCE,'margin sentence differs'
   a['message']=b['message'];category='margin sentence added'
  elif a['kind']=='ok':category='estimate flags added'
  elif a['kind']=='numerical' and b['kind']=='tolerance' and o['reason']=='tolerance':
   a['kind']=b['kind'];category='numerical to tolerance refinement (outcome reason)'
  elif removed:category='extra field removed'
 assert a==b,'unapproved payload changes: '+str([k for k in a.keys()|b.keys() if a.get(k)!=b.get(k)])
 if category is None and old!=new:category='derived metadata on classed stop'
 return category

def main():
 ap=argparse.ArgumentParser();ap.add_argument('baseline');ap.add_argument('new');ap.add_argument('--output');args=ap.parse_args()
 old,new=load(args.baseline),load(args.new);assert old.keys()==new.keys(),'case inventory changed'
 counts=collections.Counter();cases=[];defects=[];identical=0
 for k in sorted(old):
  try:
   category=check(old[k],new[k])
   if category:counts[category]+=1;cases.append(dict(family=k[0],source=k[1],case=k[2],category=category))
   else:identical+=1
  except (AssertionError,KeyError,ValueError) as e:defects.append(dict(family=k[0],source=k[1],case=k[2],defect=str(e)))
 result=dict(cases=len(old),categories=dict(sorted(counts.items())),identical=identical,defects=defects,classified=cases)
 if args.output:
  p=Path(args.output)
  data=json.dumps(result,indent=1)+'\n'
  if p.suffix=='.gz':
   with gzip.open(p,'wt') as f:f.write(data)
  else:p.write_text(data)
 print(json.dumps({k:v for k,v in result.items() if k!='classified'},indent=1))
 raise SystemExit(bool(defects))
if __name__=='__main__':main()
