#!/usr/bin/env python3
"""Strict, disjoint classification of step-5a snapshot changes (no fit arithmetic).

Usage: classify.py BASELINE NEW [--output FILE]. New may be a snapshot directory.
Outcome-derived reason and stop certification are checked before comparison;
adding those metadata to an already classed stop is not a payload difference.
Categories have priority tie, plain error, margin, estimate flags, removed extra.
Every other payload field must be bit-identical.
"""
import argparse,collections,gzip,json,math
from pathlib import Path

SENTENCE=' No improvement above the likelihood margin was found.'
def load(path):
 p=Path(path)
 if p.is_dir():p=p/'snapshot.jsonl.gz'
 with gzip.open(p,'rt') as f:return {(r['family'],r['source'],r['case']):r for r in map(json.loads,f)}
def number(v):return float(v) if v in ('nan','inf','-inf') else float.fromhex(v)
def check(old,new):
 a=dict(old['result']);b=dict(new['result']);o=b.get('outcome')
 assert {k:v for k,v in old.items() if k!='result'}=={k:v for k,v in new.items() if k!='result'},'case input changed'
 if not o:
  assert a==b,'unstructured termination changed'
  return None
 assert b.pop('reason')==o['reason'],'reason disagrees with outcome'
 if o['kind'] in ('estimate','limit'):assert b.pop('certified')==o['certified'],'certified disagrees'
 if o['kind']=='estimate':assert b.pop('unique')==o['unique'],'unique disagrees'
 assert a['outcome']==o,'outcome changed'
 ae=a['extra'].copy();be=b['extra'].copy()
 ae.pop('certified',None)
 assert ae==be,'non-certification extra changed'
 removed=a['extra']!=b['extra'];a['extra']=b['extra']
 category=None
 if a['kind']=='tie':
  assert b['kind']=='ok' and o['kind']=='estimate','tie did not become estimate'
  assert a['candidates'],'tie without candidates'
  candidates=a['candidates'];idx=0 if not o['unique'] else max(range(len(candidates)),key=lambda i:number(candidates[i]['loglik_lower']))
  chosen=candidates[idx]
  assert b['estimates']==chosen['estimates'],'wrong candidate selected'
  assert b['logLik']==chosen['loglik_lower'],'candidate log likelihood changed'
  assert b['message']=='','estimate remains error'
  # A returned estimate must retain the caller's holds, which the old stop
  # did not expose. Values remain exactly those of the selected witness.
  expected=set(old['held'])
  if '+' in old['family']:
   expected-= {'a','b','N'}
   if old['family'].startswith('lgser'):expected={'theta' if k=='prob' else k for k in expected}
  assert set(b['fixed'])==expected,'comparison estimate lost requested holds'
  a['fixed']=b['fixed']
  for k in ('kind','estimates','logLik','message'):a[k]=b[k]
  category='tie stop to estimate'
 elif a['kind']=='error':
  expected={'invalid-input':'invalid','invalid-argument':'invalid','unsupported':'unsupported','no-convergence':'numerical','evaluation':'numerical'}
  assert b['kind']==expected[o['reason']],'wrong formerly plain error class'
  assert a['message']==b['message'],'plain error message changed'
  a['kind']=b['kind'];category='plain error to classed stop'
 elif a['kind']=='limit' and not o['certified']:
  assert b['message']==a['message']+SENTENCE,'margin sentence differs'
  a['message']=b['message'];category='margin sentence added'
 elif a['kind']=='ok':category='estimate flags added'
 elif removed:category='extra field removed'
 assert a==b,'unapproved payload changes: '+str([k for k in a.keys()|b.keys() if a.get(k)!=b.get(k)])
 return category

def main():
 ap=argparse.ArgumentParser();ap.add_argument('baseline');ap.add_argument('new');ap.add_argument('--output');args=ap.parse_args()
 old,new=load(args.baseline),load(args.new);assert old.keys()==new.keys(),'case inventory changed'
 counts=collections.Counter();cases=[];defects=[];metadata_only=[]
 for k in sorted(old):
  try:
   category=check(old[k],new[k])
   if category:counts[category]+=1;cases.append(dict(family=k[0],source=k[1],case=k[2],category=category))
   elif old[k]!=new[k]:metadata_only.append(dict(family=k[0],source=k[1],case=k[2]))
  except (AssertionError,KeyError) as e:defects.append(dict(family=k[0],source=k[1],case=k[2],defect=str(e)))
 result=dict(cases=len(old),categories=dict(counts),derived_metadata_only=len(metadata_only),defects=defects,classified=cases,metadata_only_cases=metadata_only)
 if args.output:
  p=Path(args.output)
  data=json.dumps(result,indent=1)+'\n'
  if p.suffix=='.gz':
   with gzip.open(p,'wt') as f:f.write(data)
  else:p.write_text(data)
 print(json.dumps({k:v for k,v in result.items() if k not in ('classified','metadata_only_cases')},indent=1))
 raise SystemExit(bool(defects))
if __name__=='__main__':main()
