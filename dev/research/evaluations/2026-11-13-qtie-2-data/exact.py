from fractions import Fraction as F
from decimal import Decimal,localcontext
from math import comb
from pathlib import Path
import json
out=Path('dev/research/evaluations/2026-11-13-qtie-2-data');rows=[]
for n in (2,3,8,10):
 for prob in (F(1,4),F(1,2),F(3,4)):
  masses=[F(comb(n,k))*prob**k*(1-prob)**(n-k) for k in range(n+1)]
  den=1-masses[0];acc=F(0)
  for k in range(1,n):
   acc+=masses[k]/den;rows.append(('ztbinom',[n,float(prob)],k,acc))
  for pi in (F(1,4),F(1,2)):
   acc=pi
   for k in range(n):
    acc+=(1-pi)*masses[k];rows.append(('zib',[n,float(prob),float(pi)],k,acc))
for k in range(1,21):rows.append(('ztgeom',[.5],k,1-F(1,2)**k))
# Finite zeta exponent -1 has masses proportional to the integers.
for k in range(1,8):rows.append(('zeta-window',[8,-1],k,F(k*(k+1),8*9)))
json.dump([dict(id=id,args=args,k=k,numerator=p.numerator,denominator=p.denominator) for id,args,k,p in rows],open(out/'exact.json','w'),indent=2)
r=['\ntest_that("modifier exact rational jumps use the supplied-scale tie rule", {']
by={}
for id,args,k,p in rows:
 for lower in (True,False):
  tail=p if lower else 1-p
  for log in (False,True):
   with localcontext() as ctx:
    ctx.prec=100;d=Decimal(tail.numerator)/Decimal(tail.denominator);v=float(d.ln() if log else d)
   arg=','.join(map(str,args));l=str(lower).lower();g=str(log).lower()
   if id=='zeta-window':expr=f"cppunivariate::zeta_window_eval('q',{v!r},8,-1,{l},{g})"
   else:expr=f'cppunivariate::distributions::q{id}({v!r},{arg},{l},{g})'
   by.setdefault('zipf' if id=='zeta-window' else id,[]).append(f' CHECK({expr}=={k});')
   if id=='ztbinom':fit=f'mlbinom(size={args[0]},prob={args[1]},zero="truncated")'
   elif id=='zib':fit=f'mlbinom(size={args[0]},prob={args[1]},pi={args[2]},zero="inflated")'
   elif id=='ztgeom':fit='mlgeom(prob=.5,zero="truncated")'
   else:fit='mlzeta(s=-1,truncate=c(1,8))'
   r.append(f' expect_equal(qml({v!r},{fit},lower.tail={str(lower).upper()},log.p={str(log).upper()}),{k})')
for id,lines in by.items():
 p=Path(f'univariateML-cpp/tests/modifiers/{id}.cpp')
 if 'exact rational modifier ties' in p.read_text():continue
 with p.open('a') as f:
  if id=='zipf':f.write('\n#include "cppunivariate/distributions/zeta/truncation.hpp"\n')
  f.write(f'\nTEST_CASE("{id}: exact rational modifier ties") {{\n'+ '\n'.join(lines)+'\n}\n')
r.append('})\n')
rpath=Path('bindings/r/tests/testthat/test_modifiers.R')
if 'modifier exact rational jumps use the supplied-scale tie rule' not in rpath.read_text():
 with rpath.open('a') as f:f.write('\n'.join(r))
