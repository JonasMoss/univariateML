"""Read-only source inventory, including structural and protected dependencies.
Run from the worktree root; no git state is written.
"""
from pathlib import Path
import csv,re,subprocess,json,difflib
root=Path.cwd();base=Path(__file__).resolve().parent
core=root/'univariateML-cpp/include/cppunivariate'
paths=sorted([*core.glob('core/*.hpp'),*core.glob('distributions/*/*.hpp'),*core.glob('special/*.hpp')])
# Work-unit overrides for global work queues and discrete scans.
certificate={'bbinom','burr','cauchy','dgamma','foldnorm','gpd','kumar','lomax','lst','nbinom','paralogis','slash','skellam','sn','std','trgamma','invtrgamma','zinb','ztnbinom'}
noiter=set()
default={}
old_default={}
for p in (root/'bindings/common/distributions').glob('*/fit.hpp'):
 m=re.search(r'int iterlim\s*=\s*(\d+)',p.read_text())
 if m:
  default[p.parent.name]=int(m[1])
  previous=subprocess.check_output(['git','show','HEAD:'+p.relative_to(root).as_posix()],text=True)
  original=re.search(r'int iterlim\s*=\s*(\d+)',previous)
  old_default[p.parent.name]=int(original[1]) if original else int(m[1])
 else:noiter.add(p.parent.name)
def loops(s):
 out=[]
 for m in re.finditer(r'\b(for|while)\s*\(',s):
  # Ignore comment-only matches.
  start=s.rfind('\n',0,m.start())+1
  if s[start:m.start()].lstrip().startswith('//'):continue
  i=m.end();level=1
  while i<len(s) and level:
   level+=(s[i]=='(')-(s[i]==')');i+=1
  expr=' '.join(s[m.end():i-1].split())
  if m[1]=='for' and ';' not in expr:continue
  out.append((s.count('\n',0,m.start())+1,m[1],expr))
 return out
rows=[]
for p in paths:
 rel=p.relative_to(root).as_posix()
 try:old=subprocess.check_output(['git','show','HEAD:'+rel],stderr=subprocess.DEVNULL,text=True)
 except subprocess.CalledProcessError:old=''
 new=p.read_text();family=p.parent.name if '/distributions/' in rel else p.parent.name
 before=loops(old);after=loops(new)
 # Align loop sequences so inserted accounting loops do not shift every
 # subsequent old/new comparison. Unpaired additions are explicitly new.
 paired={}
 matcher=difflib.SequenceMatcher(a=[r[2] for r in before],b=[r[2] for r in after],autojunk=False)
 for tag,a,b,c,d in matcher.get_opcodes():
  if tag=='equal':
   for k in range(d-c):paired[c+k]=before[a+k][2]
  elif tag=='replace':
   for k in range(min(b-a,d-c)):paired[c+k]=before[a+k][2]
 for ordinal,(line,kind,expr) in enumerate(after):
  previous=paired.get(ordinal,'[new loop]')
  structural=not re.search(r'maxit|max_ls|budget|cap|limit|visited|work|eval|profiles|\b\d{2,}\b',expr)
  if family in certificate and re.search(r'(pending|queue|heap|stack|cells)\.empty',expr):classification='certificate queue (body guard; see cap table)';unit='certificate evaluation'
  elif family=='cmb' and 'blocks.empty' in expr:classification='enumeration queue (shared ledger)';unit='enumeration cell'
  elif structural:classification='data/dimension or structural bound';unit='not a work cap'
  elif '/special/' in rel or p.name=='dpqr.hpp':classification='protected evaluator dependency: retained; not all q/r paths are reached by fitters';unit='review separately'
  elif family in noiter and previous==expr:classification='no-iterlim exception: retained';unit='fixed (policy exception)'
  elif 'enumeration_budget' in expr:classification='enumeration control';unit='enumeration cell'
  elif re.search(r'maxit|max_ls|solver_budget',expr) or (family=='dweibull' and expr=='int i = 0;; ++i, step *= 2'):classification='solver control';unit='solver iteration'
  elif re.search(r'budget|visited|eval|profiles|work',expr) and family in certificate:classification='certificate queue (see cap table)';unit='certificate evaluation'
  elif family=='cmb' and re.search(r'K|budget|work',expr):classification='enumeration (see cap table)';unit='enumeration cell'
  else:classification='structural formula, grid, seed set or numeric-domain termination; see exceptions';unit='not an adaptive work cap'
  def cap_value(expression,old=False):
   control=(old_default if old else default).get(family,100 if family in ['ztpois','core'] else None)
   if 'enumeration_budget' in expression:return 100000*min(control,100000) if control else 'caller control'
   if 'certificate_budget' in expression:return 128*min(control,100000) if control else 'caller control'
   if old and 'max_ls' in expression:return 20
   if re.search(r'maxit|max_ls',expression):
    m=re.search(r'std::min\(\s*(?:maxit\s*,\s*(\d+)|(\d+)\s*,\s*maxit)\s*\)',expression)
    if control is None:return 'caller control'
    return min(control,int(m[1] or m[2])) if m else min(control,100000)
   parts=expression.split(';')
   condition=parts[1].strip() if len(parts)==3 else expression
   m=re.search(r'\b\w+\s*(?:<|<=)\s*(\d+)\b',condition)
   if m:return int(m[1])
   return 'data bound' if structural else 'see explicit cap table/source guard'
  rows.append(dict(family=family,file=rel,line=line,ordinal=ordinal,loop=kind,old_expression=previous,new_expression=expr,unit=unit,classification=classification,default_iterlim=default.get(family,''),old_cap_at_default=cap_value(previous,True),new_cap_at_default=cap_value(expr)))
with (base/'loop-inventory.csv').open('w') as f:
 writer=csv.DictWriter(f,fieldnames=list(rows[0]));writer.writeheader();writer.writerows(rows)
# Explicit adaptive-budget table: default R/JS control, including delegates.
entries=[
 ('bbinom','shape profile certificate','certificate evaluation','200000','128*I'),
 ('bbinom','size range and support scans, shared ledger','enumeration cell','1000 size points; no aggregate cell cap','100000*I'),
 ('bbinom','held-shape fixed-size likelihood scans','enumeration cell','unbudgeted finite support','100000*I'),
 ('burr','Weibull bracket doubling','solver iteration','80','I'),
 ('burr','nuisance enclosure widening','solver iteration','6','I'),
 ('gb2','adaptive shape-box expansion','solver iteration','3','I'),
 ('huber','cold full-reduction Newton trajectory','solver iteration','min(I,max(12,16000000/n))','I'),
 ('huber','fallback scale bracket doubling','solver iteration','1024','I'),
 ('dweibull','inner bracket doubling','solver iteration','65','I'),
 ('gpd','root-center Newton polish','solver iteration','8','I'),
 ('lomax','two root-center Newton polishes (each)','solver iteration','8','I'),
 ('paralogis','certificate center Newton polish','solver iteration','8','I'),
 ('slash','scale-bound bracket halving','solver iteration','8','I'),
 ('tlambda','support rounding solves (each)','solver iteration','8','I'),
 ('burr','all-free global cover','certificate evaluation','4096','128*I'),
 ('burr','held-scale profile points','certificate evaluation','I','128*I'),
 ('cauchy','held-scale location cover','certificate evaluation','64*I','128*I'),
 ('cmb','fixed-size normalizers and nested mean matches, shared ledger','enumeration cell','unbudgeted N+1 tables per solve','100000*I'),
 ('cmb','free-size monotone rescue normalizers','enumeration cell','(I+110)*(N+1)*230 reserved cells','100000*I'),
 ('cmb','three free-size normalizer covers','enumeration cell','10000000','100000*I'),
 ('cmb','both natural coordinates held, size cover','enumeration cell','4000000','100000*I'),
 ('dgamma','shape global cover','certificate evaluation','1000*I','128*I'),
 ('foldnorm','held-location and free covers','certificate evaluation','512*I','128*I'),
 ('gpd','held-threshold covers','certificate evaluation','128*I','128*I'),
 ('kumar','free-shape score certificate','certificate evaluation','16*I','128*I'),
 ('lomax','global profile cover','certificate evaluation','128*I','128*I'),
 ('lst','all-free t cover','certificate evaluation','4096','128*I'),
 ('nbinom','size score certificate','certificate evaluation','16*I','128*I'),
 ('paralogis','free and held-scale covers including rescue','certificate evaluation','32768','128*I'),
 ('slash','held and free covers','certificate evaluation','1000000','128*I'),
 ('skellam','joint-rate finite cover including tail nominations','certificate evaluation','1000*I','128*I'),
 ('trgamma','shared free-shape segment cover','certificate evaluation','depth 40; no aggregate cap','128*I'),
 ('invtrgamma','reciprocal transformed-gamma segment cover','certificate evaluation','depth 40; no aggregate cap','128*I'),
 ('sn','shape cover','certificate evaluation','20000','128*I'),
 ('std','all-free t cover','certificate evaluation','4096','128*I'),
 ('zinb','held-probability size cover','certificate evaluation','32*I','128*I'),
 ('zinb','joint finite and ZIP face covers (each)','certificate evaluation','64*I','128*I'),
 ('ztnbinom','size cover','certificate evaluation','16*I','128*I'),
 ('ztnbinom','recurrence support','enumeration cell','100000','100000*I'),
 ('zib','held-probability integer scan','enumeration cell','1000000 profile points','100000*I'),
 ('ztbinom','held-probability integer scan','enumeration cell','1000000 profile points','100000*I'),
]
parents={'invburr':'burr','invparalogis':'paralogis','invpareto':'lomax','pareto2':'lomax','pareto4':'burr','betapr':'lomax','gb2':'burr/lomax/trgamma','fpareto':'gb2','zmnbinom':'ztnbinom','bnbinom':'nbinom'}
old_delegate={
 'invburr':'4096 all-free; caller I held-scale',
 'invparalogis':'32768',
 'invpareto':'128*I','pareto2':'128*I',
 'pareto4':'4096 all-free; caller I held-scale',
 'betapr':'128*I',
 'gb2':'4096 all-free Burr; caller I held-scale Burr; 128*I Lomax; depth-40 trgamma with solver I=100',
 'fpareto':'4096 all-free Burr; caller I held-scale Burr; 128*I Lomax; depth-40 trgamma with solver I=100',
 'zmnbinom':'16*I','bnbinom':'3200'}
for f,parent in parents.items():
 entries.append((f,'certified delegates: '+parent,'certificate evaluation',old_delegate[f],'128*I'))
caprows=[]
for fam,loop,unit,old,new in entries:
 i=default[fam]
 def evaluate(s,control):
  if re.fullmatch(r'[0-9*I]+',s):return str(eval(s,{'__builtins__':{}},{'I':control}))
  return s
 caprows.append(dict(family=fam,loop=loop,unit=unit,old_default_iterlim=old_default[fam],default_iterlim=i,old_cap_at_default=evaluate(old,old_default[fam]),new_cap_at_default=evaluate(new,i),new_formula=new))
for fam in sorted(default):
 if fam=='cmp':continue
 caprows.append(dict(family=fam,loop='local solves, bracketing and backtracking (details in loop inventory)',unit='solver iteration',old_default_iterlim=old_default[fam],default_iterlim=default[fam],old_cap_at_default='I or fixed inner caps (source expressions retained in inventory)',new_cap_at_default=default[fam],new_formula='1*min(I,100000)'))
with (base/'cap-table.csv').open('w') as f:
 writer=csv.DictWriter(f,fieldnames=list(caprows[0]));writer.writeheader();writer.writerows(caprows)
with (base/'protected-loop-inventory.csv').open('w') as f:
 writer=csv.DictWriter(f,fieldnames=list(rows[0]));writer.writeheader();writer.writerows(r for r in rows if '/special/' in r['file'] or r['file'].endswith('/dpqr.hpp'))
(base/'defaults.json').write_text(json.dumps(default,indent=2)+'\n')
print(f'{len(rows)} loop records, {len(caprows)} adaptive cap/delegate rows; {len(default)} iterlim families')
