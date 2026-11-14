#!/usr/bin/env python3
"""List changed pre-existing test assertions with their enclosing test and text."""
import difflib,json,re,subprocess
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4]

def assertions(text,js):
 pattern=r'\b(?:assert\.[A-Za-z_]+|comparisonFit)\s*\(' if js else r'\bexpect_[A-Za-z0-9_]+\s*\('
 out=[]
 labels=list(re.finditer(r'(?:test_that|test)\(\s*([\"\'`])(.*?)\1',text))
 for m in re.finditer(pattern,text):
  depth=1;quote=None;escape=False;j=m.end()
  while j<len(text) and depth:
   c=text[j]
   if quote:
    if escape:escape=False
    elif c=='\\':escape=True
    elif c==quote:quote=None
   elif c in '\"\'`':quote=c
   elif c=='(':depth+=1
   elif c==')':depth-=1
   j+=1
  label=next((x.group(2) for x in reversed(labels) if x.start()<m.start()),'(top level)')
  out.append(dict(test=label,text=text[m.start():j],line=text.count('\n',0,m.start())+1))
 return out

def main():
 rows=[]
 files=subprocess.check_output(['git','diff','--name-only'],cwd=ROOT,text=True).splitlines()
 for name in files:
  if not(name.startswith('bindings/') and (name.endswith('/test.R') or '/modifier-tests/' in name or '/tests/testthat/test' in name or name.endswith('-test.mjs') or name=='bindings/js/test.mjs')):continue
  old=subprocess.check_output(['git','show','HEAD:'+name],cwd=ROOT,text=True)
  new=(ROOT/name).read_text();a=assertions(old,name.endswith('mjs'));b=assertions(new,name.endswith('mjs'))
  for label in dict.fromkeys(item['test'] for item in a):
   aa=[r for r in a if r['test']==label];bb=[r for r in b if r['test']==label]
   seq=difflib.SequenceMatcher(a=[r['text'] for r in aa],b=[r['text'] for r in bb],autojunk=False)
   for op,i,j,k,l in seq.get_opcodes():
    if op in ('equal','insert'):continue
    before=aa[i:j];after=bb[k:l]
    def normalized(s):
     s=re.sub(r'fit_payload\((\w+)\)',r'\1',s)
     s=s.replace('conditionMessage(','fit_diagnostic(')
     return re.sub(r'\s+','',s)
    for item in before:
     # Added flag checks can shift an entire replacement block. Match retained
     # numeric/message assertions by their normalized expression, not position.
     exact=[r for r in bb if normalized(r['text'])==normalized(item['text'])]
     pool=exact or after or bb
     replacement=max(pool,key=lambda r:difflib.SequenceMatcher(None,normalized(item['text']),normalized(r['text'])).ratio()) if pool else None
     rows.append(dict(file=name,test=item['test'],old=item['text'],new=None if replacement is None else replacement['text']))
 target=Path(__file__).parent
 (target/'changed-assertions.json').write_text(json.dumps(rows,indent=1)+'\n')
 md=['| File | Test | Old assertion | New assertion |','|---|---|---|---|']
 def cell(s):return (s or '(none)').replace('|','\\|').replace('\n',' ')
 def code(s):
  s=s or '(assertion now covered by the comparison helper)'
  fence='`'*(max([len(x) for x in re.findall(r'`+',s)]+[0])+1)
  return fence+' '+cell(s)+' '+fence
 for r in rows:md.append('| '+cell(r['file'])+' | '+cell(r['test'])+' | '+code(r['old'])+' | '+code(r['new'])+' |')
 (target/'changed-assertions.md').write_text('\n'.join(md)+'\n')
 print(len(rows),'changed pre-existing assertions')
if __name__=='__main__':main()
