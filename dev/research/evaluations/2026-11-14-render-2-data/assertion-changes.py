#!/usr/bin/env python3
"""List every changed test assertion of the second pass (render-2), relative to
the first pass (commit b230aaa8), as file / enclosing test / old / new.

Usage: assertion-changes.py [--base b230aaa8] [--json FILE] [--md FILE]
Reads `git diff -U0 BASE -- <test files>` of the working tree; no test is
deleted (the script fails if a removed line has no replacement in its hunk).
"""
import argparse,json,re,subprocess
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4]
FILES=['bindings/r/distributions','bindings/r/modifier-tests','bindings/r/tests/testthat','bindings/js/test.mjs','bindings/js/stop-test.mjs','bindings/js/fit-test.mjs']
def enclosing(path,line):
 text=(ROOT/path).read_text().splitlines()
 for i in range(min(line,len(text))-1,-1,-1):
  m=re.match(r"\s*(?:test_that|test)\(\s*['\"](.*?)['\"]",text[i])
  if m:return m.group(1)
 return ''
def main():
 ap=argparse.ArgumentParser();ap.add_argument('--base',default='b230aaa8');ap.add_argument('--json');ap.add_argument('--md');a=ap.parse_args()
 diff=subprocess.run(['git','diff','-U0',a.base,'--',*FILES],cwd=ROOT,capture_output=True,text=True,check=True).stdout
 rows=[];path=None
 for hunk in re.split(r'\n(?=@@|diff --git)',diff):
  if hunk.startswith('diff --git'):
   path=re.search(r' b/(\S+)',hunk).group(1);continue
  m=re.match(r'@@ -\d+(?:,\d+)? \+(\d+)(?:,\d+)? @@',hunk)
  if not m or path is None:continue
  old=[l[1:] for l in hunk.splitlines()[1:] if l.startswith('-')]
  new=[l[1:] for l in hunk.splitlines()[1:] if l.startswith('+')]
  assert new or not old,'a removed test line has no replacement: %s:%s'%(path,m.group(1))
  rows.append(dict(file=path,test=enclosing(path,int(m.group(1))),old='\n'.join(old).strip(),new='\n'.join(new).strip()))
 if a.json:Path(a.json).write_text(json.dumps(rows,indent=1)+'\n')
 if a.md:
  cell=lambda s:('` '+s.replace('|','\\|').replace('\n',' ')+' `') if s else '(added)'
  out=['| File | Test | Old | New |','|---|---|---|---|']+['| %s | %s | %s | %s |'%(r['file'],r['test'].replace('|','\\|'),cell(r['old']),cell(r['new'])) for r in rows]
  Path(a.md).write_text('\n'.join(out)+'\n')
 print(len(rows),'changed hunks in',len({r['file'] for r in rows}),'files')
if __name__=='__main__':main()
