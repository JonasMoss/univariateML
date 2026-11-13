#!/usr/bin/env python3
"""Assert retained kernel text is unchanged apart from includes and removed entry points."""
from pathlib import Path
import json,re,subprocess
HERE=Path(__file__).resolve().parent
ROOT=HERE.parents[3]
moves=json.loads((HERE/'moves.json').read_text());checked=[]
def body(text):return re.sub(r'^#include[^\n]*\n','',text,flags=re.M).strip()
for old,new in moves.items():
 if not old.endswith('.hpp'):continue
 original=subprocess.check_output(['git','show','1e0f5185:'+old],cwd=ROOT,text=True)
 actual=(ROOT/new).read_text()
 if old.endswith('zipf/mle.hpp'):
  original=original[:original.index('enum class ZipfStatus')]+original[original.index('namespace zipf_detail'):original.index('inline ZipfResult zipf_mle')]+'} // namespace cppunivariate\n'
 if old.endswith('/tbinom/fit.hpp'):
  original=original[:original.index('inline FitResult fit_tbinom(')]+'}\n'
 assert body(original)==body(actual),(old,new)
 checked.append({'old':old,'new':new})
(HERE/'kernel-text-check.json').write_text(json.dumps({'baseline':'1e0f5185','headers':checked,'differences':0},indent=2)+'\n')
print(len(checked),'retained kernel headers match after removing include lines and the two obsolete entry points')
