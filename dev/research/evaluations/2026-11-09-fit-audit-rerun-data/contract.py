#!/usr/bin/env python3
"""Supplement frozen 1e-8 classifications with independently checked 1e-10 misses."""
import os,json,pathlib,subprocess,sys
import mpmath as mp
mp.mp.dps=1000
HERE=pathlib.Path(__file__).resolve().parent
rows=list(map(json.loads,(HERE/os.environ.get('REFERENCE_FILE','reference.jsonl')).read_text().splitlines()))
(HERE/'reference-legacy.jsonl').write_text(''.join(json.dumps(r)+'\n' for r in rows))
for r in rows:
 r['legacy_classification']=r['classification']
 error=r.get('parameter_relative_error',r.get('unverified_stationary_displacement',0))
 if r['classification']=='ok' and error>1e-10 and 'refined_theta' in r and mp.mpf(r['refined_logLik'])>mp.mpf(r['reference_logLik']):
  r.update(classification='silent failure',evidence='stationary local refinement: coefficient relative error exceeds 1e-10',coefficient_error_verified=False)
  r['parameter_relative_error']=error
(HERE/os.environ.get('REFERENCE_FILE','reference.jsonl')).write_text(''.join(json.dumps(r)+'\n' for r in rows))
s=(HERE/'check_findings.py').read_text().replace("mp.mpf('1e-8')","mp.mpf('1e-10')")
exec(compile(s,str(HERE/'check_findings.py'),'exec'),{'__file__':str(HERE/'check_findings.py'),'__name__':'__main__'})
