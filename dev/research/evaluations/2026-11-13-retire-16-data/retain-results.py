#!/usr/bin/env python3
from pathlib import Path
import gzip,json,shutil,subprocess
HERE=Path(__file__).resolve().parent;ROOT=HERE.parents[3];scratch=ROOT/'build/scratch'
for mode in ['native','double']:
 source=scratch/('routes-after-'+mode)
 shutil.copy2(source/'snapshot.jsonl.gz',HERE/('modifier-routes-after-'+mode+'.jsonl.gz'))
 shutil.copy2(source/'provenance.json',HERE/('modifier-routes-after-'+mode+'.provenance.json'))
 source=scratch/('survivors-'+mode)
 shutil.copy2(source/'provenance.json',HERE/('survivors-'+mode+'.provenance.json'))
part=(scratch/'starting-emg-double/emg.jsonl').read_text().split('\n',1)[1]
with gzip.open(HERE/'starting-emg-double.jsonl.gz','wt') as f:f.write(part)
for name in ['final-snapshots.log','starting-emg-baseline-diff.log','starting-emg-retirement-diff.log','final-relations.log','final-links.log','kernel-text-check.log']:
 shutil.copy2(scratch/name,HERE/name.replace('.log','.txt'))
archive=json.loads(gzip.decompress((ROOT/'dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz').read_bytes()))
inputs=[c for c in archive if c['index'] in [1311,7339,7366]]
(HERE/'open-case-inputs.json').write_text(json.dumps(inputs,indent=2)+'\n')
provenance={'commit':subprocess.check_output(['git','rev-parse','1e0f5185'],cwd=ROOT,text=True).strip(),
 'compiler':subprocess.check_output(['c++','--version'],text=True).splitlines()[0],
 'flags':['-std=gnu++17','-O2','-DNDEBUG','-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'],
 'family':'emg','cases':212,'cpu_limit':300,'comparison_to_retired_tree':0,
 'comparison_to_supplied_older_baseline':1,'different_case':'archive 001311'}
(HERE/'starting-emg-double.provenance.json').write_text(json.dumps(provenance,indent=2)+'\n')
