#!/usr/bin/env python3
"""Keep the final snapshots, provenance and completed validation logs compactly."""
from pathlib import Path
import gzip,json,shutil,hashlib,subprocess
ROOT=Path(__file__).resolve().parents[4];D=Path(__file__).parent
for mode in ['native','double']:
 source=ROOT/'build/snapshots'/mode
 with gzip.GzipFile(filename=str(D/(mode+'.jsonl.gz')),mode='wb',mtime=0) as target:
  for part in sorted(source.glob('*.jsonl')):
   lines=part.read_bytes().splitlines(keepends=True)
   for line in lines[1:]:target.write(line)
 shutil.copyfile(source/'provenance.json',D/(mode+'.provenance.json'))
logs=['final-suites','final-cpp-test','final-cpp-test-no-long-double','final-test','final-test-full','final-js-build','final-js-test','final-site-check','final-strict','final-seam','final-fit-judge','final-fit-density','final-fit-consistency','final-fit-refine','final-outcomes-native','final-outcomes-double','final-diff-native','final-diff-double','final-snapshot-native','final-snapshot-double','final-reference-checks','final-check-named-faces','final-check-refutations','final-check-boundaries','final-known-inventory','core-iteration']
logs += ['final-fpareto-native','final-fpareto-double','final-rechecks','final-check-sstd-refutation','final-sstd-r','final-cpp-families']
logs += ['final-sstd-native']
logs += ['final-classifications','final-inventory']
for label in logs:
 source=ROOT/'build/scratch'/(label+'.log')
 if not source.exists():raise RuntimeError('Missing final log '+str(source))
 content=source.read_bytes();dest=D/(label+'.log')
 if len(content)>200000:
  with gzip.GzipFile(filename=str(dest)+'.gz',mode='wb',mtime=0) as target:target.write(content)
  if dest.exists():dest.unlink()
 else:dest.write_bytes(content)
files=subprocess.check_output(['git','diff','--name-only'],cwd=ROOT,text=True).splitlines()
manifest={'baseline_commit':subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip(),'machine':'workstation','source_sha256':{f:hashlib.sha256((ROOT/f).read_bytes()).hexdigest() for f in files if (ROOT/f).is_file() and not f.startswith('dev/research/evaluations/2026-11-14-core5b-a-')}}
(D/'source-manifest.json').write_text(json.dumps(manifest,indent=2,sort_keys=True)+'\n')
print('Saved final snapshots, provenance, logs and source hashes.')
