from pathlib import Path
import gzip
root=Path('dev/research/evaluations/2026-11-13-qtie-data')
for name in ['final-cpp-test','final-cpp-test-no-long-double','final-test','targeted-r','final-test-full','final-js-build','initial-js-test','targeted-js','final-js-test']:
 p=Path('build/scratch')/(name+'.log')
 if p.exists() and p.stat().st_size:
  label='initial-test-dev' if name=='final-test' else name
  (root/(label+'.log.gz')).write_bytes(gzip.compress(p.read_bytes(),mtime=0))
