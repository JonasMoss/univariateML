"""Compress the large replay ledgers deterministically; retain small review tables."""
from pathlib import Path
import gzip,shutil
p=Path(__file__).resolve().parent
for name in ['cases.json','results.json','differences.csv','tbinom-default-after.json','frozen-audit.json']:
 src=p/name
 if not src.exists():continue
 with src.open('rb') as inp,(p/(name+'.gz')).open('wb') as out:
  with gzip.GzipFile(filename='',mode='wb',fileobj=out,mtime=0) as zipped:shutil.copyfileobj(inp,zipped)
 src.unlink()
print('Replay ledgers compressed')
