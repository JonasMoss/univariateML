"""Compress finished evidence files over 200 kB; run only after all writers exit."""
import gzip,pathlib,shutil
D=pathlib.Path(__file__).resolve().parent
for path in sorted(D.iterdir()):
 if path.is_file() and path.suffix!='.gz' and path.stat().st_size>200000:
  with path.open('rb') as source,gzip.open(str(path)+'.gz','wb') as target:
   shutil.copyfileobj(source,target)
  path.unlink()
  print(path.name+'.gz')
