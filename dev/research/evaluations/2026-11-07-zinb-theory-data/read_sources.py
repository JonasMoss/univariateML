"""Acquire primary sources to ignored scratch; retain only metadata/digests."""
from pathlib import Path
import urllib.request,hashlib,json,subprocess,shutil
p=Path('build/scratch/literature');p.mkdir(parents=True,exist_ok=True)
urls={'aragon':'https://stacks.stanford.edu/file/druid:gy645ty0351/gy645ty0351_OLK-NSF-282.pdf','sampford-891':'https://amsr.contentdm.oclc.org/digital/api/collection/p21050coll5/id/891/download','sampford-892':'https://amsr.contentdm.oclc.org/digital/api/collection/p21050coll5/id/892/download'}
records=[]
for name,url in urls.items():
 with urllib.request.urlopen(url,timeout=30) as r:data=r.read()
 assert data.startswith(b'%PDF')
 pdf=p/(name+'.pdf');pdf.write_bytes(data)
 records.append(dict(name=name,url=url,bytes=len(data),sha256=hashlib.sha256(data).hexdigest()))
 if shutil.which('pdftoppm'):
  args=['pdftoppm','-scale-to','1500','-png']
  if name=='aragon':args+=['-f','3','-l','5']
  subprocess.run(args+[str(pdf),str(p/name)],check=True)
Path(__file__).with_name('source-records.json').write_text(json.dumps(records,indent=2)+'\n')
print(json.dumps(dict(sources=records,pdftoppm=shutil.which('pdftoppm'),images=[str(f) for f in p.glob('*.png')]),indent=2))
