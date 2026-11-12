"""Recheck saved returned coefficients using exact binary64 support comparisons."""
import gzip,importlib.util,json,sys
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4]
sys.path.insert(0,str(ROOT/'dev/validation'))
spec=importlib.util.spec_from_file_location('seam',ROOT/'dev/validation/seam-check.py')
seam=importlib.util.module_from_spec(spec);spec.loader.exec_module(seam)
source=Path(sys.argv[1])
text=gzip.decompress(source.read_bytes()).decode() if source.suffix=='.gz' else source.read_text()
rows=[json.loads(l) for l in text.splitlines()]
checked=0;violations=[];opens=[]
for r in rows:
    if 'parameters' not in r:continue
    try:
        meta,_=seam.read(seam.DISTS/r['family'])
        bad=seam.support_violations(meta,r['parameters'],r['data'])
        checked+=1
        if bad:violations.append(dict(family=r['family'],index=r['index'],source=r['source'],outside=bad))
    except Exception as e:opens.append(dict(family=r['family'],index=r['index'],reason=repr(e)))
out=dict(checked=checked,violations=violations,opens=opens)
Path(sys.argv[2]).write_text(json.dumps(out,indent=2)+'\n')
print(out)
