"""Replay the large-shape discrete-gamma witness as a fully held model."""
import json,math,subprocess
from pathlib import Path
here=Path(__file__).resolve().parent;root=here.parents[3]
reference=next(r for r in json.loads((here/'density-references.json').read_text()) if r['family']=='dgamma')
expected=math.fsum(reference['expected']);out=[]
for label in ['before','final']:
 provenance=json.loads((here/(label+'.provenance.json')).read_text())
 exe=root/'build/scratch/seam-check'/('probe-53f7912622be-'+provenance['source_sha256'][:12])
 text=' '.join(map(str,['dgamma',3,2,len(reference['data']),*reference['parameters'],*reference['data']]))+'\n'
 cells=subprocess.check_output([str(exe)],input=text,text=True).strip().split('\t');assert cells[0]=='ok',cells
 density=math.fsum(map(float,cells[4:]));out.append(dict(mode=label,reported=float(cells[1]),density=density,expected=expected,absolute_error=abs(density-expected)))
(here/'gamma-witness.json').write_text(json.dumps(out,indent=2)+'\n');print(out);assert out[-1]['absolute_error']<1e-10
