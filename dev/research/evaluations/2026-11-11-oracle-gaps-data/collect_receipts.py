"""Keep small final validation receipts beside the report (not build products)."""
import hashlib,json,re,shutil,subprocess
from pathlib import Path
HERE=Path(__file__).resolve().parent;ROOT=HERE.parents[3];LOG=ROOT/'build/scratch/oracle-final'
files=['python-shared.log','python-baseline.log','python-gate.log','verify-tables.log','receipts.txt']
files += [p.name for p in LOG.glob('*-local.log')]+[p.name for p in LOG.glob('*-simbox.log')]
files += [p.name for p in LOG.glob('dgamma-*.log')]
files = list(dict.fromkeys(files))
receipts=[]
for name in files:
 path=LOG/name
 if path.exists():
  text=path.read_text(errors='replace');tail='\n'.join(text.splitlines()[-60:])+'\n'
  receipts.append(dict(file=name,sha256=hashlib.sha256(path.read_bytes()).hexdigest(),bytes=path.stat().st_size,tail=tail,assertion_totals=(re.findall(r'\[ FAIL \d+ \| WARN \d+ \| SKIP \d+ \| PASS \d+ \]',text) or [None])[-1]))
(HERE/'validation-receipts.json').write_text(json.dumps(receipts,indent=2)+'\n')
# Keep the complete short mathematical and baseline receipts.
for name in ('python-baseline.log','python-gate.log','verify-tables.log','receipts.txt'):
 if (LOG/name).exists():shutil.copyfile(LOG/name,HERE/name)
print('Validation receipts:',len(receipts))
