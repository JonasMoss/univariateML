"""Give every baseline audit timeout a fresh 120-second current-code replay."""
import gzip,json,subprocess,sys
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4];HERE=Path(__file__).resolve().parent
source=HERE/'before.jsonl'
text=source.read_text() if source.exists() else gzip.decompress(source.with_suffix('.jsonl.gz').read_bytes()).decode()
rows=[json.loads(l) for l in text.splitlines()]
opens=[r for r in rows if r['outcome']=='open fit']
args=[sys.executable,str(ROOT/'dev/validation/seam-check.py'),'--family',','.join(sorted({r['family'] for r in opens})),'--timeout','120','--output',str(HERE/'retry-120.jsonl')]
for r in opens:args+=['--case',str(r['index'])]
subprocess.run(args,check=True)
