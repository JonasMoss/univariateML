#!/usr/bin/env python3
"""Give numerical oracle gaps a real 20-second second attempt."""
import os,json,pathlib
HERE=pathlib.Path(__file__).resolve().parent
rows=list(map(json.loads,(HERE/os.environ.get('REFERENCE_FILE','reference.jsonl')).read_text().splitlines()))
retry=[r for r in rows if r['classification']=='open' and r['status'] not in [-997,-998,-999]]
(HERE/'reference-first-pass.jsonl').write_text(''.join(json.dumps(r)+'\n' for r in rows))
(HERE/'reference-retry-indices.json').write_text(json.dumps([r['index'] for r in retry]))
indices={r['index'] for r in retry}
(HERE/os.environ.get('REFERENCE_FILE','reference.jsonl')).write_text(''.join(json.dumps(r)+'\n' for r in rows if r['index'] not in indices))
print('Retrying',len(indices),'numerical reference gaps at 20 seconds',flush=True)
