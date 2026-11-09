#!/usr/bin/env python3
"""Four single-thread reference workers; invoke under run4 only."""
import json,os,pathlib,subprocess,sys
HERE=pathlib.Path(__file__).resolve().parent
rows=list(map(json.loads,(HERE/'reference.jsonl').read_text().splitlines()))
workers=[]
for shard in range(4):
 (HERE/f'reference-{shard}.jsonl').write_text(''.join(json.dumps(r)+'\n' for r in rows if r['index']%4==shard))
 log=open(HERE.parents[3]/'build/scratch/fit-audit'/f'reference-{shard}.log','w')
 workers.append((subprocess.Popen([sys.executable,str(HERE/'audit.py'),'reference'],env={**os.environ,'AUDIT_SHARDS':'4','AUDIT_SHARD':str(shard),'REFERENCE_FILE':f'reference-{shard}.jsonl','REFERENCE_SECONDS':os.environ.get('REFERENCE_SECONDS','2')},stdout=log,stderr=subprocess.STDOUT),log))
failed=[]
for process,log in workers:
 if process.wait():failed.append(process.returncode)
 log.close()
assert not failed,failed
subprocess.run([sys.executable,str(HERE/'audit.py'),'merge'],check=True)
