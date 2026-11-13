from pathlib import Path
import json,subprocess
moves=json.load(open('dev/research/evaluations/2026-11-13-retire-16-data/moves.json'));checks=[]
for old,new in moves.items():
 if old.endswith(('golden.json','truth.json','truth.py')):
  before=subprocess.check_output(['git','show','1e0f5185:'+old]);checks.append((new,before==Path(new).read_bytes()))
assert all(ok for path,ok in checks)
print(len(checks),'frozen golden/truth files preserved byte for byte')
