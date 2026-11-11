"""Check the large WASM-selected tbinom reference at 120 working digits."""
import json,pathlib,sys,mpmath as mp
sys.dont_write_bytecode=True
from oracle import solve
P=pathlib.Path(__file__).resolve().parent
case=json.loads((P/'cases.json').read_text())[317]
row=next(r for r in json.loads((P/'wasm-spots.json').read_text()) if r['index']==317)
stored=next(r for r in json.loads((P/'spot-checks.json').read_text())['wasm-spots.json']['rows'] if r['index']==317)
mp.mp.dps=120
point={k:mp.mpf(float(v)) for k,v in row['estimates'].items()}
reference=solve(case,point)
relative=abs(reference['prob']-mp.mpf(stored['reference']['prob']))/abs(reference['prob'])
assert relative<mp.mpf('1e-50')
out=dict(index=317,working_digits=120,relative_difference=str(relative),reference={k:str(v) for k,v in reference.items()},at_least_50_relative_digits_agree=True)
(P/'reference-stability.json').write_text(json.dumps(out,indent=2)+'\n')
print(json.dumps(out,indent=2))
