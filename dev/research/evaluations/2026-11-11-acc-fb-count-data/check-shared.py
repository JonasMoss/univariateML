"""Code identity for unchanged BNB/Skellam captures and settled files."""
import hashlib,json,pathlib,subprocess
P=pathlib.Path(__file__).resolve().parent
paths=subprocess.check_output(['git','ls-files'],text=True).splitlines()
paths=[p for p in paths if (any('/distributions/'+f+'/' in p for f in ['bnbinom','skellam','hyper','nhyper']) or '/core/' in p or '/special/' in p) and p.endswith(('.hpp','.cpp'))]
rows=[]
for p in paths:
 before=subprocess.check_output(['git','show','bc8ff7be:'+p]);after=pathlib.Path(p).read_bytes()
 rows.append(dict(path=p,identical=before==after,sha256=hashlib.sha256(after).hexdigest()))
(P/'unchanged-source.json').write_text(json.dumps(rows,indent=2)+'\n')
assert all(r['identical'] for r in rows)
print(len(rows),'unchanged source files')
