import csv,json,math,pathlib,sys
root=pathlib.Path(__file__).resolve().parent
refs=json.loads((root/'edge-oracle.json').read_text())
rows=list(csv.reader(pathlib.Path(sys.argv[1]).open()))
errors=[]
assert len(rows)==len(refs)
for got,ref in zip(rows,refs):
    assert float(got[0])==ref['nu'] and float(got[1])==ref['x']
    for actual,key in zip(got[2:],['log_density','log_upper']):
        a=float(actual);b=float(ref[key])
        error=0 if a==b else abs(a-b)/max(1,abs(b))
        assert math.isfinite(error) and error<1e-12,(got,key,b,error)
        errors.append(error)
print(json.dumps(dict(checks=len(errors),max_error=max(errors)),indent=2))
