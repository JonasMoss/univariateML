"""Compare probe.csv with the independent mpmath oracle; no fixture writes."""
import csv,json,pathlib,sys,math
root=pathlib.Path(__file__).resolve().parent
oracle=json.loads((root/'oracle.json').read_text())
lookup={}
for r in oracle:
    nu=float(r['nu'])
    if 'qged' in r: lookup[(nu,'ged',float(r['p']))]=float(r['qged'])
    if 'qsstd' in r: lookup[(nu,'sstd',float(r['p']))]=float(r['qsstd'])
    if 'log_gamma_upper' in r: lookup[(nu,'gamma-upper',float(r['x']))]=float(r['log_gamma_upper'])
    if 'qsged' in r: lookup[(nu,'sged',float(r['p']))]=float(r['qsged'])
    if 'density_t_at_1' in r: lookup[(nu,'lst-density',1.)]=float(r['density_t_at_1'])
errors=[]
for ns,f,ps,vs in csv.reader(pathlib.Path(sys.argv[1]).open()):
    key=(float(ns),f,float(ps)); got=float(vs)
    if key not in lookup: continue
    target=lookup[key]
    error=abs(got-target)/max(1,abs(target))
    errors.append(dict(nu=ns,family=f,p=ps,actual=got,reference=target,error=error))
    assert math.isfinite(got) and error<1e-12,(key,got,target)
print(json.dumps(dict(checks=len(errors),max_error=max(r['error'] for r in errors),results=errors),indent=2))
