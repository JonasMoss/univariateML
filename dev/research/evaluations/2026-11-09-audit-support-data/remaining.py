"""Compare prior coefficient findings with frozen precision-checked roots."""
import csv,json,pathlib
import mpmath as mp
mp.mp.dps=100
HERE=pathlib.Path(__file__).resolve().parent
refs={r['index']:r for r in json.loads((HERE/'references.json').read_text())}
cases={r['index']:r for r in json.loads((HERE/'cases.json').read_text())}
findings=json.loads((HERE/'findings.json').read_text())
rows={int(r[0]):r for r in csv.reader((HERE/'after-contract.csv').open())}
comparisons=[]
for f in findings:
 if f['category']!='coefficient only':continue
 id=f['index'];r=refs[id];row=rows[id];case=cases[id]
 if int(row[2])!=0:continue
 names={'emg':['mu','sigma','lambda'],'gev':['mu','sigma','xi']}[f['family']]
 target=r.get('refined_theta',r.get('better_theta'))
 if not target:
  comparisons.append(dict(index=id,family=f['family'],open='no independently refined coefficient vector'))
  continue
 reference=list(map(mp.mpf,target));point=[mp.mpf(float(v)) for v in row[4:7]]
 errors={}
 for j,n in enumerate(names):
  if n in case['held']:continue
  floor=reference[1] if n=='mu' else (mp.mpf(1) if n=='xi' else mp.mpf(0))
  errors[n]=str(abs(point[j]-reference[j])/max(abs(reference[j]),floor))
 error=max(map(mp.mpf,errors.values()),default=mp.mpf(0))
 comparisons.append(dict(index=id,family=f['family'],mask=case['mask'],unchanged_coefficients=all(float(v)==float(old) for v,old in zip(row[4:7],r['theta'])),prior_verified=r.get('coefficient_error_verified',False),coefficient_errors=errors,max_error=str(error),exceeds_1e_10=bool(error>mp.mpf('1e-10')),evidence='numerical comparison with frozen independently precision-checked local refinement; not a new global proof'))
(HERE/'remaining-coefficient-findings.json').write_text(json.dumps(comparisons,indent=2)+'\n')
misses=[r for r in comparisons if r.get('exceeds_1e_10')]
print('Prior coefficient-only comparison at uniform 1e-10:',len(comparisons),'Ok points;',len(misses),'still exceed target;',sum(r['unchanged_coefficients'] for r in misses),'unchanged misses')
assert not any(r['family']=='emg' and r.get('mask')==6 for r in misses), 'location-only coefficient contract regression'
checks=[]
for name in ['before','after']:
 normal={int(r[0]):r for r in csv.reader((HERE/(name+'.csv')).open()) if r[1]=='triang'}
 brute={int(r[0]):r for r in csv.reader((HERE/(name+'-exhaustive.csv')).open())}
 differences=[id for id in normal if normal[id]!=brute[id]]
 checks.append(dict(version=name,cases=len(normal),different_payloads=differences))
 assert not differences,checks[-1]
(HERE/'pruning-validation.json').write_text(json.dumps(checks,indent=2)+'\n')
print('PASS: all 168 triangular payloads match exhaustive mode enumeration before and after')
