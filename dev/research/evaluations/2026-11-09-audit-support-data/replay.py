import json,pathlib,subprocess,sys,os
HERE=pathlib.Path(__file__).resolve().parent
cases=json.loads((HERE/'cases.json').read_text())
if os.environ.get('AUDIT_SUPPORT_FAMILY'):cases=[x for x in cases if x['family']==os.environ['AUDIT_SUPPORT_FAMILY']]
if '--findings' in sys.argv:
 ids={x['index'] for x in json.loads((HERE/'findings.json').read_text())};cases=[x for x in cases if x['index'] in ids]
lines=[]
for c in cases:
 names={'emg':['mu','sigma','lambda'],'triang':['a','b','c'],'gev':['mu','sigma','xi']}[c['family']]
 lines.append(f"{c['family']} {c['index']} {len(c['data'])} "+' '.join(str(c['held'].get(k,'nan')) for k in names)+' '+' '.join(map(str,c['data'])))
subprocess.run([os.environ.get('AUDIT_SUPPORT_BINARY','build/scratch/audit-support-replay')],input='\n'.join(lines)+'\n',text=True,check=True)
