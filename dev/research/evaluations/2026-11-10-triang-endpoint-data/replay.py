import json,pathlib,subprocess,sys
HERE=pathlib.Path(__file__).resolve().parent
cases=json.loads((HERE/'cases.json').read_text())
lines=[f"{c['index']} {len(c['data'])} "+' '.join(str(c['held'].get(k,'nan')) for k in ['a','b','c'])+' '+' '.join(map(str,c['data'])) for c in cases]
subprocess.run([sys.argv[1]],input='\n'.join(lines)+'\n',text=True,check=True)
