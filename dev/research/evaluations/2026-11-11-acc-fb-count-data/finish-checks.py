"""Serial, one-slot binding/reference and source-preservation checks."""
import datetime,json,os,pathlib,subprocess,time
P=pathlib.Path(__file__).resolve().parent
LOG=pathlib.Path('build/scratch/acc-fb-count-logs')
LOG.mkdir(parents=True,exist_ok=True)
os.environ['MAKEFLAGS']='-j1'
checks=[('r-spots',['Rscript',str(P/'spot.R')]),
        ('wasm-spots',['node','--v8-pool-size=1',str(P/'spot.mjs')]),
        ('binding-references',['python3',str(P/'check-spots.py')]),
        ('assessment',['python3',str(P/'assess.py')]),
        ('unchanged-source',['python3',str(P/'check-shared.py')])]
rows={}
for name,command in checks:
    start=time.monotonic();print(name,'START',flush=True)
    with (LOG/f'{name}.log').open('w') as stream:
        r=subprocess.run(command,stdout=stream,stderr=subprocess.STDOUT)
    rows[name]=dict(command=command,machine='workstation',exit_code=r.returncode,
                    seconds=time.monotonic()-start,finished=datetime.datetime.now(datetime.timezone.utc).isoformat())
    (P/'binding-checks.json').write_text(json.dumps(rows,indent=2)+'\n')
    print(name,'END',r.returncode,flush=True)
    if r.returncode:raise SystemExit(r.returncode)
