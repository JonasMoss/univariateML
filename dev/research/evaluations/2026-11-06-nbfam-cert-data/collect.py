"""Collect completed, green final runs. Does not write the requested report."""
import collections,datetime,glob,hashlib,json,os,platform,re,statistics,subprocess
from pathlib import Path
base=Path(__file__).resolve().parent
root=base.parents[3]
os.chdir(root)
env=os.environ.copy();env['GIT_OPTIONAL_LOCKS']='0'
def git(*args):return subprocess.check_output(['git',*args],text=True,env=env).strip()
stages=['cpp-dev','cpp-no-long-double','cpp-full','r-dev','r-full','js-build','js-test','site-check']
runs=[]
for name in stages:
    item=json.loads((base/(name+'.json')).read_text())
    assert item['exit']==0,(name,item)
    raw=(root/'build/scratch'/f'{name}.log').read_text(errors='replace')
    last=raw.rfind('\nCOMMAND ')
    if last>=0:raw=raw[last+1:]
    lines=raw.splitlines()
    cleaned=[]
    for line in lines:
        line=re.sub(r'\x1b\[[0-9;]*[A-Za-z]','',line)
        if line.strip():cleaned.append(line)
    (base/(name+'.txt')).write_text('\n'.join(cleaned[:2]+cleaned[-60:])+'\n')
    item['stage']=name
    item['log_excerpt']=name+'.txt'
    item['full_log_sha256']=hashlib.sha256(raw.encode()).hexdigest()
    if name.startswith('r-'):
        matches=re.findall(r'\[ FAIL (\d+) \| WARN (\d+) \| SKIP (\d+) \| PASS (\d+) \]',raw)
        assert matches,name
        fail,warn,skip,passed=map(int,matches[-1]);assert fail==warn==0
        item['counts']=dict(fail=fail,warn=warn,skip=skip,passed=passed)
    elif name.startswith('cpp-'):
        matches=re.findall(r'100% tests passed out of (\d+)',raw);assert matches,name
        item['counts']=dict(fail=0,passed=int(matches[-1]))
    elif name=='js-test':
        item['counts']={k:int(v) for k,v in re.findall(r'^# (tests|pass|fail|cancelled|skipped|todo) (\d+)\s*$',raw,re.M)}
        assert item['counts']['fail']==0,item['counts']
    runs.append(item)
# The entire uncertified family stays unchanged; no fixtures are regenerated.
unchanged=['univariateML-cpp/include/cppunivariate/distributions/zinb','bindings/common/distributions/zinb','bindings/r/distributions/zinb']
unchanged += [f'univariateML-cpp/include/cppunivariate/distributions/{f}/{p}' for f in ['ztnbinom','zmnbinom','zinb'] for p in ['golden.json','dpqr.hpp','truth.py','truth.json']]
assert not git('diff','--name-only','--',*unchanged)
source=[]
for fam in ['ztnbinom','zmnbinom','zinb']:
    folder=root/'univariateML-cpp/include/cppunivariate/distributions'/fam
    source.extend(p for p in folder.iterdir() if p.is_file())
    for prefix in ['bindings/common/distributions','bindings/r/distributions']:
        source.extend(p for p in (root/prefix/fam).iterdir() if p.is_file())
source.extend(p for p in base.iterdir() if p.suffix in ['.py','.cpp'])
oracle=json.loads((base/'oracle.json').read_text())
timing=collections.defaultdict(list)
for line in (base/'timing.txt').read_text().splitlines():
    fam,mode,n,elapsed,unit=line.split();assert unit=='us/fit' and int(n)==500
    timing[fam+'/'+mode].append(float(elapsed))
assert all(len(v)==3 for v in timing.values())
versions={}
for pkg in ['Rcpp','BH']:
    desc=glob.glob(f'/home/jonas/R/*/4.6/{pkg}/DESCRIPTION')[0]
    versions[pkg]=re.search(r'^Version:\s*(.*)$',Path(desc).read_text(),re.M)[1]
summary=dict(base_revision=git('rev-parse','HEAD'),branch=git('branch','--show-current'),
             recorded_utc=datetime.datetime.now(datetime.timezone.utc).isoformat(),
             environment=dict(platform=platform.platform(),compiler=subprocess.check_output(['c++','--version'],text=True).splitlines()[0],
                              R=subprocess.check_output(['R','--version'],text=True).splitlines()[0],python=platform.python_version(),
                              node=subprocess.check_output(['node','--version'],text=True).strip(),
                              emscripten=Path('/home/jonas/emsdk/upstream/emscripten/emscripten-version.txt').read_text().strip().strip('"'),
                              **versions,numpy=oracle['numpy'],mpmath=oracle['mpmath']),
             runs=runs,oracle=dict(seed=oracle['seed'],precision=oracle['precision'],cases=oracle['cases'],
                                  statuses=dict(collections.Counter(str(v['status']) for v in oracle['results'])),
                                  retry_statuses=dict(collections.Counter(str(v['status']) for v in oracle['retries'])),
                                  interval_checks=oracle['interval_checks'],max_errors=oracle['max_errors']),
             timing=dict(repeats_per_batch=100,batches=3,n=500,median_us={k:statistics.median(v) for k,v in timing.items()}),
             unchanged=unchanged,no_commits=True,no_board_edits=True,no_package_installs=True)
(base/'validation.json').write_text(json.dumps(summary,indent=2)+'\n')
# Include source and retained output evidence, omitting this manifest and the report.
source.extend(p for p in base.iterdir() if p.is_file() and p.name!='source-sha256.txt')
manifest=''.join(f'{hashlib.sha256(p.read_bytes()).hexdigest()}  {p.relative_to(root)}\n' for p in sorted(set(source)))
(base/'source-sha256.txt').write_text(manifest)
print(json.dumps(dict(manifest_sha256=hashlib.sha256(manifest.encode()).hexdigest(),**summary),indent=2))
