"""Run after both validation launchers and the final studies have completed."""
import csv
import hashlib
import json
from pathlib import Path
import subprocess
import zipfile

root=Path.cwd()
data=Path(__file__).resolve().parent
frozen=json.loads((data/'frozen-sources.json').read_text())
checks={}
for name,expected in frozen['sha256'].items():
    assert hashlib.sha256(Path(name).read_bytes()).hexdigest()==expected,name+' changed after freeze'
checks['frozen_sources_match']=True
runs=[]
for name,required in [
    ('local-validation.csv',{'js-build','js-test','site-check',
                             'js-fit','quality','targeted-gev-native-double-final'}),
    ('remote-validation.csv',{'r-dev-current','r-full','cpp-dev','cpp-full','cpp-double'})]:
    rows=list(csv.DictReader((data/name).open()))
    assert required<={r['check'] for r in rows},(name,rows)
    assert all(r['exit']=='0' for r in rows),(name,rows)
    runs.extend(rows)
assert any(r['machine']=='workstation' and r['check'] in
           ['cpp-double','cpp-double-workstation-current'] for r in runs), 'missing final workstation no-long-double run'
for name,size in [('held-before.csv',160),('study-local-100.csv',160),('study-local-1000.csv',160),
                  ('allfree-before.csv',100),('allfree-local-100.csv',100),('allfree-local-1000.csv',100),
                  ('edges-before.csv',6),('edges-after.csv',6),('seam-before.csv',4),('seam-after-rescue.csv',4),('seam-before-1000.csv',4),('seam-after-1000.csv',4),('tail-bound-before-after.csv',2)]:
    assert len(list(csv.DictReader((data/name).open())))==size,name
checks['study_sizes_match']=True
tail=list(csv.DictReader((data/'tail-bound-before-after.csv').open()))
assert [(r['version'],r['excludes']) for r in tail]==[('before','0'),('after','1')]
assert tail[0]['mean_log_density']==tail[1]['mean_log_density']
assert float(tail[1]['mean_log_density'])<float(tail[1]['value_upper'])< -200
checks['tail_bound_regression_before_after_verified']=True
public=list(csv.DictReader((data/'study-local-1000.csv').open()))
open_keys={(r['sample_id'],r['hold']) for r in public if r['family']=='gev' and r['certified']=='0' and r['status']=='0'}
phases=list(csv.DictReader((data/'phase-final-1000.csv').open()))
assert len(phases)==20 and {(r['sample_id'],r['hold']) for r in phases if r['phase']!='complete'}==open_keys
assert all(r['phase'] in ['complete','local_inclusion','global_stagnation','global_budget'] for r in phases)
checks['diagnostic_trace_matches_remaining_open_public_fits']=True
changed=subprocess.check_output(['git','diff','--name-only','-z']).decode().split('\0')
assert {name for name in changed if name}==set(frozen['sha256']),changed
checks['changed_tracked_files_match_snapshot']=True
for name in changed:
    assert name not in ['bindings/r/NEWS.md','dev/STATUS.md','dev/KNOWN_ISSUES.md'],name
    assert not name.startswith(('dev/roadmap/','univariateML-cpp/include/cppunivariate/core/',
                                'univariateML-cpp/include/cppunivariate/special/','bindings/r/scripts/codegen')),name
    assert not name.endswith(('/dpqr.hpp','/golden.json','/truth.py','/truth.json')),name
checks['protected_sources_unchanged']=True
fixtures={}
for family in ['gev','frechet']:
    for name in ['golden.json','dpqr.hpp','truth.py','truth.json']:
        p='univariateML-cpp/include/cppunivariate/distributions/'+family+'/'+name
        original=subprocess.check_output(['git','show','HEAD:'+p])
        assert Path(p).read_bytes()==original,p
        fixtures[p]=hashlib.sha256(original).hexdigest()
    p='univariateML-cpp/include/cppunivariate/distributions/'+family+'/test.cpp'
    original=subprocess.check_output(['git','show','HEAD:'+p]).decode()
    if family=='gev':
        old=' CHECK(cppunivariate::gev_mle(mltest::as_span(centered)).status==cppunivariate::GevStatus::NumericalFailure);'
        new=' const auto centered_fit=cppunivariate::gev_mle(mltest::as_span(centered));\n CHECK(centered_fit.status==cppunivariate::GevStatus::Ok);\n CHECK_FALSE(centered_fit.certified);'
        assert original.count(old)==1
        original=original.replace(old,new)
    assert Path(p).read_text().startswith(original),p+' changed beyond the allowed assertion and append'
checks['cpp_tests_append_only_except_authorized_gev_fallback_assertion']=True
package=Path('univariateML-r')
package_headers={}
headers=['cppunivariate/distributions/gev/mle.hpp','cppunivariate/distributions/gev/held_certificate.hpp',
         'cppunivariate/distributions/gev/joint_certificate.hpp','cppunivariate/distributions/frechet/mle.hpp',
         'cppunivariate/distributions/frechet/certificate.hpp','common/distributions/gev/fit.hpp','common/distributions/frechet/fit.hpp']
for suffix in headers:
    source=Path('univariateML-cpp/include')/suffix if suffix.startswith('cppunivariate/') else Path('bindings')/suffix
    copies=[p for p in package.rglob(Path(suffix).name) if str(p).endswith(suffix)]
    assert copies,'missing generated package header '+suffix
    for p in copies:assert p.read_bytes()==source.read_bytes(),str(p)
    package_headers[str(source)]=[str(p) for p in copies]
checks['generated_package_headers_match']=True
archive={}
with zipfile.ZipFile(data/'before-headers.zip') as z:
    for name in z.namelist():
        original=subprocess.check_output(['git','show','HEAD:univariateML-cpp/include/'+name])
        assert z.read(name)==original,name
        archive[name]=hashlib.sha256(original).hexdigest()
checks['baseline_archive_matches_head']=True
software={}
for name,command in [('cpp',['g++','--version']),('r',['Rscript','--version']),('node',['node','--version'])]:
    software[name]=subprocess.check_output(command,stderr=subprocess.STDOUT).decode().splitlines()[0]
record={'head':frozen['head'],'branch':subprocess.check_output(['git','branch','--show-current']).decode().strip(),
        'checks':checks,'runs':runs,'software':software,'source_sha256':frozen['sha256'],
        'fixture_sha256':fixtures,'baseline_header_sha256':archive,'generated_package_copies':package_headers}
(data/'validation.json').write_text(json.dumps(record,indent=2)+'\n')
print(json.dumps(checks,indent=2))
