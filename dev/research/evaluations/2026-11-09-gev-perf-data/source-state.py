import subprocess,pathlib,hashlib,json,gzip
root=pathlib.Path.cwd();folder=pathlib.Path(__file__).resolve().parent
changed=subprocess.check_output(['git','diff','--name-only'],text=True).splitlines()
expected=['univariateML-cpp/include/cppunivariate/distributions/gev/'+p for p in ['README.md','joint_certificate.hpp','test.cpp']]
assert sorted(changed)==sorted(expected),changed
manifest={}
for path in expected:
 data=(root/path).read_bytes();old=subprocess.check_output(['git','show','HEAD:'+path])
 manifest[path]={'before_sha256':hashlib.sha256(old).hexdigest(),'after_sha256':hashlib.sha256(data).hexdigest()}
 if path.endswith('test.cpp'):assert data.startswith(old)
 if path.endswith('README.md'):assert data.split(b'\n---\n',1)[0]==old.split(b'\n---\n',1)[0]
 if path.endswith('joint_certificate.hpp'):assert gzip.decompress((folder/'baseline-joint.hpp.gz').read_bytes())==old
fixtures=[]
for family in ['gev','frechet']:
 for filename in ['golden.json','dpqr.hpp','truth.json','truth.py']:
  path=f'univariateML-cpp/include/cppunivariate/distributions/{family}/{filename}'
  data=(root/path).read_bytes();assert data==subprocess.check_output(['git','show','HEAD:'+path])
  fixtures.append({'path':path,'sha256':hashlib.sha256(data).hexdigest()})
result=dict(base=subprocess.check_output(['git','rev-parse','HEAD'],text=True).strip(),changed=manifest,unchanged_fixtures=fixtures,test_append_only=True,front_matter_unchanged=True)
(folder/'source-state.json').write_text(json.dumps(result,indent=2)+'\n')
print('Source boundary, baseline archive, append-only test and eight fixture hashes passed')
