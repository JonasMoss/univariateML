import hashlib,json,subprocess
from pathlib import Path
out=Path(__file__).parent
family=Path('univariateML-cpp/include/cppunivariate/distributions/bnbinom')
paths=[family/n for n in ('mle.hpp','pair_certificate.hpp','face_certificate.hpp','alpha_interval_certificate.hpp','joint_certificate.hpp','README.md','test.cpp','golden.json','dpqr.hpp','truth.py','truth.json')]+[Path('bindings/common/distributions/bnbinom/fit.hpp'),Path('bindings/r/distributions/bnbinom/test.R'),Path('bindings/r/distributions/bnbinom/adapter.cpp'),Path('bindings/js/test.mjs')]
current={str(p):hashlib.sha256(p.read_bytes()).hexdigest() for p in paths}
if Path('.git').exists():
    base=subprocess.check_output(['git','rev-parse','HEAD'],text=True).strip()
    for p in [family/n for n in ('golden.json','dpqr.hpp','truth.py','truth.json')]:
        assert p.read_bytes()==subprocess.check_output(['git','show',f'HEAD:{p}']),p
    for p in [family/'test.cpp',Path('bindings/r/distributions/bnbinom/test.R'),Path('bindings/js/test.mjs')]:
        original=subprocess.check_output(['git','show',f'HEAD:{p}'])
        assert p.read_bytes().startswith(original),p
    subprocess.run(['git','diff','--check'],check=True)
    fingerprints=dict(base=base,sha256=current)
    (out/'source.json').write_text(json.dumps(fingerprints,indent=2)+'\n')
    print('Frozen inputs, append-only original tests and whitespace: PASS')
else:
    fingerprints=json.loads((out/'source.json').read_text())
    assert current==fingerprints['sha256'],'Remote source mismatch'
    print('Remote source fingerprints: PASS')
print(json.dumps(fingerprints))
