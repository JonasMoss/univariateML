"""Record inputs before and after remote validation, without requiring .git."""
import hashlib,json
from pathlib import Path
paths=['univariateML-cpp/include/cppunivariate/distributions/bnbinom/'+n for n in ('mle.hpp','pair_certificate.hpp','README.md','test.cpp','golden.json','dpqr.hpp','truth.py','truth.json')]+['bindings/common/distributions/bnbinom/fit.hpp','bindings/r/distributions/bnbinom/test.R','bindings/js/test.mjs']
print('SOURCE_FINGERPRINTS '+json.dumps({p:hashlib.sha256(Path(p).read_bytes()).hexdigest() for p in paths},sort_keys=True))
