from pathlib import Path
import importlib.util,types,json
root=Path.cwd();path=root/'dev/validation/fit-snapshot.py'
spec=importlib.util.spec_from_file_location('snapshot',path);m=importlib.util.module_from_spec(spec);spec.loader.exec_module(m)
m.ROOT=root/'build/scratch/starting-tree';m.DISTS=m.ROOT/'univariateML-cpp/include/cppunivariate/distributions';m.COMMON=m.ROOT/'bindings/common'
args=types.SimpleNamespace(output=root/'build/scratch/starting-emg-double',no_long_double=True,cpu_limit=300)
args.output.mkdir(exist_ok=True);work=args.output/'build';work.mkdir(exist_ok=True)
a={}
for c in json.loads(m.gzip.decompress(m.ARCHIVE.read_bytes())):a.setdefault(c['family'],[]).append(c)
print(m.run_family('emg',args,a,work,m.boost_include(),'# starting commit 1e0f5185 double'),flush=True)
