"""Create an EMG-only instrumented include overlay; production headers stay clean."""
from pathlib import Path
import shutil, sys
source, target = map(Path, sys.argv[1:])
root = target / 'cppunivariate'
(root / 'distributions').mkdir(parents=True, exist_ok=True)
base = Path('univariateML-cpp/include/cppunivariate').resolve()
for p in base.iterdir():
    if p.name != 'distributions' and not (root/p.name).exists():
        (root/p.name).symlink_to(p, target_is_directory=p.is_dir())
for p in (base/'distributions').iterdir():
    if p.name != 'emg' and not (root/'distributions'/p.name).exists():
        (root/'distributions'/p.name).symlink_to(p, target_is_directory=p.is_dir())
folder=root/'distributions/emg'
shutil.copytree(source,folder,dirs_exist_ok=True)
p=folder/'certificate_math.hpp';s=p.read_text()
s=s.replace('struct CertModel {', '''inline unsigned long long perf_evaluations=0,perf_hessians=0,perf_newton_calls=0,perf_newton_steps=0,perf_fallbacks=0,perf_depth=0;
struct CertModel {''')
s=s.replace('inline CertEvaluation cert_evaluate(const CertModel& model,const std::array<double,3>& p) {','inline CertEvaluation cert_evaluate(const CertModel& model,const std::array<double,3>& p) {\n  ++perf_evaluations;')
s=s.replace('inline std::array<std::array<CertInterval,3>,3> cert_hessian(const CertModel& model,const std::array<CertInterval,3>& p) {','inline std::array<std::array<CertInterval,3>,3> cert_hessian(const CertModel& model,const std::array<CertInterval,3>& p) {\n  ++perf_hessians;')
s=s.replace('  CertRoot last;','  ++perf_newton_calls;\n  CertRoot last;')
start=s.index('inline CertRoot cert_newton(')
s=s[:start]+s[start:].replace('for(int it=0;it<solver_budget(maxit);++it) {','for(int it=0;it<solver_budget(maxit);++it) {\n    ++perf_newton_steps;',1)
p.write_text(s)
p=folder/'fixed_shape_certificate.hpp';s=p.read_text().replace('  const double fixed_b=', '  ++perf_fallbacks;\n  const double fixed_b=');p.write_text(s)
p=folder/'certificate.hpp';s=p.read_text().replace('double lo,hi,left,right,upper;', 'double lo,hi,left,right,upper; unsigned long long depth=0;')
s=s.replace('  span x;double tol,mu,sigma,lambda;int maxit;', '  unsigned long long perf_parent_depth=0;\n  span x;double tol,mu,sigma,lambda;int maxit;')
s=s.replace('queue.push({lo,hi,left,right,upper});', 'perf_depth=std::max(perf_depth,perf_parent_depth);queue.push({lo,hi,left,right,upper,perf_parent_depth});')
s=s.replace('const auto segment=queue.top();queue.pop();', 'const auto segment=queue.top();queue.pop();perf_parent_depth=segment.depth+1;')
p.write_text(s)
