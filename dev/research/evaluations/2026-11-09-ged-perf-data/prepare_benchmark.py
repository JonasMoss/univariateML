"""Create baseline/current shadow headers; instrumentation never enters the library."""
import pathlib, shutil, subprocess, sys
root=pathlib.Path.cwd(); out=root/'build/scratch/ged-perf'; out.mkdir(parents=True,exist_ok=True)
base='52820d8c9094a256aefd2d00223e9419350b22ed'
for variant in ['before','after']:
    tree=out/variant/'include'
    if tree.exists(): shutil.rmtree(tree)
    shutil.copytree(root/'univariateML-cpp/include',tree)
    if variant=='before':
        for name in ['mle.hpp','shape_certificate.hpp','free_shape_bounds.hpp']:
            path='univariateML-cpp/include/cppunivariate/distributions/ged/'+name
            (tree/'cppunivariate/distributions/ged'/name).write_bytes(subprocess.check_output(['git','show',base+':'+path]))
    profile=out/(variant+'-profile')/'include'
    if profile.exists():shutil.rmtree(profile)
    shutil.copytree(tree,profile)
    for name, funcs in [('mle.hpp',['conditional','conditional_loglik']),('free_shape_bounds.hpp',['moment','tail','interval']),('shape_certificate.hpp',['location_score','locations','score_box'])]:
        path=profile/'cppunivariate/distributions/ged'/name
        text=path.read_text()
        import re
        for fun in funcs:
            pattern=r'(inline [^\n]+ '+fun+r'\([^;]*?\) \{)'
            text,count=re.subn(pattern,lambda m:m[0]+' ged_perf::Scope perf_scope("'+fun+'");',text)
            assert count==1 or (variant=='before' and fun=='conditional_loglik' and count==0),(name,fun,count)
        if name=='mle.hpp':
            text=text.replace('for(int it=0;it<maxit;++it){const double mid=', 'for(int it=0;it<maxit;++it){++ged_perf::counters["conditional_location_steps"];const double mid=')
        if name=='free_shape_bounds.hpp':
            text=text.replace('++out.iterations;', '++out.iterations;++ged_perf::counters["moment_location_steps"];')
        if name=='shape_certificate.hpp':
            text=text.replace('Sum derivative;', '++ged_perf::counters["location_hessian_passes"];Sum derivative;')
            text=text.replace('const auto cell=pending.back();pending.pop_back();++work;', 'const auto cell=pending.back();pending.pop_back();++work; ged_perf::cells.push_back({cell.lo,cell.hi});')
        path.write_text(text)
print(base)
