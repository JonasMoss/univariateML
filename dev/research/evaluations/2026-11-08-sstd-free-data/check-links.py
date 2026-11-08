import pathlib,re
root=pathlib.Path(__file__).resolve().parents[4]
files=[root/'dev/research/evaluations/2026-11-08-sstd-free-report.md',root/'univariateML-cpp/include/cppunivariate/distributions/sstd/README.md',pathlib.Path(__file__).resolve().parent/'derivation.md']
count=0
for file in files:
    for target in re.findall(r'\[[^\]]*\]\(([^)]+)\)',file.read_text()):
        if target.startswith(('http:','https:','#','mailto:')):continue
        target=target.split('#')[0]
        assert (file.parent/target).exists(),(str(file),target)
        count+=1
print(count,'local links checked')
