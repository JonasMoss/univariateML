import gzip,pathlib,shutil
P=pathlib.Path(__file__).resolve().parent
LOG=pathlib.Path('build/scratch/acc-fb-count-logs')
out=LOG/'first-pass';out.mkdir(parents=True,exist_ok=True)
shutil.copyfile(P/'final-checks.json',P/'first-pass-checks.json')
for path in LOG.glob('*.log'):
 with path.open('rb') as src,gzip.open(out/(path.name+'.gz'),'wb') as dst:shutil.copyfileobj(src,dst)
print('Archived first-pass validation logs.')
