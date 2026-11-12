"""Audit the reviewer-authorized test changes without changing production files."""
from pathlib import Path
import hashlib,json,re,subprocess,collections
root=Path.cwd();p=root/'dev/research/evaluations/2026-11-11-acc-fb-cont-data';start=json.loads((p/'review-start.json').read_text());allowed=set(start['authorized_test_files'])
def sha(s):return hashlib.sha256(s.encode()).hexdigest()
def end_bracket(s,begin,opening='{',closing='}',language='cpp'):
 depth=0;quote=None;i=begin
 while i<len(s):
  c=s[i]
  if quote:
   if c=='\\':i+=2;continue
   if c==quote:quote=None
   i+=1;continue
  if c in '\"\'`':quote=c;i+=1;continue
  if s.startswith('//',i) or language=='r' and c=='#':
   j=s.find('\n',i);i=len(s) if j<0 else j+1;continue
  if s.startswith('/*',i):
   j=s.find('*/',i+2);i=len(s) if j<0 else j+2;continue
  if c==opening:depth+=1
  if c==closing:
   depth-=1
   if depth==0:return i+1
  i+=1
 raise AssertionError(('Unclosed bracket',s[begin:begin+70]))
def blocks(s,language):
 regex={'cpp':r'TEST_CASE\("([^"]+)"\)\s*\{','r':r'test_that\("([^"]+)",\s*\{','js':r'''test\((['"])(.*?)\1,\s*(?:async\s*)?\([^)]*\)\s*=>\s*\{'''}[language]
 found=[]
 for m in re.finditer(regex,s):
  name=m[2] if language=='js' else m[1];end=end_bracket(s,m.end()-1,language=language)
  if language!='cpp':
   end=end_bracket(s,s.index('(',m.start()),'(',')',language)
   if s[end:end+1]==';':end+=1
  found.append((name,m.start(),end,s[m.start():end]))
 return found
def outside(s,b):
 for _,a,z,_ in reversed(b):s=s[:a]+s[z:]
 return ''.join(s.split())
permissions={
 'burr/test.cpp':{'burr: certified retained workloads and guarded stops','burr: certificate arithmetic and failure gates','Burr uncertified fallback rejects a refuted or failed search'},
 'bhatt/test.cpp':{'bhatt certificate: quantitative location, precision fallback and iteration failure','bhatt inconclusive normal certificate preserves the existing attained member','bhatt failed enclosure preserves the successful existing search as uncertified'},
 'ged/test.cpp':set(),
 'burr/test.R':{'Burr global certificate stops preserve their diagnostics','burr nearly-constant retains an uncertified finite search'},
 'invburr/test.R':{'invburr preserves certificate refusal diagnostics','invburr nearly-constant retains an uncertified finite search'},
 'pareto4/test.R':{'pareto4 preserves certificate refusal diagnostics'},
 'bhatt/test.R':{'bhatt tiny requested tolerance preserves a successful uncertified search'},
 'ged/test.R':set(),
 'stop-test.mjs':{'Burr and reciprocal nearly-constant uncertified fallback'},
 'test.mjs':{'Bhattacharjee held certificates and quantitative numerical stops'}}
changed=[];errors=[];details=[]
cache=p/'review-test-input-hashes.json';initial=json.loads(cache.read_text()) if cache.exists() else {}
for name,expected in start['sha256'].items():
 if hashlib.sha256((root/name).read_bytes()).hexdigest()!=expected:changed.append(name)
assert set(changed)<=allowed,changed
for name in sorted(allowed):
 language='cpp' if name.endswith('.cpp') else 'r' if name.endswith('.R') else 'js';key='/'.join(Path(name).parts[-2:]) if language!='js' else Path(name).name
 oldfile=root/'build/scratch/review-round/start'/name;new=(root/name).read_text();newblocks=blocks(new,language)
 if oldfile.exists():
  old=oldfile.read_text();oldblocks=blocks(old,language)
  patterns=[r'[^\n]*(?:std::vector<double>\s+(?:x|original)\b|\bx\s*<-|const x\s*=)[^\n]*']
  data=[sha(m[0].strip()) for pat in patterns for m in re.finditer(pat,old)]
  calls=[]
  regex=r'\b(?:mlburr|mlinvburr|mlpareto4|mlbhatt|api\[family\]\.fit|api\.bhatt\.fit)\('
  for m in re.finditer(regex,old):calls.append(sha(old[m.start():end_bracket(old,m.end()-1,'(',')',language)]))
  initial[name]=dict(blocks={x[0]:sha(x[3]) for x in oldblocks},outside=sha(outside(old,oldblocks)),data=data,calls=calls)
 state=initial[name];newmap={x[0]:sha(x[3]) for x in newblocks};modified=[k for k,v in state['blocks'].items() if newmap.get(k)!=v];extra=set(newmap)-set(state['blocks'])
 assert set(modified)<=permissions[key],(name,modified)
 expected_extra={'GED held-scale accuracy propagates to SGED unit skew case 31101'} if key=='ged/test.cpp' else {'GED held-scale accuracy reaches SGED unit skew case 31101'} if key=='ged/test.R' else set()
 assert extra==expected_extra,(name,extra)
 stripped=new
 if key=='burr/test.cpp':
  at=stripped.index('void check_burr_refutation_witness(');z=end_bracket(stripped,stripped.index('{',at));stripped=stripped[:at]+stripped[z:]
 assert sha(outside(stripped,blocks(stripped,language)))==state['outside'],('outside changed',name)
 nowdata=collections.Counter(sha(m[0].strip()) for pat in [r'[^\n]*(?:std::vector<double>\s+(?:x|original)\b|\bx\s*<-|const x\s*=)[^\n]*'] for m in re.finditer(pat,new))
 assert not (collections.Counter(state['data'])-nowdata),('input data changed',name)
 nowcalls=[]
 for m in re.finditer(r'\b(?:mlburr|mlinvburr|mlpareto4|mlbhatt|api\[family\]\.fit|api\.bhatt\.fit)\(',new):nowcalls.append(sha(new[m.start():end_bracket(new,m.end()-1,'(',')',language)]))
 assert not (collections.Counter(state['calls'])-collections.Counter(nowcalls)),('fit arguments changed',name)
 details.append(dict(file=name,modified_existing_tests=modified,added_tests=sorted(extra),inputs_and_fit_arguments_preserved=True))
cache.write_text(json.dumps(initial,indent=2)+'\n')
def body(s):
 at=s.index('inline BhattResult legacy_candidate(');return s[at:end_bracket(s,s.index('{',at))]
name='univariateML-cpp/include/cppunivariate/distributions/bhatt/mle.hpp';baseline=body(subprocess.check_output(['git','show','bc8ff7be:'+name],text=True));current=body((root/name).read_text());assert current==baseline
r=dict(authorized_files=sorted(allowed),changed_files=changed,production_files_unchanged=True,private_proposal_unchanged_from_bc8ff7be=True,private_proposal_sha256=sha(current),details=details,errors=[])
(p/'review-scope-audit.json').write_text(json.dumps(r,indent=2)+'\n');print(json.dumps(r,indent=2))
