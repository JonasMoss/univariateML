import test from 'node:test';
import assert from 'node:assert/strict';
import {readFileSync,writeFileSync} from 'node:fs';
import create from '../../build/js/univariateML.mjs';
const m=await create();
const cases=JSON.parse(readFileSync(new URL('../../build/js/generated/fit-cases.json',import.meta.url)),(_,v)=>v==='Inf'||v==='Infinity'?Infinity:v==='-Inf'||v==='-Infinity'?-Infinity:v==='NaN'?NaN:v);
if(process.env.FIT_CAPTURE) {
 const selected=new Set(process.env.FIT_CAPTURE.split(','));
 const capture=cases.filter(c=>selected.has(c.id)).map(c=>({id:c.id,g:c.g,result:m['fit'+c.id](new Float64Array(c.g.data),c.held,c.control)}));
 writeFileSync('build/scratch/wasm-fit-capture.json',JSON.stringify(capture,null,2));
}
// TASK-151.1: the frozen tlambda sample still carries legacy estimates.
// Independent 50-digit reference; golden.json is owned by TASK-149.1.
// Evidence: dev/research/evaluations/2026-11-08-tlambda-speed-impl-data/golden-checks.json
const profileReferences={tlambda:{mu:0.04752470769410265657093355,sigma:1.098815160848591024380682,lambda:0.1844784658509907723491377}};
for(const c of cases) {
 if(process.env.FIT_FAMILY && !process.env.FIT_FAMILY.split(/[,|]/).includes(c.id)) continue;
 const fit=(held,control=c.control)=>m['fit'+c.id](new Float64Array(c.g.data),held,control);
 test(`${c.id} shared golden`,()=>{
  const r=fit(c.held);assert.equal(r.error,undefined,r.error);assert.equal(r.status,'Ok');
  const reference=profileReferences[c.id];
  for(const [k,v] of Object.entries(reference??c.g.estimates)) {
   const tolerance=reference?1e-10:(c.tolerances[k]??c.tol);
   assert.ok(r.estimates[k]===v || Math.abs(r.estimates[k]-v)<=tolerance*((c.tolerance_scales[k]??0)+Math.max(Math.abs(v),Math.abs(r.estimates[k]))),`${k}: ${r.estimates[k]} vs ${v} tolerance ${tolerance}`);
  }
  assert.ok(Math.abs(r.logLik-c.g.logLik)<=c.ll_tol*(c.ll_scale+Math.max(Math.abs(r.logLik),Math.abs(c.g.logLik))),`${r.logLik} vs ${c.g.logLik}`);
 });
 test(`${c.id} all held subsets`,()=>{
  const names=Object.keys(c.g.estimates);
  for(let mask=0;mask<1<<names.length;mask++) {
   const held=Object.fromEntries(names.filter((_,i)=>mask>>i&1).map(k=>[k,c.g.estimates[k]]));
   const r=fit(held);
   if(r.error) {
    if(c.id==='cmb' && !('size' in held) && !('nu' in held) && /not implemented for this sample|not excluded|work budget/i.test(r.error)) continue;
    if(c.id==='hyper' && (!Object.keys(held).length || (Object.keys(held).length===1 && ('m' in held || 'k' in held))) && /numerical failure|not excluded/i.test(r.error)) continue;
    const exceptions=c.holds.filter(h=>h.held.every(k=>k in held)&&(h.held.length===Object.keys(held).length||h.held.length&&!h.reason.includes('Estimating')));
    const reasons=[...exceptions.map(h=>h.reason),...c.limits.map(l=>l.message||l.model&&`ml${l.model}`||l.name&&l.name.replace(/^shifted /,'').split(';')[0]||'')].filter(Boolean);
    assert.ok(exceptions.length || reasons.some(s=>r.error.toLowerCase().includes(s.toLowerCase())),`${JSON.stringify(held)}: ${r.error}`);
   } else {
    for(const [k,v] of Object.entries(held)) assert.equal(r.estimates[k],v);
    assert.ok(names.filter(k=>!(k in held)).every(k=>Number.isFinite(r.estimates[k])));
    assert.ok(r.logLik>=c.g.logLik-1e-6*c.g.n,`${JSON.stringify(held)}: logLik ${r.logLik}`);
   }
  }
 });
 test(`${c.id} held-value diagnostic`,()=>{
  for(const name of Object.keys(c.g.estimates)) {
   const held={...c.held,[name]:NaN};
   const r=fit(held);assert.ok(r.error,`${name}: ${JSON.stringify(r)}`);
  }
  for(const bad of [NaN,Infinity]) {
   const data=[...c.g.data];data[0]=bad;
   const invalid=m['fit'+c.id](new Float64Array(data),c.held,c.control);
   assert.ok(invalid.error,`${c.id} accepted nonfinite observations`);
  }
  if(c.integer) {
   const data=[...c.g.data];data[0]+=.5;
   const invalid=m['fit'+c.id](new Float64Array(data),c.held,c.control);
   assert.match(invalid.error,/representable finite integers/);
  }
 });
}

test('GB2 held power transfers non-unit conditional certificates and gamma witnesses',()=>{
 const y=new Float64Array([.03,.1,.3,.7,1,2,5,20]);
 const parent=m.fitbetapr(y,{shape1:2},{iterlim:1000});
 assert.equal(parent.error,undefined);
 for(const a of [.7,1,2]) {
  const x=y.map(v=>v**(1/a));
  const r=m.fitgb2(x,{shape2:a,shape3:2},{iterlim:1000});
  assert.equal(r.error,undefined,r.error);
  assert.ok(Math.abs(r.estimates.shape1/parent.estimates.shape2-1)<1e-6);
  assert.ok(Math.abs(r.estimates.scale/(parent.estimates.scale**(1/a))-1)<1e-6);
 }
 const r=m.fitgb2(new Float64Array(8).fill(2),{shape2:2,shape3:2},{});
 assert.equal(r.kind,'limit');assert.equal(r.model,'trgamma');
 assert.equal(r.estimates.shape1,2);assert.equal(r.estimates.shape2,2);
 const shifted=m.fitfpareto(new Float64Array(8).fill(4),{min:2,shape2:2,shape3:2},{});
 assert.equal(shifted.kind,'limit');assert.equal(shifted.model,'trgamma');
 assert.equal(shifted.estimates.min,undefined);
});

test('Feller Pareto held-endpoint power face keeps its model identity',()=>{
 const r=m.fitfpareto(new Float64Array([3,3,2.5,2.25]),{min:2,shape1:1,scale:1},{});
 assert.equal(r.kind,'limit');assert.equal(r.model,'power');
 assert.equal(r.estimates.alpha,1);assert.ok(r.estimates.beta>0);
});

test('scaled normal residuals preserve extreme units in every held subset',()=>{
 for(const unit of [1e-200,1e200]) for(let mask=0;mask<4;mask++) {
  const held={};if(mask&1)held.mean=0;if(mask&2)held.sd=unit;
  const mean=mask&1?0:1.5,sd=mask&2?1:mask&1?Math.sqrt(2.5):.5;
  const ll=-2*(Math.log(unit)+Math.log(sd)+.5*Math.log(2*Math.PI))-((1-mean)**2+(2-mean)**2)/(2*sd*sd);
  for(const [family,extra] of [['norm',{}],['tnorm',{a:-Infinity,b:Infinity}]]) {
   const r=m['fit'+family](new Float64Array([unit,2*unit]),{...held,...extra},{});
   assert.equal(r.error,undefined,r.error);assert.equal(r.status,'Ok');
   assert.ok(Math.abs(r.estimates.mean/unit-mean)<1e-13);
   assert.ok(Math.abs(r.estimates.sd/unit-sd)<1e-13);
   assert.ok(Math.abs(r.logLik-ll)<1e-10);
  }
 }
 const collapsed=m.fitnorm(new Float64Array([0,0]),{mean:0},{});
 assert.ok(collapsed.error);
 assert.equal(m.fitnorm(new Float64Array([0,0]),{mean:0,sd:1e-200},{}).status,'Ok');
});

test('transformed normal and normalized scale families preserve extreme units',()=>{
 for(const unit of [1e-200,1e200]) {
  for(const [family,data,location,scale,jac] of [['lnorm',[1,1],'meanlog','sdlog',0],['logitnorm',[.5,.5],'mu','sigma',2*Math.log(4)]]) for(const held of [false,true]) {
   const args={[location]:unit};if(held)args[scale]=unit;
   const r=m['fit'+family](new Float64Array(data),args,{});
   assert.equal(r.error,undefined,r.error);assert.equal(r.status,'Ok');
   assert.ok(Math.abs(r.estimates[scale]/unit-1)<1e-13);
   assert.ok(Math.abs(r.logLik-(-2*(Math.log(unit)+.5*Math.log(2*Math.PI)+.5)+jac))<1e-10);
  }
  for(const [family,held] of [['hnorm',{mu:0}],['foldnorm',{mu:0}],['bhatt',{mu:0,a:0}]]) {
   const r=m['fit'+family](new Float64Array([unit,2*unit]),held,{});
   assert.equal(r.error,undefined,r.error);assert.equal(r.status,'Ok');
   assert.ok(Math.abs(r.estimates.sigma/unit-Math.sqrt(2.5))<1e-12);
   assert.ok(Number.isFinite(r.logLik));
  }
  const r=m.fitsnorm(new Float64Array([-1,...Array(10).fill(0),1].map(v=>v*unit)),{},{});
  assert.equal(r.error,undefined,r.error);assert.equal(r.status,'Ok');
  assert.ok(Math.abs(r.estimates.sd/unit-Math.sqrt(1/6))<1e-12);
 }
});

test('Gaussian face witnesses retain extreme residual scales',()=>{
 for(const unit of [1e-200,1e200]) for(const [family,data,held,sd] of [
  ['huber',[-unit,unit],{},1],['emg',[-unit,-2*unit],{mu:0},Math.sqrt(2.5)]]) {
  const r=m['fit'+family](new Float64Array(data),held,{});
  assert.equal(r.kind,'limit',JSON.stringify(r));assert.equal(r.model,'norm');
  assert.ok(Number.isFinite(r.limit_loglik));
  assert.ok(Math.abs(r.estimates.sd/unit-sd)<1e-12);
 }
});

test('sstd xi-only certificate, reflection, budget and half-t comparison',()=>{
 const x=new Float64Array([-2,-1,-.4,0,.2,.3,.5,1,2,5]);
 const a=m.fitsstd(x,{mean:0,sd:1,nu:5},{});
 assert.equal(a.error,undefined,a.error);
 const b=m.fitsstd(Float64Array.from(x,v=>-v*1e-20),{mean:0,sd:1e-20,nu:5},{});
 assert.equal(b.error,undefined,b.error);assert.ok(Math.abs(a.estimates.xi*b.estimates.xi-1)<1e-9);
 const budget=m.fitsstd(x,{mean:0,sd:1,nu:5},{iterlim:1});assert.equal(budget.kind,'budget');
 const tied=m.fitsstd(new Float64Array([-1,1]),{mean:0,sd:1,nu:5},{});assert.equal(tied.kind,'tie');
 const face=m.fitsstd(new Float64Array([.5,1,1.5,2,2.5,3,10]),{mean:0,sd:1,nu:5},{});assert.equal(face.kind,'limit');assert.equal(face.model,'ht');assert.equal(face.estimates.nu,5);
 const reflected=m.fitsstd(new Float64Array([-.5,-1,-1.5,-2,-2.5,-3,-10]),{mean:0,sd:1,nu:5},{});
 assert.equal(reflected.kind,'limit');assert.equal(reflected.model,'unavailable:reflected-half-t');assert.equal(reflected.estimates.nu,5);
 const large=m.fitsstd(x,{mean:0,sd:1,nu:1e12},{});
 assert.equal(large.error,undefined,large.error);assert.equal(large.status,'Ok');
 assert.equal(large.estimates.nu,1e12);assert.ok(large.estimates.xi>0 && Number.isFinite(large.estimates.xi));
 const terms=m.dsstd(x,[0,1,1e12,large.estimates.xi],true,true);
 assert.ok(Math.abs([...terms].reduce((a,b)=>a+b,0)-large.logLik)<1e-11);
});

test('GEV and Frechet all-free fits expose per-fit certificate markers',()=>{
 const g=m.fitgev(new Float64Array([0,1,1,1,2,3,4]),{},{});
 assert.equal(g.error,undefined,g.error);assert.equal(g.certified,1);
 const x=Float64Array.from({length:50},(_,j)=>Math.pow(-Math.log((j+.5)/50),-.5));
 const f=m.fitfrechet(x,{},{});
 assert.equal(f.error,undefined,f.error);assert.equal(f.certified,1);
});

test('Frechet finite two-point cancellation retains an uncertified fit',()=>{
 const p=.45,mu=p*.1/(Math.sqrt(p*(1-p))+p);
 const f=m.fitfrechet(new Float64Array([...Array(9).fill(mu),...Array(11).fill(mu+1)]),{},{});
 assert.equal(f.error,undefined,f.error);assert.equal(f.certified,0);
 assert.ok(Math.abs(f.estimates.mu)<1e-12);
});

test('an unordered Frechet Gumbel comparison retains the finite search',()=>{
 const shape=.0041309589552387081;
 const x=Float64Array.from({length:80},(_,j)=>Math.expm1(-shape*Math.log(-Math.log((j+.5)/80)))/shape);
 const f=m.fitfrechet(x,{},{});
 assert.equal(f.error,undefined,f.error);assert.equal(f.certified,0);
 assert.ok(f.estimates.lambda>1e5);assert.ok(Number.isFinite(f.logLik));
});

test('GEV coupled support corners survive normalization and outer coverage',()=>{
 const cases=[[-.55225555774106538,1.2538261877399721,.35823280453135459],
  [1.696102873837644,-1.2511415791724954,-1.142128614350542],
  [36.78357020148276,62.016500801895056,79.137895173444065],
  [.47613503331464652,2.0186627711562051,1.3293650873876488]];
 for(const x of cases) {
  const f=m.fitgev(new Float64Array(x),{mu:0},{});
  assert.equal(f.error,undefined,f.error);assert.equal(f.certified,1);
  assert.equal(f.estimates.xi,-1);assert.equal(f.estimates.mu,0);
  assert.equal(f.estimates.sigma,Math.max(...x));
 }
});

test('count fallback exposes per-fit certification',()=>{
 const r=m.fithyper(new Float64Array([0,0,0,5,5,5]),{k:5},{});
 assert.equal(r.status,'Ok');
 assert.equal(r.error,undefined);
 assert.equal(r.certified,0);
 assert.ok(Number.isFinite(r.logLik));
 assert.equal(r.estimates.k,5);
 const f=m.fitztbinom(new Float64Array([1,2,2,3]),{size:5},{});
 assert.equal(f.status,'Ok');assert.equal(f.certified,1);
});


test('Frechet held scale keeps coefficient accuracy at large shape',()=>{
 const data=new Float64Array([0,2.439431989742502e-10,3.8143577185678623e-10,4.835630784683076e-10,5.672827763092414e-10,6.39559960546876e-10,7.039885341342256e-10,7.626852482900404e-10,8.170100151971837e-10,8.678938678841064e-10,9.160041614109105e-10,9.618379426257206e-10,1.0057765731374957e-09,1.0481199241851868e-09,1.0891091362097427e-09,1.1289412737980342e-09,1.167780427913101e-09,1.2057650433661138e-09,1.2430135809538e-09,1.279628292216728e-09,1.3156989941975894e-09,1.3513048457980403e-09,1.3865166792470518e-09,1.4213985544131447e-09,1.4560084249382044e-09,1.4903998035720178e-09,1.5246223172837858e-09,1.5587222623736352e-09,1.5927433816287362e-09,1.6267270863679073e-09,1.6607136776869424e-09,1.6947413472578887e-09,1.7288476206189785e-09,1.763069801263839e-09,1.7974443045076782e-09,1.8320075456657037e-09,1.866796162097728e-09,1.9018474572973787e-09,1.937198734758283e-09,1.972889074330908e-09,2.008958221999535e-09,2.04544747806068e-09,2.082400141212304e-09,2.119861508553811e-09,2.157879541719865e-09,2.196504644835784e-09,2.2357909967851697e-09,2.2757963291653027e-09,2.316582592420957e-09,2.358217510156635e-09,2.4007736909581467e-09,2.4443308488386606e-09,2.488976247327912e-09,2.5348062537844385e-09,2.5819267834847892e-09,2.630456852337204e-09,2.6805287989262183e-09,2.732291171092527e-09,2.7859131668250825e-09,2.8415866326625405e-09,2.8995313927637767e-09,2.96000213229064e-09,3.0232943926122857e-09,3.0897556735354215e-09,3.159798533935998e-09,3.2339161348815537e-09,3.3127082188499912e-09,3.3969117518850567e-09,3.4874473309187692e-09,3.5854869073759232e-09,3.692554373380119e-09,3.810682325244841e-09,3.9426680809029335e-09,4.092513772313566e-09,4.266237030314812e-09,4.4735025683806384e-09,4.731339875441165e-09,5.074264231019754e-09,5.591473173360839e-09,6.696401322336953e-09]);
 const f=m.fitfrechet(data,{sigma:1},{});
 assert.equal(f.error,undefined,f.error);
 assert.ok(Math.abs(f.estimates.lambda/1008905594.239193-1)<1e-10);
 assert.ok(Math.abs(f.estimates.mu+.99999999837500435)<1e-10);
 assert.equal(f.estimates.sigma,1);
});
