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
