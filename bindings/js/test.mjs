import test from 'node:test';
import assert from 'node:assert/strict';
import {readFileSync,writeFileSync,mkdirSync} from 'node:fs';
import create from '../../build/js/univariateML.mjs';
const m=await create();
// The public facade throws after C++ has returned, so C++ temporaries are freed.
for(const f of ['exp','gamma','gb2']) {const raw=m[`fit${f}`];m[`fit${f}`]=(...args)=>{const r=raw(...args);if(r.error)throw new Error(r.error);return r;};}
export const dossier=new URL('../../univariateML-cpp/include/cppunivariate/distributions/',import.meta.url);
export const read=(f,n)=>JSON.parse(readFileSync(new URL(`${f}/${n}.json`,dossier),'utf8'),(_,v)=>['Infinity','Inf'].includes(v)?Infinity:['-Infinity','-Inf'].includes(v)?-Infinity:v==='NaN'?NaN:v);
test('public facade',async()=>{
 const facade=await import('./index.mjs');
 const api=await facade.default();
 assert.deepEqual(Object.keys(api),facade.families);
 for(const f of facade.families) {
  for(const op of ["d","p","q","r"]) assert.equal(typeof api[f][op],"function",`${f}.${op}`);
  assert.equal(typeof api[f].fit,facade.fitFamilies.includes(f)?"function":"undefined",`${f}.fit`);
 }
 for(const f of ['exp','gamma','gb2']) {
  const g=read(f,'golden'),p=Object.values(g.estimates),x=new Float64Array([.2,.5]);
  assert.deepEqual(api[f].d(x,p,{log:true}),m['d'+f](x,p,true,true));
  assert.deepEqual(api[f].p(x,p,{lower:false,log:true}),m['p'+f](x,p,false,true));
  assert.deepEqual(api[f].q(x,p,{lower:false}),m['q'+f](x,p,false,false));
  assert.deepEqual(api[f].r(20,p,42),m['r'+f](20,p,42));
  assert.equal(api[f].fit(new Float64Array(g.data)).status,'Ok');
 }
 assert.throws(()=>api.gamma.fit(new Float64Array([1,1])),{name:'Error',message:'The gamma maximum likelihood estimator does not exist for constant data: the likelihood is unbounded as the shape grows.'});
 assert.throws(()=>api.exp.r(-1,[1],1),RangeError);
 assert.throws(()=>api.exp.r(1,[1],-1),RangeError);
});
const failures=[];let checks=0;
const dpqrFamilies=JSON.parse(readFileSync(new URL('../../build/js/generated/families.json',import.meta.url),'utf8'));
for(const f of dpqrFamilies) test(`${f} generated truth and sampler`,()=>checkTruth(f));
function checkTruth(f) {
  const t=read(f,'truth');const call=(op,x,p,lower=true,log=true)=>m[`${op}${f}`](new Float64Array([x]),p,lower,log)[0];
  function check(kind,row,a,b,ok){checks++;if(!ok)failures.push({family:f,kind,row,actual:a,truth:b,error:Math.abs(a-b),scaledError:Math.abs(a-b)/Math.max(1,Math.abs(b))});}
  for(const row of t.values){const [i,x,d,l,u]=row,p=t.parameters[i];
   check('log_d',row,call('d',x,p),d,close(call('d',x,p),d,1e-10));
   for(const [lower,b] of [[true,l],[false,u]]) {
    const a=call('p',x,p,lower);check(lower?'log_lower':'log_upper',row,a,b,close(a,b,1e-10));
    const natural=Math.exp(b);if(natural>1e-300){const got=call('p',x,p,lower,false);check(lower?'lower':'upper',row,got,natural,Number.isFinite(got)&&Math.abs(got-natural)<=1e-10*natural);}
   }
  }
  for(const row of t.quantiles){const [i,lower,log,x,b,lo,hi,lattice,abs]=row,a=call('q',x,t.parameters[i],!!lower,!!log);
   const ok=a===b||Number.isFinite(a)&&Number.isFinite(b)&&Math.abs(a-b)<=Math.max(1e-9*Math.abs(b),abs)||(lattice?a===lo||a===hi:a>=lo&&a<=hi);
   check('q',row,a,b,ok);
  }
  for(const [i] of t.sampler_points){const n=20000,seed=20261029+i,draw=m[`r${f}`](n,t.parameters[i],seed);assert.ok(draw instanceof Float64Array);assert.deepEqual(draw,m[`r${f}`](n,t.parameters[i],seed));assert.ok([...draw].every(x=>!Number.isNaN(x)));
   for(const row of t.values){if(row[0]!==i)continue;const p=Math.exp(row[3]);if(p<1e-3||p>1-1e-3)continue;const got=draw.reduce((s,x)=>s+(x<=row[1]),0)/n;check('r_cdf',row,got,p,Math.abs(got-p)<=5*Math.sqrt(p*(1-p)/n)+1/n);}
  }
  const own=failures.filter(x=>x.family===f);assert.equal(own.length,0,JSON.stringify(own));
}

const close=(a,b,t)=>a===b||Number.isNaN(a)&&Number.isNaN(b)||Number.isFinite(a)&&Number.isFinite(b)&&Math.abs(a-b)<=t*Math.max(1,Math.abs(b));
for(const f of ['exp','gamma','gb2']) {
 test(`${f} golden`,()=>{
  const g=read(f,'golden'),r=m[`fit${f}`](new Float64Array(g.data),{},{});
  assert.equal(r.status,'Ok');
  for(const [k,v] of Object.entries(g.estimates)) assert.ok(Math.abs(r.estimates[k]-v)<=(f==='gb2'?1e-5:1e-10)*Math.max(Math.abs(v),Math.abs(r.estimates[k])),`${k}: ${r.estimates[k]} vs ${v}`);
  assert.ok(Math.abs(r.logLik-g.logLik)<=(f==='gb2'?1e-9:1e-10)*Math.max(Math.abs(r.logLik),Math.abs(g.logLik)));
 });
 test(`${f} every held subset`,()=>{const g=read(f,'golden'),names=Object.keys(g.estimates);for(let mask=1;mask<1<<names.length;mask++){const held=Object.fromEntries(names.filter((_,i)=>mask>>i&1).map(k=>[k,g.estimates[k]]));const r=m[`fit${f}`](new Float64Array(g.data),held,{});for(const [k,v] of Object.entries(held))assert.equal(r.estimates[k],v);assert.equal(r.status,'Ok');}});
}
test('diagnostics and vector flags',()=>{
 for(const data of [[-1,3],[NaN,2],[Infinity,2]]) assert.throws(()=>m.fitexp(new Float64Array(data),{},{}),/Data must be nonnegative and finite/);
 assert.throws(()=>m.fitexp(new Float64Array([0,0]),{},{}),/does not exist when every observation is zero/);
 assert.throws(()=>m.fitgamma(new Float64Array([1,1]),{},{}),/does not exist for constant data/);
 assert.throws(()=>m.fitgamma(new Float64Array(read('gamma','golden').data),{scale:.5},{iterlim:1}),/Gamma delegate optimizer IterationFailure; increase iterlim./);
 assert.throws(()=>m.fitgb2(new Float64Array([1,1]),{},{}),/GB2 requires positive finite nonconstant observations/);
 assert.throws(()=>m.fitgb2(new Float64Array([0,1]),{},{}),/GB2 requires positive finite nonconstant observations/);
 assert.throws(()=>m.fitgamma(new Float64Array([]),{},{}),/^Error: Gamma MLE failed$/);
 assert.throws(()=>m.fitgb2(new Float64Array(read('gb2','golden').data),{},{gradtol:1e-8,iterlim:1}),/GB2 search unresolved: no verified interior maximum/);
 for(const f of ['exp','gamma','gb2']) {const g=read(f,'golden'),name=Object.keys(g.estimates)[0];assert.throws(()=>m['fit'+f](new Float64Array(g.data),{[name]:NaN},{}),/must be.*positive.*finite/);}
 for(const f of ['exp','gamma','gb2']){const p=Object.values(read(f,'golden').estimates),x=new Float64Array([.2,1,2]);for(const op of ['d','p','q'])for(const lower of [true,false])for(const log of [true,false]){const a=m[op+f](x,p,lower,log);assert.equal(a.length,3);for(let i=0;i<3;i++)assert.equal(a[i],m[op+f](x.slice(i,i+1),p,lower,log)[0]);}}
});
process.on('exit',()=>{mkdirSync(new URL('../../build/scratch/',import.meta.url),{recursive:true});writeFileSync(new URL('../../build/scratch/truth-results.json',import.meta.url),JSON.stringify({checks,failures},(_,v)=>typeof v==='number'&&!Number.isFinite(v)?String(v):v,2));});

// Shared-fit goldens, all held subsets and diagnostics for every migrated family.
await import('./fit-test.mjs');

await import('./vcov-test.mjs');

await import('./stop-test.mjs');
