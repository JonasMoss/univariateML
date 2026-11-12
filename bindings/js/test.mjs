import {selectedFamily} from './test-register.mjs';
import test from './test-register.mjs';
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
function previousDouble(x) {
  const view=new DataView(new ArrayBuffer(8));view.setFloat64(0,x);
  view.setBigUint64(0,view.getBigUint64(0)+(x>0 ? -1n : 1n));
  return view.getFloat64(0);
}
const dpqrFamilies=JSON.parse(readFileSync(new URL('../../build/js/generated/families.json',import.meta.url),'utf8'));
for(const f of dpqrFamilies.filter(selectedFamily)) test(`${f} generated truth and sampler`,()=>checkTruth(f));
function checkTruth(f) {
  const discrete=/type: integer/.test(readFileSync(new URL(`${f}/README.md`,dossier),'utf8'));
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
   let ok=a===b||Number.isFinite(a)&&Number.isFinite(b)&&Math.abs(a-b)<=Math.max(1e-9*Math.abs(b),abs)||(lattice?a===lo||a===hi:a>=lo&&a<=hi);
   if(!discrete&&!ok&&Number.isFinite(a)&&Number.isFinite(b)&&(log?Number.isFinite(x)&&x<0:x>0&&x<1)) {
    const at=call('p',a,t.parameters[i],!!lower,!!log);
    ok=Number.isFinite(at)&&Math.abs(at-x)<=1e-12*(log?Math.max(1,Math.abs(x)):Math.abs(x));
   }
   const large=discrete&&Math.abs(b)>2**53&&(log?Number.isFinite(x)&&x<0:x>0&&x<1);
   const at=large?call('p',a,t.parameters[i],!!lower,!!log):NaN;
   const before=large?call('p',previousDouble(a),t.parameters[i],!!lower,!!log):NaN;
   check('q',row,a,b,large ? ok&&(Number.isFinite(a)||b===Infinity&&a===Infinity)&&(lower?at>=x&&before<x:at<=x&&before>x) : ok);
  }
  for(const [i] of t.sampler_points){const n=20000,seed=20261029+i,draw=m[`r${f}`](n,t.parameters[i],seed);assert.ok(draw instanceof Float64Array);assert.deepEqual(draw,m[`r${f}`](n,t.parameters[i],seed));assert.ok([...draw].every(x=>!Number.isNaN(x)));
   for(const row of t.values){if(row[0]!==i)continue;const p=Math.exp(row[3]);if(p<1e-3||p>1-1e-3)continue;const got=draw.reduce((s,x)=>s+(x<=row[1]),0)/n;check('r_cdf',row,got,p,Math.abs(got-p)<=5*Math.sqrt(p*(1-p)/n)+1/n);}
  }
  const own=failures.filter(x=>x.family===f);assert.equal(own.length,0,JSON.stringify(own));
}

const goldenCases=JSON.parse(readFileSync(new URL('../../build/js/generated/fit-cases.json',import.meta.url)));
const close=(a,b,t)=>a===b||Number.isNaN(a)&&Number.isNaN(b)||Number.isFinite(a)&&Number.isFinite(b)&&Math.abs(a-b)<=t*Math.max(1,Math.abs(b));
for(const f of ['exp','gamma','gb2'].filter(selectedFamily)) {
 test(`${f} golden`,()=>{
  const g=read(f,'golden'),r=m[`fit${f}`](new Float64Array(g.data),{},{});
  const policy=goldenCases.find(c=>c.id===f);
  assert.equal(r.status,'Ok');
  for(const [k,v] of Object.entries(g.estimates)) assert.ok(Math.abs(r.estimates[k]-v)<=policy.tol*Math.max(Math.abs(v),Math.abs(r.estimates[k])),`${k}: ${r.estimates[k]} vs ${v}`);
  assert.ok(Math.abs(r.logLik-g.logLik)<=policy.ll_tol*Math.max(Math.abs(r.logLik),Math.abs(g.logLik)));
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
 assert.throws(()=>m.fitgb2(new Float64Array(read('gb2','golden').data),{},{gradtol:1e-8,iterlim:1}),/GB2 certified delegate budget stop; no verified maximum/);
 for(const f of ['exp','gamma','gb2']) {const g=read(f,'golden'),name=Object.keys(g.estimates)[0];assert.throws(()=>m['fit'+f](new Float64Array(g.data),{[name]:NaN},{}),/must be.*positive.*finite/);}
 for(const f of ['exp','gamma','gb2']){const p=Object.values(read(f,'golden').estimates),x=new Float64Array([.2,1,2]);for(const op of ['d','p','q'])for(const lower of [true,false])for(const log of [true,false]){const a=m[op+f](x,p,lower,log);assert.equal(a.length,3);for(let i=0;i<3;i++)assert.equal(a[i],m[op+f](x.slice(i,i+1),p,lower,log)[0]);}}
});
process.on('exit',()=>{mkdirSync(new URL('../../build/scratch/',import.meta.url),{recursive:true});writeFileSync(new URL(`../../build/scratch/truth-results-${process.env.UMVL_JS_GROUP ?? 'all'}.json`,import.meta.url),JSON.stringify({checks,failures},(_,v)=>typeof v==='number'&&!Number.isFinite(v)?String(v):v,2));});

// Shared-fit goldens, all held subsets and diagnostics for every migrated family.
await import('./fit-test.mjs');

await import('./vcov-test.mjs');

await import('./stop-test.mjs');

test('lst and std certificate normal stops name the fitted face', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  for (const family of ['lst', 'std']) {
    assert.throws(() => api[family].fit(new Float64Array([-1, -1, 1, 1])), (error) => {
      assert.equal(error.kind, 'limit');
      assert.equal(error.model, 'norm');
      assert.equal(error.estimates.mean, 0);
      assert.equal(error.estimates.sd, 1);
      assert.ok(Number.isFinite(error.loglik_lower));
      assert.ok(error.loglik_upper >= error.loglik_lower);
      return true;
    });
  }
});

test('std certificate distinguishes ordinary t2 and collapse paths', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  const cluster = new Float64Array([
    ...Array.from({length: 30}, (_, i) => -1e-4 + 2e-4 * i / 29),
    ...Array.from({length: 10}, (_, i) => .9999 + 2e-4 * i / 9),
  ]);
  assert.throws(() => api.std.fit(cluster), error => {
    assert.equal(error.kind, 'limit');
    assert.equal(error.model, 'lst');
    assert.equal(error.estimates.df, 2);
    assert.ok(error.estimates.sigma > 0 && Number.isFinite(error.estimates.sigma));
    return true;
  });
  assert.throws(() => api.std.fit(new Float64Array([-1, 0, 0, 0, 0, 1])), error => {
    assert.equal(error.kind, 'nomle');
    assert.equal(error.model, 'unavailable:t-scale-collapse');
    assert.deepEqual(error.estimates, {mu: 0, df: 2});
    assert.ok(error.loglik_lower <= -6 * Math.log(2));
    assert.ok(error.loglik_upper >= -6 * Math.log(2));
    return true;
  });
});

test('std certificate resolves the shallow quantile maximum', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  const probabilities = Float64Array.from({length: 4000}, (_, i) => (i + .5) / 4000);
  const x = api.lst.q(probabilities, [0, 1, 300]);
  const fit = api.std.fit(x, {}, {iterlim: 500});
  assert.equal(fit.status, 'Ok');
  assert.ok(Math.abs(fit.estimates.nu / 539.46057298 - 1) < 1e-7);
  assert.ok(Math.abs(fit.estimates.mean) < 1e-7);
  assert.ok(fit.estimates.sd > 1);
});


test('lst and std iterlim scales the certificate and preserves budget witnesses', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  const x = new Float64Array([0.050954662455960954,-0.41009991819347347,-0.27514948570271125,1.738894665940981,0.7827929830488856,-0.6094885407586247,0.16676804415486185,2.1605246276102874,-0.2387465089482371,-2.306020779595189,0.7712778390740149,0.868444961637359,1.0920372377409862,-0.6483986076864828,0.46991123089786935,-0.42877220249733083,-0.6711991838272713,-0.02262549573968949,-1.4089791730105745,0.03065718253139144,0.7394838316847435,-0.9188201716268543,-0.9824239413172151,-1.2717118121176605,0.7072827328968989,1.1005705147845657,-0.31552546144261623,0.24877221879696262,-0.7638326991319095,-0.8839285063895818,0.6899451665252806,-0.06974028824316554,-0.497841648238298,0.8381413762133866,-0.26457437687827884,-0.38679889567518655,-0.1278879326472602,-0.06134395969980899,-1.9342834627670904,-0.9650194594318576,0.12782341531106875,-0.5397482053246428,1.393079864160065,-1.8866780642217882,0.48159934707033075,0.37619107597635915,-0.7842379492323549,0.16661327780961455,0.06570257810373901,1.5033378387590548,-1.5584653425173214,0.5575584145186088,-0.7733568166152429,0.17687773844287813,-0.453478962001212,-2.6420087840828708,-2.0359122776319354,0.40101402508509254,2.637322231411576,-1.2594962766931357,0.896216299275718,-1.124759799790428,-0.31742515720096587,-0.3950423506433641,2.0094231968658294,0.6377817892826372,1.362962386207389,0.4899601440673617,0.7184328548359303,0.7179914812225302,0.616123650652679,0.2661075130421014,-0.6585341582181332,-1.012868755256849,-0.5993024660073559,1.0810268064227586,0.5679558382979852,1.3096494218560304,0.8540889501811413,-1.3334550306342379]);
  for (const family of ['lst','std']) {
    assert.throws(() => api[family].fit(x, {}, {iterlim: 7}), error => {
      assert.equal(error.kind, 'budget');
      assert.match(error.message, /Raising iterlim may resolve/);
      assert.ok(Number.isFinite(error.loglik_lower));
      assert.ok(error.candidates.length > 0);
      return true;
    });
    assert.equal(api[family].fit(x, {}, {iterlim: 20}).status, 'Ok');
  }
});

test('J6 tnorm CMB and CMP quantitative acceptance uses common binding routes', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  const x = new Float64Array([0,0,1,1,2,2,3,4]);
  const cases = [
    ['tnorm', {a:-2,b:8}, [{},{mean:1},{sd:2},{mean:1,sd:2}], ['mean','sd','a','b']],
    ['cmb', {size:8}, [{},{prob:.3},{nu:1},{prob:.3,nu:1}], ['size','prob','nu']],
    ['cmp', {}, [{},{lambda:2},{nu:1},{lambda:2,nu:1}], ['lambda','nu']],
  ];
  for (const [family, design, subsets, names] of cases) for (const held of subsets) {
    const fit = api[family].fit(x, {...design,...held});
    assert.equal(fit.status, 'Ok');
    const parameters = names.map(name => fit.estimates[name]);
    const ll = api[family].d(x, parameters, {log:true}).reduce((a,b) => a+b, 0);
    assert.ok(Math.abs(fit.logLik-ll) < 1e-9*(1+Math.abs(ll)));
  }
  assert.throws(() => api.tnorm.fit(x, {a:-2,b:8}, {iterlim:1}), error => error.kind==='numerical');
  assert.throws(() => api.cmb.fit(x, {size:8}, {reltol:.1,iterlim:1}), error => error.kind==='numerical');
  assert.throws(() => api.cmp.fit(x, {}, {reltol:.1}), error => error.kind==='numerical');
  const geometric = new Float64Array([0,0,0,20000]);
  assert.throws(() => api.cmp.fit(geometric, {lambda:5000/5001}, {iterlim:1}), error => {
    assert.equal(error.kind, 'budget');
    assert.match(error.message, /Raising iterlim/);
    assert.equal(error.candidates.length, 1);
    assert.ok(Number.isFinite(error.candidates[0].loglik_lower));
    return true;
  });
  assert.equal(api.cmp.fit(geometric, {lambda:5000/5001}, {iterlim:10}).estimates.nu, 0);
});

test('sn conditional certificates and nonunique shape have shared stops', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  const x = new Float64Array([-2,-1,0,1,2]);
  for (const held of [{xi: 0}, {xi: 0, omega: 1.7}]) {
    const f = api.sn.fit(x, held);
    assert.ok(Math.abs(f.estimates.alpha) < 1e-9);
    assert.ok(Math.abs(f.estimates.omega - (held.omega ?? Math.sqrt(2))) < 1e-10);
  }
  assert.throws(() => api.sn.fit(new Float64Array([3,3,3,3,3]), {xi: 3, omega: 2}), e => {
    assert.equal(e.kind, 'tie');
    assert.equal(e.candidates.length, 2);
    assert.equal(e.loglik_lower, e.loglik_upper);
    assert.notEqual(e.candidates[0].alpha, e.candidates[1].alpha);
    return true;
  });
  assert.throws(() => api.sn.fit(new Float64Array([-2,-.3,.1,.8,4]), {alpha: 2}, {iterlim: 1}), e => {
    assert.equal(e.kind, 'numerical');
    assert.equal(e.candidates.length, 1);
    return true;
  });
});

test('sn finite normalization overflow is a numerical stop', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  assert.throws(() => api.sn.fit(new Float64Array([-1e308,1e308]), {alpha: 0}), e => {
    assert.equal(e.kind, 'numerical');
    assert.equal(e.candidates.length, 0);
    return true;
  });
});

test('sn rounded residuals cannot fabricate held-endpoint factors', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  assert.throws(() => api.sn.fit(new Float64Array([1,2,1e308]), {xi: 0}), e => {
    assert.equal(e.kind, 'numerical');
    assert.equal(e.candidates.length, 0);
    return true;
  });
});

test('sn checks likelihood after restoring large held shapes', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  const x = new Float64Array([1,2,3]), p = [1+1e-15,1.3,1e16];
  const ll = [...api.sn.d(x,p,{log:true})].reduce((a,b) => a+b,0);
  let f;
  try { f = api.sn.fit(x,{xi:p[0],omega:p[1],alpha:p[2]}); }
  catch (e) {
    assert.equal(e.kind,'numerical');
    assert.equal(e.candidates.length,1);
    assert.ok(Math.abs(e.candidates[0].loglik_lower-ll)<1e-12);
    return;
  }
  assert.ok(Math.abs(f.logLik-ll)<3e-9);
});

test('trgamma certificates preserve held endpoints and reciprocal likelihoods', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  const x = new Float64Array([.1,.3,1]);
  const a = api.trgamma.fit(x, {scale: 1});
  const b = api.invtrgamma.fit(Float64Array.from(x, v => 1/v), {scale: 1});
  assert.equal(a.status, 'Ok');
  assert.equal(b.status, 'Ok');
  for (const name of ['shape1','shape2'])
    assert.ok(Math.abs(a.estimates[name]/b.estimates[name]-1) < 1e-9);
  assert.equal(a.estimates.scale, 1);
  assert.equal(b.estimates.scale, 1);
  assert.ok(Math.abs(b.logLik-a.logLik-2*(Math.log(.1)+Math.log(.3))) < 1e-10);
});

test('trgamma held-scale concentration and numerical stops have distinct kinds', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  for (const [family,value] of [['trgamma',2],['invtrgamma',.5]]) {
    assert.throws(() => api[family].fit(new Float64Array([value,value,value]), {scale: 1}), error => {
      assert.equal(error.kind, 'nomle');
      return true;
    });
  }
  assert.throws(() => api.trgamma.fit(new Float64Array([.1,.3,1]), {scale: 1}, {iterlim: 1}), error => {
    assert.ok(['numerical','budget'].includes(error.kind));
    return true;
  });
});

test('trgamma held endpoint comparisons stop without asserting nonattainment', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  const x = new Float64Array([Math.exp(-1),1-Number.EPSILON/2,1]);
  for (const [family,data] of [['trgamma',x],['invtrgamma',Float64Array.from(x,v=>1/v)]]) {
    assert.throws(() => api[family].fit(data,{scale:1}),error => {
      assert.equal(error.kind,'tie');
      assert.equal(error.candidates.length,2);
      assert.ok(error.loglik_upper>=error.loglik_lower);
      assert.ok(error.loglik_upper-error.loglik_lower<1e-7);
      return true;
    });
  }
});

test('trgamma partial logarithmic ties stop numerically', async () => {
  const {default:createApi}=await import('./index.mjs');
  const api=await createApi();
  const x=Float64Array.from({length:16},(_,i)=>7-i*4*Number.EPSILON);
  for(const family of ['trgamma','invtrgamma'])assert.throws(()=>api[family].fit(x),error=>{
    assert.equal(error.kind,'numerical');
    assert.match(error.message,/taking logarithms/);
    return true;
  });
});

test('trgamma limit witnesses preserve raw support endpoints', async () => {
  const {default:createApi}=await import('./index.mjs');
  const api=await createApi();
  const x=Float64Array.from({length:400},(_,i)=>7*Math.pow((i+1)/400,1/2.5));
  assert.throws(()=>api.trgamma.fit(x),error=>{
    assert.equal(error.kind,'limit');assert.equal(error.estimates.alpha,7);return true;
  });
  const inverse=Float64Array.from(x,v=>1/v);
  assert.throws(()=>api.invtrgamma.fit(inverse),error=>{
    assert.equal(error.kind,'limit');assert.equal(error.estimates.b,Math.min(...inverse));return true;
  });
});

test('trgamma unrepresentable scales retain numerical stop details', async () => {
  const {default:createApi}=await import('./index.mjs');
  const api=await createApi();
  const z=api.norm.q(Float64Array.from({length:201},(_,i)=>(i+.5)/201),[0,1]);
  const x=Float64Array.from(z,v=>Math.exp(v-.001*(v*v-1)));
  for(const [family,data] of [['trgamma',x],['invtrgamma',Float64Array.from(x,v=>1/v)]])
    assert.throws(()=>api[family].fit(data),error=>{
      assert.equal(error.kind,'numerical');assert.match(error.message,/log\(scale\)/);
      assert.equal(error.loglik_upper,Infinity);return true;
    });
});

test('scalar-trio certificates expose fatigue ties and solve held scalar roots', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  assert.throws(() => api.fatigue.fit(new Float64Array([1]), {alpha: 3, mu: 0}), error => {
    assert.equal(error.kind, 'tie');
    assert.equal(error.candidates.length, 2);
    return true;
  });
  assert.throws(() => api.fatigue.fit(new Float64Array([1,2,3]), {alpha: 1, mu: 0}, {iterlim: 1}), error => {
    assert.equal(error.kind, 'numerical');
    return true;
  });
  const x = new Float64Array([.02,.2,.6,.9]);
  const fit = api.genbeta.fit(x, {scale: 1, shape1: 2, shape2: .7});
  const a = fit.estimates.shape3;
  const score = Array.from(x, v => {const z = -a*Math.log(v); return 1-2*z-.3*z/Math.expm1(z);}).reduce((a,b) => a+b, 0);
  assert.ok(Math.abs(score) < 1e-9);
  assert.equal(api.sgomp.fit(new Float64Array([0,.1,.3,2,9]), {eta: 2}).status, 'Ok');
});

test('fatigue certificate stops when rounding hides alpha resolution', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  const x = Float64Array.from({length: 5}, (_, i) => 1+i*Number.EPSILON);
  assert.throws(() => api.fatigue.fit(x, {mu: 0}), error => {
    assert.equal(error.kind, 'numerical');
    assert.equal(error.candidates.length, 1);
    return true;
  });
});

test('genbeta power certificate keeps tiny held beta shapes in double precision', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  const x = new Float64Array([.02,.2,.6,.9]);
  const mean = Array.from(x, v => -Math.log(v)).reduce((a,b) => a+b,0)/x.length;
  for(const q of [1e-16,1e-100,1e-300]) {
    const fit = api.genbeta.fit(x, {scale: 1, shape1: 2, shape2: q});
    assert.ok(Math.abs(fit.estimates.shape3/q*1.5*mean-1) < 1e-12);
    assert.ok(Number.isFinite(fit.logLik));
  }
});

test('genbeta power certificate retains a large-shape weighted exponential tail', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  const fit = api.genbeta.fit(new Float64Array([.5]), {scale: 1, shape1: 2, shape2: Math.exp(705)});
  const z = fit.estimates.shape3*Math.log(2);
  assert.ok(z > 700);
  assert.ok(Math.abs(z-(705-Math.log(2-1/z))) < 1e-9);
});

test('genbeta critical held p one half preserves the small-power quadratic score', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  const x = new Float64Array([.02,.2,.6,.9]);
  const second = Array.from(x,v => Math.log(v)**2).reduce((a,b) => a+b,0)/x.length;
  const fit = api.genbeta.fit(x,{scale: 1,shape1: .5,shape2: 1e-100});
  assert.ok(Math.abs(fit.estimates.shape3/1e-50/Math.sqrt(12/second)-1) < 1e-12);
});

test('fatigue rounded threshold translations stop numerically rather than claiming nonexistence', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  for(const [x, held] of [[new Float64Array([1,2]), {mu: -1e300}], [new Float64Array([1]), {mu: .1,beta: .9}]]) {
    assert.throws(() => api.fatigue.fit(x,held), error => {assert.equal(error.kind,'numerical');return true;});
  }
});

test('fatigue subnormal coefficient reconstruction must retain relative accuracy', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  assert.throws(() => api.fatigue.fit(new Float64Array([1e-320,3e-320,4e-320]),{mu: 0}), error => {
    assert.equal(error.kind,'numerical');return true;
  });
});

test('shifted Gompertz unfinished bracket preserves a finite witness', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  assert.throws(() => api.sgomp.fit(new Float64Array([1,1.01]),{},{iterlim: 1}), error => {
    assert.equal(error.kind,'numerical');
    assert.ok(Number.isFinite(error.loglik_lower));return true;
  });
});
test('lst and std fixed-scale location certificates transport equal maxima', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  for (const [family, held] of [['lst', {sigma: 1, df: 3}], ['std', {sd: 1, nu: 3}]]) {
    assert.throws(() => api[family].fit(new Float64Array([-3, 3]), held), error => {
      assert.equal(error.kind, 'tie');
      assert.equal(error.candidates.length, 2);
      assert.ok(Number.isFinite(error.loglik_lower));
      assert.ok(Number.isFinite(error.loglik_upper));
      return true;
    });
  }
});

test('lst and std uncertified held free-shape searches preserve golden fits', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  for (const family of ['lst','std']) {
    const g = read(family,'golden'), names = Object.keys(g.estimates);
    for (const mask of [1,2,3]) {
      const held = Object.fromEntries(names.filter((_,i) => mask >> i & 1).map(k => [k,g.estimates[k]]));
      const fit = api[family].fit(new Float64Array(g.data), held, {iterlim: 1000});
      assert.equal(fit.status, 'Ok');
      for (const [key,value] of Object.entries(held)) assert.equal(fit.estimates[key],value);
      assert.ok(Object.values(fit.estimates).every(Number.isFinite));
      assert.ok(fit.logLik >= g.logLik - 1e-6*g.data.length);
      assert.throws(() => api[family].fit(new Float64Array(g.data), held, {iterlim: 1}), error => {
        assert.equal(error.kind, family === 'lst' && mask === 2 ? 'budget' : 'numerical');
        return true;
      });
    }
  }
});

test('std collapse comparisons preserve the sign at binary64 rational shapes', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  assert.throws(() => api.std.fit(new Float64Array([0,0,0,0,0,0,0,0,1,2,3]), {nu: 8/3}), error => {
    assert.equal(error.kind, 'nomle');
    assert.match(error.message, /unbounded/);
    assert.equal(error.loglik_lower, Infinity);
    return true;
  });
});

test('lst Cauchy equality checks and projects the represented maximizing curve', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  const a = 2 ** 20, step = 2 ** -32;
  assert.throws(() => api.lst.fit(new Float64Array([a,a,a+step,a+step]), {df: 1}), error => {
    assert.equal(error.kind, 'numerical');
    assert.ok(Number.isFinite(error.loglik_lower));
    assert.equal(error.loglik_upper, Infinity);
    return true;
  });
  const fit = api.lst.fit(new Float64Array([a,a,a+3*step,a+3*step]), {df: 1});
  assert.equal(fit.status, 'Ok');
  assert.ok(fit.estimates.mu > a && fit.estimates.mu < a+3*step);
  assert.ok(Math.abs(fit.estimates.sigma/step-Math.sqrt(2)) < 1e-6);
});

test('std held-shape sd overflow retains a proper ordinary-scale witness', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  const nu = 2 + 2*Number.EPSILON;
  for (const held of [{nu}, {mean: 0, nu}]) {
    assert.throws(() => api.std.fit(new Float64Array([-1e301,1e301]), held), error => {
      assert.equal(error.kind, 'numerical');
      assert.ok(Number.isFinite(error.loglik_lower));
      assert.ok(error.candidates.length > 0);
      return true;
    });
  }
});

test('lst and std held solver caps return structured numerical stops', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  for (const [family, held] of [['lst', {df: 3}], ['lst', {mu: 0, df: 3}], ['std', {nu: 3}], ['std', {mean: 0, nu: 3}]]) {
    assert.throws(() => api[family].fit(new Float64Array([-3,-1,.2,1,8]), held, {iterlim: 1}), error => {
      assert.equal(error.kind, 'numerical');
      assert.ok(Number.isFinite(error.loglik_lower));
      assert.equal(error.loglik_upper, Infinity);
      return true;
    });
  }
});
test('SGED subunit asymmetric scale certificates agree with independent fixtures', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  const x = new Float64Array([-3,-1,-.2,0,.3,.4,2]);
  for (const [nu,sd,ll] of [[.3,1.6240399853168161,-12.241713109781369], [.9,1.4916863638394504,-12.781716887610742]]) {
    const fit = api.sged.fit(x,{mean: .1,nu,xi: 1.3});
    assert.equal(fit.status,'Ok');
    assert.ok(Math.abs(fit.estimates.sd/sd-1)<2e-7);
    assert.ok(Math.abs(fit.logLik-ll)<2e-6);
    const density = api.sged.d(x,[.1,fit.estimates.sd,nu,1.3],{log: true});
    // logLik is the certified supremum; at a subunit cusp the density at the
    // rounded sd is lower by about eps^nu per cusp observation.
    const sum = Array.from(density).reduce((a,b)=>a+b,0);
    assert.ok(sum<=fit.logLik+1e-12*(1+Math.abs(fit.logLik)));
    assert.ok(fit.logLik-sum<=x.length*Math.pow(64*Number.EPSILON,nu));
  }
});

test('SGED subunit scale ties and exhausted certificates stop in the common binding', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  let lo=4,hi=6;
  for(let j=0;j<60;++j){const r=(lo+hi)/2;if(Math.log(r)>2*(1-1/r))hi=r;else lo=r;}
  const r=(lo+hi)/2,k=2*Math.sqrt(r-1)/r,xi=Math.sqrt(1+k*k/6);
  assert.throws(()=>api.sged.fit(new Float64Array([-1,-1,-1]),{mean: 0,nu: .5,xi},{iterlim: 400}),error=>{
    assert.equal(error.kind,'tie');
    assert.ok(error.candidates.length>=2);
    const scales=error.candidates.map(c=>c.sd);
    assert.ok(Math.abs(Math.max(...scales)/Math.min(...scales)/r-1)<2e-6);
    assert.ok(Number.isFinite(error.loglik_upper));
    return true;
  });
  assert.throws(()=>api.sged.fit(Float64Array.from({length:160},(_,j)=>-1-j*.1),{mean: 0,nu: .5,xi: 1.3},{iterlim: 1}),error=>{
    assert.equal(error.kind,'budget');assert.ok(error.candidates.length>=1);return true;
  });
});
test('extreme scalar certificates expose common binding stops', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  for(const [family,x,held] of [
    ['frechet',[0,.2,.5,1,3,20],{lambda:2}],
    ['gev',[-2,0,.3,1,5],{sigma:2,xi:-.3}]
  ]) {
    assert.throws(() => api[family].fit(new Float64Array(x),held,{iterlim:1}), e => {
      assert.equal(e.kind,'numerical');assert.equal(e.loglik_upper,Infinity);
      assert.ok(e.candidates.length>0);return true;
    });
    assert.equal(api[family].fit(new Float64Array(x),held).status,'Ok');
  }
  assert.throws(() => api.frechet.fit(new Float64Array([0,0,1,2]),{lambda:1}), e => {
    assert.equal(e.kind,'nomle');assert.ok(Number.isFinite(e.loglik_upper));return true;
  });
});
test('betapr non-unit conditional certificate transports finite fits and gamma faces', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  const x = new Float64Array([.03,.1,.4,.7,1,2,4,12]);
  const f = api.betapr.fit(x,{shape1: 2});
  const inv = api.betapr.fit(Float64Array.from(x,v => 1/v),{shape2: 2});
  assert.ok(Math.abs(f.estimates.shape2/.6398504179662102-1)<1e-6);
  assert.ok(Math.abs(inv.estimates.scale*f.estimates.scale-1)<1e-6);
  assert.throws(() => api.betapr.fit(new Float64Array([1,2,3,4]),{shape1: .7}), e => {
    assert.equal(e.kind,'limit');assert.equal(e.model,'gamma');
    assert.ok(Math.abs(e.estimates.scale-2.5/.7)<1e-10);return true;
  });
  assert.throws(() => api.betapr.fit(new Float64Array([3,3]),{shape2: 2}), e => {
    assert.equal(e.kind,'limit');assert.equal(e.model,'invgamma');
    assert.equal(e.estimates.alpha,2);assert.ok(Math.abs(e.estimates.beta-6)<1e-10);return true;
  });
  assert.throws(() => api.betapr.fit(new Float64Array([1,2,3,4]),{shape1: 1e100}), e => {
    assert.equal(e.kind,'numerical');assert.ok(e.candidates.length>0);return true;
  });
});

test('betapr rounded conditional peaks retain an uncertified search', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  const x = new Float64Array([1.9901008294052028e-5,2.209857117039297e-5,.0002450722434289249,
    12.288342947780086,942.976056905759,16.239512580216314,2.898690421966857,
    1.2684036923492177,400970.63293690106*Math.exp(5.646438395028704)]);
  const f=api.betapr.fit(x,{shape1: .22690393135372058});
  assert.equal(f.status,'Ok');assert.equal(f.certified,0);assert.ok(Number.isFinite(f.logLik));
});

test('betapr flat gamma-adjacent profile retains coefficient accuracy in WASM', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  const x = new Float64Array([
    0.2802410396791599,1.3436077191767044,0.3472030471432838,0.2441657957218209,0.37701961388941624,
    0.6833708903622272,0.6278389000252419,0.38052450377474095,0.36732564867894735,0.29465588306090007,
    1.6337413655534816,0.5004872207354496,0.25644728823570834,0.30489045402771314,0.43156701444273454,
    0.3417576191295499,0.6299669667981216,0.4052784001144798,0.4067675092434208,0.4723512023354753,
    0.23656005267689753,0.2642974860041385,0.9126126455038472,0.32651442909992406,0.46787154802779246,
    0.1628156505909309,0.3788340921782178,0.835567043527286,0.3323693396507257,0.3235089636008228,
    0.5129659623359408,0.6237662717585744,0.2941132697632576,0.529388645275596,0.9425930099919333,
    1.2345113855705605,0.29093411659111756,0.800363767645574,0.5766059381215861,0.6692187265125815,
    0.20447619833715736,0.49035079695672834,0.16071566219449587,0.6181761242554693,0.2267230146767822,
    0.12765259697091694,0.41677556632417445,0.4059483378259152,0.8440600080403166,0.20514995369266417,
    0.4942728850503481,0.26190221637225936,0.26275727790533004,0.9032343024104332,0.7268230941680275,
    0.286034486110887,1.4472051241771995,1.0248139974432968,0.275316269565675,0.23313051172217203,
    1.4110065473371882,0.4366764752390559,0.7648443462094332,0.17455770282334382,0.2530796200266882,
    0.2894980917784592,1.3011788730484601,0.5665417375334098,0.2566793308738982,0.32048779836743785,
    0.23663945218855686,0.5998659518183862,0.2678331031882215,0.5032930931900615,0.6874786603810399,
    0.3214440324208265,0.14211011008082958,0.819017807409365,0.4105961256971879,0.5740438256003082,
    0.21131695936725742,0.3306741868864489,0.3793284722734561,0.7089210134356432,2.128146114762274,
    0.2130429847932904,1.6613597787317225,0.22276444863059733,0.2896374900002532,6.6784395851634635,
    0.3039549833938396,0.28494128883588377,0.40624286664406933,0.22108556498133647,0.507836372617378,
    0.18205988265034304,0.2673444641589703,0.9012350282744386,0.3236899952843548,0.7274713901255461]);
  const f = api.betapr.fit(x,{shape1: .7});
  assert.ok(Math.abs(f.estimates.shape2/95.516952-1)<1e-6);
  assert.ok(Math.abs(f.estimates.scale/78.232314-1)<1e-6);
  assert.ok(Math.abs(f.logLik+58.0668187021122)<1e-8);
  const fallback=api.betapr.fit(x,{shape1: .66110553857791232},{iterlim:64});
  assert.equal(fallback.status,'Ok');assert.equal(fallback.certified,0);
  assert.ok(Number.isFinite(fallback.logLik));
});
test('binomial relatives prove their Poisson-type limits with finite witnesses', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  for(const [family,x,held] of [
    ['ztbinom',[1,1,1,2,3,7,12],{}],
    ['zmbinom',[0,0,1,1,1,2,3,7,12],{p0:.2}],
    ['zib',[0,0,0,0,0,0,0,0,1,1,1,2,3,7,12],{pi:.2}],
    ['tbinom',[1,2,3],{}]
  ]) {
    assert.throws(() => api[family].fit(new Float64Array(x),held),error => {
      assert.equal(error.kind,'limit');
      assert.ok(Number.isFinite(error.loglik_upper));
      assert.ok(error.candidates.length>=2);
      return true;
    });
  }
});

test('binomial probability solves stop numerically before returning an unresolved coefficient', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  for(const [family,face,values] of [['ztbinom','ztpois',[1,2,3]],['zib','zip',[0,0,0,0,1,2,3,4]]]) {
    const x=new Float64Array(values);
    assert.throws(() => api[family].fit(x,{size:1e100}),error => {
      assert.equal(error.kind,'numerical');
      assert.equal(error.loglik_upper,Infinity);
      assert.equal(error.candidates.length,1);
      return true;
    });
    const f=api[family].fit(x,{size:1e100},{iterlim:1000});
    const limiting=api[face].fit(x);
    assert.ok(Math.abs(f.estimates.prob*1e100-limiting.estimates.lambda)<1e-9);
  }
});

test('SGED fixed-shape free-skew certificates share core routing', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  const x = new Float64Array([-4,-1,0,0,0,0,0,0,.3,2]);
  const fit = api.sged.fit(x,{nu:.5});
  assert.ok(Math.abs(fit.logLik+5.836273250972211)<2e-7);
  assert.ok(Math.abs(fit.estimates.xi-Math.sqrt(.05211963387596999/.06917655828196827))<1e-10);
  assert.throws(()=>api.sged.fit(new Float64Array([-1,1]),{nu:.5}),error=>{
    assert.equal(error.kind,'tie');assert.equal(error.candidates.length,2);
    assert.ok(Number.isFinite(error.loglik_upper));return true;
  });
  const y = new Float64Array([-3,-1,-.5,0,.1,.2,.4,1,2,4]);
  for(const held of [{},{mean:.2},{sd:1.4},{mean:.2,sd:1.4}]) {
    let normal;
    try { normal=api.snorm.fit(y,held,{iterlim:1000}); }
    catch(error) {
      assert.throws(()=>api.sged.fit(y,{...held,nu:2},{iterlim:1000}));
      continue;
    }
    const sged=api.sged.fit(y,{...held,nu:2},{iterlim:1000});
    assert.ok(Math.abs(sged.logLik-normal.logLik)<1e-10);
    for(const name of ['mean','sd','xi'])assert.ok(Math.abs(sged.estimates[name]-normal.estimates[name])<1e-10);
  }
});

test('SGED normal face conversion reports scale overflow numerically', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  assert.throws(()=>api.sged.fit(new Float64Array([0,0,0]),{mean:1.1e308,nu:2}),error=>{
    assert.equal(error.kind,'numerical');assert.equal(error.candidates.length,0);
    assert.equal(error.loglik_upper,Infinity);return true;
  });
});

test('SGED normal face conversion rejects rounded subnormal scales', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  assert.throws(()=>api.sged.fit(new Float64Array([0,2**(-1047)]),{nu:2}),error=>{
    assert.equal(error.kind,'numerical');assert.equal(error.candidates.length,0);
    assert.equal(error.loglik_upper,Infinity);return true;
  });
});
test('bnbinom alpha-held global certificate keeps both labels and the diagonal witness',async()=>{
  const api=await (await import('./index.mjs')).default();
  const x=read('bnbinom','golden').data;
  const f=api.bnbinom.fit(x,{alpha:2});
  assert.equal(f.status,'Ok');
  assert.ok(Math.abs(f.estimates.size-1.15097395202066)<2e-8);
  assert.ok(Math.abs(f.estimates.beta-4.50374529147196)<5e-8);
  assert.deepEqual(Array.from(f.swap_maximum),[f.estimates.beta,2,f.estimates.size]);
  assert.equal(f.diagonal_witness[0],f.diagonal_witness[2]);
  const seamLL=api.bnbinom.d(x,f.diagonal_witness,{log:true}).reduce((a,b)=>a+b,0);
  assert.ok(seamLL<f.logLik-.1);
  const d=api.bnbinom.fit([0,0,1,2,3,6,12,24],{alpha:.5});
  assert.ok(Math.abs(d.estimates.size-1.1088158545389952)<2e-8);
  assert.ok(Math.abs(d.estimates.beta-d.estimates.size)<1e-8);
  assert.throws(()=>api.bnbinom.fit([0,0,1,2,3,6,12,24],{alpha:2},{iterlim:1}),e=>{
    assert.equal(e.kind,'budget');assert.ok(e.candidates.length>0);
    assert.ok(e.loglik_upper>=e.loglik_lower);return true;
  });
});

test('bnbinom alpha-held range exhaustion has numerical stop details',async()=>{
  const api=await (await import('./index.mjs')).default();
  assert.throws(()=>api.bnbinom.fit([0,0,1,2,3,6,12,24],{alpha:1e308}),e=>{
    assert.equal(e.kind,'numerical');
    assert.ok(e.candidates.every(c=>Number.isFinite(c.loglik_lower)&&Number.isFinite(c.loglik_upper)));
    return true;
  });
});
test('genbeta held p one shares the Kumaraswamy certificate and stop transport', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  const u = [.02,.2,.6,.9];
  const parent = api.kumar.fit(new Float64Array(u));
  for (const scale of [1e-200,1,7,1e200]) {
    const f = api.genbeta.fit(new Float64Array(u.map(v=>scale*v)),{scale,shape1:1});
    assert.ok(Math.abs(f.estimates.shape3/parent.estimates.a-1)<1e-6);
    assert.ok(Math.abs(f.estimates.shape2/parent.estimates.b-1)<1e-6);
    assert.ok(Math.abs(f.logLik+u.length*Math.log(scale)-parent.logLik)<1e-9);
  }
  const k = api.kumar.fit(new Float64Array(u),{},{iterlim:1});
  const g = api.genbeta.fit(new Float64Array(u),{scale:1,shape1:1},{iterlim:1});
  assert.ok(Math.abs(g.estimates.shape3/k.estimates.a-1)<1e-6);
  assert.throws(()=>api.genbeta.fit(new Float64Array(u),{scale:1,shape1:2,shape2:.7},{gradtol:1e-20}),e=>{
    assert.equal(e.kind,'numerical');assert.equal(e.candidates.length,1);
    assert.equal(e.loglik_upper,Infinity);return true;
  });
});

test('genbeta preserves a parent budget without a finite witness', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  const x = new Float64Array([.5,.500000000001]);
  assert.throws(()=>api.genbeta.fit(x,{scale:1,shape1:1},{iterlim:1}),e=>{
    assert.equal(e.kind,'budget');assert.equal(e.candidates.length,0);return true;
  });
  assert.throws(()=>api.genbeta.fit(x,{scale:1,shape1:1}),e=>{
    assert.equal(e.kind,'numerical');assert.equal(e.candidates.length,1);
    assert.equal(e.candidates[0].shape1,1);assert.equal(e.candidates[0].scale,1);return true;
  });
});

test('genbeta held p uses the complete power-profile certificate', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  const x = new Float64Array(read('genbeta','golden').data);
  for (const shape1 of [.1,.4,.5,2,10]) {
    const f = api.genbeta.fit(x,{scale:1,shape1});
    assert.equal(f.estimates.shape1,shape1);
    assert.ok(Number.isFinite(f.logLik));
    const held = api.genbeta.fit(x,{scale:1,...f.estimates});
    assert.ok(Math.abs(held.logLik-f.logLik)<1e-8);
  }
  assert.throws(()=>api.genbeta.fit(x,{scale:1,shape1:.4},{iterlim:1}),e=>{
    assert.equal(e.kind,'numerical');assert.match(e.message,/shape solve unresolved/);return true;
  });
});

test('genbeta rounded log ties are a numerical stop', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  const v = Math.exp(-700),x = new Float64Array([v,v*(1+Number.EPSILON)]);
  assert.notEqual(x[0],x[1]);
  assert.throws(()=>api.genbeta.fit(x,{scale:1,shape1:.4}),e=>{
    assert.equal(e.kind,'numerical');assert.match(e.message,/arithmetic unresolved/);return true;
  });
});
test('GEV and Frechet half-minimum certificates distinguish attainment', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  for(const family of ['gev','frechet']) {
    assert.throws(() => api[family].fit(new Float64Array([0,0,1,2]),{}), e => {
      assert.equal(e.kind,'nomle');
      assert.ok(Math.abs(e.loglik_upper-(4*Math.log(2)-4-2*Math.log(2)))<1e-12);
      assert.equal(e.loglik_lower,e.loglik_upper);
      return true;
    });
  }
  const f=api.gev.fit(new Float64Array([0,0,2,2]));
  assert.deepEqual(f.estimates,{mu:1,sigma:1,xi:-1});
  assert.equal(f.logLik,-4);
});

test('GEV shape-only certificate handles flat shapes and typed solve failure', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  const f=api.gev.fit(new Float64Array([4,4,4]),{mu:4,sigma:2});
  assert.equal(f.estimates.xi,0);
  assert.throws(() => api.gev.fit(new Float64Array([-.5,-.3,0,.2,.5]),{mu:0,sigma:1},{iterlim:1}), e => {
    assert.equal(e.kind,'numerical');assert.equal(e.loglik_upper,Infinity);return true;
  });
});

test('GEV held-minimum half ties retain the attained member', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api=await createApi();
  assert.throws(() => api.gev.fit(new Float64Array([0,0,1,2]),{mu:0}),e => {
    assert.equal(e.kind,'nomle');assert.ok(Math.abs(e.loglik_upper+2+2*Math.log(2))<1e-12);return true;
  });
  for(const held of [{mu:0},{mu:0,sigma:2}]) {
    const f=api.gev.fit(new Float64Array([0,0,2,2]),held);
    assert.deepEqual(f.estimates,{mu:0,sigma:2,xi:-1});
    assert.ok(Math.abs(f.logLik+2+4*Math.log(2))<1e-12);
  }
});

test('GEV two-point count certificate avoids a rounded sample mean', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api=await createApi();
  const f=api.gev.fit(new Float64Array([0,...Array(100).fill(1)]));
  assert.equal(f.estimates.xi,-1);
  assert.ok(Math.abs(f.estimates.sigma*101-1)<1e-10);
  assert.ok(Math.abs(f.estimates.mu-100/101)<1e-10);
  const centered=api.gev.fit(new Float64Array([-9,...Array(9).fill(1)]));
  assert.equal(centered.certified,0);
  assert.equal(centered.estimates.mu,0);assert.equal(centered.estimates.sigma,1);
  assert.equal(centered.estimates.xi,-1);
});
test('EMG certified conditional routes and numerical location stops', async () => {
  const {default:createApi}=await import('./index.mjs');
  const api=await createApi();
  const x=new Float64Array([-2,-1,0,1,2]);
  const f=api.emg.fit(x,{sigma:1,lambda:1},{reltol:1e-10});
  assert.equal(f.status,'Ok');
  assert.equal(f.estimates.sigma,1);assert.equal(f.estimates.lambda,1);
  for(const controls of [{iterlim:1},{reltol:1e-30}]) {
    assert.throws(()=>api.emg.fit(x,{sigma:1,lambda:1},controls),e=>{
      assert.equal(e.kind,'numerical');assert.ok(e.candidates.length>0);
      assert.ok(e.loglik_lower<=e.loglik_upper);return true;
    });
  }
  assert.throws(()=>api.emg.fit(x,{mu:2}),e=>{
    assert.equal(e.kind,'limit');assert.equal(e.model,'norm');
    assert.equal(e.estimates.mean,2);assert.equal(e.estimates.sd,Math.sqrt(6));return true;
  });
  assert.throws(()=>api.emg.fit(new Float64Array([4,4,4]),{mu:4,lambda:2}),e=>{
    assert.equal(e.kind,'limit');assert.equal(e.limit_loglik,0);return true;
  });
});

test('EMG outer certificate preserves budget and unresolved witnesses', async () => {
  const {default:createApi}=await import('./index.mjs');
  const api=await createApi();
  const x=new Float64Array([-2,-1,0,1,2]);
  assert.throws(()=>api.emg.fit(x,{mu:0,sigma:1},{iterlim:2}),e=>{
    assert.equal(e.kind,'budget');assert.match(e.message,/raising iterlim/);
    assert.ok(e.candidates.length>=2);assert.ok(e.loglik_lower<=e.loglik_upper);return true;
  });
  assert.throws(()=>api.emg.fit(x,{mu:0}),e=>{
    assert.equal(e.kind,'unresolved');assert.ok(e.candidates.length>=2);
    assert.ok(e.loglik_lower<=e.loglik_upper);return true;
  });
});
test('bbinom signed face and small finite gains agree with the shared fitter',async()=>{
  const api=await (await import('./index.mjs')).default();
  assert.throws(()=>api.bbinom.fit([0,1,1,2],{size:2}),e=>{
    assert.equal(e.kind,'limit');assert.equal(e.model,'binom');
    assert.equal(e.estimates.size,2);assert.equal(e.estimates.prob,.5);
    assert.ok(e.candidates.length>0);return true;
  });
  const saturated=api.bbinom.fit([0,0,1,2,2],{size:2});
  assert.ok(Math.abs(saturated.estimates.alpha-1/3)<1e-12);
  assert.ok(Math.abs(saturated.estimates.beta-1/3)<1e-12);
  const x=new Float64Array(100000);let offset=0;
  for(const [v,n] of [[0,12501],[1,37499],[2,37499],[3,12501]]) {
    x.fill(v,offset,offset+n);offset+=n;
  }
  const fit=api.bbinom.fit(x,{size:3});
  assert.ok(Math.abs(fit.estimates.alpha/18749.5-1)<1e-6);
  assert.ok(Math.abs(fit.estimates.beta/18749.5-1)<1e-6);
});
test('GED analytic held-sd shape fits and median ties agree across bindings', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  for (const held of [{sd:1},{mean:2,sd:1}]) {
    const fit=api.ged.fit(new Float64Array([2,2,2]),held,{iterlim:1});
    assert.equal(fit.estimates.mean,2);
    assert.equal(fit.estimates.nu,1);
    assert.ok(Math.abs(fit.logLik+1.5*Math.log(2))<1e-12);
  }
  const odd=api.ged.fit(new Float64Array([-3,-1,0,1,4]),{sd:.1},{iterlim:1});
  assert.equal(odd.estimates.mean,0);
  assert.equal(odd.estimates.nu,1);
  assert.throws(()=>api.ged.fit(new Float64Array([-1,1]),{sd:.1},{iterlim:1}),error=>{
    assert.equal(error.kind,'tie');
    assert.match(error.message,/median interval/);
    assert.deepEqual(error.candidates.map(c=>c.mean),[-1,1]);
    assert.ok(Number.isFinite(error.loglik_upper));
    assert.ok(error.loglik_lower<=error.loglik_upper);
    return true;
  });
});

test('GED a completed low-budget certificate can prove the Laplace face', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  const x=new Float64Array([-3,-1,0,1,4]);
  const fit=api.ged.fit(x,{mean:0,sd:1},{iterlim:1});
  assert.equal(fit.estimates.nu,1);
  const ll=Array.from(api.ged.d(x,[0,1,1],{log:true})).reduce((a,b)=>a+b,0);
  assert.ok(Math.abs(fit.logLik-ll)<1e-10);
});

test('GED rounded held-sd endpoints cannot select a uniform face', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  for (const held of [{sd:1},{mean:0,sd:1}]) {
    assert.throws(()=>api.ged.fit(new Float64Array([-Math.sqrt(3),Math.sqrt(3)]),held),error=>{
      assert.equal(error.kind,'numerical');
      assert.ok(error.candidates.length>0);
      assert.ok(error.loglik_upper>=-2*Math.log(2*Math.sqrt(3)));
      return true;
    });
  }
});

test('GED constant data away from a held mean have a uniform limit', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  assert.throws(()=>api.ged.fit(new Float64Array([2,2,2]),{mean:0},{iterlim:1}),error=>{
    assert.equal(error.kind,'limit');
    assert.equal(error.model,'unif');
    assert.deepEqual([error.estimates.min,error.estimates.max],[-2,2]);
    assert.ok(Math.abs(error.limit_loglik+3*Math.log(4))<1e-12);
    return true;
  });
});

test('GED free-scale uniform stops include certified global bounds', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  const x=Float64Array.from({length:40},(_,j)=>-1+2*j/39);
  for (const held of [{},{mean:0}]) {
    assert.throws(()=>api.ged.fit(x,held),error=>{
      assert.equal(error.kind,'limit');
      assert.ok(Number.isFinite(error.loglik_upper));
      assert.ok(error.loglik_lower<=-40*Math.log(2));
      assert.ok(error.loglik_upper>=-40*Math.log(2));
      return true;
    });
  }
});

test('GED finite free-shape golden subsets pass the global certificate', async () => {
  const {default:createApi}=await import('./index.mjs');const api=await createApi();
  const g=read('ged','golden'),x=new Float64Array(g.data);
  for(const held of [{},{mean:g.estimates.mean},{sd:g.estimates.sd},{mean:g.estimates.mean,sd:g.estimates.sd}]) {
    const fit=api.ged.fit(x,held);
    assert.ok(Math.abs(fit.estimates.nu-g.estimates.nu)<1e-5);
    assert.ok(Math.abs(fit.logLik-g.logLik)<1e-7);
  }
});

test('GED overlapping finite shape roots retain an uncertified search', async () => {
  const {default:createApi}=await import('./index.mjs');const api=await createApi();
  const values=Array(8).fill(0);
  for(let j=0;j<23;j++)values.push(-.32998660809120006,.32998660809120006);
  for(let j=0;j<12;j++)values.push(-1.4239025460520294,1.4239025460520294);
  for(const held of [{mean:0,sd:1},{sd:1}]) {
    const f=api.ged.fit(new Float64Array(values),held);
    assert.equal(f.status,'Ok');assert.equal(f.certified,0);
    assert.ok(Number.isFinite(f.logLik));
  }
});

test('GED exhausted finite root certificates stop on budget and retain witnesses', async () => {
  const {default:createApi}=await import('./index.mjs');const api=await createApi();
  const g=read('ged','golden'),x=new Float64Array(g.data),held={mean:g.estimates.mean};
  assert.throws(()=>api.ged.fit(x,held,{iterlim:1}),error=>{
    assert.equal(error.kind,'budget');
    assert.ok(error.candidates.length>0);
    assert.ok(error.loglik_lower<=error.loglik_upper);
    return true;
  });
  const fit=api.ged.fit(x,held,{iterlim:200});
  assert.ok(Math.abs(fit.estimates.nu-g.estimates.nu)<1e-5);
});

test('GED inverse normalization rejects a rounded location with a different likelihood', async () => {
  const {default:createApi}=await import('./index.mjs');const api=await createApi();
  const g=read('ged','golden'),x=Float64Array.from(g.data,v=>1e16+10*v);
  for(const held of [{},{nu:2}]) {
    assert.throws(()=>api.ged.fit(x,held),error=>{
      assert.equal(error.kind,'numerical');
      assert.match(error.message,/normalization/);
      assert.ok(error.candidates.length>0);
      return true;
    });
  }
});


test('GED rounded residual ties cannot select an invalid uniform support', async () => {
  const {default:createApi}=await import('./index.mjs');const api=await createApi();
  const u=2**-53;
  assert.throws(()=>api.ged.fit(new Float64Array([-1+u,1+2*u]),{mean:u}),error=>{
    assert.equal(error.kind,'numerical');
    assert.match(error.message,/normalization/);
    assert.ok(!error.estimates || Object.keys(error.estimates).length===0);
    return true;
  });
});

test('GED Laplace median intervals retain the original observation endpoints', async () => {
  const {default:createApi}=await import('./index.mjs');const api=await createApi();
  const x=Float64Array.from([...Array.from({length:19},(_,j)=>j+1),1e308]);
  assert.throws(()=>api.ged.fit(x),error=>{
    assert.equal(error.kind,'tie');
    assert.deepEqual(error.candidates.map(c=>c.mean),[10,11]);
    return true;
  });
});

test('GED held-shape arithmetic losses produce numerical stops', async () => {
  const {default:createApi}=await import('./index.mjs');const api=await createApi();
  for(const [x,held] of [
    [[0,0,1e-320],{nu:.5}], [[0,0,1e-200],{nu:.02}],
    [[1e-100,1e-100],{mean:0,sd:1e300,nu:.02}]
  ]) assert.throws(()=>api.ged.fit(new Float64Array(x),held),error=>{
    assert.equal(error.kind,'numerical');assert.match(error.message,/accuracy/);return true;
  });
});
test('bnbinom alpha-free pair certificates transport the actual held value',async()=>{
  const api=await (await import('./index.mjs')).default();
  const x=read('bnbinom','golden').data;
  const a=api.bnbinom.fit(x,{size:1}),b=api.bnbinom.fit(x,{beta:1});
  assert.equal(a.estimates.size,1);
  assert.equal(b.estimates.beta,1);
  assert.equal(a.estimates.alpha,b.estimates.alpha);
  assert.equal(a.estimates.beta,b.estimates.size);
  for(const held of [{size:1},{beta:1}]) {
    assert.throws(()=>api.bnbinom.fit([0,0,1,2,3,6,12,24],held,{iterlim:1}),e=>{
      assert.equal(e.kind,'budget');assert.ok(e.candidates.length>=1);return true;
    });
    assert.throws(()=>api.bnbinom.fit([0,0,5,5],held),e=>{
      assert.equal(e.kind,'numerical');assert.ok(e.candidates.length>=2);return true;
    });
  }
});

test('bnbinom supporting-polynomial proofs select reachable faces',async()=>{
  const api=await (await import('./index.mjs')).default();
  for(const x of [[2,2,2],[0,0,1,1,1],[0,1,2,3,4]]) {
    for(const held of [{size:1},{beta:1}])
      assert.throws(()=>api.bnbinom.fit(x,held),e=>{assert.equal(e.kind,'limit');assert.equal(e.model,'nbinom');return true;});
    assert.throws(()=>api.bnbinom.fit(x),e=>{assert.equal(e.kind,'limit');assert.equal(e.model,'pois');return true;});
  }
});

test('bnbinom NB delegate precision failures remain structured numerical stops',async()=>{
  const api=await (await import('./index.mjs')).default();
  for(const held of [{size:1e308},{beta:1e308}])
    assert.throws(()=>api.bnbinom.fit([0,1,2,4],held),e=>{
      assert.equal(e.kind,'numerical');assert.equal(e.loglik_upper,0);return true;
    });
});
test('signed binomial face certificates have finite global bounds', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  for(const [family,x,held,model] of [
    ['ztbinom',[1,1,1,2,2,8],{},'ztpois'],
    ['zmbinom',[0,0,1,1,1,2,2,8],{p0:.2},'zmpois'],
    ['tbinom',[0,1,1,2,2],{a:-1,b:2},'tpois'],
    ['zib',[0,0,0,0,0,1,1,2,2,4],{pi:.2},'zip']
  ]) {
    assert.throws(() => api[family].fit(new Float64Array(x),held),error => {
      assert.equal(error.kind,'limit');
      assert.equal(error.model,model);
      assert.ok(Number.isFinite(error.loglik_upper));
      assert.ok(error.loglik_upper>=error.loglik_lower);
      assert.ok(error.candidates.length>=1);
      if(family==='tbinom')assert.equal(error.estimates.lambda,2);
      if(family==='zib') {
        assert.equal(error.estimates.pi,.2);
        assert.ok(Math.abs(error.estimates.lambda-1.3184257723276573)<1e-9);
      }
      return true;
    });
  }
});

test('higher-order binomial face signs retain proved stops or uncertified fits', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  const q=Math.exp(-1.6),pi=.75*q/(.25+.75*q);
  for(const [family,x,held] of [
    ['tbinom',[...Array(50).fill(0),...Array(41).fill(1),...Array(33).fill(2),...Array(3).fill(3),...Array(3).fill(4)],{a:-1,b:4}],
    ['zib',[0,0,0,0,0,1,1,2,2,4],{pi}]
  ]) {
    if(family==='zib') {
      const f=api[family].fit(new Float64Array(x),held);
      assert.equal(f.status,'Ok');assert.equal(f.certified,0);
      assert.equal(f.estimates.pi,pi);assert.ok(Number.isFinite(f.logLik));
      continue;
    }
    assert.throws(() => api[family].fit(new Float64Array(x),held),error => {
      assert.equal(error.kind,family==='tbinom'?'limit':'unresolved');
      if(family==='tbinom')assert.ok(Number.isFinite(error.loglik_upper));
      else assert.equal(error.loglik_upper,Infinity);
      assert.ok(Number.isFinite(error.loglik_lower));
      assert.ok(error.candidates.length>=2);
      return true;
    });
  }
});

test('two-point binomial windows attain a finite member despite their face tie', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  const fit=api.tbinom.fit(new Float64Array([2,2,3,3]),{a:1,b:3});
  assert.equal(fit.estimates.size,3);
  assert.ok(Math.abs(fit.logLik+4*Math.log(2))<1e-10);
});

test('exact factorial moments certify a nonsaturated truncated Poisson face', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  const x=[0,...Array(9).fill(1),...Array(6).fill(2),...Array(10).fill(3)];
  assert.throws(() => api.tbinom.fit(new Float64Array(x),{a:-1,b:3}),error => {
    assert.equal(error.kind,'limit');
    assert.equal(error.estimates.lambda,3);
    assert.ok(Number.isFinite(error.loglik_upper));
    return true;
  });
});

test('globally winning tied ZIP faces retain both rate candidates', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  assert.throws(() => api.zib.fit(new Float64Array([...Array(198).fill(0),1,19]),{pi:.7254971052291191}),error => {
    assert.equal(error.kind,'tie');
    assert.equal(error.model,'zip');
    assert.ok(Number.isFinite(error.loglik_upper));
    assert.ok(error.candidates.length>=3);
    return true;
  });
});

test('noninflated ZIP faces include the equal-moment binomial escape', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  for(const x of [[0,1,1,1,2,4],[0,1,1,1,2,3,4,4]]) {
    assert.throws(() => api.zib.fit(new Float64Array(x)),error => {
      assert.equal(error.kind,'limit');
      assert.equal(error.estimates.pi,0);
      assert.ok(Number.isFinite(error.loglik_upper));
      return true;
    });
  }
});

test('ZIB subnormal held inflation preserves log masses and its probability root', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  const mix=Number.MIN_VALUE;
  const f=api.zib.fit(new Float64Array([0,500]),{size:1075,prob:.5,pi:mix});
  assert.ok(Math.abs(f.logLik+750.36663618821149)<1e-9);
  const g=api.zib.fit(new Float64Array([...Array(1000).fill(535),0]),{size:1075,pi:mix});
  assert.ok(Math.abs(g.estimates.prob/.4971795798757613128165584136-1)<1e-10);
  assert.ok(Math.abs(g.logLik+4455.659181549239638934)<1e-7);
  const tiny=api.zib.fit(new Float64Array([0]),{size:1,prob:1e-300,pi:.5});
  assert.ok(tiny.logLik<0);
  assert.ok(Math.abs(tiny.logLik/(-5e-301)-1)<1e-10);
});

test('ZIB unrepresentable finite log likelihoods stop numerically', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  assert.throws(() => api.zib.fit(new Float64Array([0]),{size:1e308,prob:.9,pi:0}),error => {
    assert.equal(error.kind,'numerical');
    assert.equal(error.loglik_upper,Infinity);
    return true;
  });
});

test('ZIB tiny held probabilities preserve the unique all-zero inflation', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  const f=api.zib.fit(new Float64Array([0]),{size:1,prob:1e-300});
  assert.equal(f.estimates.pi,1);
  assert.equal(f.logLik,0);
});
test('Bhattacharjee held certificates and quantitative numerical stops', async () => {
  const {default:createApi}=await import('./index.mjs');
  const api=await createApi();
  const x=new Float64Array([-3,-1,0,1,2]);
  const f=api.bhatt.fit(x,{sigma:1,a:2},{reltol:1e-10});
  assert.equal(f.status,'Ok');assert.equal(f.estimates.sigma,1);assert.equal(f.estimates.a,2);
  for(const controls of [{iterlim:1}]) {
    assert.throws(()=>api.bhatt.fit(x,{sigma:1,a:2},controls),e=>{
      assert.equal(e.kind,'numerical');assert.ok(e.candidates.length>0);
      assert.ok(e.loglik_lower<=e.loglik_upper);return true;
    });
  }
  assert.throws(()=>api.bhatt.fit(x,{sigma:1,a:2},{reltol:1e-30}),e=>{
    assert.equal(e.kind,'numerical');assert.match(e.message,/numerical accuracy failure/i);
    assert.ok(e.candidates.length>0);
    const best=e.candidates.reduce((a,b)=>a.loglik_lower>b.loglik_lower?a:b);
    assert.ok(Number.isFinite(best.mu));assert.equal(best.sigma,1);assert.equal(best.a,2);
    assert.ok(Number.isFinite(best.loglik_lower)&&Number.isFinite(best.loglik_upper));
    assert.ok(best.loglik_lower<=best.loglik_upper);
    assert.ok(best.loglik_lower<=f.logLik+1e-11&&best.loglik_upper>=f.logLik-1e-11);
    return true;
  });
  const normal=api.bhatt.fit(new Float64Array([-10,...Array(20).fill(0),10]),{mu:0,sigma:4});
  assert.equal(normal.certified,1);
  assert.deepEqual(normal.estimates,{mu:0,sigma:4,a:0});
});

test('Bhattacharjee normal member distinguishes residual underflow and precision', async () => {
  const {default:createApi}=await import('./index.mjs');
  const api=await createApi();
  const f=api.bhatt.fit(new Float64Array([1e-200,2e-200]),{mu:0,a:0},{reltol:1e-10});
  assert.equal(f.status,'Ok');assert.ok(Number.isFinite(f.logLik));
  assert.ok(Math.abs(f.estimates.sigma/1e-200-Math.sqrt(2.5))<1e-12);
  assert.throws(()=>api.bhatt.fit(new Float64Array([1e16,1e16+2]),{a:0},{reltol:1e-10}),e=>{
    assert.equal(e.kind,'numerical');assert.ok(e.candidates.length>0);return true;
  });
});

test('Bhattacharjee all-held evaluation keeps a finite extreme log likelihood', async () => {
  const {default:createApi}=await import('./index.mjs');
  const api=await createApi();
  const f=api.bhatt.fit(new Float64Array([1e154,1e154]),{mu:0,sigma:1,a:0});
  assert.equal(f.status,'Ok');assert.ok(Number.isFinite(f.logLik));
  assert.ok(Math.abs(f.logLik/1e308+1)<1e-14);
});

test('Bhattacharjee physical comparisons and stopped likelihood witnesses', async () => {
  const {default:createApi}=await import('./index.mjs');
  const api=await createApi();
  try {
    const f=api.bhatt.fit(new Float64Array([1]),{mu:-Number.EPSILON/4,sigma:1},{reltol:1e-10});
    assert.ok(f.estimates.a>0,'an exterior distance cannot certify the normal member');
  } catch(e) {assert.ok(['unresolved','numerical','budget','tie'].includes(e.kind));}
  const x=new Float64Array([1e16,1e16+2]);
  assert.throws(()=>api.bhatt.fit(x,{sigma:1},{reltol:1e-10}),e=>{
    assert.ok(e.candidates.length>0);
    for(const c of e.candidates)if(c.model==='') {
      const ll=api.bhatt.d(x,[c.mu,c.sigma,c.a],{log:true}).reduce((a,b)=>a+b,0);
      assert.ok(c.loglik_lower<=ll&&ll<=c.loglik_upper);
    }
    return true;
  });
});
test('binomial relatives retain proved finite and Poisson witnesses', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  for(const [family,x,held] of [
    ['ztbinom',[1,1,1,2,3,7,12],{}],
    ['zmbinom',[0,0,1,1,1,2,3,7,12],{p0:.2}],
    ['zib',[0,0,0,0,0,0,0,0,1,1,1,2,3,7,12],{pi:.2}],
    ['tbinom',[1,2,3],{}]
  ]) {
    assert.throws(() => api[family].fit(new Float64Array(x),held),error => {
      assert.equal(error.kind,'limit');
      assert.ok(Number.isFinite(error.loglik_upper));
      assert.ok(error.candidates.length>=2);
      return true;
    });
  }
});

test('binomial relatives certify Poisson limits and retain finite witnesses', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  for(const [family,x,held,model] of [
    ['ztbinom',[1,1,1,2,3,7,12],{},'ztpois'],
    ['zmbinom',[0,0,1,1,1,2,3,7,12],{p0:.2},'zmpois'],
    ['zib',[0,0,0,0,0,0,0,0,1,1,1,2,3,7,12],{pi:.2},'zip'],
    ['tbinom',[1,2,3],{},'tpois']
  ]) {
    assert.throws(() => api[family].fit(new Float64Array(x),held),error => {
      assert.equal(error.kind,'limit');
      assert.equal(error.model,model);
      assert.ok(Number.isFinite(error.loglik_lower));
      assert.ok(Number.isFinite(error.loglik_upper));
      assert.ok(error.loglik_lower<=error.limit_loglik);
      assert.ok(error.limit_loglik<=error.loglik_upper);
      assert.ok(error.loglik_upper-error.loglik_lower<1e-7);
      assert.ok(error.candidates.some(c=>c.model===model));
      assert.ok(error.candidates.some(c=>c.model===''));
      return true;
    });
  }
});

test('binomial relatives retain certified Poisson limit witnesses', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  for(const [family,x,held,model] of [
    ['ztbinom',[1,1,1,2,3,7,12],{},'ztpois'],
    ['zmbinom',[0,0,1,1,1,2,3,7,12],{p0:.2},'zmpois'],
    ['zib',[0,0,0,0,0,0,0,0,1,1,1,2,3,7,12],{pi:.2},'zip'],
    ['tbinom',[1,2,3],{},'tpois']
  ]) {
    assert.throws(() => api[family].fit(new Float64Array(x),held),error => {
      assert.equal(error.kind,'limit');
      assert.equal(error.model,model);
      assert.ok(Number.isFinite(error.loglik_upper));
      assert.ok(Number.isFinite(error.limit_loglik));
      assert.ok(error.limit_loglik<=error.loglik_upper+1e-8);
      assert.ok(error.candidates.length>=2);
      return true;
    });
  }
});

test('zib quantiles stop on extreme numerical failure',()=>{
 const q=m.qzib(new Float64Array([.5]),[1e308,.9,0],true,false)[0];
 assert.ok(Number.isNaN(q));
 assert.equal(m.qzib(new Float64Array([.5]),[1e308,1,0],true,false)[0],1e308);
 assert.equal(m.qzib(new Float64Array([Math.log(.5)]),[10,1,.5],true,true)[0],10);
});
test('genbeta held q uses certified finite maxima and terminal neighborhood stops', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  const x = new Float64Array(read('genbeta','golden').data);
  for (const shape2 of [.01,.1,.3]) {
    const f = api.genbeta.fit(x,{scale:1,shape2});
    assert.equal(f.estimates.shape2,shape2);
    assert.ok(Number.isFinite(f.logLik));
  }
  assert.throws(()=>api.genbeta.fit(x,{scale:1,shape2:.7}),e=>{
    assert.equal(e.kind,'numerical');
    assert.match(e.message,/gamma-limit neighborhood unresolved/);
    return true;
  });
});
test('SGED symmetric free-shape delegate transports fits and structured stops', async () => {
  const {default:createApi}=await import('./index.mjs');
  const api=await createApi(),g=read('ged','golden'),x=new Float64Array(g.data);
  for(const held of [{},{mean:g.estimates.mean},{sd:g.estimates.sd},{mean:g.estimates.mean,sd:g.estimates.sd}]) {
    const a=api.sged.fit(x,{...held,xi:1}),b=api.ged.fit(x,held);
    assert.equal(a.status,'Ok');
    for(const name of ['mean','sd','nu'])assert.equal(a.estimates[name],b.estimates[name]);
    assert.equal(a.estimates.xi,1);assert.equal(a.logLik,b.logLik);
  }
  assert.throws(()=>api.sged.fit(new Float64Array([-1,1]),{sd:.1,xi:1},{iterlim:1}),e=>{
    assert.equal(e.kind,'tie');assert.equal(e.candidates.length,2);
    for(const c of e.candidates){assert.equal(c.xi,1);assert.equal(c.nu,1);}
    return true;
  });
  assert.throws(()=>api.sged.fit(new Float64Array([-1,1]),{xi:1}),e=>{
    assert.equal(e.kind,'limit');assert.equal(e.model,'unif');
    assert.deepEqual(Object.values(e.estimates),[-1,1]);return true;
  });
  assert.throws(()=>api.sged.fit(new Float64Array([-Math.sqrt(3),Math.sqrt(3)]),{sd:1,xi:1}),e=>{
    assert.equal(e.kind,'numerical');assert.ok(e.candidates.length>0);
    for(const c of e.candidates)if(c.model==='')assert.equal(c.xi,1);
    return true;
  });
});
test('SGED balanced endpoint uniform proof covers asymmetric and free skew', async () => {
  const {default:createApi}=await import('./index.mjs');const api=await createApi();
  for(const held of [{},{xi:.1},{xi:2},{xi:100}])
    assert.throws(()=>api.sged.fit(new Float64Array([-2,-2,3,3]),held,{iterlim:1}),e=>{
      assert.equal(e.kind,'limit');assert.equal(e.model,'unif');
      assert.deepEqual(Object.values(e.estimates),[-2,3]);
      assert.ok(Math.abs(e.limit_loglik+4*Math.log(5))<1e-12);return true;
    });
});

test('SGED weighted free-shape and smooth free-skew profiles certify golden fits', async()=>{
  const {default:createApi}=await import('./index.mjs');const api=await createApi();
  const g=read('sged','golden'),x=new Float64Array(g.data);
  const weighted=api.sged.fit(x,{xi:1.4});
  assert.ok(Math.abs(weighted.estimates.nu-3.12977540545)<2e-8);
  assert.ok(Math.abs(weighted.logLik+649.7033316424115)<2e-8);
  for(const held of [{xi:1.4},{nu:1.7},{nu:4}]) {
    const f=api.sged.fit(x,held);assert.equal(f.status,'Ok');
    const params=['mean','sd','nu','xi'].map(k=>f.estimates[k]);
    const actual=api.sged.d(x,params,{log:true}).reduce((a,b)=>a+b,0);
    assert.ok(Math.abs(actual-f.logLik)<2e-7);
    assert.throws(()=>api.sged.fit(x,held,{iterlim:1}),e=>{
      assert.ok(['budget','numerical'].includes(e.kind));assert.ok(e.candidates.length>0);return true;
    });
  }
});
test('SGED weighted median and smooth reflected half-GED ties retain their witnesses', async()=>{
  const {default:createApi}=await import('./index.mjs');const api=await createApi();
  assert.throws(()=>api.sged.fit(new Float64Array([-100,...Array(3).fill(-.01),...Array(15).fill(.01),100]),{xi:2}),e=>{
    assert.equal(e.kind,'tie');assert.ok(e.candidates.length>=2);
    for(const c of e.candidates){assert.equal(c.nu,1);assert.equal(c.xi,2);}return true;
  });
  for(const nu of [1.1,1.7,4,20])
    assert.throws(()=>api.sged.fit(new Float64Array([-1,1]),{nu}),e=>{
      assert.equal(e.kind,'tie');assert.ok(Number.isFinite(e.loglik_upper));
      assert.deepEqual(new Set(e.candidates.map(c=>c.model)),new Set(['unavailable:half-ged','unavailable:reflected-half-ged']));return true;
    });
});

test('CMB finite-size ties retain two distinct witnesses and nu-held census certifies',async()=>{
  const {default:createApi}=await import('./index.mjs');const api=await createApi();
  for(const [data,held] of [[[1,1],{prob:.5,nu:1}],[[0,0],{}],[[0,0],{nu:2}],[[0,0],{prob:0,nu:2}]])
    assert.throws(()=>api.cmb.fit(new Float64Array(data),held),e=>{
      assert.equal(e.kind,'tie');assert.match(e.message,/Distinct finite sizes/);
      assert.equal(e.candidates.length,2);
      assert.equal(new Set(e.candidates.map(c=>c.size)).size,2);
      assert.equal(e.loglik_upper,Infinity);return true;
    });
  const g=read('cmb','golden');
  const f=api.cmb.fit(new Float64Array(g.data),{nu:g.estimates.nu});
  assert.equal(f.status,'Ok');assert.equal(f.estimates.size,10);
  assert.ok(Math.abs(f.estimates.prob-g.estimates.prob)<1e-8);
  assert.ok(Math.abs(f.logLik-g.logLik)<1e-7);
});
test('heavy-tail inverse samplers replay finite quantiles beyond 2^53',()=>{
  const n=4096,seed=155;
  const uniforms=m.runif(n,[0,1],seed);
  for(const [family,shape] of [['zeta',1.1],['yulesimon',.1]]) {
    const draws=m[`r${family}`](n,[shape],seed);
    assert.ok([...draws].every(Number.isFinite));
    assert.ok([...draws].some(x=>x>2**53));
    assert.deepEqual(draws,m[`q${family}`](uniforms,[shape],true,false));
  }
});

test('large geometric quantiles preserve the supplied probability scale',()=>{
  const scale=1-2**-53;
  for(const [family,parameters] of [['dlaplace',[0,scale]],['cmp',[scale,0]]]) {
    for(const lower of [false,true]) for(const log of [false,true]) {
      const u=lower?.999:.001,p=log?Math.log(u):u;
      const h=m[`q${family}`](new Float64Array([p]),parameters,lower,log)[0];
      assert.ok(Number.isFinite(h)&&h>2**53);
      const f=m[`p${family}`](new Float64Array([h,previousDouble(h)]),parameters,lower,log);
      assert.ok(lower?f[0]>=p&&f[1]<p:f[0]<=p&&f[1]>p);
    }
  }
});

test('translated lattice normal and Laplace tails survive overflowing differences',()=>{
  for(const sign of [-1,1]) {
    const x=new Float64Array([sign*1e308]);
    for(const lower of [false,true]) for(const log of [false,true]) {
      const tail=((sign<0)===lower)?.0227501319481792072:.977249868051820793;
      const expected=log?Math.log(tail):tail;
      assert.ok(Math.abs(m.pdnorm(x,[-sign*1e308,1e308],lower,log)[0]-expected)<2e-14);
      const p=log?Math.log(.01):.01,mean=lower?1e308:-1e308;
      const h=m.qdnorm(new Float64Array([p]),[mean,1e308],lower,log)[0];
      assert.ok(Number.isFinite(h));
      const f=m.pdnorm(new Float64Array([h,previousDouble(h)]),[mean,1e308],lower,log);
      assert.ok(lower?f[0]>=p&&f[1]<p:f[0]<=p&&f[1]>p);
      assert.ok(Math.abs(h/1e308-(mean/1e308+(lower?-1:1)*2.3263478740408408))<2e-14);
    }
    assert.ok(Math.abs(m.ddnorm(x,[-sign*1e308,1e308],true,true)[0]+712.11514717537074343)<2e-13);
    const params=[-sign*1e308,1-2**-53],expected=-2.2204460492503132285e292;
    assert.ok(Math.abs(m.pdlaplace(x,params,sign<0,true)[0]/expected-1)<3e-15);
    assert.ok(Math.abs(m.ddlaplace(x,params,true,true)[0]/expected-1)<3e-15);
  }
});
test('skew-t and skew-GED retain large-shape gamma moments',()=>{
  for(const [nu,expected] of [[128,.36125492616400606],[1e4,.3597599586665302],
      [1e8,.359740838771626],[1e12,.35974083685996705],
      [1e16,.35974083685977598],[1e100,.35974083685977598],[Number.MAX_VALUE,.35974083685977598]]) {
    const value=m.dsstd(new Float64Array([0]),[0,1,nu,2],true,false)[0];
    assert.ok(Math.abs(value-expected)<5e-15);
    if(nu<=1e16) {
      const ps=new Float64Array([.1,.5,.9]);
      const qs=m.qsstd(ps,[0,1,nu,2],true,false);
      const back=m.psstd(qs,[0,1,nu,2],true,false);
      for(let i=0;i<3;i++)assert.ok(Math.abs(back[i]-ps[i])<1e-11);
    }
    // The largest shape is exercised in the bounded child process below.
    if(nu<Number.MAX_VALUE) assert.ok([...m.rsstd(10,[0,1,nu,2],31)].every(Number.isFinite));
  }
  for(const nu of [1e8,1e16,1e100,Number.MAX_VALUE]) {
    assert.ok(Math.abs(m.dsged(new Float64Array([0]),[0,1,nu,2],true,false)[0]-.28867513459481288225)<5e-15);
  }
});

test('large held skew-t shape retains a certified fit and finite skew covariance',async()=>{
  const {default:createApi}=await import('./index.mjs');const api=await createApi();
  const x=new Float64Array([-2,-1,0,1,3]);
  const fit=api.sstd.fit(x,{mean:0,sd:1,nu:1e12});
  assert.equal(fit.status,'Ok');assert.ok(fit.estimates.xi>1.2 && fit.estimates.xi<1.4);
  const covariance=api.sstd.vcov(x,fit);
  assert.ok(Number.isFinite(covariance.matrix[0][0]) && covariance.matrix[0][0]>0);
});

test('betapr all-free certificate returns verified fits and structured global stops', async () => {
  const {default:createApi}=await import('./index.mjs');const api=await createApi();
  const f=api.betapr.fit(new Float64Array([.03,.1,.4,.7,1,2,4,12]));
  assert.ok(Math.abs(f.estimates.shape1/.665253303-1)<1e-6);
  assert.ok(Math.abs(f.estimates.shape2/1.846932901-1)<1e-6);
  assert.throws(()=>api.betapr.fit(new Float64Array([1,2,3,4])),e=>{
    assert.equal(e.kind,'limit');assert.equal(e.model,'gamma');assert.ok(Number.isFinite(e.loglik_upper));return true;
  });
  assert.throws(()=>api.betapr.fit(new Float64Array([.5,1,2])),e=>{
    assert.equal(e.kind,'tie');assert.ok(e.candidates.length>=2);return true;
  });
  assert.throws(()=>api.betapr.fit(new Float64Array([.03,.1,.4,.7,1,2,4,12]),{},{iterlim:1}),e=>{
    assert.equal(e.kind,'budget');assert.ok(e.candidates.length>0);return true;
  });
  assert.throws(()=>api.betapr.fit(new Float64Array([2,2,2])),e=>{assert.equal(e.kind,'nomle');return true;});
});
test('EVT two-point certificates preserve public holds and typed Gumbel face',async()=>{
  const {default:createApi}=await import('./index.mjs');const api=await createApi();
  const f=api.frechet.fit(new Float64Array([...Array(9).fill(0),...Array(11).fill(1)]));
  assert.equal(f.estimates.lambda,1);assert.ok(Math.abs(f.estimates.sigma-.1)<1e-12);
  assert.throws(()=>api.frechet.fit(new Float64Array([0,...Array(19).fill(1)])),e=>{
    assert.equal(e.kind,'limit');assert.equal(e.model,'gumbel');
    assert.ok(e.estimates.sigma>0);assert.ok(e.loglik_upper-e.loglik_lower<1e-7);return true;
  });
  assert.throws(()=>api.frechet.fit(new Float64Array([0,...Array(19).fill(1)]),{},{iterlim:1}),e=>e.kind==='numerical');
  for(const held of [{mu:1.5},{sigma:.5},{mu:1.5,sigma:.5}]) {
    const g=api.gev.fit(new Float64Array([0,2,2,2]),held);
    assert.deepEqual(g.estimates,{mu:1.5,sigma:.5,xi:-1});
  }
});

test('Frechet bounded two-point solve cap preserves numerical stop details',async()=>{
  const {default:createApi}=await import('./index.mjs');const api=await createApi();
  assert.throws(()=>api.frechet.fit(new Float64Array([0,...Array(19).fill(1)]),{},{iterlim:10}),e=>{
    assert.equal(e.kind,'numerical');assert.ok(Number.isFinite(e.loglik_upper));assert.ok(e.candidates.length>0);return true;
  });
});

test('EVT compatible holds keep certificate failures and constrained witnesses',async()=>{
  const {default:createApi}=await import('./index.mjs');const api=await createApi();
  assert.throws(()=>api.gev.fit(new Float64Array([0,2,2,2]),{mu:1.5,sigma:.5},{reltol:1e-15}),e=>e.kind==='numerical');
  assert.throws(()=>api.frechet.fit(new Float64Array([...Array(7).fill(0),...Array(9).fill(1)]),{sigma:.125},{reltol:1e-15}),e=>e.kind==='numerical');
  assert.throws(()=>api.frechet.fit(new Float64Array([0,...Array(15).fill(1)]),{sigma:.875},{iterlim:10}),e=>{
    assert.equal(e.kind,'numerical');assert.equal(e.loglik_lower,Math.max(...e.candidates.map(c=>c.loglik_lower)));
    assert.ok(e.candidates.every(c=>c.sigma===.875));return true;
  });
});

test('GEV joint cover isolates the seam maximum and keeps precision stops',async()=>{
  const {default:createApi}=await import('./index.mjs');const api=await createApi();
  const x=new Float64Array([0,1,1,1,2,3,4]);
  const fit=api.gev.fit(x);
  assert.ok(Math.abs(fit.estimates.xi-.022797535412548)<1e-10);
  assert.throws(()=>api.gev.fit(x,{},{reltol:1e-16}),e=>{
    assert.equal(e.kind,'numerical');assert.ok(e.candidates.length>0);return true;
  });
  const face=api.gev.fit(new Float64Array([0,1,2,3,3]));
  assert.equal(face.estimates.xi,-1);
});

test('general Frechet certificate transports the Gumbel closure in public coordinates',async()=>{
  const {default:createApi}=await import('./index.mjs');const api=await createApi();
  const x=Float64Array.from({length:80},(_,i)=>-Math.log(-Math.log((i+.5)/80)));
  assert.throws(()=>api.frechet.fit(x),e=>{
    assert.equal(e.kind,'limit');assert.equal(e.model,'gumbel');
    assert.ok(e.estimates.sigma>0);assert.ok(Number.isFinite(e.loglik_upper));
    assert.ok(e.loglik_lower<=e.loglik_upper);return true;
  });
});

test('all-free Frechet minimum-majority collapse is typed nonexistence',async()=>{
  const {default:createApi}=await import('./index.mjs');const api=await createApi();
  assert.throws(()=>api.frechet.fit(new Float64Array([0,0,0,1,2])),e=>e.kind==='nomle');
});

test('GEV negative-shape coefficient corners preserve the gap dependency',async()=>{
  const {default:createApi}=await import('./index.mjs');const api=await createApi();
  const f=api.gev.fit(new Float64Array([0,.22725282343514291,.56276665892011701,.58275143915118077,1]));
  for(const [name,expected] of Object.entries({mu:.35972611509033436,sigma:.33733043138434093,xi:-.31554486140157456}))
    assert.ok(Math.abs(f.estimates[name]-expected)<1e-10);
});
// A process timeout is essential: a synchronous WASM rejection loop blocks
// node:test's in-process timeout too.
test('extreme t and GED shapes terminate in WASM and retain inversion', async()=>{
  const {spawnSync}=await import('node:child_process');
  const source=`import create from './build/js/univariateML.mjs';
import assert from 'node:assert/strict';
const m=await create();
for(const [x,ld] of [[1.3407807929942597e+154,-6.2303296397089188e+307],[1.4e+154,-6.6272046396795535e+307]]) {
 const q=new Float64Array([x]),p=[0,1,Number.MAX_VALUE];
 assert.ok(Math.abs(m.dstd(q,p,true,true)[0]/ld-1)<1e-12,'Student density overflow');
 assert.equal(m.dstd(q,p,true,false)[0],0,'Student density underflow');
 assert.ok(Math.abs(m.dsstd(q,[...p,1],true,true)[0]/ld-1)<1e-12,'skew Student density overflow');
}
const edgeRows=[[100000000,-1.803912823679201,-19.824051541098797],[1e+100,-1.2424533248940002,-38.080312530773824],[1.7976931348623157e+308,-1.2424533248940002,-38.080312530773824]];
for(const [nu,ld,lp] of edgeRows) {
 const x=new Float64Array([Math.sqrt(3)]),p=[0,1,nu];
 assert.ok(Math.abs(m.dged(x,p,true,true)[0]-ld)<1e-11,'GED edge density');
 assert.ok(Math.abs(m.pged(x,p,false,true)[0]-lp)<1e-11,'GED edge tail');
 assert.ok(Math.abs(m.pged(x,p,false,false)[0]/Math.exp(lp)-1)<1e-10,'GED edge natural tail');
}
assert.ok(Number.isNaN(m.psged(new Float64Array([Math.sqrt(3)]),[0,1,Number.MAX_VALUE,2],true,false)[0]),'uncertain skew-GED edge');
assert.ok(Number.isNaN(m.pged(new Float64Array([1]),[0,1,4e-306],false,true)[0]),'GED unsupported log scale');
for(const [f,p] of [['lst',[0,1,Number.MIN_VALUE]],['ht',[0,Number.MIN_VALUE,1]],['st',[0,1,2,Number.MIN_VALUE]]])
 assert.ok([...m['r'+f](1,p,163)].every(Number.isNaN),f+' subnormal gamma shape');
for(const nu of [1e8,1e100,Number.MAX_VALUE]) {
 for(const [f,p] of [['lst',[0,1,nu]],['std',[0,1,nu]],['sstd',[0,1,nu,2]],['ht',[0,nu,1]],['st',[0,1,2,nu]],['ged',[0,1,nu]],['sged',[0,1,nu,2]]]) {
  assert.ok(Number.isFinite(m['p'+f](new Float64Array([0]),p,true,false)[0]),f+' origin CDF');
  const probs=new Float64Array([.01,.25,.5,.75,.99]);
  const q=m['q'+f](probs,p,true,false), got=m['p'+f](q,p,true,false);
  got.forEach((v,i)=>assert.ok(Math.abs(v-probs[i])<1e-9,f+' inversion '+nu+' '+v));
  const draw=m['r'+f](1000,p,163);
  assert.ok([...draw].every(Number.isFinite),f+' draws');
  assert.ok(new Set(draw).size>900,f+' spread');
  if(f==='ged'||f==='sged') {
   q.forEach((v,i)=>assert.ok(Math.abs(v-Math.sqrt(3)*(2*probs[i]-1))<1e-6,f+' uniform limit'));
   const mean=draw.reduce((s,v)=>s+v,0)/draw.length;
   const square=draw.reduce((s,v)=>s+v*v,0)/draw.length;
   assert.ok(Math.abs(mean)<.12 && Math.abs(square-1)<.15,f+' moments');
  }
  if(f==='lst' && nu>1e99) {
   const x=new Float64Array([40,100,1000]);
   const lp=m.plst(x,p,false,true);
   const back=m.qlst(lp,p,false,true);
   back.forEach((v,i)=>assert.ok(Math.abs(v/x[i]-1)<1e-10,'log tails'));
  }
 }
}`;
  const r=spawnSync(process.execPath,['--v8-pool-size=1','--input-type=module','-e',source],
    {cwd:new URL('../../',import.meta.url),timeout:30000,killSignal:'SIGKILL',encoding:'utf8'});
  assert.equal(r.error,undefined,String(r.error));
  assert.equal(r.status,0,r.stderr);
});
test('GB2 subunit held shapes certify endpoint faces with held-endpoint advice',async()=>{
  const {default:createApi}=await import('./index.mjs');const api=await createApi();
  const x=new Float64Array([.1,.2,.4,.8]);
  assert.throws(()=>api.gb2.fit(x,{shape1:.4,scale:1},{iterlim:1}),e=>{
    assert.equal(e.kind,'limit');assert.equal(e.model,'power');
    assert.equal(e.estimates.alpha,1);
    assert.ok(Math.abs(e.estimates.beta+4/[...x].reduce((s,v)=>s+Math.log(v),0))<1e-12);
    assert.match(e.message,/alpha = scale/);return true;
  });
  assert.throws(()=>api.gb2.fit(Float64Array.from(x,v=>1/v),{shape3:.4,scale:1},{iterlim:1}),e=>{
    assert.equal(e.kind,'limit');assert.equal(e.model,'pareto');
    assert.equal(e.estimates.b,1);assert.match(e.message,/b = scale/);return true;
  });
  assert.throws(()=>api.fpareto.fit(Float64Array.from(x,v=>v+2),{min:2,shape1:.4,scale:1},{iterlim:1}),e=>{
    assert.equal(e.kind,'limit');assert.equal(e.model,'power');
    assert.equal(e.estimates.alpha,1);return true;
  });
  assert.throws(()=>api.gb2.fit(x,{shape1:2,shape3:3},{iterlim:1}),e=>{
    assert.equal(e.kind,'numerical');assert.equal(e.candidates.length,1);return true;
  });
});

test('GB2 constant held-shape scale rejects tiny-power log-ratio amplification',async()=>{
  const {default:createApi}=await import('./index.mjs');const api=await createApi();
  assert.throws(()=>api.gb2.fit(new Float64Array([2,2,2,2,2]),{shape1:10,shape2:1e-300,shape3:10+2e-15}),e=>{
    assert.equal(e.kind,'numerical');return true;
  });
});

test('GB2 subunit held-shape endpoint ties retain their likelihood penalty',async()=>{
  const {default:createApi}=await import('./index.mjs');const api=await createApi();
  const x=new Float64Array([.25,.5,1,1]),logs=[...x].reduce((s,v)=>s+Math.log(v),0);
  assert.throws(()=>api.gb2.fit(x,{shape1:.4,scale:1},{iterlim:1}),e=>{
    assert.equal(e.kind,'limit');assert.equal(e.model,'power');
    assert.ok(Math.abs(e.limit_loglik-(4*Math.log(-4/logs)-4-logs-.8*Math.log(2)))<1e-12);return true;
  });
});

test('GB2 constant mixed-power finite regime has a certified fit',async()=>{
  const {default:createApi}=await import('./index.mjs');const api=await createApi();
  const f=api.gb2.fit(new Float64Array([.5,.5,.5,.5,.5]),{shape1:2,scale:1});
  assert.equal(f.status,'Ok');
  const p=f.estimates.shape3,u=f.estimates.shape2*Math.log(2);
  assert.ok(Math.abs(1/p+1/(p+1)-Math.log1p(Math.exp(u)))<1e-6);
  assert.ok(Math.abs(1/u-p+(p+2)/(1+Math.exp(u)))<1e-6);
});

test('GB2 held-scale free-power profiles use global coefficient enclosures',async()=>{
  const {default:createApi}=await import('./index.mjs');const api=await createApi();
  const x=new Float64Array([.03,.1,.3,.7,1,2,5,20]);
  const f=api.gb2.fit(x,{shape1:2,scale:1});
  assert.equal(f.status,'Ok');assert.ok(Math.abs(f.estimates.shape2/.57834363213651174-1)<1e-7);
  const e=new Float64Array([.25,.5,1,1]);
  assert.throws(()=>api.fpareto.fit(Float64Array.from(e,v=>v+2),{min:2,shape1:1.1,scale:1},{iterlim:1}),error=>{
    assert.equal(error.kind,'limit');assert.equal(error.model,'power');return true;
  });
});
test('bnbinom Bernstein supporting polynomials prove additional all-free and pair faces',async()=>{
  const api=await (await import('./index.mjs')).default();
  const x=[0,0,0,...Array(10).fill(1),3];
  assert.throws(()=>api.bnbinom.fit(x),e=>{
    assert.equal(e.kind,'limit');assert.equal(e.model,'pois');
    assert.ok(Math.abs(e.estimates.lambda-13/14)<1e-14);return true;
  });
  for(const held of [{size:1},{beta:1}]){
    assert.throws(()=>api.bnbinom.fit(x,held),e=>{
      assert.equal(e.kind,'limit');assert.equal(e.model,'nbinom');
      assert.equal(e.estimates.size,1);
      assert.ok(Math.abs(e.estimates.prob-14/27)<1e-14);return true;
    });
  }
});

test('bnbinom all-free results expose a certified global inner profile interval',async()=>{
  const api=await (await import('./index.mjs')).default();
  const f=api.bnbinom.fit(read('bnbinom','golden').data);
  assert.ok(f.profile_alpha_interval[0]<f.estimates.alpha);
  assert.ok(f.profile_alpha_interval[1]>f.estimates.alpha);
  const values=[f.estimates.size,f.estimates.alpha,f.estimates.beta];
  for(let j=0;j<3;++j){
    assert.ok(f.profile_mode_lower[j]<=values[j]);
    assert.ok(values[j]<=f.profile_mode_upper[j]);
  }
});

test('bnbinom all-free finite certificate bounds an independent high-precision maximizer',async()=>{
  const api=await (await import('./index.mjs')).default();
  const f=api.bnbinom.fit(read('bnbinom','golden').data);
  const truth=[1.0672234392758119184,2.1419792202550637215,5.3647342962537853096];
  for(let j=0;j<3;++j){
    assert.ok(f.global_coefficient_lower[j]<=truth[j]);
    assert.ok(truth[j]<=f.global_coefficient_upper[j]);
  }
  assert.ok(f.global_loglik_interval[0]<=-1275.6474863695586167);
  assert.ok(f.global_loglik_interval[1]>=-1275.6474863695586167);
});

test('bnbinom unproved all-free face comparisons retain the legacy limit',async()=>{
  const api=await (await import('./index.mjs')).default();
  assert.throws(()=>api.bnbinom.fit([0,0,5,5]),e=>{
    assert.equal(e.kind,'limit');assert.equal(e.model,'nbinom');
    assert.ok(e.candidates.length>=2);assert.ok(e.loglik_upper>=e.loglik_lower);return true;
  });
});

test('bnbinom delegated NB precision failures retain NB candidate names',async()=>{
  const api=await (await import('./index.mjs')).default();
  const x=[[969,3182],[970,1816],[1001,1],[1003,1],[1004,1],[1032,1785],[1033,3215]].flatMap(([value,n])=>Array(n).fill(value));
  assert.throws(()=>api.bnbinom.fit(x),e=>{
    assert.equal(e.kind,'numerical');assert.ok(e.candidates.length>=1);
    for(const c of e.candidates){assert.equal(c.model,'nbinom');assert.ok(c.size>0);assert.ok(c.prob>0 && c.prob<1);}
    return true;
  });
});

test('bnbinom legacy fallbacks and proved decisions expose certification labels',async()=>{
  const api=await (await import('./index.mjs')).default();
  for(const x of [[0,0,5,5],[...Array(20).fill(0),...Array(20).fill(5)]]){
    assert.throws(()=>api.bnbinom.fit(x),e=>{
      assert.equal(e.kind,'limit');assert.equal(e.model,'nbinom');
      assert.match(e.message,/Certified global comparison/);return true;
    });
  }
  assert.throws(()=>api.bnbinom.fit([0,1,2,3,4]),e=>{
    assert.equal(e.kind,'limit');assert.match(e.message,/Certified global comparison/);return true;
  });
  const x=read('bnbinom','golden').data;
  const legacy=api.bnbinom.fit(x,{}, {iterlim:200});
  assert.equal(legacy.certified,0);
  assert.equal(legacy.global_coefficient_lower,undefined);
  assert.equal(api.bnbinom.fit(x).certified,1);
});


test('binomial near-critical ZIP inputs return uncertified fits',async()=>{
  const {default:createApi}=await import('./index.mjs');const api=await createApi();
  for(const pi of [.3772146069463776,.37721460694637765]) {
    const f=api.zib.fit(new Float64Array([0,0,0,0,0,1,1,2,2,4]),{size:null,pi});
    assert.equal(f.status,'Ok');assert.equal(f.certified,0);
    assert.equal(f.estimates.pi,pi);assert.ok(Number.isFinite(f.logLik));
  }
});

test('truncated binomial certifies the finite zero-second-order boundary',async()=>{
  const {default:createApi}=await import('./index.mjs');const api=await createApi();
  const counts=[242187,60672,121344,1488980,406274];
  const x=new Float64Array(counts.reduce((a,b)=>a+b,0));let at=0;
  for(let k=0;k<counts.length;++k){x.fill(k,at,at+counts[k]);at+=counts[k];}
  const f=api.tbinom.fit(x,{a:-1,b:4,size:null});
  assert.equal(f.status,'Ok');assert.equal(f.estimates.size,7);
  assert.ok(Math.abs(f.estimates.prob-.44715659335717363398)<1e-10);
  assert.ok(Math.abs(f.logLik+3379063.91960425442105)<1e-7);
});
test('bnbinom signed Poisson faces and budget fallbacks keep their labels',async()=>{
  const api=await (await import('./index.mjs')).default();
  assert.throws(()=>api.bnbinom.fit([0,2,2,2]),e=>{
    assert.equal(e.kind,'limit');assert.equal(e.model,'pois');
    assert.match(e.message,/Certified global comparison/);return true;
  });
  assert.throws(()=>api.bnbinom.fit([0,0,5,5],{}, {iterlim:100}),e=>{
    assert.equal(e.kind,'limit');assert.equal(e.model,'nbinom');
    assert.match(e.message,/Uncertified legacy comparison/);return true;
  });
});
test('triangular public rounding loss is a numerical failure', async () => {
  const api=await (await import('./index.mjs')).default();
  const x=new Float64Array([0.03627073533558047,0.03627073533558048,0.036270735335580485,0.0362707353355805,0.03627073533558053]);
  assert.throws(() => api.triang.fit(x,{a:0.036270735335580465},{reltol:1e-10,iterlim:100}),error => {
    assert.equal(error.kind,'numerical');
    assert.match(error.message,/numerical certificate margin/);
    return true;
  });
});

test('GEV public support rejects a negative zero residual', async () => {
  const api=await (await import('./index.mjs')).default();
  const f=api.gev.fit(new Float64Array([-1,0,Number.MIN_VALUE]),{sigma:2,xi:-1});
  assert.equal(f.status,'Ok');
  assert.equal(f.estimates.sigma,2);
  assert.ok(f.estimates.mu+f.estimates.sigma>0);
});
test('Bhatt per-fit certification preserves successful searches',async ()=>{
  const {default:createApi}=await import('./index.mjs');
  const api=await createApi();
  const x=new Float64Array([-10,...Array(20).fill(0),10]);
  const f=api.bhatt.fit(x);
  assert.equal(f.estimates.a,0);assert.equal(f.certified,0);
  const g=api.bhatt.fit(new Float64Array(read('bhatt','golden').data));
  assert.equal(g.certified,1);
  const seed=api.bhatt.fit(new Float64Array([1.5186427188633,-.893644554187554,-1.08058198203889,-.639737926177478,.655034880479549,-2.35672046800855,2.46887599116556,.273853493748292,2.5355294429514,-.299436099194099,-.160890965931292,-.201888161544889,.6721053216777,-.344350831779183,1.51377727236596,.163578425435854,1.41905463032963,.838599300343392,-.0826733918168545,-1.34035410608697,-.343170308885225,.824451373169256,-.404339571078608,.0120556813316649]));
  assert.equal(seed.status,"Ok");assert.equal(seed.certified,0);
  assert.ok(Object.values(seed.estimates).every(Number.isFinite));
  assert.throws(()=>api.bhatt.fit(new Float64Array([-3,-1,0,1,2]),{}, {iterlim:1}));
});
test('transformed profiles retain finite ulp-wide fits with explicit uncertified flags',async()=>{
  const api=await (await import('./index.mjs')).default();
  const x=Float64Array.from([0,1,2,3,4],v=>1+v*Number.EPSILON);
  const y=Float64Array.from([0,1,2,4,7],v=>1+v*Number.EPSILON);
  const fits=[api.invweibull.fit(x),api.frechet.fit(x,{mu:0}),
    api.llogis.fit(y,{}, {abstol:1e-10}),api.pareto3.fit(y,{min:0}),
    api.invburr.fit(x,{shape1:.855},{gradtol:1e-10}),
    api.gb2.fit(x,{shape1:1,shape3:.855},{gradtol:1e-10}),
    api.fpareto.fit(x,{min:0,shape1:1,shape3:.855},{reltol:1e-10})];
  for(const fit of fits){assert.equal(fit.status,'Ok');assert.equal(fit.certified,0);assert.ok(Number.isFinite(fit.logLik));}
});

test('raw transformed sufficient statistics match independent concentrated references',async()=>{
  const api=await (await import('./index.mjs')).default();
  const cases=[
    ['invgamma',[1.471602327564204,1.4716023275656755,1.4716023275671473,1.471602327568619,1.4716023275715622],
      {alpha:3.3782209017569925e23,beta:4.9713977420625717e23},126.41556407783901],
    ['lnorm',[1.445187510724387,1.445187510725832,1.4451875107272774,1.4451875107287226,1.445187510731613],
      {meanlog:.3682390783550075238,sdlog:1.7204957773060615e-12},126.50615506228813],
    ['invweibull',[.5071697345584799,.5071697345589871,.5071697345594942,.5071697345600014,.5071697345610158],
      {shape:707675622377.0812,scale:.5071697345591792610},131.91568101929355]];
  for(const [family,x,coefficients,ll] of cases){
    const fit=api[family].fit(new Float64Array(x));
    for(const [name,value] of Object.entries(coefficients))assert.ok(Math.abs(fit.estimates[name]/value-1)<1e-10,`${family}.${name}`);
    assert.ok(Math.abs(fit.logLik/ll-1)<1e-10,family);
  }
});
test('genbeta all-free finite certificate preserves the golden and never certifies unattainable precision', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  const g = read('genbeta','golden');
  const x = new Float64Array(g.data);
  const f = api.genbeta.fit(x,{scale:1});
  for (const key of ['shape1','shape2','shape3'])
    assert.ok(Math.abs(f.estimates[key]/g.estimates[key]-1)<1e-5);
  assert.ok(Math.abs(f.logLik-g.logLik)<1e-8);
  try {
    const tight=api.genbeta.fit(x,{scale:1},{gradtol:1e-20});
    assert.equal(tight.certified,0);
    for(const key of ['shape1','shape2','shape3'])
      assert.ok(Math.abs(tight.estimates[key]/g.estimates[key]-1)<1e-5);
  } catch(e) {
    assert.ok(['numerical','budget'].includes(e.kind));
    assert.ok(e.candidates.length>0);
    assert.equal(e.loglik_upper,Infinity);
  }
});

test('genbeta moving-power certificate transports global likelihood bounds', async () => {
  const {default:createApi}=await import('./index.mjs');
  const api=await createApi();
  const x=Array.from({length:300},(_,i)=>.7*((i+.5)/300)**2);
  assert.throws(()=>api.genbeta.fit(x,{scale:1}),e=>{
    assert.equal(e.kind,'limit');assert.equal(e.model,'power');
    assert.ok(Number.isFinite(e.loglik_lower)&&Number.isFinite(e.loglik_upper));
    assert.ok(e.loglik_lower<=e.limit_loglik&&e.limit_loglik<=e.loglik_upper);
    return true;
  });
});

test('genbeta gamma face preserves its structured legacy payload', async () => {
  const {default:createApi}=await import('./index.mjs');
  const api=await createApi();
  const u=Float64Array.from({length:300},(_,i)=>(i+.5)/300);
  const x=Float64Array.from(api.gamma.q(u,[2,1/3]),v=>Math.exp(-v));
  assert.throws(()=>api.genbeta.fit(x,{scale:1}),e=>{
    assert.equal(e.kind,'limit');assert.equal(e.model,'unavailable:unit-gamma');
    assert.match(e.message,/unit-gamma limit/);
    assert.ok(Number.isFinite(e.limit_loglik));
    assert.ok(e.loglik_upper>=e.limit_loglik);
    assert.ok(e.candidates.some(c=>c.model==='unavailable:unit-gamma'));
    return true;
  });
});

test('genbeta shallow coverage retains the uncertified legacy finite fit', async () => {
  const {default:createApi}=await import('./index.mjs');
  const api=await createApi();
  const u=Float64Array.from({length:300},(_,i)=>(i+.5)/300);
  const x=api.genbeta.q(u,[3,2,.7,1]);
  const f=api.genbeta.fit(x,{scale:1});
  assert.equal(f.certified,0);
  assert.ok(Math.abs(f.estimates.shape1/5.364602384807961-1)<1e-10);
  const g=read('genbeta','golden');
  assert.equal(api.genbeta.fit(new Float64Array(g.data),{scale:1}).certified,1);
});

test('genbeta signed squared-power certificate selects a global gamma face', async () => {
  const {default:createApi}=await import('./index.mjs');const api=await createApi();
  const x=[Math.exp(-.001),...Array.from({length:99},(_,i)=>Math.exp(-(.5+(i+.5)/99)))];
  assert.throws(()=>api.genbeta.fit(x,{scale:1}),e=>{
    assert.equal(e.kind,'limit');assert.equal(e.model,'unavailable:unit-gamma');
    assert.doesNotMatch(e.message,/uncertified/);
    assert.ok(Number.isFinite(e.loglik_lower)&&Number.isFinite(e.loglik_upper));
    assert.ok(e.loglik_lower<=e.limit_loglik&&e.limit_loglik<=e.loglik_upper);
    return true;
  });
});
test('GEV and Frechet held nuisance certificate markers cross the common seam',()=>{
 const p=Array.from({length:50},(_,i)=>(i+.5)/50);
 const x=new Float64Array(p.map(v=>Math.expm1(-.5*Math.log(-Math.log(v)))/.5));
 for(const held of [{mu:0},{sigma:1}]) {
  const r=m.fitgev(x,held,{});
  assert.equal(r.status,'Ok');assert.equal(r.certified,1);
  for(const [key,value] of Object.entries(held))assert.equal(r.estimates[key],value);
 }
 const f=m.fitfrechet(new Float64Array(p.map(v=>Math.pow(-Math.log(v),-.5))),{sigma:1},{});
 assert.equal(f.status,'Ok');assert.equal(f.certified,1);assert.equal(f.estimates.sigma,1);
});
test('std and lst shape certificates distinguish uncertified joint fits', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  for (const family of ['lst','std']) {
    const g = read(family,'golden'), names = Object.keys(g.estimates);
    const held = Object.fromEntries(names.slice(0,2).map(k => [k,g.estimates[k]]));
    const fit = api[family].fit(new Float64Array(g.data), held);
    assert.equal(fit.status,'Ok');
    assert.equal(fit.certified,1);
    assert.ok(fit.logLik >= g.logLik - 1e-6*g.data.length);
    for (const [key,value] of Object.entries(held)) assert.equal(fit.estimates[key],value);
    for (const key of names.slice(0,2)) {
      const joint=api[family].fit(new Float64Array(g.data),{[key]:g.estimates[key]});
      assert.equal(joint.status,'Ok');
      assert.equal(joint.certified,0);
      assert.equal(joint.estimates[key],g.estimates[key]);
    }
  }
  assert.throws(() => api.std.fit(new Float64Array([0,0,1]), {mean:0,sd:1}), error => {
    assert.equal(error.kind,'nomle');
    assert.ok(error.loglik_lower <= -3*Math.log(2));
    assert.ok(error.loglik_upper >= -3*Math.log(2));
    return true;
  });
  const small = api.lst.fit(new Float64Array([-100,100]), {mu:0,sigma:1});
  assert.ok(small.estimates.df>0 && small.estimates.df<.5);
});

test('std and lst held free-shape solver caps preserve finite diagnostic witnesses', async () => {
  const {default: createApi} = await import('./index.mjs');
  const api = await createApi();
  for (const family of ['lst','std']) {
    const g=read(family,'golden'), names=Object.keys(g.estimates);
    for (const mask of [1,2,3]) {
      const held=Object.fromEntries(names.filter((_,i)=>mask>>i&1).map(k=>[k,g.estimates[k]]));
      assert.throws(()=>api[family].fit(new Float64Array(g.data),held,{iterlim:1}),error=>{
        assert.equal(error.kind,family==='lst'&&mask===2?'budget':'numerical');
        assert.ok(Number.isFinite(error.loglik_lower));
        assert.equal(error.loglik_upper,Infinity);
        assert.ok(error.candidates.length>0);
        return true;
      });
    }
  }
});


test('Student held subsets expose proved symmetry nonuniqueness', async () => {
  const {default:createApi}=await import('./index.mjs');
  const api=await createApi();
  for(const [family,x,held] of [['std',[-100,100],{sd:1}],['lst',[-100,-99,0,99,100],{df:.5}]]){
    assert.throws(()=>api[family].fit(new Float64Array(x),held),error=>{
      assert.equal(error.kind,'tie');
      assert.ok(Number.isFinite(error.loglik_lower));
      assert.equal(error.loglik_upper,Infinity);
      assert.equal(error.candidates.length,2);
      return true;
    });
  }
});

test('Student signed held-location normal faces expose certification', async () => {
  const {default:createApi}=await import('./index.mjs');
  const api=await createApi();
  for(const [family,held] of [['lst',{mu:.2}],['std',{mean:.2}]]) {
    assert.throws(()=>api[family].fit(new Float64Array([-1,2]),held),error=>{
      assert.equal(error.kind,'limit');
      assert.equal(error.model,'norm');
      assert.equal(error.certified,1);
      assert.ok(Number.isFinite(error.loglik_upper));
      return true;
    });
  }
});


test('Student legacy t2 limit keeps its class and uncertified flag', async () => {
  const {default:createApi}=await import('./index.mjs');
  const api=await createApi();
  const x=[-20,...Array(10).fill(-.1),...Array(10).fill(.1),20];
  assert.throws(()=>api.std.fit(new Float64Array(x),{mean:0}),error=>{
    assert.equal(error.kind,'limit');assert.equal(error.model,'lst');
    assert.equal(error.certified,0);assert.equal(error.loglik_upper,Infinity);
    assert.match(error.message,/mllst/);return true;
  });
});
