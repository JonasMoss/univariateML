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
 assert.throws(()=>m.fitgb2(new Float64Array(read('gb2','golden').data),{},{gradtol:1e-8,iterlim:1}),/GB2 certified delegate budget stop; no verified maximum/);
 for(const f of ['exp','gamma','gb2']) {const g=read(f,'golden'),name=Object.keys(g.estimates)[0];assert.throws(()=>m['fit'+f](new Float64Array(g.data),{[name]:NaN},{}),/must be.*positive.*finite/);}
 for(const f of ['exp','gamma','gb2']){const p=Object.values(read(f,'golden').estimates),x=new Float64Array([.2,1,2]);for(const op of ['d','p','q'])for(const lower of [true,false])for(const log of [true,false]){const a=m[op+f](x,p,lower,log);assert.equal(a.length,3);for(let i=0;i<3;i++)assert.equal(a[i],m[op+f](x.slice(i,i+1),p,lower,log)[0]);}}
});
process.on('exit',()=>{mkdirSync(new URL('../../build/scratch/',import.meta.url),{recursive:true});writeFileSync(new URL('../../build/scratch/truth-results.json',import.meta.url),JSON.stringify({checks,failures},(_,v)=>typeof v==='number'&&!Number.isFinite(v)?String(v):v,2));});

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
