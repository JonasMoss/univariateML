import {readFileSync} from 'node:fs';
import test from 'node:test';
import assert from 'node:assert/strict';
import create from './index.mjs';
const api = await create();
test('structured stops survive the JS Error facade', () => {
  assert.throws(() => api.cauchy.fit([-2,2], {scale:1}), e => {
    assert.ok(e instanceof Error);assert.equal(e.kind,'tie');
    assert.ok(Number.isNaN(e.segment_loglik_upper));
    assert.equal(e.candidates.length,2);
    for (const c of e.candidates) {
      assert.ok(Math.abs(Math.abs(c.location)-Math.sqrt(3))<1e-7);
      assert.equal(c.scale,1);assert.ok(c.loglik_lower<=c.loglik_upper);
    }
    return /Held-scale Cauchy global maxima are tied/.test(e.message);
  });
  assert.throws(() => api.cauchy.fit([1,1,2]), e => e.kind==='nomle' && e.candidates.length===0);
  assert.throws(() => api.lomax.fit([1,1,1]), e => {
    assert.equal(e.kind,'limit');assert.equal(e.model,'exp');
    assert.equal(e.estimates.rate,1);assert.equal(e.limit_loglik,-3);return true;
  });
  assert.throws(() => api.lomax.fit([1,1,40,59], {}, {iterlim:1}), e => {
    assert.equal(e.kind,'budget');assert.ok(Number.isFinite(e.loglik_lower));
    assert.ok(e.loglik_upper>=e.loglik_lower);assert.ok(e.candidates.length>0);return true;
  });
  for (const [family,x,model,names] of [
    ['burr',[1,2,4],'pareto',['a','b']],['invburr',[1,.5,.25],'power',['alpha','beta']]]) {
    assert.throws(() => api[family].fit(x, {scale:1}), e => {
      assert.equal(e.kind,'limit');assert.equal(e.model,model);
      assert.deepEqual(Object.keys(e.estimates),names);assert.ok(Number.isFinite(e.limit_loglik));return true;
    });
  }
});

test('Burr and reciprocal sign-uncertain-seam structured tie', () => {
  const x=[0.0049057434842350416,0.042501201178458653,0.19481544379842536,0.29869422522389177,0.40386521559557592,0.49096570363940406,0.5326256684997116,0.72206471601197653,0.74104603671600355,0.82688168528714312,0.98659907210500519,1.1056048590023038,1.140076235502067,1.1606952121643113,1.4300263464615877,1.4793399841671804,1.5655241685783525,1.6390163836698108,2.0838622791695847,5.1042846241626947];
  for (const [family,data] of [['burr',x],['invburr',x.map(v=>1/v)]]) {
    assert.throws(() => api[family].fit(data), e => {
      assert.equal(e.kind,'tie');assert.ok(e.candidates.length>=2);
      for(const c of e.candidates)assert.ok(c.loglik_lower<=c.loglik_upper);
      return true;
    });
  }
});

test('Burr and reciprocal nearly-constant structured tie', () => {
  const x=[0.99999900000050002,0.99999910526355817,0.99999921052662744,0.9999993157897078,0.99999942105279915,0.99999952631590161,0.99999963157901528,0.99999973684213994,0.99999984210527559,0.99999994736842246,1.0000000526315804,1.0000001578947493,1.0000002631579294,1.0000003684211205,1.0000004736843227,1.0000005789475359,1.0000006842107605,1.000000789473996,1.0000008947372423,1.0000010000005];
  for (const [family,data] of [['burr',x],['invburr',x.map(v=>1/v)]]) {
    assert.throws(() => api[family].fit(data), e => {
      assert.equal(e.kind,'tie');assert.ok(e.candidates.length>=2);
      for(const c of e.candidates)assert.ok(c.loglik_lower<=c.loglik_upper);
      return true;
    });
  }
});

test('Burr and reciprocal burr-k1000-r0 structured limit', () => {
  const x=[1.9964892694079062,1.4200005950190642,1.9743019980704946,1.2197287005958721,1.4225995479881626,1.3157966553088341,0.73993727475227145,0.54722770691864697,0.45191337211974208,1.2873097597929237,0.82121203961716593,0.71742472173341532,0.49662546185097139,0.50362133862028513,1.9039605277382454,0.44113984974587089,0.70433800798072665,1.0510023394724524,0.86043036041400101,1.6667673885263476,0.77702711842775052,1.9593669373814471,0.26458369013083416,0.45464268934386737,0.21491182426553512,1.7015469399366296,1.1819465625871135,0.8586733698248058,0.59167998109529263,0.44017579526839962,0.62624828047368308,0.98655801970217505,0.57139631035053795,0.51337745151115244,0.75748588358755875,0.43771943358567916,0.98605461903315861,1.3018311813985106,1.1866216948888411,0.42924863858380002,0.44140052283539877,1.196264737299584,1.07766835322611,1.4443112646323752,1.2516989419471247,1.0680479247006633,1.051768161411192,0.5465700675904327,1.6020198955375378,0.90951835946016879];
  for (const [family,data] of [['burr',x],['invburr',x.map(v=>1/v)]]) {
    assert.throws(() => api[family].fit(data), e => {
      assert.equal(e.kind,'limit');assert.ok(e.candidates.length>=2);
      for(const c of e.candidates)assert.ok(c.loglik_lower<=c.loglik_upper);
      return true;
    });
  }
});

test('held-power faces retain transformed coefficients', () => {
  for(const [family,x,scale] of [['burr',3,3],['invburr',1/3,1/3]]) {
    assert.throws(()=>api[family].fit([x,x,x],{shape2:2}), e=> {
      assert.equal(e.kind,'limit');assert.equal(e.estimates.shape,2);
      assert.ok(Math.abs(e.estimates.scale-scale)<1e-12);return true;
    });
  }
});

test('translated and reciprocal delegates preserve certified stops', () => {
  for(const [family,data,held,model,coef,ll] of [
    ['pareto2',[3,3,3],{min:1},'exp',{rate:.5},-3*(1+Math.log(2))],
    ['invpareto',[2,2,2],{},'invexp',{scale:2},-3*(1+Math.log(2))],
    ['pareto4',[4,4,4],{min:1,shape2:2},'weibull',{shape:2,scale:3},3*(Math.log(2/3)-1)],
    ['gb2',[3,3,3],{shape3:1,shape2:2},'weibull',{shape:2,scale:3},3*(Math.log(2/3)-1)],
    ['fpareto',[4,4,4],{min:1,shape3:1,shape2:2},'weibull',{shape:2,scale:3},3*(Math.log(2/3)-1)],
    ['betapr',[2,2,2],{shape1:1},'exp',{rate:.5},-3*(1+Math.log(2))]
  ]) {
    assert.throws(()=>api[family].fit(data,held),e=>{
      assert.equal(e.kind,'limit');assert.equal(e.model,model);
      for(const [name,value] of Object.entries(coef))assert.ok(Math.abs(e.estimates[name]-value)<1e-10);
      assert.ok(Math.abs(e.limit_loglik-ll)<1e-10);return true;
    });
  }
});

test('Lomax budget witnesses retain public delegate coordinates and bounds', () => {
  const y=[1,1,40,59];
  for(const [family,data,held,names] of [
    ['pareto2',y.map(v=>v+1),{min:1},['shape','scale','min']],
    ['invpareto',y.map(v=>1/v),{},['shape','scale']],
    ['burr',y.map(Math.sqrt),{shape2:2},['shape1','shape2','scale']],
    ['invburr',y.map(v=>1/Math.sqrt(v)),{shape2:2},['shape1','shape2','scale']],
    ['gb2',y.map(Math.sqrt),{shape2:2,shape3:1},['shape1','shape2','shape3','scale']],
    ['gb2',y.map(v=>1/Math.sqrt(v)),{shape1:1,shape2:2},['shape1','shape2','shape3','scale']],
    ['fpareto',y.map(v=>1+Math.sqrt(v)),{min:1,shape2:2,shape3:1},['shape1','shape2','shape3','scale','min']],
    ['fpareto',y.map(v=>1+1/Math.sqrt(v)),{min:1,shape1:1,shape2:2},['shape1','shape2','shape3','scale','min']],
    ['betapr',y,{shape1:1},['shape1','shape2','scale']],
    ['betapr',y.map(v=>1/v),{shape2:1},['shape1','shape2','scale']]
  ]) {
    assert.throws(()=>api[family].fit(data,held,{iterlim:1}),e=>{
      assert.equal(e.kind,'budget',family);assert.ok(e.candidates.length>0,family);
      assert.ok(e.loglik_lower<=e.loglik_upper,family);
      for(const c of e.candidates){
        assert.ok(c.loglik_lower<=c.loglik_upper,family);
        if(c.model==='')for(const name of names)assert.ok(Number.isFinite(c[name]),`${family} ${name}`);
      }
      return true;
    });
  }
});

test('unrepresentable delegate reductions stop as numerical failures', () => {
  for(const [family,data,held] of [
    ['burr',[1e-300,1e300],{shape2:2}],
    ['invburr',[1e-300,1e300],{shape2:2}],
    ['pareto4',[1e-300,1e300],{min:0,shape2:2}],
    ['gb2',[1e-300,1e300],{shape3:1,shape2:2}],
    ['fpareto',[1e-300,1e300],{min:0,shape3:1,shape2:2}],
    ['pareto4',[1e308,1.1e308],{min:-1e308}]
  ])assert.throws(()=>api[family].fit(data,held),e=>e.kind==='numerical');
});

test('discrete stops retain fitted package and non-package faces', () => {
  for(const [family,x,held,model,estimates] of [
    ['binom',[0,0,6],{},'pois',{lambda:2}],
    ['bbinom',[0,1,1,2],{size:2},'binom',{size:2,prob:.5}],
    ['dgamma',[2,2,3],{},'unavailable:two-point',{lower:2,upper:3,prob:1/3}],
    ['zip',[0,0],{pi:.5},'pois',{lambda:0}],
    ['dnorm',[1,1],{},'unavailable:point-mass',{point:1}]
  ]) assert.throws(()=>api[family].fit(x,held),e=>{
    assert.equal(e.kind,'limit');assert.equal(e.model,model);
    assert.deepEqual(e.estimates,estimates);assert.ok(Number.isFinite(e.limit_loglik));
    assert.ok(e.candidates.length>0);return true;
  });
  assert.throws(()=>api.cmb.fit([0,1]),e=>{
    assert.equal(e.kind,'tie');assert.equal(e.model,'binom');
    assert.deepEqual(e.estimates,{size:1,prob:.5});return true;
  });
});

test('discrete empty preflight budget has no invented candidate', () => {
  assert.throws(()=>api.bbinom.fit([0,100001],{alpha:1},{iterlim:1}),e=>{
    assert.equal(e.kind,'budget');assert.equal(e.loglik_upper,Infinity);
    assert.equal(e.candidates.length,0);assert.deepEqual(e.estimates,{});return true;
  });
});

test('continuous structured nonexistence and fitted faces', () => {
  for (const [family,x] of [['exp',[0,0,0]],['norm',[1,1,1]],['gamma',[1,1,1]],['beta',[.5,.5,.5]]]) {
    assert.throws(() => api[family].fit(x), e => {
      assert.equal(e.kind,'nomle');assert.deepEqual(e.estimates,{});assert.deepEqual(e.candidates,[]);return true;
    });
  }
  for (const [family,x,held,model,estimates,ll] of [
    ['bhatt',[-1,-.5,0,.5,1],{a:2},'unif',{min:-2,max:2},-5*Math.log(4)],
    ['gompertz',[0,0,1,9],{},'exp',{rate:.4},4*(Math.log(.4)-1)],
    ['tnorm',[2,2,3,11],{a:2,b:Infinity},'unavailable:translated-exponential',{mu:2,rate:.4},4*(Math.log(.4)-1)],
    ['sn',[0,.1,.2,.4,3],{},'hnorm',{mu:0,sigma:Math.sqrt(9.21/5)},2.5*(Math.log(2/Math.PI)-Math.log(9.21/5)-1)]
  ]) assert.throws(() => api[family].fit(x,held), e => {
    assert.equal(e.kind,'limit');assert.equal(e.model,model);
    assert.deepEqual(Object.keys(e.estimates),Object.keys(estimates));
    for(const k of Object.keys(estimates))assert.ok(Math.abs(e.estimates[k]-estimates[k])<1e-8);
    assert.ok(Math.abs(e.limit_loglik-ll)<1e-8);return true;
  });
  assert.throws(() => api.exp.fit([-1,2]), e => !e.kind);
  assert.throws(() => api.gamma.fit([1,1,1],{}, {abstol:0}), e => !e.kind);
  assert.throws(() => api.gev.fit([0,0,0,1,2]), e => e.kind==='nomle');
});

test('Power face payload follows public endpoint-then-shape order', () => {
  const x=Array.from({length:300},(_,i)=>.7*((i+.5)/300)**2);
  const ref=api.power.fit(x);
  assert.throws(() => api.genbeta.fit(x,{scale:1}), e => {
    assert.equal(e.kind,'limit');assert.equal(e.model,'power');
    assert.deepEqual(Object.keys(e.estimates),['alpha','beta']);
    for(const k of ['alpha','beta'])assert.ok(Math.abs(e.estimates[k]-ref.estimates[k])<1e-9);
    assert.ok(Math.abs(e.limit_loglik-ref.logLik)<1e-9);return true;
  });
});

test('held subunit GED shape retains the certified reflected half face', () => {
  assert.throws(() => api.sged.fit([1,1,1],{mean:0,nu:.5}), e => {
    assert.equal(e.kind,'limit');assert.equal(e.model,'unavailable:reflected-half-ged');
    assert.ok(Math.abs(e.estimates.mu-1)<1e-7);
    assert.ok(Math.abs(e.estimates.scale-1/6)<1e-7);assert.equal(e.estimates.nu,.5);
    assert.ok(Math.abs(e.limit_loglik-3*Math.log(3))<1e-6);return true;
  });
});

test('conditional snorm and skew-t ambiguities retain both compared models', () => {
  for(const [family,held,model] of [['snorm',{mean:0,sd:1e-4},'hnorm'],['st',{xi:0,omega:1e-4,nu:1e20},'ht']])
    assert.throws(()=>api[family].fit([1,2],held),e=>{
      assert.equal(e.kind,'tie');assert.equal(e.candidates.length,2);
      assert.ok(e.candidates.some(c=>c.model===model));assert.equal(e.loglik_upper,Infinity);
      assert.ok(e.candidates.every(c=>Number.isFinite(c.loglik_lower)));return true;
    });
});

test('ZINB certificate transports competing maxima and terminal budgets', () => {
  assert.throws(() => api.zinb.fit([...Array(99).fill(0),10], {size:10,pi:.76839477568758077746}), e => {
    assert.equal(e.kind,'tie');assert.equal(e.candidates.length,2);
    for(const c of e.candidates){assert.equal(c.size,10);assert.equal(c.pi,.76839477568758077746);assert.ok(c.loglik_lower<=c.loglik_upper);}
    assert.equal(e.loglik_upper,Infinity);return true;
  });
  assert.throws(() => api.zinb.fit([0,1,2,5], {prob:.4}, {iterlim:1}), e => {
    assert.equal(e.kind,'budget');assert.equal(e.loglik_upper,Infinity);return true;
  });
  assert.throws(() => api.zinb.fit([0,100001], {size:2}), e => {
    assert.equal(e.kind,'numerical');return /workload limit/.test(e.message);
  });
});

test('ZINB certified ZIP and Poisson stops name their limiting coefficients', () => {
  assert.throws(() => api.zinb.fit([...Array(50).fill(0),...Array(50).fill(4)], {pi:.5}), e => {
    assert.equal(e.kind,'limit');assert.equal(e.model,'zip');assert.equal(e.estimates.pi,.5);
    assert.ok(e.estimates.lambda>2 && e.estimates.lambda<4);
    assert.ok(e.loglik_lower<=e.limit_loglik && e.limit_loglik<=e.loglik_upper);return true;
  });
  assert.throws(() => api.zinb.fit(Array(100).fill(2)), e => {
    assert.equal(e.kind,'limit');assert.equal(e.model,'pois');assert.equal(e.estimates.lambda,2);return true;
  });
});

test('PIG global certificate transports Poisson, tie, budget and numerical stops', () => {
  assert.throws(() => api.pig.fit([0,1,2,3],{mean:2}),e=>{
    assert.equal(e.kind,'limit');assert.equal(e.model,'pois');
    assert.equal(e.estimates.lambda,2);
    assert.ok(e.loglik_lower<=e.limit_loglik && e.limit_loglik<=e.loglik_upper);return true;
  });
  assert.throws(() => api.pig.fit([0,1,2],{mean:1+Math.sqrt(1/3)}),e=>{
    assert.equal(e.kind,'tie');assert.equal(e.candidates[0].model,'pois');
    assert.equal(e.loglik_upper,Infinity);return true;
  });
  assert.throws(() => api.pig.fit([0,0,2147483647],{},{iterlim:1}),e=>{
    assert.equal(e.kind,'budget');assert.equal(e.candidates.length,0);return true;
  });
  assert.throws(() => api.pig.fit([0,0,10],{},{iterlim:1}),e=>{
    assert.equal(e.kind,'numerical');assert.ok(e.candidates.length>0);return true;
  });
  assert.throws(() => api.pig.fit([0,0],{shape:3}),e=>{
    assert.equal(e.kind,'limit');assert.equal(e.model,'pois');
    assert.equal(e.estimates.lambda,0);assert.equal(e.limit_loglik,0);return true;
  });
});

test('PIG binary closed form and small positive Poisson improvement survive WASM', () => {
  const binary=api.pig.fit([0,1,1],{mean:3},{iterlim:1});
  const w=Math.sqrt(1-4/9),shape=2*(2/3)**2/(w*(1+w)**2);
  assert.ok(Math.abs(binary.estimates.shape-shape)<1e-12);
  assert.throws(()=>api.pig.fit([0,1,1],{mean:4/3}),e=>e.kind==='limit');
  const adjacent=api.pig.fit([0,1,1],{mean:4/3+Number.EPSILON},{iterlim:1});
  assert.ok(Number.isFinite(adjacent.estimates.shape));
  const x=[...Array(10002).fill(0),...Array(10000).fill(2),3];
  const fit=api.pig.fit(x);
  assert.equal(fit.estimates.mean,1);assert.ok(Number.isFinite(fit.estimates.shape));
});

test('Skellam tail-cover budget preserves a feasible witness', () => {
  assert.throws(() => api.skellam.fit([-1e9,0,1e9], {}, {iterlim:1}), e => {
    assert.equal(e.kind,'budget');assert.equal(e.candidates.length,1);
    assert.ok(Number.isFinite(e.loglik_lower));assert.equal(e.loglik_upper,Infinity);
    assert.match(e.message,/Raising iterlim may resolve/);return true;
  });
});

test('CMB fixed-size normalizers spend and raise the cell budget', () => {
  const held={size:100001,prob:.5,nu:1};
  assert.throws(() => api.cmb.fit([0,1],held,{iterlim:1}), e => {
    assert.equal(e.kind,'budget');assert.equal(e.candidates.length,0);
    assert.equal(e.loglik_upper,Infinity);return true;
  });
  assert.ok(Number.isFinite(api.cmb.fit([0,1],held,{iterlim:2}).logLik));
});

test('GB2 delegated budget keeps transformed-gamma witness names', () => {
  const g=JSON.parse(readFileSync(new URL('../../univariateML-cpp/include/cppunivariate/distributions/trgamma/golden.json',import.meta.url),'utf8'));
  assert.throws(()=>api.gb2.fit(g.data,{}, {iterlim:1}),e=>{
    assert.equal(e.kind,'budget');
    const c=e.candidates.find(c=>c.model==='trgamma');assert.ok(c);
    assert.deepEqual(Object.keys(c).filter(k=>!['model','loglik_lower','loglik_upper'].includes(k)),['shape1','shape2','scale']);
    assert.ok(Number.isFinite(c.loglik_lower));assert.equal(e.loglik_upper,Infinity);return true;
  });
});

test('ZIP certified rate ties, budgets and numerical stops', () => {
  const x=Array(100).fill(0);x[99]=10;
  for(const [pi,controls,kind,count] of [
    [.7254971052291191,{},'tie',2],[.1,{iterlim:1},'budget',2],
    [.7901867873967294,{},'numerical',1]]) {
    assert.throws(()=>api.zip.fit(x,{pi},controls),e=> {
      assert.equal(e.kind,kind);assert.equal(e.candidates.length,count);
      assert.ok(Number.isFinite(e.loglik_lower));assert.ok(e.loglik_upper>=e.loglik_lower);
      for(const c of e.candidates)assert.equal(c.pi,pi);
      return true;
    });
  }
  const fit=api.zip.fit(x,{pi:.9});
  assert.ok(Math.abs(fit.estimates.lambda-9.994983435397758)<1e-10);
});

test('ZIP held tiny rate attains all-zero inflation maximum', () => {
  const fit=api.zip.fit([0,0],{lambda:1e-20});
  assert.equal(fit.estimates.pi,1);assert.equal(fit.logLik,0);
});

test('tlambda certificates keep contact ties, budget and collapse witnesses', () => {
  assert.throws(()=>api.tlambda.fit([-.25,.25],{sigma:1,lambda:1.5}), e=> {
    assert.equal(e.kind,'tie');assert.equal(e.candidates.length,2);
    assert.ok(Math.abs(e.candidates[0].mu+5/12)<1e-12);
    assert.ok(Math.abs(e.candidates[1].mu-5/12)<1e-12);return true;
  });
  assert.throws(()=>api.tlambda.fit([-80/9,80/9],{sigma:1,lambda:-1},{reltol:1e-12,iterlim:64}), e=> {
    assert.equal(e.kind,'budget');assert.ok(Number.isFinite(e.loglik_lower));return true;
  });
  assert.throws(()=>api.tlambda.fit([2,2,5,9],{lambda:-1}), e=> {
    assert.equal(e.kind,'nomle');assert.ok(Number.isFinite(e.limit_loglik));
    assert.ok(Math.abs(e.limit_loglik-(-6*Math.log(2)-2*Math.log(3)-2*Math.log(7)))<1e-12);return true;
  });
  assert.throws(()=>api.tlambda.fit([0,1,2],{lambda:-2}), e=> {
    assert.equal(e.kind,'numerical');assert.ok(Number.isFinite(e.limit_loglik));return true;
  });
});

test('tlambda free-shape half ties retain a finite improvement and an unresolved global bound', () => {
  assert.throws(()=>api.tlambda.fit([0,0,1,2]), e=> {
    assert.equal(e.kind,'numerical');assert.ok(Number.isFinite(e.limit_loglik));
    assert.ok(e.loglik_lower>e.limit_loglik);assert.equal(e.loglik_upper,Infinity);return true;
  });
});

test('snorm held certificate preserves opposite-face ties and root failures', () => {
  assert.throws(()=>api.snorm.fit([0,0,0],{sd:1}),e=> {
    assert.equal(e.kind,'tie');assert.equal(e.candidates.length,2);
    assert.deepEqual(new Set(e.candidates.map(c=>c.model)),new Set(['hnorm','unavailable:reflected-half-normal']));return true;
  });
  assert.throws(()=>api.snorm.fit([-2,-1,-.4,0,.2,.8,1,1.3,2,3],{mean:.2,sd:1.4},{iterlim:1}),e=> {
    assert.equal(e.kind,'numerical');assert.equal(e.loglik_upper,Infinity);return true;
  });
});

test('snorm reflected finite modes remain a comparison tie', () => {
  assert.throws(()=>api.snorm.fit([-1,0,1],{mean:0,sd:.755}),e=> {
    assert.equal(e.kind,'tie');assert.equal(e.candidates.length,2);
    assert.ok(e.candidates.every(c=>c.model===''));
    assert.ok(Math.abs(e.candidates[0].xi*e.candidates[1].xi-1)<1e-7);return true;
  });
});

test('snorm constant sample with held mean has a constrained half-normal supremum', () => {
  const m=Math.sqrt(2/Math.PI);
  for(const value of [-2,2])assert.throws(()=>api.snorm.fit([value,value],{mean:0},{iterlim:1}),e=> {
    assert.equal(e.kind,'limit');assert.equal(e.model,value<0?'hnorm':'unavailable:reflected-half-normal');
    assert.equal(e.estimates.mu,value);assert.ok(Math.abs(e.estimates.sigma-2/m)<1e-12);
    assert.ok(Math.abs(e.limit_loglik-2*Math.log(m*m/2))<1e-12);return true;
  });
});
test('CMP ledger budget retains witnesses; huge modes are numerical', () => {
  assert.throws(()=>api.cmp.fit([0,0,0,20000],{lambda:5000/5001},{iterlim:1}),e=> {
    assert.equal(e.kind,'budget');assert.ok(e.candidates.length>0);
    assert.ok(Number.isFinite(e.loglik_lower));assert.equal(e.loglik_upper,Infinity);
    assert.match(e.message,/Raising iterlim/);return true;
  });
  assert.throws(()=>api.cmp.fit([0,1,2],{lambda:Math.exp(100),nu:.1}),e=> {
    assert.equal(e.kind,'numerical');assert.doesNotMatch(e.message,/Raising iterlim/);return true;
  });
});

test('CMB shared CMP work remains available at higher iterlim', () => {
  const x=[0,0,1,1,2,2,2,3,3,4];
  const a=api.cmb.fit(x,{}, {iterlim:100});
  const b=api.cmb.fit(x,{}, {iterlim:2000});
  assert.deepEqual(a.estimates,b.estimates);assert.equal(a.logLik,b.logLik);
});

test('Fully held CMP evaluation uses its fixed cap and fails numerically', () => {
  assert.throws(()=>api.cmp.fit([0,1,2],{lambda:.999999,nu:1e-12},{iterlim:1}),e=> {
    assert.equal(e.kind,'numerical');assert.doesNotMatch(e.message,/Raising iterlim/);return true;
  });
});

test('bbinom fixed-size coefficient certificate and numerical accuracy stop', () => {
  const x=[...Array(10).fill(0),...Array(12).fill(1),...Array(9).fill(2),...Array(4).fill(3)];
  for(const held of [{size:3},{size:3,alpha:2},{size:3,beta:3}]) {
    const f=api.bbinom.fit(x,held);
    assert.ok(Math.abs(f.estimates.alpha-2)<2e-10);
    assert.ok(Math.abs(f.estimates.beta-3)<3e-10);
  }
  for(const held of [{size:10},{size:10,alpha:2},{size:10,beta:3}]) {
    assert.throws(()=>api.bbinom.fit([0,1,1,2,2,3,4,5,7,10],held,{reltol:1e-18}),e=>{
      assert.equal(e.kind,'numerical');assert.equal(e.loglik_upper,Infinity);
      assert.ok(e.candidates.length>0);return true;
    });
  }
});

test('bbinom near-binomial shape enclosure survives double-width WASM arithmetic', () => {
  const x=[...Array(12501).fill(0),...Array(37499).fill(1),...Array(37499).fill(2),...Array(12501).fill(3)];
  const f=api.bbinom.fit(x,{size:3});
  assert.ok(Math.abs(f.estimates.alpha/18749.5-1)<1e-10);
  assert.ok(Math.abs(f.estimates.beta/18749.5-1)<1e-10);
});

test('st conditional location and alpha-scale certificates retain stops', () => {
  assert.throws(()=>api.st.fit([-2,2],{omega:1,alpha:0,nu:1}), e=>{
    assert.equal(e.kind,'tie');
    assert.ok(e.candidates.some(c=>Math.abs(c.xi-Math.sqrt(3))<1e-8));
    assert.ok(e.candidates.some(c=>Math.abs(c.xi+Math.sqrt(3))<1e-8));return true;
  });
  assert.throws(()=>api.st.fit([-3,-1,-.2,.1,.8,2,4],{omega:1,alpha:2,nu:4},{iterlim:1}), e=>e.kind==='budget' && e.candidates.length>0);
  for(const sign of [-1,1]) {
    assert.throws(()=>api.st.fit([2,2+sign,2+2*sign,2+4*sign],{xi:2,nu:4}), e=>{
      assert.equal(e.kind,'limit');assert.equal(e.estimates.mu,2);assert.ok(e.estimates.sigma>0);return true;
    });
  }
  for(const held of [{xi:0,alpha:0,nu:1},{xi:0,nu:1}]) {
    assert.throws(()=>api.st.fit([0,1],held),e=>e.kind==='nomle' && Number.isFinite(e.loglik_lower) && Number.isFinite(e.loglik_upper));
  }
});

test('st constant-data location-skew faces retain the sharp global bound',()=>{
  assert.throws(()=>api.st.fit([2,2,2],{omega:1,nu:4}),e=>{
    assert.equal(e.kind,'tie');assert.deepEqual(e.candidates.map(c=>c.model),['ht','unavailable:reflected-half-t']);
    assert.ok(Number.isFinite(e.loglik_lower));assert.ok(Number.isFinite(e.loglik_upper));return true;
  });
});

test('st symmetric all-nuisance reduction transports the normal face',()=>{
  assert.throws(()=>api.st.fit([-2,-1,0,1,2],{alpha:0}),e=>{
    assert.equal(e.kind,'limit');assert.equal(e.model,'sn');assert.equal(e.estimates.alpha,0);
    assert.ok(e.estimates.omega>0);return true;
  });
});

test('st unresolved thin-tail score arithmetic remains a numerical stop',()=>{
  assert.throws(()=>api.st.fit([-1000,-10,0,1,2],{omega:1,alpha:1e30,nu:1e20}),e=>e.kind==='numerical');
});

test('st constant-data free-nu corner keeps both half-normal faces',()=>{
  assert.throws(()=>api.st.fit([2,2,2],{omega:1}),e=>{
    assert.equal(e.kind,'tie');assert.deepEqual(e.candidates.map(c=>c.model),['hnorm','unavailable:reflected-half-normal']);
    assert.ok(Number.isFinite(e.loglik_lower));assert.ok(Number.isFinite(e.loglik_upper));return true;
  });
});

test('Newton-family unrepresentable coefficients are numerical stops', () => {
  const u=Number.MIN_VALUE;
  for(const [family,x,held] of [['gumbel',[0,u],{}],['weibull',[u,2*u],{}],
      ['foldnorm',[0,u],{}],['pig',[0,2],{mean:1e-158}]]) {
    assert.throws(()=>api[family].fit(x,held),e=>
      e.kind==='numerical' && /requested coefficient accuracy/.test(e.message));
  }
});

test('folded-normal boundary likelihood tie receives local score polish', () => {
  const e=.0028867513459481287,r=api.foldnorm.fit([e,e,1]);
  assert.ok(Math.abs(r.estimates.mu/.0037268054741952707381-1)<1e-10);
  assert.ok(Math.abs(r.estimates.sigma/.57734305210147493127-1)<1e-10);
});

test('small positive folded-normal basin retains coefficient accuracy', () => {
  const r=api.foldnorm.fit([.02,.02,1]);
  assert.ok(Math.abs(r.estimates.mu/.025828388643715272867-1)<1e-10);
  assert.ok(Math.abs(r.estimates.sigma/.57700337463490557989-1)<1e-10);
});

test('unresolved folded-normal quartic identification is numerical', () => {
  assert.throws(()=>api.foldnorm.fit([1,1,1,Math.sqrt(9+4*Math.sqrt(6))]),e=>e.kind==='numerical');
  assert.equal(api.foldnorm.fit([0,0,1]).estimates.mu,0);
});

test('conditional scalar fits retain coefficient accuracy', () => {
  const x=[2.4349403590372,2.434940359039635,2.43494035904207,2.434940359044505,2.434940359049375];
  assert.ok(Math.abs(api.weibull.fit(x,{scale:x[0]}).estimates.shape/263511819158.377827834-1)<1e-10);
  for(const [data,kappa,ref] of [[[1,2,3],1e-14,61111111111110.9798701],
      [[1e-200,1e200],1,1],[[1e-200,1e200,1e200,1e200],1/3,4.16333634234433733222e183]])
    assert.ok(Math.abs(api.lomax.fit(data,{kappa}).estimates.lambda/ref-1)<1e-10);
});

test('conditional free coefficients reject unrepresentable subnormals', () => {
  const u=Number.MIN_VALUE;
  for(const [family,data,held] of [['gumbel',[0,u],{sigma:u}],['weibull',[u,2*u],{shape:1}],
      ['lomax',[1e158,2e158],{kappa:1e158}]])
    assert.throws(()=>api[family].fit(data,held),e=>e.kind==='numerical');
});
