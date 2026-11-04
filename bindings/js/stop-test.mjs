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

test('Burr and reciprocal nearly-constant structured budget', () => {
  const x=[0.99999900000050002,0.99999910526355817,0.99999921052662744,0.9999993157897078,0.99999942105279915,0.99999952631590161,0.99999963157901528,0.99999973684213994,0.99999984210527559,0.99999994736842246,1.0000000526315804,1.0000001578947493,1.0000002631579294,1.0000003684211205,1.0000004736843227,1.0000005789475359,1.0000006842107605,1.000000789473996,1.0000008947372423,1.0000010000005];
  for (const [family,data] of [['burr',x],['invburr',x.map(v=>1/v)]]) {
    assert.throws(() => api[family].fit(data), e => {
      assert.equal(e.kind,'budget');assert.ok(e.candidates.length>=2);
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
