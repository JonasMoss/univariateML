import create from '../../../../bindings/js/index';
const api = await create();
const x = new Float64Array([0, 1, 2]);
api.lgser.fit(x, {}, {zero: 'modified'});
api.pois.fit(x, {}, {truncate: [0, 3], iterlim: 1});
api.binom.fit(x, {}, {truncate: [0, 3], iterlim: 512, reltol: 1e-10});
api.zeta.fit(x, {}, {truncate: [1, 3]});
