import {families} from './families.mjs';
export {families} from './families.mjs';
import createModule from '../../build/js/univariateML.mjs';

/** Load the private pilot. Copy boundaries return owned Float64Arrays. */
export default async function create(options = {}) {
  const module = await createModule(options);
  return Object.fromEntries(families.map(id => [id, {
    d: (x, params, {log = false} = {}) => module[`d${id}`](x, params, true, log),
    p: (x, params, {lower = true, log = false} = {}) => module[`p${id}`](x, params, lower, log),
    q: (x, params, {lower = true, log = false} = {}) => module[`q${id}`](x, params, lower, log),
    r: (n, params, seed) => {
      if (!Number.isSafeInteger(n) || n < 0 || n > 0xffffffff) throw new RangeError('n must be an unsigned 32-bit integer.');
      if (!Number.isInteger(seed) || seed < 0 || seed > 0xffffffff) throw new RangeError('seed must be an unsigned 32-bit integer.');
      return module[`r${id}`](n, params, seed);
    },
    fit: (x, held = {}, control = {}) => {
      const result = module[`fit${id}`](x, held, control);
      if (result.error) throw new Error(result.error);
      return result;
    },
  }]));
}
