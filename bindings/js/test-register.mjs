// Registration alone is partitioned: each original callback executes unchanged.
// Static cross-family regressions stay in changed-family runs; generated family
// loops explicitly call selectedFamily before registering their tests.
import test from 'node:test';
const families = process.env.UMVL_TEST_FAMILIES ? process.env.UMVL_TEST_FAMILIES.split(',') : null;
export const selectedFamily = id => !families || families.includes(id);
const group = process.env.UMVL_JS_GROUP;
const groups = Number(process.env.UMVL_JS_GROUP_COUNT ?? '16');
export default function register(name, ...args) {
  let hash = 2166136261;
  for (const char of name) hash = Math.imul(hash ^ char.charCodeAt(0), 16777619) >>> 0;
  if (group === undefined || hash % groups === Number(group)) return test(name, ...args);
}
