// Run from the worktree root after the WASM build.
import {readFileSync} from 'node:fs';
import create from '../../../../bindings/js/index.mjs';

const api=await create();
const fixture='bindings/r/tests/testthat/fixtures/short-window.tsv';
for(const line of readFileSync(fixture,'utf8').trimEnd().split('\n').slice(1)) {
  const [,family,params,lo,hi,op,lower,log,input]=line.split('\t');
  if(op==='refuse')continue;
  const [value]=api[family][op]([Number(input)],params.split(':').map(Number),
    {truncate:[Number(lo),Number(hi)],lower:lower==='1',log:log==='1'});
  console.log(value.toPrecision(17));
}
