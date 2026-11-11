import {readFileSync} from 'node:fs';
import create from '../../../../bindings/js/index.mjs';
const api=await create();
const g=JSON.parse(readFileSync(new URL('../../../../univariateML-cpp/include/cppunivariate/distributions/sstd/golden.json',import.meta.url),'utf8'));
try{
  const f=api.sstd.fit(new Float64Array(g.data));
  console.log(JSON.stringify({index:40001,error:'',estimates:f.estimates}));
}catch(e){
  console.log(JSON.stringify({index:40001,error:e.message,estimates:{}}));
}
