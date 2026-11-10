import create from '../../../../build/js/univariateML.mjs';
import {readFileSync} from 'node:fs';
const m=await create();
const rows=JSON.parse(readFileSync(new URL('./replay-cases.json',import.meta.url)));
for(const {case:c} of rows) {
 const r=m['fit'+c.family](new Float64Array(c.data),c.held,{});
 console.log(JSON.stringify({index:c.index,id:c.family,estimates:r.estimates||{},error:r.error||''}));
}
