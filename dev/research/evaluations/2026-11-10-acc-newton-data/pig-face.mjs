import create from '../../../../build/js/univariateML.mjs';
const m=await create(),x=new Float64Array(20003);x.fill(2,10002,20002);x[20002]=3;
const r=m.fitpig(x,{},{});console.log(JSON.stringify(r));
