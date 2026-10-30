<script>
 import {onMount} from 'svelte';
 export let family; export let catalog;
 let api, error='', values=family.params.map(p=>Number(p.simulate)), held={}, probability=.5, log=false;
 let plotParams=[...values];
 let n=200, seed=123, sample=[], fit=null, fitError='', text='', ranking=[], busy=false;
 const fmt=x=>Number.isFinite(x)?Number(x).toPrecision(6):String(x);
 function bounds(p){const parts=p.domain.replace(/[()[\]]/g,'').split(',').map(x=>Number(x.trim().replace('inf','Infinity')));return parts;}
 function valid(v,p){const [lo,hi]=bounds(p);return Number.isFinite(v)&&v>lo&&v<hi;}
 function windowFor(p){const [lo,hi]=bounds(p),v=Number(p.simulate);return [Number.isFinite(lo)?lo: v-10*Math.max(1,Math.abs(v)),Number.isFinite(hi)?hi:v+10*Math.max(1,Math.abs(v))];}
 function logarithmic(p){return bounds(p)[0]===0 && Number(p.simulate)>0;}
 function slider(p,v){return logarithmic(p)?Math.log(v):v;}
 function sliderBounds(p){return logarithmic(p)?[Math.log(Number(p.simulate)/20),Math.log(Number(p.simulate)*20)]:windowFor(p);}
 function move(i,event){let next=[...values];next[i]=logarithmic(family.params[i])?Math.exp(+event.target.value):+event.target.value;values=next;fit=null;sample=[];}
 function exact(i,event){const next=[...values];next[i]=+event.target.value;values=next;fit=null;sample=[];}
 $: good=values.every((v,i)=>valid(v,family.params[i]));
 $: curves=plot(api,values,good,log,fit,sample,probability,plotParams);
 function plot(api,params,good,log,fit,sample,prob,range){
  if(!api||!good)return null;
  try{
   if(!Number.isFinite(prob)||prob<=0||prob>=1)throw Error('Choose a quantile probability strictly between 0 and 1.');
   const model=api[family.id], left=model.q(new Float64Array([.001]),range)[0],right=model.q(new Float64Array([.99]),range)[0];
   if(!Number.isFinite(left)||!Number.isFinite(right)||right<=left)throw Error('Plot range is not finite.');
   const xs=Float64Array.from({length:160},(_,i)=>left+(right-left)*i/159);
   const density=model.d(xs,params,{log}),cdf=model.p(xs,params);
   const fitted=fit?model.d(xs,family.params.map(p=>fit.estimates[p.name]),{log}):[];
   const all=[...density,...fitted].filter(Number.isFinite),low=log?Math.min(...all):0,high=Math.max(...all);
   const path=(ys,lo,hi)=>Array.from(ys,(y,i)=>Number.isFinite(y)?`${i?'L':'M'}${(30+i*540/159).toFixed(2)},${(180-150*(y-lo)/(hi-lo||1)).toFixed(2)}`:'').join(' ');
   const bins=Array.from({length:20},(_,i)=>{const a=left+i*(right-left)/20,count=sample.filter(x=>x>=a&&x<a+(right-left)/20).length;const d=count/(sample.length*(right-left)/20);const y=log?Math.log(d):d;return {x:30+i*27,y:Number.isFinite(y)?Math.max(30,Math.min(180,180-150*(y-low)/(high-low||1))):180};});
   return {d:path(density,low,high),c:path(cdf,0,1),f:path(fitted,low,high),left,right,low,high,bins,q:model.q(new Float64Array([prob]),params)[0]};
  }catch(e){return {error:e.message};}
 }
 onMount(async()=>{try{const url='/binding/index.mjs';const {default:create}=await import(/* @vite-ignore */ url);api=await create();}catch(e){error=e.message;}});
 async function simulate(){busy=true;fit=null;fitError='';await new Promise(r=>setTimeout(r,0));try{sample=Array.from(api[family.id].r(+n,values,+seed));const fixed=Object.fromEntries(family.params.filter(p=>held[p.name]).map(p=>[p.name,values[family.params.indexOf(p)]]));fit=api[family.id].fit(new Float64Array(sample),fixed);}catch(e){fitError=e.message;}finally{busy=false;}}
 function admits(f,x){const s=f.support;const lo=Number(String(s.lower).replace('inf','Infinity')),hi=Number(String(s.upper).replace('inf','Infinity'));return x.every(v=>(s.type!=='integer'||Number.isInteger(v))&&(s.closed[0]?v>=lo:v>lo)&&(s.closed[1]?v<=hi:v<hi));}
 async function rank(){busy=true;ranking=[];await new Promise(r=>setTimeout(r,0));try{const tokens=text.trim().split(/[\s,;]+/).filter(Boolean),x=tokens.map(Number);if(!text.trim()||x.some(v=>!Number.isFinite(v)))throw Error('Enter finite observations separated by spaces, commas or newlines.');ranking=catalog.map(f=>{if(!admits(f,x))return {id:f.id,error:'Support does not admit these observations.'};try{const result=api[f.id].fit(new Float64Array(x));return {id:f.id,result,aic:2*(Object.keys(result.estimates).length-result.fixed.length)-2*result.logLik};}catch(e){return {id:f.id,error:e.message};}}).sort((a,b)=>(a.aic??Infinity)-(b.aic??Infinity));}catch(e){ranking=[{id:'Input',error:e.message}];}finally{busy=false;}}
</script>
<section class="explorer" aria-label="Distribution explorer">
 <h2>Explore and fit</h2>
 <noscript>Enable JavaScript to use the local explorer.</noscript>
 <p>All calculations run locally through the C++ WebAssembly binding. Slider windows are illustrative; enter an exact value anywhere inside the stated domain.</p>
 {#if error}<p role="alert">{error}</p>{:else if !api}<p role="status">Loading local calculation module…</p>{/if}
 {#each family.params as p,i}
 <div class="parameter"><label for={`slider-${p.name}`}>{p.name} ({p.domain}){logarithmic(p)?' · logarithmic slider':''}</label>
 <input id={`slider-${p.name}`} aria-label={`${p.name} slider`} type="range" min={sliderBounds(p)[0]} max={sliderBounds(p)[1]} step="any" value={slider(p,values[i])} oninput={e=>move(i,e)}/>
 <label>{p.name} exact value <input type="number" step="any" value={values[i]} oninput={e=>exact(i,e)}/></label>
 <label><input type="checkbox" bind:checked={held[p.name]}/>Hold {p.name} when fitting</label></div>
 {/each}
 {#if !good}<p role="alert">Enter finite values strictly inside the coefficient domains.</p>{/if}
 <label><input type="checkbox" bind:checked={log}/>Log density</label>
 <button disabled={!good} onclick={()=>plotParams=[...values]}>Rescale plots to chosen values</button>
 <p>Horizontal axes stay fixed while dragging. Rescale to inspect a different window; tails outside the window are omitted.</p>
 {#if curves?.error}<p role="alert">{curves.error}</p>{:else if curves}
 <figure><figcaption>Density{log?' (log scale)':''}: x {fmt(curves.left)}–{fmt(curves.right)}; y {fmt(curves.low)}–{fmt(curves.high)}. Green: chosen values; orange: fit; grey: simulated histogram.</figcaption>
 <svg viewBox="0 0 600 210" role="img" aria-label="Density plot"><path d="M30 30V180H570" class="axis"/>{#if sample.length}{#each curves.bins as b}<rect x={b.x} y={b.y} width="26" height={180-b.y} fill="#cad0c8"/>{/each}{/if}<path class="density" d={curves.d}/><path class="fitted" d={curves.f}/></svg></figure>
 <figure><figcaption>CDF: x {fmt(curves.left)}–{fmt(curves.right)}; probability 0–1.</figcaption><svg viewBox="0 0 600 210" role="img" aria-label="CDF plot"><path d="M30 30V180H570" class="axis"/><path class="density" d={curves.c}/></svg></figure>
 <label>Quantile probability <input type="number" min="0.001" max="0.999" step=".01" bind:value={probability}/></label><p>Quantile: <output>{fmt(curves.q)}</output></p>
 {/if}
 <h3>Simulate and fit</h3><label>Observations n <input type="number" min="2" max="10000" step="1" bind:value={n}/></label>
 <label>Seed <input type="number" min="0" max="4294967295" step="1" bind:value={seed}/></label>
 <button disabled={!api||!good||busy||!Number.isInteger(n)||n<2||n>10000||!Number.isInteger(seed)||seed<0||seed>4294967295} onclick={simulate}>Simulate and fit</button>
 <div aria-live="polite">{#if busy}<p>Fitting…</p>{/if}{#if fitError}<p role="alert">{fitError}</p>{/if}
 {#if fit}<p class="fit-status">Status: {fit.status}; logLik {fmt(fit.logLik)}</p><table><thead><tr><th>Coefficient</th><th>True</th><th>Estimate</th></tr></thead><tbody>{#each family.params as p,i}<tr><th>{p.name}</th><td>{fmt(values[i])}</td><td>{fmt(fit.estimates[p.name])}{held[p.name]?' (held)':''}</td></tr>{/each}</tbody></table>{/if}</div>
 <h3>Your data</h3><label for="observations">Observations (spaces, commas or newlines)</label><textarea id="observations" rows="4" bind:value={text}></textarea>
 <button disabled={!api||busy} onclick={rank}>Fit and rank models</button><p>AIC ranks fitted models and does not test them. All binding families are considered; unsupported data and failed fits retain their diagnostics.</p>
 <div aria-live="polite" class="ranking">{#each ranking as row}<p><strong>{row.id}</strong>: {#if row.error}{row.error}{:else}AIC {fmt(row.aic)}; status {row.result.status}; logLik {fmt(row.result.logLik)}; {Object.entries(row.result.estimates).map(([k,v])=>`${k} = ${fmt(v)}`).join(', ')}{/if}</p>{/each}</div>
</section>
<style>
 .explorer{font-size:.9rem} .parameter{display:grid;gap:.4rem;margin:1rem 0;padding:.7rem;border:1px solid #c9c8bc}.parameter input[type=range]{width:100%}label{flex-wrap:wrap;margin:.3rem 0}input[type=number]{width:9rem}figure{margin:1rem 0}figcaption{font-size:.8rem}svg{width:100%;display:block}path{fill:none;stroke-width:2}.axis{stroke:#888}.density{stroke:#23645e}.fitted{stroke:#b45b21}textarea{display:block;width:100%;max-width:100%;font:inherit}button{margin:.5rem 0}.ranking p{overflow-wrap:anywhere}
</style>
