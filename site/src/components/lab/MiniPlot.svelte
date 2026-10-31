<script>
 // Infobox plot: density/mass, CDF or quantile at chosen coefficients.
 // All evaluation through the C++ WebAssembly binding; only axis layout here.
 import {onMount} from 'svelte';
 let {family, labels=[]} = $props();
 const num=v=>{const s=String(v).trim().toLowerCase();return s==='inf'?Infinity:s==='-inf'?-Infinity:Number(v);};
 const required=p=>['design','threshold'].includes(p.kind);
 const bounds=p=>String(p.domain).replace(/[()[\]]/g,'').split(',').map(num);
 const discrete=family.support.type==='integer';
 const supportBounds=[num(family.support.lower),num(family.support.upper)];
 function valid(p,v){
  const [lo,hi]=bounds(p),d=String(p.domain);
  if(Number.isNaN(v))return false;
  if(p.integer&&!Number.isInteger(v))return false;
  if(Number.isFinite(lo)&&(d.startsWith('(')?v<=lo:v<lo))return false;
  if(Number.isFinite(hi)&&(d.endsWith(')')?v>=hi:v>hi))return false;
  return Number.isFinite(v)||(v===hi&&d.endsWith(']'))||(v===lo&&d.startsWith('['));
 }
 function scale(p){
  const [lo,hi]=bounds(p),v=num(p.simulate),open=[String(p.domain).startsWith('('),String(p.domain).endsWith(')')];
  if(p.integer){const a=Number.isFinite(lo)?lo+(open[0]?1:0):v-10,b=Number.isFinite(hi)?hi-(open[1]?1:0):Math.max(3*v,v+10);return {kind:'lin',min:a,max:b,step:1};}
  if(Number.isFinite(lo)&&Number.isFinite(hi)){const d=(hi-lo)/1000;return {kind:'lin',min:lo+(open[0]?d:0),max:hi-(open[1]?d:0),step:'any'};}
  if(lo===0&&v>0&&!Number.isFinite(hi))return {kind:'log',min:Math.log(v/20),max:Math.log(v*20),step:'any'};
  const w=5*Math.max(1,Math.abs(v));return {kind:'lin',min:Number.isFinite(lo)?lo+(open[0]?w/1000:0):v-w,max:Number.isFinite(hi)?hi:v+w,step:'any'};
 }
 const scales=family.params.map(scale);
 let api=$state(null), error=$state(''), view=$state('density'), log=$state(false);
 let values=$state(family.params.map(p=>num(p.simulate)));
 let frame=$state(family.params.map(p=>num(p.simulate)));
 let bad=$state(family.params.map(()=>false));
 const toSlider=(i,v)=>scales[i].kind==='log'?Math.log(v):v;
 function move(i,e,commit){const raw=+e.target.value;values[i]=scales[i].kind==='log'?Math.exp(raw):raw;bad[i]=false;if(commit)frame=[...values];}
 function exact(i,e){const v=num(e.target.value);if(!valid(family.params[i],v)){bad[i]=true;return;}bad[i]=false;values[i]=v;frame=[...values];}
 const show=x=>Number.isFinite(x)?Number(x.toPrecision(4)).toString():String(x);

 // Axes: round ticks; snap the x-range to a nearby finite support bound.
 function niceStep(span,count){const raw=span/count,mag=10**Math.floor(Math.log10(raw)),f=raw/mag;return (f<=1?1:f<=2?2:f<=2.5?2.5:f<=5?5:10)*mag;}
 function ticks(lo,hi,count,integer=false){
  if(!(hi>lo))return [lo];
  let st=niceStep(hi-lo,count);if(integer)st=Math.max(1,Math.round(st));
  const out=[];for(let t=Math.ceil(lo/st-1e-9)*st;t<=hi+1e-9*st;t+=st)out.push(Math.abs(t)<st*1e-9?0:t);
  return out;
 }
 const tick=t=>Math.abs(t)>=1e5||(t!==0&&Math.abs(t)<1e-4)?t.toExponential(1):Number(t.toPrecision(6)).toString();
 const W=320,H=206,L=40,R=10,T=24,B=22;
 const P0=.005,P1=.995;
 const curves=$derived.by(()=>{
  if(!api)return null;
  try{
   const model=api[family.id],vals=$state.snapshot(values),fr=$state.snapshot(frame),useLog=log&&view!=='quantile';
   let left,right,xs,ys,n,plotLeft,plotRight;
   if(view==='quantile'){
    n=200;xs=Float64Array.from({length:n},(_,i)=>P0+(P1-P0)*i/(n-1));
    ys=Array.from(model.q(xs,vals));plotLeft=0;plotRight=1;
   }else{
    left=model.q(new Float64Array([P0]),fr)[0];right=model.q(new Float64Array([P1]),fr)[0];
    if(!Number.isFinite(left)||!Number.isFinite(right))return {error:'Plot range is not finite for these values.'};
    const w=right-left;
    if(Number.isFinite(supportBounds[0])&&left-supportBounds[0]<.25*w)left=supportBounds[0];
    if(Number.isFinite(supportBounds[1])&&supportBounds[1]-right<.25*w)right=supportBounds[1];
    if(discrete){left=Math.floor(left);right=Math.ceil(right);if(left===right){left-=1;right+=1;}}
    if(right<=left)return {error:'Plot range is empty for these values.'};
    n=discrete?Math.min(400,right-left+1):240;
    xs=discrete?Float64Array.from({length:n},(_,i)=>left+i):Float64Array.from({length:n},(_,i)=>left+(right-left)*i/(n-1));
    ys=Array.from(view==='density'?model.d(xs,vals,{log:useLog}):model.p(xs,vals,{log:useLog}));
    plotLeft=discrete?left-.5:left;plotRight=discrete?right+.5:right;
   }
   const finite=ys.filter(Number.isFinite).sort((a,b)=>a-b);
   if(!finite.length)return {error:'No finite values to plot.'};
   let low=finite[0],high=finite[finite.length-1];
   if(view==='density'&&!useLog){low=0;if(finite.length>10){const q=finite[Math.floor(.97*(finite.length-1))];if(high>4*q)high=1.5*q;}}
   if(view==='cdf'&&!useLog){low=0;high=1;}
   if(view==='cdf'&&useLog)high=0;
   if(useLog)low=Math.max(low,high-40);
   if(high-low<=1e-9*Math.max(1,Math.abs(high))){const c=(high+low)/2;low=c-1;high=c+1;}
   const yst=niceStep(high-low,4);
   if(!(view==='cdf'&&!useLog)){low=view==='density'&&!useLog?0:Math.floor(low/yst+1e-9)*yst;high=Math.ceil(high/yst-1e-9)*yst;}
   const span=high-low;
   const xAt=x=>L+(x-plotLeft)*(W-L-R)/(plotRight-plotLeft), yAt=y=>H-B-(H-B-T)*(Math.min(Math.max(y,low-.05*span),high+.05*span)-low)/span;
   const step=discrete&&view!=='density';
   let d='';
   if(discrete&&view==='density')d=ys.map((y,i)=>Number.isFinite(y)?`M${xAt(xs[i]).toFixed(1)},${(H-B).toFixed(1)}V${yAt(y).toFixed(1)}`:'').join('');
   else{let pen=false;for(let i=0;i<n;i++){const y=ys[i];if(!Number.isFinite(y)){pen=false;continue;}const X=xAt(xs[i]).toFixed(1),Y=yAt(y).toFixed(1);d+=!pen?`M${X},${Y}`:step?`H${X}V${Y}`:`L${X},${Y}`;pen=true;}}
   const xt=view==='quantile'?[0,.25,.5,.75,1]:ticks(left,right,5,discrete);
   const yt=ticks(low,high,4);
   return {d,dots:discrete&&view==='density',xt:xt.map(t=>({at:xAt(t),label:tick(t)})),yt:yt.map(t=>({at:yAt(t),label:tick(t)}))};
  }catch(e){return {error:e.message};}
 });
 const ylabel=$derived(view==='quantile'?'Q(p)':(log?'log ':'')+(view==='density'?(discrete?'p(x)':'f(x)'):'F(x)'));
 const xlabel=$derived(view==='quantile'?'p':'x');
 onMount(async()=>{try{const url=`${location.origin}/binding/index.mjs`;const {default:create}=await import(/* @vite-ignore */ url);api=await create();}catch(e){error=e.message;}});
</script>
<div class="miniplot">
 <div class="views" role="group" aria-label="Plot">
  <button type="button" aria-pressed={view==='density'} onclick={()=>view='density'}>{discrete?'PMF':'PDF'}</button>
  <button type="button" aria-pressed={view==='cdf'} onclick={()=>view='cdf'}>CDF</button>
  <button type="button" aria-pressed={view==='quantile'} onclick={()=>view='quantile'}>Quantile</button>
  <label class="log"><input type="checkbox" bind:checked={log} disabled={view==='quantile'}/> log</label>
 </div>
 {#if error}<p role="alert">{error}</p>{:else if !api}<div class="placeholder" style={`aspect-ratio:${W}/${H}`}>Loading…</div>
 {:else if curves?.error}<p role="alert" class="plot-error">{curves.error}</p>
 {:else if curves}
 <svg viewBox={`0 0 ${W} ${H}`} role="img" aria-label={`${ylabel} at the chosen coefficients`}>
  <defs><clipPath id={`clip-${family.id}`}><rect x={L} y={T-4} width={W-L-R} height={H-B-T+4}/></clipPath></defs>
  {#each curves.yt as t}<path d={`M${L},${t.at.toFixed(1)}H${W-R}`} class="grid"/><text x={L-4} y={t.at+3} text-anchor="end">{t.label}</text>{/each}
  {#each curves.xt as t}<path d={`M${t.at.toFixed(1)},${T}V${H-B}`} class="grid"/><text x={t.at} y={H-B+13} text-anchor="middle">{t.label}</text>{/each}
  <path d={`M${L},${T}V${H-B}H${W-R}`} class="axis"/>
  <text x={6} y={11} text-anchor="start" class="axis-label">{ylabel}</text>
  <text x={W-R} y={H-B-4} text-anchor="end" class="axis-label">{xlabel}</text>
  <path d={curves.d} class={curves.dots?'stem':'curve'} clip-path={`url(#clip-${family.id})`}/>
 </svg>
 {/if}
 {#each family.params as p,i}
  <div class="param">
   <label class="name" for={`exact-${family.id}-${p.name}`}><span class="sym">{@html labels[i]||p.name}</span> <code>{p.name}</code></label>
   {#if required(p)}<span class="kind">{p.kind}</span>
   {:else}<input type="range" aria-label={`${p.name} slider`} min={scales[i].min} max={scales[i].max} step={scales[i].step} value={toSlider(i,values[i])} oninput={e=>move(i,e,false)} onchange={e=>move(i,e,true)}/>{/if}
   <input id={`exact-${family.id}-${p.name}`} class="exact" class:bad={bad[i]} type="text" inputmode="decimal" value={show(values[i])} onchange={e=>exact(i,e)} aria-invalid={bad[i]} title={`${p.name} in ${p.domain}`}/>
  </div>
  {#if bad[i]}<p class="warn">{p.name} must lie in {p.domain}{p.integer?' (integer)':''}.</p>{/if}
 {/each}
 <p class="hint">Type a value or drag; axes rescale on release.</p>
</div>
<style>
 .miniplot{font-family:system-ui,sans-serif;font-size:.78rem}
 svg{width:100%;display:block;background:#fffdf8}
 .axis{fill:none;stroke:#8a8f88;stroke-width:1}
 .grid{fill:none;stroke:#ebe8de;stroke-width:1}
 .curve{fill:none;stroke:#23645e;stroke-width:2}
 .stem{fill:none;stroke:#23645e;stroke-width:2.5}
 text{font-size:9.5px;fill:#656c66;font-variant-numeric:tabular-nums}
 .axis-label{font-style:italic;fill:#262b2b;font-size:10.5px}
 .views{display:flex;gap:.3rem;margin-bottom:.3rem;align-items:center;flex-wrap:wrap}
 .views button{font:inherit;padding:.1rem .6rem;border:1px solid #c9c8bc;background:#fffdf8;border-radius:3px;cursor:pointer}
 .views button[aria-pressed=true]{background:#23645e;color:#fff;border-color:#23645e}
 .log{margin-left:auto;display:flex;gap:.25rem;align-items:center;cursor:pointer}
 .log:has(input:disabled){opacity:.45}
 .param{display:grid;grid-template-columns:5.2rem 1fr 4.2rem;align-items:center;gap:.4rem;margin:.2rem 0}
 .name{display:flex;gap:.3rem;align-items:baseline;white-space:nowrap;overflow:hidden}
 .name code{font-size:.72rem;color:#656c66}
 .param input[type=range]{width:100%}
 .kind{color:#656c66;font-style:italic}
 .exact{width:100%;font:inherit;font-variant-numeric:tabular-nums;text-align:right;padding:.1rem .3rem;border:1px solid #c9c8bc;border-radius:3px;background:#fffdf8}
 .exact.bad{border-color:#b0412e;background:#fbecea}
 .warn{margin:0 0 .2rem;color:#b0412e}
 .plot-error{color:#b0412e}
 .hint{margin:.3rem 0 0;color:#656c66}
 .placeholder{display:grid;place-items:center;background:#fffdf8;color:#656c66}
</style>
