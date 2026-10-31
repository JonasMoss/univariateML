<script>
 // Infobox plot: density (or mass) and CDF with coefficient sliders.
 // All evaluation through the C++ WebAssembly binding; no numerics here.
 import {onMount} from 'svelte';
 let {family, labels=[]} = $props();
 const num=v=>{const s=String(v).trim().toLowerCase();return s==='inf'?Infinity:s==='-inf'?-Infinity:Number(v);};
 const required=p=>['design','threshold'].includes(p.kind);
 const bounds=p=>String(p.domain).replace(/[()[\]]/g,'').split(',').map(num);
 const discrete=family.support.type==='integer';
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
 const toSlider=(i,v)=>scales[i].kind==='log'?Math.log(v):v;
 function move(i,e,commit){const s=scales[i];const raw=+e.target.value;values[i]=s.kind==='log'?Math.exp(raw):raw;if(commit)frame=[...values];}
 const fmt=x=>Number.isFinite(x)?(Math.abs(x)>=1e4||(Math.abs(x)<1e-3&&x!==0)?x.toExponential(2):Number(x.toPrecision(3)).toString()):String(x);
 const W=320,H=190,L=40,R=8,T=10,B=24;
 const P0=.005,P1=.995;
 const curves=$derived.by(()=>{
  if(!api)return null;
  try{
   const model=api[family.id],vals=$state.snapshot(values),fr=$state.snapshot(frame),useLog=log&&view!=='quantile';
   let left,right,xs,ys,n;
   if(view==='quantile'){
    left=P0;right=P1;n=200;
    xs=Float64Array.from({length:n},(_,i)=>P0+(P1-P0)*i/(n-1));
    ys=Array.from(model.q(xs,vals));
   }else{
    left=model.q(new Float64Array([P0]),fr)[0];right=model.q(new Float64Array([P1]),fr)[0];
    if(discrete){left=Math.floor(left);right=Math.ceil(right);if(left===right){left-=1;right+=1;}}
    if(!Number.isFinite(left)||!Number.isFinite(right)||right<=left)return {error:'Plot range is not finite for these values.'};
    n=discrete?Math.min(400,right-left+1):200;
    xs=discrete?Float64Array.from({length:n},(_,i)=>left+i):Float64Array.from({length:n},(_,i)=>left+(right-left)*i/(n-1));
    ys=Array.from(view==='density'?model.d(xs,vals,{log:useLog}):model.p(xs,vals,{log:useLog}));
   }
   const finite=ys.filter(Number.isFinite).sort((a,b)=>a-b);
   if(!finite.length)return {error:'No finite values to plot.'};
   let low=finite[0],high=finite[finite.length-1];
   if(view==='density'&&!useLog){low=0;if(finite.length>10){const q=finite[Math.floor(.97*(finite.length-1))];if(high>4*q)high=1.5*q;}}
   if(view==='cdf'&&!useLog){low=0;high=1;}
   if(view==='cdf'&&useLog)high=0;
   if(useLog)low=Math.max(low,high-40);
   if(high-low<=1e-9*Math.max(1,Math.abs(high))){const c=(high+low)/2;low=c-1;high=c+1;}
   const span=high-low,base=view==='density'&&!useLog?low:null;
   const xAt=x=>L+(x-left)*(W-L-R)/(right-left), yAt=y=>H-B-(H-B-T)*(Math.min(Math.max(y,low-.05*span),high+.05*span)-low)/span;
   const step=discrete&&view!=='density';
   let d='';
   if(discrete&&view==='density')d=ys.map((y,i)=>Number.isFinite(y)?`M${xAt(xs[i]).toFixed(1)},${(base===null?H-B:yAt(base)).toFixed(1)}V${yAt(y).toFixed(1)}`:'').join('');
   else{let pen=false;for(let i=0;i<n;i++){const y=ys[i];if(!Number.isFinite(y)){pen=false;continue;}const X=xAt(xs[i]).toFixed(1),Y=yAt(y).toFixed(1);d+=!pen?`M${X},${Y}`:step?`H${X}V${Y}`:`L${X},${Y}`;pen=true;}}
   return {d,left,right,low,high,mid:(left+right)/2,dots:discrete&&view==='density'};
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
 {:else if curves?.error}<p role="alert">{curves.error}</p>
 {:else if curves}
 <svg viewBox={`0 0 ${W} ${H}`} role="img" aria-label={`${ylabel} at the chosen coefficients`}>
  <defs><clipPath id={`clip-${family.id}`}><rect x={L} y={T-2} width={W-L-R} height={H-B-T+2}/></clipPath></defs>
  <path d={`M${L},${T}V${H-B}H${W-R}`} class="axis"/>
  <text x={L} y={H-6} text-anchor="start">{fmt(curves.left)}</text>
  <text x={(L+W-R)/2} y={H-6} text-anchor="middle">{fmt(curves.mid)}</text>
  <text x={W-R} y={H-6} text-anchor="end">{fmt(curves.right)}</text>
  <text x={L-4} y={T+8} text-anchor="end">{fmt(curves.high)}</text>
  <text x={L-4} y={H-B} text-anchor="end">{fmt(curves.low)}</text>
  <text x={L-4} y={(T+H-B)/2} text-anchor="end" class="axis-label">{ylabel}</text>
  <text x={W-R} y={H-B-4} text-anchor="end" class="axis-label">{xlabel}</text>
  <path d={curves.d} class={curves.dots?'stem':'curve'} clip-path={`url(#clip-${family.id})`}/>
 </svg>
 {/if}
 {#each family.params as p,i}
  <div class="param">
   <span class="sym">{@html labels[i]||p.name}</span>
   {#if required(p)}<span class="fixed">{fmt(values[i])} <small>({p.kind})</small></span>
   {:else}<input type="range" aria-label={`${p.name}`} min={scales[i].min} max={scales[i].max} step={scales[i].step} value={toSlider(i,values[i])} oninput={e=>move(i,e,false)} onchange={e=>move(i,e,true)}/>
   <output>{fmt(values[i])}</output>{/if}
  </div>
 {/each}
 <p class="hint">Axes rescale when you release a slider.</p>
</div>
<style>
 .miniplot{font-family:system-ui,sans-serif;font-size:.78rem}
 svg{width:100%;display:block;background:#fffdf8}
 .axis{fill:none;stroke:#8a8f88;stroke-width:1}
 .curve{fill:none;stroke:#23645e;stroke-width:2}
 .stem{fill:none;stroke:#23645e;stroke-width:2.5}
 text{font-size:10px;fill:#656c66}
 .views{display:flex;gap:.3rem;margin-bottom:.3rem;align-items:center;flex-wrap:wrap}
 .log{margin-left:auto;display:flex;gap:.25rem;align-items:center;cursor:pointer}
 .log:has(input:disabled){opacity:.45}
 .axis-label{font-style:italic;fill:#262b2b}
 .views button{font:inherit;padding:.1rem .6rem;border:1px solid #c9c8bc;background:#fffdf8;border-radius:3px;cursor:pointer}
 .views button[aria-pressed=true]{background:#23645e;color:#fff;border-color:#23645e}
 .param{display:grid;grid-template-columns:2.2rem 1fr 4rem;align-items:center;gap:.4rem;margin:.15rem 0}
 .param input{width:100%}
 .param output,.fixed{text-align:right;font-variant-numeric:tabular-nums}
 .fixed{grid-column:2/4;text-align:left}
 .hint{margin:.3rem 0 0;color:#656c66}
 .placeholder{display:grid;place-items:center;background:#fffdf8;color:#656c66}
</style>
