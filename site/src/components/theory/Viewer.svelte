<script>
 import {onMount} from 'svelte';
 import {specs,preset,parseSample,profile,profileSvg,faces,facesSvg,faceSample,browserApi} from './model.mjs';
 export let initial;
 export let mode='profile';
 export let gallery=false;
 let result=initial,id=initial.id||'gamma',text=initial.sample.join(', '),api=null,busy=false,error='',seed=20261101,heldMean=initial.heldMean??0,heldSd=initial.heldSd??1;
 onMount(async()=>{try{api=await browserApi();}catch(e){error='Interactive fitting could not load. The default figure remains available. '+e.message;}});
 async function update(action='paste') {
  busy=true;error='';
  await new Promise(resolve=>setTimeout(resolve,0));
  try {
   let sample;
   if(action==='preset'){sample=mode==='faces'?faceSample:preset(id);text=sample.join(', ');}
   else if(action==='simulate'){
    const params={gamma:[2,1],lomax:[1,2],std:[0,1,5],nbinom:[2,.5],burr:[2,2,1]};
    sample=Array.from(api[mode==='faces'?'norm':id].r(60,mode==='faces'?[1,1.3]:params[id],Number(seed)));text=sample.join(', ');
   }else sample=parseSample(text);
   if(mode==='faces'){
    if(!Number.isFinite(Number(heldMean))||!Number.isFinite(Number(heldSd))||Number(heldSd)<=0)throw Error('Held mean must be finite and held sd must be positive.');
    result=faces(api,sample,Number(heldMean),Number(heldSd));
   } else result=profile(api,id,sample);
  } catch(e){error=e.message;}finally{busy=false;}
 }
 const fmt=n=>Number(n).toPrecision(6);
</script>
<div class="theory-view" aria-busy={busy}>
 {#if !gallery}
 <fieldset disabled={!api||busy}>
  <legend>{mode==='faces'?'Change the held faces':'Explore a profile'}</legend>
  {#if mode==='profile'}<label>Family <select bind:value={id} on:change={()=>update('preset')}>{#each Object.entries(specs) as [key,s]}<option value={key}>{s.title}</option>{/each}</select></label>{:else}
  <label>Held mean <input type="number" step="0.1" bind:value={heldMean}/></label><label>Held sd <input type="number" min="0.001" step="0.1" bind:value={heldSd}/></label>{/if}
  <label class="sample">Observations (3–500)<textarea rows="3" bind:value={text}/></label>
  <div class="actions"><button on:click={()=>update()}>Fit pasted data</button><button on:click={()=>update('preset')}>Reset preset</button><label>Seed <input type="number" min="0" max="4294967295" step="1" bind:value={seed}/></label><button on:click={()=>update('simulate')}>Simulate 60 &amp; fit</button></div>
 </fieldset>
 {:else}
 <button disabled={!api||busy} on:click={()=>update('preset')}>Recompute preset</button>
 {/if}
 {#if error}<p class="error" role="alert">{error} The last displayed figure is retained.</p>{/if}
 {#if mode==='profile'}
 <p>{#if result.id==='lomax'}<math><mi>κ</mi><mo>→</mo><mi>∞</mi></math> while <math><mi>λ</mi><mi>κ</mi></math> tends to the fitted exponential rate.{:else if result.id==='std'}<math><mi>ν</mi><mo>→</mo><mi>∞</mi></math>; mean and standard deviation are refitted. This parameterization requires <math><mi>ν</mi><mo>&gt;</mo><mn>2</mn></math>.{:else if result.id==='nbinom'}Size grows and <math><mi>p</mi><mo>→</mo><mn>1</mn></math>, preserving the sample mean.{:else if result.id==='burr'}Scale is held at <math><mn>1</mn></math>. <math><msub><mi>k</mi><mn>2</mn></msub><mo>→</mo><mi>∞</mi></math> and <math><msub><mi>k</mi><mn>1</mn></msub><msub><mi>k</mi><mn>2</mn></msub></math> tends to the Pareto shape; its lower endpoint is <math><mn>1</mn></math>. This is a conditional escape.{:else}{specs[result.id].explanation}{/if}</p>
 <div class="figure">{@html profileSvg(result)}</div>
 <p class="legend"><span class="curve">━</span> held-fit profile{#if result.free.fit} · ● free fit{/if} {#if result.limit?.fit}<span class="limit">┄</span> maximised limit model{/if}</p>
 <div class="diagnostic" aria-live="polite">
 {#if result.free.fit}<p>Free fit: log-likelihood {fmt(result.free.fit.logLik)}; {Object.entries(result.free.fit.estimates).map(([k,v])=>`${k} = ${fmt(v)}`).join(', ')}.</p>{:else}<p><strong>Fitter message:</strong> <code>{result.free.error}</code></p>{/if}
 {#if result.limit?.fit}<p>Limit <a href={`/distributions/${specs[result.id].limit}/`}>{specs[result.id].limit}</a>: maximised log-likelihood {fmt(result.limit.fit.logLik)}{specs[result.id].limitHeld?' (endpoint b held at 1)':''}. Last grid value minus limit: {result.points.at(-1).y===null?'unavailable (held fit failed)':fmt(result.points.at(-1).y-result.limit.fit.logLik)}.</p>{:else if result.limit?.error}<p>Limit fit failed: {result.limit.error}</p>{/if}
 <p>{result.sample.length} observations; {result.points.filter(p=>p.y===null).length} failed held fits out of {result.points.length}. A finite grid illustrates the likelihood; it does not certify a global maximum or escape.</p>
 {#each [...new Set(result.points.filter(p=>p.error).map(p=>p.error))] as message}<p class="error">Held-fit gap: {message}</p>{/each}
 {#if result.free.fit?.warnings?.length}<p>{result.free.fit.warnings.join('; ')}</p>{/if}
 </div>
 <details><summary>Sample and numerical profile</summary><p class="sample-data">{result.sample.join(', ')}</p><table><thead><tr><th>{specs[result.id].coefficient}</th><th>Profile log-likelihood</th></tr></thead><tbody>{#each result.points as p}<tr><td>{fmt(p.x)}</td><td>{p.y===null?p.error:fmt(p.y)}</td></tr>{/each}</tbody></table></details>
 {:else}
 <div class="figure">{@html facesSvg(result)}</div>
 <p class="legend">● free fit · <span class="limit">● mean held</span> · <span class="sd">● sd held</span>. Dashed lines are the two held faces.</p>
 <p>Contours are 0.5, 2, 5, 10 and 20 log-likelihood units below the free maximum. They are numerical level curves, without a confidence-region interpretation.</p>
 <table aria-label="Normal held fits"><thead><tr><th>Fit</th><th>Mean</th><th>Sd</th><th>Log-likelihood</th></tr></thead><tbody>{#each [['Free',result.free],['Mean held',result.meanFit],['Sd held',result.sdFit]] as [name,fit]}<tr><td>{name}</td><td>{fmt(fit.estimates.mean)}</td><td>{fmt(fit.estimates.sd)}</td><td>{fmt(fit.logLik)}</td></tr>{/each}</tbody></table>
 <details><summary>Sample</summary><p class="sample-data">{result.sample.join(', ')}</p></details>
 {/if}
</div>
<style>
 .theory-view{margin:1.5rem 0 2.5rem}fieldset{border:1px solid #c9c8bc;padding:1rem;min-width:0;font:.82rem system-ui,sans-serif}legend{padding:0 .4rem}label{flex-wrap:wrap;margin:.3rem .5rem .6rem 0}textarea{width:100%;max-width:100%;font:inherit;padding:.5rem;background:#fffdf8;border:1px solid #a8aea6}.sample{display:block}.actions{display:flex;flex-wrap:wrap;align-items:center;gap:.5rem}.actions label{margin:0}input{width:9rem}.figure{margin-top:1rem}.figure :global(svg){display:block;width:100%;height:auto;border:1px solid #dedbcf}.figure :global(text){font:13px system-ui,sans-serif;fill:#454b45}.legend{font:.8rem system-ui,sans-serif}.curve{color:#23645e}.limit{color:#a65330}.sd{color:#6553a0}.diagnostic{border-left:3px solid #b27542;padding:0 1rem;font-size:.9rem}.error{color:#8d3024;overflow-wrap:anywhere}.sample-data{overflow-wrap:anywhere;font-size:.8rem}button:disabled{cursor:wait}table{font-variant-numeric:tabular-nums}@media(max-width:650px){.figure :global(text){font-size:18px}}
</style>
