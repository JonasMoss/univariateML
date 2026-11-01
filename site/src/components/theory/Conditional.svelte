<script>
 import {onMount} from 'svelte';
 import {conditional,conditionalSvg,browserApi} from './model.mjs';
 export let initial;
 let result=initial,api,error='';
 onMount(async()=>{try{api=await browserApi();}catch(e){error=e.message;}});
 function update(){try{result=conditional(api);error='';}catch(e){error=e.message;}}
 const fmt=n=>Number(n).toPrecision(6);
</script>
<div class="conditional">
 <button disabled={!api} on:click={update}>Recompute held-rate example</button>
 {#if error}<p role="alert">{error}</p>{/if}
 <div class="figure">{@html conditionalSvg(result)}</div>
 <p>● black: free fit; green: <math><mi>a</mi><mo>=</mo><mn>0.2</mn></math>; purple: <math><mi>a</mi><mo>=</mo><mn>1</mn></math>; rust: <math><mi>a</mi><mo>=</mo><mn>2</mn></math>. The open circle marks an excluded boundary supremum, not a fitted estimate.</p>
 <table aria-label="Gompertz held fits"><thead><tr><th>Fit</th><th>a</th><th>b</th><th>Log-likelihood / diagnostic</th></tr></thead><tbody>
 <tr><td>Free</td><td>{fmt(result.free.fit.estimates.a)}</td><td>{fmt(result.free.fit.estimates.b)}</td><td>{fmt(result.free.fit.logLik)}</td></tr>
 {#each result.held as h}<tr><td>Rate held</td><td>{h.a}</td><td>{h.fit?fmt(h.fit.estimates.b):'→ 0 (excluded)'}</td><td>{h.fit?fmt(h.fit.logLik):h.error}</td></tr>{/each}
 </tbody></table>
 <details><summary>Sample</summary><p>{result.sample.join(', ')}</p></details>
</div>
<style>
 .figure{margin-top:1rem}.figure :global(svg){display:block;width:100%;height:auto;border:1px solid #dedbcf}.figure :global(text){font:13px system-ui,sans-serif;fill:#454b45}table{font-variant-numeric:tabular-nums}td{overflow-wrap:anywhere}p{font-size:.9rem}@media(max-width:650px){.figure :global(text){font-size:18px}}
</style>
