import {readFileSync,writeFileSync} from 'node:fs';
import {gzipSync} from 'node:zlib';
import {performance} from 'node:perf_hooks';
import {pathToFileURL} from 'node:url';
const results=[];
for(const name of process.argv.slice(2)) {
 const path=`build/js/${name}.mjs`,wasm=readFileSync(`build/js/${name}.wasm`),js=readFileSync(path);
 const times=[];
 for(let i=0;i<5;i++) {
  const start=performance.now();
  const {default:create}=await import(pathToFileURL(`${process.cwd()}/${path}`).href+`?trial=${i}`);
  await create();times.push(performance.now()-start);
 }
 results.push({name,wasm:wasm.length,wasmGzip:gzipSync(wasm).length,js:js.length,jsGzip:gzipSync(js).length,loadMs:times});
}
writeFileSync('build/scratch/module-layout.json',JSON.stringify(results,null,2));
console.log(JSON.stringify(results,null,2));
