import {existsSync, mkdirSync, rmSync, cpSync, readFileSync, writeFileSync} from 'node:fs';
import {spawnSync} from 'node:child_process';
import {families} from '../../bindings/js/families.mjs';
const sdk=process.env.EMSDK || `${process.env.HOME}/emsdk`;
const enabled=process.env.SITE_WASM !== '0' && existsSync(`${sdk}/emsdk_env.sh`);
rmSync('public/binding',{recursive:true,force:true});
if(enabled){
 const result=spawnSync('bash',['bindings/js/build.sh'],{cwd:'..',stdio:'inherit'});
 if(result.status!==0)process.exit(result.status || 1);
 mkdirSync('public/binding',{recursive:true});
 for(const file of ['univariateML.mjs','univariateML.wasm'])cpSync(`../build/js/${file}`,`public/binding/${file}`);
 writeFileSync('public/binding/index.mjs',readFileSync('../bindings/js/index.mjs','utf8').replace('../../build/js/univariateML.mjs','./univariateML.mjs'));
 cpSync('../bindings/js/families.mjs','public/binding/families.mjs');
}else console.warn('WARNING: Emscripten unavailable (or SITE_WASM=0); explorers omitted.');
mkdirSync('src/data',{recursive:true});
writeFileSync('src/data/binding.json',JSON.stringify(enabled?families:[]));
