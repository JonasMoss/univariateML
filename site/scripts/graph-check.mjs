import {readFileSync} from 'node:fs';
import {graphData,atlasSvg,mapSvg} from '../src/lib/graph.ts';
const data=graphData(JSON.parse(readFileSync(new URL('../src/data/families.json',import.meta.url))));
process.stdout.write(JSON.stringify([mapSvg(data),...data.groups.flatMap(g=>[atlasSvg(data,g),atlasSvg(data,g,true)])]));
