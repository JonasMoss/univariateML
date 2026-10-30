import path from 'node:path';
// npm scripts execute in site/. Avoid import.meta.url: Vite moves server modules
// during a production build, so their output locations are not source locations.
export const repoRoot = path.resolve(process.cwd(), '..');
