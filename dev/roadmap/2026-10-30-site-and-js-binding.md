# Website and JavaScript binding

2026-10-30. Refines "Distribution catalog and website" in
[package-and-documentation](2026-10-30-package-and-documentation.md);
Jonas set the content goals and left the machinery to Claude.

## Goals (Jonas)

- A site general enough to grow into interactive tooling: parameter
  sliders, density visualisations, estimation in the browser.
- Each distribution page is a micropaper: definition, the key one to
  three references, the ML theory (existence, limits, what happens at the
  edges), short notices of what happens in practice, and documentation of
  the implementation and its troubles. Implementation detail can be
  hidden or switched on.
- Usage in Python, R, C++ and JS, selected once at the top of the page.
- Ambitious in scope, delivered in stages.

## Architecture

- **JS is a binding, not a port.** The header-only C++ core compiles to
  WebAssembly with Emscripten (embind) in `bindings/js/`, a thin shell
  like `bindings/r/` and the planned Python binding. The same goldens and
  truth tables check it. The site is its first consumer; publishing an npm
  package is a later decision. Why: a hand-written JS implementation is a
  second numerical code base to keep in agreement; the core has no
  exceptions, I/O or R dependencies, so WebAssembly is cheap.
- **Site: Astro, in `site/`.** Static pages with interactive islands
  (Svelte) only where needed; content collections read the family
  README dossiers directly, so there is no second catalog. Mathematics is
  converted to MathML at build time; no MathJax or KaTeX scripts. Static
  search (Pagefind). The Node toolchain stays inside `site/`.
- Alternatives considered: Quarto (strong for R and math, awkward for
  app-like interactivity and WebAssembly); SvelteKit (most freedom, but
  search, tabs and navigation are hand-built); Observable Framework
  (strong visualisation, weaker document structure, client-side TeX);
  VitePress and Docusaurus (documentation-shaped, less suited to custom
  islands).

## Family page

Rendered from the README front matter and prose, in this order; sections
without content are omitted, never shown as placeholders.

1. Title, one-line summary, classification, key references.
2. Notices generated from `limits`, `holds` and `fit_domain`: when the MLE
   does not exist, which limit model the fit points to, held-parameter and
   restriction behaviour.
3. Definition: density, support, parameters and domains.
4. Explorer (from stage 3): sliders over the coefficient domains, density
   and CDF plots, simulate-and-fit, fit pasted data.
5. ML theory: existence, uniqueness, limit models; proofs folded.
6. Usage tabs R | C++ | Python | JS, synchronised by a header selector
   that persists across pages; unavailable bindings say "planned".
7. Implementation and troubles, folded by default and controlled by a
   header detail switch: method, numerical notes, known issues, validation
   evidence.
8. Decision history (folded) and the full reference list.

New front-matter fields (summary, classification, key references,
aliases) are added while filling dossiers (TASK-101.6), not invented by
the site.

## Stages

1. **Scaffold**: catalog with search and filters, family pages with the
   static sections, language tabs (R and C++ working), validation guide and
   theory articles as pages; pilot on exp, gamma, gb2, then all families.
2. **JS binding pilot**: d/p/q/r and fitters for a few families in
   WebAssembly, checked against goldens and truth tables in Node; size and
   speed recorded.
3. **Explorer**: sliders and plots, then simulate-and-fit, pasted data,
   model selection, all through the JS binding.
4. **Theory visualisations**: profile likelihoods, fits escaping to limit
   models, held-parameter faces.

Hosting (GitHub Pages, custom domain) stays with the release review
(migration step 8).
