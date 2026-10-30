# univariateML hex logo

The current logo is **F**: a neon pipe whose smoke puffs are normal densities
with equal area and growing variance. `bindings/r/man/figures/logo.png` is
F rendered at 240 x 278.

The `logo_*.svg` files have their text converted to outlines, so they render
the same without the fonts installed. The other candidates are kept for
reference:

- A: portrait
- B: rowboat
- C: neon face
- D: smoke stack with a one-line title
- E: separate puffs

## Regenerate

```sh
python3 make_minimal.py   # D, E, F -> logo_{D,E,F}_src.svg (live text)
python3 make.py           # A, B, C -> logo_{A,B,C}_src.svg
inkscape logo_F_src.svg --export-text-to-path --export-plain-svg \
  --export-type=svg --export-filename=logo_F.svg
inkscape logo_F.svg --export-type=png -w 240 -h 278 \
  --export-filename=../../bindings/r/man/figures/logo.png
```

Fonts used by the `_src.svg` files:

- Comfortaa Bold (C–F)
- Inter (A–C)
- STIX Two Text (A, B)
