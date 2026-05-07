# instructions/

Working notes and porting references used during the de-R-ification of
univariateML. Markdown files here are design notes; subdirectories vendor
upstream source as **temporary porting reference**.

## Vendored porting references (delete after port)

The following directories hold a snapshot of upstream source we are
porting algorithms from. Files in them are read-only references —
**never edit** them in place. Once the corresponding distributions are
ported into `src/ml/distributions/`, the directory is deleted as part of
the migration's cleanup commit (see `~/.claude/plans/`).

| Directory             | Upstream                              | License | Phase   |
|-----------------------|---------------------------------------|---------|---------|
| `extraDistr/`         | github.com/twolodzko/extraDistr        | GPL-2   | Phase C |
| `actuar/`             | gitlab.com/vigou3/actuar               | GPL-2+  | Phase C |

The previous round used `instructions/nmath/` (vendored R Mathlib sources)
the same way; it was deleted in commit `522f0d5` once the boost::math
backend pivot landed.

## License note

Vendoring GPL-licensed source as a **porting reference** is fine: we read
the algorithms (which are not copyrightable) and re-implement them in our
own style atop `boost::math` and `<cmath>`. The vendored source is removed
from the tree before the package ships, so univariateML's published
distribution (MIT-licensed) contains no GPL code.
