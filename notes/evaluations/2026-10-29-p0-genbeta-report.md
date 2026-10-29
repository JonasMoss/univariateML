# TASK-79: generalized beta group — still incomplete

- The independent external-density sweep ran on simbox for all 13 families,
  all proper hold subsets, defaults at n=2, 3, 5, 20, 100, 1000, ties,
  constant samples, named limit samples and relevant scale positions.
  Among 1,961 sample/hold rows, 641 fits returned with the requested holds
  and all agreed with the optimizer within 1e-6*n; 86 returned with a hold
  ignored; 1,234 stopped. CSVs and checkpointed runnable scripts are saved.
- Proved unrestricted free-endpoint divergences and several complete held
  cases; the note distinguishes those arguments from numerical checks.
  Pareto IV and Feller–Pareto collapse have c=shape1*shape2, but also have
  independent endpoint singularities with exponents shape2 and
  shape2*shape3. Thus c>=1 alone does not prevent unbounded likelihood.
- Under the proposed bounds, free-endpoint Pareto III/IV and Feller–Pareto
  have logistic limits, nsbeta has a normal limit, and genbeta has a gamma
  limit. Reference-density paths are in `limit-paths.csv`. Face existence
  alone does not prove which samples actually escape.
- R7 discrepancy: extraDistr nsbeta accepts zero beta shapes in p/q/r as
  endpoint point masses, while README domains exclude zero. A density-based
  fit across continuous and point-mass members needs a measure decision.
- Incomplete: exact global classifications for many GB2/genbeta and free
  endpoint hold subsets; a proof that any combined R5 restrictions suffice;
  exhaustive reference-domain checks; complete holds/fit_domain YAML; and
  certification that every joint limit path has been enumerated. These
  need further mathematical work: multistart optimization and the existing
  numerical face searches do not establish global nonexistence or exhaust
  the closure. The supplied YAML is a partial proposal, not a migration.
- The optimizer excludes equality at free endpoints, cannot certify global
  maxima, and external densities may lose accuracy near extreme shapes.
  The reported fit agreement therefore has those limits. The full detail
  and remaining question marks are in the accompanying evaluation note.
- No package code, README, test, golden, board, or Git history was changed.
