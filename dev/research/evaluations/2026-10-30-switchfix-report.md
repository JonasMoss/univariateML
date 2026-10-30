# switchfix (TASK-78.9)

No commits or git writes. Goldens, truth tables, README domains and fitter
implementations are unchanged. All builds and suites ran through
`/tmp/umvl-simbox/run`; simbox remained reachable.

Initial uncapped R suite: **FAIL 12 | WARN 48 | SKIP 0 | PASS 32292**.
The complete run found no failure families beyond the five originally reported.

- **PIG, three failures:** `shape=0` is an attained zero point mass in the
  declared domain, but d/p/q/r rejected it. Added the exact point-mass branch
  to every operation, including both tails, log scales, quantile endpoints
  and sampling. Existing mean-domain restrictions remain in force.
- **ZINB, six failures:** the declared, attainable `size=0` boundary was
  rejected. Added the exact zero point mass throughout d/p/q/r; sampler
  validates parameters before taking any degenerate branch. The CDF also
  handles `pi=1` and `prob=1` directly. No generated validation change is
  needed: generated wrappers call these scalar functions, including density
  validation before sampling. R tests exercise those generated entrypoints.
- **Discrete normal, one failure:** the quantile was correct; its R test
  compared a log-probability input against the rounded ordinary probability.
  At mean=0, sd=1, P(X<=-1)=1/2 exactly. Mpmath at 100 digits gives
  binary64 log(.5) minus exact log(.5) = 2.31904681384629961549e-17,
  and exp(binary64 log(.5)) minus .5 = 1.15952340692314982119e-17.
  Thus q(.5)=-1 but q(log(.5),log.p=TRUE)=0. The test now asserts that
  distinction explicitly; C++ regression checks adjacent log inputs and
  both tails. No d/p/q/r implementation change was warranted.
- **ZIB, one error:** the L-BFGS-B oracle reached `prob=1-1e-9`, `pi=0`;
  zero mass was evaluated through log1p(expm1(8*log1p(-prob))), cancelling
  to -Inf instead of the finite 8*log1p(-prob), about -165.786.
  Fixed the density with a log sum and the CDF with direct binomial log
  tails and mixture log sums. The oracle itself is unchanged. Closed-form
  regressions check this zero mass, tiny upper tails, and the near-one
  zero-mass log density -4e-20 (size=8, prob=1e-20, pi=.5).
- **Inverse Burr, one error, still open:** `set.seed(3); y <- rburr(200,
  shape1=1,shape2=2,scale=3); x <- 1/y; mlinvburr(x)` fails at default
  controls. This is a well-posed interior sample, not grounds for replacing
  the data. The independent multistart oracle on y gives logLik
  -441.505283853845, versus Weibull -456.581869923870 and Pareto
  -579.556588898714, with parameters (1.19985791098,2.15385898701,
  3.24854714510). Inverse Burr logLik is -20.609810876162.
  The C++ result is `InvBurrStatus::IterationFailure` (4), 16 iterations,
  estimates (1.199857975994,2.153858732312,0.307829898845).
  The profiled Burr multistart chooses a `LineSearchFailure` (2) with
  average objective 1.1552877368250123 and gradient norm 3.332e-8 over
  an `Ok` result with objective 1.1552877368250130: a difference of
  6.66e-16. Selection ignores convergence status. `iterlim=10000` still
  fails; `gradtol=1e-7` succeeds with the same estimates and likelihood.
  The sample and failing default-control test are preserved; no fitter
  change or relaxed assertion masks this issue.

Warnings: fractional-x checks for dnorm/dgamma now explicitly expect the
shell's discrete-density warning. Optimizer oracles in invexp, Kumar,
lst, ZINB and ZIP now reject invalid parameter proposals before calling
the density (overflow/underflow of exp, zero probability, negative Kumar
shapes). These domain guards retain the objective on its declared domain;
no warnings are merely suppressed.

Checks: added closed-form C++ and generated-layer R tests for both attained
boundaries; retained all interior truth-table and sampler checks. The
focused R run had no failures; its one remaining ZINB oracle-domain warning
was subsequently guarded before the full rerun.

Final uncapped R suite: **FAIL 1 | WARN 0 | SKIP 0 | PASS 32373**.
The only remaining failure is the preserved inverse Burr default-control
regression above. All other reported failures and all 48 warnings are gone.
Final `just cpp-test`: **6/6 CTest targets passed, 0 failed**
(151.75 seconds): **869 doctest cases, 1,209,540 assertions, all passed**.
This includes every distribution truth-table and sampler check and the
new boundary/tail regressions.

Evidence and exact binary64 sample: `/tmp/umvl-codex/switchfix/`.
Key files: `full-initial.log`, `full-final.log`, `cpp-final.log`,
`dnorm-rounding.txt`, `invburr-data.txt`, `failure-200-3.rds`,
`invburr-status.cpp`, `invburr-status.log`, `burr-profile.cpp`,
`burr-profile.log`, `invburr-detail.log`.

Open decision: repairing inverse Burr multistart selection/convergence
requires a separate authorized fitter change. A green default-control R
suite cannot be claimed while this failure remains.
