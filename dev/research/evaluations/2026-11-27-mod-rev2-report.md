# TASK-443 · Independent review of zero-modifier proofs

Reviewed all 249 TASK-440 proposals across 38 parent hold masks and seven families. The [join file](2026-11-27-mod-rev2-data/reviewed.csv) contains **246 rows**, of which **142 were accepted as proposed** and **104 were regraded or reworded**. **Three were rejected.** All 249 proposals have an atlas parent and satisfy the zero-modifier eligibility rule. The parent hold masks in this file omit modifier coefficients. A claim may distinguish free and held values of the modifier coefficient within a row.

The review uses the closed retained window `[1, Inf]` intersected with the integer parent support. For zero modification the positive counts determine the conditional likelihood and the zero count determines the Bernoulli factor. Free `p0` can equal 0 or 1. A held `p0` must give positive probability to the observed zero/nonzero categories. At `p0 = 1` on all zeros the positive component is unused. For free inflation and a sample with positives, the optimal total zero probability at a fixed parent is the larger of the empirical zero fraction and the parent zero mass. Equality gives `pi = 0` and the two likelihood branches agree in both value and first derivative on smooth parent charts. Held inflation uses its full objective. The scope of exact positive-sample face claims is the joint likelihood under the stated holds. [Modifier design](../../design/modifiers.md) · [Source proofs](2026-11-27-zero-mod-math-report.md) · [Atlas scope](../existence-atlas/overview.md)

An actual finite candidate at the largest reachable boundary height suffices for attainment. If a point has higher likelihood, its superlevel stays in a compact part of the feasible parameter space and a finite maximum follows by continuity. The real-arithmetic criterion can therefore be exact even when a terminating search or equality decision is open. The CMB conditional hull uses a finite carrier at each of finitely many sizes, with rank checked before using strict concavity. The CMP positive natural domain is convex, and its included geometric segment has finite dispersion-score moments by exponential domination. The proofs use these family-specific facts. [Critical-value rule](2026-11-27-zero-mod-math-report.md#critical-values-and-boundary-ties) · [CMB parent hull](2026-11-25-cmb-math-report.md#held-size) · [CMP conditional chart](2026-11-27-zero-mod-math-report.md#cmp-all-free-truncated)

## Row verdicts

| Family | Held | Modifier | Question | Verdict | Reason |
|---|---|---|---|---|---|
| bbinom | all free | zero:truncated | criterion | accepted | The fixed-size rational score and the growing-size recurrence exhaust the candidate and face comparison, including finite equality. |
| bbinom | all free | zero:truncated | failure | accepted | The fixed-size ratio limits and the summable or divergent growing-size recurrence bounds cover every compatible positive-likelihood escape. |
| bbinom | all free | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| bbinom | all free | zero:modified | failure | accepted | The separated Bernoulli factor is fixed at its selected value, so the hold-specific conditional winning limits transfer exactly on positive samples. |
| bbinom | all free | zero:inflated | criterion | regraded | The Bernoulli envelope settles seam equality and zero-only cases, and the held-inflation objective requires its own critical score. |
| bbinom | all free | zero:inflated | failure | regraded | The parent closure transfers through the inflation objective on positive samples, including its zero-mass comparison. |
| bbinom | alpha | zero:truncated | criterion | accepted | The fixed-size rational score and the growing-size recurrence exhaust the candidate and face comparison, including finite equality. |
| bbinom | alpha | zero:truncated | failure | accepted | The fixed-size ratio limits and the summable or divergent growing-size recurrence bounds cover every compatible positive-likelihood escape. |
| bbinom | alpha | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| bbinom | alpha | zero:modified | failure | accepted | The separated Bernoulli factor is fixed at its selected value, so the hold-specific conditional winning limits transfer exactly on positive samples. |
| bbinom | alpha | zero:inflated | criterion | regraded | The Bernoulli envelope settles seam equality and zero-only cases, and the held-inflation objective requires its own critical score. |
| bbinom | alpha | zero:inflated | failure | regraded | The parent closure transfers through the inflation objective on positive samples, including its zero-mass comparison. |
| bbinom | alpha+size | zero:truncated | criterion | accepted | The fixed-size rational score and the growing-size recurrence exhaust the candidate and face comparison, including finite equality. |
| bbinom | alpha+size | zero:truncated | failure | accepted | The fixed-size ratio limits and the summable or divergent growing-size recurrence bounds cover every compatible positive-likelihood escape. |
| bbinom | alpha+size | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| bbinom | alpha+size | zero:modified | failure | accepted | The separated Bernoulli factor is fixed at its selected value, so the hold-specific conditional winning limits transfer exactly on positive samples. |
| bbinom | alpha+size | zero:inflated | criterion | regraded | The Bernoulli envelope settles seam equality and zero-only cases, and the held-inflation objective requires its own critical score. |
| bbinom | alpha+size | zero:inflated | failure | regraded | The parent closure transfers through the inflation objective on positive samples, including its zero-mass comparison. |
| bbinom | beta | zero:truncated | criterion | accepted | The fixed-size rational score and the growing-size recurrence exhaust the candidate and face comparison, including finite equality. |
| bbinom | beta | zero:truncated | failure | accepted | The fixed-size ratio limits and the summable or divergent growing-size recurrence bounds cover every compatible positive-likelihood escape. |
| bbinom | beta | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| bbinom | beta | zero:modified | failure | accepted | The separated Bernoulli factor is fixed at its selected value, so the hold-specific conditional winning limits transfer exactly on positive samples. |
| bbinom | beta | zero:inflated | criterion | regraded | The Bernoulli envelope settles seam equality and zero-only cases, and the held-inflation objective requires its own critical score. |
| bbinom | beta | zero:inflated | failure | regraded | The parent closure transfers through the inflation objective on positive samples, including its zero-mass comparison. |
| bbinom | beta+size | zero:truncated | criterion | accepted | The fixed-size rational score and the growing-size recurrence exhaust the candidate and face comparison, including finite equality. |
| bbinom | beta+size | zero:truncated | failure | accepted | The fixed-size ratio limits and the summable or divergent growing-size recurrence bounds cover every compatible positive-likelihood escape. |
| bbinom | beta+size | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| bbinom | beta+size | zero:modified | failure | accepted | The separated Bernoulli factor is fixed at its selected value, so the hold-specific conditional winning limits transfer exactly on positive samples. |
| bbinom | beta+size | zero:inflated | criterion | regraded | The Bernoulli envelope settles seam equality and zero-only cases, and the held-inflation objective requires its own critical score. |
| bbinom | beta+size | zero:inflated | failure | regraded | The parent closure transfers through the inflation objective on positive samples, including its zero-mass comparison. |
| bbinom | size | zero:truncated | criterion | accepted | The fixed-size rational score and the growing-size recurrence exhaust the candidate and face comparison, including finite equality. |
| bbinom | size | zero:truncated | failure | accepted | The fixed-size ratio limits and the summable or divergent growing-size recurrence bounds cover every compatible positive-likelihood escape. |
| bbinom | size | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| bbinom | size | zero:modified | failure | accepted | The separated Bernoulli factor is fixed at its selected value, so the hold-specific conditional winning limits transfer exactly on positive samples. |
| bbinom | size | zero:inflated | criterion | regraded | The Bernoulli envelope settles seam equality and zero-only cases, and the held-inflation objective requires its own critical score. |
| bbinom | size | zero:inflated | failure | regraded | The parent closure transfers through the inflation objective on positive samples, including its zero-mass comparison. |
| bbinom | all free | zero:truncated | uniqueness | regraded | The exact empirical two-cell law is realized along the size-two ray and larger finite sizes put mass outside the sample. |
| bbinom | all free | zero:truncated | case:all-ones | accepted | Size one attains likelihood one with every compatible shape, and the recurrence identifies the applicable point-one escapes. |
| bbinom | alpha | zero:truncated | case:all-ones | accepted | Size one attains likelihood one with every compatible shape, and the recurrence identifies the applicable point-one escapes. |
| bbinom | alpha+size | zero:truncated | case:all-ones | accepted | At sizes above one sending free beta to infinity gives the point-one limit, while size one is flat. |
| bbinom | beta | zero:truncated | case:all-ones | accepted | Size one attains likelihood one with every compatible shape, and the recurrence identifies the applicable point-one escapes. |
| bbinom | beta+size | zero:truncated | case:all-ones | accepted | At sizes above one the ratios increase with alpha, giving the rare-alpha endpoint, while size one is flat. |
| bbinom | size | zero:truncated | case:all-ones | accepted | At sizes above one sending free beta to infinity gives the point-one limit, while size one is flat. |
| bnbinom | all free | zero:truncated | criterion | accepted | The normalized recurrence cases exhaust the hold-compatible escapes, so an actual equal or strictly better finite critical value suffices. |
| bnbinom | all free | zero:truncated | failure | accepted | The bounded and divergent recurrence regimes cover all subsequences, with the numerator swap respecting the designated holds. |
| bnbinom | all free | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| bnbinom | all free | zero:modified | failure | accepted | The separated Bernoulli factor is fixed at its selected value, so the hold-specific conditional winning limits transfer exactly on positive samples. |
| bnbinom | all free | zero:inflated | criterion | regraded | The Bernoulli envelope settles seam equality and zero-only cases, and the held-inflation objective requires its own critical score. |
| bnbinom | all free | zero:inflated | failure | regraded | The parent closure transfers through the inflation objective on positive samples, including its zero-mass comparison. |
| bnbinom | alpha | zero:truncated | criterion | accepted | The normalized recurrence cases exhaust the hold-compatible escapes, so an actual equal or strictly better finite critical value suffices. |
| bnbinom | alpha | zero:truncated | failure | accepted | The bounded and divergent recurrence regimes cover all subsequences, with the numerator swap respecting the designated holds. |
| bnbinom | alpha | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| bnbinom | alpha | zero:modified | failure | accepted | The separated Bernoulli factor is fixed at its selected value, so the hold-specific conditional winning limits transfer exactly on positive samples. |
| bnbinom | alpha | zero:inflated | criterion | regraded | The Bernoulli envelope settles seam equality and zero-only cases, and the held-inflation objective requires its own critical score. |
| bnbinom | alpha | zero:inflated | failure | regraded | The parent closure transfers through the inflation objective on positive samples, including its zero-mass comparison. |
| bnbinom | alpha+beta | zero:truncated | criterion | accepted | The normalized recurrence cases exhaust the hold-compatible escapes, so an actual equal or strictly better finite critical value suffices. |
| bnbinom | alpha+beta | zero:truncated | failure | accepted | The bounded and divergent recurrence regimes cover all subsequences, with the numerator swap respecting the designated holds. |
| bnbinom | alpha+beta | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| bnbinom | alpha+beta | zero:modified | failure | accepted | The separated Bernoulli factor is fixed at its selected value, so the hold-specific conditional winning limits transfer exactly on positive samples. |
| bnbinom | alpha+beta | zero:inflated | criterion | regraded | The Bernoulli envelope settles seam equality and zero-only cases, and the held-inflation objective requires its own critical score. |
| bnbinom | alpha+beta | zero:inflated | failure | regraded | The parent closure transfers through the inflation objective on positive samples, including its zero-mass comparison. |
| bnbinom | alpha+beta+size | zero:truncated | criterion | regraded | The fully held model requires evaluation and a positive conditioning event. |
| bnbinom | alpha+beta+size | zero:truncated | failure | regraded | A fully held parent domain consists of one feasible point. |
| bnbinom | alpha+beta+size | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| bnbinom | alpha+beta+size | zero:modified | failure | accepted | The separated Bernoulli factor is fixed at its selected value, so the hold-specific conditional winning limits transfer exactly on positive samples. |
| bnbinom | alpha+beta+size | zero:inflated | criterion | regraded | The held parent coefficients leave only the closed Bernoulli mixing optimization or evaluation. |
| bnbinom | alpha+beta+size | zero:inflated | failure | regraded | A fixed parent and closed inflation interval leave no escaping maximizing sequence. |
| bnbinom | alpha+size | zero:truncated | criterion | accepted | The normalized recurrence cases exhaust the hold-compatible escapes, so an actual equal or strictly better finite critical value suffices. |
| bnbinom | alpha+size | zero:truncated | failure | accepted | The bounded and divergent recurrence regimes cover all subsequences, with the numerator swap respecting the designated holds. |
| bnbinom | alpha+size | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| bnbinom | alpha+size | zero:modified | failure | accepted | The separated Bernoulli factor is fixed at its selected value, so the hold-specific conditional winning limits transfer exactly on positive samples. |
| bnbinom | alpha+size | zero:inflated | criterion | regraded | The Bernoulli envelope settles seam equality and zero-only cases, and the held-inflation objective requires its own critical score. |
| bnbinom | alpha+size | zero:inflated | failure | regraded | The parent closure transfers through the inflation objective on positive samples, including its zero-mass comparison. |
| bnbinom | beta | zero:truncated | criterion | accepted | The normalized recurrence cases exhaust the hold-compatible escapes, so an actual equal or strictly better finite critical value suffices. |
| bnbinom | beta | zero:truncated | failure | accepted | The bounded and divergent recurrence regimes cover all subsequences, with the numerator swap respecting the designated holds. |
| bnbinom | beta | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| bnbinom | beta | zero:modified | failure | accepted | The separated Bernoulli factor is fixed at its selected value, so the hold-specific conditional winning limits transfer exactly on positive samples. |
| bnbinom | beta | zero:inflated | criterion | regraded | The Bernoulli envelope settles seam equality and zero-only cases, and the held-inflation objective requires its own critical score. |
| bnbinom | beta | zero:inflated | failure | regraded | The parent closure transfers through the inflation objective on positive samples, including its zero-mass comparison. |
| bnbinom | beta+size | zero:truncated | criterion | accepted | The normalized recurrence cases exhaust the hold-compatible escapes, so an actual equal or strictly better finite critical value suffices. |
| bnbinom | beta+size | zero:truncated | failure | accepted | The bounded and divergent recurrence regimes cover all subsequences, with the numerator swap respecting the designated holds. |
| bnbinom | beta+size | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| bnbinom | beta+size | zero:modified | failure | accepted | The separated Bernoulli factor is fixed at its selected value, so the hold-specific conditional winning limits transfer exactly on positive samples. |
| bnbinom | beta+size | zero:inflated | criterion | regraded | The Bernoulli envelope settles seam equality and zero-only cases, and the held-inflation objective requires its own critical score. |
| bnbinom | beta+size | zero:inflated | failure | regraded | The parent closure transfers through the inflation objective on positive samples, including its zero-mass comparison. |
| bnbinom | size | zero:truncated | criterion | accepted | The normalized recurrence cases exhaust the hold-compatible escapes, so an actual equal or strictly better finite critical value suffices. |
| bnbinom | size | zero:truncated | failure | accepted | The bounded and divergent recurrence regimes cover all subsequences, with the numerator swap respecting the designated holds. |
| bnbinom | size | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| bnbinom | size | zero:modified | failure | accepted | The separated Bernoulli factor is fixed at its selected value, so the hold-specific conditional winning limits transfer exactly on positive samples. |
| bnbinom | size | zero:inflated | criterion | regraded | The Bernoulli envelope settles seam equality and zero-only cases, and the held-inflation objective requires its own critical score. |
| bnbinom | size | zero:inflated | failure | regraded | The parent closure transfers through the inflation objective on positive samples, including its zero-mass comparison. |
| bnbinom | all free | zero:truncated | uniqueness | regraded | The exact rational recurrence bounds localize the complete competing level to a compact interior box off the swap seam. |
| bnbinom | all free | zero:truncated | case:all-ones | accepted | Increasing free alpha at fixed numerator shapes takes the positive law to the point mass at one, which no finite law attains. |
| bnbinom | alpha | zero:truncated | case:all-ones | accepted | Every free numerator shape increases each recurrence ratio, so sending all free numerator shapes to zero selects the normalized rare-shape law. |
| bnbinom | alpha+beta | zero:truncated | case:all-ones | accepted | Every free numerator shape increases each recurrence ratio, so sending all free numerator shapes to zero selects the normalized rare-shape law. |
| bnbinom | alpha+beta+size | zero:truncated | case:all-ones | accepted | The held positive law gives one evaluated value with no parameter escape. |
| bnbinom | alpha+size | zero:truncated | case:all-ones | accepted | Every free numerator shape increases each recurrence ratio, so sending all free numerator shapes to zero selects the normalized rare-shape law. |
| bnbinom | beta | zero:truncated | case:all-ones | accepted | Increasing free alpha at fixed numerator shapes takes the positive law to the point mass at one, which no finite law attains. |
| bnbinom | beta+size | zero:truncated | case:all-ones | accepted | Increasing free alpha at fixed numerator shapes takes the positive law to the point mass at one, which no finite law attains. |
| bnbinom | size | zero:truncated | case:all-ones | accepted | Increasing free alpha at fixed numerator shapes takes the positive law to the point mass at one, which no finite law attains. |
| cmb | all free | zero:truncated | criterion | accepted | Each admissible design has the stated conditional moment or endpoint solution, and the finite design comparison includes equality. |
| cmb | all free | zero:truncated | failure | regraded | The hull-face account omits flat small-design escapes that can tie an attained conditional optimum. |
| cmb | all free | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| cmb | all free | zero:modified | failure | regraded | The hull-face account omits flat small-design escapes that can tie an attained conditional optimum. |
| cmb | all free | zero:inflated | criterion | regraded | The Bernoulli envelope settles seam equality and zero-only cases, and the held-inflation objective requires its own critical score. |
| cmb | all free | zero:inflated | failure | regraded | At size one free dispersion can diverge while the parent Bernoulli law stays fixed, so the exposed-law description misses a co-winning escape. |
| cmb | nu | zero:truncated | criterion | accepted | Each admissible design has the stated conditional moment or endpoint solution, and the finite design comparison includes equality. |
| cmb | nu | zero:truncated | failure | regraded | The hull-face account omits flat small-design escapes that can tie an attained conditional optimum. |
| cmb | nu | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| cmb | nu | zero:modified | failure | regraded | The hull-face account omits flat small-design escapes that can tie an attained conditional optimum. |
| cmb | nu | zero:inflated | criterion | regraded | The Bernoulli envelope settles seam equality and zero-only cases, and the held-inflation objective requires its own critical score. |
| cmb | nu | zero:inflated | failure | regraded | The parent closure transfers through the inflation objective on positive samples, including its zero-mass comparison. |
| cmb | nu+prob | zero:truncated | criterion | accepted | Each admissible design has the stated conditional moment or endpoint solution, and the finite design comparison includes equality. |
| cmb | nu+prob | zero:truncated | failure | accepted | Both canonical coefficients are fixed and there are only finitely many allowed sizes, so there is no positive-sample escape. |
| cmb | nu+prob | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| cmb | nu+prob | zero:modified | failure | accepted | The separated Bernoulli factor is fixed at its selected value, so the hold-specific conditional winning limits transfer exactly on positive samples. |
| cmb | nu+prob | zero:inflated | criterion | regraded | The Bernoulli envelope settles seam equality and zero-only cases, and the held-inflation objective requires its own critical score. |
| cmb | nu+prob | zero:inflated | failure | regraded | The parent closure transfers through the inflation objective on positive samples, including its zero-mass comparison. |
| cmb | nu+prob+size | zero:truncated | criterion | regraded | The fully held model requires evaluation and a positive conditioning event. |
| cmb | nu+prob+size | zero:truncated | failure | regraded | A fully held parent domain consists of one feasible point. |
| cmb | nu+prob+size | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| cmb | nu+prob+size | zero:modified | failure | accepted | The separated Bernoulli factor is fixed at its selected value, so the hold-specific conditional winning limits transfer exactly on positive samples. |
| cmb | nu+prob+size | zero:inflated | criterion | regraded | The held parent coefficients leave only the closed Bernoulli mixing optimization or evaluation. |
| cmb | nu+prob+size | zero:inflated | failure | regraded | A fixed parent and closed inflation interval leave no escaping maximizing sequence. |
| cmb | nu+size | zero:truncated | criterion | accepted | Each admissible design has the stated conditional moment or endpoint solution, and the finite design comparison includes equality. |
| cmb | nu+size | zero:truncated | failure | regraded | The hull-face account omits flat small-design escapes that can tie an attained conditional optimum. |
| cmb | nu+size | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| cmb | nu+size | zero:modified | failure | regraded | The hull-face account omits flat small-design escapes that can tie an attained conditional optimum. |
| cmb | nu+size | zero:inflated | criterion | regraded | The Bernoulli envelope settles seam equality and zero-only cases, and the held-inflation objective requires its own critical score. |
| cmb | nu+size | zero:inflated | failure | regraded | The parent closure transfers through the inflation objective on positive samples, including its zero-mass comparison. |
| cmb | prob | zero:truncated | criterion | accepted | Each admissible design has the stated conditional moment or endpoint solution, and the finite design comparison includes equality. |
| cmb | prob | zero:truncated | failure | regraded | The hull-face account omits flat small-design escapes that can tie an attained conditional optimum. |
| cmb | prob | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| cmb | prob | zero:modified | failure | regraded | The hull-face account omits flat small-design escapes that can tie an attained conditional optimum. |
| cmb | prob | zero:inflated | criterion | regraded | The Bernoulli envelope settles seam equality and zero-only cases, and the held-inflation objective requires its own critical score. |
| cmb | prob | zero:inflated | failure | regraded | At size one free dispersion can diverge while the parent Bernoulli law stays fixed, so the exposed-law description misses a co-winning escape. |
| cmb | prob+size | zero:truncated | criterion | accepted | Each admissible design has the stated conditional moment or endpoint solution, and the finite design comparison includes equality. |
| cmb | prob+size | zero:truncated | failure | regraded | The hull-face account omits flat small-design escapes that can tie an attained conditional optimum. |
| cmb | prob+size | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| cmb | prob+size | zero:modified | failure | regraded | The hull-face account omits flat small-design escapes that can tie an attained conditional optimum. |
| cmb | prob+size | zero:inflated | criterion | regraded | The Bernoulli envelope settles seam equality and zero-only cases, and the held-inflation objective requires its own critical score. |
| cmb | prob+size | zero:inflated | failure | regraded | At size one free dispersion can diverge while the parent Bernoulli law stays fixed, so the exposed-law description misses a co-winning escape. |
| cmb | size | zero:truncated | criterion | accepted | Each admissible design has the stated conditional moment or endpoint solution, and the finite design comparison includes equality. |
| cmb | size | zero:truncated | failure | regraded | The hull-face account omits flat small-design escapes that can tie an attained conditional optimum. |
| cmb | size | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| cmb | size | zero:modified | failure | regraded | The hull-face account omits flat small-design escapes that can tie an attained conditional optimum. |
| cmb | size | zero:inflated | criterion | regraded | The Bernoulli envelope settles seam equality and zero-only cases, and the held-inflation objective requires its own critical score. |
| cmb | size | zero:inflated | failure | regraded | At size one free dispersion can diverge while the parent Bernoulli law stays fixed, so the exposed-law description misses a co-winning escape. |
| cmb | all free | zero:truncated | uniqueness | regraded | At size two the rank-one chart attains the empirical law on a line, while larger finite sizes cannot match that law. |
| cmp | all free | zero:truncated | criterion | accepted | The applicable moment equations and geometric KKT condition resolve finite attainment after excluding the point and adjacent concentration cases. |
| cmp | all free | zero:truncated | failure | accepted | The fixed-cell ratios exhaust diverging dispersion and the remaining rate tails, with the geometric endpoint included when proper. |
| cmp | all free | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| cmp | all free | zero:modified | failure | accepted | The separated Bernoulli factor is fixed at its selected value, so the hold-specific conditional winning limits transfer exactly on positive samples. |
| cmp | all free | zero:inflated | criterion | regraded | The Bernoulli envelope settles seam equality and zero-only cases, and the held-inflation objective requires its own critical score. |
| cmp | all free | zero:inflated | failure | regraded | The parent closure transfers through the inflation objective on positive samples, including its zero-mass comparison. |
| cmp | lambda | zero:truncated | criterion | accepted | The applicable moment equations and geometric KKT condition resolve finite attainment after excluding the point and adjacent concentration cases. |
| cmp | lambda | zero:truncated | failure | accepted | The fixed-cell ratios exhaust diverging dispersion and the remaining rate tails, with the geometric endpoint included when proper. |
| cmp | lambda | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| cmp | lambda | zero:modified | failure | accepted | The separated Bernoulli factor is fixed at its selected value, so the hold-specific conditional winning limits transfer exactly on positive samples. |
| cmp | lambda | zero:inflated | criterion | regraded | The Bernoulli envelope settles seam equality and zero-only cases, and the held-inflation objective requires its own critical score. |
| cmp | lambda | zero:inflated | failure | regraded | The parent closure transfers through the inflation objective on positive samples, including its zero-mass comparison. |
| cmp | lambda+nu | zero:truncated | criterion | regraded | The fully held model requires evaluation and a positive conditioning event. |
| cmp | lambda+nu | zero:truncated | failure | regraded | A fully held parent domain consists of one feasible point. |
| cmp | lambda+nu | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| cmp | lambda+nu | zero:modified | failure | accepted | The separated Bernoulli factor is fixed at its selected value, so the hold-specific conditional winning limits transfer exactly on positive samples. |
| cmp | lambda+nu | zero:inflated | criterion | regraded | The held parent coefficients leave only the closed Bernoulli mixing optimization or evaluation. |
| cmp | lambda+nu | zero:inflated | failure | regraded | A fixed parent and closed inflation interval leave no escaping maximizing sequence. |
| cmp | nu | zero:truncated | criterion | accepted | The applicable moment equations and geometric KKT condition resolve finite attainment after excluding the point and adjacent concentration cases. |
| cmp | nu | zero:truncated | failure | accepted | The fixed-cell ratios exhaust diverging dispersion and the remaining rate tails, with the geometric endpoint included when proper. |
| cmp | nu | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| cmp | nu | zero:modified | failure | accepted | The separated Bernoulli factor is fixed at its selected value, so the hold-specific conditional winning limits transfer exactly on positive samples. |
| cmp | nu | zero:inflated | criterion | regraded | The Bernoulli envelope settles seam equality and zero-only cases, and the held-inflation objective requires its own critical score. |
| cmp | nu | zero:inflated | failure | regraded | The parent closure transfers through the inflation objective on positive samples, including its zero-mass comparison. |
| cmp | all free | zero:truncated | uniqueness | regraded | Positive covariance on the convex natural chart yields strict concavity over the entire attained regime. |
| dgamma | all free | zero:truncated | criterion | accepted | The conditional gamma faces and the hold-specific score or compactness test include equality at the excluded shape-zero boundary. |
| dgamma | all free | zero:truncated | failure | accepted | The conditional integral bounds exhaust the shape-zero, concentration and loss-of-mass paths after specialization to the holds. |
| dgamma | all free | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| dgamma | all free | zero:modified | failure | accepted | The separated Bernoulli factor is fixed at its selected value, so the hold-specific conditional winning limits transfer exactly on positive samples. |
| dgamma | all free | zero:inflated | criterion | regraded | The Bernoulli envelope settles seam equality and zero-only cases, and the held-inflation objective requires its own critical score. |
| dgamma | all free | zero:inflated | failure | regraded | The parent closure transfers through the inflation objective on positive samples, including its zero-mass comparison. |
| dgamma | rate | zero:truncated | criterion | accepted | The conditional gamma faces and the hold-specific score or compactness test include equality at the excluded shape-zero boundary. |
| dgamma | rate | zero:truncated | failure | accepted | The conditional integral bounds exhaust the shape-zero, concentration and loss-of-mass paths after specialization to the holds. |
| dgamma | rate | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| dgamma | rate | zero:modified | failure | accepted | The separated Bernoulli factor is fixed at its selected value, so the hold-specific conditional winning limits transfer exactly on positive samples. |
| dgamma | rate | zero:inflated | criterion | regraded | The Bernoulli envelope settles seam equality and zero-only cases, and the held-inflation objective requires its own critical score. |
| dgamma | rate | zero:inflated | failure | regraded | The parent closure transfers through the inflation objective on positive samples, including its zero-mass comparison. |
| dgamma | rate+shape | zero:truncated | criterion | regraded | The fully held model requires evaluation and a positive conditioning event. |
| dgamma | rate+shape | zero:truncated | failure | regraded | A fully held parent domain consists of one feasible point. |
| dgamma | rate+shape | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| dgamma | rate+shape | zero:modified | failure | accepted | The separated Bernoulli factor is fixed at its selected value, so the hold-specific conditional winning limits transfer exactly on positive samples. |
| dgamma | rate+shape | zero:inflated | criterion | regraded | The held parent coefficients leave only the closed Bernoulli mixing optimization or evaluation. |
| dgamma | rate+shape | zero:inflated | failure | regraded | A fixed parent and closed inflation interval leave no escaping maximizing sequence. |
| dgamma | shape | zero:truncated | criterion | accepted | The conditional gamma faces and the hold-specific score or compactness test include equality at the excluded shape-zero boundary. |
| dgamma | shape | zero:truncated | failure | accepted | The conditional integral bounds exhaust the shape-zero, concentration and loss-of-mass paths after specialization to the holds. |
| dgamma | shape | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| dgamma | shape | zero:modified | failure | accepted | The separated Bernoulli factor is fixed at its selected value, so the hold-specific conditional winning limits transfer exactly on positive samples. |
| dgamma | shape | zero:inflated | criterion | regraded | The Bernoulli envelope settles seam equality and zero-only cases, and the held-inflation objective requires its own critical score. |
| dgamma | shape | zero:inflated | failure | regraded | The parent closure transfers through the inflation objective on positive samples, including its zero-mass comparison. |
| dgamma | all free | zero:truncated | uniqueness | rejected | The cited uniqueness proofs hold a parent coefficient fixed, and the joint conditional shape-zero comparison does not determine the cardinality of the all-free fit. |
| dweibull | all free | zero:truncated | criterion | accepted | Strict scalar concavity handles a single hold, and the all-free Pareto comparison includes finite equality. |
| dweibull | all free | zero:truncated | failure | accepted | The limits of conditional survival give the complete hold-compatible Pareto, point and adjacent laws on positive-likelihood paths. |
| dweibull | all free | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| dweibull | all free | zero:modified | failure | accepted | The separated Bernoulli factor is fixed at its selected value, so the hold-specific conditional winning limits transfer exactly on positive samples. |
| dweibull | all free | zero:inflated | criterion | regraded | The Bernoulli envelope settles seam equality and zero-only cases, and the held-inflation objective requires its own critical score. |
| dweibull | all free | zero:inflated | failure | regraded | The parent closure transfers through the inflation objective on positive samples, including its zero-mass comparison. |
| dweibull | shape1 | zero:truncated | criterion | accepted | Strict scalar concavity handles a single hold, and the all-free Pareto comparison includes finite equality. |
| dweibull | shape1 | zero:truncated | failure | accepted | The limits of conditional survival give the complete hold-compatible Pareto, point and adjacent laws on positive-likelihood paths. |
| dweibull | shape1 | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| dweibull | shape1 | zero:modified | failure | accepted | The separated Bernoulli factor is fixed at its selected value, so the hold-specific conditional winning limits transfer exactly on positive samples. |
| dweibull | shape1 | zero:inflated | criterion | regraded | The Bernoulli envelope settles seam equality and zero-only cases, and the held-inflation objective requires its own critical score. |
| dweibull | shape1 | zero:inflated | failure | regraded | The parent closure transfers through the inflation objective on positive samples, including its zero-mass comparison. |
| dweibull | shape1+shape2 | zero:truncated | criterion | regraded | The fully held model requires evaluation and a positive conditioning event. |
| dweibull | shape1+shape2 | zero:truncated | failure | regraded | A fully held parent domain consists of one feasible point. |
| dweibull | shape1+shape2 | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| dweibull | shape1+shape2 | zero:modified | failure | accepted | The separated Bernoulli factor is fixed at its selected value, so the hold-specific conditional winning limits transfer exactly on positive samples. |
| dweibull | shape1+shape2 | zero:inflated | criterion | regraded | The held parent coefficients leave only the closed Bernoulli mixing optimization or evaluation. |
| dweibull | shape1+shape2 | zero:inflated | failure | regraded | A fixed parent and closed inflation interval leave no escaping maximizing sequence. |
| dweibull | shape2 | zero:truncated | criterion | accepted | Strict scalar concavity handles a single hold, and the all-free Pareto comparison includes finite equality. |
| dweibull | shape2 | zero:truncated | failure | accepted | The limits of conditional survival give the complete hold-compatible Pareto, point and adjacent laws on positive-likelihood paths. |
| dweibull | shape2 | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| dweibull | shape2 | zero:modified | failure | accepted | The separated Bernoulli factor is fixed at its selected value, so the hold-specific conditional winning limits transfer exactly on positive samples. |
| dweibull | shape2 | zero:inflated | criterion | regraded | The Bernoulli envelope settles seam equality and zero-only cases, and the held-inflation objective requires its own critical score. |
| dweibull | shape2 | zero:inflated | failure | regraded | The parent closure transfers through the inflation objective on positive samples, including its zero-mass comparison. |
| dweibull | all free | zero:truncated | uniqueness | rejected | The strict scalar tests keep a parent coefficient fixed, and there is no proof that the free-shape profile cannot have distinct global maxima. |
| pig | all free | zero:truncated | criterion | accepted | The Bessel profile covers the finite candidates and the held-compatible boundary cases, with a strict inward exclusion of Lévy. |
| pig | all free | zero:truncated | failure | accepted | The conditional pgf exhausts the held-compatible paths and the strictly improving inward derivative excludes the Lévy candidate. |
| pig | all free | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| pig | all free | zero:modified | failure | accepted | The separated Bernoulli factor is fixed at its selected value, so the hold-specific conditional winning limits transfer exactly on positive samples. |
| pig | all free | zero:inflated | criterion | regraded | The Bernoulli envelope settles seam equality and zero-only cases, and the held-inflation objective requires its own critical score. |
| pig | all free | zero:inflated | failure | regraded | The parent closure transfers through the inflation objective on positive samples, including its zero-mass comparison. |
| pig | mean | zero:truncated | criterion | accepted | The Bessel profile covers the finite candidates and the held-compatible boundary cases, with a strict inward exclusion of Lévy. |
| pig | mean | zero:truncated | failure | accepted | The conditional pgf exhausts the held-compatible paths and the strictly improving inward derivative excludes the Lévy candidate. |
| pig | mean | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| pig | mean | zero:modified | failure | accepted | The separated Bernoulli factor is fixed at its selected value, so the hold-specific conditional winning limits transfer exactly on positive samples. |
| pig | mean | zero:inflated | criterion | regraded | The Bernoulli envelope settles seam equality and zero-only cases, and the held-inflation objective requires its own critical score. |
| pig | mean | zero:inflated | failure | regraded | The parent closure transfers through the inflation objective on positive samples, including its zero-mass comparison. |
| pig | mean+shape | zero:truncated | criterion | regraded | The fully held model requires evaluation and a positive conditioning event. |
| pig | mean+shape | zero:truncated | failure | regraded | A fully held parent domain consists of one feasible point. |
| pig | mean+shape | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| pig | mean+shape | zero:modified | failure | accepted | The separated Bernoulli factor is fixed at its selected value, so the hold-specific conditional winning limits transfer exactly on positive samples. |
| pig | mean+shape | zero:inflated | criterion | regraded | The held parent coefficients leave only the closed Bernoulli mixing optimization or evaluation. |
| pig | mean+shape | zero:inflated | failure | regraded | A fixed parent and closed inflation interval leave no escaping maximizing sequence. |
| pig | shape | zero:truncated | criterion | accepted | The Bessel profile covers the finite candidates and the held-compatible boundary cases, with a strict inward exclusion of Lévy. |
| pig | shape | zero:truncated | failure | accepted | The conditional pgf exhausts the held-compatible paths and the strictly improving inward derivative excludes the Lévy candidate. |
| pig | shape | zero:modified | criterion | accepted | Bernoulli factorization gives the positive-sample equivalence and leaves the parent flat on compatible all-zero data. |
| pig | shape | zero:modified | failure | accepted | The separated Bernoulli factor is fixed at its selected value, so the hold-specific conditional winning limits transfer exactly on positive samples. |
| pig | shape | zero:inflated | criterion | regraded | The Bernoulli envelope settles seam equality and zero-only cases, and the held-inflation objective requires its own critical score. |
| pig | shape | zero:inflated | failure | regraded | The parent closure transfers through the inflation objective on positive samples, including its zero-mass comparison. |
| pig | all free | zero:truncated | uniqueness | rejected | A unique inner root at fixed Bessel argument does not establish all-free cardinality, and the held-mean example ties two excluded ends rather than two fits. |

## Proof errors and scope corrections

**CMB's exposed-face account misses escapes along flat directions.** At size two, with both canonical coefficients free and sample `(1,2)`, take `nu -> +Inf` and `eta = nu log(2)`, where `eta` is log odds. The two positive weights are equal along the entire sequence. The conditional likelihood is constantly (1/4), the multinomial maximum, and the limit occupies the entire rank-one positive hull. Applying the hurdle construction with a compatible zero mass preserves the tie. At size one the conditional law is always the point mass at one when (p>0), even along a path toward excluded (p=0) or with free dispersion diverging. For inflation, hold size one and (p=1/2) and take `nu -> +Inf` on `(0,1)). At (pi=0) the law stays Bernoulli with probability (1/2) and attains the saturated likelihood throughout the path. The parent hull reduction must account for escapes with the full law unchanged in this nonminimal chart. The 16 affected failure rows are exported as one-sided claims about the proved exposed or scalar laws. The attainment criteria survive because an escaping flat direction at these designs ties a feasible point. [Conditional hull](2026-11-27-zero-mod-math-report.md#cmb-all-free-truncated) · [Inflated hull](2026-11-27-zero-mod-math-report.md#cmb-all-free-inflated)

**An infeasible modified winner does not force a zero-inflation boundary fit.** The [historical modifier roadmap](../../roadmap/2026-11-11-zero-and-truncation-modifiers.md) states that shortcut without the necessary restriction. The CMB sample with one zero and seven twos at held (
u=1, p=317/500) refutes it. Size two uniquely maximizes the positive likelihood but has parent zero mass above (1/8). Size three can achieve (1/8) at positive inflation and beats the size-two boundary value by an exact rational comparison. Every larger size has strictly smaller conditional mass at two than size three, and the saturated Bernoulli factor bounds its likelihood. Thus size three and its positive inflation weight give the unique global fit. Neither the TASK-440 proposals nor the companion TASK-439 inflation argument uses the shortcut across competing designs. TASK-439 uses it within the specified fixed-carrier charts and compares the free-size candidates separately. [TASK-440 counterexample](2026-11-27-zero-mod-math-report.md#cmb-all-free-inflated) · [TASK-439 inflation argument](2026-11-27-trunc-expfam-math-report.md#exact-factorization-and-inflation-boundary)

**Three all-free uniqueness rows use narrower results.** The discretized gamma and discrete Weibull proofs establish uniqueness on particular held slices. The PIG proof establishes uniqueness of an inner root at fixed Bessel argument, and its held-mean example is a tie between two excluded limits. None of those arguments establishes an all-free maximizing-set result at the submitted key. CMP's positive covariance proves exact all-free uniqueness on the attained set. For beta-binomial, beta-negative-binomial and CMB the exported one-sided claims specify actual finite nonunique samples. The held-(p,
u) CMB two-design tie is also exact, but the all-free refutation in the join file uses the size-two ridge. [Source uniqueness proofs](2026-11-27-zero-mod-math-report.md)

## Rows not joined

| Family | Held | Modifier | Question | Evidence needed |
|---|---|---|---|---|
| dgamma | all free | zero:truncated | uniqueness | An all-free cardinality theorem or an exact attained all-free counterexample. |
| dweibull | all free | zero:truncated | uniqueness | A global result on the shape profile or an exact sample with multiple attained global maxima. |
| pig | all free | zero:truncated | uniqueness | A result on the outer Bessel profile or an exact sample with multiple attained global maxima. |

All three exclusions are for mathematical scope. There are no missing parent regimes, ineligible modifiers or unresolved evidence anchors. The narrower held-slice results can be proposed under their own keys.

## Open questions by family

| Family | What remains open |
|---|---|
| bbinom | An effective bound on free-size conditional and inflated comparisons, including finite/limit equality and cross-size cardinality. The rational fixed-size decomposition gives a separate procedure at algebraic holds. |
| bnbinom | Root coverage in the conditional canonical chart and with held nonzero inflation, and a terminating comparison at equality with the conditional NB, Poisson or applicable rare-shape face. Labeled nonuniqueness is proved, while uniqueness up to the swap remains open. |
| cmb | A complete account of co-winning escapes in the original small-size parametrizations, efficient comparison over the supported size range, and a terminating held-inflation score and tie procedure. |
| cmp | A global root or value enclosure and an equality decision for the held nonzero inflation objective on mixed samples. |
| dgamma | All-free conditional cardinality and a terminating comparison against shape zero, rate-root counting below held shape one, and all-free inflation comparison for short positive ranges with held inflation below the empirical zero fraction. |
| dweibull | A global shape-profile analysis and effective Pareto-face comparison, and the corresponding held-inflation comparison for short positive ranges with inflation below the empirical zero fraction. |
| pig | Root coverage for the outer Bessel profile and exact comparison with the Sibuya-type or Poisson height, and conditional mean uniqueness at held positive shape. The held-mean all-ones tie settles only that boundary case. |

## Checks

The [TASK-440 replay](2026-11-27-zero-mod-math-data/check.py) and [inventory check](2026-11-27-zero-mod-math-data/inventory.py) passed on the workstation. The CMB counterexample and irrational two-design equality use exact arithmetic. The rational beta-negative-binomial recurrence enclosures, together with the entropy bound, place the full competing level in a compact interior box off the swap seam. The PIG tie follows analytically from (e^mu=1+2mu) and the strict endpoint comparison. The [review validator](2026-11-27-mod-rev2-data/validate.py) encloses its positive root in ((1.2564312,1.2564313)) with rational Taylor bounds. The high-precision mass grids in TASK-440 are corroborative.

The [TASK-439 replay](2026-11-27-trunc-expfam-math-data/check.py) also passed on the workstation. For its normal example on ([-1,1]), the validator checks the exact positive inward score (373/1200000000), a rational likelihood improvement bound for a feasible negative quadratic tilt, and a strict upper bound of (10^{-8}) on the optimal improvement per observation. The estimated-bound reduction in that companion report keeps observations tied at a cut by using closed windows. Estimated bounds and normal truncation do not occur in the 249 TASK-440 rows.

All Python computations used one workstation slot through `/tmp/umvl-cpu/run`. No builds were required.

The repository link checker scanned 10,826 destinations. It reported 47 inherited entries and exited 1 on the known `P+Q` false positive at line 461 of the beta-prime report. It reported no broken destination in this review.
