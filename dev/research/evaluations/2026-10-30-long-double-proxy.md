# long double proxy run (TASK-125), 2026-10-30

Question: which C++ checks fail when Boost.Math has no `long double` math, as
under Emscripten, and approximately as on macOS arm64 or MSVC, where
`long double` is `double`?

Source: master `307645e` (after the WebAssembly pilot, whose `special.hpp`
seam makes `ExtendedReal` `double` under this macro). Local x86-64, GCC 15.2,
BH 1.90. The macro also turns off Boost's internal promotion of `double` to
`long double` (`BOOST_MATH_PROMOTE_DOUBLE_POLICY`). Raw `long double`
arithmetic in the other headers stays 80-bit here, so this is a partial
proxy: it covers Boost and the seam, not those headers.

```sh
cmake -S univariateML-cpp -B build/ld-proxy -DCMAKE_CXX_FLAGS=-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS \
  -DBoost_INCLUDE_DIR=<BH include>
cmake --build build/ld-proxy -j4 && ctest --test-dir build/ld-proxy --output-on-failure
```

Result: 5/6 targets pass; `distribution_tests` 915 of 922 cases pass,
35 of 1,281,403 assertions fail, all at extreme inputs:

| Family | Failing entries |
|---|---|
| beta, betapr, nsbeta | lower CDF at the smallest subnormal input 4.94e-324: 3.040e-65 against truth 2.880e-65 (6% relative); boundary rows near 1e-300 (CDF and quantile) |
| dgamma | log-density and log upper tail where the density underflows: -743.342 against -743.379 |
| dnorm | log-density and log lower tail at -40: -739.172 against -739.174 |
| logitnorm | lower quantile at log p = -800: off by a factor of 50 to 70 at six points (5.48e-50 against 8.01e-52); `plogitnorm` extreme log probabilities (`test.cpp:46`) |

Every other truth table, golden, hold and limit check passes. The full log
was `build/scratch/ld-proxy-ctest.log` (ignored); rerun the commands above.
