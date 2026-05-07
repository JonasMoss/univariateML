// DPQ macros for log-prob / lower-tail handling, ported verbatim from
// instructions/nmath/dpq.h. Used by the vendored nmath dpqr primitives in
// src/nmath_*.cpp. Keeps the original macro names so the porting diff
// from upstream .c → .cpp is mechanical.
//
// Macros expect `lower_tail` and `log_p` to be in scope (typically as
// function parameters). The naming `give_log` is a synonym used by `d*`
// functions; aliased to `log_p` per upstream.

#pragma once

#include "rmath_macros.hpp"

/* give_log in "d";  log_p in "p" & "q" : */
#define give_log log_p

/* "DEFAULT" */
#define R_D__0  (log_p ? ML_NEGINF : 0.)        /* 0 */
#define R_D__1  (log_p ? 0. : 1.)               /* 1 */
#define R_DT_0  (lower_tail ? R_D__0 : R_D__1)  /* 0 */
#define R_DT_1  (lower_tail ? R_D__1 : R_D__0)  /* 1 */
#define R_D_half (log_p ? -M_LN2 : 0.5)         /* 1/2 (lower- or upper tail) */

/* Use 0.5 - p + 0.5 to perhaps gain 1 bit of accuracy */
#define R_D_Lval(p) (lower_tail ? (p) : (0.5 - (p) + 0.5))      /*  p  */
#define R_D_Cval(p) (lower_tail ? (0.5 - (p) + 0.5) : (p))      /*  1 - p */

#define R_D_val(x)  (log_p ? std::log(x) : (x))                 /*  x  in pF(x,..) */
#define R_D_qIv(p)  (log_p ? std::exp(p) : (p))                 /*  p  in qF(p,..) */
#define R_D_exp(x)  (log_p ? (x) : std::exp(x))                 /* exp(x) */
#define R_D_log(p)  (log_p ? (p) : std::log(p))                 /* log(p) */
#define R_D_Clog(p) (log_p ? std::log1p(-(p)) : (0.5 - (p) + 0.5)) /* [log](1-p) */

/* log(1 - exp(x)) in more stable form than log1p(- R_D_qIv(x)) : */
#define R_Log1_Exp(x) ((x) > -M_LN2 ? std::log(-std::expm1(x)) : std::log1p(-std::exp(x)))

/* log(1-exp(x)):  R_D_LExp(x) */
#define R_D_LExp(x) (log_p ? R_Log1_Exp(x) : std::log1p(-(x)))

#define R_DT_val(x)  (lower_tail ? R_D_val(x)  : R_D_Clog(x))
#define R_DT_Cval(x) (lower_tail ? R_D_Clog(x) : R_D_val(x))

#define R_DT_qIv(p)  (log_p ? (lower_tail ? std::exp(p) : -std::expm1(p)) : R_D_Lval(p))
#define R_DT_CIv(p)  (log_p ? (lower_tail ? -std::expm1(p) : std::exp(p)) : R_D_Cval(p))

#define R_DT_exp(x)  R_D_exp(R_D_Lval(x))                /* exp(x) */
#define R_DT_Cexp(x) R_D_exp(R_D_Cval(x))                /* exp(1 - x) */

#define R_DT_log(p)  (lower_tail ? R_D_log(p) : R_D_LExp(p))   /* log(p) in qF */
#define R_DT_Clog(p) (lower_tail ? R_D_LExp(p) : R_D_log(p))   /* log(1-p) in qF */
#define R_DT_Log(p)  (lower_tail ? (p) : R_Log1_Exp(p))

#define R_Q_P01_check(p)                        \
    if ((log_p  && p > 0) ||                    \
        (!log_p && (p < 0 || p > 1)) )          \
        ML_WARN_return_NAN

#define R_Q_P01_boundaries(p, _LEFT_, _RIGHT_)          \
    if (log_p) {                                        \
        if (p > 0)                                      \
            ML_WARN_return_NAN;                         \
        if (p == 0)                                     \
            return lower_tail ? _RIGHT_ : _LEFT_;       \
        if (p == ML_NEGINF)                             \
            return lower_tail ? _LEFT_ : _RIGHT_;       \
    } else {                                            \
        if (p < 0 || p > 1)                             \
            ML_WARN_return_NAN;                         \
        if (p == 0)                                     \
            return lower_tail ? _LEFT_ : _RIGHT_;       \
        if (p == 1)                                     \
            return lower_tail ? _RIGHT_ : _LEFT_;       \
    }

#define R_P_bounds_01(x, x_min, x_max)          \
    if (x <= x_min) return R_DT_0;              \
    if (x >= x_max) return R_DT_1

#define R_P_bounds_Inf_01(x)                    \
    if (!R_FINITE(x)) {                         \
        if (x > 0) return R_DT_1;               \
        return R_DT_0;                          \
    }
