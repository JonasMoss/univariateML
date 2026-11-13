modifier_law_metadata <- 
list(.modifier_tbinom = list(model = "Internal modifier law tbinom", 
    density = "dtbinom", support = list(bounds = c(-Inf, Inf), 
        closed = c(FALSE, FALSE), type = "Z", expressions = list(
            "max(0, floor(a)+1)", "min(size, floor(b))"), endpoint_closed = c(TRUE, 
        TRUE), endpoint_log_density = list()), names = c("prob", 
    "a", "b", "size"), simulate = c(0.5, 0, 10, 10), params = list(
        list(name = "prob", symbol = "p", role = "probability", 
            domain = "[0, 1]", simulate = 0.5, kind = "regular", 
            required = FALSE), list(name = "a", symbol = "a", 
            role = "known design quantity", domain = "(-inf, inf)", 
            simulate = 0L, kind = "design", required = TRUE), 
        list(name = "b", symbol = "b", role = "known design quantity", 
            domain = "(-inf, inf)", simulate = 10L, kind = "design", 
            required = TRUE), list(name = "size", symbol = "n", 
            role = "known design quantity", domain = "[0, inf)", 
            integer = TRUE, simulate = 10L, kind = "design", 
            required = TRUE)), required = c("a", "b", "size")), 
    .modifier_tnorm = list(model = "Internal modifier law tnorm", 
        density = "dtnorm", support = list(bounds = c(-Inf, Inf
        ), closed = c(FALSE, FALSE), type = "R", expressions = list(
            "a", "b"), endpoint_closed = c(TRUE, TRUE), endpoint_log_density = list()), 
        names = c("mean", "sd", "a", "b"), simulate = c(0, 1, 
        -Inf, Inf), params = list(list(name = "mean", symbol = "\\mu", 
            role = "location", domain = "(-inf, inf)", simulate = 0L, 
            kind = "regular", required = FALSE), list(name = "sd", 
            symbol = "\\sigma", role = "scale", domain = "(0, inf)", 
            simulate = 1L, kind = "regular", required = FALSE), 
            list(name = "a", symbol = "a", role = "lower", domain = "[-inf, inf)", 
                simulate = -Inf, kind = "design", advice = "Supply NULL to estimate this bound at the sample extreme, or an infinite value for an unbounded side.", 
                required = TRUE), list(name = "b", symbol = "b", 
                role = "upper", domain = "(-inf, inf]", simulate = Inf, 
                kind = "design", advice = "Supply NULL to estimate this bound at the sample extreme, or an infinite value for an unbounded side.", 
                required = TRUE)), required = c("a", "b")), .modifier_tpois = list(
        model = "Internal modifier law tpois", density = "dtpois", 
        support = list(bounds = c(-Inf, Inf), closed = c(FALSE, 
        FALSE), type = "Z", expressions = list("max(0, floor(a)+1)", 
            "floor(b)"), endpoint_closed = c(TRUE, TRUE), endpoint_log_density = list()), 
        names = c("lambda", "a", "b"), simulate = c(1, 0, 10), 
        params = list(list(name = "lambda", symbol = "\\lambda", 
            role = "rate", domain = "[0, inf)", simulate = 1L, 
            kind = "regular", required = FALSE), list(name = "a", 
            symbol = "a", role = "known design quantity", domain = "(-inf, inf)", 
            simulate = 0L, kind = "design", required = TRUE), 
            list(name = "b", symbol = "b", role = "known design quantity", 
                domain = "(-inf, inf)", simulate = 10L, kind = "design", 
                required = TRUE)), required = c("a", "b")), .modifier_zib = list(
        model = "Internal modifier law zib", density = "dzib", 
        support = list(bounds = c(0, Inf), closed = c(TRUE, FALSE
        ), type = "Z", expressions = list(0, "size"), endpoint_closed = c(TRUE, 
        TRUE), endpoint_log_density = list()), names = c("size", 
        "prob", "pi"), simulate = c(10, 0.5, 0.5), params = list(
            list(name = "size", symbol = "n", role = "trials", 
                domain = "[0, inf)", integer = TRUE, simulate = 10L, 
                kind = "design", required = TRUE), list(name = "prob", 
                symbol = "p", role = "probability", domain = "[0, 1]", 
                simulate = 0.5, kind = "regular", required = FALSE), 
            list(name = "pi", symbol = "\\pi", role = "probability", 
                domain = "[0, 1]", simulate = 0.5, kind = "regular", 
                required = FALSE)), required = "size"), .modifier_zinb = list(
        model = "Internal modifier law zinb", density = "dzinb", 
        support = list(bounds = c(0, Inf), closed = c(TRUE, FALSE
        ), type = "Z", expressions = list(0L, Inf), endpoint_closed = c(TRUE, 
        FALSE), endpoint_log_density = list()), names = c("size", 
        "prob", "pi"), simulate = c(1, 0.5, 0.5), params = list(
            list(name = "size", symbol = "r", role = "shape", 
                domain = "[0, inf)", simulate = 1L, kind = "regular", 
                required = FALSE), list(name = "prob", symbol = "p", 
                role = "probability", domain = "(0, 1]", simulate = 0.5, 
                kind = "regular", required = FALSE), list(name = "pi", 
                symbol = "\\pi", role = "probability", domain = "[0, 1]", 
                simulate = 0.5, kind = "regular", required = FALSE)), 
        required = character(0)), .modifier_zip = list(model = "Internal modifier law zip", 
        density = "dzip", support = list(bounds = c(0, Inf), 
            closed = c(TRUE, FALSE), type = "Z", expressions = list(
                0L, Inf), endpoint_closed = c(TRUE, FALSE), endpoint_log_density = list()), 
        names = c("lambda", "pi"), simulate = c(1, 0.5), params = list(
            list(name = "lambda", symbol = "\\lambda", role = "rate", 
                domain = "(0, inf)", simulate = 1, kind = "regular", 
                required = FALSE), list(name = "pi", symbol = "\\pi", 
                role = "probability", domain = "[0, 1]", simulate = 0.5, 
                kind = "regular", required = FALSE)), required = character(0)), 
    .modifier_zmbinom = list(model = "Internal modifier law zmbinom", 
        density = "dzmbinom", support = list(bounds = c(0, Inf
        ), closed = c(TRUE, FALSE), type = "Z", expressions = list(
            0, "size"), endpoint_closed = c(TRUE, TRUE), endpoint_log_density = list()), 
        names = c("size", "prob", "p0"), simulate = c(10, 0.5, 
        0.5), params = list(list(name = "size", symbol = "n", 
            role = "trials", domain = "[1, inf)", integer = TRUE, 
            simulate = 10L, kind = "design", required = TRUE), 
            list(name = "prob", symbol = "p", role = "probability", 
                domain = "[0, 1]", simulate = 0.5, kind = "regular", 
                required = FALSE), list(name = "p0", symbol = "p_0", 
                role = "probability", domain = "[0, 1]", simulate = 0.5, 
                kind = "regular", required = FALSE)), required = "size"), 
    .modifier_zmgeom = list(model = "Internal modifier law zmgeom", 
        density = "dzmgeom", support = list(bounds = c(0, Inf
        ), closed = c(TRUE, FALSE), type = "Z", expressions = list(
            0L, Inf), endpoint_closed = c(TRUE, FALSE), endpoint_log_density = list()), 
        names = c("prob", "p0"), simulate = c(0.5, 0.5), params = list(
            list(name = "prob", symbol = "p", role = "probability", 
                domain = "(0, 1]", simulate = 0.5, kind = "regular", 
                required = FALSE), list(name = "p0", symbol = "p_0", 
                role = "probability", domain = "[0, 1]", simulate = 0.5, 
                kind = "regular", required = FALSE)), required = character(0)), 
    .modifier_zmlogarithmic = list(model = "Internal modifier law zmlogarithmic", 
        density = "dzmlogarithmic", support = list(bounds = c(0, 
        Inf), closed = c(TRUE, FALSE), type = "Z", expressions = list(
            0L, Inf), endpoint_closed = c(TRUE, FALSE), endpoint_log_density = list()), 
        names = c("prob", "p0"), simulate = c(0.5, 0.5), params = list(
            list(name = "prob", symbol = "p", role = "probability", 
                domain = "[0, 1)", simulate = 0.5, kind = "regular", 
                required = FALSE), list(name = "p0", symbol = "p_0", 
                role = "probability", domain = "[0, 1]", simulate = 0.5, 
                kind = "regular", required = FALSE)), required = character(0)), 
    .modifier_zmnbinom = list(model = "Internal modifier law zmnbinom", 
        density = "dzmnbinom", support = list(bounds = c(0, Inf
        ), closed = c(TRUE, FALSE), type = "Z", expressions = list(
            0L, Inf), endpoint_closed = c(TRUE, FALSE), endpoint_log_density = list()), 
        names = c("size", "prob", "p0"), simulate = c(1, 0.5, 
        0.5), params = list(list(name = "size", symbol = "r", 
            role = "shape", domain = "[0, inf)", simulate = 1L, 
            kind = "regular", required = FALSE), list(name = "prob", 
            symbol = "p", role = "probability", domain = "(0, 1]", 
            simulate = 0.5, kind = "regular", required = FALSE), 
            list(name = "p0", symbol = "p_0", role = "probability", 
                domain = "[0, 1]", simulate = 0.5, kind = "regular", 
                required = FALSE)), required = character(0)), 
    .modifier_zmpois = list(model = "Internal modifier law zmpois", 
        density = "dzmpois", support = list(bounds = c(0, Inf
        ), closed = c(TRUE, FALSE), type = "Z", expressions = list(
            0L, Inf), endpoint_closed = c(TRUE, FALSE), endpoint_log_density = list()), 
        names = c("lambda", "p0"), simulate = c(1, 0.5), params = list(
            list(name = "lambda", symbol = "\\lambda", role = "rate", 
                domain = "[0, inf)", simulate = 1L, kind = "regular", 
                required = FALSE), list(name = "p0", symbol = "p_0", 
                role = "probability", domain = "[0, 1]", simulate = 0.5, 
                kind = "regular", required = FALSE)), required = character(0)), 
    .modifier_ztbinom = list(model = "Internal modifier law ztbinom", 
        density = "dztbinom", support = list(bounds = c(1, Inf
        ), closed = c(TRUE, FALSE), type = "Z", expressions = list(
            1, "size"), endpoint_closed = c(TRUE, TRUE), endpoint_log_density = list()), 
        names = c("size", "prob"), simulate = c(10, 0.5), params = list(
            list(name = "size", symbol = "n", role = "trials", 
                domain = "[1, inf)", integer = TRUE, simulate = 10L, 
                kind = "design", required = TRUE), list(name = "prob", 
                symbol = "p", role = "probability", domain = "[0, 1]", 
                simulate = 0.5, kind = "regular", required = FALSE)), 
        required = "size"), .modifier_ztgeom = list(model = "Internal modifier law ztgeom", 
        density = "dztgeom", support = list(bounds = c(1, Inf
        ), closed = c(TRUE, FALSE), type = "Z", expressions = list(
            1L, Inf), endpoint_closed = c(TRUE, FALSE), endpoint_log_density = list()), 
        names = "prob", simulate = 0.5, params = list(list(name = "prob", 
            symbol = "p", role = "probability", domain = "(0, 1]", 
            simulate = 0.5, kind = "regular", required = FALSE)), 
        required = character(0)), .modifier_ztnbinom = list(model = "Internal modifier law ztnbinom", 
        density = "dztnbinom", support = list(bounds = c(1, Inf
        ), closed = c(TRUE, FALSE), type = "Z", expressions = list(
            1L, Inf), endpoint_closed = c(TRUE, FALSE), endpoint_log_density = list()), 
        names = c("size", "prob"), simulate = c(1, 0.5), params = list(
            list(name = "size", symbol = "r", role = "shape", 
                domain = "[0, inf)", simulate = 1L, kind = "regular", 
                required = FALSE), list(name = "prob", symbol = "p", 
                role = "probability", domain = "(0, 1]", simulate = 0.5, 
                kind = "regular", required = FALSE)), required = character(0)), 
    .modifier_ztpois = list(model = "Internal modifier law ztpois", 
        density = "dztpois", support = list(bounds = c(1, Inf
        ), closed = c(TRUE, FALSE), type = "Z", expressions = list(
            1L, Inf), endpoint_closed = c(TRUE, FALSE), endpoint_log_density = list()), 
        names = "lambda", simulate = 1, params = list(list(name = "lambda", 
            symbol = "\\lambda", role = "rate", domain = "[0, inf)", 
            simulate = 1L, kind = "regular", required = FALSE)), 
        required = character(0)))

ml_law_metadata <- function(name) {
  if(startsWith(name,".modifier_")) modifier_law_metadata[[name]] else univariateML_metadata[[name]]
}
