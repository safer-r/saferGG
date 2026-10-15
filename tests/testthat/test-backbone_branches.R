# tests for the backbone branches shared by all saferGG functions:
# - management of NULL arguments
# - management of empty non NULL arguments
# - management of NA arguments
# - safer_check argument checking (including the function branch)
# - check of lib_path (non-character, non-existing dir, existing dir with trailing /)
# - check of the required functions (with safer_check = TRUE)
# - management of "" in arguments of mode character (where present)
# - other checkings (n == 0 for gg_palette)
# Each test also verifies the error text embedding (error_text argument)

# NULL-forbidden, EMPTY-forbidden, EMPTYSTRING-forbidden argument names extracted from R/*.R

## initialization of tests
# minimal valid calls for each function, used as the base for the invalid-argument variants
base_calls <- base::list(
    gg_palette = base::quote(gg_palette(n = 3, kind = "std", safer_check = FALSE, error_text = "")),
    gg_just = base::quote(gg_just(angle = 45, pos = "bottom", kind = "axis", safer_check = FALSE, error_text = "")),
    gg_empty = base::quote(gg_empty(text = "TEST", text_size = 12, title = "T", title_size = 8, safer_check = FALSE, error_text = "")),
    gg_get_legend = base::quote(gg_get_legend(ggplot_built = ggplot2::ggplot_build(ggplot2::ggplot(data = base::data.frame(x = 1:3, y = 1:3, stringsAsFactors = TRUE), ggplot2::aes(x = x, y = y))), safer_check = FALSE, error_text = "")),
    gg_point_rast = base::quote(gg_point_rast(data = base::data.frame(x = 1:3, y = 1:3, stringsAsFactors = TRUE), mapping = ggplot2::aes(x = x, y = y), safer_check = FALSE, error_text = "")),
    gg_heatmap = base::quote(gg_heatmap(data1 = base::data.frame(a = 1:3, b = base::c("x", "y", "x"), c = 1:3, stringsAsFactors = TRUE), legend.name1 = "TEST", low.color1 = "blue", mid.color1 = "white", high.color1 = "red", limit1 = base::c(0, 3), safer_check = FALSE, error_text = "")),
    gg_donut = base::quote(gg_donut(data1 = base::data.frame(a = base::c(1, 2), stringsAsFactors = TRUE), freq = "a", categ = "a", safer_check = FALSE, error_text = "")),
    gg_miami = base::quote(gg_miami(file = "BAD_FILE_PATH_DOES_NOT_EXIST", chr = "BAD_FILE_PATH_DOES_NOT_EXIST", top.y.column = "AF", x_lim = "whole", vgrid = FALSE, reverse1 = FALSE, reverse2 = FALSE, y.log1 = FALSE, y.log2 = FALSE, warn.print = FALSE, safer_check = FALSE, error_text = "")),
    gg_box = base::quote(gg_box(data1 = base::data.frame(a = base::c(1, 2, 3), b = base::c("x", "y", "x"), stringsAsFactors = TRUE), y = "a", categ = "b", safer_check = FALSE, error_text = "")),
    gg_scatter = base::quote(gg_scatter(data1 = base::data.frame(x = 1:3, y = 1:3, stringsAsFactors = TRUE), x = "x", y = "y", safer_check = FALSE, error_text = ""))
)
## end initialization of tests

# generic runner: evaluates a call with a modified argument, returns the error message or "NO_ERROR"
# BEWARE: cl[[nm]] <- NULL would REMOVE the argument from the call, and base::quote(expr = NULL) returns NULL: use cl[nm] <- base::list(NULL) which sets the argument with a NULL value
run_mod <- function(fn, mod_args){
    cl <- base_calls[[fn]]
    for(nm in base::names(x = mod_args)){
        cl[nm] <- base::list(mod_args[[nm]])
    }
    res <- base::tryCatch(
        expr = base::eval(expr = cl, envir = base::asNamespace(ns = "saferGG")),
        error = function(e){base::conditionMessage(e)}
    )
    base::ifelse(test = base::is.character(x = res), yes = res, no = "NO_ERROR")
}

# generic checks for a given function, argument and kind of forbidden value
test_arg <- function(fn, arg, value, pattern){
    msg <- run_mod(fn = fn, mod_args = stats::setNames(base::list(value), arg))
    testthat::expect_true(
        object = base::grepl(x = msg, pattern = pattern, fixed = TRUE),
        label = base::paste0(fn, ": ", arg, " -> ", pattern)
    )
    testthat::expect_true(
        object = base::grepl(x = msg, pattern = "================", fixed = TRUE),
        label = base::paste0(fn, ": ", arg, " error framing")
    )
}

testthat::test_that("management of NULL arguments (backbone)", {
    # per function: the argument to set NULL (first NULL-forbidden arg that accepts any value)
    null_specs <- base::list(
        gg_palette = "safer_check",
        gg_just = "angle",
        gg_empty = "text_size",
        gg_get_legend = "ggplot_built",
        gg_point_rast = "stat",
        gg_heatmap = "legend.name1",
        gg_donut = "freq",
        gg_miami = "top.y.column",
        gg_box = "y",
        gg_scatter = "geom"
    )
    for(fn in base::names(x = null_specs)){
        msg <- run_mod(fn = fn, mod_args = stats::setNames(base::list(NULL), null_specs[[fn]]))
        testthat::expect_true(object = base::grepl(x = msg, pattern = "CANNOT BE NULL", fixed = TRUE), label = base::paste0(fn, ": NULL ", null_specs[[fn]]))
    }
})

testthat::test_that("management of empty non NULL arguments (backbone)", {
    empty_specs <- base::list(
        gg_palette = "kind",
        gg_just = "pos",
        gg_empty = "title",
        gg_get_legend = "safer_check", # ggplot_built is not in EMPTY-forbidden for gg_get_legend (checked: arg1, seed... legacy); use safer_check
        gg_point_rast = "stat",
        gg_heatmap = "alpha2",
        gg_donut = "freq",
        gg_miami = "top.y.column",
        gg_box = "y",
        gg_scatter = "y"
    )
    for(fn in base::names(x = empty_specs)){
        # gg_get_legend: safer_check empty triggers the empty-non-null branch before the safer_check type check
        msg <- run_mod(fn = fn, mod_args = stats::setNames(base::list(base::character(0)), empty_specs[[fn]]))
        testthat::expect_true(object = base::grepl(x = msg, pattern = "CANNOT BE AN EMPTY NON NULL OBJECT", fixed = TRUE), label = base::paste0(fn, ": empty ", empty_specs[[fn]]))
    }
})

testthat::test_that("management of NA arguments (backbone)", {
    na_specs <- base::list(
        gg_palette = "kind",
        gg_just = "angle",
        gg_empty = "text_size",
        gg_get_legend = "safer_check",
        gg_point_rast = "raster.dpi",
        gg_heatmap = "alpha2",
        gg_donut = "freq",
        gg_miami = "vgrid",
        gg_box = "y",
        gg_scatter = "alpha"
    )
    for(fn in base::names(x = na_specs)){
        msg <- run_mod(fn = fn, mod_args = stats::setNames(base::list(base::as.character(x = NA)), na_specs[[fn]]))
        testthat::expect_true(object = base::grepl(x = msg, pattern = "CANNOT BE MADE OF NA ONLY", fixed = TRUE), label = base::paste0(fn, ": NA ", na_specs[[fn]]))
    }
})

testthat::test_that("safer_check argument checking (backbone)", {
    for(fn in base::names(x = base_calls)){
        msg <- run_mod(fn = fn, mod_args = list(safer_check = "BAD"))
        testthat::expect_true(object = base::grepl(x = msg, pattern = "THE safer_check ARGUMENT VALUE MUST BE A SINGLE LOGICAL VALUE", fixed = TRUE), label = base::paste0(fn, ": safer_check string"))
        # function branch: a function value is deparsed then reported
        msg2 <- run_mod(fn = fn, mod_args = list(safer_check = base::mean))
        testthat::expect_true(object = base::grepl(x = msg2, pattern = "THE safer_check ARGUMENT VALUE MUST BE A SINGLE LOGICAL VALUE", fixed = TRUE), label = base::paste0(fn, ": safer_check function"))
        testthat::expect_true(object = base::grepl(x = msg2, pattern = "mean", fixed = TRUE), label = base::paste0(fn, ": safer_check function deparsed"))
    }
})

testthat::test_that("check of lib_path (backbone)", {
    for(fn in base::names(x = base_calls)){
        # safer_check must be TRUE for the lib_path block to run
        # non-character lib_path with safer_check = TRUE
        msg <- run_mod(fn = fn, mod_args = list(safer_check = TRUE, lib_path = 1))
        testthat::expect_true(object = base::grepl(x = msg, pattern = "THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT MUST BE A VECTOR OF CHARACTERS", fixed = TRUE), label = base::paste0(fn, ": lib_path numeric"))
        # function lib_path: deparsed then reported as non character
        msg2 <- run_mod(fn = fn, mod_args = list(safer_check = TRUE, lib_path = base::mean))
        testthat::expect_true(object = base::grepl(x = msg2, pattern = "MUST BE A VECTOR OF CHARACTERS", fixed = TRUE), label = base::paste0(fn, ": lib_path function"))
        # non-existing directory
        msg3 <- run_mod(fn = fn, mod_args = list(safer_check = TRUE, lib_path = "Z:/BAD_PATH_DOES_NOT_EXIST"))
        testthat::expect_true(object = base::grepl(x = msg3, pattern = "NOT EXIST"), label = base::paste0(fn, ": lib_path non existing"))
        # multiple paths: plural message
        msg4 <- run_mod(fn = fn, mod_args = list(safer_check = TRUE, lib_path = base::c("Z:/BAD1", "Z:/BAD2")))
        testthat::expect_true(object = base::grepl(x = msg4, pattern = "PATHS", fixed = TRUE), label = base::paste0(fn, ": lib_path plural"))
        # existing directory: function must run without lib_path error
        tempo_dir <- base::tempdir()
        msg5 <- run_mod(fn = fn, mod_args = list(safer_check = TRUE, lib_path = tempo_dir))
        testthat::expect_false(object = base::grepl(x = msg5, pattern = "lib_path", fixed = TRUE), label = base::paste0(fn, ": lib_path existing dir ok"))
        # lib_path = NULL under safer_check = TRUE: default .libPaths() branch
        msg6 <- run_mod(fn = fn, mod_args = list(safer_check = TRUE, lib_path = NULL))
        testthat::expect_false(object = base::grepl(x = msg6, pattern = "lib_path", fixed = TRUE), label = base::paste0(fn, ": lib_path NULL ok"))
    }
})

testthat::test_that("management of empty string in character arguments (backbone)", {
    # functions having a "management of \"\" in arguments of mode character" section with a per-argument loop
    emptystring_specs <- base::list(
        gg_empty = "text",
        gg_box = "y"
    )
    for(fn in base::names(x = emptystring_specs)){
        msg <- run_mod(fn = fn, mod_args = stats::setNames(base::list(""), emptystring_specs[[fn]]))
        testthat::expect_true(object = base::grepl(x = msg, pattern = "CANNOT CONTAIN EMPTY STRING", fixed = TRUE), label = base::paste0(fn, ": empty string ", emptystring_specs[[fn]]))
    }
    # gg_scatter: x is not in the tempo_arg ""-management list (can be a list), but the dedicated column-name check catches ""
    msg <- run_mod(fn = "gg_scatter", mod_args = list(x = ""))
    testthat::expect_true(object = base::grepl(x = msg, pattern = "MUST BE A COLUMN NAME OF data1 ARGUMENT", fixed = TRUE), label = "gg_scatter: empty string x caught by the column-name check")
})

testthat::test_that("arg with no default values and all-default calls (backbone)", {
    # missing no-default argument triggers the dedicated branch (gg_get_legend)
    msg <- base::tryCatch(expr = base::eval(expr = base::quote(gg_get_legend(safer_check = FALSE, error_text = "")), envir = base::asNamespace(ns = "saferGG")), error = function(e){base::conditionMessage(e)})
    testthat::expect_true(object = base::grepl(x = msg, pattern = "NO DEFAULT VALUE", fixed = TRUE), label = "gg_get_legend: missing ggplot_built")
    # all-default user arguments: arg_user_setting_eval <- NULL branch (gg_empty bare call)
    testthat::expect_no_error(object = saferGG::gg_empty(safer_check = FALSE, error_text = ""))
    # same branch for gg_palette and gg_get_legend (bare calls: arg_user_setting is length 0 -> NULL branch, then the mandatory-args check fires because n / ggplot_built have no default)
    msg <- base::tryCatch(expr = saferGG::gg_palette(safer_check = FALSE, error_text = ""), error = function(e){base::conditionMessage(e)})
    testthat::expect_true(object = base::grepl(x = msg, pattern = "NO DEFAULT VALUE", fixed = TRUE), label = "gg_palette: bare call covers arg_user_setting_eval <- NULL")
    msg2 <- base::tryCatch(expr = base::suppressWarnings(base::suppressMessages(saferGG::gg_get_legend(safer_check = FALSE, error_text = ""))), error = function(e){base::conditionMessage(e)})
    testthat::expect_true(object = base::grepl(x = msg2, pattern = "NO DEFAULT VALUE", fixed = TRUE), label = "gg_get_legend: bare call covers arg_user_setting_eval <- NULL")
})

testthat::test_that("gg_palette n == 0 branch", {
    msg <- run_mod(fn = "gg_palette", mod_args = list(n = 0))
    testthat::expect_true(object = base::grepl(x = msg, pattern = "MUST BE A NON ZERO INTEGER", fixed = TRUE), label = "gg_palette: n = 0")
    # missing n (no default value branch)
    msg2 <- base::tryCatch(expr = base::eval(expr = base::quote(gg_palette(kind = "std", safer_check = FALSE, error_text = "")), envir = base::asNamespace(ns = "saferGG")), error = function(e){base::conditionMessage(e)})
    testthat::expect_true(object = base::grepl(x = msg2, pattern = "NO DEFAULT VALUE", fixed = TRUE), label = "gg_palette: missing n")
})

testthat::test_that("error_text embedding (backbone)", {
    # error_text is embedded in the error message: the arg_check error uses " INSIDE " (embed_error_text)
    msg <- run_mod(fn = "gg_palette", mod_args = list(kind = "BAD"))
    testthat::expect_true(object = base::grepl(x = msg, pattern = "ERROR INSIDE saferGG::gg_palette()", fixed = TRUE), label = "gg_palette: default error_text embedding")
    # custom error_text
    cl <- base_calls[["gg_palette"]]
    cl$kind <- "BAD"
    cl$error_text <- "MY CUSTOM CONTEXT:"
    res <- base::tryCatch(expr = base::eval(expr = cl, envir = base::asNamespace(ns = "saferGG")), error = function(e){base::conditionMessage(e)})
    testthat::expect_true(object = base::grepl(x = res, pattern = "ERROR INSIDE saferGG::gg_palette()MY CUSTOM CONTEXT:", fixed = TRUE), label = "gg_palette: custom error_text")
})

base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
