# This file tests the "mandatory arg of safer-r functions" checking block present in every saferGG function
# This block fires only if the function is written without one of the mandatory formals (lib_path, safer_check, error_text for public functions)
# It cannot be triggered by user values: the test uses a temporary function built from the same body but with the mandatory formal removed from formals()
# No # nocov markers are used, and no R source files are modified: the block is actually executed by the tests

testthat::test_that("mandatory arg block of safer-r functions (public functions)", {

    ## data argument values
    # full calls with all formals except the mandatory one to drop (lib_path for all these functions)
    full_calls <- base::list(
        gg_box = base::quote(gg_box(data1 = base::data.frame(a = base::c(1, 2, 3), b = base::c("x", "y", "x"), stringsAsFactors = TRUE), y = "a", categ = "b", safer_check = FALSE, error_text = "")),
        gg_empty = base::quote(gg_empty(text = "TEST", safer_check = FALSE, error_text = "")),
        gg_get_legend = base::quote(gg_get_legend(ggplot_built = ggplot2::ggplot_build(ggplot2::ggplot(data = base::data.frame(x = 1:3, y = 1:3, stringsAsFactors = TRUE), ggplot2::aes(x = x, y = y))), safer_check = FALSE, error_text = "")),
        gg_just = base::quote(gg_just(angle = 45, pos = "top", kind = "simple", safer_check = FALSE, error_text = "")),
        gg_palette = base::quote(gg_palette(n = 3, kind = "gg", safer_check = FALSE, error_text = "")),
        gg_heatmap = base::quote(gg_heatmap(data1 = base::data.frame(a = 1:3, b = base::c("x", "y", "x"), c = 1:3, stringsAsFactors = TRUE), legend_name1 = "TEST", low_color1 = "blue", mid_color1 = "white", high_color1 = "red", limit1 = c(0, 3), safer_check = FALSE, error_text = "")),
        gg_miami = base::quote(gg_miami(file = "BAD_FILE_PATH_DOES_NOT_EXIST", chr = "BAD_FILE_PATH_DOES_NOT_EXIST", top_y_column = "AF", safer_check = FALSE, error_text = "")),
        gg_scatter = base::quote(gg_scatter(data1 = base::data.frame(x = 1:3, y = 1:3, stringsAsFactors = TRUE), x = "x", y = "y", safer_check = FALSE, error_text = "")),
        gg_point_rast = base::quote(gg_point_rast(data = base::data.frame(x = 1:3, y = 1:3, stringsAsFactors = TRUE), mapping = ggplot2::aes(x = x, y = y), safer_check = FALSE, error_text = "")),
        gg_donut = base::quote(gg_donut(data1 = base::data.frame(a = base::c(1, 2), stringsAsFactors = TRUE), freq = "a", safer_check = FALSE, error_text = ""))
    )
    ## end data argument values

    ## initialization of tests
    # none
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    ######## mandatory arg of safer-r functions
    for(fn in base::names(x = full_calls)){
        f <- base::get(x = fn, envir = base::asNamespace("saferGG"))
        fml <- base::formals(fun = f)
        fml_dropped <- fml[base::setdiff(x = base::names(x = fml), y = "lib_path")] # drop lib_path: the block must fire
        tempo_env <- base::new.env(parent = base::environment(fun = f))
        tempo_env[[fn]] <- base::as.function(base::c(fml_dropped, base::list(base::body(f))), envir = tempo_env)
        cl <- full_calls[[fn]]
        res <- base::tryCatch(
            expr = base::eval(expr = cl, envir = tempo_env),
            error = function(e){base::conditionMessage(e)}
        )
        testthat::expect_true(object = base::grepl(x = res, pattern = "FOLLOWING ARGUMENT", fixed = TRUE), label = base::paste0(fn, ": mandatory-args block message"))
        testthat::expect_true(object = base::grepl(x = res, pattern = "lib_path", fixed = TRUE), label = base::paste0(fn, ": lib_path reported as missing mandatory arg"))
        testthat::expect_false(object = base::grepl(x = res, pattern = "object '", fixed = TRUE), label = base::paste0(fn, ": no error before the mandatory-args block"))
    }
    ######## end mandatory arg of safer-r functions

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    # restore check: with all formals present, the block must NOT fire
    testthat::expect_no_error(object = saferGG::gg_empty(text = "TEST", safer_check = FALSE, lib_path = NULL, error_text = ""))
    ## end other tests
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
