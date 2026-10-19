# tests for the gg_donut() function of the saferGG package

testthat::test_that("gg_donut draws donut plots and returns the expected structure", {

    ## data argument values
    obs1 <- base::data.frame(Km = base::c(20, 10, 1, 5), Car = base::c("TUUT", "WIIM", "BIP", "WROUM"), Country = base::c("FR", "UK", "US", NA), stringsAsFactors = TRUE)
    ## end data argument values

    ## initialization of tests
    tempo_dir <- base::tempdir()
    tempo_file <- base::file.path(tempo_dir, "test_gg_donut.pdf")
    grDevices::pdf(file = tempo_file)
    ## end initialization of tests

    ## tests
    # basic donut drawn
    testthat::expect_no_error(object = saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", safer_check = FALSE))
    # annotation (hole_text) drawn
    testthat::expect_no_error(object = saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", hole_text = TRUE, safer_check = FALSE))
    # return structure
    output <- saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", legend_limit = 0.9, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::all(base::c("data", "removed_row_nb", "removed_rows", "plot_data", "panel", "axes", "warn", "ggplot", "gtable") %in% base::names(x = output)))
    # title displayed: title.grob built
    testthat::expect_no_error(object = saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", title = "DONUT TITLE", safer_check = FALSE))
    # return_gtable = FALSE: NULL gtable component
    output_nogtable <- saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", return_gtable = FALSE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output_nogtable, "list")
    testthat::expect_null(object = output_nogtable$gtable)
    # legend_limit warning
    output <- saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", legend_limit = 0.9, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "legend_limit", fixed = TRUE))
    # fill_color provided
    testthat::expect_no_error(object = saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", fill_color = base::c("red", "blue", "green", "orange"), safer_check = FALSE))
    # fill_palette used (fill_color NULL)
    testthat::expect_no_error(object = saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", fill_palette = "Set1", safer_check = FALSE))
    # both fill_color and fill_palette -> fill_color wins
    testthat::expect_no_error(object = saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", fill_color = base::c("red", "blue", "green", "orange"), fill_palette = "Set1", safer_check = FALSE))
    # legend_limit NULL with >= 10 categories (line 1138 branch)
    obs10 <- base::data.frame(Km = 1:12, Car = base::paste0("CAT", 1:12), stringsAsFactors = TRUE)
    output <- saferGG::gg_donut(data1 = obs10, freq = "Km", categ = "Car", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # legend_limit NULL with < 10 categories (line 1140 branch)
    output <- saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # add with ggplot2::theme -> internal theme inactivated (warn branch)
    output <- saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", add = "+ggplot2::theme_void()", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "\"ggplot2::theme\" STRING DETECTED", fixed = TRUE))
    # add with ggplot2::facet_grid
    testthat::expect_no_error(object = saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", add = "+ggplot2::facet_grid(rows = ~Country)", plot = FALSE, return = TRUE, safer_check = FALSE))
    # add with facet_grid but bad facet variable
    testthat::expect_error(object = saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", add = "+ggplot2::facet_grid(rows = ~BADFACET)", plot = FALSE, safer_check = FALSE), regexp = "PROBLEM OF VARIABLE DETECTION", fixed = TRUE)
    # legend_name provided
    testthat::expect_no_error(object = saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", legend_name = "MY LEGEND", plot = FALSE, return = TRUE, safer_check = FALSE))
    # legend_name bad type
    testthat::expect_error(object = saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", legend_name = 12, plot = FALSE, safer_check = FALSE), regexp = "legend_name ARGUMENT MUST BE", fixed = FALSE)
    # annotation provided -> annotation column check branches (duplicated values -> warn)
    obs_ann_dup <- obs1
    obs_ann_dup$Country <- base::c("FR", "FR", "US", NA)
    output <- saferGG::gg_donut(data1 = obs_ann_dup, freq = "Km", categ = "Car", annotation = "Country", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "PRESENCE OF DUPLICATED VALUES IN THE Country COLUMN", fixed = TRUE))
    # annotation column of bad class (numeric)
    obs_ann_num <- obs1
    obs_ann_num$NumCol <- 1:4
    testthat::expect_error(object = saferGG::gg_donut(data1 = obs_ann_num, freq = "Km", categ = "Car", annotation = "NumCol", plot = FALSE, safer_check = FALSE), regexp = "THE annotation COLUMN OF data1 MUST BE CLASS", fixed = TRUE)
    # freq with Inf values -> warn + removal
    obs_inf_freq <- obs1
    obs_inf_freq$Km <- base::c(20, Inf, 1, 5)
    output <- saferGG::gg_donut(data1 = obs_inf_freq, freq = "Km", categ = "Car", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "PRESENCE OF Inf OR NA VALUES IN THE Km COLUMN", fixed = TRUE))
    # freq with 0 values -> warn + removal
    obs_zero_freq <- obs1
    obs_zero_freq$Km <- base::c(20, 0, 1, 5)
    output <- saferGG::gg_donut(data1 = obs_zero_freq, freq = "Km", categ = "Car", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "PRESENCE OF 0 VALUES IN THE Km COLUMN", fixed = TRUE))
    # freq with NA values -> warn + removal
    obs_na_freq_partial <- obs1
    obs_na_freq_partial$Km <- base::c(20, NA, 1, 5)
    output <- saferGG::gg_donut(data1 = obs_na_freq_partial, freq = "Km", categ = "Car", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "PRESENCE OF Inf OR NA VALUES IN THE Km COLUMN", fixed = TRUE))
    # categ with NA values -> warn + removal
    obs_na_categ <- obs1
    obs_na_categ$Car <- base::c("TUUT", "WIIM", "BIP", NA)
    output <- saferGG::gg_donut(data1 = obs_na_categ, freq = "Km", categ = "Car", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "PRESENCE OF NA VALUES IN THE Car COLUMN", fixed = TRUE))
    # warn_print = TRUE
    testthat::expect_warning(object = saferGG::gg_donut(data1 = obs_na_categ, freq = "Km", categ = "Car", plot = FALSE, warn_print = TRUE, safer_check = FALSE), regexp = "PRESENCE OF NA", fixed = FALSE)
    # return_ggplot = TRUE
    output <- saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", plot = FALSE, return = TRUE, return_ggplot = TRUE, safer_check = FALSE)
    testthat::expect_s3_class(object = output$ggplot, class = "ggplot")
    ## end tests

    ## errors
    # bad freq column
    testthat::expect_error(object = saferGG::gg_donut(data1 = obs1, freq = "BAD", categ = "Car", plot = FALSE, safer_check = FALSE), regexp = "MUST BE A COLUMN NAME", fixed = TRUE)
    # bad hole_size
    testthat::expect_error(object = saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", hole_size = 2, plot = FALSE, safer_check = FALSE), regexp = "MUST BE|BETWEEN", fixed = FALSE)
    # add without +
    testthat::expect_error(object = saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", add = "ggplot2::coord_flip()", plot = FALSE, safer_check = FALSE), regexp = "MUST START WITH", fixed = TRUE)
    # bad fill_palette
    testthat::expect_error(object = saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", fill_palette = "BADPAL", plot = FALSE, safer_check = FALSE), regexp = "THE fill_palette ARGUMENT MUST BE SOME OF THESE OPTIONS", fixed = TRUE)
    # bad fill_color (not color, not factor, not positive integer)
    testthat::expect_error(object = saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", fill_color = "BAD", plot = FALSE, safer_check = FALSE), regexp = "fill_color ARGUMENT MUST BE A VECTOR OF (1) HEXADECIMAL COLOR STRINGS", fixed = TRUE)
    # fill_color with Inf among integers: Inf is of mode double, so the dedicated integer check fails and the generic message fires
    testthat::expect_error(object = saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", fill_color = base::c(1L, Inf), plot = FALSE, safer_check = FALSE), regexp = "fill_color ARGUMENT MUST BE A VECTOR OF (1) HEXADECIMAL COLOR STRINGS", fixed = TRUE)
    # fill_color with 0 among integers
    testthat::expect_error(object = saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", fill_color = base::c(1L, 0L), plot = FALSE, safer_check = FALSE), regexp = "fill_color ARGUMENT CANNOT CONTAIN 0 AMONG POSITIVE INTEGER VALUES", fixed = TRUE)
    # fill_color factor class accepted (as.character path)
    testthat::expect_no_error(object = saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", fill_color = base::factor(x = base::c("red", "blue", "green", "orange")), plot = FALSE, safer_check = FALSE))
    # fill_color as positive integers accepted
    testthat::expect_no_error(object = saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", fill_color = base::c(1L, 2L, 3L, 4L), plot = FALSE, safer_check = FALSE))
    # border_color bad value (not a color name, not hex, not integer)
    testthat::expect_error(object = saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", border_color = "BADCOL", plot = FALSE, safer_check = FALSE), regexp = "border_color ARGUMENT MUST BE (1) A HEXADECIMAL COLOR STRING", fixed = TRUE)
    # border_color as integer accepted
    testthat::expect_no_error(object = saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", border_color = 2L, plot = FALSE, safer_check = FALSE))
    # border_color as factor rejected (must be single character string or integer)
    testthat::expect_error(object = saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", border_color = base::factor(x = "red"), plot = FALSE, safer_check = FALSE), regexp = "border_color ARGUMENT MUST BE A SINGLE CHARACTER STRING OR POSITIVE INTEGER", fixed = TRUE)
    # annotation provided: valid column
    testthat::expect_no_error(object = saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", annotation = "Car", plot = FALSE, safer_check = FALSE))
    # annotation not a column name
    testthat::expect_error(object = saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", annotation = "BAD", plot = FALSE, safer_check = FALSE), regexp = "annotation ARGUMENT MUST BE A COLUMN NAME", fixed = TRUE)
    # add without ggplot2::/lemon:: prefix
    testthat::expect_error(object = saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", add = "+coord_flip()", plot = FALSE, safer_check = FALSE), regexp = "MUST CONTAIN \"ggplot2::\" OR \"lemon::\"", fixed = TRUE)
    # add not finished by )
    testthat::expect_error(object = saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", add = "+ggplot2::coord_flip(", plot = FALSE, safer_check = FALSE), regexp = "MUST FINISH BY", fixed = TRUE)
    # add with facet_wrap but bad facet variable
    testthat::expect_error(object = saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", add = "+ggplot2::facet_wrap(facets = ~BADFACET)", plot = FALSE, safer_check = FALSE), regexp = "PROBLEM OF VARIABLE DETECTION", fixed = TRUE)
    # add with facet_wrap on a valid column
    testthat::expect_no_error(object = saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", add = "+ggplot2::facet_wrap(facets = ~Country)", plot = FALSE, safer_check = FALSE))
    # legend_limit too high -> warn + fallback branch
    output <- saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", legend_limit = 0.999, legend_width = 0.4, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "HIGHER THAN THE HIGHEST PROPORTION", fixed = TRUE))
    # legend_show = FALSE with legend_width kept: empty legend space (device-leak regression check)
    output <- saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", legend_show = FALSE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # legend_limit = "all"
    testthat::expect_no_error(object = saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", legend_limit = "all", legend_width = 0.4, plot = FALSE, safer_check = FALSE))
    # legend_show = FALSE
    testthat::expect_no_error(object = saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", legend_show = FALSE, plot = FALSE, return = TRUE, safer_check = FALSE))
    # legend_width = NULL -> legend_plot NULL path
    testthat::expect_no_error(object = saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "Car", legend_width = NULL, plot = FALSE, return = TRUE, safer_check = FALSE))
    # annotation NA-only column error
    obs_na <- obs1
    obs_na$Car <- base::factor(x = base::rep(x = NA, times = 4))
    testthat::expect_error(object = saferGG::gg_donut(data1 = obs_na, freq = "Km", categ = "Car", safer_check = FALSE), regexp = "THE categ COLUMN OF data1 CANNOT BE JUST NA", fixed = TRUE)
    # annotation column all NA: error from the annotation branch
    obs_ann_na <- obs1
    obs_ann_na$ann <- base::c(NA, NA, NA, NA)
    testthat::expect_error(object = saferGG::gg_donut(data1 = obs_ann_na, freq = "Km", categ = "Car", annotation = "ann", plot = FALSE, safer_check = FALSE), regexp = "IF NON NULL, THE annotation COLUMN OF data1 CANNOT BE JUST NA", fixed = TRUE)
    # annotation column of numeric class: error from the factor-or-character check
    obs_ann_num2 <- obs1
    obs_ann_num2$ann <- 1:4
    testthat::expect_error(object = saferGG::gg_donut(data1 = obs_ann_num2, freq = "Km", categ = "Car", annotation = "ann", plot = FALSE, safer_check = FALSE), regexp = "THE annotation COLUMN OF data1 MUST BE CLASS \"factor\" OR \"character\"", fixed = TRUE)
    # data1 empty: error
    testthat::expect_error(object = saferGG::gg_donut(data1 = base::data.frame(), freq = "Km", categ = "Car", plot = FALSE, safer_check = FALSE), regexp = "data1 ARGUMENT CANNOT BE LENGTH 0", fixed = TRUE)
    # zero-argument call: mandatory/no-default block lists data1, freq, categ
    res_donut_zeroargs <- base::tryCatch(
        expr = saferGG::gg_donut(),
        error = function(e){base::conditionMessage(e)}
    )
    testthat::expect_true(object = base::grepl(x = res_donut_zeroargs, pattern = "NO DEFAULT VALUE AND REQUIRE ONE", fixed = TRUE))
    testthat::expect_true(object = base::grepl(x = res_donut_zeroargs, pattern = "freq", fixed = TRUE))
    testthat::expect_true(object = base::grepl(x = res_donut_zeroargs, pattern = "categ", fixed = TRUE))
    # freq column made only of 0 -> empty after removal
    obs_zero <- obs1
    obs_zero$Km <- base::c(0, 0, 0, 0)
    testthat::expect_error(object = saferGG::gg_donut(data1 = obs_zero, freq = "Km", categ = "Car", plot = FALSE, safer_check = FALSE), regexp = "THE data1 ARGUMENT IS EMPTY AFTER Inf, NA AND 0 REMOVAL", fixed = TRUE)
    # freq column all NA or Inf
    obs_na_freq <- obs1
    obs_na_freq$Km <- base::c(NA, NA, NA, NA)
    testthat::expect_error(object = saferGG::gg_donut(data1 = obs_na_freq, freq = "Km", categ = "Car", plot = FALSE, safer_check = FALSE), regexp = "THE freq COLUMN OF data1 CANNOT BE JUST NA OR Inf", fixed = TRUE)
    # freq with non numeric values -> arg_check error
    obs_char_freq <- obs1
    obs_char_freq$Km <- base::as.character(x = obs_char_freq$Km)
    testthat::expect_error(object = saferGG::gg_donut(data1 = obs_char_freq, freq = "Km", categ = "Car", plot = FALSE, safer_check = FALSE), regexp = "MUST BE MODE numeric", fixed = TRUE)
    # freq with negative values
    obs_neg_freq <- obs1
    obs_neg_freq$Km <- base::c(-1, 2, 3, 4)
    testthat::expect_error(object = saferGG::gg_donut(data1 = obs_neg_freq, freq = "Km", categ = "Car", plot = FALSE, safer_check = FALSE), regexp = "MUST BE MADE OF NON NEGATIVE VALUES", fixed = TRUE)
    # duplicated categ values
    obs_dup <- obs1
    obs_dup$Car <- base::factor(x = base::c("TUUT", "TUUT", "BIP", "WROUM"))
    testthat::expect_error(object = saferGG::gg_donut(data1 = obs_dup, freq = "Km", categ = "Car", plot = FALSE, safer_check = FALSE), regexp = "THE categ COLUMN OF data1 CANNOT CONTAIN DUPLICATED VALUES", fixed = TRUE)
    # freq or categ not column names
    testthat::expect_error(object = saferGG::gg_donut(data1 = obs1, freq = "BAD", categ = "Car", plot = FALSE, safer_check = FALSE), regexp = "freq ARGUMENT MUST BE A COLUMN NAME OF THE data1 ARGUMENT", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "BAD", plot = FALSE, safer_check = FALSE), regexp = "categ ARGUMENT MUST BE A COLUMN NAME OF THE data1 ARGUMENT", fixed = TRUE)
    # empty string in a character-mode argument: error
    testthat::expect_error(object = saferGG::gg_donut(data1 = obs1, freq = "", categ = "Car", plot = FALSE, safer_check = FALSE), regexp = "CANNOT CONTAIN EMPTY STRING \"\"", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_donut(data1 = obs1, freq = "Km", categ = "", plot = FALSE, safer_check = FALSE), regexp = "CANNOT CONTAIN EMPTY STRING \"\"", fixed = TRUE)
    ## end errors
    # cleanup of all files and devices created during the tests
    while(base::length(x = grDevices::dev.list()) > 0){ grDevices::dev.off() }
    base::unlink(x = tempo_dir, recursive = TRUE, force = TRUE)
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
