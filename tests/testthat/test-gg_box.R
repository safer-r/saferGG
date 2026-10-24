# tests for the gg_box() function of the saferGG package

testthat::test_that("gg_box draws boxplots and returns the expected structure", {

    ## data argument values
    base::set.seed(seed = 1, kind = NULL, normal.kind = NULL, sample.kind = NULL) # to make the test data deterministic (the tick-number checks below are data-dependent)
    obs1 <- base::data.frame(y = stats::rnorm(n = 30, mean = 10, sd = 2), grp = base::rep(x = c("A", "B", "C"), times = 10), stringsAsFactors = TRUE)
    ## end data argument values

    ## initialization of tests
    tempo_dir <- base::tempdir()
    tempo_file <- base::file.path(tempo_dir, "test_gg_box.pdf")
    grDevices::pdf(file = tempo_file)
    ## end initialization of tests

    ## tests
    # basic boxplot drawn
    testthat::expect_no_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y_tick_nb = 5L, safer_check = FALSE))
    # return structure
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y_tick_nb = 5L, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::all(base::c("data", "removed_row_nb", "removed_rows", "plot", "panel", "axes", "warn", "ggplot", "gtable") %in% base::names(x = output)))
    # dots added
    testthat::expect_no_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = "same", safer_check = FALSE))
    # mean dot
    testthat::expect_no_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box_mean = TRUE, safer_check = FALSE))
    # grouped boxes (2 categ columns)
    obs2g <- base::data.frame(y = base::c(1.5, 2.3, 0.9, 3.1, 2.7, 1.1, 2.9, 3.3, 1.7, 2.5), grp1 = base::factor(x = base::rep(x = c("A", "B"), times = 5)), grp2 = base::factor(x = base::c("X", "X", "Y", "Y", "Z", "Z", "X", "X", "Y", "Y")), num = 1:10, ch = base::c("p", "q", "p", "q", "p", "q", "p", "q", "p", "q"), stringsAsFactors = TRUE)
    output <- saferGG::gg_box(data1 = obs2g, y = "y", categ = base::c("grp1", "grp2"), return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # box_legend_name
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box_legend_name = "BL", return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # categ_color character vector
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ_color = base::c("red", "blue", "green"), return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # categ_color integer vector
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ_color = base::c(1L, 2L, 3L), return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # box_fill = FALSE
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box_fill = FALSE, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # box_fill = TRUE: polygon boxes, whiskers, mean diamond, median
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box_fill = TRUE, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::any(output$plot$main.box$group != 0))
    # box_fill = TRUE with whisker edges and no mean
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box_fill = TRUE, box_mean = FALSE, box_whisker_width = 0.5, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = "sup.whisker.edge" %in% base::names(output$plot))
    # box_fill = TRUE with 2 categ columns: polygon with grouped boxes
    output <- saferGG::gg_box(data1 = obs2g, y = "y", categ = base::c("grp1", "grp2"), box_fill = TRUE, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # box_fill = FALSE with whisker edges (box_whisker_width > 0)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box_fill = FALSE, box_whisker_width = 0.5, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = "sup.whisker.edge" %in% base::names(output$plot))
    # dot display sub-branches: dot_border_size = 0 with and without dot_categ
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_border_size = 0, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    obs_dotc <- obs1
    obs_dotc$dotc <- base::rep(x = c("P", "Q"), times = 15)
    output <- saferGG::gg_box(data1 = obs_dotc, y = "y", categ = "grp", dot_color = base::c("red", "green"), dot_categ = "dotc", dot_border_size = 0, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    output <- saferGG::gg_box(data1 = obs_dotc, y = "y", categ = "grp", dot_color = base::c("red", "green"), dot_categ = "dotc", dot_border_color = "black", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # box_notch
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box_notch = TRUE, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # dot_color character vector
    output <- saferGG::gg_box(data1 = obs2g, y = "y", categ = base::c("grp1", "grp2"), dot_color = base::c("red", "blue", "green"), return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # dot_categ with dot_color
    output <- saferGG::gg_box(data1 = obs2g, y = "y", categ = "grp1", dot_color = base::c("red", "blue", "green"), dot_categ = "grp2", return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # dot_categ identical to last categ: allowed
    output <- saferGG::gg_box(data1 = obs2g, y = "y", categ = base::c("grp1", "grp2"), dot_color = base::c("red", "blue", "green"), dot_categ = "grp2", return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # dot_tidy
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = "same", dot_tidy = TRUE, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # dot_jitter and dot_seed
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = "same", dot_jitter = 0.2, dot_seed = 1L, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # dot_border_color
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = "same", dot_border_color = "red", return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # x_lab as expression
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", x_lab = base::expression(paste("LAB")), return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # y_lim
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y_lim = base::c(0, 5), return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # y_log
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y_log = "log10", return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # y_log forces vertical = TRUE (warn branch)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", vertical = FALSE, y_log = "log10", return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "CANNOT FLIP BOXES HORIZONTALLY WITH A Y_LOG2 SCALE", fixed = TRUE))
    # vertical = FALSE (horizontal boxes); y_tick_nb set explicitly to make the tick count deterministic (dot jitter can shift the y-range)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", vertical = FALSE, y_tick_nb = 5L, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # y_tick_nb and y_second_tick_nb
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y_tick_nb = 5L, y_second_tick_nb = 2L, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # stat_pos top / stat_mean
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", stat_pos = "top", stat_mean = TRUE, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # article and grid
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", article = TRUE, grid = FALSE, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # add with ggplot2::theme
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", add = "+ggplot2::theme_void()", return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # add with facet_wrap
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", add = "+ggplot2::facet_wrap(~grp)", return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # y with Inf values: warn + removal
    obs_inf <- obs1
    obs_inf$y[1] <- Inf
    output <- saferGG::gg_box(data1 = obs_inf, y = "y", categ = "grp", return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "PRESENCE OF -Inf OR Inf VALUES IN THE y_modif COLUMN", fixed = TRUE))
    # y with NA values: warn + removal
    obs_na <- obs1
    obs_na$y[1] <- NA
    output <- saferGG::gg_box(data1 = obs_na, y = "y", categ = "grp", return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "NA DETECTED IN COLUMNS OF data1", fixed = TRUE))
    # categ_class_order provided
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ_class_order = base::list(base::c("C", "B", "A")), return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # dot_color integer vector with a categ column: integers converted to colors
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = base::c(1L, 2L, 3L), return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # dot_border_color integer
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = "same", dot_border_color = 3L, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # dot_color one color per class of categ (length == number of classes) without dot_categ
    output <- saferGG::gg_box(data1 = obs2g, y = "y", categ = "grp1", dot_color = base::c("red", "blue"), return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # dot_color single string with dot_categ
    output <- saferGG::gg_box(data1 = obs2g, y = "y", categ = "grp1", dot_color = "red", dot_categ = "grp2", return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # dot_color with dot_categ: one color per class of dot_categ
    output <- saferGG::gg_box(data1 = obs2g, y = "y", categ = "grp1", dot_color = base::c("red", "blue", "green"), dot_categ = "grp2", return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # dot_color NULL with dot_categ
    output <- saferGG::gg_box(data1 = obs2g, y = "y", categ = "grp1", dot_categ = "grp2", return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # dot_categ_class_order without dot_color: accepted
    output <- saferGG::gg_box(data1 = obs2g, y = "y", categ = "grp1", dot_categ_class_order = base::c("X", "Y", "Z"), return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # categ_class_order with a NULL compartment (2 categ columns)
    output <- saferGG::gg_box(data1 = obs2g, y = "y", categ = base::c("grp1", "grp2"), categ_class_order = base::list(NULL, base::c("X", "Y", "Z")), return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # stat_pos = "above" (second valid option)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", stat_mean = TRUE, stat_pos = "above", return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # stat_angle
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", stat_mean = TRUE, stat_angle = 45, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # x_angle
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", x_angle = 45, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # 2 categ with stat_mean
    output <- saferGG::gg_box(data1 = obs2g, y = "y", categ = base::c("grp1", "grp2"), stat_mean = TRUE, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # facet by a column different from categ + dot_tidy
    output <- saferGG::gg_box(data1 = obs2g, y = "y", categ = "grp1", dot_color = "same", dot_tidy = TRUE, add = "+ggplot2::facet_wrap(~ch)", return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # box_line_size
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box_line_size = 2, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # box_space with 2 categ
    output <- saferGG::gg_box(data1 = obs2g, y = "y", categ = base::c("grp1", "grp2"), box_space = 0.5, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # y_log = "log2" with tick numbers
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y_log = "log2", y_tick_nb = 3L, y_second_tick_nb = 2L, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # dot_alpha
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = "same", dot_alpha = 0.3, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # warn_print with dot_color = "same" (dot color warning printed)
    testthat::expect_warning(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = "same", warn_print = TRUE, plot = FALSE, safer_check = FALSE), regexp = "dot_color ARGUMENT HAS BEEN SET TO \"same\"", fixed = TRUE)
    # stat_mean with stat_pos = "top" combined with box_mean
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box_mean = TRUE, stat_mean = TRUE, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    ## end tests

    ## errors
    # bad y column
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "BAD", categ = "grp", safer_check = FALSE), regexp = "MUST BE A COLUMN NAME", fixed = TRUE)
    # add without +
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", add = "ggplot2::coord_flip()", safer_check = FALSE), regexp = "MUST START WITH", fixed = TRUE)
    # more than 2 categ columns
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = base::c("grp", "grp", "grp"), plot = FALSE, safer_check = FALSE), regexp = "categ ARGUMENT CANNOT HAVE MORE THAN 2 COLUMN NAMES", fixed = TRUE)
    # bad categ column
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "BAD", plot = FALSE, safer_check = FALSE), regexp = "categ ARGUMENT MUST BE COLUMN NAMES OF data1", fixed = TRUE)
    # categ column not character/factor
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "y", plot = FALSE, safer_check = FALSE), regexp = "MUST BE A FACTOR OR CHARACTER VECTOR", fixed = TRUE)
    # categ_color with 0 among integers
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ_color = base::c(0L, 2L), plot = FALSE, safer_check = FALSE), regexp = "categ_color ARGUMENT MUST BE A FACTOR OR CHARACTER VECTOR OR POSITVE INTEGER VECTOR", fixed = TRUE)
    # categ_color with bad color names
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ_color = base::c("red", "BAD"), plot = FALSE, safer_check = FALSE), regexp = "categ_color ARGUMENT MUST BE A HEXADECIMAL COLOR VECTOR", fixed = TRUE)
    # box_whisker_kind bad option
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box_whisker_kind = "BAD", plot = FALSE, safer_check = FALSE), regexp = "box_whisker_kind ARGUMENT MUST BE SOME OF THESE OPTIONS", fixed = TRUE)
    # box_whisker_kind = "std" with box_whisker_width out of [0, 1]
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box_whisker_kind = "std", box_whisker_width = 1.5, plot = FALSE, safer_check = FALSE), regexp = "box_whisker_width ARGUMENT MUST BE DECIMAL VALUES BETWEEN 0 AND 1", fixed = TRUE)
    # dot_tidy_bin_nb = 0
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = "same", dot_tidy = TRUE, dot_tidy_bin_nb = 0L, plot = FALSE, safer_check = FALSE), regexp = "dot_tidy_bin_nb ARGUMENT MUST BE A NON-NULL AND POSITVE INTEGER VALUE", fixed = TRUE)
    # y_tick_nb negative
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y_tick_nb = -1L, plot = FALSE, safer_check = FALSE), regexp = "y_tick_nb ARGUMENT MUST BE A NON NULL POSITIVE INTEGER", fixed = TRUE)
    # y_log with non-positive y_lim
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y_log = "log10", y_lim = base::c(-1, 5), plot = FALSE, safer_check = FALSE), regexp = "y_lim ARGUMENT CANNOT HAVE ZERO OR NEGATIVE VALUES WITH THE y_log ARGUMENT", fixed = TRUE)
    # stat_pos bad option
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", stat_pos = 1.1, plot = FALSE, safer_check = FALSE), regexp = "stat_pos ARGUMENT MUST BE SOME OF THESE OPTIONS", fixed = TRUE)
    # add not finished by )
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", add = "+ggplot2::coord_flip(", plot = FALSE, safer_check = FALSE), regexp = "MUST FINISH BY", fixed = TRUE)
    # add without ggplot2::/lemon:: prefix
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", add = "+coord_flip()", plot = FALSE, safer_check = FALSE), regexp = "MUST CONTAIN", fixed = TRUE)
    # add with facet_wrap but bad facet variable
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", add = "+ggplot2::facet_wrap(~BADFACET)", plot = FALSE, safer_check = FALSE), regexp = "PROBLEM OF VARIABLE DETECTION", fixed = TRUE)
    # y non numeric column: reserved-word rename happens first, then the internal consistency check fires
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "grp", categ = "grp", plot = FALSE, safer_check = FALSE), regexp = "INTERNAL CODE ERROR IN gg_box", fixed = TRUE)
    # dot_color with 0 among integers
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = base::c(0L, 1L), dot_categ = "grp", plot = FALSE, safer_check = FALSE), regexp = "dot_color MUST BE A FACTOR OR CHARACTER VECTOR OR POSITVE INTEGER VECTOR", fixed = TRUE)
    # dot_color integer vector with wrong length vs categ classes
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = base::c(1L, 2L), plot = FALSE, safer_check = FALSE), regexp = "dot_color ARGUMENT MUST BE (1) LENGTH 1", fixed = TRUE)
    # dot_color with bad color values
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = "BAD", plot = FALSE, safer_check = FALSE), regexp = "dot_color ARGUMENT MUST BE (1) A HEXADECIMAL COLOR VECTOR", fixed = TRUE)
    # dot_border_color with bad color value
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = "same", dot_border_color = "BAD", plot = FALSE, safer_check = FALSE), regexp = "dot_border_color ARGUMENT MUST BE (1) A HEXADECIMAL COLOR STRING", fixed = TRUE)
    # y_lab neither character nor expression
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y_lab = 12, plot = FALSE, safer_check = FALSE), regexp = "y_lab ARGUMENT MUST BE A SINGLE CHARACTER STRING OR EXPRESSION", fixed = TRUE)
    # y_lim with infinite values
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y_lim = base::c(0, Inf), plot = FALSE, safer_check = FALSE), regexp = "THE y_lim ARGUMENT MUST BE MADE OF NON INFINITE VALUES", fixed = TRUE)
    # dot_categ_class_order with duplicated classes
    testthat::expect_error(object = saferGG::gg_box(data1 = obs2g, y = "y", categ = "grp1", dot_color = base::c("red", "blue"), dot_categ = "grp2", dot_categ_class_order = base::c("X", "X"), plot = FALSE, safer_check = FALSE), regexp = "dot_categ_class_order ARGUMENT CANNOT HAVE DUPLICATED CLASSES", fixed = TRUE)
    # dot_categ_class_order with values absent from dot_categ
    testthat::expect_error(object = saferGG::gg_box(data1 = obs2g, y = "y", categ = "grp1", dot_color = base::c("red", "blue"), dot_categ = "grp2", dot_categ_class_order = base::c("X", "Q"), plot = FALSE, safer_check = FALSE), regexp = "dot_categ_class_order ARGUMENT MUST BE CLASSES OF dot_categ ARGUMENT", fixed = TRUE)
    # categ_class_order list length != length(categ)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs2g, y = "y", categ = base::c("grp1", "grp2"), categ_class_order = base::list(base::c("A", "B")), plot = FALSE, safer_check = FALSE), regexp = "categ_class_order ARGUMENT MUST BE A LIST OF LENGTH EQUAL TO LENGTH OF categ", fixed = TRUE)
    # categ_class_order with duplicated classes
    testthat::expect_error(object = saferGG::gg_box(data1 = obs2g, y = "y", categ = "grp1", categ_class_order = base::list(base::c("A", "A")), plot = FALSE, safer_check = FALSE), regexp = "COMPARTMENT 1 OF categ_class_order ARGUMENT CANNOT HAVE DUPLICATED CLASSES", fixed = TRUE)
    # categ_class_order with values absent from the categ column
    testthat::expect_error(object = saferGG::gg_box(data1 = obs2g, y = "y", categ = "grp1", categ_class_order = base::list(base::c("A", "Q")), plot = FALSE, safer_check = FALSE), regexp = "COMPARTMENT 1 OF categ_class_order ARGUMENT MUST BE CLASSES OF ELEMENT 1 OF categ ARGUMENT", fixed = TRUE)
    # dot_color per-row length incorrectly associated to the classes of dot_categ
    testthat::expect_error(object = saferGG::gg_box(data1 = obs2g, y = "y", categ = "grp1", dot_color = base::c("red", "blue", "green", "red", "blue", "green", "red", "blue", "green", "red"), dot_categ = "grp2", plot = FALSE, safer_check = FALSE), regexp = "dot_color ARGUMENT IS INCORRECTLY ASSOCIATED TO EACH CLASS OF dot_categ", fixed = TRUE)
    # stat_pos bad option (only "top" and "above" accepted)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", stat_mean = TRUE, stat_pos = "center", plot = FALSE, safer_check = FALSE), regexp = "THE stat_pos ARGUMENT MUST BE SOME OF THESE OPTIONS", fixed = TRUE)
    # box_space > 1
    testthat::expect_error(object = saferGG::gg_box(data1 = obs2g, y = "y", categ = base::c("grp1", "grp2"), box_space = 1.5, plot = FALSE, safer_check = FALSE), regexp = "THE box_space ARGUMENT MUST BE DECIMAL VALUES BETWEEN 0 AND 1", fixed = TRUE)
    # dot_size length > 1
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = "same", dot_size = base::c(1, 3), plot = FALSE, safer_check = FALSE), regexp = "THE dot_size ARGUMENT MUST BE LENGTH 1", fixed = TRUE)
    # dot_size negative
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = "same", dot_size = -1, plot = FALSE, safer_check = FALSE), regexp = "THE dot_size ARGUMENT MUST BE MADE OF NON NEGATIVE VALUES", fixed = TRUE)
    # dot_alpha > 1
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = "same", dot_alpha = 2, plot = FALSE, safer_check = FALSE), regexp = "THE dot_alpha ARGUMENT MUST BE DECIMAL VALUES BETWEEN 0 AND 1", fixed = TRUE)
    # dot_border_size negative
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = "same", dot_border_size = -1, plot = FALSE, safer_check = FALSE), regexp = "THE dot_border_size ARGUMENT MUST BE MADE OF NON NEGATIVE VALUES", fixed = TRUE)
    # box_line_size negative
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box_line_size = -1, plot = FALSE, safer_check = FALSE), regexp = "THE box_line_size ARGUMENT MUST BE MADE OF NON NEGATIVE VALUES", fixed = TRUE)
    # categ_color factor: must be colors or column name (internal error message)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ_color = base::factor(x = base::c("A", "B", "C")), plot = FALSE, safer_check = FALSE), regexp = "categ_color ARGUMENT MUST BE A HEXADECIMAL COLOR VECTOR STARTING BY # AND/OR COLOR NAMES GIVEN BY grDevices::colors() OR A COLUMN NAME OF THE data1 PARAMETER", fixed = TRUE)
    # categ_color integer with zero value: internal error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ_color = base::c(0L, 1L), plot = FALSE, safer_check = FALSE), regexp = "categ_color ARGUMENT MUST BE A FACTOR OR CHARACTER VECTOR OR POSITVE INTEGER VECTOR", fixed = TRUE)
    # dot_color factor: internal error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = base::factor(x = base::c("A", "B", "C")), dot_categ = "grp", plot = FALSE, safer_check = FALSE), regexp = "dot_color ARGUMENT MUST BE (1) A HEXADECIMAL COLOR VECTOR STARTING BY #, OR (2) COLOR NAMES GIVEN BY grDevices::colors(), OR (3) INTEGERS, OR THE STRING", fixed = TRUE)
    # dot_color integer with zero value: internal error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = base::c(0L, 1L), dot_categ = "grp", plot = FALSE, safer_check = FALSE), regexp = "dot_color MUST BE A FACTOR OR CHARACTER VECTOR OR POSITVE INTEGER VECTOR", fixed = TRUE)
    # dot_legend_name non character
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_legend_name = 12, plot = FALSE, safer_check = FALSE), regexp = "THE dot_legend_name ARGUMENT MUST BE TYPEOF character", fixed = TRUE)
    # dot_legend_name valid: dot legend built
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = base::c("red", "blue", "green"), dot_categ = "grp", dot_legend_name = "DL", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # dot_seed valid and invalid
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = "same", dot_jitter = 0.2, dot_seed = 42L, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_seed = 1.5, plot = FALSE, safer_check = FALSE), regexp = "THE dot_seed ARGUMENT MUST BE CLASS integer AND TYPEOF integer", fixed = TRUE)
    # dot_tidy_bin_nb: zero and bad class
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = "same", dot_tidy = TRUE, dot_tidy_bin_nb = 0L, plot = FALSE, safer_check = FALSE), regexp = "dot_tidy_bin_nb ARGUMENT MUST BE A NON-NULL AND POSITVE INTEGER VALUE", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_tidy_bin_nb = 2.5, plot = FALSE, safer_check = FALSE), regexp = "THE dot_tidy_bin_nb ARGUMENT MUST BE CLASS integer AND TYPEOF integer", fixed = TRUE)
    # y_second_tick_nb / stat_pos / legend_width explicitly NULL (checked_arg_names branches)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y_second_tick_nb = NULL, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", stat_mean = TRUE, stat_pos = NULL, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", legend_width = NULL, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # dot_border_color bad color name and bad integer
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = "same", dot_border_color = "colc", plot = FALSE, safer_check = FALSE), regexp = "dot_border_color ARGUMENT MUST BE (1) A HEXADECIMAL COLOR STRING STARTING BY #, OR (2) A COLOR NAME GIVEN BY grDevices::colors(), OR (3) AN INTEGER VALUE", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = "same", dot_border_color = base::c(1L, 2L), plot = FALSE, safer_check = FALSE), regexp = "dot_border_color ARGUMENT MUST BE (1) A HEXADECIMAL COLOR STRING STARTING BY #, OR (2) A COLOR NAME GIVEN BY grDevices::colors(), OR (3) AN INTEGER VALUE", fixed = TRUE)
    # dot_categ with several columns and with a bad column
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = base::c("red", "blue", "green"), dot_categ = base::c("grp", "grp2"), plot = FALSE, safer_check = FALSE), regexp = "THE dot_categ ARGUMENT MUST BE LENGTH 1", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_categ = "BAD", plot = FALSE, safer_check = FALSE), regexp = "dot_categ ARGUMENT MUST BE COLUMN NAMES OF data1. HERE IT IS", fixed = TRUE)
    # facet_grid on a single data frame: accepted
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", add = "+ggplot2::facet_grid(~grp)", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # character categ converted to factor with box_alpha non zero
    obs_chr <- obs1
    obs_chr$grp <- base::as.character(x = obs_chr$grp)
    output <- saferGG::gg_box(data1 = obs_chr, y = "y", categ = "grp", box_alpha = 0.5, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "THE CHARACTER", fixed = TRUE))
    # categ_color with NA: warning
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ_color = base::c("red", NA, "blue"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "categ_color ARGUMENT CONTAINS NA", fixed = TRUE))
    # categ_color length equal to both nrow and class number: attribution according to levels (warn)
    obs3 <- base::data.frame(y = base::c(1, 2, 3), grp = base::factor(x = base::c("A", "B", "C")), stringsAsFactors = TRUE)
    output <- saferGG::gg_box(data1 = obs3, y = "y", categ = "grp", categ_color = base::c("red", "blue", "green"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "BOX COLORS WILL BE ATTRIBUTED ACCORDING THE LEVELS OF", fixed = TRUE))
    # categ_color with the length of data1 rows, correct association: conversion warning
    obs2g2 <- base::data.frame(y = base::c(1, 2, 3, 4), grp = base::factor(x = base::c("A", "A", "B", "B")), stringsAsFactors = TRUE)
    output <- saferGG::gg_box(data1 = obs2g2, y = "y", categ = "grp", categ_color = base::c("red", "red", "blue", "blue"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "COLORS HAVE BEEN RESPECTIVELY ASSOCIATED TO EACH CLASS OF categ", fixed = TRUE))
    # categ_color with the length of data1 rows, wrong association: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs2g2, y = "y", categ = "grp", categ_color = base::c("red", "blue", "blue", "red"), plot = FALSE, safer_check = FALSE), regexp = "BUT IS INCORRECTLY ASSOCIATED TO EACH CLASS OF categ", fixed = TRUE)
    # categ_color length 1: same color for all classes warning (box_alpha != 0)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ_color = "red", box_alpha = 0.5, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "categ_color ARGUMENT HAS LENGTH 1, MEANING THAT ALL THE DIFFERENT CLASSES", fixed = TRUE))
    # categ_color wrong length: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ_color = base::c("red", "blue", "green", "orange"), plot = FALSE, safer_check = FALSE), regexp = "categ_color ARGUMENT MUST BE (1) LENGTH 1, OR (2) THE LENGTH OF data1 NROWS", fixed = TRUE)
    # dot_categ not a column of data1 (when dot_color non NULL): error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = base::c("red", "blue", "green"), dot_categ = "BADCOL", plot = FALSE, safer_check = FALSE), regexp = "dot_categ ARGUMENT MUST BE COLUMN NAMES OF data1. HERE IT IS:", fixed = TRUE)
    # dot_categ not a column of data1 (dot_color NULL): error from the dot_categ checking block
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_categ = "BAD", plot = FALSE, safer_check = FALSE), regexp = "dot_categ ARGUMENT MUST BE COLUMN NAMES OF data1. HERE IT IS:", fixed = TRUE)
    # dot_categ equal to a categ column: renamed with _DOT and warning
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = base::c("red", "blue", "green"), dot_categ = "grp", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "HAS BEEN REPLACED BY grp_DOT", fixed = TRUE))
    # dot_categ column neither factor nor character: internal error
    obs_num_dotc <- obs1
    obs_num_dotc$numdot <- base::c(1.5, 2.5, 1.5, 2.5, 1.5, 2.5, 1.5, 2.5, 1.5, 2.5)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs_num_dotc, y = "y", categ = "grp", dot_color = "same", dot_categ = "numdot", plot = FALSE, safer_check = FALSE), regexp = "WHEN dot_color ARGUMENT IS \"same\", THE COLUMN NAME IN dot_categ ARGUMENT MUST BE IDENTICAL TO THE LAST COLUMN NAME IN categ ARGUMENT. HERE IT IS:", fixed = TRUE)
    # dot_categ_class_order: duplicated classes, mismatch, and valid reorder
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = base::c("red", "blue", "green"), dot_categ = "grp", dot_categ_class_order = base::c("A", "A"), plot = FALSE, safer_check = FALSE), regexp = "dot_categ_class_order ARGUMENT CANNOT HAVE DUPLICATED CLASSES", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = base::c("red", "blue", "green"), dot_categ = "grp", dot_categ_class_order = base::c("A", "Q"), plot = FALSE, safer_check = FALSE), regexp = "dot_categ_class_order ARGUMENT MUST BE CLASSES OF dot_categ ARGUMENT", fixed = TRUE)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = base::c("red", "blue", "green"), dot_categ = "grp", dot_categ_class_order = base::c("B", "A", "C"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # dot_color "same" with dot_categ != last categ: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = "same", dot_categ = "grp2", plot = FALSE, safer_check = FALSE), regexp = "dot_categ ARGUMENT MUST BE COLUMN NAMES OF data1. HERE IT IS:", fixed = TRUE)
    # dot_color "same" with dot_categ = last categ and different order: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = "same", dot_categ = "grp", dot_categ_class_order = base::c("B", "A", "C"), plot = FALSE, safer_check = FALSE), regexp = "LAST COMPARTMENT OF categ_class_order ARGUMENT AND dot_categ_class_order ARGUMENT CANNOT BE DIFFERENT", fixed = TRUE)
    # dot_categ present in categ with different order: warning
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ_class_order = base::list(x = base::c("A", "B", "C")), dot_color = base::c("red", "blue", "green"), dot_categ = "grp", dot_categ_class_order = base::c("B", "A", "C"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "BUT ORDER OF THE CLASSES IS NOT THE SAME", fixed = TRUE))
    # dot_color with NA: warning
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = base::c("red", NA, "blue"), dot_categ = "grp", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "dot_color ARGUMENT CONTAINS NA", fixed = TRUE))
    # dot_color length equal to nrow AND levels of dot_categ: attribution by levels warning
    output <- saferGG::gg_box(data1 = obs3, y = "y", categ = "grp", dot_color = base::c("red", "blue", "green"), dot_categ = "grp", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "DOT COLORS WILL BE ATTRIBUTED ACCORDING THE LEVELS OF", fixed = TRUE))
    # dot_color wrong length vs dot_categ: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = base::c("red", "blue", "green", "orange"), dot_categ = "grp", plot = FALSE, safer_check = FALSE), regexp = "WHEN LENGTH OF THE dot_color ARGUMENT IS MORE THAN 1, IT MUST BE EQUAL TO THE NUMBER OF 1) ROWS OR 2) LEVELS OF dot_categ COLUMN", fixed = TRUE)
    # dot_color wrongly associated to dot_categ classes: error
    obs2g3 <- base::data.frame(y = base::c(1, 2, 3, 4), grp = base::factor(x = base::c("A", "A", "B", "B")), stringsAsFactors = TRUE)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs2g3, y = "y", categ = "grp", dot_color = base::c("red", "blue", "blue", "red"), dot_categ = "grp", plot = FALSE, safer_check = FALSE), regexp = "dot_color ARGUMENT IS INCORRECTLY ASSOCIATED TO EACH CLASS OF dot_categ", fixed = TRUE)
    # dot_color correctly associated to dot_categ classes: conversion warning
    output <- saferGG::gg_box(data1 = obs2g3, y = "y", categ = "grp", dot_color = base::c("red", "red", "blue", "blue"), dot_categ = "grp", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "HAVE BEEN ATTRIBUTED TO THESE CLASSES", fixed = TRUE))
    # dot_color length 1 (not "same") without dot_categ: same color warning
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = "red", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "dot_color ARGUMENT HAS LENGTH 1, MEANING THAT ALL THE DIFFERENT CLASSES", fixed = TRUE))
    # dot_color wrong length without dot_categ: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = base::c("red", "blue", "green", "orange"), plot = FALSE, safer_check = FALSE), regexp = "dot_color ARGUMENT MUST BE (1) LENGTH 1, OR (2) THE LENGTH OF data1 NROWS", fixed = TRUE)
    # dot_categ/dot_categ_class_order/dot_legend_name without dot_color: warning (no dot plotted)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = NULL, dot_categ = "grp", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "ARGUMENT HAS BEEN SPECIFIED BUT dot_color ARGUMENT IS NULL", fixed = TRUE))
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = NULL, dot_categ_class_order = base::c("A", "B", "C"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "ARGUMENT HAS BEEN SPECIFIED BUT dot_color ARGUMENT IS NULL", fixed = TRUE))
    # dot_color NULL + box_fill FALSE + dot_alpha almost 0: warning
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = NULL, box_fill = FALSE, dot_alpha = 0.01, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "POTENTIAL OUTLIER DOTS MIGHT NOT BE VISIBLE", fixed = TRUE))
    # dot_color NULL + box_fill FALSE + dot_border_size 0: error (invisible dots)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = NULL, box_fill = FALSE, dot_border_size = 0, plot = FALSE, safer_check = FALSE), regexp = "THESE SETTINGS ARE NOT ALLOWED BECAUSE THE POTENTIAL OUTLIER DOTS WILL NOT BE VISIBLE", fixed = TRUE)
    # integer dot_border_color: converted via gg_palette
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = "same", dot_border_color = 3L, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # categ_color integer with 0: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ_color = base::c(0L, 1L, 2L), plot = FALSE, safer_check = FALSE), regexp = "categ_color ARGUMENT MUST BE A FACTOR OR CHARACTER VECTOR OR POSITVE INTEGER VECTOR", fixed = TRUE)
    # dot_color integer with 0: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = base::c(0L, 1L, 2L), plot = FALSE, safer_check = FALSE), regexp = "dot_color MUST BE A FACTOR OR CHARACTER VECTOR OR POSITVE INTEGER VECTOR", fixed = TRUE)
    # duplicated column names of data1: error
    obs_dupcol <- obs1
    obs_dupcol <- base::cbind(obs_dupcol, obs_dupcol[, "y", drop = FALSE])
    base::names(obs_dupcol)[base::length(x = base::names(x = obs_dupcol))] <- "y"
    testthat::expect_error(object = saferGG::gg_box(data1 = obs_dupcol, y = "y", categ = "grp", plot = FALSE, safer_check = FALSE), regexp = "DUPLICATED COLUMN NAMES OF data1 ARGUMENT NOT ALLOWED:", fixed = TRUE)
    # y not numeric column: error
    obs_ycar <- obs1
    obs_ycar$y <- base::as.character(x = obs_ycar$y)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs_ycar, y = "y", categ = "grp", plot = FALSE, safer_check = FALSE), regexp = "THE y COLUMN OF data1 OBJECT MUST BE MODE numeric", fixed = TRUE)
    # dot_categ with more than 1 column name: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = base::c("red", "blue", "green"), dot_categ = base::c("grp", "grp"), plot = FALSE, safer_check = FALSE), regexp = "THE dot_categ ARGUMENT MUST BE LENGTH 1", fixed = TRUE)
    # add not starting by +: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", add = "ggplot2::coord_flip()", plot = FALSE, safer_check = FALSE), regexp = "add ARGUMENT MUST START WITH \"+\":", fixed = TRUE)
    # add without ggplot2:: or lemon::: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", add = "+ theme_void() ", plot = FALSE, safer_check = FALSE), regexp = "add ARGUMENT MUST CONTAIN \"ggplot2::\" OR \"lemon::\" IN FRONT OF EACH GGPLOT2 FUNCTION", fixed = TRUE)
    # add not finishing by ): error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", add = "+ ggplot2::theme_void( ", plot = FALSE, safer_check = FALSE), regexp = "add ARGUMENT MUST FINISH BY \")\":", fixed = TRUE)
    # add containing gg_box argument names: warning
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", add = "+ ggplot2::labs(color = \"vertical\")", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "NAMES OF gg_box() ARGUMENTS DETECTED IN THE add STRING", fixed = TRUE))
    # add with facet on a column that is not in data1: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", add = "+ ggplot2::facet_grid(rows = ggplot2::vars(BAD))", plot = FALSE, safer_check = FALSE), regexp = "PROBLEM OF VARIABLE DETECTION (COLUMN NAMES OF data1)", fixed = TRUE)
    # categ column as character: conversion into factor with warning
    obs_catchar <- obs1
    obs_catchar$gchar <- base::as.character(x = obs_catchar$grp)
    output <- saferGG::gg_box(data1 = obs_catchar, y = "y", categ = "gchar", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "THE CHARACTER COLUMN HAS BEEN CONVERTED TO FACTOR", fixed = TRUE))
    # Inf in y: removed with warning
    obs_yinf <- obs1
    obs_yinf$y[5] <- Inf
    output <- saferGG::gg_box(data1 = obs_yinf, y = "y", categ = "grp", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "PRESENCE OF -Inf OR Inf VALUES IN THE y_modif COLUMN", fixed = TRUE))
    # negative y with y_log: new Inf values introduced with warning
    obs_yneg <- obs1
    obs_yneg$y[5] <- -1
    output <- saferGG::gg_box(data1 = obs_yneg, y = "y", categ = "grp", y_log = "log10", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "LOG CONVERSION INTRODUCED -Inf OR Inf OR NaN VALUES", fixed = TRUE))
    # y_lim with zero or negative values with y_log: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y_log = "log10", y_lim = base::c(-5, 20), plot = FALSE, safer_check = FALSE), regexp = "y_lim ARGUMENT CANNOT HAVE ZERO OR NEGATIVE VALUES WITH THE y_log ARGUMENT SET TO log10", fixed = TRUE)
    # y_include_zero with y_log: warning
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y_log = "log10", y_include_zero = TRUE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "y_log ARGUMENT SET TO log10 AND y_include_zero ARGUMENT SET TO TRUE", fixed = TRUE))
    # vertical = FALSE with y_log: warning and vertical reset to TRUE
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y_log = "log10", vertical = FALSE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "CANNOT FLIP BOXES HORIZONTALLY WITH A Y_LOG2 SCALE", fixed = TRUE))
    # NA categ_color: rows removed, class disappeared warnings and plot ok
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ_color = base::c("red", NA, "blue"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "categ_color ARGUMENT CONTAINS NA", fixed = TRUE))
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "THE FOLLOWING CLASSES HAVE DISAPPEARED AFTER NA/Inf REMOVAL", fixed = TRUE))
    # categ_class_order: wrong length, NULL compartment, non character compartment, duplicated classes: errors
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ_class_order = base::list(x = base::c("A", "B"), y = base::c("A", "B")), plot = FALSE, safer_check = FALSE), regexp = "categ_class_order ARGUMENT MUST BE A LIST OF LENGTH EQUAL TO LENGTH OF categ", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ_class_order = base::list(x = base::c("A", "B", "C", "D")), plot = FALSE, safer_check = FALSE), regexp = "MUST BE LENGTH 3", fixed = TRUE)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ_class_order = base::list(x = NULL), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "THE categ_class_order COMPARTMENT 1 IS NULL", fixed = TRUE))
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ_class_order = base::list(x = 1.5), plot = FALSE, safer_check = FALSE), regexp = "MUST BE MODE character", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ_class_order = base::list(x = base::c("A", "A", "A")), plot = FALSE, safer_check = FALSE), regexp = "COMPARTMENT 1 OF categ_class_order ARGUMENT CANNOT HAVE DUPLICATED CLASSES", fixed = TRUE)
    # dot_categ not a column of data1 with dot_color NULL: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = NULL, dot_categ = "BAD", plot = FALSE, safer_check = FALSE), regexp = "dot_categ ARGUMENT MUST BE COLUMN NAMES OF data1. HERE IT IS:", fixed = TRUE)
    # dot_categ not a column of data1 with dot_color non-NULL: error (message from the column names check at the same place)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = base::c("red", "blue", "green"), dot_categ = "badcol", plot = FALSE, safer_check = FALSE), regexp = "dot_categ ARGUMENT MUST BE COLUMN NAMES OF data1. HERE IT IS:", fixed = TRUE)
    # categ_color not factor/character/positive-integer (double): error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ_color = base::c(1.5, 2.5, 3.5), plot = FALSE, safer_check = FALSE), regexp = "categ_color ARGUMENT MUST BE A FACTOR OR CHARACTER VECTOR OR POSITVE INTEGER VECTOR", fixed = TRUE)
    # categ_color integer with 0: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ_color = base::c(0L, 1L, 2L), plot = FALSE, safer_check = FALSE), regexp = "categ_color ARGUMENT MUST BE A FACTOR OR CHARACTER VECTOR OR POSITVE INTEGER VECTOR", fixed = TRUE)
    # dot_color not factor/character/positive-integer (double): error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = base::c(1.5, 2.5, 3.5), plot = FALSE, safer_check = FALSE), regexp = "dot_color MUST BE A FACTOR OR CHARACTER VECTOR OR POSITVE INTEGER VECTOR", fixed = TRUE)
    # dot_color integer with 0: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = base::c(0L, 1L, 2L), plot = FALSE, safer_check = FALSE), regexp = "dot_color MUST BE A FACTOR OR CHARACTER VECTOR OR POSITVE INTEGER VECTOR", fixed = TRUE)
    # x_lab neither character nor expression: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", x_lab = 1.5, plot = FALSE, safer_check = FALSE), regexp = "x_lab ARGUMENT MUST BE A SINGLE CHARACTER STRING OR EXPRESSION", fixed = TRUE)
    # x_lab as a real expression object: ok
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", x_lab = base::expression(lab), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # y_second_tick_nb = 0: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y_second_tick_nb = 0L, plot = FALSE, safer_check = FALSE), regexp = "y_second_tick_nb ARGUMENT MUST BE A NON NULL POSITIVE INTEGER", fixed = TRUE)
    # dot_seed = NULL explicit: ok (checked_arg_names branch)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_seed = NULL, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # empty string in categ_color: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ_color = "", plot = FALSE, safer_check = FALSE), regexp = "CANNOT CONTAIN EMPTY STRING \"\"", fixed = TRUE)
    # empty string in dot_color: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = "", plot = FALSE, safer_check = FALSE), regexp = "CANNOT CONTAIN EMPTY STRING \"\"", fixed = TRUE)
    # dot_categ column name is a reserved word: renamed with warning (dot_color length must match the 2 levels)
    obs_resv <- obs1
    obs_resv$x <- base::rep(x = c("P", "Q"), times = 15)
    output <- saferGG::gg_box(data1 = obs_resv, y = "y", categ = "grp", dot_color = base::c("red", "blue"), dot_categ = "x", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "IN dot_categ ARGUMENT (COLUMN NAMES OF data1 ARGUMENT),\nx HAS BEEN REPLACED BY x_modif", fixed = TRUE))
    # reserved column name in the add string: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs_resv, y = "y", categ = "grp", add = "+ ggplot2::labs(x = \"x\")", plot = FALSE, safer_check = FALSE), regexp = "DETECTION OF COLUMN NAMES OF data1 IN THE add ARGUMENT STRING", fixed = TRUE)
    # duplicated column names of data1 with a non-reserved name: error from the second duplicated-names check
    obs_dup3 <- obs1
    obs_dup3$extra <- 1:30
    base::names(obs_dup3)[3] <- "grp"
    testthat::expect_error(object = saferGG::gg_box(data1 = obs_dup3, y = "y", categ = "grp", plot = FALSE, safer_check = FALSE), regexp = "DUPLICATED COLUMN NAMES OF data1 ARGUMENT NOT ALLOWED", fixed = TRUE)
    # duplicated column names of data1 with a reserved name: error from the reserved-word block
    obs_dup4 <- obs1
    obs_dup4$extra <- 1:30
    base::names(obs_dup4)[3] <- "dot_color"
    obs_dup4$extra2 <- 1:30
    base::names(obs_dup4)[4] <- "dot_color"
    testthat::expect_error(object = saferGG::gg_box(data1 = obs_dup4, y = "y", categ = "grp", plot = FALSE, safer_check = FALSE), regexp = "DUPLICATED COLUMN NAMES OF data1 ARGUMENT NOT ALLOWED", fixed = TRUE)
    # mandatory args block: gg_box built with the lib_path formal removed must error with the mandatory-args message
    f <- base::get(x = "gg_box", envir = base::asNamespace("saferGG"))
    fml <- base::formals(fun = f)
    fml_dropped <- fml[base::setdiff(x = base::names(x = fml), y = "lib_path")] # drop lib_path: the block must fire
    tempo_env <- base::new.env(parent = base::environment(fun = f))
    tempo_env[["gg_box"]] <- base::as.function(base::c(fml_dropped, base::list(base::body(f))), envir = tempo_env)
    res <- base::tryCatch(
        expr = base::eval(expr = base::quote(gg_box(data1 = base::data.frame(y = 1:6, grp = base::rep(x = c("A", "B"), times = 3), stringsAsFactors = TRUE), y = "y", categ = "grp", plot = FALSE, safer_check = FALSE, error_text = "")), envir = tempo_env),
        error = function(e){base::conditionMessage(e)}
    )
    testthat::expect_true(object = base::grepl(x = res, pattern = "FOLLOWING ARGUMENT IS MANDATORY IN SAFER-R FUNCTIONS:", fixed = TRUE))
    testthat::expect_true(object = base::grepl(x = res, pattern = "lib_path", fixed = TRUE))
    testthat::expect_false(object = base::grepl(x = res, pattern = "object '", fixed = TRUE))
    # no-default-value block: calling gg_box without the mandatory y argument must error with the no-default message
    res2 <- base::tryCatch(
        expr = gg_box(data1 = base::data.frame(y = 1:6, grp = base::rep(x = c("A", "B"), times = 3), stringsAsFactors = TRUE), categ = "grp", plot = FALSE, safer_check = FALSE),
        error = function(e){base::conditionMessage(e)}
    )
    testthat::expect_true(object = base::grepl(x = res2, pattern = "NO DEFAULT VALUE AND REQUIRE ONE", fixed = TRUE))
    testthat::expect_true(object = base::grepl(x = res2, pattern = ":\ny\n", fixed = TRUE))
    testthat::expect_false(object = base::grepl(x = res2, pattern = "object '", fixed = TRUE))
    # dot_categ column neither factor nor character with dot_color length 3: error
    obs_num_dotc2 <- obs1
    obs_num_dotc2$numdot <- base::rep(x = 1.5, times = 30)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs_num_dotc2, y = "y", categ = "grp", dot_color = base::c("red", "blue", "green"), dot_categ = "numdot", plot = FALSE, safer_check = FALSE), regexp = "WHEN LENGTH OF THE dot_color ARGUMENT IS MORE THAN 1, IT MUST BE EQUAL TO THE NUMBER OF 1) ROWS OR 2) LEVELS OF dot_categ COLUMN", fixed = TRUE)
    # dot_color with the length of data1 nrows: attributed to rows with warning
    obs_dotc_nrow <- obs1
    obs_dotc_nrow$dotc <- base::rep(x = c("red", "blue", "green"), times = 10)
    output <- saferGG::gg_box(data1 = obs_dotc_nrow, y = "y", categ = "grp", dot_color = obs_dotc_nrow$dotc, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # NA categ_color with a single remaining color and more than 1 class: single color replicated with warning
    obs_singlecol <- obs1
    obs_singlecol$g2 <- base::factor(x = base::c(rep("A", times = 12), rep("B", times = 10), rep("C", times = 8)), levels = base::c("A", "B", "C"))
    output <- saferGG::gg_box(data1 = obs_singlecol, y = "y", categ = "g2", categ_color = base::c(NA, "blue", "green"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # NA dot_color with a single remaining color and more than 1 dot_categ class: single color replicated with warning
    output <- saferGG::gg_box(data1 = obs_singlecol, y = "y", categ = "g2", dot_color = base::c(NA, "blue", "green"), dot_categ = "g2", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # 2 of 3 classes removed by NA categ_color -> single color replicated via categ_class_order (length > 1)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ_color = base::c(NA, "blue", NA), categ_class_order = base::list(x = base::c("A", "B", "C")), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "IN COLUMN categ_color OF data1, THE FOLLOWING CLASSES HAVE DISAPPEARED", fixed = TRUE))
    # 1 of 3 classes removed by NA dot_color (nrow-form dot_color on a distinct dot_categ column) with dot_categ_class_order: single color replicated via dot_categ_class_order (length > 1)
    obs_dotc_lev <- obs1
    obs_dotc_lev$dotc <- base::factor(x = base::rep(x = c("A", "B", "C"), times = 10))
    output <- saferGG::gg_box(data1 = obs_dotc_lev, y = "y", categ = "grp", dot_color = base::ifelse(test = obs_dotc_lev$dotc == "A", yes = "red", no = base::ifelse(test = obs_dotc_lev$dotc == "B", yes = "blue", no = NA)), dot_categ = "dotc", dot_categ_class_order = base::c("A", "B", "C"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "IN COLUMN dot_color OF data1, THE FOLLOWING CLASSES HAVE DISAPPEARED", fixed = TRUE))
    # full class of the categ column removed by y-NA removal: class-disappearance warn fires for the categ column
    obs_blk <- base::data.frame(y = stats::rnorm(n = 30, mean = 10, sd = 2), grp = base::factor(x = base::rep(x = c("A", "B", "C"), each = 10)), stringsAsFactors = FALSE)
    obs_blk$y[1:10] <- NA
    output <- saferGG::gg_box(data1 = obs_blk, y = "y", categ = "grp", y_tick_nb = 5L, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "IN COLUMN grp OF data1, THE FOLLOWING CLASSES HAVE DISAPPEARED", fixed = TRUE))
    # full class of the categ column removed by y-NA removal + distinct dot_categ column: dot_categ class order rebuilt after removal (no error, correct colors per remaining class)
    obs_blk2 <- obs_blk
    obs_blk2$dotc <- base::factor(x = base::rep(x = c("P", "Q"), each = 15))
    output <- saferGG::gg_box(data1 = obs_blk2, y = "y", categ = "grp", dot_color = base::c("red", "blue"), dot_categ = "dotc", y_tick_nb = 5L, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # full class of the categ column removed by y-NA removal + facet column: facet levels rebuilt after removal (no error, plot ok)
    obs_blk3 <- obs_blk
    obs_blk3$fac <- base::factor(x = base::rep(x = c("F1", "F2"), each = 15))
    output <- saferGG::gg_box(data1 = obs_blk3, y = "y", categ = "grp", y_tick_nb = 5L, add = "+ ggplot2::facet_wrap(~fac)", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # zero-argument call: mandatory-args block lists all three arguments (no-default + mandatory checks)
    res_zeroargs <- base::tryCatch(
        expr = saferGG::gg_box(),
        error = function(e){base::conditionMessage(e)}
    )
    testthat::expect_true(object = base::grepl(x = res_zeroargs, pattern = "NO DEFAULT VALUE AND REQUIRE ONE", fixed = TRUE))
    testthat::expect_true(object = base::grepl(x = res_zeroargs, pattern = "data1", fixed = TRUE))
    testthat::expect_true(object = base::grepl(x = res_zeroargs, pattern = "categ", fixed = TRUE))
    # empty string in a character-mode argument: error from the empty-string block (categ_color and dot_color)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ_color = "", plot = FALSE, safer_check = FALSE), regexp = "THIS ARGUMENT
categ_color
CANNOT CONTAIN EMPTY STRING", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = "", plot = FALSE, safer_check = FALSE), regexp = "THIS ARGUMENT
dot_color
CANNOT CONTAIN EMPTY STRING", fixed = TRUE)
    # duplicated column names with NO reserved word among them: second duplicated-names check fires
    obs_dup5 <- obs1
    obs_dup5$extra <- 1:30
    base::names(obs_dup5)[2] <- "extra"
    testthat::expect_error(object = saferGG::gg_box(data1 = obs_dup5, y = "y", categ = "grp", plot = FALSE, safer_check = FALSE), regexp = "DUPLICATED COLUMN NAMES OF data1 ARGUMENT NOT ALLOWED", fixed = TRUE)
# theme/legend/limits variants
output_theme1 <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box_fill = FALSE, y_tick_nb = 5L, plot = FALSE, return = TRUE, safer_check = FALSE)
testthat::expect_type(output_theme1, "list")
output_theme2 <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box_alpha = 0.5, y_tick_nb = 5L, plot = FALSE, return = TRUE, safer_check = FALSE)
testthat::expect_type(output_theme2, "list")
output_theme3 <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box_alpha = 0, y_tick_nb = 5L, plot = FALSE, return = TRUE, safer_check = FALSE)
testthat::expect_type(output_theme3, "list")
output_theme4 <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", legend_width = 0.4, y_tick_nb = 5L, plot = FALSE, return = TRUE, safer_check = FALSE)
testthat::expect_type(output_theme4, "list")
output_theme5 <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y_log = "log10", y_tick_nb = 3L, y_second_tick_nb = 2L, plot = FALSE, return = TRUE, safer_check = FALSE)
testthat::expect_type(output_theme5, "list")
# tidy dots with dot_categ (regression: the binwidth-compartment detection used to crash with an INTERNAL CODE ERROR when a second blank geom_dotplot legend layer also carried binwidth)
obs_2cat <- base::data.frame(y = stats::rnorm(n = 30, mean = 10, sd = 2), grp = base::rep(x = c("A", "B", "C"), times = 10), sub = base::rep(x = c("u", "v"), times = 15), stringsAsFactors = TRUE)
output_tidycat <- saferGG::gg_box(data1 = obs_2cat, y = "y", categ = c("grp", "sub"), dot_tidy = TRUE, dot_categ = "sub", dot_tidy_bin_nb = 20L, plot = FALSE, return = TRUE, safer_check = FALSE)
testthat::expect_type(output_tidycat, "list")
    # pass-2 removal via categ_color NA removes all rows of one dot_categ class: dot_categ factor rebuilt so the alpha scale has matching labels (regression test)
    obs_dotc_na <- obs1
    obs_dotc_na$dotc <- base::factor(x = base::rep(x = c("P", "Q"), each = 15))
    obs_dotc_na$y[1:10] <- NA
    dc <- base::c(base::rep(x = NA_character_, times = 5), base::rep(x = "blue", times = 15))
    output <- saferGG::gg_box(data1 = obs_dotc_na, y = "y", categ = "grp", dot_color = dc, dot_categ = "dotc", dot_categ_class_order = base::c("P", "Q"), y_tick_nb = 5L, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "IN COLUMN dot_color OF data1, THE FOLLOWING CLASSES HAVE DISAPPEARED", fixed = TRUE))
    # single dot_color with a 2-class dot_categ while pass-2 removes a categ class: single-color replication (1717) fires
    obs_dotc_na2 <- obs1
    obs_dotc_na2$dotc <- base::factor(x = base::rep(x = c("P", "Q"), times = 15))
    output <- saferGG::gg_box(data1 = obs_dotc_na2, y = "y", categ = "grp", categ_color = base::c(NA, "green", "green"), dot_color = "blue", dot_categ = "dotc", y_tick_nb = 5L, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # facet_grid on a distinct character column (not in categ): ok
    obs_facet2 <- obs1
    obs_facet2$fcol <- base::rep(x = c("F2", "F1", "F3"), times = 10)
    output <- saferGG::gg_box(data1 = obs_facet2, y = "y", categ = "grp", add = "+ ggplot2::facet_grid(rows = ggplot2::vars(fcol))", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # facet_wrap on a distinct character column: ok
    output <- saferGG::gg_box(data1 = obs_facet2, y = "y", categ = "grp", add = "+ ggplot2::facet_wrap(facets = ggplot2::vars(fcol))", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # facet_grid on a column also used in categ: ok (no duplicated facet column)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", add = "+ ggplot2::facet_grid(rows = ggplot2::vars(grp))", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # box_alpha = 0: legend removed and ok
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box_alpha = 0, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # stat_pos = "above" with dot_tidy = TRUE: ok
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_tidy = TRUE, stat_pos = "above", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # stat_pos = "above" with dot_color NULL: ok (stat on box whiskers)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = NULL, stat_pos = "above", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # stat_pos = "above" with stat_mean = TRUE: ok (MEAN as text center reference)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = NULL, stat_pos = "above", stat_mean = TRUE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # stat_pos = "above" with y_log: ok (text format conversion)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = NULL, stat_pos = "above", y_log = "log10", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # stat_pos = "BAD": error (stat_pos must be top or above)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", stat_pos = "BAD", plot = FALSE, safer_check = FALSE), regexp = "THE stat_pos ARGUMENT MUST BE SOME OF THESE OPTIONS", fixed = TRUE)
    # legend_show = FALSE: ok (legend inactivated)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", legend_show = FALSE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # y_tick_nb = 1 and y_second_tick_nb = 1: error (not enough ticks generated)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y_tick_nb = 1, y_second_tick_nb = 1, plot = FALSE, safer_check = FALSE), regexp = "THE NUMBER OF GENERATED TICKS FOR THE Y-AXIS IS NOT CORRECT", fixed = TRUE)
    # y_second_tick_nb = 3 with y_log = "no": ok (secondary ticks generated)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y_second_tick_nb = 3, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # grid = TRUE with vertical = FALSE: ok (other theme branch)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", grid = TRUE, vertical = FALSE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # article = FALSE with vertical = FALSE: ok (other theme branch)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", article = FALSE, vertical = FALSE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # add with ggplot2::theme: internal themes inactivated with warning
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", add = "+ ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = \"red\"))", article = FALSE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "\"ggplot2::theme\" STRING DETECTED IN THE add ARGUMENT", fixed = TRUE))
    # plot = TRUE with legend_width = NULL: ok (grid.arrange without legend)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", plot = TRUE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # both NA/Inf removal passes fire: y NA rows removed (pass 1) AND NA in dot_color nlevels form (pass 2): no rbind error
    obs_dotna <- obs1
    obs_dotna$y[21:22] <- NA
    output <- saferGG::gg_box(data1 = obs_dotna, y = "y", categ = "grp", dot_color = base::c("red", NA, "blue"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_identical(object = base::ncol(output$removed_rows), expected = base::ncol(obs1)) # removed_rows keeps the data1_ini column set
    testthat::expect_identical(object = base::nrow(output$removed_rows), expected = 12L) # 2 rows (y NA) + 10 rows (dot_color NA in class B)
    # same with Inf in y (pass 1 at the Inf removal step) AND NA in dot_color (pass 2): class-disappearance warn for dot_color
    obs_dotna2 <- obs1
    obs_dotna2$y[1:10] <- Inf
    output <- saferGG::gg_box(data1 = obs_dotna2, y = "y", categ = "grp", dot_color = base::c("red", NA, "blue"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "IN COLUMN dot_color OF data1, THE FOLLOWING CLASSES HAVE DISAPPEARED", fixed = TRUE))
    # same with categ_color instead of dot_color: class-disappearance warn for categ_color
    obs_ccna <- obs1
    obs_ccna$y[11:14] <- NA
    output <- saferGG::gg_box(data1 = obs_ccna, y = "y", categ = "grp", dot_color = NULL, categ_color = base::c("red", NA, "blue"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "IN COLUMN categ_color OF data1, THE FOLLOWING CLASSES HAVE DISAPPEARED", fixed = TRUE))
    ## end errors
    # duplicated non-reserved column names of data1: error
    obs_dup <- base::data.frame(y = 1:10, g = base::rep(c("A", "B"), 5), h = 1:10, stringsAsFactors = TRUE)
    base::names(obs_dup)[3] <- "g"
    testthat::expect_error(object = saferGG::gg_box(data1 = obs_dup, y = "y", categ = "g", plot = FALSE, safer_check = FALSE), regexp = "DUPLICATED COLUMN NAMES OF data1 ARGUMENT NOT ALLOWED", fixed = TRUE)
    # valid y_lim with y_log: y_lim log-transformed without error
    output_ylimlog <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y_lim = base::c(1, 20), y_log = "log10", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_ylimlog, "list")
    # y_include_zero = TRUE: y_lim includes 0
    output_yzero <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y_include_zero = TRUE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_yzero, "list")
    # dot_tidy = TRUE with dot_categ equal to categ[1]: tidy_group_coord from dot_coord$group
    output_dc <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_tidy = TRUE, dot_categ = "grp", dot_categ_class_order = base::c("A", "B", "C"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_dc, "list")
    # dot_tidy = TRUE with dot_categ = categ but dot_color NULL: tidy_group_coord = dot_coord$group branch
    output_dc2 <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_color = NULL, dot_categ = "grp", dot_tidy = TRUE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_dc2, "list")
    # two categ columns: 2-categ group creation path
    obs_2c <- base::data.frame(y = 1:12, g1 = base::rep(c("A", "B"), 6), g2 = base::rep(c("X", "Y", "Z"), 4), stringsAsFactors = TRUE)
    output_2c <- saferGG::gg_box(data1 = obs_2c, y = "y", categ = base::c("g1", "g2"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_2c, "list")
    # two categ + notch: notch polygon path
    output_2cn <- saferGG::gg_box(data1 = obs_2c, y = "y", categ = base::c("g1", "g2"), box_notch = TRUE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_2cn, "list")
    # two categ + notch + facet on a distinct column: facet column appended to tempo_polygon
    obs_2cf <- base::data.frame(y = 1:12, g1 = base::rep(c("A", "B"), 6), g2 = base::rep(c("X", "Y", "Z"), 4), f = base::rep(c("F1", "F2"), 6), stringsAsFactors = TRUE)
    output_2cf <- saferGG::gg_box(data1 = obs_2cf, y = "y", categ = base::c("g1", "g2"), box_notch = TRUE, add = "+ggplot2::facet_wrap(~f)", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_2cf, "list")
    # stat_mean = TRUE: mean display
    output_mean <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", stat_mean = TRUE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_mean, "list")
    # Windows transparency warn (windows device + plot + legend_width + alpha < 1)
    if(Sys.info()[["sysname"]] == "Windows"){
        grDevices::windows(width = 7, height = 7)
        output_win <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_alpha = 0.5, legend_width = 0.2, plot = TRUE, return = TRUE, safer_check = FALSE)
        testthat::expect_true(object = base::grepl(x = output_win$warn, pattern = "TRANSPARENCY OF THE LINES IS INACTIVATED IN THE LEGEND", fixed = TRUE))
        grDevices::dev.off()
    }
    # plot = TRUE with legend_width = NULL: grid.arrange single plot branch
    if(Sys.info()[["sysname"]] == "Windows"){
        grDevices::windows(width = 7, height = 7)
        output_print <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", legend_width = NULL, plot = TRUE, return = TRUE, return_gtable = TRUE, safer_check = FALSE)
        testthat::expect_type(object = output_print, "list")
        testthat::expect_s3_class(object = output_print$gtable, class = "gtable")
        grDevices::dev.off()
    }
    # duplicated NON-reserved column names of data1: the second duplicated-names check (after name_change)
    obs_dup_nr <- obs1
    base::names(obs_dup_nr)[1] <- "yv"
    obs_dup_nr <- base::cbind(obs_dup_nr, obs1$y)
    obs_dup_nr <- base::cbind(obs_dup_nr, obs1$y)
    base::names(obs_dup_nr)[3] <- "zz"
    base::names(obs_dup_nr)[4] <- "zz"
    testthat::expect_error(object = saferGG::gg_box(data1 = obs_dup_nr, y = "yv", categ = "grp", plot = FALSE, safer_check = FALSE), regexp = "DUPLICATED COLUMN NAMES OF data1 ARGUMENT NOT ALLOWED", fixed = TRUE)
    # all-Inf y column with a finite y_lim: 0 remaining rows -> data.frame(dot_color) errors first ("differing number of rows", before the all-Inf y_lim guard)
    obs_allinf <- base::data.frame(y = base::c(base::rep(x = Inf, times = 6), base::rep(x = -Inf, times = 6)), grp = base::rep(c("A", "B"), 6), stringsAsFactors = TRUE)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs_allinf, y = "y", categ = "grp", y_lim = base::c(0, 10), plot = FALSE, safer_check = FALSE), regexp = "differing number of rows: 0, 1", fixed = TRUE)
    # single-class categ, 1-categ data: length(categ_color) == 1L branches of the per-box color/fill scales
    obs_sc <- base::data.frame(y = 1:6, grp = base::rep(x = "A", times = 6), stringsAsFactors = TRUE)
    output_sc <- saferGG::gg_box(data1 = obs_sc, y = "y", categ = "grp", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_sc, "list")
    # single-class categ with box_fill = TRUE: length(categ_color) == 1L branch of the final fill scale
    output_sc_fill <- saferGG::gg_box(data1 = obs_sc, y = "y", categ = "grp", box_fill = TRUE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_sc_fill, "list")
    # 2-categ data with a single class in categ[2]: length(categ_color) == 1L branches of the 2-categ per-box scales
    obs_2c1 <- base::data.frame(y = 1:6, g1 = base::rep(c("A", "B"), 3), g2 = base::rep("X", 6), stringsAsFactors = TRUE)
    output_2c1 <- saferGG::gg_box(data1 = obs_2c1, y = "y", categ = base::c("g1", "g2"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_2c1, "list")
    # box_whisker_kind = "no": coef 0 branch of the recovery boxplots (1-categ and 2-categ)
    output_wno <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box_whisker_kind = "no", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_wno, "list")
    output_wno2 <- saferGG::gg_box(data1 = obs_2c, y = "y", categ = base::c("g1", "g2"), box_whisker_kind = "no", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_wno2, "list")
    # box_whisker_kind = "max": coef Inf branch of the recovery boxplots (1-categ and 2-categ)
    output_wmax <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box_whisker_kind = "max", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_wmax, "list")
    output_wmax2 <- saferGG::gg_box(data1 = obs_2c, y = "y", categ = base::c("g1", "g2"), box_whisker_kind = "max", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_wmax2, "list")
    # 2-categ data with dot_tidy = TRUE and default dot_categ (NULL): tidy_group paste branch
    output_tidy2c <- saferGG::gg_box(data1 = obs_2c, y = "y", categ = base::c("g1", "g2"), dot_tidy = TRUE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_tidy2c, "list")
    # single row of data with dots: runif branch of the single-dot jitter
    obs_1r <- base::data.frame(y = 5, grp = "A", stringsAsFactors = TRUE)
    output_1r <- saferGG::gg_box(data1 = obs_1r, y = "y", categ = "grp", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_1r, "list")
    # box_notch = TRUE with box_fill = TRUE: X_NOTCH_INF/X_NOTCH_SUP polygon vertices (fill path)
    output_notchfill <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box_notch = TRUE, box_fill = TRUE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_notchfill, "list")
    # dot_tidy = TRUE with dot_categ outside categ, dot_color vector, non-NULL dot_border_color: rep(dot_border_color, nrow) path and ini.dot_categ legend title path
    obs_dc <- obs1
    obs_dc$dc <- base::rep(x = c("P", "Q"), times = 15)
    output_dc3 <- saferGG::gg_box(data1 = obs_dc, y = "y", categ = "grp", dot_tidy = TRUE, dot_color = base::c("red", "green"), dot_categ = "dc", dot_border_color = "black", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_dc3, "list")
    # 2-categ data with stat_pos = "above": above branch of the 2-categ aggregate
    output_2cab <- saferGG::gg_box(data1 = obs_2c, y = "y", categ = base::c("g1", "g2"), stat_pos = "above", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_2cab, "list")
    # grid = TRUE with default vertical = TRUE: panel.grid.major.y / minor.y element_line and axis.text branches
    output_gv <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", grid = TRUE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_gv, "list")
    # article = FALSE with default vertical = TRUE: axis.text element_text and NULL branches
    output_afv <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", article = FALSE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_afv, "list")
    # y_lab provided: used as y-axis label
    output_ylab <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y_lab = "MY Y", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_ylab, "list")
    # return_ggplot = TRUE: ggplot object in output
    output_ggp <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", return_ggplot = TRUE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_ggp, "list")
    # return_gtable = FALSE: NULL gtable component
    output_nogtable <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", return_gtable = FALSE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_nogtable, "list")
    testthat::expect_null(object = output_nogtable$gtable)
    # cleanup of all files and devices created during the tests
    while(base::length(x = grDevices::dev.list()) > 0){ grDevices::dev.off() }
    base::unlink(x = tempo_dir, recursive = TRUE, force = TRUE)
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})

testthat::test_that("gg_box regressions of saferGG 1.0.0 (warn_print, add facets, comp_2d field names)", {
    base::set.seed(1)
    obs_reg <- base::data.frame(y = stats::rnorm(n = 40, mean = 0, sd = 1), g = base::factor(x = base::rep(x = base::c("a", "b"), times = 20)), h = base::factor(x = base::rep(x = base::c("x", "y"), each = 20)), stringsAsFactors = TRUE)
    base::set.seed(NULL)
    # bug 1: warn_print = FALSE must not print the warnings, but warn must still be returned
    testthat::expect_no_warning(object = output_wp <- saferGG::gg_box(data1 = obs_reg, y = "y", categ = "g", dot_color = "same", warn_print = FALSE, plot = FALSE, return = TRUE, safer_check = FALSE))
    testthat::expect_true(object = base::grepl(pattern = "dot_color ARGUMENT HAS BEEN SET TO \"same\"", x = output_wp$warn, fixed = TRUE))
    testthat::expect_warning(object = saferGG::gg_box(data1 = obs_reg, y = "y", categ = "g", dot_color = "same", warn_print = TRUE, plot = FALSE, return = TRUE, safer_check = FALSE), regexp = "dot_color ARGUMENT HAS BEEN SET TO \"same\"", fixed = TRUE)
    # bug 2: a non-NULL add (facets) must not fail with "object 'arg.names' not found"
    output_add <- saferGG::gg_box(data1 = obs_reg, y = "y", categ = "g", add = "+ggplot2::facet_wrap(~h)", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_add, "list")
    # bug 3: comp_2d() field used by gg_box must exist in the saferTool output (identical_content, not identical.content)
    tempo_comp <- saferTool::comp_2d(data1 = obs_reg["g"], data2 = obs_reg["g"], safer_check = FALSE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = "identical_content" %in% base::names(x = tempo_comp))
    testthat::expect_true(object = tempo_comp$identical_content)
    # bug 3: dots drawn (dot_categ set) must not fail with "invalid argument type"
    output_dot <- saferGG::gg_box(data1 = obs_reg, y = "y", categ = "g", dot_categ = "h", plot = FALSE, return = TRUE, return_ggplot = TRUE, return_gtable = FALSE, safer_check = FALSE)
    testthat::expect_s3_class(object = output_dot$ggplot, class = "ggplot")
    output_dot_tidy <- saferGG::gg_box(data1 = obs_reg, y = "y", categ = "g", dot_categ = "h", dot_tidy = TRUE, plot = FALSE, return = TRUE, return_ggplot = FALSE, return_gtable = FALSE, safer_check = FALSE)
    testthat::expect_type(object = output_dot_tidy, "list")
    while(base::length(x = grDevices::dev.list()) > 0){ grDevices::dev.off() }
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})

testthat::test_that("gg_box warn_print = FALSE also silences the saferGraph::inter_ticks() warnings", {
    obs_tick <- base::data.frame(y = base::c(2.2, 2.3, 2.5, 2.8, 2.7, 2.4), g = base::factor(x = base::rep(x = base::c("a", "b"), times = 3)), stringsAsFactors = TRUE)
    # y_lim inside one log10 tick interval: inter_ticks() returns the "NO INTER TICKS COMPUTED" warning
    testthat::expect_no_warning(object = output_tick <- saferGG::gg_box(data1 = obs_tick, y = "y", categ = "g", y_lim = base::c(2.1, 2.9), y_log = "log10", y_top_extra_margin = 0, y_bottom_extra_margin = 0, warn_print = FALSE, plot = FALSE, return = TRUE, safer_check = FALSE))
    testthat::expect_true(object = base::grepl(pattern = "FROM saferGraph::inter_ticks():\n(1) NO INTER TICKS COMPUTED", x = output_tick$warn, fixed = TRUE))
    testthat::expect_warning(object = saferGG::gg_box(data1 = obs_tick, y = "y", categ = "g", y_lim = base::c(2.1, 2.9), y_log = "log10", y_top_extra_margin = 0, y_bottom_extra_margin = 0, warn_print = TRUE, plot = FALSE, return = TRUE, safer_check = FALSE), regexp = "NO INTER TICKS COMPUTED", fixed = TRUE)
    while(base::length(x = grDevices::dev.list()) > 0){ grDevices::dev.off() }
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
