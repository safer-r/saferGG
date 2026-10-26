# tests for the gg_scatter() function of the saferGG package

testthat::test_that("gg_scatter draws scatter plots and returns the expected structure", {

    ## data argument values
    base::set.seed(seed = 1, kind = NULL, normal.kind = NULL, sample.kind = NULL) # to make the test data deterministic
    obs1 <- base::data.frame(x = 1:10, y = (1:10)^2, grp = base::factor(x = base::rep(x = c("A", "B"), times = 5)), stringsAsFactors = TRUE)
    obs2 <- base::data.frame(x = 1:5, y = base::c(2, 4, 6, 8, 10), stringsAsFactors = TRUE)
    ## end data argument values

    ## initialization of tests
    tempo_dir <- base::tempdir()
    tempo_file <- base::file.path(tempo_dir, "test_gg_scatter.pdf")
    grDevices::pdf(file = tempo_file)
    ## end initialization of tests

    ## tests
    # basic dots, return structure
    output <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", categ = "grp", return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::all(base::c("data", "removed_row_nb", "removed_rows", "plot", "panel", "axes", "warn", "ggplot", "gtable") %in% base::names(x = output)))
    # plot drawn with color
    testthat::expect_no_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", categ = "grp", color = base::c("red", "blue"), legend_name = "GROUP", safer_check = FALSE))
    # geom_line
    testthat::expect_no_error(object = saferGG::gg_scatter(data1 = obs2, x = "x", y = "y", geom = "geom_line", safer_check = FALSE))
    # facet_wrap via add
    testthat::expect_no_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", add = "+ggplot2::facet_wrap(~grp)", safer_check = FALSE))
    # theme in add
    testthat::expect_no_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", add = "+ggplot2::theme(title = ggplot2::element_text(color = \"red\"))", safer_check = FALSE))
    # log scale
    testthat::expect_no_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", x_log = "log10", safer_check = FALSE))
    # geom_step with direction
    testthat::expect_no_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", geom = "geom_step", geom_step_dir = "hv", safer_check = FALSE))
    # geom_stick
    testthat::expect_no_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", geom = "geom_stick", safer_check = FALSE))
    # geom_hline with NULL x (fake column warning)
    output <- saferGG::gg_scatter(data1 = obs1[, "y", drop = FALSE], x = NULL, y = "y", geom = "geom_hline", y_lim = base::c(1, 100), x_lim = base::c(1, 10), return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "FAKE COLUMN", fixed = TRUE))
    # geom_vline with NULL y (fake column warning)
    output <- saferGG::gg_scatter(data1 = obs1[, "x", drop = FALSE], x = "x", y = NULL, geom = "geom_vline", x_lim = base::c(1, 10), y_lim = base::c(1, 100), return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "FAKE COLUMN", fixed = TRUE))
    # legend_width path (grid.arrange with legend)
    testthat::expect_no_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", categ = "grp", color = base::c("red", "blue"), legend_width = 0.5, safer_check = FALSE))
    # raster plot via raster_threshold (requires Cairo at draw time)
    if(base::requireNamespace("Cairo", quietly = TRUE)){
        output <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", categ = "grp", color = base::c("red", "blue"), raster = TRUE, plot = FALSE, return = TRUE, return_ggplot = FALSE, return_gtable = FALSE, safer_check = FALSE)
        testthat::expect_type(output, "list")
        testthat::expect_true(object = base::grepl(x = output$warn, pattern = "RASTER PLOT GENERATED", fixed = TRUE))
    }else{
        testthat::skip(message = "Cairo package not available for raster drawing")
    }
    # multiple data frames (list input)
    output <- saferGG::gg_scatter(data1 = base::list(OBS1 = obs1, OBS2 = obs2), x = base::list("x", "x"), y = base::list("y", "y"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # multiple data frames with per-compartment categ, color, legend_name, categ_class_order
    output <- saferGG::gg_scatter(
        data1 = base::list(OBS1 = obs1, OBS2 = obs2),
        x = base::list("x", "x"),
        y = base::list("y", "y"),
        categ = base::list("grp", NULL),
        color = base::list(base::c("red", "blue"), "green"),
        legend_name = base::list("L1", NULL),
        categ_class_order = base::list(base::c("A", "B"), NULL),
        plot = FALSE,
        return = TRUE,
        safer_check = FALSE
    )
    testthat::expect_type(output, "list")
    # single-value -> list conversion (geom, alpha, dot_size, line_size, line_type, dot_shape, color, dot_border_color, geom_step_dir, geom_stick_base)
    output <- saferGG::gg_scatter(
        data1 = base::list(OBS1 = obs1, OBS2 = obs2),
        x = base::list("x", "x"),
        y = base::list("y", "y"),
        geom = "geom_line",
        alpha = 0.5,
        dot_size = 2,
        line_size = 1.5,
        line_type = 2,
        dot_shape = 16,
        color = "red",
        dot_border_color = "blue",
        geom_step_dir = "vh",
        geom_stick_base = 5,
        plot = FALSE,
        return = TRUE,
        safer_check = FALSE
    )
    testthat::expect_type(output, "list")
    # unnamed data1 list: names attributed with a warning
    output <- saferGG::gg_scatter(data1 = base::list(obs1, obs2), x = base::list("x", "x"), y = base::list("y", "y"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "NULL NAME COMPARTMENT OF data1 LIST", fixed = TRUE))
    # single character legend_name with a single data frame
    output <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", categ = "grp", legend_name = "LN", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # x with Inf values: warn + removal
    obs_inf_x <- obs1
    obs_inf_x$x[1] <- Inf
    output <- saferGG::gg_scatter(data1 = obs_inf_x, x = "x", y = "y", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "PRESENCE OF -Inf OR Inf VALUES IN ELEMENT 1 OF x ARGUMENT", fixed = TRUE))
    # y with -Inf values: warn + removal
    obs_inf_y <- obs1
    obs_inf_y$y[1] <- -Inf
    output <- saferGG::gg_scatter(data1 = obs_inf_y, x = "x", y = "y", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "PRESENCE OF -Inf OR Inf VALUES IN ELEMENT 1 OF y ARGUMENT", fixed = TRUE))
    # x_log with x_include_zero: warn + reset
    output <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", x_log = "log10", x_include_zero = TRUE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "x_include_zero ARGUMENT RESET TO FALSE", fixed = TRUE))
    # y_log with y_include_zero: warn + reset
    output <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", y_log = "log10", y_include_zero = TRUE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "y_include_zero ARGUMENT RESET TO FALSE", fixed = TRUE))
    # geom_hline with x NULL: fake column + removal warnings (return path)
    output <- saferGG::gg_scatter(data1 = obs1[, "y", drop = FALSE], x = NULL, y = "y", geom = "geom_hline", x_lim = base::c(1, 10), y_lim = base::c(1, 100), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # geom_vline without x_lim: works when y is NULL (no axis needed)
    output <- saferGG::gg_scatter(data1 = obs1[, "x", drop = FALSE], x = "x", y = NULL, geom = "geom_vline", y_lim = base::c(1, 100), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # geom_hline without y_lim
    output <- saferGG::gg_scatter(data1 = obs1[, "y", drop = FALSE], x = NULL, y = "y", geom = "geom_hline", x_lim = base::c(1, 10), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # mixed geom_hline + geom_vline in a list input
    output <- saferGG::gg_scatter(
        data1 = base::list(H = obs1[, "y", drop = FALSE], V = obs1[, "x", drop = FALSE]),
        x = base::list(NULL, "x"),
        y = base::list("y", NULL),
        geom = base::list("geom_hline", "geom_vline"),
        x_lim = base::c(1, 10),
        y_lim = base::c(1, 100),
        plot = FALSE,
        return = TRUE,
        safer_check = FALSE
    )
    testthat::expect_type(output, "list")
    # color as an unknown column-name-like string: no column-name path exists, so the hex/color-name check fires
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", color = "colc", plot = FALSE, safer_check = FALSE), regexp = "color MUST BE A HEXADECIMAL COLOR VECTOR", fixed = TRUE)
    # color NULL compartment in a list: grey attributed with a warning
    output <- saferGG::gg_scatter(data1 = base::list(OBS1 = obs1, OBS2 = obs2), x = base::list("x", "x"), y = base::list("y", "y"), color = base::list(NULL, "green"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "NULL COLOR IN ", fixed = TRUE))
    # color as integers: converted to colors via gg_palette
    output <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", categ = "grp", color = base::c(1, 2), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # categ_class_order with a NULL compartment in a list
    output <- saferGG::gg_scatter(
        data1 = base::list(OBS1 = obs1, OBS2 = obs2),
        x = base::list("x", "x"),
        y = base::list("y", "y"),
        categ = base::list("grp", NULL),
        categ_class_order = base::list(base::c("B", "A"), NULL),
        plot = FALSE,
        return = TRUE,
        safer_check = FALSE
    )
    testthat::expect_type(output, "list")
    # geom_stick_base with NULL compartment (geom_stick without base: default used -> bottom y-axis limit as base; ifelse() bug check)
    output <- saferGG::gg_scatter(
        data1 = base::list(OBS1 = obs1, OBS2 = obs2),
        x = base::list("x", "x"),
        y = base::list("y", "y"),
        geom = base::list("geom_stick", "geom_stick"),
        geom_stick_base = base::list(5, NULL),
        plot = FALSE,
        return = TRUE,
        safer_check = FALSE
    )
    testthat::expect_type(output, "list")
    # geom_stick_base entirely NULL with geom_stick: y-axis bottom limit used as base
    output <- saferGG::gg_scatter(
        data1 = base::list(OBS1 = obs1, OBS2 = obs2),
        x = base::list("x", "x"),
        y = base::list("y", "y"),
        geom = base::list("geom_stick", "geom_stick"),
        plot = FALSE,
        return = TRUE,
        safer_check = FALSE
    )
    testthat::expect_type(output, "list")
    # per-compartment single-value args in list form (alpha, dot_size, dot_shape, dot_border_size, line_size, line_type, geom_step_dir, dot_border_color)
    output <- saferGG::gg_scatter(
        data1 = base::list(OBS1 = obs1, OBS2 = obs2),
        x = base::list("x", "x"),
        y = base::list("y", "y"),
        geom = base::list("geom_step", "geom_line"),
        geom_step_dir = base::list("vh", "hv"),
        alpha = base::list(0.5, 0.8),
        dot_size = base::list(2, 4),
        dot_shape = base::list(16, 17),
        dot_border_size = base::list(1, 2),
        line_size = base::list(1, 2),
        line_type = base::list(2, 3),
        dot_border_color = base::list("red", NULL),
        plot = FALSE,
        return = TRUE,
        safer_check = FALSE
    )
    testthat::expect_type(output, "list")
    ## end tests

    ## errors
    # bad x column
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "BAD", y = "y", safer_check = FALSE), regexp = "MUST BE A COLUMN NAME", fixed = TRUE)
    # bad geom
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", geom = "geom_bad", safer_check = FALSE), regexp = "MUST BE SOME OF THESE OPTIONS", fixed = TRUE)
    # data1 list with more than 6 compartments
    testthat::expect_error(object = saferGG::gg_scatter(data1 = base::lapply(X = 1:7, FUN = function(i){obs1}), x = base::lapply(X = 1:7, FUN = function(i){"x"}), y = base::lapply(X = 1:7, FUN = function(i){"y"}), safer_check = FALSE), regexp = "data1 ARGUMENT MUST BE A LIST OF 6 DATA FRAMES MAXIMUM", fixed = TRUE)
    # x not a list while data1 is a list
    testthat::expect_error(object = saferGG::gg_scatter(data1 = base::list(OBS1 = obs1, OBS2 = obs2), x = "x", y = base::list("y", "y"), safer_check = FALSE), regexp = "x ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1", fixed = TRUE)
    # x list with wrong length vs data1
    testthat::expect_error(object = saferGG::gg_scatter(data1 = base::list(OBS1 = obs1, OBS2 = obs2), x = base::list("x", "x", "x"), y = base::list("y", "y"), safer_check = FALSE), regexp = "MUST BE A LIST OF SAME LENGTH AS data1", fixed = FALSE)
    # add not finished by )
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", add = "+ggplot2::coord_flip(", safer_check = FALSE), regexp = "add ARGUMENT MUST FINISH BY", fixed = TRUE)
    # add without ggplot2::/lemon:: prefix
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", add = "+coord_flip()", safer_check = FALSE), regexp = "add ARGUMENT MUST CONTAIN", fixed = TRUE)
    # add with facet_wrap but bad facet variable
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", add = "+ggplot2::facet_wrap(~BADFACET)", safer_check = FALSE), regexp = "PROBLEM OF VARIABLE DETECTION", fixed = TRUE)
    # x_lim made of Inf/-Inf only (not with vline-only geoms)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", x_lim = base::c(Inf, -Inf), safer_check = FALSE), regexp = "x_lim ARGUMENT MADE OF NA, -Inf OR Inf ONLY", fixed = TRUE)
    # x_log with non-positive x_lim
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", x_log = "log10", x_lim = base::c(-1, 100), safer_check = FALSE), regexp = "x_lim ARGUMENT CANNOT HAVE ZERO OR NEGATIVE VALUES WITH THE x_log ARGUMENT", fixed = TRUE)
    # y_log with non-positive y_lim
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", y_log = "log10", y_lim = base::c(1, -5), safer_check = FALSE), regexp = "y_lim ARGUMENT CANNOT HAVE ZERO OR NEGATIVE VALUES WITH THE y_log ARGUMENT", fixed = TRUE)
    # color with wrong length vs categ
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", categ = "grp", color = base::c("red", "blue", "green"), safer_check = FALSE), regexp = "color MUST BE\n(1) LENGTH 1", fixed = TRUE)
    # color with bad color names
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", categ = "grp", color = base::c("red", "BADCOL"), safer_check = FALSE), regexp = "color MUST BE A HEXADECIMAL COLOR VECTOR", fixed = TRUE)
    # color as a wrong column name
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", color = "BADCOLNAME", safer_check = FALSE), regexp = "color MUST BE A HEXADECIMAL COLOR VECTOR", fixed = TRUE)
    # alpha out of proportion
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", alpha = 2, safer_check = FALSE), regexp = "DECIMAL VALUES BETWEEN 0 AND 1", fixed = FALSE)
    # add without +
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", add = "ggplot2::coord_flip()", safer_check = FALSE), regexp = "MUST START WITH", fixed = TRUE)
    # x must be NULL with geom_hline
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", geom = "geom_hline", safer_check = FALSE), regexp = "MUST BE NULL IF", fixed = TRUE)
    # y must be NULL with geom_vline
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", geom = "geom_vline", safer_check = FALSE), regexp = "MUST BE NULL IF", fixed = TRUE)
    # x as non character (numeric)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = 12, y = "y", plot = FALSE, safer_check = FALSE), regexp = "THE x ARGUMENT MUST BE TYPEOF character", fixed = TRUE)
    # y as non character (numeric)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = 12, plot = FALSE, safer_check = FALSE), regexp = "THE y ARGUMENT MUST BE TYPEOF character", fixed = TRUE)
    # x column not numeric (factor): dedicated factor check (factors have mode "numeric" but cannot be plotted)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "grp", y = "y", plot = FALSE, safer_check = FALSE), regexp = "x ARGUMENT (AS COLUMN NAME OF data1 DATA FRAME) CANNOT BE A FACTOR BUT MUST BE MADE OF NUMBERS", fixed = TRUE)
    # y column not numeric (factor)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "grp", plot = FALSE, safer_check = FALSE), regexp = "y ARGUMENT (AS COLUMN NAME OF data1 DATA FRAME) CANNOT BE A FACTOR BUT MUST BE MADE OF NUMBERS", fixed = TRUE)
    # x NULL with non hline geom
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = NULL, y = "y", plot = FALSE, safer_check = FALSE), regexp = "x ARGUMENT CANNOT BE NULL EXCEPT IF x ARGUMENT IS \"geom_hline\"", fixed = TRUE)
    # y NULL with non vline geom
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = NULL, plot = FALSE, safer_check = FALSE), regexp = "y ARGUMENT CANNOT BE NULL EXCEPT IF y ARGUMENT IS \"geom_vline\"", fixed = TRUE)
    # x NULL with geom_hline in list form: fake_x warn + OK
    output <- saferGG::gg_scatter(data1 = base::list(H = obs1[, "y", drop = FALSE]), x = base::list(NULL), y = base::list("y"), geom = base::list("geom_hline"), x_lim = base::c(1, 10), y_lim = base::c(1, 100), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "FAKE COLUMN ADDED", fixed = TRUE))
    # y NULL with geom_vline in list form: fake_y warn + OK
    output <- saferGG::gg_scatter(data1 = base::list(V = obs1[, "x", drop = FALSE]), x = base::list("x"), y = base::list(NULL), geom = base::list("geom_vline"), x_lim = base::c(1, 10), y_lim = base::c(1, 100), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # geom_hline with full data1: x NULL with both limits set (hline conversion path)
    output_hline <- saferGG::gg_scatter(data1 = obs1, x = NULL, y = "y", geom = "geom_hline", x_lim = base::c(0, 20), y_lim = base::c(0, 20), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output_hline, "list")
    # geom_vline with full data1: y NULL with both limits set (vline conversion path)
    output_vline <- saferGG::gg_scatter(data1 = obs1, x = "x", y = NULL, geom = "geom_vline", x_lim = base::c(0, 20), y_lim = base::c(0, 20), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output_vline, "list")
    # categ bad column name in list form
    testthat::expect_error(object = saferGG::gg_scatter(data1 = base::list(OBS1 = obs1, OBS2 = obs2), x = base::list("x", "x"), y = base::list("y", "y"), categ = base::list("BAD", NULL), plot = FALSE, safer_check = FALSE), regexp = "MUST BE A COLUMN NAME OF DATA FRAME NUMBER 1 OF data1 ARGUMENT", fixed = TRUE)
    # categ column not factor/character
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", categ = "x", plot = FALSE, safer_check = FALSE), regexp = "MUST BE A FACTOR OR CHARACTER VECTOR", fixed = TRUE)
    # character categ converted to factor with a warning
    obs_chr <- obs1
    obs_chr$grp <- base::as.character(x = obs_chr$grp)
    output <- saferGG::gg_scatter(data1 = obs_chr, x = "x", y = "y", categ = "grp", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "THE CHARACTER COLUMN HAS BEEN CONVERTED TO FACTOR", fixed = TRUE))
    # character categ converted to factor with a warning
    obs_chr <- obs1
    obs_chr$grp <- base::as.character(x = obs_chr$grp)
    output <- saferGG::gg_scatter(data1 = obs_chr, x = "x", y = "y", categ = "grp", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "THE CHARACTER COLUMN HAS BEEN CONVERTED TO FACTOR", fixed = TRUE))
    # data1 not a data frame nor a list
    testthat::expect_error(object = saferGG::gg_scatter(data1 = "BAD", x = "x", y = "y", plot = FALSE, safer_check = FALSE), regexp = "data1 ARGUMENT MUST BE A DATA FRAME OR A LIST OF DATA FRAMES", fixed = TRUE)
    # list-form args with a data frame data1: errors for every converted argument
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = base::list("x"), y = "y", plot = FALSE, safer_check = FALSE), regexp = "x ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = base::list("y"), plot = FALSE, safer_check = FALSE), regexp = "y ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", categ = base::list("grp"), plot = FALSE, safer_check = FALSE), regexp = "categ ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", categ = "grp", categ_class_order = base::list(base::c("A", "B")), plot = FALSE, safer_check = FALSE), regexp = "categ_class_order ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", categ = "grp", color = base::list("red"), plot = FALSE, safer_check = FALSE), regexp = "color ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", geom = base::list("geom_point"), plot = FALSE, safer_check = FALSE), regexp = "geom ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", geom = "geom_step", geom_step_dir = base::list("vh"), plot = FALSE, safer_check = FALSE), regexp = "geom_step_dir ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", geom = "geom_stick", geom_stick_base = base::list(5), plot = FALSE, safer_check = FALSE), regexp = "geom_stick_base ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", alpha = base::list(0.5), plot = FALSE, safer_check = FALSE), regexp = "alpha ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", dot_size = base::list(2), plot = FALSE, safer_check = FALSE), regexp = "dot_size ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", dot_shape = base::list(16), plot = FALSE, safer_check = FALSE), regexp = "dot_shape ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", dot_border_size = base::list(1), plot = FALSE, safer_check = FALSE), regexp = "dot_border_size ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", dot_border_color = base::list("red"), plot = FALSE, safer_check = FALSE), regexp = "dot_border_color ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", geom = "geom_line", line_size = base::list(1), plot = FALSE, safer_check = FALSE), regexp = "line_size ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", geom = "geom_line", line_type = base::list(2), plot = FALSE, safer_check = FALSE), regexp = "line_type ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", legend_name = base::list("LN"), plot = FALSE, safer_check = FALSE), regexp = "legend_name ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME", fixed = TRUE)
    # list-form args with wrong length vs data1 (single data frame in a list)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = base::list(OBS1 = obs1), x = base::list("x", "x"), y = base::list("y"), plot = FALSE, safer_check = FALSE), regexp = "x ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = base::list(OBS1 = obs1), x = base::list("x"), y = base::list("y", "y"), plot = FALSE, safer_check = FALSE), regexp = "y ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = base::list(OBS1 = obs1), x = base::list("x"), y = base::list("y"), categ = base::list("grp", "grp"), plot = FALSE, safer_check = FALSE), regexp = "categ ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = base::list(OBS1 = obs1), x = base::list("x"), y = base::list("y"), categ = base::list("grp"), categ_class_order = base::list(base::c("A", "B"), base::c("A", "B")), plot = FALSE, safer_check = FALSE), regexp = "categ_class_order ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = base::list(OBS1 = obs1), x = base::list("x"), y = base::list("y"), color = base::list("red", "blue"), plot = FALSE, safer_check = FALSE), regexp = "color ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = base::list(OBS1 = obs1), x = base::list("x"), y = base::list("y"), geom = base::list("geom_point", "geom_point"), plot = FALSE, safer_check = FALSE), regexp = "geom ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = base::list(OBS1 = obs1), x = base::list("x"), y = base::list("y"), geom = "geom_step", geom_step_dir = base::list("vh", "hv"), plot = FALSE, safer_check = FALSE), regexp = "geom_step_dir ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = base::list(OBS1 = obs1), x = base::list("x"), y = base::list("y"), geom = "geom_stick", geom_stick_base = base::list(5, 6), plot = FALSE, safer_check = FALSE), regexp = "geom_stick_base ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = base::list(OBS1 = obs1), x = base::list("x"), y = base::list("y"), alpha = base::list(0.5, 0.8), plot = FALSE, safer_check = FALSE), regexp = "alpha ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = base::list(OBS1 = obs1), x = base::list("x"), y = base::list("y"), dot_size = base::list(2, 4), plot = FALSE, safer_check = FALSE), regexp = "dot_size ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = base::list(OBS1 = obs1), x = base::list("x"), y = base::list("y"), dot_shape = base::list(16, 17), plot = FALSE, safer_check = FALSE), regexp = "dot_shape ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = base::list(OBS1 = obs1), x = base::list("x"), y = base::list("y"), dot_border_size = base::list(1, 2), plot = FALSE, safer_check = FALSE), regexp = "dot_border_size ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = base::list(OBS1 = obs1), x = base::list("x"), y = base::list("y"), dot_border_color = base::list("red", "blue"), plot = FALSE, safer_check = FALSE), regexp = "dot_border_color ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = base::list(OBS1 = obs1), x = base::list("x"), y = base::list("y"), geom = "geom_line", line_size = base::list(1, 2), plot = FALSE, safer_check = FALSE), regexp = "line_size ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = base::list(OBS1 = obs1), x = base::list("x"), y = base::list("y"), geom = "geom_line", line_type = base::list(2, 3), plot = FALSE, safer_check = FALSE), regexp = "line_type ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = base::list(OBS1 = obs1), x = base::list("x"), y = base::list("y"), legend_name = base::list("A", "B"), plot = FALSE, safer_check = FALSE), regexp = "legend_name ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1", fixed = TRUE)
    # NA warn: rows removed + per-column removal messages + class disappearance warning
    obs_na <- base::data.frame(x = base::c(1, NA, 3, 4, 5), y = base::c(1, 2, NA, 4, 5), grp = base::factor(x = base::c("A", "B", "A", "C", "B")), stringsAsFactors = TRUE)
    output <- saferGG::gg_scatter(data1 = obs_na, x = "x", y = "y", categ = "grp", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "NA DETECTED IN COLUMNS", fixed = TRUE))
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "NA REMOVAL DUE TO COLUMN", fixed = TRUE))
    # categ NULL compartment with non-NULL categ_class_order compartment: error (compartment 1 value "Q" triggers the mismatch check against fake_categ before the NULL-categ check)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = base::list(O1 = obs1, O2 = obs2), x = base::list("x", "x"), y = base::list("y", "y"), categ = base::list("grp", NULL), categ_class_order = base::list(base::c("B", "A"), "Q"), plot = FALSE, safer_check = FALSE), regexp = "COMPARTMENT 2 OF categ_class_order ARGUMENT MUST BE CLASSES OF COMPARTMENT 2 OF categ ARGUMENT", fixed = TRUE)
    # categ_class_order NULL compartment: alphabetical order warning
    output <- saferGG::gg_scatter(data1 = base::list(O1 = obs1, O2 = obs2), x = base::list("x", "x"), y = base::list("y", "y"), categ = base::list("grp", NULL), categ_class_order = base::list(NULL, NULL), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "THE categ_class_order COMPARTMENT 1 IS NULL", fixed = TRUE))
    # categ_class_order wrong length (compartment arg_check)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", categ = "grp", categ_class_order = base::c("A", "B", "C"), plot = FALSE, safer_check = FALSE), regexp = "COMPARTMENT 1 OF categ_class_order ARGUMENT ARGUMENT MUST BE LENGTH 2", fixed = TRUE)
    # categ_class_order not character
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", categ = "grp", categ_class_order = base::c(1, 2), plot = FALSE, safer_check = FALSE), regexp = "COMPARTMENT 1 OF categ_class_order ARGUMENT ARGUMENT MUST BE TYPEOF character", fixed = TRUE)
    # categ_class_order duplicated classes
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", categ = "grp", categ_class_order = base::c("A", "A"), plot = FALSE, safer_check = FALSE), regexp = "COMPARTMENT 1 OF categ_class_order ARGUMENT CANNOT HAVE DUPLICATED CLASSES", fixed = TRUE)
    # categ_class_order mismatched classes
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", categ = "grp", categ_class_order = base::c("A", "Q"), plot = FALSE, safer_check = FALSE), regexp = "COMPARTMENT 1 OF categ_class_order ARGUMENT MUST BE CLASSES OF COMPARTMENT 1 OF categ ARGUMENT", fixed = TRUE)
    # color length mismatch vs categ
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", categ = "grp", color = base::c("red", "blue", "green"), plot = FALSE, safer_check = FALSE), regexp = "OR (2) THE LENGTH OF categ IN data1 ARGUMENT COLUMN VALUES", fixed = TRUE)
    # color containing NA
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", categ = "grp", color = base::c("red", NA), plot = FALSE, safer_check = FALSE), regexp = "color ARGUMENT CANNOT CONTAIN NA", fixed = TRUE)
    # color incorrectly associated to each class of categ (per-row length with inconsistent mapping)
    obs2c <- base::data.frame(x = 1:4, y = c(2, 4, 6, 8), grp = base::factor(x = base::c("A", "A", "B", "B")), stringsAsFactors = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs2c, x = "x", y = "y", categ = "grp", color = base::c("red", "blue", "blue", "red"), plot = FALSE, safer_check = FALSE), regexp = "BUT IS INCORRECTLY ASSOCIATED TO EACH CLASS OF THIS categ", fixed = TRUE)
    # color correct per-row association: warning + conversion
    output <- saferGG::gg_scatter(data1 = obs2c, x = "x", y = "y", categ = "grp", color = base::c("red", "red", "blue", "blue"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "COLORS HAVE BEEN RESPECTIVELY ASSOCIATED TO EACH CLASS OF categ", fixed = TRUE))
    # color of length 1: same color for all classes warning
    output <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", categ = "grp", color = "red", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "COLOR HAS LENGTH 1 MEANING THAT ALL THE DIFFERENT CLASSES", fixed = TRUE))
    # color with NULL compartment in a list: grey attributed (already tested) + LAST CHECK error path for fake_categ with length != 1
    # categ NULL + multi-color in a list: single-color override warning (color reset)
    output <- saferGG::gg_scatter(data1 = base::list(O1 = obs2c, O2 = obs2), x = base::list("x", "x"), y = base::list("y", "y"), categ = base::list(NULL, NULL), color = base::list(base::c("red", "blue"), "green"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "WHICH IS NOT COMPATIBLE WITH NULL CATEG -> COLOR RESET TO A SINGLE COLOR", fixed = TRUE))
    # geom with 4 geom_point elements in a list of 4 data frames
    d4 <- base::list(O1 = obs1, O2 = obs1, O3 = obs1, O4 = obs1)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = d4, x = base::list("x", "x", "x", "x"), y = base::list("y", "y", "y", "y"), geom = base::list("geom_point", "geom_point", "geom_point", "geom_point"), plot = FALSE, safer_check = FALSE), regexp = "geom ARGUMENT CANNOT HAVE MORE THAN THREE \\\"geom_point\\\" ELEMENTS", fixed = FALSE)
    # geom with 4 line elements in a list of 4 data frames
    testthat::expect_error(object = saferGG::gg_scatter(data1 = d4, x = base::list("x", "x", "x", "x"), y = base::list("y", "y", "y", "y"), geom = base::list("geom_line", "geom_line", "geom_line", "geom_line"), plot = FALSE, safer_check = FALSE), regexp = "geom ARGUMENT CANNOT HAVE MORE THAN THREE LINE ELEMENTS", fixed = TRUE)
    # y column names differ between data frames: overlay warning
    output <- saferGG::gg_scatter(data1 = base::list(O1 = obs1, O2 = obs2), x = base::list("x", "x"), y = base::list("y", "y"), plot = FALSE, return = TRUE, safer_check = FALSE)
    # x_lim with non positive values and x_log
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", x_log = "log10", x_lim = base::c(-1, 100), plot = FALSE, safer_check = FALSE), regexp = "x_lim ARGUMENT CANNOT HAVE ZERO OR NEGATIVE VALUES WITH THE x_log ARGUMENT SET TO log10", fixed = TRUE)
    # x_lim returning Inf with x_log
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", x_log = "log10", x_lim = base::c(0, 100), plot = FALSE, safer_check = FALSE), regexp = "x_lim ARGUMENT CANNOT HAVE ZERO OR NEGATIVE VALUES", fixed = TRUE)
    # y_lim with non positive values and y_log
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", y_log = "log2", y_lim = base::c(0, 100), plot = FALSE, safer_check = FALSE), regexp = "y_lim ARGUMENT CANNOT HAVE ZERO OR NEGATIVE VALUES WITH THE y_log ARGUMENT SET TO log2", fixed = TRUE)
    # y_lim made of Inf only with only geom_vline
    testthat::expect_error(object = saferGG::gg_scatter(data1 = base::list(V = obs1[, "x", drop = FALSE]), x = base::list("x"), y = base::list(NULL), geom = base::list("geom_vline"), y_lim = base::c(Inf, -Inf), x_lim = base::c(1, 10), plot = FALSE, safer_check = FALSE), regexp = "NOT POSSIBLE TO DRAW geom_vline KIND OF LINES ALONE", fixed = TRUE)
    # y_lim made of NA only
    testthat::expect_error(object = saferGG::gg_scatter(data1 = base::list(V = obs1[, "x", drop = FALSE]), x = base::list("x"), y = base::list(NULL), geom = base::list("geom_vline"), y_lim = base::c(NA_real_, NA_real_), x_lim = base::c(1, 10), plot = FALSE, safer_check = FALSE), regexp = "THIS ARGUMENT CANNOT BE MADE OF NA ONLY:\ny_lim", fixed = TRUE)
    # x_lim made of Inf only (with a data frame and y set: hline path)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", x_lim = base::c(Inf, -Inf), y_lim = base::c(1, 100), plot = FALSE, safer_check = FALSE), regexp = "x_lim ARGUMENT MADE OF NA, -Inf OR Inf ONLY: Inf -Inf", fixed = TRUE)
    # raster = TRUE (gg_point_rast path)
    output <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", categ = "grp", raster = TRUE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # raster = TRUE with geom_line: raster applied to points only
    output <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", geom = "geom_line", raster = TRUE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # 2nd and 3rd geom_point layers + 1st/2nd line layers (legend bookkeeping lines)
    obs1b <- obs1
    obs1b$grp2 <- base::factor(x = base::rep(x = c("X", "Y"), times = 5))
    output <- saferGG::gg_scatter(
        data1 = base::list(P1 = obs1, P2 = obs1b, P3 = obs1, L1 = obs1, L2 = obs1),
        x = base::list("x", "x", "x", "x", "x"),
        y = base::list("y", "y", "y", "y", "y"),
        geom = base::list("geom_point", "geom_point", "geom_point", "geom_line", "geom_line"),
        categ = base::list("grp", "grp2", "grp", "grp", "grp"),
        plot = FALSE,
        return = TRUE,
        safer_check = FALSE
    )
    testthat::expect_type(output, "list")
    # raster: small data uses gg_point_rast, large data stays geom_point (threshold path)
    output <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", categ = "grp", raster = TRUE, raster_threshold = 100, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # 2nd and 3rd point layers with dot_shape in 21:24 and non-NULL dot_border_color: border color path in shape and stroke layers
    output <- saferGG::gg_scatter(
        data1 = base::list(P1 = obs1, P2 = obs1b, P3 = obs1),
        x = base::list("x", "x", "x"),
        y = base::list("y", "y", "y"),
        geom = base::list("geom_point", "geom_point", "geom_point"),
        categ = base::list("grp", "grp2", "grp"),
        dot_shape = base::list(21, 21, 21),
        dot_border_color = base::list("black", "black", "black"),
        plot = FALSE,
        return = TRUE,
        safer_check = FALSE
    )
    testthat::expect_type(output, "list")
    # stick as single line layer (linetype aesthetic) with non-NULL geom_stick_base: base of sticks = provided value
    obs_stick <- obs1
    output <- saferGG::gg_scatter(data1 = obs_stick, x = "x", y = "y", geom = "geom_stick", categ = "grp", geom_stick_base = 5, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # stick as 2nd line layer (alpha aesthetic) with non-NULL geom_stick_base in the corresponding compartment
    output <- saferGG::gg_scatter(
        data1 = base::list(L1 = obs1, P1 = obs1, S1 = obs1),
        x = base::list("x", "x", "x"),
        y = base::list("y", "y", "y"),
        geom = base::list("geom_line", "geom_point", "geom_stick"),
        categ = base::list("grp", NULL, "grp"),
        geom_stick_base = base::list(NULL, NULL, 5),
        plot = FALSE,
        return = TRUE,
        safer_check = FALSE
    )
    testthat::expect_type(output, "list")
    # stick as 3rd line layer (size aesthetic) with non-NULL geom_stick_base in the corresponding compartment
    output <- saferGG::gg_scatter(
        data1 = base::list(L1 = obs1, L2 = obs1, P1 = obs1, S1 = obs1),
        x = base::list("x", "x", "x", "x"),
        y = base::list("y", "y", "y", "y"),
        geom = base::list("geom_line", "geom_line", "geom_point", "geom_stick"),
        categ = base::list("grp", "grp", NULL, "grp"),
        geom_stick_base = base::list(NULL, NULL, NULL, 5),
        plot = FALSE,
        return = TRUE,
        safer_check = FALSE
    )
    testthat::expect_type(output, "list")
    # dot_shape in 21:24 with dot_border_color: border color used
    output <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", categ = "grp", dot_shape = 21, dot_border_color = "black", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # legend_show = FALSE with a categ: empty legend + warn (only when plot = TRUE)
    output <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", categ = "grp", legend_show = FALSE, plot = TRUE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "LEGEND REQUESTED (NON-NULL categ ARGUMENT", fixed = TRUE))
    # legend_show = FALSE + non-NULL legend_width
    output <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", categ = "grp", legend_show = FALSE, legend_width = 0.4, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # x-axis with null range: friendly error
    testthat::expect_error(object = saferGG::gg_scatter(data1 = base::data.frame(x = base::c(5, 5), y = base::c(1, 2), stringsAsFactors = TRUE), x = "x", y = "y", x_lim = base::c(5, 5), y_lim = base::c(1, 2), plot = FALSE, safer_check = FALSE), regexp = "IT SEEMS THAT X-AXIS VALUES HAVE A NULL RANGE", fixed = TRUE)
    # y-axis with null range: friendly error
    testthat::expect_error(object = saferGG::gg_scatter(data1 = base::data.frame(x = base::c(1, 2), y = base::c(5, 5), stringsAsFactors = TRUE), x = "x", y = "y", x_lim = base::c(1, 2), y_lim = base::c(5, 5), plot = FALSE, safer_check = FALSE), regexp = "IT SEEMS THAT Y-AXIS VALUES HAVE A NULL RANGE", fixed = TRUE)
    # x tick count too low vs second tick count: friendly error
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", x_tick_nb = 1L, x_second_tick_nb = 5L, plot = FALSE, safer_check = FALSE), regexp = "THE NUMBER OF GENERATED TICKS FOR THE X-AXIS IS NOT CORRECT", fixed = TRUE)
    # second ticks on both axes
    output <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", x_tick_nb = 5L, x_second_tick_nb = 2L, y_tick_nb = 5L, y_second_tick_nb = 2L, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # second ticks with inverted limits
    output <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", x_lim = base::c(10, 1), y_lim = base::c(100, 1), x_tick_nb = 5L, x_second_tick_nb = 2L, y_tick_nb = 5L, y_second_tick_nb = 2L, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # plot = TRUE with return: gtable built
    output <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", plot = TRUE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::all(base::c("data", "plot", "ggplot", "gtable") %in% base::names(x = output)))
    # facet panels with more than one data frame: error
    testthat::expect_error(object = saferGG::gg_scatter(data1 = base::list(O1 = obs1, O2 = obs2), x = base::list("x", "x"), y = base::list("y", "y"), add = "+ggplot2::facet_wrap(~grp)", plot = FALSE, safer_check = FALSE), regexp = "facet PANELS CANNOT BE USED IF MORE THAN ONE DATA FRAME", fixed = TRUE)
    # facet_grid on a single data frame: accepted
    output <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", add = "+ggplot2::facet_grid(~grp)", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # facet with an unknown variable: friendly variable detection error
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", add = "+ggplot2::facet_wrap(~BADVAR)", plot = FALSE, safer_check = FALSE), regexp = "PROBLEM OF VARIABLE DETECTION (COLUMN NAMES OF data1)", fixed = TRUE)
    # add containing a gg_scatter argument name (plot): warning only
    output <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", add = "+ggplot2::labs(title = plot)", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # geom_step_dir with a bad option
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", geom = "geom_step", geom_step_dir = "BAD", plot = FALSE, safer_check = FALSE), regexp = "THE geom_step_dir ARGUMENT MUST BE SOME OF THESE OPTIONS", fixed = TRUE)
    # geom_stick_base with a non numeric value
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", geom = "geom_stick", geom_stick_base = "BAD", plot = FALSE, safer_check = FALSE), regexp = "THE geom_stick_base ARGUMENT MUST BE MODE numeric", fixed = TRUE)
    # x NULL + y NULL: friendly error (cannot be NULL except for hline/vline geoms)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = NULL, y = NULL, plot = FALSE, safer_check = FALSE), regexp = "x ARGUMENT CANNOT BE NULL EXCEPT IF x ARGUMENT IS", fixed = TRUE)
    # categ NULL with non-NULL categ_class_order on a single data frame
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", categ = NULL, categ_class_order = base::c("A", "B"), plot = FALSE, safer_check = FALSE), regexp = "COMPARTMENT 1 OF categ ARGUMENT CANNOT BE NULL IF COMPARTMENT 1 OF categ_class_order ARGUMENT IS NOT NULL", fixed = TRUE)
    # log conversion introducing NaN (-1 with log10): rows removed with a warning
    obs_neg <- obs1
    obs_neg$x[1] <- -1
    output <- saferGG::gg_scatter(data1 = obs_neg, x = "x", y = "y", x_log = "log10", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "LOG CONVERSION INTRODUCED -Inf OR Inf OR NaN VALUES", fixed = TRUE))
    # NA in a categ column: class disappearance warning (seq_len fix: no more "undefined columns selected" crash)
    obs_na <- base::data.frame(x = base::c(1, 2, 3, 4), y = base::c(1, 2, 3, 4), grp = base::factor(x = base::c("A", "A", NA, "B")), stringsAsFactors = TRUE)
    output <- saferGG::gg_scatter(data1 = obs_na, x = "x", y = "y", categ = "grp", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "THE FOLLOWING CLASSES HAVE DISAPPEARED AFTER NA REMOVAL", fixed = TRUE))
    # theme variants: article with grid, article without grid, non-article add
    obs_v <- obs1
    output_v1 <- saferGG::gg_scatter(data1 = obs_v, x = "x", y = "y", article = TRUE, grid = TRUE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output_v1, "list")
    output_v2 <- saferGG::gg_scatter(data1 = obs_v, x = "x", y = "y", article = TRUE, grid = FALSE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output_v2, "list")
    output_v3 <- saferGG::gg_scatter(data1 = obs_v, x = "x", y = "y", add = "+ggplot2::coord_cartesian()", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output_v3, "list")
    # add with ggplot2::theme: warn + add_check FALSE (internal theme not applied)
    output_v4 <- saferGG::gg_scatter(data1 = obs_v, x = "x", y = "y", add = "+ggplot2::theme()", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output_v4, "list")
    testthat::expect_true(object = base::grepl(x = output_v4$warn, pattern = "\"ggplot2::theme\" STRING DETECTED IN THE add ARGUMENT", fixed = TRUE))
    # overlay color=NULL: grey color warn in multi-overlay list form
    output_v5 <- saferGG::gg_scatter(data1 = base::list(obs1, obs1), x = base::list("x", "x"), y = base::list("y", "y"), color = base::list("grey", NULL), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output_v5, "list")
    testthat::expect_true(object = base::grepl(x = output_v5$warn, pattern = "NULL COLOR IN", fixed = TRUE))
    # x column names differ between overlays: warn
    obs2b <- base::data.frame(x = stats::rnorm(n = 30, mean = 12, sd = 3), y2 = stats::rnorm(n = 30, mean = 11, sd = 3), stringsAsFactors = TRUE)
    output_v6 <- saferGG::gg_scatter(data1 = base::list(obs1, obs2b), x = base::list("x", "x"), y = base::list("y", "y2"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output_v6, "list")
    testthat::expect_true(object = base::grepl(x = output_v6$warn, pattern = "THE y ARGUMENT DOES NOT CONTAIN IDENTICAL COLUMN NAMES", fixed = TRUE))
    obs2b <- base::data.frame(x2 = stats::rnorm(n = 30, mean = 12, sd = 3), y2 = stats::rnorm(n = 30, mean = 11, sd = 3), stringsAsFactors = TRUE)
    output_v6 <- saferGG::gg_scatter(data1 = base::list(obs1, obs2b), x = base::list("x", "x2"), y = base::list("y", "y2"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output_v6, "list")
    testthat::expect_true(object = base::grepl(x = output_v6$warn, pattern = "THE x ARGUMENT DOES NOT CONTAIN IDENTICAL COLUMN NAMES", fixed = TRUE))
    output_v6 <- saferGG::gg_scatter(data1 = base::list(obs1, obs2b), x = base::list("x", "x2"), y = base::list("y", "y2"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output_v6, "list")
    testthat::expect_true(object = base::grepl(x = output_v6$warn, pattern = "THE x ARGUMENT DOES NOT CONTAIN IDENTICAL COLUMN NAMES", fixed = TRUE))
    # x_lim <= 0 with x_log: error
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", x_log = "log10", x_lim = base::c(-5, 20), plot = FALSE, safer_check = FALSE), regexp = "x_lim ARGUMENT CANNOT HAVE ZERO OR NEGATIVE VALUES WITH THE x_log ARGUMENT SET TO log10", fixed = TRUE)
    # x_log with x_include_zero: warn + reset to FALSE
    output_v7 <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", x_log = "log10", x_include_zero = TRUE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output_v7, "list")
    testthat::expect_true(object = base::grepl(x = output_v7$warn, pattern = "x_log ARGUMENT SET TO log10 AND x_include_zero ARGUMENT SET TO TRUE", fixed = TRUE))
    # zero-arg call: mandatory/no-default block (data1, x, y)
    res_zero <- base::tryCatch(
        expr = saferGG::gg_scatter(),
        error = function(e){base::conditionMessage(e)}
    )
    testthat::expect_true(object = base::grepl(x = res_zero, pattern = "NO DEFAULT VALUE AND REQUIRE ONE", fixed = TRUE))
    # data1 as list with x = NULL (geom_hline) and both limits set: x recycled to a list of NULL (775 branch)
    output_u7 <- saferGG::gg_scatter(data1 = base::list(obs1, obs1), x = NULL, y = base::list("y", "y"), geom = "geom_hline", x_lim = base::c(0, 20), y_lim = base::c(0, 20), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output_u7, "list")
    # data1 as list with y = NULL (geom_vline) and both limits set: y recycled to a list of NULL (783 branch)
    output_u8 <- saferGG::gg_scatter(data1 = base::list(obs1, obs1), x = base::list("x", "x"), y = NULL, geom = "geom_vline", x_lim = base::c(0, 20), y_lim = base::c(0, 20), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output_u8, "list")
    # reserved column name (fake_x) in data1: error from the reserved-words block
    obs_fx <- obs1
    obs_fx$fake_x <- base::seq_len(length.out = base::nrow(x = obs_fx))
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs_fx, x = "x", y = "y", plot = FALSE, safer_check = FALSE), regexp = "THESE ARE RESERVED FOR THE gg_scatter() FUNCTION", fixed = TRUE)
    # reserved string in the add argument: error from the add-reserved check
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", add = "+ ggplot2::labs(x = 'fake_x')", plot = FALSE, safer_check = FALSE), regexp = "DETECTION OF COLUMN NAMES OF data1 IN THE add ARGUMENT STRING", fixed = TRUE)
    # geom_step without geom_step_dir: error (geom_step_dir cannot be NULL)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", geom = "geom_step", geom_step_dir = NULL, plot = FALSE, safer_check = FALSE), regexp = "THIS ARGUMENT CANNOT BE NULL", fixed = TRUE)
    # y as a non-existing column name: error
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "BAD", plot = FALSE, safer_check = FALSE), regexp = "y ARGUMENT MUST BE A COLUMN NAME OF data1 ARGUMENT", fixed = TRUE)
    # categ of a bad class (numeric): error from arg_check
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", categ = 1, plot = FALSE, safer_check = FALSE), regexp = "THE categ ARGUMENT MUST BE TYPEOF character", fixed = TRUE)
    # legend_name of bad class: error
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", categ = "grp", legend_name = 5, plot = FALSE, safer_check = FALSE), regexp = "THE legend_name ARGUMENT MUST BE TYPEOF character", fixed = TRUE)
    # color neither a valid color nor a factor column of data1: error
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", categ = "grp", color = "BADCOLOR", plot = FALSE, safer_check = FALSE), regexp = "color MUST BE A HEXADECIMAL COLOR VECTOR STARTING BY #", fixed = TRUE)
    # color with length > 1 and categ NULL: error
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", color = c("red", "blue"), plot = FALSE, safer_check = FALSE), regexp = "color MUST BE A SINGLE COLOR IF categ IS NULL", fixed = TRUE)
    # y_log introducing -Inf/Inf: warn + rows removed
    obs_neg <- obs1
    obs_neg$y[1] <- -1
    output_neg <- saferGG::gg_scatter(data1 = obs_neg, x = "x", y = "y", y_log = "log10", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output_neg, "list")
    testthat::expect_true(object = base::grepl(x = output_neg$warn, pattern = "LOG CONVERSION INTRODUCED -Inf OR Inf OR NaN VALUES", fixed = TRUE))
    # categ with an NA class and categ_class_order provided as vector: class-order rebuild branch
    obs_na <- obs1
    obs_na$grp[1] <- NA
    output_ord <- saferGG::gg_scatter(data1 = obs_na, x = "x", y = "y", categ = "grp", categ_class_order = c("A", "B", "C"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output_ord, "list")
    # x column containing only Inf values with mixed geoms: all-Inf x_lim error
    obs_infonly <- base::data.frame(x = base::rep(x = Inf, times = 10), y = stats::rnorm(n = 10, mean = 10, sd = 2), stringsAsFactors = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs_infonly, x = "x", y = "y", y_lim = base::c(0, 20), plot = FALSE, safer_check = FALSE), regexp = "x_lim ARGUMENT MADE OF NA, -Inf OR Inf ONLY", fixed = TRUE)
    # x column containing only Inf values with all-geom_vline: the vline-alone error (no x-axis defined)
    obs_inf_v <- base::data.frame(x = base::rep(x = Inf, times = 10), y = stats::rnorm(n = 10, mean = 10, sd = 2), stringsAsFactors = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs_inf_v, x = "x", y = NULL, geom = "geom_vline", y_lim = base::c(0, 20), plot = FALSE, safer_check = FALSE), regexp = "NOT POSSIBLE TO DRAW geom_vline OR geom_stick KIND OF LINES ALONE IF x_lim ARGUMENT IS SET TO NULL", fixed = TRUE)
    # raster = TRUE with geom_point: raster layer generated + aspect-ratio warn
    output_raster <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", raster = TRUE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output_raster, "list")
    testthat::expect_true(object = base::grepl(x = output_raster$warn, pattern = "RASTER PLOT GENERATED", fixed = TRUE))
    # two and three line layers in one call: line.count == 2 and 3 legend-management branches
    output_2lines <- saferGG::gg_scatter(data1 = base::list(obs1, obs1), x = base::list("x", "x"), y = base::list("y", "y"), geom = base::list("geom_line", "geom_line"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output_2lines, "list")
    output_3lines <- saferGG::gg_scatter(data1 = base::list(obs1, obs1, obs1), x = base::list("x", "x", "x"), y = base::list("y", "y", "y"), geom = base::list("geom_line", "geom_line", "geom_line"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output_3lines, "list")
    # geom_stick: line.count branches with stick converted to geom_segment
    output_stick <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", geom = "geom_stick", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output_stick, "list")
    # legend_show = FALSE with hidden legends: empty graph placed instead of the legend
    output_noLg <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", categ = "grp", legend_show = FALSE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output_noLg, "list")
    # y_tick_nb producing a single tick with y_second_tick_nb: tick-number error (symmetric lim around 0 with n=1 gives a single tick)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", y_lim = base::c(-10, 10), y_tick_nb = 1L, y_second_tick_nb = 2L, plot = FALSE, safer_check = FALSE), regexp = "THE NUMBER OF GENERATED TICKS FOR THE Y-AXIS IS NOT CORRECT", fixed = TRUE)
    # x_tick_nb producing a single tick with x_second_tick_nb: tick-number error
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", x_lim = base::c(-10, 10), x_tick_nb = 1L, x_second_tick_nb = 2L, plot = FALSE, safer_check = FALSE), regexp = "THE NUMBER OF GENERATED TICKS FOR THE X-AXIS IS NOT CORRECT", fixed = TRUE)
    # plot = TRUE without legend (no categ): direct print path
    output_plot2 <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", plot = TRUE)
    testthat::expect_null(object = output_plot2)
    # empty string in the add argument: error from the empty-string block
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", add = "", plot = FALSE, safer_check = FALSE), regexp = "THIS ARGUMENT\nadd\nCANNOT CONTAIN EMPTY STRING", fixed = TRUE)
    # a matrix compartment inside a list data1: data-frame check error
    obs_mat <- base::matrix(data = 1:20, ncol = 2)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = base::list(obs1, obs_mat), x = base::list("x", "x"), y = base::list("y", "y"), plot = FALSE, safer_check = FALSE), regexp = "data1 ARGUMENT MUST BE A DATA FRAME OR A LIST OF DATA FRAMES", fixed = TRUE)
    # x column of character class: cannot-be-a-factor error
    obs_carx <- base::data.frame(x = base::as.character(x = 1:10), y = stats::rnorm(n = 10, mean = 10, sd = 2), stringsAsFactors = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs_carx, x = "x", y = "y", plot = FALSE, safer_check = FALSE), regexp = "CANNOT BE A FACTOR BUT MUST BE MADE OF NUMBERS", fixed = TRUE)
    # y column of character class: cannot-be-a-factor error
    obs_cary <- base::data.frame(x = stats::rnorm(n = 10, mean = 10, sd = 2), y = base::as.character(x = 1:10), stringsAsFactors = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs_cary, x = "x", y = "y", plot = FALSE, safer_check = FALSE), regexp = "CANNOT BE A FACTOR BUT MUST BE MADE OF NUMBERS", fixed = TRUE)
    # multi-data1 call with hline compartments with NULL categ: fake_categ Line_... column of the right length (regression test for a former data1[[i2]] typo at the fake_categ creation)
    obs_d1 <- base::data.frame(a = 1:5, b = (1:5) * 2, g = base::as.factor(x = paste0("G", 1:5)), stringsAsFactors = TRUE)
    obs_d2 <- base::data.frame(b = 7, stringsAsFactors = TRUE)
    obs_d3 <- base::data.frame(b = c(8, 9, 10), stringsAsFactors = TRUE)
    output_fakec <- saferGG::gg_scatter(data1 = base::list(obs_d1, obs_d2, obs_d3), x = base::list("a", NULL, NULL), y = base::list("b", "b", "b"), geom = base::list("geom_point", "geom_hline", "geom_hline"), categ = base::list("g", NULL, NULL), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output_fakec, "list")
    # multi-overlay call with different x column names: X-AXIS OVERLAYING warn
    obs_xx1 <- base::data.frame(a = 1:5, c = (1:5)^2, stringsAsFactors = TRUE)
    obs_xx2 <- base::data.frame(b = 1:5, c = (1:5) + 10, stringsAsFactors = TRUE)
    output_xx <- saferGG::gg_scatter(data1 = base::list(obs_xx1, obs_xx2), x = base::list("a", "b"), y = base::list("c", "c"), geom = base::list("geom_point", "geom_point"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output_xx, "list")
    testthat::expect_true(object = base::grepl(x = output_xx$warn, pattern = "THE x ARGUMENT DOES NOT CONTAIN IDENTICAL COLUMN NAMES", fixed = TRUE))
    # multi-overlay call with different y column names: Y-AXIS OVERLAYING warn
    obs_yy1 <- base::data.frame(a = 1:5, c = (1:5)^2, stringsAsFactors = TRUE)
    obs_yy2 <- base::data.frame(a = 1:5, d = (1:5) + 10, stringsAsFactors = TRUE)
    output_yy <- saferGG::gg_scatter(data1 = base::list(obs_yy1, obs_yy2), x = base::list("a", "a"), y = base::list("c", "d"), geom = base::list("geom_point", "geom_point"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output_yy, "list")
    testthat::expect_true(object = base::grepl(x = output_yy$warn, pattern = "THE y ARGUMENT DOES NOT CONTAIN IDENTICAL COLUMN NAMES", fixed = TRUE))
    # Windows-transparency warns (windows device open): dot layers 1, 2, 3 and line layers 1, 2, 3
    if(Sys.info()[["sysname"]] == "Windows"){
        obs_win <- base::data.frame(x = 1:10, y = (1:10) * 2, grp = base::rep(x = c("A", "B"), each = 5), stringsAsFactors = TRUE)
        grDevices::windows(width = 7, height = 7)
        output_win1 <- saferGG::gg_scatter(data1 = obs_win, x = "x", y = "y", categ = "grp", alpha = 0.5, dot_shape = 1, legend_show = TRUE, plot = TRUE, return = TRUE, safer_check = FALSE)
        testthat::expect_true(object = base::grepl(x = output_win1$warn, pattern = "TRANSPARENCY OF THE DOTS (DOT LAYER NUMBER 1)", fixed = TRUE))
        output_win2 <- saferGG::gg_scatter(data1 = base::list(obs_win, obs_win), x = base::list("x", "x"), y = base::list("y", "y"), geom = base::list("geom_point", "geom_point"), categ = base::list("grp", "grp"), alpha = 0.5, dot_shape = base::list(1, 1), plot = TRUE, return = TRUE, safer_check = FALSE)
        testthat::expect_true(object = base::grepl(x = output_win2$warn, pattern = "TRANSPARENCY OF THE DOTS (DOT LAYER NUMBER 2)", fixed = TRUE))
        output_win3 <- saferGG::gg_scatter(data1 = base::list(obs_win, obs_win, obs_win), x = base::list("x", "x", "x"), y = base::list("y", "y", "y"), geom = base::list("geom_point", "geom_point", "geom_point"), categ = base::list("grp", "grp", "grp"), alpha = 0.5, dot_shape = base::list(1, 1, 1), plot = TRUE, return = TRUE, safer_check = FALSE)
        testthat::expect_true(object = base::grepl(x = output_win3$warn, pattern = "TRANSPARENCY OF THE DOTS (DOT LAYER NUMBER 3)", fixed = TRUE))
        output_win4 <- saferGG::gg_scatter(data1 = obs_win, x = "x", y = "y", geom = "geom_line", categ = "grp", alpha = 0.5, legend_show = TRUE, plot = TRUE, return = TRUE, safer_check = FALSE)
        testthat::expect_true(object = base::grepl(x = output_win4$warn, pattern = "TRANSPARENCY OF THE LINES (LINE LAYER NUMBER 1)", fixed = TRUE))
        output_win5 <- saferGG::gg_scatter(data1 = base::list(obs_win, obs_win), x = base::list("x", "x"), y = base::list("y", "y"), geom = base::list("geom_line", "geom_line"), categ = base::list("grp", "grp"), alpha = 0.5, plot = TRUE, return = TRUE, safer_check = FALSE)
        testthat::expect_true(object = base::grepl(x = output_win5$warn, pattern = "TRANSPARENCY OF THE LINES (LINE LAYER NUMBER 2)", fixed = TRUE))
        output_win6 <- saferGG::gg_scatter(data1 = base::list(obs_win, obs_win, obs_win), x = base::list("x", "x", "x"), y = base::list("y", "y", "y"), geom = base::list("geom_line", "geom_line", "geom_line"), categ = base::list("grp", "grp", "grp"), alpha = 0.5, plot = TRUE, return = TRUE, safer_check = FALSE)
        testthat::expect_true(object = base::grepl(x = output_win6$warn, pattern = "TRANSPARENCY OF THE LINES (LINE LAYER NUMBER 3)", fixed = TRUE))
        grDevices::dev.off()
    }
    ## end errors
    # x column of logical class: mode-not-numeric arg_check error
    testthat::expect_error(object = saferGG::gg_scatter(data1 = base::data.frame(x = base::rep(c(TRUE, FALSE), 5), y = 1:10, stringsAsFactors = TRUE), x = "x", y = "y", plot = FALSE, safer_check = FALSE), regexp = "MUST BE MODE numeric", fixed = TRUE)
    # y column of logical class: idem
    testthat::expect_error(object = saferGG::gg_scatter(data1 = base::data.frame(x = 1:10, y = base::rep(c(TRUE, FALSE), 5), stringsAsFactors = TRUE), x = "x", y = "y", plot = FALSE, safer_check = FALSE), regexp = "MUST BE MODE numeric", fixed = TRUE)
    # geom_hline with a duplicated-class factor categ: different-class-per-line error
    obs_hdup <- base::data.frame(x = base::factor(c("A", "A", "B")), y = c(5, 5, 5), stringsAsFactors = TRUE)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs_hdup, x = NULL, y = "y", geom = "geom_hline", categ = "x", x_lim = base::c(0, 10), y_lim = base::c(0, 10), plot = FALSE, safer_check = FALSE), regexp = "MUST HAVE A DIFFERENT CLASS PER LINE", fixed = TRUE)
    # color of non character/factor/integer class: arg_check error
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", color = 1.5, plot = FALSE, safer_check = FALSE), regexp = "MUST BE A FACTOR OR CHARACTER VECTOR OR INTEGER VECTOR", fixed = TRUE)
    # x_lim with Inf and x_log: log of Inf is Inf -> error
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", x_lim = base::c(0.5, Inf), x_log = "log10", plot = FALSE, safer_check = FALSE), regexp = "RETURNS INF/NA WITH THE x_log ARGUMENT", fixed = TRUE)
    # y_lim with Inf and y_log: idem
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", y_lim = base::c(0.5, Inf), y_log = "log10", plot = FALSE, safer_check = FALSE), regexp = "RETURNS INF/NA WITH THE y_log ARGUMENT", fixed = TRUE)
    # valid x_lim with x_log: x_lim log-transformed without error
    output_xloglim <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", x_lim = base::c(1, 100), x_log = "log10", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_xloglim, "list")
    # x_lim with Inf and x_log = "log2": log2 of Inf is Inf -> error (log2 branch)
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", x_lim = base::c(0.5, Inf), x_log = "log2", plot = FALSE, safer_check = FALSE), regexp = "RETURNS INF/NA WITH THE x_log ARGUMENT", fixed = TRUE)
    # y_lim with Inf and y_log = "log2": idem
    testthat::expect_error(object = saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", y_lim = base::c(0.5, Inf), y_log = "log2", plot = FALSE, safer_check = FALSE), regexp = "RETURNS INF/NA WITH THE y_log ARGUMENT", fixed = TRUE)
    # x_lab and y_lab provided: used as axis labels
    output_lab <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", x_lab = "X LABEL", y_lab = "Y LABEL", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_lab, "list")
    # raster = TRUE with article = TRUE and grid = TRUE: aspect_ratio in the grid theme
    if(base::requireNamespace(package = "Cairo", quietly = TRUE)){
        output_rastergrid <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", raster = TRUE, grid = TRUE, plot = FALSE, return = TRUE, safer_check = FALSE)
        testthat::expect_type(object = output_rastergrid, "list")
        # raster = TRUE with article = FALSE: aspect_ratio in the non-article theme
        output_rasterna <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", raster = TRUE, article = FALSE, plot = FALSE, return = TRUE, safer_check = FALSE)
        testthat::expect_type(object = output_rasterna, "list")
    }else{
        testthat::skip(message = "Cairo package not available for raster drawing")
    }
    # log scales with decreasing limits on both axes: second-tick segment annotation else branches
    output_revlog <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", x_log = "log10", x_lim = base::c(10, 1), y_log = "log10", y_lim = base::c(100, 1), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_revlog, "list")
    # return_ggplot = TRUE: ggplot object in output
    output_ggp <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", return_ggplot = TRUE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_ggp, "list")
    # x_include_zero = TRUE: x_lim includes 0
    output_xzero <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", x_include_zero = TRUE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_xzero, "list")
    # y_include_zero = TRUE: y_lim includes 0
    output_yzero <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", y_include_zero = TRUE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_yzero, "list")
    # raster = TRUE with a raster_threshold smaller than nrow(data1): gg_point_rast layer used
    obs_rast <- base::data.frame(x = 1:30, y = (1:30)^2, stringsAsFactors = TRUE)
    output_rast <- saferGG::gg_scatter(data1 = obs_rast, x = "x", y = "y", raster = TRUE, raster_threshold = 15, raster_ratio = 0.5, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_rast, "list")
    # add argument with article = FALSE: non-article theme branch
    output_art <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", add = "+ggplot2::coord_cartesian()", article = FALSE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_art, "list")
    # plot = TRUE with legend_width = NULL: print branch (windows device must be open on Windows)
    if(Sys.info()[["sysname"]] == "Windows"){
        grDevices::windows(width = 7, height = 7)
        output_print <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", plot = TRUE, return = TRUE, safer_check = FALSE)
        testthat::expect_type(object = output_print, "list")
        grDevices::dev.off()
    }
    # x column containing Inf without x_lim: Inf rows removed in the per-compartment loop, then range() of finite values
    obs_xinf <- base::data.frame(x = base::c(1:9, Inf), y = 1:10, stringsAsFactors = TRUE)
    output_xinf <- saferGG::gg_scatter(data1 = obs_xinf, x = "x", y = "y", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_xinf, "list")
    # y column containing Inf without y_lim: idem
    obs_yinf <- base::data.frame(x = 1:10, y = base::c(1:9, Inf), stringsAsFactors = TRUE)
    output_yinf <- saferGG::gg_scatter(data1 = obs_yinf, x = "x", y = "y", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_yinf, "list")
    # valid y_lim with y_log: y_lim log-transformed without error
    output_yloglim <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", y_lim = base::c(1, 100), y_log = "log10", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_yloglim, "list")
    # geom_stick with a categ: the per-class geom_segment stick layer branch (line.count == 1 block)
    output_stickc <- saferGG::gg_scatter(data1 = obs1, x = "x", y = "y", geom = "geom_stick", categ = "grp", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_stickc, "list")
    # geom_stick as the THIRD line layer (point first, then 2 lines, then the stick): line.count == 3 block with stick xend/y paste
    d_stick3 <- base::list(obs1, obs1, obs1, obs1)
    output_stick3 <- saferGG::gg_scatter(data1 = d_stick3, x = base::list("x", "x", "x", "x"), y = base::list("y", "y", "y", "y"), geom = base::list("geom_point", "geom_line", "geom_line", "geom_stick"), categ = base::list("grp", "grp", "grp", "grp"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(object = output_stick3, "list")
    # cleanup of all files and devices created during the tests
    while(base::length(x = grDevices::dev.list()) > 0){ grDevices::dev.off() }
    base::unlink(x = tempo_dir, recursive = TRUE, force = TRUE)
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})

testthat::test_that("gg_scatter prints its warnings only when warn_print = TRUE, also with return = TRUE", {
    obs_wp <- base::data.frame(a = 1:5, b = 1:5)
    testthat::expect_warning(object = output_wp <- saferGG::gg_scatter(data1 = obs_wp, x = "a", y = "b", plot = FALSE, return = TRUE, warn_print = TRUE, safer_check = FALSE), regexp = "PLOT NOT SHOWN AS REQUESTED", fixed = TRUE)
    testthat::expect_no_warning(object = output_wp <- saferGG::gg_scatter(data1 = obs_wp, x = "a", y = "b", plot = FALSE, return = TRUE, warn_print = FALSE, safer_check = FALSE))
    testthat::expect_true(object = base::grepl(pattern = "PLOT NOT SHOWN AS REQUESTED", x = output_wp$warn, fixed = TRUE))
    testthat::expect_no_warning(object = saferGG::gg_scatter(data1 = obs_wp, x = "a", y = "b", plot = FALSE, return = FALSE, warn_print = FALSE, safer_check = FALSE))
    while(base::length(x = grDevices::dev.list()) > 0){ grDevices::dev.off() }
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})

testthat::test_that("gg_scatter add argument accepts extra layers (geom_hline(), geom_vline(), geom_abline(), annotate())", {
    obs_lay <- base::data.frame(a = base::c(1, 2, 3, 4, 5, 6), b = base::c(2, 1, 4, 3, 6, 5), g = base::factor(x = base::rep(x = base::c("A", "B"), times = 3)), f = base::factor(x = base::rep(x = base::c("u", "v"), each = 3)), stringsAsFactors = TRUE)
    output_ref <- saferGG::gg_scatter(data1 = obs_lay, x = "a", y = "b", categ = "g", plot = FALSE, return = TRUE, return_ggplot = TRUE, warn_print = FALSE, safer_check = FALSE)
    for(tempo_add in base::c("+ ggplot2::geom_hline(yintercept = 3)", "+ ggplot2::geom_vline(xintercept = 3)", "+ ggplot2::geom_abline(intercept = 0, slope = 1)", "+ ggplot2::annotate(geom = \"text\", x = 2, y = 5, label = \"a\")")){
        # before the fix: "INTERNAL ERROR 5" (length(output$data) AND length(coord_names) MUST BE IDENTICAL)
        output_lay <- saferGG::gg_scatter(data1 = obs_lay, x = "a", y = "b", add = tempo_add, plot = FALSE, return = TRUE, warn_print = FALSE, safer_check = FALSE)
        testthat::expect_type(object = output_lay, "list")
        output_lay <- saferGG::gg_scatter(data1 = base::list(obs_lay, obs_lay), x = base::list("a", "a"), y = base::list("b", "b"), categ = base::list("g", "g"), add = tempo_add, plot = FALSE, return = TRUE, warn_print = FALSE, safer_check = FALSE)
        testthat::expect_type(object = output_lay, "list")
    }
    # the add layer is the last layer of the plot and is not returned in $plot
    output_lay <- saferGG::gg_scatter(data1 = obs_lay, x = "a", y = "b", categ = "g", add = "+ ggplot2::geom_hline(yintercept = 3)", plot = FALSE, return = TRUE, return_ggplot = TRUE, warn_print = FALSE, safer_check = FALSE)
    testthat::expect_equal(object = base::length(x = output_lay$ggplot$layers), expected = base::length(x = output_ref$ggplot$layers) + 1L)
    testthat::expect_s3_class(object = output_lay$ggplot$layers[[base::length(x = output_lay$ggplot$layers)]]$geom, class = "GeomHline")
    testthat::expect_identical(object = base::names(x = output_lay$plot), expected = base::names(x = output_ref$plot))
    testthat::expect_identical(object = output_lay$plot[[1]], expected = output_ref$plot[[1]])
    # with facets, the add layer is drawn in each panel
    output_lay <- saferGG::gg_scatter(data1 = obs_lay, x = "a", y = "b", add = "+ ggplot2::facet_grid(cols = ggplot2::vars(f)) + ggplot2::geom_hline(yintercept = 3)", plot = FALSE, return = TRUE, return_ggplot = TRUE, warn_print = FALSE, safer_check = FALSE)
    tempo_built <- ggplot2::ggplot_build(plot = output_lay$ggplot)
    testthat::expect_identical(object = base::as.integer(x = tempo_built$data[[base::length(x = tempo_built$data)]]$PANEL), expected = base::c(1L, 2L))
    while(base::length(x = grDevices::dev.list()) > 0){ grDevices::dev.off() }
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
