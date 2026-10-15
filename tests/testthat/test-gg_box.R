# tests for the gg_box() function of the saferGG package

testthat::test_that("gg_box draws boxplots and returns the expected structure", {

    ## data argument values
    obs1 <- base::data.frame(y = stats::rnorm(n = 30, mean = 10, sd = 2), grp = base::rep(x = c("A", "B", "C"), times = 10), stringsAsFactors = TRUE)
    ## end data argument values

    ## initialization of tests
    tempo_dir <- base::tempdir()
    tempo_file <- base::file.path(tempo_dir, "test_gg_box.pdf")
    grDevices::pdf(file = tempo_file)
    ## end initialization of tests

    ## tests
    # basic boxplot drawn
    testthat::expect_no_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y.tick.nb = 5L, safer_check = FALSE))
    # return structure
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y.tick.nb = 5L, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::all(base::c("data", "removed.row.nb", "removed.rows", "plot", "panel", "axes", "warn", "ggplot", "gtable") %in% base::names(x = output)))
    # dots added
    testthat::expect_no_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = "same", safer_check = FALSE))
    # mean dot
    testthat::expect_no_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box.mean = TRUE, safer_check = FALSE))
    # grouped boxes (2 categ columns)
    obs2g <- base::data.frame(y = base::c(1.5, 2.3, 0.9, 3.1, 2.7, 1.1, 2.9, 3.3, 1.7, 2.5), grp1 = base::factor(x = base::rep(x = c("A", "B"), times = 5)), grp2 = base::factor(x = base::c("X", "X", "Y", "Y", "Z", "Z", "X", "X", "Y", "Y")), num = 1:10, ch = base::c("p", "q", "p", "q", "p", "q", "p", "q", "p", "q"), stringsAsFactors = TRUE)
    output <- saferGG::gg_box(data1 = obs2g, y = "y", categ = base::c("grp1", "grp2"), return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # box.legend.name
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box.legend.name = "BL", return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # categ.color character vector
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ.color = base::c("red", "blue", "green"), return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # categ.color integer vector
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ.color = base::c(1L, 2L, 3L), return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # box.fill = FALSE
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box.fill = FALSE, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # box.fill = TRUE: polygon boxes, whiskers, mean diamond, median
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box.fill = TRUE, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::any(output$plot$main.box$group != 0))
    # box.fill = TRUE with whisker edges and no mean
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box.fill = TRUE, box.mean = FALSE, box.whisker.width = 0.5, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = "sup.whisker.edge" %in% base::names(output$plot))
    # box.fill = TRUE with 2 categ columns: polygon with grouped boxes
    output <- saferGG::gg_box(data1 = obs2g, y = "y", categ = base::c("grp1", "grp2"), box.fill = TRUE, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # box.fill = FALSE with whisker edges (box.whisker.width > 0)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box.fill = FALSE, box.whisker.width = 0.5, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = "sup.whisker.edge" %in% base::names(output$plot))
    # dot display sub-branches: dot.border.size = 0 with and without dot.categ
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.border.size = 0, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    obs_dotc <- obs1
    obs_dotc$dotc <- base::rep(x = c("P", "Q"), times = 15)
    output <- saferGG::gg_box(data1 = obs_dotc, y = "y", categ = "grp", dot.color = base::c("red", "green"), dot.categ = "dotc", dot.border.size = 0, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    output <- saferGG::gg_box(data1 = obs_dotc, y = "y", categ = "grp", dot.color = base::c("red", "green"), dot.categ = "dotc", dot.border.color = "black", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # box.notch
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box.notch = TRUE, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # dot.color character vector
    output <- saferGG::gg_box(data1 = obs2g, y = "y", categ = base::c("grp1", "grp2"), dot.color = base::c("red", "blue", "green"), return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # dot.categ with dot.color
    output <- saferGG::gg_box(data1 = obs2g, y = "y", categ = "grp1", dot.color = base::c("red", "blue", "green"), dot.categ = "grp2", return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # dot.categ identical to last categ: allowed
    output <- saferGG::gg_box(data1 = obs2g, y = "y", categ = base::c("grp1", "grp2"), dot.color = base::c("red", "blue", "green"), dot.categ = "grp2", return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # dot.tidy
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = "same", dot.tidy = TRUE, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # dot.jitter and dot_seed
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = "same", dot.jitter = 0.2, dot_seed = 1L, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # dot.border.color
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = "same", dot.border.color = "red", return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # x.lab as expression
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", x.lab = base::expression(x = paste("LAB")), return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # y.lim
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y.lim = base::c(0, 5), return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # y.log
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y.log = "log10", return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # y.log forces vertical = TRUE (warn branch)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", vertical = FALSE, y.log = "log10", return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "CANNOT FLIP BOXES HORIZONTALLY WITH A Y.LOG SCALE", fixed = TRUE))
    # vertical = FALSE (horizontal boxes); y.tick.nb set explicitly to make the tick count deterministic (dot jitter can shift the y-range)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", vertical = FALSE, y.tick.nb = 5L, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # y.tick.nb and y.second.tick.nb
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y.tick.nb = 5L, y.second.tick.nb = 2L, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # stat.pos top / stat.mean
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", stat.pos = "top", stat.mean = TRUE, return = TRUE, plot = FALSE, safer_check = FALSE)
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
    # categ.class.order provided
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ.class.order = base::list(base::c("C", "B", "A")), return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # dot.color integer vector with a categ column: integers converted to colors
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = base::c(1L, 2L, 3L), return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # dot.border.color integer
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = "same", dot.border.color = 3L, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # dot.color one color per class of categ (length == number of classes) without dot.categ
    output <- saferGG::gg_box(data1 = obs2g, y = "y", categ = "grp1", dot.color = base::c("red", "blue"), return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # dot.color single string with dot.categ
    output <- saferGG::gg_box(data1 = obs2g, y = "y", categ = "grp1", dot.color = "red", dot.categ = "grp2", return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # dot.color with dot.categ: one color per class of dot.categ
    output <- saferGG::gg_box(data1 = obs2g, y = "y", categ = "grp1", dot.color = base::c("red", "blue", "green"), dot.categ = "grp2", return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # dot.color NULL with dot.categ
    output <- saferGG::gg_box(data1 = obs2g, y = "y", categ = "grp1", dot.categ = "grp2", return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # dot.categ.class.order without dot.color: accepted
    output <- saferGG::gg_box(data1 = obs2g, y = "y", categ = "grp1", dot.categ.class.order = base::c("X", "Y", "Z"), return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # categ.class.order with a NULL compartment (2 categ columns)
    output <- saferGG::gg_box(data1 = obs2g, y = "y", categ = base::c("grp1", "grp2"), categ.class.order = base::list(NULL, base::c("X", "Y", "Z")), return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # stat.pos = "above" (second valid option)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", stat.mean = TRUE, stat.pos = "above", return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # stat.angle
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", stat.mean = TRUE, stat.angle = 45, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # x.angle
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", x.angle = 45, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # 2 categ with stat.mean
    output <- saferGG::gg_box(data1 = obs2g, y = "y", categ = base::c("grp1", "grp2"), stat.mean = TRUE, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # facet by a column different from categ + dot.tidy
    output <- saferGG::gg_box(data1 = obs2g, y = "y", categ = "grp1", dot.color = "same", dot.tidy = TRUE, add = "+ggplot2::facet_wrap(~ch)", return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # box.line.size
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box.line.size = 2, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # box.space with 2 categ
    output <- saferGG::gg_box(data1 = obs2g, y = "y", categ = base::c("grp1", "grp2"), box.space = 0.5, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # y.log = "log2" with tick numbers
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y.log = "log2", y.tick.nb = 3L, y.second.tick.nb = 2L, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # dot.alpha
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = "same", dot.alpha = 0.3, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # warn.print with dot.color = "same" (dot color warning printed)
    testthat::expect_warning(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = "same", warn.print = TRUE, plot = FALSE, safer_check = FALSE), regexp = "dot.color ARGUMENT HAS BEEN SET TO \"same\"", fixed = TRUE)
    # stat.mean with stat.pos = "top" combined with box.mean
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box.mean = TRUE, stat.mean = TRUE, return = TRUE, plot = FALSE, safer_check = FALSE)
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
    # categ.color with 0 among integers
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ.color = base::c(0L, 2L), plot = FALSE, safer_check = FALSE), regexp = "categ.color ARGUMENT MUST BE A FACTOR OR CHARACTER VECTOR OR POSITVE INTEGER VECTOR", fixed = TRUE)
    # categ.color with bad color names
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ.color = base::c("red", "BAD"), plot = FALSE, safer_check = FALSE), regexp = "categ.color ARGUMENT MUST BE A HEXADECIMAL COLOR VECTOR", fixed = TRUE)
    # box.whisker.kind bad option
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box.whisker.kind = "BAD", plot = FALSE, safer_check = FALSE), regexp = "box.whisker.kind ARGUMENT MUST BE SOME OF THESE OPTIONS", fixed = TRUE)
    # box.whisker.kind = "std" with box.whisker.width out of [0, 1]
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box.whisker.kind = "std", box.whisker.width = 1.5, plot = FALSE, safer_check = FALSE), regexp = "box.whisker.width ARGUMENT MUST BE DECIMAL VALUES BETWEEN 0 AND 1", fixed = TRUE)
    # dot.tidy.bin.nb = 0
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = "same", dot.tidy = TRUE, dot.tidy.bin.nb = 0L, plot = FALSE, safer_check = FALSE), regexp = "dot.tidy.bin.nb ARGUMENT MUST BE A NON-NULL AND POSITVE INTEGER VALUE", fixed = TRUE)
    # y.tick.nb negative
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y.tick.nb = -1L, plot = FALSE, safer_check = FALSE), regexp = "y.tick.nb ARGUMENT MUST BE A NON NULL POSITIVE INTEGER", fixed = TRUE)
    # y.log with non-positive y.lim
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y.log = "log10", y.lim = base::c(-1, 5), plot = FALSE, safer_check = FALSE), regexp = "y.lim ARGUMENT CANNOT HAVE ZERO OR NEGATIVE VALUES WITH THE y.log ARGUMENT", fixed = TRUE)
    # stat.pos bad option
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", stat.pos = 1.1, plot = FALSE, safer_check = FALSE), regexp = "stat.pos ARGUMENT MUST BE SOME OF THESE OPTIONS", fixed = TRUE)
    # add not finished by )
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", add = "+ggplot2::coord_flip(", plot = FALSE, safer_check = FALSE), regexp = "MUST FINISH BY", fixed = TRUE)
    # add without ggplot2::/lemon:: prefix
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", add = "+coord_flip()", plot = FALSE, safer_check = FALSE), regexp = "MUST CONTAIN", fixed = TRUE)
    # add with facet_wrap but bad facet variable
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", add = "+ggplot2::facet_wrap(~BADFACET)", plot = FALSE, safer_check = FALSE), regexp = "PROBLEM OF VARIABLE DETECTION", fixed = TRUE)
    # y non numeric column: reserved-word rename happens first, then the internal consistency check fires
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "grp", categ = "grp", plot = FALSE, safer_check = FALSE), regexp = "INTERNAL CODE ERROR IN gg_box", fixed = TRUE)
    # dot.color with 0 among integers
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = base::c(0L, 1L), dot.categ = "grp", plot = FALSE, safer_check = FALSE), regexp = "dot.color MUST BE A FACTOR OR CHARACTER VECTOR OR POSITVE INTEGER VECTOR", fixed = TRUE)
    # dot.color integer vector with wrong length vs categ classes
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = base::c(1L, 2L), plot = FALSE, safer_check = FALSE), regexp = "dot.color ARGUMENT MUST BE (1) LENGTH 1", fixed = TRUE)
    # dot.color with bad color values
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = "BAD", plot = FALSE, safer_check = FALSE), regexp = "dot.color ARGUMENT MUST BE (1) A HEXADECIMAL COLOR VECTOR", fixed = TRUE)
    # dot.border.color with bad color value
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = "same", dot.border.color = "BAD", plot = FALSE, safer_check = FALSE), regexp = "dot.border.color ARGUMENT MUST BE (1) A HEXADECIMAL COLOR STRING", fixed = TRUE)
    # y.lab neither character nor expression
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y.lab = 12, plot = FALSE, safer_check = FALSE), regexp = "y.lab ARGUMENT MUST BE A SINGLE CHARACTER STRING OR EXPRESSION", fixed = TRUE)
    # y.lim with infinite values
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y.lim = base::c(0, Inf), plot = FALSE, safer_check = FALSE), regexp = "THE y.lim ARGUMENT MUST BE MADE OF NON INFINITE VALUES", fixed = TRUE)
    # dot.categ.class.order with duplicated classes
    testthat::expect_error(object = saferGG::gg_box(data1 = obs2g, y = "y", categ = "grp1", dot.color = base::c("red", "blue"), dot.categ = "grp2", dot.categ.class.order = base::c("X", "X"), plot = FALSE, safer_check = FALSE), regexp = "dot.categ.class.order ARGUMENT CANNOT HAVE DUPLICATED CLASSES", fixed = TRUE)
    # dot.categ.class.order with values absent from dot.categ
    testthat::expect_error(object = saferGG::gg_box(data1 = obs2g, y = "y", categ = "grp1", dot.color = base::c("red", "blue"), dot.categ = "grp2", dot.categ.class.order = base::c("X", "Q"), plot = FALSE, safer_check = FALSE), regexp = "dot.categ.class.order ARGUMENT MUST BE CLASSES OF dot.categ ARGUMENT", fixed = TRUE)
    # categ.class.order list length != length(categ)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs2g, y = "y", categ = base::c("grp1", "grp2"), categ.class.order = base::list(base::c("A", "B")), plot = FALSE, safer_check = FALSE), regexp = "categ.class.order ARGUMENT MUST BE A LIST OF LENGTH EQUAL TO LENGTH OF categ", fixed = TRUE)
    # categ.class.order with duplicated classes
    testthat::expect_error(object = saferGG::gg_box(data1 = obs2g, y = "y", categ = "grp1", categ.class.order = base::list(base::c("A", "A")), plot = FALSE, safer_check = FALSE), regexp = "COMPARTMENT 1 OF categ.class.order ARGUMENT CANNOT HAVE DUPLICATED CLASSES", fixed = TRUE)
    # categ.class.order with values absent from the categ column
    testthat::expect_error(object = saferGG::gg_box(data1 = obs2g, y = "y", categ = "grp1", categ.class.order = base::list(base::c("A", "Q")), plot = FALSE, safer_check = FALSE), regexp = "COMPARTMENT 1 OF categ.class.order ARGUMENT MUST BE CLASSES OF ELEMENT 1 OF categ ARGUMENT", fixed = TRUE)
    # dot.color per-row length incorrectly associated to the classes of dot.categ
    testthat::expect_error(object = saferGG::gg_box(data1 = obs2g, y = "y", categ = "grp1", dot.color = base::c("red", "blue", "green", "red", "blue", "green", "red", "blue", "green", "red"), dot.categ = "grp2", plot = FALSE, safer_check = FALSE), regexp = "dot.color ARGUMENT IS INCORRECTLY ASSOCIATED TO EACH CLASS OF dot.categ", fixed = TRUE)
    # stat.pos bad option (only "top" and "above" accepted)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", stat.mean = TRUE, stat.pos = "center", plot = FALSE, safer_check = FALSE), regexp = "THE stat.pos ARGUMENT MUST BE SOME OF THESE OPTIONS", fixed = TRUE)
    # box.space > 1
    testthat::expect_error(object = saferGG::gg_box(data1 = obs2g, y = "y", categ = base::c("grp1", "grp2"), box.space = 1.5, plot = FALSE, safer_check = FALSE), regexp = "THE box.space ARGUMENT MUST BE DECIMAL VALUES BETWEEN 0 AND 1", fixed = TRUE)
    # dot.size length > 1
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = "same", dot.size = base::c(1, 3), plot = FALSE, safer_check = FALSE), regexp = "THE dot.size ARGUMENT MUST BE LENGTH 1", fixed = TRUE)
    # dot.size negative
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = "same", dot.size = -1, plot = FALSE, safer_check = FALSE), regexp = "THE dot.size ARGUMENT MUST BE MADE OF NON NEGATIVE VALUES", fixed = TRUE)
    # dot.alpha > 1
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = "same", dot.alpha = 2, plot = FALSE, safer_check = FALSE), regexp = "THE dot.alpha ARGUMENT MUST BE DECIMAL VALUES BETWEEN 0 AND 1", fixed = TRUE)
    # dot.border.size negative
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = "same", dot.border.size = -1, plot = FALSE, safer_check = FALSE), regexp = "THE dot.border.size ARGUMENT MUST BE MADE OF NON NEGATIVE VALUES", fixed = TRUE)
    # box.line.size negative
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box.line.size = -1, plot = FALSE, safer_check = FALSE), regexp = "THE box.line.size ARGUMENT MUST BE MADE OF NON NEGATIVE VALUES", fixed = TRUE)
    # categ.color factor: must be colors or column name (internal error message)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ.color = base::factor(x = base::c("A", "B", "C")), plot = FALSE, safer_check = FALSE), regexp = "categ.color ARGUMENT MUST BE A HEXADECIMAL COLOR VECTOR STARTING BY # AND/OR COLOR NAMES GIVEN BY grDevices::colors() OR A COLUMN NAME OF THE data1 PARAMETER", fixed = TRUE)
    # categ.color integer with zero value: internal error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ.color = base::c(0L, 1L), plot = FALSE, safer_check = FALSE), regexp = "categ.color ARGUMENT MUST BE A FACTOR OR CHARACTER VECTOR OR POSITVE INTEGER VECTOR", fixed = TRUE)
    # dot.color factor: internal error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = base::factor(x = base::c("A", "B", "C")), dot.categ = "grp", plot = FALSE, safer_check = FALSE), regexp = "dot.color ARGUMENT MUST BE (1) A HEXADECIMAL COLOR VECTOR STARTING BY #, OR (2) COLOR NAMES GIVEN BY grDevices::colors(), OR (3) INTEGERS, OR THE STRING", fixed = TRUE)
    # dot.color integer with zero value: internal error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = base::c(0L, 1L), dot.categ = "grp", plot = FALSE, safer_check = FALSE), regexp = "dot.color MUST BE A FACTOR OR CHARACTER VECTOR OR POSITVE INTEGER VECTOR", fixed = TRUE)
    # dot.legend.name non character
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.legend.name = 12, plot = FALSE, safer_check = FALSE), regexp = "THE dot.legend.name ARGUMENT MUST BE TYPEOF character", fixed = TRUE)
    # dot.legend.name valid: dot legend built
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = base::c("red", "blue", "green"), dot.categ = "grp", dot.legend.name = "DL", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # dot_seed valid and invalid
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = "same", dot.jitter = 0.2, dot_seed = 42L, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_seed = 1.5, plot = FALSE, safer_check = FALSE), regexp = "THE dot_seed ARGUMENT MUST BE CLASS integer AND TYPEOF integer", fixed = TRUE)
    # dot.tidy.bin.nb: zero and bad class
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = "same", dot.tidy = TRUE, dot.tidy.bin.nb = 0L, plot = FALSE, safer_check = FALSE), regexp = "dot.tidy.bin.nb ARGUMENT MUST BE A NON-NULL AND POSITVE INTEGER VALUE", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.tidy.bin.nb = 2.5, plot = FALSE, safer_check = FALSE), regexp = "THE dot.tidy.bin.nb ARGUMENT MUST BE CLASS integer AND TYPEOF integer", fixed = TRUE)
    # y.second.tick.nb / stat.pos / legend.width explicitly NULL (checked_arg_names branches)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y.second.tick.nb = NULL, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", stat.mean = TRUE, stat.pos = NULL, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", legend.width = NULL, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # dot.border.color bad color name and bad integer
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = "same", dot.border.color = "colc", plot = FALSE, safer_check = FALSE), regexp = "dot.border.color ARGUMENT MUST BE (1) A HEXADECIMAL COLOR STRING STARTING BY #, OR (2) A COLOR NAME GIVEN BY grDevices::colors(), OR (3) AN INTEGER VALUE", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = "same", dot.border.color = base::c(1L, 2L), plot = FALSE, safer_check = FALSE), regexp = "dot.border.color ARGUMENT MUST BE (1) A HEXADECIMAL COLOR STRING STARTING BY #, OR (2) A COLOR NAME GIVEN BY grDevices::colors(), OR (3) AN INTEGER VALUE", fixed = TRUE)
    # dot.categ with several columns and with a bad column
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = base::c("red", "blue", "green"), dot.categ = base::c("grp", "grp2"), plot = FALSE, safer_check = FALSE), regexp = "THE dot.categ ARGUMENT MUST BE LENGTH 1", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.categ = "BAD", plot = FALSE, safer_check = FALSE), regexp = "dot.categ ARGUMENT MUST BE COLUMN NAMES OF data1. HERE IT IS", fixed = TRUE)
    # facet_grid on a single data frame: accepted
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", add = "+ggplot2::facet_grid(~grp)", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # character categ converted to factor with box.alpha non zero
    obs_chr <- obs1
    obs_chr$grp <- base::as.character(x = obs_chr$grp)
    output <- saferGG::gg_box(data1 = obs_chr, y = "y", categ = "grp", box.alpha = 0.5, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "THE CHARACTER", fixed = TRUE))
    # categ.color with NA: warning
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ.color = base::c("red", NA, "blue"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "categ.color ARGUMENT CONTAINS NA", fixed = TRUE))
    # categ.color length equal to both nrow and class number: attribution according to levels (warn)
    obs3 <- base::data.frame(y = base::c(1, 2, 3), grp = base::factor(x = base::c("A", "B", "C")), stringsAsFactors = TRUE)
    output <- saferGG::gg_box(data1 = obs3, y = "y", categ = "grp", categ.color = base::c("red", "blue", "green"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "BOX COLORS WILL BE ATTRIBUTED ACCORDING THE LEVELS OF", fixed = TRUE))
    # categ.color with the length of data1 rows, correct association: conversion warning
    obs2g2 <- base::data.frame(y = base::c(1, 2, 3, 4), grp = base::factor(x = base::c("A", "A", "B", "B")), stringsAsFactors = TRUE)
    output <- saferGG::gg_box(data1 = obs2g2, y = "y", categ = "grp", categ.color = base::c("red", "red", "blue", "blue"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "COLORS HAVE BEEN RESPECTIVELY ASSOCIATED TO EACH CLASS OF categ", fixed = TRUE))
    # categ.color with the length of data1 rows, wrong association: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs2g2, y = "y", categ = "grp", categ.color = base::c("red", "blue", "blue", "red"), plot = FALSE, safer_check = FALSE), regexp = "BUT IS INCORRECTLY ASSOCIATED TO EACH CLASS OF categ", fixed = TRUE)
    # categ.color length 1: same color for all classes warning (box.alpha != 0)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ.color = "red", box.alpha = 0.5, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "categ.color ARGUMENT HAS LENGTH 1, MEANING THAT ALL THE DIFFERENT CLASSES", fixed = TRUE))
    # categ.color wrong length: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ.color = base::c("red", "blue", "green", "orange"), plot = FALSE, safer_check = FALSE), regexp = "categ.color ARGUMENT MUST BE (1) LENGTH 1, OR (2) THE LENGTH OF data1 NROWS", fixed = TRUE)
    # dot.categ not a column of data1 (when dot.color non NULL): error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = base::c("red", "blue", "green"), dot.categ = "BADCOL", plot = FALSE, safer_check = FALSE), regexp = "dot.categ ARGUMENT MUST BE COLUMN NAMES OF data1. HERE IT IS:", fixed = TRUE)
    # dot.categ not a column of data1 (dot.color NULL): error from the dot.categ checking block
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.categ = "BAD", plot = FALSE, safer_check = FALSE), regexp = "dot.categ ARGUMENT MUST BE COLUMN NAMES OF data1. HERE IT IS:", fixed = TRUE)
    # dot.categ equal to a categ column: renamed with _DOT and warning
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = base::c("red", "blue", "green"), dot.categ = "grp", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "HAS BEEN REPLACED BY grp_DOT", fixed = TRUE))
    # dot.categ column neither factor nor character: internal error
    obs_num_dotc <- obs1
    obs_num_dotc$numdot <- base::c(1.5, 2.5, 1.5, 2.5, 1.5, 2.5, 1.5, 2.5, 1.5, 2.5)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs_num_dotc, y = "y", categ = "grp", dot.color = "same", dot.categ = "numdot", plot = FALSE, safer_check = FALSE), regexp = "WHEN dot.color ARGUMENT IS \"same\", THE COLUMN NAME IN dot.categ ARGUMENT MUST BE IDENTICAL TO THE LAST COLUMN NAME IN categ ARGUMENT. HERE IT IS:", fixed = TRUE)
    # dot.categ.class.order: duplicated classes, mismatch, and valid reorder
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = base::c("red", "blue", "green"), dot.categ = "grp", dot.categ.class.order = base::c("A", "A"), plot = FALSE, safer_check = FALSE), regexp = "dot.categ.class.order ARGUMENT CANNOT HAVE DUPLICATED CLASSES", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = base::c("red", "blue", "green"), dot.categ = "grp", dot.categ.class.order = base::c("A", "Q"), plot = FALSE, safer_check = FALSE), regexp = "dot.categ.class.order ARGUMENT MUST BE CLASSES OF dot.categ ARGUMENT", fixed = TRUE)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = base::c("red", "blue", "green"), dot.categ = "grp", dot.categ.class.order = base::c("B", "A", "C"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # dot.color "same" with dot.categ != last categ: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = "same", dot.categ = "grp2", plot = FALSE, safer_check = FALSE), regexp = "dot.categ ARGUMENT MUST BE COLUMN NAMES OF data1. HERE IT IS:", fixed = TRUE)
    # dot.color "same" with dot.categ = last categ and different order: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = "same", dot.categ = "grp", dot.categ.class.order = base::c("B", "A", "C"), plot = FALSE, safer_check = FALSE), regexp = "LAST COMPARTMENT OF categ.class.order ARGUMENT AND dot.categ.class.order ARGUMENT CANNOT BE DIFFERENT", fixed = TRUE)
    # dot.categ present in categ with different order: warning
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ.class.order = base::list(x = base::c("A", "B", "C")), dot.color = base::c("red", "blue", "green"), dot.categ = "grp", dot.categ.class.order = base::c("B", "A", "C"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "BUT ORDER OF THE CLASSES IS NOT THE SAME", fixed = TRUE))
    # dot.color with NA: warning
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = base::c("red", NA, "blue"), dot.categ = "grp", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "dot.color ARGUMENT CONTAINS NA", fixed = TRUE))
    # dot.color length equal to nrow AND levels of dot.categ: attribution by levels warning
    output <- saferGG::gg_box(data1 = obs3, y = "y", categ = "grp", dot.color = base::c("red", "blue", "green"), dot.categ = "grp", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "DOT COLORS WILL BE ATTRIBUTED ACCORDING THE LEVELS OF", fixed = TRUE))
    # dot.color wrong length vs dot.categ: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = base::c("red", "blue", "green", "orange"), dot.categ = "grp", plot = FALSE, safer_check = FALSE), regexp = "WHEN LENGTH OF THE dot.color ARGUMENT IS MORE THAN 1, IT MUST BE EQUAL TO THE NUMBER OF 1) ROWS OR 2) LEVELS OF dot.categ COLUMN", fixed = TRUE)
    # dot.color wrongly associated to dot.categ classes: error
    obs2g3 <- base::data.frame(y = base::c(1, 2, 3, 4), grp = base::factor(x = base::c("A", "A", "B", "B")), stringsAsFactors = TRUE)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs2g3, y = "y", categ = "grp", dot.color = base::c("red", "blue", "blue", "red"), dot.categ = "grp", plot = FALSE, safer_check = FALSE), regexp = "dot.color ARGUMENT IS INCORRECTLY ASSOCIATED TO EACH CLASS OF dot.categ", fixed = TRUE)
    # dot.color correctly associated to dot.categ classes: conversion warning
    output <- saferGG::gg_box(data1 = obs2g3, y = "y", categ = "grp", dot.color = base::c("red", "red", "blue", "blue"), dot.categ = "grp", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "HAVE BEEN ATTRIBUTED TO THESE CLASSES", fixed = TRUE))
    # dot.color length 1 (not "same") without dot.categ: same color warning
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = "red", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "dot.color ARGUMENT HAS LENGTH 1, MEANING THAT ALL THE DIFFERENT CLASSES", fixed = TRUE))
    # dot.color wrong length without dot.categ: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = base::c("red", "blue", "green", "orange"), plot = FALSE, safer_check = FALSE), regexp = "dot.color ARGUMENT MUST BE (1) LENGTH 1, OR (2) THE LENGTH OF data1 NROWS", fixed = TRUE)
    # dot.categ/dot.categ.class.order/dot.legend.name without dot.color: warning (no dot plotted)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = NULL, dot.categ = "grp", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "ARGUMENT HAS BEEN SPECIFIED BUT dot.color ARGUMENT IS NULL", fixed = TRUE))
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = NULL, dot.categ.class.order = base::c("A", "B", "C"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "ARGUMENT HAS BEEN SPECIFIED BUT dot.color ARGUMENT IS NULL", fixed = TRUE))
    # dot.color NULL + box.fill FALSE + dot.alpha almost 0: warning
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = NULL, box.fill = FALSE, dot.alpha = 0.01, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "POTENTIAL OUTLIER DOTS MIGHT NOT BE VISIBLE", fixed = TRUE))
    # dot.color NULL + box.fill FALSE + dot.border.size 0: error (invisible dots)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = NULL, box.fill = FALSE, dot.border.size = 0, plot = FALSE, safer_check = FALSE), regexp = "THESE SETTINGS ARE NOT ALLOWED BECAUSE THE POTENTIAL OUTLIER DOTS WILL NOT BE VISIBLE", fixed = TRUE)
    # integer dot.border.color: converted via gg_palette
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = "same", dot.border.color = 3L, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # categ.color integer with 0: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ.color = base::c(0L, 1L, 2L), plot = FALSE, safer_check = FALSE), regexp = "categ.color ARGUMENT MUST BE A FACTOR OR CHARACTER VECTOR OR POSITVE INTEGER VECTOR", fixed = TRUE)
    # dot.color integer with 0: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = base::c(0L, 1L, 2L), plot = FALSE, safer_check = FALSE), regexp = "dot.color MUST BE A FACTOR OR CHARACTER VECTOR OR POSITVE INTEGER VECTOR", fixed = TRUE)
    # duplicated column names of data1: error
    obs_dupcol <- obs1
    obs_dupcol <- base::cbind(obs_dupcol, obs_dupcol[, "y", drop = FALSE])
    base::names(obs_dupcol)[base::length(x = base::names(x = obs_dupcol))] <- "y"
    testthat::expect_error(object = saferGG::gg_box(data1 = obs_dupcol, y = "y", categ = "grp", plot = FALSE, safer_check = FALSE), regexp = "DUPLICATED COLUMN NAMES OF data1 ARGUMENT NOT ALLOWED:", fixed = TRUE)
    # y not numeric column: error
    obs_ycar <- obs1
    obs_ycar$y <- base::as.character(x = obs_ycar$y)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs_ycar, y = "y", categ = "grp", plot = FALSE, safer_check = FALSE), regexp = "THE y COLUMN OF data1 OBJECT MUST BE MODE numeric", fixed = TRUE)
    # dot.categ with more than 1 column name: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = base::c("red", "blue", "green"), dot.categ = base::c("grp", "grp"), plot = FALSE, safer_check = FALSE), regexp = "THE dot.categ ARGUMENT MUST BE LENGTH 1", fixed = TRUE)
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
    # negative y with y.log: new Inf values introduced with warning
    obs_yneg <- obs1
    obs_yneg$y[5] <- -1
    output <- saferGG::gg_box(data1 = obs_yneg, y = "y", categ = "grp", y.log = "log10", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "LOG CONVERSION INTRODUCED -Inf OR Inf OR NaN VALUES", fixed = TRUE))
    # y.lim with zero or negative values with y.log: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y.log = "log10", y.lim = base::c(-5, 20), plot = FALSE, safer_check = FALSE), regexp = "y.lim ARGUMENT CANNOT HAVE ZERO OR NEGATIVE VALUES WITH THE y.log ARGUMENT SET TO log10", fixed = TRUE)
    # y.include.zero with y.log: warning
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y.log = "log10", y.include.zero = TRUE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "y.log ARGUMENT SET TO log10 AND y.include.zero ARGUMENT SET TO TRUE", fixed = TRUE))
    # vertical = FALSE with y.log: warning and vertical reset to TRUE
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y.log = "log10", vertical = FALSE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "CANNOT FLIP BOXES HORIZONTALLY WITH A Y.LOG SCALE", fixed = TRUE))
    # NA categ.color: rows removed, class disappeared warnings and plot ok
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ.color = base::c("red", NA, "blue"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "categ.color ARGUMENT CONTAINS NA", fixed = TRUE))
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "THE FOLLOWING CLASSES HAVE DISAPPEARED AFTER NA/Inf REMOVAL", fixed = TRUE))
    # categ.class.order: wrong length, NULL compartment, non character compartment, duplicated classes: errors
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ.class.order = base::list(x = base::c("A", "B"), y = base::c("A", "B")), plot = FALSE, safer_check = FALSE), regexp = "categ.class.order ARGUMENT MUST BE A LIST OF LENGTH EQUAL TO LENGTH OF categ", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ.class.order = base::list(x = base::c("A", "B", "C", "D")), plot = FALSE, safer_check = FALSE), regexp = "MUST BE LENGTH 3", fixed = TRUE)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ.class.order = base::list(x = NULL), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "THE categ.class.order COMPARTMENT 1 IS NULL", fixed = TRUE))
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ.class.order = base::list(x = 1.5), plot = FALSE, safer_check = FALSE), regexp = "MUST BE MODE character", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ.class.order = base::list(x = base::c("A", "A", "A")), plot = FALSE, safer_check = FALSE), regexp = "COMPARTMENT 1 OF categ.class.order ARGUMENT CANNOT HAVE DUPLICATED CLASSES", fixed = TRUE)
    # dot.categ not a column of data1 with dot.color NULL: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = NULL, dot.categ = "BAD", plot = FALSE, safer_check = FALSE), regexp = "dot.categ ARGUMENT MUST BE COLUMN NAMES OF data1. HERE IT IS:", fixed = TRUE)
    # dot.categ not a column of data1 with dot.color non-NULL: error (message from the column names check at the same place)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = base::c("red", "blue", "green"), dot.categ = "badcol", plot = FALSE, safer_check = FALSE), regexp = "dot.categ ARGUMENT MUST BE COLUMN NAMES OF data1. HERE IT IS:", fixed = TRUE)
    # categ.color not factor/character/positive-integer (double): error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ.color = base::c(1.5, 2.5, 3.5), plot = FALSE, safer_check = FALSE), regexp = "categ.color ARGUMENT MUST BE A FACTOR OR CHARACTER VECTOR OR POSITVE INTEGER VECTOR", fixed = TRUE)
    # categ.color integer with 0: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ.color = base::c(0L, 1L, 2L), plot = FALSE, safer_check = FALSE), regexp = "categ.color ARGUMENT MUST BE A FACTOR OR CHARACTER VECTOR OR POSITVE INTEGER VECTOR", fixed = TRUE)
    # dot.color not factor/character/positive-integer (double): error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = base::c(1.5, 2.5, 3.5), plot = FALSE, safer_check = FALSE), regexp = "dot.color MUST BE A FACTOR OR CHARACTER VECTOR OR POSITVE INTEGER VECTOR", fixed = TRUE)
    # dot.color integer with 0: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = base::c(0L, 1L, 2L), plot = FALSE, safer_check = FALSE), regexp = "dot.color MUST BE A FACTOR OR CHARACTER VECTOR OR POSITVE INTEGER VECTOR", fixed = TRUE)
    # x.lab neither character nor expression: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", x.lab = 1.5, plot = FALSE, safer_check = FALSE), regexp = "x.lab ARGUMENT MUST BE A SINGLE CHARACTER STRING OR EXPRESSION", fixed = TRUE)
    # x.lab as a real expression object: ok
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", x.lab = base::expression(lab), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # y.second.tick.nb = 0: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y.second.tick.nb = 0L, plot = FALSE, safer_check = FALSE), regexp = "y.second.tick.nb ARGUMENT MUST BE A NON NULL POSITIVE INTEGER", fixed = TRUE)
    # dot_seed = NULL explicit: ok (checked_arg_names branch)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot_seed = NULL, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # empty string in categ.color: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ.color = "", plot = FALSE, safer_check = FALSE), regexp = "CANNOT CONTAIN EMPTY STRING \"\"", fixed = TRUE)
    # empty string in dot.color: error
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = "", plot = FALSE, safer_check = FALSE), regexp = "CANNOT CONTAIN EMPTY STRING \"\"", fixed = TRUE)
    # dot.categ column name is a reserved word: renamed with warning (dot.color length must match the 2 levels)
    obs_resv <- obs1
    obs_resv$x <- base::rep(x = c("P", "Q"), times = 15)
    output <- saferGG::gg_box(data1 = obs_resv, y = "y", categ = "grp", dot.color = base::c("red", "blue"), dot.categ = "x", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "IN dot.categ ARGUMENT (COLUMN NAMES OF data1 ARGUMENT),\nx HAS BEEN REPLACED BY x_modif", fixed = TRUE))
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
    base::names(obs_dup4)[3] <- "dot.color"
    obs_dup4$extra2 <- 1:30
    base::names(obs_dup4)[4] <- "dot.color"
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
    # dot.categ column neither factor nor character with dot.color length 3: error
    obs_num_dotc2 <- obs1
    obs_num_dotc2$numdot <- base::rep(x = 1.5, times = 30)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs_num_dotc2, y = "y", categ = "grp", dot.color = base::c("red", "blue", "green"), dot.categ = "numdot", plot = FALSE, safer_check = FALSE), regexp = "WHEN LENGTH OF THE dot.color ARGUMENT IS MORE THAN 1, IT MUST BE EQUAL TO THE NUMBER OF 1) ROWS OR 2) LEVELS OF dot.categ COLUMN", fixed = TRUE)
    # dot.color with the length of data1 nrows: attributed to rows with warning
    obs_dotc_nrow <- obs1
    obs_dotc_nrow$dotc <- base::rep(x = c("red", "blue", "green"), times = 10)
    output <- saferGG::gg_box(data1 = obs_dotc_nrow, y = "y", categ = "grp", dot.color = obs_dotc_nrow$dotc, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # NA categ.color with a single remaining color and more than 1 class: single color replicated with warning
    obs_singlecol <- obs1
    obs_singlecol$g2 <- base::factor(x = base::c(rep("A", times = 12), rep("B", times = 10), rep("C", times = 8)), levels = base::c("A", "B", "C"))
    output <- saferGG::gg_box(data1 = obs_singlecol, y = "y", categ = "g2", categ.color = base::c(NA, "blue", "green"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # NA dot.color with a single remaining color and more than 1 dot.categ class: single color replicated with warning
    output <- saferGG::gg_box(data1 = obs_singlecol, y = "y", categ = "g2", dot.color = base::c(NA, "blue", "green"), dot.categ = "g2", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # 2 of 3 classes removed by NA categ.color -> single color replicated via categ.class.order (length > 1)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ.color = base::c(NA, "blue", NA), categ.class.order = base::list(x = base::c("A", "B", "C")), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "IN COLUMN categ.color OF data1, THE FOLLOWING CLASSES HAVE DISAPPEARED", fixed = TRUE))
    # 1 of 3 classes removed by NA dot.color (nrow-form dot.color on a distinct dot.categ column) with dot.categ.class.order: single color replicated via dot.categ.class.order (length > 1)
    obs_dotc_lev <- obs1
    obs_dotc_lev$dotc <- base::factor(x = base::rep(x = c("A", "B", "C"), times = 10))
    output <- saferGG::gg_box(data1 = obs_dotc_lev, y = "y", categ = "grp", dot.color = base::ifelse(test = obs_dotc_lev$dotc == "A", yes = "red", no = base::ifelse(test = obs_dotc_lev$dotc == "B", yes = "blue", no = NA)), dot.categ = "dotc", dot.categ.class.order = base::c("A", "B", "C"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "IN COLUMN dot.color OF data1, THE FOLLOWING CLASSES HAVE DISAPPEARED", fixed = TRUE))
    # full class of the categ column removed by y-NA removal: class-disappearance warn fires for the categ column
    obs_blk <- base::data.frame(y = stats::rnorm(n = 30, mean = 10, sd = 2), grp = base::factor(x = base::rep(x = c("A", "B", "C"), each = 10)), stringsAsFactors = FALSE)
    obs_blk$y[1:10] <- NA
    output <- saferGG::gg_box(data1 = obs_blk, y = "y", categ = "grp", y.tick.nb = 5L, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "IN COLUMN grp OF data1, THE FOLLOWING CLASSES HAVE DISAPPEARED", fixed = TRUE))
    # full class of the categ column removed by y-NA removal + distinct dot.categ column: dot.categ class order rebuilt after removal (no error, correct colors per remaining class)
    obs_blk2 <- obs_blk
    obs_blk2$dotc <- base::factor(x = base::rep(x = c("P", "Q"), each = 15))
    output <- saferGG::gg_box(data1 = obs_blk2, y = "y", categ = "grp", dot.color = base::c("red", "blue"), dot.categ = "dotc", y.tick.nb = 5L, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # full class of the categ column removed by y-NA removal + facet column: facet levels rebuilt after removal (no error, plot ok)
    obs_blk3 <- obs_blk
    obs_blk3$fac <- base::factor(x = base::rep(x = c("F1", "F2"), each = 15))
    output <- saferGG::gg_box(data1 = obs_blk3, y = "y", categ = "grp", y.tick.nb = 5L, add = "+ ggplot2::facet_wrap(~fac)", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # zero-argument call: mandatory-args block lists all three arguments (no-default + mandatory checks)
    res_zeroargs <- base::tryCatch(
        expr = saferGG::gg_box(),
        error = function(e){base::conditionMessage(e)}
    )
    testthat::expect_true(object = base::grepl(x = res_zeroargs, pattern = "NO DEFAULT VALUE AND REQUIRE ONE", fixed = TRUE))
    testthat::expect_true(object = base::grepl(x = res_zeroargs, pattern = "data1", fixed = TRUE))
    testthat::expect_true(object = base::grepl(x = res_zeroargs, pattern = "categ", fixed = TRUE))
    # empty string in a character-mode argument: error from the empty-string block (categ.color and dot.color)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", categ.color = "", plot = FALSE, safer_check = FALSE), regexp = "THIS ARGUMENT
categ.color
CANNOT CONTAIN EMPTY STRING", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = "", plot = FALSE, safer_check = FALSE), regexp = "THIS ARGUMENT
dot.color
CANNOT CONTAIN EMPTY STRING", fixed = TRUE)
    # duplicated column names with NO reserved word among them: second duplicated-names check fires
    obs_dup5 <- obs1
    obs_dup5$extra <- 1:30
    base::names(obs_dup5)[2] <- "extra"
    testthat::expect_error(object = saferGG::gg_box(data1 = obs_dup5, y = "y", categ = "grp", plot = FALSE, safer_check = FALSE), regexp = "DUPLICATED COLUMN NAMES OF data1 ARGUMENT NOT ALLOWED", fixed = TRUE)
# theme/legend/limits variants
output_theme1 <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box.fill = FALSE, y.tick.nb = 5L, plot = FALSE, return = TRUE, safer_check = FALSE)
testthat::expect_type(output_theme1, "list")
output_theme2 <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box.alpha = 0.5, y.tick.nb = 5L, plot = FALSE, return = TRUE, safer_check = FALSE)
testthat::expect_type(output_theme2, "list")
output_theme3 <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box.alpha = 0, y.tick.nb = 5L, plot = FALSE, return = TRUE, safer_check = FALSE)
testthat::expect_type(output_theme3, "list")
output_theme4 <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", legend.width = 0.4, y.tick.nb = 5L, plot = FALSE, return = TRUE, safer_check = FALSE)
testthat::expect_type(output_theme4, "list")
output_theme5 <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y.log = "log10", y.tick.nb = 3L, y.second.tick.nb = 2L, plot = FALSE, return = TRUE, safer_check = FALSE)
testthat::expect_type(output_theme5, "list")
# tidy dots with dot.categ (regression: the binwidth-compartment detection used to crash with an INTERNAL CODE ERROR when a second blank geom_dotplot legend layer also carried binwidth)
obs_2cat <- base::data.frame(y = stats::rnorm(n = 30, mean = 10, sd = 2), grp = base::rep(x = c("A", "B", "C"), times = 10), sub = base::rep(x = c("u", "v"), times = 15), stringsAsFactors = TRUE)
output_tidycat <- saferGG::gg_box(data1 = obs_2cat, y = "y", categ = c("grp", "sub"), dot.tidy = TRUE, dot.categ = "sub", dot.tidy.bin.nb = 20L, plot = FALSE, return = TRUE, safer_check = FALSE)
testthat::expect_type(output_tidycat, "list")
    # pass-2 removal via categ.color NA removes all rows of one dot.categ class: dot.categ factor rebuilt so the alpha scale has matching labels (regression test)
    obs_dotc_na <- obs1
    obs_dotc_na$dotc <- base::factor(x = base::rep(x = c("P", "Q"), each = 15))
    obs_dotc_na$y[1:10] <- NA
    dc <- base::c(base::rep(x = NA_character_, times = 5), base::rep(x = "blue", times = 15))
    output <- saferGG::gg_box(data1 = obs_dotc_na, y = "y", categ = "grp", dot.color = dc, dot.categ = "dotc", dot.categ.class.order = base::c("P", "Q"), y.tick.nb = 5L, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "IN COLUMN dot.color OF data1, THE FOLLOWING CLASSES HAVE DISAPPEARED", fixed = TRUE))
    # single dot.color with a 2-class dot.categ while pass-2 removes a categ class: single-color replication (1717) fires
    obs_dotc_na2 <- obs1
    obs_dotc_na2$dotc <- base::factor(x = base::rep(x = c("P", "Q"), times = 15))
    output <- saferGG::gg_box(data1 = obs_dotc_na2, y = "y", categ = "grp", categ.color = base::c(NA, "green", "green"), dot.color = "blue", dot.categ = "dotc", y.tick.nb = 5L, plot = FALSE, return = TRUE, safer_check = FALSE)
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
    # box.alpha = 0: legend removed and ok
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", box.alpha = 0, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # stat.pos = "above" with dot.tidy = TRUE: ok
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.tidy = TRUE, stat.pos = "above", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # stat.pos = "above" with dot.color NULL: ok (stat on box whiskers)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = NULL, stat.pos = "above", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # stat.pos = "above" with stat.mean = TRUE: ok (MEAN as text center reference)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = NULL, stat.pos = "above", stat.mean = TRUE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # stat.pos = "above" with y.log: ok (text format conversion)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", dot.color = NULL, stat.pos = "above", y.log = "log10", plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # stat.pos = "BAD": error (stat.pos must be top or above)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", stat.pos = "BAD", plot = FALSE, safer_check = FALSE), regexp = "THE stat.pos ARGUMENT MUST BE SOME OF THESE OPTIONS", fixed = TRUE)
    # legend.show = FALSE: ok (legend inactivated)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", legend.show = FALSE, plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # y.tick.nb = 1 and y.second.tick.nb = 1: error (not enough ticks generated)
    testthat::expect_error(object = saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y.tick.nb = 1, y.second.tick.nb = 1, plot = FALSE, safer_check = FALSE), regexp = "THE NUMBER OF GENERATED TICKS FOR THE Y-AXIS IS NOT CORRECT", fixed = TRUE)
    # y.second.tick.nb = 3 with y.log = "no": ok (secondary ticks generated)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", y.second.tick.nb = 3, plot = FALSE, return = TRUE, safer_check = FALSE)
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
    # plot = TRUE with legend.width = NULL: ok (grid.arrange without legend)
    output <- saferGG::gg_box(data1 = obs1, y = "y", categ = "grp", plot = TRUE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # both NA/Inf removal passes fire: y NA rows removed (pass 1) AND NA in dot.color nlevels form (pass 2): no rbind error
    obs_dotna <- obs1
    obs_dotna$y[21:22] <- NA
    output <- saferGG::gg_box(data1 = obs_dotna, y = "y", categ = "grp", dot.color = base::c("red", NA, "blue"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_identical(object = base::ncol(output$removed.rows), expected = base::ncol(obs1)) # removed.rows keeps the data1.ini column set
    testthat::expect_identical(object = base::nrow(output$removed.rows), expected = 12L) # 2 rows (y NA) + 10 rows (dot.color NA in class B)
    # same with Inf in y (pass 1 at the Inf removal step) AND NA in dot.color (pass 2): class-disappearance warn for dot.color
    obs_dotna2 <- obs1
    obs_dotna2$y[1:10] <- Inf
    output <- saferGG::gg_box(data1 = obs_dotna2, y = "y", categ = "grp", dot.color = base::c("red", NA, "blue"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "IN COLUMN dot.color OF data1, THE FOLLOWING CLASSES HAVE DISAPPEARED", fixed = TRUE))
    # same with categ.color instead of dot.color: class-disappearance warn for categ.color
    obs_ccna <- obs1
    obs_ccna$y[11:14] <- NA
    output <- saferGG::gg_box(data1 = obs_ccna, y = "y", categ = "grp", dot.color = NULL, categ.color = base::c("red", NA, "blue"), plot = FALSE, return = TRUE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::grepl(x = output$warn, pattern = "IN COLUMN categ.color OF data1, THE FOLLOWING CLASSES HAVE DISAPPEARED", fixed = TRUE))
    ## end errors
    grDevices::dev.off()
    base::unlink(x = tempo_file)
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
