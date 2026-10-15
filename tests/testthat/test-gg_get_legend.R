# tests for the gg_get_legend() function of the saferGG package

testthat::test_that("gg_get_legend extracts the legend", {

    ## data argument values
    # none
    ## end data argument values

    ## initialization of tests
    tempo_dir <- base::tempdir()
    tempo_file <- base::file.path(tempo_dir, "test_gg_get_legend.pdf")
    grDevices::pdf(file = tempo_file)
    plot1 <- ggplot2::ggplot(data = base::data.frame(x = 1:6, y = 1:6, grp = base::rep(x = c("A", "B", "C"), times = 2), stringsAsFactors = TRUE), mapping = ggplot2::aes(x = x, y = y, color = grp)) + ggplot2::geom_point()
    built1 <- ggplot2::ggplot_build(plot = plot1)
    ## end initialization of tests

    ## tests
    # legend present -> grob returned
    legend1 <- saferGG::gg_get_legend(ggplot_built = built1, safer_check = FALSE)
    testthat::expect_s3_class(legend1, "grob")
    # no legend -> NULL or empty plot depending on the ggplot2 version
    plot2 <- ggplot2::ggplot(data = base::data.frame(x = 1:6, y = 1:6, stringsAsFactors = TRUE), mapping = ggplot2::aes(x = x, y = y)) + ggplot2::geom_point()
    built2 <- ggplot2::ggplot_build(plot = plot2)
    legend2 <- saferGG::gg_get_legend(ggplot_built = built2, safer_check = FALSE)
    testthat::expect_true(object = base::is.null(x = legend2) | base::inherits(x = legend2, what = "grob"))
    ## end tests

    ## errors
    # ggplot_built wrong class
    testthat::expect_error(object = saferGG::gg_get_legend(ggplot_built = "BAD", safer_check = FALSE), regexp = "MUST BE", fixed = TRUE)
    # zero-arg call: mandatory/no-default block
    res_zero <- base::tryCatch(
        expr = saferGG::gg_get_legend(),
        error = function(e){base::conditionMessage(e)}
    )
    testthat::expect_true(object = base::grepl(x = res_zero, pattern = "NO DEFAULT VALUE AND REQUIRE ONE", fixed = TRUE))
    testthat::expect_true(object = base::grepl(x = res_zero, pattern = "ggplot_built", fixed = TRUE))
    ## end errors
    # cleanup of all files and devices created during the tests
    while(base::length(x = grDevices::dev.list()) > 0){ grDevices::dev.off() }
    base::unlink(x = tempo_dir, recursive = TRUE, force = TRUE)
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
