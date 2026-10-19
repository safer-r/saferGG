# tests for the gg_point_rast() function of the saferGG package
# note: the raster drawing path requires the Cairo package: drawing tests are skipped if not available

testthat::test_that("gg_point_rast builds a raster layer", {

    ## data argument values
    obs1 <- base::data.frame(x = 1:20, y = (1:20)^2, stringsAsFactors = TRUE)
    ## end data argument values

    ## initialization of tests
    tempo_dir <- base::tempdir()
    tempo_file <- base::file.path(tempo_dir, "test_gg_point_rast.pdf")
    cairo_ok <- base::requireNamespace("Cairo", quietly = TRUE)
    grDevices::pdf(file = tempo_file)
    ## end initialization of tests

    ## tests
    # layer object returned
    layer1 <- saferGG::gg_point_rast(data = obs1, mapping = ggplot2::aes(x = x, y = y), safer_check = FALSE)
    testthat::expect_s3_class(layer1, "LayerInstance")
    # plot drawn with dot aesthetics (requires Cairo at draw time)
    if(cairo_ok == TRUE){
        testthat::expect_no_error(object = base::print(ggplot2::ggplot() + saferGG::gg_point_rast(data = obs1, mapping = ggplot2::aes(x = x, y = y), shape = 21, size = 2, stroke = 1, color = "black", alpha = 0.5, safer_check = FALSE)))
    }else{
        testthat::skip(message = "Cairo package not available for raster drawing")
    }
    ## end tests

    ## errors
    # bad na.rm
    testthat::expect_error(object = saferGG::gg_point_rast(data = obs1, mapping = ggplot2::aes(x = x, y = y), na_rm = 1, safer_check = FALSE), regexp = "MUST BE MODE logical", fixed = TRUE)
    # negative raster_width
    testthat::expect_error(object = saferGG::gg_point_rast(data = obs1, mapping = ggplot2::aes(x = x, y = y), raster_width = -1, safer_check = FALSE), regexp = "NEG", fixed = TRUE)
    # bad show.legend type
    testthat::expect_error(object = saferGG::gg_point_rast(data = obs1, mapping = ggplot2::aes(x = x, y = y), show_legend = "BAD", safer_check = FALSE), regexp = "MUST BE", fixed = TRUE)
    # bad mapping class (numeric list instead of aes)
    testthat::expect_error(object = saferGG::gg_point_rast(data = obs1, mapping = base::list(x = "BAD"), safer_check = FALSE), regexp = "MUST BE", fixed = TRUE)
    # fill argument of bad class (numeric)
    testthat::expect_error(object = saferGG::gg_point_rast(data = obs1, mapping = ggplot2::aes(x = x, y = y), fill = 1.5, safer_check = FALSE), regexp = "MUST BE", fixed = TRUE)
    # raster_height of bad class (character)
    testthat::expect_error(object = saferGG::gg_point_rast(data = obs1, mapping = ggplot2::aes(x = x, y = y), raster_height = "BAD", safer_check = FALSE), regexp = "MUST BE", fixed = TRUE)
    # zero-arg call: arg_user_setting_eval NULL path (a bare layer is returned)
    res_zero <- saferGG::gg_point_rast(safer_check = FALSE)
    testthat::expect_s3_class(object = res_zero, class = "LayerInstance")
    ## end errors
    # bare zero-argument call: arg_user_setting_eval NULL branch
    output_zero <- saferGG::gg_point_rast()
    testthat::expect_s3_class(object = output_zero, class = "LayerInstance")

    # cleanup of all files and devices created during the tests
    while(base::length(x = grDevices::dev.list()) > 0){ grDevices::dev.off() }
    base::unlink(x = tempo_dir, recursive = TRUE, force = TRUE)
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
