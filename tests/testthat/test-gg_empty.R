# tests for the gg_empty() function of the saferGG package

testthat::test_that("gg_empty draws an empty graph", {

    ## data argument values
    # none
    ## end data argument values

    ## initialization of tests
    tempo_dir <- base::tempdir()
    tempo_file <- base::file.path(tempo_dir, "test_gg_empty.pdf")
    grDevices::pdf(file = tempo_file)
    ## end initialization of tests

    ## tests
    testthat::expect_no_error(object = saferGG::gg_empty(text = "TEST EMPTY GRAPH", safer_check = FALSE))
    testthat::expect_no_error(object = saferGG::gg_empty(text = "TEST EMPTY GRAPH", title = "TITLE", safer_check = FALSE))
    ## end tests

    ## errors
    # bad text
    testthat::expect_error(object = saferGG::gg_empty(text = 1, safer_check = FALSE), regexp = "MUST BE", fixed = TRUE)
    # bad text_size
    testthat::expect_error(object = saferGG::gg_empty(text = "TEST", text_size = "BAD", safer_check = FALSE), regexp = "MUST BE", fixed = TRUE)
    # zero-arg call: the arg_user_setting_eval else path (defaults returned)
    res_zero <- saferGG::gg_empty(safer_check = FALSE)
    testthat::expect_s3_class(object = res_zero, class = "ggplot")
    ## end errors
    grDevices::dev.off()
    base::unlink(x = tempo_file)
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
