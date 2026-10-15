# tests for the gg_palette() function of the saferGG package

testthat::test_that("gg_palette returns the expected colors", {

    ## data argument values
    # none
    ## end data argument values

    ## tests
    # std palette
    output <- saferGG::gg_palette(n = 3, kind = "std", safer_check = FALSE)
    testthat::expect_type(output, "character")
    testthat::expect_equal(object = base::length(x = output), expected = 3)
    testthat::expect_equal(object = output[1], expected = "#F8766D")
    testthat::expect_equal(object = output[3], expected = "#619CFF")
    # dark palette
    output <- saferGG::gg_palette(n = 2, kind = "dark", safer_check = FALSE)
    testthat::expect_equal(object = base::length(x = output), expected = 2)
    testthat::expect_equal(object = output[1], expected = "#9F2108")
    # light palette
    output <- saferGG::gg_palette(n = 2, kind = "light", safer_check = FALSE)
    testthat::expect_equal(object = base::length(x = output), expected = 2)
    testthat::expect_equal(object = output[1], expected = "#FFAEA5")
    ## end tests

    ## errors
    # bad n
    testthat::expect_error(object = saferGG::gg_palette(n = "BAD", kind = "std", safer_check = FALSE), regexp = "MUST BE", fixed = TRUE)
    # bad kind
    testthat::expect_error(object = saferGG::gg_palette(n = 3, kind = "BAD", safer_check = FALSE), regexp = "MUST BE SOME OF THESE OPTIONS", fixed = TRUE)
    # zero-arg call: mandatory/no-default block
    res_zero <- base::tryCatch(
        expr = saferGG::gg_palette(),
        error = function(e){base::conditionMessage(e)}
    )
    testthat::expect_true(object = base::grepl(x = res_zero, pattern = "NO DEFAULT VALUE AND REQUIRE ONE", fixed = TRUE))
    testthat::expect_true(object = base::grepl(x = res_zero, pattern = "n", fixed = TRUE))
    ## end errors
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
