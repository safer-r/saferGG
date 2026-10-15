# tests for the internal functions of the saferGG package
# saferDev:::.pack_and_function_check() and saferDev:::.base_op_check() are checked through
# the safer_check = TRUE pathway of the migrated saferGG functions, because internal
# functions must not be called with ::: from test files (CRAN NOTE avoidance). Here the
# ::: calls are accepted because this is a test file of the package itself.

test_that("test .internal_function.R", {

    # .pack_and_function_check()
    fun_wrong1 <- "geom_point"  # incorrect input
    fun_wrong2 <- "ggplot2::non_existent_function"  # incorrect function name
    fun_good <- "ggplot2::geom_point" # correct input
    path_wrong <- "path/to/library"  # incorrect input
    path_good <- NULL  # incorrect input

    pack_and_function_check <- utils::getFromNamespace(x = ".pack_and_function_check", ns = "saferDev")
    base_op_check <- utils::getFromNamespace(x = ".base_op_check", ns = "saferDev")

    testthat::expect_no_error(pack_and_function_check(
        fun = fun_good, 
        lib_path = path_good, 
        error_text = ""
    ))
    testthat::expect_error(pack_and_function_check(
        fun = fun_wrong1, 
        lib_path = path_good, 
        error_text = ""
    ))
    testthat::expect_error(pack_and_function_check(
        fun = fun_wrong2, 
        lib_path = path_good, 
        error_text = ""
    ))
    testthat::expect_error(pack_and_function_check(
        fun = fun_good, 
        lib_path = path_wrong, 
        error_text = ""
    ))

    # .base_op_check()
    testthat::expect_no_error(base_op_check(
        error_text = ""
    ))

})
