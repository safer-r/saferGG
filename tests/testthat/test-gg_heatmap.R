# tests for the gg_heatmap() function of the saferGG package

testthat::test_that("gg_heatmap draws heatmaps and returns the expected structure", {

    ## data argument values
    mat1 <- base::matrix(data = 1:16, ncol = 4)
    mat2 <- base::matrix(data = base::rep(x = base::c(1, 0, 0, 0), times = 4), ncol = 4)
    melted1 <- reshape2::melt(mat1)
    melted2 <- reshape2::melt(mat2)
    ## end data argument values

    ## initialization of tests
    tempo_dir <- base::tempdir()
    tempo_file <- base::file.path(tempo_dir, "test_gg_heatmap.png")
    grDevices::png(filename = tempo_file, width = 600, height = 600)
    ## end initialization of tests

    ## tests
    # matrix input, plot drawn
    testthat::expect_no_error(object = saferGG::gg_heatmap(data1 = mat1, title = "GRAPH 1", safer_check = FALSE))
    # return structure
    output <- saferGG::gg_heatmap(data1 = mat1, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::all(base::c("data", "axes", "scale", "warn") %in% base::names(x = output)))
    # mask matrix
    testthat::expect_no_error(object = saferGG::gg_heatmap(data1 = mat1, data2 = mat2, title = "MASK", safer_check = FALSE))
    # melted data frames
    output <- saferGG::gg_heatmap(data1 = melted1, data2 = melted2, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # rotate
    output <- saferGG::gg_heatmap(data1 = mat1, rotate = TRUE, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # rotate with melted data frames and mask, and data2 rotate path
    output <- saferGG::gg_heatmap(data1 = melted1, data2 = melted2, rotate = TRUE, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # limit1 provided (else branch of the NULL limit1 path), midpoint1 provided but different from mean of limit1 (warn branch), warn_print = TRUE
    output <- saferGG::gg_heatmap(data1 = mat1, limit1 = base::c(0, 16), midpoint1 = 8, warn_print = FALSE, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    output <- saferGG::gg_heatmap(data1 = mat1, limit1 = base::c(0, 16), midpoint1 = 2, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::any(base::grepl(x = output$warn, pattern = "COLOR SCALE IS NOT LINEAR", fixed = TRUE)))
    # limit1 NULL with Inf values in data1 (warn branch "CONTAINS -Inf OR Inf VALUES")
    mat_inf <- mat1
    mat_inf[1, 1] <- Inf
    output <- saferGG::gg_heatmap(data1 = mat_inf, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::any(base::grepl(x = output$warn, pattern = "CONTAINS -Inf OR Inf VALUES", fixed = TRUE)))
    # add with ggplot2::theme -> internal theme inactivated (warn branch)
    output <- saferGG::gg_heatmap(data1 = mat1, add = "+ggplot2::theme_void()", return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::any(base::grepl(x = output$warn, pattern = "ggplot2::theme\" STRING DETECTED", fixed = TRUE)))
    # add without ggplot2::theme: add string used in the final plot string (plot = TRUE branch with add)
    testthat::expect_no_error(object = saferGG::gg_heatmap(data1 = mat1, add = "+ggplot2::theme_void()", safer_check = FALSE))
    # data1 as data frame with NA values in column 3 (limit1 range with na.rm)
    melted_na <- melted1
    melted_na[1, 3] <- NA
    output <- saferGG::gg_heatmap(data1 = melted_na, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # data2 mask with invert2 = TRUE: alpha scale with c(alpha2, 0) values
    output <- saferGG::gg_heatmap(data1 = melted1, data2 = melted2, invert2 = TRUE, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(output, "list")
    # data1 (melted data frame) with NA only in column 3: caught by arg_check (NA in data) before the Inf-limit branch
    melted_na_all <- melted1
    melted_na_all[, 3] <- NA
    testthat::expect_error(object = saferGG::gg_heatmap(data1 = melted_na_all, safer_check = FALSE), regexp = "CANNOT BE MADE OF NA ONLY", fixed = TRUE)
    # data1 (matrix) with all Inf values: round2 of the computed Inf limit errors inside saferTool::round2
    mat_inf_all <- base::matrix(data = Inf, ncol = 4, nrow = 4)
    testthat::expect_error(object = saferGG::gg_heatmap(data1 = mat_inf_all, safer_check = FALSE), regexp = "MUST BE A VECTOR OF STRINGS OR A VECTOR OF NUMBERS", fixed = TRUE)
    ## end tests

    ## errors
    # bad data1
    testthat::expect_error(object = saferGG::gg_heatmap(data1 = "bad", safer_check = FALSE), regexp = "MUST BE A NUMERIC MATRIX OR A DATA FRAME", fixed = TRUE)
    # wrong mask dimension
    testthat::expect_error(object = saferGG::gg_heatmap(data1 = mat1, data2 = base::matrix(data = 1:25, ncol = 5), safer_check = FALSE), regexp = "MASK MATRIX", fixed = TRUE)
    # add without +
    testthat::expect_error(object = saferGG::gg_heatmap(data1 = mat1, add = "ggplot2::theme_void()", safer_check = FALSE), regexp = "MUST START WITH", fixed = TRUE)
    # bad color
    testthat::expect_error(object = saferGG::gg_heatmap(data1 = mat1, low_color1 = "notacolor", safer_check = FALSE), regexp = "MUST BE|NOT A", fixed = FALSE)
    # bad mid_color1, high_color1, color2 (same branch)
    testthat::expect_error(object = saferGG::gg_heatmap(data1 = mat1, mid_color1 = "notacolor", safer_check = FALSE), regexp = "mid_color1 ARGUMENT MUST BE A HEXADECIMAL COLOR VECTOR", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_heatmap(data1 = mat1, high_color1 = "notacolor", safer_check = FALSE), regexp = "high_color1 ARGUMENT MUST BE A HEXADECIMAL COLOR VECTOR", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_heatmap(data1 = mat1, color2 = "notacolor", safer_check = FALSE), regexp = "color2 ARGUMENT MUST BE A HEXADECIMAL COLOR VECTOR", fixed = TRUE)
    # limit1 with Inf values: caught by arg_check (inf_values = FALSE) BEFORE the dedicated branch
    testthat::expect_error(object = saferGG::gg_heatmap(data1 = mat1, limit1 = base::c(0, Inf), safer_check = FALSE), regexp ="MUST BE MADE OF NON INFINITE VALUES", fixed = TRUE)
    # add without ggplot2:: prefix
    testthat::expect_error(object = saferGG::gg_heatmap(data1 = mat1, add = "+theme_void()", safer_check = FALSE), regexp = "add ARGUMENT MUST CONTAIN \"ggplot2::\" IN FRONT OF EACH GGPLOT2 FUNCTION", fixed = TRUE)
    # add not finished by )
    testthat::expect_error(object = saferGG::gg_heatmap(data1 = mat1, add = "+ggplot2::theme_void(", safer_check = FALSE), regexp = "add ARGUMENT MUST FINISH BY \")\"", fixed = TRUE)
    # melted data2 with wrong third column values
    testthat::expect_error(object = saferGG::gg_heatmap(data1 = mat1, data2 = reshape2::melt(base::matrix(data = 1:16, ncol = 4)), safer_check = FALSE), regexp = "THIRD COLUMN OF DATA FRAME IN data2 MUST BE MADE OF 0 AND 1 ONLY", fixed = TRUE)
    # melted data2 with wrong row number vs matrix data1
    testthat::expect_error(object = saferGG::gg_heatmap(data1 = mat1, data2 = reshape2::melt(base::matrix(data = rep(x = c(1, 0), times = 4), ncol = 2)), safer_check = FALSE), regexp = "DATA FRAME IN data2 MUST HAVE ROW NUMBER EQUAL TO PRODUCT OF DIMENSION OF data1 MATRIX", fixed = TRUE)
    # matrix data2 with correct 0/1 values but wrong dimension vs data frame data1 (reshape2 vs matrix path)
    testthat::expect_error(object = saferGG::gg_heatmap(data1 = melted1, data2 = base::matrix(data = base::rep(x = c(1, 0), times = 4), ncol = 2), safer_check = FALSE), regexp = "DATA FRAME IN data2 MUST HAVE ROW NUMBER EQUAL TO PRODUCT OF DIMENSIONS OF data1 MATRIX", fixed = TRUE)
    # melted data1 and melted data2 with different dimensions
    testthat::expect_error(object = saferGG::gg_heatmap(data1 = melted1, data2 = reshape2::melt(base::matrix(data = rep(x = c(1, 0), times = 6), ncol = 3)), safer_check = FALSE), regexp = "DATA FRAME DIMENSION IN data2 MUST BE IDENTICAL TO DATA FRAME DIMENSION IN data1", fixed = TRUE)
    # data2 neither matrix nor data frame
    testthat::expect_error(object = saferGG::gg_heatmap(data1 = mat1, data2 = "bad", safer_check = FALSE), regexp = "THE data2 ARGUMENT MUST BE A NUMERIC MATRIX OR A DATA FRAME", fixed = TRUE)
    # alpha2 out of proportion
    testthat::expect_error(object = saferGG::gg_heatmap(data1 = mat1, data2 = mat1, alpha2 = 2, safer_check = FALSE), regexp = "BETWEEN 0 AND 1|PROPORTION|NOT IN", fixed = FALSE)
    # data2 mask matrix with wrong dimensions vs data1 matrix (uncovered 552-562 path)
    testthat::expect_error(object = saferGG::gg_heatmap(data1 = mat1, data2 = base::matrix(data = 1, nrow = 2, ncol = 2), safer_check = FALSE), regexp = "MATRIX DIMENSION IN data2 MUST BE IDENTICAL AS MATRIX DIMENSION IN data1", fixed = TRUE)
    # data2 data frame with wrong row number vs data1 data frame: the nrow check fires (3 != 25)
    testthat::expect_error(object = saferGG::gg_heatmap(data1 = melted1, data2 = base::data.frame(Var1 = base::rep(x = 1L, times = 3), Var2 = base::rep(x = 1L, times = 3), value = base::c(0, 1, 0), stringsAsFactors = TRUE), safer_check = FALSE), regexp = "DATA FRAME DIMENSION IN data2 MUST BE IDENTICAL TO DATA FRAME DIMENSION IN data1", fixed = TRUE)
    # zero-arg call: mandatory/no-default block (data1 has no default value)
    res_zero <- base::tryCatch(
        expr = saferGG::gg_heatmap(),
        error = function(e){base::conditionMessage(e)}
    )
    testthat::expect_true(object = base::grepl(x = res_zero, pattern = "NO DEFAULT VALUE AND REQUIRE ONE", fixed = TRUE))
    testthat::expect_true(object = base::grepl(x = res_zero, pattern = "data1", fixed = TRUE))
    ## end errors
    # warn_print = TRUE with a warn (limit1 NULL): on.exit warning
    output_wp <- saferGG::gg_heatmap(data1 = mat1, limit1 = NULL, warn_print = TRUE, return = TRUE, plot = FALSE, safer_check = FALSE)
    testthat::expect_type(object = output_wp, "list")
    testthat::expect_true(object = base::grepl(x = output_wp$warn, pattern = "THE limit1 ARGUMENT IS NULL", fixed = TRUE))
    # cleanup of all files and devices created during the tests
    while(base::length(x = grDevices::dev.list()) > 0){ grDevices::dev.off() }
    base::unlink(x = tempo_dir, recursive = TRUE, force = TRUE)
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
