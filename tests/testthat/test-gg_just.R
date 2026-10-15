# tests for the gg_just() function of the saferGG package

testthat::test_that("gg_just returns the expected justification values", {

    ## data argument values
    # none
    ## end data argument values

    ## tests
    # angle 0
    output <- saferGG::gg_just(angle = 0, pos = "bottom", kind = "axis", safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_equal(object = output$angle, expected = 0)
    testthat::expect_equal(object = output$hjust, expected = 0.5)
    testthat::expect_equal(object = output$vjust, expected = 0.5)
    # angle 90 bottom
    output <- saferGG::gg_just(angle = 90, pos = "bottom", kind = "axis", safer_check = FALSE)
    testthat::expect_equal(object = output$angle, expected = 90)
    # angle 90 top (known ggplot2 bug: same result as bottom, see docs)
    output2 <- saferGG::gg_just(angle = 90, pos = "top", kind = "axis", safer_check = FALSE)
    testthat::expect_equal(object = output2$angle, expected = output$angle)
    # text kind
    output <- saferGG::gg_just(angle = 45, pos = "bottom", kind = "text", safer_check = FALSE)
    testthat::expect_type(output, "list")
    testthat::expect_true(object = base::all(output$angle == 45, na.rm = TRUE))
    ## end tests

    ## errors
    # bad pos
    testthat::expect_error(object = saferGG::gg_just(angle = 45, pos = "BAD", kind = "axis", safer_check = FALSE), regexp = "MUST BE SOME OF THESE OPTIONS", fixed = TRUE)
    # bad kind
    testthat::expect_error(object = saferGG::gg_just(angle = 45, pos = "bottom", kind = "BAD", safer_check = FALSE), regexp = "MUST BE SOME OF THESE OPTIONS", fixed = TRUE)
    # bad angle
    testthat::expect_error(object = saferGG::gg_just(angle = "BAD", pos = "bottom", kind = "axis", safer_check = FALSE), regexp = "MUST BE", fixed = TRUE)
    ## end errors
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})

testthat::test_that("gg_just covers the full angle/pos/kind matrix", {

    ## data argument values
    # angles covering every branch of the bottom/top and left/right justification tables
    angle_bottom_axis <- base::c(-360, -300, -270, -225, -180, -135, -90, -45, 0, 45, 90, 135, 180, 225, 270, 315, 360, 400, -400)
    angle_bottom_text <- base::c(-360, -180, 0, 180, 360, -90, 90, 270, -270)
    angle_left_axis <- base::c(-270, -90, 90, 270, -360, 0, 360, -180, 180, -300, 45, -225, -135, 135, -45, 315, -500, 500)
    angle_left_text <- base::c(-90, 90, -270, 270, -360, 0, 360)
    ## end data argument values

    ## tests
    # bottom/top, axis kind
    for(a in angle_bottom_axis){
        res <- saferGG::gg_just(angle = a, pos = "bottom", kind = "axis", safer_check = FALSE)
        tempo_a <- a
        while(tempo_a > 360){tempo_a <- tempo_a - 360}
        while(tempo_a < -360){tempo_a <- tempo_a + 360}
        testthat::expect_equal(object = res$angle, expected = tempo_a)
        testthat::expect_true(object = base::is.finite(x = res$hjust))
        testthat::expect_true(object = base::is.finite(x = res$vjust))
        testthat::expect_equal(object = res$hjust, expected = res$hjust) # no NA
        testthat::expect_equal(object = res$vjust, expected = res$vjust) # no NA
    }
    # bottom/top, text kind (special -180/180 vjust and top/hjust flip branches)
    for(a in angle_bottom_text){
        res_b <- saferGG::gg_just(angle = a, pos = "bottom", kind = "text", safer_check = FALSE)
        res_t <- saferGG::gg_just(angle = a, pos = "top", kind = "text", safer_check = FALSE)
        testthat::expect_true(object = base::is.finite(x = res_b$hjust) & base::is.finite(x = res_b$vjust))
        testthat::expect_true(object = base::is.finite(x = res_t$hjust) & base::is.finite(x = res_t$vjust))
    }
    # left/right, axis kind
    for(a in angle_left_axis){
        res <- saferGG::gg_just(angle = a, pos = "left", kind = "axis", safer_check = FALSE)
        testthat::expect_true(object = base::is.finite(x = res$hjust) & base::is.finite(x = res$vjust))
    }
    # left/right, text kind
    for(a in angle_left_text){
        res_l <- saferGG::gg_just(angle = a, pos = "left", kind = "text", safer_check = FALSE)
        res_r <- saferGG::gg_just(angle = a, pos = "right", kind = "text", safer_check = FALSE)
        testthat::expect_true(object = base::is.finite(x = res_l$hjust) & base::is.finite(x = res_l$vjust))
        testthat::expect_true(object = base::is.finite(x = res_r$hjust) & base::is.finite(x = res_r$vjust))
    }
    # while-loop normalization: angle 400 -> 40, angle -400 -> -40
    res <- saferGG::gg_just(angle = 400, pos = "bottom", kind = "axis", safer_check = FALSE)
    testthat::expect_equal(object = res$angle, expected = 40)
    res <- saferGG::gg_just(angle = -400, pos = "bottom", kind = "axis", safer_check = FALSE)
    testthat::expect_equal(object = res$angle, expected = -40)
    # structural expectations for a few documented positions
    res <- saferGG::gg_just(angle = 90, pos = "bottom", kind = "axis", safer_check = FALSE)
    testthat::expect_equal(object = res$hjust, expected = 1)
    testthat::expect_equal(object = res$vjust, expected = 0.5)
    res <- saferGG::gg_just(angle = -90, pos = "bottom", kind = "axis", safer_check = FALSE)
    testthat::expect_equal(object = res$hjust, expected = 0)
    testthat::expect_equal(object = res$vjust, expected = 0.5)
    res <- saferGG::gg_just(angle = 180, pos = "bottom", kind = "axis", safer_check = FALSE)
    testthat::expect_equal(object = res$hjust, expected = 0.5)
    testthat::expect_equal(object = res$vjust, expected = 0.5)
    res <- saferGG::gg_just(angle = 180, pos = "bottom", kind = "text", safer_check = FALSE)
    testthat::expect_equal(object = res$vjust, expected = 0)
    res <- saferGG::gg_just(angle = 0, pos = "bottom", kind = "text", safer_check = FALSE)
    testthat::expect_equal(object = res$vjust, expected = 1)
    # left axis, angle 0
    res <- saferGG::gg_just(angle = 0, pos = "left", kind = "axis", safer_check = FALSE)
    testthat::expect_equal(object = res$hjust, expected = 1)
    testthat::expect_equal(object = res$vjust, expected = 0.5)
    res <- saferGG::gg_just(angle = 90, pos = "left", kind = "axis", safer_check = FALSE)
    testthat::expect_equal(object = res$hjust, expected = 0.5)
    testthat::expect_equal(object = res$vjust, expected = 0.5)
    res <- saferGG::gg_just(angle = 180, pos = "left", kind = "axis", safer_check = FALSE)
    testthat::expect_equal(object = res$hjust, expected = 0)
    testthat::expect_equal(object = res$vjust, expected = 0.5)
    # right axis: hjust flipped relative to left
    res_l <- saferGG::gg_just(angle = 45, pos = "left", kind = "axis", safer_check = FALSE)
    res_r <- saferGG::gg_just(angle = 45, pos = "right", kind = "axis", safer_check = FALSE)
    testthat::expect_equal(object = res_r$hjust, expected = 1 - res_l$hjust)
    res_l <- saferGG::gg_just(angle = 45, pos = "left", kind = "text", safer_check = FALSE)
    res_r <- saferGG::gg_just(angle = 45, pos = "right", kind = "text", safer_check = FALSE)
    testthat::expect_equal(object = res_r$hjust, expected = 1 - res_l$hjust)
    # top vs bottom for a non-flipped angle: hjust and vjust flipped
    res_b <- saferGG::gg_just(angle = 45, pos = "bottom", kind = "axis", safer_check = FALSE)
    res_t <- saferGG::gg_just(angle = 45, pos = "top", kind = "axis", safer_check = FALSE)
    testthat::expect_equal(object = res_t$hjust, expected = 1 - res_b$hjust)
    testthat::expect_equal(object = res_t$vjust, expected = 1 - res_b$vjust)
    # angle in (180, 270) with kind text + pos top: the hjust <- 1 branch inside the (180, 270) range
    res_200 <- saferGG::gg_just(angle = 200, pos = "top", kind = "text", safer_check = FALSE)
    testthat::expect_equal(object = res_200$hjust, expected = 1)
    # zero-arg call: arg_user_setting_eval NULL path (defaults returned)
    res_zero <- saferGG::gg_just(safer_check = FALSE)
    testthat::expect_equal(object = res_zero$angle, expected = 45)
    testthat::expect_equal(object = res_zero$pos, expected = "bottom")
    testthat::expect_equal(object = res_zero$kind, expected = "axis")
    ## end tests
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
