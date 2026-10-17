# tests for the gg_miami() function of the saferGG package
# the function writes a miami.png file in the current directory: the tests run in a temporary directory

testthat::test_that("gg_miami draws manhattan and miami plots", {

    ## data argument values
    tempo_dir <- testthat::test_path()
    # small synthetic TSV
    n <- 60
    base::set.seed(seed = 1, kind = NULL, normal.kind = NULL, sample.kind = NULL) # to make the test data deterministic
    obs <- base::data.frame(
        CHROM = base::rep(x = base::c("chr1", "chr2"), times = c(30, 30)), 
        POS = base::c(base::seq(from = 1000, to = 20000, length.out = 30), base::seq(from = 1000, to = 15000, length.out = 30)), 
        PVAL1 = -base::log10(x = stats::runif(n = n, min = 1e-8, max = 1)), 
        PVAL2 = -base::log10(x = stats::runif(n = n, min = 1e-6, max = 1)), 
        AF = stats::runif(n = n, min = 0.01, max = 0.5), 
        stringsAsFactors = TRUE
    )
    file1 <- base::file.path(tempo_dir, "gg_miami_test.tsv")
    utils::write.table(x = obs, file = file1, sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)
    chr <- base::data.frame(
        CHR = base::c("chr1", "chr2"), 
        BP_LENGTH = base::c(20000, 16000), 
        LENGTH_CUMUL = base::c(20000, 36000), 
        LENGTH_CUMUL_TO_ADD = base::c(0, 20000), 
        CHR_NAME = base::c(1, 2), 
        CHR_NB = base::c(1, 2), 
        stringsAsFactors = FALSE
    )
    file2 <- base::file.path(tempo_dir, "gg_miami_chr.tsv")
    utils::write.table(x = chr, file = file2, sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)
    ## end data argument values

    ## initialization of tests
    # none
    ## end initialization of tests

    ## tests
    # manhattan only
    testthat::expect_no_error(object = saferGG::gg_miami(file = file1, chr = file2, top.y.column = "PVAL1", bottom.y.column = NULL, safer_check = FALSE))
    testthat::expect_true(object = base::file.exists("miami.png"))
    # full miami with x_lim as chromosome
    testthat::expect_no_error(object = saferGG::gg_miami(file = file1, chr = file2, top.y.column = "PVAL1", bottom.y.column = "PVAL2", x_lim = "chr1", safer_check = FALSE))
    # x_lim on a non-first chromosome: exercises the tempo.coord[1] > 1 branch (xmin_plot offset)
    testthat::expect_no_error(object = saferGG::gg_miami(file = file1, chr = file2, top.y.column = "PVAL1", bottom.y.column = "PVAL2", x_lim = "chr2", safer_check = FALSE))
    # coordinates
    testthat::expect_no_error(object = saferGG::gg_miami(file = file1, chr = file2, top.y.column = "PVAL1", bottom.y.column = "PVAL2", x_lim = "chr1:0-10000", safer_check = FALSE))
    # all options
    testthat::expect_no_error(object = saferGG::gg_miami(file = file1, chr = file2, top.y.column = "PVAL1", bottom.y.column = "PVAL2", vgrid = TRUE, y.log1 = TRUE, y.log2 = TRUE, reverse1 = TRUE, reverse2 = TRUE, color.column = "AF", dot.border.color = "black", safer_check = FALSE))
    # top only with vgrid, threshold, x_lim with coordinates
    testthat::expect_no_error(object = saferGG::gg_miami(file = file1, chr = file2, top.y.column = "PVAL1", bottom.y.column = NULL, vgrid = TRUE, y.threshold1 = 6, x_lim = "chr1:1000-10000", safer_check = FALSE))
    # miami with thresholds, no color.column, dot.border.color set (fill path)
    testthat::expect_no_error(object = saferGG::gg_miami(file = file1, chr = file2, top.y.column = "PVAL1", bottom.y.column = "PVAL2", y.threshold1 = 6, y.threshold2 = 4, dot.border.color = "black", x_lim = "whole", warn.print = FALSE, safer_check = FALSE))
    # y.log on both sides with default (non-reversed) orientation: FALSE branch of the limits if/else of both log scales
    testthat::expect_no_error(object = saferGG::gg_miami(file = file1, chr = file2, top.y.column = "PVAL1", bottom.y.column = "PVAL2", y.log1 = TRUE, y.log2 = TRUE, safer_check = FALSE))
    # reversed orientation on both sides without y.log: TRUE branch of the limits if/else of both non-log scales
    testthat::expect_no_error(object = saferGG::gg_miami(file = file1, chr = file2, top.y.column = "PVAL1", bottom.y.column = "PVAL2", reverse1 = TRUE, reverse2 = TRUE, safer_check = FALSE))
    # miami with color.column but no dot.border.color (aes_string color path)
    testthat::expect_no_error(object = saferGG::gg_miami(file = file1, chr = file2, top.y.column = "PVAL1", bottom.y.column = "PVAL2", color.column = "AF", warn.print = FALSE, safer_check = FALSE))
    # single top plot with color.column and dot.border.color (aes_string fill path)
    testthat::expect_no_error(object = saferGG::gg_miami(file = file1, chr = file2, top.y.column = "PVAL1", bottom.y.column = NULL, color.column = "AF", dot.border.color = "black", warn.print = FALSE, safer_check = FALSE))
    # miami without thresholds, vgrid FALSE (tick lines branch), y_lim set
    testthat::expect_no_error(object = saferGG::gg_miami(file = file1, chr = file2, top.y.column = "PVAL1", bottom.y.column = "PVAL2", y_lim1 = base::c(0, 8), y_lim2 = base::c(0, 6), warn.print = TRUE, safer_check = FALSE))
    # chr names with X, Y and MT (relabeling branch), several chromos with coordinates
    obs2 <- obs
    obs2$CHROM <- base::as.character(x = obs2$CHROM)
    obs2$CHROM[1:5] <- "X"
    obs2$CHROM[6:10] <- "Y"
    obs2$CHROM[11:15] <- "MT"
    file3 <- base::file.path(tempo_dir, "gg_miami_test_xy.tsv")
    utils::write.table(x = obs2, file = file3, sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)
    chr2 <- base::data.frame(
        CHR = base::c("chr1", "chr2", "chr23", "chr24", "chr25"), 
        BP_LENGTH = base::c(20000, 16000, 1, 1, 1), 
        LENGTH_CUMUL = base::c(20000, 36000, 36001, 36002, 36003), 
        LENGTH_CUMUL_TO_ADD = base::c(0, 20000, 36000, 36001, 36002), 
        CHR_NAME = base::c(1, 2, 23, 24, 25), 
        CHR_NB = base::c(1, 2, 23, 24, 25), 
        stringsAsFactors = FALSE
    )
    file4 <- base::file.path(tempo_dir, "gg_miami_chr_xy.tsv")
    utils::write.table(x = chr2, file = file4, sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)
    testthat::expect_no_error(object = saferGG::gg_miami(file = file3, chr = file4, top.y.column = "PVAL1", bottom.y.column = "PVAL2", x_lim = "chr1:0-10000, chr2:0-10000", warn.print = FALSE, safer_check = FALSE))
    ## end tests

    ## errors
    # file does not exist
    testthat::expect_error(object = saferGG::gg_miami(file = "nonexistent.tsv", chr = file2, top.y.column = "PVAL1", safer_check = FALSE), regexp = "DOES NOT EXISTS", fixed = TRUE)
    # chr file does not exist
    testthat::expect_error(object = saferGG::gg_miami(file = file1, chr = "nonexistent.tsv", top.y.column = "PVAL1", safer_check = FALSE), regexp = "chr ARGUMENT DOES NOT EXISTS", fixed = TRUE)
    # bad column
    testthat::expect_error(object = saferGG::gg_miami(file = file1, chr = file2, top.y.column = "BAD_COL", safer_check = FALSE), regexp = "top.y.column ARGUMENT MUST BE A COLUMN NAME", fixed = TRUE)
    # bad bottom column
    testthat::expect_error(object = saferGG::gg_miami(file = file1, chr = file2, top.y.column = "PVAL1", bottom.y.column = "BAD_COL", safer_check = FALSE), regexp = "bottom.y.column ARGUMENT MUST BE A COLUMN NAME", fixed = TRUE)
    # bad color column
    testthat::expect_error(object = saferGG::gg_miami(file = file1, chr = file2, top.y.column = "PVAL1", color.column = "BAD_COL", safer_check = FALSE), regexp = "color.column ARGUMENT MUST BE A COLUMN NAME", fixed = TRUE)
    # bad x_lim
    testthat::expect_error(object = saferGG::gg_miami(file = file1, chr = file2, top.y.column = "PVAL1", x_lim = "bad", safer_check = FALSE), regexp = "MUST START WITH", fixed = TRUE)
    # x_lim with coordinates but no dash
    testthat::expect_error(object = saferGG::gg_miami(file = file1, chr = file2, top.y.column = "PVAL1", x_lim = "chr1:10000", safer_check = FALSE), regexp = "MUST BE WRITTEN LIKE THIS", fixed = TRUE)
    # x_lim with non numeric coordinates
    testthat::expect_error(object = saferGG::gg_miami(file = file1, chr = file2, top.y.column = "PVAL1", x_lim = "chr1:a-b", safer_check = FALSE), regexp = "MUST BE WRITTEN LIKE THIS", fixed = TRUE)
    # unordered coordinates
    testthat::expect_error(object = saferGG::gg_miami(file = file1, chr = file2, top.y.column = "PVAL1", x_lim = "chr1:10000-0", safer_check = FALSE), regexp = "ORDERED COORDINATES", fixed = TRUE)
    # unknown chromosome
    testthat::expect_error(object = saferGG::gg_miami(file = file1, chr = file2, top.y.column = "PVAL1", x_lim = "chr99", safer_check = FALSE), regexp = "NOT FOUND IN THE chr", fixed = TRUE)
    # coordinates above chromo length
    testthat::expect_error(object = saferGG::gg_miami(file = file1, chr = file2, top.y.column = "PVAL1", x_lim = "chr1:0-999999", safer_check = FALSE), regexp = "ABOVE THE MAX LENGTH", fixed = TRUE)
    # y.log1 with 0 or neg values: create a file with a non positive top column
    obs3 <- obs
    obs3$NEGVAL1 <- -1 * (1:n) # negative values
    obs3$NEGVAL2 <- 0 # zeros
    file5 <- base::file.path(tempo_dir, "gg_miami_test_neg.tsv")
    utils::write.table(x = obs3, file = file5, sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)
    testthat::expect_error(object = saferGG::gg_miami(file = file5, chr = file2, top.y.column = "NEGVAL1", y.log1 = TRUE, x_lim = "chr1", safer_check = FALSE), regexp = "y.log1 ARGUMENT CANNOT BE SET TO TRUE", fixed = TRUE)
    testthat::expect_error(object = saferGG::gg_miami(file = file5, chr = file2, top.y.column = "PVAL1", bottom.y.column = "NEGVAL2", y.log2 = TRUE, x_lim = "chr1", safer_check = FALSE), regexp = "y.log2 ARGUMENT CANNOT BE SET TO TRUE", fixed = TRUE)
    # empty file: no plot drawn branch
    file6 <- base::file.path(tempo_dir, "gg_miami_test_empty.tsv")
    utils::write.table(x = base::data.frame(CHROM = character(0), POS = numeric(0), PVAL1 = numeric(0), stringsAsFactors = TRUE), file = file6, sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)
    testthat::expect_no_error(object = saferGG::gg_miami(file = file6, chr = file2, top.y.column = "PVAL1", safer_check = FALSE))
    # vgrid as string
    testthat::expect_error(object = saferGG::gg_miami(file = file1, chr = file2, top.y.column = "PVAL1", vgrid = "TRUE", safer_check = FALSE), regexp = "MUST BE|typeof|mode", fixed = FALSE)
    # y_lim1 as string
    testthat::expect_error(object = saferGG::gg_miami(file = file1, chr = file2, top.y.column = "PVAL1", y_lim1 = "1-10", safer_check = FALSE), regexp = "MUST BE|typeof|mode", fixed = FALSE)
    # x_lim = NULL: error (x_lim cannot be NULL in gg_miami)
    testthat::expect_error(object = saferGG::gg_miami(file = file1, chr = file2, top.y.column = "PVAL1", x_lim = NULL, safer_check = FALSE), regexp = "THIS ARGUMENT CANNOT BE NULL", fixed = TRUE)
    # letters in the chr column other than X/Y/MT: error from the letters check
    file7 <- base::file.path(tempo_dir, "gg_miami_test_letters.tsv")
    obs_letters <- obs
    obs_letters$CHROM[1:5] <- "Z"
    utils::write.table(x = obs_letters, file = file7, sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)
    testthat::expect_error(object = saferGG::gg_miami(file = file7, chr = file2, top.y.column = "PVAL1", x_lim = "whole", safer_check = FALSE), regexp = "HAS LETTERS IN IT, OTHER THAN X, Y and MT", fixed = TRUE)
    # zero-arg call: mandatory/no-default block
    res_zero <- base::tryCatch(
        expr = saferGG::gg_miami(),
        error = function(e){base::conditionMessage(e)}
    )
    testthat::expect_true(object = base::grepl(x = res_zero, pattern = "NO DEFAULT VALUE AND REQUIRE ONE", fixed = TRUE))
    testthat::expect_true(object = base::grepl(x = res_zero, pattern = "file", fixed = TRUE))
    ## end errors
    # cleanup of all files created during the tests
    base::unlink(x = base::c(file1, file2, file3, file4, file5, file6, file7, "miami.png"))
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
