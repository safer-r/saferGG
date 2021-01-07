######## fun_gg_scatter() #### ggplot2 scatterplot + lines (up to 6 overlays totally)


# TESTS
# 1 tests checking the all the arguments, using a single value

# b <- list(NULL)
# b <- list("a")
# b <- list(NA)
a <- fun_test(
    fun = "fun_gg_scatter", 
    arg = c(
        L1 = "data1", 
        L2 = "x", 
        L3 = "y", 
        L4 = "categ", 
        L5 = "legend.name", 
        L6 = "color", 
        L7 = "geom", 
        L8 = "alpha", 
        L9 = "dot.size", 
        L10 = "line.size", 
        L11 = "x.lim", 
        L12 = "x.lab", 
        L13 = "x.log", 
        L14 = "x.tick.nb", 
        L15 = "x.inter.tick.nb", 
        L16 = "x.include.zero", 
        L17 = "x.left.extra.margin", 
        L18 = "x.right.extra.margin", 
        L19 = "x.text.angle", 
        L20 = "y.lim", 
        L21 = "y.lab", 
        L22 = "y.log", 
        L23 = "y.tick.nb", 
        L24 = "y.inter.tick.nb", 
        L25 = "y.include.zero", 
        L26 = "y.top.extra.margin", 
        L27 = "y.bottom.extra.margin", 
        L28 = "y.text.angle", 
        L29 = "text.size", 
        L30 = "title", 
        L31 = "title.text.size", 
        L32 = "legend.show", 
        L33 = "article", 
        L34 = "grid", 
        L35 = "raster", 
        L36 = "raster.threshold", 
        L37 = "return", 
        L38 = "plot", 
        L39 = "add", 
        L40 = "warn.print", 
        L41 = "lib.path"
    ), 
    val = list(
        L1 = b, 
        L2 = b, 
        L3 = b, 
        L4 = b, 
        L5 = b, 
        L6 = b, 
        L7 = b, 
        L8 = b, 
        L9 = b, 
        L10 = b, 
        L11 = b, 
        L12 = b, 
        L13 = b, 
        L14 = b, 
        L15 = b, 
        L16 = b, 
        L17 = b, 
        L18 = b, 
        L19 = b, 
        L20 = b, 
        L21 = b, 
        L22 = b, 
        L23 = b, 
        L24 = b, 
        L25 = b, 
        L26 = b, 
        L27 = b, 
        L28 = b, 
        L29 = b, 
        L30 = b, 
        L31 = b, 
        L32 = b, 
        L33 = b, 
        L34 = b, 
        L35 = b, 
        L36 = b, 
        L37 = b, 
        L38 = b, 
        L39 = b, 
        L40 = b, 
        L41 = b
    ),
    thread.nb = NULL, 
    plot.fun = TRUE, 
    export = TRUE, 
    res.path = "C:\\Users\\Gael\\Desktop\\"
)

# 576 tests checking the critical arguments

set.seed(1) ; obs1 <- data.frame(km = rnorm(10, 10, 3), time = rnorm(10, 10, 3), Group1 = rep(c("A1", "A2"), 5)) ; obs1$km[2:3] <- NA
obs2 <-data.frame(km = rnorm(10, 15, 3), time = rnorm(10, 15, 3), Group2 = rep(c("G1", "G2"), 5)) ; set.seed(NULL)
a <- fun_test(
    fun = "fun_gg_scatter", 
    arg = c(
        L1 = "data1", 
        L2 = "x", 
        L3 = "y", 
        L4 = "categ", 
        L5 = "legend.name", 
        L6 = "color", 
        L7 = "geom", 
        L8 = "alpha"
    ), 
    val = list(
        L1 = list(L1.1 = list(L1 = obs1, L2 = obs2), L1.2 = list(obs1), L1.3 = list("a")), 
        L2 = list(L2.1 = list("km", "km"), L2.2 = list("km")), 
        L3 = list(L3.1 = list("time", "time"), L3.2 = list("time")), 
        L4 = list(L4.1 = list("Group1", "Group2"), L4.2 = list("Group1")), 
        L5 = list(L5.1 = list(NULL, NULL), L5.2 = list(NULL)),
        L6 = list(L6.1 = list("green", "blue"), L6.2 = list(1:2), L6.3 = list(1:2, 3:4)), 
        L7 = list(L7.1 = list("geom_point", "geom_point"), L7.2 = list("geom_point")), 
        L8 = list(L8.1 = list(1, 0.1), L7.2 = list(0.1))
    ),
    thread.nb = 7, 
    plot.fun = TRUE, 
    export = TRUE, 
    res.path = "C:\\Users\\Gael\\Desktop\\"
)

# 1 test checking the data frame aspects

set.seed(1) ; obs1 <- data.frame(km = rnorm(10, 10, 3), time = rnorm(10, 10, 3), Group1 = rep(c("A1", "A2"), 5)) ; obs1$km[2:3] <- NA
obs2 <-data.frame(km = rnorm(10, 15, 3), time = rnorm(10, 15, 3), Group2 = rep(c("G1", "G2"), 5)) ; set.seed(NULL)
a <- fun_test(
    fun = "fun_gg_scatter", 
    arg = c(
        L1 = "data1", 
        L2 = "x", 
        L3 = "y", 
        L4 = "categ", 
        L5 = "legend.name", 
        L6 = "color", 
        L7 = "geom", 
        L8 = "alpha"
    ), 
    val = list(
        L1 = list(L1.1 = obs1), 
        L2 = list(L2.1 = "km"), 
        L3 = list(L3.1 = "time"), 
        L4 = list(L4.1 = "Group1"), 
        L5 = list(L5.1 = NULL, "LEGEND"),
        L6 = list(L6.1 = c("green", "blue"), L6.2 = c("green")), 
        L7 = list(L7.1 = "geom_point", L7.2 = "geom_line" , L7.3 = "geom_path"), 
        L8 = list(L8.1 = 0.25, L8.2 = 0.5, L8.3 = 1)
    ),
    thread.nb = NULL, 
    plot.fun = TRUE, 
    export = TRUE, 
    res.path = "C:\\Users\\Gael\\Desktop\\"
)

# 1 tests checking the list aspects

set.seed(1) ; obs1 <- data.frame(km = rnorm(10, 10, 3), time = rnorm(10, 10, 3), Group1 = rep(c("A1", "A2"), 5)) ; obs1$km[2:3] <- NA
obs2 <-data.frame(km = rnorm(10, 15, 3), time = rnorm(10, 15, 3), Group2 = rep(c("G1", "G2"), 5)) ; set.seed(NULL)
a <- fun_test(
    fun = "fun_gg_scatter", 
    arg = c(
        L1 = "data1", 
        L2 = "x", 
        L3 = "y", 
        L4 = "categ", 
        L5 = "legend.name", 
        L6 = "color", 
        L7 = "geom", 
        L8 = "alpha"
    ), 
    val = list(
        L1 = list(L1.1 = list(L1 = obs1, L2 = obs2)), 
        L2 = list(L2.1 = list("km", "km")), 
        L3 = list(L3.1 = list("time", "time")), 
        L4 = list(L4.1 = list("Group1", "Group2")), 
        L5 = list(L5.1 = list("LEG1", "LEG2")),
        L6 = list(L6.1 = list(1:2, 3:4)), 
        L7 = list(L7.1 = list("geom_point", "geom_point")), 
        L8 = list(L8.1 = list(1, 0.5))
    ),
    thread.nb = NULL, 
    plot.fun = TRUE, 
    export = TRUE, 
    res.path = "C:\\Users\\Gael\\Desktop\\"
)

# 16384 tests checking the data frame aspects

set.seed(1) ; obs1 <- data.frame(km = rnorm(10, 10, 3), time = rnorm(10, 10, 3), Group1 = rep(c("A1", "A2"), 5)) ; obs1$km[2:3] <- NA
obs2 <-data.frame(km = rnorm(10, 15, 3), time = rnorm(10, 15, 3), Group2 = rep(c("G1", "G2"), 5)) ; set.seed(NULL)
a <- fun_test(
    fun = "fun_gg_scatter", 
    arg = c(
        L1 = "data1", 
        L2 = "x", 
        L3 = "y", 
        L4 = "categ", 
        L5 = "legend.name", 
        L6 = "color", 
        L7 = "geom", 
        L8 = "alpha",
        L9 = "dot.size", 
        L10 = "line.size", 
        L11 = "x.lim", 
        L12 = "x.lab", 
        L13 = "x.log", 
        L14 = "x.tick.nb", 
        L15 = "x.inter.tick.nb", 
        L16 = "x.include.zero", 
        L17 = "x.left.extra.margin", 
        L18 = "x.right.extra.margin", 
        L19 = "x.text.angle"
    ), 
    val = list(
        L1 = list(L1.1 = obs1), 
        L2 = list(L2.1 = "km"), 
        L3 = list(L3.1 = "time"), 
        L4 = list(L4.1 = "Group1"), 
        L5 = list(L5.1 = NULL, "LEGEND"),
        L6 = list(L6.1 = c("green", "blue"), L6.2 = 1), 
        L7 = list(L7.1 = "geom_point"), 
        L8 = list(L8.1 = 0.25, L8.2 = 1), 
        L9 = list(L9.1 = 2, L9.2 = 0), 
        L10 = list(L10.1 = 2, L10.2 = 0), 
        L11 = list(L11.1 = NULL, L11.2 = c(1, 20)), 
        L12 = list(L12.1 = NULL, L12.2 = "XLAB"), 
        L13 = list(L13.1 = "no", L13.2 = "log10"), 
        L14 = list(L14.1 = NULL, L14.2 = 6), 
        L15 = list(L15.1 = NULL, L15.2 = 1), 
        L16 = list(L16.1 = TRUE, L16.2 = FALSE), 
        L17 = list(L17.1 = 0, L17.2 = 0.2), 
        L18 = list(L18.1 = 0, L18.2 = 0.3), 
        L19 = list(L19.1 = 0, L19.2 = 120)
    ),
    thread.nb = NULL, 
    plot.fun = TRUE, 
    export = TRUE, 
    res.path = "C:\\Users\\Gael\\Desktop\\"
)


# Matrix tests checking the data frame aspects

set.seed(1) ; obs1 <- data.frame(km = rnorm(10, 10, 3), time = rnorm(10, 10, 3), Group1 = rep(c("A1", "A2"), 5)) ; obs1$km[2:3] <- NA
obs2 <-data.frame(km = rnorm(10, 15, 3), time = rnorm(10, 15, 3), Group2 = rep(c("G1", "G2"), 5)) ; set.seed(NULL)
a <- fun_test(
    fun = "fun_gg_scatter", 
    arg = c(
        L1 = "data1", 
        L2 = "x", 
        L3 = "y", 
        L4 = "categ", 
        L5 = "legend.name", 
        L6 = "color", 
        L7 = "geom", 
        L8 = "alpha",
        L9 = "dot.size", 
        L10 = "line.size", 
        L11 = "x.lim", 
        L12 = "x.lab", 
        L13 = "x.log", 
        L14 = "x.tick.nb", 
        L15 = "x.inter.tick.nb", 
        L16 = "x.include.zero", 
        L17 = "x.left.extra.margin", 
        L18 = "x.right.extra.margin", 
        L19 = "x.text.angle", 
        L20 = "y.lim", 
        L21 = "y.lab", 
        L22 = "y.log", 
        L23 = "y.tick.nb", 
        L24 = "y.inter.tick.nb", 
        L25 = "y.include.zero", 
        L26 = "y.top.extra.margin", 
        L27 = "y.bottom.extra.margin", 
        L28 = "y.text.angle", 
        L29 = "text.size", 
        L30 = "title", 
        L31 = "title.text.size", 
        L32 = "show.legend", 
        L33 = "article", 
        L34 = "grid", 
        L35 = "raster", 
        L36 = "vectorial.limit", 
        L37 = "return", 
        L38 = "plot", 
        L39 = "add", 
        L40 = "warn.print"
    ), 
    val = list(
        L1 = list(L1.1 = obs1), 
        L2 = list(L2.1 = "km"), 
        L3 = list(L3.1 = "time"), 
        L4 = list(L4.1 = "Group1"), 
        L5 = list(L5.1 = NULL, "LEGEND"),
        L6 = list(L6.1 = c("green", "blue"), L6.2 = 1), 
        L7 = list(L7.1 = "geom_point"), 
        L8 = list(L8.1 = 0.25, L8.2 = 1), 
        L9 = list(L9.1 = 2, L9.2 = 0), 
        L10 = list(L10.1 = 2, L10.2 = 0), 
        L11 = list(L11.1 = NULL, L11.2 = c(1, 20)), 
        L12 = list(L12.1 = NULL, L12.2 = "XLAB"), 
        L13 = list(L13.1 = "no", L13.2 = "log10"), 
        L14 = list(L14.1 = NULL, L14.2 = 6), 
        L15 = list(L15.1 = NULL, L15.2 = 1), 
        L16 = list(L16.1 = TRUE, L16.2 = FALSE), 
        L17 = list(L17.1 = 0, L17.2 = 0.2), 
        L18 = list(L18.1 = 0, L18.2 = 0.3), 
        L19 = list(L19.1 = 0, L19.2 = 120), 
        L20 = list(L20.1 = NULL, L20.2 = c(1, 20)), 
        L21 = list(L21.1 = NULL, L21.2 = "YLAB"), 
        L22 = list(L22.1 = "no", L22.2 = "log10"), 
        L23 = list(L23.1 = NULL, L23.2 = 7), 
        L24 = list(L24.1 = NULL, L24.2 = 3), 
        L25 = list(L25.1 = TRUE, L25.2 = FALSE), 
        L26 = list(L26.1 = 0, L26.2 = 0.2), 
        L27 = list(L27.1 = 0, L27.2 = 0.3), 
        L28 = list(L28.1 = 0, L28.2 = 270), 
        L29 = list(L29.1 = 0, L29.2 = 15), 
        L30 = list(L30.1 = NULL, L30.2 = "TITLE"), 
        L31 = list(L31.1 = 0, L31.2 = 20), 
        L32 = list(L32.1 = TRUE, L32.2 = FALSE), 
        L33 = list(L33.1 = TRUE, L33.2 = FALSE), 
        L34 = list(L34.1 = TRUE, L34.2 = FALSE), 
        L35 = list(L35.1 = TRUE, L35.2 = FALSE), 
        L36 = list(L36.1 = NULL, L36.2 = 5), 
        L37 = list(L37.1 = TRUE, L37.2 = FALSE), 
        L38 = list(L38.1 = TRUE, L38.2 = FALSE), 
        L39 = list(L39.1 = NULL, L39.2 = "+ggplot2::theme_dark()"), 
        L40 = list(L40.1 = TRUE, L40.2 = FALSE)
    ),
    thread.nb = 8, 
    plot.fun = TRUE, 
    export = TRUE, 
    res.path = "C:\\Users\\Gael\\Desktop\\"
)


