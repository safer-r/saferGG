# 20201126 checking each argument separately, with default values for the others

load("C:/Users/gael/Documents/Git_projects/cute_little_R_functions/other/check_dataset.t26_20201124.RData") # recover the test list tl file
source("https://gitlab.pasteur.fr/gmillot/cute_little_R_functions/-/raw/7ceacbc07ba1cc65b3dd8db53ef0d8db2a2823d7/cute_little_R_functions.R")


# performed with check_dataset.RData commited 20201107 (25 different objects)
# in green the values that are not the default values
path <- "C:\\Users\\Gael\\Desktop\\fun_boxplot_test1\\"
for(i0 in 1:57){
    Sys.sleep(1)
    cat("\n\nLOOP ", i0, " / 57\n\n")
    res <- fun_test(
        fun = "fun_gg_boxplot", 
        arg = c(
            L1 = "data1", 
            L2 = "y", 
            L3 = "categ", 
            L4 = "categ.class.order", 
            L5 = "categ.color", 
            L6 = "box.legend.name", 
            L7 = "box.fill", 
            L8 = "box.width", 
            L9 = "box.space", 
            L10 = "box.line.size", 
            L11 = "box.notch", 
            L12 = "box.alpha", 
            L13 = "box.mean", 
            L14 = "box.whisker.kind", 
            L15 = "box.whisker.width", 
            L16 = "dot.color", 
            L17 = "dot.categ", 
            L18 = "dot.categ.class.order", 
            L19 = "dot.legend.name", 
            L20 = "dot.tidy", 
            L21 = "dot.tidy.bin.nb", 
            L22 = "dot.jitter", 
            L23 = "dot.seed", 
            L24 = "dot.size", 
            L25 = "dot.alpha", 
            L26 = "dot.border.size", 
            L27 = "dot.border.color", 
            L28 = "x.lab", 
            L29 = "x.angle", 
            L30 = "y.lab", 
            L31 = "y.lim", 
            L32 = "y.log", 
            L33 = "y.tick.nb", 
            L34 = "y.second.tick.nb", 
            L35 = "y.include.zero", 
            L36 = "y.top.extra.margin", 
            L37 = "y.bottom.extra.margin", 
            L38 = "stat.disp", 
            L39 = "stat.disp.mean", 
            L40 = "stat.size", 
            L41 = "stat.dist", 
            L42 = "stat.angle", 
            L43 = "vertical", 
            L44 = "text.size", 
            L45 = "title", 
            L46 = "title.text.size", 
            L47 = "legend.show", 
            L48 = "legend.width", 
            L49 = "article", 
            L50 = "grid", 
            L51 = "add", 
            L52 = "return", 
            L53 = "return.ggplot", 
            L54 = "return.gtable", 
            L55 = "plot", 
            L56 = "warn.print", 
            L57 = "lib.path"
        ), 
        val = list(
            L1 = if(i0 == 1){t26_20201124}else{list(df2)},
            L2 = if(i0 == 2){t26_20201124}else{"M1"},
            L3 = if(i0 == 3){t26_20201124}else{"CAT"},
            L4 = if(i0 == 4){t26_20201124}else{list(NULL)},
            L5 = if(i0 == 5){t26_20201124}else{list(NULL)},
            L6 = if(i0 == 6){t26_20201124}else{list(NULL)},
            L7 = if(i0 == 7){t26_20201124}else{FALSE},
            L8 = if(i0 == 8){t26_20201124}else{0.5},
            L9 = if(i0 == 9){t26_20201124}else{0.1},
            L10 = if(i0 == 10){t26_20201124}else{0.75},
            L11 = if(i0 == 11){t26_20201124}else{FALSE},
            L12 = if(i0 == 12){t26_20201124}else{1},
            L13 = if(i0 == 13){t26_20201124}else{TRUE},
            L14 = if(i0 == 14){t26_20201124}else{"std"},
            L15 = if(i0 == 15){t26_20201124}else{0},
            L16 = if(i0 == 16){t26_20201124}else{grey(0.25)},
            L17 = if(i0 == 17){t26_20201124}else{list(NULL)},
            L18 = if(i0 == 18){t26_20201124}else{list(NULL)},
            L19 = if(i0 == 19){t26_20201124}else{list(NULL)},
            L20 = if(i0 == 20){t26_20201124}else{FALSE},
            L21 = if(i0 == 21){t26_20201124}else{50},
            L22 = if(i0 == 22){t26_20201124}else{0.5},
            L23 = if(i0 == 23){t26_20201124}else{2},
            L24 = if(i0 == 24){t26_20201124}else{3},
            L25 = if(i0 == 25){t26_20201124}else{0.5},
            L26 = if(i0 == 26){t26_20201124}else{0.5},
            L27 = if(i0 == 27){t26_20201124}else{list(NULL)},
            L28 = if(i0 == 28){t26_20201124}else{list(NULL)},
            L29 = if(i0 == 29){t26_20201124}else{0},
            L30 = if(i0 == 30){t26_20201124}else{list(NULL)},
            L31 = if(i0 == 31){t26_20201124}else{list(NULL)},
            L32 = if(i0 == 32){t26_20201124}else{"no"},
            L33 = if(i0 == 33){t26_20201124}else{list(NULL)},
            L34 = if(i0 == 34){t26_20201124}else{1},
            L35 = if(i0 == 35){t26_20201124}else{FALSE},
            L36 = if(i0 == 36){t26_20201124}else{0.05},
            L37 = if(i0 == 37){t26_20201124}else{0.05},
            L38 = if(i0 == 38){t26_20201124}else{"top"},
            L39 = if(i0 == 39){t26_20201124}else{FALSE},
            L40 = if(i0 == 40){t26_20201124}else{4},
            L41 = if(i0 == 41){t26_20201124}else{5},
            L42 = if(i0 == 42){t26_20201124}else{0},
            L43 = if(i0 == 43){t26_20201124}else{TRUE},
            L44 = if(i0 == 44){t26_20201124}else{12},
            L45 = if(i0 == 45){t26_20201124}else{""},
            L46 = if(i0 == 46){t26_20201124}else{8},
            L47 = if(i0 == 47){t26_20201124}else{TRUE},
            L48 = if(i0 == 48){t26_20201124}else{0.5},
            L49 = if(i0 == 49){t26_20201124}else{TRUE},
            L50 = if(i0 == 50){t26_20201124}else{FALSE},
            L51 = if(i0 == 51){t26_20201124}else{list(NULL)},
            L52 = if(i0 == 52){t26_20201124}else{FALSE},
            L53 = if(i0 == 53){t26_20201124}else{FALSE},
            L54 = if(i0 == 54){t26_20201124}else{TRUE},
            L55 = if(i0 == 55){t26_20201124}else{TRUE},
            L56 = if(i0 == 56){t26_20201124}else{TRUE},
            L57 = if(i0 == 57){t26_20201124}else{list(NULL)}
        ),
        thread.nb = NULL, 
        plot.fun = FALSE, 
        export = TRUE, 
        res.path = path
    )
}
file.list <- list.files(path, full.names = TRUE)
tempo.df <- NULL
for(i0 in 1:length(file.list)){
    tempo.df <- rbind(tempo.df, cbind(loop = paste0("loop_", i0), read.table(list.files(file.list[i0], full.names = TRUE)[grepl(x = list.files(file.list[i0]), pattern = "^table_from_fun_test.*")], header = TRUE, sep = "\t", comment.char="")))
}
write.table(tempo.df, file = paste0(path, "/final_table_from_fun_test.txt"), row.names = FALSE, col.names = TRUE, append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "")


# 20201126 checking all the possible values of each argument, with default values for the others

load("C:/Users/gael/Documents/Git_projects/cute_little_R_functions/other/check_dataset.t8_20201126.RData") # recover the test list tl file
source("https://gitlab.pasteur.fr/gmillot/cute_little_R_functions/-/raw/7ceacbc07ba1cc65b3dd8db53ef0d8db2a2823d7/cute_little_R_functions.R")

# performed with check_dataset.RData commited 20201107 (25 different objects)
# in green the values that are not the default values
path <- "C:\\Users\\Gael\\Desktop\\fun_boxplot_test2\\"

### Data set
set.seed(1)
obs1 <- data.frame(
    Time = c(rnorm(20, 100, 10), rnorm(20, 200, 50), rnorm(20, 500, 60), rep(NA, 20)), 
    Categ1 = rep(c("CAT", "DOG"), times = 40), 
    Categ2 = rep(c("A", "B", "C", "D"), each = 20), 
    Color1 = rep(c("coral", "lightblue"), times = 40), 
    Color2 = rep(c("#9F2108", "#306100", "#007479", "#8500C0"), each = 20), 
    stringsAsFactors = TRUE
)
set.seed(NULL)
### end Data set


for(i0 in 1:57){
    Sys.sleep(1)
    cat("\n\nLOOP ", i0, " / 57\n\n")
    res <- fun_test(
        fun = "fun_gg_boxplot", 
        arg = c(
            L1 = "data1", 
            L2 = "y", 
            L3 = "categ", 
            L4 = "categ.class.order", 
            L5 = "categ.color", 
            L6 = "box.legend.name", 
            L7 = "box.fill", 
            L8 = "box.width", 
            L9 = "box.space", 
            L10 = "box.line.size", 
            L11 = "box.notch", 
            L12 = "box.alpha", 
            L13 = "box.mean", 
            L14 = "box.whisker.kind", 
            L15 = "box.whisker.width", 
            L16 = "dot.color", 
            L17 = "dot.categ", 
            L18 = "dot.categ.class.order", 
            L19 = "dot.legend.name", 
            L20 = "dot.tidy", 
            L21 = "dot.tidy.bin.nb", 
            L22 = "dot.jitter", 
            L23 = "dot.seed", 
            L24 = "dot.size", 
            L25 = "dot.alpha", 
            L26 = "dot.border.size", 
            L27 = "dot.border.color", 
            L28 = "x.lab", 
            L29 = "x.angle", 
            L30 = "y.lab", 
            L31 = "y.lim", 
            L32 = "y.log", 
            L33 = "y.tick.nb", 
            L34 = "y.second.tick.nb", 
            L35 = "y.include.zero", 
            L36 = "y.top.extra.margin", 
            L37 = "y.bottom.extra.margin", 
            L38 = "stat.disp", 
            L39 = "stat.disp.mean", 
            L40 = "stat.size", 
            L41 = "stat.dist", 
            L42 = "stat.angle", 
            L43 = "vertical", 
            L44 = "text.size", 
            L45 = "title", 
            L46 = "title.text.size", 
            L47 = "legend.show", 
            L48 = "legend.width", 
            L49 = "article", 
            L50 = "grid", 
            L51 = "add", 
            L52 = "return", 
            L53 = "return.ggplot", 
            L54 = "return.gtable", 
            L55 = "plot", 
            L56 = "warn.print", 
            L57 = "lib.path"
        ), 
        val = list(
            L1 = list(obs1),
            L2 = "Time",
            L3 = if(i0 == 3){list("Categ1", "Categ2", c("Categ1", "Categ2"))}else{list(c("Categ1", "Categ2"))},
            L4 = if(i0 == 4){list(NULL, list(c("CAT", "DOG"), c("A", "B", "C", "D")), list(c("DOG", "CAT"), c("B", "C", "A", "D")))}else{list(NULL)},
            L5 = if(i0 == 5){list(NULL, 1:4)}else{list(NULL)},
            L6 = if(i0 == 6){list(NULL, c("ANIMAL", "SECTOR"))}else{list(NULL)},
            L7 = if(i0 == 7){logic1}else{FALSE},
            L8 = if(i0 == 8){prop1}else{0.5},
            L9 = if(i0 == 9){prop1}else{0.1},
            L10 = if(i0 == 10){int1}else{0.75},
            L11 = if(i0 == 11){logic1}else{FALSE},
            L12 = if(i0 == 12){prop1}else{1},
            L13 = if(i0 == 13){logic1}else{TRUE},
            L14 = if(i0 == 14){list("no", "std", "max")}else{"std"},
            L15 = if(i0 == 15){prop1}else{0},
            L16 = if(i0 == 16){list(NULL, "same", grey(0.25), 1:4)}else{grey(0.25)},
            L17 = if(i0 == 17){list(NULL, "categ2")}else{list(NULL)},
            L18 = if(i0 == 18){list(NULL, c("A", "B", "C", "D"), c("B", "A", "C", "D"))}else{list(NULL)},
            L19 = if(i0 == 19){list(NULL, "DOT_LEGEND")}else{list(NULL)},
            L20 = if(i0 == 20){logic1}else{FALSE},
            L21 = if(i0 == 21){int2}else{50},
            L22 = if(i0 == 22){prop1}else{0.5},
            L23 = if(i0 == 23){int1}else{2},
            L24 = if(i0 == 24){int1}else{3},
            L25 = if(i0 == 25){prop1}else{0.5},
            L26 = if(i0 == 26){int1}else{0.5},
            L27 = if(i0 == 27){list(NULL, 2, grey(0.5))}else{list(NULL)},
            L28 = if(i0 == 28){list(NULL, "LAB_ANIMAL")}else{list(NULL)},
            L29 = if(i0 == 29){angle1}else{0},
            L30 = if(i0 == 30){list(NULL, "LAB_TIME")}else{list(NULL)},
            L31 = if(i0 == 31){list(NULL, c(0, 1000), c(1000, 0), c(-30, -10))}else{list(NULL)},
            L32 = if(i0 == 32){log1}else{"no"},
            L33 = if(i0 == 33){list(NULL, int1)}else{list(NULL)},
            L34 = if(i0 == 34){int1}else{1},
            L35 = if(i0 == 35){logic1}else{FALSE},
            L36 = if(i0 == 36){prop1}else{0.05},
            L37 = if(i0 == 37){prop1}else{0.05},
            L38 = if(i0 == 38){list(NULL, "top", "above")}else{"top"},
            L39 = if(i0 == 39){logic1}else{FALSE},
            L40 = if(i0 == 40){int1}else{4},
            L41 = if(i0 == 41){int1}else{5},
            L42 = if(i0 == 42){angle1}else{0},
            L43 = if(i0 == 43){logic1}else{TRUE},
            L44 = if(i0 == 44){int2}else{12},
            L45 = if(i0 == 45){"TITLE"}else{""},
            L46 = if(i0 == 46){int1}else{8},
            L47 = if(i0 == 47){logic1}else{TRUE},
            L48 = if(i0 == 48){prop1}else{0.5},
            L49 = if(i0 == 49){logic1}else{TRUE},
            L50 = if(i0 == 50){logic1}else{FALSE},
            L51 = if(i0 == 51){list(NULL, "+ggplot2::facet_wrap(facets = 'Categ2', labeller = 'label_both') + ggplot2::theme(strip.background = ggplot2::element_rect(color = 'grey', size = 0.5), strip.text = ggplot2::element_text(size = 10, face = 'bold'), panel.spacing = ggplot2::unit(0.5, 'lines'))")}else{list(NULL)},
            L52 = if(i0 == 52){logic1}else{FALSE},
            L53 = if(i0 == 53){logic1}else{FALSE},
            L54 = if(i0 == 54){logic1}else{TRUE},
            L55 = if(i0 == 55){logic1}else{TRUE},
            L56 = if(i0 == 56){logic1}else{TRUE},
            L57 = if(i0 == 57){list(NULL, path1)}else{list(NULL)}
        ),
        thread.nb = NULL, 
        plot.fun = TRUE, 
        export = TRUE, 
        res.path = path
    )
}
file.list <- list.files(path, full.names = TRUE)
tempo.df <- NULL
tempo.pdf <- NULL
for(i0 in 1:length(file.list)){
    tempo.df <- rbind(tempo.df, cbind(loop = paste0("loop_", i0), read.table(list.files(file.list[i0], , full.names = TRUE)[grepl(x = list.files(file.list[i0]), pattern = "^table_from_fun_test.*")], header = TRUE, sep = "\t", comment.char="")))
    tempo.pdf <- c(tempo.pdf, list.files(file.list[i0], , full.names = TRUE)[grepl(x = list.files(file.list[i0]), pattern = "^plots_from_fun_test.*")])
}
write.table(tempo.df, file = paste0(path, "/final_table_from_fun_test.txt"), row.names = FALSE, col.names = TRUE, append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "")
pdftools::pdf_combine(input = tempo.pdf, output = paste0(path, "/final_plots_from_fun_test.pdf"))












# 20201125 36 tests checking the critical arguments

set.seed(1) ; obs1 <- data.frame(Time = c(rnorm(10), rnorm(10) + 2), Group1 = rep(c("G", "H"), each = 10)) ; set.seed(NULL)
a <- fun_test(
    fun = "fun_gg_boxplot", 
    arg = c(
        L1 = "data1", 
        L2 = "y", 
        L3 = "categ"
    ), 
    val = list(
        L1 = list(L1.1 = NULL, L1.2 = obs1, L1.3 = "a"), 
        L2 = list(L2.1 = NULL, L2.2 = "Time", L2.3 = list(data.frame())), 
        L3 = list(L3.1 = NULL, L3.2 = "Group1", L3.3 = c("Group1", "Group2"), L3.4 = list(data.frame()))
    ),
    thread.nb = NULL, 
    plot.fun = TRUE, 
    export = TRUE, 
    res.path = "C:\\Users\\Gael\\Desktop\\"
)

# 20201125 960 tests checking the box and dot colors with tidy dots & NA removing classes

set.seed(1) ; obs1 <- data.frame(Time = c(rnorm(10), rnorm(10) + 2), Group1 = rep(c("G", "H"), each = 10), Group2 = rep(c("A", "B"), time = 10)) ;  set.seed(NULL) ; obs1$Time[1:11] <- NA
a <- fun_test(
    fun = "fun_gg_boxplot", 
    arg = c(
        L1 = "data1", 
        L2 = "y", 
        L3 = "categ", 
        L4 = "categ.class.order", 
        L5 = "categ.color", 
        L6 = "dot.color", 
        L7 = "dot.categ", 
        L8 = "dot.categ.class.order", 
        L9 = "dot.legend.name"
    ),
    val = list(
        L1 = list(L1.1 =  obs1), 
        L2 = list(L2.1 = "Time"), 
        L3 = list(L3.1 = "Group1", L3.2 = c("Group1", "Group2")), 
        L4 = list(L4.1 = list(c("G", "H")), L4.2 = list(c("H", "G")), L4.3 = list(c("G", "H"), c("A", "B")), L4.4 = list(c("H", "G"), c("A", "B")), L4.5 = list(c("H", "G"), c("B", "A"))),
        L5 = list(L5.1 = NULL, L5.2 = "green", L5.3 = c("blue", "green"), L5.4 = c("green", "blue")), 
        L6 = list(L6.1 = "same", L6.2 = NULL, L6.3= "black", L6.4 = c("red", "brown")), 
        L7 = list(L7.1 = c("Group1")), 
        L8 = list(L7.1 = NULL, L7.2 = c("G", "H"), L7.3 = c("H", "G")), 
        L9 = list(L9.1 = NULL, L9.2 = "DOT1")
    ),
    thread.nb = 15,
    plot.fun = TRUE,
    res.path = "C:\\Users\\Gael\\Desktop\\",
    export = TRUE
)

# 960 tests checking the box and dot colors with jitter dots & WITHOUT NA

set.seed(1) ; obs1 <- data.frame(Time = c(rnorm(10), rnorm(10) + 2), Group1 = rep(c("G", "H"), each = 10), Group2 = rep(c("A", "B"), time = 10)) ;  set.seed(NULL)
a <- fun_test(
    fun = "fun_gg_boxplot", 
    arg = c(
        L1 = "data1", 
        L2 = "y", 
        L3 = "categ", 
        L4 = "categ.class.order", 
        L5 = "categ.color", 
        L6 = "dot.color", 
        L7 = "dot.categ", 
        L8 = "dot.categ.class.order", 
        L9 = "dot.legend.name",
        L10 = "dot.tidy"
    ),
    val = list(
        L1 = list(L1.1 =  obs1), 
        L2 = list(L2.1 = "Time"), 
        L3 = list(L3.1 = "Group1", L3.2 = c("Group1", "Group2")), 
        L4 = list(L4.1 = list(c("G", "H")), L4.2 = list(c("H", "G")), L4.3 = list(c("G", "H"), c("A", "B")), L4.4 = list(c("H", "G"), c("A", "B")), L4.5 = list(c("H", "G"), c("B", "A"))),
        L5 = list(L5.1 = NULL, L5.2 = "green", L5.3 = c("blue", "green"), L5.4 = c("green", "blue")), 
        L6 = list(L6.1 = "same", L6.2 = NULL, L6.3= "black", L6.4 = c("red", "brown")), 
        L7 = list(L7.1 = c("Group1")), 
        L8 = list(L7.1 = NULL, L7.2 = c("G", "H"), L7.3 = c("H", "G")), 
        L9 = list(L9.1 = NULL, L9.2 = "DOT1"),
        L10 = list(L10.1 = FALSE)
    ),
    thread.nb = 16,
    plot.fun = TRUE,
    res.path = "C:\\Users\\Gael\\Desktop\\",
    export = TRUE
)

# 512 tests checking the box and dot colors with tidy dots & NA removing classes

set.seed(1) ; obs1 <- data.frame(Time = c(rnorm(10), rnorm(10) + 2), Group1 = rep(c("G", "H"), each = 10), Group2 = rep(c("A", "B"), time = 10)) ;  set.seed(NULL) ; obs1$Time[1:11] <- NA
b <- list(TRUE, FALSE)
z <- list(0, 0.5)
y <- list(NULL, "TEST_LEG")
a <- fun_test(
    fun = "fun_gg_boxplot", 
    arg = c(
        L1 = "data1", 
        L2 = "y", 
        L3 = "categ", 
        # L4 = "categ.class.order", 
        # L5 = "categ.legend.name", 
        # L6 = "categ.color", 
        L7 = "box.fill", 
        L8 = "box.width", 
        L9 = "box.space", 
        L10 = "box.line.size", 
        L11 = "box.notch", 
        L12 = "box.alpha", 
        L13 = "box.mean", 
        L14 = "box.whisker.kind", 
        L15 = "box.whisker.width"
        # L16 = "dot.color", 
        # L17 = "dot.categ", 
        # L18 = "dot.categ.class.order", 
        # L19 = "dot.categ.legend.name", 
        # L20 = "dot.tidy", 
        # L21 = "dot.tidy.bin.nb", 
        # L22 = "dot.jitter", 
        # L23 = "dot.size", 
        # L24 = "dot.alpha", 
        # L25 = "dot.border.size", 
        # L26 = "dot.border.color", 
        # L27 = "x.lab", 
        # L28 = "y.lab", 
        # L29 = "y.lim", 
        # L30 = "y.log"
        # L31 = "y.tick.nb", 
        # L32 = "y.inter.tick.nb", 
        # L33 = "y.include.zero", 
        # L34 = "y.top.extra.margin", 
        # L35 = "y.bottom.extra.margin", 
        # L36 = "stat.disp", 
        # L37 = "stat.disp.mean", 
        # L38 = "stat.size", 
        # L39 = "stat.dist", 
        # L40 = "vertical", 
        # L41 = "text.size", 
        # L42 = "text.angle", 
        # L43 = "title", 
        # L44 = "title.text.size", 
        # L45 = "article", 
        # L46 = "grid", 
        # L47 = "return", 
        # L48 = "plot", 
        # L49 = "add", 
        # L50 = "warn.print"
        # L51 = "lib.path"
    ), 
    val = list(
        L1 = list(obs1), 
        L2 = list("Time"), 
        L3 = list(c("Group1", "Group2")), 
        # L4 = b, 
        # L5 = b, 
        # L6 = b, 
        L7 = b, 
        L8 = z, 
        L9 = z, 
        L10 =z, 
        L11 = b, 
        L12 = z, 
        L13 = b, 
        L14 = list("std", "no"), 
        L15 = z
        # L16 = b, 
        # L17 = b, 
        # L18 = b, 
        # L19 = b, 
        # L20 = b, 
        # L21 = c(5, 100), 
        # L22 = z, 
        # L23 = z, 
        # L24 = z, 
        # L25 = z, 
        # L26 = "blue", 
        # L27 = y, 
        # L28 = y, 
        # L29 = list(c(2, 10)), 
        # L30 = list("no", "log10")
        # L31 = list(6), 
        # L32 = list(4), 
        # L33 = b, 
        # L34 = list(0, 0.1), 
        # L35 = list(0, 0.1), 
        # L36 = list(NULL, "above"), 
        # L37 = b, 
        # L38 = list(10), 
        # L39 = 1, 
        # L40 = b, 
        # L41 = list(10), 
        # L42 = list(120), 
        # L43 = "TITLE_TEST", 
        # L44 = list(20), 
        # L45 = b, 
        # L46 = b, 
        # L47 = b, 
        # L48 = b, 
        # L49 = list(NULL), 
        # L50 = FALSE
        # L51 = b
    ),
    thread.nb = 16,
    plot.fun = TRUE,
    res.path = "C:\\Users\\Gael\\Desktop\\",
    export = TRUE
)


# 512 tests checking the box and dot colors with tidy dots & NA removing classes

set.seed(1) ; obs1 <- data.frame(Time = c(rnorm(10), rnorm(10) + 2), Group1 = rep(c("G", "H"), each = 10), Group2 = rep(c("A", "B"), time = 10)) ;  set.seed(NULL) ; obs1$Time[1:11] <- NA
b <- list(TRUE, FALSE)
z <- list(0, 0.5)
y <- list(NULL, "TEST_LEG")
a <- fun_test(
    fun = "fun_gg_boxplot", 
    arg = c(
        L1 = "data1", 
        L2 = "y", 
        L3 = "categ", 
        # L4 = "categ.class.order", 
        # L5 = "categ.legend.name", 
        # L6 = "categ.color", 
        # L7 = "box.fill", 
        # L8 = "box.width", 
        # L9 = "box.space", 
        # L10 = "box.line.size", 
        # L11 = "box.notch", 
        # L12 = "box.alpha", 
        # L13 = "box.mean", 
        # L14 = "box.whisker.kind", 
        # L15 = "box.whisker.width"
        # L16 = "dot.color", 
        # L17 = "dot.categ", 
        # L18 = "dot.categ.class.order", 
        # L19 = "dot.categ.legend.name", 
        L20 = "dot.tidy", 
        # L21 = "dot.tidy.bin.nb", 
        L22 = "dot.jitter", 
        L23 = "dot.size", 
        L24 = "dot.alpha", 
        L25 = "dot.border.size", 
        L26 = "dot.border.color", 
        L27 = "x.lab", 
        L28 = "y.lab", 
        L29 = "y.lim", 
        L30 = "y.log", 
        L31 = "y.tick.nb", 
        L32 = "y.inter.tick.nb", 
        L33 = "y.include.zero"
        # L34 = "y.top.extra.margin", 
        # L35 = "y.bottom.extra.margin", 
        # L36 = "stat.disp", 
        # L37 = "stat.disp.mean", 
        # L38 = "stat.size", 
        # L39 = "stat.dist", 
        # L40 = "vertical", 
        # L41 = "text.size", 
        # L42 = "text.angle", 
        # L43 = "title", 
        # L44 = "title.text.size", 
        # L45 = "article", 
        # L46 = "grid", 
        # L47 = "return", 
        # L48 = "plot", 
        # L49 = "add", 
        # L50 = "warn.print"
        # L51 = "lib.path"
    ), 
    val = list(
        L1 = list(obs1), 
        L2 = list("Time"), 
        L3 = list(c("Group1", "Group2")), 
        # L4 = b, 
        # L5 = b, 
        # L6 = b, 
        # L7 = b, 
        # L8 = z, 
        # L9 = z, 
        # L10 =z, 
        # L11 = b, 
        # L12 = z, 
        # L13 = b, 
        # L14 = list("std", "no"), 
        # L15 = z
        # L16 = b, 
        # L17 = b, 
        # L18 = b, 
        # L19 = b, 
        L20 = b, 
        # L21 = c(5, 100), 
        L22 = z, 
        L23 = z, 
        L24 = z, 
        L25 = z, 
        L26 = "blue", 
        L27 = y, 
        L28 = y, 
        L29 = list(c(2, 10)), 
        L30 = list("no", "log10"), 
        L31 = list(6), 
        L32 = list(4), 
        L33 = b
        # L34 = list(0, 0.1), 
        # L35 = list(0, 0.1), 
        # L36 = list(NULL, "above"), 
        # L37 = b, 
        # L38 = list(10), 
        # L39 = 1, 
        # L40 = b, 
        # L41 = list(10), 
        # L42 = list(120), 
        # L43 = "TITLE_TEST", 
        # L44 = list(20), 
        # L45 = b, 
        # L46 = b, 
        # L47 = b, 
        # L48 = b, 
        # L49 = list(NULL), 
        # L50 = FALSE
        # L51 = b
    ),
    thread.nb = 8,
    plot.fun = TRUE,
    res.path = "C:\\Users\\Gael\\Desktop\\",
    export = TRUE
)


