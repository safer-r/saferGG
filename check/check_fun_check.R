######## fun_check() #### check class, type, length, etc., of objects


# 20201216 checking each argument separately, with default values for the others

load("C:/Users/gael/Documents/Git_projects/cute_little_R_functions/other/check_dataset.t26_20201124.RData") # recover the test list tl file
source("https://gitlab.pasteur.fr/gmillot/cute_little_R_functions/-/raw/7ceacbc07ba1cc65b3dd8db53ef0d8db2a2823d7/cute_little_R_functions.R")

# performed with check_dataset.RData commited 20201107 (25 different objects)
# in green the values that are not the default values
path <- "C:\\Users\\Gael\\Desktop\\fun_check_test1\\"
for(i0 in 1:14){
    Sys.sleep(1)
    cat("\n\nLOOP ", i0, " / 14\n\n")
    res <- fun_test(
        fun = "fun_check", 
        arg = c(
            L1 = "data", 
            L2 = "class", 
            L3 = "typeof", 
            L4 = "mode", 
            L5 = "length", 
            L6 = "prop", 
            L7 = "double.as.integer.allowed", 
            L8 = "options", 
            L9 = "all.options.in.data", 
            L10 = "na.contain", 
            L11 = "neg.values", 
            L12 = "print", 
            L13 = "data.name", 
            L14 = "fun.name"
        ), 
        val = list(
            L1 = if(i0 == 1){t26_20201124}else{"a"}, 
            L2 = if(i0 == 2){t26_20201124}else{"vector"}, 
            L3 = if(i0 == 3){t26_20201124}else{list(NULL)}, 
            L4 = if(i0 == 4){t26_20201124}else{list(NULL)}, 
            L5 = if(i0 == 5){t26_20201124}else{list(NULL)}, 
            L6 = if(i0 == 6){t26_20201124}else{FALSE}, 
            L7 = if(i0 == 7){t26_20201124}else{FALSE}, 
            L8 = if(i0 == 8){t26_20201124}else{list(NULL)}, 
            L9 = if(i0 == 9){t26_20201124}else{FALSE}, 
            L10 = if(i0 == 10){t26_20201124}else{FALSE}, 
            L11 = if(i0 == 11){t26_20201124}else{TRUE}, 
            L12 = if(i0 == 12){t26_20201124}else{FALSE}, 
            L13 = if(i0 == 13){t26_20201124}else{"test.function"}, 
            L14 = if(i0 == 14){t26_20201124}else{list(NULL)}
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
    tempo.df <- rbind(tempo.df, cbind(loop = paste0("loop_", i0), read.table(list.files(file.list[i0], , full.names = TRUE)[grepl(x = list.files(file.list[i0]), pattern = "^table_from_fun_test.*")], header = TRUE, sep = "\t", comment.char="")))
}
write.table(tempo.df, file = paste0(path, "/final_table_from_fun_test.txt"), row.names = FALSE, col.names = TRUE, append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "")


# 20201216 checking all the possible values of each argument, with default values for the others

load("C:/Users/gael/Documents/Git_projects/cute_little_R_functions/other/check_dataset.t26_20201124.RData") # recover the test list tl file
load("C:/Users/gael/Documents/Git_projects/cute_little_R_functions/other/check_dataset.t8_20201126.RData") # recover the test list tl file
source("https://gitlab.pasteur.fr/gmillot/cute_little_R_functions/-/raw/7ceacbc07ba1cc65b3dd8db53ef0d8db2a2823d7/cute_little_R_functions.R")

# performed with check_dataset.RData commited 20201107 (25 different objects)
# in green the values that are not the default values
path <- "C:\\Users\\Gael\\Desktop\\fun_check_test2\\"
for(i0 in 1:14){
    Sys.sleep(1)
    cat("\n\nLOOP ", i0, " / 14\n\n")
    res <- fun_test(
        fun = "fun_check", 
        arg = c(
            L1 = "data", 
            L2 = "class", 
            L3 = "typeof", 
            L4 = "mode", 
            L5 = "length", 
            L6 = "prop", 
            L7 = "double.as.integer.allowed", 
            L8 = "options", 
            L9 = "all.options.in.data", 
            L10 = "na.contain", 
            L11 = "neg.values", 
            L12 = "print", 
            L13 = "data.name", 
            L14 = "fun.name"
        ), 
        val = list(
            L1 = if(i0 == 1){t26_20201124}else{"a"}, 
            L2 = if(i0 == 2){list(NULL, "vector", "logical", "integer", "numeric", "complex", "character", "matrix", "array", "data.frame", "list", "factor", "table", "expression", "name", "symbol", "function", "uneval", "environment", "ggplot2", "ggplot_built", "call")}else if(i0 == 8){list(NULL)}else{"vector"}, 
            L3 = if(i0 == 3){list(NULL, "logical", "integer", "double", "complex", "character", "list", "expression", "symbol", "closure", "special", "builtin", "environment", "S4", "language")}else{list(NULL)}, 
            L4 = if(i0 == 4){list(NULL, "logical", "numeric", "complex", "character", "list", "expression", "name", "symbol", "function", "environment", "S4", "call")}else{list(NULL)}, 
            L5 = if(i0 == 5){list(NULL, 0, 2, 4)}else{list(NULL)}, 
            L6 = if(i0 == 6){logic1}else{FALSE}, 
            L7 = if(i0 == 7){logic1}else{FALSE}, 
            L8 = if(i0 == 8){list(NULL, "a")}else{list(NULL)}, 
            L9 = if(i0 == 9){logic1}else{FALSE}, 
            L10 = if(i0 == 10){logic1}else{FALSE}, 
            L11 = if(i0 == 11){logic1}else{TRUE}, 
            L12 = if(i0 == 12){logic1}else{TRUE}, 
            L13 = if(i0 == 13){list(NULL, "test.function")}else{list(NULL)}, 
            L14 = if(i0 == 14){list(NULL, "FUN_NAME")}else{list(NULL)}
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
    tempo.df <- rbind(tempo.df, cbind(loop = paste0("loop_", i0), read.table(list.files(file.list[i0], , full.names = TRUE)[grepl(x = list.files(file.list[i0]), pattern = "^table_from_fun_test.*")], header = TRUE, sep = "\t", comment.char="")))
}
write.table(tempo.df, file = paste0(path, "/final_table_from_fun_test.txt"), row.names = FALSE, col.names = TRUE, append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "")


