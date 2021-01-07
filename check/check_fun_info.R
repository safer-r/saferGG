######## fun_info() #### recover object information


# 20201108 checking each argument separately, with default values for the others

load("C:/Users/gael/Documents/Git_projects/cute_little_R_functions/other/check_dataset.t26_20201124.RData") # recover the test list tl file
source("https://gitlab.pasteur.fr/gmillot/cute_little_R_functions/-/raw/7ceacbc07ba1cc65b3dd8db53ef0d8db2a2823d7/cute_little_R_functions.R")

# performed with check_dataset.RData commited 20201107 (25 different objects)
# in green the values that are not the default values
for(i0 in 1:3){
    res <- fun_test(
        fun = "fun_info", 
        arg = c(
            L1 = "data", 
            L2 = "n",
            L3 = "warn.print"
        ), 
        val = list(
            L1 = if(i0 == 1){t26_20201124}else{"a"}, 
            L2 = if(i0 == 2){t26_20201124}else{list(NULL)}, 
            L3 = if(i0 == 3){t26_20201124}else{TRUE}
        ),
        expect.error = list(
            L1 = if(i0 == 1){c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)}else{FALSE}, 
            L2 = if(i0 == 2){c(FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)}else{FALSE},
            L3 = if(i0 == 3){c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)}else{FALSE}
        ), 
        thread.nb = NULL, 
        plot.fun = FALSE, 
        export = TRUE, 
        res.path = "C:\\Users\\Gael\\Desktop\\"
    )
}

# 20201108 checking each argument separately, with all the other authorized / challenging values for the others


load("C:/Users/gael/Documents/Git_projects/cute_little_R_functions/other/check_dataset.t26_20201124.RData") # recover the test list tl file
source("https://gitlab.pasteur.fr/gmillot/cute_little_R_functions/-/raw/7ceacbc07ba1cc65b3dd8db53ef0d8db2a2823d7/cute_little_R_functions.R")

# performed with check_dataset.RData commited 20201107 (25 different objects)
# in green the values that are not the default values
for(i0 in 1:3){
    res <- fun_test(
        fun = "fun_info", 
        arg = c(
            L1 = "data", 
            L2 = "n",
            L3 = "warn.print"
        ), 
        val = list(
            L1 = t26_20201124, 
            L2 = if(i0 == 2){t26_20201124}else{list(NULL, -1, 0, 1, Inf)}, 
            L3 = if(i0 == 3){t26_20201124}else{list(TRUE, FALSE, NA)}
        ),
        expect.error = list(
            L1 = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), 
            L2 = if(i0 == 2){c(FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)}else{c(FALSE, TRUE, TRUE, FALSE, FALSE)},
            L3 = if(i0 == 3){c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)}else{c(FALSE, FALSE, TRUE)}
        ), 
        thread.nb = 16, 
        plot.fun = FALSE, 
        export = TRUE, 
        res.path = "C:\\Users\\Gael\\Desktop\\"
    )
}

