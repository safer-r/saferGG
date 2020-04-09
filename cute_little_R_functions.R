################################################################
##                                                            ##
##     CUTE FUNCTIONS v6.0.0                                  ##
##                                                            ##
##     Gael A. Millot                                         ##
##                                                            ##
##     Compatible with R v3.6.1                               ##
##                                                            ##
################################################################


# https://usethis.r-lib.org/ and usethat also
# BEWARE: do not forget to save the modifications in the .R file (through RSTUDIO for indentation)
# update graphic examples with good comment, as in barplot
#is there any interest to be able to source the cute file elsewhere than in global env? If yes, but may be interesting to put it into a new environement just above .GlobalEnv environment. See https://stackoverflow.com/questions/9002544/how-to-add-functions-in-an-existing-environment
# Make a first round check for each function if required
# Update all argument description, saying, character vector, etc.
# check all the functions using fun_test
# Templates: https://prettydoc.statr.me/themes.html
# # package: http://r-pkgs.had.co.nz/
# https://pkgdown.r-lib.org/
# https://rdrr.io/github/gastonstat/cointoss/
# doc:https://www.sphinx-doc.org/en/master/man/sphinx-autogen.html considering that https://www.ericholscher.com/blog/2014/feb/11/sphinx-isnt-just-for-python/
# https://docs.readthedocs.io/en/stable/intro/getting-started-with-sphinx.html
# https://docs.gitlab.com/ee/user/project/pages/
# also register into biotools


################################ OUTLINE ################################


################ Object analysis    2
######## fun_check() #### check class, type, length, etc., of objects   2
######## fun_info() #### recover object information 9
######## fun_head() #### head of the left or right of big 2D objects    11
######## fun_tail() #### tail of the left or right of big 2D objects    12
######## fun_comp_1d() #### comparison of two 1D datasets (vectors, factors, 1D tables) 13
######## fun_comp_2d() #### comparison of two 2D datasets (row & col names, dimensions, etc.)   17
######## fun_comp_list() #### comparison of two lists   24
######## fun_test() #### test combinations of argument values of a function 26
################ Object modification    38
######## fun_name_change() #### check a vector of character strings and modify any string if present in another vector  38
######## fun_df_remod() #### remodeling a data frame to have column name as a qualitative values and vice-versa 40
######## fun_merge() #### merge the columns of two 2D objects, by common rows   43
######## fun_round() #### rounding number if decimal present    47
######## fun_mat_rotate() #### 90° clockwise matrix rotation    49
######## fun_mat_num2color() #### convert a numeric matrix into hexadecimal color matrix    50
######## fun_mat_op() #### assemble several matrices with operation 53
######## fun_mat_inv() #### return the inverse of a square matrix   55
######## fun_mat_fill() #### fill the empty half part of a symmetric square matrix  56
######## fun_permut() #### progressively breaks a vector order  60
################ Graphics management    70
######## fun_width() #### window width depending on classes to plot 71
######## fun_open() #### open a GUI or pdf graphic window   72
######## fun_prior_plot() #### set graph param before plotting (erase axes for instance)    76
######## fun_scale() #### select nice label numbers when setting number of ticks on an axis 80
######## fun_post_plot() #### set graph param after plotting (axes redesign for instance)   85
######## fun_close() #### close specific graphic windows    96
################ Standard graphics  98
######## fun_empty_graph() #### text to display for empty graphs    98
################ gg graphics    99
######## fun_gg_palette() #### ggplot2 default color palette    99
######## fun_gg_just() #### ggplot2 justification of the axis labeling, depending on angle  101
######## fun_gg_point_rast() #### ggplot2 raster scatterplot layer  104
######## fun_gg_scatter() #### ggplot2 scatterplot + lines (up to 6 overlays totally)   107
######## fun_gg_bar() #### ggplot2 mean barplot + overlaid dots if required 107
######## fun_gg_boxplot() #### ggplot2 boxplot + background dots if required    142
######## fun_gg_prop() #### ggplot2 proportion barplot  190
######## fun_gg_dot() #### ggplot2 categorial dotplot + mean/median 191
######## fun_gg_violin() #### ggplot2 violins   191
######## fun_gg_line() #### ggplot2 lines + background dots and error bars  191
######## fun_gg_heatmap() #### ggplot2 heatmap + overlaid mask if required  193
######## fun_gg_empty_graph() #### text to display for empty graphs 207
################ Graphic extraction 208
######## fun_trim() #### display values from a quantitative variable and trim according to defined cut-offs 209
######## fun_segmentation() #### segment a dot cloud on a scatterplot and define the dots from another cloud outside the segmentation   217
################ Import 249
######## fun_pack() #### check if R packages are present and import into the working environment    249
######## fun_python_pack() #### check if python packages are present    251
################ Print / Exporting results (text & tables)  254
######## fun_report() #### print string or data object into output file 254
######## fun_get_message() #### return messages of an expression (that can be exported) 256


################################ FUNCTIONS ################################


################ Object analysis


######## fun_check() #### check class, type, length, etc., of objects


# Check r_debugging_tools-v1.2.R OK
# Check fun_test() (see cute_checks.docx) Ok
# check manual: example to scan again
# clear to go Apollo
fun_check <- function(data, data.name = NULL, class = NULL, typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double.as.integer.allowed = FALSE, options = NULL, all.options.in.data = FALSE, na.contain = FALSE, neg.values = TRUE, print = FALSE, fun.name = NULL){
# AIM
# check the class, type, mode and length of the data argument
# mainly used to check the arguments of other functions
# check also other kind of data parameters, is it a proportion? Is it type double but numbers without decimal part?
# if options == NULL, then at least class or type or mode or length argument must be non null
# if options is non null, then class, type and mode must be NULL, and length can be NULL or specified
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# none
# ARGUMENTS
# data: object to test
# data.name: character string indicating the name of the object to test. If NULL, use the name of the object assigned to the data argument
# class: character string. Either one of the class() result or "vector" or NULL
# typeof: character string. Either one of the typeof() result or NULL
# mode: character string. Either one of the mode() result (for non vector object) or NULL
# length: numeric value indicating the length of the object. Not considered if NULL
# prop: logical. Are the numeric values between 0 and 1 (proportion)? If TRUE, can be used alone, without considering class, etc.
# double.as.integer.allowed: logical. If TRUE, no error is reported if argument is set to typeof == "integer" or class == "integer", while the reality is typeof == "double" or class == "numeric" but the numbers have a zero as modulo (remainder of a division). This means that i <- 1 , which is typeof(i) -> "double" is considered as integer with double.as.integer.allowed = TRUE
# options: a vector of character strings indicating all the possible option values for data
# all.options.in.data: logical. If TRUE, all of the options must be present at least once in data, and nothing else. If FALSE, some or all of the options must be present in data, and nothing else. Ignored if options is NULL
# na.contain: logical. Can data contain NA?
# neg.values: logical. Are negative numeric values authorized? BEWARE: only considered if set to FALSE, to check for non negative values when class is set to "vector", "numeric", "matrix", "array", "data.frame", "table", or typeof is set to "double", "integer", or mode is set to "numeric". Ignored in other cases, notably with prop argument
# print: logical. Print the error message if $problem is TRUE? See the example section
# fun.name: character string indicating the name of the function checked (i.e., when fun_check() is used to check its argument). If non NULL, name will be added into the error message returned by fun_check()
# RETURN
# a list containing:
# $problem: logical. Is there any problem detected?
# $text: the problem detected
# $fun.name: name of the checked parameter
# EXAMPLES
# test <- 1:3 ; fun_check(data = test, data.name = NULL, print = TRUE, options = NULL, all.options.in.data = FALSE, class = NULL, typeof = NULL, mode = NULL, prop = TRUE, double.as.integer.allowed = FALSE, length = NULL)
# test <- 1:3 ; fun_check(data = test, print = TRUE, class = "numeric", typeof = NULL, double.as.integer.allowed = FALSE)
# test <- 1:3 ; fun_check(data = test, print = TRUE, class = "vector", mode = "numeric")
# argument print with and without assignation
# test <- 1:3 ; tempo <- fun_check(data = test, print = TRUE, class = "vector", mode = "character")
# test <- 1:3 ; tempo <- fun_check(data = test, print = FALSE, class = "vector", mode = "character") # the assignation allows to recover a problem without printing it
# test <- 1:3 ; fun_check(data = test, print = TRUE, class = "vector", mode = "character")
# test <- matrix(1:3) ; fun_check(data = test, print = TRUE, class = "vector", mode = "numeric")
# DEBUGGING
# data = expression(TEST) ; data.name = NULL ; class = "vector" ; typeof = NULL ; mode = NULL ; length = 1 ; prop = FALSE ; double.as.integer.allowed = FALSE ; options = NULL ; all.options.in.data = FALSE ; na.contain = FALSE ; neg.values = TRUE ; print = TRUE ; fun.name = NULL
# function name: no used in this function for the error message, to avoid env colliding
# argument checking
# fun.name checked first because required next
if( ! is.null(fun.name)){
if( ! (class(fun.name) == "character" & length(fun.name) == 1)){
tempo.cat <- paste0("\n\n================\n\nERROR IN fun_check(): THE fun.name ARGUMENT MUST BE A CHARACTER VECTOR OF LENGTH 1: ", paste(fun.name, collapse = " "), "\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
}
# end fun.name checked first because required next
# arg with no default values
if(missing(data)){
tempo.cat <- paste0("\n\n================\n\nERROR IN fun_check()", ifelse(is.null(fun.name), "", paste0(" IN ", fun.name)), ": ARGUMENT data HAS NO DEFAULT VALUE AND REQUIRES ONE\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end arg with no default values
# dealing with NA
if(any(is.na(data.name)) | any(is.na(class)) | any(is.na(typeof)) | any(is.na(mode)) | any(is.na(length)) | any(is.na(prop)) | any(is.na(double.as.integer.allowed)) | any(is.na(all.options.in.data)) | any(is.na(na.contain)) | any(is.na(neg.values)) | any(is.na(print)) | any(is.na(fun.name))){
tempo.cat <- paste0("\n\n================\n\nERROR IN fun_check()", ifelse(is.null(fun.name), "", paste0(" IN ", fun.name)), ": NO ARGUMENT EXCEPT data AND options CAN HAVE NA VALUES\nPROBLEMATIC ARGUMENTS ARE: ", paste(c("data.name", "class", "typeof", "mode", "length", "prop", "double.as.integer.allowed", "all.options.in.data", "na.contain", "neg.values", "print", "fun.name")[c(any(is.na(data.name)), any(is.na(class)), any(is.na(typeof)), any(is.na(mode)), any(is.na(length)), any(is.na(prop)), any(is.na(double.as.integer.allowed)), any(is.na(all.options.in.data)), any(is.na(na.contain)), any(is.na(neg.values)), any(is.na(print)), any(is.na(fun.name)))], collapse = " "), "\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end dealing with NA
# dealing with NULL
if(is.null(prop) | is.null(double.as.integer.allowed) | is.null(all.options.in.data) | is.null(na.contain) | is.null(neg.values) | is.null(print)){
tempo.cat <- paste0("\n\n================\n\nERROR IN fun_check()", ifelse(is.null(fun.name), "", paste0(" IN ", fun.name)), ": THESE ARGUMENTS prop, double.as.integer.allowed, all.options.in.data, na.contain, neg.values AND print CANNOT BE NULL\nPROBLEMATIC ARGUMENTS ARE: ", paste(c("prop", "double.as.integer.allowed", "all.options.in.data", "na.contain", "neg.values", "print")[c(is.null(prop), is.null(double.as.integer.allowed), is.null(all.options.in.data), is.null(na.contain), is.null(neg.values), is.null(print))], collapse = " "), "\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end dealing with NULL
# dealing with logical
# tested below
# end dealing with logical
if( ! is.null(data.name)){
if( ! (length(data.name) == 1 & class(data.name) == "character")){
tempo.cat <- paste0("\n\n================\n\nERROR IN fun_check()", ifelse(is.null(fun.name), "", paste0(" IN ", fun.name)), ": data.name ARGUMENT MUST BE A SINGLE CHARACTER ELEMENT AND NOT ", paste(data.name, collapse = " "), "\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
}
if(is.null(options) & is.null(class) & is.null(typeof) & is.null(mode) &  prop == FALSE & is.null(length)){
tempo.cat <- paste0("\n\n================\n\nERROR IN fun_check()", ifelse(is.null(fun.name), "", paste0(" IN ", fun.name)), ": AT LEAST ONE OF THE options, class, typeof, mode, prop, OR length ARGUMENT MUST BE SPECIFIED (I.E, TRUE FOR prop)\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if( ! is.null(options) & ( ! is.null(class) | ! is.null(typeof) | ! is.null(mode) | prop == TRUE)){
tempo.cat <- paste0("\n\n================\n\nERROR IN fun_check()", ifelse(is.null(fun.name), "", paste0(" IN ", fun.name)), ": THE class, typeof, mode ARGUMENTS MUST BE NULL, AND prop FALSE, IF THE options ARGUMENT IS SPECIFIED\nTHE options ARGUMENT MUST BE NULL IF THE class AND/OR typeof AND/OR mode AND/OR prop ARGUMENT IS SPECIFIED\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if( ! (all(class(neg.values) == "logical") & length(neg.values) == 1 & any(is.na(neg.values)) != TRUE)){
tempo.cat <- paste0("\n\n================\n\nERROR IN fun_check()", ifelse(is.null(fun.name), "", paste0(" IN ", fun.name)), ": THE neg.values ARGUMENT MUST BE TRUE OR FALSE ONLY\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if(neg.values == FALSE & is.null(class) & is.null(typeof) & is.null(mode)){
tempo.cat <- paste0("\n\n================\n\nERROR IN fun_check()", ifelse(is.null(fun.name), "", paste0(" IN ", fun.name)), ": THE neg.values ARGUMENT CANNOT BE SWITCHED TO FALSE IF class, typeof AND mode ARGUMENTS ARE NULL\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if( ! is.null(class)){
if( ! all(class %in% c("vector", "logical", "integer", "numeric", "complex", "character", "matrix", "array", "data.frame", "list", "factor", "table", "expression", "name", "symbol", "function", "uneval", "environment") & any(is.na(class)) != TRUE)){ # not length == 1 here because ordered factors are class "factor" "ordered" (length == 2)
tempo.cat <- paste0("\n\n================\n\nERROR IN fun_check()", ifelse(is.null(fun.name), "", paste0(" IN ", fun.name)), ": class ARGUMENT MUST BE ONE OF THESE VALUE:\n\"vector\", \"logical\", \"integer\", \"numeric\", \"complex\", \"character\", \"matrix\", \"array\", \"data.frame\", \"list\", \"factor\", \"table\", \"expression\", \"name\", \"symbol\", \"function\", \"environment\"\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if(neg.values == FALSE & ! any(class %in% c("vector", "numeric", "integer", "matrix", "array", "data.frame", "table"))){
tempo.cat <- paste0("\n\n================\n\nERROR IN fun_check()", ifelse(is.null(fun.name), "", paste0(" IN ", fun.name)), ": class ARGUMENT CANNOT BE OTHER THAN \"vector\", \"numeric\", \"integer\", \"matrix\", \"array\", \"data.frame\", \"table\" IF neg.values ARGUMENT IS SWITCHED TO FALSE\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
}
if( ! is.null(typeof)){
if( ! (all(typeof %in% c("logical", "integer", "double", "complex", "character", "list", "expression", "name", "symbol", "closure", "special", "builtin", "environment", "S4")) & length(typeof) == 1 & any(is.na(typeof)) != TRUE)){
tempo.cat <- paste0("\n\n================\n\nERROR IN fun_check()", ifelse(is.null(fun.name), "", paste0(" IN ", fun.name)), ": typeof ARGUMENT MUST BE ONE OF THESE VALUE:\n\"logical\", \"integer\", \"double\", \"complex\", \"character\", \"list\", \"expression\", \"name\", \"symbol\", \"closure\", \"special\", \"builtin\", \"environment\", \"S4\"\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if(neg.values == FALSE & ! typeof %in% c("double", "integer")){
tempo.cat <- paste0("\n\n================\n\nERROR IN fun_check()", ifelse(is.null(fun.name), "", paste0(" IN ", fun.name)), ": typeof ARGUMENT CANNOT BE OTHER THAN \"double\" OR \"integer\" IF neg.values ARGUMENT IS SWITCHED TO FALSE\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
}
if( ! is.null(mode)){
if( ! (all(mode %in% c("logical", "numeric", "complex", "character", "list", "expression", "name", "symbol", "function", "environment", "S4")) & length(mode) == 1 & any(is.na(mode)) != TRUE)){
tempo.cat <- paste0("\n\n================\n\nERROR IN fun_check()", ifelse(is.null(fun.name), "", paste0(" IN ", fun.name)), ": mode ARGUMENT MUST BE ONE OF THESE VALUE:\n\"logical\", \"numeric\", \"complex\", \"character\", \"list\", \"expression\", \"name\", \"symbol\", \"function\", \"environment\", \"S4\"\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if(neg.values == FALSE & mode != "numeric"){
tempo.cat <- paste0("\n\n================\n\nERROR IN fun_check()", ifelse(is.null(fun.name), "", paste0(" IN ", fun.name)), ": mode ARGUMENT CANNOT BE OTHER THAN \"numeric\" IF neg.values ARGUMENT IS SWITCHED TO FALSE\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
}
if( ! is.null(length)){
if( ! (is.numeric(length) & length(length) == 1 & ! grepl(length, pattern = "\\.") & any(is.na(length)) != TRUE)){
tempo.cat <- paste0("\n\n================\n\nERROR IN fun_check()", ifelse(is.null(fun.name), "", paste0(" IN ", fun.name)), ": length ARGUMENT MUST BE A SINGLE INTEGER VALUE\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
}
if( ! (is.logical(prop) | (length(prop) == 1 & any(is.na(prop)) != TRUE))){
tempo.cat <- paste0("\n\n================\n\nERROR IN fun_check()", ifelse(is.null(fun.name), "", paste0(" IN ", fun.name)), ": prop ARGUMENT MUST BE TRUE OR FALSE ONLY\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}else if(prop == TRUE){
if( ! is.null(class)){
if( ! any(class %in% c("vector", "numeric", "matrix", "array", "data.frame", "table"))){
tempo.cat <- paste0("\n\n================\n\nERROR IN fun_check()", ifelse(is.null(fun.name), "", paste0(" IN ", fun.name)), ": class ARGUMENT CANNOT BE OTHER THAN NULL, \"vector\", \"numeric\", \"matrix\", \"array\", \"data.frame\", \"table\" IF prop ARGUMENT IS TRUE\n\n================\n\n") # not integer because prop
stop(tempo.cat, call. = FALSE)
}
}
if( ! is.null(mode)){
if(mode != "numeric"){
tempo.cat <- paste0("\n\n================\n\nERROR IN fun_check()", ifelse(is.null(fun.name), "", paste0(" IN ", fun.name)), ": mode ARGUMENT CANNOT BE OTHER THAN NULL OR \"numeric\" IF prop ARGUMENT IS TRUE\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
}
if( ! is.null(typeof)){
if(typeof != "double"){
tempo.cat <- paste0("\n\n================\n\nERROR IN fun_check()", ifelse(is.null(fun.name), "", paste0(" IN ", fun.name)), ": typeof ARGUMENT CANNOT BE OTHER THAN NULL OR \"double\" IF prop ARGUMENT IS TRUE\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
}
}
if( ! (all(class(double.as.integer.allowed) == "logical") & length(double.as.integer.allowed) == 1 & any(is.na(double.as.integer.allowed)) != TRUE)){
tempo.cat <- paste0("\n\n================\n\nERROR IN fun_check()", ifelse(is.null(fun.name), "", paste0(" IN ", fun.name)), ": THE double.as.integer.allowed ARGUMENT MUST BE TRUE OR FALSE ONLY: ", paste(double.as.integer.allowed, collapse = " "), "\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if( ! (is.logical(all.options.in.data) & length(all.options.in.data) == 1 & any(is.na(all.options.in.data)) != TRUE)){
tempo.cat <- paste0("\n\n================\n\nERROR IN fun_check()", ifelse(is.null(fun.name), "", paste0(" IN ", fun.name)), ": all.options.in.data ARGUMENT MUST BE A SINGLE LOGICAL VALUE (TRUE OR FALSE ONLY): ", paste(all.options.in.data, collapse = " "), "\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if( ! (all(class(na.contain) == "logical") & length(na.contain) == 1 & any(is.na(na.contain)) != TRUE)){
tempo.cat <- paste0("\n\n================\n\nERROR IN fun_check(): THE na.contain ARGUMENT MUST BE TRUE OR FALSE ONLY: ", paste(na.contain, collapse = " "), "\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if( ! (all(class(print) == "logical") & length(print) == 1 & any(is.na(print)) != TRUE)){
tempo.cat <- paste0("\n\n================\n\nERROR IN fun_check(): THE print ARGUMENT MUST BE TRUE OR FALSE ONLY: ", paste(print, collapse = " "), "\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# fun.name tested at the beginning
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) # activate this line and use the function to check arguments status
# end argument checking
# main code
if(is.null(data.name)){
data.name <- deparse(substitute(data))
}
problem <- FALSE
text <- paste0(ifelse(is.null(fun.name), "", paste0("IN ", fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", data.name, " PARAMETER")
if( ! is.null(options)){
text <- ""
if( ! all(data %in% options)){
problem <- TRUE
text <- paste0(ifelse(is.null(fun.name), "ERROR", paste0("ERROR IN ", fun.name)), ": THE ", data.name, " PARAMETER MUST BE SOME OF THESE OPTIONS: ", paste(options, collapse = " "), "\nTHE PROBLEMATIC ELEMENTS OF data ARE: ", paste(unique(data[ ! (data %in% options)]), collapse = " "))
}
if(all.options.in.data == TRUE){
if( ! all(options %in% data)){
problem <- TRUE
text <- paste0(ifelse(text == "", "", paste0(text, "\n")), ifelse(is.null(fun.name), "ERROR", paste0("ERROR IN ", fun.name)), ": THE ", data.name, " PARAMETER MUST BE MADE OF ALL THESE OPTIONS: ", paste(options, collapse = " "), "\nTHE MISSING ELEMENTS OF THE options ARGUMENT ARE: ",  paste(unique(options[ ! (options %in% data)]), collapse = " "))
}
}
if( ! is.null(length)){
if(length(data) != length){
problem <- TRUE
text <- paste0(ifelse(text == "", "", paste0(text, "\n")), ifelse(is.null(fun.name), "ERROR", paste0("ERROR IN ", fun.name)), ": THE LENGTH OF ", data.name, " MUST BE ", length, " AND NOT ", length(data))
}
}
if(text == ""){
text <- paste0(ifelse(is.null(fun.name), "", paste0("IN ", fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", data.name, " PARAMETER")
}
}
arg.names <- c("class", "typeof", "mode", "length")
if(is.null(options)){
for(i2 in 1:length(arg.names)){
if( ! is.null(get(arg.names[i2]))){
# script to execute
tempo.script <- '
problem <- TRUE ;
if(identical(text, paste0(ifelse(is.null(fun.name), "", paste0("IN ", fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", data.name, " PARAMETER"))){
text <- paste0(ifelse(is.null(fun.name), "ERROR", paste0("ERROR IN ", fun.name)), ": THE ", data.name, " PARAMETER MUST BE ") ;
}else{
text <- paste0(text, " AND ") ; 
}
text <- paste0(text, toupper(arg.names[i2]), " ", get(arg.names[i2]))
'
# end script to execute
if(typeof(data) == "double" & double.as.integer.allowed == TRUE & ((arg.names[i2] == "class" & get(arg.names[i2]) == "integer") | (arg.names[i2] == "typeof" & get(arg.names[i2]) == "integer"))){
if( ! all(data%%1 == 0)){ # to check integers (use %%, meaning the remaining of a division): see the precedent line. isTRUE(all.equal(data%%1, rep(0, length(data)))) not used because we strictly need zero as a result
eval(parse(text = tempo.script)) # execute tempo.script
}
}else if(get(arg.names[i2]) != "vector" & eval(parse(text = paste0(arg.names[i2], "(data)"))) != get(arg.names[i2])){
eval(parse(text = tempo.script)) # execute tempo.script
}else if(arg.names[i2] == "class" & get(arg.names[i2]) == "vector" & ! (class(data) == "numeric" | class(data) == "integer" | class(data) == "character" | class(data) == "logical")){
eval(parse(text = tempo.script)) # execute tempo.script
}
}
}
}
if(prop == TRUE){
if(is.null(data) | any(data < 0 | data > 1, na.rm = TRUE)){
problem <- TRUE
if(identical(text, paste0(ifelse(is.null(fun.name), "", paste0("IN ", fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", data.name, " PARAMETER"))){
text <- paste0(ifelse(is.null(fun.name), "ERROR", paste0("ERROR IN ", fun.name)), ": ")
}else{
text <- paste0(text, " AND ")
}
text <- paste0(text, "THE ", data.name, " PARAMETER MUST BE DECIMAL VALUES BETWEEN 0 AND 1")
}
}
if(all(class(data) %in% "expression")){
data <- as.character(data) # to evaluate the presence of NA
}
if(na.contain == FALSE & (mode(data) %in% c("logical", "numeric", "complex", "character", "list", "expression", "name", "symbol"))){ # before it was ! (class(data) %in% c("function", "environment"))
if(any(is.na(data)) == TRUE){ # not on the same line because when data is class envir or function , do not like that
problem <- TRUE
if(identical(text, paste0(ifelse(is.null(fun.name), "", paste0("IN ", fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", data.name, " PARAMETER"))){
text <- paste0(ifelse(is.null(fun.name), "ERROR", paste0("ERROR IN ", fun.name)), ": ")
}else{
text <- paste0(text, " AND ")
}
text <- paste0(text, "THE ", data.name, " PARAMETER CONTAINS NA WHILE NOT AUTHORIZED")
}
}
if(neg.values == FALSE){
if(any(data < 0, na.rm = TRUE)){
problem <- TRUE
if(identical(text, paste0(ifelse(is.null(fun.name), "", paste0("IN ", fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", data.name, " PARAMETER"))){
text <- paste0(ifelse(is.null(fun.name), "ERROR", paste0("ERROR IN ", fun.name)), ": ")
}else{
text <- paste0(text, " AND ")
}
text <- paste0(text, "THE ", data.name, " PARAMETER MUST BE NON NEGATIVE NUMERIC VALUES")
}
}
if(print == TRUE & problem == TRUE){
cat(paste0("\n\n================\n\n", text, "\n\n================\n\n"))
}
output <- list(problem = problem, text = text, fun.name = data.name)
return(output)
}


######## fun_info() #### recover object information


# Check OK: clear to go Apollo
fun_info <- function(data){
# AIM
# provide a full description of an object
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# none
# ARGUMENTS
# data: object to test
# RETURN
# a list containing information, depending on the class and type of data
# if data is made of numerics, provide range, sum, mean, number of NA and number of Inf
# please, use names(fun_info()) and remove what can be too big for easy analysis
# EXAMPLES
# fun_info(data = 1:3)
# fun_info(data.frame(a = 1:2, b = ordered(factor(c("A", "B")))))
# fun_info(list(a = 1:3, b = ordered(factor(c("A", "B")))))
# DEBUGGING
# data = NULL # for function debugging
# data = 1:3 # for function debugging
# data = matrix(1:3) # for function debugging
# data = data.frame(a = 1:2, b = c("A", "B")) # for function debugging
# data = factor(c("b", "a")) # for function debugging
# data = ordered(factor(c("b", "a"))) # for function debugging
# data = list(a = 1:3, b = factor(c("A", "B"))) # for function debugging
# data = list(a = 1:3, b = ordered(factor(c("A", "B")))) # for function debugging
# function name: no need because no check and no message
# argument checking
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) # activate this line and use the function to check arguments status
# end argument checking
# main code
data.name <- deparse(substitute(data))
output <- list("NAME" = data.name)
tempo <- list("CLASS" = class(data))
output <- c(output, tempo)
tempo <- list("TYPE" = typeof(data))
output <- c(output, tempo)
tempo <- list("LENGTH" = length(data))
output <- c(output, tempo)
if(all(typeof(data) %in% c("integer", "numeric", "double"))){
tempo <- list("RANGE" = range(data[ ! is.infinite(data)], na.rm = TRUE))
output <- c(output, tempo)
tempo <- list("SUM" = sum(data[ ! is.infinite(data)], na.rm = TRUE))
output <- c(output, tempo)
tempo <- list("MEAN" = mean(data[ ! is.infinite(data)], na.rm = TRUE))
output <- c(output, tempo)
tempo <- list("NA.NB" = sum(is.na(data)))
output <- c(output, tempo)
tempo <- list("INF.NB" = sum(is.infinite(data)))
output <- c(output, tempo)
}
tempo <- list("HEAD" = head(data))
output <- c(output, tempo)
if( ! is.null(data)){
tempo <- list("TAIL" = tail(data))
output <- c(output, tempo)
if( ! is.null(dim(data))){
tempo <- list("DIMENSION" = dim(data))
names(tempo[[1]]) <- c("NROW", "NCOL")
output <- c(output, tempo)
}
tempo <- list("SUMMARY" = summary(data))
output <- c(output, tempo)
}
if(all(class(data) == "data.frame" | class(data) == "matrix")){
tempo <- list("ROW_NAMES" = dimnames(data)[[1]])
output <- c(output, tempo)
tempo <- list("COLUM_NAMES" = dimnames(data)[[2]])
output <- c(output, tempo)
}
if(all(class(data) == "data.frame")){
tempo <- list("STRUCTURE" = ls.str(data)) # str() print automatically, ls.str() not but does not give the order of the data.frame
output <- c(output, tempo)
tempo <- list("COLUMN_TYPE" = sapply(data, FUN = "typeof"))
if(any(sapply(data, FUN = "class") %in% "factor")){ # if an ordered factor is present, then sapply(data, FUN = "class") return a list but works with any(sapply(data, FUN = "class") %in% "factor") 
tempo.class <- sapply(data, FUN = "class")
if(any(unlist(tempo.class) %in% "ordered")){
tempo2 <- sapply(tempo.class, paste, collapse = " ") # paste the "ordered" factor" in "ordered factor"
}else{
tempo2 <- unlist(tempo.class)
}
tempo[["COLUMN_TYPE"]][grepl(x = tempo2, pattern = "factor")] <- tempo2[grepl(x = tempo2, pattern = "factor")]
}
output <- c(output, tempo)
}
if(all(class(data) == "list")){
tempo <- list("COMPARTMENT_NAMES" = names(data))
output <- c(output, tempo)
tempo <- list("COMPARTMENT_TYPE" = sapply(data, FUN = "typeof"))
if(any(unlist(sapply(data, FUN = "class")) %in% "factor")){ # if an ordered factor is present, then sapply(data, FUN = "class") return a list but works with any(sapply(data, FUN = "class") %in% "factor") 
tempo.class <- sapply(data, FUN = "class")
if(any(unlist(tempo.class) %in% "ordered")){
tempo2 <- sapply(tempo.class, paste, collapse = " ") # paste the "ordered" factor" in "ordered factor"
}else{
tempo2 <- unlist(tempo.class)
}
tempo[["COMPARTMENT_TYPE"]][grepl(x = tempo2, pattern = "factor")] <- tempo2[grepl(x = tempo2, pattern = "factor")]
}
output <- c(output, tempo)
}
return(output)
}


######## fun_head() #### head of the left or right of big 2D objects


# Check OK: clear to go Apollo
fun_head <- function(data1, n = 6, side = "l"){
# AIM
# as head() but display the left or right head of big 2D objects
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# fun_check()
# ARGUMENTS
# data1: any object but more dedicated for matrix, data frame or table
# n: as in head() but for for matrix, data frame or table, number of dimension to print (10 means 10 rows and columns)
# side: either "l" or "r" for the left or right side of the 2D object (only for matrix, data frame or table)
# BEWARE: other arguments of head() not used
# RETURN
# the head
# EXAMPLES
# obs1 = matrix(1:30, ncol = 5, dimnames = list(letters[1:6], LETTERS[1:5])) ; obs1 ; fun_head(obs1, 3)
# obs1 = matrix(1:30, ncol = 5, dimnames = list(letters[1:6], LETTERS[1:5])) ; obs1 ; fun_head(obs1, 3, "right")
# DEBUGGING
# data1 = matrix(1:30, ncol = 5) # for function debugging
# data1 = matrix(1:30, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) # for function debugging
# function name
function.name <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
# end function name
# required function checking
if(length(utils::find("fun_check", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_check() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end required function checking
# argument checking
arg.check <- NULL #
text.check <- NULL #
checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$fun.name))
tempo <- fun_check(data = n, class = "vector", typeof = "integer", double.as.integer.allowed = TRUE, length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = side, options = c("l", "r"), length = 1, fun.name = function.name) ; eval(ee)
if(any(arg.check) == TRUE){
stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
}
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
# end argument checking
# main code
if( ! any(class(data1) %in% c("matrix", "data.frame", "table"))){
return(head(data1, n))
}else{
obs.dim <- dim(data1)
row <- 1:ifelse(obs.dim[1] < n, obs.dim[1], n)
if(side == "l"){
col <- 1:ifelse(obs.dim[2] < n, obs.dim[2], n)
}
if(side == "r"){
col <- ifelse(obs.dim[2] < n, 1, obs.dim[2] - n + 1):obs.dim[2]
}
return(data1[row, col])
}
}


######## fun_tail() #### tail of the left or right of big 2D objects


# Check OK: clear to go Apollo
fun_tail <- function(data1, n = 10, side = "l"){
# AIM
# as tail() but display the left or right head of big 2D objects
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# fun_check()
# ARGUMENTS
# data1: any object but more dedicated for matrix, data frame or table
# n: as in tail() but for for matrix, data frame or table, number of dimension to print (10 means 10 rows and columns)
# side: either "l" or "r" for the left or right side of the 2D object (only for matrix, data frame or table)
# BEWARE: other arguments of tail() not used
# RETURN
# the tail
# EXAMPLES
# obs1 = matrix(1:30, ncol = 5, dimnames = list(letters[1:6], LETTERS[1:5])) ; obs1 ; fun_tail(obs1, 3)
# obs1 = matrix(1:30, ncol = 5, dimnames = list(letters[1:6], LETTERS[1:5])) ; obs1 ; fun_tail(obs1, 3, "r")
# DEBUGGING
# data1 = matrix(1:10, ncol = 5) # for function debugging
# data1 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) # for function debugging
# function name
function.name <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
# end function name
# required function checking
if(length(utils::find("fun_check", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_check() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end required function checking
# argument checking
arg.check <- NULL #
text.check <- NULL #
checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$fun.name))
tempo <- fun_check(data = n, class = "vector", typeof = "integer", double.as.integer.allowed = TRUE, length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = side, options = c("l", "r"), length = 1, fun.name = function.name) ; eval(ee)
if(any(arg.check) == TRUE){
stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
}
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
# end argument checking
# main code
if( ! any(class(data1) %in% c("matrix", "data.frame", "table"))){
return(tail(data1, n))
}else{
obs.dim <- dim(data1)
row <- ifelse(obs.dim[1] < n, 1, obs.dim[1] - n + 1):obs.dim[1]
if(side == "l"){
col <- 1:ifelse(obs.dim[2] < n, obs.dim[2], n)
}
if(side == "r"){
col <- ifelse(obs.dim[2] < n, 1, obs.dim[2] - n + 1):obs.dim[2]
}
return(data1[row, col])
}
}


######## fun_comp_1d() #### comparison of two 1D datasets (vectors, factors, 1D tables)


# Check OK: clear to go Apollo
fun_comp_1d <- function(data1, data2){
# AIM
# compare two 1D datasets (vector of factor or 1D table) of the same class or not. Check and report in a list if the 2 datasets have:
# same class
# common elements
# common element names (except factors)
# common levels (factors only)
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# none
# ARGUMENTS
# data1: vector or factor or 1D table
# data2: vector or factor or 1D table
# RETURN
# a list containing:
# $same.class: logical. Are class identical?
# $class: class of the 2 datasets (NULL otherwise)
# $same.length: logical. Are number of elements identical?
# $length: number of elements in the 2 datasets (NULL otherwise)
# $same.levels: logical. Are levels identical? (NULL if data1 and data2 are not factors)
# $levels: levels of the 2 datasets if identical (NULL otherwise or NULL if data1 and data2 are not factors)
# $any.id.levels: logical. Is there any identical levels? (NULL if data1 and data2 are not factors)
# $same.levels.pos1: position, in data1, of the levels identical in data2 (NULL if data1 and data2 are not factors)
# $same.levels.pos2: position, in data2, of the levels identical in data1 (NULL if data1 and data2 are not factors)
# $common.levels: common levels between data1 and data2 (can be a subset of $levels or not). NULL if no common levels or if data1 and data2 are not factors
# $same.name: logical. Are element names identical ?
# $name: name of elements of the 2 datasets if identical (NULL otherwise)
# $any.id.name: logical. Is there any element names identical ?
# $same.name.pos1: position, in data1, of the element names identical in data2
# $same.name.pos2: position, in data2, of the elements names identical in data1
# $common.names: common element names between data1 and data2 (can be a subset of $name or not). NULL if no common element names
# $any.id.element: logical. is there any identical elements ?
# $same.element.pos1: position, in data1, of the elements identical in data2
# $same.element.pos2: position, in data2, of the elements identical in data1
# $common.elements: common elements between data1 and data2. NULL if no common elements
# $identical.object: logical. Are objects identical (kind of object, element names and content)?
# $identical.content: logical. Are content objects identical (identical elements excluding kind of object and element names)?
# EXAMPLES
# obs1 = 1:5 ; obs2 = 1:5 ; names(obs1) <- LETTERS[1:5] ; names(obs2) <- LETTERS[1:5] ; fun_comp_1d(obs1, obs2)
# obs1 = 1:5 ; obs2 = 1:5 ; names(obs1) <- LETTERS[1:5] ; fun_comp_1d(obs1, obs2)
# obs1 = 1:5 ; obs2 = 3:6 ; names(obs1) <- LETTERS[1:5] ; names(obs2) <- LETTERS[1:4] ; fun_comp_1d(obs1, obs2)
# obs1 = factor(LETTERS[1:5]) ; obs2 = factor(LETTERS[1:5]) ; fun_comp_1d(obs1, obs2)
# obs1 = factor(LETTERS[1:5]) ; obs2 = factor(LETTERS[10:11]) ; fun_comp_1d(obs1, obs2)
# obs1 = factor(LETTERS[1:5]) ; obs2 = factor(LETTERS[4:7]) ; fun_comp_1d(obs1, obs2)
# obs1 = 1:5 ; obs2 = factor(LETTERS[1:5]) ; fun_comp_1d(obs1, obs2)
# obs1 = 1:5 ; obs2 = 1.1:6.1 ; fun_comp_1d(obs1, obs2)
# obs1 = as.table(1:5); obs2 = as.table(1:5) ; fun_comp_1d(obs1, obs2)
# obs1 = as.table(1:5); obs2 = 1:5 ; fun_comp_1d(obs1, obs2)
# DEBUGGING
# data1 = 1:5 ; data2 = 1:5 ; names(data1) <- LETTERS[1:5] ; names(data2) <- LETTERS[1:5] # for function debugging
# function name
function.name <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
# end function name
# argument checking
if( ! any(class(data1) %in% c("logical", "integer", "numeric", "character", "factor", "table"))){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": THE data1 ARGUMENT MUST BE A NON NULL VECTOR, FACTOR OR 1D TABLE\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}else if(all(class(data1) %in% "table")){
if(length(dim(data1)) > 1){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": THE data1 ARGUMENT MUST BE A 1D TABLE\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
}
if( ! any(class(data2) %in% c("logical", "integer", "numeric", "character", "factor", "table"))){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": THE data2 ARGUMENT MUST BE A NON NULL VECTOR, FACTOR OR 1D TABLE\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}else if(all(class(data2) %in% "table")){
if(length(dim(data2)) > 1){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": THE data2 ARGUMENT MUST BE A 1D TABLE\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
}
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) # activate this line and use the function to check arguments status
# end argument checking
# main code
same.class <- NULL
class <- NULL
same.length <- NULL
length <- NULL
same.levels <- NULL
levels <- NULL
any.id.levels <- NULL
same.levels.pos1 <- NULL
same.levels.pos2 <- NULL
common.levels <- NULL
same.name <- NULL
name <- NULL
any.id.name <- NULL
same.name.pos1 <- NULL
same.name.pos2 <- NULL
common.names <- NULL
any.id.element <- NULL
same.element.pos1 <- NULL
same.element.pos2 <- NULL
common.elements <- NULL
identical.object <- NULL
identical.content <- NULL
if(identical(data1, data2)){
same.class <- TRUE
class <- class(data1)
same.length <- TRUE
length <- length(data1)
if(any(class(data1) %in% "factor")){
same.levels <- TRUE
levels <- levels(data1)
any.id.levels <- TRUE
same.levels.pos1 <- 1:length(levels(data1))
same.levels.pos2 <- 1:length(levels(data2))
common.levels <- levels(data1)
}
if( ! is.null(names(data1))){
same.name <- TRUE
name <- names(data1)
any.id.name <- TRUE
same.name.pos1 <- 1:length(data1)
same.name.pos2 <- 1:length(data2)
common.names <- names(data1)
}
any.id.element <- TRUE
same.element.pos1 <- 1:length(data1)
same.element.pos2 <- 1:length(data2)
common.elements <- data1
identical.object <- TRUE
identical.content <- TRUE
}else{
identical.object <- FALSE
if( ! identical(class(data1), class(data2))){
same.class <- FALSE
}else{
same.class <- TRUE
class <- class(data1)
}
if( ! identical(length(data1), length(data2))){
same.length<- FALSE
}else{
same.length<- TRUE
length <- length(data1)
}
if(any(class(data1) %in% "factor") & any(class(data2) %in% "factor")){
if( ! identical(levels(data1), levels(data2))){
same.levels <- FALSE
}else{
same.levels <- TRUE
levels <- levels(data1)
}
any.id.levels <- FALSE
if(any(levels(data1) %in% levels(data2))){
any.id.levels <- TRUE
same.levels.pos1 <- which(levels(data1) %in% levels(data2))
}
if(any(levels(data2) %in% levels(data1))){
any.id.levels <- TRUE
same.levels.pos2 <- which(levels(data2) %in% levels(data1))
}
if(any.id.levels == TRUE){
common.levels <- unique(c(levels(data1)[same.levels.pos1], levels(data2)[same.levels.pos2]))
}
}
if(any(class(data1) %in% "factor")){ # to compare content
data1 <- as.character(data1)
}
if(any(class(data2) %in% "factor")){ # to compare content
data2 <- as.character(data2)
}
if( ! (is.null(names(data1)) & is.null(names(data2)))){
if( ! identical(names(data1), names(data2))){
same.name <- FALSE
}else{
same.name <- TRUE
name <- names(data1)
}
any.id.name <- FALSE
if(any(names(data1) %in% names(data2))){
any.id.name <- TRUE
same.name.pos1 <- which(names(data1) %in% names(data2))
}
if(any(names(data2) %in% names(data1))){
any.id.name <- TRUE
same.name.pos2 <- which(names(data2) %in% names(data1))
}
if(any.id.name == TRUE){
common.names <- unique(c(names(data1)[same.name.pos1], names(data2)[same.name.pos2]))
}
}
any.id.element <- FALSE
if(any(data1 %in% data2)){
any.id.element <- TRUE
same.element.pos1 <- which(data1 %in% data2)
}
if(any(data2 %in% data1)){
any.id.element <- TRUE
same.element.pos2 <- which(data2 %in% data1)
}
if(any.id.element == TRUE){
common.elements <- unique(c(data1[same.element.pos1], data2[same.element.pos2]))
}
if(same.length == TRUE & ! all(is.null(same.element.pos1), is.null(same.element.pos2))){
names(same.element.pos1) <- NULL
names(same.element.pos2) <- NULL
if(identical(same.element.pos1, same.element.pos2)){
identical.content <- TRUE
}else{
identical.content <- FALSE
}
}else{
identical.content <- FALSE
}
}
output <- list(same.class = same.class, class = class, same.length = same.length, length = length, same.levels = same.levels, levels = levels, any.id.levels = any.id.levels, same.levels.pos1 = same.levels.pos1, same.levels.pos2 = same.levels.pos2, common.levels = common.levels, same.name = same.name, name = name, any.id.name = any.id.name, same.name.pos1 = same.name.pos1, same.name.pos2 = same.name.pos2, common.names = common.names, any.id.element = any.id.element, same.element.pos1 = same.element.pos1, same.element.pos2 = same.element.pos2, common.elements = common.elements, identical.object = identical.object, identical.content = identical.content)
return(output)
}


######## fun_comp_2d() #### comparison of two 2D datasets (row & col names, dimensions, etc.)


# Check OK: clear to go Apollo
fun_comp_2d <- function(data1, data2){
# AIM
# compare two 2D datasets of the same class or not. Check and report in a list if the 2 datasets have:
# same class
# common row names
# common column names
# same row number
# same column number
# potential identical rows between the 2 datasets
# potential identical columns between the 2 datasets
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# none
# ARGUMENTS
# data1: matrix, data frame or table
# data2: matrix, data frame or table
# RETURN
# a list containing:
# $same.class: logical. Are class identical ?
# $class: classes of the 2 datasets (NULL otherwise)
# $same.dim: logical. Are dimension identical ?
# $dim: dimension of the 2 datasets (NULL otherwise)
# $same.row.nb: logical. Are number of rows identical ?
# $row.nb: nb of rows of the 2 datasets if identical (NULL otherwise)
# $same.col.nb: logical. Are number of columns identical ?
# $col.nb: nb of columns of the 2 datasets if identical (NULL otherwise)
# $same.row.name: logical. Are row names identical ? NULL if no row names in the two 2D datasets
# $row.name: name of rows of the 2 datasets if identical (NULL otherwise)
# $any.id.row.name: logical. Is there any row names identical ? NULL if no row names in the two 2D datasets
# $same.row.name.pos1: position, in data1, of the row names identical in data2
# $same.row.name.pos2: position, in data2, of the row names identical in data1
# $common.row.names: common row names between data1 and data2 (can be a subset of $name or not). NULL if no common row names
# $same.col.name: logical. Are column names identical ? NULL if no col names in the two 2D datasets
# $col.name: name of columns of the 2 datasets if identical (NULL otherwise)
# $any.id.col.name: logical. Is there any column names identical ? NULL if no col names in the two 2D datasets
# $same.col.name.pos1: position, in data1, of the column names identical in data2
# $same.col.name.pos2: position, in data2, of the column names identical in data1
# $common.col.names: common column names between data1 and data2 (can be a subset of $name or not). NULL if no common column names
# $any.id.row: logical. is there identical rows (not considering row names) ?
# $same.row.pos1: position, in data1, of the rows identical in data2 (not considering row names)
# $same.row.pos2: position, in data2, of the rows identical in data1 (not considering row names)
# $any.id.col: logical. is there identical columns (not considering column names)?
# $same.col.pos1: position in data1 of the cols identical in data2 (not considering column names)
# $same.col.pos2: position in data2 of the cols identical in data1 (not considering column names)
# $identical.object: logical. Are objects identical (including row & column names)?
# $identical.content: logical. Are content objects identical (identical excluding row & column names)?
# EXAMPLES
# obs1 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; obs2 = as.data.frame(matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5]))) ; obs1 ; obs2 ; fun_comp_2d(obs1, obs2)
# obs1 = matrix(101:110, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; obs2 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; obs1 ; obs2 ; fun_comp_2d(obs1, obs2)
# obs1 = matrix(1:10, byrow = TRUE, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; obs2 = matrix(c(1:5, 101:105, 6:10), byrow = TRUE, ncol = 5, dimnames = list(c("a", "z", "b"), c(LETTERS[1:2], "k", LETTERS[5:4]))) ; obs1 ; obs2 ; fun_comp_2d(obs1, obs2)
# obs1 = t(matrix(1:10, byrow = TRUE, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5]))) ; obs2 = t(matrix(c(1:5, 101:105, 6:10), byrow = TRUE, ncol = 5, dimnames = list(c("a", "z", "b"), c(LETTERS[1:2], "k", LETTERS[5:4])))) ; obs1 ; obs2 ; fun_comp_2d(obs1, obs2)
# DEBUGGING
# data1 = matrix(1:10, ncol = 5) ; data2 = matrix(1:10, ncol = 5) # for function debugging
# data1 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; data2 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) # for function debugging
# data1 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; data2 = matrix(1:10, ncol = 5) # for function debugging
# data1 = matrix(1:15, byrow = TRUE, ncol = 5, dimnames = list(letters[1:3], LETTERS[1:5])) ; data2 = matrix(1:10, byrow = TRUE, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) # for function debugging
# data1 = matrix(1:15, ncol = 5, dimnames = list(letters[1:3], LETTERS[1:5])) ; data2 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) # for function debugging
# data1 = matrix(1:15, ncol = 5, dimnames = list(paste0("A", letters[1:3]), LETTERS[1:5])) ; data2 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) # for function debugging
# data1 = matrix(1:15, ncol = 5, dimnames = list(letters[1:3], LETTERS[1:5])) ; data2 = matrix(1:12, ncol = 4, dimnames = list(letters[1:3], LETTERS[1:4])) # for function debugging
# data1 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; data2 = matrix(101:110, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) # for function debugging
# data1 = data.frame(a = 1:3, b= letters[1:3], row.names = LETTERS[1:3]) ; data2 = data.frame(A = 1:3, B= letters[1:3]) # for function debugging
# data1 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; data2 = as.data.frame(matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5]))) # for function debugging
# data1 = matrix(1:10, byrow = TRUE, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; data2 = matrix(c(1:5, 101:105, 6:10), byrow = TRUE, ncol = 5, dimnames = list(c("a", "z", "b"), c(LETTERS[1:2], "k", LETTERS[5:4]))) # for function debugging
# data1 = table(Exp1 = c("A", "A", "A", "B", "B", "B"), Exp2 = c("A1", "B1", "A1", "C1", "C1", "B1")) ; data2 = data.frame(A = 1:3, B= letters[1:3]) # for function debugging
# function name
function.name <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
# end function name
# argument checking
if( ! any(class(data1) %in% c("matrix", "data.frame", "table"))){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": THE data1 ARGUMENT MUST BE A MATRIX, DATA FRAME OR TABLE\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if( ! any(class(data2) %in% c("matrix", "data.frame", "table"))){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": THE data2 ARGUMENT MUST BE A MATRIX, DATA FRAME OR TABLE\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) # activate this line and use the function to check arguments status
# end argument checking
# main code
same.class <- NULL
class <- NULL
same.dim <- NULL
dim <- NULL
same.row.nb <- NULL
row.nb <- NULL
same.col.nb <- NULL
col.nb <- NULL
same.row.name <- NULL
row.name <- NULL
any.id.row.name <- NULL
same.row.name.pos1 <- NULL
same.row.name.pos2 <- NULL
common.row.names <- NULL
same.col.name <- NULL
any.id.col.name <- NULL
same.col.name.pos1 <- NULL
same.col.name.pos2 <- NULL
common.col.names <- NULL
col.name <- NULL
any.id.row <- NULL
same.row.pos1 <- NULL
same.row.pos2 <- NULL
any.id.col <- NULL
same.col.pos1 <- NULL
same.col.pos2 <- NULL
identical.object <- NULL
identical.content <- NULL
if(identical(data1, data2) & any(class(data1) %in% c("matrix", "data.frame", "table"))){
same.class <- TRUE
class <- class(data1)
same.dim <- TRUE
dim <- dim(data1)
same.row.nb <- TRUE
row.nb <- nrow(data1)
same.col.nb <- TRUE
col.nb <- ncol(data1)
same.row.name <- TRUE
row.name <- dimnames(data1)[[1]]
any.id.row.name <- TRUE
same.row.name.pos1 <- 1:row.nb
same.row.name.pos2 <- 1:row.nb
common.row.names <- dimnames(data1)[[1]]
same.col.name <- TRUE
col.name <- dimnames(data1)[[2]]
any.id.col.name <- TRUE
same.col.name.pos1 <- 1:col.nb
same.col.name.pos2 <- 1:col.nb
common.col.names <- dimnames(data1)[[2]]
any.id.row <- TRUE
same.row.pos1 <- 1:row.nb
same.row.pos2 <- 1:row.nb
any.id.col <- TRUE
same.col.pos1 <- 1:col.nb
same.col.pos2 <- 1:col.nb
identical.object <- TRUE
identical.content <- TRUE
}else{
identical.object <- FALSE
if(all(class(data1) == "table") & length(dim(data1)) == 1){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": THE data1 ARGUMENT IS A 1D TABLE. USE THE fun_comp_1d FUNCTION\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if(all(class(data2) == "table") & length(dim(data2)) == 1){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": THE data2 ARGUMENT IS A 1D TABLE. USE THE fun_comp_1d FUNCTION\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if( ! identical(class(data1), class(data2))){
same.class <- FALSE
}else if( ! any(class(data1) %in% c("matrix", "data.frame", "table"))){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": THE data1 AND data2 ARGUMENTS MUST BE EITHER MATRIX, DATA FRAME OR TABLE\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}else{
same.class <- TRUE
class <- class(data1)
}
if( ! identical(dim(data1), dim(data2))){
same.dim <- FALSE
}else{
same.dim <- TRUE
dim <- dim(data1)
}
if( ! identical(nrow(data1), nrow(data2))){
same.row.nb <- FALSE
}else{
same.row.nb <- TRUE
row.nb <- nrow(data1)
}
if( ! identical(ncol(data1), ncol(data2))){
same.col.nb <- FALSE
}else{
same.col.nb <- TRUE
col.nb <- ncol(data1)
}
# row and col names
if(is.null(dimnames(data1)) & is.null(dimnames(data2))){
same.row.name <- NULL
same.col.name <- NULL
# row and col names remain NULL
}else if((is.null(dimnames(data1)) & ! is.null(dimnames(data2))) | ( ! is.null(dimnames(data1)) & is.null(dimnames(data2)))){
same.row.name <- FALSE
same.col.name <- FALSE
# row and col names remain NULL
}else{
if( ! identical(dimnames(data1)[[1]], dimnames(data2)[[1]])){
same.row.name <- FALSE
# row names remain NULL
}else{
same.row.name <- TRUE
row.name <- dimnames(data1)[[1]]
}
# row names
any.id.row.name <- FALSE
if(any(dimnames(data1)[[1]] %in% dimnames(data2)[[1]])){
any.id.row.name <- TRUE
same.row.name.pos1 <- which(dimnames(data1)[[1]] %in% dimnames(data2)[[1]])
}
if(any(dimnames(data2)[[1]] %in% dimnames(data1)[[1]])){
any.id.row.name <- TRUE
same.row.name.pos2 <- which(dimnames(data2)[[1]] %in% dimnames(data1)[[1]])
}
if(any.id.row.name == TRUE){
common.row.names <- unique(c(dimnames(data1)[[1]][same.row.name.pos1], dimnames(data2)[[1]][same.row.name.pos2]))
}
# col names
any.id.col.name <- FALSE
if(any(dimnames(data1)[[2]] %in% dimnames(data2)[[2]])){
any.id.col.name <- TRUE
same.col.name.pos1 <- which(dimnames(data1)[[2]] %in% dimnames(data2)[[2]])
}
if(any(dimnames(data2)[[2]] %in% dimnames(data1)[[2]])){
any.id.col.name <- TRUE
same.col.name.pos2 <- which(dimnames(data2)[[2]] %in% dimnames(data1)[[2]])
}
if(any.id.col.name == TRUE){
common.col.names <- unique(c(dimnames(data1)[[2]][same.col.name.pos1], dimnames(data2)[[2]][same.col.name.pos2]))
}
if( ! identical(dimnames(data1)[[2]], dimnames(data2)[[2]])){
same.col.name <- FALSE
# col names remain NULL
}else{
same.col.name <- TRUE
col.name <- dimnames(data1)[[2]]
}
}
# identical row and col content
if(all(class(data1) == "table")){
as.data.frame(matrix(data1, ncol = ncol(data1)), stringsAsFactors = FALSE)
}else if(all(class(data1) == "matrix")){
data1 <- as.data.frame(data1, stringsAsFactors = FALSE)
}else if(all(class(data1) == "data.frame")){
data1 <- data.frame(lapply(data1, as.character), stringsAsFactors=FALSE)
}
if(all(class(data2) == "table")){
as.data.frame(matrix(data2, ncol = ncol(data2)), stringsAsFactors = FALSE)
}else if(all(class(data2) == "matrix")){
data2 <- as.data.frame(data2, stringsAsFactors = FALSE)
}else if(all(class(data2) == "data.frame")){
data2 <- data.frame(lapply(data2, as.character), stringsAsFactors=FALSE)
}
row.names(data1) <- paste0("A", 1:nrow(data1))
row.names(data2) <- paste0("A", 1:nrow(data2))
if(same.col.nb == TRUE){ # because if not the same col nb, the row cannot be identical
same.row.pos1 <- which(c(as.data.frame(t(data1), stringsAsFactors = FALSE)) %in% c(as.data.frame(t(data2), stringsAsFactors = FALSE)))
same.row.pos2 <- which(c(as.data.frame(t(data2), stringsAsFactors = FALSE)) %in% c(as.data.frame(t(data1), stringsAsFactors = FALSE)))
names(same.row.pos1) <- NULL
names(same.row.pos2) <- NULL
if(all(is.na(same.row.pos1))){
same.row.pos1 <- NULL
}else{
same.row.pos1 <- same.row.pos1[ ! is.na(same.row.pos1)]
any.id.row <- TRUE
}
if(all(is.na(same.row.pos2))){
same.row.pos2 <- NULL
}else{
same.row.pos2 <- same.row.pos2[ ! is.na(same.row.pos2)]
any.id.row <- TRUE
}
if(is.null(same.row.pos1) & is.null(same.row.pos2)){
any.id.row <- FALSE
}
}else{
any.id.row <- FALSE
# same.row.pos1 and 2 remain NULL
}
if(same.row.nb == TRUE){ # because if not the same row nb, the col cannot be identical
same.col.pos1 <- which(c(data1) %in% c(data2))
same.col.pos2 <- which(c(data2) %in% c(data1))
names(same.col.pos1) <- NULL
names(same.col.pos2) <- NULL
if(all(is.na(same.col.pos1))){
same.col.pos1 <- NULL
}else{
same.col.pos1 <- same.col.pos1[ ! is.na(same.col.pos1)]
any.id.col <- TRUE
}
if(all(is.na(same.col.pos2))){
same.col.pos2 <- NULL
}else{
same.col.pos2 <- same.col.pos2[ ! is.na(same.col.pos2)]
any.id.col <- TRUE
}
if(is.null(same.col.pos1) & is.null(same.col.pos2)){
any.id.col <- FALSE
}
}else{
any.id.col <- FALSE
# same.col.pos1 and 2 remain NULL
}
if(same.dim == TRUE & ! all(is.null(same.row.pos1), is.null(same.row.pos2), is.null(same.col.pos1), is.null(same.col.pos2))){ # same.dim == TRUE means that same.row.nb == TRUE and same.col.nb == TRUE, meaning that row.nb != NULL and col.nb != NULL. Thus, no need to include these checkings
if(identical(same.row.pos1, 1:row.nb) & identical(same.row.pos2, 1:row.nb) & identical(same.col.pos1, 1:col.nb) & identical(same.col.pos2, 1:col.nb)){
identical.content <- TRUE
}else{
identical.content <- FALSE
}
}else{
identical.content <- FALSE
}
}
output <- list(same.class = same.class, class = class, same.dim = same.dim, dim = dim, same.row.nb = same.row.nb, row.nb = row.nb, same.col.nb = same.col.nb , col.nb = col.nb, same.row.name = same.row.name, row.name = row.name, any.id.row.name = any.id.row.name, same.row.name.pos1 = same.row.name.pos1, same.row.name.pos2 = same.row.name.pos2, common.row.names = common.row.names, same.col.name = same.col.name, col.name = col.name,any.id.col.name = any.id.col.name, same.col.name.pos1 = same.col.name.pos1, same.col.name.pos2 = same.col.name.pos2, common.col.names = common.col.names, any.id.row = any.id.row, same.row.pos1 = same.row.pos1, same.row.pos2 = same.row.pos2, any.id.col = any.id.col, same.col.pos1 = same.col.pos1, same.col.pos2 = same.col.pos2, identical.object = identical.object, identical.content = identical.content)
return(output)
}


######## fun_comp_list() #### comparison of two lists


# Check OK: clear to go Apollo
fun_comp_list <- function(data1, data2){
# AIM
# compare two lists. Check and report in a list if the 2 datasets have:
# same length
# common names
# common compartments
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# none
# ARGUMENTS
# data1: list
# data2: list
# RETURN
# a list containing:
# $same.length: logical. Are number of elements identical?
# $length: number of elements in the 2 datasets (NULL otherwise)
# $same.name: logical. Are element names identical ?
# $name: name of elements of the 2 datasets if identical (NULL otherwise)
# $any.id.name: logical. Is there any element names identical ?
# $same.name.pos1: position, in data1, of the element names identical in data2
# $same.name.pos2: position, in data2, of the compartment names identical in data1
# $any.id.compartment: logical. is there any identical compartments ?
# $same.compartment.pos1: position, in data1, of the compartments identical in data2
# $same.compartment.pos2: position, in data2, of the compartments identical in data1
# $identical.object: logical. Are objects identical (kind of object, compartment names and content)?
# $identical.content: logical. Are content objects identical (identical compartments excluding compartment names)?
# EXAMPLES
# obs1 = list(a = 1:5, b = LETTERS[1:2], d = matrix(1:6)) ; obs2 = list(a = 1:5, b = LETTERS[1:2], d = matrix(1:6)) ; fun_comp_list(obs1, obs2)
# obs1 = list(1:5, LETTERS[1:2]) ; obs2 = list(a = 1:5, b = LETTERS[1:2]) ; fun_comp_list(obs1, obs2)
# obs1 = list(b = 1:5, c = LETTERS[1:2]) ; obs2 = list(a = 1:5, b = LETTERS[1:2], d = matrix(1:6)) ; fun_comp_list(obs1, obs2)
# obs1 = list(b = 1:5, c = LETTERS[1:2]) ; obs2 = list(LETTERS[5:9], matrix(1:6), 1:5) ; fun_comp_list(obs1, obs2)
# DEBUGGING
# data1 = list(a = 1:5, b = LETTERS[1:2], d = matrix(1:6)) ; data2 = list(a = 1:5, b = LETTERS[1:2], d = matrix(1:6)) # for function debugging
# data1 = list(a = 1:5, b = LETTERS[1:2]) ; data2 = list(a = 1:5, b = LETTERS[1:2], d = matrix(1:6)) # for function debugging
# function name
function.name <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
# end function name
# argument checking
if( ! any(class(data1) %in% "list")){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": THE data1 ARGUMENT MUST BE A LIST\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if( ! any(class(data2) %in% "list")){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": THE data2 ARGUMENT MUST BE A LIST\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) # activate this line and use the function to check arguments status
# end argument checking
# main code
same.length <- NULL
length <- NULL
same.name <- NULL
name <- NULL
any.id.name <- NULL
same.name.pos1 <- NULL
same.name.pos2 <- NULL
any.id.compartment <- NULL
same.compartment.pos1 <- NULL
same.compartment.pos2 <- NULL
identical.object <- NULL
identical.content <- NULL
if(identical(data1, data2)){
same.length <- TRUE
length <- length(data1)
if( ! is.null(names(data1))){
same.name <- TRUE
name <- names(data1)
any.id.name <- TRUE
same.name.pos1 <- 1:length(data1)
same.name.pos2 <- 1:length(data2)
}
any.id.compartment <- TRUE
same.compartment.pos1 <- 1:length(data1)
same.compartment.pos2 <- 1:length(data2)
identical.object <- TRUE
identical.content <- TRUE
}else{
identical.object <- FALSE
if( ! identical(length(data1), length(data2))){
same.length<- FALSE
}else{
same.length<- TRUE
length <- length(data1)
}
if( ! (is.null(names(data1)) & is.null(names(data2)))){
if( ! identical(names(data1), names(data2))){
same.name <- FALSE
}else{
same.name <- TRUE
name <- names(data1)
}
any.id.name <- FALSE
if(any(names(data1) %in% names(data2))){
any.id.name <- TRUE
same.name.pos1 <- which(names(data1) %in% names(data2))
}
if(any(names(data2) %in% names(data1))){
any.id.name <- TRUE
same.name.pos2 <- which(names(data2) %in% names(data1))
}
}
names(data1) <- NULL
names(data2) <- NULL
any.id.compartment <- FALSE
if(any(data1 %in% data2)){
any.id.compartment <- TRUE
same.compartment.pos1 <- which(data1 %in% data2)
}
if(any(data2 %in% data1)){
any.id.compartment <- TRUE
same.compartment.pos2 <- which(data2 %in% data1)
}
if(same.length == TRUE & ! all(is.null(same.compartment.pos1), is.null(same.compartment.pos2))){
if(identical(same.compartment.pos1, same.compartment.pos2)){
identical.content <- TRUE
}else{
identical.content <- FALSE
}
}else{
identical.content <- FALSE
}
}
output <- list(same.length = same.length, length = length, same.name = same.name, name = name, any.id.name = any.id.name, same.name.pos1 = same.name.pos1, same.name.pos2 = same.name.pos2, any.id.compartment = any.id.compartment, same.compartment.pos1 = same.compartment.pos1, same.compartment.pos2 = same.compartment.pos2, identical.object = identical.object, identical.content = identical.content)
return(output)
}


######## fun_test() #### test combinations of argument values of a function


# add traceback https://stackoverflow.com/questions/47414119/how-to-read-a-traceback-in-r

# Check OK: clear to go Apollo
fun_test <- function(fun, arg, val, expect.error = NULL, thread.nb = NULL, print.count = 10, plot.fun = FALSE, export = FALSE, res.path = NULL, lib.path = NULL, cute.path = "C:\\Users\\Gael\\Documents\\Git_projects\\cute_little_R_functions\\cute_little_R_functions.R"){
# AIM
# test combinations of argument values of a function
# WARNING
# Limited to 43 arguments with at least 2 values each. The total number of arguments tested can be more if the additional arguments have a single value. The limit is due to nested "for" loops (https://stat.ethz.ch/pipermail/r-help/2008-March/157341.html), but it should not be a problem since the number of tests would be 2^43 > 8e12
# ARGUMENTS
# fun: character string indicating the name of the function tested (without brackets)
# arg: vector of character strings of arguments of fun. At least arguments that do not have default values must be present in this vector
# val: list with number of compartments equal to length of arg, each compartment containing values of the corresponding argument in arg. Each different value must be in a list or in a vector. For instance, argument 3 in arg is a logical argument (values accepted TRUE, FALSE, NA). Thus, compartment 3 of val can be either list(TRUE, FALSE, NA), or c(TRUE, FALSE, NA)
# expect.error: list of exactly the same structure as val argument, but containing FALSE or TRUE, depending on whether error is expected (TRUE) or not (FALSE) for each corresponding value of val. A message is returned depending on discrepancies between the expected and observed errors. BEWARE: not always possible to write the expected errors for all the combination of argument values. Ignored if NULL
# thread.nb: numeric value indicating the number of available threads. Write NULL if no parallelization wanted
# print.count: interger value. Print a working progress message every print.count during loops. BEWARE: can increase substentially the time to complete the process using a small value, like 10 for instance. Use Inf is no loop message desired
# plot.fun: logical. Plot the plotting function tested for each test?
# export: logical. Export the results into a .RData file and into a .txt file? If FALSE, return a list into the console (see below). BEWARE: will be automatically set to TRUE if thread.nb is not NULL. This means that when using parallelization, the results are systematically exported, not returned into the console
# res.path: character string indicating the absolute pathway of folder where the txt results and pdfs, containing all the plots, will be saved. Several txt and pdf, one per thread, if parallelization. Ignored if export is FALSE. Must be specified if thread.nb is not NULL or if export is TRUE
# lib.path: character string indicating the absolute path of the required packages, if not in the default folders
# cute.path: character string indicating the absolute path of the cute.R file. Will be remove when cute will be a package. Not considered if thread.nb is NULL
# REQUIRED PACKAGES
# lubridate
# parallel if thread.nb argument is not NULL
# if the tested function is in a package, this package must be imported first (no parallelization) or must be in the classical R package folder indicated by the lib.path argument (parallelization)
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# fun_check()
# fun_get_message()
# fun_pack()
# RETURN
# if export is FALSE a list containing:
# $fun: the tested function
# $data: a data frame of all the combination tested, containing the following columns:
# the different values tested, named by arguments
# $kind: a vector of character strings indicating the kind of test result: either "ERROR", or "WARNING", or "OK"
# $problem: a logical vector indicating if error or not
# $expected.error: optional logical vector indicating the expected error specified in the expect.error argument
# $message: either NULL if $kind is always "OK", or the messages
# $instruction: the initial instruction
# $sys.info: system and packages info
# if export is TRUE 1) the same list object into a .RData file, 2) also the $data data frame into a .txt file, and 3) if expect.error is non NULL and if any discrepancy, the $data data frame into a .txt file but containing only the rows with discrepancies between expected and observed errors
# one or several pdf if a plotting function is tested and if the plot.fun argument is TRUE
# EXAMPLES
# fun_test(fun = "unique", arg = c("x", "incomparables"), val = list(x = list(1:10, c(1,1,2,8), NA), incomparable = c(TRUE, FALSE, NA)))
# fun_test(fun = "fun_round", arg = c("data", "dec.nb", "after.lead.zero"), val = list(L1 = list(c(1, 1.0002256, 1.23568), "a", NA), L2 = list(2, c(1,3), NA), L3 = c(TRUE, FALSE, NA)))
# fun_test(fun = "plot", arg = c("x", "y"), val = list(x = list(1:10, 12:13, NA, (1:10)^2), y = list(1:10, NA, NA)),  expect.error = list(x = list(FALSE, TRUE, TRUE, FALSE), y = list(FALSE, TRUE, TRUE)), thread.nb = NULL, plot.fun = TRUE, res.path = "C:\\Users\\Gael\\Desktop\\", lib.path = NULL)
# fun_test(fun = "plot", arg = c("x", "y"), val = list(x = list(1:10, 12:13, NA, (1:10)^2), y = list(1:10, NA, NA)), thread.nb = 4, plot.fun = TRUE, res.path = "C:\\Users\\Gael\\Desktop\\", lib.path = "C:\\Program Files\\R\\R-3.6.1\\library\\")
# set.seed(1) ; obs1 <- data.frame(Time = c(rnorm(10), rnorm(10) + 2), Group1 = rep(c("G", "H"), each = 10)) ; fun_test(fun = "fun_gg_boxplot", arg = c("data1", "y", "categ"), val = list(L1 = list(L1 = obs1), L2 = list(L1 = "Time"), L3 = list(L1 = "Group1")))
# set.seed(1) ; obs1 <- data.frame(Time = c(rnorm(10), rnorm(10) + 2), Group1 = rep(c("G", "H"), each = 10)) ; fun_test(fun = "fun_gg_boxplot", arg = c("data1", "y", "categ"), val = list(L1 = list(obs1), L2 = "Time", L3 = "Group1"), thread.nb = NULL, plot.fun = TRUE, res.path = "C:\\Users\\Gael\\Desktop\\", lib.path = "C:\\Program Files\\R\\R-3.6.1\\library\\")
# library(ggplot2) ; fun_test(fun = "geom_histogram", arg = c("data", "mapping"), val = list(x = list(data.frame(X = "a")), y = list(ggplot2::aes(x = X))), thread.nb = NULL, plot.fun = TRUE, res.path = "C:\\Users\\Gael\\Desktop\\", lib.path = "C:\\Program Files\\R\\R-3.6.1\\library\\") # BEWARE: ggplot2::geom_histogram does not work
# DEBUGGING
# fun = "unique" ; arg = "x" ; val = list(x = list(1:10, c(1,1,2,8), NA)) ; expect.error = list(x = list(FALSE, FALSE, TRUE)) ; thread.nb = NULL ; plot.fun = FALSE ; export = FALSE ; res.path = "C:\\Users\\Gael\\Desktop\\" ; lib.path = NULL ; print.count = 1 ; cute.path = "C:\\Users\\Gael\\Documents\\Git_projects\\cute_little_R_functions\\cute_little_R_functions.R" # for function debugging
# fun = "unique" ; arg = c("x", "incomparables") ; val = list(x = list(1:10, c(1,1,2,8), NA), incomparable = c(TRUE, FALSE, NA)) ; expect.error = NULL ; thread.nb = 2 ; plot.fun = FALSE ; export = TRUE ; res.path = "C:\\Users\\Gael\\Desktop\\" ; lib.path = NULL ; print.count = 10 ; cute.path = "C:\\Users\\Gael\\Documents\\Git_projects\\cute_little_R_functions\\cute_little_R_functions.R" # for function debugging
# fun = "plot" ; arg = c("x", "y") ; val = list(x = list(1:10, 12:13, NA), y = list(1:10, NA, NA)) ; expect.error = list(x = list(FALSE, FALSE, TRUE, FALSE), y = list(FALSE, TRUE, TRUE)) ; print.count = 10 ; thread.nb = NULL ; plot.fun = TRUE ; export = TRUE ; res.path = "C:\\Users\\Gael\\Desktop\\" ; lib.path = NULL # for function debugging
# set.seed(1) ; obs1 <- data.frame(Time = c(rnorm(10), rnorm(10) + 2), Group1 = rep(c("G", "H"), each = 10)) ; fun = "fun_gg_boxplot" ; arg = c("data1", "y", "categ") ; val = list(L1 = list(L1 = obs1), L2 = list(L1 = "Time"), L3 = list(L1 = "Group1")) ; expect.error = NULL ; print.count = 10 ; thread.nb = NULL ; plot.fun = TRUE ; export = TRUE ; res.path = "C:\\Users\\Gael\\Desktop\\" ; lib.path = NULL # for function debugging
# function name
function.name <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
instruction <- match.call()
# end function name
# required function checking
req.function <- c(
"fun_check", 
"fun_get_message", 
"fun_pack"
)
for(i1 in req.function){
if(length(find(i1, mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED ", i1, "() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat)
}
}
# end required function checking
# argument primary checking
# arg with no default values
if(any(missing(fun) | missing(arg) | missing(val))){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": ARGUMENTS fun, arg AND val HAVE NO DEFAULT VALUE AND REQUIRE ONE\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end arg with no default values
# using fun_check()
arg.check <- NULL #
text.check <- NULL #
checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$fun.name))
tempo <- fun_check(data = fun, class = "vector", mode = "character", length = 1, fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE){
if(grepl(x = fun, pattern = "()$")){ # remove ()
fun <- sub(x = fun, pattern = "()$", replacement = "")
}
if( ! exists(fun)){
tempo.cat <- paste0("ERROR IN ", function.name, ": CHARACTER STRING IN fun ARGUMENT DOES NOT EXIST IN THE R WORKING ENVIRONMENT: ", paste(fun, collapse = "\n"))
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}else if( ! all(class(get(fun)) == "function")){
tempo.cat <- paste0("ERROR IN ", function.name, ": fun ARGUMENT IS NOT CLASS \"function\" BUT: ", paste(class(get(fun)), collapse = "\n"), "\nCHECK IF ANY CREATED OBJECT WOULD HAVE THE NAME OF THE TESTED FUNCTION")
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
}
tempo <- fun_check(data = arg, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE & length(arg) == 0){
tempo.cat <- paste0("ERROR IN ", function.name, ": arg ARGUMENT CANNOT BE LENGTH 0")
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
tempo <- fun_check(data = val, class = "list", fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE){
for(i2 in 1:length(val)){
tempo1 <- fun_check(data = val[[i2]], class = "vector", na.contain = TRUE, fun.name = function.name, print = FALSE)
tempo2 <- fun_check(data = val[[i2]], class = "list", na.contain = TRUE, fun.name = function.name, print = FALSE)
if(tempo1$problem == TRUE & tempo2$problem == TRUE){
tempo.cat <- paste0("ERROR IN ", function.name, ": COMPARTMENT ", i2, " OF val ARGUMENT MUST BE A VECTOR OR A LIST")
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}else if(tempo1$problem == FALSE){ # vector split into list compartments
val[[i2]] <- split(x = val[[i2]], f = 1:length(val[[i2]]))
}
}
}
if( ! is.null(expect.error)){
tempo <- fun_check(data = expect.error, class = "list", fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE){
for(i3 in 1:length(expect.error)){
tempo1 <- fun_check(data = expect.error[[i3]], class = "vector",  mode = "logical", fun.name = function.name, print = FALSE)
tempo2 <- fun_check(data =  expect.error[[i3]], class = "list", fun.name = function.name, print = FALSE)
if(tempo1$problem == TRUE & tempo2$problem == TRUE){
tempo.cat <- paste0("ERROR IN ", function.name, ": COMPARTMENT ", i3, " OF expect.error ARGUMENT MUST BE TRUE OR FALSE")
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}else if(tempo1$problem == FALSE){ # vector split into list compartments
expect.error[[i3]] <- split(x = expect.error[[i3]], f = 1:length(expect.error[[i3]]))
}
}
}
}
if( ! is.null(thread.nb)){
tempo <- fun_check(data = thread.nb, typeof = "integer", double.as.integer.allowed = TRUE, neg.values = FALSE, length = 1, fun.name = "SLITHERINE") ; eval(ee)
if(tempo$problem == FALSE & thread.nb < 1){
tempo.cat <- paste0("ERROR IN ", function.name, ": thread.nb PARAMETER MUST EQUAL OR GREATER THAN 1: ", thread.nb)
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
}
tempo <- fun_check(data = print.count, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = plot.fun, class = "vector", mode = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = export, class = "vector", mode = "logical", length = 1, fun.name = function.name) ; eval(ee)
if( ! is.null(res.path)){
tempo <- fun_check(data = res.path, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE){
if( ! all(dir.exists(res.path))){ # separation to avoid the problem of tempo$problem == FALSE and res.path == NA
tempo.cat <- paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE res.path ARGUMENT DOES NOT EXISTS:\n", paste(res.path, collapse = "\n"))
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
}
}
if( ! is.null(lib.path)){
tempo <- fun_check(data = lib.path, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE){
if( ! all(dir.exists(lib.path))){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
tempo.cat <- paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", paste(lib.path, collapse = "\n"))
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
}
}
if( ! is.null(thread.nb)){
tempo <- fun_check(data = cute.path, class = "vector", typeof = "character", length = 1, fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE){
if( ! file.exists(cute.path)){
tempo.cat <- paste0("ERROR IN ", function.name, ": FILE PATH INDICATED IN THE cute.path PARAMETER DOES NOT EXISTS:\n", cute.path)
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
}
}
if(any(arg.check) == TRUE){
stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
}
# end using fun_check()
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
# end argument primary checking
# second round of checking and data preparation
# dealing with NA
if(any(is.na(fun)) | any(is.na(arg)) | any(is.na(expect.error)) | any(is.na(thread.nb)) | any(is.na(print.count)) | any(is.na(plot.fun)) | any(is.na(export)) | any(is.na(res.path)) | any(is.na(lib.path))){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": NO ARGUMENT EXCEPT val CAN HAVE NA VALUES\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end dealing with NA
# dealing with NULL
if(is.null(fun) | is.null(arg) | is.null(val) | is.null(print.count) | is.null(plot.fun) | is.null(export)){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": THESE ARGUMENTS fun, arg, val, print.count, plot.fun AND export CANNOT BE NULL\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end dealing with NULL
if(length(arg) != length(val)){
tempo.cat <- paste0("ERROR IN ", function.name, ": LENGTH OF arg ARGUMENT MUST BE IDENTICAL TO LENGTH OF val ARGUMENT:\nHERE IT IS: ", length(arg), " VERSUS ", length(val))
stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
}
args <- names(formals(get(fun)))
if( ! all(arg %in% args)){
tempo.cat <- paste0("ERROR IN ", function.name, ": SOME OF THE STRINGS IN arg ARE NOT ARGUMENTS OF fun\nfun ARGUMENTS: ", paste(args, collapse = " "),"\nPROBLEMATIC STRINGS IN arg: ", paste(arg[ ! arg %in% args], collapse = " "))
stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
}
if(sum(sapply(val, FUN = length) > 1) > 43){
tempo.cat <- paste0("ERROR IN ", function.name, ": CANNOT TEST MORE THAN 43 ARGUMENTS IF THEY ALL HAVE AT LEAST 2 VALUES EACH\nHERE THE NUMBER IS: ", sum(sapply(val, FUN = length) > 1))
stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
}
if( ! is.null(expect.error)){
if(length(val) != length(expect.error)){
tempo.cat <- paste0("ERROR IN ", function.name, ": LENGTH OF val ARGUMENT MUST BE IDENTICAL TO LENGTH OF expect.error ARGUMENT:\nHERE IT IS: ", length(val), " VERSUS ", length(expect.error))
stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
}
}
if( ! is.null(thread.nb) & is.null(res.path)){
tempo.cat <- paste0("ERROR IN ", function.name, ": res.path ARGUMENT MUST BE SPECIFIED IF thread.nb ARGUMENT IS NOT NULL")
stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
}
if(is.null(res.path) & export == TRUE){
tempo.cat <- paste0("ERROR IN ", function.name, ": res.path ARGUMENT MUST BE SPECIFIED IF export ARGUMENT TRUE")
stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
}
if( ! is.null(thread.nb) & export == FALSE){
export <- TRUE
tempo.cat <- paste0("WARNING FROM ", function.name, ": export ARGUMENT CONVERTED TO TRUE BECAUSE thread.nb ARGUMENT IS NOT NULL")
warning(paste0("\n", tempo.cat, "\n"), call. = FALSE)
}
# end second round of checking and data preparation
# package checking
fun_pack(req.package = c("lubridate"), lib.path = lib.path)
if( ! is.null(thread.nb)){
fun_pack(req.package = c("parallel"), lib.path = lib.path)
}
# end package checking
# declaration of special plot functions
sp.plot.fun <- c("fun_gg_scatter", "fun_gg_bar", "fun_gg_boxplot")
# end declaration of special plot functions
# main code
cat("\nfun_test JOB IGNITION\n")
ini.date <- Sys.time()
ini.time <- as.numeric(ini.date) # time of process begin, converted into seconds
if(export == TRUE){
res.path <- paste0(res.path, "/fun_test_res_", trunc(ini.time))
if(dir.exists(res.path)){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": FOLDER ALREADY EXISTS\n", res.path, "\nPLEASE RERUN ONCE\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}else{
dir.create(res.path)
}
}
total.comp.nb <- prod(sapply(val, FUN = "length"))
cat(paste0("\nTHE TOTAL NUMBER OF TESTS IS: ", total.comp.nb, "\n"))
# creation of the txt instruction that includes several loops
loop.string <- NULL
end.loop.string <- NULL
fun.args <- NULL
fun.args2 <- NULL
error.values <- NULL
arg.values <- "list("
for(i1 in 1:length(arg)){
if(is.null(thread.nb)){
if(length(val[[i1]]) > 1){ # loop only if more than one value in length(val[[i1]])
loop.string <- paste0(loop.string, "for(i", i1, " in 1:", length(val[[i1]]), "){")
end.loop.string <- paste0(end.loop.string, "}")
}
}else{
loop.string <- "for(i in x){"
end.loop.string <- "}"
}
fun.args <- paste0(
fun.args, 
ifelse(i1 == 1, "", ", "), 
arg[i1], 
" = val[[", 
i1, 
"]][[", 
if(is.null(thread.nb)){
if(length(val[[i1]]) > 1){
paste0("i", i1)
}else{
"1" # a unique element in val[[i1]]
}
}else{
paste0("i.list[[", i1, "]][i]")
}, 
"]]"
)
fun.args2 <- paste0(
fun.args2, 
ifelse(i1 == 1, "", ", "), 
arg[i1], 
" = val[[", 
i1, 
"]][[', ", 
if(is.null(thread.nb)){
if(length(val[[i1]]) > 1){
paste0("i", i1)
}else{
"1" # a unique element in val[[i1]]
}
}else{
paste0("i.list[[", i1, "]][i]")
}, 
", ']]"
)
arg.values <- paste0(
arg.values, 
"val[[", i1, "]][[", 
if(is.null(thread.nb)){
if(length(val[[i1]]) > 1){
paste0("i", i1)
}else{
"1" # a unique element in val[[i1]]
}
}else{
paste0("i.list[[", i1, "]][i]")
}, 
"]]", 
ifelse(i1 == length(arg), "", ", ")
)
error.values <- paste0(
error.values, 
ifelse(i1 == 1, "", " | "), 
"expect.error[[", i1, "]][[", 
if(is.null(thread.nb)){
if(length(expect.error[[i1]]) > 1){
paste0("i", i1)
}else{
"1" # a unique element in expect.error[[i1]]
}
}else{
paste0("i.list[[", i1, "]][i]")
}, 
"]]"
)
}
arg.values <- paste0(arg.values, ")")
fun.test <- paste0(fun, "(", fun.args, ")")
fun.test2 <- paste0("paste0('", fun, "(", fun.args2, ")')")
# plot title for special plot functions
if(plot.fun == TRUE){
plot.kind <- "classic"
if(fun %in% sp.plot.fun){
plot.kind <- "special"
if(any(arg %in% "title")){ # this is for the special functions
tempo.match <- regmatches(x = fun.test, m = regexpr(text = fun.test, pattern = "title = .+[,)]"))
tempo.match <- substring(tempo.match , 1, nchar(tempo.match) - 1)
fun.test <- sub(x = fun.test, pattern = tempo.match, replacement = paste0(tempo.match, "\ntempo.title"))
}else{
fun.test <- sub(x = fun.test, pattern = ")$", replacement = ", title = tempo.title)")
}
}
}
# end plot title for special plot functions
kind <- character()
problem <- logical()
expected.error <- logical()
res <- character()
count <- 0
print.count.loop <- 0
plot.count <- 0
if(length(arg) == 1){
data <- data.frame()
}else{ # length(arg) == 0 already tested above
data <- data.frame(t(vector("character", length(arg))), stringsAsFactors = FALSE)[-1, ] # -1 to remove the single row created and to have an empty data frame with length(arg) columns
}
code <- paste(
loop.string, '
count <- count + 1
print.count.loop <- print.count.loop + 1
data <- rbind(data, as.character(sapply(eval(parse(text = arg.values)), FUN = "paste", collapse = " ")), stringsAsFactors = FALSE) # each colum is a test
tempo.try.error <- fun_get_message(data = eval(parse(text = fun.test2)), kind = "error", header = FALSE, env = get(env.name)) # data argument needs a character string but eval(parse(text = fun.test2)) provides it (eval parse replace the i1, i2, etc., by the correct values, meaning that only val is required in the env.name environment)
tempo.try.warning <- fun_get_message(data = eval(parse(text = fun.test2)), kind = "warning", header = FALSE, env = get(env.name), print.no = TRUE) # data argument needs a character string but eval(parse(text = fun.test2)) provides it (eval parse replace the i1, i2, etc., by the correct values, meaning that only val is required in the env.name environment)
if( ! is.null(expect.error)){
expected.error <- c(expected.error, eval(parse(text = error.values)))
}
if( ! is.null(tempo.try.error)){
kind <- c(kind, "ERROR")
problem <- c(problem, TRUE)
res <- c(res, tempo.try.error)
}else{
if( ! is.null(tempo.try.warning)){
kind <- c(kind, "WARNING")
problem <- c(problem, FALSE)
res <- c(res, tempo.try.warning)
}else{
kind <- c(kind, "OK")
problem <- c(problem, FALSE)
res <- c(res, "")
}
if(plot.fun == TRUE){
dev.set(window.nb)
plot.count <- plot.count + 1
tempo.title <- paste0("test_", sprintf(paste0("%0", nchar(total.comp.nb), "d"), ifelse(is.null(thread.nb), count, x[count])))
if(plot.kind == "classic"){
eval(parse(text = fun.test))
tempo <- fun_post_plot(corner.text = tempo.title)
}else if(plot.kind == "special"){
eval(parse(text = fun.test))
}else{
tempo.cat <- paste0("\n\n================\n\nINTERNAL CODE ERROR 1 IN ", function.name, ": CODE HAS TO BE MODIFIED\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
}
}
if(print.count.loop == print.count){
print.count.loop <- 0
tempo.time <- as.numeric(Sys.time())
tempo.lapse <- round(lubridate::seconds_to_period(tempo.time - ini.time))
final.loop <- (tempo.time - ini.time) / count * ifelse(is.null(thread.nb), total.comp.nb, length(x)) # intra nb.compar loop lapse: time lapse / cycles done * cycles remaining
final.exp <- as.POSIXct(final.loop, origin = ini.date)
cat(paste0(ifelse(is.null(thread.nb), "\n", paste0("\nIN PROCESS ", process.id, " | ")), "LOOP ", format(count, big.mark=","), " / ", format(ifelse(is.null(thread.nb), total.comp.nb, length(x)), big.mark=","), " | TIME SPENT: ", tempo.lapse, " | EXPECTED END: ", final.exp))
}
if(count == ifelse(is.null(thread.nb), total.comp.nb, length(x))){
tempo.time <- as.numeric(Sys.time())
tempo.lapse <- round(lubridate::seconds_to_period(tempo.time - ini.time))
cat(paste0(ifelse(is.null(thread.nb), "\nLOOP PROCESS ENDED | ", paste0("\nPROCESS ", process.id, " ENDED | ")), "LOOP ", format(count, big.mark=","), " / ", format(ifelse(is.null(thread.nb), total.comp.nb, length(x)), big.mark=","), " | TIME SPENT: ", tempo.lapse, "\n\n"))
}
', 
end.loop.string
)
# end creation of the txt instruction that includes several loops
if( ! is.null(thread.nb)){
# list of i numbers that will be split
i.list <- vector("list", length(val)) # positions to split in parallel jobs
for(i2 in 1:length(arg)){
if(i2 == 1){
tempo.divisor <- total.comp.nb / length(val[[i2]])
i.list[[i2]] <- rep(1:length(val[[i2]]), each = as.integer(tempo.divisor))
tempo.multi <- length(val[[i2]])
}else{
tempo.divisor <- tempo.divisor / length(val[[i2]])
i.list[[i2]] <- rep(rep(1:length(val[[i2]]), each = as.integer(tempo.divisor)), time = as.integer(tempo.multi))
tempo.multi <- tempo.multi * length(val[[i2]])
}
}
# end list of i numbers that will be split
tempo.cat <- paste0("PARALLELIZATION INITIATED AT: ", ini.date)
cat(paste0("\n", tempo.cat, "\n"))
tempo.thread.nb = parallel::detectCores(all.tests = FALSE, logical = TRUE) # detect the number of threads
if(tempo.thread.nb < thread.nb){
thread.nb <- tempo.thread.nb
}
tempo.cat <- paste0("NUMBER OF THREADS USED: ", thread.nb)
cat(paste0("\n    ", tempo.cat, "\n"))
Clust <- parallel::makeCluster(thread.nb, outfile = paste0(res.path, "/fun_test_parall_log.txt")) # outfile to print or cat during parallelization (only possible in a file, outfile = "" do not work on windows)
tempo.cat <- paste0("SPLIT OF TEST NUMBERS IN PARALLELISATION:")
cat(paste0("\n    ", tempo.cat, "\n"))
cluster.list <- parallel::clusterSplit(Clust, 1:total.comp.nb) # split according to the number of cluster
str(cluster.list) # using print(str()) add a NULL below the result
cat("\n")
paral.output.list <- parallel::clusterApply( # paral.output.list is a list made of thread.nb compartments, each made of n / thread.nb (mat theo column number) compartment. Each compartment receive the corresponding results of fun_permut(), i.e., data (permuted mat1.perm), warning message, cor (final correlation) and count (number of permutations)
cl = Clust,
x = cluster.list,
function.name = function.name, 
instruction = instruction, 
thread.nb = thread.nb, 
print.count = print.count, 
total.comp.nb = total.comp.nb, 
sp.plot.fun = sp.plot.fun,
i.list = i.list, 
fun.tested = fun,
arg.values = arg.values,
fun.test = fun.test,
fun.test2 = fun.test2,
kind = kind,
problem = problem,
res = res,
count = count,
plot.count = plot.count,
data = data,
code = code,
plot.fun = plot.fun, 
res.path = res.path, 
lib.path = lib.path, 
cute.path = cute.path, 
fun = function(
x, 
function.name, 
instruction, 
thread.nb, 
print.count, 
total.comp.nb, 
sp.plot.fun, 
i.list, 
fun.tested, 
arg.values, 
fun.test, 
fun.test2, 
kind, 
problem, 
res, 
count, 
plot.count, 
data, 
code, 
plot.fun, 
res.path, 
lib.path, 
cute.path
){
# check again: very important because another R
process.id <- Sys.getpid()
cat(paste0("\nPROCESS ID ", process.id, " -> TESTS ", x[1], " TO ", x[length(x)], "\n"))
source(cute.path, local = .GlobalEnv)
fun_pack(req.package = "lubridate", lib.path = lib.path, load = TRUE) # load = TRUE to be sure that functions are present in the environment. And this prevent to use R.lib.path argument of fun_python_pack()
# end check again: very important because another R
# plot management
if(plot.fun == TRUE){
pdf(file = paste0(res.path, "/plots_from_fun_test_", x[1], ifelse(length(x) == 1, ".pdf", paste0("-", x[length(x)], ".pdf"))))
}else{
pdf(file = NULL) # send plots into a NULL file, no pdf file created
}
window.nb <- dev.cur()
dev.set(window.nb)
# end plot management
# new environment
env.name <- paste0("env", ini.time)
if(exists(env.name, where = -1)){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": ENVIRONMENT env.name ALREADY EXISTS. PLEASE RERUN ONCE\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}else{
assign(env.name, new.env())
assign("var", var, envir = get(env.name))
}
# end new environment
ini.date <- Sys.time()
ini.time <- as.numeric(ini.date) # time of process begin, converted into 
print.count.loop <- 0
suppressMessages(suppressWarnings(eval(parse(text = code))))
colnames(data) <- arg
if( ! is.null(expect.error)){
data <- data.frame(data, kind = kind, problem = problem, expected.error = expected.error, message = res, stringsAsFactors = FALSE)
}else{
data <- data.frame(data, kind = kind, problem = problem, message = res, stringsAsFactors = FALSE)
}
row.names(data) <- paste0("test_", sprintf(paste0("%0", nchar(total.comp.nb), "d"), x))
sys.info <- sessionInfo()
invisible(dev.off(window.nb))
rm(env.name) # optional, because should disappear at the end of the function execution
# output
output <- list(fun = fun, data = data, instruction = instruction, sys.info = sys.info)
save(output, file = paste0(res.path, "/fun_test_", x[1], ifelse(length(x) == 1, ".RData", paste0("-", x[length(x)], ".RData"))))
if(plot.fun == TRUE & plot.count == 0){
warning(paste0("\nWARNING FROM ", function.name, " IN PROCESS ", process.id, ": NO PDF PLOT BECAUSE ONLY ERRORS REPORTED\n"), call. = FALSE)
file.remove(paste0(res.path, "/plots_from_fun_test_", x[1], ifelse(length(x) == 1, ".pdf", paste0("-", x[length(x)], ".pdf"))))
}
table.out <- as.matrix(output$data)
# table.out[table.out == ""] <- " " # does not work # because otherwise read.table() converts "" into NA
table.out <- gsub(table.out, pattern = "\n", replacement = " ")
write.table(table.out, file = paste0(res.path, "/table_from_fun_test_", x[1], ifelse(length(x) == 1, ".txt", paste0("-", x[length(x)], ".txt"))), row.names = TRUE, col.names = NA, append = FALSE, quote = FALSE, sep = "\t", eol = "\n")
}
)
parallel::stopCluster(Clust)
# txt files assembly
if(length(cluster.list) > 1){
for(i2 in 1:length(cluster.list)){
tempo.name <- paste0(res.path, "/table_from_fun_test_", min(cluster.list[[i2]], na.rm = TRUE), ifelse(length(cluster.list[[i2]]) == 1, ".txt", paste0("-", max(cluster.list[[i2]], na.rm = TRUE), ".txt")))
tempo <- read.table(file = tempo.name, header = TRUE, stringsAsFactors = FALSE, sep = "\t", row.names = 1, comment.char = "", colClasses = "character") #  row.names = 1 (1st column) because now read.table() adds a NA in the header if the header starts by a tabulation, comment.char = "" because colors with #, colClasses = "character" otherwise convert "" (from NULL) into NA
file.remove(tempo.name)
if(i2 == 1){
final.file <- tempo
}else{
final.file <- rbind(final.file, tempo)
}
}
write.table(final.file, file = paste0(res.path, "/table_from_fun_test_1-", total.comp.nb, ".txt"), row.names = TRUE, col.names = NA, append = FALSE, quote = FALSE, sep = "\t", eol = "\n")
if( ! is.null(expect.error)){
final.file <- final.file[ ! final.file$problem == final.file$expected.error, ]
if(nrow(final.file) == 0){
cat(paste0("NO DISCREPANCY BETWEEN EXPECTED AND OBSERVED ERRORS\n\n"))
}else{
cat(paste0("DISCREPANCIES BETWEEN EXPECTED AND OBSERVED ERRORS (SEE THE discrepancy_table_from_fun_test_1-", total.comp.nb, ".txt FILE)\n\n"))
write.table(final.file, file = paste0(res.path, "/discrepancy_table_from_fun_test_1-", total.comp.nb, ".txt"), row.names = TRUE, col.names = NA, append = FALSE, quote = FALSE, sep = "\t", eol = "\n")
}
}
}
# end txt files assembly
}else{
# plot management
if(plot.fun == TRUE){
pdf(file = paste0(res.path, "/plots_from_fun_test_1", ifelse(total.comp.nb == 1, ".pdf", paste0("-", total.comp.nb, ".pdf"))))
}else{
pdf(file = NULL) # send plots into a NULL file, no pdf file created
}
window.nb <- dev.cur()
dev.set(window.nb)
# end plot management
# new environment
env.name <- paste0("env", ini.time)
if(exists(env.name, where = -1)){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": ENVIRONMENT env.name ALREADY EXISTS. PLEASE RERUN ONCE\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}else{
assign(env.name, new.env())
assign("var", var, envir = get(env.name))
}
# end new environment
suppressMessages(suppressWarnings(eval(parse(text = code))))
colnames(data) <- arg
expect.data <- data.frame()
if( ! is.null(expect.error)){
data <- data.frame(data, kind = kind, problem = problem, expected.error = expected.error, message = res, stringsAsFactors = FALSE)
}else{
data <- data.frame(data, kind = kind, problem = problem, message = res, stringsAsFactors = FALSE)
}
row.names(data) <- paste0("test_", sprintf(paste0("%0", nchar(total.comp.nb), "d"), 1:total.comp.nb))
sys.info <- sessionInfo()
invisible(dev.off(window.nb))
rm(env.name) # optional, because should disappear at the end of the function execution
# output
output <- list(fun = fun, data = data, instruction = instruction, sys.info = sys.info)
if(plot.fun == TRUE & plot.count == 0){
warning(paste0("\nWARNING FROM ", function.name, ": NO PDF PLOT BECAUSE ONLY ERRORS REPORTED\n"), call. = FALSE)
file.remove(paste0(res.path, "/plots_from_fun_test_1", ifelse(total.comp.nb == 1, ".pdf", paste0("-", total.comp.nb, ".pdf"))))
}
if( ! is.null(expect.error)){
expect.data <- output$data[ ! output$data$problem == output$data$expected.error, ]
if(nrow(expect.data) == 0){
cat(paste0("NO DISCREPANCY BETWEEN EXPECTED AND OBSERVED ERRORS\n\n"))
}else{
cat(paste0("DISCREPANCIES BETWEEN EXPECTED AND OBSERVED ERRORS (SEE THE ", if(export == TRUE){paste0("discrepancy_table_from_fun_test_1", ifelse(total.comp.nb == 1, "", paste0("-", total.comp.nb)), ".txt FILE")}else{"$data RESULT"}, ")\n\n"))
if(export == TRUE){
expect.data <- as.matrix(expect.data)
expect.data <- gsub(expect.data, pattern = "\n", replacement = "  ")
write.table(expect.data, file = paste0(res.path, "/discrepancy_table_from_fun_test_1", ifelse(total.comp.nb == 1, ".txt", paste0("-", total.comp.nb, ".txt"))), row.names = TRUE, col.names = NA, append = FALSE, quote = FALSE, sep = "\t", eol = "\n")
}
}
}
if(export == TRUE){
save(output, file = paste0(res.path, "/fun_test_1", ifelse(total.comp.nb == 1, ".RData", paste0("-", total.comp.nb, ".RData"))))
table.out <- as.matrix(output$data)
table.out <- gsub(table.out, pattern = "\n", replacement = "  ")
write.table(table.out, file = paste0(res.path, "/table_from_fun_test_1", ifelse(total.comp.nb == 1, ".txt", paste0("-", total.comp.nb, ".txt"))), row.names = TRUE, col.names = NA, append = FALSE, quote = FALSE, sep = "\t", eol = "\n")
}else{
return(output)
}
}
end.date <- Sys.time()
end.time <- as.numeric(end.date)
total.lapse <- round(lubridate::seconds_to_period(end.time - ini.time))
cat(paste0("fun_test JOB END\n\nTIME: ", end.date, "\n\nTOTAL TIME LAPSE: ", total.lapse, "\n\n\n"))
}


################ Object modification


######## fun_name_change() #### check a vector of character strings and modify any string if present in another vector


# Check OK: clear to go Apollo
fun_name_change <- function(data1, data2, added.string = "_modif"){
# AIM
# this function allow to check if a vector of character strings, like column names of a data frame, has elements present in another vector (vector of reserved words or column names of another data frame before merging)
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# fun_check()
# ARGUMENTS
# data1: vector of character strings to check and modify
# data2: reference vector of character strings
# added.string: string added at the end of the modified string in data1 if present in data2
# RETURN
# a list containing
# $data: the modified data1 (in the same order as in the initial data1)
# $ini: the initial elements before modification. NULL if no modification
# $post: the modified elements in the same order as in ini. NULL if no modification
# EXAMPLES
# obs1 <- c("A", "B", "C", "D") ; obs2 <- c("A", "C") ; fun_name_change(obs1, obs2)
# obs1 <- c("A", "B", "C", "C_modif1", "D") ; obs2 <- c("A", "A_modif1", "C") ; fun_name_change(obs1, obs2) # the function checks that the new names are neither in obs1 nor in obs2 (increment the number after the added string)
# DEBUGGING
# data1 = c("A", "B", "C", "D") ; data2 <- c("A", "C") ; added.string = "_modif" # for function debugging
# function name
function.name <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
# end function name
# required function checking
if(length(utils::find("fun_check", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_check() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end required function checking
# argument checking
arg.check <- NULL #
text.check <- NULL #
checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$fun.name))
tempo <- fun_check(data = data1, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = data2, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = added.string, class = "vector", mode = "character", length = 1, fun.name = function.name) ; eval(ee)
if(any(arg.check) == TRUE){
stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
}
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
# end argument checking
# main code
ini <- NULL
post <- NULL
if(any(data1 %in% data2)){
tempo.names <- data1[data1 %in% data2]
ini <- NULL
post <- NULL
for(i2 in 1:length(tempo.names)){
count <- 0
tempo <- tempo.names[i2]
while(any(tempo %in% data2) | any(tempo %in% data1)){
count <- count + 1
tempo <- paste0(tempo.names[i2], "_modif", count)
}
data1[data1 %in% tempo.names[i2]] <- paste0(tempo.names[i2], "_modif", count)
if(count != 0){
ini <- c(ini, tempo.names[i2])
post <- c(post, paste0(tempo.names[i2], "_modif", count))
}
}
data <- data1
}else{
data <- data1
}
output <- list(data = data, ini = ini, post = post)
return(output)
}


######## fun_df_remod() #### remodeling a data frame to have column name as a qualitative values and vice-versa


# Check OK: clear to go Apollo
fun_df_remod <- function(data, quanti.col.name = "quanti", quali.col.name = "quali"){
# AIM
# if the data frame is made of numeric columns, a new data frame is created, with the 1st column gathering all the numeric values, and the 2nd column being the name of the columns of the initial data frame. If row names were present in the initial data frame, then a new ini_rowname column is added with the names of the rows

 
# If the data frame is made of one numeric column and one character or factor column, a new data frame is created, with the new columns corresponding to the split numeric values (according to the character column). NA are added a the end of each column to have the same number of rows. BEWARE: in such data frame, rows are not individuals. This means that in the example below, values 10 and 20 are associated on the same row but that means nothing in term of association

 

# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# fun_check()
# ARGUMENTS
# data: data frame to convert
# quanti.col.name: optional name for the quanti column of the new data frame
# quali.col.name: optional name for the quali column of the new data frame
# RETURN
# the modified data frame
# EXAMPLES
# obs <- data.frame(col1 = (1:4)*10, col2 = c("A", "B", "A", "A")) ; obs ; fun_df_remod(obs)
# obs <- data.frame(col1 = (1:4)*10, col2 = 5:8) ; obs ; fun_df_remod(obs, quanti.col.name = "quanti", quali.col.name = "quali")
# obs <- data.frame(col1 = (1:4)*10, col2 = 5:8) ; rownames(obs) <- paste0("row", 1:4) ; obs ; fun_df_remod(obs, quanti.col.name = "quanti", quali.col.name = "quali")
# DEBUGGING
# data = data.frame(a = 1:3, b = 4:6) ; quanti.col.name = "quanti" ; quali.col.name = "quali" # for function debugging
# data = data.frame(a = 1:3, b = 4:6, c = 11:13) ; quanti.col.name = "quanti" ; quali.col.name = "quali" # for function debugging
# data = data.frame(a = 1:3, b = letters[1:3]) ; quanti.col.name = "quanti" ; quali.col.name = "quali" # for function debugging
# data = data.frame(a = 1:3, b = letters[1:3]) ; quanti.col.name = "TEST" ; quali.col.name = "quali" # for function debugging
# data = data.frame(b = letters[1:3], a = 1:3) ; quanti.col.name = "quanti" ; quali.col.name = "quali" # for function debugging
# data = data.frame(b = c("e", "e", "h"), a = 1:3) ; quanti.col.name = "quanti" ; quali.col.name = "quali" # for function debugging
# function name
function.name <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
# end function name
# required function checking
if(length(utils::find("fun_check", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_check() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end required function checking
# argument checking
# argument checking without fun_check()
if( ! any(class(data) %in% "data.frame")){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": THE data ARGUMENT MUST BE A DATA FRAME\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end argument checking without fun_check()
# argument checking with fun_check()
arg.check <- NULL #
text.check <- NULL #
checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$fun.name))
tempo <- fun_check(data = quanti.col.name, class = "character", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = quali.col.name, class = "character", length = 1, fun.name = function.name) ; eval(ee)
if(any(arg.check) == TRUE){
stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
}
# end argument checking with fun_check()
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
# end argument checking
# main code
tempo.factor <- unlist(lapply(data, class))
for(i in 1:length(tempo.factor)){ # convert factor columns as character
if(all(tempo.factor[i] == "factor")){
data[, i] <- as.character(data[, i])
}
}
tempo.factor <- unlist(lapply(data, mode))
if(length(data) == 2){
if( ! ((mode(data[, 1]) == "character" & mode(data[, 2]) == "numeric") | mode(data[, 2]) == "character" & mode(data[, 1]) == "numeric" | mode(data[, 2]) == "numeric" & mode(data[, 1]) == "numeric") ){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": IF data ARGUMENT IS A DATA FRAME MADE OF 2 COLUMNS, EITHER A COLUMN MUST BE NUMERIC AND THE OTHER CHARACTER, OR THE TWO COLUMNS MUST BE NUMERIC\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if((mode(data[, 1]) == "character" | mode(data[, 2]) == "character") & (quanti.col.name != "quanti" | quali.col.name != "quali")){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": IMPROPER quanti.col.name OR quali.col.name RESETTINGS. THESE ARGUMENTS ARE RESERVED FOR DATA FRAMES MADE OF n NUMERIC COLUMNS ONLY\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
}else{
if( ! all(tempo.factor %in% "numeric")){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": IF data ARGUMENT IS A DATA FRAME MADE OF ONE COLUMN, OR MORE THAN 2 COLUMNS, THESE COLUMNS MUST BE NUMERIC\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
}
if(( ! any(tempo.factor %in% "character")) & is.null(names(data))){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": NUMERIC DATA FRAME in the data ARGUMENT MUST HAVE COLUMN NAMES\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if(all(tempo.factor %in% "numeric")){ # transfo 1
quanti <- NULL
for(i in 1:length(data)){
quanti <-c(quanti, data[, i])
}
quali <- rep(names(data), each = nrow(data))
output.data <- data.frame(quanti, quali)
names(output.data) <- c(quanti.col.name, quali.col.name)
# add the ini_rowname column
ini.rownames <- rownames(data)
tempo.data <- data
rownames(tempo.data) <- NULL
null.rownames <- (tempo.data)
if( ! identical(ini.rownames, null.rownames)){
ini_rowname <- rep(ini.rownames, times = ncol(data))
output.data <- cbind(output.data, ini_rowname)
}
}else{ # transfo 2
if(class(data[, 1]) == "character"){
data <- cbind(data[2], data[1])
}
nc.max <- max(table(data[, 2])) # effectif maximum des classes
nb.na <- nc.max - table(data[,2]) # nombre de NA à ajouter pour réaliser la data frame
tempo<-split(data[, 1], data[, 2])
for(i in 1:length(tempo)){tempo[[i]] <- append(tempo[[i]], rep(NA, nb.na[i]))} # des NA doivent être ajoutés lorsque les effectifs sont différents entre les classes. C'est uniquement pour que chaque colonne ait le même nombre de lignes
output.data<-data.frame(tempo)
}
return(output.data)
}


######## fun_merge() #### merge the columns of two 2D objects, by common rows


fun_merge <- function(data1, data2, name1, name2, factor.as = "numeric", warn.print = FALSE){
# AIM
# merge the columns of 2 data frames or 2 matrices or 2 tables, by associating the rows according to 1 or several common colums that must be strictly similar between the 2 objects
# contrary to the classical merge() function of R, fun_merge() orders the rows of the 2 objects according to the common columns, and merge only and only if the ordered common columns are strictly identical. Otherwise return an error
# keep row names of data1 in the merged object if they exist. Do not consider row names of data2
# keep the intial row order of data1 after merging
# BEWARE:
# see: https://stackoverflow.com/questions/1299871/how-to-join-merge-data-frames-inner-outer-left-right
# see: https://en.wikipedia.org/wiki/Join_(SQL) and the french version
# REQUIRED PACKAGES
# none
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# fun_comp_2d()
# fun_check()
# ARGUMENTS
# data1: matrix or data frame or table
# data2: same class of object as data1 (data frame for data1 data frame, matrix for data1 matrix and table for data1 table) with same number of rows as in data1
# name1: either a vector of character strings or a vector of integer. If character strings, they must be the name of the columns in data1 that are common to the columns in data2. If integers, they must be the column numbers in data1 that are common to column numbers in data2. name1 can be strings and name2 (below) integers, and vice-versa. BEWARE: order of the elements in data1 are important as ordering is according to the first element, then the second, etc.
# name2: as in name1 but for data2. Order in name2 is not important as order in name1 is used for the ordering
# factor.as: either "numeric" (sort factors according to levels order, i.e., class number) or "character" (sort factors according to alphabetical order)
# warn.print: logical. Print warnings at the end of the execution? No print if no warning messages
# RETURN
# a list containing:
# $data: the merged data frame or matrix or table
# $warn: the warning messages. Use cat() for proper display. NULL if no warning
# EXAMPLES
# obs1 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; obs2 = as.data.frame(matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5]))) ; obs1 ; obs2 ; fun_comp_2d(obs1, obs2)
# DEBUGGING
# data1 = matrix(1.0001:21, ncol = 4) ; dimnames(data1) <- list(LETTERS[1:5], letters[1:4]); data2 = matrix(1.0001:31, ncol = 6) ; dimnames(data2) <- list(NULL, c("a", "aa", "c", "d", "aaa", "aaaa")) ; set.seed(1) ; data2[, "c"] <- sample(data2[, "c"]) ; data2[, "d"] <- sample(data2[, "d"]) ; set.seed(NULL) ; data1 ; data2 ; name1 = c("c", "d") ; name2 = c("d", "c") ; factor.as = "numeric" # for function debugging
# function name
function.name <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
# end function name
# required function checking
if(length(utils::find("fun_check", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_check() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end required function checking
# argument checking using fun_check()
arg.check <- NULL #
text.check <- NULL #
checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$fun.name))
tempo1 <- fun_check(data = data1, class = "matrix", print = FALSE)
tempo2 <- fun_check(data = data1, class = "data.frame", print = FALSE)
tempo3 <- fun_check(data = data1, class = "table", print = FALSE)
if(tempo1$problem == TRUE & tempo2$problem == TRUE & tempo3$problem == TRUE){
tempo.cat <- paste0("ERROR IN ", function.name, ":\ndata1 ARGUMENT MUST BE A 2D OBJECT (MATRIX, DATA FRAME OR TABLE)\nHERE IT IS: ", paste(class(data1), collapse = " ")) #
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
tempo1 <- fun_check(data = data2, class = "matrix", print = FALSE)
tempo2 <- fun_check(data = data2, class = "data.frame", print = FALSE)
tempo3 <- fun_check(data = data2, class = "table", print = FALSE)
if(tempo1$problem == TRUE & tempo2$problem == TRUE & tempo3$problem == TRUE){
tempo.cat <- paste0("ERROR IN ", function.name, ":\ndata2 ARGUMENT MUST BE A 2D OBJECT (MATRIX, DATA FRAME OR TABLE)\nHERE IT IS: ", paste(class(data2), collapse = " ")) #
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
if( ! identical(class(data1), class(data2))){
tempo.cat <- paste0("ERROR IN ", function.name, ":\ndata1 and data2 ARGUMENTS MUST BE A 2D OBJECT (MATRIX, DATA FRAME OR TABLE) OF SAME CLASS\nHERE IT IS RESPECTIVELY: ", paste(class(data1), collapse = " "), " AND ", paste(class(data2), collapse = " ")) #
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
tempo1 <- fun_check(data = name1, class = "vector", typeof = "integer", double.as.integer.allowed = TRUE, print = FALSE)
tempo2 <- fun_check(data = name1, class = "vector", typeof = "character", print = FALSE)
if(tempo1$problem == TRUE & tempo2$problem == TRUE){
tempo.cat <- paste0("ERROR IN ", function.name, ":\nname1 ARGUMENT MUST BE A UNIQUE CHARACTER STRING OR INTEGER\nHERE IT IS: ", paste(name1, collapse = " ")) #
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
tempo1 <- fun_check(data = name2, class = "vector", typeof = "integer", double.as.integer.allowed = TRUE, print = FALSE)
tempo2 <- fun_check(data = name2, class = "vector", typeof = "character", print = FALSE)
if(tempo1$problem == TRUE & tempo2$problem == TRUE){
tempo.cat <- paste0("ERROR IN ", function.name, ":\nname2 ARGUMENT MUST BE A UNIQUE CHARACTER STRING OR INTEGER\nHERE IT IS: ", paste(name2, collapse = " ")) #
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
tempo <- fun_check(data = factor.as, options = c("numeric", "character"), length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = warn.print, class = "vector", mode = "logical", length = 1, fun.name = function.name) ; eval(ee)
if(any(arg.check) == TRUE){
stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
}
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
# end argument checking using fun_check()
# other argument checking
# column existence
if(mode(name1) == "character"){
if( ! all(name1 %in% colnames(data1))){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ":\nTHE CHARACTER STRINGS IN name1 ARGUMENT ARE NOT ALL COLUMN NAMES OF data1:\n", paste(name1, collapse = " "), "\n", colnames(data1), "\n\n================\n\n") #
stop(tempo.cat, call. = FALSE)
}
}else if(mode(name1) == "numeric"){
if( ! all((name1 > ncol(data1) & name1 <= 0))){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ":\nINTEGERS IN name1 ARGUMENT ARE NOT ALL COLUMN NUMBERS OF data1:\n", paste(name1, collapse = " "), "\n1:", ncol(data1), "\n\n================\n\n") #
stop(tempo.cat, call. = FALSE)
}
}else{
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": CODE INCONSISTENCY 1\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
if(mode(name2) == "character"){
if( ! all(name2 %in% colnames(data2))){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ":\nTHE CHARACTER STRINGS IN name2 ARGUMENT ARE NOT ALL COLUMN NAMES OF data2:\n", paste(name2, collapse = " "), "\n", colnames(data2), "\n\n================\n\n") #
stop(tempo.cat, call. = FALSE)
}
}else if(mode(name2) == "numeric"){
if( ! all((name2 > ncol(data2) & name2 <= 0))){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ":\nINTEGERS IN name2 ARGUMENT ARE NOT ALL COLUMN NUMBERS OF data2:\n", paste(name2, collapse = " "), "\n1:", ncol(data2), "\n\n================\n\n") #
stop(tempo.cat, call. = FALSE)
}
}else{
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": CODE INCONSISTENCY 2\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
if(length(name1) != length(name2)){
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ":\nLENGTH OF name1 ARGUMENT (", length(name1), ") IS NOT THE SAME AS LENGTH OF name2 ARGUMENT (", length(name2), "):\n", paste(name1, collapse = " "), "\n", paste(name2, collapse = " "), "\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
# end column existence
# end other argument checking
# main code
# definition of set1 and set2: common columns
set1 <- data1[, name1, drop = FALSE] # set1 will be the reference for merging, drop = FALSE to keep the 2D structure
if(any(apply(set1, 2, FUN = "%in%", "factor"))){
if(factor.as == "numeric"){
set1[, apply(set1, 2, FUN = "%in%", "factor")] <- as.numeric(set1[, apply(set1, 2, FUN = "%in%", "factor")])
}
}
set2 <- data2[, name2, drop = FALSE] # set2 will be the reference for merging, drop = FALSE to keep the 2D structure
if(any(apply(set2, 2, FUN = "%in%", "factor"))){
if(factor.as == "numeric"){
set2[, apply(set2, 2, FUN = "%in%", "factor")] <- as.numeric(set2[, apply(set2, 2, FUN = "%in%", "factor")])
}
}
# end definition of set1 and set2: common columns
# conversion as character to avoid floating point problems
options.ini <- options()$digits
options(digits = 22)
set1 <- as.matrix(set1)
set2 <- as.matrix(set2)
mode(set1) <- "character"
mode(set2) <- "character"
options(digits = options.ini)
# end conversion as character to avoid floating point problems
# recovering initial order of set1
ini.set1.order <- eval(parse(text = paste("order(", paste("set1[, ", 1:ncol(set1), "]", sep = "", collapse = ", "), ")")))
set1 <- set1[ini.set1.order, ]
ini.set2.order <- eval(parse(text = paste("order(", paste("set2[, ", 1:ncol(set2), "]", sep = "", collapse = ", "), ")")))
set2 <- set2[ini.set2.order, ]
# end recovering initial order of set1
# check non identical columns
if(length(name1) > 1){
for(i2 in 1:(length(name1) - 1)){
for(i3 in (i2 + 1):length(name1)){
if(identical(set1[, i2], set1[, i3])){
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ":\nCOLUMN ", i2, " OF data1 CORRESPONDING TO ELEMENT ", name1[i2], " OF name1 ARGUMENT IS IDENTICAL TO COLUMN ", i3, " OF data1 CORRESPONDING TO ELEMENT ", name1[i3], " OF name1 ARGUMENT\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
}
}
}
if(length(name2) > 1){
for(i2 in 1:(length(name2) - 1)){
for(i3 in (i2 + 1):length(name2)){
if(identical(set2[, i2], set2[, i3])){
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ":\nCOLUMN ", i2, " OF data2 CORRESPONDING TO ELEMENT ", name2[i2], " OF name2 ARGUMENT IS IDENTICAL TO COLUMN ", i3, " OF data2 CORRESPONDING TO ELEMENT ", name2[i3], " OF name2 ARGUMENT\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
}
}
}
# end check non identical columns
# warning duplicates
# repositioning of the column in set2 as in set1 by comparing the two sorted column
#deal with identical col names when merging -> .x for data1, .y for data2


if(warn.print == TRUE & ! is.null(warn)){
warning(warn, call. = FALSE)
}
# output <- list()
return(output)
}


######## fun_round() #### rounding number if decimal present


# Check OK: clear to go Apollo
fun_round <- function(data, dec.nb = 2, after.lead.zero = TRUE){
# AIM
# round a vector of values, if decimal, with the desired number of decimal digits after the decimal leading zeros
# WARNINGS
# Work well with numbers as character strings, but not always with numerical numbers because of the floating point
# Numeric values are really truncated from a part of their decimal digits, whatever options(digits) settings
# See ?.Machine or https://stackoverflow.com/questions/5173692/how-to-return-number-of-decimal-places-in-r, with the interexting formula: abs(x - round(x)) > .Machine$double.eps^0.5
# ARGUMENTS
# data: a vector of numbers (numeric or character mode)
# dec.nb: number of required decimal digits
# after.lead.zero: logical. If FALSE, rounding is performed for all the decimal numbers, whatever the leading zeros (e.g., 0.123 -> 0.12 and 0.00128 -> 0.00). If TRUE, dec.nb are taken after the leading zeros (e.g., 0.123 -> 0.12 and 0.00128 -> 0.0013)
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# fun_check()
# RETURN
# the modified vector
# EXAMPLES
# ini.options <- options()$digits ; options(digits = 8) ; cat(fun_round(data = c(NA, 10, 100.001, 333.0001254, 12312.1235), dec.nb = 2, after.lead.zero = FALSE), "\n\n") ; options(digits = ini.options)
# ini.options <- options()$digits ; options(digits = 8) ; cat(fun_round(data = c(NA, 10, 100.001, 333.0001254, 12312.1235), dec.nb = 2, after.lead.zero = TRUE), "\n\n") ; options(digits = ini.options)
# ini.options <- options()$digits ; options(digits = 8) ; cat(fun_round(data = c(NA, "10", "100.001", "333.0001254", "12312.1235"), dec.nb = 2, after.lead.zero = FALSE), "\n\n") ; options(digits = ini.options)
# ini.options <- options()$digits ; options(digits = 8) ; cat(fun_round(data = c(NA, "10", "100.001", "333.0001254", "12312.1235"), dec.nb = 2, after.lead.zero = TRUE), "\n\n") ; options(digits = ini.options)
# DEBUGGING
# data = data = c(10, 100.001, 333.0001254, 12312.1235) ; dec.nb = 2 ; after.lead.zero = FALSE # # for function debugging
# data = data = c("10", "100.001", "333.0001254", "12312.1235") ; dec.nb = 2 ; after.lead.zero = TRUE # # for function debugging
# function name
function.name <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
# end function name
# required function checking
if(length(utils::find("fun_check", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_check() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end required function checking
# argument checking
# argument checking without fun_check()
if( ! (all(typeof(data) == "character") | all(typeof(data) == "double") | all(typeof(data) == "integer"))){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": data ARGUMENT MUST BE A VECTOR OF NUMBERS (IN NUMERIC OR CHARACTER MODE)\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end argument checking without fun_check()
# argument checking with fun_check()
arg.check <- NULL #
text.check <- NULL #
checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$fun.name))
tempo <- fun_check(data = data, class = "vector", na.contain = TRUE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = dec.nb, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = after.lead.zero, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
if(any(arg.check) == TRUE){
stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
}
# end argument checking with fun_check()
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
# end argument checking
# main code
tempo <- grepl(x = data, pattern = "\\.") # detection of decimal numbers
ini.mode <- mode(data)
data <- as.character(data) # to really truncate decimal digits
for(i in 1:length(data)){ # scan all the numbers of the vector
if(tempo[i] == TRUE){ # means decimal number
if(after.lead.zero == TRUE){
zero.pos <- unlist(gregexpr(text=data[i], pattern = 0)) # recover all the position of the zeros in the number. -1 if no zeros (do not record the leading and trailing zeros)
}else{
zero.pos <- -1 # -1 as if no zero
}
dot.pos <- unlist(gregexpr(text=data[i], pattern = "\\.")) # recover all the position of the zeros in the number
digit.pos <- unlist(gregexpr(text=data[i], pattern = "[[:digit:]]")) # recover all the position of the digits in the number
dec.pos <- digit.pos[digit.pos > dot.pos]
count <- 0
while((dot.pos + count + 1) %in% zero.pos & (dot.pos + count + 1) <= max(dec.pos) & (count + dec.nb) < length(dec.pos)){ # count the number of leading zeros in the decimal part
count <- count + 1
}
data[i] <- formatC(as.numeric(data[i]), digits = (count + dec.nb), format = "f")
}
}
if(ini.mode != "character"){
data <- as.numeric(data)
}
return(data)
}


######## fun_mat_rotate() #### 90° clockwise matrix rotation


# Check OK: clear to go Apollo
fun_mat_rotate <- function(data){
# AIM
# 90° clockwise matrix rotation
# applied twice, the function provide the mirror matrix, according to vertical and horizontal symmetry
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# fun_check()
# ARGUMENTS
# data: matrix (matrix class)
# RETURN
# the modified matrix
# EXAMPLES
# obs <- matrix(1:10, ncol = 1) ; obs ; fun_mat_rotate(obs)
# obs <- matrix(LETTERS[1:10], ncol = 5) ; obs ; fun_mat_rotate(obs)
# DEBUGGING
# data = matrix(1:10, ncol = 1)
# function name
function.name <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
# end function name
# required function checking
if(length(utils::find("fun_check", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_check() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end required function checking
# argument checking
arg.check <- NULL #
text.check <- NULL #
checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$fun.name))
tempo <- fun_check(data = data, class = "matrix", fun.name = function.name) ; eval(ee)
if(any(arg.check) == TRUE){
stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
}
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
# end argument checking
# main code
for (i in 1:ncol(data)){data[,i] <- rev(data[,i])}
data <- t(data)
return(data)
}


######## fun_mat_num2color() #### convert a numeric matrix into hexadecimal color matrix


# Check OK: clear to go Apollo
fun_mat_num2color <- function(mat1, mat.hsv.h = TRUE, notch = 1, s = 1, v = 1, forced.color = NULL){
# AIM
# convert a matrix made of numbers into a hexadecimal matrix for rgb colorization
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# fun_check()
# ARGUMENTS:
# mat1: matrix 1 of non negative numerical values that has to be colored (matrix class). NA allowed
# mat.hsv.h: logical. Is mat1 the h of hsv colors ? (if TRUE, mat1 must be between zero and 1)
# notch: single value between 0 and 1 to shift the successive colors on the hsv circle by + notch
# s: s argument of hsv(). Must be between 0 and 1
# v: v argument of hsv(). Must be between 0 and 1
# forced.color: Must be NULL or hexadecimal color code or name given by colors(). The first minimal values of mat1 will be these colors. All the color of mat1 can be forced using this argument
# RETURN
# a list containing:
# $mat1.name: name of mat1
# $colored.mat: colors of mat1 in hexa
# $problem: logical. Is any colors of forced.color overlap the colors designed by the function. NULL if forced.color = NULL
# $text.problem: text when overlapping colors. NULL if forced.color = NULL or problem == FALSE
# EXAMPLES
# mat1 = matrix(c(1,1,1,2,1,5,9,NA), ncol = 2) ; dimnames(mat1) <- list(LETTERS[1:4], letters[1:2]) ; fun_mat_num2color(mat1, mat.hsv.h = FALSE, notch = 1, s = 1, v = 1, forced.color = NULL)
# DEBUGGING
# mat1 = matrix(c(1,1,1,2,1,5,9,NA), ncol = 2) ; dimnames(mat1) <- list(LETTERS[1:4], letters[1:2]); mat.hsv.h = FALSE ; notch = 1 ; s = 1 ; v = 1 ; forced.color = c(hsv(1,1,1), hsv(0,0,0)) # for function debugging
# function name
function.name <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
# end function name
# required function checking
if(length(utils::find("fun_check", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_check() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end required function checking
# argument checking
# argument checking with fun_check()
arg.check <- NULL #
text.check <- NULL #
checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$fun.name))
tempo <- fun_check(data = mat1, mode = "numeric", class = "matrix", na.contain = TRUE, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = mat.hsv.h, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = notch, class = "vector", mode = "numeric", length = 1, prop = TRUE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = s, class = "vector", mode = "numeric", length = 1, prop = TRUE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = v, class = "vector", mode = "numeric", length = 1, prop = TRUE, fun.name = function.name) ; eval(ee)
if(any(arg.check) == TRUE){
stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
}
# end argument checking with fun_check()
# argument checking without fun_check()
if(mat.hsv.h == TRUE & fun_check(data = mat1, mode = "numeric", prop = TRUE, print = FALSE)$problem == TRUE){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": mat1 ARGUMENT MUST BE A MATRIX OF PROPORTIONS SINCE THE mat.hsv.h ARGUMENT IS SET TO TRUE\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if( ! is.null(forced.color)){
tempo <- fun_check(data = forced.color, class = "character")
if(any(tempo$problem == TRUE)){
paste0("\n\n================\n\n", paste(tempo$text[tempo$problem], collapse = "\n"), "\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if( ! all(forced.color %in% colors() | grepl(pattern = "^#", forced.color))){ # check that all strings of forced.color start by #
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": forced.color ARGUMENT MUST BE A HEXADECIMAL COLOR VECTOR STARTING BY # AND/OR COLOR NAMES GIVEN BY colors()\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
}
# end argument checking without fun_check()
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
# end argument checking
# main code
problem <- NULL
text.problem <- NULL
mat1.name <- deparse(substitute(mat1))
# change the scale of the plotted matrix
if(mat.hsv.h == TRUE){
if(any(min(mat1, na.rm = TRUE) < 0 | max(mat1, na.rm = TRUE) > 1, na.rm = TRUE)){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": mat1 MUST BE MADE OF VALUES BETWEEN 0 AND 1 BECAUSE mat.hsv.h ARGUMENT SET TO TRUE\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
}else{
if(any(mat1 - floor(mat1) > 0, na.rm = TRUE) | any(mat1 == 0, na.rm = TRUE)){ # no need of isTRUE(all.equal()) because we do not require approx here but strictly 0, thus == is ok
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": mat1 MUST BE MADE OF INTEGER VALUES WITHOUT 0 BECAUSE mat.hsv.h ARGUMENT SET TO FALSE\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}else{
mat1 <- mat1 / max(mat1, na.rm = TRUE)
}
}
if(notch != 1){
different.color <- unique(as.vector(mat1))
different.color <- different.color[ ! is.na(different.color)]
tempo.different.color <- different.color + c(0, cumsum(rep(notch, length(different.color) - 1)))
tempo.different.color <- tempo.different.color - floor(tempo.different.color)
if(any(duplicated(tempo.different.color) == TRUE)){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": DUPLICATED VALUES AFTER USING notch (", paste(tempo.different.color[duplicated(tempo.different.color)], collapse = " "), "). TRY ANOTHER notch VALUE\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}else if(length(different.color) != length(tempo.different.color)){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": LENGTH OF different.color (", paste(different.color, collapse = " "), ") DIFFERENT FROM LENGTH OF tempo.different.color (", paste(tempo.different.color, collapse = " "), ")\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}else{
for(i in 1:length(different.color)){
mat1[mat1 == different.color[i]] <- tempo.different.color[i] # no need of isTRUE(all.equal()) because different.color comes from mat1
}
}
}
if( ! is.null(forced.color)){
hexa.values.to.change <- hsv(unique(sort(mat1))[1:length(forced.color)], s, v)
}
mat1[ ! is.na(mat1)] <- hsv(mat1[ ! is.na(mat1)], s, v)
if( ! is.null(forced.color)){
if(any(forced.color %in% mat1, na.rm = TRUE)){
problem <- TRUE
text.problem <- paste0("THE FOLLOWING COLORS WHERE INTRODUCED USING forced.color BUT WHERE ALREADY PRESENT IN THE COLORED MATRIX :", paste(forced.color[forced.color %in% mat1], collapse = " "))
}else{
problem <- FALSE
}
for(i in 1:length(hexa.values.to.change)){
if( ! any(mat1 == hexa.values.to.change[i], na.rm = TRUE)){# no need of isTRUE(all.equal()) because character
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": THE ", hexa.values.to.change[i], " VALUE FROM hexa.values.to.change IS NOT REPRESENTED IN mat1 : ", paste(unique(as.vector(mat1)), collapse = " "), "\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}else{
mat1[which(mat1 == hexa.values.to.change[i])] <- forced.color[i] # no need of isTRUE(all.equal()) because character
}
}
}
output <- list(mat1.name = mat1.name, colored.mat = mat1, problem = problem, text.problem = text.problem)
return(output)
}


######## fun_mat_op() #### assemble several matrices with operation


# Check OK: clear to go Apollo
fun_mat_op <- function(mat.list, kind.of.operation = "+"){
# AIM
# assemble several matrices of same dimensions by performing by case operation. For instance add the value of all the case 1 (row1 & column1) of the matrices and put it in the case 1 of a new matrix M, add the value of all the case 2 (row2 & column1) of the matrices and put it in the case 2 of a new matrix M, etc.
 
# c: case
# i: row number
# j: column number
# k: matrix number
# z: number of matrices
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# fun_check()
# fun_comp_2d()
# ARGUMENTS:
# mat.list: list of matrices
# kind.of.operation: either "+" (by case addition), "-" (by case subtraction) or "*" (by case multiplication)
# RETURN
# the assembled matrix, with row and/or column names only if all the matrices have identical row/column names
# EXAMPLES
# mat1 = matrix(c(1,1,1,2,1,5,9,8), ncol = 2) ; mat2 = matrix(c(1,1,1,2,1,5,9,NA), ncol = 2) ; fun_mat_op(mat.list = list(mat1, mat2), kind.of.operation = "+")
# mat1 = matrix(c(1,1,1,2,1,5,9,8), ncol = 2, dimnames = list(LETTERS[1:4], letters[1:2])) ; mat2 = matrix(c(1,1,1,2,1,5,9,NA), ncol = 2, dimnames = list(LETTERS[1:4], letters[1:2])) ; fun_mat_op(mat.list = list(mat1, mat2), kind.of.operation = "*")
# mat1 = matrix(c(1,1,1,2,1,5,9,8), ncol = 2, dimnames = list(LETTERS[1:4], c(NA, NA))) ; mat2 = matrix(c(1,1,1,2,1,5,9,NA), ncol = 2, dimnames = list(LETTERS[1:4], letters[1:2])) ; fun_mat_op(mat.list = list(mat1, mat2), kind.of.operation = "-")
# mat1 = matrix(c(1,1,1,2,1,5,9,8), ncol = 2, dimnames = list(c("A1", "A2", "A3", "A4"), letters[1:2])) ; mat2 = matrix(c(1,1,1,2,1,5,9,NA), ncol = 2, dimnames = list(LETTERS[1:4], letters[1:2])) ; mat3 = matrix(c(1,1,1,2,1,5,9,NA), ncol = 2, dimnames = list(LETTERS[1:4], letters[1:2])) ; fun_mat_op(mat.list = list(mat1, mat2, mat3), kind.of.operation = "+")
# DEBUGGING
# mat1 = matrix(c(1,1,1,2,1,5,9,8), ncol = 2) ; mat2 = matrix(c(1,1,1,2,1,5,9,NA), ncol = 2) ; mat.list = list(mat1, mat2) ; kind.of.operation = "+" # for function debugging
# mat1 = matrix(c(1,1,1,2,1,5,9,8), ncol = 2, dimnames = list(LETTERS[1:4], c(NA, NA))) ; mat2 = matrix(c(1,1,1,2,1,5,9,NA), ncol = 2, dimnames = list(LETTERS[1:4], letters[1:2])) ; mat.list = list(mat1, mat2) ; kind.of.operation = "*" # for function debugging
# function name
function.name <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
# end function name
# required function checking
if(length(utils::find("fun_check", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_check() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if(length(utils::find("fun_check", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_comp_2d() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end required function checking
# argument checking
# argument checking with fun_check()
arg.check <- NULL #
text.check <- NULL #
checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$fun.name))
tempo <- fun_check(data = mat.list, class = "list", fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = kind.of.operation, options = c("+", "-", "*"), length = 1, fun.name = function.name) ; eval(ee)
if(any(arg.check) == TRUE){
stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
}
# end argument checking with fun_check()
# argument checking without fun_check()
if(length(mat.list) < 2){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": mat.list ARGUMENT MUST BE A LIST CONTAINING AT LEAST 2 MATRICES\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
for(i1 in 1:length(mat.list)){
tempo <- fun_check(data = mat.list[[i1]], class = "matrix", mode = "numeric", na.contain = TRUE)
if(tempo$problem == TRUE){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": ELEMENT ", i1, " OF mat.list ARGUMENT MUST BE A NUMERIC MATRIX\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
}
ident.row.names <- TRUE
ident.col.names <- TRUE
for(i1 in 2:length(mat.list)){
tempo <- fun_comp_2d(data1 = mat.list[[1]], data2 = mat.list[[i1]])
if(tempo$same.dim == FALSE){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": MATRIX ", i1, " OF mat.list ARGUMENT MUST HAVE THE SAME DIMENSION (", paste(dim(mat.list[[i1]]), collapse = " "), ") THAN THE MATRIX 1 IN mat.list (", paste(dim(mat.list[[1]]), collapse = " "), ")\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if( ! is.null(tempo$same.row.name)){
if(tempo$same.row.name != TRUE){ # != TRUE to deal with NA
ident.row.names <- FALSE
}
}
if( ! is.null(tempo$same.col.name)){
if(tempo$same.col.name != TRUE){ # != TRUE to deal with NA
ident.col.names <- FALSE
}
}
}
# end argument checking without fun_check()
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
# end argument checking
# main code
output <- mat.list[[1]]
for(i1 in 2:length(mat.list)){
output <- get(kind.of.operation)(output, mat.list[[i1]])
}
dimnames(output) <- NULL
if(ident.row.names == TRUE){
rownames(output) <- rownames(mat.list[[1]])
}
if(ident.col.names == TRUE){
colnames(output) <- colnames(mat.list[[1]])
}
return(output)
}


######## fun_mat_inv() #### return the inverse of a square matrix


# Check OK: clear to go Apollo
fun_mat_inv <- function(mat){
# AIM
# return the inverse of a square matrix when solve() cannot
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# fun_check()
# ARGUMENTS:
# mat: a square numeric matrix without NULL, NA, Inf or single case (dimension 1, 1) of 0
# RETURN
# the inversed matrix
# EXAMPLES
# mat1 = matrix(c(1,1,1,2,1,5,9,8,9), ncol = 3) ; fun_mat_inv(mat = mat1) # use solve()
# mat1 = matrix(c(0,0,0,0,0,0,0,0,0), ncol = 3) ; fun_mat_inv(mat = mat1) # use the trick
# mat1 = matrix(c(1,1,1,2,Inf,5,9,8,9), ncol = 3) ; fun_mat_inv(mat = mat1)
# mat1 = matrix(c(1,1,1,2,NA,5,9,8,9), ncol = 3) ; fun_mat_inv(mat = mat1)
# mat1 = matrix(c(1,2), ncol = 1) ; fun_mat_inv(mat = mat1)
# mat1 = matrix(0, ncol = 1) ; fun_mat_inv(mat = mat1)
# mat1 = matrix(2, ncol = 1) ; fun_mat_inv(mat = mat1)
# DEBUGGING
# mat = matrix(c(1,1,1,2,1,5,9,8,9), ncol = 3) # for function debugging
# function name
function.name <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
# end function name
# required function checking
if(length(utils::find("fun_check", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_check() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end required function checking
# argument checking
# argument checking with fun_check()
arg.check <- NULL #
text.check <- NULL #
checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$fun.name))
tempo <- fun_check(data = mat, class = "matrix", mode = "numeric", fun.name = function.name) ; eval(ee)
if(any(arg.check) == TRUE){
stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
}
# end argument checking with fun_check()
# argument checking without fun_check()
if(ncol(mat) != nrow(mat)){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": mat ARGUMENT MUST BE A SQUARE MATRIX\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if(any(mat %in% c(Inf, -Inf, NA))){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": mat ARGUMENT MUST BE A MATRIX WITHOUT Inf, -Inf OR NA\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if(all(mat == 0) & ncol(mat) == 1){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": mat ARGUMENT CANNOT BE A SQUARE MATRIX MADE OF A SINGLE CASE OF 0\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end argument checking without fun_check()
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
# end argument checking
# main code
if(any(grepl(x = try(solve(mat), silent = TRUE)[], pattern = "[Ee]rror"))){
tempo <- svd(mat)
val.critique <- which(tempo$d < 10^-8)
Diag.mod <- diag(1 / tempo$d)
for(i in val.critique){
Diag.mod[i, i] <- 0
}
return(tempo$v %*% Diag.mod %*% t(tempo$u))
}else{
return(solve(mat))
}
}


######## fun_mat_fill() #### fill the empty half part of a symmetric square matrix


# Check OK: clear to go Apollo
fun_mat_fill <- function(mat, empty.cell.string = 0, warn.print = FALSE){
# AIM
# detect the empty half part of a symmetric square matrix (either topleft, topright, bottomleft or bottomright)
# fill this empty half part using the other symmetric half part of the matrix
# WARNINGS
# a plot verification using fun_gg_heatmap() is recommanded
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# fun_check()
# ARGUMENTS:
# mat: a numeric or character square matrix with the half part (according to the grand diagonal) filled with NA (any kind of matrix), "0" (character matrix) or 0 (numeric matrix) exclusively (not a mix of 0 and NA in the empty part)
# empty.cell.string: a numeric, character or NA (no quotes) indicating what empty cells are filled with
# warn.print: logical. Print warnings at the end of the execution? No print if no warning messages
# RETURN
# a list containing:
# $mat: the filled matrix
# $warn: the warning messages. Use cat() for proper display. NULL if no warning
# EXAMPLES
# mat1 = matrix(c(1,NA,NA,NA, 0,2,NA,NA, NA,3,4,NA, 5,6,7,8), ncol = 4) ; mat1 ; fun_mat_fill(mat = mat1, empty.cell.string = NA, warn.print = TRUE) # bottomleft example
# mat1 = matrix(c(1,1,1,2, 0,2,3,0, NA,3,0,0, 5,0,0,0), ncol = 4) ; mat1 ; fun_mat_fill(mat = mat1, empty.cell.string = NA, warn.print = TRUE) # error example
# mat1 = matrix(c(1,1,1,2, 0,2,3,0, NA,3,0,0, 5,0,0,0), ncol = 4) ; mat1 ; fun_mat_fill(mat = mat1, empty.cell.string = 0, warn.print = TRUE) # bottomright example
# mat1 = matrix(c(1,1,1,2, "a",2,3,NA, "a","a",0,0, "a","a","a",0), ncol = 4) ; mat1 ; fun_mat_fill(mat = mat1, empty.cell.string = "a", warn.print = TRUE) # topright example
# mat1 = matrix(c(0,0,0,2, 0,0,3,0, 0,3,0,NA, 5,0,0,0), ncol = 4) ; mat1 ; fun_mat_fill(mat = mat1, empty.cell.string = 0, warn.print = TRUE) # topleft example
# mat1 = matrix(c(0,0,0,2, 0,0,3,0, 0,3,0,0, 5,0,0,0), ncol = 4) ; mat1 ; fun_mat_fill(mat = mat1, empty.cell.string = 0, warn.print = TRUE) # error example
# DEBUGGING
# mat = matrix(c(1,NA,NA,NA, 0,2,NA,NA, NA,3,4,NA, 5,6,7,8), ncol = 4) ; empty.cell.string = NA ; warn.print = TRUE # for function debugging
# mat = matrix(c(0,0,0,2, 0,0,3,0, 0,3,0,NA, 5,0,0,0), ncol = 4) ; empty.cell.string = 0 ; warn.print = TRUE # for function debugging # topleft example
# mat = matrix(c(0,0,0,2, 0,0,3,0, 0,3,0,NA, 5,0,0,0), ncol = 4) ; empty.cell.string = NA ; warn.print = TRUE # for function debugging # topleft example
# function name
function.name <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
# end function name
# required function checking
if(length(utils::find("fun_check", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_check() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end required function checking
# argument checking
# argument checking with fun_check()
arg.check <- NULL #
text.check <- NULL #
checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$fun.name))
tempo <- fun_check(data = mat, class = "matrix", na.contain = TRUE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = empty.cell.string, class = "vector", na.contain = TRUE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = warn.print, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
if(any(arg.check) == TRUE){
stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
}
# end argument checking with fun_check()
# argument checking without fun_check()
if(ncol(mat) != nrow(mat)){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": mat ARGUMENT MUST BE A SQUARE MATRIX\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if( ! (mode(mat) %in% c("numeric", "character"))){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": mat ARGUMENT MUST BE A NUMERIC OR CHARACTER MATRIX\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if(nrow(mat) == 1 & ncol(mat) == 1){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": mat ARGUMENT CANNOT BE A SQUARE MATRIX MADE OF A SINGLE CASE\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if(ifelse(is.na(empty.cell.string), ! any(is.na(mat)), ! any(mat == empty.cell.string, na.rm = TRUE))){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": mat ARGUMENT MATRIX MUST HAVE CELLS WITH THE EMPTY STRING SPECIFIED IN empty.cell.string ARGUMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end argument checking without fun_check()
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
# end argument checking
# main code
list.diag <- vector("list", length = nrow(mat) - 1)
for(i1 in 1:(nrow(mat) - 1)){
list.diag[[i1]] <- numeric(length = nrow(mat) - i1)
}
sector <- c("topleft", "topright", "bottomright", "bottomleft")
diag.scan <-c( # same order as sector. Recover each diag from center to corner
"mat[as.matrix(as.data.frame(list(1:(nrow(mat) - i2), (ncol(mat) -i2):1)))]", # topleft part
"mat[as.matrix(as.data.frame(list(1:(nrow(mat) - i2), (1:ncol(mat))[-(1:i2)])))]", # topright part
"mat[as.matrix(as.data.frame(list((1 + i2):nrow(mat), ncol(mat):(1 + i2))))]", # bottomright part
"mat[as.matrix(as.data.frame(list((1 + i2):nrow(mat), 1:(ncol(mat) -i2))))]" # bottomleft part
)
# empty part detection
tempo.list.diag <- list.diag
empty.sector <- NULL
full.sector <- NULL
warn <- NULL
warn.count <- 0
for(i1 in 1:length(sector)){
tempo.list.diag <- list.diag
for(i2 in 1:(nrow(mat) - 1)){
tempo.list.diag[[i2]] <- eval(parse(text = diag.scan[i1]))
if(ifelse(is.na(empty.cell.string), ! all(is.na(tempo.list.diag[[i2]])), ! (all(tempo.list.diag[[i2]] == empty.cell.string, na.rm = TRUE) & ! (is.na(all(tempo.list.diag[[i2]] == empty.cell.string, na.rm = FALSE)))))){ # I had to add this ! (is.na(all(tempo.list.diag[[i2]] == empty.cell.string, na.rm = FALSE))) because all(tempo.list.diag[[i2]] == empty.cell.string, na.rm = FALSE) gives NA and not FALSE if one NA in tempo.list.diag[[i2]] -> not good for if()
full.sector <- c(full.sector, sector[i1])
break
}
}
if(i1 == nrow(mat) - 1){
if(all(unlist(lapply(tempo.list.diag, FUN = function(x){if(is.na(empty.cell.string)){is.na(x)}else{x == empty.cell.string}})), na.rm = TRUE)){
empty.sector <- c(empty.sector, sector[i1])
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") FROM FUNCTION ", function.name, ": EMPTY SECTOR DETECTED ON THE ", toupper(sector[i1]), " CORNER, FULL OF ", empty.cell.string)
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}else{
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": THE ", toupper(sector[i1]), " SECTOR, DETECTED AS EMPTY, IS NOT? DIFFERENT VALUES IN THIS SECTOR:\n", paste(names(table(unlist(tempo.list.diag), useNA = "ifany")), collapse = " "), "\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
}
}
# end empty part detection
if(length(empty.sector) == 0){
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") FROM FUNCTION ", function.name, ": ACCORDING TO empty.cell.string ARGUMENT (", empty.cell.string, "), mat ARGUMENT MATRIX HAS ZERO EMPTY HALF PART")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}else{
if(length(empty.sector) > 1){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": ACCORDING TO empty.cell.string ARGUMENT (", empty.cell.string, "), mat ARGUMENT MATRIX HAS MORE THAN ONE EMPTY HALF PART (ACCORDING TO THE GRAND DIAGONAL): ", paste(empty.sector, collapse = " "), "\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}else if(any(full.sector %in% empty.sector, na.rm = TRUE)){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": THE FUNCTION HAS DETECTED EMPTY AND NON EMPTY HALF PART IN THE SAME SECTOR: ", paste(full.sector[full.sector %in% empty.sector], collapse = " "), "\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}else if(length(empty.sector) + length(full.sector)!= 4){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": THE FUNCTION HAS DETECTED MORE OR LESS SECTORS THAN 4:\nHALF SECTORS:", paste(empty.sector, collapse = " "), "\nFULL SECTORS:", paste(full.sector, collapse = " "), "\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}else{
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") FROM FUNCTION ", function.name, ": ", toupper(empty.sector), " SECTOR HAS BEEN COMPLETED TO BECOME SYMMETRICAL")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}
# matrix filling
for(i1 in 1:(nrow(mat) - 1)){
if(empty.sector == "topleft"){
eval(parse(text = paste0(diag.scan[1], " <- ", diag.scan[3])))
}else if(empty.sector == "topright"){
eval(parse(text = paste0(diag.scan[2], " <- ", diag.scan[4])))
}else if(empty.sector == "bottomright"){
eval(parse(text = paste0(diag.scan[3], " <- ", diag.scan[1])))
}else if(empty.sector == "bottomleft"){
eval(parse(text = paste0(diag.scan[4], " <- ", diag.scan[2])))
}
}
# end matrix filling
}
if(warn.print == TRUE & ! is.null(warn)){
warning(warn, call. = FALSE)
}
return(list(mat = mat, warn = warn))
}


######## fun_permut() #### progressively breaks a vector order


fun_permut <- function(data1, data2 = NULL, n = NULL, seed = NULL, print.count = 10, text.print = "", cor.method = "spearman", cor.limit = 0.2, warn.print = FALSE, lib.path = NULL){
# AIM
# reorder the elements of the data1 vector by flipping 2 randomly selected  consecutive positions either:
# 1) n times (when n is precised) or
# 2) until the correlation between data1 and data2 decreases down to the cor.limit (0.2 by default). See cor.limit below to deal with negative correlations
# Example of consecutive position flipping: ABCD -> BACD -> BADC, etc.
# WARNINGS
# see # https://www.r-bloggers.com/strategies-to-speedup-r-code/ for code speedup
# the random switch of non consecutive positions (ABCD -> DBCA for instance) does not work very well as the correaltion is quickly obtained but the initial vector structure is mainly kept (no much order). Ths code would be: pos <- ini.pos[1:2] ; pos <- sample.int(n = n , size = 2, replace = FALSE) ; tempo.pos[pos] <- tempo.pos[rev(pos)]
# ARGUMENTS
# data1: a vector of at least 2 elements. Must be numeric if data2 is specified
# data2: a numeric vector of same length as data1
# n: number of times "flipping 2 randomly selected consecutive positions". Ignored if data2 is specified
# seed: integer number used by set.seed(). Write NULL if random result is required, an integer otherwise. BEWARE: if not NULL, fun_permut() will systematically return the same result when the other parameters keep the same settings
# print.count: interger value. Print a working progress message every print.count during loops. BEWARE: can increase substentially the time to complete the process using a small value, like 10 for instance. Use Inf is no loop message desired
# text.print: optional message to add to the working progress message every print.count loop
# cor.method: correlation method. Either "pearson", "kendall" or "spearman". Ignored if data2 is not specified
# cor.limit: a correlation limit (between 0 and 1). Ignored if data2 is not specified. Compute the correlation between data1 and data2, permute the data1 values, and stop the permutation process when the correlation between data1 and data2 decreases down below the cor limit value (0.2 by default). If cor(data1, data2) is negative, then -cor.limit is used and the process stops until the correlation between data1 and data2 increases up over cor.limit (-0.2 by default). BEWARE: write a positive cor.limit even if cor(data1, data2) is known to be negative. The function will automatically uses -cor.limit. If the initial correlation is already below cor.limit (positive correlation) or over -cor.limit (negative correlation), then the data1 value positions are completely randomized (correlation between data1 and data2 is expected to be 0)
# warn.print: logical. Print warnings at the end of the execution? No print if no warning messages
# lib.path: absolute path of the required packages, if not in the default folders
# REQUIRED PACKAGES
# lubridate
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# fun_check()
# fun_pack()
# fun_round()
# RETURN
# a list containing:
# $data: the modified vector
# $warn: potential warning messages (in case of negative correlation when data2 is specified). NULL if non warning message
# $cor: a spearman correlation between the initial positions (1:length(data1) and the final positions if data2 is not specified and the final correlation between data1 and data2 otherwise, according to cor.method
# $count: the number of loops used
# EXAMPLES
# example (1) showing that for loop, used in fun_permut(), is faster than while loop
# ini.time <- as.numeric(Sys.time()) ; count <- 0 ; for(i0 in 1:1e9){count <- count + 1} ; tempo.time <- as.numeric(Sys.time()) ; tempo.lapse <- round(lubridate::seconds_to_period(tempo.time - ini.time)) ; tempo.lapse
# example (2) showing that for loop, used in fun_permut(), is faster than while loop
# ini.time <- as.numeric(Sys.time()) ; count <- 0 ; while(count < 1e9){count <- count + 1} ; tempo.time <- as.numeric(Sys.time()) ; tempo.lapse <- round(lubridate::seconds_to_period(tempo.time - ini.time)) ; tempo.lapse
# fun_permut(data1 = LETTERS[1:5], data2 = NULL, n = 100, seed = 1, print.count = 10, text.print = "CPU NB 4")
# fun_permut(data1 = 101:110, data2 = 21:30, seed = 1, print.count = 1e4, text.print = "", cor.method = "spearman", cor.limit = 0.2)
# a way to use the cor.limit argument just considering data1
# obs1 <- 101:110 ; fun_permut(data1 = obs1, data2 = obs1, seed = 1, print.count = 10, cor.method = "spearman", cor.limit = 0.2)
# fun_permut(data1 = 1:1e3, data2 = 1e3:1, seed = 1, print.count = 1e6, text.print = "", cor.method = "spearman", cor.limit = 0.7)
# fun_permut(data1 = 1:1e2, data2 = 1e2:1, seed = 1, print.count = 1e3, cor.limit = 0.5)
# fun_permut(data1 = c(0,0,0,0,0), n = 5, data2 = NULL, seed = 1, print.count = 1e3, cor.limit = 0.5)
# DEBUGGING
# data1 = LETTERS[1:5] ; data2 = NULL ; n = 1e6 ; seed = NULL ; print.count = 1e3 ; text.print = "" ; cor.method = "spearman" ; cor.limit = 0.2 ; warn.print = TRUE ; lib.path = NULL
# data1 = LETTERS[1:5] ; data2 = NULL ; n = 10 ; seed = 22 ; print.count = 10 ; text.print = "" ; cor.method = "spearman" ; cor.limit = 0.2 ; warn.print = TRUE ; lib.path = NULL
# data1 = 101:110 ; data2 = 21:30 ; n = 10 ; seed = 22 ; print.count = 10 ; text.print = "" ; cor.method = "spearman" ; cor.limit = 0.2 ; warn.print = TRUE ; lib.path = NULL
# data1 = 1:1e3 ; data2 = 1e3:1 ; n = 20 ; seed = 22 ; print.count = 1e6 ; text.print = "" ; cor.method = "spearman" ; cor.limit = 0.5 ; warn.print = TRUE ; lib.path = NULL
# function name
function.name <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
# end function name
# required function checking
if(length(utils::find("fun_check", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_check() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if(length(utils::find("fun_pack", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_pack() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if(length(utils::find("fun_round", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_pack() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end required function checking
# argument checking
arg.check <- NULL #
text.check <- NULL #
checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$fun.name))
tempo <- fun_check(data = data1, class = "vector", fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE & length(data1) < 2){
tempo.cat <- paste0("ERROR IN ", function.name, ": data1 ARGUMENT MUST BE A VECTOR OF MINIMUM LENGTH 2. HERE IT IS: ", length(data1),"\n\n================\n\n")
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
if( ! is.null(data2)){
tempo <- fun_check(data = data1, class = "vector", mode = "numeric", fun.name = function.name) ; eval(ee)
if(tempo$problem == TRUE){
tempo.cat <- paste0("ERROR IN ", function.name, ": data1 MUST BE A NUMERIC VECTOR IF data2 ARGUMENT IS SPECIFIED\n\n================\n\n")
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
tempo <- fun_check(data = data2, class = "vector", mode = "numeric", fun.name = function.name) ; eval(ee)
if(length(data1) != length(data2)){
tempo.cat <- paste0("ERROR IN ", function.name, ": data1 AND data2 MUST BE VECTOR OF SAME LENGTH. HERE IT IS ", length(data1)," AND ", length(data2))
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
}else if(is.null(n)){
tempo.cat <- paste0("ERROR IN ", function.name, ": n ARGUMENT CANNOT BE NULL IF data2 ARGUMENT IS NULL\n\n================\n\n")
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
if( ! is.null(n)){
tempo <- fun_check(data = n, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, neg.values = FALSE, fun.name = function.name) ; eval(ee)
}
if( ! is.null(seed)){
tempo <- fun_check(data = seed, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, neg.values = FALSE, fun.name = function.name) ; eval(ee)
}
tempo <- fun_check(data = print.count, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = text.print, class = "character", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = cor.method, options = c("pearson", "kendall", "spearman"), length =1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = cor.limit, class = "vector", mode = "numeric", prop = TRUE, length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = warn.print, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
if( ! is.null(lib.path)){
tempo <- fun_check(data = lib.path, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE){
if( ! all(dir.exists(lib.path))){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
tempo.cat <- paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", paste(lib.path, collapse = "\n"))
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
}
}
if(any(arg.check) == TRUE){
stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
}
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
# end argument checking
# package checking
fun_pack(req.package = "lubridate", lib.path = lib.path)
# end package checking
# main code
# code that protects set.seed() in the global environment
# see also Protocol 100-rev0 Parallelization in R.docx
if(exists(".Random.seed", envir = .GlobalEnv)){ # if .Random.seed does not exists, it means that no random operation has been performed yet in any R environment
tempo.random.seed <- .Random.seed
on.exit(assign(".Random.seed", tempo.random.seed, env = .GlobalEnv))
}else{
on.exit(set.seed(NULL)) # inactivate seeding -> return to complete randomness
}
# end code that protects set.seed() in the global environment
if( ! is.null(seed)){
set.seed(seed)
}
ini.date <- Sys.time() # time of process begin, converted into seconds
ini.time <- as.numeric(ini.date) # time of process begin, converted into seconds
ini.pos <- 1:length(data1) # positions of data1 before permutation loops
tempo.pos <- ini.pos # positions of data1 that will be modified during loops
# pos.selec.seq <- ini.pos[-length(data1)] # selection of 1 position in initial position, without the last because always up permutation (pos -> pos+1 & pos+1 -> pos)
pos.selec.seq.max <- length(ini.pos) - 1 # max position (used by sample.int() function). See  below for - 1
warn <- NULL
warn.count <- 0
count <- 0
round <- 0
BREAK <- FALSE
tempo.cor <- 0
if(is.null(data2)){
if(length(table(data1)) == 1){
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") FROM FUNCTION ", function.name, ": NO PERMUTATION PERFORMED BECAUSE data1 ARGUMENT SEEMS TO BE MADE OF IDENTICAL ELEMENTS: ", names(table(data1)))
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn))) #
}else{
if(print.count > n){
print.count <- n
}
cat(paste0("\n", ifelse(text.print == "", "", paste0(text.print, " | ")), "FOR LOOP OF ", n, " LOOPS INITIATED | LOOP COUNT: ", format(count, big.mark=",")))
print.count.loop <- logical(length = print.count)
print.count.loop[length(print.count.loop)] <- TRUE # not this to avoid long vector, but not forget to reset during printing: print.count.loop[(1:trunc(n / print.count) * print.count)] <- TRUE # counter to speedup
count.loop <- 0
pos <- sample.int(n = pos.selec.seq.max , size = print.count, replace = TRUE) # selection of random positions. BEWARE: n = pos.selec.seq.max because already - 1 (see above) but is connected to tempo.pos[c(pos2 + 1, pos2)] <- tempo.pos[c(pos2, pos2 + 1)]
tempo.date.loop <- Sys.time()
tempo.time.loop <- as.numeric(tempo.date.loop)
for(i3 in 1:n){
count.loop <- count.loop + 1
pos2 <- pos[count.loop] # selection of 1 position
tempo.pos[c(pos2 + 1, pos2)] <- tempo.pos[c(pos2, pos2 + 1)]
if(print.count.loop[count.loop]){
count.loop <- 0
pos <- sample.int(n = pos.selec.seq.max , size = print.count, replace = TRUE) # BEWARE: never forget to resample here
tempo.time <- as.numeric(Sys.time())
tempo.lapse <- round(lubridate::seconds_to_period(tempo.time - tempo.time.loop))
final.loop <- (tempo.time - tempo.time.loop) / i3 * n
final.exp <- as.POSIXct(final.loop, origin = tempo.date.loop)
cat(paste0("\n", ifelse(text.print == "", "", paste0(text.print, " | ")), "FOR LOOP ", i3, " / ", n, " | TIME SPENT: ", tempo.lapse, " | EXPECTED END: ", final.exp))
}
}
count <- count + n # out of the loop to speedup
cat(paste0("\n", ifelse(text.print == "", "", paste0(text.print, " | ")), "FOR LOOP ENDED | LOOP COUNT: ", format(count, big.mark=",")))
cat("\n\n")
}
}else{
if(length(table(data1)) == 1){
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") FROM FUNCTION ", function.name, ": NO PERMUTATION PERFORMED BECAUSE data1 ARGUMENT SEEMS TO BE MADE OF IDENTICAL ELEMENTS: ", names(table(data1)))
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn))) #
tempo.cor <- 1
}else if(length(table(data2)) == 1){
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") FROM FUNCTION ", function.name, ": NO PERMUTATION PERFORMED BECAUSE data2 ARGUMENT SEEMS TO BE MADE OF IDENTICAL ELEMENTS: ", names(table(data2)))
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn))) #
tempo.cor <- 1
}else{
cor.ini <- cor(x = data1, y = data2, use = "pairwise.complete.obs", method = cor.method)
tempo.cor <- cor.ini # correlation that will be modified during loops
neg.cor <- FALSE
if(tempo.cor < 0){
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") FROM FUNCTION ", function.name, ": INITIAL ", toupper(cor.method), " CORRELATION BETWEEN data1 AND data2 HAS BEEN DETECTED AS NEGATIVE: ", tempo.cor, ". THE LOOP STEPS WILL BE PERFORMED USING POSITIVE CORRELATIONS BUT THE FINAL CORRELATION WILL BE NEGATIVE")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn))) #
neg.cor <- TRUE
tempo.cor <- abs(tempo.cor)
cor.ini <- abs(cor.ini)
}
if(tempo.cor < cor.limit){ # randomize directly all the position to be close to correlation zero
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") FROM FUNCTION ", function.name, ": INITIAL ABSOLUTE VALUE OF THE ", toupper(cor.method), " CORRELATION ", fun_round(tempo.cor), " BETWEEN data1 AND data2 HAS BEEN DETECTED AS BELOW THE CORRELATION LIMIT PARAMETER ", cor.limit, "\nTHE data1 SEQUENCE HAS BEEN COMPLETELY RANDOMIZED TO CORRESPOND TO CORRELATION ZERO")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn))) #
for(i4 in 1:5){ # done 5 times to be sure of the complete randomness
tempo.pos <- sample(x = tempo.pos, size = length(tempo.pos), replace = FALSE)
}
count <- count + 5 # out of the loop to speedup
}else{
# smallest correlation decrease
count <- count + 1 # 1 and not 0 because already 1 performed just below
pos <- sample.int(n = pos.selec.seq.max , size = 1, replace = TRUE) # selection of 1 position # pos.selec.seq.max  because selection of 1 position in initial position, without the last because always up permutation (pos -> pos+1 & pos+1 -> pos)
tempo.pos[c(pos + 1, pos)] <- tempo.pos[c(pos, pos + 1)]
tempo.cor <- abs(cor(x = data1[tempo.pos], y = data2, use = "pairwise.complete.obs", method = cor.method))
smallest.cor.dec <- cor.ini - tempo.cor
# end smallest correlation decrease
# going out of tempo.cor == cor.ini
cat(paste0("\n", ifelse(text.print == "", "", paste0(text.print, " | ")), "CORRELATION DECREASE AFTER A SINGLE PERMUTATION: ", fun_round(smallest.cor.dec, 4)))
cat(paste0("\n", ifelse(text.print == "", "", paste0(text.print, " | ")), "FIRST WHILE LOOP STEP -> GOING OUT FROM EQUALITY | LOOP COUNT: ", format(count, big.mark=","), " | CORRELATION LIMIT: ", fun_round(cor.limit, 4), " | ABS TEMPO CORRELATION: ", fun_round(tempo.cor, 4)))
print.count.loop <- logical(length = print.count)
print.count.loop[length(print.count.loop)] <- TRUE # counter to speedup
count.loop <- 0 # 
pos <- sample.int(n = pos.selec.seq.max , size = print.count, replace = TRUE) # selection of random positions. BEWARE: n = pos.selec.seq.max because already - 1 (see above) but is connected to tempo.pos[c(pos2 + 1, pos2)] <- tempo.pos[c(pos2, pos2 + 1)]
tempo.date.loop <- Sys.time()
tempo.time.loop <- as.numeric(tempo.date.loop)
while(tempo.cor == cor.ini){ # to be out of equality between tempo.cor and cor.ini at the beginning (only valid for very long vector)
count <- count + 1
count.loop <- count.loop + 1
pos2 <- pos[count.loop]
tempo.pos[c(pos2 + 1, pos2)] <- tempo.pos[c(pos2, pos2 + 1)]
tempo.cor <- abs(cor(x = data1[tempo.pos], y = data2, use = "pairwise.complete.obs", method = cor.method))
if(print.count.loop[count.loop]){
count.loop <- 0
pos <- sample.int(n = pos.selec.seq.max , size = print.count, replace = TRUE) # BEWARE: never forget to resample here
tempo.time <- as.numeric(Sys.time())
tempo.lapse <- round(lubridate::seconds_to_period(tempo.time - tempo.time.loop))
cat(paste0("\n", ifelse(text.print == "", "", paste0(text.print, " | ")), "FIRST WHILE LOOP STEP", format(count.loop, big.mark=","), " / ? | COUNT: ", format(count, big.mark=","), " | CORRELATION LIMIT: ", fun_round(cor.limit, 4), " | ABS TEMPO CORRELATION: ", fun_round(tempo.cor, 4), " | TIME SPENT: ", tempo.lapse))
}
}
tempo.time <- as.numeric(Sys.time())
tempo.lapse <- round(lubridate::seconds_to_period(tempo.time - ini.time))
cat(paste0("\n", ifelse(text.print == "", "", paste0(text.print, " | ")), "FIRST WHILE LOOP STEP END | LOOP COUNT: ", format(count, big.mark=","), " | CORRELATION LIMIT: ", fun_round(cor.limit, 4), " | ABS TEMPO CORRELATION: ", fun_round(tempo.cor, 4), " | TOTAL SPENT TIME: ", tempo.lapse))
if(tempo.cor < cor.limit){
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") FROM FUNCTION ", function.name, ": THE FIRST FOR & WHILE LOOP STEPS HAVE BEEN TOO FAR AND SUBSEQUENT LOOP STEPS WILL NOT RUN")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}
# end going out of tempo.cor == cor.ini
# estimation of the average correlation decrease per loop on x loops and for loop execution
cat(paste0("\n", ifelse(text.print == "", "", paste0(text.print, " | ")), "WHILE/FOR LOOPS INITIATION | LOOP COUNT: ", format(count, big.mark=","), " | CORRELATION LIMIT: ", fun_round(cor.limit, 4), " | ABS TEMPO CORRELATION: ", fun_round(tempo.cor, 4)))
count.est <- 1e5
first.round <- TRUE
GOBACK <- FALSE
while(tempo.cor > cor.limit){
round <- round + 1
# estimation step
if(first.round == TRUE){
first.round <- FALSE
cor.dec.per.loop <- numeric(length = 5)
loop.nb.est <- Inf
cor.est.ini <- tempo.cor
cor.est <- numeric(length = 5)
for(i6 in 1:5){ # connected to cor.dec.per.loop
tempo.pos.est <- tempo.pos
pos <- sample.int(n = pos.selec.seq.max , size = count.est, replace = TRUE) # selection of n position
for(i7 in 1:count.est){
pos2 <- pos[i7] # selection of 1 position
tempo.pos.est[c(pos2 + 1, pos2)] <- tempo.pos.est[c(pos2, pos2 + 1)]
}
tempo.cor.est <- abs(cor(x = data1[tempo.pos.est], y = data2, use = "pairwise.complete.obs", method = cor.method))
cor.est[i6] <- tempo.cor.est
tempo.cor.dec.per.loop <- (cor.est.ini - tempo.cor.est) / count.est # correlation decrease per loop
if(is.na(tempo.cor.dec.per.loop) | ! is.finite(tempo.cor.dec.per.loop)){
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": CODE INCONSISTENCY 2\ncor.est.ini: ", cor.est.ini, "\ntempo.cor.est: ", tempo.cor.est, "\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
cor.dec.per.loop[i6] <- tempo.cor.dec.per.loop
}
cor.est <- cor.est[which.max(cor.dec.per.loop)] # max to avoid to go to far with for loop (tempo.cor below tempo.limit)
cor.dec.per.loop <- max(cor.dec.per.loop, na.rm = TRUE) # max to avoid to go to far with for loop (tempo.cor below tempo.limit)
loop.nb.est <- round((tempo.cor - cor.limit) / cor.dec.per.loop)
}else{
if(GOBACK == TRUE){
loop.nb.est <- round(loop.nb.est / 2)
}else{
cor.dec.per.loop <- (cor.ini - tempo.cor) / count
loop.nb.est <- round((tempo.cor - cor.limit) / cor.dec.per.loop)
}
}
# end estimation step
# loop step
if(is.na(loop.nb.est) | ! is.finite(loop.nb.est)){
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": CODE INCONSISTENCY 1\nloop.nb.est: ", loop.nb.est, "\ncor.ini: ", cor.ini, "\ntempo.cor: ", tempo.cor, "\ncor.limit: ", cor.limit, "\ncor.dec.per.loop: ", cor.dec.per.loop, "\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}else if(loop.nb.est > 1e4){ # below -> leave the while loop
tempo.pos.secu <- tempo.pos
count.secu <- count
tempo.cor.secu <- tempo.cor
cat(paste0("\n", ifelse(text.print == "", "", paste0(text.print, " | ")), "INITIAL SETTINGS BEFORE ROUND: ", round, " | LOOP COUNT: ", format(count, big.mark=","), " | GO BACK: ", GOBACK, " | LOOP NUMBER ESTIMATION: ", format(loop.nb.est, big.mark=","), " | CORRELATION LIMIT: ", fun_round(cor.limit, 4), " | ABS TEMPO CORRELATION: ", fun_round(tempo.cor, 4)))
print.count.loop <- logical(length = print.count)
print.count.loop[length(print.count.loop)] <- TRUE # not this to avoid long vector, but not forget to reset during printing: print.count.loop[(1:trunc(n / print.count) * print.count)] <- TRUE # counter to speedup
count.loop <- 0
pos <- sample.int(n = pos.selec.seq.max , size = print.count, replace = TRUE) # selection of random positions. BEWARE: n = pos.selec.seq.max because already - 1 (see above) but is connected to tempo.pos[c(pos2 + 1, pos2)] <- tempo.pos[c(pos2, pos2 + 1)]
tempo.date.loop <- Sys.time()
tempo.time.loop <- as.numeric(tempo.date.loop)
for(i6 in 1:loop.nb.est){
count.loop <- count.loop + 1
pos2 <- pos[count.loop] # selection of 1 position
tempo.pos[c(pos2 + 1, pos2)] <- tempo.pos[c(pos2, pos2 + 1)]
if(print.count.loop[count.loop]){
count.loop <- 0
pos <- sample.int(n = pos.selec.seq.max , size = print.count, replace = TRUE) # BEWARE: never forget to resample here
tempo.time <- as.numeric(Sys.time())
tempo.lapse <- round(lubridate::seconds_to_period(tempo.time - tempo.time.loop))
final.loop <- (tempo.time - tempo.time.loop) / i6 * loop.nb.est # intra nb.compar loop lapse: time lapse / cycles done * cycles remaining
final.exp <- as.POSIXct(final.loop, origin = tempo.date.loop)
cat(paste0("\n", ifelse(text.print == "", "", paste0(text.print, " | ")), "FOR LOOP | ROUND ", round, " | LOOP: ", format(i6, big.mark=","), " / ", format(loop.nb.est, big.mark=","), " | TIME SPENT: ", tempo.lapse, " | EXPECTED END: ", final.exp))
}
}
count <- count + loop.nb.est # out of the loop to speedup
tempo.cor <- abs(cor(x = data1[tempo.pos], y = data2, use = "pairwise.complete.obs", method = cor.method))
if(tempo.cor > tempo.cor.secu | ((tempo.cor - cor.limit) < 0 & abs(tempo.cor - cor.limit) > smallest.cor.dec * round(log10(max(ini.pos, na.rm = TRUE))))){
GOBACK <- TRUE
tempo.pos <- tempo.pos.secu
count <- count.secu
tempo.cor <- tempo.cor.secu
}else{
GOBACK <- FALSE
}
}else{
cat(paste0("\n", ifelse(text.print == "", "", paste0(text.print, " | ")), "FINAL WHILE LOOP | LOOP COUNT: ", format(count, big.mark=","), " | CORRELATION LIMIT: ", fun_round(cor.limit, 4), " | ABS TEMPO CORRELATION: ", fun_round(tempo.cor, 4)))
print.count.loop <- logical(length = print.count)
print.count.loop[length(print.count.loop)] <- TRUE # counter to speedup
count.loop <- 0 # 
pos <- sample.int(n = pos.selec.seq.max , size = print.count, replace = TRUE) # selection of random positions. BEWARE: n = pos.selec.seq.max because already - 1 (see above) but is connected to tempo.pos[c(pos2 + 1, pos2)] <- tempo.pos[c(pos2, pos2 + 1)]
tempo.cor.loop <- tempo.cor
tempo.date.loop <- Sys.time()
tempo.time.loop <- as.numeric(tempo.date.loop)
while(tempo.cor > cor.limit){
count <- count + 1
count.loop <- count.loop + 1
pos2 <- pos[count.loop]
tempo.pos[c(pos2 + 1, pos2)] <- tempo.pos[c(pos2, pos2 + 1)]
tempo.cor <- abs(cor(x = data1[tempo.pos], y = data2, use = "pairwise.complete.obs", method = cor.method))
if(print.count.loop[count.loop]){
count.loop <- 0
pos <- sample.int(n = pos.selec.seq.max , size = print.count, replace = TRUE) # BEWARE: never forget to resample here
tempo.time <- as.numeric(Sys.time())
tempo.lapse <- round(lubridate::seconds_to_period(tempo.time - tempo.time.loop))
final.loop <- (tempo.time - tempo.time.loop) / (tempo.cor.loop - tempo.cor) * (tempo.cor - cor.limit) # tempo.cor.loop - tempo.cor always positive and tempo.cor decreases progressively starting from tempo.cor.loop
final.exp <- as.POSIXct(final.loop, origin = tempo.date.loop)
cat(paste0("\n", ifelse(text.print == "", "", paste0(text.print, " | ")), "WHILE LOOP | LOOP NB: ", format(count.loop, big.mark=","), " | COUNT: ", format(count, big.mark=","), " | CORRELATION LIMIT: ", fun_round(cor.limit, 4), " | ABS TEMPO CORRELATION: ", fun_round(tempo.cor, 4), " | TIME SPENT: ", tempo.lapse, " | EXPECTED END: ", final.exp))
}
}
}
}
tempo.time <- as.numeric(Sys.time())
tempo.lapse <- round(lubridate::seconds_to_period(tempo.time - ini.time))
cat(paste0("\n", ifelse(text.print == "", "", paste0(text.print, " | ")), "WHILE/FOR LOOPS END | LOOP COUNT: ", format(count, big.mark=","), " | NB OF ROUNDS: ", round, " | CORRELATION LIMIT: ", fun_round(cor.limit, 4), " | ABS TEMPO CORRELATION: ", fun_round(tempo.cor, 4), " | TOTAL SPENT TIME: ", tempo.lapse))
}
tempo.cor <- ifelse(neg.cor == TRUE, -tempo.cor, tempo.cor)
}
}
cat("\n\n")
if(warn.print == TRUE & ! is.null(warn)){
warning(warn, call. = FALSE)
cat("\n\n")
}
output <- list(data = data1[tempo.pos], warn = warn, cor = if(is.null(data2)){cor(ini.pos, tempo.pos, method = "spearman")}else{tempo.cor}, count = count)
return(output)
}


################ Graphics management


# this order can be used:
# fun_width()
# fun_open()
# fun_prior_plot() # not for ggplot2
# plot() or any other plotting
# fun_post_plot() if fun_prior_plot() has been used # not for ggplot2
# fun_close()


######## fun_width() #### window width depending on classes to plot


# Check OK: clear to go Apollo
fun_width <- function(class.nb, inches.per.class.nb = 1, ini.window.width = 7, inch.left.space, inch.right.space, boundarie.space = 0.5){
# AIM
# rescale the width of a window to open depending on the number of classes to plot
# can be used for height, considering that it is as if it was a width
# this order can be used:
# fun_width()
# fun_open()
# fun_prior_plot() # not for ggplot2
# plot() or any other plotting
# fun_post_plot() if fun_prior_plot() has been used # not for ggplot2
# fun_close()
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# fun_check()
# ARGUMENTS
# class.nb: number of class to plot
# inches.per.class.nb: number of inches per unit of class.nb. 2 means 2 inches for each boxplot for instance
# ini.window.width:initial window width in inches
# inch.left.space: left horizontal margin of the figure region (in inches)
# inch.right.space: right horizontal margin of the figure region (in inches)
# boundarie.space: space between the right and left limits of the plotting region and the plot (0.5 means half a class width)
# RETURN
# the new window width in inches
# EXAMPLES
# fun_width(class.nb = 10, inches.per.class.nb = 0.2, ini.window.width = 7, inch.left.space = 1, inch.right.space = 1, boundarie.space = 0.5)
# DEBUGGING
# class.nb = 10 ; inches.per.class.nb = 0.2 ; ini.window.width = 7 ; inch.left.space = 1 ; inch.right.space = 1 ; boundarie.space = 0.5 # for function debugging
# function name
function.name <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
# end function name
# required function checking
if(length(utils::find("fun_check", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_check() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end required function checking
# argument checking
arg.check <- NULL #
text.check <- NULL #
checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$fun.name))
tempo <- fun_check(data = class.nb, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = inches.per.class.nb, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = ini.window.width, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = inch.left.space, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = inch.right.space, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = boundarie.space, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
if(any(arg.check) == TRUE){
stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
}
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
# end argument checking
# main code
range.max <- class.nb + boundarie.space # the max range of the future plot
range.min <- boundarie.space # the min range of the future plot
window.width <- inch.left.space + inch.right.space + inches.per.class.nb * (range.max - range.min)
return(window.width)
}


######## fun_open() #### open a GUI or pdf graphic window


# Check OK: clear to go Apollo
fun_open <- function(pdf.disp = TRUE, fun.path = "working.dir", pdf.name.file = "graph", width.fun = 7, height.fun = 7, paper = "special", no.pdf.overwrite = TRUE, remove.read.only = TRUE, return.output = FALSE){
# AIM
# open a pdf or screen (GUI) graphic window and return initial graphic parameters
# this order can be used:
# fun_width()
# fun_open()
# fun_prior_plot() # not for ggplot2
# plot() or any other plotting
# fun_post_plot() if fun_prior_plot() has been used # not for ggplot2
# fun_close()
#WARNING
# On Linux, use pdf.disp = TRUE, if (GUI) graphic window is not always available, meaning that X is not installed (clusters for instance). Use X11() in R to test if available
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# fun_check()
# ARGUMENTS:
# pdf.disp: logical. Use pdf display?
# fun.path: where the pdf is saved (do not terminate by / or \\). Write "working.dir" if working directory is required (default). Ignored if pdf.disp == FALSE
# pdf.name.file: name of the pdf file containing the graphs (the .pdf extension is added by the function). Ignored if pdf.disp == FALSE
# width.fun: width of the window (in inches)
# height.fun: height of the window (in inches)
# paper: paper argument of the pdf function (paper format). Only used for pdf(). Either "a4", "letter", "legal", "us", "executive", "a4r", "USr" or "special". If "special", means that the paper dimension will be width.fun and height.fun. With another paper format, if width.fun or height.fun is over the size of the paper, width.fun or height.fun will be modified such that the plot is adjusted to the paper dimension (see $dim in the returned list below to see the modified dimensions). Ignored if pdf.disp == FALSE
# no.pdf.overwrite: logical. Existing pdf can be overwritten? . Ignored if pdf.disp == FALSE
# remove.read.only: logical. remove the read only (R.O.) graphical parameters? If TRUE, the graphical parameters are  returned without the R.O. parameters. The returned $ini.par list can be used to set the par() of a new graphical device. If FALSE, graphical parameters are returned with the R.O. parameters, which provides information like text dimension (see ?par() ). The returned $ini.par list can be used to set the par() of a new graphical device, but generate a warning message. Ignored if return.output == FALSE. 
# return.output: logical. Return output ? If TRUE but function not assigned, the output list is displayed
# RETURN
# a list containing:
# $pdf.loc: path of the pdf created
# $ini.par: initial par() parameters
# $zone.ini: initial window spliting
# $dim: dimension of the graphical device (in inches)
# EXAMPLES
# fun_open(pdf.disp = FALSE, fun.path = "C:/Users/Gael/Desktop", pdf.name.file = "graph", width.fun = 7, height.fun = 7, paper = "special", no.pdf.overwrite = TRUE, return.output = TRUE)
# DEBUGGING
# pdf.disp = TRUE ; fun.path = "C:/Users/Gael/Desktop" ; pdf.name.file = "graphs" ; width.fun = 7 ; height.fun = 7 ; paper = "special" ; no.pdf.overwrite = TRUE ; remove.read.only = TRUE ; return.output = TRUE # for function debugging
# function name
function.name <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
# end function name
# required function checking
if(length(utils::find("fun_check", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_check() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end required function checking
# argument checking
arg.check <- NULL #
text.check <- NULL #
checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$fun.name))
tempo <- fun_check(data = pdf.disp, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
if( ! is.null(fun.path)){
tempo <- fun_check(data = fun.path, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
}
tempo <- fun_check(data = fun.path, class = "character", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = pdf.name.file, class = "character", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = width.fun, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = height.fun, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = paper, options = c("a4", "letter", "legal", "us", "executive", "a4r", "USr", "special", "A4", "LETTER", "LEGAL", "US"), length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data =no.pdf.overwrite, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = remove.read.only, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = return.output, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
if(any(arg.check) == TRUE){
stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
}
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
# end argument checking
# main code
if(fun.path == "working.dir"){
fun.path <- getwd()
}else{
if(grepl(x = fun.path, pattern = ".+/$")){
fun.path <- substr(fun.path, 1, nchar(fun.path) - 1) # remove the last /
}
if(dir.exists(fun.path) == FALSE){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": fun.path ARGUMENT DOES NOT CORRESPOND TO EXISTING DIRECTORY\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
}
# par.ini recovery
# cannot use pdf(file = NULL), because some small differences between pdf() and other devices. For instance, differences with windows() for par()$fin, par()$pin and par()$plt
if(Sys.info()["sysname"] == "Windows"){ # Note that .Platform$OS.type() only says "unix" for macOS and Linux and "Windows" for Windows
open.fail <- NULL
windows()
ini.par <- par(no.readonly = remove.read.only) # to recover the initial graphical parameters if required (reset). BEWARE: this command alone opens a pdf of GUI window if no window already opened. But here, protected with the code because always a tempo window opened
invisible(dev.off()) # close the new window
}else if(Sys.info()["sysname"] == "Linux"){
if(pdf.disp == TRUE){
tempo.code <- 0
while(file.exists(paste0(fun.path, "/recover_ini_par", tempo.code, ".pdf")) == TRUE){
tempo.code <- tempo.code + 1
}
pdf(width = width.fun, height = height.fun, file=paste0(fun.path, "/recover_ini_par", tempo.code, ".pdf"), paper = paper)
ini.par <- par(no.readonly = remove.read.only) # to recover the initial graphical parameters if required (reset). BEWARE: this command alone opens a pdf of GUI window if no window already opened. But here, protected with the code because always a tempo window opened
invisible(dev.off()) # close the pdf window
file.remove(paste0(fun.path, "/recover_ini_par", tempo.code, ".pdf")) # remove the pdf file
}else{
# test if X11 can be opened
if(file.exists(paste0(getwd(), "/Rplots.pdf"))){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": THIS FUNCTION CANNOT BE USED ON LINUX IF A Rplots.pdf FILE ALREADY EXISTS HERE: ", getwd(), "\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}else{
open.fail <- suppressWarnings(try(X11(), silent = TRUE))[] # try to open a X11 window. If open.fail == NULL, no problem, meaning that the X11 window is opened. If open.fail != NULL, a pdf can be opened here paste0(getwd(), "/Rplots.pdf")
if(is.null(open.fail)){
ini.par <- par(no.readonly = remove.read.only) # to recover the initial graphical parameters if required (reset). BEWARE: this command alone opens a pdf of GUI window if no window already opened. But here, protected with the code because always a tempo window opened
invisible(dev.off()) # close the new window
}else if(file.exists(paste0(getwd(), "/Rplots.pdf"))){
file.remove(paste0(getwd(), "/Rplots.pdf")) # remove the pdf file
tempo.cat <- ("\n\n================\n\nPROBLEM IN fun_open(): THIS FUNCTION CANNOT OPEN GUI ON LINUX OR NON MACOS UNIX SYSTEM (X GRAPHIC INTERFACE HAS TO BE SET).\nTO OVERCOME THIS, PLEASE SET pdf.disp ARGUMENT TO TRUE AND RERUN\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
}
}
}else{
open.fail <- NULL
quartz()
ini.par <- par(no.readonly = remove.read.only) # to recover the initial graphical parameters if required (reset). BEWARE: this command alone opens a pdf of GUI window if no window already opened. But here, protected with the code because always a tempo window opened
invisible(dev.off()) # close the new window
}
# end par.ini recovery 
zone.ini <- matrix(1, ncol=1) # to recover the initial parameters for next figure region when device region split into several figure regions
if(pdf.disp == TRUE){
pdf.loc <- paste0(fun.path, "/", pdf.name.file, ".pdf")
if(file.exists(pdf.loc) == TRUE & no.pdf.overwrite == TRUE){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": pdf.loc FILE ALREADY EXISTS AND CANNOT BE OVERWRITTEN DUE TO no.pdf.overwrite ARGUMENT SET TO TRUE: ", pdf.loc, "\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}else{
pdf(width = width.fun, height = height.fun, file=pdf.loc, paper = paper)
}
}else if(pdf.disp == FALSE){
pdf.loc <- NULL
if(Sys.info()["sysname"] == "Windows"){ # .Platform$OS.type() only says "unix" for macOS and Linux and "Windows" for Windows
windows(width = width.fun, height = height.fun, rescale="fixed")
}else if(Sys.info()["sysname"] == "Linux"){
if( ! is.null(open.fail)){
tempo.cat <- "\n\n================\n\nPROBLEM IN fun_open(): THIS FUNCTION CANNOT OPEN GUI ON LINUX OR NON MACOS UNIX SYSTEM (X GRAPHIC INTERFACE HAS TO BE SET).\nTO OVERCOME THIS, PLEASE SET pdf.disp ARGUMENT TO TRUE AND RERUN\n\n================\n\n"
stop(tempo.cat, call. = FALSE)
}else{
X11(width = width.fun, height = height.fun)
}
}else{
quartz(width = width.fun, height = height.fun)
}
}
if(return.output == TRUE){
output <- list(pdf.loc = pdf.loc, ini.par = ini.par, zone.ini = zone.ini, dim = dev.size())
return(output)
}
}


######## fun_prior_plot() #### set graph param before plotting (erase axes for instance)


# Check OK: clear to go Apollo
fun_prior_plot <- function(param.reinitial = FALSE, xlog.scale = FALSE, ylog.scale = FALSE, remove.label = TRUE, remove.x.axis = TRUE, remove.y.axis = TRUE, std.x.range = TRUE, std.y.range = TRUE, down.space = 1, left.space = 1, up.space = 1, right.space = 1, orient = 1, dist.legend = 3.5, tick.length = 0.5, box.type = "n", amplif.label = 1, amplif.axis = 1, display.extend = FALSE, return.par = FALSE){
# AIM
# very convenient to erase the axes for post plot axis redrawing using fun_post_plot()
# reinitialize and set the graphic parameters before plotting
# CANNOT be used if no graphic device already opened
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# fun_check()
# ARGUMENTS
# param.reinitial: reinitialize graphic parameters before applying the new ones, as defined by the other arguments? Either TRUE or FALSE
# xlog.scale: Log scale for the x-axis? Either TRUE or FALSE. If TRUE, erases the x-axis, except legend, for further drawing by fun_post_plot()(xlog argument of par())
# ylog.scale: Log scale for the y-axis? Either TRUE or FALSE. If TRUE, erases the y-axis, except legend, for further drawing by fun_post_plot()(ylog argument of par())
# remove.label: remove labels (axis legend) of the two axes? Either TRUE or FALSE (ann argument of par())
# remove.x.axis: remove x-axis except legend? Either TRUE or FALSE (control the xaxt argument of par()). Automately set to TRUE if xlog.scale == TRUE
# remove.y.axis: remove y-axis except legend? Either TRUE or FALSE (control the yaxt argument of par()). Automately set to TRUE if ylog.scale == TRUE
# std.x.range: standard range on the x-axis? TRUE (no range extend) or FALSE (4% range extend). Controls xaxs argument of par() (TRUE is xaxs = "i", FALSE is xaxs = "r")
# std.y.range: standard range on the y-axis? TRUE (no range extend) or FALSE (4% range extend). Controls yaxs argument of par() (TRUE is yaxs = "i", FALSE is yaxs = "r")
# down.space: lower vertical margin (in inches, mai argument of par())
# left.space: left horizontal margin (in inches, mai argument of par())
# up.space: upper vertical margin between plot region and grapical window (in inches, mai argument of par())
# right.space: right horizontal margin (in inches, mai argument of par())
# orient: scale number orientation (las argument of par()). 0, always parallel to the axis; 1, always horizontal; 2, always perpendicular to the axis; 3, always vertical
# dist.legend: numeric value that moves axis legends away in inches (first number of mgp argument of par() but in inches thus / 0.2)
# tick.length: length of the ticks (1 means complete the distance between the plot region and the axis numbers, 0.5 means half the length, etc. 0 means no tick
# box.type: bty argument of par(). Either "o", "l", "7", "c", "u", "]", the resulting box resembles the corresponding upper case letter. A value of "n" suppresses the box
# amplif.label: increase or decrease the size of the text in legends
# amplif.axis: increase or decrease the size of the scale numbers in axis
# display.extend: extend display beyond plotting region? Either TRUE or FALSE (xpd argument of par() without NA)
# return.par: return graphic parameter modification?
# RETURN
# return graphic parameter modification
# EXAMPLES
# fun_prior_plot(param.reinitial = FALSE, xlog.scale = FALSE, ylog.scale = FALSE, remove.label = TRUE, remove.x.axis = TRUE, remove.y.axis = TRUE, std.x.range = TRUE, std.y.range = TRUE, down.space = 1, left.space = 1, up.space = 1, right.space = 1, orient = 1, dist.legend = 4.5, tick.length = 0.5, box.type = "n", amplif.label = 1, amplif.axis = 1, display.extend = FALSE, return.par = FALSE)
# DEBUGGING
# param.reinitial = FALSE ; xlog.scale = FALSE ; ylog.scale = FALSE ; remove.label = TRUE ; remove.x.axis = TRUE ; remove.y.axis = TRUE ; std.x.range = TRUE ; std.y.range = TRUE ; down.space = 1 ; left.space = 1 ; up.space = 1 ; right.space = 1 ; orient = 1 ; dist.legend = 4.5 ; tick.length = 0.5 ; box.type = "n" ; amplif.label = 1 ; amplif.axis = 1 ; display.extend = FALSE ; return.par = FALSE # for function debugging
# function name
function.name <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
# end function name
# required function checking
if(length(utils::find("fun_check", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_check() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end required function checking
# argument checking
arg.check <- NULL #
text.check <- NULL #
checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$fun.name))
tempo <- fun_check(data = param.reinitial, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = xlog.scale, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = ylog.scale, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = remove.label, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = remove.x.axis, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = remove.y.axis, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = std.x.range, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = std.y.range, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = down.space, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = left.space, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = up.space, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = right.space, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = orient, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = dist.legend, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = tick.length, class = "vector", mode = "numeric", length = 1, prop = TRUE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = box.type, options = c("o", "l", "7", "c", "u", "]", "n"), length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = amplif.label, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = amplif.axis, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = display.extend, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = return.par, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
if(any(arg.check) == TRUE){
stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
}
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
# end argument checking
# main code
if(is.null(dev.list())){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": THIS FUNCTION CANNOT BE USED IF NO GRAPHIC DEVICE ALREADY OPENED (dev.list() IS CURRENTLY NULL)\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# par.ini recovery
# cannot use pdf(file = NULL), because some small differences between pdf() and other devices. For instance, differences with windows() for par()$fin, par()$pin and par()$plt
if(param.reinitial == TRUE){
if( ! all(names(dev.cur()) == "null device")){
active.wind.nb <- dev.cur()
}else{
active.wind.nb <- 0
}
if(Sys.info()["sysname"] == "Windows"){ # Note that .Platform$OS.type() only says "unix" for macOS and Linux and "Windows" for Windows
windows()
ini.par <- par(no.readonly = FALSE) # to recover the initial graphical parameters if required (reset). BEWARE: this command alone opens a pdf of GUI window if no window already opened. But here, protected with the code because always a tempo window opened
invisible(dev.off()) # close the new window
}else if(Sys.info()["sysname"] == "Linux"){
if(file.exists(paste0(getwd(), "/Rplots.pdf"))){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": THIS FUNCTION CANNOT BE USED ON LINUX WITH param.reinitial SET TO TRUE IF A Rplots.pdf FILE ALREADY EXISTS HERE: ", getwd(), "\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}else{
open.fail <- suppressWarnings(try(X11(), silent = TRUE))[] # try to open a X11 window. If open.fail == NULL, no problem, meaning that the X11 window is opened. If open.fail != NULL, a pdf can be opened here paste0(getwd(), "/Rplots.pdf")
if(is.null(open.fail)){
ini.par <- par(no.readonly = FALSE) # to recover the initial graphical parameters if required (reset). BEWARE: this command alone opens a pdf of GUI window if no window already opened. But here, protected with the code because always a tempo window opened
invisible(dev.off()) # close the new window
}else if(file.exists(paste0(getwd(), "/Rplots.pdf"))){
ini.par <- par(no.readonly = FALSE) # to recover the initial graphical parameters if required (reset). BEWARE: this command alone opens a pdf of GUI window if no window already opened. But here, protected with the code because always a tempo window opened
invisible(dev.off()) # close the new window
file.remove(paste0(getwd(), "/Rplots.pdf")) # remove the pdf file
}else{
tempo.cat <- ("\n\n================\n\nPROBLEM IN fun_prior_plot(): THIS FUNCTION CANNOT OPEN GUI ON LINUX OR NON MACOS UNIX SYSTEM (X GRAPHIC INTERFACE HAS TO BE SET).\nTO OVERCOME THIS, PLEASE USE PDF GRAPHIC INTERFACES AND RERUN\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
}
}else{ # macOS
quartz()
ini.par <- par(no.readonly = FALSE) # to recover the initial graphical parameters if required (reset). BEWARE: this command alone opens a pdf of GUI window if no window already opened. But here, protected with the code because always a tempo window opened)
invisible(dev.off()) # close the new window
}
if( ! all(names(dev.cur()) == "null device")){
dev.set(active.wind.nb) # go back to the active window if exists
par(ini.par) # apply the initial par to current window
}
}
# end par.ini recovery
if(remove.x.axis == TRUE){
par(xaxt = "n") # suppress the y-axis label
}else{
par(xaxt = "s")
}
if(remove.y.axis == TRUE){
par(yaxt = "n") # suppress the y-axis label
}else{
par(yaxt = "s")
}
if(std.x.range == TRUE){
par(xaxs = "i")
}else{
par(xaxs = "r")
}
if(std.y.range == TRUE){
par(yaxs = "i")
}else{
par(yaxs = "r")
}
par(mai = c(down.space, left.space, up.space, right.space), ann = ! remove.label, las = orient, mgp = c(dist.legend/0.2, 1, 0), xpd = display.extend, bty= box.type, cex.lab = amplif.label, cex.axis = amplif.axis)
par(tcl = -par()$mgp[2] * tick.length) # tcl gives the length of the ticks as proportion of line text, knowing that mgp is in text lines. So the main ticks are a 0.5 of the distance of the axis numbers by default. The sign provides the side of the tick (negative for outside of the plot region)
if(xlog.scale == TRUE){
par(xaxt = "n", xlog = TRUE) # suppress the x-axis label
}else{
par(xlog = FALSE)
}
if(ylog.scale == TRUE){
par(yaxt = "n", ylog = TRUE) # suppress the y-axis label
}else{
par(ylog = FALSE)
}
if(return.par == TRUE){
tempo.par <- par()
return(tempo.par)
}
}


######## fun_scale() #### select nice label numbers when setting number of ticks on an axis


 


# Check OK: clear to go Apollo
fun_scale <- function(n, lim, kind = "approx", lib.path = NULL){
# AIM
# attempt to select nice scale numbers when setting n ticks on a lim axis range
# ARGUMENTS
# n: desired number of main ticks on the axis (integer above 0)
# lim: vector of 2 numbers indicating the limit range of the axis. Order of the 2 values matters (for inverted axis). Can be log transformed values
# kind: either "approx" (approximative), "strict" (strict) or "strict.cl" (strict clean). If "approx", use the scales::trans_breaks() function to provide an easy to read scale of approximately n ticks spanning the range of the lim argument. If "strict", cut the range of the lim argument into n + 1 equidistant part and return the n numbers at each boundary. This often generates numbers uneasy to read. If "strict.cl", provide an easy to read scale of exactly n ticks, but sometimes not completely spanning the range of the lim argument
# lib.path: absolute path of the required packages, if not in the default folders
# REQUIRED PACKAGES
# if kind = "approx":
# ggplot2
# scales
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# fun_check()
# fun_round()
# RETURN
# a vector of numbers
# EXAMPLES
# approximate number of main ticks
# ymin = 2 ; ymax = 3.101 ; n = 5 ; scale <- fun_scale(n = n, lim = c(ymin, ymax), kind = "approx") ; scale ; par(yaxt = "n", yaxs = "i", las = 1) ; plot(ymin:ymax, ymin:ymax, xlim = range(scale, ymin, ymax)[order(c(ymin, ymax))], ylim = range(scale, ymin, ymax)[order(c(ymin, ymax))], xlab = "DEFAULT SCALE", ylab = "NEW SCALE") ; par(yaxt = "s") ; axis(side = 2, at = scale)
# strict number of main ticks
# ymin = 2 ; ymax = 3.101 ; n = 5 ; scale <- fun_scale(n = n, lim = c(ymin, ymax), kind = "strict") ; scale ; par(yaxt = "n", yaxs = "i", las = 1) ; plot(ymin:ymax, ymin:ymax, xlim = range(scale, ymin, ymax)[order(c(ymin, ymax))], ylim = range(scale, ymin, ymax)[order(c(ymin, ymax))], xlab = "DEFAULT SCALE", ylab = "NEW SCALE") ; par(yaxt = "s") ; axis(side = 2, at = scale)
# strict "clean" number of main ticks
# ymin = 2 ; ymax = 3.101 ; n = 5 ; scale <- fun_scale(n = n, lim = c(ymin, ymax), kind = "strict.cl") ; scale ; par(yaxt = "n", yaxs = "i", las = 1) ; plot(ymin:ymax, ymin:ymax, xlim = range(scale, ymin, ymax)[order(c(ymin, ymax))], ylim = range(scale, ymin, ymax)[order(c(ymin, ymax))], xlab = "DEFAULT SCALE", ylab = "NEW SCALE") ; par(yaxt = "s") ; axis(side = 2, at = scale)
# approximate number of main ticks, scale inversion
# ymin = 3.101 ; ymax = 2 ; n = 5 ; scale <- fun_scale(n = n, lim = c(ymin, ymax), kind = "approx") ; scale ; par(yaxt = "n", yaxs = "i", las = 1) ; plot(ymin:ymax, ymin:ymax, xlim = range(scale, ymin, ymax)[order(c(ymin, ymax))], ylim = range(scale, ymin, ymax)[order(c(ymin, ymax))], xlab = "DEFAULT SCALE", ylab = "NEW SCALE") ; par(yaxt = "s") ; axis(side = 2, at = scale)
# DEBUGGING
# n = 9 ; lim = c(2, 3.101) ; kind = "approx" ; lib.path = NULL # for function debugging
# n = 10 ; lim = c(1e-4, 1e6) ; kind = "approx" ; lib.path = NULL # for function debugging
# function name
function.name <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
# end function name
# end initial argument checking
# required function checking
if(length(utils::find("fun_check", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_check() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end required function checking
# argument checking
arg.check <- NULL #
text.check <- NULL #
checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$fun.name))
tempo <- fun_check(data = n, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, neg.values = FALSE, fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE & isTRUE(all.equal(n, 0))){ # isTRUE(all.equal(n, 0)) equivalent to n == 0 but deals with floats (approx ok)
tempo.cat <- paste0("ERROR IN ", function.name, ": n ARGUMENT MUST BE A NON NULL AND POSITIVE INTEGER")
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE) # 
}
tempo <- fun_check(data = lim, class = "vector", mode = "numeric", length = 2, fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE & all(diff(lim) == 0)){ # isTRUE(all.equal(diff(lim), rep(0, length(diff(lim))))) not used because we strictly need zero as a result
tempo.cat <- paste0("ERROR IN ", function.name, ": lim ARGUMENT HAS A NULL RANGE (2 IDENTICAL VALUES)")
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}else if(tempo$problem == FALSE & any(lim %in% c(Inf, -Inf))){
tempo.cat <- paste0("ERROR IN ", function.name, ": lim ARGUMENT CANNOT CONTAIN -Inf OR Inf VALUES")
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
tempo <- fun_check(data = kind, options = c("approx", "strict", "strict.cl"), length = 1, fun.name = function.name) ; eval(ee)
if( ! is.null(lib.path)){
tempo <- fun_check(data = lib.path, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE){
if( ! all(dir.exists(lib.path))){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
tempo.cat <- paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", paste(lib.path, collapse = "\n"))
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
}
}
if(any(arg.check) == TRUE){
stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
}
# end argument checking with fun_check()
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
# end argument checking
# main code
lim.order <- order(lim) # to deal with inverted axis
lim <- sort(lim)
if(kind == "approx"){
# package checking
fun_pack(req.package = c("ggplot2"), lib.path = lib.path)
fun_pack(req.package = c("scales"), lib.path = lib.path)
# end package checking
output <- ggplot2::ggplot_build(ggplot2::ggplot() + ggplot2::scale_y_continuous(
breaks = scales::trans_breaks(
trans = "identity", 
inv = "identity", 
n = n
), 
limits = lim
))$layout$panel_params[[1]]$y$breaks # pretty() alone is not appropriate: tempo.pret <-  pretty(seq(lim[1] ,lim[2], length.out = n)) ; tempo.pret[tempo.pret > = lim[1] & tempo.pret < = lim[2]]. # in ggplot 3.3.0, tempo.coord$y.major_source replaced by tempo.coord$y$breaks
if( ! is.null(attributes(output))){ # layout$panel_params[[1]]$y$breaks can be characters (labels of the axis). In that case, it has attributes that corresponds to positions
output <- unlist(attributes(output))
}
output <- output[ ! is.na(output)]
}else if(kind == "strict"){
output <- fun_round(seq(lim[1] ,lim[2], length.out = n), 2)
}else if(kind == "strict.cl"){
tempo.range <- diff(sort(lim))
tempo.max <- max(lim)
tempo.min <- min(lim)
mid <- tempo.min + (tempo.range/2) # middle of axis
tempo.inter <- tempo.range / (n + 1) # current interval between two ticks, between 0 and Inf
if(tempo.inter == 0){ # isTRUE(all.equal(tempo.inter, rep(0, length(tempo.inter)))) not used because we strictly need zero as a result
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": THE INTERVAL BETWEEN TWO TICKS OF THE SCALE IS NULL. MODIFY THE lim OR n ARGUMENT\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
log10.abs.lim <- 200
log10.range <- (-log10.abs.lim):log10.abs.lim
log10.vec <- 10^log10.range
round.vec <- c(5, 4, 3, 2.5, 2, 1.25, 1)
dec.table <- outer(log10.vec, round.vec) # table containing the scale units (row: power of ten from -201 to +199, column: the 5, 2.5, 2, 1.25, 1 notches

 

# recover the number of leading zeros in tempo.inter
ini.scipen <- options()$scipen
options(scipen = -1000) # force scientific format
if(any(grepl(pattern = "\\+", x = tempo.inter))){ # tempo.inter > 1
power10.exp <- as.integer(substring(text = tempo.inter, first = (regexpr(pattern = "\\+", text = tempo.inter) + 1))) # recover the power of 10. Example recover 08 from 1e+08
mantisse <- as.numeric(substr(x = tempo.inter, start = 1, stop = (regexpr(pattern = "\\+", text = tempo.inter) - 2))) # recover the mantisse. Example recover 1.22 from 1.22e+08
}else if(any(grepl(pattern = "\\-", x = tempo.inter))){ # tempo.inter < 1
power10.exp <- as.integer(substring(text = tempo.inter, first = (regexpr(pattern = "\\-", text = tempo.inter)))) # recover the power of 10. Example recover 08 from 1e+08
mantisse <- as.numeric(substr(x = tempo.inter, start = 1, stop = (regexpr(pattern = "\\-", text = tempo.inter) - 2))) # recover the mantisse. Example recover 1.22 from 1.22e+08
}else{
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": CODE INCONSISTENCY 1\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
tempo.scale <- dec.table[log10.range == power10.exp, ]
# new interval 
inter.select <- NULL
for(i1 in 1:length(tempo.scale)){
tempo.first.tick <- trunc((tempo.min + tempo.scale[i1]) / tempo.scale[i1]) * (tempo.scale[i1]) # this would be use to have a number not multiple of tempo.scale[i1]: ceiling(tempo.min) + tempo.scale[i1] * 10^power10.exp
tempo.last.tick <- tempo.first.tick + tempo.scale[i1] * (n - 1)
if((tempo.first.tick >= tempo.min) & (tempo.last.tick <= tempo.max)){
inter.select <- tempo.scale[i1]
break()
}
}
if(is.null(inter.select)){
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": CODE INCONSISTENCY 2\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
options(scipen = ini.scipen) # restore the initial scientific penalty
# end new interval 
# centering the new scale 
tempo.mid <- trunc((mid + (-1:1) * inter.select) / inter.select) * inter.select # tempo middle tick closest to the middle axis
mid.tick <- tempo.mid[which.min(abs(tempo.mid - mid))]
if(isTRUE(all.equal(n, rep(1, length(n))))){ # isTRUE(all.equal(n, rep(1, length(n)))) is similar to n == 1 but deals with float
output <- mid.tick
}else if(isTRUE(all.equal(n, rep(2, length(n))))){ # isTRUE(all.equal(n, rep(0, length(n)))) is similar to n == 2 but deals with float
output <- mid.tick
tempo.min.dist <- mid.tick - inter.select - tempo.min
tempo.max.dist <- tempo.max - mid.tick + inter.select
if(tempo.min.dist <= tempo.max.dist){ # distance between lowest tick and bottom axis <= distance between highest tick and top axis. If yes, extra tick but at the top, otherwise at the bottom
output <- c(mid.tick, mid.tick + inter.select)
}else{
output <- c(mid.tick - inter.select, mid.tick)
}
}else if((n / 2 - trunc(n / 2)) > 0.1){ # > 0.1 to avoid floating point. Because result can only be 0 or 0.5. Thus, > 0.1 means odd number
output <- c(mid.tick - (trunc(n / 2):1) * inter.select, mid.tick, mid.tick + (1:trunc(n / 2)) * inter.select)
}else if((n / 2 - trunc(n / 2)) < 0.1){ # < 0.1 to avoid floating point. Because result can only be 0 or 0.5. Thus, < 0.1 means even number
tempo.min.dist <- mid.tick - trunc(n / 2) * inter.select - tempo.min
tempo.max.dist <- tempo.max - mid.tick + trunc(n / 2) * inter.select
if(tempo.min.dist <= tempo.max.dist){ # distance between lowest tick and bottom axis <= distance between highest tick and top axis. If yes, extra tick but at the bottom, otherwise at the top
output <- c(mid.tick - ((trunc(n / 2) - 1):1) * inter.select, mid.tick, mid.tick + (1:trunc(n / 2)) * inter.select)
}else{
output <- c(mid.tick - (trunc(n / 2):1) * inter.select, mid.tick, mid.tick + (1:(trunc(n / 2) - 1)) * inter.select)
}
}else{
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": CODE INCONSISTENCY 3\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
# end centering the new scale 
# last check
if(min(output) < tempo.min){
output <- c(output[-1], max(output) + inter.select) # remove the lowest tick and add a tick at the top
}else if( max(output) > tempo.max){
output <- c(min(output) - inter.select, output[-length(output)])
}
if(min(output) < tempo.min | max(output) > tempo.max){
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": CODE INCONSISTENCY 4\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
if(any(is.na(output))){
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": CODE INCONSISTENCY 5 (NA GENERATION)\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
# end last check
}else{
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": CODE INCONSISTENCY 6\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
if(diff(lim.order) < 0){
output <- rev(output)
}
return(output)
}


######## fun_post_plot() #### set graph param after plotting (axes redesign for instance)


 


# Check OK: clear to go Apollo
fun_post_plot <- function(x.side = 0, x.log.scale = FALSE, x.categ = NULL, x.categ.pos = NULL, x.lab = "", x.axis.size = 1.5, x.label.size = 1.5, x.dist.legend = 0.5, x.nb.inter.tick = 1, y.side = 0, y.log.scale = FALSE, y.categ = NULL, y.categ.pos = NULL, y.lab = "", y.axis.size = 1.5, y.label.size = 1.5, y.dist.legend = 0.5, y.nb.inter.tick = 1, text.angle = 90, tick.length = 0.5, sec.tick.length = 0.3, bg.color = NULL, grid.lwd = NULL, grid.col = "white", corner.text = "", corner.text.size = 1, just.label.add = FALSE, par.reset = FALSE, custom.par = NULL){
# AIM
# redesign axis. If x.side = 0, y.side = 0, the function just adds text at topright of the graph and reset par() for next graphics and provides outputs (see below)
# provide also positions for legend or additional text on the graph
# use fun_prior_plot() before this function for initial inactivation of the axis drawings
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# fun_check()
# fun_open() to reinitialize graph parameters if par.reset = TRUE and custom.par = NULL
# ARGUMENTS
# x.side: axis at the bottom (1) or top (3) of the region figure. Write 0 for no change
# x.log.scale: Log scale for the x-axis? Either TRUE or FALSE
# x.categ: character vector representing the classes (levels()) to specify when the x-axis is qualititative(stripchart, boxplot)
# x.categ.pos: position of the classes names (numeric vector of identical length than x.categ). If left NULL, this will be 1:length(levels())
# x.lab: label of the x-axis. If x.side == 0 and x.lab != "", then x.lab is printed
# x.axis.size: positive numeric. Increase or decrease the size of the x axis numbers. Value 1 does not change it, 0.5 decreases by half, 2 increases by 2. Also control the size of displayed categories
# x.label.size: positive numeric. Increase or decrease the size of the x axis legend text. Value 1 does not change it, 0.5 decreases by half, 2 increases by 2
# x.dist.legend: increase the number to move x-axis legends away in inches (first number of mgp argument of par() but in inches)
# x.nb.inter.tick: number of secondary ticks between main ticks on x-axis (only if not log scale). 0 means no secondary ticks
# y.side: axis at the left (2) or right (4) of the region figure. Write 0 for no change
# y.log.scale: Log scale for the y-axis? Either TRUE or FALSE
# y.categ: classes (levels()) to specify when the y-axis is qualititative(stripchart, boxplot)
# y.categ.pos: position of the classes names (numeric vector of identical length than y.categ). If left NULL, this will be 1:length(levels())
# y.lab: label of the y-axis. If y.side == 0 and y.lab != "", then y.lab is printed
# y.axis.size: positive numeric. Increase or decrease the size of the y axis numbers. Value 1 does not change it, 0.5 decreases by half, 2 increases by 2. Also control the size of displayed categories
# y.label.size: positive numeric. Increase or decrease the size of the y axis legend text. Value 1 does not change it, 0.5 decreases by half, 2 increases by 2
# y.dist.legend: increase the number to move y-axis legends away in inches (first number of mgp argument of par() but in inches)
# y.nb.inter.tick: number of secondary ticks between main ticks on y-axis (only if not log scale). 0 means non secondary ticks
# text.angle: angle of the text when axis is qualitative
# tick.length: length of the main ticks (1 means complete the distance between the plot region and the axis numbers, 0.5 means half the length, etc., 0 for no ticks)
# sec.tick.length: length of the secondary ticks (1 means complete the distance between the plot region and the axis numbers, 0.5 means half the length, etc., 0 for no ticks)
# bg.color: background color of the plot region. NULL for no color. BEWARE: cover/hide an existing plot !
# grid.lwd: if non NULL, activate the grid line (specify the line width)
# grid.col: grid line color (only if grid.lwd non NULL)
# corner.text: text to add at the top right corner of the window
# corner.text.size: positive numeric. Increase or decrease the size of the text. Value 1 does not change it, 0.5 decreases by half, 2 increases by 2
# par.reset: to reset all the graphics parameters. BEWARE: TRUE can generate display problems, mainly in graphic devices with multiple figure regions
# just.label.add: just add axis labels (legend)? Either TRUE or FALSE. If TRUE, at least (x.side == 0 & x.lab != "") or (y.side == 0 & y.lab != "") must be set to display the corresponding x.lab or y.lab
# custom.par: list that provides the parameters that reset all the graphics parameters. BEWARE: if NULL and par.reset == TRUE, the default par() parameters are used
# RETURN
# a list containing: 
# $x.mid.left.dev.region: middle of the left margin of the device region, in coordinates of the x-axis
# $x.left.dev.region: left side of the left margin (including the potential margin of the device region), in coordinates of the x-axis
# $x.mid.right.dev.region: middle of the right margin of the device region, in coordinates of the x-axis
# $x.right.dev.region: right side of the right margin (including the potential margin of the device region), in coordinates of the x-axis
# $x.mid.left.fig.region: middle of the left margin of the figure region, in coordinates of the x-axis
# $x.left.fig.region: left side of the left margin, in coordinates of the x-axis
# $x.mid.right.fig.region: middle of the right margin of the figure region, in coordinates of the x-axis
# $x.right.fig.region: right side of the right margin, in coordinates of the x-axis
# $x.left.plot.region: left side of the plot region, in coordinates of the x-axis
# $x.right.plot.region: right side of the plot region, in coordinates of the x-axis
# $x.mid.plot.region: middle of the plot region, in coordinates of the x-axis
# $y.mid.bottom.dev.region: middle of the bottom margin of the device region, in coordinates of the y-axis
# $y.bottom.dev.region: bottom side of the bottom margin (including the potential margin of the device region), in coordinates of the y-axis
# $y.mid.top.dev.region: middle of the top margin of the device region, in coordinates of the y-axis
# $y.top.dev.region: top side of the top margin (including the potential margin of the device region), in coordinates of the y-axis
# $y.mid.bottom.fig.region: middle of the bottom margin of the figure region, in coordinates of the y-axis
# $y.bottom.fig.region: bottom of the bottom margin of the figure region, in coordinates of the y-axis
# $y.mid.top.fig.region: middle of the top margin of the figure region, in coordinates of the y-axis
# $y.top.fig.region: top of the top margin of the figure region, in coordinates of the y-axis
# $y.top.plot.region: top of the plot region, in coordinates of the y-axis
# $y.bottom.plot.region: bottom of the plot region, in coordinates of the y-axis
# $y.mid.plot.region: middle of the plot region, in coordinates of the y-axis
# $text: warning text
# EXAMPLES
# Example of log axis with log y-axis and unmodified x-axis:
# prior.par <- fun_prior_plot(param.reinitial = TRUE, xlog.scale = FALSE, ylog.scale = TRUE, remove.label = TRUE, remove.x.axis = FALSE, remove.y.axis = TRUE, down.space = 1, left.space = 1, up.space = 1, right.space = 1, orient = 1, dist.legend = 0.5, tick.length = 0.5, box.type = "n", amplif.label = 1, amplif.axis = 1, display.extend = FALSE, return.par = TRUE) ; plot(1:100, log = "y") ; fun_post_plot(y.side = 2, y.log.scale = prior.par$ylog, x.lab = "Values", y.lab = "TEST", y.axis.size = 1.25, y.label.size = 1.5, y.dist.legend = 0.7, just.label.add = ! prior.par$ann)
# Example of log axis with redrawn x-axis and y-axis:
# prior.par <- fun_prior_plot(param.reinitial = TRUE) ; plot(1:100) ; fun_post_plot(x.side = 1, x.lab = "Values", y.side = 2, y.lab = "TEST", y.axis.size = 1, y.label.size = 2, y.dist.legend = 0.6)
# Example of title easily added to a plot:
# plot(1:100) ; para <- fun_post_plot(corner.text = "TITLE ADDED") # try also: par(xpd = TRUE) ; text(x = para$x.mid.left.fig.region, y = para$y.mid.top.fig.region, labels = "TITLE ADDED", cex = 0.5)
# example with margins in the device region:
# windows(5,5) ; fun_prior_plot(box.type = "o") ; par(mai=c(0.5,0.5,0.5,0.5), omi = c(0.25,0.25,1,0.25), xaxs = "i", yaxs = "i") ; plot(0:10) ; a <- fun_post_plot(x.side = 0, y.side = 0) ; x <- c(a$x.mid.left.dev.region, a$x.left.dev.region, a$x.mid.right.dev.region, a$x.right.dev.region, a$x.mid.left.fig.region, a$x.left.fig.region, a$x.mid.right.fig.region, a$x.right.fig.region, a$x.right.plot.region, a$x.left.plot.region, a$x.mid.plot.region) ; y <- c(a$y.mid.bottom.dev.region, a$y.bottom.dev.region, a$y.mid.top.dev.region, a$y.top.dev.region, a$y.mid.bottom.fig.region, a$y.bottom.fig.region, a$y.mid.top.fig.region, a$y.top.fig.region, a$y.top.plot.region, a$y.bottom.plot.region, a$y.mid.plot.region) ; par(xpd = NA) ; points(x = rep(5, length(y)), y = y, pch = 16, col = "red") ; text(x = rep(5, length(y)), y = y, c("y.mid.bottom.dev.region", "y.bottom.dev.region", "y.mid.top.dev.region", "y.top.dev.region", "y.mid.bottom.fig.region", "y.bottom.fig.region", "y.mid.top.fig.region", "y.top.fig.region", "y.top.plot.region", "y.bottom.plot.region", "y.mid.plot.region"), cex = 0.65, col = grey(0.25)) ; points(y = rep(5, length(x)), x = x, pch = 16, col = "blue") ; text(y = rep(5, length(x)), x = x, c("x.mid.left.dev.region", "x.left.dev.region", "x.mid.right.dev.region", "x.right.dev.region", "x.mid.left.fig.region", "x.left.fig.region", "x.mid.right.fig.region", "x.right.fig.region", "x.right.plot.region", "x.left.plot.region", "x.mid.plot.region"), cex = 0.65, srt = 90, col = grey(0.25))
# DEBUGGING
# x.side = 0 ; x.log.scale = FALSE ; x.categ = NULL ; x.categ.pos = NULL ; x.lab = "" ; x.axis.size = 1.5 ; x.label.size = 1.5 ; x.dist.legend = 1 ; x.nb.inter.tick = 1 ; y.side = 0 ; y.log.scale = FALSE ; y.categ = NULL ; y.categ.pos = NULL ; y.lab = "" ; y.axis.size = 1.5 ; y.label.size = 1.5 ; y.dist.legend = 0.7 ; y.nb.inter.tick = 1 ; text.angle = 90 ; tick.length = 0.5 ; sec.tick.length = 0.3 ; bg.color = NULL ; grid.lwd = NULL ; grid.col = "white" ; corner.text = "" ; corner.text.size = 1 ; just.label.add = FALSE ; par.reset = FALSE ; custom.par = NULL # for function debugging
# function name
function.name <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
# end function name
# required function checking
if(length(utils::find("fun_check", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_check() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if(length(utils::find("fun_open", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_open() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end required function checking
# argument checking
arg.check <- NULL #
text.check <- NULL #
checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$fun.name))
tempo <- fun_check(data = x.side, options = c(0, 1, 3), length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = x.log.scale, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
if( ! is.null(x.categ)){
tempo <- fun_check(data = x.categ, class = "character", na.contain = TRUE, fun.name = function.name) ; eval(ee)
}
if( ! is.null(x.categ.pos)){
tempo <- fun_check(data = x.categ.pos, class = "vector", mode = "numeric", fun.name = function.name) ; eval(ee)
}
tempo <- fun_check(data = x.lab, class = "character", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = x.axis.size, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = x.label.size, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = x.dist.legend, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = x.nb.inter.tick, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = y.side, options = c(0, 2, 4), length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = y.log.scale, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
if( ! is.null(y.categ)){
tempo <- fun_check(data = y.categ, class = "character", na.contain = TRUE, fun.name = function.name) ; eval(ee)
}
if( ! is.null(y.categ.pos)){
tempo <- fun_check(data = y.categ.pos, class = "vector", mode = "numeric", fun.name = function.name) ; eval(ee)
}
tempo <- fun_check(data = y.lab, class = "character", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = y.axis.size, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = y.label.size, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = y.dist.legend, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = y.nb.inter.tick, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = text.angle, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = tick.length, class = "vector", mode = "numeric", length = 1, prop = TRUE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = sec.tick.length, class = "vector", mode = "numeric", length = 1, prop = TRUE, fun.name = function.name) ; eval(ee)
if( ! is.null(bg.color)){
tempo <- fun_check(data = bg.color, class = "character", length = 1, fun.name = function.name) ; eval(ee)
if( ! (bg.color %in% colors() | grepl(pattern = "^#", bg.color))){ # check color
tempo.cat <- paste0("ERROR IN ", function.name, ": bg.color ARGUMENT MUST BE A HEXADECIMAL COLOR VECTOR STARTING BY # OR A COLOR NAME GIVEN BY colors()")
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
}
if( ! is.null(grid.lwd)){
tempo <- fun_check(data = grid.lwd, class = "vector", mode = "numeric", neg.values = FALSE, fun.name = function.name) ; eval(ee)
}
if( ! is.null(grid.col)){
tempo <- fun_check(data = grid.col, class = "character", length = 1, fun.name = function.name) ; eval(ee)
if( ! (grid.col %in% colors() | grepl(pattern = "^#", grid.col))){ # check color
tempo.cat <- paste0("ERROR IN ", function.name, ": grid.col ARGUMENT MUST BE A HEXADECIMAL COLOR VECTOR STARTING BY # OR A COLOR NAME GIVEN BY colors()")
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
}
tempo <- fun_check(data = corner.text, class = "character", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = corner.text.size, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = just.label.add, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = par.reset, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
if( ! is.null(custom.par)){
tempo <- fun_check(data = custom.par, typeof = "list", length = 1, fun.name = function.name) ; eval(ee)
}
if(any(arg.check) == TRUE){
stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
}
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
# end argument checking
# main code
text <- NULL
par(tcl = -par()$mgp[2] * tick.length)
if(x.log.scale == TRUE){
grid.coord.x <- c(10^par("usr")[1], 10^par("usr")[2])
}else{
grid.coord.x <- c(par("usr")[1], par("usr")[2])
}
if(y.log.scale == TRUE){
grid.coord.y <- c(10^par("usr")[3], 10^par("usr")[4])
}else{
grid.coord.y <- c(par("usr")[3], par("usr")[4])
}
if( ! is.null(bg.color)){
rect(grid.coord.x[1], grid.coord.y[1], grid.coord.x[2], grid.coord.y[2], col = bg.color, border = NA)
}
if( ! is.null(grid.lwd)){
grid(nx = NA, ny = NULL, col = grid.col, lty = 1, lwd = grid.lwd)
}
if(x.log.scale == TRUE){
x.mid.left.dev.region <- 10^(par("usr")[1] - ((par("usr")[2] - par("usr")[1]) / (par("plt")[2] - par("plt")[1])) * par("plt")[1] - ((par("usr")[2] - par("usr")[1]) / ((par("omd")[2] - par("omd")[1]) * (par("plt")[2] - par("plt")[1]))) * par("omd")[1] / 2) # in x coordinates, to position axis labeling at the bottom of the graph (according to x scale)
x.left.dev.region <- 10^(par("usr")[1] - ((par("usr")[2] - par("usr")[1]) / (par("plt")[2] - par("plt")[1])) * par("plt")[1] - ((par("usr")[2] - par("usr")[1]) / ((par("omd")[2] - par("omd")[1]) * (par("plt")[2] - par("plt")[1]))) * par("omd")[1]) # in x coordinates
x.mid.right.dev.region <- 10^(par("usr")[2] + ((par("usr")[2] - par("usr")[1]) / (par("plt")[2] - par("plt")[1])) * (1 - par("plt")[2]) + ((par("usr")[2] - par("usr")[1]) / ((par("omd")[2] - par("omd")[1]) * (par("plt")[2] - par("plt")[1]))) * (1 - par("omd")[2]) / 2) # in x coordinates, to position axis labeling at the top of the graph (according to x scale)
x.right.dev.region <- 10^(par("usr")[2] + ((par("usr")[2] - par("usr")[1]) / (par("plt")[2] - par("plt")[1])) * (1 - par("plt")[2]) + ((par("usr")[2] - par("usr")[1]) / ((par("omd")[2] - par("omd")[1]) * (par("plt")[2] - par("plt")[1]))) * (1 - par("omd")[2])) # in x coordinates
x.mid.left.fig.region <- 10^(par("usr")[1] - ((par("usr")[2] - par("usr")[1]) / (par("plt")[2] - par("plt")[1])) * par("plt")[1] / 2) # in x coordinates, to position axis labeling at the bottom of the graph (according to x scale)
x.left.fig.region <- 10^(par("usr")[1] - ((par("usr")[2] - par("usr")[1]) / (par("plt")[2] - par("plt")[1])) * par("plt")[1]) # in x coordinates
x.mid.right.fig.region <- 10^(par("usr")[2] + ((par("usr")[2] - par("usr")[1]) / (par("plt")[2] - par("plt")[1])) * (1 - par("plt")[2]) / 2) # in x coordinates, to position axis labeling at the top of the graph (according to x scale)
x.right.fig.region <- 10^(par("usr")[2] + ((par("usr")[2] - par("usr")[1]) / (par("plt")[2] - par("plt")[1])) * (1 - par("plt")[2])) # in x coordinates
x.left.plot.region <- 10^par("usr")[1] # in x coordinates, left of the plot region (according to x scale)
x.right.plot.region <- 10^par("usr")[2] # in x coordinates, right of the plot region (according to x scale)
x.mid.plot.region <- 10^((par("usr")[2] + par("usr")[1]) / 2) # in x coordinates, right of the plot region (according to x scale)
}else{
x.mid.left.dev.region <- (par("usr")[1] - ((par("usr")[2] - par("usr")[1]) / (par("plt")[2] - par("plt")[1])) * par("plt")[1] - ((par("usr")[2] - par("usr")[1]) / ((par("omd")[2] - par("omd")[1]) * (par("plt")[2] - par("plt")[1]))) * par("omd")[1] / 2) # in x coordinates, to position axis labeling at the bottom of the graph (according to x scale)
x.left.dev.region <- (par("usr")[1] - ((par("usr")[2] - par("usr")[1]) / (par("plt")[2] - par("plt")[1])) * par("plt")[1] - ((par("usr")[2] - par("usr")[1]) / ((par("omd")[2] - par("omd")[1]) * (par("plt")[2] - par("plt")[1]))) * par("omd")[1]) # in x coordinates
x.mid.right.dev.region <- (par("usr")[2] + ((par("usr")[2] - par("usr")[1]) / (par("plt")[2] - par("plt")[1])) * (1 - par("plt")[2]) + ((par("usr")[2] - par("usr")[1]) / ((par("omd")[2] - par("omd")[1]) * (par("plt")[2] - par("plt")[1]))) * (1 - par("omd")[2]) / 2) # in x coordinates, to position axis labeling at the top of the graph (according to x scale)
x.right.dev.region <- (par("usr")[2] + ((par("usr")[2] - par("usr")[1]) / (par("plt")[2] - par("plt")[1])) * (1 - par("plt")[2]) + ((par("usr")[2] - par("usr")[1]) / ((par("omd")[2] - par("omd")[1]) * (par("plt")[2] - par("plt")[1]))) * (1 - par("omd")[2])) # in x coordinates
x.mid.left.fig.region <- (par("usr")[1] - ((par("usr")[2] - par("usr")[1]) / (par("plt")[2] - par("plt")[1])) * par("plt")[1] / 2) # in x coordinates, to position axis labeling at the bottom of the graph (according to x scale)
x.left.fig.region <- (par("usr")[1] - ((par("usr")[2] - par("usr")[1]) / (par("plt")[2] - par("plt")[1])) * par("plt")[1]) # in x coordinates
x.mid.right.fig.region <- (par("usr")[2] + ((par("usr")[2] - par("usr")[1]) / (par("plt")[2] - par("plt")[1])) * (1 - par("plt")[2]) / 2) # in x coordinates, to position axis labeling at the top of the graph (according to x scale)
x.right.fig.region <- (par("usr")[2] + ((par("usr")[2] - par("usr")[1]) / (par("plt")[2] - par("plt")[1])) * (1 - par("plt")[2])) # in x coordinates
x.left.plot.region <- par("usr")[1] # in x coordinates, left of the plot region (according to x scale)
x.right.plot.region <- par("usr")[2] # in x coordinates, right of the plot region (according to x scale)
x.mid.plot.region <- (par("usr")[2] + par("usr")[1]) / 2 # in x coordinates, right of the plot region (according to x scale)
}
if(y.log.scale == TRUE){
y.mid.bottom.dev.region <- 10^(par("usr")[3] - ((par("usr")[4] - par("usr")[3]) / (par("plt")[4] - par("plt")[3])) * par("plt")[3] - ((par("usr")[4] - par("usr")[3]) / ((par("omd")[4] - par("omd")[3]) * (par("plt")[4] - par("plt")[3]))) * (par("omd")[3] / 2)) # in y coordinates, to position axis labeling at the bottom of the graph (according to y scale). Ex mid.bottom.space
y.bottom.dev.region <- 10^(par("usr")[3] - ((par("usr")[4] - par("usr")[3]) / (par("plt")[4] - par("plt")[3])) * par("plt")[3] - ((par("usr")[4] - par("usr")[3]) / ((par("omd")[4] - par("omd")[3]) * (par("plt")[4] - par("plt")[3]))) * par("omd")[3]) # in y coordinates
y.mid.top.dev.region <- 10^(par("usr")[4] + ((par("usr")[4] - par("usr")[3]) / (par("plt")[4] - par("plt")[3])) * (1 - par("plt")[4]) + ((par("usr")[4] - par("usr")[3]) / ((par("omd")[4] - par("omd")[3]) * (par("plt")[4] - par("plt")[3]))) * (1 - par("omd")[4]) / 2) # in y coordinates, to position axis labeling at the top of the graph (according to y scale). Ex mid.top.space
y.top.dev.region <- 10^(par("usr")[4] + ((par("usr")[4] - par("usr")[3]) / (par("plt")[4] - par("plt")[3])) * (1 - par("plt")[4]) + ((par("usr")[4] - par("usr")[3]) / ((par("omd")[4] - par("omd")[3]) * (par("plt")[4] - par("plt")[3]))) * (1 - par("omd")[4])) # in y coordinates
y.mid.bottom.fig.region <- 10^(par("usr")[3] - ((par("usr")[4] - par("usr")[3]) / (par("plt")[4] - par("plt")[3])) * par("plt")[3] / 2) # in y coordinates, to position axis labeling at the bottom of the graph (according to y scale). Ex mid.bottom.space
y.bottom.fig.region <- 10^(par("usr")[3] - ((par("usr")[4] - par("usr")[3]) / (par("plt")[4] - par("plt")[3])) * par("plt")[3]) # in y coordinates
y.mid.top.fig.region <- 10^(par("usr")[4] + ((par("usr")[4] - par("usr")[3]) / (par("plt")[4] - par("plt")[3])) * (1 - par("plt")[4]) / 2) # in y coordinates, to position axis labeling at the top of the graph (according to y scale). Ex mid.top.space
y.top.fig.region <- 10^(par("usr")[4] + ((par("usr")[4] - par("usr")[3]) / (par("plt")[4] - par("plt")[3])) * (1 - par("plt")[4])) # in y coordinates
y.top.plot.region <- 10^par("usr")[4] # in y coordinates, top of the plot region (according to y scale)
y.bottom.plot.region <- 10^par("usr")[3] # in y coordinates, bottom of the plot region (according to y scale)
y.mid.plot.region <- (par("usr")[3] + par("usr")[4]) / 2 # in x coordinates, right of the plot region (according to x scale)
}else{
y.mid.bottom.dev.region <- (par("usr")[3] - ((par("usr")[4] - par("usr")[3]) / (par("plt")[4] - par("plt")[3])) * par("plt")[3] - ((par("usr")[4] - par("usr")[3]) / ((par("omd")[4] - par("omd")[3]) * (par("plt")[4] - par("plt")[3]))) * (par("omd")[3] / 2)) # in y coordinates, to position axis labeling at the bottom of the graph (according to y scale). Ex mid.bottom.space
y.bottom.dev.region <- (par("usr")[3] - ((par("usr")[4] - par("usr")[3]) / (par("plt")[4] - par("plt")[3])) * par("plt")[3] - ((par("usr")[4] - par("usr")[3]) / ((par("omd")[4] - par("omd")[3]) * (par("plt")[4] - par("plt")[3]))) * par("omd")[3]) # in y coordinates
y.mid.top.dev.region <- (par("usr")[4] + ((par("usr")[4] - par("usr")[3]) / (par("plt")[4] - par("plt")[3])) * (1 - par("plt")[4]) + ((par("usr")[4] - par("usr")[3]) / ((par("omd")[4] - par("omd")[3]) * (par("plt")[4] - par("plt")[3]))) * (1 - par("omd")[4]) / 2) # in y coordinates, to position axis labeling at the top of the graph (according to y scale). Ex mid.top.space
y.top.dev.region <- (par("usr")[4] + ((par("usr")[4] - par("usr")[3]) / (par("plt")[4] - par("plt")[3])) * (1 - par("plt")[4]) + ((par("usr")[4] - par("usr")[3]) / ((par("omd")[4] - par("omd")[3]) * (par("plt")[4] - par("plt")[3]))) * (1 - par("omd")[4])) # in y coordinates
y.mid.bottom.fig.region <- (par("usr")[3] - ((par("usr")[4] - par("usr")[3]) / (par("plt")[4] - par("plt")[3])) * par("plt")[3] / 2) # in y coordinates, to position axis labeling at the bottom of the graph (according to y scale). Ex mid.bottom.space
y.bottom.fig.region <- (par("usr")[3] - ((par("usr")[4] - par("usr")[3]) / (par("plt")[4] - par("plt")[3])) * par("plt")[3]) # in y coordinates
y.mid.top.fig.region <- (par("usr")[4] + ((par("usr")[4] - par("usr")[3]) / (par("plt")[4] - par("plt")[3])) * (1 - par("plt")[4]) / 2) # in y coordinates, to position axis labeling at the top of the graph (according to y scale). Ex mid.top.space
y.top.fig.region <- (par("usr")[4] + ((par("usr")[4] - par("usr")[3]) / (par("plt")[4] - par("plt")[3])) * (1 - par("plt")[4])) # in y coordinates
y.top.plot.region <- par("usr")[4] # in y coordinates, top of the plot region (according to y scale)
y.bottom.plot.region <- par("usr")[3] # in y coordinates, bottom of the plot region (according to y scale)
y.mid.plot.region <- ((par("usr")[3] + par("usr")[4]) / 2) # in x coordinates, right of the plot region (according to x scale)
}
if(any(sapply(FUN = all.equal, c(1, 3), x.side) == TRUE)){
par(xpd=FALSE, xaxt="s")
if(is.null(x.categ) & x.log.scale == TRUE){
if(any(par()$xaxp[1:2] == 0)){ # any(sapply(FUN = all.equal, par()$xaxp[1:2], 0) == TRUE) not used because we strictly need zero as a result. Beware: write "== TRUE", because the result is otherwise character and a warning message appears using any()
if(par()$xaxp[1] == 0){ # isTRUE(all.equal(par()$xaxp[1], 0)) not used because we strictly need zero as a result
par(xaxp = c(10^-30, par()$xaxp[2:3])) # because log10(par()$xaxp[1] == 0) == -Inf
}
if(par()$xaxp[2] == 0){ # isTRUE(all.equal(par()$xaxp[1], 0)) not used because we strictly need zero as a result
par(xaxp = c(par()$xaxp[1], 10^-30, par()$xaxp[3])) # because log10(par()$xaxp[2] == 0) == -Inf
}
}
axis(side = x.side, at = c(10^par()$usr[1], 10^par()$usr[2]), labels=rep("", 2), lwd=1, lwd.ticks = 0) # draw the axis line
mtext(side = x.side, text = x.lab, line = x.dist.legend / 0.2, las = 0, cex = x.label.size)
par(tcl = -par()$mgp[2] * sec.tick.length) # length of the secondary ticks are reduced
suppressWarnings(rug(10^outer(c((log10(par("xaxp")[1]) -1):log10(par("xaxp")[2])), log10(1:10), "+"), ticksize = NA, side = x.side)) # ticksize = NA to allow the use of par()$tcl value
par(tcl = -par()$mgp[2] * tick.length) # back to main ticks
axis(side = x.side, at = c(1e-15, 1e-14, 1e-13, 1e-12, 1e-11, 1e-10, 1e-9, 1e-8, 1e-7, 1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10), labels = expression(10^-15, 10^-14, 10^-13, 10^-12, 10^-11, 10^-10, 10^-9, 10^-8, 10^-7, 10^-6, 10^-5, 10^-4, 10^-3, 10^-2, 10^-1, 10^0, 10^1, 10^2, 10^3, 10^4, 10^5, 10^6, 10^7, 10^8, 10^9, 10^10), lwd = 0, lwd.ticks = 1, cex.axis = x.axis.size)
x.text <- 10^par("usr")[2]
}else if(is.null(x.categ) & x.log.scale == FALSE){
axis(side=x.side, at=c(par()$usr[1], par()$usr[2]), labels=rep("", 2), lwd=1, lwd.ticks=0) # draw the axis line
axis(side=x.side, at=round(seq(par()$xaxp[1], par()$xaxp[2], length.out=par()$xaxp[3]+1), 2), cex.axis = x.axis.size) # axis(side=x.side, at=round(seq(par()$xaxp[1], par()$xaxp[2], length.out=par()$xaxp[3]+1), 2), labels = format(round(seq(par()$xaxp[1], par()$xaxp[2], length.out=par()$xaxp[3]+1), 2), big.mark=','), cex.axis = x.axis.size) # to get the 1000 comma separator
mtext(side = x.side, text = x.lab, line = x.dist.legend / 0.2, las = 0, cex = x.label.size)
if(x.nb.inter.tick > 0){
inter.tick.unit <- (par("xaxp")[2] - par("xaxp")[1]) / par("xaxp")[3]
par(tcl = -par()$mgp[2] * sec.tick.length) # length of the ticks are reduced
suppressWarnings(rug(seq(par("xaxp")[1] - 10 * inter.tick.unit, par("xaxp")[2] + 10 * inter.tick.unit, by = inter.tick.unit / (1 + x.nb.inter.tick)), ticksize = NA, x.side)) # ticksize = NA to allow the use of par()$tcl value
par(tcl = -par()$mgp[2] * tick.length) # back to main ticks
}
x.text <- par("usr")[2]
}else if(( ! is.null(x.categ)) & x.log.scale == FALSE){
if(is.null(x.categ.pos)){
x.categ.pos <- 1:length(x.categ)
}else if(length(x.categ.pos) != length(x.categ)){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": x.categ.pos MUST BE THE SAME LENGTH AS x.categ\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
par(xpd = TRUE)
if(isTRUE(all.equal(x.side, 1))){ #isTRUE(all.equal(x.side, 1)) is similar to x.side == 1 but deals with float
segments(x0 = x.left.plot.region, x1 = x.right.plot.region, y0 = y.bottom.plot.region, y1 = y.bottom.plot.region) # draw the line of the axis
text(x = x.categ.pos, y = y.mid.bottom.fig.region, labels = x.categ, srt = text.angle, cex = x.axis.size)
}else if(isTRUE(all.equal(x.side, 3))){ #isTRUE(all.equal(x.side, 1)) is similar to x.side == 3 but deals with float
segments(x0 = x.left.plot.region, x1 = x.right.plot.region, y0 = y.top.plot.region, y1 = y.top.plot.region) # draw the line of the axis
text(x = x.categ.pos, y = y.mid.top.fig.region, labels = x.categ, srt = text.angle, cex = x.axis.size)
}else{
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": ARGUMENT x.side CAN ONLY BE 1 OR 3\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
par(xpd = FALSE)
x.text <- par("usr")[2]
}else{
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": PROBLEM WITH THE x.side (", x.side ,") OR x.log.scale (", x.log.scale,") ARGUMENTS\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
}else{
x.text <- par("usr")[2]
}
if(any(sapply(FUN = all.equal, c(2, 4), y.side) == TRUE)){
par(xpd=FALSE, yaxt="s")
if(is.null(y.categ) & y.log.scale == TRUE){
if(any(par()$yaxp[1:2] == 0)){ # any(sapply(FUN = all.equal, par()$yaxp[1:2], 0) == TRUE) not used because we strictly need zero as a result. Beware: write "== TRUE", because the result is otherwise character and a warning message appears using any()
if(par()$yaxp[1] == 0){ # strict zero needed
par(yaxp = c(10^-30, par()$yaxp[2:3])) # because log10(par()$yaxp[1] == 0) == -Inf
}
if(par()$yaxp[2] == 0){ # strict zero needed
par(yaxp = c(par()$yaxp[1], 10^-30, par()$yaxp[3])) # because log10(par()$yaxp[2] == 0) == -Inf
}
}
axis(side=y.side, at=c(10^par()$usr[3], 10^par()$usr[4]), labels=rep("", 2), lwd=1, lwd.ticks=0) # draw the axis line
par(tcl = -par()$mgp[2] * sec.tick.length) # length of the ticks are reduced
suppressWarnings(rug(10^outer(c((log10(par("yaxp")[1])-1):log10(par("yaxp")[2])), log10(1:10), "+"), ticksize = NA, side = y.side)) # ticksize = NA to allow the use of par()$tcl value
par(tcl = -par()$mgp[2] * tick.length) # back to main tick length
axis(side = y.side, at = c(1e-15, 1e-14, 1e-13, 1e-12, 1e-11, 1e-10, 1e-9, 1e-8, 1e-7, 1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10), labels = expression(10^-15, 10^-14, 10^-13, 10^-12, 10^-11, 10^-10, 10^-9, 10^-8, 10^-7, 10^-6, 10^-5, 10^-4, 10^-3, 10^-2, 10^-1, 10^0, 10^1, 10^2, 10^3, 10^4, 10^5, 10^6, 10^7, 10^8, 10^9, 10^10), lwd = 0, lwd.ticks = 1, cex.axis = y.axis.size)
y.text <- 10^(par("usr")[4] + (par("usr")[4] - par("usr")[3]) / (par("plt")[4] - par("plt")[3]) * (1 - par("plt")[4]))
mtext(side = y.side, text = y.lab, line = y.dist.legend / 0.2, las = 0, cex = y.label.size)
}else if(is.null(y.categ) & y.log.scale == FALSE){
axis(side=y.side, at=c(par()$usr[3], par()$usr[4]), labels=rep("", 2), lwd=1, lwd.ticks=0) # draw the axis line
axis(side=y.side, at=round(seq(par()$yaxp[1], par()$yaxp[2], length.out=par()$yaxp[3]+1), 2), cex.axis = y.axis.size)
mtext(side = y.side, text = y.lab, line = y.dist.legend / 0.2, las = 0, cex = y.label.size)
if(y.nb.inter.tick > 0){
inter.tick.unit <- (par("yaxp")[2] - par("yaxp")[1]) / par("yaxp")[3]
par(tcl = -par()$mgp[2] * sec.tick.length) # length of the ticks are reduced
suppressWarnings(rug(seq(par("yaxp")[1] - 10 * inter.tick.unit, par("yaxp")[2] + 10 * inter.tick.unit, by = inter.tick.unit / (1 + y.nb.inter.tick)), ticksize = NA, side=y.side)) # ticksize = NA to allow the use of par()$tcl value
par(tcl = -par()$mgp[2] * tick.length) # back to main tick length
}
y.text <- (par("usr")[4] + (par("usr")[4] - par("usr")[3]) / (par("plt")[4] - par("plt")[3]) * (1 - par("plt")[4]))
}else if(( ! is.null(y.categ)) & y.log.scale == FALSE){
if(is.null(y.categ.pos)){
y.categ.pos <- 1:length(y.categ)
}else if(length(y.categ.pos) != length(y.categ)){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": y.categ.pos MUST BE THE SAME LENGTH AS y.categ\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
axis(side = y.side, at = y.categ.pos, labels = rep("", length(y.categ)), lwd=0, lwd.ticks=1) # draw the line of the axis
par(xpd = TRUE)
if(isTRUE(all.equal(y.side, 2))){ #isTRUE(all.equal(y.side, 2)) is similar to y.side == 2 but deals with float
text(x = x.mid.left.fig.region, y = y.categ.pos, labels = y.categ, srt = text.angle, cex = y.axis.size)
}else if(isTRUE(all.equal(y.side, 4))){ # idem
text(x = x.mid.right.fig.region, y = y.categ.pos, labels = y.categ, srt = text.angle, cex = y.axis.size)
}else{
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": ARGUMENT y.side CAN ONLY BE 2 OR 4\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
par(xpd = FALSE)
y.text <- (par("usr")[4] + (par("usr")[4] - par("usr")[3]) / (par("plt")[4] - par("plt")[3]) * (1 - par("plt")[4]))
}else{
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": PROBLEM WITH THE y.side (", y.side ,") OR y.log.scale (", y.log.scale,") ARGUMENTSn\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
}else{
y.text <- (par("usr")[4] + (par("usr")[4] - par("usr")[3]) / (par("plt")[4] - par("plt")[3]) * (1 - par("plt")[4]))
}
par(xpd=NA)
text(x = x.mid.right.fig.region, y = y.text, corner.text, adj=c(1, 1.1), cex = corner.text.size) # text at the topright corner. Replace x.right.fig.region by x.text if text at the right edge of the plot region
if(just.label.add == TRUE & isTRUE(all.equal(x.side, 0)) & x.lab != ""){
text(x = x.mid.plot.region, y = y.mid.bottom.fig.region, x.lab, adj=c(0.5, 0.5), cex = x.label.size) # x label
}
if(just.label.add == TRUE & isTRUE(all.equal(y.side, 0)) & y.lab != ""){
text(x = y.mid.plot.region, y = x.mid.left.fig.region, y.lab, adj=c(0.5, 0.5), cex = y.label.size) # x label
}
par(xpd=FALSE)
if(par.reset == TRUE){
tempo.par <- fun_open(pdf.disp = FALSE, return.output = TRUE)
invisible(dev.off()) # close the new window
if( ! is.null(custom.par)){
if( ! names(custom.par) %in% names(tempo.par$ini.par)){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": custom.par ARGUMENT SHOULD HAVE THE NAMES OF THE COMPARTMENT LIST COMING FROM THE par() LIST\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
par(custom.par)
text <- c(text, "\nGRAPH PARAMETERS SET TO VALUES DEFINED BY custom.par ARGUMENT\n")
}else{
par(tempo.par$ini.par)
text <- c(text, "\nGRAPH PARAMETERS RESET TO par() DEFAULT VALUES\n")
}
}
output <- list(x.mid.left.dev.region = x.mid.left.dev.region, x.left.dev.region = x.left.dev.region, x.mid.right.dev.region = x.mid.right.dev.region, x.right.dev.region = x.right.dev.region, x.mid.left.fig.region = x.mid.left.fig.region, x.left.fig.region = x.left.fig.region, x.mid.right.fig.region = x.mid.right.fig.region, x.right.fig.region = x.right.fig.region, x.left.plot.region = x.left.plot.region, x.right.plot.region = x.right.plot.region, x.mid.plot.region = x.mid.plot.region, y.mid.bottom.dev.region = y.mid.bottom.dev.region, y.bottom.dev.region = y.bottom.dev.region, y.mid.top.dev.region = y.mid.top.dev.region, y.top.dev.region = y.top.dev.region, y.mid.bottom.fig.region = y.mid.bottom.fig.region, y.bottom.fig.region = y.bottom.fig.region, y.mid.top.fig.region = y.mid.top.fig.region, y.top.fig.region = y.top.fig.region, y.top.plot.region = y.top.plot.region, y.bottom.plot.region = y.bottom.plot.region, y.mid.plot.region = y.mid.plot.region, text = text)
return(output)
}


######## fun_close() #### close specific graphic windows


# Check OK: clear to go Apollo
fun_close <- function(kind = "pdf", return.text = FALSE){
# AIM
# close only specific graphic windows (devices)
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# fun_check()
# ARGUMENTS:
# kind: vector, among c("windows", "quartz", "x11", "X11", "pdf", "bmp", "png", "tiff"), indicating the kind of graphic windows (devices) to close. BEWARE: either "windows", "quartz", "x11" or "X11" means that all the X11 GUI graphics devices will be closed, whatever the OS used
# return.text: print text regarding the kind parameter and the devices that were finally closed?
# RETURN
# text regarding the kind parameter and the devices that were finally closed
# EXAMPLES
# windows() ; windows() ; pdf() ; dev.list() ; fun_close(kind = c("pdf", "x11"), return.text = TRUE) ; dev.list()
# DEBUGGING
# kind = c("windows", "pdf") ; return.text = FALSE # for function debugging
# function name
function.name <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
# end function name
# required function checking
if(length(utils::find("fun_check", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_check() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end required function checking
# argument checking
arg.check <- NULL #
text.check <- NULL #
checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$fun.name))
tempo <- fun_check(data = kind, options = c("windows", "quartz", "x11", "X11", "pdf", "bmp", "png", "tiff"), fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = return.text, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
if(any(arg.check) == TRUE){
stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
}
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
# end argument checking
# main code
text <- paste0("THE REQUIRED KIND OF GRAPHIC DEVICES TO CLOSE ARE ", paste(kind, collapse = " "))
if(Sys.info()["sysname"] == "Windows"){ # Note that .Platform$OS.type() only says "unix" for macOS and Linux and "Windows" for Windows
if(any(kind %in% c("windows", "quartz", "x11", "X11"))){
tempo <- kind %in% c("windows", "quartz", "x11", "X11")
kind[tempo] <- "windows" # term are replaced by what is displayed when using a <- dev.list() ; names(a)
}
}else if(Sys.info()["sysname"] == "Linux"){
if(any(kind %in% c("windows", "quartz", "x11", "X11"))){
tempo.device <- suppressWarnings(try(X11(), silent = TRUE))[] # open a X11 window to try to recover the X11 system used
if( ! is.null(tempo.device)){
text <- paste0(text, "\nCANNOT CLOSE GUI GRAPHIC DEVICES AS REQUIRED BECAUSE THIS LINUX SYSTEM DOES NOT HAVE IT")
}else{
tempo <- kind %in% c("windows", "quartz", "x11", "X11")
kind[tempo] <- names(dev.list()[length(dev.list())]) # term are replaced by what is displayed when using a <- dev.list() ; names(a)
invisible(dev.off()) # close the X11 opened by tempo
}
}
}else{ # for macOS
if(any(kind %in% c("windows", "quartz", "x11", "X11"))){
tempo <- kind %in% c("windows", "quartz", "x11", "X11")
kind[tempo] <- "quartz" # term are replaced by what is displayed when using a <- dev.list() ; names(a)
}
}
kind <- unique(kind)
if(length(dev.list()) != 0){
for(i in length(names(dev.list())):1){
if(names(dev.list())[i] %in% kind){
text <- paste0(text, "\n", names(dev.list())[i], " DEVICE NUMBER ", dev.list()[i], " HAS BEEN CLOSED")
invisible(dev.off(dev.list()[i]))
}
}
}
if(return.text == TRUE){
return(text)
}
}


################ Standard graphics


######## fun_empty_graph() #### text to display for empty graphs


 


# Check OK: clear to go Apollo
fun_empty_graph <- function(text = NULL, text.size = 1, title = NULL, title.size = 1.5){
# AIM
# display an empty plot with a text in the middle of the window (for instance to specify that no plot can be drawn)
# ARGUMENTS
# text: character string of the message to display
# text.size: numeric value of the text size
# title: character string of the graph title
# title.size: numeric value of the title size (in points)
# REQUIRED PACKAGES
# none
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# fun_check()
# RETURN
# an empty plot
# EXAMPLES
# simple example
# fun_empty_graph(text = "NO GRAPH")
# white page
# fun_empty_graph() # white page
# all the arguments
# fun_empty_graph(text = "NO GRAPH", text.size = 2, title = "GRAPH1", title.size = 1)
# DEBUGGING
# text = "NO GRAPH" ; title = "GRAPH1" ; text.size = 1
# function name
function.name <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
# end function name
# required function checking
if(length(utils::find("fun_check", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_check() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end required function checking
# argument checking
arg.check <- NULL #
text.check <- NULL #
checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$fun.name))
if( ! is.null(text)){
tempo <- fun_check(data = text, class = "vector", mode = "character", length = 1, fun.name = function.name) ; eval(ee)
}
tempo <- fun_check(data = text.size, class = "vector", mode = "numeric", length = 1, fun.name = function.name) ; eval(ee)
if( ! is.null(title)){
tempo <- fun_check(data = title, class = "vector", mode = "character", length = 1, fun.name = function.name) ; eval(ee)
}
tempo <- fun_check(data = title.size, class = "vector", mode = "numeric", length = 1, fun.name = function.name) ; eval(ee)
if(any(arg.check) == TRUE){
stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
}
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
# end argument checking
# main code
ini.par <- par(no.readonly = TRUE) # to recover the initial graphical parameters if required (reset). BEWARE: this command alone opens a pdf of GUI window if no window already opened. But here, protected with the code because always a tempo window opened
par(ann=FALSE, xaxt="n", yaxt="n", mar = rep(1, 4), bty = "n", xpd = NA)
plot(1, 1, type = "n") # no display with type = "n"
x.left.dev.region <- (par("usr")[1] - ((par("usr")[2] - par("usr")[1]) / (par("plt")[2] - par("plt")[1])) * par("plt")[1] - ((par("usr")[2] - par("usr")[1]) / ((par("omd")[2] - par("omd")[1]) * (par("plt")[2] - par("plt")[1]))) * par("omd")[1])
y.top.dev.region <- (par("usr")[4] + ((par("usr")[4] - par("usr")[3]) / (par("plt")[4] - par("plt")[3])) * (1 - par("plt")[4]) + ((par("usr")[4] - par("usr")[3]) / ((par("omd")[4] - par("omd")[3]) * (par("plt")[4] - par("plt")[3]))) * (1 - par("omd")[4]))
if( ! is.null(text)){
text(x = 1, y = 1, labels = text, cex = text.size)
}
if( ! is.null(title)){
text(x = x.left.dev.region, y = y.top.dev.region, labels = title, adj=c(0, 1), cex = title.size)
}
par(ini.par)
}


################ gg graphics


######## fun_gg_palette() #### ggplot2 default color palette


 


# Check OK: clear to go Apollo
fun_gg_palette <- function(n, kind = "std"){
# AIM
# provide colors used by ggplot2
# the interest is to use another single color that is not the red one used by default
# for ggplot2 specifications, see: https://ggplot2.tidyverse.org/articles/ggplot2-specs.html
# ARGUMENTS
# n: number of groups on the graph
# kind: either "std" for standard gg colors, "dark" for darkened gg colors, or "light" for pastel gg colors
# REQUIRED PACKAGES
# none
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# fun_check()
# RETURN
# the vector of hexadecimal colors
# EXAMPLES
# output of the function
# fun_gg_palette(n = 2)
# the ggplot2 palette when asking for 7 different colors
# plot(1:7, pch = 16, cex = 5, col = fun_gg_palette(n = 7))
# selection of the 5th color of the ggplot2 palette made of 7 different colors
# plot(1:7, pch = 16, cex = 5, col = fun_gg_palette(n = 7)[5])
# the ggplot2 palette made of 7 darkened colors
# plot(1:7, pch = 16, cex = 5, col = fun_gg_palette(n = 7, kind = "dark"))
# the ggplot2 palette made of 7 lighten colors
# plot(1:7, pch = 16, cex = 5, col = fun_gg_palette(n = 7, kind = "light"))
# DEBUGGING
# n = 0
# kind = "std"
# function name
function.name <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
# end function name
# required function checking
if(length(utils::find("fun_check", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_check() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end required function checking
# argument checking
arg.check <- NULL #
text.check <- NULL #
checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$fun.name))
tempo <- fun_check(data = n, class = "integer", length = 1, double.as.integer.allowed = TRUE, neg.values = FALSE, fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE & isTRUE(all.equal(n, 0))){ # isTRUE(all.equal(n, 0))) is similar to n == 0 but deals with float
tempo.cat <- paste0("ERROR IN ", function.name, ": n ARGUMENT MUST BE A NON ZERO INTEGER. HERE IT IS: ", paste(n, collapse = " "))
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
tempo <- fun_check(data = kind, options = c("std", "dark", "light"), length = 1, fun.name = function.name) ; eval(ee)
}
if(any(arg.check) == TRUE){
stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
}
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
# end argument checking
# main code
hues = seq(15, 375, length = n + 1)
hcl(h = hues, l = if(kind == "std"){65}else if(kind == "dark"){35}else if(kind == "light"){85}, c = 100)[1:n]
}


######## fun_gg_just() #### ggplot2 justification of the axis labeling, depending on angle


 


# Check OK: clear to go Apollo
fun_gg_just <- function(angle, axis){
# AIM
# provide correct justification for axis labeling, depending on the chosen angle
# ARGUMENTS
# angle: integer value of the text angle for the axis labels. Positive values for counterclockwise rotation: 0 for horizontal, 90 for vertical, 180 for upside down etc. Negative values for clockwise rotation: 0 for horizontal, -90 for vertical, -180 for upside down etc.
# axis: which axis for? Either "x" or "y"
# REQUIRED PACKAGES
# none
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# fun_check()
# RETURN
# a list containing:
# $angle: the submitted angle (value potentially reduced to fit the [-360 ; 360] interval, e.g., 460 -> 100, without impact on the final angle displayed)
# $hjust: the horizontal justification
# $vjust: the vertical justification
# EXAMPLES
# fun_gg_just(angle = 45, axis = "x")
# fun_gg_just(angle = (360*2 + 45), axis = "y")
# output <- fun_gg_just(angle = 45, axis = "x") ; obs1 <- data.frame(time = 1:20, group = rep(c("CLASS_1", "CLASS_2"), times = 10)) ; ggplot2::ggplot() + ggplot2::geom_bar(data = obs1, mapping = ggplot2::aes(x = group, y = time), stat = "identity") + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = output$angle, hjust = output$hjust, vjust = output$vjust))
# output <- fun_gg_just(angle = -45, axis = "y") ; obs1 <- data.frame(time = 1:20, group = rep(c("CLASS_1", "CLASS_2"), times = 10)) ; ggplot2::ggplot() + ggplot2::geom_bar(data = obs1, mapping = ggplot2::aes(x = group, y = time), stat = "identity") + ggplot2::theme(axis.text.y = ggplot2::element_text(angle = output$angle, hjust = output$hjust, vjust = output$vjust)) + ggplot2::coord_flip()
# output1 <- fun_gg_just(angle = 90, axis = "x") ; output2 <- fun_gg_just(angle = -45, axis = "y") ; obs1 <- data.frame(time = 1:20, group = rep(c("CLASS_1", "CLASS_2"), times = 10)) ; ggplot2::ggplot() + ggplot2::geom_bar(data = obs1, mapping = ggplot2::aes(x = group, y = time), stat = "identity") + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = output1$angle, hjust = output1$hjust, vjust = output1$vjust), axis.text.y = ggplot2::element_text(angle = output2$angle, hjust = output2$hjust, vjust = output2$vjust))
# DEBUGGING
# angle = 45 ; axis = "y"
# function name
function.name <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
# end function name
# required function checking
if(length(utils::find("fun_check", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_check() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end required function checking
# argument checking
arg.check <- NULL #
text.check <- NULL #
checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$fun.name))
tempo <- fun_check(data = angle, class = "integer", length = 1, double.as.integer.allowed = TRUE, neg.values = TRUE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = axis, options = c("x", "y"), length = 1, fun.name = function.name) ; eval(ee)
if(any(arg.check) == TRUE){
stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
}
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
# end argument checking
# main code
# to get angle between -360 and 360
while(angle > 360){
angle <- angle - 360
}
while(angle < -360){
angle <- angle + 360
}
# end to get angle between -360 and 360
# justifications
if(axis == "x"){
if(any(sapply(FUN = all.equal, c(-360, -180, 0, 180, 360), angle) == TRUE)){ # equivalent of angle == -360 | angle == -180 | angle == 0 | angle == 180 | angle == 360 but deals with floats
hjust <- 0.5
vjust <- 0.5
}else if(any(sapply(FUN = all.equal, c(-270, 90), angle) == TRUE)){
hjust <- 1
vjust <- 0.5
}else if(any(sapply(FUN = all.equal, c(-90, 270), angle) == TRUE)){
hjust <- 0
vjust <- 0.5
}else if((angle > -360 & angle < -270) | (angle > 0 & angle < 90)){
hjust <- 1
vjust <- 1
}else if((angle > -270 & angle < -180) | (angle > 90 & angle < 180)){
hjust <- 1
vjust <- 0
}else if((angle > -180 & angle < -90) | (angle > 180 & angle < 270)){
hjust <- 0
vjust <- 0
}else if((angle > -90 & angle < 0) | (angle > 270 & angle < 360)){
hjust <- 0
vjust <- 1
}
}else if(axis == "y"){
if(any(sapply(FUN = all.equal, c(-270, -90, 90, 270), angle) == TRUE)){ # equivalent of angle == -270 | angle == -90 | angle == 90 | angle == 270 but deals with floats
hjust <- 0.5
vjust <- 0.5
}else if(any(sapply(FUN = all.equal, c(-360, 0, 360), angle) == TRUE)){
hjust <- 1
vjust <- 0.5
}else if(any(sapply(FUN = all.equal, c(-180, 180), angle) == TRUE)){
hjust <- 0
vjust <- 0.5
}else if((angle > -360 & angle < -270) | (angle > 0 & angle < 90)){
hjust <- 1
vjust <- 0
}else if((angle > -270 & angle < -180) | (angle > 90 & angle < 180)){
hjust <- 0
vjust <- 0
}else if((angle > -180 & angle < -90) | (angle > 180 & angle < 270)){
hjust <- 0
vjust <- 1
}else if((angle > -90 & angle < 0) | (angle > 270 & angle < 360)){
hjust <- 1
vjust <- 1
}
}
# end justifications
output <- list(angle = angle, hjust = hjust, vjust = vjust)
return(output)
}


######## fun_gg_point_rast() #### ggplot2 raster scatterplot layer


 


# Check OK: clear to go Apollo
fun_gg_point_rast <- function(data = NULL, mapping = NULL, stat = "identity", position = "identity", ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, raster.width = NULL, raster.height = NULL, raster.dpi = 300, inactivate = TRUE, lib.path = NULL){
# AIM
# equivalent to ggplot2::geom_point() but in raster mode
# use it like ggplot2::geom_point() with the main raster.dpi additional argument
# WARNINGS
# can be long to generate the plot
# use a square plot region. Otherwise, the dots will have ellipsoid shape
# solve the transparency problems with some GUI
# this function is derived from the geom_point_rast() function, created by Viktor Petukhov , and present in the ggrastr package (https://rdrr.io/github/VPetukhov/ggrastr/src/R/geom-point-rast.R, MIT License, Copyright (c) 2017 Viktor Petukhov). Has been placed here to minimize package dependencies
# ARGUMENTS
# classical arguments of geom_point(), shown here https://rdrr.io/github/VPetukhov/ggrastr/man/geom_point_rast.html
# raster.width : width of the result image (in inches). Default: deterined by the current device parameters
# raster.height: height of the result image (in inches). Default: deterined by the current device parameters
# raster.dpi: resolution of the result image
# inactivate: logical. Inactivate the fun.name argument of the fun_check() function? If TRUE, the name of the fun_check() function in error messages coming from this function. Use TRUE if fun_gg_point_rast() is used like this: eval(parse(text = "fun_gg_point_rast"))
# lib.path: absolute path of the required packages, if not in the default folders
# REQUIRED PACKAGES
# ggplot2
# grid
# Cairo
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# fun_check()
# fun_pack()
# RETURN
# a raster scatter plot
# EXAMPLES
# Two pdf in the current directory
# set.seed(1) ; data1 = data.frame(x = rnorm(100000), y = rnorm(10000)) ; fun_open(pdf.name.file = "Raster") ; ggplot2::ggplot() + fun_gg_point_rast(data = data1, mapping = ggplot2::aes(x = x, y = y)) ; fun_open(pdf.name.file = "Vectorial") ; ggplot2::ggplot() + ggplot2::geom_point(data = data1, mapping = ggplot2::aes(x = x, y = y)) ; dev.off() ; dev.off()
# DEBUGGING
# 
# function name
if(all(inactivate == FALSE)){ # inactivate has to be used here but will be fully checked below
function.name <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
}else if(all(inactivate == TRUE)){
function.name <- NULL
}else{
tempo.cat <- paste0("\n\n============\n\nERROR IN fun_gg_point_rast(): CODE INCONSISTENCY 1\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
# end function name
# required function checking
if(length(utils::find("fun_check", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_check() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if(length(utils::find("fun_pack", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_pack() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end required function checking
# argument checking
arg.check <- NULL #
text.check <- NULL #
checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$fun.name))
if( ! is.null(data)){
tempo <- fun_check(data = data, class = "data.frame", na.contain = TRUE, fun.name = function.name) ; eval(ee)
}
if( ! is.null(mapping)){
tempo <- fun_check(data = mapping, class = "uneval", typeof = "list", fun.name = function.name) ; eval(ee) # aes() is tested
}
# stat and position not tested because too complicate
tempo <- fun_check(data = na.rm, class = "vector", mode = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = show.legend, class = "vector", mode = "logical", length = 1, na.contain = TRUE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = inherit.aes, class = "vector", mode = "logical", length = 1, fun.name = function.name) ; eval(ee)
if( ! is.null(raster.width)){
tempo <- fun_check(data = raster.width, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
}
if( ! is.null(raster.height)){
tempo <- fun_check(data = raster.height, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
}
tempo <- fun_check(data = raster.dpi, class = "integer", length = 1, double.as.integer.allowed = TRUE, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = inactivate, class = "vector", mode = "logical", length = 1, fun.name = function.name) ; eval(ee)
if( ! is.null(lib.path)){
tempo <- fun_check(data = lib.path, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE){
if( ! all(dir.exists(lib.path))){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
tempo.cat <- paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", paste(lib.path, collapse = "\n"))
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
}
}
if(any(arg.check) == TRUE){
stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
}
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
# end argument checking
# package checking
fun_pack(req.package = c("ggplot2"), lib.path = lib.path)
fun_pack(req.package = c("grid"), lib.path = lib.path)
fun_pack(req.package = c("Cairo"), lib.path = lib.path)
# end package checking
# additional functions
DrawGeomPointRast <- function(data, panel_params, coord, na.rm = FALSE, raster.width = NULL, raster.height= NULL, raster.dpi = 300){
if (is.null(raster.width)){
raster.width <- par('fin')[1]
}
if (is.null(raster.height)){
 raster.height <- par('fin')[2]
}
prev_dev_id <- dev.cur()
p <- ggplot2::GeomPoint$draw_panel(data, panel_params, coord)
dev_id <- Cairo::Cairo(type='raster', width = raster.width*raster.dpi, height = raster.height*raster.dpi, dpi = raster.dpi, units = 'px', bg = "transparent")[1]
grid::pushViewport(grid::viewport(width = 1, height = 1))
grid::grid.points(x = p$x, y = p$y, pch = p$pch, size = p$size,
name = p$name, gp = p$gp, vp = p$vp, draw = T)
grid::popViewport()
cap <- grid::grid.cap()
dev.off(dev_id)
dev.set(prev_dev_id)
grid::rasterGrob(cap, x = 0, y = 0, width = 1, height = 1, default.units = "native", just = c("left","bottom"))
}
# end additional functions
# main code
GeomPointRast <- ggplot2::ggproto("GeomPointRast", ggplot2::GeomPoint, draw_panel = DrawGeomPointRast)
ggplot2::layer(
data = data, 
mapping = mapping, 
stat = stat, 
geom = GeomPointRast, 
position = position, 
show.legend = show.legend, 
inherit.aes = inherit.aes, 
params = list(
na.rm = na.rm, 
raster.width = raster.width, 
raster.height = raster.height, 
raster.dpi = raster.dpi, 
...
)
)
# end main code
}


######## fun_gg_scatter() #### ggplot2 scatterplot + lines (up to 6 overlays totally)




######## fun_gg_bar() #### ggplot2 mean barplot + overlaid dots if required



######## fun_gg_boxplot() #### ggplot2 boxplot + background dots if required


# No need to change for log.
# change article mode with and without article when facet are present
# https://cran.r-project.org/web/packages/lemon/vignettes/facet-rep-labels.html
# facet title in bold


######## fun_gg_prop() #### ggplot2 proportion barplot


######## fun_gg_dot() #### ggplot2 categorial dotplot + mean/median


######## fun_gg_violin() #### ggplot2 violins


######## fun_gg_line() #### ggplot2 lines + background dots and error bars


######## fun_gg_empty_graph() #### text to display for empty graphs


 


# Check OK: clear to go Apollo
fun_gg_empty_graph <- function(text = NULL, text.size = 12, title = NULL, title.size = 8, lib.path = NULL){
# AIM
# display an empty ggplot2 plot with a text in the middle of the window (for instance to specify that no plot can be drawn)
# ARGUMENTS
# text: character string of the message to display
# text.size: numeric value of the text size (in points)
# title: character string of the graph title
# title.size: numeric value of the title size (in points)
# lib.path: absolute path of the required packages, if not in the default folders
# REQUIRED PACKAGES
# ggplot2
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# fun_check()
# fun_pack()
# RETURN
# an empty plot
# EXAMPLES
### simple example
# fun_gg_empty_graph(text = "NO GRAPH")
### white page
# fun_gg_empty_graph()
### all the arguments
# fun_gg_empty_graph(text = "NO GRAPH", text.size = 8, title = "GRAPH1", title.size = 10, lib.path = NULL)
# DEBUGGING
# text = "NO GRAPH" ; text.size = 12 ; title = "GRAPH1" ; title.size = 8 ; lib.path = NULL
# function name
function.name <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
# end function name
# required function checking
if(length(utils::find("fun_check", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_check() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if(length(utils::find("fun_pack", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_pack() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end required function checking
# argument checking
arg.check <- NULL #
text.check <- NULL #
checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$fun.name))
if( ! is.null(text)){
tempo <- fun_check(data = text, class = "vector", mode = "character", length = 1, fun.name = function.name) ; eval(ee)
}
tempo <- fun_check(data = text.size, class = "vector", mode = "numeric", length = 1, fun.name = function.name) ; eval(ee)
if( ! is.null(title)){
tempo <- fun_check(data = title, class = "vector", mode = "character", length = 1, fun.name = function.name) ; eval(ee)
}
tempo <- fun_check(data = title.size, class = "vector", mode = "numeric", length = 1, fun.name = function.name) ; eval(ee)
if(any(arg.check) == TRUE){
stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
}
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
# end argument checking
# package checking
fun_pack(req.package = c("ggplot2"), lib.path = lib.path)
# end package checking
# main code
tempo.gg.name <- "gg.indiv.plot."
tempo.gg.count <- 0
# no need loop part
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::ggplot())
if( ! is.null(text)){
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_text(data = data.frame(x = 1, y = 1), ggplot2::aes(x = x, y = y, label = text), size = text.size))
}
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::ggtitle(title))
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::theme_void())
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), m.gg <- ggplot2::theme(
plot.title = ggplot2::element_text(size = title.size) # stronger than text
))
suppressWarnings(print(eval(parse(text = paste(paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + ")))))
}


################ Graphic extraction


######## fun_trim() #### display values from a quantitative variable and trim according to defined cut-offs


# Check OK: clear to go Apollo
fun_trim <- function(data, displayed.nb = NULL, single.value.display = FALSE, trim.method = "", trim.cutoffs = c(0.05, 0.975), interval.scale.disp = TRUE, down.space = 0.75, left.space = 0.75, up.space = 0.3, right.space = 0.25, orient = 1, dist.legend = 0.37, box.type = "l", amplif.label = 1.25, amplif.axis = 1.25, std.x.range = TRUE, std.y.range = TRUE, cex.pt = 0.2, col.box = hsv(0.55, 0.8, 0.8), x.nb.inter.tick = 4, y.nb.inter.tick = 0, tick.length = 1, sec.tick.length = 0.75, corner.text = "", amplif.legend = 1, corner.text.size = 0.75, trim.return = FALSE){
# AIM
# trim and display values from a numeric vector or matrix
# plot 4 graphs: stripchart of values, stripchart of rank of values, hitogramme and normal QQPlot
# different kinds of intervals are displayed on the top of graphes to facilitate the analysis of the variable and a trimming setting
# the trimming interval chosen is displayed on top of graphs
# both trimmed and not trimmed values are returned in a list
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# fun_check()
# ARGUMENTS
# data: values to plot (either a numeric vector or a numeric matrix)
# displayed.nb: number of values displayed. If NULL, all the values are displayed. Otherwise, if the number of values is over displayed.nb, then displayed.nb values are displayed after random selection
# single.value.display: provide the 4 graphs if data is made of a single (potentially repeated value)? If FALSE, an empty graph is displayed if data is made of a single (potentially repeated value). And the return list is made of NULL compartments
# trim.method: Write "" if not required. write "mean.sd" if mean +/- sd has to be displayed as a trimming interval (only recommanded for normal distribution). Write "quantile" to display a trimming interval based on quantile cut-offs. No other possibility allowed. See trim.cutoffs below
# trim.cutoffs: 2 values cutoff for the trimming interval displayed, each value between 0 and 1. Not used if trim.method == "".The couple of values c(lower, upper) represents the lower and upper boundaries of the trimming interval (in proportion), which represent the interval of distribution kept (between 0 and 1). Example: trim.cutoffs = c(0.05, 0.975). What is strictly kept for the display is ]lower , upper[, boundaries excluded. Using the "mean.sd" method, 0.025 and 0.975 represent 95% CI which is mean +/- 1.96 * sd
# interval.scale.disp: display sd and quantiles intervals on top of graphs ?
# down.space: lower vertical margin (in inches, mai argument of par())
# left.space: left horizontal margin (in inches, mai argument of par())
# up.space: upper vertical margin between plot region and grapical window (in inches, mai argument of par())
# right.space: right horizontal margin (in inches, mai argument of par())
# orient: scale number orientation (las argument of par()). 0, always parallel to the axis; 1, always horizontal; 2, always perpendicular to the axis; 3, always vertical
# dist.legend: numeric value that moves axis legends away in inches (first number of mgp argument of par() but in inches thus / 0.2)
# box.type: bty argument of par(). Either "o", "l", "7", "c", "u", "]", the resulting box resembles the corresponding upper case letter. A value of "n" suppresses the box
# amplif.label: increase or decrease the size of the text in legends
# amplif.axis: increase or decrease the size of the scale numbers in axis
# std.x.range: standard range on the x-axis? TRUE (no range extend) or FALSE (4% range extend). Controls xaxs argument of par() (TRUE is xaxs = "i", FALSE is xaxs = "r")
# std.y.range: standard range on the y-axis? TRUE (no range extend) or FALSE (4% range extend). Controls yaxs argument of par() (TRUE is yaxs = "i", FALSE is yaxs = "r")
# cex.pt: size of points in stripcharts (in inches, thus cex.pt will be thereafter / 0.2)
# col.box: color of boxplot
# x.nb.inter.tick: number of secondary ticks between main ticks on x-axis (only if not log scale). Zero means non secondary ticks
# y.nb.inter.tick: number of secondary ticks between main ticks on y-axis (only if not log scale). Zero means non secondary ticks
# tick.length: length of the ticks (1 means complete the distance between the plot region and the axis numbers, 0.5 means half the length, etc. 0 means no tick
# sec.tick.length: length of the secondary ticks (1 means complete the distance between the plot region and the axis numbers, 0.5 means half the length, etc., 0 for no ticks)
# corner.text: text to add at the top right corner of the window
# amplif.legend: increase or decrease the size of the text of legend
# corner.text.size: positive numeric. Increase or decrease the size of the text. Value 1 does not change it, 0.5 decreases by half, 2 increases by 2
# trim.return: return the trimmed and non trimmed values? NULL returned for trimmed and non trimmed values if trim.method == ""
# RETURN
# a list containing:
# $trim.method: correspond to trim.method above
# $trim.cutoffs: correspond to trim.cutoffs above
# $real.trim.cutoffs: the two boundary values (in the unit of the numeric vector or numeric matrix analyzed). NULL 
# $trimmed.values: the values outside of the trimming interval as defined in trim.cutoffs above
# $kept.values: the values inside the trimming interval as defined in trim.cutoffs above
# EXAMPLES
# fun_trim(data = c(1:100, 1:10), displayed.nb = NULL, single.value.display = FALSE, trim.method = "mean.sd", trim.cutoffs = c(0.05, 0.975), interval.scale.disp = TRUE, down.space = 0.75, left.space = 0.75, up.space = 0.3, right.space = 0.25, orient = 1, dist.legend = 0.37, box.type = "l", amplif.label = 1.25, amplif.axis = 1.25, std.x.range = TRUE, std.y.range = TRUE, cex.pt = 0.2, col.box = hsv(0.55, 0.8, 0.8), x.nb.inter.tick = 4, y.nb.inter.tick = 0, tick.length = 0.5, sec.tick.length = 0.3, corner.text = "", amplif.legend = 1, corner.text.size = 0.75, trim.return = TRUE)
# DEBUGGING
# data = c(1:100, 1:10) ; displayed.nb = NULL ; single.value.display = FALSE ; trim.method = "quantile" ; trim.cutoffs = c(0.05, 0.975) ; interval.scale.disp = TRUE ; down.space = 1 ; left.space = 1 ; up.space = 0.5 ; right.space = 0.25 ; orient = 1 ; dist.legend = 0.5 ; box.type = "l" ; amplif.label = 1 ; amplif.axis = 1 ; std.x.range = TRUE ; std.y.range = TRUE ; cex.pt = 0.1 ; col.box = hsv(0.55, 0.8, 0.8) ; x.nb.inter.tick = 4 ; y.nb.inter.tick = 0 ; tick.length = 0.5 ; sec.tick.length = 0.3 ; corner.text = "" ; amplif.legend = 1 ; corner.text.size = 0.75 ; trim.return = TRUE # for function debugging
# function name
function.name <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
# end function name
# required function checking
if(length(utils::find("fun_check", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_check() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end required function checking
# argument checking
# argument checking without fun_check()
if( ! (all(class(data) == "numeric") | all(class(data) == "integer") | (all(class(data) == "matrix") & mode(data) == "numeric"))){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": data ARGUMENT MUST BE A NUMERIC VECTOR OR NUMERIC MATRIX\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end argument checking without fun_check()
# argument checking with fun_check()
arg.check <- NULL #
text.check <- NULL #
checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$fun.name))
if( ! is.null(displayed.nb)){
tempo <- fun_check(data = displayed.nb, class = "vector", mode = "numeric", length = 1, fun.name = function.name) ; eval(ee)
if(displayed.nb < 2){
tempo.cat <- paste0("ERROR IN ", function.name, ": displayed.nb ARGUMENT MUST BE A SINGLE INTEGER VALUE GREATER THAN 1 AND NOT: ", paste(displayed.nb, collapse = " "))
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
}
tempo <- fun_check(data = single.value.display, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = trim.method, options = c("", "mean.sd", "quantile"), length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = trim.cutoffs, class = "vector", mode = "numeric", length = 2, prop = TRUE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = interval.scale.disp, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = down.space, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = left.space, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = up.space, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = right.space, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = orient, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = dist.legend, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = box.type, options = c("o", "l", "7", "c", "u", "]", "n"), length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = amplif.label, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = amplif.axis, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = std.x.range, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = std.y.range, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = cex.pt, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = col.box, class = "character", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = x.nb.inter.tick, class = "integer", length = 1, neg.values = FALSE, double.as.integer.allowed = TRUE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = y.nb.inter.tick, class = "integer", length = 1, neg.values = FALSE, double.as.integer.allowed = TRUE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = tick.length, class = "vector", mode = "numeric", length = 1, prop = TRUE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = sec.tick.length, class = "vector", mode = "numeric", length = 1, prop = TRUE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = corner.text, class = "character", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = amplif.legend, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = corner.text.size, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = trim.return, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
if(any(arg.check) == TRUE){
stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
}
# end argument checking with fun_check()
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
# end argument checking
# main code
if(class(data) == "matrix"){
data <- as.vector(data)
}
color.cut <- hsv(0.75, 1, 1) # color of interval selected
col.mean <- hsv(0.25, 1, 0.8) # color of interval using mean+/-sd
col.quantile <- "orange" # color of interval using quantiles
quantiles.selection <- c(0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975, 0.99) # quantiles used in axis to help for choosing trimming cutoffs
if(single.value.display == FALSE & length(unique(data)) == 1){
par(bty = "n", xaxt = "n", yaxt = "n", xpd = TRUE)
plot(1, pch = 16, col = "white", xlab = "", ylab = "")
text(x = 1, y = 1, paste0("No graphic displayed\nBecause data made of a single different value (", formatC(as.double(table(data))), ")"), cex = 2)
output <- list(trim.method = NULL, trim.cutoffs = NULL, real.trim.cutoffs = NULL, trimmed.values = NULL, kept.values = NULL)
}else{
output <- list(trim.method = trim.method, trim.cutoffs = trim.cutoffs, real.trim.cutoffs = NULL, trimmed.values = NULL, kept.values = NULL)
fun.rug <- function(sec.tick.length.f = sec.tick.length, x.nb.inter.tick.f = x.nb.inter.tick, y.nb.inter.tick.f = y.nb.inter.tick){
if(x.nb.inter.tick.f > 0){
inter.tick.unit <- (par("xaxp")[2] - par("xaxp")[1]) / par("xaxp")[3]
par.ini <- par()[c("xpd", "tcl")]
par(xpd = FALSE)
par(tcl = -par()$mgp[2] * sec.tick.length.f) # tcl gives the length of the ticks as proportion of line text, knowing that mgp is in text lines. So the main ticks are a 0.5 of the distance of the axis numbers by default. The sign provides the side of the tick (negative for outside of the plot region)
suppressWarnings(rug(seq(par("xaxp")[1] - 10 * inter.tick.unit, par("xaxp")[2] + 10 * inter.tick.unit, by = inter.tick.unit / (1 + x.nb.inter.tick.f)), ticksize = NA, side = 1)) # ticksize = NA to allow the use of par()$tcl value
par(par.ini)
rm(par.ini)
}
if(y.nb.inter.tick.f > 0){
inter.tick.unit <- (par("yaxp")[2] - par("yaxp")[1]) / par("yaxp")[3]
par.ini <- par()[c("xpd", "tcl")]
par(xpd = FALSE)
par(tcl = -par()$mgp[2] * sec.tick.length.f) # tcl gives the length of the ticks as proportion of line text, knowing that mgp is in text lines. So the main ticks are a 0.5 of the distance of the axis numbers by default. The sign provides the side of the tick (negative for outside of the plot region)
suppressWarnings(rug(seq(par("yaxp")[1] - 10 * inter.tick.unit, par("yaxp")[2] + 10 * inter.tick.unit, by = inter.tick.unit / (1 + y.nb.inter.tick.f)), ticksize = NA, side = 2)) # ticksize = NA to allow the use of par()$tcl value
par(par.ini)
rm(par.ini)
}
}
fun.add.cut <- function(data.f, trim.method.f = trim.method, trim.cutoffs.f = trim.cutoffs, color.cut.f = color.cut, return.f = FALSE){
# DEBUGGING
# data.f = data ; trim.method.f = "mean.sd"; trim.cutoffs.f = trim.cutoffs ; color.cut.f = color.cut ; return.f = TRUE
real.trim.cutoffs.f <- NULL
if(trim.method.f != ""){
data.f <- sort(data.f)
par.ini <- par()$xpd
par(xpd = FALSE)
if(trim.method.f == "mean.sd"){
real.trim.cutoffs.f <- qnorm(trim.cutoffs.f, mean(data.f, na.rm = TRUE), sd(data.f, na.rm = TRUE))
abline(v = qnorm(trim.cutoffs.f, mean(data.f, na.rm = TRUE), sd(data.f, na.rm = TRUE)), col = color.cut.f)
segments(qnorm(trim.cutoffs.f[1], mean(data.f, na.rm = TRUE), sd(data.f, na.rm = TRUE)), par()$usr[4] * 0.75, qnorm(trim.cutoffs.f[2], mean(data.f, na.rm = TRUE), sd(data.f, na.rm = TRUE)), par()$usr[4] * 0.75, col = color.cut.f)
}
if(trim.method.f == "quantile"){
real.trim.cutoffs.f <- quantile(data.f, probs = trim.cutoffs.f, type = 7)
abline(v = quantile(data.f, probs = trim.cutoffs.f, type = 7), col = color.cut.f)
segments(quantile(data.f, probs = trim.cutoffs.f[1], type = 7), par()$usr[4] * 0.75, quantile(data.f, probs = trim.cutoffs.f[2], type = 7), par()$usr[4] * 0.75, col = color.cut.f)
}
par(par.ini)
if(return.f == TRUE){
trimmed.values.f <- data.f[data.f <= real.trim.cutoffs.f[1] | data.f >= real.trim.cutoffs.f[2]]
kept.values.f <- data.f[data.f > real.trim.cutoffs.f[1] & data.f < real.trim.cutoffs.f[2]]
}
}else{
real.trim.cutoffs.f <- NULL
trimmed.values.f <- NULL
kept.values.f <- NULL
}
if(return.f == TRUE){
output <- list(trim.method = trim.method.f, trim.cutoffs = trim.cutoffs.f, real.trim.cutoffs = real.trim.cutoffs.f, trimmed.values = trimmed.values.f, kept.values = kept.values.f)
return(output)
}
}
fun.interval.scale.display <- function(data.f, col.quantile.f = col.quantile, quantiles.selection.f = quantiles.selection, col.mean.f = col.mean){ # intervals on top of graphs
par.ini <- par()[c("mgp", "xpd")]
par(mgp = c(0.25, 0.25, 0), xpd = NA)
axis(side = 3, at = c(par()$usr[1], par()$usr[2]), labels = rep("", 2), col = col.quantile.f, lwd.ticks = 0)
par(xpd = FALSE)
axis(side = 3, at = quantile(as.vector(data.f), probs = quantiles.selection.f, type = 7), labels = quantiles.selection.f, col.axis = col.quantile.f, col = col.quantile.f)
par(mgp = c(1.75, 1.75, 1.5), xpd = NA)
axis(side = 3, at = c(par()$usr[1], par()$usr[2]), labels = rep("", 2), col = col.mean.f, lwd.ticks = 0)
par(xpd = FALSE)
axis(side = 3, at = m + s * qnorm(quantiles.selection.f), labels = formatC(round(qnorm(quantiles.selection.f), 2)), col.axis = col.mean.f, col = col.mean.f, lwd.ticks = 1)
par(par.ini)
}
zone<-matrix(1:4, ncol=2)
layout(zone)
par(omi = c(0, 0, 1.5, 0), mai = c(down.space, left.space, up.space, right.space), las = orient, mgp = c(dist.legend / 0.2, 0.5, 0), xpd = FALSE, bty= box.type, cex.lab = amplif.label, cex.axis = amplif.axis, xaxs = ifelse(std.x.range, "i", "r"), yaxs = ifelse(std.y.range, "i", "r"))
par(tcl = -par()$mgp[2] * tick.length) # tcl gives the length of the ticks as proportion of line text, knowing that mgp is in text lines. So the main ticks are a 0.5 of the distance of the axis numbers by default. The sign provides the side of the tick (negative for outside of the plot region)
if(is.null(displayed.nb)){
sampled.data <- as.vector(data)
if(corner.text == ""){
corner.text <- paste0("ALL VALUES OF THE DATASET DISPLAYED")
}else{
corner.text <- paste0(corner.text, "\nALL VALUES OF THE DATASET DISPLAYED")
}
}else{
if(length(as.vector(data)) > displayed.nb){
sampled.data <- sample(as.vector(data), displayed.nb, replace = FALSE)
if(corner.text == ""){
corner.text <- paste0("BEWARE: ONLY ", displayed.nb, " VALUES ARE DISPLAYED AMONG THE ", length(as.vector(data)), " VALUES OF THE DATASET ANALYZED")
}else{
corner.text <- paste0(corner.text, "\nBEWARE: ONLY ", displayed.nb, " VALUES ARE DISPLAYED AMONG THE ", length(as.vector(data)), " VALUES OF THE DATASET ANALYZED")
}
}else{
sampled.data <- as.vector(data)
if(corner.text == ""){
corner.text <- paste0("BEWARE: THE DISPLAYED NUMBER OF VALUES PARAMETER ", deparse(substitute(displayed.nb)), " HAS BEEN SET TO ", displayed.nb, " WHICH IS ABOVE THE NUMBER OF VALUES OF THE DATASET ANALYZED -> ALL VALUES DISPLAYED")
}else{
corner.text <- paste0(corner.text, "\nBEWARE: THE DISPLAYED NUMBER OF VALUES PARAMETER ", deparse(substitute(displayed.nb)), " HAS BEEN SET TO ", displayed.nb, " WHICH IS ABOVE THE NUMBER OF VALUES OF THE DATASET ANALYZED -> ALL VALUES DISPLAYED")
}
}
}
stripchart(sampled.data, method="jitter", jitter=0.4, vertical=FALSE, ylim=c(0.5, 1.5), group.names = "", xlab = "Value", ylab="", pch=1, cex = cex.pt / 0.2)
fun.rug(y.nb.inter.tick.f = 0)
boxplot(as.vector(data), horizontal=TRUE, add=TRUE, boxwex = 0.4, staplecol = col.box, whiskcol = col.box, medcol = col.box, boxcol = col.box, range = 0, whisklty = 1)
m <- mean(as.vector(data), na.rm = TRUE)
s <- sd(as.vector(data), na.rm = TRUE)
segments(m, 0.8, m, 1, lwd=2, col="red") # mean 
segments(m -1.96 * s, 0.9, m + 1.96 * s, 0.9, lwd=1, col="red") # mean 
graph.xlim <- par()$usr[1:2] # for hist() and qqnorm() below
if(interval.scale.disp == TRUE){
fun.interval.scale.display(data.f = data)
if(corner.text == ""){
corner.text <- paste0("MULTIPLYING FACTOR DISPLAYED (MEAN +/- SD) ON SCALES: ", paste(formatC(round(qnorm(quantiles.selection), 2))[-(1:(length(quantiles.selection) - 1) / 2)], collapse = ", "), "\nQUANTILES DISPLAYED ON SCALES: ", paste(quantiles.selection, collapse = ", "))
}else{
corner.text <- paste0(corner.text, "\nMULTIPLYING FACTOR DISPLAYED (MEAN +/- SD) ON SCALES: ", paste(formatC(round(qnorm(quantiles.selection), 2))[-(1:(length(quantiles.selection) - 1) / 2)], collapse = ", "), "\nQUANTILES DISPLAYED ON SCALES: ", paste(quantiles.selection, collapse = ", "))
}
}
output.tempo <- fun.add.cut(data.f = data, return.f = TRUE) # to recover real.trim.cutoffs
if(trim.return == TRUE){
output <- output.tempo
}
par(xpd = NA)
if(trim.method != ""){
if(corner.text == ""){
corner.text <- paste0("SELECTED CUT-OFFS (PROPORTION): ", paste(trim.cutoffs, collapse = ", "), "\nSELECTED CUT-OFFS: ", paste(output.tempo$real.trim.cutoffs, collapse = ", "))
}else{
corner.text <- paste0(corner.text, "\nSELECTED CUT-OFFS (PROPORTION): ", paste(trim.cutoffs, collapse = ", "), "\nSELECTED CUT-OFFS: ", paste(output.tempo$real.trim.cutoffs, collapse = ", "))
}
if(interval.scale.disp == TRUE){
legend(x = (par("usr")[1] - ((par("usr")[2] - par("usr")[1]) / (par("plt")[2] - par("plt")[1])) * par("plt")[1] - ((par("usr")[2] - par("usr")[1]) / (par("omd")[2] - par("omd")[1])) * par("omd")[1]), y = (par("usr")[4] + ((par("usr")[4] - par("usr")[3]) / (par("plt")[4] - par("plt")[3])) * (1 - par("plt")[4]) + ((par("usr")[4] - par("usr")[3]) / (par("omd")[4] - par("omd")[3])) * (1 - par("omd")[4]) / 2), legend = c(c("min, Q1, Median, Q3, max"), "mean +/- 1.96sd", paste0("Trimming interval: ", paste0(trim.cutoffs, collapse = " , ")), "Mean +/- sd multiplying factor", "Quantile"), yjust = 0, lty=1, col=c(col.box, "red", color.cut, col.mean, col.quantile), bty="n", cex = amplif.legend)
}else{
legend(x = (par("usr")[1] - ((par("usr")[2] - par("usr")[1]) / (par("plt")[2] - par("plt")[1])) * par("plt")[1] - ((par("usr")[2] - par("usr")[1]) / (par("omd")[2] - par("omd")[1])) * par("omd")[1]), y = (par("usr")[4] + ((par("usr")[4] - par("usr")[3]) / (par("plt")[4] - par("plt")[3])) * (1 - par("plt")[4]) + ((par("usr")[4] - par("usr")[3]) / (par("omd")[4] - par("omd")[3])) * (1 - par("omd")[4]) / 2), legend = c(c("min, Q1, Median, Q3, max"), "mean +/- 1.96sd", paste0("Trimming interval: ", paste0(trim.cutoffs, collapse = " , "))), yjust = 0, lty=1, col=c(col.box, "red", color.cut), bty="n", cex = amplif.legend, y.intersp=1.25)
}
}else{
if(interval.scale.disp == TRUE){
legend(x = (par("usr")[1] - ((par("usr")[2] - par("usr")[1]) / (par("plt")[2] - par("plt")[1])) * par("plt")[1] - ((par("usr")[2] - par("usr")[1]) / (par("omd")[2] - par("omd")[1])) * par("omd")[1]), y = (par("usr")[4] + ((par("usr")[4] - par("usr")[3]) / (par("plt")[4] - par("plt")[3])) * (1 - par("plt")[4]) + ((par("usr")[4] - par("usr")[3]) / (par("omd")[4] - par("omd")[3])) * (1 - par("omd")[4]) / 2), legend = c(c("min, Q1, Median, Q3, max"), "mean +/- sd", "Mean +/- sd multiplying factor", "Quantile"), yjust = 0, lty=1, col=c(col.box, "red", col.mean, col.quantile), bty="n", cex = amplif.legend)
}else{
legend(x = (par("usr")[1] - ((par("usr")[2] - par("usr")[1]) / (par("plt")[2] - par("plt")[1])) * par("plt")[1] - ((par("usr")[2] - par("usr")[1]) / (par("omd")[2] - par("omd")[1])) * par("omd")[1]), y = (par("usr")[4] + ((par("usr")[4] - par("usr")[3]) / (par("plt")[4] - par("plt")[3])) * (1 - par("plt")[4]) + ((par("usr")[4] - par("usr")[3]) / (par("omd")[4] - par("omd")[3])) * (1 - par("omd")[4]) / 2), legend = c(c("min, Q1, Median, Q3, max"), "mean +/- sd"), yjust = 0, lty=1, col=c(col.box, "red"), bty="n", cex = amplif.legend, y.intersp=1.25)
}
}
par(xpd = FALSE, xaxs = ifelse(std.x.range, "i", "r"), yaxs = ifelse(std.y.range, "i", "r"))
hist(as.vector(data), main = "", breaks = seq(min(as.vector(data), na.rm = TRUE), max(as.vector(data), na.rm = TRUE), length.out = length(as.vector(data)) / 10), xlim = graph.xlim, xlab = "Value", ylab="Density", col = grey(0.25))
abline(h = par()$usr[3])
fun.rug()
if(interval.scale.disp == TRUE){
fun.interval.scale.display(data.f = data)
}
fun.add.cut(data.f = data)
par(xaxs = ifelse(std.x.range, "i", "r"))
stripchart(rank(sampled.data), method="stack", vertical=FALSE, ylim=c(0.99, 1.3), group.names = "", xlab = "Rank of values", ylab="", pch=1, cex = cex.pt / 0.2)
fun.rug(y.nb.inter.tick.f = 0)
x.text <- par("usr")[2] + (par("usr")[2] - par("usr")[1]) / (par("plt")[2] - par("plt")[1]) * (1 - par("plt")[2]) / 2
y.text <- (par("usr")[4] + ((par("usr")[4] - par("usr")[3]) / (par("plt")[4] - par("plt")[3])) * (1 - par("plt")[4]) + ((par("usr")[4] - par("usr")[3]) / ((par()$omd[4] / 2) * ((par("plt")[4] - par("plt")[3])))) * (1 - par("omd")[4])) # BEWARE. Here in "(par()$omd[4] / 2", division by two because there are 2 graphs staked on the y axis, and not one
par(xpd=NA)
text(x = x.text, y = y.text, paste0(corner.text), adj=c(1, 1.1), cex = corner.text.size) # text at the topright corner
par(xpd=FALSE)
par(xaxs = ifelse(std.x.range, "i", "r"), yaxs = ifelse(std.y.range, "i", "r"))
qqnorm(as.vector(sampled.data), main = "", datax = TRUE, ylab = "Value", pch = 1, col = "red", cex = cex.pt / 0.2)
fun.rug()
if(diff(quantile(as.vector(data), probs = c(0.25, 0.75), na.rm = TRUE)) != 0){ # otherwise, error generated
qqline(as.vector(data), datax = TRUE)
}
if(interval.scale.disp == TRUE){
fun.interval.scale.display(data.f = data)
}
fun.add.cut(data.f = data)
}
if(trim.return == TRUE){
return(output)
}
}


######## fun_segmentation() #### segment a dot cloud on a scatterplot and define the dots from another cloud outside the segmentation


# Check OK: clear to go Apollo
fun_segmentation <- function(data1, x1, y1, x.range.split = NULL, x.step.factor = 10, y.range.split = NULL, y.step.factor = 10, error = 0, data2 = NULL, x2, y2, data2.pb.dot = "unknown", xy.cross.kind = "&", plot = FALSE, graph.in.file = FALSE, raster = TRUE, warn.print = FALSE, lib.path = NULL){
# AIM
# if data1 is a data frame corresponding to the data set of a scatterplot (with a x column for x-axis values and a y column for the y-axis column), then fun_segmentation() delimits a frame around the dots cloud using a sliding window set by x.range.split and x.step.factor to frame the top and bottom part of the cloud, and set by y.range.split and y.step.factor to frame the left and right part of the cloud
# if a second data frame is provided, corresponding to the data set of a scatterplot (with a x column for x-axis values and a y column for the y-axis column), then fun_segmentation() defines the dots of this data frame, outside of the frame of the first data frame
# WARNINGS
# if dots from data2 look significant on the graph (outside the frame) but are not (not black on the last figure), this is probably because the frame is flat on the zero coordinate (no volume inside the frame at this position). Thus, no way to conclude that data2 dots here are significant. These dots are refered to as "unknown". The pb.dot argument deals with such dots
# dots that are sometimes inside and outside the frame, depending on the sliding window, are treated differently: they are removed. Such dots are neither classified as "signif", "non signif" or "unknown", but as "inconsistent"
# unknown dots are treated as finally significant, not significant, or unknown (data2.pb.dot argument) for each x-axis and y-axis separately. Then, the union or intersection of significant dots is performed (argument xy.cross.kind). See the example section
# ARGUMENTS
# data1: a dataframe containing a column of x-axis values and a column of y-axis values
# x1: character string of the data1 column name for x-axis (first column of data1 by default)
# y1: character string of the data1 column name for y-axis (second column of data1 by default)
# x.range.split: positive non null numeric value giving the number of interval on the x value range. if x.range is the range of the dots on the x-axis, then abs(diff(x.range) / x.range.split) gives the window size. Window size decreases when range.split increases. In unit of x-axis. Write NULL if not required. At least one of the x.range.split and y.range.split must be non NULL
# x.step.factor: positive non null numeric value giving the shift step of the window. If x.step.factor = 1, no overlap during the sliding (when the window slides from position n to position n+1, no overlap between the two positions). If x.step.factor = 2, 50% of overlap (when the window slides from position n to position n+1, the window on position n+1 overlap 50% of the window when it was on position n)
# y.range.split: same as x.range.split for the y-axis. At least one of the x.range.split and y.range.split must be non NULL
# y.step.factor: same as x.step.factor for the y-axis
# error: proportion (from 0 to 1) of false positives (i.e., proportion of dots from data1 outside of the frame). 0.05 means 5% of the dots from data1 outside of the frame
# data2: a dataframe containing a column of x-axis values and a column of y-axis values, for which outside dots of the data1 cloud has to be determined. Write NULL if not required
# x2: character string of the data1 column name for x-axis (first column of data1 by default)
# y2: character string of the data1 column name for y-axis (second column of data1 by default)
# data2.pb.dot: unknown dots are explain in the warning section above. If "signif", then the unknown dots are finally considered as significant (outside the frame). If "not.signif", then the unknown dots are finally considered as non significant (inside the frame). If "unknown", no conclusion are drawn from these dots. See the examples below
# xy.cross.kind: if data2 is non null and if both x.range.split and y.range.split are non null, which dots are finally significants? Write "&" for intersection of outside dots on x and on y. Write "|" for union of outside dots on x and on y. See the examples below
# plot: logical. Print graphs that check the frame?
# graph.in.file: logical. Graphs sent into a graphic device already opened? If FALSE, GUI are opened for each graph. If TRUE, no GUI are opended. The graphs are displayed on the current active graphic device. Ignored if plot is FALSE
# raster: logical. Dots in raster mode? If FALSE, dots from each geom_point from geom argument are in vectorial mode (bigger pdf and long to display if millions of dots). If TRUE, dots from each geom_point from geom argument are in matricial mode (smaller pdf and easy display if millions of dots, but long to generate the layer). If TRUE, the region plot will be square to avoid a bug in fun_gg_point_rast(). If TRUE, solve the transparency problem with some GUI. Not considered if plot is FALSE
# warn.print: logical. Print warnings at the end of the execution? No print if no warning messages
# lib.path: absolute path of the required packages, if not in the default folders. Ignored if plot is FALSE
# REQUIRED PACKAGES
# ggplot2 if plot is TRUE
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# fun_check()
# if plot is TRUE:
# fun_pack()
# fun_open()
# fun_gg_palette()
# fun_gg_scatter()
# fun_gg_empty_graph()
# fun_close()
# RETURN
# several graphs if plot is TRUE
# a list containing:
# $data1.removed.row.nb: which rows have been removed due to NA; NaN, -Inf or Inf detection in x1 or y1 columns (NULL if no row removed)
# $data1.removed.rows: removed rows (NULL if no row removed)
# $data2.removed.row.nb: which rows have been removed due to NA; NaN, -Inf or Inf detection in x2 or y2 columns (NULL if no row removed)
# $data2.removed.rows: removed rows (NULL if no row removed)
# $hframe: x and y coordinates of the bottom and top frames for frame plotting (frame1 for the left step and frame2 for the right step)
# $vframe: x and y coordinates of the left and right frames for frame plotting (frame1 for the down step and frame2 for the top step)
# $data1.signif.dot: the significant dots of data1 (i.e., dots outside the frame). A good segmentation should not have any data1.signif.dot
# $data1.non.signif.dot: the non significant dots of data1 (i.e., dots inside the frame)
# $data1.inconsistent.dot: see the warning section above
# $data2.signif.dot: the significant dots of data2 if non NULL (i.e., dots outside the frame)
# $data2.non.signif.dot: the non significant dots of data2 (i.e., dots inside the frame)
# $data2.unknown.dot: the problematic dots of data2 (i.e., data2 dots outside of the range of data1, or data2 dots in a sliding window without data1 dots). Is systematically NULL except if argument data2.pb.dot = "unknown" and some data2 dots are in such situation. Modifying the segmentation x.range.split, x.step.factor, y.range.split, y.step.factor arguments can solve this problem
# $data2.inconsistent.dot: see the warning section above
# $axes: the x-axis and y-axis info
# $warn: the warning messages. Use cat() for proper display. NULL if no warning
# EXAMPLES
# example explaining the unknown and inconsistent dots, and the cross 

# set.seed(1) ; data1 = data.frame(x = rnorm(500), y = rnorm(500)) ; data1[5:7, 2] <- NA ; data2 = data.frame(x = rnorm(500, 0, 2), y = rnorm(500, 0, 2)) ; data2[11:13, 1] <- Inf ; set.seed(NULL) ; fun_segmentation(data1 = data1, x1 = names(data1)[1], y1 = names(data1)[2], x.range.split = 20, x.step.factor = 10, y.range.split = 23, y.step.factor = 10, error = 0, data2 = data2, x2 = names(data2)[1], y2 = names(data2)[2], data2.pb.dot = "not.signif", xy.cross.kind = "|", plot = TRUE, graph.in.file = FALSE, raster = FALSE, lib.path = NULL)
# set.seed(1) ; data1 = data.frame(x = rnorm(500), y = rnorm(500)) ; data2 = data.frame(x = rnorm(500, 0, 2), y = rnorm(500, 0, 2)) ; set.seed(NULL) ; fun_segmentation(data1 = data1, x1 = names(data1)[1], y1 = names(data1)[2], x.range.split = NULL, x.step.factor = 10, y.range.split = 23, y.step.factor = 10, error = 0, data2 = data2, x2 = names(data2)[1], y2 = names(data2)[2], data2.pb.dot = "unknown", xy.cross.kind = "|", plot = TRUE, graph.in.file = FALSE, raster = FALSE, lib.path = NULL)
# set.seed(1) ; data1 = data.frame(x = rnorm(500), y = rnorm(500)) ; data2 = data.frame(x = rnorm(500, 0, 2), y = rnorm(500, 0, 2)) ; set.seed(NULL) ; fun_segmentation(data1 = data1, x1 = names(data1)[1], y1 = names(data1)[2], x.range.split = 20, x.step.factor = 10, y.range.split = NULL, y.step.factor = 10, error = 0, data2 = data2, x2 = names(data2)[1], y2 = names(data2)[2], data2.pb.dot = "unknown", xy.cross.kind = "&", plot = TRUE, graph.in.file = FALSE, raster = FALSE, lib.path = NULL)
# DEBUGGING
# set.seed(1) ; data1 = data.frame(x = rnorm(50), y = rnorm(50)) ; data1[5:7, 2] <- NA ; x1 = names(data1)[1] ; y1 = names(data1)[2] ; x.range.split = 5 ; x.step.factor = 10 ; y.range.split = 5 ; y.step.factor = 10 ; error = 0 ; data2 = data.frame(x = rnorm(50, 0, 2), y = rnorm(50, 0, 2)) ; set.seed(NULL) ; x2 = names(data2)[1] ; y2 = names(data2)[2] ; data2.pb.dot = "unknown" ; xy.cross.kind = "|" ; plot = TRUE ; graph.in.file = FALSE ; raster = FALSE ; warn.print = TRUE ; lib.path = NULL
# set.seed(1) ; data1 = data.frame(x = rnorm(500), y = rnorm(500)) ; data2 = data.frame(x = rnorm(500, 0, 2), y = rnorm(500, 0, 2)) ; set.seed(NULL) ; x1 = names(data1)[1] ; y1 = names(data1)[2] ; x.range.split = 20 ; x.step.factor = 10 ; y.range.split = 23 ; y.step.factor = 10 ; error = 0 ; x2 = names(data2)[1] ; y2 = names(data2)[2] ; data2.pb.dot = "not.signif" ; xy.cross.kind = "|" ; plot = TRUE ; graph.in.file = FALSE ; raster = FALSE ; warn.print = TRUE ; lib.path = NULL
# set.seed(1) ; data1 = data.frame(x = rnorm(500), y = rnorm(500)) ; data2 = data.frame(x = rnorm(500, 0, 2), y = rnorm(500, 0, 2)) ; set.seed(NULL) ; x1 = names(data1)[1] ; y1 = names(data1)[2] ; x.range.split = 20 ; x.step.factor = 10 ; y.range.split = NULL ; y.step.factor = 10 ; error = 0 ; x2 = names(data2)[1] ; y2 = names(data2)[2] ; data2.pb.dot = "unknown" ; xy.cross.kind = "&" ; plot = TRUE ; graph.in.file = FALSE ; raster = FALSE ; warn.print = TRUE ; lib.path = NULL
# function name
function.name <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
# end function name
# required function checking
if(length(utils::find("fun_check", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_check() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end required function checking
# argument checking
warn <- NULL
warn.count <- 0
arg.check <- NULL #
text.check <- NULL #
checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$fun.name))
tempo <- fun_check(data = data1, class = "data.frame", na.contain = TRUE, fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE & length(data1) < 2){
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": data1 ARGUMENT MUST BE A DATA FRAME OF AT LEAST 2 COLUMNS\n\n============\n\n")
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
tempo <- fun_check(data = x1, class = "vector", mode = "character", length = 1, fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE & ! (x1 %in% names(data1))){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": x1 ARGUMENT MUST BE A COLUMN NAME OF data1\n\n================\n\n")
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}else if(tempo$problem == FALSE & x1 %in% names(data1)){
tempo <- fun_check(data = data1[, x1], data.name = "x1 COLUMN OF data1", class = "vector", mode = "numeric", na.contain = TRUE, fun.name = function.name) ; eval(ee)
}
tempo <- fun_check(data = y1, class = "vector", mode = "character", length = 1, fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE & ! (y1 %in% names(data1))){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": y1 ARGUMENT MUST BE A COLUMN NAME OF data1\n\n================\n\n")
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}else if(tempo$problem == FALSE & y1 %in% names(data1)){
tempo <- fun_check(data = data1[, y1], data.name = "y1 COLUMN OF data1", class = "vector", mode = "numeric", na.contain = TRUE, fun.name = function.name) ; eval(ee)
}
if(is.null(x.range.split) & is.null(y.range.split)){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": AT LEAST ONE OF THE x.range.split AND y.range.split ARGUMENTS MUST BE NON NULL\n\n================\n\n")
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
if( ! is.null(x.range.split)){
tempo <- fun_check(data = x.range.split, class = "vector", mode = "numeric", length = 1, fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE & x.range.split < 1){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": x.range.split ARGUMENT CANNOT BE LOWER THAN 1\n\n================\n\n")
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
}
if( ! is.null(y.range.split)){
tempo <- fun_check(data = y.range.split, class = "vector", mode = "numeric", length = 1, fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE & y.range.split < 1){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": y.range.split ARGUMENT CANNOT BE LOWER THAN 1\n\n================\n\n")
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
}
tempo <- fun_check(data = x.step.factor, class = "vector", mode = "numeric", length = 1, fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE & x.step.factor < 1){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": x.step.factor ARGUMENT CANNOT BE LOWER THAN 1\n\n================\n\n")
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
tempo <- fun_check(data = y.step.factor, class = "vector", mode = "numeric", length = 1, fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE & y.step.factor < 1){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": y.step.factor ARGUMENT CANNOT BE LOWER THAN 1\n\n================\n\n")
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
tempo <- fun_check(data = error, prop = TRUE, length = 1, fun.name = function.name) ; eval(ee)
if( ! is.null(data2)){
if(is.null(x2) | is.null(y2)){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": x2 AND y2 ARGUMENTS CANNOT BE NULL IF data2 ARGUMENT IS NON NULL\n\n================\n\n")
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
tempo <- fun_check(data = data2, class = "data.frame", na.contain = TRUE, fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE & length(data2) < 2){
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": data2 ARGUMENT MUST BE A DATA FRAME OF AT LEAST 2 COLUMNS\n\n============\n\n")
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
if( ! is.null(x2)){
tempo <- fun_check(data = x2, class = "vector", mode = "character", length = 1, fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE & ! (x2 %in% names(data2))){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": x2 ARGUMENT MUST BE A COLUMN NAME OF data2\n\n================\n\n")
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}else if(tempo$problem == FALSE & x2 %in% names(data2)){
tempo <- fun_check(data = data2[, x2], data.name = "x2 COLUMN OF data2", class = "vector", mode = "numeric", na.contain = TRUE, fun.name = function.name) ; eval(ee)
}
}
if( ! is.null(y2)){
tempo <- fun_check(data = y2, class = "vector", mode = "character", length = 1, fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE & ! (y2 %in% names(data2))){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": y2 ARGUMENT MUST BE A COLUMN NAME OF data2\n\n================\n\n")
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}else if(tempo$problem == FALSE & y2 %in% names(data2)){
tempo <- fun_check(data = data2[, y2], data.name = "y2 COLUMN OF data2", class = "vector", mode = "numeric", na.contain = TRUE, fun.name = function.name) ; eval(ee)
}
}
}
if( ! is.null(data2)){
tempo <- fun_check(data = data2.pb.dot, options = c("signif", "not.signif", "unknown"), length = 1, fun.name = function.name) ; eval(ee)
}
if( ! (is.null(x.range.split)) & ! (is.null(y.range.split))){
tempo <- fun_check(data = xy.cross.kind, options = c("&", "|"), length = 1, fun.name = function.name) ; eval(ee)
}
tempo <- fun_check(data = plot, class = "vector", mode = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = warn.print, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE & plot == TRUE){
tempo <- fun_check(data = raster, class = "vector", mode = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = graph.in.file, class = "vector", mode = "logical", length = 1, fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE & graph.in.file == TRUE & is.null(dev.list())){
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": \ngraph.in.file PARAMETER SET TO TRUE BUT NO ACTIVE GRAPHIC DEVICE DETECTED\n\n============\n\n")
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}else if(tempo$problem == FALSE & graph.in.file == TRUE & ! is.null(dev.list())){
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") FROM FUNCTION ", function.name, ": GRAPHS PRINTED IN THE CURRENT DEVICE (TYPE ", toupper(names(dev.cur())), ")")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}
if( ! is.null(lib.path)){
tempo <- fun_check(data = lib.path, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE){
if( ! all(dir.exists(lib.path))){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
tempo.cat <- paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", paste(lib.path, collapse = "\n"))
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
}
}
}
if(any(arg.check) == TRUE){
stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
}
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
# end argument checking
# other required function checking
if(plot == TRUE){
if(length(utils::find("fun_pack", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_pack() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if(length(utils::find("fun_open", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_open() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if(length(utils::find("fun_gg_palette", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_gg_palette() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if(length(utils::find("fun_gg_empty_graph", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_gg_empty_graph() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if(length(utils::find("fun_gg_scatter", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_gg_scatter() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if(length(utils::find("fun_close", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_close() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
}
# end other required function checking
# package checking
if(plot == TRUE){
fun_pack(req.package = c("ggplot2"), lib.path = lib.path)
}
# end package checking
# main code
# na and Inf detection and removal (done now to be sure of the correct length of categ)
data1.removed.row.nb <- NULL
data1.removed.rows <- NULL
data2.removed.row.nb <- NULL
data2.removed.rows <- NULL
if(any(is.na(data1[, c(x1, y1)])) | any(is.infinite(data1[, x1])) | any(is.infinite(data1[, y1]))){
tempo.na <- unlist(lapply(lapply(c(data1[c(x1, y1)]), FUN = is.na), FUN = which))
tempo.inf <- unlist(lapply(lapply(c(data1[c(x1, y1)]), FUN = is.infinite), FUN = which))
data1.removed.row.nb <- sort(unique(c(tempo.na, tempo.inf)))
if(length(data1.removed.row.nb) > 0){
data1.removed.rows <- data1[data1.removed.row.nb, ]
}
if(length(data1.removed.row.nb) == nrow(data1)){
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": AT LEAST ONE NA, NaN, -Inf OR Inf DETECTED IN EACH ROW OF data1. FUNCTION CANNOT BE USED ON EMPTY DATA FRAME\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
if(length(data1.removed.row.nb) > 0){
data1 <- data1[-data1.removed.row.nb, ]
}
if(nrow(data1) == 0){
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": CODE INCONSISTENCY 1\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") FROM FUNCTION ", function.name, ": NA, NaN, -Inf OR Inf DETECTED IN COLUMN ", paste(c(x1, y1), collapse = " "), " OF data1 AND CORRESPONDING ROWS REMOVED (SEE $data1.removed.row.nb AND $data1.removed.rows)")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}else{
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") FROM FUNCTION ", function.name, ": NO NA, NaN, -Inf OR Inf DETECTED IN COLUMN ", paste(c(x1, y1), collapse = " "), " OF data1. NO ROW REMOVED")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}
if( ! is.null(data2)){
if(any(is.na(data2[, c(x2, y2)])) | any(is.infinite(data2[, x2])) | any(is.infinite(data2[, y2]))){
tempo.na <- unlist(lapply(lapply(c(data2[c(x2, y2)]), FUN = is.na), FUN = which))
tempo.inf <- unlist(lapply(lapply(c(data2[c(x2, y2)]), FUN = is.infinite), FUN = which))
data2.removed.row.nb <- sort(unique(c(tempo.na, tempo.inf)))
if(length(data2.removed.row.nb) > 0){
data2.removed.rows <- data2[data2.removed.row.nb, ]
}
if(length(data2.removed.row.nb) == nrow(data2)){
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": AT LEAST ONE NA, NaN, -Inf OR Inf DETECTED IN EACH ROW OF data2. FUNCTION CANNOT BE USED ON EMPTY DATA FRAME\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
if(length(data2.removed.row.nb) > 0){
data2 <- data2[-data2.removed.row.nb, ]
}
if(nrow(data2) == 0){
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": CODE INCONSISTENCY 2\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") FROM FUNCTION ", function.name, ": NA, NaN, -Inf OR Inf DETECTED IN COLUMN ", paste(c(x2, y2), collapse = " "), " OF data2 AND CORRESPONDING ROWS REMOVED (SEE $data2.removed.row.nb AND $data2.removed.rows)")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}else{
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") FROM FUNCTION ", function.name, ": NO NA, NaN, -Inf OR Inf DETECTED IN COLUMN ", paste(c(x2, y2), collapse = " "), " OF data2. NO ROW REMOVED")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
print(warn)
}
}
# end na and Inf detection and removal (done now to be sure of the correct length of categ)
# row annotation (dot number)
# data1 <- data1[ ! duplicated(data1[, c(x1, y1)]), ] # do not remove the dots that have same x and y values, because they will have different dot number -> not the same position on the matrices (so true for symmetric matrices)
data1 <- cbind(data1, DOT_NB = 1:nrow(data1))
if( ! is.null(data2)){
# data2 <- data2[ ! duplicated(data2[, c(x2, y2)]), ] # do not remove the dots that have same x and y values, because they will have different dot number -> not the same position on the matrices (so true for symmetric matrices)
data2 <- cbind(data2, DOT_NB = 1:nrow(data2))
}
# end row annotation (dot number)




# Method using x unit interval 
# may be create vector of each column to increase speed
x.data1.l <- NULL # x coord of the y upper and lower limits defined on the data1 cloud for left step line
x.data1.r <- NULL # x coord of the y upper and lower limits defined on the data1 cloud for right step line
y.data1.down.limit.l <- NULL # lower limit of the data1 cloud for left step line
y.data1.top.limit.l <- NULL # upper limit of the data1 cloud for left step line
y.data1.down.limit.r <- NULL # lower limit of the data1 cloud for right step line
y.data1.top.limit.r <- NULL # upper limit of the data1 cloud for left step line
if(any(data1[, x1] %in% c(Inf, -Inf))){
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") FROM FUNCTION ", function.name, ": THE data1 ARGUMENT CONTAINS -Inf OR Inf VALUES IN THE x1 COLUMN, THAT WILL NOT BE CONSIDERED IN THE PLOT RANGE")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}
x.range <- range(data1[, x1], na.rm = TRUE, finite = TRUE) # finite = TRUE removes all the -Inf and Inf except if only this. In that case, whatever the -Inf and/or Inf present, output -Inf;Inf range. Idem with NA only
if(suppressWarnings(any(x.range %in% c(Inf, -Inf)))){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, " COMPUTED x.range CONTAINS Inf VALUES, BECAUSE VALUES FROM data1 ARGUMENTS ARE NA OR Inf ONLY\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if(any(data1[, y1] %in% c(Inf, -Inf))){
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") FROM FUNCTION ", function.name, ": THE data1 ARGUMENT CONTAINS -Inf OR Inf VALUES IN THE y1 COLUMN, THAT WILL NOT BE CONSIDERED IN THE PLOT RANGE")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}
y.range <- range(data1[, y1], na.rm = TRUE, finite = TRUE) # finite = TRUE removes all the -Inf and Inf except if only this. In that case, whatever the -Inf and/or Inf present, output -Inf;Inf range. Idem with NA only
if(suppressWarnings(any(x.range %in% c(Inf, -Inf)))){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, " COMPUTED y.range CONTAINS Inf VALUES, BECAUSE VALUES FROM data1 ARGUMENTS ARE NA OR Inf ONLY\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
x.range.plot <- range(data1[, x1], na.rm = TRUE, finite = TRUE) # finite = TRUE removes all the -Inf and Inf except if only this. In that case, whatever the -Inf and/or Inf present, output -Inf;Inf range. Idem with NA only
y.range.plot <- range(data1[, y1], na.rm = TRUE, finite = TRUE) # finite = TRUE removes all the -Inf and Inf except if only this. In that case, whatever the -Inf and/or Inf present, output -Inf;Inf range. Idem with NA only
if( ! is.null(data2)){
if(any(data2[, x2] %in% c(Inf, -Inf))){
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") FROM FUNCTION ", function.name, ": THE data2 ARGUMENT CONTAINS -Inf OR Inf VALUES IN THE x2 COLUMN, THAT WILL NOT BE CONSIDERED IN THE PLOT RANGE")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}
x.range.plot <- range(data1[, x1], data2[, x2], na.rm = TRUE, finite = TRUE) # finite = TRUE removes all the -Inf and Inf except if only this. In that case, whatever the -Inf and/or Inf present, output -Inf;Inf range. Idem with NA only
if(any(data2[, y2] %in% c(Inf, -Inf))){
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") FROM FUNCTION ", function.name, ": THE data2 ARGUMENT CONTAINS -Inf OR Inf VALUES IN THE y2 COLUMN, THAT WILL NOT BE CONSIDERED IN THE PLOT RANGE")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}
y.range.plot <- range(data1[, y1], data2[, y2], na.rm = TRUE, finite = TRUE) # finite = TRUE removes all the -Inf and Inf except if only this. In that case, whatever the -Inf and/or Inf present, output -Inf;Inf range. Idem with NA only
}
if(suppressWarnings(any(x.range.plot %in% c(Inf, -Inf)))){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, " COMPUTED x.range.plot CONTAINS Inf VALUES, BECAUSE VALUES FROM data1 (AND data2?) ARGUMENTS ARE NA OR Inf ONLY\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if(suppressWarnings(any(y.range.plot %in% c(Inf, -Inf)))){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, " COMPUTED y.range.plot CONTAINS Inf VALUES, BECAUSE VALUES FROM data1 (AND data2?) ARGUMENTS ARE NA OR Inf ONLY\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if( ! is.null(x.range.split)){
# data.frame ordering to slide the window from small to big values + sliding window definition
data1 <- data1[order(data1[, x1], na.last = TRUE), ]
if( ! is.null(data2)){
data2 <- data2[order(data2[, x2], na.last = TRUE), ]
}
x.win.size <- abs(diff(x.range) / x.range.split) # in unit of x-axis
step <- x.win.size / x.step.factor
# end data.frame ordering to slide the window from small to big values + sliding window definition
# x-axis sliding and y-axis limits of the data1 cloud -> y significant data2
loop.nb <- ceiling((diff(x.range) - x.win.size) / step) # x.win.size + n * step covers the x range if x.win.size + n * step >= diff(x.range), thus if n >= (diff(x.range) - x.win.size) / step 
y.outside.data1.dot.nb <- integer() # vector that will contain the selected rows numbers of data1 that are upper or lower than the frame
y.inside.data1.dot.nb <- integer() # vector that will contain the selected rows numbers of data1 that are not upper or lower than the frame
y.data1.median <- median(data1[, y1], na.rm = TRUE) # will be used for sliding windows without data1 in it
if( ! is.null(data2)){
y.outside.data2.dot.nb <- integer() # vector that will contain the selected 1D coordinates (i.e., dots) of data2 that are upper or lower than the data1 frame
y.inside.data2.dot.nb <- integer() # vector that will contain the 1D coordinates (i.e., dots) of data2 that are not upper or lower than the data1 frame
y.unknown.data2.dot.nb <- integer() # vector that will contain the 1D coordinates (i.e., dots) of data2 that are problematic: data2 dots outside of the range of data1, or data2 dots in a sliding window without data1 dots
# recover data2 dots outside the range of data1
if(any(data2[, x2] < x.range[1])){
y.unknown.data2.dot.nb <- c(y.unknown.data2.dot.nb, data2$DOT_NB[data2[, x2] < x.range[1]])
#tempo.warn & indicate the interval
}
if(any(data2[, x2] > x.range[2])){
y.unknown.data2.dot.nb <- c(y.unknown.data2.dot.nb, data2$DOT_NB[data2[, x2] > x.range[2]])
#tempo.warn & indicate the interval
}
# end recover data2 dots outside the range of data1
}
# loop.ini.time <- as.numeric(Sys.time())
for(i1 in 0:(loop.nb + 1)){
min.pos <- x.range[1] + step * i1 # lower position of the sliding window in data1
max.pos <- min.pos + x.win.size # upper position of the sliding window in data1
x.data1.l <- c(x.data1.l, min.pos, min.pos + step) # min.pos + step to make the steps
x.data1.r <- c(x.data1.r, max.pos, max.pos + step) # max.pos + step to make the steps
x.data1.dot.here <- data1[, x1] >= min.pos & data1[, x1] < max.pos # is there data1 dot present in the sliding window, considering the x axis?
if( ! is.null(data2)){
x.data2.dot.here <- data2[, x2] >= min.pos & data2[, x2] < max.pos # is there data2 dot present in the sliding window, considering the x axis?
}
# recover the data1 dots outside the frame
if(any(x.data1.dot.here == TRUE)){
tempo.y.data1.top.limit <- quantile(data1[x.data1.dot.here, y1], probs = 1 - error, na.rm = TRUE)
tempo.y.data1.down.limit <- quantile(data1[x.data1.dot.here, y1], probs = 0 + error, na.rm = TRUE)
y.data1.top.limit.l <- c(y.data1.top.limit.l, tempo.y.data1.top.limit, tempo.y.data1.top.limit)
y.data1.down.limit.l <- c(y.data1.down.limit.l, tempo.y.data1.down.limit, tempo.y.data1.down.limit)
y.data1.top.limit.r <- c(y.data1.top.limit.r, tempo.y.data1.top.limit, tempo.y.data1.top.limit)
y.data1.down.limit.r <- c(y.data1.down.limit.r, tempo.y.data1.down.limit, tempo.y.data1.down.limit)
y.data1.dot.signif <- ( ! ((data1[, y1] <= tempo.y.data1.top.limit) & (data1[, y1] >= tempo.y.data1.down.limit))) & x.data1.dot.here # is there data1 dot present in the sliding window, above or below the data1 limits, considering the y axis?
y.data1.dot.not.signif <- x.data1.dot.here & ! y.data1.dot.signif
y.outside.data1.dot.nb <- c(y.outside.data1.dot.nb, data1$DOT_NB[y.data1.dot.signif]) # recover the row number of data1
y.outside.data1.dot.nb <- unique(y.outside.data1.dot.nb)
y.inside.data1.dot.nb <- c(y.inside.data1.dot.nb, data1$DOT_NB[y.data1.dot.not.signif])
y.inside.data1.dot.nb <- unique(y.inside.data1.dot.nb)
}else{
y.data1.top.limit.l <- c(y.data1.top.limit.l, y.data1.median, y.data1.median)
y.data1.down.limit.l <- c(y.data1.down.limit.l, y.data1.median, y.data1.median)
y.data1.top.limit.r <- c(y.data1.top.limit.r, y.data1.median, y.data1.median)
y.data1.down.limit.r <- c(y.data1.down.limit.r, y.data1.median, y.data1.median)
}
# end recover the data1 dots outside the frame
# recover the data2 dots outside the frame
if( ! is.null(data2)){
if(any(x.data1.dot.here == TRUE) & any(x.data2.dot.here == TRUE)){ 
y.data2.dot.signif <- ( ! ((data2[, y2] <= tempo.y.data1.top.limit) & (data2[, y2] >= tempo.y.data1.down.limit))) & x.data2.dot.here # is there data2 dot present in the sliding window, above or below the data1 limits, considering the y axis?
y.data2.dot.not.signif <- x.data2.dot.here & ! y.data2.dot.signif
y.outside.data2.dot.nb <- c(y.outside.data2.dot.nb, data2$DOT_NB[y.data2.dot.signif])
y.outside.data2.dot.nb <- unique(y.outside.data2.dot.nb)
y.inside.data2.dot.nb <- c(y.inside.data2.dot.nb, data2$DOT_NB[y.data2.dot.not.signif])
y.inside.data2.dot.nb <- unique(y.inside.data2.dot.nb)
}else if(any(x.data1.dot.here == FALSE) & any(x.data2.dot.here == TRUE)){ # problem: data2 dots in the the window but no data1 dots to generates the quantiles
y.unknown.data2.dot.nb <- c(y.unknown.data2.dot.nb, data2$DOT_NB[x.data2.dot.here])
y.unknown.data2.dot.nb <- unique(y.unknown.data2.dot.nb)
#tempo.warn & indicate the interval




# tempo.warn <- paste0("FROM FUNCTION ", function.name, ": THE [", round(min.pos, 3), " ; ", round(max.pos, 3), "] INTERVAL DOES NOT CONTAIN data1 X VALUES BUT CONTAINS data2 X VALUES WHICH CANNOT BE EVALUATED.\nTHE CONCERNED data2 ROW NUMBERS ARE:\n", paste(which(x.data1.dot.here == FALSE & x.data2.dot.here == TRUE), collapse = "\n"))
# warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}
}
# end recover the data2 dots outside the frame
# if(any(i1 == seq(1, loop.nb, 500))){
# loop.fin.time <- as.numeric(Sys.time()) # time of process end
# cat(paste0("COMPUTATION TIME OF LOOP ", i1, " / ", loop.nb, ": ", as.character(lubridate::seconds_to_period(round(loop.fin.time - loop.ini.time))), "\n"))
# }
}
if(max.pos < x.range[2]){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": THE SLIDING WINDOW HAS NOT REACHED THE MAX VALUE OF data1 ON THE X-AXIS: ", max.pos, " VERSUS ", x.range[2], "\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
y.incon.data1.dot.nb.final <- unique(c(y.outside.data1.dot.nb[y.outside.data1.dot.nb %in% y.inside.data1.dot.nb], y.inside.data1.dot.nb[y.inside.data1.dot.nb %in% y.outside.data1.dot.nb])) # inconsistent dots: if a row number of y.inside.data1.dot.nb is present in y.outside.data1.dot.nb (and vice versa), it means that during the sliding, a dot has been sometime inside, sometime outside -> removed from the outside list
y.outside.data1.dot.nb.final <- y.outside.data1.dot.nb[ ! (y.outside.data1.dot.nb %in% y.incon.data1.dot.nb.final)] # inconsistent dots removed from the outside list
y.inside.data1.dot.nb.final <- y.inside.data1.dot.nb[ ! (y.inside.data1.dot.nb %in% y.incon.data1.dot.nb.final)] # inconsistent dots removed from the inside list
if( ! is.null(data2)){
# if some unknown dots are also inside, and/or outside, they are put in the inside and/or outside. Ok, because then the intersection between inside and outside is treated -> inconsistent dots
tempo.unknown.out <- y.unknown.data2.dot.nb[y.unknown.data2.dot.nb %in% y.outside.data2.dot.nb]
y.outside.data2.dot.nb <- unique(c(y.outside.data2.dot.nb, tempo.unknown.out)) # if a row number of y.unknown.data2.dot.nb is present in y.outside.data2.dot.nb, it is put into outside
tempo.unknown.in <- y.unknown.data2.dot.nb[y.unknown.data2.dot.nb %in% y.inside.data2.dot.nb]
y.inside.data2.dot.nb <- unique(c(y.inside.data2.dot.nb, tempo.unknown.in)) # if a row number of y.unknown.data2.dot.nb is present in y.inside.data2.dot.nb, it is put into inside
y.unknown.data2.dot.nb.final <- y.unknown.data2.dot.nb[ ! (y.unknown.data2.dot.nb %in% c(y.outside.data2.dot.nb, y.inside.data2.dot.nb))] # then dots also in inside and outside are remove from unknown
y.incon.data2.dot.nb.final <- unique(c(y.outside.data2.dot.nb[y.outside.data2.dot.nb %in% y.inside.data2.dot.nb], y.inside.data2.dot.nb[y.inside.data2.dot.nb %in% y.outside.data2.dot.nb])) # inconsistent dots: if a row number of y.inside.data2.dot.nb is present in y.outside.data2.dot.nb (and vice versa), it means that during the sliding, a dot has been sometime inside, sometime outside -> removed from the outside list
y.outside.data2.dot.nb.final <- y.outside.data2.dot.nb[ ! (y.outside.data2.dot.nb %in% y.incon.data2.dot.nb.final)] # inconsistent dots removed from the outside list
y.inside.data2.dot.nb.final <- y.inside.data2.dot.nb[ ! (y.inside.data2.dot.nb %in% y.incon.data2.dot.nb.final)] # inconsistent dots removed from the inside list
}
# end x-axis sliding and y-axis limits of the data1 cloud -> y significant data2
}
# end Method using x unit interval 




# Method using y unit interval 
y.data1.d <- NULL # y coord of the x upper and lower limits defined on the data1 cloud for down step line
y.data1.t <- NULL # y coord of the x upper and lower limits defined on the data1 cloud for top step line
x.data1.left.limit.d <- NULL # left limit of the data1 cloud for down step line
x.data1.right.limit.d <- NULL # right limit of the data1 cloud for down step line
x.data1.left.limit.t <- NULL # left limit of the data1 cloud for top step line
x.data1.right.limit.t <- NULL # right limit of the data1 cloud for top step line
if( ! is.null(y.range.split)){
# data.frame ordering to slide the window from small to big values + sliding window definition
data1 <- data1[order(data1[, y1], na.last = TRUE), ]
if( ! is.null(data2)){
data2 <- data2[order(data2[, y2], na.last = TRUE), ]
}
y.win.size <- abs(diff(y.range) / y.range.split) # in unit of y-axis
step <- y.win.size / y.step.factor
# end data.frame ordering to slide the window from small to big values + sliding window definition
# y-axis sliding and x-axis limits of the data1 cloud -> x significant data2
loop.nb <- ceiling((diff(y.range) - y.win.size) / step) # y.win.size + n * step covers the y range if y.win.size + n * step >= diff(y.range), thus if n >= (diff(y.range) - y.win.size) / step 
x.outside.data1.dot.nb <- integer() # vector that will contain the selected rows numbers of data1 that are upper or lower than the frame
x.inside.data1.dot.nb <- integer() # vector that will contain the selected rows numbers of data1 that are not upper or lower than the frame
x.data1.median <- median(data1[, x1], na.rm = TRUE) # will be used for sliding window without data1 in it
if( ! is.null(data2)){
x.outside.data2.dot.nb <- integer() # vector that will contain the selected 1D coordinates (i.e., dots) of data2 that are upper or lower than the data1 frame
x.inside.data2.dot.nb <- integer() # vector that will contain the 1D coordinates (i.e., dots) of data2 that are not upper or lower than the data1 frame
x.unknown.data2.dot.nb <- integer() # vector that will contain the 1D coordinates (i.e., dots) of data2 that are problematic: data2 dots outside of the range of data1, or data2 dots in a sliding window without data1 dots
# recover data2 dots outside the range of data1
if(any(data2[, y2] < y.range[1])){
x.unknown.data2.dot.nb <- c(x.unknown.data2.dot.nb, data2$DOT_NB[data2[, y2] < y.range[1]])
}
if(any(data2[, y2] > y.range[2])){
x.unknown.data2.dot.nb <- c(x.unknown.data2.dot.nb, data2$DOT_NB[data2[, y2] > y.range[2]])
}
# end recover data2 dots outside the range of data1
}
# loop.ini.time <- as.numeric(Sys.time())
for(i1 in 0:(loop.nb + 1)){
min.pos <- y.range[1] + step * i1 # lower position of the sliding window in data1
max.pos <- min.pos + y.win.size # upper position of the sliding window in data1
y.data1.d <- c(y.data1.d, min.pos, min.pos + step) # min.pos + step to make the steps
y.data1.t <- c(y.data1.t, max.pos, max.pos + step) # max.pos + step to make the steps
y.data1.dot.here <- data1[, y1] >= min.pos & data1[, y1] < max.pos # is there data1 dot present in the sliding window, considering the y axis?
if( ! is.null(data2)){
y.data2.dot.here <- data2[, y2] >= min.pos & data2[, y2] < max.pos # is there data2 dot present in the sliding window, considering the y axis?
}
# recover the data1 dots outside the frame
if(any(y.data1.dot.here == TRUE)){
tempo.x.data1.right.limit <- quantile(data1[y.data1.dot.here, x1], probs = 1 - error, na.rm = TRUE)
tempo.x.data1.left.limit <- quantile(data1[y.data1.dot.here, x1], probs = 0 + error, na.rm = TRUE)
x.data1.right.limit.d <- c(x.data1.right.limit.d, tempo.x.data1.right.limit, tempo.x.data1.right.limit)
x.data1.left.limit.d <- c(x.data1.left.limit.d, tempo.x.data1.left.limit, tempo.x.data1.left.limit)
x.data1.right.limit.t <- c(x.data1.right.limit.t, tempo.x.data1.right.limit, tempo.x.data1.right.limit)
x.data1.left.limit.t <- c(x.data1.left.limit.t, tempo.x.data1.left.limit, tempo.x.data1.left.limit)
x.data1.dot.signif <- ( ! ((data1[, x1] <= tempo.x.data1.right.limit) & (data1[, x1] >= tempo.x.data1.left.limit))) & y.data1.dot.here # is there data2 dot present in the sliding window, above or below the data1 limits, considering the x axis?
x.data1.dot.not.signif <- y.data1.dot.here & ! x.data1.dot.signif
x.outside.data1.dot.nb <- c(x.outside.data1.dot.nb, data1$DOT_NB[x.data1.dot.signif]) # recover the row number of data1
x.outside.data1.dot.nb <- unique(x.outside.data1.dot.nb)
x.inside.data1.dot.nb <- c(x.inside.data1.dot.nb, data1$DOT_NB[x.data1.dot.not.signif])
x.inside.data1.dot.nb <- unique(x.inside.data1.dot.nb)
}else{
x.data1.right.limit.d <- c(x.data1.right.limit.d, x.data1.median, x.data1.median)
x.data1.left.limit.d <- c(x.data1.left.limit.d, x.data1.median, x.data1.median)
x.data1.right.limit.t <- c(x.data1.right.limit.t, x.data1.median, x.data1.median)
x.data1.left.limit.t <- c(x.data1.left.limit.t, x.data1.median, x.data1.median)
}
# end recover the data1 dots outside the frame
# recover the data2 dots outside the frame
if( ! is.null(data2)){
if(any(y.data1.dot.here == TRUE) & any(y.data2.dot.here == TRUE)){ 
x.data2.dot.signif <- ( ! ((data2[, x2] <= tempo.x.data1.right.limit) & (data2[, x2] >= tempo.x.data1.left.limit))) & y.data2.dot.here # is there data2 dot present in the sliding window, above or below the data1 limits, considering the x axis?
x.data2.dot.not.signif <- y.data2.dot.here & ! x.data2.dot.signif
x.outside.data2.dot.nb <- c(x.outside.data2.dot.nb, data2$DOT_NB[x.data2.dot.signif])
x.outside.data2.dot.nb <- unique(x.outside.data2.dot.nb)
x.inside.data2.dot.nb <- c(x.inside.data2.dot.nb, data2$DOT_NB[x.data2.dot.not.signif])
x.inside.data2.dot.nb <- unique(x.inside.data2.dot.nb)
}else if(any(y.data1.dot.here == FALSE) & any(y.data2.dot.here == TRUE)){ # recover the data2 dots outside the range of the data1 cloud
x.unknown.data2.dot.nb <- c(x.unknown.data2.dot.nb, data2$DOT_NB[y.data2.dot.here])
x.unknown.data2.dot.nb <- unique(x.unknown.data2.dot.nb)



# tempo.warn <- paste0("FROM FUNCTION ", function.name, ": THE [", round(min.pos, 3), " ; ", round(max.pos, 3), "] INTERVAL DOES NOT CONTAIN data1 Y VALUES BUT CONTAINS data2 Y VALUES WHICH CANNOT BE EVALUATED.\nTHE CONCERNED data2 ROW NUMBERS ARE:\n", paste(which(y.data1.dot.here == FALSE & y.data2.dot.here == TRUE), collapse = "\n"))
# warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}
}
# end recover the data2 dots outside the frame
# if(any(i1 == seq(1, loop.nb, 500))){
# loop.fin.time <- as.numeric(Sys.time()) # time of process end
# cat(paste0("COMPUTATION TIME OF LOOP ", i1, " / ", loop.nb, ": ", as.character(lubridate::seconds_to_period(round(loop.fin.time - loop.ini.time))), "\n"))
# }
}
if(max.pos < y.range[2]){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": THE SLIDING WINDOW HAS NOT REACHED THE MAX VALUE OF data1 ON THE Y-AXIS: ", max.pos, " VERSUS ", y.range[2], "\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
x.incon.data1.dot.nb.final <- unique(c(x.outside.data1.dot.nb[x.outside.data1.dot.nb %in% x.inside.data1.dot.nb], x.inside.data1.dot.nb[x.inside.data1.dot.nb %in% x.outside.data1.dot.nb])) # inconsistent dots: if a row number of x.inside.data1.dot.nb is present in x.outside.data1.dot.nb (and vice versa), it means that during the sliding, a dot has been sometime inside, sometime outside -> removed from the outside list
x.outside.data1.dot.nb.final <- x.outside.data1.dot.nb[ ! (x.outside.data1.dot.nb %in% x.incon.data1.dot.nb.final)] # inconsistent dots removed from the outside list
x.inside.data1.dot.nb.final <- x.inside.data1.dot.nb[ ! (x.inside.data1.dot.nb %in% x.incon.data1.dot.nb.final)] # inconsistent dots removed from the inside list
if( ! is.null(data2)){
# if some unknown dots are also inside, and/or outside, they are put in the inside and/or outside. Ok, because then the intersection between inside and outside is treated -> inconsistent dots
tempo.unknown.out <- x.unknown.data2.dot.nb[x.unknown.data2.dot.nb %in% x.outside.data2.dot.nb]
x.outside.data2.dot.nb <- unique(c(x.outside.data2.dot.nb, tempo.unknown.out)) # if a row number of x.unknown.data2.dot.nb is present in x.outside.data2.dot.nb, it is put into outside
tempo.unknown.in <- x.unknown.data2.dot.nb[x.unknown.data2.dot.nb %in% x.inside.data2.dot.nb]
x.inside.data2.dot.nb <- unique(c(x.inside.data2.dot.nb, tempo.unknown.in)) # if a row number of x.unknown.data2.dot.nb is present in x.inside.data2.dot.nb, it is put into inside
x.unknown.data2.dot.nb.final <- x.unknown.data2.dot.nb[ ! (x.unknown.data2.dot.nb %in% c(x.outside.data2.dot.nb, x.inside.data2.dot.nb))] # then dots also in inside and outside are remove from unknown
x.incon.data2.dot.nb.final <- unique(c(x.outside.data2.dot.nb[x.outside.data2.dot.nb %in% x.inside.data2.dot.nb], x.inside.data2.dot.nb[x.inside.data2.dot.nb %in% x.outside.data2.dot.nb])) # inconsistent dots: if a row number of x.inside.data2.dot.nb is present in x.outside.data2.dot.nb (and vice versa), it means that during the sliding, a dot has been sometime inside, sometime outside -> removed from the outside list
x.outside.data2.dot.nb.final <- x.outside.data2.dot.nb[ ! (x.outside.data2.dot.nb %in% x.incon.data2.dot.nb.final)] # inconsistent dots removed from the outside list
x.inside.data2.dot.nb.final <- x.inside.data2.dot.nb[ ! (x.inside.data2.dot.nb %in% x.incon.data2.dot.nb.final)] # inconsistent dots removed from the inside list
}
# end y-axis sliding and x-axis limits of the data1 cloud -> x significant data2
}
# end Method using y unit interval 



# recovering the frame coordinates
hframe = rbind(
data.frame(
x = if(is.null(x.data1.l)){NULL}else{x.data1.l}, 
y = if(is.null(x.data1.l)){NULL}else{y.data1.down.limit.l}, 
kind = if(is.null(x.data1.l)){NULL}else{"down.frame1"}
), 
data.frame(
x = if(is.null(x.data1.r)){NULL}else{x.data1.r}, 
y = if(is.null(x.data1.r)){NULL}else{y.data1.down.limit.r}, 
kind = if(is.null(x.data1.r)){NULL}else{"down.frame2"}
), 
data.frame(
x = if(is.null(x.data1.l)){NULL}else{x.data1.l}, 
y = if(is.null(x.data1.l)){NULL}else{y.data1.top.limit.l}, 
kind = if(is.null(x.data1.l)){NULL}else{"top.frame1"}
), 
data.frame(
x = if(is.null(x.data1.r)){NULL}else{x.data1.r}, 
y = if(is.null(x.data1.r)){NULL}else{y.data1.top.limit.r}, 
kind = if(is.null(x.data1.r)){NULL}else{"top.frame2"}
)
)
vframe = rbind(
data.frame(
x = if(is.null(y.data1.d)){NULL}else{x.data1.left.limit.d}, 
y = if(is.null(y.data1.d)){NULL}else{y.data1.d}, 
kind = if(is.null(y.data1.d)){NULL}else{"left.frame1"}
), 
data.frame(
x = if(is.null(y.data1.t)){NULL}else{x.data1.left.limit.t}, 
y = if(is.null(y.data1.t)){NULL}else{y.data1.t}, 
kind = if(is.null(y.data1.t)){NULL}else{"left.frame2"}
), 
data.frame(
x = if(is.null(y.data1.d)){NULL}else{x.data1.right.limit.d}, 
y = if(is.null(y.data1.d)){NULL}else{y.data1.d}, 
kind = if(is.null(y.data1.d)){NULL}else{"right.frame1"}
),
data.frame(
x = if(is.null(y.data1.t)){NULL}else{x.data1.right.limit.t}, 
y = if(is.null(y.data1.t)){NULL}else{y.data1.t}, 
kind = if(is.null(y.data1.t)){NULL}else{"right.frame2"}
)
)
# end recovering the frame coordinates
# recovering the dot coordinates
data1.signif.dot <- NULL
data1.non.signif.dot <- NULL
data1.incon.dot <- NULL
data2.signif.dot <- NULL
data2.non.signif.dot <- NULL
data2.unknown.dot <- NULL
data2.incon.dot <- NULL
if(( ! is.null(x.range.split)) & ( ! is.null(y.range.split))){
# inconsistent dots recovery 
if(length(unique(c(x.incon.data1.dot.nb.final, y.incon.data1.dot.nb.final))) > 0){
data1.incon.dot <- data1[data1$DOT_NB %in% unique(c(x.incon.data1.dot.nb.final, y.incon.data1.dot.nb.final)), ] # if a dot in inconsistent in x or y -> classified as inconsistent (so unique() used)
# removal of the inconsistent dot in the other classifications
x.inside.data1.dot.nb.final <- x.inside.data1.dot.nb.final[ ! x.inside.data1.dot.nb.final %in% data1.incon.dot$DOT_NB]
y.inside.data1.dot.nb.final <- y.inside.data1.dot.nb.final[ ! y.inside.data1.dot.nb.final %in% data1.incon.dot$DOT_NB]
x.outside.data1.dot.nb.final <- x.outside.data1.dot.nb.final[ ! x.outside.data1.dot.nb.final %in% data1.incon.dot$DOT_NB]
y.outside.data1.dot.nb.final <- y.outside.data1.dot.nb.final[ ! y.outside.data1.dot.nb.final %in% data1.incon.dot$DOT_NB]
x.unknown.data1.dot.nb.final <- x.unknown.data1.dot.nb.final[ ! x.unknown.data1.dot.nb.final %in% data1.incon.dot$DOT_NB]
y.unknown.data1.dot.nb.final <- y.unknown.data1.dot.nb.final[ ! y.unknown.data1.dot.nb.final %in% data1.incon.dot$DOT_NB]
# end removal of the inconsistent dot in the other classifications
}
if( ! is.null(data2)){
if(length(unique(c(x.incon.data2.dot.nb.final, y.incon.data2.dot.nb.final))) > 0){
data2.incon.dot <- data2[data2$DOT_NB %in% unique(c(x.incon.data2.dot.nb.final, y.incon.data2.dot.nb.final)), ]
# removal of the inconsistent dot in the other classifications
x.inside.data2.dot.nb.final <- x.inside.data2.dot.nb.final[ ! x.inside.data2.dot.nb.final %in% data2.incon.dot$DOT_NB]
y.inside.data2.dot.nb.final <- y.inside.data2.dot.nb.final[ ! y.inside.data2.dot.nb.final %in% data2.incon.dot$DOT_NB]
x.outside.data2.dot.nb.final <- x.outside.data2.dot.nb.final[ ! x.outside.data2.dot.nb.final %in% data2.incon.dot$DOT_NB]
y.outside.data2.dot.nb.final <- y.outside.data2.dot.nb.final[ ! y.outside.data2.dot.nb.final %in% data2.incon.dot$DOT_NB]
x.unknown.data2.dot.nb.final <- x.unknown.data2.dot.nb.final[ ! x.unknown.data2.dot.nb.final %in% data2.incon.dot$DOT_NB]
y.unknown.data2.dot.nb.final <- y.unknown.data2.dot.nb.final[ ! y.unknown.data2.dot.nb.final %in% data2.incon.dot$DOT_NB]
# end removal of the inconsistent dot in the other classifications
}
}
# end inconsistent dots recovery 
# unknown dots recovery 
if( ! is.null(data2)){
if(data2.pb.dot == "signif"){
x.outside.data2.dot.nb.final <- unique(c(x.outside.data2.dot.nb.final, x.unknown.data2.dot.nb.final))
x.inside.data2.dot.nb.final <- x.inside.data2.dot.nb.final[ ! x.inside.data2.dot.nb.final %in% x.unknown.data2.dot.nb.final] # remove x.unknown.data2.dot.nb.final from x.inside.data2.dot.nb.final
y.outside.data2.dot.nb.final <- unique(c(y.outside.data2.dot.nb.final, y.unknown.data2.dot.nb.final))
y.inside.data2.dot.nb.final <- y.inside.data2.dot.nb.final[ ! y.inside.data2.dot.nb.final %in% y.unknown.data2.dot.nb.final] # remove y.unknown.data2.dot.nb.final from y.inside.data2.dot.nb.final
x.unknown.data2.dot.nb.final <- NULL
y.unknown.data2.dot.nb.final <- NULL
data2.unknown.dot <- NULL
}else if(data2.pb.dot == "not.signif"){
x.inside.data2.dot.nb.final <- unique(c(x.inside.data2.dot.nb.final, x.unknown.data2.dot.nb.final))
x.outside.data2.dot.nb.final <- x.outside.data2.dot.nb.final[ ! x.outside.data2.dot.nb.final %in% x.unknown.data2.dot.nb.final] # remove x.unknown.data2.dot.nb.final from x.outside.data2.dot.nb.final
y.inside.data2.dot.nb.final <- unique(c(y.inside.data2.dot.nb.final, y.unknown.data2.dot.nb.final))
y.outside.data2.dot.nb.final <- y.outside.data2.dot.nb.final[ ! y.outside.data2.dot.nb.final %in% y.unknown.data2.dot.nb.final] # remove y.unknown.data2.dot.nb.final from y.outside.data2.dot.nb.final
x.unknown.data2.dot.nb.final <- NULL
y.unknown.data2.dot.nb.final <- NULL
data2.unknown.dot <- NULL
}else if(data2.pb.dot == "unknown"){
if(length(unique(c(x.unknown.data2.dot.nb.final, y.unknown.data2.dot.nb.final))) > 0){
data2.unknown.dot <- data2[data2$DOT_NB %in% unique(c(x.unknown.data2.dot.nb.final, y.unknown.data2.dot.nb.final)), ] # if a dot in unknown in x or y -> classified as unknown (so unique() used)
x.outside.data2.dot.nb.final <- x.outside.data2.dot.nb.final[ ! x.outside.data2.dot.nb.final %in% data2.unknown.dot$DOT_NB] # remove x.unknown.data2.dot.nb.final from x.outside.data2.dot.nb.final
x.inside.data2.dot.nb.final <- x.inside.data2.dot.nb.final[ ! x.inside.data2.dot.nb.final %in% data2.unknown.dot$DOT_NB] # remove x.unknown.data2.dot.nb.final from x.inside.data2.dot.nb.final
y.outside.data2.dot.nb.final <- y.outside.data2.dot.nb.final[ ! y.outside.data2.dot.nb.final %in% data2.unknown.dot$DOT_NB] # remove y.unknown.data2.dot.nb.final from y.outside.data2.dot.nb.final
y.inside.data2.dot.nb.final <- y.inside.data2.dot.nb.final[ ! y.inside.data2.dot.nb.final %in% data2.unknown.dot$DOT_NB] # remove y.unknown.data2.dot.nb.final from y.inside.data2.dot.nb.final
}
}else{
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": CODE INCONSISTENCY 3\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
}
# end unknown dots recovery 
# sign and non sign dot recovery
if(xy.cross.kind == "|"){ # here the problem is to deal with significant dots depending on x and y. Thus I start with that, recover dots finally non significant in outside and put them in inside (when &), and remove from inside the dots in outside
if(length(unique(c(x.outside.data1.dot.nb.final, y.outside.data1.dot.nb.final))) > 0){
tempo.outside <- unique(c(x.outside.data1.dot.nb.final, y.outside.data1.dot.nb.final)) # union so unique() used
tempo.inside <- unique(c(x.inside.data1.dot.nb.final, y.inside.data1.dot.nb.final))
tempo.inside <- tempo.inside[ ! tempo.inside %in% tempo.outside]
data1.signif.dot <- data1[data1$DOT_NB %in% tempo.outside, ]
data1.non.signif.dot <- data1[data1$DOT_NB %in% tempo.inside, ]
}else{
data1.non.signif.dot <- data1[unique(c(x.inside.data1.dot.nb.final, y.inside.data1.dot.nb.final)), ] # if no outside dots, I recover all the inside dots and that's it
}
}else if(xy.cross.kind == "&"){
if(sum(x.outside.data1.dot.nb.final %in% y.outside.data1.dot.nb.final) > 0){ # that is intersection
tempo.outside <- unique(x.outside.data1.dot.nb.final[x.outside.data1.dot.nb.final %in% y.outside.data1.dot.nb.final]) # intersection
tempo.outside.removed <- unique(c(x.outside.data1.dot.nb.final, y.outside.data1.dot.nb.final))[ ! unique(c(x.outside.data1.dot.nb.final, y.outside.data1.dot.nb.final)) %in% tempo.outside]
tempo.inside <- unique(c(x.inside.data1.dot.nb.final, y.inside.data1.dot.nb.final))
data1.signif.dot <- data1[data1$DOT_NB %in% tempo.outside, ]
data1.non.signif.dot <- data1[data1$DOT_NB %in% tempo.inside, ]
}else{
data1.non.signif.dot <- data1[unique(c(x.inside.data1.dot.nb.final, y.inside.data1.dot.nb.final)), ] # if no outside dots, I recover all the inside dots and that's it
}
}else{
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": CODE INCONSISTENCY 4\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
if( ! is.null(data2)){
if(xy.cross.kind == "|"){ # here the problem is to deal with significant dots depending on x and y. Thus I start with that, recover dots finally non significant in outside and put them in inside (when &), and remove from inside the dots in outside
if(length(unique(c(x.outside.data2.dot.nb.final, y.outside.data2.dot.nb.final))) > 0){
tempo.outside <- unique(c(x.outside.data2.dot.nb.final, y.outside.data2.dot.nb.final)) # union so unique() used
tempo.inside <- unique(c(x.inside.data2.dot.nb.final, y.inside.data2.dot.nb.final))
tempo.inside <- tempo.inside[ ! tempo.inside %in% tempo.outside]
data2.signif.dot <- data2[data2$DOT_NB %in% tempo.outside, ]
data2.non.signif.dot <- data2[data2$DOT_NB %in% tempo.inside, ]
}else{
data2.non.signif.dot <- data2[unique(c(x.inside.data2.dot.nb.final, y.inside.data2.dot.nb.final)), ] # if no outside dots, I recover all the inside dots and that's it
}
}else if(xy.cross.kind == "&"){
if(sum(x.outside.data2.dot.nb.final %in% y.outside.data2.dot.nb.final) > 0){ # that is intersection
tempo.outside <- unique(x.outside.data2.dot.nb.final[x.outside.data2.dot.nb.final %in% y.outside.data2.dot.nb.final]) # intersection
tempo.outside.removed <- unique(c(x.outside.data2.dot.nb.final, y.outside.data2.dot.nb.final))[ ! unique(c(x.outside.data2.dot.nb.final, y.outside.data2.dot.nb.final)) %in% tempo.outside]
tempo.inside <- unique(c(x.inside.data2.dot.nb.final, y.inside.data2.dot.nb.final))
data2.signif.dot <- data2[data2$DOT_NB %in% tempo.outside, ]
data2.non.signif.dot <- data2[data2$DOT_NB %in% tempo.inside, ]
}else{
data2.non.signif.dot <- data2[unique(c(x.inside.data2.dot.nb.final, y.inside.data2.dot.nb.final)), ] # if no outside dots, I recover all the inside dots and that's it
}
}else{
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": CODE INCONSISTENCY 5\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
}
# end sign and non sign dot recovery
}else if(( ! is.null(x.range.split)) & is.null(y.range.split)){
# inconsistent dots recovery 
if(length(y.incon.data1.dot.nb.final) > 0){
data1.incon.dot <- data1[data1$DOT_NB %in% y.incon.data1.dot.nb.final, ]
}
if( ! is.null(data2)){
if(length(y.incon.data2.dot.nb.final) > 0){
data2.incon.dot <- data2[data2$DOT_NB %in% y.incon.data2.dot.nb.final, ]
}
}# end inconsistent dots recovery 
# unknown dots recovery 
if( ! is.null(data2)){
if(data2.pb.dot == "signif"){
y.outside.data2.dot.nb.final <- unique(c(y.outside.data2.dot.nb.final, y.unknown.data2.dot.nb.final))
}else if(data2.pb.dot == "not.signif"){
y.inside.data2.dot.nb.final <- unique(c(y.inside.data2.dot.nb.final, y.unknown.data2.dot.nb.final))
}else if(data2.pb.dot == "unknown"){
if(length(y.unknown.data2.dot.nb.final) > 0){
data2.unknown.dot <- data2[data2$DOT_NB %in% y.unknown.data2.dot.nb.final, ]
}
}else{
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": CODE INCONSISTENCY 6\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
}
# end unknown dots recovery 
# sign and non sign dot recovery
if(length(y.outside.data1.dot.nb.final) > 0){
data1.signif.dot <- data1[data1$DOT_NB %in% y.outside.data1.dot.nb.final, ]
}
if(length(y.inside.data1.dot.nb.final) > 0){
data1.non.signif.dot <- data1[data1$DOT_NB %in% y.inside.data1.dot.nb.final, ]
}
if( ! is.null(data2)){
if(length(y.outside.data2.dot.nb.final) > 0){
data2.signif.dot <- data2[data2$DOT_NB %in% y.outside.data2.dot.nb.final, ]
}
if(length(y.inside.data2.dot.nb.final) > 0){
data2.non.signif.dot <- data2[data2$DOT_NB %in% y.inside.data2.dot.nb.final, ]
}
}
# end sign and non sign dot recovery
}else if(is.null(x.range.split) & ( ! is.null(y.range.split))){
# inconsistent dots recovery 
if(length(x.incon.data1.dot.nb.final) > 0){
data1.incon.dot <- data1[data1$DOT_NB %in% x.incon.data1.dot.nb.final, ]
}
if( ! is.null(data2)){
if(length(x.incon.data2.dot.nb.final) > 0){
data2.incon.dot <- data2[data2$DOT_NB %in% x.incon.data2.dot.nb.final, ]
}
}# end inconsistent dots recovery 
# unknown dots recovery 
if( ! is.null(data2)){
if(data2.pb.dot == "signif"){
x.outside.data2.dot.nb.final <- unique(c(x.outside.data2.dot.nb.final, x.unknown.data2.dot.nb.final))
}else if(data2.pb.dot == "not.signif"){
x.inside.data2.dot.nb.final <- unique(c(x.inside.data2.dot.nb.final, x.unknown.data2.dot.nb.final))
}else if(data2.pb.dot == "unknown"){
if(length(x.unknown.data2.dot.nb.final) > 0){
data2.unknown.dot <- data2[data2$DOT_NB %in% x.unknown.data2.dot.nb.final, ]
}
}else{
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": CODE INCONSISTENCY 7\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
}
# end unknown dots recovery 
# sign and non sign dot recovery
if(length(x.outside.data1.dot.nb.final) > 0){
data1.signif.dot <- data1[data1$DOT_NB %in% x.outside.data1.dot.nb.final, ]
}
if(length(x.inside.data1.dot.nb.final) > 0){
data1.non.signif.dot <- data1[data1$DOT_NB %in% x.inside.data1.dot.nb.final, ]
}
if( ! is.null(data2)){
if(length(x.outside.data2.dot.nb.final) > 0){
data2.signif.dot <- data2[data2$DOT_NB %in% x.outside.data2.dot.nb.final, ]
}
if(length(x.inside.data2.dot.nb.final) > 0){
data2.non.signif.dot <- data2[data2$DOT_NB %in% x.inside.data2.dot.nb.final, ]
}
}
# end sign and non sign dot recovery
}
# end recovering the dot coordinates
# verif
if(any(data1.signif.dot$DOT_NB %in% data1.non.signif.dot$DOT_NB)){
tempo.cat <- paste0("\n\n============\n\nERROR IN ", FUNCTION.NAME, ": CODE INCONSISTENCY 8\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
if(any(data1.non.signif.dot$DOT_NB %in% data1.signif.dot$DOT_NB)){
tempo.cat <- paste0("\n\n============\n\nERROR IN ", FUNCTION.NAME, ": CODE INCONSISTENCY 9\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
if(any(data1.signif.dot$DOT_NB %in% data1.incon.dot$DOT_NB)){
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": CODE INCONSISTENCY 10\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
if(any(data1.incon.dot$DOT_NB %in% data1.signif.dot$DOT_NB)){
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": CODE INCONSISTENCY 11\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
if(any(data1.non.signif.dot$DOT_NB %in% data1.incon.dot$DOT_NB)){
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": CODE INCONSISTENCY 12\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
if(any(data1.incon.dot$DOT_NB %in% data1.non.signif.dot$DOT_NB)){
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": CODE INCONSISTENCY 13\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
if( ! is.null(data2)){
if(any(data2.signif.dot$DOT_NB %in% data2.non.signif.dot$DOT_NB)){
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": CODE INCONSISTENCY 14\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
if(any(data2.non.signif.dot$DOT_NB %in% data2.signif.dot$DOT_NB)){
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": CODE INCONSISTENCY 15\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
if(any(data2.signif.dot$DOT_NB %in% data2.unknown.dot$DOT_NB)){
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": CODE INCONSISTENCY 16\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
if(any(data2.unknown.dot$DOT_NB %in% data2.signif.dot$DOT_NB)){
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": CODE INCONSISTENCY 17\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
if(any(data2.signif.dot$DOT_NB %in% data2.incon.dot$DOT_NB)){
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": CODE INCONSISTENCY 18\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
if(any(data2.incon.dot$DOT_NB %in% data2.signif.dot$DOT_NB)){
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": CODE INCONSISTENCY 19\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
if(any(data2.non.signif.dot$DOT_NB %in% data2.unknown.dot$DOT_NB)){
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": CODE INCONSISTENCY 20\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
if(any(data2.unknown.dot$DOT_NB %in% data2.non.signif.dot$DOT_NB)){
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": CODE INCONSISTENCY 21\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
if(any(data2.non.signif.dot$DOT_NB %in% data2.incon.dot$DOT_NB)){
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": CODE INCONSISTENCY 22\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
if(any(data2.incon.dot$DOT_NB %in% data2.non.signif.dot$DOT_NB)){
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": CODE INCONSISTENCY 23\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
if(any(data2.unknown.dot$DOT_NB %in% data2.incon.dot$DOT_NB)){
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": CODE INCONSISTENCY 24\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
if(any(data2.incon.dot$DOT_NB %in% data2.unknown.dot$DOT_NB)){
tempo.cat <- paste0("\n\n============\n\nERROR IN ", function.name, ": CODE INCONSISTENCY 25\n\n============\n\n")
stop(tempo.cat, call. = FALSE)
}
}
# end verif
# plot
# recovering the axes data whatever plot or not
if(is.null(data2)){
axes <- fun_gg_scatter(data1 = list(data1), x = list(x1), y = list(y1), categ = list(NULL), color = list(fun_gg_palette(2)[2]), geom = list("geom_point"), alpha = list(0.5), x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, plot = FALSE, return = TRUE)$axes
}else{
axes <- fun_gg_scatter(data1 = list(data1, data2), x = list(x1, x2), y = list(y1, y2), categ = list(NULL, NULL), color = list(fun_gg_palette(2)[2], fun_gg_palette(2)[1]), geom = list("geom_point", "geom_point"), alpha = list(0.5, 0.5), x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, plot = FALSE, return = TRUE)$axes
}
# end recovering the axes data whatever plot or not
if(plot == TRUE){
# add a categ for plot legend
tempo.df.name <- c("data1", "data1.signif.dot", "data1.incon.dot", "data2", "data2.signif.dot", "data2.unknown.dot", "data2.incon.dot")
tempo.class.name <- c("data1", "data1", "data1", "data2", "data2", "data2", "data2")
for(i2 in 1:length(tempo.df.name)){
if( ! is.null(get(tempo.df.name[i2]))){
assign(tempo.df.name[i2], data.frame(get(tempo.df.name[i2]), kind = tempo.class.name[i2]))
}
}
# end add a categ for plot legend
if(( ! is.null(x.range.split)) & ( ! is.null(y.range.split))){
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
tempo.graph <- fun_gg_scatter(data1 = list(data1, hframe, vframe), x = list(x1, "x", "x"), y = list(y1, "y", "y"), categ = list("kind", "kind", "kind"), legend.name = list("DATASET", "HORIZ FRAME" , "VERT FRAME"), color = list(fun_gg_palette(2)[2], rep(hsv(h = c(0.1, 0.15), v = c(0.75, 1)), 2), rep(hsv(h = c(0.5, 0.6), v = c(0.9, 1)), 2)), geom = list("geom_point", "geom_path", "geom_path"), alpha = list(0.5, 0.5, 0.5), title = "DATA1", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
if( ! is.null(tempo.graph$warn)){
warn <- paste0(ifelse(is.null(warn), tempo.graph$warn, paste0(warn, "\n", tempo.graph$warn)))
}
if( ! is.null(data1.signif.dot)){
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
tempo.graph <- fun_gg_scatter(data1 = list(data1, hframe, vframe, data1.signif.dot), x = list(x1, "x", "x", x1), y = list(y1, "y", "y", y1), categ = list("kind", "kind", "kind", "kind"), legend.name = list("DATASET", "HORIZ FRAME" , "VERT FRAME", "SIGNIF DOTS"), color = list(fun_gg_palette(2)[2], rep(hsv(h = c(0.1, 0.15), v = c(0.75, 1)), 2), rep(hsv(h = c(0.5, 0.6), v = c(0.9, 1)), 2), "black"), geom = list("geom_point", "geom_path", "geom_path", "geom_point"), alpha = list(0.5, 0.5, 0.5, 0.5), title = "DATA1 + DATA1 SIGNIFICANT DOTS", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
if( ! is.null(tempo.graph$warn)){
warn <- paste0(ifelse(is.null(warn), tempo.graph$warn, paste0(warn, "\n", tempo.graph$warn)))
}
}else{
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
fun_gg_empty_graph(text = "NO PLOT\nBECAUSE\nNO DATA1 DOTS\nOUTSIDE THE FRAMES", text.size = 8, title = "DATA1 + DATA1 SIGNIFICANT DOTS")
}
if( ! is.null(data1.incon.dot)){
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
tempo.graph <- fun_gg_scatter(data1 = list(data1, hframe, vframe, data1.incon.dot), x = list(x1, "x", "x", x1), y = list(y1, "y", "y", y1), categ = list("kind", "kind", "kind", "kind"), legend.name = list("DATASET", "HORIZ FRAME" , "VERT FRAME", "INCONSISTENT DOTS"), color = list(fun_gg_palette(2)[2], rep(hsv(h = c(0.1, 0.15), v = c(0.75, 1)), 2), rep(hsv(h = c(0.5, 0.6), v = c(0.9, 1)), 2), fun_gg_palette(7)[6]), geom = list("geom_point", "geom_path", "geom_path", "geom_point"), alpha = list(0.5, 0.5, 0.5, 0.5), title = "DATA1 + DATA1 INCONSISTENT DOTS", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
if( ! is.null(tempo.graph$warn)){
warn <- paste0(ifelse(is.null(warn), tempo.graph$warn, paste0(warn, "\n", tempo.graph$warn)))
}
}else{
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
fun_gg_empty_graph(text = "NO PLOT\nBECAUSE\nNO DATA1\nINCONSISTENT DOTS", text.size = 8, title = "DATA1 + DATA1 INCONSISTENT DOTS")
}
if( ! is.null(data2)){
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
tempo.graph <- fun_gg_scatter(data1 = list(data1, data2, hframe , vframe), x = list(x1, x2, "x", "x"), y = list(y1, y2, "y", "y"), categ = list("kind", "kind", "kind", "kind"), legend.name = list("DATASET", "DATASET", "HORIZ FRAME" , "VERT FRAME"), color = list(fun_gg_palette(2)[2], fun_gg_palette(2)[1], rep(hsv(h = c(0.1, 0.15), v = c(0.75, 1)), 2), rep(hsv(h = c(0.5, 0.6), v = c(0.9, 1)), 2)), geom = list("geom_point", "geom_point", "geom_path", "geom_path"), alpha = list(0.5, 0.5, 0.5, 0.5), title = "DATA1 + DATA2", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
if( ! is.null(tempo.graph$warn)){
warn <- paste0(ifelse(is.null(warn), tempo.graph$warn, paste0(warn, "\n", tempo.graph$warn)))
}
if( ! is.null(data2.signif.dot)){
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
tempo.graph <- fun_gg_scatter(data1 = list(data1, data2, data2.signif.dot, hframe , vframe), x = list(x1, x2, x2, "x", "x"), y = list(y1, y2, y2, "y", "y"), categ = list("kind", "kind", "kind", "kind", "kind"), legend.name = list("DATASET", "DATASET", "SIGNIF DOTS", "HORIZ FRAME" , "VERT FRAME"), color = list(fun_gg_palette(2)[2], fun_gg_palette(2)[1], "black", rep(hsv(h = c(0.1, 0.15), v = c(0.75, 1)), 2), rep(hsv(h = c(0.5, 0.6), v = c(0.9, 1)), 2)), geom = list("geom_point", "geom_point", "geom_point", "geom_path", "geom_path"), alpha = list(0.5, 0.5, 0.5, 0.5, 0.5), title = "DATA1 + DATA2 + DATA2 SIGNIFICANT DOTS", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
if( ! is.null(tempo.graph$warn)){
warn <- paste0(ifelse(is.null(warn), tempo.graph$warn, paste0(warn, "\n", tempo.graph$warn)))
}
}else{
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
fun_gg_empty_graph(text = "NO PLOT\nBECAUSE\nNO DATA2 DOTS\nOUTSIDE THE FRAMES", text.size = 8, title = "DATA1 + DATA2 + DATA2 SIGNIFICANT DOTS")
}
if( ! is.null(data2.incon.dot)){
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
tempo.graph <- fun_gg_scatter(data1 = list(data1, data2, data2.incon.dot, hframe , vframe), x = list(x1, x2, x2, "x", "x"), y = list(y1, y2, y2, "y", "y"), categ = list("kind", "kind", "kind", "kind", "kind"), legend.name = list("DATASET", "DATASET", "INCONSISTENT DOTS", "HORIZ FRAME" , "VERT FRAME"), color = list(fun_gg_palette(2)[2], fun_gg_palette(2)[1], fun_gg_palette(7)[6], rep(hsv(h = c(0.1, 0.15), v = c(0.75, 1)), 2), rep(hsv(h = c(0.5, 0.6), v = c(0.9, 1)), 2)), geom = list("geom_point", "geom_point", "geom_point", "geom_path", "geom_path"), alpha = list(0.5, 0.5, 0.5, 0.5, 0.5), title = "DATA1 + DATA2 + DATA2 INCONSISTENT DOTS", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
if( ! is.null(tempo.graph$warn)){
warn <- paste0(ifelse(is.null(warn), tempo.graph$warn, paste0(warn, "\n", tempo.graph$warn)))
}
}else{
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
fun_gg_empty_graph(text = "NO PLOT\nBECAUSE\nNO DATA2\nINCONSISTENT DOTS", text.size = 8, title = "DATA2 + DATA2 INCONSISTENT DOTS")
}
if( ! is.null(data2.unknown.dot)){
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
tempo.graph <- fun_gg_scatter(data1 = list(data1, data2, data2.unknown.dot, hframe , vframe), x = list(x1, x2, x2, "x", "x"), y = list(y1, y2, y2, "y", "y"), categ = list("kind", "kind", "kind", "kind", "kind"), legend.name = list("DATASET", "DATASET", "UNKNOWN DOTS", "HORIZ FRAME" , "VERT FRAME"), color = list(fun_gg_palette(2)[2], fun_gg_palette(2)[1], fun_gg_palette(7)[5], rep(hsv(h = c(0.1, 0.15), v = c(0.75, 1)), 2), rep(hsv(h = c(0.5, 0.6), v = c(0.9, 1)), 2)), geom = list("geom_point", "geom_point", "geom_point", "geom_path", "geom_path"), alpha = list(0.5, 0.5, 0.5, 0.5, 0.5), title = "DATA1 + DATA2 + DATA2 UNKNOWN DOTS", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)

if( ! is.null(tempo.graph$warn)){
warn <- paste0(ifelse(is.null(warn), tempo.graph$warn, paste0(warn, "\n", tempo.graph$warn)))
}
}else{
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
fun_gg_empty_graph(text = "NO PLOT\nBECAUSE\nNO DATA2\nUNKNOWN DOTS", text.size = 12, title = "DATA2 + DATA2 UNKNOWN DOTS")
}
}
}else if(( ! is.null(x.range.split)) & is.null(y.range.split)){
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
tempo.graph <- fun_gg_scatter(data1 = list(data1, hframe), x = list(x1, "x"), y = list(y1, "y"), categ = list("kind", "kind"), legend.name = list("DATASET", "HORIZ FRAME"), color = list(fun_gg_palette(2)[2], rep(hsv(h = c(0.1, 0.15), v = c(0.75, 1)), 2)), geom = list("geom_point", "geom_path"), alpha = list(0.5, 0.5), title = "DATA1", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
if( ! is.null(tempo.graph$warn)){
warn <- paste0(ifelse(is.null(warn), tempo.graph$warn, paste0(warn, "\n", tempo.graph$warn)))
}
if( ! is.null(data1.signif.dot)){
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
tempo.graph <- fun_gg_scatter(data1 = list(data1, hframe, data1.signif.dot), x = list(x1, "x", x1), y = list(y1, "y", y1), categ = list("kind", "kind", "kind"), legend.name = list("DATASET", "HORIZ FRAME", "SIGNIF DOTS"), color = list(fun_gg_palette(2)[2], rep(hsv(h = c(0.1, 0.15), v = c(0.75, 1)), 2), "black"), geom = list("geom_point", "geom_path", "geom_point"), alpha = list(0.5, 0.5, 0.5), title = "DATA1 + DATA1 SIGNIFICANT DOTS", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
if( ! is.null(tempo.graph$warn)){
warn <- paste0(ifelse(is.null(warn), tempo.graph$warn, paste0(warn, "\n", tempo.graph$warn)))
}
}else{
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
fun_gg_empty_graph(text = "NO PLOT\nBECAUSE\nNO DATA1 DOTS\nOUTSIDE THE FRAMES", text.size = 8, title = "DATA1 + DATA1 SIGNIFICANT DOTS")
}
if( ! is.null(data1.incon.dot)){
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
tempo.graph <- fun_gg_scatter(data1 = list(data1, hframe, data1.incon.dot), x = list(x1, "x", x1), y = list(y1, "y", y1), categ = list("kind", "kind", "kind"), legend.name = list("DATASET", "HORIZ FRAME", "INCONSISTENT DOTS"), color = list(fun_gg_palette(2)[2], rep(hsv(h = c(0.1, 0.15), v = c(0.75, 1)), 2), fun_gg_palette(7)[6]), geom = list("geom_point", "geom_path", "geom_point"), alpha = list(0.5, 0.5, 0.5), title = "DATA1 + DATA1 INCONSISTENT DOTS", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
if( ! is.null(tempo.graph$warn)){
warn <- paste0(ifelse(is.null(warn), tempo.graph$warn, paste0(warn, "\n", tempo.graph$warn)))
}
}else{
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
fun_gg_empty_graph(text = "NO PLOT\nBECAUSE\nNO DATA1\nINCONSISTENT DOTS", text.size = 8, title = "DATA1 + DATA1 INCONSISTENT DOTS")
}
if( ! is.null(data2)){
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
tempo.graph <- fun_gg_scatter(data1 = list(data1, data2, hframe), x = list(x1, x2, "x"), y = list(y1, y2, "y"), categ = list("kind", "kind", "kind"), legend.name = list("DATASET", "DATASET", "HORIZ FRAME"), color = list(fun_gg_palette(2)[2], fun_gg_palette(2)[1], rep(hsv(h = c(0.1, 0.15), v = c(0.75, 1)), 2)), geom = list("geom_point", "geom_point", "geom_path"), alpha = list(0.5, 0.5, 0.5), title = "DATA1 + DATA2", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
if( ! is.null(tempo.graph$warn)){
warn <- paste0(ifelse(is.null(warn), tempo.graph$warn, paste0(warn, "\n", tempo.graph$warn)))
}
if( ! is.null(data2.signif.dot)){
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
tempo.graph <- fun_gg_scatter(data1 = list(data1, data2, data2.signif.dot, hframe), x = list(x1, x2, x2, "x"), y = list(y1, y2, y2, "y"), categ = list("kind", "kind", "kind", "kind"), legend.name = list("DATASET", "DATASET", "SIGNIF DOTS", "HORIZ FRAME"), color = list(fun_gg_palette(2)[2], fun_gg_palette(2)[1], "black", rep(hsv(h = c(0.1, 0.15), v = c(0.75, 1)), 2)), geom = list("geom_point", "geom_point", "geom_point", "geom_path"), alpha = list(0.5, 0.5, 0.5, 0.5), title = "DATA1 + DATA2 + DATA2 SIGNIFICANT DOTS", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
if( ! is.null(tempo.graph$warn)){
warn <- paste0(ifelse(is.null(warn), tempo.graph$warn, paste0(warn, "\n", tempo.graph$warn)))
}
}else{
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
fun_gg_empty_graph(text = "NO PLOT\nBECAUSE\nNO DATA2 DOTS\nOUTSIDE THE FRAMES", text.size = 8, title = "DATA1 + DATA2 + DATA2 SIGNIFICANT DOTS")
}
if( ! is.null(data2.incon.dot)){
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
tempo.graph <- fun_gg_scatter(data1 = list(data1, data2, data2.incon.dot, hframe), x = list(x1, x2, x2, "x"), y = list(y1, y2, y2, "y"), categ = list("kind", "kind", "kind", "kind"), legend.name = list("DATASET", "DATASET", "INCONSISTENT DOTS", "HORIZ FRAME"), color = list(fun_gg_palette(2)[2], fun_gg_palette(2)[1], fun_gg_palette(7)[6], rep(hsv(h = c(0.1, 0.15), v = c(0.75, 1)), 2)), geom = list("geom_point", "geom_point", "geom_point", "geom_path"), alpha = list(0.5, 0.5, 0.5, 0.5), title = "DATA1 + DATA2 + DATA2 INCONSISTENT DOTS", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
if( ! is.null(tempo.graph$warn)){
warn <- paste0(ifelse(is.null(warn), tempo.graph$warn, paste0(warn, "\n", tempo.graph$warn)))
}
}else{
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
fun_gg_empty_graph(text = "NO PLOT\nBECAUSE\nNO DATA2\nINCONSISTENT DOTS", text.size = 8, title = "DATA2 + DATA2 INCONSISTENT DOTS")
}
if( ! is.null(data2.unknown.dot)){
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
tempo.graph <- fun_gg_scatter(data1 = list(data1, data2, data2.unknown.dot, hframe), x = list(x1, x2, x2, "x"), y = list(y1, y2, y2, "y"), categ = list("kind", "kind", "kind", "kind"), legend.name = list("DATASET", "DATASET", "UNKNOWN DOTS", "HORIZ FRAME"), color = list(fun_gg_palette(2)[2], fun_gg_palette(2)[1], fun_gg_palette(7)[5], rep(hsv(h = c(0.1, 0.15), v = c(0.75, 1)), 2)), geom = list("geom_point", "geom_point", "geom_point", "geom_path"), alpha = list(0.5, 0.5, 0.5, 0.5), title = "DATA1 + DATA2 + DATA2 UNKNOWN DOTS", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
if( ! is.null(tempo.graph$warn)){
warn <- paste0(ifelse(is.null(warn), tempo.graph$warn, paste0(warn, "\n", tempo.graph$warn)))
}
}else{
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
fun_gg_empty_graph(text = "NO PLOT\nBECAUSE\nNO DATA2\nUNKNOWN DOTS", text.size = 8, title = "DATA2 + DATA2 UNKNOWN DOTS")
}
}
}else if(is.null(x.range.split) & ( ! is.null(y.range.split))){
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
tempo.graph <- fun_gg_scatter(data1 = list(data1, vframe), x = list(x1, "x"), y = list(y1, "y"), categ = list("kind", "kind"), legend.name = list("DATASET", "VERT FRAME"), color = list(fun_gg_palette(2)[2], rep(hsv(h = c(0.5, 0.6), v = c(0.9, 1)), 2)), geom = list("geom_point", "geom_path"), alpha = list(0.5, 0.5), title = "DATA1", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
if( ! is.null(tempo.graph$warn)){
warn <- paste0(ifelse(is.null(warn), tempo.graph$warn, paste0(warn, "\n", tempo.graph$warn)))
}
if( ! is.null(data1.signif.dot)){
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
tempo.graph <- fun_gg_scatter(data1 = list(data1, vframe, data1.signif.dot), x = list(x1, "x", x1), y = list(y1, "y", y1), categ = list("kind", "kind", "kind"), legend.name = list("DATASET", "VERT FRAME", "SIGNIF DOTS"), color = list(fun_gg_palette(2)[2], rep(hsv(h = c(0.5, 0.6), v = c(0.9, 1)), 2), "black"), geom = list("geom_point", "geom_path", "geom_point"), alpha = list(0.5, 0.5, 0.5), title = "DATA1 + DATA1 SIGNIFICANT DOTS", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
if( ! is.null(tempo.graph$warn)){
warn <- paste0(ifelse(is.null(warn), tempo.graph$warn, paste0(warn, "\n", tempo.graph$warn)))
}
}else{
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
fun_gg_empty_graph(text = "NO PLOT\nBECAUSE\nNO DATA1 DOTS\nOUTSIDE THE FRAMES", text.size = 8, title = "DATA1 + DATA1 SIGNIFICANT DOTS")
}
if( ! is.null(data1.incon.dot)){
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
tempo.graph <- fun_gg_scatter(data1 = list(data1, vframe, data1.incon.dot), x = list(x1, "x", x1), y = list(y1, "y", y1), categ = list("kind", "kind", "kind"), legend.name = list("DATASET", "VERT FRAME", "INCONSISTENT DOTS"), color = list(fun_gg_palette(2)[2], rep(hsv(h = c(0.5, 0.6), v = c(0.9, 1)), 2), fun_gg_palette(7)[6]), geom = list("geom_point", "geom_path", "geom_point"), alpha = list(0.5, 0.5, 0.5), title = "DATA1 + DATA1 INCONSISTENT DOTS", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
if( ! is.null(tempo.graph$warn)){
warn <- paste0(ifelse(is.null(warn), tempo.graph$warn, paste0(warn, "\n", tempo.graph$warn)))
}
}else{
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
fun_gg_empty_graph(text = "NO PLOT\nBECAUSE\nNO DATA1\nINCONSISTENT DOTS", text.size = 8, title = "DATA1 + DATA1 INCONSISTENT DOTS")
}
if( ! is.null(data2)){
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
tempo.graph <- fun_gg_scatter(data1 = list(data1, data2, vframe), x = list(x1, x2, "x"), y = list(y1, y2, "y"), categ = list("kind", "kind", "kind"), legend.name = list("DATASET", "DATASET", "VERT FRAME"), color = list(fun_gg_palette(2)[2], fun_gg_palette(2)[1], rep(hsv(h = c(0.5, 0.6), v = c(0.9, 1)), 2)), geom = list("geom_point", "geom_point", "geom_path"), alpha = list(0.5, 0.5, 0.5), title = "DATA1 + DATA2", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
if( ! is.null(tempo.graph$warn)){
warn <- paste0(ifelse(is.null(warn), tempo.graph$warn, paste0(warn, "\n", tempo.graph$warn)))
}
if( ! is.null(data2.signif.dot)){
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
tempo.graph <- fun_gg_scatter(data1 = list(data1, data2, data2.signif.dot, vframe), x = list(x1, x2, x2, "x"), y = list(y1, y2, y2, "y"), categ = list("kind", "kind", "kind", "kind"), legend.name = list("DATASET", "DATASET", "SIGNIF DOTS", "VERT FRAME"), color = list(fun_gg_palette(2)[2], fun_gg_palette(2)[1], "black", rep(hsv(h = c(0.5, 0.6), v = c(0.9, 1)), 2)), geom = list("geom_point", "geom_point", "geom_point", "geom_path"), alpha = list(0.5, 0.5, 0.5, 0.5), title = "DATA1 + DATA2 + DATA2 SIGNIFICANT DOTS", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
if( ! is.null(tempo.graph$warn)){
warn <- paste0(ifelse(is.null(warn), tempo.graph$warn, paste0(warn, "\n", tempo.graph$warn)))
}
}else{
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
fun_gg_empty_graph(text = "NO PLOT\nBECAUSE\nNO DATA2 DOTS\nOUTSIDE THE FRAMES", text.size = 8, title = "DATA1 + DATA2 + DATA2 SIGNIFICANT DOTS")
}
if( ! is.null(data2.incon.dot)){
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
tempo.graph <- fun_gg_scatter(data1 = list(data1, data2, data2.incon.dot, vframe), x = list(x1, x2, x2, "x"), y = list(y1, y2, y2, "y"), categ = list("kind", "kind", "kind", "kind"), legend.name = list("DATASET", "DATASET", "INCONSISTENT DOTS", "VERT FRAME"), color = list(fun_gg_palette(2)[2], fun_gg_palette(2)[1], fun_gg_palette(7)[6], rep(hsv(h = c(0.5, 0.6), v = c(0.9, 1)), 2)), geom = list("geom_point", "geom_point", "geom_point", "geom_path"), alpha = list(0.5, 0.5, 0.5, 0.5), title = "DATA1 + DATA2 + DATA2 INCONSISTENT DOTS", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
if( ! is.null(tempo.graph$warn)){
warn <- paste0(ifelse(is.null(warn), tempo.graph$warn, paste0(warn, "\n", tempo.graph$warn)))
}
}else{
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
fun_gg_empty_graph(text = "NO PLOT\nBECAUSE\nNO DATA2\nINCONSISTENT DOTS", text.size = 8, title = "DATA2 + DATA2 INCONSISTENT DOTS")
}
if( ! is.null(data2.unknown.dot)){
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
tempo.graph <- fun_gg_scatter(data1 = list(data1, data2, data2.unknown.dot, vframe), x = list(x1, x2, x2, "x"), y = list(y1, y2, y2, "y"), categ = list("kind", "kind", "kind", "kind"), legend.name = list("DATASET", "DATASET", "UNKNOWN DOTS", "VERT FRAME"), color = list(fun_gg_palette(2)[2], fun_gg_palette(2)[1], fun_gg_palette(7)[5], rep(hsv(h = c(0.5, 0.6), v = c(0.9, 1)), 2)), geom = list("geom_point", "geom_point", "geom_point", "geom_path"), alpha = list(0.5, 0.5, 0.5, 0.5), title = "DATA1 + DATA2 + DATA2 UNKNOWN DOTS", x.lim = x.range.plot, y.lim = y.range.plot, raster = raster, return = TRUE)
if( ! is.null(tempo.graph$warn)){
warn <- paste0(ifelse(is.null(warn), tempo.graph$warn, paste0(warn, "\n", tempo.graph$warn)))
}
}else{
if(graph.in.file == FALSE){
fun_open(pdf.disp = FALSE)
}
fun_gg_empty_graph(text = "NO PLOT\nBECAUSE\nNO DATA2\nUNKNOWN DOTS", text.size = 8, title = "DATA2 + DATA2 UNKNOWN DOTS")
}
}
}
}
# end plot
if(warn.print == TRUE & ! is.null(warn)){
warning(warn, call. = FALSE)
cat("\n\n")
}
tempo.list <- list(data1.removed.row.nb = data1.removed.row.nb, data1.removed.rows = data1.removed.rows, data2.removed.row.nb = data2.removed.row.nb, data2.removed.rows = data2.removed.rows, hframe = hframe, vframe = vframe, data1.signif.dot = data1.signif.dot, data1.non.signif.dot = data1.non.signif.dot, data1.inconsistent.dot = data1.incon.dot, data2.signif.dot = data2.signif.dot, data2.non.signif.dot = data2.non.signif.dot, data2.unknown.dot = data2.unknown.dot, data2.inconsistent.dot = data2.incon.dot, axes = axes, warn = warn)
return(tempo.list)
}


################ Import


######## fun_pack() #### check if R packages are present and import into the working environment


# Check OK: clear to go Apollo
fun_pack <- function(req.package, load = FALSE, lib.path = NULL){
# AIM
# check if the specified R packages are present in the computer and import them into the working environment
# ARGUMENTS
# req.package: character vector of package names to import
# req.package: logical. Load the package into the environement (using library())?
# lib.path: optional character vector specifying the absolute pathways of the directories containing some of the listed packages
# REQUIRED PACKAGES
# none
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# fun_check()
# RETURN
# nothing
# EXAMPLES
# fun_pack(req.package = "nopackage")
# fun_pack(req.package = "ggplot2")
# fun_pack(req.package = "ggplot2", lib.path = "blablabla")
# DEBUGGING
# req.package = "ggplot2" ; lib.path = "C:/Program Files/R/R-3.5.1/library"
# req.package = "serpentine" ; lib.path = "C:/users/gael/appdata/roaming/python/python36/site-packages"
# function name
function.name <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
# end function name
# required function checking
if(length(utils::find("fun_check", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_check() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end required function checking
# argument checking
arg.check <- NULL #
text.check <- NULL #
checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$fun.name))
tempo <- fun_check(data = req.package, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = load, class = "vector", mode = "logical", length = 1, fun.name = function.name) ; eval(ee)
if( ! is.null(lib.path)){
tempo <- fun_check(data = lib.path, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE){
if( ! all(dir.exists(lib.path))){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
tempo.cat <- paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", paste(lib.path, collapse = "\n"))
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
}
}
if(any(arg.check) == TRUE){
stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
}
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
# end argument checking
# main code
if(is.null(lib.path)){
lib.path <- .libPaths() # .libPaths(new = lib.path) # or .libPaths(new = c(.libPaths(), lib.path))
}else{
.libPaths(new = sub(x = lib.path, pattern = "/$|\\\\$", replacement = "")) # .libPaths(new = ) add path to default path. BEWARE: .libPaths() does not support / at the end of a submitted path. Thus check and replace last / or \\ in path
}
for(i1 in 1:length(req.package)){
if( ! req.package[i1] %in% rownames(utils::installed.packages(lib.loc = lib.path))){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": PACKAGE ", req.package[i1], " MUST BE INSTALLED IN:\n", paste(lib.path, collapse = "\n"), "\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}else{
if(load == TRUE){
suppressMessages(suppressWarnings(suppressPackageStartupMessages(library(req.package[i1], lib.loc = lib.path, quietly = TRUE, character.only = TRUE))))
}
}
}
}


######## fun_python_pack() #### check if python packages are present


# Check OK: clear to go Apollo
fun_python_pack <- function(req.package, python.exec.path = NULL, lib.path = NULL, R.lib.path = NULL){
# AIM
# check if the specified python packages are present in the computer (no import)
# WARNINGS
# for python 3.7. Previous versions return an error "Error in sys$stdout$flush() : attempt to apply non-function"
# ARGUMENTS
# req.package: character vector of package names to import
# python.exec.path: optional character vector specifying the absolute pathways of the executable python file to use (associated to the packages to use). If NULL, the reticulate::import_from_path() function used in fun_python_pack() seeks for an available version of python.exe, and then uses python_config(python_version, required_module, python_versions). But might not be the correct one for the lib.path parameter specified. Thus, it is recommanded to do not leave NULL, notably when using computing clusters
# lib.path: optional character vector specifying the absolute pathways of the directories containing some of the listed packages in the req.package argument
# R.lib.path: absolute path of the reticulate packages, if not in the default folders
# REQUIRED PACKAGES
# reticulate
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# fun_check()
# fun_pack()
# RETURN
# nothing
# EXAMPLES
# example of error message
# fun_python_pack(req.package = "nopackage")
# example without error message (require the installation of the python serpentine package from https://github.com/koszullab/serpentine
# fun_python_pack(req.package = "serpentine", python.exec.path = "C:/ProgramData/Anaconda3/python.exe", lib.path = "c:/programdata/anaconda3/lib/site-packages/")
# another example of error message
# fun_python_pack(req.package = "serpentine", lib.path = "blablabla")
# DEBUGGING
# req.package = "serpentine" ; python.exec.path = "C:/ProgramData/Anaconda3/python.exe" ; lib.path = "c:/programdata/anaconda3/lib/site-packages/" ; R.lib.path = NULL
# req.package = "bad" ; lib.path = NULL ; R.lib.path = NULL
# function name
function.name <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
# end function name
# required function checking
if(length(utils::find("fun_check", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_check() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
if(length(utils::find("fun_pack", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_pack() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end required function checking
# argument checking
arg.check <- NULL #
text.check <- NULL #
checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$fun.name))
tempo <- fun_check(data = req.package, class = "character", fun.name = function.name) ; eval(ee)
if( ! is.null(python.exec.path)){
tempo <- fun_check(data = python.exec.path, class = "character", length = 1, fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE){
if( ! all(file.exists(python.exec.path))){ # separation to avoid the problem of tempo$problem == FALSE and python.exec.path == NA
tempo.cat <- paste0("ERROR IN ", function.name, ": FILE PATH INDICATED IN THE python.exec.path ARGUMENT DOES NOT EXISTS:\n", paste(python.exec.path, collapse = "\n"))
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
}
}
if( ! is.null(lib.path)){
tempo <- fun_check(data = lib.path, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE){
if( ! all(dir.exists(lib.path))){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
tempo.cat <- paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", paste(lib.path, collapse = "\n"))
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
}
}
if( ! is.null(R.lib.path)){
tempo <- fun_check(data = R.lib.path, class = "character", fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE){
if( ! all(dir.exists(R.lib.path))){ # separation to avoid the problem of tempo$problem == FALSE and R.lib.path == NA
tempo.cat <- paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE R.lib.path ARGUMENT DOES NOT EXISTS:\n", paste(R.lib.path, collapse = "\n"))
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
}
}
if(any(arg.check) == TRUE){
stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
}
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
# end argument checking
# package checking
fun_pack(req.package = "reticulate", lib.path = R.lib.path)
# end package checking
# main code
if(is.null(python.exec.path)){
python.exec.path <- reticulate::py_run_string("
import sys ;
path_lib = sys.path
") # python string
python.exec.path <- python.exec.path$path_lib
}
if(is.null(lib.path)){
lib.path <- reticulate::py_run_string("
import sys ;
path_lib = sys.path
") # python string
lib.path <- lib.path$path_lib
}
reticulate::use_python(Sys.which(python.exec.path), required = TRUE) # required to avoid the use of erratic python exec by reticulate::import_from_path()
for(i1 in 1:length(req.package)){
tempo.try <- vector("list", length = length(lib.path))
for(i2 in 1:length(lib.path)){
tempo.try[[i2]] <- suppressWarnings(try(reticulate::import_from_path(req.package[i1], path = lib.path[i2]), silent = TRUE))
tempo.try[[i2]] <- suppressWarnings(try(reticulate::import_from_path(req.package[i1], path = lib.path[i2]), silent = TRUE)) # done twice to avoid the error message  about flushing present the first time but not the second time. see https://stackoverflow.com/questions/57357001/reticulate-1-13-error-in-sysstdoutflush-attempt-to-apply-non-function
}
if(all(sapply(tempo.try, FUN = grepl, pattern = "[Ee]rror"))){
print(tempo.try)
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": PACKAGE ", req.package[i1], " MUST BE INSTALLED IN THE MENTIONNED DIRECTORY:\n", paste(lib.path, collapse = "\n"), "\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
} # else{
# suppressMessages(suppressWarnings(suppressPackageStartupMessages(assign(req.package[i1], reticulate::import(req.package[i1]))))) # not required because try() already evaluates
# }
}
}


################ Print / Exporting results (text & tables)


######## fun_report() #### print string or data object into output file


# Check OK: clear to go Apollo
fun_report <- function(data, output = "results.txt", path = "C:/Users/Gael/Desktop/", no.overwrite = TRUE, rownames.kept = FALSE, vector.cat = FALSE, noquote = TRUE, sep = 2){
# AIM
# log file function: print a character string or a data object into a same output file
# REQUIRED PACKAGES
# utils
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# fun_check()
# ARGUMENTS
# data: object to print in the output file. If NULL, nothing is done, with no warning
# output: name of the output file
# path: location of the output file
# no.overwrite: (logical) if output file already exists, defines if the printing is appended (default TRUE) or if the output file content is erased before printing (FALSE)
# rownames.kept: (logical) defines whether row names have to be removed or not in small tables (less than length.rows rows)
# vector.cat (logical). If TRUE print a vector of length > 1 using cat() instead of capture.output(). Otherwise (default FALSE) the opposite
# noquote: (logical). If TRUE no quote are present for the characters
# sep: number of separating lines after printed data (must be integer)
# RETURN
# nothing
# EXAMPLES
# fun_report()
# fun_report(data = 1:3, output = "results.txt", path = "C:/Users/Gael/Desktop", no.overwrite = TRUE, rownames.kept = FALSE, vector.cat = FALSE, noquote = FALSE, sep = 2)
# DEBUGGING
# data = 1:3 ; output = "results.txt" ; path = "C:/Users/Gael/Desktop" ; no.overwrite = TRUE ; rownames.kept = FALSE ; vector.cat = FALSE ; noquote = FALSE ; sep = 2 # for function debugging
# function name
function.name <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
# end function name
# required function checking
if(length(utils::find("fun_check", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_check() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end required function checking
# argument checking
arg.check <- NULL #
text.check <- NULL #
checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$fun.name))
tempo <- fun_check(data = output, class = "character", length = 1, fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE & output == ""){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": output ARGUMENT AS \"\" DOES NOT CORRESPOND TO A VALID FILE NAME\n\n================\n\n")
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
tempo <- fun_check(data = path, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE){
if( ! all(dir.exists(path))){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": path ARGUMENT DOES NOT CORRESPOND TO EXISTING DIRECTORY\n", paste(path, collapse = "\n"),"\n\n================\n\n")
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
}
tempo <- fun_check(data = no.overwrite, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = rownames.kept, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = vector.cat, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = noquote, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = sep, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, fun.name = function.name) ; eval(ee)
if(any(arg.check) == TRUE){
stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
}
# end argument checking
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
# the 4 next lines are inactivated but kept because at a time, I might have a problem with data (solved with data = NULL). These 4 lines are just to know how to detect a missing argument. Important here because if data is not provided, print the code of the data function
# arg.user.list <- as.list(match.call(expand.dots=FALSE))[-1] # recover all the arguments provided by the function user (excluding the argument with defaults values not provided by the user. Thus, it is really the list indicated by the user)
# default.arg.list <- formals(fun = sys.function(sys.parent())) # list of all the arguments of the function with their default values (not the values of the user !). It seems that ls() as first line of the function provide the names of the arguments (empty, called, etc., or not)
# arg.without.default.value <- sapply(default.arg.list, is.symbol) & sapply(sapply(default.arg.list, as.character), identical, "") # logical to detect argument without default values (these are typeof "symbol" and class "name" and empty character
# if( ! all(names(default.arg.list)[arg.without.default.value] %in% names(arg.user.list))){ # test that the arguments with no null values are provided by the user
# tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": VALUE REQUIRED FOR THESE ARGUMENTS WITH NO DEFAULTS VALUES: ", paste(names(default.arg.list)[arg.without.default.value][ ! names(default.arg.list)[arg.without.default.value] %in% names(arg.user.list)], collapse = " "), "\n\n================\n\n")
#stop(tempo.cat, call. = FALSE)
# }
# end argument checking
# main code
if( ! is.null(data)){
if(all(class(data) %in% c("matrix", "data.frame", "table"))){
if(rownames.kept == FALSE & all(class(data) == "data.frame") & nrow(data) != 0 & nrow(data) <= 4){ # for data frames with nrows <= 4
rownames.output.tables <- ""
length.rows <- nrow(data)
for(i in 1:length.rows){ # replace the rownames of the first 4 rows by increasing number of spaces (because identical row names not allowed in data frames). This method cannot be extended to more rows as the printed data frame is shifted on the right because of "big empty rownames"
rownames.output.tables <- c(rownames.output.tables, paste0(rownames.output.tables[i]," ", collapse=""))
}
row.names(data) <- rownames.output.tables[1:length.rows]
}else if(rownames.kept == FALSE & all(class(data) %in% c("matrix", "table"))){
rownames(data) <- rep("", nrow(data)) # identical row names allowed in matrices and tables
}
if(noquote == TRUE){
utils::capture.output(noquote(data), file=paste0(path, "/", output), append = no.overwrite)
}else{
utils::capture.output(data, file=paste0(path, "/", output), append = no.overwrite)
}
}else if(is.vector(data) & all(class(data) != "list") & (length(data) == 1 | vector.cat == TRUE)){
if(noquote == TRUE){
cat(noquote(data), file= paste0(path, "/", output), append = no.overwrite)
}else{
cat(data, file= paste0(path, "/", output), append = no.overwrite)
}
}else if(all(mode(data) == "character")){ # characters (array, list, factor or vector with vector.cat = FALSE)
if(noquote == TRUE){
utils::capture.output(noquote(data), file=paste0(path, "/", output), append = no.overwrite)
}else{
utils::capture.output(data, file=paste0(path, "/", output), append = no.overwrite)
}
}else{ # other object (S4 for instance, which do not like noquote()
utils::capture.output(data, file=paste0(path, "/", output), append = no.overwrite)
}
sep.final <- paste0(rep("\n", sep), collapse = "")
write(sep.final, file= paste0(path, "/", output), append = TRUE) # add a sep
}
}


######## fun_get_message() #### return messages of an expression (that can be exported)


# Check OK: clear to go Apollo
fun_get_message <- function(data, kind = "error", header = TRUE, print.no = FALSE, text = NULL, env = NULL){
# AIM
# evaluate an instruction written between "" and return the first of the error, or warning or standard (non error non warning) messages if ever exist
# using argument print.no = FALSE, return NULL if no message, which is convenient in some cases
# WARNING
# Only the first message is returned
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# fun_check()
# ARGUMENTS
# data: character string to evaluate
# kind: character string. Either "error" to get error messages, or "warning" to get warning messages, or "message" to get non error and non warning messages
# header: logical. Add a header in the returned message?
# print.no: logical. Print a message saying that no message reported?
# text: character string added to the output message (even if no message exists and print.no is TRUE). Inactivated if header is FALSE
# env: the name of an existing environment. NULL if not required
# RETURN
# the message or NULL if no message and print.no is FALSE
# EXAMPLES
# fun_get_message(data = "wilcox.test(c(1,1,3), c(1, 2, 4), paired = TRUE)", kind = "error", print.no = TRUE, text = "IN A")
# fun_get_message(data = "wilcox.test(c(1,1,3), c(1, 2, 4), paired = TRUE)", kind = "warning", print.no = TRUE, text = "IN A")
# fun_get_message(data = "wilcox.test(c(1,1,3), c(1, 2, 4), paired = TRUE)", kind = "message", print.no = TRUE, text = "IN A")
# fun_get_message(data = "wilcox.test()", kind = "error", print.no = TRUE, text = "IN A")
# fun_get_message(data = "sum(1)", kind = "error", print.no = TRUE, text = "IN A")
# fun_get_message(data = "message('ahah')", kind = "error", print.no = TRUE, text = "IN A")
# fun_get_message(data = "message('ahah')", kind = "message", print.no = TRUE, text = "IN A")
# fun_get_message(data = "ggplot2::ggplot(data = data.frame(X = 1:10), mapping = ggplot2::aes(x = X)) + ggplot2::geom_histogram()", kind = "message", print.no = TRUE, text = "IN FUNCTION 1")
# set.seed(1) ; obs1 <- data.frame(Time = c(rnorm(10), rnorm(10) + 2), Group1 = rep(c("G", "H"), each = 10)) ; fun_get_message(data = 'fun_gg_boxplot(data = obs1, y = "Time", categ = "Group1")', kind = "message", print.no = TRUE, text = "IN FUNCTION 1")
# DEBUGGING
# data = "wilcox.test(c(1,1,3), c(1, 2, 4), paired = TRUE)" ; kind = "warning" ; header = TRUE ; print.no = FALSE ; text = NULL ; env = NULL # for function debugging
# data = "sum(1)" ; kind = "warning" ; header = TRUE ; print.no = FALSE ; text = NULL ; env = NULL  # for function debugging
# set.seed(1) ; obs1 <- data.frame(Time = c(rnorm(10), rnorm(10) + 2), Group1 = rep(c("G", "H"), each = 10)) ; data = 'fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Group1")' ; kind = "warning" ; header = TRUE ; print.no = FALSE ; text = NULL ; env = NULL  # for function debugging
# data = "message('ahah')" ; kind = "error" ; header = TRUE ; print.no = TRUE ; text = "IN A" ; env = NULL 
# data = 'ggplot2::ggplot(data = data.frame(X = "a"), mapping = ggplot2::aes(x = X)) + ggplot2::geom_histogram()' ; kind = "message" ; header = TRUE ; print.no = FALSE ; text = NULL # for function debugging
# data = 'ggplot2::ggplot(data = data.frame(X = "a"), mapping = ggplot2::aes(x = X)) + ggplot2::geom_histogram()' ; kind = "warning" ; header = TRUE ; print.no = FALSE ; text = NULL # for function debugging
# function name
function.name <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
# end function name
# required function checking
if(length(utils::find("fun_check", mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED fun_check() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat, call. = FALSE)
}
# end required function checking
# no need to use reserved words to avoid bugs, because it is local, and  exists("tempo.warning", inherit = FALSE), never use the scope
# argument checking
# argument checking with fun_check()
arg.check <- NULL #
text.check <- NULL #
checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$fun.name))
tempo <- fun_check(data = data, class = "character", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = kind, options = c("error", "warning", "message"), length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = print.no, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = header, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
if( ! is.null(text)){
tempo <- fun_check(data = text, class = "character", length = 1, fun.name = function.name) ; eval(ee)
}
if( ! is.null(env)){
tempo <- fun_check(data = env, class = "environment", fun.name = function.name) ; eval(ee) #
}
if(any(arg.check) == TRUE){
stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
}
# end argument checking with fun_check()
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
# end argument checking
# main code
pdf(file = NULL) # send plots into a NULL file, no pdf file created
window.nb <- dev.cur()
dev.set(window.nb)
# last warning cannot be used because suppressWarnings() does not modify last.warning present in the base evironment (created at first warning in a new R session), or warnings() # to reset the warning history : unlockBinding("last.warning", baseenv()) ; assign("last.warning", NULL, envir = baseenv())
output <- NULL
tempo.error <- try(suppressMessages(suppressWarnings(eval(parse(text = data), envir = if(is.null(env)){parent.frame()}else{env}))), silent = TRUE) # get error message, not warning or messages
if(any(class(tempo.error) %in% c("gg", "ggplot"))){
tempo.error <- try(suppressMessages(suppressWarnings(ggplot2::ggplot_build(tempo.error))), silent = TRUE)[1]
}
if(exists("tempo.error", inherit = FALSE) == TRUE){ # inherit = FALSE avoid the portee lexical and thus the declared word
if( ! all(class(tempo.error) == "try-error")){ # deal with NULL and S4 objects. Old code:  ! (all(class(tempo.error) == "try-error") & any(grepl(x = tempo.error, pattern = "^Error|^error|^ERROR"))) but problem with S4 objects. Old code : if((length(tempo.error) > 0 & ! any(grepl(x = tempo.error, pattern = "^Error|^error|^ERROR"))) | (length(tempo.error) == 0) ){ but problem when tempo.error is a list but added this did not work: | ! all(class(tempo.error) == "character")
tempo.error <- NULL
}
}else{
tempo.error <- NULL
}
if(kind == "error" & ! is.null(tempo.error)){ # 
if(header == TRUE){
tempo.error[1] <- gsub(x = tempo.error[1], pattern = "^Error i|^error i|^ERROR I", replacement = "I")
output <- paste0("ERROR MESSAGE REPORTED", ifelse(is.null(text), "", " "), text, ":\n", tempo.error[1]) #
}else{
output <- tempo.error[1] #
}
}else if(kind == "error" & is.null(tempo.error) & print.no == TRUE){
output <- paste0("NO ERROR MESSAGE REPORTED", ifelse(is.null(text), "", " "), text)
}else if(kind != "error" & ( ! is.null(tempo.error)) & print.no == TRUE){
output <- paste0("NO ", ifelse(kind == "warning", "WARNING", "STANDARD (NON ERROR AND NON WARNING)"), " MESSAGE BECAUSE OF ERROR MESSAGE REPORTED", ifelse(is.null(text), "", " "), text)
}else if(is.null(tempo.error)){
fun.warning.capture <- function(expr){
# from demo(error.catching) typed in the R console, coming from ?tryCatch
# see also http://mazamascience.com/WorkingWithData/?p=912
# return a character string or NULL
# expr <- wilcox.test.default(c(1, 1, 3), c(1, 2, 4), paired = TRUE)
W <- NULL
w.handler <- function(w){ # warning handler
W <<- w # send to the above env, i.e., the inside of the fun.warning.capture function
invokeRestart("muffleWarning") # here w.handler() muffles all the warnings. See http://romainfrancois.blog.free.fr/index.php?post/2009/05/20/Disable-specific-warnings to muffle specific warnings and print others
}
output <- list(
value = suppressMessages(withCallingHandlers(tryCatch(expr, error = function(e){e}), warning = w.handler)), # BEWARE: w.handler is a function written without (), like in other functions with FUN argument
warning = W # processed by w.handler()
)
return(if(is.null(output$warning)){NULL}else{as.character(output$warning)})
}
tempo.warn <- fun.warning.capture(eval(parse(text = data), envir = if(is.null(env)){parent.frame()}else{env}))
# warn.options.ini <- options()$warn ; options(warn = 1) ; tempo.warn <- utils::capture.output({tempo <- suppressMessages(eval(parse(text = data), envir = if(is.null(env)){parent.frame()}else{env}))}, type = "message") ; options(warn = warn.options.ini) # this recover warnings not messages and not errors but does not work in all enviroments
tempo.message <- utils::capture.output({
tempo <- suppressMessages(suppressWarnings(eval(parse(text = data), envir = if(is.null(env)){parent.frame()}else{env})))
if(any(class(tempo) %in% c("gg", "ggplot"))){
tempo <- ggplot2::ggplot_build(tempo)
}else{
tempo <- suppressWarnings(eval(parse(text = data), envir = if(is.null(env)){parent.frame()}else{env}))
}
}, type = "message") # recover messages not warnings and not errors
if(kind == "warning" & ! is.null(tempo.warn)){
if(length(tempo.warn) > 0){ # to avoid character(0)
if( ! any(sapply(tempo.warn, FUN = "grepl", pattern = "() FUNCTION:$"))){
tempo.warn <- paste(unique(tempo.warn), collapse = "\n") # if FALSE, means that the tested data is a special function. If TRUE, means that the data is a standard function. In that case, the output of capture.output() is two strings per warning messages: if several warning messages -> identical first string, which is removed in next messages by unique()
}else{
tempo.warn <- paste(tempo.warn, collapse = "\n")
}
if(header == TRUE){
if(any(grepl(x = tempo.warn[[1]], pattern = "^simpleWarning i"))){
tempo.warn[[1]] <- gsub(x = tempo.warn[[1]], pattern = "^Warning i", replacement = "I")
}
if(any(grepl(x = tempo.warn[[1]], pattern = "^Warning i"))){
tempo.warn[[1]] <- gsub(x = tempo.warn[[1]], pattern = "^Warning i", replacement = "I")
}
output <- paste0("WARNING MESSAGE REPORTED", ifelse(is.null(text), "", " "), text, ":\n", tempo.warn) #
}else{
output <- tempo.warn #
}
}else if(print.no == TRUE){
output <- paste0("NO WARNING MESSAGE REPORTED", ifelse(is.null(text), "", " "), text)
}
}else if(kind == "warning" & is.null(tempo.warn) & print.no == TRUE){
output <- paste0("NO WARNING MESSAGE REPORTED", ifelse(is.null(text), "", " "), text)
}else if(kind == "message" & exists("tempo.message", inherit = FALSE) == TRUE){ # inherit = FALSE avoid the portee lexical and thus the declared word
if(length(tempo.message) > 0){ # if something is returned by capture.ouptput() (only in this env) with a length more than 1
if(header == TRUE){
output <- paste0("STANDARD (NON ERROR AND NON WARNING) MESSAGE REPORTED", ifelse(is.null(text), "", " "), text, ":\n", tempo.message) #
}else{
output <- tempo.message #
}
}else if(print.no == TRUE){
output <- paste0("NO STANDARD (NON ERROR AND NON WARNING) MESSAGE REPORTED", ifelse(is.null(text), "", " "), text)
}
}else if(kind == "message" & exists("tempo.message", inherit = FALSE) == FALSE & print.no == TRUE){
output <- paste0("NO STANDARD (NON ERROR AND NON WARNING) MESSAGE REPORTED", ifelse(is.null(text), "", " "), text)
}
}
invisible(dev.off(window.nb)) # end send plots into a NULL file
return(output) # do not use cat() because the idea is to reuse the message
}
































# https://www.tidyverse.org/blog/2020/03/ggplot2-3-3-0/
# https://ggplot2.tidyverse.org/news/index.html


fun_gg_boxplot <- function(
data1, 
y, 
categ, 
categ.class.order = NULL, 
categ.legend.name = NULL, 
categ.color = NULL, 
box.fill = FALSE, 
box.width = 0.5, 
box.space = 0.1, 
box.line.size = 0.5, 
box.notch = FALSE, 
box.alpha = 1, 
box.mean = TRUE, 
box.whisker.kind = "std", 
box.whisker.width = 0, 
dot.color = "black", 
dot.categ = NULL, 
dot.categ.class.order = NULL, 
dot.categ.legend.name = NULL, 
dot.tidy = FALSE, 
dot.tidy.bin.nb = 50, 
dot.jitter = 0.5, 
dot.size = 3, 
dot.alpha = 0.5, 
dot.border.size = 0.5, 
dot.border.color = NULL, 
x.lab = NULL, 
y.lab = NULL, 
y.lim = NULL, 
y.log = "no", 
y.tick.nb = NULL, 
y.inter.tick.nb = NULL, 
y.include.zero = FALSE, 
y.top.extra.margin = 0.05, 
y.bottom.extra.margin = 0.05, 
stat.disp = NULL, 
stat.disp.mean = FALSE, 
stat.size = 4, 
stat.dist = 2, 
vertical = TRUE, 
text.size = 12, 
text.angle = 0, 
title = "", 
title.text.size = 8, 
article = TRUE, 
grid = FALSE, 
return = FALSE, 
plot = TRUE, 
add = NULL, 
warn.print = TRUE, 
lib.path = NULL
){
# AIM
# ggplot2 boxplot with the possibility to add background or foreground dots
# for ggplot2 specifications, see: https://ggplot2.tidyverse.org/articles/ggplot2-specs.html
# WARNINGS
# Rows containing NA in data1[, c(y, categ)] will be removed before processing, with a warning (see below)
# Hinges are not computed like in the classical boxplot() function of R
# To have a single box, create a factor column with a single class and specify the name of this column in the categ argument. For a single set of grouped boxs, create a factor column with a single class and specify this column in categ argument as first element (i.e., as categ1, knowing that categ2 must also be specified in this situation). See categ below
# with separated boxs (categ argument with only one element), box.width argument defines each box width. The box.width argument also defines the space between boxs by using (1 - box.width). In addition, xmin and xmax of the fun_gg_boxplot() output report the box boundaries (around x-axis unit 1, 2, 3, etc., for each box)
# with grouped boxs (categ argument with two elements), box.width argument defines each set of grouped box width. The box.width argument also defines the space between set of grouped boxs by using (1 - box.width). In addition, xmin and xmax of the fun_gg_boxplot() output report the box boundaries (around x-axis unit 1, 2, 3, etc., for each set of grouped box)
# The dot.alpha argument can alter the display of the color boxes when using pdf output
# ARGUMENTS
# data1: dataframe containing one column of values (see y argument below) and one or two columns of categories (see categ argument below). Duplicated column names are not allowed
# y: character string of the data1 column name for y-axis (column containing numeric values). Numeric values will be split according to the classes of the column names indicated in the categ argument to generate the boxs and will also be used to plot the dots
# categ: vector of character strings of the data1 column name for categories (column of characters or factor). Must be either one or two column names. If a single column name (further refered to as categ1), then one box per class of categ1. If two column names (further refered to as categ1 and categ2), then one box per class of categ2, which form a group of boxs in each class of categ1. BEWARE: no empty classes allowed. To have a single box, create a factor column with a single class and specify the name of this column in the categ argument (here, no categ2 in categ argument). For a single set of grouped boxs, create a factor column with a single class and specify this column in categ argument as first element (i.e., as categ1, knowing that categ2 must also be specified in this situation)
# categ.class.order: list indicating the order of the classes of categ1 and categ2 represented on the boxplot (the first compartment for categ1 and and the second for categ2). If categ.class.order == NULL, classes are represented according to the alphabetical order. Some compartment can be NULL and other not
# categ.legend.name: character string of the legend title for categ2. If categ.legend.name == NULL, then categ.legend.name <- categ1 if only categ1 is present, and categ.legend.name <- categ2 if categ1 and categ2 are present. Write "" if no legend required
# categ.color: vector of color character string for box frame
#If categ.color == NULL, default colors of ggplot2, whatever categ1 and categ2
# If categ.color is non null and only categ1 in categ argument, categ.color can be either: (1) a single color string (all the boxs will have this color, whatever the number of classes of categ1), (2) a vector of string colors, one for each class of categ1 (each color will be associated according to categ.class.order of categ1), (3) a vector or factor of string colors, like if it was one of the column of data1 data frame (beware: a single color per class of categ1 and a single class of categ1 per color must be respected). Integers are also accepted instead of character strings, as long as above rules about length are respected. Integers will be processed by fun_gg_palette() using the max integer value among all the integers in categ.color
# If categ.color is non null and categ1 and categ2 specified, all the rules described above will apply to categ2 instead of categ1 (colors will be determined for boxs inside a group of boxs)
# box.fill: logical. Fill the box? If TRUE, the categ.color argument will be used to generate filled boxplot (the box frames being black) as well as filled outlier dots (the dot border being controled by the dot.border.color argument) and if all the dots are plotted (argument dot.color other than NULL), they will be over the boxes. If FALSE, the categ.color argument will be used to color the box frames and the outlier dot borders, and if all the dots are plotted, they will be beneath the boxes
# box.width: numeric value (from 0 to 1) of the box or set of grouped box width (see warnings above)
# box.space: numeric value (from 0 to 1) indicating the box separation in grouped boxes. 0 means no space and 1 means boxes shrinked to a vertical line. Ignored if no grouped boxes
# box.line.size: numeric value of line size of boxes and whiskers (in mm)
# box.notch: logical. Notched boxplot? It TRUE, display notched boxplot, the notches corresponding approximately to the 95% confidence interval of the median (the notch interval is exactly 1.58 x Inter Quartile Range (IQR) / sqrt(n), with n the number of values that made the box). If notch intervals between two boxes do not overlap, it can be interpreted as significant median differences
# box.alpha: numeric value (from 0 to 1) of box transparency (full transparent to full opaque, respectively). BEWARE: work only for the fill of boxplots, not for the frame. See https://github.com/tidyverse/ggplot2/issues/252
# box.mean: logical. Add mean value? It TRUE, a losange dot, additional to the solid median bar and corresponding to the mean value, is incorporated into each boxplot
# box.whisker.kind: range of the whiskers. Either "no" (no whiskers), or "std" (length of each whisker equal to 1.5 x Inter Quartile Range (IQR)), or "max" (length of the whiskers up or down to the most distant dot)
# box.whisker.width: numeric value (from 0 to 1) of the whisker width, with 0 meaning no whiskers and 1 meaning a width equal to the corresponding boxplot width
# dot.color: vector of color character string for color of dots.
# If NULL, no dots plotted
# If "same", the dots will have the same colors as the respective boxplots
# Otherwise, colors will depend on the dot.categ argument. If dot.categ is NULL, then colors will be applied to each class of the last column name specified in categ. If dot.categ is non NULL, colors will be applied to each class of the column name specified in dot.categ. Color strings can be (1), (2) or (3) of categ.color argument, except that in the possibility (3), the rule "a single color per class of categ and a single class of categ per color", does not have to be respected (for instance, each dot can have a different color). See examples Put in example or leave here? -> Example: with categ = "Group1", dot.color = "red" and dot.categ = NULL, all the dots will be red, whatever the classes in Group1 column of data1, and no legend will be display for dots. With categ = c("Group1", "Group2"), dot.color = c("red", "blue") and dot.categ = NULL, the dots will be red for first class of Group2 and blue for the 2nd class of Group2, and no legend will be display for dots. With categ = c("Group1", "Group2"), dot.color = c("red", "blue") and dot.categ = "Group1", the dots will be red for first class of Group1 and blue for the 2nd class of Group1, and a legend will be display for dots
# dot.categ: optional single character string of a data1 column name (further refered to as categ3), which is associated to the dot.color argument to generate a legend for dots. If non NULL, then a legend will be created for the dots, in addition to the legend for the boxes. If NULL, no legend created and the colors of dot will depend on dot.color and categ arguments (see the explanation in dot.color)
# dot.categ.class.order: optional vector of character strings indicating the order of the classes of categ3. If dot.categ is non NULL and dot.categ.class.order is NULL, classes are displayed in the legend according to the alphabetical order. Ignored if dot.categ is NULL
# dot.categ.legend.name: optional character string of the legend title for categ3. If categ.legend.name == NULL, categ3 value is used (name of the column in data1). Write "" if no legend required. Ignored if dot.categ is NULL
# dot.tidy: logical. Nice dot spreading? If TRUE, use the geom_dotplot() function for a nice representation. BEWARE: change the true coordinates of dots that are aligned. Thus the gain in aestheticism is associated with a loss in precision that can be very important. If FALSE, dots are randomly spread, using the dot.jitter argument (see below) keeping the true dot coordinates
# dot.tidy.bin.nb: positive integer indicating the number of bins (i.e., nb of separations) of the y.lim range. Each dot will then be put in one of the bin, with the size the width of the bin. In other words, increase the number to have smaller dots. Not considered if dot.tidy is FALSE
# dot.jitter: numeric value (from 0 to 1) of random dot horizontal dispersion, with 0 meaning no dispersion and 1 meaning a dispersion in the corresponding box width interval. Not considered if dot.tidy is TRUE
# dot.size: numeric value of dot size (in mm). Not considered if dot.tidy is TRUE
# dot.alpha: numeric value (from 0 to 1) of dot transparency (full transparent to full opaque, respectively)
# dot.border.size: numeric value of border dot size (in mm). Write zero for no dot border. If dot.tidy is TRUE, value 0 remove the border. Another one leave the border without size control (geom_doplot() feature)
# dot.border.color: single character color string defining the color of the dot border (same color for all the dots, whatever their categories). If dot.border.color == NULL, the border color will be the same as the dot color. A single integer is also accepted instead of a character string, that will be processed by fun_gg_palette()
# x.lab: a character string or expression for x-axis legend. If NULL, character string of categ1
# y.lab: a character string or expression for y-axis legend. If NULL, character string of the y argument
# y.lim: 2 numeric values indicating the range of the y-axis
# y.log: Either "no" (values in the y argument column of the data1 data frame are not log), "log2" (values in the y argument column of the data1 data frame are log2 transformed) or "log10" (values in the y argument column of the data1 data frame are log10 transformed). BEWARE: do not tranform the data, but just display ticks in a log scale manner. Thus, negative or zero values allowed. BEWARE: not possible to have horizontal boxs with a log axis, due to a bug in ggplot2 (see https://github.com/tidyverse/ggplot2/issues/881)
# y.tick.nb: approximate number of desired label values (i.e., main ticks) on the y-axis (n argument of the the cute::fun_scale() function). BEWARE: provide this number even if y.log is "log2" or "log10", which can be difficult to read (e.g., ..., 2^2, 2^2.5, 2^3, ...). If NULL and if y.log is "no", then the number of label values is set by ggplot2. If NULL and if y.log is "log2" or "log10", then the number of label values correspond to integer units between y.lim (e.g., ..., 2^1, 2^2, 2^3, ...)
# y.inter.tick.nb: number of desired secondary ticks between main ticks. Ignored if y.log is other than "no" (log scale plotted)
# y.include.zero: logical. Does y.lim range include 0? Ok even if y.log == TRUE because y.lim must already be log transformed values
# y.top.extra.margin: single proportion (between 0 and 1) indicating if extra margins must be added to y.lim. If different from 0, add the range of the axis * y.top.extra.margin (e.g., abs(y.lim[2] - y.lim[1]) * y.top.extra.margin) to the top of y-axis
# y.bottom.extra.margin: idem as y.top.extra.margin but to the bottom of y-axis
# stat.disp: add the median number above the corresponding box. Either NULL (no number shown), "top" (at the top of the figure region) or "above" (above each box)
# stat.disp.mean: logical. Diplay means instead of medians ?
# stat.size: numeric value of the stat size (in points). Increase the value to increase text size
# stat.dist: numeric value of the stat distance. Increase the value to increase the distance from the box plot
# vertical: logical. Vertical boxs? BEWARE: will be automatically set to TRUE if y.log argument is other than "no". Indeed, not possible to have horizontal boxs with a log axis, due to a bug in ggplot2 (see https://github.com/tidyverse/ggplot2/issues/881)
# text.size: numeric value of the size of the (1) axis numbers and axis legends, (2) texts in the graphic legend, (3) stats above boxs (in points)
# text.angle: integer value of the text angle for the x-axis labels. Positive values for counterclockwise rotation: 0 for horizontal, 90 for vertical, 180 for upside down etc. Negative values for clockwise rotation: 0 for horizontal, -90 for vertical, -180 for upside down etc.
# title: character string of the graph title
# title.text.size: numeric value of the title size (in points)
# article: logical. If TRUE, use a article theme (article like). If FALSE, use a classic related ggplot theme. Use the add argument (add = "+ggplot2::theme_classic()" for the exact classic ggplot theme
# grid: logical. Draw horizontal lines in the background to better read the box values? Not considered if article == FALSE
# return: logical. Return the graph parameters?
# plot: logical. Plot the graphic? If FALSE and return argument is TRUE, graphical parameters and associated warnings are provided without plotting
# add: character string allowing to add more ggplot2 features (dots, lines, themes, facet, etc.). Ignored if NULL
# BEWARE: (1) the string must start with "+", (2) the string must finish with ")" and (3) each function must be preceded by "ggplot2::". Example: "+ ggplot2::coord_flip() + ggplot2::theme_bw()"
# If the character string contains the "ggplot2::theme" string, then the article argument of fun_gg_boxplot() (see above) is ignored with a warning
# Handle the add argument with caution since added functions can create conflicts with the preexisting internal ggplot2 functions
# warn.print: logical. Print warnings at the end of the execution? No print if no warning messages. some of the warning messages (those delivered by the internal ggplot2 functions) are not apparent when using the argument plot = FALSE
# lib.path: character string indicating the absolute path of the required packages (see below). if NULL, the function will use the R library default folders
# REQUIRED PACKAGES
# ggplot2
# scales
# REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
# fun_comp_2d()
# fun_gg_just()
# fun_gg_palette()
# fun_name_change()
# fun_pack()
# fun_check()
# fun_round()
# fun_scale()
# RETURN
# a boxplot if plot argument is TRUE
# a list of the graph info if return argument is TRUE:
# $data: the initial data
# $stat: the graphic statistics
# $removed.row.nb: which rows have been removed due to NA detection in y and categ columns (NULL if no row removed)
# $removed.rows: removed rows containing NA (NULL if no row removed)
# $plot: the graphic box and dot coordinates
# $dots: dot coordinates
# $main.box: coordinates of boxes
# $median: median coordinates
# $sup.whisker: coordinates of top whiskers (y for base and y.end for extremities)
# $inf.whisker: coordinates of bottom whiskers (y for base and y.end for extremities)
# $sup.whisker.edge: coordinates of top whisker edges (x and xend)
# $inf.whisker.edge: coordinates of bottom whisker edges(x and xend)
# $mean: diamon mean coordinates (only if box.mean argument is TRUE)
# $stat.display.positive: coordinates of stat numbers when positive (only if stat.disp argument is TRUE)
# $stat.display.negative: coordinates of stat numbers when negative (only if stat.disp argument is TRUE)
# y.second.tick.positions: coordinates of secondary ticks (only if y.inter.tick.nb argument is non NULL or if y.log argument is different from "no")
# y.second.tick.values: values of secondary ticks. NULL except if y.inter.tick.nb argument is non NULL or if y.log argument is different from "no")
# $panel: the variable names used for the panels (NULL if no panels)
# $axes: the x-axis and y-axis info
# $warn: the warning messages. Use cat() for proper display. NULL if no warning. BEWARE: some of the warning messages (those delivered by the internal ggplot2 functions) are not apparent when using the argument plot = FALSE
# EXAMPLE
# obs1 <- data.frame(x = 1:20, Group1 = rep(c("G", "H"), times = 10), Group2 = rep(c("A", "B"), each = 10)) ; fun_gg_boxplot(data1 = obs1, y = "x", categ = c("Group1", "Group2"), categ.class.order = list(NULL, c("B", "A")), categ.legend.name = "", categ.color = c("red", "blue"),box.fill = FALSE, box.width = 0.5, box.space = 0.1, box.line.size = 0.5, box.notch = FALSE, box.alpha = 1, box.mean = TRUE, box.whisker.kind = "std", box.whisker.width = 0, dot.color = "black", dot.categ = NULL, dot.categ.class.order = NULL, dot.categ.legend.name = NULL, dot.tidy = TRUE, dot.tidy.bin.nb = 50, dot.jitter = 0.5, dot.size = 3, dot.alpha = 0.5, dot.border.size = 0.5, dot.border.color = NULL, x.lab = NULL, y.lab = NULL, y.lim = NULL, y.log = "no", y.tick.nb = NULL, y.inter.tick.nb = NULL, y.include.zero = FALSE, y.top.extra.margin = 0.05, y.bottom.extra.margin = 0.05, stat.disp = NULL, stat.disp.mean = FALSE, stat.size = 4, stat.dist = 2, vertical = TRUE, text.size = 12, text.angle = 0, title = "", title.text.size = 8, article = TRUE, grid = FALSE, return = FALSE, plot = TRUE, add = NULL, warn.print = TRUE, lib.path = NULL)
# DEBUGGING
# set.seed(1) ; obs1 <- data.frame(Time = c(rnorm(10), rnorm(10) + 2), Group1 = rep(c("G", "H"), each = 10)) ; set.seed(NULL) ; obs1$Time[1:10] <- NA ; data1 = obs1 ; y = "Time" ; categ = c("Group1") ; categ.class.order = NULL ; categ.legend.name = NULL ; categ.color = c("green") ; box.fill = FALSE ; box.width = 0.5 ; box.space = 0.1 ; box.notch = FALSE ; box.line.size = 0.5 ; box.alpha = 0.5 ; box.mean = TRUE ; box.whisker.kind = "std" ; box.whisker.width = 0.5 ; dot.color = "black" ; dot.categ = "Group1"; dot.categ.class.order = c("G", "H") ; dot.categ.legend.name = NULL ; dot.tidy = TRUE ; dot.tidy.bin.nb = 50 ; dot.jitter = 0.25 ; dot.size = 3 ;  dot.alpha = 0.5 ; dot.border.size = 0.5 ; dot.border.color = NULL ; y.lim = NULL ; y.log = "no" ; y.tick.nb = NULL ; y.inter.tick.nb = NULL ; y.include.zero = FALSE ; y.top.extra.margin = 0.05 ; y.bottom.extra.margin = 0.05 ; stat.disp = NULL ; stat.disp.mean = FALSE ; stat.size = 4 ; stat.dist = 2 ; x.lab = NULL ; y.lab = NULL ; vertical = TRUE ; text.size = 12 ; title = "" ; title.text.size = 8 ; text.angle = 0 ; article = FALSE ; grid = FALSE ; return = TRUE ; plot = TRUE ; add = NULL ; warn.print = FALSE ; lib.path = NULL
# set.seed(1) ; obs1 <- data.frame(Time = c(rnorm(10), rnorm(10) + 2), Group1 = rep(c("G", "H"), each = 10), Group2 = rep(c("A", "B"), time = 10), Group3 = rep(c("I", "J"), time = 10)) ; set.seed(NULL) ; obs1$Time[1:10] <- NA ; data1 = obs1 ; y = "Time" ; categ = c("Group1", "Group2") ; categ.class.order = list(c("G", "H"), c("A", "B")); categ.legend.name = NULL ; categ.color = c("green", "blue") ; box.fill = FALSE ; box.width = 0.5 ; box.space = 0.1 ; box.notch = FALSE ; box.line.size = 0.5 ; box.alpha = 0.5 ; box.mean = TRUE ; box.whisker.kind = "std" ; box.whisker.width = 0.5 ; dot.color = "black" ; dot.categ = "Group1" ; dot.categ.class.order = NULL ; dot.categ.legend.name = NULL ; dot.tidy = TRUE ; dot.tidy.bin.nb = 30 ; dot.jitter = 0.25 ; dot.size = 3 ;  dot.alpha = 0.5 ; dot.border.size = 0.5 ; dot.border.color = NULL ; y.lim = NULL ; y.log = "no" ; y.tick.nb = NULL ; y.inter.tick.nb = NULL ; y.include.zero = FALSE ; y.top.extra.margin = 0.05 ; y.bottom.extra.margin = 0.05 ; stat.disp = NULL ; stat.disp.mean = FALSE ; stat.size = 4 ; stat.dist = 2 ; x.lab = NULL ; y.lab = NULL ; vertical = TRUE ; text.size = 12 ; title = "" ; title.text.size = 8 ; text.angle = 0 ; article = FALSE ; grid = FALSE ; return = FALSE ; plot = TRUE ; add = NULL ; warn.print = FALSE ; lib.path = NULL
# set.seed(1) ; obs1 <- data.frame(Time = c(rnorm(10), rnorm(10) + 2), Group1 = rep(c("G", "H"), each = 10), Group2 = rep(c("A", "B"), time = 10)) ; set.seed(NULL) ; data1 = obs1 ; y = "Time" ; categ = c("Group1") ; categ.class.order = list(c("H", "G")); categ.legend.name = NULL ; categ.color = c("blue") ; box.fill = FALSE ; box.width = 0.5 ; box.space = 0.1 ; box.notch = TRUE ; box.line.size = 1 ; box.alpha = 1 ; box.mean = FALSE ; box.whisker.kind = "max" ; box.whisker.width = 0 ; dot.color = "black" ; dot.categ = "Group1" ; dot.categ.class.order = NULL ; dot.categ.legend.name = NULL ; dot.tidy = TRUE ; dot.tidy.bin.nb = 30 ; dot.jitter = 0.25 ; dot.size = 3 ;  dot.alpha = 0.5 ; dot.border.size = 0.5 ; dot.border.color = NULL ; y.lim = NULL ; y.log = "no" ; y.tick.nb = NULL ; y.inter.tick.nb = NULL ; y.include.zero = FALSE ; y.top.extra.margin = 0.05 ; y.bottom.extra.margin = 0.05 ; stat.disp = NULL ; stat.disp.mean = FALSE ; stat.size = 4 ; stat.dist = 2 ; x.lab = NULL ; y.lab = NULL ; vertical = TRUE ; text.size = 12 ; title = "" ; title.text.size = 8 ; text.angle = 0 ; article = FALSE ; grid = FALSE ; return = FALSE ; plot = TRUE ; add = NULL ; warn.print = FALSE ; lib.path = NULL
# function name
function.name <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
# end function name
# required function checking
req.function <- c(
"fun_comp_2d", 
"fun_gg_just", 
"fun_gg_palette", 
"fun_name_change", 
"fun_pack", 
"fun_check", 
"fun_round", 
"fun_scale"
)
for(i1 in req.function){
if(length(find(i1, mode = "function")) == 0){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED ", i1, "() FUNCTION IS MISSING IN THE R ENVIRONMENT\n\n================\n\n")
stop(tempo.cat)
}
}
# end required function checking
# reserved words to avoid bugs (names of dataframe columns used in this function)
reserved.words <- c("categ.check", "categ.color", "dot.color", "dot.categ", "dot.max", "dot.min", "group", "PANEL", "group.check", "MEAN", "tempo.categ1", "tempo.categ2", "text.max.pos", "text.min.pos", "x", "x.y", "y", "y.check", "y_from.dot.max", "ymax", "tidy_group")
# end reserved words to avoid bugs (used in this function)
# argument primary checking
arg.check <- NULL #
text.check <- NULL #
checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$fun.name))
tempo <- fun_check(data = data1, class = "data.frame", na.contain = TRUE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = y, class = "vector", mode = "character", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = categ, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
if( ! is.null(categ.class.order)){
tempo <- fun_check(data = categ.class.order, class = "list", fun.name = function.name) ; eval(ee)
}
if( ! is.null(categ.legend.name)){
tempo <- fun_check(data = categ.legend.name, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
}
if( ! is.null(categ.color)){
tempo1 <- fun_check(data = categ.color, class = "vector", mode = "character", na.contain = TRUE, fun.name = function.name, print = FALSE)
tempo2 <- fun_check(data = categ.color, class = "factor", na.contain = TRUE, fun.name = function.name, print = FALSE)
if(tempo1$problem == TRUE & tempo2$problem == TRUE){
tempo.check.color <- fun_check(data = categ.color, class = "integer", double.as.integer.allowed = TRUE, na.contain = TRUE, fun.name = function.name, print = FALSE)$problem
if(tempo.check.color == TRUE){
tempo.cat <- paste0("ERROR IN ", function.name, ": categ.color MUST BE A FACTOR OR CHARACTER VECTOR OR INTEGER VECTOR") # integer possible because dealt above
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
}
}
tempo <- fun_check(data = box.fill, class = "vector", mode = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = box.width, prop = TRUE, length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = box.space, prop = TRUE, length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = box.line.size, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = box.notch, class = "vector", mode = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = box.alpha, prop = TRUE, length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = box.mean, class = "vector", mode = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = box.whisker.kind, options = c("no", "std", "max"), length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = box.whisker.width, prop = TRUE, length = 1, fun.name = function.name) ; eval(ee)
if( ! is.null(dot.color)){
tempo1 <- fun_check(data = dot.color, class = "vector", mode = "character", na.contain = TRUE, fun.name = function.name, print = FALSE)
tempo2 <- fun_check(data = dot.color, class = "factor", na.contain = TRUE, fun.name = function.name, print = FALSE)
if(tempo1$problem == TRUE & tempo2$problem == TRUE){
tempo.check.color <- fun_check(data = dot.color, class = "integer", double.as.integer.allowed = TRUE, na.contain = TRUE, fun.name = function.name, print = FALSE)$problem
if(tempo.check.color == TRUE){
tempo.cat <- paste0("ERROR IN ", function.name, ": dot.color MUST BE A FACTOR OR CHARACTER VECTOR OR INTEGER VECTOR") # integer possible because dealt above
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
}
}
if( ! is.null(dot.categ)){
tempo <- fun_check(data = dot.categ, class = "vector", mode = "character", length = 1, fun.name = function.name) ; eval(ee)
}
if( ! is.null(dot.categ.class.order)){
tempo <- fun_check(data = dot.categ.class.order, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
}
if( ! is.null(dot.categ.legend.name)){
tempo <- fun_check(data = dot.categ.legend.name, class = "vector", mode = "character", length = 1, fun.name = function.name) ; eval(ee)
}
tempo <- fun_check(data = dot.tidy, class = "vector", mode = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = dot.tidy.bin.nb, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = dot.jitter, prop = TRUE, length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = dot.size, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = dot.alpha, prop = TRUE, length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = dot.border.size, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
if( ! is.null(dot.border.color)){
tempo1 <- fun_check(data = dot.border.color, class = "vector", mode = "character", length = 1, fun.name = function.name, print = FALSE)
tempo2 <- fun_check(data = dot.border.color, class = "vector", typeof = "integer", double.as.integer.allowed = TRUE, length = 1, fun.name = function.name, print = FALSE)
if(tempo1$problem == TRUE & tempo2$problem == TRUE){
# integer colors into gg_palette
tempo.cat <- paste0("ERROR IN ", function.name, ": dot.border.color MUST BE A SINGLE CHARACTER STRING OF COLOR OR A SINGLE INTEGER VALUE") # integer possible because dealt above
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
}
if( ! is.null(x.lab)){
if(all(class(x.lab) %in% "expression")){ # to deal with math symbols
tempo <- fun_check(data = x.lab, class = "expression", length = 1, fun.name = function.name) ; eval(ee)
}else{
tempo <- fun_check(data = x.lab, class = "vector", mode = "character", length = 1, fun.name = function.name) ; eval(ee)
}
}
if( ! is.null(y.lab)){
if(all(class(y.lab) %in% "expression")){ # to deal with math symbols
tempo <- fun_check(data = y.lab, class = "expression", length = 1, fun.name = function.name) ; eval(ee)
}else{
tempo <- fun_check(data = y.lab, class = "vector", mode = "character", length = 1, fun.name = function.name) ; eval(ee)
}
}
if( ! is.null(y.lim)){
tempo <- fun_check(data = y.lim, class = "vector", mode = "numeric", length = 2, fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE & any(y.lim %in% c(Inf, -Inf))){
tempo.cat <- paste0("ERROR IN ", function.name, ": y.lim ARGUMENT CANNOT CONTAIN -Inf OR Inf VALUES")
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
}
tempo <- fun_check(data = y.log, options = c("no", "log2", "log10"), length = 1, fun.name = function.name) ; eval(ee)
if( ! is.null(y.tick.nb)){
tempo <- fun_check(data = y.tick.nb, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE & y.tick.nb < 0){
tempo.cat <- paste0("ERROR IN ", function.name, ": y.tick.nb ARGUMENT MUST BE A NON NULL POSITIVE INTEGER")
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
}
if( ! is.null(y.inter.tick.nb)){
tempo <- fun_check(data = y.inter.tick.nb, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE & y.inter.tick.nb < 0){
tempo.cat <- paste0("ERROR IN ", function.name, ": y.inter.tick.nb ARGUMENT MUST BE A NON NULL POSITIVE INTEGER")
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
}
tempo <- fun_check(data = y.include.zero, class = "vector", mode = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = y.top.extra.margin, prop = TRUE, length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = y.bottom.extra.margin, prop = TRUE, length = 1, fun.name = function.name) ; eval(ee)
if( ! is.null(stat.disp)){
tempo <- fun_check(data = stat.disp, options = c("top", "above"), length = 1, fun.name = function.name) ; eval(ee)
}
tempo <- fun_check(data = stat.disp.mean, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = stat.size, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = stat.dist, class = "vector", mode = "numeric", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = vertical, class = "vector", mode = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = text.size, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = text.angle, class = "vector", typeof = "integer", double.as.integer.allowed = TRUE, length = 1, neg.values = TRUE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = title, class = "vector", mode = "character", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = title.text.size, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = article, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = grid, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = return, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
tempo <- fun_check(data = plot, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
if( ! is.null(add)){
tempo <- fun_check(data = add, class = "vector", mode = "character", length = 1, fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE & ! grepl(pattern = "^\\s*\\+", add)){ # check that the add string start by +
tempo.cat <- paste0("ERROR IN ", function.name, ": add ARGUMENT MUST START WITH \"+\": ", paste(unique(add), collapse = " "))
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}else if(tempo$problem == FALSE & ! grepl(pattern = "(ggplot2|lemon)\\s*::", add)){ #
tempo.cat <- paste0("ERROR IN ", function.name, ": FOR EASIER FUNCTION DETECTION, add ARGUMENT MUST CONTAIN \"ggplot2::\" OR \"lemon::\" IN FRONT OF EACH GGPLOT2 FUNCTION: ", paste(unique(add), collapse = " "))
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}else if(tempo$problem == FALSE & ! grepl(pattern = ")\\s*$", add)){ # check that the add string  finished by )
tempo.cat <- paste0("ERROR IN ", function.name, ": add ARGUMENT MUST FINISH BY \")\": ", paste(unique(add), collapse = " "))
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
}
tempo <- fun_check(data = warn.print, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
if( ! is.null(lib.path)){
tempo <- fun_check(data = lib.path, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
if(tempo$problem == FALSE){
if( ! all(dir.exists(lib.path))){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
tempo.cat <- paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", paste(lib.path, collapse = "\n"))
text.check <- c(text.check, tempo.cat)
arg.check <- c(arg.check, TRUE)
}
}
}
if(any(arg.check) == TRUE){
stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
}
# source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.2/r_debugging_tools-v1.2.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
# end argument primary checking
# second round of checking and data preparation
warn <- NULL
warn.count <- 0
if(any(duplicated(names(data1)))){
tempo.cat <- paste0("ERROR IN ", function.name, ": DUPLICATED COLUMN NAMES OF data1 ARGUMENT NOT ALLOWED:\n", paste(names(data1)[duplicated(names(data1))], collapse = " "))
stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
}
if( ! (y %in% names(data1))){
tempo.cat <- paste0("ERROR IN ", function.name, ": y ARGUMENT MUST BE A COLUMN NAME OF data1")
stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
}else{
tempo <- fun_check(data = data1[, y], data.name = "y COLUMN OF data1", class = "vector", mode = "numeric", na.contain = TRUE, fun.name = function.name) ; eval(ee)
}
if(length(categ) > 2){
tempo.cat <- paste0("ERROR IN ", function.name, ": categ ARGUMENT CANNOT HAVE MORE THAN 2 COLUMN NAMES OF data1")
stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
}else if( ! all(categ %in% names(data1))){
tempo.cat <- paste0("ERROR IN ", function.name, ": categ ARGUMENT MUST BE COLUMN NAMES OF data1. HERE IT IS:\n", paste(categ, collapse = " "))
stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
}
# reserved word checking
if(any(names(data1) %in% reserved.words)){
if(any(duplicated(names(data1)))){
tempo.cat <- paste0("ERROR IN ", function.name, ": DUPLICATED COLUMN NAMES OF data1 ARGUMENT NOT ALLOWED:\n", paste(names(data1)[duplicated(names(data1))], collapse = " "))
stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
}
if( ! is.null(dot.categ)){
if(dot.categ %in% categ){
reserved.words <- c(reserved.words, paste0(dot.categ, "_DOT")) # paste0(dot.categ, "_DOT") is added to the reserved words because in such situation, a new column will be added to data1 that is named paste0(dot.categ, "_DOT")
}
}
tempo.output <- fun_name_change(names(data1), reserved.words)
for(i2 in 1:length(tempo.output$ini)){ # a loop to be sure to take the good ones
names(data1)[names(data1) == tempo.output$ini[i2]] <- tempo.output$post[i2]
if(any(y == tempo.output$ini[i2])){
y[y == tempo.output$ini[i2]] <- tempo.output$post[i2]
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") IN y ARGUMENT (COLUMN NAMES OF data1 ARGUMENT),\n", tempo.output$ini[i2], " HAS BEEN REPLACED BY ", tempo.output$post[i2], "\nBECAUSE RISK OF BUG AS SOME NAMES IN y ARGUMENT ARE RESERVED WORD USED BY THE ", function.name, " FUNCTION")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}
# BEWARE: names of y argument potentially replaced
if(any(categ == tempo.output$ini[i2])){
categ[categ == tempo.output$ini[i2]] <- tempo.output$post[i2]
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") IN categ ARGUMENT (COLUMN NAMES OF data1 ARGUMENT),\n", tempo.output$ini[i2], " HAS BEEN REPLACED BY ", tempo.output$post[i2], "\nBECAUSE RISK OF BUG AS SOME NAMES IN categ ARGUMENT ARE RESERVED WORD USED BY THE ", function.name, " FUNCTION")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}
# BEWARE: names of categ argument potentially replaced
if( ! is.null(dot.categ)){
if(any(dot.categ == tempo.output$ini[i2])){
dot.categ[dot.categ == tempo.output$ini[i2]] <- tempo.output$post[i2]
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") IN dot.categ ARGUMENT (COLUMN NAMES OF data1 ARGUMENT),\n", tempo.output$ini[i2], " HAS BEEN REPLACED BY ", tempo.output$post[i2], "\nBECAUSE RISK OF BUG AS SOME NAMES IN dot.categ ARGUMENT ARE RESERVED WORD USED BY THE ", function.name, " FUNCTION")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}
}
# BEWARE: names of dot.categ argument potentially replaced
}
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") REGARDING COLUMN NAMES REPLACEMENT, THE NAMES\n", paste(tempo.output$ini, collapse = " "), "\nHAVE BEEN REPLACED BY\n", paste(tempo.output$post, collapse = " "))
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
if( ! (is.null(add) | is.null(tempo.output$ini))){
if(grepl(x = add, pattern = paste(tempo.output$ini, collapse = "|"))){
tempo.cat <- paste0("ERROR IN ", function.name, ": DETECTION OF COLUMN NAMES OF data1 IN THE add ARGUMENT STRING, THAT CORRESPOND TO RESERVED STRINGS FOR ", function.name, "\nCOLUMN NAMES HAVE TO BE CHANGED\nTHE PROBLEMATIC COLUMN NAMES ARE SOME OF THESE NAMES:\n", paste(tempo.output$ini, collapse = " "), "\nIN THE DATA FRAME OF data1 AND IN THE STRING OF add ARGUMENT, TRY TO REPLACE NAMES BY:\n", paste(tempo.output$post, collapse = " "), "\n\nFOR INFORMATION, THE RESERVED WORDS ARE:\n", paste(reserved.words, collapse = "\n"))
stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
}
}
}
# end reserved word checking
# management of add containing facet
facet.categ <- NULL
if( ! is.null(add)){
facet.check <- TRUE
tempo <- unlist(strsplit(x = add, split = "\\s*\\+\\s*(ggplot2|lemon)\\s*::\\s*")) #
tempo <- sub(x = tempo, pattern = "^facet_wrap", replacement = "ggplot2::facet_wrap")
tempo <- sub(x = tempo, pattern = "^facet_grid", replacement = "ggplot2::facet_grid")
tempo <- sub(x = tempo, pattern = "^facet_rep", replacement = "lemon::facet_rep")
if(any(grepl(x = tempo, pattern = "ggplot2::facet_wrap|lemon::facet_rep_wrap"))){
tempo1 <- suppressWarnings(eval(parse(text = tempo[grepl(x = tempo, pattern = "ggplot2::facet_wrap|lemon::facet_rep_wrap")])))
facet.categ <- names(tempo1$params$facets)
tempo.text <- "facet_wrap OR facet_rep_wrap"
facet.check <- FALSE
}else if(grepl(x = add, pattern = "ggplot2::facet_grid|lemon::facet_rep_grid")){
tempo1 <- suppressWarnings(eval(parse(text = tempo[grepl(x = tempo, pattern = "ggplot2::facet_grid|lemon::facet_rep_grid")])))
facet.categ <- c(names(tempo1$params$rows), names(tempo1$params$cols))
tempo.text <- "facet_grid OR facet_rep_grid"
facet.check <- FALSE
}
if(facet.check == FALSE & ! all(facet.categ %in% names(data1))){ # BEWARE: all(facet.categ %in% names(data1)) is TRUE when facet.categ is NULL
tempo.cat <- paste0("ERROR IN ", function.name, ": DETECTION OF \"", tempo.text, "\" STRING IN THE add ARGUMENT BUT PROBLEM OF VARIABLE DETECTION (COLUMN NAMES OF data1)\nTHE DETECTED VARIABLES ARE:\n", paste(facet.categ, collapse = " "), "\nTHE data1 COLUMN NAMES ARE:\n", paste(names(data1), collapse = " "), "\nPLEASE REWRITE THE add STRING AND RERUN")
stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
}
}
# end management of add containing facet
# conversion of categ columns in data1 into factors
for(i1 in 1:length(categ)){
tempo1 <- fun_check(data = data1[, categ[i1]], data.name = paste0("categ NUMBER ", i1, " OF data1"), class = "vector", mode = "character", na.contain = TRUE, fun.name = function.name, print = FALSE)
tempo2 <- fun_check(data = data1[, categ[i1]], data.name = paste0("categ NUMBER ", i1, " OF data1"), class = "factor", na.contain = TRUE, fun.name = function.name, print = FALSE)
if(tempo1$problem == TRUE & tempo2$problem == TRUE){
tempo.cat <- paste0("ERROR IN ", function.name, ": ", paste0("categ NUMBER ", i1, " OF data1"), " MUST BE A FACTOR OR CHARACTER VECTOR")
stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
}else if(tempo1$problem == FALSE){ # character vector
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") IN categ NUMBER ", i1, " IN data1, THE CHARACTER COLUMN HAS BEEN CONVERTED TO FACTOR, WITH LEVELS ACCORDING TO THE ALPHABETICAL ORDER")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}
data1[, categ[i1]] <- factor(data1[, categ[i1]]) # if already a factor, change nothing, if characters, levels according to alphabetical order
}
# OK: all the categ columns of data1 are factors from here
# end conversion of categ columns in data1 into factors
if( ! is.null(categ.class.order)){
if(length(categ.class.order) != length(categ)){
tempo.cat <- paste0("ERROR IN ", function.name, ": categ.class.order ARGUMENT MUST BE A LIST OF LENGTH EQUAL TO LENGTH OF categ\nHERE IT IS LENGTH: ", length(categ.class.order), " VERSUS ", length(categ))
stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
}else{
for(i3 in 1:length(categ.class.order)){
if(is.null(categ.class.order[[i3]])){
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") THE categ.class.order COMPARTMENT ", i3, " IS NULL. ALPHABETICAL ORDER WILL BE APPLIED")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
data1[, categ[i3]] <- factor(as.character(data1[, categ[i3]])) # if already a factor, change nothing, if characters, levels according to alphabetical order
categ.class.order[[i3]] <- levels(data1[, categ[i3]]) # character vector that will be used later
}else{
tempo <- fun_check(data = categ.class.order[[i3]], data.name = paste0("COMPARTMENT ", i3 , " OF categ.class.order ARGUMENT"), class = "vector", mode = "character", length = length(levels(data1[, categ[i3]])), fun.name = function.name) ; eval(ee) # length(data1[, categ[i1]) -> if data1[, categ[i1] was initially character vector, then conversion as factor after the NA removal, thus class number ok. If data1[, categ[i1] was initially factor, no modification after the NA removal, thus class number ok
}
if(any(duplicated(categ.class.order[[i3]]))){
tempo.cat <- paste0("ERROR IN ", function.name, ": COMPARTMENT ", i3, " OF categ.class.order ARGUMENT CANNOT HAVE DUPLICATED CLASSES: ", paste(categ.class.order[[i3]], collapse = " "))
stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
}else if( ! (all(categ.class.order[[i3]] %in% unique(data1[, categ[i3]])) & all(unique(data1[, categ[i3]]) %in% categ.class.order[[i3]]))){
tempo.cat <- paste0("ERROR IN ", function.name, ": COMPARTMENT ", i3, " OF categ.class.order ARGUMENT MUST BE CLASSES OF ELEMENT ", i3, " OF categ ARGUMENT\nHERE IT IS:\n", paste(categ.class.order[[i3]], collapse = " "), "\nFOR COMPARTMENT ", i3, " OF categ.class.order AND IT IS:\n", paste(unique(data1[, categ[i3]]), collapse = " "), "\nFOR COLUMN ", categ[i3], " OF data1")
stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
}else{
data1[, categ[i3]] <- factor(data1[, categ[i3]], levels = categ.class.order[[i3]]) # reorder the factor

}
names(categ.class.order)[i3] <- categ[i3]
}
}
}else{
categ.class.order <- vector("list", length = length(categ))
tempo.categ.class.order <- NULL
for(i2 in 1:length(categ.class.order)){
categ.class.order[[i2]] <- levels(data1[, categ[i2]])
names(categ.class.order)[i2] <- categ[i2]
tempo.categ.class.order <- c(tempo.categ.class.order, ifelse(i2 != 1, "\n", ""), categ.class.order[[i2]])
}
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") THE categ.class.order SETTING IS NULL. ALPHABETICAL ORDER WILL BE APPLIED FOR BOX ORDERING:\n", paste(tempo.categ.class.order, collapse = " "))
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}
# categ.class.order not NULL anymore (list)
if(is.null(categ.legend.name)){
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") THE categ.legend.name SETTING IS NULL. NAMES OF categ WILL BE USED: ", paste(categ, collapse = " "))
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
categ.legend.name <- categ[length(categ)] # if only categ1, then legend name of categ1, if length(categ) == 2, then legend name of categ2
}
# categ.legend.name not NULL anymore (character string)
# management of categ.color
if( ! is.null(categ.color)){
# check the nature of color
# integer colors into gg_palette
tempo.check.color <- fun_check(data = categ.color, class = "integer", double.as.integer.allowed = TRUE, na.contain = TRUE, fun.name = function.name, print = FALSE)$problem
if(tempo.check.color == FALSE){
# convert integers into colors
categ.color <- fun_gg_palette(max(categ.color, na.rm = TRUE))[categ.color]
}
# end integer colors into gg_palette
if( ! (all(categ.color %in% colors() | grepl(pattern = "^#", categ.color)))){ # check that all strings of low.color start by #
tempo.cat <- paste0("ERROR IN ", function.name, ": categ.color ARGUMENT MUST BE A HEXADECIMAL COLOR VECTOR STARTING BY # AND/OR COLOR NAMES GIVEN BY colors(): ", paste(unique(categ.color), collapse = " "))
stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
}
if(any(is.na(categ.color))){
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") categ.color ARGUMENT CONTAINS NA")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}
# end check the nature of color
# check the length of color
categ.len <- length(categ) # if only categ1, then colors for classes of categ1, if length(categ) == 2, then colors for classes of categ2
if(length(categ.color) == length(levels(data1[, categ[categ.len]]))){ # here length(categ.color) is equal to the different number of categ
# data1[, categ[categ.len]] <- factor(data1[, categ[categ.len]]) # not required because sure that is is a factor
data1 <- data.frame(data1, categ.color = data1[, categ[categ.len]])
data1$categ.color <- factor(data1$categ.color, labels = categ.color)
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") IN ", categ[categ.len], " OF categ ARGUMENT, THE FOLLOWING COLORS:\n", paste(categ.color, collapse = " "), "\nHAVE BEEN ATTRIBUTED TO THESE CLASSES:\n", paste(levels(factor(data1[, categ[categ.len]])), collapse = " "))
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}else if(length(categ.color) == length(data1[, categ[categ.len]])){# here length(categ.color) is equal to nrow(data1) -> Modif to have length(categ.color) equal to the different number of categ (length(categ.color) == length(levels(data1[, categ[categ.len]])))
data1 <- data.frame(data1, categ.color = categ.color)
tempo.check <- unique(data1[ , c(categ[categ.len], "categ.color")])
if( ! (nrow(tempo.check) == length(unique(categ.color)) & nrow(tempo.check) == length(unique(data1[ , categ[categ.len]])))){
tempo.cat <- paste0("ERROR IN ", function.name, ": categ.color ARGUMENT HAS THE LENGTH OF data1 ROW NUMBER\nBUT IS INCORRECTLY ASSOCIATED TO EACH CLASS OF categ ", categ[categ.len], ":\n", paste(unique(mapply(FUN = "paste", data1[ ,categ[categ.len]], data1[ ,"categ.color"])), collapse = "\n"))
stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
}else{
# data1[, categ[categ.len]] <- factor(data1[, categ[categ.len]]) # not required because sure that is is a factor
categ.color <- unique(data1$categ.color[order(data1[, categ[categ.len]])]) # Modif to have length(categ.color) equal to the different number of categ (length(categ.color) == length(levels(data1[, categ[categ.len]])))
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") categ.color ARGUMENT HAS THE LENGTH OF data1 ROW NUMBER\nCOLORS HAVE BEEN RESPECTIVELY ASSOCIATED TO EACH CLASS OF categ ", categ[categ.len], " AS:\n", paste(levels(factor(data1[, categ[categ.len]])), collapse = " "), "\n", paste(categ.color, collapse = " "))
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}
}else if(length(categ.color) == 1){
# data1[, categ[categ.len]] <- factor(data1[, categ[categ.len]]) # not required because sure that is is a factor
data1 <- data.frame(data1, categ.color = categ.color)
categ.color <- rep(categ.color, length(levels(data1[, categ[categ.len]])))
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") categ.color ARGUMENT HAS LENGTH 1, MEANING THAT ALL THE DIFFERENT CLASSES OF ", categ[categ.len], "\n", paste(levels(factor(data1[, categ[categ.len]])), collapse = " "), "\nWILL HAVE THE SAME COLOR\n", paste(categ.color, collapse = " "))
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}else{
tempo.cat <- paste0("ERROR IN ", function.name, ": categ.color ARGUMENT MUST BE (1) LENGTH 1, OR (2) THE LENGTH OF data1 NROWS, OR (3) THE LENGTH OF THE CLASSES IN THE categ ", categ[categ.len], " COLUMN. HERE IT IS COLOR LENGTH ", length(categ.color), " VERSUS CATEG LENGTH ", length(data1[, categ[categ.len]]), " AND CATEG CLASS LENGTH ", length(unique(data1[, categ[categ.len]])), "\nPRESENCE OF NA COULD BE THE PROBLEM")
stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
}
}else{
categ.len <- length(categ) # if only categ1, then colors for classes of categ1, if length(categ) == 2, then colors for classes of categ2
# data1[, categ[categ.len]] <- factor(data1[, categ[categ.len]]) # not required because sure that is is a factor
categ.color <- fun_gg_palette(length(levels(data1[, categ[categ.len]])))
data1 <- data.frame(data1, categ.color = data1[, categ[categ.len]])
data1$categ.color <- factor(data1$categ.color, labels = categ.color)
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") NULL categ.color ARGUMENT -> COLORS RESPECTIVELY ATTRIBUTED TO EACH CLASS OF ", categ[categ.len], " IN data1:\n", paste(categ.color, collapse = " "), "\n", paste(levels(data1[, categ[categ.len]]), collapse = " "))
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}
# categ.color not NULL anymore
categ.color <- as.character(categ.color)
# categ.color is a character string representing the diff classes
data1$categ.color <- factor(data1$categ.color, levels = unique(categ.color)) # ok because if categ.color is a character string, the order make class 1, class 2, etc. unique() because no duplicates allowed
# data1$categ.color is a factor with order of levels -> categ.color
# end management of categ.color
# management of dot.color
if( ! is.null(dot.color)){
# optional legend of dot colors
if( ! is.null(dot.categ)){
ini.dot.categ <- dot.categ
if( ! dot.categ %in% names(data1)){ # no need to use all() because length(dot.categ) = 1
tempo.cat <- paste0("ERROR IN ", function.name, ": dot.categ ARGUMENT MUST BE A COLUMN NAME OF data1. HERE IT IS:\n", dot.categ)
stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
}else if(dot.categ %in% categ){ # no need to use all() because length(dot.categ) = 1. Do not use dot.categ %in% categ[length(categ)] -> error
# management of dot legend if dot.categ %in% categ (because legends with the same name are joined in ggplot2) 
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") THE COLUMN NAME OF data1 INDICATED IN THE dot.categ ARGUMENT (", dot.categ, ") HAS BEEN REPLACED BY ", paste0(dot.categ, "_DOT"), " TO AVOID MERGED LEGEND BY GGPLOT2")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
data1 <- data.frame(data1, dot.categ = data1[, dot.categ]) # dot.categ is not a column name of data1 (checked above with reserved words)
dot.categ <- paste0(dot.categ, "_DOT")
names(data1)[names(data1) == "dot.categ"] <- dot.categ # paste0(dot.categ, "_DOT") is not a column name of data1 (checked above with reserved words)
# tempo.cat <- paste0("ERROR IN ", function.name, ": dot.categ ARGUMENT CANNOT BE A COLUMN NAME OF data1 ALREADY SPECIFIED IN THE categ ARGUMENT:\n", dot.categ, "\nINDEED, dot.categ ARGUMENT IS MADE TO HAVE MULTIPLE DOT COLORS NOT RELATED TO THE BOXPLOT CATEGORIES")
# stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
}
tempo1 <- fun_check(data = data1[, dot.categ], data.name = paste0(dot.categ, " COLUMN OF data1"), class = "vector", mode = "character", na.contain = TRUE, fun.name = function.name, print = FALSE)
tempo2 <- fun_check(data =  data1[, dot.categ], data.name = paste0(dot.categ, " COLUMN OF data1"), class = "factor", na.contain = TRUE, fun.name = function.name, print = FALSE)
if(tempo1$problem == TRUE & tempo2$problem == TRUE){
tempo.cat <- paste0("ERROR IN ", function.name, ": dot.categ COLUMN MUST BE A FACTOR OR CHARACTER VECTOR") #
stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
}
data1[, dot.categ] <- factor(data1[, dot.categ]) # if already a factor, change nothing, if characters, levels according to alphabetical order
# dot.categ column of data1 is factor from here
if( ! is.null(dot.categ.class.order)){
if(any(duplicated(dot.categ.class.order))){
tempo.cat <- paste0("ERROR IN ", function.name, ": dot.categ.class.order ARGUMENT CANNOT HAVE DUPLICATED CLASSES: ", paste(dot.categ.class.order, collapse = " "))
stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
}else if( ! (all(dot.categ.class.order %in% levels(data1[, dot.categ])) & all(levels(data1[, dot.categ]) %in% dot.categ.class.order))){
tempo.cat <- paste0("ERROR IN ", function.name, ": dot.categ.class.order ARGUMENT MUST BE CLASSES OF dot.categ ARGUMENT\nHERE IT IS:\n", paste(dot.categ.class.order, collapse = " "), "\nFOR dot.categ.class.order AND IT IS:\n", paste(levels(data1[, dot.categ]), collapse = " "), "\nFOR dot.categ COLUMN (", ini.dot.categ, ") OF data1")
stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
}else{
data1[, dot.categ] <- factor(data1[, dot.categ], levels = dot.categ.class.order) # reorder the factor
}
}else{
if(all(dot.color == "same") & length(dot.color) == 1){
dot.categ.class.order <- unlist(categ.class.order[length(categ)])
data1[, dot.categ] <- factor(data1[, dot.categ], levels = dot.categ.class.order) # reorder the factor
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") THE dot.categ.class.order SETTING IS NULL AND dot.color IS \"same\". ORDER OF categ.class.order WILL BE APPLIED FOR LEGEND DISPLAY: ", paste(dot.categ.class.order, collapse = " "))
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}else{
dot.categ.class.order <- sort(levels(data1[, dot.categ]))
data1[, dot.categ] <- factor(data1[, dot.categ], levels = dot.categ.class.order) # reorder the factor
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") THE dot.categ.class.order SETTING IS NULL. ALPHABETICAL ORDER WILL BE APPLIED FOR LEGEND DISPLAY: ", paste(dot.categ.class.order, collapse = " "))
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}
}
# dot.categ.class.order not NULL anymore (character string) if dot.categ is not NULL
if(all(dot.color == "same") & length(dot.color) == 1){
if( ! identical(ini.dot.categ, categ[length(categ)])){
tempo.cat <- paste0("ERROR IN ", function.name, ":WHEN dot.color ARGUMENT IS \"same\", THE COLUMN NAME IN dot.categ ARGUMENT MUST BE IDENTICAL TO THE LAST COLUMN NAME IN categ ARGUMENT. HERE IT IS:\ndot.categ: ", paste(ini.dot.categ, collapse = " "), "\ncateg: ", paste(categ, collapse = " "))
stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
}else if( ! identical(unlist(categ.class.order[length(categ)]), dot.categ.class.order)){
tempo.cat <- paste0("ERROR IN ", function.name, ":WHEN dot.color ARGUMENT IS \"same\",\nLAST COMPARTMENT OF categ.class.order ARGUMENT AND dot.categ.class.order ARGUMENT CANNOT BE DIFFERENT:\nLAST COMPARTMENT OF categ.class.order: ", paste(unlist(categ.class.order[length(categ)]), collapse = " "), "\ndot.categ.class.order: ", paste(dot.categ.class.order, collapse = " "))
stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
}
}
for(i3 in 1:length(categ)){
if(identical(categ[i3], ini.dot.categ) & ! identical(unlist(categ.class.order[i3]), dot.categ.class.order) & identical(sort(unlist(categ.class.order[i3])), sort(dot.categ.class.order))){
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") THE dot.categ ARGUMENT SETTING IS PRESENT IN THE categ ARGUMENT SETTING, BUT ORDER OF THE CLASSES IS NOT THE SAME:\ncateg.class.order: ", paste(unlist(categ.class.order[i3]), collapse = " "), "\ndot.categ.class.order: ", paste(dot.categ.class.order, collapse = " "), "\nNOTE THAT ORDER OF categ.class.order IS THE ONE USED FOR THE AXIS REPRESENTATION")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}
}
if(is.null(dot.categ.legend.name)){
dot.categ.legend.name <- if(ini.dot.categ %in% categ[length(categ)]){dot.categ}else{ini.dot.categ} #
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") THE dot.categ.legend.name SETTING IS NULL -> ", dot.categ.legend.name, " WILL BE USED AS LEGEND TITLE OF DOTS")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}
# dot.categ.legend.name not NULL anymore (character string)
}else{
if( ! is.null(dot.categ.class.order)){
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") THE dot.categ.class.order ARGUMENT IS NOT NULL, BUT THE dot.categ ARGUMENT IS\n-> dot.categ.class.order NOT CONSIDERED AS NO LEGEND WILL BE DRAWN")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}
# But dot.categ.class.order will be converted to NULL below (not now)
}
# end optional legend of dot colors
# check the nature of color
# integer colors into gg_palette
tempo.check.color <- fun_check(data = dot.color, class = "integer", double.as.integer.allowed = TRUE, na.contain = TRUE, fun.name = function.name, print = FALSE)$problem
if(tempo.check.color == FALSE){
# convert integers into colors
dot.color <- fun_gg_palette(max(dot.color, na.rm = TRUE))[dot.color]
}
# end integer colors into gg_palette
if(all(dot.color == "same") & length(dot.color) == 1){
dot.color <- categ.color # same color of the dots as the corresponding box color
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") dot.color ARGUMENT HAS BEEN SET TO \"same\"\nTHUS, DOTS WILL HAVE THE SAME COLORS AS THE CORRESPONDING BOXPLOT")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}else if( ! (all(dot.color %in% colors() | grepl(pattern = "^#", dot.color)))){ # check that all strings of low.color start by #
tempo.cat <- paste0("ERROR IN ", function.name, ": dot.color ARGUMENT MUST BE (1) A HEXADECIMAL COLOR VECTOR STARTING BY #, OR (2) COLOR NAMES GIVEN BY colors(), OR (3) INTEGERS, OR THE STRING\"same\"\nHERE IT IS: ", paste(unique(dot.color), collapse = " "))
stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
}
if(any(is.na(dot.color))){
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") dot.color ARGUMENT CONTAINS NA")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}
# end check the nature of color
# check the length of color
if( ! is.null(dot.categ)){
# optional legend of dot colors
if(length(dot.color) > 1 & length(unique(data1[, dot.categ])) != length(dot.color)){
tempo.cat <- paste0("ERROR IN ", function.name, ": dot.color ARGUMENT IS NOT THE SAME LENGTH AS LEVELS OF dot.categ COLUMN (", dot.categ, "):\ndot.color: ", paste(dot.color, collapse = " "), "\ndot.categ LEVELS: ", paste(levels(data1[, dot.categ]), collapse = " "))
stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
}else if(length(dot.color) == 1 & length(dot.categ.class.order) > 1){ # to deal with single color
dot.color <- rep(dot.color, length(dot.categ.class.order))
}
data1 <- data.frame(data1, dot.color = data1[, dot.categ])
data1$dot.color <- factor(data1$dot.color, labels = dot.color) # do not use labels = unique(dot.color). Otherwise, we can have green1 green2 when dot.color is c("green", "green")
dot.color <- as.character(unique(data1$dot.color[order(data1[, dot.categ])])) # reorder the dot.color character vector
if(length(dot.color) == 1 & length(dot.categ.class.order) > 1){ # to deal with single color
dot.color <- rep(dot.color, length(dot.categ.class.order))
}
tempo.check <- unique(data1[ , c(dot.categ, "dot.color")])
if(length(unique(data1[ , "dot.color"])) > 1 & ( ! (nrow(tempo.check) == length(unique(data1[ , "dot.color"])) & nrow(tempo.check) == length(unique(data1[ , dot.categ]))))){ # length(unique(data1[ , "dot.color"])) > 1 because if only one color, can be attributed to each class of dot.categ
tempo.cat <- paste0("ERROR IN ", function.name, ": dot.color ARGUMENT IS INCORRECTLY ASSOCIATED TO EACH CLASS OF dot.categ (", dot.categ, ") COLUMN:\n", paste(unique(mapply(FUN = "paste", data1[ , dot.categ], data1[ ,"dot.color"])), collapse = "\n"))
stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
}else{
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") IN dot.categ ARGUMENT (", ini.dot.categ, "), THE FOLLOWING COLORS OF DOTS:\n", paste(dot.color, collapse = " "), "\nHAVE BEEN ATTRIBUTED TO THESE CLASSES:\n", paste(levels(data1[, dot.categ]), collapse = " "))
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}
# dot.color is a character string representing the diff classes of dot.categ
# data1$dot.color is a factor with order of levels -> dot.categ
# end optional legend of dot colors
}else{
categ.len <- length(categ) # if only categ1, then colors for classes of categ1, if length(categ) == 2, then colors for classes of categ2
if(length(dot.color) == length(levels(data1[, categ[categ.len]]))){ # here length(dot.color) is equal to the different number of categ
# data1[, categ[categ.len]] <- factor(data1[, categ[categ.len]]) # not required because sure that is is a factor
data1 <- data.frame(data1, dot.color = data1[, categ[categ.len]])
data1$dot.color <- factor(data1$dot.color, labels = dot.color)
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") IN ", categ[categ.len], " OF categ ARGUMENT, THE FOLLOWING COLORS:\n", paste(dot.color, collapse = " "), "\nHAVE BEEN ATTRIBUTED TO THESE CLASSES:\n", paste(levels(factor(data1[, categ[categ.len]])), collapse = " "))
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}else if(length(dot.color) == length(data1[, categ[categ.len]])){# here length(dot.color) is equal to nrow(data1) -> Modif to have length(dot.color) equal to the different number of categ (length(dot.color) == length(levels(data1[, categ[categ.len]])))
data1 <- data.frame(data1, dot.color = dot.color)
}else if(length(dot.color) == 1 & ! all(dot.color == "same")){
# data1[, categ[categ.len]] <- factor(data1[, categ[categ.len]]) # not required because sure that is is a factor
data1 <- data.frame(data1, dot.color = dot.color)
dot.color <- rep(dot.color, length(levels(data1[, categ[categ.len]])))
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") dot.color ARGUMENT HAS LENGTH 1, MEANING THAT ALL THE DIFFERENT CLASSES OF ", categ[categ.len], "\n", paste(levels(factor(data1[, categ[categ.len]])), collapse = " "), "\nWILL HAVE THE SAME COLOR\n", paste(dot.color, collapse = " "))
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}else{
tempo.cat <- paste0("ERROR IN ", function.name, ": dot.color ARGUMENT MUST BE (1) LENGTH 1, OR (2) THE LENGTH OF data1 NROWS, OR (3) THE LENGTH OF THE CLASSES IN THE categ ", categ[categ.len], " COLUMN. HERE IT IS COLOR LENGTH ", length(dot.color), " VERSUS CATEG LENGTH ", length(data1[, categ[categ.len]]), " AND CATEG CLASS LENGTH ", length(unique(data1[, categ[categ.len]])), "\nPRESENCE OF NA COULD BE THE PROBLEM")
stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
}
# end check the length of color
dot.color <- as.character(dot.color)
# dot.color is a character string representing the diff classes
data1$dot.color <- factor(data1$dot.color, levels = unique(dot.color)) # ok because if dot.color is a character string, the order make class 1, class 2, etc. If dot.color is a column of data1, then levels will be created, without incidence, except if dot.categ specified (see below). unique() because no duplicates allowed
# data1$dot.color is a factor with order of levels -> dot.color
}
# end optional legend of dot colors
}else if(is.null(dot.color) & ! (is.null(dot.categ) & is.null(dot.categ.class.order) & is.null(dot.categ.legend.name))){
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") dot.categ OR dot.categ.class.order OR dot.categ.legend.name ARGUMENT HAS BEEN SPECIFIED BUT dot.color ARGUMENT IS NULL (NO DOT PLOTTED)")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}
# dot.color either NULL (no dot plotted) or character string (potentially representing the diff classes of dot.categ)
# data1$dot.color is either NA or a factor (with order of levels -> depending on dot.categ or categ[length(categ)], or other
if(is.null(dot.categ)){
dot.categ.class.order <- NULL # because not used anyway
}
# dot.categ.class.order either NULL if dot.categ is NULL (no legend displayed) or character string (potentially representing the diff classes of dot.categ)
# end management of dot.color
if(is.null(dot.color) & box.fill == FALSE & dot.alpha <= 0.025){
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") THE FOLLOWING ARGUMENTS WERE SET AS:\ndot.color = NULL (NOT ALL DOTS BUT ONLY POTENTIAL OUTLIER DOTS DISPLAYED)\nbox.fill = FALSE (NO FILLING COLOR FOR BOTH BOXES AND POTENTIAL OUTLIER DOTS)\ndot.alpha = ", fun_round(dot.alpha, 4), "\n-> POTENTIAL OUTLIER DOTS MIGHT NOT BE VISIBLE BECAUSE ALMOST TRANSPARENT")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}
if(is.null(dot.color) & box.fill == FALSE & dot.border.size == 0){
tempo.cat <- paste0("ERROR IN ", function.name, ": THE FOLLOWING ARGUMENTS WERE SET AS:\ndot.color = NULL (NOT ALL DOTS BUT ONLY POTENTIAL OUTLIER DOTS DISPLAYED)\nbox.fill = FALSE (NO FILLING COLOR FOR BOTH BOXES AND POTENTIAL OUTLIER DOTS)\ndot.border.size = 0 (NO BORDER FOR POTENTIAL OUTLIER DOTS)\n-> THESE SETTINGS ARE NOT ALLOWED BECAUSE THE POTENTIAL OUTLIER DOTS WILL NOT BE VISIBLE")
stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
}
if( ! is.null(dot.border.color)){
tempo1 <- fun_check(data = dot.border.color, class = "vector", mode = "character", length = 1, fun.name = function.name, print = FALSE)
tempo2 <- fun_check(data = dot.border.color, class = "vector", typeof = "integer", double.as.integer.allowed = TRUE, length = 1, fun.name = function.name, print = FALSE)
if(tempo1$problem == FALSE & tempo2$problem == TRUE & ! (all(dot.border.color %in% colors() | grepl(pattern = "^#", dot.border.color)))){ # check that all strings of low.color start by #
tempo.cat <- paste0("ERROR IN ", function.name, ": dot.border.color ARGUMENT MUST BE (1) A HEXADECIMAL COLOR STRING STARTING BY #, OR (2) A COLOR NAME GIVEN BY colors(), OR (3) AN INTEGER VALUE\nHERE IT IS: ", paste(unique(dot.border.color), collapse = " "))
stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
}else if(tempo1$problem == TRUE & tempo2$problem == FALSE){ # convert integers into colors
dot.border.color <- fun_gg_palette(max(dot.border.color, na.rm = TRUE))[dot.border.color]
}
# end integer colors into gg_palette
}
if(y.log != "no"){
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") y.log ARGUMENT SET TO ", y.log, ".\nVALUES FROM THE y ARGUMENT COLUMN OF THE data1 DATA FRAME MUST BE ALREADY ", toupper(y.log), " TRANSFORMED, AS THE y.log ARGUMENT JUST MODIFIES THE AXIS SCALE")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
if( ! is.null(y.lim)){
if(any(y.lim <= 0)){
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") y.lim ARGUMENT CAN SPAN ZERO OR NEGATIVE VALUES IF y.log ARGUMENT IS SET TO ", y.log, " BECAUSE y.log DOES NOT TRANSFORM DATA, JUST MODIFIES THE AXIS SCALE")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}else if(any( ! is.finite(if(y.log == "log10"){10^y.lim}else{2^y.lim}))){
tempo.cat <- paste0("ERROR IN ", function.name, ": y.lim ARGUMENT RETURNS INF WITH THE y.log ARGUMENT SET TO ", y.log, "\nAS SCALE COMPUTATION IS ", ifelse(y.log == "log10", 10, 2), "^y.lim:\n", paste(ifelse(y.log == "log10", 10, 2)^y.lim, collapse = " "), "\nARE YOU SURE THAT y.lim ARGUMENT HAS BEEN SPECIFIED WITH VALUES ALREADY IN LOG SCALE?\n", paste(y.lim, collapse = " "))
stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE)
}
}
}
# inactivated because y must already be log transformed data
# if(y.log != "no" & y.include.zero == TRUE){
# warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") y.log ARGUMENT SET TO ", y.log, " AND y.include.zero ARGUMENT SET TO TRUE -> y.include.zero ARGUMENT RESET TO FALSE BECAUSE NO 0 ALLOWED IN LOG SCALE")
# warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
# }
if(y.log != "no" & vertical == FALSE){
vertical <- TRUE
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") BECAUSE OF A BUG IN ggplot2, CANNOT FLIP BOXS HORIZONTALLY WITH A Y.LOG SCALE -> vertical ARGUMENT RESET TO TRUE")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}
# end second round of checking and data preparation


# package checking
fun_pack(req.package = c("ggplot2"), lib.path = lib.path)
fun_pack(req.package = c("scales"), lib.path = lib.path)
# end package checking



# main code
# na detection and removal (done now to be sure of the correct length of categ)
column.check <- c(y, categ, "categ.color", if( ! is.null(dot.color)){"dot.color"}, if( ! is.null(dot.color) & ! is.null(dot.categ)){if(dot.categ != ini.dot.categ){dot.categ}}, if( ! is.null(facet.categ)){facet.categ}) # dot.categ because can be a 3rd column of data1
if(any(is.na(data1[, column.check]))){
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") NA DETECTED IN COLUMNS ", paste(column.check, collapse = " "), " OF data1 AND CORRESPONDING ROWS REMOVED (SEE $removed.row.nb AND $removed.rows)")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
for(i2 in 1:length(column.check)){
if(any(is.na(data1[, column.check[i2]]))){
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") NA REMOVAL DUE TO COLUMN ", column.check[i2], " OF data1")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}
}
removed.row.nb <- unlist(lapply(lapply(c(data1[column.check]), FUN = is.na), FUN = which))
removed.rows <- data1[removed.row.nb, ]
column.check <- column.check[ ! column.check == y] # remove y to keep quali columns
if(length(removed.row.nb) != 0){
data1 <- data1[-removed.row.nb, ]
for(i3 in 1:length(column.check)){
if(any( ! unique(removed.rows[, column.check[i3]]) %in% unique(data1[, column.check[i3]]))){
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") IN COLUMN ", column.check[i3], " OF data1, THE FOLLOWING CLASSES HAVE DISAPPEARED AFTER NA REMOVAL (IF COLUMN USED IN THE PLOT, THIS CLASS WILL NOT BE DISPLAYED):\n", paste(unique(removed.rows[, column.check[i3]])[ ! unique(removed.rows[, column.check[i3]]) %in% unique(data1[, column.check[i3]])], collapse = " "))
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}
}
}
count.categ <- 0
for(i2 in 1:length(column.check)){
if(column.check[i2] %in% categ){
count.categ <- count.categ + 1
}
if(column.check[i2] == categ[count.categ]){
categ.class.order[count.categ] <- list(levels(data1[, column.check[i2]])[levels(data1[, column.check[i2]]) %in% unique(data1[, column.check[i2]])]) # remove the absent color in the character vector
data1[, column.check[i2]] <- factor(as.character(data1[, column.check[i2]]), levels = unique(categ.class.order[[count.categ]]))
}
if( ! is.null(dot.color) & ! is.null(dot.categ)){ # reminder : dot.categ cannot be a column name of categ anymore (because in that case dot.categ name is changed into "..._DOT"
if(column.check[i2] == ini.dot.categ){
dot.categ.class.order <- levels(data1[, column.check[i2]])[levels(data1[,  column.check[i2]]) %in% unique(data1[, column.check[i2]])] # remove the absent color in the character vector
data1[, column.check[i2]] <- factor(as.character(data1[, column.check[i2]]), levels = unique(dot.categ.class.order))
}
}
if(column.check[i2] == "categ.color"){
categ.color <- levels(data1[, column.check[i2]])[levels(data1[, column.check[i2]]) %in% unique(data1[, column.check[i2]])] # remove the absent color in the character vector
if(length(categ.color) == 1 & length(unlist(categ.class.order[length(categ)])) > 1){ # to deal with single color
categ.color <- rep(categ.color, length(unlist(categ.class.order[length(categ)])))
}
data1[, column.check[i2]] <- factor(as.character(data1[, column.check[i2]]), levels = unique(categ.color))
}
if(column.check[i2] == "dot.color"){
dot.color <- levels(data1[, column.check[i2]])[levels(data1[, column.check[i2]]) %in% unique(data1[, column.check[i2]])] # remove the absent color in the character vector
if(length(dot.color) == 1 & length(dot.categ.class.order) > 1){ # to deal with single color. If dot.categ.class.order == NULL (which is systematically the case if dot.categ == NULL), no rep(dot.color, length(dot.categ.class.order)
dot.color <- rep(dot.color, length(dot.categ.class.order))
}
data1[, column.check[i2]] <- factor(as.character(data1[, column.check[i2]]), levels = unique(dot.color))
}
if(column.check[i2] %in% facet.categ){ # works if facet.categ == NULL this method should keep the order of levels when removing some levels
tempo.levels <- levels(data1[, column.check[i2]])[levels(data1[, column.check[i2]]) %in% unique(as.character(data1[, column.check[i2]]))]
data1[, column.check[i2]] <- factor(as.character(data1[, column.check[i2]]), levels = tempo.levels)
}
}
}else{
removed.row.nb <- NULL
removed.rows <- NULL
}
# end na detection and removal (done now to be sure of the correct length of categ)


# y coordinates recovery (create ini.box.coord, dot.coord and modify data1)
if(length(categ) == 1){
# width commputations
box.width2 <- box.width
box.space <- 0 # to inactivate the shrink that add space between grouped boxes, because  no grouped boxes here
# end width commputations
# data1 check categ order for dots coordinates recovery
data1 <- data.frame(data1, categ.check = data1[, categ[1]])
data1$categ.check <- as.integer(data1$categ.check) # to check that data1[, categ[1]] and dot.coord$group are similar, during merging
# end data1 check categ order for dots coordinates recovery
# per box dots coordinates recovery
tempo.gg.name <- "gg.indiv.plot."
tempo.gg.count <- 0
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), eval(parse(text = paste0("ggplot2::ggplot()", if(is.null(add)){""}else{add})))) # add added here to have the facets
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_point(data = data1, mapping = ggplot2::aes_string(x = categ[1], y = y, color = categ[1]), stroke = dot.border.size, size = dot.size, alpha = dot.alpha, shape = 21))
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "color", name = categ.legend.name, values = if(is.null(dot.color)){rep(NA, length(unique(data1[, categ[1]])))}else if(length(dot.color) == 1){rep(dot.color, length(unique(data1[, categ[1]])))}else{dot.color}))
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_boxplot(data = data1, mapping = ggplot2::aes_string(x = categ[1], y = y, fill = categ[1]), coef = if(box.whisker.kind == "no"){0}else if(box.whisker.kind == "std"){1.5}else if(box.whisker.kind == "max"){Inf})) # fill because this is what is used with geom_box # to easily have the equivalent of the grouped boxs
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "fill", name = categ.legend.name, values = if(length(categ.color) == 1){rep(categ.color, length(unique(data1[, categ[1]])))}else{categ.color}))
# end per box dots coordinates recovery
}else if(length(categ) == 2){
# width commputations
box.width2 <- box.width / length(unique(data1[, categ[length(categ)]])) # real width of each box in x-axis unit, among the set of grouped box. Not relevant if no grouped boxs length(categ) == 1
# end width commputations
# data1 check categ order for dots coordinates recovery
tempo.factor <- paste0(data1[order(data1[, categ[2]], data1[, categ[1]]), categ[2]], "_", data1[order(data1[, categ[2]], data1[, categ[1]]), categ[1]])
data1 <- data.frame(data1[order(data1[, categ[2]], data1[, categ[1]]), ], categ.check = factor(tempo.factor, levels = unique(tempo.factor)))
data1$categ.check <- as.integer(data1$categ.check)
# end data1 check categ order for dots coordinates recovery
# per box dots coordinates recovery
tempo.gg.name <- "gg.indiv.plot."
tempo.gg.count <- 0
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), eval(parse(text = paste0("ggplot2::ggplot()", if(is.null(add)){""}else{add})))) # add added here to have the facets
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_point(data = data1, mapping = ggplot2::aes_string(x = categ[1], y = y, color = categ[2]), stroke = dot.border.size, size = dot.size, alpha = dot.alpha, shape = 21))
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "color", name = categ.legend.name, values = if(is.null(dot.color)){rep(NA, length(unique(data1[, categ[2]])))}else if(length(dot.color) == 1){rep(dot.color, length(unique(data1[, categ[2]])))}else{dot.color}))
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_boxplot(data = data1, mapping = ggplot2::aes_string(x = categ[1], y = y, fill = categ[2]), coef = if(box.whisker.kind == "no"){0}else if(box.whisker.kind == "std"){1.5}else if(box.whisker.kind == "max"){Inf})) # fill because this is what is used with geom_box # to easily have the equivalent of the grouped boxs
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "fill", name = categ.legend.name, values = if(length(categ.color) == 1){rep(categ.color, length(unique(data1[, categ[2]])))}else{categ.color}))
# end per box dots coordinates recovery
}else{
tempo.cat <- paste0("\n\n============\n\nINTERNAL CODE ERROR IN ", function.name, ": CODE INCONSISTENCY 2\n\n============\n\n")
stop(tempo.cat)
}
tempo <- ggplot2::ggplot_build(eval(parse(text = paste(paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + "))))
dot.coord <- tempo$data[[1]]
ini.box.coord <- tempo$data[[2]]
tempo.mean <- aggregate(x = dot.coord$y, by = list(dot.coord$group, dot.coord$PANEL), FUN = mean, na.rm = TRUE)
names(tempo.mean)[names(tempo.mean) == "x"] <- "MEAN"
names(tempo.mean)[names(tempo.mean) == "Group.1"] <- "BOX"
names(tempo.mean)[names(tempo.mean) == "Group.2"] <- "PANEL"
dot.coord <- data.frame(
dot.coord[order(dot.coord$group, dot.coord$y), ], 
y.check = as.double(data1[order(data1$categ.check, data1[, y]), y]), 
categ.check = data1[order(data1$categ.check, data1[, y]), "categ.check"], 
dot.color = if(is.null(dot.color)){NA}else{data1[order(data1$categ.check, data1[, y]), "dot.color"]}, 
data1[order(data1$categ.check, data1[, y]), ][categ] # avoid the renaming below
) # y.check to be sure that the order is the same between the y of data1 and the y of dot.coord
# names(dot.coord)[names(dot.coord) == "tempo.categ1"] <- categ[1]
if( ! is.null(dot.categ)){
dot.coord <- data.frame(dot.coord, data1[order(data1$categ.check, data1[, y]), ][dot.categ]) # avoid the renaming
}
if( ! is.null(facet.categ)){
dot.coord <- data.frame(dot.coord, data1[order(data1$categ.check, data1[, y]), ][facet.categ]) # for facet panels
tempo.test <- NULL
for(i2 in 1:length(facet.categ)){
tempo.test <- paste0(tempo.test, ".", formatC(as.numeric(dot.coord[, facet.categ[i2]]), width = nchar(max(as.numeric(dot.coord[, facet.categ[i2]]), na.rm = TRUE)), flag = "0")) # convert factor into numeric with leading zero for proper ranking # merge the formatC() to create a new factor. The convertion to integer should recreate the correct group number. Here as.numeric is used and not as.integer in case of numeric in facet.categ (because comes from add and not checked by fun_check, contrary to categ)
}
tempo.test <- as.integer(factor(tempo.test))
if( ! identical(as.integer(dot.coord$PANEL), tempo.test)){
tempo.cat <- paste0("\n\n================\n\nINTERNAL CODE ERROR IN ", function.name, ": as.integer(dot.coord$PANEL) AND tempo.test MUST BE IDENTICAL. CODE HAS TO BE MODIFIED\n\n================\n\n")
stop(tempo.cat)
}
}
if(dot.tidy == TRUE){
dot.coord <- data.frame(dot.coord, tidy_group = if(length(categ) == 1){dot.coord[, categ]}else{as.integer(factor(paste0(
formatC(as.integer(dot.coord[, categ[1]]), width = nchar(max(as.integer(dot.coord[, categ[1]]), na.rm = TRUE)), flag = "0"), # convert factor into numeric with leading zero for proper ranking
".", 
formatC(as.integer(dot.coord[, categ[2]]), width = nchar(max(as.integer(dot.coord[, categ[2]]), na.rm = TRUE)), flag = "0")# convert factor into numeric with leading zero for proper ranking
))) # merge the 2 formatC() to create a new factor. The convertion to integer should recreate the correct group number
}) # for tidy dot plots
}
if( ! (identical(dot.coord$y, dot.coord$y.check) & identical(dot.coord$group, dot.coord$categ.check))){
tempo.cat <- paste0("\n\n================\n\nINTERNAL CODE ERROR IN ", function.name, ": (dot.coord$y AND dot.coord$y.check) AS WELL AS (dot.coord$group AND dot.coord$categ.check) MUST BE IDENTICAL. CODE HAS TO BE MODIFIED\n\n================\n\n")
stop(tempo.cat)
}else{
if( ! identical(tempo.mean[order(tempo.mean$BOX, tempo.mean$PANEL), ]$BOX, unique(dot.coord[order(dot.coord$group, dot.coord$PANEL), c("group", "PANEL")])$group)){
tempo.cat <- paste0("\n\n================\n\nINTERNAL CODE ERROR IN ", function.name, ": (tempo.mean$BOX, tempo.mean$PANEL) AND (dot.coord$group, dot.coord$PANEL) MUST BE IDENTICAL. CODE HAS TO BE MODIFIED\n\n================\n\n")
stop(tempo.cat)
}else{
tempo.mean <- data.frame(tempo.mean[order(tempo.mean$BOX, tempo.mean$PANEL), ], unique(dot.coord[order(dot.coord$group, dot.coord$PANEL), c(categ, if( ! is.null(dot.color) & ! is.null(dot.categ)){if(dot.categ != ini.dot.categ){dot.categ}}, if( ! is.null(facet.categ)){facet.categ}), drop = FALSE]))
}
}
# at that stage, categ color and dot color are correctly attributed in data1, box.coord and dot.coord
# end y dot coordinates recovery (create ini.box.coord, dot.coord and modify data1)













# stat output (will also serve for boxplot and mean display)
ini.box.coord <- ini.box.coord[order(ini.box.coord$group), ]
stat <- data.frame(MIN = ini.box.coord$ymin, QUART1 = ini.box.coord$lower, MEDIAN = ini.box.coord$middle, QUART3 = ini.box.coord$upper, MAX = ini.box.coord$ymax, NOTCHUPPER = ini.box.coord$notchupper, NOTCHLOWER = ini.box.coord$notchlower, OUTLIERS = ini.box.coord["outliers"], COLOR = ini.box.coord$fill, stringsAsFactors = TRUE) # ini.box.coord["outliers"] written like this because it is a list. X coordinates not put now because several features to set
names(stat)[names(stat) == "outliers"] <- "OUTLIERS"
tempo.mean <- tempo.mean[order(tempo.mean$BOX), ]
if( ! fun_comp_2d(ini.box.coord[c("PANEL", "group")], tempo.mean[c("PANEL", "BOX")])$identical.content){
tempo.cat <- paste0("\n\n================\n\nINTERNAL CODE ERROR IN ", function.name, ": ini.box.coord[c(\"PANEL\", \"group\")] AND tempo.mean[c(\"PANEL\", \"BOX\")] MUST BE IDENTICAL. CODE HAS TO BE MODIFIED\n\n================\n\n")
stop(tempo.cat)
}else{
stat <- data.frame(stat[c("MIN", "QUART1", "MEDIAN")], MEAN = tempo.mean$MEAN, stat[c("QUART3", "MAX", "NOTCHUPPER", "NOTCHLOWER", "OUTLIERS")], tempo.mean[colnames(tempo.mean) != "MEAN"], stat["COLOR"], stringsAsFactors = TRUE) # ini.box.coord["outliers"] written like this because it is a list
}
# end stat output (will also serve for boxplot and mean display)








# ylim range
if(is.null(y.lim)){
if(any(data1[, y] %in% c(Inf, -Inf))){
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") THE data1 ARGUMENT CONTAINS -Inf OR Inf VALUES IN THE y COLUMN, THAT WILL NOT BE CONSIDERED IN THE PLOT RANGE")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}
y.lim <- range(data1[, y], na.rm = TRUE, finite = TRUE) # finite = TRUE removes all the -Inf and Inf except if only this. In that case, whatever the -Inf and/or Inf present, output -Inf;Inf range. Idem with NA only
}
if(suppressWarnings(all(y.lim %in% c(Inf, -Inf)))){
tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, " COMPUTED y.lim CONTAINS Inf VALUES, BECAUSE VALUES FROM data1 ARGUMENTS ARE NA OR Inf ONLY\n\n================\n\n")
stop(tempo.cat)
}
y.lim.order <- order(y.lim) # to deal with inverse axis
y.lim <- sort(y.lim)
y.lim[1] <- y.lim[1] - abs(y.lim[2] - y.lim[1]) * ifelse(diff(y.lim.order) > 0, y.bottom.extra.margin, y.top.extra.margin) # diff(y.lim.order) > 0 medians not inversed axis
y.lim[2] <- y.lim[2] + abs(y.lim[2] - y.lim[1]) * ifelse(diff(y.lim.order) > 0, y.top.extra.margin, y.bottom.extra.margin) # diff(y.lim.order) > 0 medians not inversed axis
if(y.include.zero == TRUE){ # no need to check y.log != "no" because done before
y.lim <- range(c(y.lim, 0), na.rm = TRUE, finite = TRUE) # finite = TRUE removes all the -Inf and Inf except if only this. In that case, whatever the -Inf and/or Inf present, output -Inf;Inf range. Idem with NA only
}
y.lim <- y.lim[y.lim.order]
if(any(is.na(y.lim))){
tempo.cat <- paste0("\n\n============\n\nINTERNAL CODE ERROR IN ", function.name, ": CODE INCONSISTENCY 4\n\n============\n\n")
stop(tempo.cat)
}
# end ylim range






# drawing
# constant part
tempo.gg.name <- "gg.indiv.plot."
tempo.gg.count <- 0
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), eval(parse(text = paste0("ggplot2::ggplot()", if(is.null(add)){""}else{add})))) # add is directly put here to deal with additional variable of data, like when using facet_grid. No problem if add is a theme, will be dealt below
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::xlab(if(is.null(x.lab)){categ[1]}else{x.lab}))
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::ylab(if(is.null(y.lab)){y}else{y.lab}))
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::ggtitle(title))
# text angle management
tempo.just <- fun_gg_just(angle = text.angle, axis = ifelse(vertical == TRUE, "x", "y"))
# end text angle management
add.check <- TRUE
if( ! is.null(add)){ # if add is NULL, then = 0
if(grepl(pattern = "ggplot2::theme", add) == TRUE){
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") \"ggplot2::theme\" STRING DETECTED IN THE add ARGUMENT -> article ARGUMENT WILL BE IGNORED")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
add.check <- FALSE
}
}
if(add.check == TRUE & article == TRUE){
# BEWARE: not possible to add theme()several times. NO message but the last one overwrites the others
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::theme_classic(base_size = text.size))
if(grid == TRUE){
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), m.gg <- ggplot2::theme(
text = ggplot2::element_text(size = text.size), 
plot.title = ggplot2::element_text(size = title.text.size), # stronger than text
line = ggplot2::element_line(size = 0.5), 
legend.key = ggplot2::element_rect(color = "white", size = 1.5), # size of the frame of the legend
axis.line.y.left = ggplot2::element_line(colour = "black"), # draw lines for the y axis
axis.line.x.bottom = ggplot2::element_line(colour = "black"), # draw lines for the x axis
panel.grid.major.x = if(vertical == TRUE){NULL}else{ggplot2::element_line(colour = "grey85")},
panel.grid.major.y = if(vertical == TRUE){ggplot2::element_line(colour = "grey85")}else{NULL},
axis.text.x = if(vertical == TRUE){ggplot2::element_text(angle = tempo.just$angle, hjust = tempo.just$hjust, vjust = tempo.just$vjust)}else{NULL},
axis.text.y = if(vertical == TRUE){NULL}else{ggplot2::element_text(angle = tempo.just$angle, hjust = tempo.just$hjust, vjust = tempo.just$vjust)},
strip.background = ggplot2::element_rect(fill = NA, colour = NA)
))
}else{
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), m.gg <- ggplot2::theme(
text = ggplot2::element_text(size = text.size), 
plot.title = ggplot2::element_text(size = title.text.size), # stronger than text
line = ggplot2::element_line(size = 0.5), 
legend.key = ggplot2::element_rect(color = "white", size = 1.5), # size of the frame of the legend
axis.line.y.left = ggplot2::element_line(colour = "black"), 
axis.line.x.bottom = ggplot2::element_line(colour = "black"),
axis.text.x = if(vertical == TRUE){ggplot2::element_text(angle = tempo.just$angle, hjust = tempo.just$hjust, vjust = tempo.just$vjust)}else{NULL},
axis.text.y = if(vertical == TRUE){NULL}else{ggplot2::element_text(angle = tempo.just$angle, hjust = tempo.just$hjust, vjust = tempo.just$vjust)},
strip.background = ggplot2::element_rect(fill = NA, colour = NA)
))
}
}else if(add.check == TRUE & article == FALSE){
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), m.gg <- ggplot2::theme(
text = ggplot2::element_text(size = text.size), 
plot.title = ggplot2::element_text(size = title.text.size), # stronger than text
line = ggplot2::element_line(size = 0.5), 
legend.key = ggplot2::element_rect(color = "white", size = 1.5), # size of the frame of the legend
panel.background = ggplot2::element_rect(fill = "grey95"), 
axis.line.y.left = ggplot2::element_line(colour = "black"), 
axis.line.x.bottom = ggplot2::element_line(colour = "black"), 
panel.grid.major.x = ggplot2::element_line(colour = "grey85"), 
panel.grid.major.y = ggplot2::element_line(colour = "grey85"), 
panel.grid.minor.x = ggplot2::element_blank(), 
panel.grid.minor.y = ggplot2::element_blank(), 
strip.background = ggplot2::element_rect(fill = NA, colour = NA),
axis.text.x = if(vertical == TRUE){ggplot2::element_text(angle = tempo.just$angle, hjust = tempo.just$hjust, vjust = tempo.just$vjust)}else{NULL},
axis.text.y = if(vertical == TRUE){NULL}else{ggplot2::element_text(angle = tempo.just$angle, hjust = tempo.just$hjust, vjust = tempo.just$vjust)}
))
}
# Contrary to fun_gg_bar(), cannot plot the  boxplot right now, because I need the dots plotted first
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_boxplot(data = data1, mapping = ggplot2::aes_string(x = categ[1], y = y, group = categ[length(categ)]), position = ggplot2::position_dodge(width = NULL), color = NA, width = box.width, fill = NA)) # this is to set the graph (i.e., a blanck boxplot to be able to use x coordinates to plot dots before boxes)
# end constant part




# x coordinates management (for random plotting and for stat display)
# boxs
tempo.graph.info <- ggplot2::ggplot_build(eval(parse(text = paste0(paste(paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + "), ' + ggplot2::geom_boxplot(data = data1, mapping = ggplot2::aes_string(x = categ[1], y = y, fill = categ[length(categ)]), position = ggplot2::position_dodge(width = NULL), width = box.width, notch = box.notch, coef = if(box.whisker.kind == "no"){0}else if(box.whisker.kind == "std"){1.5}else if(box.whisker.kind == "max"){Inf}) + ggplot2::scale_discrete_manual(aesthetics = "fill", name = categ.legend.name, values = if(length(categ.color) == 1){rep(categ.color, length(unique(data1[, categ[length(categ)]])))}else{categ.color})')))) # will be recovered later again, when ylim will be considered
tempo.yx.ratio <- (tempo.graph.info$layout$panel_params[[1]]$y.range[2] - tempo.graph.info$layout$panel_params[[1]]$y.range[1]) / (tempo.graph.info$layout$panel_params[[1]]$x.range[2] - tempo.graph.info$layout$panel_params[[1]]$x.range[1])
box.coord <- tempo.graph.info$data[[2]] # to have the summary statistics of the plot. Contrary to ini.box.plot, now integrates ylim Here because can be required for stat.disp when just box are plotted
box.coord <- box.coord[order(box.coord$group), ]
if(stat.disp.mean == TRUE){ # for mean display
if( ! identical(tempo.mean$BOX, box.coord$group)){
tempo.cat <- paste0("\n\n============\n\nINTERNAL CODE ERROR IN ", function.name, ": tempo.mean$BOX AND box.coord$group DO NOT HAVE THE SAME VALUE ORDER\n\n============\n\n")
stop(tempo.cat)
}else{
box.coord <- data.frame(box.coord, tempo.mean)
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") MEAN VALUES INSTEAD OF MEDIAN VALUES DISPLAYED")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))

}
}
# width commputations
width.ini <- c(box.coord$xmax - box.coord$xmin)[1] # all the box widths are equal here. Only the first one taken
width.correct <- width.ini * box.space / 2
if( ! identical(box.coord$group, stat$BOX)){
tempo.cat <- paste0("\n\n================\n\nINTERNAL CODE ERROR IN ", function.name, ": (box.coord$group AND stat$BOX) MUST BE IDENTICAL. CODE HAS TO BE MODIFIED\n\n================\n\n")
stop(tempo.cat)
}else{
stat <- data.frame(
stat, 
X = box.coord$x, 
X_BOX_INF = box.coord$xmin + width.correct, 
X_BOX_SUP = box.coord$xmax - width.correct, 
X_NOTCH_INF = box.coord$x - (box.coord$x - (box.coord$xmin + width.correct)) / 2,  
X_NOTCH_SUP = box.coord$x + (box.coord$x - (box.coord$xmin + width.correct)) / 2, 
X_WHISK_INF = box.coord$x - (box.coord$x - (box.coord$xmin + width.correct)) * box.whisker.width, 
X_WHISK_SUP = box.coord$x + (box.coord$x - (box.coord$xmin + width.correct)) * box.whisker.width, 
# tempo.mean[colnames(tempo.mean) != "MEAN"], # already added above
stringsAsFactors = TRUE
)
stat$COLOR <- factor(stat$COLOR, levels = unique(categ.color))
if( ! all(stat$NOTCHUPPER < stat$QUART3 & stat$NOTCHLOWER > stat$QUART1) & box.notch == TRUE){
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") SOME NOTCHES ARE BEYOND BOX HINGES. TRY ARGUMENT box.notch = FALSE")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}
}
dot.jitter <- c((box.coord$xmax - width.correct) - (box.coord$xmin + width.correct))[1] * dot.jitter # real dot.jitter. (box.coord$xmin + width.correct) - (box.coord$xmax - width.correct))[1] is the width of the box. Is  equivalent to (box.coord$x - (box.coord$xmin + width.correct))[1] * 2
# end width commputations
# end boxs
if( ! is.null(dot.color)){
# random dots
if(dot.tidy == FALSE){
dot.coord.rd1 <- merge(dot.coord, box.coord[c("fill", "PANEL", "group", "x")], by = c("PANEL", "group"), sort = FALSE) # rd for random. Send the coord of the boxs into the coord data.frame of the dots (in the column x.y). BEWARE: by = c("PANEL", "group") without fill column because PANEL & group columns are enough as only one value of x column per group number in box.coord. Thus, no need to consider fill column
if(nrow(dot.coord.rd1) != nrow(dot.coord)){
tempo.cat <- paste0("\n\n================\n\nINTERNAL CODE ERROR IN ", function.name, ": THE merge() FUNCTION DID NOT RETURN A CORRECT dot.coord.rd1 DATA FRAME. CODE HAS TO BE MODIFIED\n\n================\n\n")
stop(tempo.cat)
}
set.seed(1)
sampled.dot.jitter <- if(nrow(dot.coord.rd1) == 1){runif(n = nrow(dot.coord.rd1), min = - dot.jitter / 2, max = dot.jitter / 2)}else{sample(x = runif(n = nrow(dot.coord.rd1), min = - dot.jitter / 2, max = dot.jitter / 2), size = nrow(dot.coord.rd1), replace = FALSE)}
dot.coord.rd2 <- data.frame(dot.coord.rd1, dot.x = dot.coord.rd1$x.y + sampled.dot.jitter) # set the dot.jitter thanks to runif and dot.jitter range. Then, send the coord of the boxs into the coord data.frame of the dots (in the column x.y)
set.seed(NULL)
if(length(categ) == 1){
tempo.data1 <- unique(data.frame(data1[categ[1]], group = as.integer(data1[, categ[1]]))) # categ[1] is factor
names(tempo.data1)[names(tempo.data1) == categ[1]] <- paste0(categ[1], ".check")
verif <- paste0(categ[1], ".check")
}else if(length(categ) == 2){
tempo.data1 <- unique(
data.frame(
data1[c(categ[1], categ[2])], 
group = as.integer(factor(paste0(
formatC(as.integer(data1[, categ[2]]), width = nchar(max(as.integer(data1[, categ[2]]), na.rm = TRUE)), flag = "0"), # convert factor into numeric with leading zero for proper ranking
".", 
formatC(as.integer(data1[, categ[1]]), width = nchar(max(as.integer(data1[, categ[1]]), na.rm = TRUE)), flag = "0")# convert factor into numeric with leading zero for proper ranking
))) # merge the 2 formatC() to create a new factor. The convertion to integer should recreate the correct group number
)
) # categ[2] first if categ[2] is used to make the categories in ggplot and categ[1] is used to make the x-axis
names(tempo.data1)[names(tempo.data1) == categ[1]] <- paste0(categ[1], ".check")
names(tempo.data1)[names(tempo.data1) == categ[2]] <- paste0(categ[2], ".check")
verif <- c(paste0(categ[1], ".check"), paste0(categ[2], ".check"))
}else{
tempo.cat <- paste0("\n\n============\n\nINTERNAL CODE ERROR IN ", function.name, ": CODE INCONSISTENCY 6\n\n============\n\n")
stop(tempo.cat)
}
dot.coord.rd3 <- merge(dot.coord.rd2, tempo.data1, by = intersect("group", "group"), sort = FALSE) # send the factors of data1 into coord. BEWARE: I have replaced by = "group" by intersect("group", "group") because of an error due to wrong group group merging in dot.coord.rd3
if(nrow(dot.coord.rd3) != nrow(dot.coord) | ( ! fun_comp_2d(dot.coord.rd3[categ], dot.coord.rd3[verif])$identical.content)){
tempo.cat <- paste0("\n\n================\n\nINTERNAL CODE ERROR IN ", function.name, ": THE merge() FUNCTION DID NOT RETURN A CORRECT dot.coord.rd3 DATA FRAME. CODE HAS TO BE MODIFIED\n\n================\n\n")
stop(tempo.cat)
}
# end random dots
}
# tidy dots
# coordinates are recover during plotting (see dot.coord.tidy1 below)
# end tidy dots
}
# end x coordinates management (for random plotting and for stat display)





# boxplot display before dot display if box.fill = TRUE
coord.names <- NULL
# creation of the data frame for (main box + legend) and data frame for means
if(box.notch == FALSE){
for(i3 in 1:length(categ)){
if(i3 == 1){
tempo.polygon <- data.frame(GROUPX = c(t(stat[, rep(categ[i3], 5)])), stringsAsFactors = TRUE)
}else{
tempo.polygon <- cbind(tempo.polygon, c(t(stat[, rep(categ[i3], 5)])), stringsAsFactors = TRUE)
}
}
names(tempo.polygon) <- categ
tempo.polygon <- data.frame(X = c(t(stat[, c("X_BOX_INF", "X_BOX_SUP", "X_BOX_SUP", "X_BOX_INF", "X_BOX_INF")])), Y = c(t(stat[, c("QUART1", "QUART1", "QUART3", "QUART3", "QUART1")])), COLOR = c(t(stat[, c("COLOR", "COLOR", "COLOR", "COLOR", "COLOR")])), BOX = as.character(c(t(stat[, c("BOX", "BOX", "BOX", "BOX", "BOX")]))), tempo.polygon, stringsAsFactors = TRUE)
if( ! is.null(facet.categ)){
for(i4 in 1:length(facet.categ)){
tempo.polygon <- data.frame(tempo.polygon, c(t(stat[, c(facet.categ[i4], facet.categ[i4], facet.categ[i4], facet.categ[i4], facet.categ[i4])])), stringsAsFactors = TRUE)
names(tempo.polygon)[length(names(tempo.polygon))] <- facet.categ[i4]
}
}
}else{
for(i3 in 1:length(categ)){
if(i3 == 1){
tempo.polygon <- data.frame(GROUPX = c(t(stat[, rep(categ[i3], 11)])), stringsAsFactors = TRUE)
}else{
tempo.polygon <- cbind(tempo.polygon, c(t(stat[, rep(categ[i3], 11)])), stringsAsFactors = TRUE)
}
}
names(tempo.polygon) <- categ
tempo.polygon <- data.frame(X = c(t(stat[, c("X_BOX_INF", "X_BOX_SUP", "X_BOX_SUP", "X_NOTCH_SUP", "X_BOX_SUP", "X_BOX_SUP", "X_BOX_INF", "X_BOX_INF", "X_NOTCH_INF", "X_BOX_INF", "X_BOX_INF")])), Y = c(t(stat[, c("QUART1", "QUART1", "NOTCHLOWER", "MEDIAN", "NOTCHUPPER", "QUART3", "QUART3", "NOTCHUPPER", "MEDIAN", "NOTCHLOWER", "QUART1")])), COLOR = c(t(stat[, c("COLOR", "COLOR", "COLOR", "COLOR", "COLOR", "COLOR", "COLOR", "COLOR", "COLOR", "COLOR", "COLOR")])), BOX = as.character(c(t(stat[, c("BOX", "BOX", "BOX", "BOX", "BOX", "BOX", "BOX", "BOX", "BOX", "BOX", "BOX")]))), tempo.polygon, stringsAsFactors = TRUE)
if( ! is.null(facet.categ)){
for(i4 in 1:length(facet.categ)){
tempo.polygon <- data.frame(tempo.polygon, c(t(stat[, c(facet.categ[i4], facet.categ[i4], facet.categ[i4], facet.categ[i4], facet.categ[i4], facet.categ[i4], facet.categ[i4], facet.categ[i4], facet.categ[i4], facet.categ[i4], facet.categ[i4])])), stringsAsFactors = TRUE)
names(tempo.polygon)[length(names(tempo.polygon))] <- facet.categ[i4]
}
}
}
tempo.polygon$COLOR <- factor(tempo.polygon$COLOR, levels = unique(categ.color))
if( ! is.null(categ.class.order)){
for(i3 in 1:length(categ)){
tempo.polygon[, categ[i3]] <- factor(tempo.polygon[, categ[i3]], levels = categ.class.order[[i3]])
}
}
# modified name of dot.categ column (e.g., "Group1_DOT") must be included for boxplot using ridy dots
if( ! is.null(dot.color) & ! is.null(dot.categ)){
if(dot.categ != ini.dot.categ){
tempo.polygon <- data.frame(tempo.polygon, GROUPX = tempo.polygon[, ini.dot.categ])
names(tempo.polygon)[names(tempo.polygon) == "GROUPX"] <- dot.categ

}
}
tempo.diamon.mean <- data.frame(X = c(t(stat[, c("X", "X_NOTCH_INF", "X", "X_NOTCH_SUP", "X")])), Y = c(t(cbind(stat["MEAN"] - (stat[, "X"] - stat[, "X_NOTCH_INF"]) * tempo.yx.ratio, stat["MEAN"], stat["MEAN"] + (stat[, "X"] - stat[, "X_NOTCH_INF"]) * tempo.yx.ratio, stat["MEAN"], stat["MEAN"] - (stat[, "X"] - stat[, "X_NOTCH_INF"]) * tempo.yx.ratio))), COLOR = c(t(stat[, c("COLOR", "COLOR", "COLOR", "COLOR", "COLOR")])), GROUP = c(t(stat[, c("BOX", "BOX", "BOX", "BOX", "BOX")])), stringsAsFactors = TRUE)
if( ! is.null(facet.categ)){
for(i3 in 1:length(facet.categ)){
tempo.diamon.mean <- data.frame(tempo.diamon.mean, c(t(stat[, c(facet.categ[i3], facet.categ[i3], facet.categ[i3], facet.categ[i3], facet.categ[i3])])), stringsAsFactors = TRUE)
names(tempo.diamon.mean)[length(names(tempo.diamon.mean))] <- facet.categ[i3]
}
}
tempo.diamon.mean$COLOR <- factor(tempo.diamon.mean$COLOR, levels = unique(categ.color))
# end creation of the data frame for (main box + legend) and data frame for means
if(box.fill == TRUE){
# assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_boxplot(data = data1, mapping = ggplot2::aes_string(x = categ[1], y = y, color = categ[length(categ)], fill = categ[length(categ)]), position = ggplot2::position_dodge(width = NULL), width = box.width, size = box.line.size, notch = box.notch, coef = if(box.whisker.kind == "no"){0}else if(box.whisker.kind == "std"){1.5}else if(box.whisker.kind == "max"){Inf}, alpha = box.alpha, outlier.shape = if( ! is.null(dot.color)){NA}else{21}, outlier.color = if( ! is.null(dot.color)){NA}else{dot.border.color}, outlier.fill = if( ! is.null(dot.color)){NA}else{NULL}, outlier.size = if( ! is.null(dot.color)){NA}else{dot.size}, outlier.stroke = if( ! is.null(dot.color)){NA}else{dot.border.size}, outlier.alpha = if( ! is.null(dot.color)){NA}else{dot.alpha})) # the color, size, etc. of the outliers are dealt here. outlier.color = NA to do not plot outliers when dots are already plotted. Finally, boxplot redrawn (see below)
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_polygon(
data = tempo.polygon, 
mapping = ggplot2::aes_string(x = "X", y = "Y", group = "BOX", fill = categ[length(categ)], color = categ[length(categ)]), 
size = box.line.size, 
alpha = box.alpha
))
coord.names <- c(coord.names, "main.box")
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = X, xend = X, y = QUART3, yend = MAX, group = categ[length(categ)]), color = "black", size = box.line.size, alpha = box.alpha)) # 
coord.names <- c(coord.names, "sup.whisker")
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = X, xend = X, y = QUART1, yend = MIN, group = categ[length(categ)]), color = "black", size = box.line.size, alpha = box.alpha)) # 
coord.names <- c(coord.names, "inf.whisker")
if(box.whisker.width > 0){
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = X_WHISK_INF, xend = X_WHISK_SUP, y = MAX, yend = MAX, group = categ[length(categ)]), color = "black", size = box.line.size, alpha = box.alpha, lineend = "round")) # 
coord.names <- c(coord.names, "sup.whisker.edge")
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = X_WHISK_INF, xend = X_WHISK_SUP, y = MIN, yend = MIN, group = categ[length(categ)]), color = "black", size = box.line.size, alpha = box.alpha, lineend = "round")) # 
coord.names <- c(coord.names, "inf.whisker.edge")
}
if(box.mean == TRUE){
# assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_point(data = stat, mapping = ggplot2::aes_string(x = "X", y = "MEAN", group = categ[length(categ)]), shape = 23, stroke = box.line.size * 2, fill = stat$COLOR, size = box.mean.size, color = "black", alpha = box.alpha)) # group used in aesthetic to do not have it in the legend
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_polygon(
data = tempo.diamon.mean, 
mapping = ggplot2::aes(x = X, y = Y, group = GROUP), 
fill = tempo.diamon.mean[, "COLOR"], 
color = hsv(0, 0, 0, alpha = box.alpha), # outline of the polygon in black but with alpha
size = box.line.size * 2, 
alpha = box.alpha
))
coord.names <- c(coord.names, "mean")
}
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = if(box.notch == FALSE){X_BOX_INF}else{X_NOTCH_INF}, xend = if(box.notch == FALSE){X_BOX_SUP}else{X_NOTCH_SUP}, y = MEDIAN, yend = MEDIAN, group = categ[length(categ)]), color = "black", size = box.line.size * 2, alpha = box.alpha)) # 
coord.names <- c(coord.names, "median")
}
# end boxplot display before dot display if box.fill = TRUE






# dot display
if( ! is.null(dot.color)){
if(dot.tidy == FALSE){
if(is.null(dot.categ)){
if(dot.border.size == 0){
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_point(
data = dot.coord.rd3, 
mapping = ggplot2::aes_string(x = "dot.x", y = "y", group = categ[length(categ)]), 
size = dot.size, 
shape = 19, 
color = dot.coord.rd3$dot.color, 
alpha = dot.alpha
)) # group used in aesthetic to do not have it in the legend. Here ggplot2::scale_discrete_manual() cannot be used because of the group easthetic
}else{
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_point(
data = dot.coord.rd3, 
mapping = ggplot2::aes_string(x = "dot.x", y = "y", group = categ[length(categ)]), 
shape = 21, 
stroke = dot.border.size, 
color = if(is.null(dot.border.color)){dot.coord.rd3$dot.color}else{rep(dot.border.color, nrow(dot.coord.rd3))}, 
size = dot.size, 
fill = dot.coord.rd3$dot.color, 
alpha = dot.alpha
)) # group used in aesthetic to do not have it in the legend. Here ggplot2::scale_discrete_manual() cannot be used because of the group easthetic
}
}else{
if(dot.border.size == 0){
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_point(
data = dot.coord.rd3, 
mapping = ggplot2::aes_string(x = "dot.x", y = "y", alpha = dot.categ), 
size = dot.size, 
shape = 19, 
color = dot.coord.rd3$dot.color
)) # group used in aesthetic to do not have it in the legend. Here ggplot2::scale_discrete_manual() cannot be used because of the group easthetic
}else{
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_point(
data = dot.coord.rd3, 
mapping = ggplot2::aes_string(x = "dot.x", y = "y", alpha = dot.categ), 
size = dot.size, 
shape = 21, 
stroke = dot.border.size, 
color = if(is.null(dot.border.color)){dot.coord.rd3$dot.color}else{rep(dot.border.color, nrow(dot.coord.rd3))}, 
fill = dot.coord.rd3$dot.color
)) # group used in aesthetic to do not have it in the legend. Here ggplot2::scale_discrete_manual() cannot be used because of the group easthetic
}
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "alpha", name = dot.categ.legend.name, values = rep(dot.alpha,  length(dot.categ.class.order)), guide = ggplot2::guide_legend(override.aes = list(fill = dot.color, color = if(is.null(dot.border.color)){dot.color}else{dot.border.color}, stroke = dot.border.size, alpha = dot.alpha)))) # values are the values of color (which is the border color in geom_box. BEWARE: values = categ.color takes the numbers to make the colors if categ.color is a factor
}
}else if(dot.tidy == TRUE){
if(is.null(dot.categ)){
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_dotplot(
data = dot.coord, 
mapping = ggplot2::aes_string(x = categ[1], y = "y", group = "tidy_group"), 
position = ggplot2::position_dodge(width = box.width), 
binaxis = "y", 
stackdir = "center", 
alpha = dot.alpha, 
fill = as.character(dot.coord[order(dot.coord[, categ[1]], decreasing = TRUE), "dot.color"]), 
stroke = dot.border.size, 
color = if(is.null(dot.border.color)){as.character(dot.coord[order(dot.coord[, categ[1]], decreasing = TRUE), "dot.color"])}else{rep(dot.border.color, nrow(dot.coord))}, 
show.legend = FALSE, # BEWARE: do not use show.legend = TRUE because it uses the arguments outside aes() as aesthetics (here color and fill). Thus I must find a way using ggplot2::scale_discrete_manual()
binwidth = (y.lim[2] - y.lim[1]) / dot.tidy.bin.nb
)) # geom_dotplot ggplot2 v3.3.0: I had to remove rev() in fill and color # very weird behavior of geom_dotplot ggplot2 v3.2.1, (1) because with aes group = (to avoid legend), the dot plotting is not good in term of coordinates, and (2) because data1 seems reorderer according to x = categ[1] before plotting. Thus, I have  to use fill = dot.coord[rev(order(dot.coord[, categ[1]], decreasing = TRUE)), "dot.color"] to have the good corresponding colors # show.legend option do not remove the legend, only the aesthetic of the legend (dot, line, etc.)
}else{
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_dotplot(
data = dot.coord, 
mapping = ggplot2::aes_string(x = categ[1], y = "y", alpha = categ[length(categ)]), 
position = ggplot2::position_dodge(width = box.width), 
binaxis = "y", 
stackdir = "center", 
fill = as.character(dot.coord[order(dot.coord[, categ[1]], decreasing = TRUE), "dot.color"]), 
stroke = dot.border.size, 
color = if(is.null(dot.border.color)){as.character(dot.coord[order(dot.coord[, categ[1]], decreasing = TRUE), "dot.color"])}else{rep(dot.border.color, nrow(dot.coord))}, 
# BEWARE: do not use show.legend =  TRUE because it uses the arguments outside aes() as aesthetics (here color and fill). Thus I must find a way using ggplot2::scale_discrete_manual()
binwidth = (y.lim[2] - y.lim[1]) / dot.tidy.bin.nb
)) # geom_dotplot ggplot2 v3.3.0: I had to remove rev() in fill and color # very weird behavior of geom_dotplot ggplot2 v3.2.1, (1) because with aes group = (to avoid legend), the dot plotting is not good in term of coordinates, and (2) because data1 seems reorderer according to x = categ[1] before plotting. Thus, I have  to use fill = dot.coord[rev(order(dot.coord[, categ[1]], decreasing = TRUE)), "dot.color"] to have the good corresponding colors # show.legend option do not remove the legend, only the aesthetic of the legend (dot, line, etc.)
# assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "linetype", name = dot.categ.legend.name, values = rep(1, length(categ.color)))) # values = rep("black", length(categ.color)) are the values of color (which is the border color of dots), and this modify the border color on the plot. BEWARE: values = categ.color takes the numbers to make the colors if categ.color is a factor
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "alpha", name = dot.categ.legend.name, values = rep(dot.alpha,  length(dot.categ.class.order)), labels = dot.categ.class.order, guide = ggplot2::guide_legend(title = if(ini.dot.categ == categ[length(categ)]){dot.categ}else{ini.dot.categ}, override.aes = list(fill = dot.color, color = if(is.null(dot.border.color)){dot.color}else{dot.border.color}, stroke = dot.border.size, alpha = dot.alpha)))) # values are the values of color (which is the border color in geom_box. BEWARE: values = categ.color takes the numbers to make the colors if categ.color is a factor
}
# coordinates of tidy dots
tempo.coord <- ggplot2::ggplot_build(eval(parse(text = paste(paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + "))))$data # to have the tidy dot coordinates
if(length(which(sapply(tempo.coord, FUN = nrow) == nrow(data1))) > 1){
tempo.cat <- paste0("\n\n================\n\nINTERNAL CODE ERROR IN ", function.name, ": MORE THAN 2 COMPARTMENT WITH NROW EQUAL TO nrow(data1) IN THE tempo.coord LIST (FOR TIDY DOT COORDINATES). CODE HAS TO BE MODIFIED\n\n================\n\n")
stop(tempo.cat)
}else{
dot.coord.tidy1 <- tempo.coord[[which(sapply(tempo.coord, FUN = nrow) == nrow(data1))]]
}
tempo.box.coord <- merge(box.coord, unique(dot.coord[, c("PANEL", "group", categ)]), by = c("PANEL", "group"), sort = FALSE) # add the categ in box.coord. BEWARE: by = c("PANEL", "group") without fill column because PANEL & group columns are enough as only one value of x column per group number in box.coord. Thus, no need to consider fill column
if(nrow(tempo.box.coord) != nrow(box.coord)){
tempo.cat <- paste0("\n\n================\n\nINTERNAL CODE ERROR IN ", function.name, ": THE merge() FUNCTION DID NOT RETURN A CORRECT tempo.box.coord DATA FRAME. CODE HAS TO BE MODIFIED\n\n================\n\n")
stop(tempo.cat)
}
dot.coord.tidy2 <- merge(dot.coord.tidy1, tempo.box.coord[c("fill", "PANEL", "group", "x", categ)], by = c("PANEL", "group"), sort = FALSE) # send the coord of the boxs into the coord data.frame of the dots (in the column x.y).BEWARE: by = c("PANEL", "group") without fill column because PANEL & group columns are enough as only one value of x column per group number in tempo.box.coord. Thus, no need to consider fill colum # DANGER: from here the fill.y and x.y (from tempo.box.coord) are not good in dot.coord.tidy2. It is ok because Group1 Group2 from tempo.box.coord are ok with the group column from dot.coord.tidy1. This is due to the fact that dot.coord.tidy resulting from geom_dotplot does not make the same groups as the other functions
if(nrow(dot.coord.tidy2) != nrow(dot.coord)){
tempo.cat <- paste0("\n\n================\n\nINTERNAL CODE ERROR IN ", function.name, ": THE merge() FUNCTION DID NOT RETURN A CORRECT dot.coord.tidy2 DATA FRAME. CODE HAS TO BE MODIFIED\n\n================\n\n")
stop(tempo.cat)
}
if(length(categ) == 1){
tempo.data1 <- unique(data.frame(data1[categ[1]], group = as.integer(data1[, categ[1]]))) # categ[1] is factor
names(tempo.data1)[names(tempo.data1) == categ[1]] <- paste0(categ[1], ".check")
verif <- paste0(categ[1], ".check")
}else if(length(categ) == 2){
tempo.data1 <- unique(
data.frame(
data1[c(categ[1], categ[2])], 
group = as.integer(factor(paste0(
formatC(as.integer(data1[, categ[2]]), width = nchar(max(as.integer(data1[, categ[2]]), na.rm = TRUE)), flag = "0"), # convert factor into numeric with leading zero for proper ranking
".", 
formatC(as.integer(data1[, categ[1]]), width = nchar(max(as.integer(data1[, categ[1]]), na.rm = TRUE)), flag = "0")# convert factor into numeric with leading zero for proper ranking
))) # merge the 2 formatC() to create a new factor. The convertion to integer should recreate the correct group number
)
) # categ[2] first if categ[2] is used to make the categories in ggplot and categ[1] is used to make the x-axis
names(tempo.data1)[names(tempo.data1) == categ[1]] <- paste0(categ[1], ".check")
names(tempo.data1)[names(tempo.data1) == categ[2]] <- paste0(categ[2], ".check")
verif <- c(paste0(categ[1], ".check"), paste0(categ[2], ".check"))
}else{
tempo.cat <- paste0("\n\n============\n\nINTERNAL CODE ERROR IN ", function.name, ": CODE INCONSISTENCY 7\n\n============\n\n")
stop(tempo.cat)
}
dot.coord.tidy3 <- merge(dot.coord.tidy2, tempo.data1, by = intersect("group", "group"), sort = FALSE) # send the factors of data1 into coord. BEWARE: I have tested intersect("group", "group") instead of by = "group". May be come back to by = "group"  in case of error. But I did this because of an error in dot.coord.rd3 above
if(nrow(dot.coord.tidy3) != nrow(dot.coord) | ( ! fun_comp_2d(dot.coord.tidy3[categ], dot.coord.tidy3[verif])$identical.content)){
tempo.cat <- paste0("\n\n================\n\nINTERNAL CODE ERROR IN ", function.name, ": THE merge() FUNCTION DID NOT RETURN A CORRECT dot.coord.tidy3 DATA FRAME. CODE HAS TO BE MODIFIED\n\n================\n\n")
stop(tempo.cat)
}
# end coordinates of tidy dots
}
coord.names <- c(coord.names, "dots")
}
# end dot display



# boxplot display (if box.fill = FALSE, otherwise, already plotted above)
if(box.fill == TRUE){
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "fill", name = categ.legend.name, values =  if(length(categ.color) == 1){rep(categ.color, length(unique(data1[, categ[length(categ)]])))}else{categ.color})) #, guide = ggplot2::guide_legend(override.aes = list(fill = levels(tempo.polygon$COLOR), color = "black")))) # values are the values of color (which is the border color in geom_box. BEWARE: values = categ.color takes the numbers to make the colors if categ.color is a factor
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "color", name = categ.legend.name, values = rep(hsv(0, 0, 0, alpha = box.alpha), length(unique(data1[, categ[length(categ)]]))))) # , guide = ggplot2::guide_legend(override.aes = list(color = "black", alpha = box.alpha)))) # values are the values of color (which is the border color in geom_box. BEWARE: values = categ.color takes the numbers to make the colors if categ.color is a factor # outline of the polygon in black but with alpha
}else{
# assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_boxplot(data = data1, mapping = ggplot2::aes_string(x = categ[1], y = y, color = categ[length(categ)], fill = categ[length(categ)]), position = ggplot2::position_dodge(width = NULL), width = box.width, size = box.line.size, notch = box.notch, alpha = box.alpha, coef = if(box.whisker.kind == "no"){0}else if(box.whisker.kind == "std"){1.5}else if(box.whisker.kind == "max"){Inf}, outlier.shape = if( ! is.null(dot.color)){NA}else{21}, outlier.color = if( ! is.null(dot.color)){NA}else{if(dot.border.size == 0){NA}else{dot.border.color}}, outlier.fill = if( ! is.null(dot.color)){NA}else{NULL}, outlier.size = if( ! is.null(dot.color)){NA}else{dot.size}, outlier.stroke = if( ! is.null(dot.color)){NA}else{dot.border.size}, outlier.alpha = if( ! is.null(dot.color)){NA}else{dot.alpha})) # the color, size, etc. of the outliers are dealt here. outlier.color = NA to do not plot outliers when dots are already plotted
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_path(
data = tempo.polygon, 
mapping = ggplot2::aes_string(x = "X", y = "Y", group = "BOX", color = categ[length(categ)]), 
size = box.line.size, 
alpha = box.alpha, 
lineend = "round", 
linejoin = "round"
))
coord.names <- c(coord.names, "main.box")
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = if(box.notch == FALSE){X_BOX_INF}else{X_NOTCH_INF}, xend = if(box.notch == FALSE){X_BOX_SUP}else{X_NOTCH_SUP}, y = MEDIAN, yend = MEDIAN, group = categ[length(categ)]), color = stat$COLOR, size = box.line.size * 2, alpha = box.alpha)) # 
coord.names <- c(coord.names, "median")
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = X, xend = X, y = QUART3, yend = MAX, group = categ[length(categ)]), color = stat$COLOR, size = box.line.size, alpha = box.alpha)) # 
coord.names <- c(coord.names, "sup.whisker")
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = X, xend = X, y = QUART1, yend = MIN, group = categ[length(categ)]), color = stat$COLOR, size = box.line.size, alpha = box.alpha)) # 
coord.names <- c(coord.names, "inf.whisker")
if(box.whisker.width > 0){
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = X_WHISK_INF, xend = X_WHISK_SUP, y = MAX, yend = MAX, group = categ[length(categ)]), color = stat$COLOR, size = box.line.size, alpha = box.alpha, lineend = "round")) # 
coord.names <- c(coord.names, "sup.whisker.edge")
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = X_WHISK_INF, xend = X_WHISK_SUP, y = MIN, yend = MIN, group = categ[length(categ)]), color = stat$COLOR, size = box.line.size, alpha = box.alpha, lineend = "round")) # 
coord.names <- c(coord.names, "inf.whisker.edge")
}
if(box.mean == TRUE){
# assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_point(data = stat, mapping = ggplot2::aes_string(x = "X", y = "MEAN", group = categ[length(categ)]), shape = 23, stroke = box.line.size * 2, color = stat$COLOR, size = box.mean.size, fill = NA, alpha = box.alpha)) # group used in aesthetic to do not have it in the legend. Here ggplot2::scale_discrete_manual() cannot be used because of the group easthetic
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_path(
data = tempo.diamon.mean, 
mapping = ggplot2::aes(x = X, y = Y, group = GROUP), 
color = tempo.diamon.mean[, "COLOR"], 
size = box.line.size * 2, 
alpha = box.alpha, 
lineend = "round", 
linejoin = "round"
))
coord.names <- c(coord.names, "mean")
}
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "fill", name = categ.legend.name, values = rep(NA, length(unique(data1[, categ[length(categ)]]))))) #, guide = ggplot2::guide_legend(override.aes = list(color = categ.color)))) # values are the values of color (which is the border color in geom_box. BEWARE: values = categ.color takes the numbers to make the colors if categ.color is a factor
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "color", name = categ.legend.name, values = if(length(categ.color) == 1){rep(categ.color, length(unique(data1[, categ[length(categ)]])))}else{categ.color}, guide = ggplot2::guide_legend(override.aes = list(alpha = if(plot == TRUE & ((length(dev.list()) > 0 & names(dev.cur()) == "windows") | (length(dev.list()) == 0 & Sys.info()["sysname"] == "Windows"))){1}else{box.alpha})))) # , guide = ggplot2::guide_legend(override.aes = list(color = as.character(categ.color))))) # values are the values of color (which is the border color in geom_box. BEWARE: values = categ.color takes the numbers to make the colors if categ.color is a factor
if(plot == TRUE & ((length(dev.list()) > 0 & names(dev.cur()) == "windows") | (length(dev.list()) == 0 & Sys.info()["sysname"] == "Windows"))){ # if any Graph device already open and this device is "windows", or if no Graph device opened yet and we are on windows system -> prevention of alpha legend bug on windows using value 1
# to avoid a bug on windows: if alpha argument is different from 1 for lines (transparency), then lines are not correctly displayed in the legend when using the R GUI (bug https://github.com/tidyverse/ggplot2/issues/2452). No bug when using a pdf
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") FROM FUNCTION ", function.name, ": GRAPHIC DEVICE USED ON A WINDOWS SYSTEM ->\nTRANSPARENCY OF THE LINES IS INACTIVATED IN THE LEGEND TO PREVENT A WINDOWS DEPENDENT BUG (SEE https://github.com/tidyverse/ggplot2/issues/2452)\nTO OVERCOME THIS ON WINDOWS, USE ANOTHER DEVICE (pdf() FOR INSTANCE)")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}
}
# end boxplot display (if box.fill = FALSE, otherwise, already plotted above)




# stat display
# layer after dots but ok, behind dots on the plot
if( ! is.null(stat.disp)){
if(stat.disp == "top"){
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1),  ggplot2::annotate(geom = "text", x = stat$X, y = y.lim[2], label = if(stat.disp.mean == FALSE){fun_round(stat$MEDIAN, 2)}else{fun_round(stat$MEAN, 2)}, size = stat.size, color = "black", hjust = ifelse(vertical == TRUE, 0.5, 1.1), vjust = ifelse(vertical == TRUE, 1.1, 0.5))) # beware: no need of order() for labels because box.coord$x set the order. For justification, see https://stackoverflow.com/questions/7263849/what-do-hjust-and-vjust-do-when-making-a-plot-using-ggplot
coord.names <- c(coord.names, "stat.display")
}else if(stat.disp == "above"){
# stat coordinates
if( ! is.null(dot.color)){ # for text just above max dot
if(dot.tidy == FALSE){
tempo.stat.ini <- dot.coord.rd3
}else if(dot.tidy == TRUE){
tempo.stat.ini <- dot.coord.tidy3
tempo.stat.ini$x.y <- tempo.stat.ini$x.x # this is just to be able to use tempo.stat.ini$x.y for untidy or tidy dots (remember that dot.coord.tidy3$x.y is not good, see above)
}
stat.coord1 <- aggregate(x = tempo.stat.ini["y"], by = {x.env <- if(length(categ) == 1){list(tempo.stat.ini$group, tempo.stat.ini$x.y, tempo.stat.ini[, categ[1]])}else if(length(categ) == 2){list(tempo.stat.ini$group, tempo.stat.ini$x.y, tempo.stat.ini[, categ[1]], tempo.stat.ini[, categ[2]])} ; names(x.env) <- if(length(categ) == 1){c("group", "x.y", categ[1])}else if(length(categ) == 2){c("group", "x.y", categ[1], categ[2])} ; x.env}, FUN = min, na.rm = TRUE)
names(stat.coord1)[names(stat.coord1) == "y"] <- "dot.min"
stat.coord2 <- aggregate(x = tempo.stat.ini["y"], by = {x.env <- if(length(categ) == 1){list(tempo.stat.ini$group, tempo.stat.ini$x.y, tempo.stat.ini[, categ[1]])}else if(length(categ) == 2){list(tempo.stat.ini$group, tempo.stat.ini$x.y, tempo.stat.ini[, categ[1]], tempo.stat.ini[, categ[2]])} ; names(x.env) <- if(length(categ) == 1){c("group", "x.y", categ[1])}else if(length(categ) == 2){c("group", "x.y", categ[1], categ[2])} ; x.env}, FUN = max, na.rm = TRUE)
names(stat.coord2) <- paste0(names(stat.coord2), "_from.dot.max")
names(stat.coord2)[names(stat.coord2) == "y_from.dot.max"] <- "dot.max"
stat.coord3 <- cbind(box.coord[order(box.coord$x), ], stat.coord1[order(stat.coord1$x.y), ], stat.coord2[order(stat.coord2$x.y), ]) # should be ok to use box.coord$x and stat.coord$x.y to assemble the two data frames because x coordinates of the boxs. Thus, we cannot have identical values
if( ! all(identical(round(stat.coord3$x, 9), round(stat.coord3$x.y, 9)))){
tempo.cat <- paste0("\n\n================\n\nINTERNAL CODE ERROR IN ", function.name, ": FUSION OF box.coord, stat.coord1 AND stat.coord2 ACCORDING TO box.coord$x, stat.coord1$x.y AND stat.coord2$x.y IS NOT CORRECT. CODE HAS TO BE MODIFIED\n\n================\n\n")
stop(tempo.cat)
}
text.coord <- stat.coord3[, c("x", "group", "dot.min", "dot.max")]
names(text.coord)[names(text.coord) == "dot.min"] <- "text.min.pos"
names(text.coord)[names(text.coord) == "dot.max"] <- "text.max.pos"
box.coord <- box.coord[order(box.coord$x), ]
text.coord <- text.coord[order(text.coord$x), ] # to be sure to have the two objects in the same order for x. BEWARE: cannot add identical(as.integer(text.coord$group), as.integer(box.coord$group)) because with error, the correspondence between x and group is not the same
if( ! identical(text.coord$x, box.coord$x)){
tempo.cat <- paste0("\n\n============\n\nINTERNAL CODE ERROR IN ", function.name, ": text.coord AND box.coord DO NOT HAVE THE SAME x COLUMN CONTENT\n\n============\n\n")
stop(tempo.cat)
}
}
# end stat coordinates
# stat display
if(is.null(dot.color)){ # text just above boxs
# performed twice: first for y values >=0, then y values < 0, because only a single value allowed for hjust anf vjust
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::annotate(
geom = "text", 
x = box.coord$x[box.coord$middle >= 0], 
y = box.coord$middle[box.coord$middle >= 0], 
label = if(stat.disp.mean == FALSE){fun_round(box.coord$middle, 2)[box.coord$middle >= 0]}else{fun_round(box.coord$MEAN, 2)[box.coord$MEAN >= 0]}, 
size = stat.size, 
color = "black", 
hjust = ifelse(vertical == TRUE, 0.5, 0.5 - stat.dist), 
vjust = ifelse(vertical == TRUE, 0.5 - stat.dist, 0.5)
)) # beware: no need of order() for labels because box.coord$x set the order
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::annotate(
geom = "text", 
x = box.coord$x[box.coord$middle < 0], 
y = box.coord$middle[box.coord$middle < 0], 
label = if(stat.disp.mean == FALSE){fun_round(box.coord$middle, 2)[box.coord$middle < 0]}else{fun_round(box.coord$MEAN, 2)[box.coord$MEAN < 0]}, 
size = stat.size, 
color = "black", 
hjust = ifelse(vertical == TRUE, 0.5, 0.5 + stat.dist), 
vjust = ifelse(vertical == TRUE, 0.5 + stat.dist, 0.5)
)) # beware: no need of order() for labels because box.coord$x set the order
}else{ # text just above error boxs or dots
# I checked that text.coord and box.coord have the same x and group column content. Thus, ok to use them together
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::annotate(
geom = "text", 
x = text.coord$x[box.coord$middle >= 0], 
y = text.coord$text.max.pos[box.coord$middle >= 0], 
label = if(stat.disp.mean == FALSE){fun_round(box.coord$middle, 2)[box.coord$middle >= 0]}else{fun_round(box.coord$MEAN, 2)[box.coord$MEAN >= 0]}, 
size = stat.size, 
color = "black", 
hjust = ifelse(vertical == TRUE, 0.5, 0.5 - stat.dist), 
vjust = ifelse(vertical == TRUE, 0.5 - stat.dist, 0.5)
)) # beware: no need of order() for labels because box.coord$x set the order
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::annotate(
geom = "text", 
x = text.coord$x[box.coord$middle < 0], 
y = text.coord$text.min.pos[box.coord$middle < 0], 
label = if(stat.disp.mean == FALSE){fun_round(box.coord$middle, 2)[box.coord$middle < 0]}else{fun_round(box.coord$MEAN, 2)[box.coord$MEAN < 0]}, 
size = stat.size, 
color = "black", 
hjust = ifelse(vertical == TRUE, 0.5, 0.5 + stat.dist), 
vjust = ifelse(vertical == TRUE, 0.5 + stat.dist, 0.5)
)) # beware: no need of order() for labels because box.coord$x set the order
}
# end stat display
coord.names <- c(coord.names, "stat.display.positive", "stat.display.negative")
}else{
tempo.cat <- paste0("\n\n============\n\nINTERNAL CODE ERROR IN ", function.name, ": CODE INCONSISTENCY 9\n\n============\n\n")
stop(tempo.cat)
}
}
# end stat display



# y scale management (cannot be before dot plot management)
tempo.coord <- ggplot2::ggplot_build(eval(parse(text = paste(paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + "))))$layout$panel_params[[1]]
if(is.null(y.tick.nb) & y.log != "no"){ # integer main ticks for log2 and log10
tempo.scale <- (as.integer(min(y.lim, na.rm = TRUE)) - 1):(as.integer(max(y.lim, na.rm = TRUE)) + 1)
}else{
tempo <- if(is.null(attributes(tempo.coord$y$breaks))){tempo.coord$y$breaks}else{unlist(attributes(tempo.coord$y$breaks))}
tempo.scale <- fun_scale(lim = y.lim, n = ifelse(is.null(y.tick.nb), length(tempo[ ! is.na(tempo)]), y.tick.nb)) # in ggplot 3.3.0, tempo.coord$y.major_source replaced by tempo.coord$y$breaks
}
# for the ggplot2 bug with y.log, this does not work: eval(parse(text = ifelse(vertical == FALSE & y.log == "log10", "ggplot2::scale_x_continuous", "ggplot2::scale_y_continuous")))
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_y_continuous(
breaks = tempo.scale, 
labels = if(y.log == "log10"){scales::trans_format("identity", scales::math_format(10^.x))}else if(y.log == "log2"){scales::trans_format("identity",  scales::math_format(2^.x))}else if(y.log == "no"){ggplot2::waiver()}else{tempo.cat <- paste0("\n\n============\n\nINTERNAL CODE ERROR IN ", function.name, ": CODE INCONSISTENCY 10\n\n============\n\n") ; stop(tempo.cat)}, 
expand = c(0, 0),
limits = NA,
trans = ifelse(diff(y.lim) < 0, "reverse", "identity") # equivalent to ggplot2::scale_y_reverse()
))
if(vertical == TRUE){
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::coord_cartesian(ylim = y.lim)) # clip = "off" to have secondary ticks outside plot region does not work
}else{
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::coord_flip(ylim = y.lim)) # clip = "off" to have secondary ticks outside plot region does not work
}
# secondary ticks (after ggplot2::coord_cartesian() or ggplot2::coord_flip())
tempo.coord <- ggplot2::ggplot_build(eval(parse(text = paste(paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + "))))$layout$panel_params[[1]]
# no secondary ticks for log2. Play with y.lim
# y.second.tick.positions: coordinates of secondary ticks (only if y.inter.tick.nb argument is non NULL or if y.log argument is different from "no")
y.second.tick.values <- NULL
if(y.log != "no"){
# make a function from this
y.lim.order <- order(y.lim) # to deal with inverse axis
ini.scipen <- options()$scipen
options(scipen = -1000) # force scientific format
power10.exp <- as.integer(substring(text = 10^y.lim, first = (regexpr(pattern = "\\+|\\-", text = 10^y.lim)))) # recover the power of 10, i.e., integer part of y.lim. Example recover 08 from 1e+08. Works for log2
# mantisse <- as.numeric(substr(x = 10^y.lim, start = 1, stop = (regexpr(pattern = "\\+|\\-", text = 10^y.lim) - 2))) # recover the mantisse. Example recover 1.22 from 1.22e+08
options(scipen = ini.scipen) # restore the initial scientific penalty
tempo.tick.pos <- unique(as.vector(outer(2:10, ifelse(y.log == "log2", 2, 10)^((power10.exp[1] - ifelse(diff(y.lim.order) > 0, 1, -1)):(power10.exp[2] + ifelse(diff(y.lim.order) > 0, 1, -1)))))) # use log10(2:10) even if log2: it is to get log values between 0 and 1
tempo.tick.pos <- sort(tempo.tick.pos, decreasing = ifelse(diff(y.lim.order) > 0, FALSE, TRUE))
y.second.tick.values <- tempo.tick.pos
if(y.log == "log2"){
tempo.tick.pos <- log2(tempo.tick.pos[tempo.tick.pos >= min(2^y.lim) & tempo.tick.pos <= max(2^y.lim)])
}else if(y.log == "log10"){
tempo.tick.pos <- log10(tempo.tick.pos[tempo.tick.pos >= min(10^y.lim) & tempo.tick.pos <= max(10^y.lim)])
}
if(any(is.na(tempo.tick.pos) | ! is.finite(tempo.tick.pos))){ 
tempo.cat <- paste0("\n\n============\n\nINTERNAL CODE ERROR IN ", function.name, ": CODE INCONSISTENCY 11\n\n============\n\n")
stop(tempo.cat)
}
# if(vertical == TRUE){ # do not remove in case the bug is fixed
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::annotate(geom = "segment", y = tempo.tick.pos, yend = tempo.tick.pos, x = tempo.coord$x.range[1], xend = tempo.coord$x.range[1] + diff(tempo.coord$x.range) / 80))
# }else{ # not working because  of the ggplot2 bug
# assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::annotate(geom = "segment", x = tempo.tick.pos, xend = tempo.tick.pos, y = tempo.coord$y.range[1], yend = tempo.coord$y.range[1] + diff(tempo.coord$y.range) / 80))
# }
coord.names <- c(coord.names, "y.second.tick.positions")
}else if(( ! is.null(y.inter.tick.nb)) & y.log == "no"){
# if(y.inter.tick.nb > 0){ #inactivated because already checked before
if(vertical == TRUE){
ticks.pos <- if(is.null(attributes(tempo.coord$y$breaks))){tempo.coord$y$breaks}else{unlist(attributes(tempo.coord$y$breaks))} # layout$panel_params[[1]]$y$breaks can be characters (labels of the axis). In that case, it has attributes that corresponds to positions # code before ggplot2 3.3.0: suppressWarnings(as.numeric(tempo.coord$y.labels)) # too difficult to predict the behavior of tempo.coord$x.major_source depending on y.lim neg or not, inv or not
ticks.pos <- ticks.pos[ ! is.na(ticks.pos)]
if(length(ticks.pos) == 0){
tempo.cat <- paste0("\n\n============\n\nINTERNAL CODE ERROR IN ", function.name, ": CODE INCONSISTENCY 12\n\n============\n\n")
stop(tempo.cat)
}
tick.dist <- mean(diff(ticks.pos), na.rm = TRUE)
minor.tick.dist <- tick.dist / (y.inter.tick.nb + 1)
minor.tick.pos <- seq(ticks.pos[1] - tick.dist, ticks.pos[length(ticks.pos)] + tick.dist, by = minor.tick.dist)
minor.tick.pos <- minor.tick.pos[minor.tick.pos >= min(y.lim) & minor.tick.pos <= max(y.lim)]
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::annotate(geom = "segment", y = minor.tick.pos, yend = minor.tick.pos, x = tempo.coord$x.range[1], xend = tempo.coord$x.range[1] + diff(tempo.coord$x.range) / 80))
}else{
ticks.pos  <- if(is.null(attributes(tempo.coord$x$breaks))){tempo.coord$x$breaks}else{unlist(attributes(tempo.coord$x$breaks))} # layout$panel_params[[1]]$y$breaks can be characters (labels of the axis). In that case, it has attributes that corresponds to positions # code before ggplot2 3.3.0: suppressWarnings(as.numeric(tempo.coord$x.labels))# too difficult to predict the behavior of tempo.coord$x.major_source depending on y.lim neg or not, inv or not
ticks.pos <- ticks.pos[ ! is.na(ticks.pos)]
if(length(ticks.pos) == 0){
tempo.cat <- paste0("\n\n============\n\nINTERNAL CODE ERROR IN ", function.name, ": CODE INCONSISTENCY 13\n\n============\n\n")
stop(tempo.cat)
}
tick.dist <- mean(diff(ticks.pos), na.rm = TRUE)
minor.tick.dist <- tick.dist / (y.inter.tick.nb + 1)
minor.tick.pos <- seq(ticks.pos[1] - tick.dist, ticks.pos[length(ticks.pos)] + tick.dist, by = minor.tick.dist)
minor.tick.pos <- minor.tick.pos[minor.tick.pos >= min(y.lim) & minor.tick.pos <= max(y.lim)]
assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::annotate(geom = "segment", y = minor.tick.pos, yend = minor.tick.pos, x = tempo.coord$y.range[1], xend = tempo.coord$y.range[1] + diff(tempo.coord$y.range) / 80))
}
y.second.tick.values <- minor.tick.pos
coord.names <- c(coord.names, "y.second.tick.positions")
}
# end secondary ticks (after ggplot2::coord_cartesian() or ggplot2::coord_flip())
# end y scale  management (cannot be before dot plot management)




# drawing
fin.plot <- eval(parse(text = paste(paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + ")))
if(plot == TRUE){
# following lines inactivated because of problem in warn.recov and message.recov
# assign("env_fun_get_message", new.env())
# assign("tempo.gg.name", tempo.gg.name, envir = env_fun_get_message)
# assign("tempo.gg.count", tempo.gg.count, envir = env_fun_get_message)
# assign("add", add, envir = env_fun_get_message)
# two next line: for the moment, I cannot prevent the warning printing
# warn.recov <- fun_get_message(paste(paste(paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + "), if(is.null(add)){NULL}else{add}), kind = "warning", header = FALSE, print.no = FALSE, env = env_fun_get_message) # for recovering warnings printed by ggplot() functions
# message.recov <- fun_get_message('print(eval(parse(text = paste(paste(paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + "), if(is.null(add)){NULL}else{add}))))', kind = "message", header = FALSE, print.no = FALSE, env = env_fun_get_message) # for recovering messages printed by ggplot() functions
suppressMessages(suppressWarnings(print(fin.plot)))
# suppressMessages(suppressWarnings(print(eval(parse(text = paste(paste(paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + "), if(is.null(add)){NULL}else{add}))))))
}else{
# following lines inactivated because of problem in warn.recov and message.recov
# message.recov <- NULL
# warn.recov <- NULL
warn.count <- warn.count + 1
tempo.warn <- paste0("(", warn.count,") PLOT NOT SHOWN AS REQUESTED")
warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
}
# end drawing



# outputs
# following lines inactivated because of problem in warn.recov and message.recov
# if( ! (is.null(warn) & is.null(warn.recov) & is.null(message.recov))){
# warn <- paste0(warn, "\n\n", if(length(warn.recov) > 0 | length(message.recov) > 0){paste0(paste0("MESSAGES FROM ggplot2 FUNCTIONS: ", ifelse( ! is.null(warn.recov), unique(message.recov), ""), ifelse( ! is.null(message.recov), unique(message.recov), ""), collapse = "\n\n"), "\n\n")})
# }else if( ! (is.null(warn) & is.null(warn.recov)) & is.null(message.recov)){
# warn <- paste0(warn, "\n\n", if(length(warn.recov) > 0){paste0(paste0("MESSAGES FROM ggplot2 FUNCTIONS: ", unique(warn.recov), collapse = "\n\n"), "\n\n")})
# }else if( ! (is.null(warn) & is.null(message.recov)) & is.null(warn.recov)){
# warn <- paste0(warn, "\n\n", if(length(message.recov) > 0){paste0(paste0("MESSAGES FROM ggplot2 FUNCTIONS: ", unique(message.recov), collapse = "\n\n"), "\n\n")})
# }
if(warn.print == TRUE & ! is.null(warn)){
warning(paste0("FROM ", function.name, " FUNCTION:\n\n", warn), call. = FALSE) # to recover the warning messages, use return = TRUE
}
if(return == TRUE){
output <- ggplot2::ggplot_build(fin.plot)
output$data <- output$data[-1] # remove the first data because corresponds to the initial empty boxplot
if(length(output$data) != length(coord.names)){
tempo.cat <- paste0("\n\n================\n\nINTERNAL CODE ERROR IN ", function.name, ": length(output$data) AND length(coord.names) MUST BE IDENTICAL. CODE HAS TO BE MODIFIED\n\n================\n\n")
stop(tempo.cat)
}else{
names(output$data) <- coord.names
}
tempo <- output$layout$panel_params[[1]]
output <- list(
data = data1, 
stat = stat, 
removed.row.nb = removed.row.nb, 
removed.rows = removed.rows, 
plot = c(output$data, y.second.tick.values = list(y.second.tick.values)), 
panel = facet.categ, 
axes = list(
x.range = tempo$x.range, 
x.labels = if(is.null(attributes(tempo$x$breaks))){tempo$x$breaks}else{tempo$x$scale$get_labels()}, # is.null(attributes(tempo$x$breaks)) test if it is number (TRUE) or character (FALSE)
x.positions = if(is.null(attributes(tempo$x$breaks))){tempo$x$breaks}else{unlist(attributes(tempo$x$breaks))}, 
y.range = tempo$y.range, 
y.labels = if(is.null(attributes(tempo$y$breaks))){tempo$y$breaks}else{tempo$y$scale$get_labels()}, 
y.positions = if(is.null(attributes(tempo$y$breaks))){tempo$y$breaks}else{unlist(attributes(tempo$y$breaks))}
), 
warn = paste0("\n", warn, "\n\n")
)
return(output)
}
# end outputs
# end main code
}

