#' @title gg_miami
#' @description
#' Plot two ggplot2 manhattan plots in mirror.
#' @param x_lim single character string of the x-axis limits. Either "whole" for the whole genome, region to have the regions of the region parameter (i.e., "whole" if region == none), or a character string written like the region parameter, to have the x-axis limited to the x_lim parameter. Write NULL to do not plot results. 
#' @param vgrid single character string of logical value TRUE or FALSE. Display the chromosome separators in the miami plot? Example: vgrid = TRUE.
#' @param top.y.column NEG_LOG10_P_VALUE_CARRIER_MODEL: single character string of any of the quantitative column of the res_fisher.tsv file for the y-axis of the manhattan plot at the top of the miami plot. Can also be an added column through the tsv_extra_fields parameter..
#' @param bottom.y.column  as the top_y_column parameter but for the bottom manhattan plot of the miami plot. NULL generates a simple manhattan plot.
#' @param color.column single character string of one of the column name of the res_fisher.tsv file (see bottom_y_column) in order color the dots. Write NULL if not required (dots will be alternatively grey and blue, according to chromo order).
#' @param dot.border.color single color character string to color the border of the dots. Write NULL if not required.
#' @param y_lim1 = NULL single character string of the y-axis limits of the top panel in the miami plot, made of two numbers, separated by a single space. Example: y_lim1 = 0 3. Write NULL for no particular limit.
#' @param y_lim2 = NULL single character string of the y-axis limits of the bottom panel in the miami plot, made of two numbers, separated by a single space. Example: y_lim2 = 0 3 .Write NULL for no particular limit. Not considered if bottom_y_column = NULL.
#' @param y_reverse1 = FALSE single character string of logical value TRUE or FALSE, y-axis coordinates flip for the top panel in the miami plot. Example: y_reverse1 = TRUE.
#' @param y_reverse2 = FALSE: single character string of logical value TRUE or FALSE, y-axis coordinates flip for the bottom panel in the miami plot. Example: y_reverse2 = TRUE.
#' @param y_threshold single character string made of 1 numeric value for the y-axis threshold of the top panel in the miami plot, beyond which values are of interest. Example: y_threshold1 = 3. Write NULL for no particular threshold.
#' @param y_threshold2 single numeric value for the y-axis threshold of the bottom panel in the miami plot, beyond which values are of interest. Example: y_threshold2 = 3. Write NULL for no particular threshold. Not considered if bottom_y_column = NULL.
#' @param y_log1: single logical value TRUE or FALSE for the  y-axis log10 scale of the top panel in the miami plot. Example: y_log1 = TRUE.
#' @param y_log2: single logical value TRUE or FALSE for the  y-axis log10 scale of the bottom panel in the miami plot. Example: y_log2 = TRUE.
#' @returns a list containing: $angle: the submitted angle (value potentially reduced to fit the [-360 ; 360] interval, e.g., 460 -> 100, without impact on the final angle displayed); $pos: the selected position (argument pos); $kind: the selected kind of text (argument kind); $hjust: the horizontal justification; $vjust: the vertical justification.
#' @examples
#' obs1 <- data.frame(Km = c(20, 10, 1, 5), Car = c("TUUT", "WIIM", "BIP", "WROUM"), Color1 = 1:4, color2 = c("red", "blue", "green", "black"), Country = c("FR", "UK", "US", NA), stringsAsFactors = TRUE) ; gg_donut(data1 = obs1, freq = "Km", categ = "Car", annotation = "Country")
#' @importFrom ggplot2 annotation_custom
#' @importFrom ggplot2 ggtitle
#' @importFrom gridExtra grid.arrange
#' @importFrom grDevices pdf
#' @importFrom grDevices png
#' @importFrom saferDev arg_check
#' @importFrom scales math_format
#' @importFrom scales rescale_none
#' @importFrom scales trans_breaks
#' @importFrom scales trans_format
#' @importFrom scales rescale_none
#' @details
#' - Rows containing NA in data1[, c(freq, categ)] will be removed before processing, with a warning (see below).
#' - Size arguments (hole.text.size, border.size, title.text.size and annotation.size) are in mm. See Hadley comment in https://stackoverflow.com/questions/17311917/ggplot2-the-unit-of-size. See also http://sape.inf.usi.ch/quick-reference/ggplot2/size). Unit object are not accepted, but conversion can be used (e.g., grid::convertUnit(grid::unit(0.2, "inches"), "mm", valueOnly = TRUE)).
#' @export
gg_miami <- function(
        fisher, 
        chr, 
        top.y.column,
        bottom.y.column,
        x.lim = "whole", 
        vgrid = FALSE, 
        color.column = NULL,
        dot.border.color = NULL, 
        y.lim1 = NULL, 
        y.lim2 = NULL,
        reverse1 = FALSE, 
        reverse2 = FALSE, 
        y.threshold1 = NULL, 
        y.threshold2 = NULL, 
        y.log1 = FALSE, 
        y.log2 = FALSE){
    # ARGUMENTS
    # data1: a dataframe compatible with ggplot2
    # freq: single character string of the data1 column name of the frequencies
    # categ: single character string of the data1 column name of categories (qualitative variable)
    # fill.palette: single character string of a palette name (see ?ggplot2::scale_fill_brewer() for the list).Ignored if fill.color is not NULL
    # fill.color: either (1) NULL, or (2) a vector of character strings or integers of same length as the number of classes in categ. Colors can be color names (see ?colors() in R), hexadecimal color codes, or integers (according to the ggplot2 palette). The order of the elements will be used according to the frequency values, from highest to lowest. An easy way to use this argument is to sort data1 according to the frequencies values, add a color column with the corresponding desired colors and use the content of this column as values of fill.color. If color is NULL and fill.palette is NULL, default colors of ggplot2 are used. If color is not NULL, it overrides fill.palette
    # hole.size: single positive proportion of donut central hole, 0 meaning no hole (pie chart) and 1 no plot (donut with a null thickness)
    # hole.text: logical (either TRUE or FALSE). Display the sum of frequencies (column of data1 indicated in the freq argument) ?
    # hole.text.size: single positive numeric value of the title font size in mm. Ignored if hole.text is FALSE
    # border.color: a single character string or integer. Colors can be color names (see ?colors() in R), hexadecimal color codes, or integers (according to the ggplot2 palette)
    # border.size: single numeric value of border tickness in mm. Write zero for no dot border
    # title: single character string of the graph title
    # title.text.size: single numeric value of the title font size in mm
    # annotation: single character string of the data1 column name of annotations. Values inside this column will be displayed over the corresponding slices of the donut. Write NULL if not required
    # annotation.distance: single positive numeric value of the distance from the center of the slice. 0 means center of the slice, 0.5 means at the edge. Above 0.5, the donut will be reduced to make place for the annotation. Ignored if annotation is NULL
    # annotation.size: single positive numeric value of the annotation font size in mm. Ignored if annotation is NULL
    # annotation.force: single positive numeric value of the force of repulsion between overlapping text labels. See ?ggrepel::geom_text_repel() in R. Ignored if annotation is NULL
    # annotation.force.pull: single positive numeric value of the force of attraction between a text label and its corresponding data point. See ?ggrepel::geom_text_repel() in R. Ignored if annotation is NULL
    # legend.show: logical (either TRUE or FALSE). Show legend? 
    # legend.width: single proportion (between 0 and 1) indicating the relative width of the legend sector (on the right of the plot) relative to the width of the plot. Value 1 means that the window device width is split in 2, half for the plot and half for the legend. Value 0 means no room for the legend, which will overlay the plot region. Write NULL to inactivate the legend sector. In such case, ggplot2 will manage the room required for the legend display, meaning that the width of the plotting region can vary between graphs, depending on the text in the legend
    # legend.name: character string of the legend title. If legend.name is NULL then legend.name is the value of the categ argument. Write legend.name = "" to remove the legend
    # legend.text.size: single numeric value of the font size in mm of the legend labels
    # legend.box.size: single numeric value of the size of the legend squares in mm
    # legend.box.space: single numeric value of the space between the legend boxes in mm
    # legend.limit: single positive proportion of the classes displayed in the legend for which the corresponding proportion is over legend.limit. Write NULL to display all the classes
    # legend.add.prop: logical (either TRUE or FALSE). add the proportion after the class names in the legend ?
    # add: character string allowing to add more ggplot2 features (dots, lines, themes, facet, etc.). Ignored if NULL
    # WARNING: (1) the string must start with "+", (2) the string must finish with ")" and (3) each function must be preceded by "ggplot2::". Example: "+ ggplot2::coord_flip() + ggplot2::theme_bw()"
    # If the character string contains the "ggplot2::theme" string, then the article argument of gg_donut() (see above) is ignored with a warning. In addition, some arguments can be overwritten, like x.angle (check all the arguments)
    # Handle the add argument with caution since added functions can create conflicts with the preexisting internal ggplot2 functions
    # WARNING: the call of objects inside the quotes of add can lead to an error if the name of these objects are some of the gg_donut() arguments. Indeed, the function will use the internal argument instead of the global environment object. Example article <- "a" in the working environment and add = '+ ggplot2::ggtitle(article)'. The risk here is to have TRUE as title. To solve this, use add = '+ ggplot2::ggtitle(get("article", envir = .GlobalEnv))'
    # return: logical (either TRUE or FALSE). Return the graph parameters?
    # return.ggplot: logical (either TRUE or FALSE). Return the ggplot object in the output list? Ignored if return argument is FALSE. WARNING: always assign the gg_donut() function (e.g., a <- gg_donut()) into something if the return.ggplot argument is TRUE, otherwise, double plotting is performed. See $ggplot in the RETURN section below for more details
    # return.gtable: logical (either TRUE or FALSE). Return the full graph (main, title and legend) as a gtable of grobs in the output list? See $gtable in the RETURN section below for more details
    # plot: logical (either TRUE or FALSE). Plot the graphic? If FALSE and return argument is TRUE, graphical parameters and associated warnings are provided without plotting
    # warn.print: logical (either TRUE or FALSE). Print warnings at the end of the execution? ? If FALSE, warning messages are never printed, but can still be recovered in the returned list. Some of the warning messages (those delivered by the internal ggplot2 functions) are not apparent when using the argument plot = FALSE
    # lib.path: vector of character strings indicating the absolute path of the required packages (see below). if NULL, the function will use the R library default folders
    # RETURN
    # a donut plot if plot argument is TRUE
    # a list of the graph info if return argument is TRUE:
    # $data: the initial data with modifications and with graphic information added
    # $removed.row.nb: a list of the removed rows numbers in data frames (because of NA). NULL if no row removed
    # $removed.rows: a list of the removed rows in data frames (because of NA). NULL if no row removed
    # $plot.data
    # $panel: the variable names used for the panels (NULL if no panels). WARNING: NA can be present according to ggplot2 upgrade to v3.3.0
    # $axes: the x-axis and y-axis info
    # $warn: the warning messages. Use cat() for proper display. NULL if no warning. WARNING: warning messages delivered by the internal ggplot2 functions are not apparent when using the argument plot = FALSE
    # $ggplot: ggplot object that can be used for reprint (use print($ggplot) or update (use $ggplot + ggplot2::...). NULL if return.ggplot argument is FALSE. Warning: the legend is not in $ggplot as it is in a separated grob (use $gtable to get it). Of note, a non-null $ggplot in the output list is sometimes annoying as the manipulation of this list prints the plot
    # $gtable: gtable object that can be used for reprint (use gridExtra::grid.arrange(...$ggplot) or with additionnal grobs (see the grob decomposition in the examples). Contrary to $ggplot, a non-NULL $gtable in the output list is not annoying as the manipulation of this list does not print the plot
    

    # DEBUGGING
    # obs1 <- data.frame(Km = c(20, 10, 1, 5), Car = c("TUUT", "WIIM", "BIP", "WROUM"), Color1 = 1:4, color2 = c("red", "blue", "green", "black"), Country = c("FR", "UK", "US", NA), stringsAsFactors = TRUE) ; data1 = obs1 ; freq = "Km" ; categ = "Car" ; fill.palette = NULL ; fill.color = NULL ; hole.size = 0.5 ; hole.text = TRUE ; hole.text.size = 12 ; border.color = "gray50" ; border.size = 0.1 ; title = "" ; title.text.size = 12 ; annotation = "Country" ; annotation.distance = 0.5 ; annotation.size = 3 ; annotation.force = 1 ; annotation.force.pull = 100 ; legend.show = TRUE ; legend.width = 0.5 ; legend.name = NULL ; legend.text.size = 10 ; legend.box.size = 5 ; legend.box.space = 2 ; legend.limit = NULL ; legend.add.prop = FALSE ; add = NULL ; return = TRUE ; return.ggplot = FALSE ; return.gtable = TRUE ; plot = TRUE ; warn.print = FALSE ; lib.path = NULL
    # package name
    package.name <- "ggcute"
    # end package name
    # function name
    function.name <- base::paste0(base::as.list(base::match.call(expand.dots = FALSE))[[1]], "()") # function name with "()" paste, which split into a vector of three: c("::()", "package()", "function()") if "package::function()" is used.
    if(function.name[1] == "::()"){
        function.name <- function.name[3]
    }
    arg.names <- base::names(base::formals(fun = base::sys.function(base::sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- base::as.list(base::match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name
    # critical operator checking
    .base_op_check(external.function.name = function.name)
    # end critical operator checking
    # package checking
    # check of lib.path
    # end check of lib.path

    # check of the required function from the required packages
    .pack_and_function_check(
        fun = base::c(
            "ggplot2::annotation_custom",
            "ggplot2::ggtitle",
            "gridExtra::grid.arrange",
            "grDevices::pdf",
            "grDevices::png",
            "saferDev::arg_check",
            "scales::math_format",
            "scales::rescale_none",
            "scales::trans_breaks",
            "scales::trans_format",
            "scales::rescale_none"
       
        ),
        lib.path = NULL,
        external.function.name = function.name
    )
    # end check of the required function from the required packages
    # end package checking

  
    # argument primary checking
    # arg with no default values
    mandat.args <- base::c(
        "bottom.y.column",
        "chr", 
        "fisher", 
        "top.y.column"
    )
    tempo <- base::eval(base::parse(text = base::paste0("base::missing(", base::paste0(mandat.args, collapse = ") | base::missing("), ")")))
    if(base::any(tempo)){ # normally no NA for missing() output
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nFOLLOWING ARGUMENT", base::ifelse(base::sum(tempo, na.rm = TRUE) > 1, "S HAVE", "HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", base::paste0(mandat.args, collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    # argument checking with arg_check()    
    # argument checking
    argum.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- base::expression(argum.check <- base::c(argum.check, tempo$problem) , text.check <- base::c(text.check, tempo$text) , checked.arg.names <- base::c(checked.arg.names, tempo$object.name))
    tempo <- saferDev::arg_check(data = fisher, class = "vector", typeof = "character", length = 1) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = chr.path, class = "vector", typeof = "character", length = 1) ; base::eval(ee)
    # tempo <- saferDev::arg_check(data = cute, class = "vector", typeof = "character", length = 1) ; eval(ee) # check above
    if(base::all(x.lim != "NULL")){
        tempo <- saferDev::arg_check(data = x.lim, class = "vector", typeof = "character", length = 1) ; base::eval(ee)
    }else{
        x.lim <- NULL
    }
    tempo <- saferDev::arg_check(data = vgrid, class = "vector", typeof = "character", length = 1) ; base::eval(ee)
    if(base::all(top.y.column != "NULL")){
        tempo <- saferDev::arg_check(data = top.y.column, class = "vector", typeof = "character", length = 1) ; base::eval(ee)
    }else{
        top.y.column <- NULL
    }
    if(base::all(bottom.y.column != "NULL")){
        tempo <- saferDev::arg_check(data = bottom.y.column, class = "vector", typeof = "character", length = 1) ; base::eval(ee)
    }else{
        bottom.y.column <- NULL
    }
    if(base::all(color.column != "NULL")){
        tempo <- saferDev::arg_check(data = color.column, class = "vector", typeof = "character", length = 1) ; base::eval(ee)
    }else{
        color.column <- NULL
    }
    if(base::all(dot.border.color != "NULL")){
        tempo <- saferDev::arg_check(data = dot.border.color, class = "vector", typeof = "character", length = 1) ; base::eval(ee)
    }else{
        dot.border.color <- NULL
    }
    if(base::all(y.lim1 != "NULL")){
        tempo <- saferDev::arg_check(data = y.lim1, class = "vector", typeof = "character", length = 1) ; base::eval(ee)
    }else{
        y.lim1 <- NULL
    }
    if(base::all(y.lim2 != "NULL")){
        tempo <- saferDev::arg_check(data = y.lim2, class = "vector", typeof = "character", length = 1) ; base::eval(ee)
    }else{
        y.lim2 <- NULL
    }
    tempo <- saferDev::arg_check(data = reverse1, class = "vector", typeof = "character", length = 1) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = reverse2, class = "vector", typeof = "character", length = 1) ; base::eval(ee)
    if(base::all(y.threshold1 != "NULL")){
        tempo <- saferDev::arg_check(data = y.threshold1, class = "vector", typeof = "character", length = 1) ; base::eval(ee)
    }else{
        y.threshold1 <- NULL
    }
    if(base::all(y.threshold2 != "NULL")){
        tempo <- saferDev::arg_check(data = y.threshold2, class = "vector", typeof = "character", length = 1) ; base::eval(ee)
    }else{
        y.threshold2 <- NULL
    }
    tempo <- saferDev::arg_check(data = y.log1, class = "vector", typeof = "character", length = 1) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = log, class = "vector", typeof = "character", length = 1) ; base::eval(ee)
    if(base::any(arg.check) == TRUE){ # normally no NA
        base::stop(base::paste0("\n\n================\n\n", base::paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between == #
    }
     # end argument checking with arg_check()
    # check with r_debugging_tools
    # source("C:/Users/yhan/Documents/Git_projects/debugging_tools_for_r_dev/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using arg_check()
    # end check with r_debugging_tools
    # end argument primary checking


    # second round of checking and data preparation
    # reserved words
    # end reserved words
    # management of NA arguments
    if( ! (base::all(base::class(arg.user.setting) == "list", na.rm = TRUE) & base::length(arg.user.setting) == 0)){
        tempo.arg <- base::names(arg.user.setting) # values provided by the user
        tempo.log <- base::suppressWarnings(base::sapply(base::lapply(base::lapply(tempo.arg, FUN = base::get, envir = base::sys.nframe(), inherits = FALSE), FUN = base::is.na), FUN = base::any)) & base::lapply(base::lapply(tempo.arg, FUN = base::get, envir = base::sys.nframe(), inherits = FALSE), FUN = base::length) == 1L # no argument provided by the user can be just NA
        if(base::any(tempo.log) == TRUE){ # normally no NA because is.na() used here
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", base::paste0(tempo.arg[tempo.log], collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    # management of NULL arguments
    tempo.arg <-base::c(
        "fisher",
        "chr.path", 
        "vgrid", 
        "reverse1", 
        "reverse2", 
        "y.log1", 
        "y.log2", 
        "log"
    )
    tempo.log <- base::sapply(base::lapply(tempo.arg, FUN = base::get, envir = base::sys.nframe(), inherits = FALSE), FUN = base::is.null)
    if(base::any(tempo.log) == TRUE){# normally no NA with is.null()
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE:\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), base::paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end management of NULL arguments
    # management of ""
    tempo.arg <-base::c(
        "fisher", 
        "chr.path", 
        "x.lim", 
        "vgrid", 
        "top.y.column",
        "bottom.y.column", 
        "color.column", 
        "dot.border.color", 
        "y.lim1", 
        "y.lim2", 
        "reverse1", 
        "reverse2",
        "y.threshold1", 
        "y.threshold2", 
        "y.log1", 
        "y.log2", 
        "cute", 
        "log"
    )
    tempo.log <- base::sapply(base::lapply(tempo.arg, FUN = get, env = base::sys.nframe(), inherit = FALSE), FUN = function(x){base::any(x == "")})
    if(base::any(tempo.log) == TRUE){# normally no NA with is.null()
        tempo.cat <- base::paste0("ERROR IN miami.R:\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), base::paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE \"\"")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end management of ""
    # code that protects set.seed() in the global environment
    # end code that protects set.seed() in the global environment
    # warning initiation
    ini.warning.length <- base::options()$warning.length
    base::options(warning.length = 8170)
    warn <- NULL
    # warn.count <- 0 # not required
    # end warning initiation
    # other checkings
    for(i0 in base::c("vgrid", "reverse1", "reverse2", "y.log1", "y.log2")){
        if(base::get(i0) == "TRUE"){
            base::assign(i0, TRUE)
        }else if(base::get(i0) == "FALSE"){
            base::assign(i0, FALSE)
        }else{
            tempo.cat <- base::paste0("ERROR IN miami.R\n", i0, " PARAMETER CAN ONLY BE \"TRUE\" OR \"FALSE\": ", base::get(i0))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end other checkings
    # end second round of checking and data preparation

    # main code
    # ignition
    fun_report(data = base::paste0("\n\n################################################################ miami PROCESS\n\n"), output = log, path = "./", overwrite = TRUE)
    ini.date <- base::Sys.time()
    ini.time <- base::as.numeric(ini.date) # time of process begin, converted into seconds
    fun_report(data = base::paste0("\n\n################################ RUNNING DATE AND STARTING TIME\n\n"), output = log, path = "./", overwrite = FALSE)
    fun_report(data = base::paste0(ini.date, "\n\n"), output = log, path = "./", overwrite = FALSE)
    fun_report(data = base::paste0("\n\n################################ RUNNING\n\n"), output = log, path = "./", overwrite = FALSE)
    # end ignition
    # graphical parameter initialization


    grDevices::pdf(file = NULL)
    par.ini <- base::par(no.readonly = TRUE) # to recover the initial graphical parameters if required (reset)
    base::invisible(base::dev.off()) # close the new window
    zone.ini <- base::matrix(1, ncol=1)
    if(erase.graphs == TRUE){
        base::graphics.off()
    }else{
        tempo.warn <- base::paste0("GRAPHICS HAVE NOT BEEN ERASED. GRAPHICAL PARAMETERS MAY HAVE NOT BEEN REINITIALIZED")
        fun_report(data = base::paste0("WARNING\n", tempo.warn), output = log, path = "./", overwrite = FALSE)
        warn <- base::paste0(base::ifelse(base::is.null(warn), tempo.warn, base::paste0(warn, "\n\n", tempo.warn)))
    }
    # end graphical parameter initialization


    # data import
    if( ! base::file.exists(fisher)){
        base::stop(base::paste0("\n\n============\n\nERROR IN miami.R\nFILE INDICATED IN THE fisher PARAMETER DOES NOT EXISTS: ", fisher, "\n\n============\n\n"), call. = FALSE)
    }else{
        obs <- base::read.table(fisher, sep = "\t", stringsAsFactors = FALSE, header = TRUE, comment.char = "")
        if(base::length(obs) > 0 & base::nrow(obs) > 0){
            empty.obs <- FALSE
        }else{
            empty.obs <- TRUE
        }
    }
    if( ! base::file.exists(chr.path)){
        base::stop(base::paste0("\n\n============\n\nERROR IN miami.R\nFILE INDICATED IN THE chr.path PARAMETER DOES NOT EXISTS: ", chr.path, "\n\n============\n\n"), call. = FALSE)
    }else{
        chr <- base::read.table(chr.path, sep = "\t", stringsAsFactors = FALSE, header = TRUE, comment.char = "")
    }
    # end data import
    # modifications of imported tables
    xmin_plot <- 0 # coordinates for plotting 
    if(base::length(obs) > 0 & base::nrow(obs) > 0){
        # names(obs)[names(obs) == "NEG_LOG10_P_VALUE"] <- "neg.log10.p"
        # names(obs)[names(obs) == "PATIENT_NB"] <- "Nb_of_indiv"
        if(base::any(base::grepl(x = obs$CHROM, pattern = "chr"))){
            obs$CHROM <- base::sub(x = obs$CHROM, pattern = "chr", replacement = "")
        }
        if(base::any(base::grepl(x = obs$CHROM, pattern = "^X$"))){
            obs$CHROM[base::grepl(x = obs$CHROM, pattern = "^X$")] <- "23"
        }
        if(base::any(base::grepl(x = obs$CHROM, pattern = "^Y$"))){
            obs$CHROM[base::grepl(x = obs$CHROM, pattern = "^Y$")] <- "24"
        }
        if(base::any(base::grepl(x = obs$CHROM, pattern = "^MT$|^M$"))){
            obs$CHROM[base::grepl(x = obs$CHROM, pattern = "^MT$|^M$")] <- "25"
        }
        if(base::any( ! base::grepl(x = obs$CHROM, pattern = "\\d"))){
            tempo.cat <- base::paste0("ERROR IN miami.R:\nTHE chr COLUMN of the fisher.tsv FILE HAS LETTERS IN IT, OTHER THAN X, Y and MT:\n", base::paste0(obs$CHROM[base::grepl(x = obs$CHROM, pattern = "^\\d")], collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else{
            obs$CHROM <- base::as.integer(obs$CHROM)
        }
        # add the chr info to obs
        obs <- base::data.frame(obs, coord = base::vector(mode = "numeric", length = base::nrow(obs)))
        for(i1 in chr$CHR_NB){
            obs$coord[obs$CHROM == i1] <- obs$POS[obs$CHROM == i1] + chr$LENGTH_CUMUL_TO_ADD[i1]
        }
        # preparation of the x coordinates: three solutions: 1) whole object (see above), 2) single chromo "chr7" or "chr7:0-15", 3) several chromo chr7, chr8" or "chr7:0-15, chr8" or "chr7:0-15, chr8:0-20"
        # The idea is to select rows of chr and potentially restrict some chr limits
        if( ! base::is.null(x.lim)){
            is.whole <- FALSE
            if(x.lim == whole){ #at that stage, x.lim is a single character
                is.whole <- TRUE
            }
            tempo <- base::strsplit(x = x.lim, split = ",")[[1]]
            tempo <- base::gsub(x = tempo, pattern = " ", replacement = "")
            if( ! base::all(base::grepl(x = tempo, pattern = "^chr.+"))){
                tempo.cat <- base::paste0("ERROR IN miami.R:\nTHE x_lim PARAMETER MUST START WITH \"chr\" IF NOT \"none\":\n", base::paste0(x.lim, collapse = " "))
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            if(base::any(base::grepl(x = tempo, pattern = ":"))){
                # means that there are coordinates
                if( ! base::all(base::grepl(tempo, pattern = "-"))){# normally no NA with is.null()
                    tempo.cat <- base::paste0("ERROR IN miami.R:\nTHE x_lim PARAMETER MUST BE WRITTEN LIKE THIS \"chr7:0-147000000, chr10:1000000-2000000\" IF COORDINATES ARE SPECIFIED: \n", base::paste0(x.lim, collapse = " "))
                    base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
                }
                tempo2 <- base::strsplit(x = tempo, split = ":")
                chr_x_lim <- base::sapply(X = tempo2, FUN = function(x){x[1]})
                chr_x_lim <- base::gsub(x = chr_x_lim, pattern = " ", replacement = "")
                coord_x_lim <- base::sapply(X = tempo2, FUN = function(x){x[2]})
                tempo3 <- base::strsplit(x = coord_x_lim, split = "-")
                xmin_x_lim <- base::sapply(X = tempo3, FUN = function(x){x[1]})
                xmin_x_lim <- base::gsub(x = xmin_x_lim, pattern = " ", replacement = "")
                xmax_x_lim <- base::sapply(X = tempo3, FUN = function(x){x[2]})
                xmax_x_lim <- base::gsub(x = xmax_x_lim, pattern = " ", replacement = "")
                if(base::any(base::grepl(xmin_x_lim, pattern = "\\D")) | base::any(base::grepl(xmax_x_lim, pattern = "\\D"))){# normally no NA with is.null()
                    tempo.cat <- base::paste0("ERROR IN miami.R:\nTHE x_lim PARAMETER MUST BE WRITTEN LIKE THIS \"chr7:0-147000000, chr10:1000000-2000000\" IF COORDINATES ARE SPECIFIED: \n", base::paste0(x.lim, collapse = " "))
                    base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
                }else{
                    xmin_x_lim <- base::as.integer(xmin_x_lim)
                    xmax_x_lim <- base::as.integer(xmax_x_lim)
                    if(base::any(xmax_x_lim - xmin_x_lim < 0)){
                        tempo.cat <- base::paste0("ERROR IN miami.R:\nTHE x_lim PARAMETER MUST BE WRITTEN WITH ORDERED COORDINATES, LIKE THIS \"chr7:0-147000000, chr10:1000000-2000000\", IF COORDINATES ARE SPECIFIED: \n", base::paste0(x.lim, collapse = " "))
                        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
                    }
                }
            }else{
                chr_x_lim <- tempo
                coord_x_lim <- NULL
                xmin_x_lim <- NULL
                xmax_x_lim <- NULL
            }
            # modification of the chr object for restricted plotting
            tempo.coord <- base::which(chr$CHR %in% chr_x_lim) # which rows of chr to take for plotting
            if(base::any(chr$BP_LENGTH[tempo.coord] - xmax_x_lim < 0)){
                tempo.cat <- base::paste0("ERROR IN miami.R:\nTHE x_lim PARAMETER HAS AT LEAST ONE COORDINATE THAT IS ABOVE THE MAX LENGTH OF THE CHROMO.\nCHROMO LENGTH: ", base::paste0(chr$BP_LENGTH[tempo.coord], collapse = " "), "\nMAX COORDINATE: ", base::paste0(xmax_x_lim, collapse = " "))
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            if(tempo.coord[1] > 1){
                xmin_plot <- chr$LENGTH_CUMUL[tempo.coord[1] - 1]
            }
            chr <- chr[tempo.coord[1]:tempo.coord[base::length(tempo.coord)], ]
            if( ! base::is.null(coord_x_lim)){
                xmin_plot <- xmin_plot + xmin_x_lim[1] # the left boundary of the plot is corrected
                chr$LENGTH_CUMUL[base::nrow(chr)] <- chr$LENGTH_CUMUL[base::nrow(chr)] - chr$BP_LENGTH[base::nrow(chr)] + xmax_x_lim[base::length(xmax_x_lim)] # the right boundary of the plot is corrected
                chr$CHR_NAME_POS <- (base::c(xmin_plot, chr$LENGTH_CUMUL[-base::nrow(chr)]) + chr$LENGTH_CUMUL) / 2 # the positions of names in the x-axis of the plot are corrected
            }
            # restriction of obs
            obs <- obs[obs$coord >= xmin_plot & obs$coord <= chr$LENGTH_CUMUL[base::nrow(chr)], ]
        }else{
            tempo.warn <- base::paste0("x.lim is NULL: NO PLOT DRAWN")
            fun_report(data = base::paste0("WARNING\n", tempo.warn), output = log, path = "./", overwrite = FALSE)
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo.warn,base::paste0(warn, "\n\n", tempo.warn)))
        }
    }

    for(i0 in base::c("y.lim1", "y.lim2")){
        if( ! base::is.null(base::get(i0))){
            tempo <- base::unlist(base::strsplit(x = base::get(i0), split = " "))
            if(base::length(tempo) != 2 | ! base::all(grepl(tempo, pattern = "^[0123456789.\\-\\+eE]*$"))){
                tempo.cat <- base::paste0("ERROR IN miami.R:\nTHE ", i0, " PARAMETER MUST BE TWO NUMERIC VALUES SEPARATED BY A SINGLE SPACE\nHERE IT IS: \n", base::paste0(base::get(i0), collapse = " "))
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }else{
                base::assign(i0, base::as.numeric(tempo))
            }
        }
    }
    for(i0 in base::c("y.threshold1", "y.threshold2")){
        if( ! base::is.null(base::get(i0))){
            tempo <- base::unlist(base::strsplit(x = base::get(i0), split = " "))
            if(base::length(tempo) != 1 | ! base::all(base::grepl(tempo, pattern = "^[0123456789.\\-\\+eE]*$"))){
                tempo.cat <- base::paste0("ERROR IN miami.R:\nTHE ", i0, " PARAMETER MUST BE TWO NUMERIC VALUES SEPARATED BY A SINGLE SPACE\nHERE IT IS: \n", base::paste0(base::get(i0), collapse = " "))
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }else{
                base::assign(i0, base::as.numeric(tempo))
            }
        }
    }

    if( ! top.y.column %in% base::names(obs)){
        tempo.cat <- base::paste0("ERROR IN miami.R:\nTHE top.y.column PARAMETER MUST BE A COLUMN NAME OF THE FISHER TABLE.\n\ntop.y.column PARAMETER:\n", base::paste0(top.y.column, collapse = " "), "\n\nCOLUMN NAMES:\n", base::paste0(base::names(obs), collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if( ! bottom.y.column %in% base::names(obs)){
        tempo.cat <- base::paste0("ERROR IN miami.R:\nTHE bottom.y.column PARAMETER MUST BE A COLUMN NAME OF THE FISHER TABLE.\n\nbottom.y.column PARAMETER:\n", base::paste0(bottom.y.column, collapse = " "), "\n\nCOLUMN NAMES:\n", base::paste0(base::names(obs), collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }



    # end modifications of imported tables


    # plotting
    #fun_open(width = 12, height = 4, pdf.name = paste0("plot_read_length_", kind)) # must be systematically opened for main.nf
    png.size <- 1800 # px
    grDevices::png(filename = base::paste0("miami.png"), width = png.size * 2, height = png.size, units = "px", res = 300)

    if(empty.obs == TRUE){
        fun_gg_empty_graph(text = base::paste0("NO PLOT DRAWN\nTHE region PARAMETER\nMIGHT BE OUTSIDE\nOF THE RANGE OF THE VCF FILE"))
    }else if(base::length(obs) > 0 & base::nrow(obs) > 0 & ! base::is.null(x.lim)){
        marging <- (chr$LENGTH_CUMUL[base::nrow(chr)] - xmin_plot) * 0.005 # chr$LENGTH_CUMUL and xmin_plot have been corrected depending on x.lim boundaries
        y.min.pos <- base::ifelse(base::is.null(y.lim1), base::min(obs[ , top.y.column]), base::min(y.lim1))
        y.max.pos <- base::ifelse(base::is.null(y.lim1), base::max(obs[ , top.y.column]), base::max(y.lim1))
        tempo.gg.name <- "gg.indiv.plot."
        tempo.gg.count <- 0
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot(obs, aes_string(x = "coord", y = top.y.column)))
        if(vgrid){
            base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), geom_vline(
                xintercept = base::c(xmin_plot, chr$LENGTH_CUMUL),
                size = 0.25,
                color = "grey80"
            ))
        }
        if( ! base::is.null(y.threshold1)){
            base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), geom_hline(
                yintercept = y.threshold1,
                linetype = "22", 
                size = 0.25, 
                color = "red"
            ))
        }
        if(base::is.null(color.column)){
            if(base::is.null(dot.border.color)){
                base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), geom_point(
                    aes(color = base::as.factor(CHROM)), 
                    alpha = 1, 
                    pch = 16, 
                    size = 1
                ))
                base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), scale_color_manual(values = base::rep(base::c("grey20", "skyblue"), 25)))
            }else{
                base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), geom_point(
                    aes(fill = base::as.factor(CHROM)), 
                    alpha = 1, 
                    color = dot.border.color, 
                    pch = 21, 
                    size = 1
                ))
                base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), scale_fill_manual(values = base::rep(base::c("grey20", "skyblue"), 25)))
            }
        }else{
            if(base::is.null(dot.border.color)){
                base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), geom_point(
                    aes(color = color.column), 
                    alpha = 1, 
                    pch = 16, 
                    size = 1
                ))
                base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), scale_color_gradient2())
            }else{
                base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), geom_point(
                    aes(fill = base::as.factor(CHROM)), 
                    alpha = 1, 
                    color = dot.border.color, 
                    pch = 21, 
                    size = 1
                ))
                base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), scale_fill_gradient2())
            }
        }
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::ggtitle(
            base::paste0("x.lim: ", base::ifelse(is.whole, "whole genome", x.lim), 
            base::ifelse( ! base::is.null(y.threshold1), base::paste0(", top threshold: ", y.threshold1), ""), 
            base::ifelse( ! (base::is.null(y.threshold2) & base::is.null(bottom.y.column)), base::paste0(", bottom threshold: ", y.threshold2), ""), 
            base::ifelse(y.log1, ", top y-axis: log10", ""), base::ifelse(y.log2, ", bottom y-axis: log10", ""))
        ))
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), scale_x_continuous(
            name = "CHR", 
            expand = base::c(0, 0), # remove space after after axis limits
            oob = scales::rescale_none,
            label = chr$CHR_NAME, 
            breaks= chr$CHR_NAME_POS, 
            limits = base::c(xmin_plot - marging, base::max(chr$LENGTH_CUMUL) + marging)
        ))
        if(y.log1){
            if(base::any(obs[ , top.y.column] <= 0)){
                tempo.cat <- base::paste0("ERROR IN miami.R:\nTHE y_log1 PARAMETER CANNOT BE SET TO \"TRUE\" IF 0 OR NEG VALUES IN THE ", top.y.column, " FIELD OF THE TSV OR VCF")
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }else{
                base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), scale_y_continuous(
                    expand = base::c(0, 0), # remove space after after axis limits
                    limits = if(reverse1){base::c(y.max.pos, y.min.pos)}else{base::c(y.min.pos, y.max.pos)}, # NA indicate that limits must correspond to data limits but ylim() already used
                    oob = scales::rescale_none, 
                    trans = "log10", 
                    breaks = scales::trans_breaks("log10", function(x){10^x}), 
                    labels = scales::trans_format("log10", scales::math_format(10^.x))
                ))
                # assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), annotation_logticks(outside = TRUE))
            }
        }else{
            base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), scale_y_continuous(
                expand = base::c(0, 0), # remove space after after axis limits
                limits = if(reverse1){c(y.max.pos, y.min.pos)}else{base::c(y.min.pos, y.max.pos)}, # NA indicate that limits must correspond to data limits but ylim() already used
                oob = scales::rescale_none, 
                trans = "identity"
            ))
        }
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), theme_bw())
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), theme(
            plot.title = ggplot2::element_text(size = 8), 
            legend.position=if(base::is.null(color.column)){"none"}, 
            panel.border = element_blank(), 
            panel.grid = element_blank(), 
            axis.ticks.x = element_blank(), 
            axis.ticks.y.left = element_line(size = 0.25), 
            # axis.line.x.bottom = element_line(size = 0.25), # ugly, thus i added geom_hline below
            axis.line.y.left = element_line(size = 0.25) 
        ))
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), geom_hline(
            yintercept = base::ifelse(
                base::is.null(y.lim1), 
                base::ifelse(reverse1, base::max(obs[ , top.y.column]), base::min(obs[ , top.y.column])), 
                base::ifelse(reverse1, base::max(y.lim1), base::min(y.lim1))
            ), 
            size = 0.25
        ))
        # add tick lines if vgrid is FALSE
        if( ! vgrid){
            gline = linesGrob(y = base::c(-0.02, 0),  gp = gpar(col = "black", lwd = 0.5))
            for(i2 in base::c(xmin_plot, chr$LENGTH_CUMUL)){
                base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::annotation_custom(gline, xmin = i2, xmax = i2, ymin = -Inf, ymax = Inf))
            }
        }


        fin.plot1 <- base::suppressMessages(base::suppressWarnings(base::eval(base::parse(text = base::paste(base::paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + ")))))
        # tempo.output <- ggplot2::ggplot_build(fin.plot1)

        if(base::is.null(bottom.y.column)){
            base::suppressMessages(base::suppressWarnings(gridExtra::grid.arrange(fin.plot1, ncol=1, nrow = 1)))
        }else{
            y.min.pos2 <- base::ifelse(base::is.null(y.lim2), base::min(obs[ , bottom.y.column]), base::min(y.lim2))
            y.max.pos2 <- base::ifelse(base::is.null(y.lim2), base::max(obs[ , bottom.y.column]), max(y.lim2))
            tempo.gg.name2 <- "gg.indiv.plot."
            tempo.gg.count2 <- 0
            base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), ggplot(obs, aes_string(x = "coord", y = bottom.y.column)))
            if(vgrid){
                base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), geom_vline(
                    xintercept = base::c(xmin_plot, chr$LENGTH_CUMUL),
                    size = 0.25,
                    color = "grey80"
                ))
            }
            if( ! base::is.null(y.threshold2)){
                base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), geom_hline(
                    yintercept = y.threshold2,
                    linetype = "22", 
                    size = 0.25, 
                    color = "red"
                ))
            }
            if(base::is.null(color.column)){
                if(base::is.null(dot.border.color)){
                    base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), geom_point(
                        aes(color = base::as.factor(CHROM)), 
                        alpha = 1, 
                        pch = 16, 
                        size = 1
                    ))
                    base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), scale_color_manual(values = base::rep(base::c("grey20", "skyblue"), 25)))
                }else{
                    base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), geom_point(
                        aes(fill = base::as.factor(CHROM)), 
                        alpha = 1, 
                        color = dot.border.color, 
                        pch = 21, 
                        size = 1
                    ))
                    base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), scale_fill_manual(values = base::rep(base::c("grey20", "skyblue"), 25)))
                }
            }else{
                if(base::is.null(dot.border.color)){
                    base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), geom_point(
                        aes(color = color.column), 
                        alpha = 1, 
                        pch = 16, 
                        size = 1
                    ))
                    base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), scale_color_gradient2())
                }else{
                    base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), geom_point(
                        aes(fill = base::as.factor(CHROM)), 
                        alpha = 1, 
                        color = dot.border.color, 
                        pch = 21, 
                        size = 1
                    ))
                    base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), scale_fill_gradient2())
                }
            }
            base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), scale_x_continuous(
                expand = base::c(0, 0), # remove space after after axis limits
                oob = scales::rescale_none,
                label = chr$CHR_NAME, 
                breaks= chr$CHR_NAME_POS, 
                limits = base::c(xmin_plot - marging, base::max(chr$LENGTH_CUMUL) + marging)
            ))
            if(y.log2){
                if(base::any(obs[ , bottom.y.column] <= 0)){
                    tempo.cat <- base::paste0("ERROR IN miami.R:\nTHE y_log2 PARAMETER CANNOT BE SET TO \"TRUE\" IF 0 OR NEG VALUES IN THE ", bottom.y.column, " FIELD OF THE TSV OR VCF")
                    base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
                }else{
                    base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), scale_y_continuous(
                        expand = base::c(0, 0), # remove space after after axis limits
                        limits = if(reverse2){base::c(y.min.pos2, y.max.pos2)}else{c(y.max.pos2, y.min.pos2)}, # NA indicate that limits must correspond to data limits but ylim() already used
                        oob = scales::rescale_none, 
                        trans = "log10", 
                        breaks = scales::trans_breaks("log10", function(x){10^x}), 
                        labels = scales::trans_format("log10", scales::math_format(10^.x))
                    ))
                    # assign(paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), annotation_logticks(outside = TRUE)) # 
                }
            }else{
                base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), scale_y_continuous(
                    expand = base::c(0, 0), # remove space after after axis limits
                    limits = if(reverse2){base::c(y.min.pos2, y.max.pos2)}else{base::c(y.max.pos2, y.min.pos2)}, # NA indicate that limits must correspond to data limits but ylim() already used
                    oob = scales::rescale_none, 
                    trans = "identity" # equivalent to ggplot2::scale_y_reverse() but create the problem of y-axis label disappearance with y.lim decreasing. Thus, do not use. Use ylim() below and after this
                ))
            }
            base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), theme_bw())
            base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), theme(
                legend.position=if(base::is.null(color.column)){"none"},
                panel.border = element_blank(),
                panel.grid = element_blank(), 
                axis.ticks.y.left = element_line(size = 0.25), 
                # axis.line.x.top = element_line(size = 0.25), # is not displayed. Thus, I add a geom_hline below
                axis.line.y.left = element_line(size = 0.25),
                axis.title.x = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(), 
            ))
            # add x-axis line
            base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), geom_hline(
                yintercept = base::ifelse(
                    base::is.null(y.lim2), 
                    base::ifelse(reverse2, base::max(obs[ , bottom.y.column]), base::min(obs[ , bottom.y.column])), 
                    base::ifelse(reverse2, base::max(y.lim2), base::min(y.lim2))
                ),
                size = 0.25
            ))
            # add tick lines if vgrid is FALSE
            if( ! vgrid){
                gline = linesGrob(y = base::c(1, 1.02),  gp = gpar(col = "black", lwd = 0.5))
                for(i2 in base::c(xmin_plot, chr$LENGTH_CUMUL)){
                    base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), ggplot2::annotation_custom(gline, xmin = i2, xmax = i2, ymin = -Inf, ymax = Inf))
                }
            }

            fin.plot2 <- base::suppressMessages(base::suppressWarnings(base::eval(base::parse(text = base::paste(base::paste0(tempo.gg.name2, 1:tempo.gg.count2), collapse = " + ")))))
            gl <- base::lapply(base::list(fin.plot1, fin.plot2), ggplotGrob)  
            wd <- base::do.call(unit.pmax, base::lapply(gl, "[[", 'widths'))
            gl <- base::lapply(gl, function(x){x[['widths']] = wd ; x})
            if( ! vgrid){
                gl[[1]]$layout$clip[gl[[1]]$layout$name=="panel"] <- "off"
                gl[[2]]$layout$clip[gl[[2]]$layout$name=="panel"] <- "off"
            }
            base::suppressMessages(base::suppressWarnings(gridExtra::grid.arrange(gl[[1]], gl[[2]], ncol=1, nrow = 2)))
        }
    }else{
        fun_gg_empty_graph(text = base::paste0("NO PLOT DRAWN\nTHE x_lim PARAMETER\nMIGHT BE OUTSIDE\nOF THE RANGE OF THE VCF FILE\nOR THE RANGE OF THE region PARAMETER\nOR NULL"))
    } 
    # else already dealt above
    # end main code
}



