#' @title gg_donut
#' @description
#' Plot a ggplot2 donut using contingency data, systematically in the decreasing order of frequencies, starting at the top and turning clockwise. 
#' For ggplot2 specifications, see: https://ggplot2.tidyverse.org/articles/ggplot2-specs.html.
#' @param data1 a dataframe compatible with ggplot2. 
#' @param freq single character string of the data1 column name of the frequencies.
#' @param categ single character string of the data1 column name of categories (qualitative variable).
#' @param fill.palette single character string of a palette name (see ?ggplot2::scale_fill_brewer() for the list).Ignored if fill.color is not NULL. 
#' @param fill.color either (1) NULL, or (2) a vector of character strings or integers of same length as the number of classes in categ. Colors can be color names (see ?colors() in R), hexadecimal color codes, or integers (according to the ggplot2 palette). The order of the elements will be used according to the frequency values, from highest to lowest. An easy way to use this argument is to sort data1 according to the frequencies values, add a color column with the corresponding desired colors and use the content of this column as values of fill.color. If color is NULL and fill.palette is NULL, default colors of ggplot2 are used. If color is not NULL, it overrides fill.palette. 
#' @param hole.size single positive proportion of donut central hole, 0 meaning no hole (pie chart) and 1 no plot (donut with a null thickness). 
#' @param hole.text logical (either TRUE or FALSE). Display the sum of frequencies (column of data1 indicated in the freq argument). 
#' @param hole.text.size single positive numeric value of the title font size in mm. Ignored if hole.text is FALSE 
#' @param border.color a single character string or integer. Colors can be color names (see ?colors() in R), hexadecimal color codes, or integers (according to the ggplot2 palette). 
#' @param title single character string of the graph title. 
#' @param title.text.size single numeric value of the title font size in mm. 
#' @param annotation single character string of the data1 column name of annotations. Values inside this column will be displayed over the corresponding slices of the donut. Write NULL if not required. 
#' @param annotation.distance single positive numeric value of the distance from the center of the slice. 0 means center of the slice, 0.5 means at the edge. Above 0.5, the donut will be reduced to make place for the annotation. Ignored if annotation is NULL. 
#' @param annotation.size single positive numeric value of the annotation font size in mm. Ignored if annotation is NULL. 
#' @param annotation.force single positive numeric value of the force of repulsion between overlapping text labels. See ?ggrepel::geom_text_repel() in R. Ignored if annotation is NULL. 
#' @param annotation.force.pull single positive numeric value of the force of attraction between a text label and its corresponding data point. See ?
#' @param legend.show logical (either TRUE or FALSE). Show legend?. 
#' @param legend.width single proportion (between 0 and 1) indicating the relative width of the legend sector (on the right of the plot) relative to the width of the plot. Value 1 means that the window device width is split in 2, half for the plot and half for the legend. Value 0 means no room for the legend, which will overlay the plot region. Write NULL to inactivate the legend sector. In such case, ggplot2 will manage the room required for the legend display, meaning that the width of the plotting region can vary between graphs, depending on the text in the legend. 
#' @param legend.name character string of the legend title. If legend.name is NULL then legend.name is the value of the categ argument. Write legend.name = "" to remove the legend. 
#' @param legend.text.size single numeric value of the font size in mm of the legend labels. 
#' @param legend.box.size single numeric value of the size of the legend squares in mm. 
#' @param legend.box.space single numeric value of the space between the legend boxes in mm. 
#' @param legend.limit single positive proportion of the classes displayed in the legend for which the corresponding proportion is over legend.limit. Write NULL to display all the classes. 
#' @param legend.add.prop logical (either TRUE or FALSE). add the proportion after the class names in the legend ? 
#' @param add character string allowing to add more ggplot2 features (dots, lines, themes, facet, etc.). Ignored if NULL. WARNING: (1) the string must start with "+", (2) the string must finish with ")" and (3) each function must be preceded by "ggplot2::". Example: "+ ggplot2::coord_flip() + ggplot2::theme_bw()". If the character string contains the "ggplot2::theme" string, then the article argument of gg_donut() (see above) is ignored with a warning. In addition, some arguments can be overwritten, like x.angle (check all the arguments). Handle the add argument with caution since added functions can create conflicts with the preexisting internal ggplot2 functions. The call of objects inside the quotes of add can lead to an error if the name of these objects are some of the gg_donut() arguments. Indeed, the function will use the internal argument instead of the global environment object. Example article <- "a" in the working environment and add = '+ ggplot2::ggtitle(article)'. The risk here is to have TRUE as title. To solve this, use add = '+ ggplot2::ggtitle(get("article", envir = .GlobalEnv))'
#' @param return logical (either TRUE or FALSE). Return the graph parameters?
#' @param return.ggplot: logical (either TRUE or FALSE). Return the ggplot object in the output list? Ignored if return argument is FALSE. WARNING: always assign the gg_donut() function (e.g., a <- gg_donut()) into something if the return.ggplot argument is TRUE, otherwise, double plotting is performed. See $ggplot in the RETURN section below for more details.
#' @param return.gtable logical (either TRUE or FALSE). Return the full graph (main, title and legend) as a gtable of grobs in the output list? See $gtable in the RETURN section below for more details.
#' @param plot logical (either TRUE or FALSE). Plot the graphic? If FALSE and return argument is TRUE, graphical parameters and associated warnings are provided without plotting.
#' @param warn.print logical (either TRUE or FALSE). Print warnings at the end of the execution? ? If FALSE, warning messages are never printed, but can still be recovered in the returned list. Some of the warning messages (those delivered by the internal ggplot2 functions) are not apparent when using the argument plot = FALSE.
#' @param lib.path vector of character strings indicating the absolute path of the required packages (see below). if NULL, the function will use the R library default folders.
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @returns a donut plot if plot argument is TRUE.
#' @returns a list of the graph info if return argument is TRUE:
    # $data: the initial data with modifications and with graphic information added
    # $removed.row.nb: a list of the removed rows numbers in data frames (because of NA). NULL if no row removed
    # $removed.rows: a list of the removed rows in data frames (because of NA). NULL if no row removed
    # $plot.data
    # $panel: the variable names used for the panels (NULL if no panels). WARNING: NA can be present according to ggplot2 upgrade to v3.3.0
    # $axes: the x-axis and y-axis info
    # $warn: the warning messages. Use cat() for proper display. NULL if no warning. WARNING: warning messages delivered by the internal ggplot2 functions are not apparent when using the argument plot = FALSE
    # $ggplot: ggplot object that can be used for reprint (use print($ggplot) or update (use $ggplot + ggplot2::...). NULL if return.ggplot argument is FALSE. Warning: the legend is not in $ggplot as it is in a separated grob (use $gtable to get it). Of note, a non-null $ggplot in the output list is sometimes annoying as the manipulation of this list prints the plot
    # $gtable: gtable object that can be used for reprint (use gridExtra::grid.arrange(...$ggplot) or with additionnal grobs (see the grob decomposition in the examples). Contrary to $ggplot, a non-NULL $gtable in the output list is not annoying as the manipulation of this list does not print the plot
#' @examples
#' obs1 <- data.frame(Km = c(20, 10, 1, 5), Car = c("TUUT", "WIIM", "BIP", "WROUM"), Color1 = 1:4, color2 = c("red", "blue", "green", "black"), Country = c("FR", "UK", "US", NA), stringsAsFactors = TRUE) ; gg_donut(data1 = obs1, freq = "Km", categ = "Car", annotation = "Country")
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 annotate
#' @importFrom ggplot2 coord_polar
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggplot_build
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_fill_brewer
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_void
#' @importFrom ggplot2 ylim
#' @importFrom ggrepel geom_text_repel
#' @importFrom grDevices colors
#' @importFrom gridExtra arrangeGrob
#' @importFrom gridExtra grid.arrange
#' @importFrom grid unit
#' @importFrom saferDev arg_check
#' @details
#' - Rows containing NA in data1[, c(freq, categ)] will be removed before processing, with a warning (see below).
#' - Size arguments (hole.text.size, border.size, title.text.size and annotation.size) are in mm. See Hadley comment in https://stackoverflow.com/questions/17311917/ggplot2-the-unit-of-size. See also http://sape.inf.usi.ch/quick-reference/ggplot2/size). Unit object are not accepted, but conversion can be used (e.g., grid::convertUnit(grid::unit(0.2, "inches"), "mm", valueOnly = TRUE)).
#' @export
gg_donut <- function(
    data1, 
    freq, 
    categ, 
    fill.palette = NULL,
    fill.color = NULL, 
    hole.size = 0.5, 
    hole.text = TRUE, 
    hole.text.size = 14, 
    border.color = "gray50", 
    border.size = 0.2, 
    title = "", 
    title.text.size = 7, 
    annotation = NULL,
    annotation.distance = 0,
    annotation.size = 3,
    annotation.force = 1,
    annotation.force.pull = 100,
    legend.show = TRUE, 
    legend.width = 0.25, 
    legend.name = NULL, 
    legend.text.size = 10, 
    legend.box.size = 5, 
    legend.box.space = 2, 
    legend.limit = NULL, 
    legend.add.prop = FALSE,
    add = NULL, 
    return = FALSE, 
    return.ggplot = FALSE,
    return.gtable = TRUE,
    plot = TRUE, 
    warn.print = TRUE, 
    lib.path = NULL,
    safer_check = TRUE
){
    # DEBUGGING
    # obs1 <- data.frame(Km = c(20, 10, 1, 5), Car = c("TUUT", "WIIM", "BIP", "WROUM"), Color1 = 1:4, color2 = c("red", "blue", "green", "black"), Country = c("FR", "UK", "US", NA), stringsAsFactors = TRUE) ; data1 = obs1 ; freq = "Km" ; categ = "Car" ; fill.palette = NULL ; fill.color = NULL ; hole.size = 0.5 ; hole.text = TRUE ; hole.text.size = 12 ; border.color = "gray50" ; border.size = 0.1 ; title = "" ; title.text.size = 12 ; annotation = "Country" ; annotation.distance = 0.5 ; annotation.size = 3 ; annotation.force = 1 ; annotation.force.pull = 100 ; legend.show = TRUE ; legend.width = 0.5 ; legend.name = NULL ; legend.text.size = 10 ; legend.box.size = 5 ; legend.box.space = 2 ; legend.limit = NULL ; legend.add.prop = FALSE ; add = NULL ; return = TRUE ; return.ggplot = FALSE ; return.gtable = TRUE ; plot = TRUE ; warn.print = FALSE ; lib.path = NULL ; safer_check = TRUE
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
    if(safer_check == TRUE){
        .base_op_check(
            external.function.name = function.name,
            external.package.name = package.name
        )
    }
    # end critical operator checking
    # package checking
    # check of lib.path
    if( ! base::is.null(lib.path)){
        if( ! base::all(base::typeof(lib.path) == "character")){ # no na.rm = TRUE with typeof
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT MUST BE A VECTOR OF CHARACTERS:\n", base::paste(lib.path, collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else if( ! base::all(base::dir.exists(lib.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
            tempo.cat <-base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", base::paste(lib.path, collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else{
            base::.libPaths(new = base::sub(x = lib.path, pattern = "/$|\\\\$", replacement = "")) # .libPaths(new = ) add path to default path. BEWARE: .libPaths() does not support / at the end of a submitted path. Thus check and replace last / or \\ in path
            lib.path <- base::.libPaths()
        }
    }else{
        lib.path <- base::.libPaths() # .libPaths(new = lib.path) # or .libPaths(new = c(.libPaths(), lib.path))
    }
    # end check of lib.path


    # check of the required function from the required packages
    if(safer_check == TRUE){
        .pack_and_function_check(
        fun = base::c(
            "ggplot2::aes_string",
            "ggplot2::annotate",
            "ggplot2::coord_polar",
            "ggplot2::element_text",
            "ggplot2::geom_col",
            "ggplot2::ggplot",
            "ggplot2::ggplot_build",
            "ggplot2::guides",
            "ggplot2::guide_legend",
            "ggplot2::labs",
            "ggplot2::scale_fill_brewer",
            "ggplot2::scale_fill_manual",
            "ggplot2::scale_x_continuous",
            "ggplot2::theme",
            "ggplot2::theme_void",
            "ggplot2::ylim",
            "ggrepel::geom_text_repel",
            "grDevices::colors",
            "gridExtra::arrangeGrob",
            "gridExtra::grid.arrange",
            "grid::unit",
            "saferDev::arg_check"
        ),
        lib.path = lib.path,
        external.function.name = function.name,
        external.package.name = package.name
    )
    }
    # end check of the required function from the required packages
    # end package checking

    # argument primary checking
    # arg with no default values
    mandat.args <- base::c(
        "data1", 
        "freq", 
        "categ"
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
    tempo <- saferDev::arg_check(data = data1, class = "data.frame", na.contain = TRUE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = freq, class = "vector", mode = "character", na.contain = FALSE, length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = categ, class = "vector", mode = "character", na.contain = FALSE, length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(fill.palette)){
        tempo <- saferDev::arg_check(data = fill.palette, options = base::c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral", "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3", "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"), length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    }else{
        # no saferDev::arg_check test here, it is just for checked.arg.names
        tempo <- saferDev::arg_check(data = fill.palette, class = "vector", safer_check = FALSE)
        checked.arg.names <- base::c(checked.arg.names, tempo$object.name)
    }
    if( ! base::is.null(fill.color)){
        tempo1 <- saferDev::arg_check(data = fill.color, class = "vector", mode = "character", na.contain = TRUE, fun.name = function.name, safer_check = FALSE)
        tempo2 <- saferDev::arg_check(data = fill.color, class = "factor", na.contain = TRUE, fun.name = function.name, safer_check = FALSE)
        tempo3 <- saferDev::arg_check(data = fill.color, class = "integer", double.as.integer.allowed = TRUE, na.contain = TRUE, neg.values = FALSE, fun.name = function.name, safer_check = FALSE) # not need to test inf with integers
        if(tempo1$problem == TRUE & tempo2$problem == TRUE & tempo3$problem == TRUE){
            tempo.cat <- base::paste0("ERROR IN ", function.name, "\nfill.color ARGUMENT MUST BE A VECTOR OF (1) HEXADECIMAL COLOR STRINGS STARTING BY #, OR (2) COLOR NAMES GIVEN BY grDevices::colors(), OR (3) POSITIVE INTEGER VALUES")
            text.check <- base::c(text.check, tempo.cat)
            argum.check <- base::c(argum.check, TRUE)
            checked.arg.names <- base::c(checked.arg.names, tempo1$object.name)
        }else if(tempo3$problem == FALSE & base::any(base::is.infinite(fill.color))){ # is.infinite() deals with NA as FALSE
            tempo.cat <- base::paste0("ERROR IN ", function.name, "\nfill.color ARGUMENT CANNOT CONTAIN Inf VALUES AMONG POSITIVE INTEGER VALUES")
            text.check <- base::c(text.check, tempo.cat)
            argum.check <- base::c(argum.check, TRUE)
            checked.arg.names <- base::c(checked.arg.names, tempo1$object.name)
        }else if(tempo3$problem == FALSE & base::any(fill.color == 0, na.rm = TRUE)){
            tempo.cat <- base::paste0("ERROR IN ", function.name, "\nfill.color ARGUMENT CANNOT CONTAIN 0 AMONG POSITIVE INTEGER VALUES")
            text.check <- base::c(text.check, tempo.cat)
            argum.check <- base::c(argum.check, TRUE)
            checked.arg.names <- base::c(checked.arg.names, tempo1$object.name)
        }
    }
    tempo <- saferDev::arg_check(data = hole.size, prop = TRUE, length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = hole.text, class = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = hole.text.size, class = "vector", mode = "numeric", neg.values = FALSE, inf.values = FALSE, length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo1 <- saferDev::arg_check(data = border.color, class = "vector", mode = "character", na.contain = FALSE, length = 1, fun.name = function.name, safer_check = FALSE)
    tempo2 <- saferDev::arg_check(data = border.color, class = "integer", double.as.integer.allowed = TRUE, neg.values = FALSE, na.contain = FALSE, length = 1, fun.name = function.name, safer_check = FALSE) # not need to test inf with integers
    if(tempo1$problem == TRUE & tempo2$problem == TRUE){
        tempo.cat <- base::paste0("ERROR IN ", function.name, "\nborder.color ARGUMENT MUST BE A SINGLE CHARACTER STRING OR POSITIVE INTEGER")
        text.check <- base::c(text.check, tempo.cat)
        argum.check <- base::c(argum.check, TRUE)
        checked.arg.names <- base::c(checked.arg.names, tempo1$object.name)
    }
    tempo <- saferDev::arg_check(data = border.size, class = "vector", mode = "numeric", na.contain = FALSE, neg.values = FALSE, inf.values = FALSE, length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = title, class = "vector", mode = "character", na.contain = FALSE, length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = title.text.size, class = "vector", mode = "numeric", na.contain = FALSE, neg.values = FALSE, inf.values = FALSE, length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(annotation)){
            tempo <- saferDev::arg_check(data = annotation, class = "vector", mode = "character", na.contain = FALSE, length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
            tempo <- saferDev::arg_check(data = annotation.distance, class = "vector", mode = "numeric", na.contain = FALSE, neg.values = FALSE, inf.values = FALSE, length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
            tempo <- saferDev::arg_check(data = annotation.size, class = "vector", mode = "numeric", na.contain = FALSE, neg.values = FALSE, inf.values = FALSE, length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
            tempo <- saferDev::arg_check(data = annotation.force, class = "vector", mode = "numeric", na.contain = FALSE, neg.values = FALSE, inf.values = FALSE, length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
            tempo <- saferDev::arg_check(data = annotation.force.pull, class = "vector", mode = "numeric", na.contain = FALSE, neg.values = FALSE, inf.values = FALSE, length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    }else{
        # no saferDev::arg_check test here, it is just for checked.arg.names
        tempo <- saferDev::arg_check(data = annotation, class = "vector", safer_check = FALSE)
        checked.arg.names <- base::c(checked.arg.names, tempo$object.name)
    }
    tempo <- saferDev::arg_check(data = legend.show, class = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(legend.width)){
        tempo <- saferDev::arg_check(data = legend.width, prop = TRUE, length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    }else{
        # no saferDev::arg_check test here, it is just for checked.arg.names
        tempo <- saferDev::arg_check(data = legend.width, class = "vector", safer_check = FALSE)
        checked.arg.names <- base::c(checked.arg.names, tempo$object.name)
    }
    if( ! base::is.null(legend.name)){
        tempo <- saferDev::arg_check(data = legend.name, class = "vector", mode = "character", na.contain = FALSE, length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    }else{
        # no saferDev::arg_check test here, it is just for checked.arg.names
        tempo <- saferDev::arg_check(data = legend.name, class = "vector", safer_check = FALSE)
        checked.arg.names <- c(checked.arg.names, tempo$object.name)
    }
    tempo <- saferDev::arg_check(data = legend.text.size, class = "vector", mode = "numeric", na.contain = FALSE, neg.values = FALSE, inf.values = FALSE, length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = legend.box.size, class = "vector", mode = "numeric", na.contain = FALSE, neg.values = FALSE, inf.values = FALSE, length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = legend.box.space, class = "vector", mode = "numeric", na.contain = FALSE, neg.values = FALSE, inf.values = FALSE, length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(legend.limit)){
        tempo <- saferDev::arg_check(data = legend.limit, prop = TRUE, length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    }else{
        # no saferDev::arg_check test here, it is just for checked.arg.names
        tempo <- saferDev::arg_check(data = legend.limit, class = "vector", safer_check = FALSE)
        checked.arg.names <- base::c(checked.arg.names, tempo$object.name)
    }
    tempo <- saferDev::arg_check(data = legend.add.prop, class = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(add)){
        tempo <- saferDev::arg_check(data = add, class = "vector", mode = "character", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    }else{
        # no saferDev::arg_check test here, it is just for checked.arg.names
        tempo <- saferDev::arg_check(data = add, class = "vector", safer_check = FALSE)
        checked.arg.names <- base::c(checked.arg.names, tempo$object.name)
    }
    tempo <- saferDev::arg_check(data = return, class = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = return.ggplot, class = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = return.gtable, class = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = plot, class = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = warn.print, class = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(lib.path)){
        tempo <- saferDev::arg_check(data = lib.path, class = "vector", mode = "character", fun.name = function.name, safer_check = FALSE) ; base::eval(ee) # several possible paths
    }else{
        # no saferDev::arg_check test here, it is just for checked.arg.names
        tempo <- saferDev::arg_check(data = lib.path, class = "vector", safer_check = FALSE)
        checked.arg.names <- base::c(checked.arg.names, tempo$object.name)
    }
    if( ! base::is.null(argum.check)){
        if(base::any(argum.check) == TRUE){
            base::stop(base::paste0("\n\n================\n\n", base::paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with arg_check()
    # check with r_debugging_tools
    # source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.7/r_debugging_tools-v1.7.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_saferDev::arg_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using saferDev::arg_check()
    # end check with r_debugging_tools
    # end argument primary checking

    # second round of checking and data preparation
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    # reserved word checking
    if( ! (base::is.null(add))){
        if(base::any(base::sapply(X = arg.names, FUN = base::grepl, x = add), na.rm = TRUE)){
            warn.count <- warn.count + 1
            tempo.warn <- base::paste0("(", warn.count,") NAMES OF ", function.name, " ARGUMENTS DETECTED IN THE add STRING:\n", base::paste(arg.names[base::sapply(X = arg.names, FUN = base::grepl, x = add)], collapse = "\n"), "\nRISK OF WRONG OBJECT USAGE INSIDE ", function.name)
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo.warn, base::paste0(warn, "\n\n", tempo.warn)))
        }
    }
    # verif of add
    if( ! base::is.null(add)){
        if( ! base::grepl(pattern = "^\\s*\\+", add)){ # check that the add string start by +
            tempo.cat <- base::paste0("ERROR IN ", function.name, "\nadd ARGUMENT MUST START WITH \"+\": ", base::paste(base::unique(add), collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else if( ! base::grepl(pattern = "(ggplot2|lemon)\\s*::", add)){ #
            tempo.cat <- base::paste0("ERROR IN ", function.name, "\nFOR EASIER FUNCTION DETECTION, add ARGUMENT MUST CONTAIN \"ggplot2::\" OR \"lemon::\" IN FRONT OF EACH GGPLOT2 FUNCTION: ", base::paste(base::unique(add), collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else if( ! base::grepl(pattern = ")\\s*$", add)){ # check that the add string finished by )
            tempo.cat <- base::paste0("ERROR IN ", function.name, "\nadd ARGUMENT MUST FINISH BY \")\": ", base::paste(base::unique(add), collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end verif of add
    # management of add containing facet
    facet.categ <- NULL
    if( ! base::is.null(add)){
        facet.check <- TRUE
        tempo <- base::unlist(base::strsplit(x = add, split = "\\s*\\+\\s*(ggplot2|lemon)\\s*::\\s*")) #
        tempo <- base::sub(x = tempo, pattern = "^facet_wrap", replacement = "ggplot2::facet_wrap")
        tempo <- base::sub(x = tempo, pattern = "^facet_grid", replacement = "ggplot2::facet_grid")
        tempo <- base::sub(x = tempo, pattern = "^facet_rep", replacement = "lemon::facet_rep")
        if(base::any(base::grepl(x = tempo, pattern = "ggplot2::facet_wrap|lemon::facet_rep_wrap"), na.rm = TRUE)){
            tempo1 <- base::suppressWarnings(base::eval(base::parse(text = tempo[base::grepl(x = tempo, pattern = "ggplot2::facet_wrap|lemon::facet_rep_wrap")])))
            facet.categ <- base::names(tempo1$params$facets)
            tempo.text <- "facet_wrap OR facet_rep_wrap"
            facet.check <- FALSE
        }else if(base::grepl(x = add, pattern = "ggplot2::facet_grid|lemon::facet_rep_grid")){
            tempo1 <- base::suppressWarnings(base::eval(base::parse(text = tempo[base::grepl(x = tempo, pattern = "ggplot2::facet_grid|lemon::facet_rep_grid")])))
            facet.categ <- base::c(base::names(tempo1$params$rows), base::names(tempo1$params$cols))
            tempo.text <- "facet_grid OR facet_rep_grid"
            facet.check <- FALSE
        }
        if(facet.check == FALSE & ! base::all(facet.categ %in% base::names(data1))){ # WARNING: all(facet.categ %in% names(data1)) is TRUE when facet.categ is NULL # all() without na.rm -> ok because facet.categ cannot be NA (tested above)
            tempo.cat <- base::paste0("ERROR IN ", function.name, "\nDETECTION OF \"", tempo.text, "\" STRING IN THE add ARGUMENT BUT PROBLEM OF VARIABLE DETECTION (COLUMN NAMES OF data1)\nTHE DETECTED VARIABLES ARE:\n", base::paste(facet.categ, collapse = " "), "\nTHE data1 COLUMN NAMES ARE:\n", base::paste(base::names(data1), collapse = " "), "\nPLEASE REWRITE THE add STRING AND RERUN")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of add containing facet
    # end reserved word checking
    # management of NA arguments
    if( ! (base::all(base::class(arg.user.setting) %in% base::c("list", "NULL"), na.rm = TRUE) & base::length(arg.user.setting) == 0)){
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
        "data1", 
        "freq", 
        "categ", 
        # "fill.palette", # inactivated because can be null
        # "fill.color", # inactivated because can be null
        "hole.size", 
        "hole.text", 
        "hole.text.size", 
        "border.color", 
        "border.size", 
        "title", 
        "title.text.size", 
        # "annotation", # inactivated because can be null
        "annotation.distance", 
        "annotation.size", 
        "annotation.force", 
        "annotation.force.pull", 
        "legend.show", 
        # "legend.width", # inactivated because can be null
        # "legend.name", # inactivated because can be null
        "legend.text.size",
        "legend.box.size",
        "legend.box.space",
        # "legend.limit", # inactivated because can be null
        "legend.add.prop", 
        # "add", # inactivated because can be null
        "return", 
        "return.ggplot", 
        "return.gtable", 
        "plot", 
        "warn.print",
        # "lib.path", # inactivated because can be null
        "safer_check"
    )
    tempo.log <- base::sapply(base::lapply(tempo.arg, FUN = base::get, envir = base::sys.nframe(), inherits = FALSE), FUN = base::is.null)
    if(base::any(tempo.log) == TRUE){# normally no NA with is.null()
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE:\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), base::paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end management of NULL arguments
    # code that protects set.seed() in the global environment
    if(base::exists(".Random.seed", envir = .GlobalEnv)){ # if .Random.seed does not exists, it means that no random operation has been performed yet in any R environment
        tempo.random.seed <- .Random.seed
        base::on.exit(base::assign(".Random.seed", tempo.random.seed, env = .GlobalEnv))
    }else{
        base::on.exit(base::set.seed(NULL)) # inactivate seeding -> return to complete randomness
    }
    base::set.seed(1)
    # end code that protects set.seed() in the global environment
    # warning initiation
    ini.warning.length <- base::options()$warning.length
    base::options(warning.length = 8170)
    warn <- NULL
    warn.count <- 0
    # end warning initiation
    # other checkings
    removed.row.nb <- NULL
    removed.rows <- base::data.frame(stringsAsFactors = FALSE)
    data1.ini <- data1 # strictly identical to data1
    if( ! freq %in% base::names(data1)){
        tempo.cat <- base::paste0("ERROR IN ", function.name, "\nfreq ARGUMENT MUST BE A COLUMN NAME OF THE data1 ARGUMENT")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
    }else{
        if(base::all(base::is.na(data1[ , freq]) | base::is.infinite(data1[ , freq]))){
            tempo.cat <- base::paste0("ERROR IN ", function.name, "\nTHE freq COLUMN OF data1 CANNOT BE JUST NA OR Inf")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
        tempo <- saferDev::arg_check(data = data1[ , freq], mode = "numeric", neg.values = FALSE, fun.name = function.name, safer_check = FALSE)
        if(tempo$problem == TRUE){
            tempo.cat <- base::paste0("ERROR IN ", function.name, "\n", tempo$text)
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
        # Inf and NA removal
        if(base::any(base::is.infinite(data1[, freq]) | base::is.na(data1[, freq]))){
            warn.count <- warn.count + 1
            tempo.warn <- base::paste0("(", warn.count,") PRESENCE OF Inf OR NA VALUES IN THE ", freq, " COLUMN OF THE data1 ARGUMENT AND CORRESPONDING ROWS REMOVED (SEE $removed.row.nb AND $removed.rows)")
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo.warn, base::paste0(warn, "\n\n", tempo.warn)))
            tempo <- base::which(base::is.infinite(data1.ini[, freq]) | base::is.na(data1.ini[, freq])) # data.ini used for the output
            removed.row.nb <- base::c(removed.row.nb, tempo)
            removed.rows <- base::rbind(removed.rows, data1.ini[tempo, ], stringsAsFactors = FALSE) # data.ini used for the output
            data1 <- data1[ ! (base::is.infinite(data1[, freq]) | base::is.na(data1[, freq])), ] #
        }
        # end Inf and NA removal
        # 0 removal
        if(base::any(data1[, freq] == 0)){
            warn.count <- warn.count + 1
            tempo.warn <- base::paste0("(", warn.count,") PRESENCE OF 0 VALUES IN THE ", freq, " COLUMN OF THE data1 ARGUMENT AND CORRESPONDING ROWS REMOVED (SEE $removed.row.nb AND $removed.rows)")
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo.warn, base::paste0(warn, "\n\n", tempo.warn)))
            tempo <- base::which(data1[, freq] == 0) # data.ini used for the output
            removed.row.nb <- base::c(removed.row.nb, tempo)
            removed.rows <- base::rbind(removed.rows, data1.ini[tempo, ], stringsAsFactors = FALSE) # data.ini used for the output
            data1 <- data1[ data1[, freq] != 0, ] #
        }
        # end 0 removal
    }

    if( ! categ %in% base::names(data1)){
        tempo.cat <- base::paste0("ERROR IN ", function.name, "\ncateg ARGUMENT MUST BE A COLUMN NAME OF THE data1 ARGUMENT")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
    }else{
        if(base::all(base::is.na(data1[ , categ]))){
            tempo.cat <- base::paste0("ERROR IN ", function.name, "\nTHE categ COLUMN OF data1 CANNOT BE JUST NA")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
        tempo1 <- saferDev::arg_check(data = categ, class = "vector", mode = "character", na.contain = TRUE, fun.name = function.name, safer_check = FALSE)
        tempo2 <- saferDev::arg_check(data = categ, class = "factor", na.contain = TRUE, fun.name = function.name, safer_check = FALSE)
        if(tempo1$problem == TRUE & tempo2$problem == TRUE){
            tempo.cat <- base::paste0("ERROR IN ", function.name, "\nTHE categ COLUMN OF data1 MUST BE CLASS \"factor\" OR \"character\"")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
        # NA removal
        if(base::any(base::is.na(data1[, categ]))){
            warn.count <- warn.count + 1
            tempo.warn <- base::paste0("(", warn.count,") PRESENCE OF NA VALUES IN THE ", categ, " COLUMN OF THE data1 ARGUMENT AND CORRESPONDING ROWS REMOVED (SEE $removed.row.nb AND $removed.rows)")
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo.warn, base::paste0(warn, "\n\n", tempo.warn)))
            tempo <- base::which(base::is.na(data1.ini[, categ])) # data.ini used for the output
            removed.row.nb <- base::c(removed.row.nb, tempo)
            removed.rows <- base::rbind(removed.rows, data1.ini[tempo, ], stringsAsFactors = FALSE) # data.ini used for the output
            data1 <- data1[ ! base::is.na(data1[, categ]), ] #
        }
        # end Inf and NA removal
        if(base::any(base::duplicated(data1[, categ]))){
            tempo.cat <- base::paste0("ERROR IN ", function.name, "\nTHE categ COLUMN OF data1 CANNOT CONTAIN DUPLICATED VALUES\n", base::paste(data1[, categ][base::duplicated(data1[, categ])], collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
    }

    if( ! base::is.null(annotation)){
        if( ! annotation %in% base::names(data1)){
            tempo.cat <- base::paste0("ERROR IN ", function.name, "\nannotation ARGUMENT MUST BE A COLUMN NAME OF THE data1 ARGUMENT")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }else{
            if(base::all(base::is.na(data1[ , annotation]))){
                tempo.cat <- base::paste0("ERROR IN ", function.name, "\nIF NON NULL, THE annotation COLUMN OF data1 CANNOT BE JUST NA")
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
            }
            tempo1 <- saferDev::arg_check(data = annotation, class = "vector", mode = "character", na.contain = TRUE, fun.name = function.name, safer_check = FALSE)
            tempo2 <- saferDev::arg_check(data = annotation, class = "factor", na.contain = TRUE, fun.name = function.name, safer_check = FALSE)
            if(tempo1$problem == TRUE & tempo2$problem == TRUE){
                tempo.cat <- base::paste0("ERROR IN ", function.name, "\nTHE annotation COLUMN OF data1 MUST BE CLASS \"factor\" OR \"character\"")
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
            }
            if(base::any(base::duplicated(data1[, annotation]))){
                warn.count <- warn.count + 1
                tempo.warn <- base::paste0("(", warn.count,") PRESENCE OF DUPLICATED VALUES IN THE ", annotation, " COLUMN OF THE data1 ARGUMENT: ", base::paste0(data1[, annotation][base::duplicated(data1[, annotation])], collapse = " "))
                warn <- base::paste0(base::ifelse(base::is.null(warn), tempo.warn, base::paste0(warn, "\n\n", tempo.warn)))
            }
        }
    }
    if(base::length(data1) == 0){
        tempo.cat <- base::paste0("ERROR IN ", function.name, "\nTHE data1 ARGUMENT IS EMPTY AFTER Inf, NA AND 0 REMOVAL IN THE ", freq, base::ifelse(base::is.null(annotation), " AND ", ", "), categ, base::ifelse(base::is.null(annotation), "", " AND "), " COLUMNS")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
    }
    if( ! base::is.null(fill.color)){
        if( ! base::is.numeric(fill.color)){
            if( ! base::all(fill.color[ ! base::is.na(fill.color)] %in% grDevices::colors() | base::grepl(pattern = "^#", fill.color[ ! base::is.na(fill.color)]), na.rm = TRUE)){
                tempo.cat <- base::paste0("ERROR IN ", function.name, "\nfill.color ARGUMENT MUST BE A VECTOR OF (1) HEXADECIMAL COLOR STRINGS STARTING BY #, OR (2) COLOR NAMES GIVEN BY grDevices::colors(), OR (3) INTEGER VALUES")
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
            }else{
                fill.color <- base::as.character(fill.color) # remove class factor is any
            }
        }
    }
    if( ! base::is.numeric(border.color)){
        if( ! (border.color %in% grDevices::colors() | base::grepl(pattern = "^#", border.color))){
            tempo.cat <- base::paste0("ERROR IN ", function.name, "\nfill.color ARGUMENT MUST BE (1) A HEXADECIMAL COLOR STRING STARTING BY #, OR (2) A COLOR NAME GIVEN BY grDevices::colors(), OR (3) AN INTEGER VALUE")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }else{
            border.color <- base::as.character(border.color) # remove class factor is any
        }
    }
    # legend name filling
    if(base::is.null(legend.name)){
        legend.name <- categ
    }
    # legend.name not NULL anymore
    # end legend name filling
    # verif of add
    if( ! base::is.null(add)){
        if( ! base::grepl(pattern = "^\\s*\\+", add)){ # check that the add string start by +
            tempo.cat <- base::paste0("ERROR IN ", function.name, "\nadd ARGUMENT MUST START WITH \"+\": ", base::paste(base::unique(add), collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
            
        }else if( ! base::grepl(pattern = "(ggplot2|lemon)\\s*::", add)){ #
            tempo.cat <- base::paste0("ERROR IN ", function.name, "\nFOR EASIER FUNCTION DETECTION, add ARGUMENT MUST CONTAIN \"ggplot2::\" OR \"lemon::\" IN FRONT OF EACH GGPLOT2 FUNCTION: ", base::paste(base::unique(add), collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }else if( ! base::grepl(pattern = ")\\s*$", add)){ # check that the add string finished by )
            tempo.cat <- base::paste0("ERROR IN ", function.name, "\nadd ARGUMENT MUST FINISH BY \")\": ", base::paste(base::unique(add), collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
    }
    # end verif of add
    # management of add containing facet
    facet.categ <- NULL
    if( ! base::is.null(add)){
        facet.check <- TRUE
        tempo <- base::unlist(base::strsplit(x = add, split = "\\s*\\+\\s*(ggplot2|lemon)\\s*::\\s*")) #
        tempo <- base::sub(x = tempo, pattern = "^facet_wrap", replacement = "ggplot2::facet_wrap")
        tempo <- base::sub(x = tempo, pattern = "^facet_grid", replacement = "ggplot2::facet_grid")
        tempo <- base::sub(x = tempo, pattern = "^facet_rep", replacement = "lemon::facet_rep")

        if(base::any(base::grepl(x = tempo, pattern = "ggplot2::facet_wrap|lemon::facet_rep_wrap"))){
            tempo1 <- base::suppressWarnings(base::eval(base::parse(text = tempo[base::grepl(x = tempo, pattern = "ggplot2::facet_wrap|lemon::facet_rep_wrap")])))
            facet.categ <- base::list(base::names(tempo1$params$facets)) # list of length 1
            tempo.text <- "facet_wrap OR facet_rep_wrap"
            facet.check <- FALSE
        }else if(base::grepl(x = add, pattern = "ggplot2::facet_grid|lemon::facet_rep_grid")){
            tempo1 <- base::suppressWarnings(base::eval(base::parse(text = tempo[base::grepl(x = tempo, pattern = "ggplot2::facet_grid|lemon::facet_rep_grid")])))
            facet.categ <- base::list(base::c(base::names(tempo1$params$rows), base::names(tempo1$params$cols))) # list of length 1
            tempo.text <- "facet_grid OR facet_rep_grid"
            facet.check <- FALSE
        }
        if(facet.check == FALSE & ! base::all(facet.categ %in% base::names(data1))){ # WARNING: all(facet.categ %in% names(data1)) is TRUE when facet.categ is NULL
            tempo.cat <- base::paste0("ERROR IN ", function.name, "\nDETECTION OF \"", tempo.text, "\" STRING IN THE add ARGUMENT BUT PROBLEM OF VARIABLE DETECTION (COLUMN NAMES OF data1)\nTHE DETECTED VARIABLES ARE:\n", base::paste(facet.categ, collapse = " "), "\nTHE data1 COLUMN NAMES ARE:\n", base::paste(base::names(data1), collapse = " "), "\nPLEASE REWRITE THE add STRING AND RERUN")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
    }
    # if facet.categ is not NULL, it is a list of length 1 now
    # end management of add containing facet
    if( ! base::is.null(lib.path)){
        if( ! base::all(base::dir.exists(lib.path))){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
            tempo.cat <- base::paste0("ERROR IN ", function.name, "\nDIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", base::paste(lib.path, collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
    }
    # end other checkings
    # end second round of checking and data preparation

    
    # main code
    data1 <- base::data.frame(data1, prop = data1[ , freq] / base::sum(data1[ , freq]))
    if(legend.add.prop == TRUE){
        data1[ , categ] <- base::paste0(data1[ , categ], " (", base::round(data1$prop, 2), ")")
    }
    data1[ , categ] <- base::factor(data1[ , categ], levels = data1[ , categ][base::order(data1$prop, decreasing = TRUE)]) # reorder so that the donut is according to decreasing proportion starting at the top in a clockwise direction
    data1 <- data1[base::order(base::as.numeric(data1[ , categ]), decreasing = FALSE), ] # data1[ , categ] with rows in decreasing order, according to prop
    data1 <- base::data.frame(data1, x = 0) # staked bar at the origin of the donut set to x = 0
    tempo.gg.name <- "gg.indiv.plot."
    tempo.gg.count <- 0
    base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), base::eval(base::parse(text = base::paste0("ggplot2::ggplot()", if(base::is.null(add)){""}else{add})))) # add added here to have the facets
    bar_width = 1
    base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_col(
        data = data1,
        mapping = ggplot2::aes_string(x = "x", y = freq, fill = categ), 
        color = border.color, 
        size = border.size, 
        width = bar_width
    )) # size is size of the separation in the donut
    # assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_text(
    #     ggplot2::aes(label = Freq), 
    #     position = ggplot2::position_stack(vjust = 0.5)
    # ))
    base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_x_continuous(
        expand = base::c(0, 0), # prevent extra limits in x axis
        limits = base::c(- bar_width / 2 - (bar_width * hole.size) / (1 - hole.size), base::max(bar_width / 2, annotation.distance))
    )) # must be centered on x = 0
    base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::ylim(base::c(0, base::max(base::cumsum(data1[ , freq])))))
    if(hole.text == TRUE){
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::annotate(
            geom = "text", 
            x = - bar_width / 2 - (bar_width * hole.size) / (1 - hole.size), 
            y = 0, 
            label = base::sum(data1[ , freq]), 
            size = hole.text.size
        ))
    }
    base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::coord_polar(theta = "y", direction = -1, start = 0, clip = "on"))
    if(base::is.null(fill.color) & ! base::is.null(fill.palette)){
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_fill_brewer(palette = fill.palette, name = legend.name))
    }else if( ! base::is.null(fill.color)){
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_fill_manual(values = fill.color, name = legend.name, na.value = "white"))
    }
    if(legend.name != ""){
       base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::labs(fill = legend.name)) # title of the legend
    }

    if( ! base::is.null(add)){ # if add is NULL, then = 0
        if(base::grepl(pattern = "ggplot2\\s*::\\s*theme", add) == TRUE){
            warn.count <- warn.count + 1
            tempo.warn <- base::paste0("(", warn.count,") \"ggplot2::theme\" STRING DETECTED IN THE add ARGUMENT\n-> INTERNAL GGPLOT2 THEME FUNCTIONS theme_void() HAS BEEN INACTIVATED, SO THAT THE USER THEME CAN BE EFFECTIVE")
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo.warn, base::paste0(warn, "\n\n", tempo.warn)))
            add.check <- FALSE
        }else{
            base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::theme_void())
            base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::theme(
                    legend.text = ggplot2::element_text(size = legend.text.size),
                    legend.spacing.y = grid::unit(legend.box.space, 'mm')
            ))
        }
    }else{
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::theme_void())
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::theme(
                legend.text = ggplot2::element_text(size = legend.text.size),
                legend.spacing.y = grid::unit(legend.box.space, 'mm')
        ))
    }
    base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::guides(
        fill = ggplot2::guide_legend(
            override.aes = base::list(color = "white", size  = legend.box.size),
            byrow = TRUE
        )
    )) # remove border of squares in legend

    # annotations on slices
    if( ! base::is.null(annotation)){
        tempo <- base::rev(base::cumsum(base::rev(data1[ , freq])))
        data1 <- base::data.frame(data1, text_y = tempo - (tempo - base::c(tempo[-1], 0)) / 2)
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggrepel::geom_text_repel(
            data = data1, 
            mapping = ggplot2::aes_string(
                x = "x", 
                y = "text_y", 
                label = annotation
            ), 
            size = annotation.size, 
            force = annotation.force, 
            force_pull = annotation.force.pull, 
            nudge_x = annotation.distance, # knowing that the bar is centered on x = 0 and that the right edge is at bar_width / 2, 0 means center of the slice, 0.5 means at the edge if bar_width = 1
            show.legend = FALSE
        ))
    }
    # end annotations on slices

    # legend management
    # removal of part of the legend 
    if( ! base::is.null(legend.limit)){
        if(base::sum(data1$prop >= legend.limit) == 0){
            tempo.cat <- base::paste0("ERROR IN ", function.name, "\nTHE legend.limit PARAMETER VALUE (", legend.limit, ") IS TOO HIGH FOR THE PROPORTIONS IN THE DONUT PLOT:\n", base::paste0(data1$prop, collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }else{
            base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_fill_discrete(
                breaks = base::as.character(data1[ , categ][data1$prop >= legend.limit])
            ))
        }
    }
    # end removal of part of the legend
    if(legend.show == FALSE){ # must be here because must be before bef.final.plot
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::guides(fill = "none")) # inactivate the initial legend
    }
    bef.final.plot <- base::suppressWarnings(base::suppressMessages(ggplot2::ggplot_build(base::eval(base::parse(text = base::paste(base::paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + "))))))
    if( ! base::is.null(legend.width)){
        legend.plot <- base::suppressWarnings(base::suppressMessages(gg_get_legend(ggplot_built = bef.final.plot, fun.name = function.name, lib.path = lib.path))) # get legend
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::guides(fill = "none")) # inactivate the initial legend
        if(base::is.null(legend.plot) & plot == TRUE){ # even if any(unlist(legend.disp)) is TRUE
            legend.plot <- ggplot2::ggplot()+ggplot2::theme_void() # empty graph instead of legend
            warn.count <- warn.count + 1
            tempo.warn <- base::paste0("(", warn.count,") LEGEND REQUESTED (legend.show ARGUMENT SET TO TRUE)\nBUT IT SEEMS THAT THE PLOT HAS NO LEGEND -> EMPTY LEGEND SPACE CREATED BECAUSE OF THE NON NULL legend.width ARGUMENT\n")
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo.warn, base::paste0(warn, "\n\n", tempo.warn)))
        }
    }else{
        legend.plot <- NULL
    }
    # end legend management

    # title
    title.grob <- grid::textGrob(
        label = title,
        x = grid::unit(0, "lines"), 
        y = grid::unit(0, "lines"),
        hjust = 0,
        vjust = 0,
        gp = grid::gpar(fontsize = title.text.size)
    )
    # end title

    # drawing
    grDevices::pdf(NULL)
    grob.save <- NULL
    main.plot <- base::eval(base::parse(text = base::paste(base::paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + ")))
    main.plot.output <- base::suppressMessages(ggplot2::ggplot_build(main.plot))
    main.grob <- base::suppressMessages(base::suppressWarnings(gridExtra::arrangeGrob(
        main.plot, 
        top = if(title == ""){" "}else{title.grob}, 
        left = " ", 
        right = " "
    ))) # , left = " ", right = " " : trick to add margins in the plot. padding =  unit(0.5, "inch") is for top margin above the title
    if( ! base::is.null(legend.width)){
        grob.save <- base::suppressMessages(base::suppressWarnings(gridExtra::grid.arrange(main.grob, legend.plot, ncol=2, widths=base::c(1, legend.width)))) # assemble grobs, ggplot, gtable into a gtable that defines the positions of the different elements (as grobs)
    }else{
        grob.save <- base::suppressMessages(base::suppressWarnings(base::print(main.grob)))
    }
    grDevices::dev.off() # inactivate the pdf(NULL) above
    if(plot == TRUE){
        gridExtra::grid.arrange(grob.save) # plot a gtable (grob)
    }else{
        warn.count <- warn.count + 1
        tempo.warn <- base::paste0("(", warn.count,") PLOT NOT SHOWN AS REQUESTED")
        warn <- base::paste0(base::ifelse(base::is.null(warn), tempo.warn, base::paste0(warn, "\n\n", tempo.warn)))
    }
    # end drawing

    # output
    if(warn.print == TRUE & ! base::is.null(warn)){
        base::on.exit(base::warning(base::paste0("FROM ", function.name, ":\n\n", warn), call. = FALSE))
    }
    base::on.exit(exp = base::options(warning.length = ini.warning.length), add = TRUE)
    if(return == TRUE){
        if(base::is.null(base::unlist(removed.row.nb))){
            removed.row.nb <- NULL
            removed.rows <- NULL
        }
        tempo <- main.plot.output$layout$panel_params[[1]]
        output <- base::list(
            data = data1, 
            removed.row.nb = removed.row.nb, 
            removed.rows = removed.rows, 
            plot.data = main.plot.output$data, 
            panel = facet.categ, 
            axes = base::list(
                x.range = tempo$x.range, 
                x.labels = if(base::is.null(base::attributes(tempo$x$breaks))){tempo$x$breaks}else{tempo$x$scale$get_labels()}, # is.null(attributes(tempo$x$breaks)) test if it is number (TRUE) or character (FALSE)
                x.positions = if(base::is.null(base::attributes(tempo$x$breaks))){tempo$x$breaks}else{base::unlist(base::attributes(tempo$x$breaks))}, 
                y.range = tempo$y.range, 
                y.labels = if(base::is.null(base::attributes(tempo$y$breaks))){tempo$y$breaks}else{tempo$y$scale$get_labels()}, 
                y.positions = if(base::is.null(base::attributes(tempo$y$breaks))){tempo$y$breaks}else{base::unlist(base::attributes(tempo$y$breaks))}
            ), 
            warn = base::paste0("\n", warn, "\n\n"), 
            ggplot = if(return.ggplot == TRUE){main.plot}else{NULL}, # main plot -> plots the graph if return == TRUE
            gtable = if(return.gtable == TRUE){grob.save}else{NULL} # gtable of the full graph (main + title + legend)
        )
        base::return(output) # this plots the graph if return.ggplot is TRUE and if no assignment
    }
    # end output
    # end main code
}



