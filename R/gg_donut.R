#' @title gg_donut
#' @description
#' Plot a ggplot2 donut using contingency data, systematically in the decreasing order of frequencies, starting at the top and turning clockwise. 
#' For ggplot2 specifications, see: https://ggplot2.tidyverse.org/articles/ggplot2-specs.html.
#' @param data1 a dataframe compatible with ggplot2. 
#' @param freq single character string of the data1 column name of the frequencies.
#' @param categ single character string of the data1 column name of categories (qualitative variable).
#' @param fill_palette single character string of a palette name (see ?ggplot2::scale_fill_brewer() for the list).If NULL, default colors of ggplot2 are used. (palette = 1 of ggplot2::scale_fill_brewer()). Ignored if fill_color is not NULL. 
#' @param fill_color either (1) NULL, or (2) a vector of character strings or integers of same length as the number of classes in categ. Colors can be color names (see ?colors() in R), hexadecimal color codes, or integers (according to the ggplot2 palette). The order of the elements will be used according to the frequency values, from highest to lowest. An easy way to use this argument is to sort data1 according to the frequencies values, add a color column with the corresponding desired colors and use the content of this column as values of fill_color. If color is NULL and fill_palette is NULL, default colors of ggplot2 are used. If color is not NULL, it overrides fill_palette. 
#' @param hole_size single positive proportion of donut central hole, 0 meaning no hole (pie chart) and 1 no plot (donut with a null thickness). 
#' @param hole_text logical (either TRUE or FALSE). Display the sum of frequencies (column of data1 indicated in the freq argument). 
#' @param hole_text_size single positive numeric value of the title font size in mm. Ignored if hole_text is FALSE 
#' @param border_color a single character string or integer. Colors can be color names (see ?colors() in R), hexadecimal color codes, or integers (according to the ggplot2 palette).
#' @param border_size a single positive numeric value in mm. 
#' @param title single character string of the graph title. 
#' @param title_text_size single numeric value of the title font size in mm. 
#' @param annotation single character string of the data1 column name of annotations. Values inside this column will be displayed over the corresponding slices of the donut. Write NULL if not required. 
#' @param annotation_distance single positive numeric value of the distance from the center of the slice. 0 means center of the slice, 0.5 means at the edge. Above 0.5, the donut will be reduced to make place for the annotation. Ignored if annotation is NULL. 
#' @param annotation_size single positive numeric value of the annotation font size in mm. Ignored if annotation is NULL. 
#' @param annotation_force single positive numeric value of the force of repulsion between overlapping text labels. See ?ggrepel::geom_text_repel() in R. Ignored if annotation is NULL. 
#' @param annotation_force_pull single positive numeric value of the force of attraction between a text label and its corresponding data point. See ?
#' @param legend_show logical (either TRUE or FALSE). Show legend?. 
#' @param legend_width single proportion (between 0 and 1) indicating the relative width of the legend sector (on the right of the plot) relative to the width of the plot. Value 1 means that the window device width is split in 2, half for the plot and half for the legend. Value 0 means no room for the legend, which will overlay the plot region. Write NULL to inactivate the legend sector. In such case, ggplot2 will manage the room required for the legend display, meaning that the width of the plotting region can vary between graphs, depending on the text in the legend. 
#' @param legend_name character string of the legend title. If legend_name is NULL then legend_name is the value of the categ argument. Write legend_name = "" to remove the legend. 
#' @param legend_text_size single numeric value of the font size in mm of the legend labels. 
#' @param legend_box_size single numeric value of the size of the legend squares in mm. 
#' @param legend_box_space single numeric value of the space between the legend boxes in mm. 
#' @param legend_limit single positive proportion of the classes displayed in the legend for which the corresponding proportion is over legend_limit. Example: legend_limit = 0.4 means that only the sectors over 40\% of the donut will be in the legend. Write "all" for all the sectors in the legend (no limit required). Write NULL (default) for the ten biggest sectors. Warning, when legend_limit is a proportion and leads to 0 sectors represented (limit too high), then 10 sectors will be finally represented in the legend with a warning message.
#' @param legend_add_prop logical (either TRUE or FALSE). add the proportion after the class names in the legend ? 
#' @param add character string allowing to add more ggplot2 features (dots, lines, themes, facet, etc.). Ignored if NULL. WARNING: (1) the string must start with "+", (2) the string must finish with ")" and (3) each function must be preceded by "ggplot2::". Example: "+ ggplot2::coord_flip() + ggplot2::theme_bw()". If the character string contains the "ggplot2::theme" string, then the article argument of gg_donut() (see above) is ignored with a warning. In addition, some arguments can be overwritten, like x_angle (check all the arguments). Handle the add argument with caution since added functions can create conflicts with the preexisting internal ggplot2 functions. The call of objects inside the quotes of add can lead to an error if the name of these objects are some of the gg_donut() arguments. Indeed, the function will use the internal argument instead of the global environment object. Example article <- "a" in the working environment and add = '+ ggplot2::ggtitle(article)'. The risk here is to have TRUE as title. To solve this, use add = '+ ggplot2::ggtitle(get("article", envir = .GlobalEnv))'
#' @param return logical (either TRUE or FALSE). Return the graph parameters?
#' @param return_ggplot logical (either TRUE or FALSE). Return the ggplot object in the output list? Ignored if return argument is FALSE. WARNING: always assign the gg_donut() function (e.g., a <- gg_donut()) into something if the return_ggplot argument is TRUE, otherwise, double plotting is performed. See $ggplot in the RETURN section below for more details.
#' @param return_gtable logical (either TRUE or FALSE). Return the full graph (main, title and legend) as a gtable of grobs in the output list? See $gtable in the RETURN section below for more details.
#' @param plot logical (either TRUE or FALSE). Plot the graphic? If FALSE and return argument is TRUE, graphical parameters and associated warnings are provided without plotting.
#' @param warn_print logical (either TRUE or FALSE). Print warnings at the end of the execution? ? If FALSE, warning messages are never printed, but can still be recovered in the returned list. Some of the warning messages (those delivered by the internal ggplot2 functions) are not apparent when using the argument plot = FALSE.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R packages are not installed in the default directories because of lack of admin rights. More precisely, \code{lib_path} is passed through the \code{new} argument of \code{.libPaths()} so that the new library paths are \code{unique(c(new, .Library.site, .Library))}. Warning: \code{.libPaths()} is restored to the initial paths, after function execution. Ignored if \code{NULL} (default): only the pathways specified by the current \code{.libPaths()} are used for package calling.
#' @param safer_check Single logical value. Perform the "safer" checks? If \code{TRUE}, checkings are performed before main code running (see the \href{https://github.com/safer-r}{safer-r project}): 1) required functions and related packages effectively present in local R libraries and 2) R classical operators (like \code{"<-"}) not overwritten by another package because of the R scope. Warning: must be set to \code{FALSE} if this function is used inside another "safer" function to avoid pointless multiple checkings.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>.". . If NULL, converted into "".
#' @returns a donut plot if plot argument is TRUE.
#' @returns a list of the graph info if return argument is TRUE:
    #' $data: the initial data with modifications and with graphic information added
    #' $removed_row_nb: a list of the removed rows numbers in data frames (because of NA). NULL if no row removed
    #' $removed_rows: a list of the removed rows in data frames (because of NA). NULL if no row removed
    #' $plot_data
    #' $panel: the variable names used for the panels (NULL if no panels). WARNING: NA can be present according to ggplot2 upgrade to v3.3.0
    #' $axes: the x-axis and y-axis info
    #' $warn: the warning messages. Use cat() for proper display. NULL if no warning. WARNING: warning messages delivered by the internal ggplot2 functions are not apparent when using the argument plot = FALSE
    #' $ggplot: ggplot object that can be used for reprint (use print($ggplot) or update (use $ggplot + ggplot2::...). NULL if return_ggplot argument is FALSE. Warning: the legend is not in $ggplot as it is in a separated grob (use $gtable to get it). Of note, a non-null $ggplot in the output list is sometimes annoying as the manipulation of this list prints the plot
    #' $gtable: gtable object that can be used for reprint (use gridExtra::grid.arrange(...$ggplot) or with additionnal grobs (see the grob decomposition in the examples). Contrary to $ggplot, a non-NULL $gtable in the output list is not annoying as the manipulation of this list does not print the plot
#' @examples
#' obs1 <- data.frame(Km = c(20, 10, 1, 5), Car = c("TUUT", "WIIM", "BIP", "WROUM"), Color1 = 1:4, color2 = c("red", "blue", "green", "black"), Country = c("FR", "UK", "US", NA), stringsAsFactors = TRUE) ; gg_donut(data1 = obs1, freq = "Km", categ = "Car", annotation = "Country")
#' @importFrom ggplot2 .data
#' @importFrom ggplot2 aes
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
#' @importFrom gridExtra arrangeGrob
#' @importFrom gridExtra grid.arrange
#' @importFrom grid unit
#' @importFrom saferDev arg_check
#' @details
#' - Rows containing NA in data1[, c(freq, categ)] will be removed before processing, with a warning (see below).
#' - Size arguments (hole_text_size, border_size, title_text_size and annotation_size) are in mm. See Hadley comment in https://stackoverflow.com/questions/17311917/ggplot2-the-unit-of-size. See also http://sape.inf.usi.ch/quick-reference/ggplot2/size). Unit object are not accepted, but conversion can be used (e.g., grid::convertUnit(grid::unit(0.2, "inches"), "mm", valueOnly = TRUE)).
#' @export
gg_donut <- function(
    data1, 
    freq, 
    categ, 
    fill_palette = NULL, 
    fill_color = NULL, 
    hole_size = 0.5, 
    hole_text = TRUE, 
    hole_text_size = 14, 
    border_color = "gray50", 
    border_size = 0.2, 
    title = "", 
    title_text_size = 7, 
    annotation = NULL, 
    annotation_distance = 0, 
    annotation_size = 3, 
    annotation_force = 1, 
    annotation_force_pull = 100, 
    legend_show = TRUE, 
    legend_width = 0.25, 
    legend_name = NULL, 
    legend_text_size = 10, 
    legend_box_size = 5, 
    legend_box_space = 2, 
    legend_limit = NULL, 
    legend_add_prop = TRUE, 
    add = NULL, 
    return = FALSE, 
    return_ggplot = FALSE, 
    return_gtable = TRUE, 
    plot = TRUE, 
    warn_print = TRUE, 
    lib_path = NULL, 
    safer_check = TRUE, 
    error_text = ""
){
    # DEBUGGING
    # obs1 <- data.frame(Km = c(20, 10, 1, 5), Car = c("TUUT", "WIIM", "BIP", "WROUM"), Color1 = 1:4, color2 = c("red", "blue", "green", "black"), Country = c("FR", "UK", "US", NA), stringsAsFactors = TRUE) ; data1 = obs1 ; freq = "Km" ; categ = "Car" ; fill_palette = NULL ; fill_color = NULL ; hole_size = 0.5 ; hole_text = TRUE ; hole_text_size = 12 ; border_color = "gray50" ; border_size = 0.1 ; title = "" ; title_text_size = 12 ; annotation = "Country" ; annotation_distance = 0.5 ; annotation_size = 3 ; annotation_force = 1 ; annotation_force_pull = 100 ; legend_show = TRUE ; legend_width = 0.5 ; legend_name = NULL ; legend_text_size = 10 ; legend_box_size = 5 ; legend_box_space = 2 ; legend_limit = NULL ; legend_add_prop = FALSE ; add = NULL ; return = TRUE ; return_ggplot = FALSE ; return_gtable = TRUE ; plot = TRUE ; warn_print = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = ""
    # 


    #### package name
    package_name <- "saferGG" # write NULL if the function developed is not in a package
    #### end package name

    #### internal error report link
    internal_error_report_link <- base::paste0("https://github.com/safer-r/", package_name, "/issues/new", collapse = NULL, recycle0 = FALSE) # link where to post an issue indicated in an internal error message. Write NULL if no link to propose, or no internal error message
    #### end internal error report link

    #### function name
    tempo_settings <- base::as.list(x = base::match.call(definition = base::sys.function(which = base::sys.parent(n = 0)), call = base::sys.call(which = base::sys.parent(n = 0)), expand.dots = FALSE, envir = base::parent.frame(n = 2L))) # warning: I have written n = 0 to avoid error when a safer function is inside another functions. In addition, arguments values retrieved are not evaluated base::match.call
    function_name <- base::paste0(tempo_settings[[1]], "()", collapse = NULL, recycle0 = FALSE) 
    # function name with "()" paste, which split into a vector of three: c("::()", "package ()", "function ()") if "package::function()" is used.
    if(function_name[1] == "::()" | function_name[1] == ":::()"){
        function_name <- function_name[3]
    }
    #### end function name

    #### arguments settings
    arg_user_setting <- tempo_settings[-1] # list of the argument settings (excluding default values not provided by the user). Always a list, even if 1 argument. So ok for lapply() usage (management of NA section)
    arg_user_setting_names <- base::names(x = arg_user_setting)
    # evaluation of values if they are expression, call, etc.
    if(base::length(x = arg_user_setting) != 0){
        arg_user_setting_eval <- base::lapply(
            X = arg_user_setting_names, 
            FUN = function(x){
                base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = TRUE) # n = 2 because of lapply(), inherit = TRUE to be sure to correctly evaluate
            }
        )
        base::names(x = arg_user_setting_eval) <- arg_user_setting_names
    }else{
        arg_user_setting_eval <- NULL
    }
    # end evaluation of values if they are expression, call, etc.
    arg_names <- base::names(x = base::formals(fun = base::sys.function(which = base::sys.parent(n = 0)), envir = base::parent.frame(n = 1))) # names of all the arguments
    #### end arguments settings

    #### error_text initiation

    ######## basic error text start
    error_text <- base::paste0(base::unlist(x = error_text, recursive = TRUE, use.names = TRUE), collapse = "", recycle0 = FALSE) # convert to string. if error_text is a string, changes nothing. If NULL -> "" so no need to check for management of NULL
    package_function_name <- base::paste0(
        base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(package_name, base::ifelse(test = base::grepl(x = function_name, pattern = "^\\.", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), yes = ":::", no = "::"), collapse = NULL, recycle0 = FALSE)), 
        function_name,
        collapse = NULL, 
        recycle0 = FALSE
    )
    error_text_start <- base::paste0(
        "ERROR IN ", # must not be changed, because this "ERROR IN " string is used for text replacement
        package_function_name, 
        base::ifelse(test = error_text == "", yes = ".", no = error_text), 
        "\n\n", 
        collapse = NULL, 
        recycle0 = FALSE
    )
    ######## end basic error text start

    ######## internal error text
    intern_error_text_start <- base::paste0(
        package_function_name, 
        base::ifelse(test = error_text == "", yes = ".", no = error_text), 
        "\n\n", 
        collapse = NULL, 
        recycle0 = FALSE
    )
    intern_error_text_end <- base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, ".", collapse = NULL, recycle0 = FALSE))
    ######## end internal error text

    ######## error text when embedding
    # use this in the error_text of safer functions if present below 
    embed_error_text  <- base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
    embed_error_text  <- base::sub(pattern = "\n*$", replacement = "", x = embed_error_text, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # remove all the trailing \n, because added later
    ######## end error text when embedding

    #### end error_text initiation

    #### argument primary checking

    ######## arg ... forbidden
    # nocov start
    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
    if("..." %in% arg_names) {
        # This check is here in case the developer has not correctly written the argument of its function
        tempo_cat <- base::paste0(
            error_text_start, 
            "ARGUMENT ... IS NOT ALLOWED IN SAFER-R FUNCTIONS.\n\nPLEASE, REWRITE YOUR FUNCTION CORRECTLY.", 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    # nocov end
    ######## end arg ... forbidden

    ######## mandatory arg of safer-r functions
    mandat_args <- base::c("lib_path", "safer_check", "error_text")
    tempo_log <- ! mandat_args %in% arg_names
    if(base::any(tempo_log, na.rm = TRUE)) {
        # This check is here in case the developer has not correctly written the argument of its function
        tempo_cat <- base::paste0(
            error_text_start, 
            "FOLLOWING ARGUMENT", 
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "S ARE", no = " IS"), 
            " MANDATORY IN SAFER-R FUNCTIONS:\n", 
            base::paste0(mandat_args[tempo_log], collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end mandatory arg of safer-r functions

    ######## arg with no default values
    mandat_args <- base::c(
        "data1",
        "freq",
        "categ"
    )
    tempo <- base::eval(expr = base::parse(text = base::paste0("base::c(base::missing(", base::paste0(mandat_args, collapse = "),base::missing(", recycle0 = FALSE), "))", collapse = NULL, recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if(base::any(tempo, na.rm = TRUE)){
        tempo_cat <- base::paste0(
            error_text_start, 
            "FOLLOWING ARGUMENT", 
            base::ifelse(test = base::sum(tempo, na.rm = TRUE) > 1, yes = "S HAVE", no = " HAS"), 
            " NO DEFAULT VALUE AND REQUIRE ONE:\n", 
            base::paste0(mandat_args[tempo], collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    ######## end arg with no default values

    ######## management of NULL arguments
    # before NA checking because is.na(NULL) return logical(0) and all(logical(0)) is TRUE
    tempo_arg <-base::c(
        "data1", 
        "freq", 
        "categ", 
        # "fill_palette", # inactivated because can be NULL 
        # "fill_color", # inactivated because can be NULL 
        "hole_size", 
        "hole_text", 
        "hole_text_size", 
        "border_color", 
        "border_size", 
        "title", 
        "title_text_size", 
        # "annotation", # inactivated because can be NULL 
        "annotation_distance", 
        "annotation_size", 
        "annotation_force", 
        "annotation_force_pull", 
        "legend_show", 
        # "legend_width", # inactivated because can be NULL 
        # "legend_name", # inactivated because can be NULL 
        "legend_text_size", 
        "legend_box_size", 
        "legend_box_space", 
        # "legend_limit", # inactivated because can be NULL 
        "legend_add_prop", 
        # "add", # inactivated because can be NULL 
        "return", 
        "return_ggplot", 
        "return_gtable", 
        "plot", 
        "warn_print", 
        # "lib_path", # inactivated because can be NULL 
        "safer_check"
        # "error_text" # inactivated because NULL converted to "" above
    )
    tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::is.null(x = x)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())
    if(base::any(tempo_log, na.rm = TRUE)){ # normally no NA with base::is.null()
        tempo_cat <- base::paste0(
            error_text_start, 
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
            " CANNOT BE NULL:\n", 
            base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # before NA checking because is.na(logical()) is logical(0) (but secured with & base::length(x = x) > 0)
    tempo_arg <- base::c(
        # "data1", # inactivated because can be empty
        "freq", 
        "categ", 
        "hole_size", 
        "hole_text", 
        "hole_text_size", 
        "border_color", 
        "border_size", 
        "title_text_size", 
        "annotation_distance", 
        "annotation_size", 
        "annotation_force", 
        "annotation_force_pull", 
        "legend_show", 
        "legend_text_size", 
        "legend_box_size", 
        "legend_box_space", 
        "legend_add_prop", 
        "return", 
        "return_ggplot", 
        "return_gtable", 
        "plot", 
        "warn_print"
        # "title", # inactivated because can be empty
        # "fill_palette", # inactivated because can be NULL
        # "fill_color", # inactivated because can be NULL
        # "annotation", # inactivated because can be NULL
        # "legend_width", # inactivated because can be NULL
        # "legend_name", # inactivated because can be NULL
        # "legend_limit", # inactivated because can be NULL
        # "add", # inactivated because can be NULL
        # "lib_path", # inactivated because can be empty
        # "error_text" # inactivated because empty value converted to "" above
    )
    tempo_arg_user_setting_eval <- arg_user_setting_eval[base::names(x = arg_user_setting_eval) %in% tempo_arg]
    if(base::length(x = tempo_arg_user_setting_eval) != 0){
        tempo_log <- base::suppressWarnings(
            expr = base::sapply(
                X = tempo_arg_user_setting_eval, 
                FUN = function(x){
                    base::length(x = x) == 0 & ! base::is.null(x = x)
                }, 
                simplify = TRUE, 
                USE.NAMES = TRUE
            ), 
            classes = "warning"
        ) # no argument provided by the user can be empty non NULL object. Warning: would not work if arg_user_setting_eval is a vector (because treat each element as a compartment), but ok because it is always a list, even if 0 or 1 argument in the developed function
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
                " CANNOT BE AN EMPTY NON NULL OBJECT:\n", 
                base::paste0(base::names(x = tempo_arg_user_setting_eval)[tempo_log], collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    # Mandatory section: argument of safer-r functions cannot have NA as only value, to prevent all(, na.rm = TRUE) or any(, na.rm = TRUE) to return a logical value
    if(base::length(x = arg_user_setting_eval) != 0){
        tempo_log <- base::suppressWarnings(
            expr = base::sapply(
                X = base::lapply(
                    X = arg_user_setting_eval, 
                    FUN = function(x){
                        base::is.na(x = x) # if x is empty, return empty, but ok with below
                    }
                ), 
                FUN = function(x){
                    base::all(x, na.rm = TRUE) & base::length(x = x) > 0 # if x is empty, return FALSE, so OK
                }, 
                simplify = TRUE, 
                USE.NAMES = TRUE
            ), 
            classes = "warning"
        ) # no argument provided by the user can be just made of NA. is.na(NULL) returns logical(0), the reason why base::length(x = x) > 0 is used # warning: all(x = x, na.rm = TRUE) but normally no NA because base::is.na() used here. Warning: would not work if arg_user_setting_eval is a vector (because treat each element as a compartment), but ok because it is always a list, even if 0 or 1 argument in the developed function
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
                " CANNOT BE MADE OF NA ONLY:\n", 
                base::paste0(base::names(x = arg_user_setting_eval)[tempo_log], collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    if( ! (base::all(base::typeof(x = safer_check) == "logical", na.rm = TRUE) & base::length(x = safer_check) == 1)){ # no need to test NA because NA only already managed above and base::length(x = safer_check) == 1)
        if(base::all(base::mode(x = safer_check) == "function", na.rm = TRUE)){
            safer_check <- base::deparse1(expr = safer_check, collapse = "", width.cutoff = 500L)
        }
        tempo_cat <- base::paste0(
            error_text_start, 
            "THE safer_check ARGUMENT VALUE MUST BE A SINGLE LOGICAL VALUE (TRUE OR FALSE ONLY).\nHERE IT IS:\n", 
            base::ifelse(test = base::length(x = safer_check) == 0 | base::all(base::suppressWarnings(expr = safer_check == base::quote(expr = ), classes = "warning"), na.rm = TRUE) | base::all(safer_check == "", na.rm = TRUE), yes = "<NULL, \"\", EMPTY OBJECT OR EMPTY NAME>", no = base::paste0(safer_check, collapse = "\n", recycle0 = FALSE)),
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end safer_check argument checking

    ######## check of lib_path
    # must be before any :: or ::: non basic package calling
    # if(safer_check == TRUE){ # this line must be activated if lib_path is only used inside if(safer_check == TRUE){}. Which is not the case using saferDev::arg_check()
    if( ! base::is.null(x = lib_path)){ #  is.null(NA) returns FALSE so OK.
        if( ! base::all(base::typeof(x = lib_path) == "character", na.rm = TRUE)){ # na.rm = TRUE but no NA returned with typeof (typeof(NA) == "character" returns FALSE)
            if(base::all(base::mode(x = lib_path) == "function", na.rm = TRUE)){
                lib_path <- base::deparse1(expr = lib_path, collapse = "", width.cutoff = 500L)
            }
            tempo_cat <- base::paste0(
                error_text_start, 
                "THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT MUST BE A VECTOR OF CHARACTERS.\nHERE IT IS:\n", 
                base::ifelse(test = base::length(x = lib_path) == 0 | base::all(base::suppressWarnings(expr = lib_path == base::quote(expr = ), classes = "warning"), na.rm = TRUE), yes = "<NULL, EMPTY OBJECT OR EMPTY NAME>", no = base::paste0(lib_path, collapse = "\n", recycle0 = FALSE)),
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if( ! base::all(base::dir.exists(paths = lib_path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib_path == NA. dir.exists(paths = NA) returns an error, so ok. dir.exists(paths = "") returns FALSE so ok
            tempo_log <- ! base::dir.exists(paths = lib_path)
            tempo_cat_b <- lib_path[tempo_log] # here lib_path is character string
            tempo_cat_b[tempo_cat_b == ""] <- "\"\""
            tempo_cat <- base::paste0(
                error_text_start, 
                "THE DIRECTORY PATH",
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "S", no = ""), 
                " INDICATED IN THE lib_path ARGUMENT DO", 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "", no = "ES"), 
                " NOT EXIST:\n", 
                base::paste0(tempo_cat_b, collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            ini_lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
            base::on.exit(expr = base::.libPaths(new = ini_lib_path, include.site = TRUE), add = TRUE, after = TRUE) # return to the previous libPaths()
            base::.libPaths(new = base::sub(x = base::c(ini_lib_path, lib_path), pattern = "/$|\\\\$", replacement = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), include.site = TRUE) # base::.libPaths(new = ) add path to default path. BEWARE: base::.libPaths() does not support / at the end of a submitted path. The reason of the check and replacement of the last / or \\ in path
            lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
        }
    }else{
        lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument # base::.libPaths(new = lib_path) # or base::.libPaths(new = base::c(base:::.libPaths(), lib_path))
    }
    # } # this line must be activated if lib_path is only used inside if(safer_check == TRUE){}. Which is not the case using saferDev::arg_check()
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    if(safer_check == TRUE){
        .pack_and_function_check <- utils::getFromNamespace(x = ".pack_and_function_check", ns = "saferDev", pos = -1, envir = )
        .pack_and_function_check(
            fun = base::c(
                "saferDev::arg_check", # write each function preceeded by their package name
                "ggplot2::aes",
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
                "ggplot2::scale_fill_discrete",
                "ggplot2::scale_x_continuous",
                "ggplot2::theme",
                "ggplot2::theme_void",
                "ggplot2::waiver",
                "ggplot2::ylim",
                "ggrepel::geom_text_repel",
                "gridExtra::arrangeGrob",
                "gridExtra::grid.arrange",
                "scales::alpha",
                "scales::censor",
                "saferGG::gg_get_legend"
                # end functions required in this code
                # internal functions required in this code
                , "saferDev:::.base_op_check"
                # end internal functions required in this code
            ),
            lib_path = lib_path, # write NULL if your function does not have any lib_path argument
            error_text = embed_error_text
        )
    }
    ######## end check of the required functions from the required packages

    ######## escaping CRAN submission NOTE for internal functions

    .base_op_check <- utils::getFromNamespace(x = ".base_op_check", ns = "saferDev", pos = -1, envir = )
    # add here in the internal functions that are used in your main code (copy-paste the line above and replace .base_op_check by the name of the internal function
    # not mandatory if your function is not designed for submission to the CRAN

    ######## end escaping CRAN submission NOTE for internal functions

    ######## critical operator checking
    if(safer_check == TRUE){
        .base_op_check(
            error_text = embed_error_text
        )
    }
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    argum_check <- NULL
    text_check <- NULL
    checked_arg_names <- NULL # for function debbuging: used by r_debugging_tools
    arg_check_error_text <- base::paste0("ERROR ", embed_error_text, "\n\n", collapse = NULL, recycle0 = FALSE) # must be used instead of error_text = embed_error_text when several arg_check are performed on the same argument (tempo1, tempo2, see below)
    ee <- base::expression(argum_check <- base::c(argum_check, tempo$problem) , text_check <- base::c(text_check, tempo$text) , checked_arg_names <- base::c(checked_arg_names, tempo$object_name))
    tempo <- saferDev::arg_check(data = data1, class = "data.frame", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = freq, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = categ, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(x = fill_palette)){
        tempo <- saferDev::arg_check(data = fill_palette, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = base::c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral", "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3", "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"), all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    if( ! base::is.null(x = fill_color)){
        tempo1 <- saferDev::arg_check(data = fill_color, class = NULL, typeof = "character", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text)
        tempo2 <- saferDev::arg_check(data = fill_color, class = "factor", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text)
        tempo3 <- saferDev::arg_check(data = fill_color, class = NULL, typeof = "integer", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) # inf values -> problem TRUE -> all three arg_check calls report a problem -> generic message below (dedicated Inf check useless because Inf is of mode double, not integer)
        if(tempo1$problem == TRUE & tempo2$problem == TRUE & tempo3$problem == TRUE){
            tempo_cat <- base::paste0(
                arg_check_error_text, 
                "fill_color ARGUMENT MUST BE A VECTOR OF (1) HEXADECIMAL COLOR STRINGS STARTING BY #, OR (2) COLOR NAMES GIVEN BY grDevices::colors(), OR (3) POSITIVE INTEGER VALUES",
                collapse = NULL, 
                recycle0 = FALSE
            )
            text_check <- base::c(text_check, tempo_cat)
            argum_check <- base::c(argum_check, TRUE)
            checked_arg_names <- base::c(checked_arg_names, tempo1$object_name)
        }else if(tempo3$problem == FALSE & base::any(fill_color == 0, na.rm = TRUE)){ # the dedicated Inf check was removed: an integer vector cannot contain Inf (is.infinite() is always FALSE on typeof integer), so this branch was unreachable
            tempo_cat <- base::paste0(
                arg_check_error_text, 
                "fill_color ARGUMENT CANNOT CONTAIN 0 AMONG POSITIVE INTEGER VALUES",
                collapse = NULL, 
                recycle0 = FALSE
            )
            text_check <- base::c(text_check, tempo_cat)
            argum_check <- base::c(argum_check, TRUE)
            checked_arg_names <- base::c(checked_arg_names, tempo1$object_name)
        }
    }
    tempo <- saferDev::arg_check(data = hole_size, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = TRUE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = hole_text, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = hole_text_size, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo1 <- saferDev::arg_check(data = border_color, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text)
    tempo2 <- saferDev::arg_check(data = border_color, class = NULL, typeof = "integer", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text)
    if(tempo1$problem == TRUE & tempo2$problem == TRUE){
            tempo_cat <- base::paste0(
                arg_check_error_text, 
                "border_color ARGUMENT MUST BE A SINGLE CHARACTER STRING OR POSITIVE INTEGER",
                collapse = NULL, 
                recycle0 = FALSE
            )
            text_check <- base::c(text_check, tempo_cat)
            argum_check <- base::c(argum_check, TRUE)
            checked_arg_names <- base::c(checked_arg_names, tempo1$object_name)
        }
    tempo <- saferDev::arg_check(data = border_size, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = title, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = title_text_size, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(x = annotation)){
        tempo <- saferDev::arg_check(data = annotation, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
        tempo <- saferDev::arg_check(data = annotation_distance, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
        tempo <- saferDev::arg_check(data = annotation_size, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
        tempo <- saferDev::arg_check(data = annotation_force, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
        tempo <- saferDev::arg_check(data = annotation_force_pull, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    tempo <- saferDev::arg_check(data = legend_show, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(x = legend_width)){
        tempo <- saferDev::arg_check(data = legend_width, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = TRUE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    if( ! base::is.null(x = legend_name)){
        tempo <- saferDev::arg_check(data = legend_name, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    tempo <- saferDev::arg_check(data = legend_text_size, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = legend_box_size, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = legend_box_space, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(x = legend_limit)){
        if(base::all(base::is.character(x = legend_limit), na.rm = FALSE)){
            tempo <- saferDev::arg_check(data = legend_limit, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = base::c("all"), all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
        }else{
            tempo <- saferDev::arg_check(data = legend_limit, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = TRUE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
        }
    }
    tempo <- saferDev::arg_check(data = legend_add_prop, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(x = add)){
        tempo <- saferDev::arg_check(data = add, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    tempo <- saferDev::arg_check(data = return, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = return_ggplot, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = return_gtable, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = plot, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = warn_print, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    # lib_path already checked above
    # safer_check already checked above
    # error_text already checked above
    if( ! base::is.null(x = argum_check)){
        if(base::any(argum_check, na.rm = TRUE)){
            base::stop(base::paste0("\n\n================\n\n", base::paste0(text_check[argum_check], collapse = "\n\n", recycle0 = FALSE), "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    # check with r_debugging_tools
    # source("https://gitlab.pasteur.fr/gmillot/debugging_tools_for_r_dev/-/raw/v1.8/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using arg_check()
    # end check with r_debugging_tools
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    tempo_arg <- base::c(
        "freq", 
        "categ", 
        "fill_palette",  
        # "fill_color", # inactivated because can be a factor or integers (checked in the dedicated fill_color check below) 
        # "hole_size", 
        # "hole_text", 
        # "hole_text_size", 
        # "border_color", # inactivated because can be an integer (checked in the dedicated border_color check below) 
        # "border_size", 
        # "title", # inactivated because can be ""
        # "title_text_size", 
        "annotation", 
        # "annotation_distance", 
        # "annotation_size", 
        # "annotation_force", 
        # "annotation_force_pull", 
        # "legend_show", 
        # "legend_width", 
        "legend_name", 
        # "legend_text_size", 
        # "legend_box_size", 
        # "legend_box_space", 
        # "legend_limit",  
        # "legend_add_prop", 
        "add",  
        # "return", 
        # "return_ggplot", 
        # "return_gtable", 
        # "plot", 
        # "warn_print", 
        "lib_path"
        # "error_text" # inactivated because can be ""
    )
    tempo_log <- ! base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){if(base::is.null(x = x)){base::return(TRUE)}else{base::all(base::mode(x = x) == "character", na.rm = TRUE)}}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())  #  need to test is.null() here
    if(base::any(tempo_log, na.rm = TRUE)){
            # nocov start (unreachable: developer-error guard - the only tempo_arg entry here is lib_path, always character by construction)
        # This check is here in case the developer has not correctly fill tempo_arg
        tempo_cat <- base::paste0(
            "INTERNAL ERROR IN THE BACKBONE PART OF ", 
            intern_error_text_start, 
            "IN THE SECTION \"management of \"\" in arguments of mode character\"\n", 
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS ARE", no = "THIS ARGUMENT IS"), 
            " NOT CLASS \"character\":\n", 
            base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE), 
            intern_error_text_end, 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
            # nocov end
    }else{
        tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::any(x == "", na.rm = TRUE)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply()).  # for character argument that can also be NULL, if NULL -> returns FALSE. Thus no need to test is.null()
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS\n", no = "THIS ARGUMENT\n"), 
                base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE),
                "\nCANNOT CONTAIN EMPTY STRING \"\".", 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
        }
    }
    ######## end management of "" in arguments of mode character

    #### end argument secondary checking

    #### second round of checking and data preparation

    ######## reserved words

    ######## end reserved words

    ######## code that protects set.seed() in the global environment

    ######## end code that protects set.seed() in the global environment

    ######## warning initiation
    ini_warning_length <- base::options()$warning.length # required to have the max characters of output messages
    base::options(warning.length = 8170)
    base::on.exit(expr = base::options(warning.length = ini_warning_length), add = TRUE, after = TRUE) # registered here so that warning.length is restored even if an error occurs before the warning-output on.exit() registrations below
    warn <- NULL
    warn_count <- 0
    ######## end warning initiation

    ######## graphic device checking
    # check the number of graphic devices on exit
    dev_list <- grDevices::dev.list() 
    if(plot == FALSE){
        base::on.exit(
        expr = # nocov start (unreachable in tests: fires only if the function leaks an open graphic device, which its own device management prevents)
        if(base::length(x = dev_list) != base::length(x = grDevices::dev.list())){
                tempo_cat <- base::paste0(
                    "INTERNAL ERROR IN THE BACKBONE PART OF ", 
                    intern_error_text_start, 
                    "SOME GRAPHIC DEVICES WERE OPENED BY ", 
                    function_name, 
                    " BUT NOT CLOSED BEFORE END OF EXECUTION.\n\nIF IT IS EXPECTED, JUST REMOVE THE CODE DISPLAYING THIS MESSAGE INSIDE ", 
                    function_name, 
                    ".\n\nOTHERWISE, THE PROBLEM COMES FROM OPENED GRAPHIC DEVICES BEFORE RUNNING ", 
                    function_name, 
                    " (n = ", 
                    base::length(x = dev_list), 
                    ") AND AFTER (n = ", 
                    base::length(x = grDevices::dev.list()), 
                    ").", 
                    intern_error_text_end, 
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        # nocov end
            }, 
            add = TRUE, 
            after = TRUE
        )
    }
    # end check the number of graphic devices on exit
    # restore the graphic parameters on exit
    if(base::length(x = grDevices::dev.list()) > 0){
        par_ini <- base::suppressWarnings(expr = graphics::par(no.readonly = TRUE), classes = "warning") # to recover the present graphical parameters
        base::on.exit(expr = base::suppressWarnings(expr = graphics::par(par_ini, no.readonly = TRUE), classes = "warning"), add = TRUE, after = TRUE)
    }
    # end restore the graphic parameters on exit
    ######## end graphic device checking

    ######## other checkings
    if(base::length(x = data1) == 0){
        tempo_cat <- base::paste0(
            error_text_start, 
            "data1 ARGUMENT CANNOT BE LENGTH 0.", 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    removed_row_nb <- NULL
    removed_rows <- base::data.frame(row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = FALSE)
    data1_ini <- data1 # strictly identical to data1
    if( ! freq %in% base::names(x = data1)){
        tempo_cat <- base::paste0(
            error_text_start, 
            "freq ARGUMENT MUST BE A COLUMN NAME OF THE data1 ARGUMENT", 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }else{
        if(base::all(base::is.na(x = data1[ , freq]) | base::is.infinite(x = data1[ , freq]), na.rm = FALSE)){
            tempo_cat <- base::paste0(
            error_text_start, 
            "THE freq COLUMN OF data1 CANNOT BE JUST NA OR Inf", 
            collapse = NULL, 
            recycle0 = FALSE
        )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        tempo <- saferDev::arg_check(data = data1[ , freq], class = NULL, typeof = NULL, mode = "numeric", length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text)
        if(tempo$problem == TRUE){
            tempo_cat <- base::paste0(
                error_text_start, 
                tempo$text,
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        # Inf and NA removal
        if(base::any(base::is.infinite(x = data1[, freq]) | base::is.na(x = data1[, freq]), na.rm = FALSE)){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") PRESENCE OF Inf OR NA VALUES IN THE ", freq, " COLUMN OF THE data1 ARGUMENT AND CORRESPONDING ROWS REMOVED (SEE $removed_row_nb AND $removed_rows)", collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            tempo <- base::which(x = base::is.infinite(x = data1_ini[, freq]) | base::is.na(x = data1_ini[, freq]), arr.ind = FALSE, useNames = TRUE) # data_ini used for the output
            removed_row_nb <- base::c(removed_row_nb, tempo)
            removed_rows <- base::rbind(removed_rows, data1_ini[tempo, ], stringsAsFactors = FALSE, deparse.level = 1) # data_ini used for the output
            data1 <- data1[ ! (base::is.infinite(x = data1[, freq]) | base::is.na(x = data1[, freq])), ] #
        }
        # end Inf and NA removal
        # 0 removal
        if(base::any(data1[, freq] == 0, na.rm = FALSE)){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") PRESENCE OF 0 VALUES IN THE ", freq, " COLUMN OF THE data1 ARGUMENT AND CORRESPONDING ROWS REMOVED (SEE $removed_row_nb AND $removed_rows)", collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            tempo <- base::which(x = data1[, freq] == 0, arr.ind = FALSE, useNames = TRUE) # data_ini used for the output
            removed_row_nb <- base::c(removed_row_nb, tempo)
            removed_rows <- base::rbind(removed_rows, data1_ini[tempo, ], stringsAsFactors = FALSE, deparse.level = 1) # data_ini used for the output
            data1 <- data1[ data1[, freq] != 0, ] #
        }
        # end 0 removal
    }

    if( ! categ %in% base::names(x = data1)){
        tempo_cat <- base::paste0(
                error_text_start, 
                "categ ARGUMENT MUST BE A COLUMN NAME OF THE data1 ARGUMENT", 
                collapse = NULL, 
                recycle0 = FALSE
            )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }else{
        if(base::nrow(x = data1) > 0 & base::all(base::is.na(x = data1[ ,categ]), na.rm = FALSE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                "THE categ COLUMN OF data1 CANNOT BE JUST NA", 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        tempo1 <- saferDev::arg_check(data = data1[ , categ], class = NULL, typeof = "character", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text)
        tempo2 <- saferDev::arg_check(data = data1[ , categ], class = "factor", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text)
        if(tempo1$problem == TRUE & tempo2$problem == TRUE){
            tempo_cat <- base::paste0(
                error_text_start, 
                "THE categ COLUMN OF data1 MUST BE CLASS \"factor\" OR \"character\"", 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        # NA removal
        if(base::any(base::is.na(x = data1[, categ]), na.rm = FALSE)){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") PRESENCE OF NA VALUES IN THE ", categ, " COLUMN OF THE data1 ARGUMENT AND CORRESPONDING ROWS REMOVED (SEE $removed_row_nb AND $removed_rows)", collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            tempo <- base::which(x = base::is.na(x = data1_ini[, categ]), arr.ind = FALSE, useNames = TRUE) # data_ini used for the output
            removed_row_nb <- base::c(removed_row_nb, tempo)
            removed_rows <- base::rbind(removed_rows, data1_ini[tempo, ], stringsAsFactors = FALSE, deparse.level = 1) # data_ini used for the output
            data1 <- data1[ ! base::is.na(x = data1[, categ]), ] #
        }
        # end Inf and NA removal
        if(base::any(base::duplicated(x = data1[, categ], incomparables = FALSE), na.rm = FALSE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                "THE categ COLUMN OF data1 CANNOT CONTAIN DUPLICATED VALUES\n", 
                base::paste0(data1[, categ][base::duplicated(x = data1[, categ], incomparables = FALSE)], collapse = " ", recycle0 = FALSE),
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }

    if( ! base::is.null(x = annotation)){
        if( ! annotation %in% base::names(x = data1)){
            tempo_cat <- base::paste0(
                error_text_start, 
                "annotation ARGUMENT MUST BE A COLUMN NAME OF THE data1 ARGUMENT",
                    collapse = NULL, 
                    recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            if(base::nrow(x = data1) > 0 & base::all(base::is.na(x = data1[ , annotation]), na.rm = FALSE)){
                tempo_cat <- base::paste0(
                    error_text_start, 
                    "IF NON NULL, THE annotation COLUMN OF data1 CANNOT BE JUST NA",
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
            tempo1 <- saferDev::arg_check(data = data1[ , annotation], class = NULL, typeof = "character", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text)
            tempo2 <- saferDev::arg_check(data = data1[ , annotation], class = "factor", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text)
            if(tempo1$problem == TRUE & tempo2$problem == TRUE){
                tempo_cat <- base::paste0(
            error_text_start, "THE annotation COLUMN OF data1 MUST BE CLASS \"factor\" OR \"character\"", collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
            if(base::any(base::duplicated(x = data1[, annotation], incomparables = FALSE), na.rm = FALSE)){
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") PRESENCE OF DUPLICATED VALUES IN THE ", annotation, " COLUMN OF THE data1 ARGUMENT: ", base::paste0(data1[, annotation][base::duplicated(x = data1[, annotation], incomparables = FALSE)], collapse = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
                warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            }
        }
    }
    if(base::nrow(x = data1) == 0){
        tempo_cat <- base::paste0(
            error_text_start,
            "THE data1 ARGUMENT IS EMPTY AFTER Inf, NA AND 0 REMOVAL IN THE ",
            freq, base::ifelse(test = base::is.null(x = annotation), yes = " AND ", no = ", "),
            categ, base::ifelse(test = base::is.null(x = annotation), yes = "", no = " AND "),
            " COLUMNS",
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    if( ! base::is.null(x = fill_color)){
        if( ! base::is.numeric(x = fill_color)){
            if( ! base::all(fill_color[ ! base::is.na(x = fill_color)] %in% grDevices::colors(distinct = FALSE) | base::grepl(pattern = "^#", x = fill_color[ ! base::is.na(x = fill_color)], ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), na.rm = TRUE)){
                tempo_cat <- base::paste0(
                    error_text_start, 
                    "fill_color ARGUMENT MUST BE A VECTOR OF (1) HEXADECIMAL COLOR STRINGS STARTING BY #, OR (2) COLOR NAMES GIVEN BY grDevices::colors(), OR (3) INTEGER VALUES", 
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                fill_color <- base::as.character(x = fill_color) # remove class factor is any
            }
        }
    }
    if( ! base::is.numeric(x = border_color)){
        if( ! (border_color %in% grDevices::colors(distinct = FALSE) | base::grepl(pattern = "^#", x = border_color, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE))){
            tempo_cat <- base::paste0(
                error_text_start, 
                "border_color ARGUMENT MUST BE (1) A HEXADECIMAL COLOR STRING STARTING BY #, OR (2) A COLOR NAME GIVEN BY grDevices::colors(), OR (3) AN INTEGER VALUE", 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            border_color <- base::as.character(x = border_color) # remove class factor is any
        }
    }
    # legend name filling
    if(base::is.null(x = legend_name)){
        legend_name <- categ
    }
    # legend_name not NULL anymore
    # end legend name filling
    # verif of add
    if( ! base::is.null(x = add)){
        if( ! base::grepl(pattern = "^\\s*\\+", x = add, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){ # check that the add string start by +
            tempo_cat <- base::paste0(
                error_text_start, 
                "add ARGUMENT MUST START WITH \"+\": ", 
                base::paste(base::unique(x = add, incomparables = FALSE), sep = " ", collapse = " ", recycle0 = FALSE),
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            
        }else if( ! base::grepl(pattern = "(ggplot2|lemon)\\s*::", x = add, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){ #
            tempo_cat <- base::paste0(
                error_text_start, 
                "FOR EASIER FUNCTION DETECTION, add ARGUMENT MUST CONTAIN \"ggplot2::\" OR \"lemon::\" IN FRONT OF EACH GGPLOT2 FUNCTION: ", 
                base::paste(base::unique(x = add, incomparables = FALSE), sep = " ", collapse = " ", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if( ! base::grepl(pattern = ")\\s*$", x = add, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){ # check that the add string finished by )
            tempo_cat <- base::paste0(
                error_text_start, 
                "add ARGUMENT MUST FINISH BY \")\": ", 
                base::paste(base::unique(x = add, incomparables = FALSE), sep = " ", collapse = " ", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    # end verif of add
    # management of add containing facet
    facet_categ <- NULL
    if( ! base::is.null(x = add)){
        facet_check <- TRUE
        tempo <- base::unlist(x = base::strsplit(x = add, split = "\\s*\\+\\s*(ggplot2|lemon)\\s*::\\s*", fixed = FALSE, perl = FALSE, useBytes = FALSE), recursive = TRUE, use.names = TRUE) #
        tempo <- base::sub(pattern = "^facet_wrap", replacement = "ggplot2::facet_wrap", x = tempo, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
        tempo <- base::sub(pattern = "^facet_grid", replacement = "ggplot2::facet_grid", x = tempo, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
        tempo <- base::sub(pattern = "^facet_rep", replacement = "lemon::facet_rep", x = tempo, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)

        if(base::any(base::grepl(pattern = "ggplot2::facet_wrap|lemon::facet_rep_wrap", x = tempo, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), na.rm = FALSE)){
            tempo1 <- base::suppressWarnings(expr = base::eval(expr = base::parse(text = tempo[base::grepl(pattern = "ggplot2::facet_wrap|lemon::facet_rep_wrap", x = tempo, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)], file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)), classes = "warning")
            facet_categ <- base::list(base::names(x = tempo1$params$facets)) # list of length 1
            tempo_text <- "facet_wrap OR facet_rep_wrap"
            facet_check <- FALSE
        }else if(base::grepl(pattern = "ggplot2::facet_grid|lemon::facet_rep_grid", x = add, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){
            tempo1 <- base::suppressWarnings(expr = base::eval(expr = base::parse(text = tempo[base::grepl(pattern = "ggplot2::facet_grid|lemon::facet_rep_grid", x = tempo, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)], file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)), classes = "warning")
            facet_categ <- base::list(base::c(base::names(x = tempo1$params$rows), base::names(x = tempo1$params$cols))) # list of length 1
            tempo_text <- "facet_grid OR facet_rep_grid"
            facet_check <- FALSE
        }
        if(facet_check == FALSE & ! base::all(facet_categ %in% base::names(x = data1), na.rm = FALSE)){ # WARNING: all(facet_categ %in% names(data1)) is TRUE when facet_categ is NULL
            tempo_cat <- base::paste0(
                error_text_start, 
                "DETECTION OF \"", 
                tempo_text, 
                "\" STRING IN THE add ARGUMENT BUT PROBLEM OF VARIABLE DETECTION (COLUMN NAMES OF data1)\nTHE DETECTED VARIABLES ARE:\n", 
                base::paste(facet_categ, sep = " ", collapse = " ", recycle0 = FALSE),
                "\nTHE data1 COLUMN NAMES ARE:\n",
                base::paste(base::names(x = data1), sep = " ", collapse = " ", recycle0 = FALSE),
                "\nPLEASE REWRITE THE add STRING AND RERUN",
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    # if facet_categ is not NULL, it is a list of length 1 now
    # end management of add containing facet
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code

    data1 <- base::data.frame(data1, prop = data1[ , freq] / base::sum(data1[ , freq], na.rm = FALSE), check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = FALSE) # row.names argument not written on purpose: an explicit row.names = NULL would drop the data1 row names (data.frame() takes them from data1 only when row.names is missing)
    if(legend_add_prop == TRUE){
        data1[ , categ] <- base::paste0(data1[ , categ], " (", base::round(x = data1$prop, digits = 2), ")", collapse = NULL, recycle0 = FALSE)
    }
    tempo_levels <- data1[ , categ][base::order(data1$prop, na.last = TRUE, decreasing = TRUE, method = "auto")]
    data1[ , categ] <- base::factor(x = data1[ , categ], levels = tempo_levels, labels = tempo_levels, exclude = NA, ordered = base::is.ordered(x = data1[ , categ]), nmax = NA) # reorder so that the donut is according to decreasing proportion starting at the top in a clockwise direction
    data1 <- data1[base::order(base::as.numeric(x = data1[ , categ]), na.last = TRUE, decreasing = FALSE, method = "auto"), ] # data1[ , categ] with rows in decreasing order, according to prop
    data1 <- base::data.frame(data1, x = 0, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = FALSE) # staked bar at the origin of the donut set to x = 0 # row.names argument not written on purpose (see above)
    tempo_gg_name <- "gg.indiv.plot."
    tempo_gg_count <- 0
    base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = base::eval(expr = base::parse(file = "", n = NULL, text = base::paste0("ggplot2::ggplot()", if(base::is.null(x = add)){""}else{add}, collapse = NULL, recycle0 = FALSE), prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # add added here to have the facets
    bar_width = 1
    base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_col(
        mapping = ggplot2::aes(x = .data[["x"]], y = .data[[freq]], fill = .data[[categ]]),
        data = data1,
        stat = "identity",
        position = "stack",
        color = border_color,
        linewidth = border_size,
        width = bar_width,
        just = 0.5,
        lineend = "butt",
        linejoin = "mitre",
        na.rm = FALSE,
        show.legend = NA,
        inherit.aes = TRUE
    ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # size is size of the separation in the donut
    # assign(paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), ggplot2::geom_text(
    #     ggplot2::aes(label = Freq), 
    #     position = ggplot2::position_stack(vjust = 0.5)
    # ))
    base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_x_continuous(
        name = ggplot2::waiver(),
        breaks = ggplot2::waiver(),
        minor_breaks = ggplot2::waiver(),
        n.breaks = NULL,
        labels = ggplot2::waiver(),
        limits = base::c(- bar_width / 2 - (bar_width * hole_size) / (1 - hole_size), base::max(bar_width / 2, annotation_distance, na.rm = FALSE)),
        expand = base::c(0, 0), # prevent extra limits in x axis
        oob = scales::censor,
        na.value = NA_real_,
        transform = "identity",
        guide = ggplot2::waiver(),
        position = "bottom",
        # trans = base::quote(expr = ): value returned by lifecycle::deprecated(), the default value of the deprecated trans argument (written without the lifecycle package, not imported by saferGG)
        sec.axis = ggplot2::waiver(), trans = base::quote(expr = )
    ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # must be centered on x = 0 # trans argument not written because deprecated (replaced by transform)
    base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::ylim(base::c(0, base::max(base::cumsum(x = data1[ , freq]), na.rm = FALSE))), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
    if(hole_text == TRUE){
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::annotate(
            geom = "text",
            x = - bar_width / 2 - (bar_width * hole_size) / (1 - hole_size),
            y = 0,
            xmin = NULL,
            xmax = NULL,
            ymin = NULL,
            ymax = NULL,
            xend = NULL,
            yend = NULL,
            label = base::sum(data1[ , freq], na.rm = FALSE),
            size = hole_text_size,
            na.rm = FALSE
        ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
    }
    base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::coord_polar(theta = "y", direction = -1, start = 0, clip = "on"), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
    if(base::is.null(x = fill_color) & ! base::is.null(x = fill_palette)){
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_fill_brewer(name = legend_name, type = "seq", palette = fill_palette, direction = 1, aesthetics = "fill"), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
    }else if( ! base::is.null(x = fill_color)){
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_fill_manual(values = fill_color, name = legend_name, aesthetics = "fill", breaks = ggplot2::waiver(), na.value = "white"), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
    }
    if(legend_name != ""){
       base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::labs(fill = legend_name, title = ggplot2::waiver(), subtitle = ggplot2::waiver(), caption = ggplot2::waiver(), tag = ggplot2::waiver(), dictionary = ggplot2::waiver(), alt = ggplot2::waiver(), alt_insight = ggplot2::waiver()), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # title of the legend
    }

    if( ! base::is.null(x = add)){ # if add is NULL, then = 0
        if(base::grepl(pattern = "ggplot2\\s*::\\s*theme", x = add, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) == TRUE){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") \"ggplot2::theme\" STRING DETECTED IN THE add ARGUMENT\n-> INTERNAL GGPLOT2 THEME FUNCTIONS ggplot2::theme_void() HAS BEEN INACTIVATED, SO THAT THE USER THEME CAN BE EFFECTIVE", collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            add_check <- FALSE
        }else{
            base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::theme_void(base_size = 11, base_family = "", header_family = NULL, base_line_size = 11 / 22, base_rect_size = 11 / 22, ink = "black", paper = scales::alpha(colour = "black", alpha = 0), accent = "#3366FF"), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
            base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::theme(
                    legend.text = ggplot2::element_text(family = NULL, face = NULL, colour = NULL, size = legend_text_size, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL, color = NULL, margin = NULL, debug = NULL, inherit.blank = FALSE),
                    legend.spacing.y = grid::unit(x = legend_box_space, units = 'mm', data = NULL),
                    complete = FALSE,
                    validate = TRUE, line = , rect = , text = , title = , point = , polygon = , geom = , spacing = , margins = , aspect.ratio = , axis.title = , axis.title.x = , axis.title.x.top = , axis.title.x.bottom = , axis.title.y = , axis.title.y.left = , axis.title.y.right = , axis.text = , axis.text.x = , axis.text.x.top = , axis.text.x.bottom = , axis.text.y = , axis.text.y.left = , axis.text.y.right = , axis.text.theta = , axis.text.r = , axis.ticks = , axis.ticks.x = , axis.ticks.x.top = , axis.ticks.x.bottom = , axis.ticks.y = , axis.ticks.y.left = , axis.ticks.y.right = , axis.ticks.theta = , axis.ticks.r = , axis.minor.ticks.x.top = , axis.minor.ticks.x.bottom = , axis.minor.ticks.y.left = , axis.minor.ticks.y.right = , axis.minor.ticks.theta = , axis.minor.ticks.r = , axis.ticks.length = , axis.ticks.length.x = , axis.ticks.length.x.top = , axis.ticks.length.x.bottom = , axis.ticks.length.y = , axis.ticks.length.y.left = , axis.ticks.length.y.right = , axis.ticks.length.theta = , axis.ticks.length.r = , axis.minor.ticks.length = , axis.minor.ticks.length.x = , axis.minor.ticks.length.x.top = , axis.minor.ticks.length.x.bottom = , axis.minor.ticks.length.y = , axis.minor.ticks.length.y.left = , axis.minor.ticks.length.y.right = , axis.minor.ticks.length.theta = , axis.minor.ticks.length.r = , axis.line = , axis.line.x = , axis.line.x.top = , axis.line.x.bottom = , axis.line.y = , axis.line.y.left = , axis.line.y.right = , axis.line.theta = , axis.line.r = , legend.background = , legend.margin = , legend.spacing = , legend.spacing.x = , legend.key = , legend.key.size = , legend.key.height = , legend.key.width = , legend.key.spacing = , legend.key.spacing.x = , legend.key.spacing.y = , legend.key.justification = , legend.frame = , legend.ticks = , legend.ticks.length = , legend.axis.line = , legend.text.position = , legend.title = , legend.title.position = , legend.position = , legend.position.inside = , legend.direction = , legend.byrow = , legend.justification = , legend.justification.top = , legend.justification.bottom = , legend.justification.left = , legend.justification.right = , legend.justification.inside = , legend.location = , legend.box = , legend.box.just = , legend.box.margin = , legend.box.background = , legend.box.spacing = , panel.background = , panel.border = , panel.spacing = , panel.spacing.x = , panel.spacing.y = , panel.grid = , panel.grid.major = , panel.grid.minor = , panel.grid.major.x = , panel.grid.major.y = , panel.grid.minor.x = , panel.grid.minor.y = , panel.ontop = , panel.widths = , panel.heights = , plot.background = , plot.title = , plot.title.position = , plot.subtitle = , plot.caption = , plot.caption.position = , plot.tag = , plot.tag.position = , plot.tag.location = , plot.margin = , strip.background = , strip.background.x = , strip.background.y = , strip.clip = , strip.placement = , strip.text = , strip.text.x = , strip.text.x.bottom = , strip.text.x.top = , strip.text.y = , strip.text.y.left = , strip.text.y.right = , strip.switch.pad.grid = , strip.switch.pad.wrap = 
            ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
        }
    }else{
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::theme_void(base_size = 11, base_family = "", header_family = NULL, base_line_size = 11 / 22, base_rect_size = 11 / 22, ink = "black", paper = scales::alpha(colour = "black", alpha = 0), accent = "#3366FF"), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::theme(
                legend.text = ggplot2::element_text(family = NULL, face = NULL, colour = NULL, size = legend_text_size, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL, color = NULL, margin = NULL, debug = NULL, inherit.blank = FALSE),
                legend.spacing.y = grid::unit(x = legend_box_space, units = 'mm', data = NULL),
                complete = FALSE,
                validate = TRUE, line = , rect = , text = , title = , point = , polygon = , geom = , spacing = , margins = , aspect.ratio = , axis.title = , axis.title.x = , axis.title.x.top = , axis.title.x.bottom = , axis.title.y = , axis.title.y.left = , axis.title.y.right = , axis.text = , axis.text.x = , axis.text.x.top = , axis.text.x.bottom = , axis.text.y = , axis.text.y.left = , axis.text.y.right = , axis.text.theta = , axis.text.r = , axis.ticks = , axis.ticks.x = , axis.ticks.x.top = , axis.ticks.x.bottom = , axis.ticks.y = , axis.ticks.y.left = , axis.ticks.y.right = , axis.ticks.theta = , axis.ticks.r = , axis.minor.ticks.x.top = , axis.minor.ticks.x.bottom = , axis.minor.ticks.y.left = , axis.minor.ticks.y.right = , axis.minor.ticks.theta = , axis.minor.ticks.r = , axis.ticks.length = , axis.ticks.length.x = , axis.ticks.length.x.top = , axis.ticks.length.x.bottom = , axis.ticks.length.y = , axis.ticks.length.y.left = , axis.ticks.length.y.right = , axis.ticks.length.theta = , axis.ticks.length.r = , axis.minor.ticks.length = , axis.minor.ticks.length.x = , axis.minor.ticks.length.x.top = , axis.minor.ticks.length.x.bottom = , axis.minor.ticks.length.y = , axis.minor.ticks.length.y.left = , axis.minor.ticks.length.y.right = , axis.minor.ticks.length.theta = , axis.minor.ticks.length.r = , axis.line = , axis.line.x = , axis.line.x.top = , axis.line.x.bottom = , axis.line.y = , axis.line.y.left = , axis.line.y.right = , axis.line.theta = , axis.line.r = , legend.background = , legend.margin = , legend.spacing = , legend.spacing.x = , legend.key = , legend.key.size = , legend.key.height = , legend.key.width = , legend.key.spacing = , legend.key.spacing.x = , legend.key.spacing.y = , legend.key.justification = , legend.frame = , legend.ticks = , legend.ticks.length = , legend.axis.line = , legend.text.position = , legend.title = , legend.title.position = , legend.position = , legend.position.inside = , legend.direction = , legend.byrow = , legend.justification = , legend.justification.top = , legend.justification.bottom = , legend.justification.left = , legend.justification.right = , legend.justification.inside = , legend.location = , legend.box = , legend.box.just = , legend.box.margin = , legend.box.background = , legend.box.spacing = , panel.background = , panel.border = , panel.spacing = , panel.spacing.x = , panel.spacing.y = , panel.grid = , panel.grid.major = , panel.grid.minor = , panel.grid.major.x = , panel.grid.major.y = , panel.grid.minor.x = , panel.grid.minor.y = , panel.ontop = , panel.widths = , panel.heights = , plot.background = , plot.title = , plot.title.position = , plot.subtitle = , plot.caption = , plot.caption.position = , plot.tag = , plot.tag.position = , plot.tag.location = , plot.margin = , strip.background = , strip.background.x = , strip.background.y = , strip.clip = , strip.placement = , strip.text = , strip.text.x = , strip.text.x.bottom = , strip.text.x.top = , strip.text.y = , strip.text.y.left = , strip.text.y.right = , strip.switch.pad.grid = , strip.switch.pad.wrap = 
        ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
    }
    base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::guides(
        fill = ggplot2::guide_legend(
            title = ggplot2::waiver(),
            theme = NULL,
            position = NULL,
            direction = NULL,
            override.aes = base::list(color = "white", size  = legend_box_size),
            nrow = NULL,
            ncol = NULL,
            reverse = FALSE,
            order = 0,
            byrow = TRUE
        )
    ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # remove border of squares in legend

    # annotations on slices
    if( ! base::is.null(x = annotation)){
        tempo <- base::rev(x = base::cumsum(x = base::rev(x = data1[ , freq])))
        data1 <- base::data.frame(data1, text_y = tempo - (tempo - base::c(tempo[-1], 0)) / 2, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = FALSE) # row.names argument not written on purpose (see above)
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggrepel::geom_text_repel(
            mapping = ggplot2::aes(
                x = .data[["x"]],
                y = .data[["text_y"]],
                label = .data[[annotation]]
            ),
            data = data1,
            stat = "identity",
            # position argument not written because ggrepel::geom_text_repel() forbids position and nudge_x/nudge_y together
            parse = FALSE,
            size = annotation_size,
            box.padding = 0.25,
            point.padding = 1e-06,
            min.segment.length = 0.5,
            arrow = NULL,
            force = annotation_force,
            force_pull = annotation_force_pull,
            max.time = 0.5,
            max.iter = 10000,
            max.overlaps = base::getOption(x = "ggrepel.max.overlaps", default = 10),
            nudge_x = annotation_distance, # knowing that the bar is centered on x = 0 and that the right edge is at bar_width / 2, 0 means center of the slice, 0.5 means at the edge if bar_width = 1
            nudge_y = 0,
            xlim = base::c(NA, NA),
            ylim = base::c(NA, NA),
            na.rm = FALSE,
            show.legend = FALSE,
            direction = "both",
            seed = NA,
            verbose = base::getOption(x = "verbose", default = FALSE),
            inherit.aes = TRUE
        ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
    }
    # end annotations on slices

    # legend management
    # removal of part of the legend
    if( ! base::is.null(x = legend_limit)){
        if(base::is.character(x = legend_limit)){ # must be all (checked above)
            legend_limit <- 0
        }else if(base::sum(data1$prop >= legend_limit, na.rm = FALSE) == 0){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") THE legend_limit PARAMETER VALUE (", 
                legend_limit, 
                ") IS HIGHER THAN THE HIGHEST PROPORTION IN THE DONUT PLOT (", 
                base::max(data1$prop, na.rm = FALSE),
                ").\nLIMIT HAS BEEN CHANGED SO THAT THE LEGEND INDICATES THE 10 HIGHEST SECTORS MAX.", 
                collapse = NULL, 
                recycle0 = FALSE
            )
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            legend_limit <- NULL
        }
    }
    if(base::is.null(x = legend_limit)){
        if(base::length(x = data1$prop) >= 10){
            legend_limit <- base::sort(x = data1$prop, decreasing = TRUE)[10]
        }else{
            legend_limit <- 0
        }
    }
    base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_fill_discrete(
            breaks = base::as.character(x = data1[ , categ][data1$prop >= legend_limit]),
            palette = NULL,
            aesthetics = "fill",
            na.value = "grey50",
            type = base::getOption(x = "ggplot2.discrete.fill", default = NULL)
        ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE
    )
    # end removal of part of the legend
    if(legend_show == FALSE){ # must be here because must be before bef_final_plot
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::guides(fill = "none"), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # inactivate the initial legend
    }
    bef_final_plot <- base::suppressWarnings(expr = base::suppressMessages(expr = ggplot2::ggplot_build(plot = base::eval(expr = base::parse(text = base::paste0(tempo_gg_name, 1:tempo_gg_count, collapse = " + ", recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))), classes = "message"), classes = "warning")
    if( ! base::is.null(x = legend_width)){
        legend_plot <- base::suppressWarnings(expr = base::suppressMessages(expr = saferGG::gg_get_legend(ggplot_built = bef_final_plot, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text), classes = "message"), classes = "warning") # get legend
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::guides(fill = "none"), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # inactivate the initial legend
        if(base::is.null(x = legend_plot)){ # even if any(unlist(legend_disp)) is TRUE # plot value removed from the condition: the gtable must be built even when plot == FALSE (otherwise grid.arrange() fails on a NULL legend.plot and the pdf(NULL) device opened above is not closed)
            legend_plot <- ggplot2::ggplot(data = NULL, mapping = ggplot2::aes(x = , y = ), environment = base::environment(fun = NULL)) + ggplot2::theme_void(base_size = 11, base_family = "", header_family = NULL, base_line_size = 11 / 22, base_rect_size = 11 / 22, ink = "black", paper = scales::alpha(colour = "black", alpha = 0), accent = "#3366FF") # empty graph instead of legend
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") LEGEND REQUESTED (legend_show ARGUMENT SET TO TRUE)\nBUT IT SEEMS THAT THE PLOT HAS NO LEGEND -> EMPTY LEGEND SPACE CREATED BECAUSE OF THE NON NULL legend_width ARGUMENT\n", collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
        }
    }else{
        legend_plot <- NULL
    }
    # end legend management

    # title
    title_grob <- grid::textGrob(
        label = title,
        x = grid::unit(x = 0, units = "lines", data = NULL),
        y = grid::unit(x = 0, units = "lines", data = NULL),
        just = "centre",
        hjust = 0,
        vjust = 0,
        rot = 0,
        check.overlap = FALSE,
        default.units = "npc",
        name = NULL,
        gp = grid::gpar(fontsize = title_text_size),
        vp = NULL
    )
    # end title

    # drawing
    grDevices::pdf(file = NULL, width = , height = , onefile = , family = , title = , fonts = , version = , paper = , encoding = , bg = , fg = , pointsize = , pagecentre = , colormodel = , useDingbats = , useKerning = , fillOddEven = , compress = , timestamp = , producer = , author = ) # other arguments not written because they have no default value (taken from grDevices::pdf.options())
    grob_save <- NULL
    main_plot <- base::eval(expr = base::parse(text = base::paste0(tempo_gg_name, 1:tempo_gg_count, collapse = " + ", recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    main_plot_output <- base::suppressMessages(expr = ggplot2::ggplot_build(plot = main_plot), classes = "message")
    main_grob <- base::suppressMessages(expr = base::suppressWarnings(expr = gridExtra::arrangeGrob(
        grobs = base::list(main_plot),
        vp = NULL,
        name = "arrange",
        as.table = TRUE,
        respect = FALSE,
        clip = "off",
        nrow = NULL,
        ncol = NULL,
        widths = NULL,
        heights = NULL,
        top = if(title == ""){" "}else{title_grob},
        bottom = NULL,
        left = " ",
        right = " ",
        padding = grid::unit(x = 0.5, units = "line", data = NULL), layout_matrix = 
    ), classes = "warning"), classes = "message") # , left = " ", right = " " : trick to add margins in the plot. padding =  unit(0.5, "inch") is for top margin above the title # layout_matrix argument not written because it has no default value
    if( ! base::is.null(x = legend_width)){
        grob_save <- base::suppressMessages(expr = base::suppressWarnings(expr = gridExtra::grid.arrange(main_grob, legend_plot, ncol = 2, widths = base::c(1, legend_width), newpage = TRUE), classes = "warning"), classes = "message") # assemble grobs, ggplot, gtable into a gtable that defines the positions of the different elements (as grobs)
    }else{
        grob_save <- base::suppressMessages(expr = base::suppressWarnings(expr = base::print(x = main_grob), classes = "warning"), classes = "message")
    }
    grDevices::dev.off(which = grDevices::dev.cur()) # inactivate the pdf(NULL) above
    if(plot == TRUE){
        gridExtra::grid.arrange(grob_save, newpage = TRUE) # plot a gtable (grob)
    }else{
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count,") PLOT NOT SHOWN AS REQUESTED", collapse = NULL, recycle0 = FALSE)
        warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
    }
    # end drawing
    #### end main code

    #### warning output
    if(warn_print == TRUE & ! base::is.null(x = warn)){
        base::on.exit(
            expr = base::warning(
                base::paste0(
                    base::sub(pattern = "^ERROR IN ", replacement = "FROM ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), 
                    warn, 
                    collapse = NULL, 
                    recycle0 = FALSE
                ), call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, domain = NULL
            ), add = TRUE, after = TRUE
        )
    }
    base::on.exit(expr = base::options(warning.length = ini_warning_length), add = TRUE, after = TRUE)
    #### end warning output

    #### output
    if(return == TRUE){
        if(base::is.null(x = base::unlist(x = removed_row_nb, recursive = TRUE, use.names = TRUE))){
            removed_row_nb <- NULL
            removed_rows <- NULL
        }
        tempo <- main_plot_output$layout$panel_params[[1]]
        output <- base::list(
            data = data1, 
            removed_row_nb = removed_row_nb, 
            removed_rows = removed_rows, 
            plot_data = main_plot_output$data, 
            panel = facet_categ, 
            axes = base::list(
                x.range = tempo$x.range, 
                x.labels = if(base::is.null(x = base::attributes(x = tempo$x$breaks))){tempo$x$breaks}else{ # is.null(attributes(tempo$x$breaks)) test if it is number (TRUE) or character (FALSE)
                    # nocov start (unreachable: the polar coordinate panel_params have no x component, so attributes(tempo$x$breaks) is always NULL here)
                    tempo$x$scale$get_labels()
                    # nocov end
                }, 
                x.positions = if(base::is.null(x = base::attributes(x = tempo$x$breaks))){tempo$x$breaks}else{
                    # nocov start (unreachable: idem as x.labels)
                    base::unlist(x = base::attributes(x = tempo$x$breaks), recursive = TRUE, use.names = TRUE)
                    # nocov end
                }, 
                y.range = tempo$y.range, 
                y.labels = if(base::is.null(x = base::attributes(x = tempo$y$breaks))){tempo$y$breaks}else{
                    # nocov start (unreachable: the polar coordinate panel_params have no y component, so attributes(tempo$y$breaks) is always NULL here)
                    tempo$y$scale$get_labels()
                    # nocov end
                }, 
                y.positions = if(base::is.null(x = base::attributes(x = tempo$y$breaks))){tempo$y$breaks}else{
                    # nocov start (unreachable: idem as y.labels)
                    base::unlist(x = base::attributes(x = tempo$y$breaks), recursive = TRUE, use.names = TRUE)
                    # nocov end
                }
            ), 
            warn = base::paste0("\n", warn, "\n\n", collapse = NULL, recycle0 = FALSE),
            ggplot = if(return_ggplot == TRUE){main_plot}else{NULL}, # main plot -> plots the graph if return == TRUE
            gtable = if(return_gtable == TRUE){grob_save}else{NULL} # gtable of the full graph (main + title + legend)
        )
        base::return(output) # this plots the graph if return_ggplot is TRUE and if no assignment
    }
    #### end output
}



