#' @title gg_heatmap
#' @description
#' ggplot2 heatmap with the possibility to overlay a mask, see also: draw : http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization same range; scale : https://stackoverflow.com/questions/44655723/r-ggplot2-heatmap-fixed-scale-color-between-graphs; for ggplot2 specifications, see: https://ggplot2.tidyverse.org/articles/ggplot2-specs.html
#' @param data1 numeric matrix or data frame resulting from the conversion of the numeric matrix by reshape2::melt(). 
#' @param legend_name1 character string of the data1 heatmap scale legend.
#' @param low_color1 character string of the color (i.e., "blue" or "#0000FF") of the lowest scale value.
#' @param mid_color1 same as low_color1 but for the middle scale value. If NULL, the middle color is the default color between low_color1 and high_color1. BEWARE: argument midpoint1 is not ignored, even if mid_color1 is NULL, meaning that the default mid color can still be controled.
#' @param high_color1 same as low_color1 but for the highest scale value.
#' @param limit1 2 numeric values defining the lowest and higest color scale values. If NULL, take the range of data1 values. Warning values are sorted by the function. Thus, use the low_color1 and high_color1 to reverse the scale color if necessary.
#' @param midpoint1 single numeric value defining the value corresponding to the mid_color1 argument. A warning message is returned if midpoint1 does not correspond to the mean of limit1 values, because the color scale is not linear anymore. If NULL, takes the mean of limit1 values. Mean of data1, instead of mean of limit1, can be used here if required.
#' @param data2 binary mask matrix (made of 0 and 1) of same dimension as data1 or a data frame resulting from the conversion of the binary mask matrix by reshape2::melt(). Value 1 of data2 will correspond to color2 argument (value 0 will be NA color), and the opposite if invert2 argument is TRUE (inverted mask).
#' @param color2 color of the 1 values of the binary mask matrix. The 0 values will be color NA.
#' @param alpha2 numeric value (from 0 to 1) of the mask transparency.
#' @param invert2 logical. Invert the mask (1 -> 0 and 0 -> 1)?.
#' @param text_size numeric value of the size of the texts in scale.
#' @param title character string of the graph title.
#' @param title_text_size numeric value of the title size (in points).
#' @param show_scale logical. Show color scale?.
#' @param rotate logical. Rotate the heatmap 90° clockwise?.
#' @param return logical. Return the graph parameters?.
#' @param plot logical. Plot the graphic? If FALSE and return argument is TRUE, graphical parameters and associated warnings are provided without plotting.
#' @param add character string allowing to add more ggplot2 features (dots, lines, themes, etc.). BEWARE: (1) must start with "+" just after the simple or double opening quote (no space, end of line, carriage return, etc., allowed), (2) must finish with ")" just before the simple or double closing quote (no space, end of line, carriage return, etc., allowed) and (3) each function must be preceded by "ggplot2::" (for instance: "ggplot2::coord_flip()). If the character string contains the "ggplot2::theme" string, then internal ggplot2 theme() and theme_classic() functions will be inactivated to be reused by add. BEWARE: handle this argument with caution since added functions can create conflicts with the preexisting internal ggplot2 functions.
#' @param warn_print logical. Print warnings at the end of the execution? No print if no warning messages.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R packages are not installed in the default directories because of lack of admin rights. More precisely, \code{lib_path} is passed through the \code{new} argument of \code{.libPaths()} so that the new library paths are \code{unique(c(new, .Library.site, .Library))}. Warning: \code{.libPaths()} is restored to the initial paths, after function execution. Ignored if \code{NULL} (default) or if the \code{safer_check} argument is \code{FALSE}: only the pathways specified by the current \code{.libPaths()} are used for package calling.
#' @param safer_check Single logical value. Perform some "safer" checks? If \code{TRUE}, checkings are performed before main code running (see the \href{https://github.com/safer-r}{safer-r project}): 1) correct \code{lib_path} argument value 2) required functions and related packages effectively present in local R libraries and 3) R classical operators (like \code{"<-"}) not overwritten by another package because of the R scope. Must be set to \code{FALSE} if this function is used inside another "safer" function to avoid pointless multiple checkings.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: \code{error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."}. If \code{NULL}, converted into \code{""}. Of note, in \code{arg_check()}, \code{error_text} is also used at the end of the string returned when no problem is detected.
#' @returns a heatmap if plot argument is TRUE; a list of the graph info if return argument is TRUE:$data: a list of the graphic info; $axes: a list of the axes info; $warn: the warning messages. Use cat() for proper display. NULL if no warning.
#' @examples
#' gg_heatmap(data1 = matrix(1:16, ncol = 4), title = "GRAPH 1")
#' gg_heatmap(data1 = matrix(1:16, ncol = 4), return = TRUE)
#' gg_heatmap(data1 = matrix(1:16, ncol = 4), legend_name1 = "VALUE", title = "GRAPH 1", text_size = 5, data2 = matrix(rep(c(1,0,0,0), 4), ncol = 4), invert2 = FALSE, return = TRUE)
#' # diagonal matrix
#' gg_heatmap(data1 = matrix(c(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1), ncol = 4))
#' gg_heatmap(data1 = reshape2::melt(matrix(c(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1), ncol = 4)))
#' \dontrun{
#' # Error messages
#' gg_heatmap(data1 = matrix(1:16, ncol = 4), data2 = matrix(rep(c(1,0,0,0), 5), ncol = 5))
#' }
#' gg_heatmap(data1 = matrix(1:16, ncol = 4), data2 = reshape2::melt(matrix(rep(c(1,0,0,0), 4), ncol = 4)))
#' gg_heatmap(data1 = reshape2::melt(matrix(1:16, ncol = 4)), data2 = reshape2::melt(matrix(rep(c(1,0,0,0), 4), ncol = 4)))
#' @importFrom ggplot2 .data
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 coord_fixed
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_raster
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggplot_build
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 scale_y_reverse
#' @importFrom ggplot2 scale_fill_gradient2
#' @importFrom ggplot2 scale_discrete_manual
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 waiver
#' @importFrom reshape2 melt
#' @importFrom saferDev arg_check
#' @importFrom saferTool round2
#' @details
#' - NA and Inf values are displayed as grey.
#' - when using limit1 all values out of the range of limit1 are also displayed as grey.
#' @export
gg_heatmap <- function(
        data1, 
        legend_name1 = "", 
        low_color1 = "blue", 
        mid_color1 = "white", 
        high_color1 = "red", 
        limit1 = NULL, 
        midpoint1 = NULL, 
        data2 = NULL, 
        color2 = "black", 
        alpha2 = 0.5, 
        invert2 = FALSE, 
        text_size = 12, 
        title = "", 
        title_text_size = 12, 
        show_scale = TRUE, 
        rotate = FALSE, 
        return = FALSE, 
        plot = TRUE, 
        add = NULL, 
        warn_print = FALSE, 
        lib_path = NULL, 
        safer_check = TRUE, 
        error_text = ""
){
    # DEBUGGING
    # data1 = matrix(1:16, ncol = 4) ; legend_name1 = "" ; low_color1 = "blue" ; mid_color1 = "white" ; high_color1 = "red" ; limit1 = NULL ; midpoint1 = NULL ; data2 = matrix(rep(c(1,0,0,0), 4), ncol = 4) ; color2 = "black" ; alpha2 = 0.5 ; invert2 = FALSE ; text_size = 12 ; title = "" ; title_text_size = 12 ; show_scale = TRUE ; rotate = FALSE ; return = FALSE ; plot = TRUE ; add = NULL ; warn_print = TRUE ; lib_path = NULL ; safer_check = TRUE ; error_text = ""

    #### package name
    package_name <- "saferGG" # write NULL if the function developed is not in a package
    #### end package name

    #### internal error report link
    internal_error_report_link <- base::paste0("https://github.com/safer-r/", package_name, "/issues/new", collapse = NULL, recycle0 = FALSE) # link where to post an issue indicated in an internal error message. Write NULL if no link to propose, or no internal error message
    #### end internal error report link

    #### function name
    tempo_settings <- base::as.list(x = base::match.call(definition = base::sys.function(which = base::sys.parent(n = 0)), call = base::sys.call(which = base::sys.parent(n = 0)), expand.dots = FALSE, envir = base::parent.frame(n = 2L))) # warning: I have written n = 0 to avoid error when a safer function is inside another functions. In addition, arguments values retrieved are not evaluated by match.call, but this is solved with get() below
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
    error_text <- base::paste0(base::unlist(x = error_text, recursive = TRUE, use.names = TRUE), collapse = "", recycle0 = FALSE) # convert everything to string. if error_text is a string, changes nothing. If NULL or empty (even list) -> "" so no need to check for management of NULL or empty value
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
    # use this in the error_text of safer functions if present in your main code 
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
    # optional section: remove the code if none of your arguments has no default value
    no_def_args <- base::c(
        "data1"
    )
    tempo <- base::eval(expr = base::parse(text = base::paste0("base::c(base::missing(", base::paste0(no_def_args, collapse = "),base::missing(", recycle0 = FALSE), "))", collapse = NULL, recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if(base::any(tempo, na.rm = TRUE)){
        tempo_cat <- base::paste0(
            error_text_start, 
            "FOLLOWING ARGUMENT", 
            base::ifelse(test = base::sum(tempo, na.rm = TRUE) > 1, yes = "S HAVE", no = " HAS"), 
            " NO DEFAULT VALUE AND REQUIRE ONE:\n", 
            base::paste0(no_def_args[tempo], collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end arg with no default values

    ######## management of NULL arguments
    # before NA checking because is.na(NULL) return logical(0) and all(logical(0)) is TRUE (but secured with & base::length(x = x) > 0)
    tempo_arg <- base::c(
        "data1", 
        "legend_name1", 
        "low_color1",
        "mid_color1",
        "high_color1",
        # "limit1", # inactivated because can be NULL
        # "midpoint1", # inactivated because can be NULL
        # "data2", # inactivated because can be NULL
        "color2",
        "alpha2",
        "invert2",
        "text_size",
        "title",
        "title_text_size",
        "show_scale",
        "rotate",
        "return",
        "plot",
        # "add", # inactivated because can be NULL
        "warn_print"
        # "lib_path", # inactivated because can be NULL
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
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # before NA checking because is.na(logical()) is logical(0) (but secured with & base::length(x = x) > 0)
    tempo_arg <- base::c(
        # "data1", # inactivated because can be empty
        # "legend_name1", # inactivated because can be empty
        # "low_color1", # inactivated because can be empty
        # "mid_color1", # inactivated because can be empty
        # "high_color1", # inactivated because can be empty
        # "data2", # inactivated because can be empty
        # "color2", # inactivated because can be empty
        # "title", # inactivated because can be empty
        # "add", # inactivated because can be empty
        "alpha2",
        "invert2",
        "text_size",
        "title_text_size",
        "show_scale",
        "rotate",
        "return",
        "plot",
        "warn_print"
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
    if(safer_check == TRUE){ # this line must be inactivated if you want to use lib_path in the main code (other than in safer functions present in the main code) 
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
    }  # this line must be inactivated if you want to use lib_path in the main code (other than in safer functions present in the main code) 
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    if(safer_check == TRUE){
        saferDev:::.pack_and_function_check(
            fun = base::c(
                # functions required in this code
                "saferDev::arg_check", 
                "ggplot2::aes",
                "ggplot2::coord_fixed",
                "ggplot2::element_blank", 
                "ggplot2::element_text", 
                "ggplot2::geom_raster", 
                "ggplot2::ggplot", 
                "ggplot2::ggplot_build", 
                "ggplot2::ggtitle", 
                "ggplot2::scale_y_reverse", 
                "ggplot2::scale_fill_gradient2", 
                "ggplot2::scale_discrete_manual", 
                "ggplot2::theme", 
                "ggplot2::theme_classic",
                "ggplot2::waiver",
                "reshape2::melt",
                "saferTool::round2", 
                # end functions required in this code
                # internal functions required in this code
                "saferDev:::.base_op_check"
                # end internal functions required in this code
            ),
            lib_path = lib_path, # write NULL if your function does not have any lib_path argument
            error_text = embed_error_text
        )
    }
    ######## end check of the required functions from the required packages

    ######## escaping CRAN submission NOTE for internal functions

    # add here in the internal functions that are used in your main code (copy-paste the line above and replace .base_op_check by the name of the internal function
    # not mandatory if your function is not designed for submission to the CRAN

    ######## end escaping CRAN submission NOTE for internal functions

    ######## critical operator checking
    if(safer_check == TRUE){
        saferDev:::.base_op_check(
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
    if(base::all(base::is.matrix(x = data1), na.rm = FALSE)){
        tempo <- saferDev::arg_check(data = data1, class = base::c("matrix", "array"), typeof = NULL, mode = "numeric", length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }else if(base::all(base::is.data.frame(x = data1), na.rm = FALSE)){
        tempo <- saferDev::arg_check(data = data1, class = "data.frame", typeof = NULL, mode = NULL, length = 3, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
        if(tempo$problem == FALSE){
            # structure of reshape2::melt() data frame
            tempo <- saferDev::arg_check(data = data1[, 1], class = NULL, typeof = "integer", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = "COLUMN 1 OF data1 (reshape2::melt() DATA FRAME)", data_arg = FALSE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
            tempo <- saferDev::arg_check(data = data1[, 2], class = NULL, typeof = "integer", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = "COLUMN 2 OF data1 (reshape2::melt() DATA FRAME)", data_arg = FALSE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
            tempo <- saferDev::arg_check(data = data1[, 3], class = NULL, typeof = NULL, mode = "numeric", length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = "COLUMN 3 OF data1 (reshape2::melt() DATA FRAME)", data_arg = FALSE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
        }
    }else{
        tempo_check <- saferDev::arg_check(data = data1, class = base::c("matrix", "array"), typeof = NULL, mode = "numeric", length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        tempo_check <- saferDev::arg_check(data = data1, class = "data.frame", typeof = NULL, mode = NULL, length = 3, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        tempo_cat <- base::paste0(
            arg_check_error_text, 
            "THE data1 ARGUMENT MUST BE A NUMERIC MATRIX OR A DATA FRAME OUTPUT OF THE reshape2::melt() FUNCTION", 
            collapse = NULL, 
            recycle0 = FALSE
        )
        text_check <- base::c(text_check, tempo_cat)
        argum_check <- base::c(argum_check, TRUE)
    }
    tempo <- saferDev::arg_check(data = legend_name1, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = low_color1, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if(tempo$problem == FALSE & ! (base::all(low_color1 %in% grDevices::colors(distinct = FALSE), na.rm = FALSE) | base::grepl(pattern = "^#", x = low_color1, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE))){ # check that all strings of low_color1 start by #
        tempo_cat <- base::paste0(
            arg_check_error_text, 
            "low_color1 ARGUMENT MUST BE A HEXADECIMAL COLOR VECTOR STARTING BY # AND/OR COLOR NAMES GIVEN BY grDevices::colors()", 
            collapse = NULL, 
            recycle0 = FALSE
        )
        text_check <- base::c(text_check, tempo_cat)
        argum_check <- base::c(argum_check, TRUE)
    }
    if( ! base::is.null(x = mid_color1)){
        tempo <- saferDev::arg_check(data = mid_color1, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
        if(tempo$problem == FALSE & ! (base::all(mid_color1 %in% grDevices::colors(distinct = FALSE), na.rm = FALSE) | base::grepl(pattern = "^#", x = mid_color1, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE))){ # check that all strings of mid_color1 start by #
            tempo_cat <- base::paste0(
                arg_check_error_text, 
                "mid_color1 ARGUMENT MUST BE A HEXADECIMAL COLOR VECTOR STARTING BY # AND/OR COLOR NAMES GIVEN BY grDevices::colors()", 
                collapse = NULL, 
                recycle0 = FALSE
            )
            text_check <- base::c(text_check, tempo_cat)
            argum_check <- base::c(argum_check, TRUE)
        }
    }
    tempo <- saferDev::arg_check(data = high_color1, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if(tempo$problem == FALSE & ! (base::all(high_color1 %in% grDevices::colors(distinct = FALSE), na.rm = FALSE) | base::grepl(pattern = "^#", x = high_color1, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE))){ # check that all strings of high_color1 start by #
        tempo_cat <- base::paste0(
            arg_check_error_text, 
            "high_color1 ARGUMENT MUST BE A HEXADECIMAL COLOR VECTOR STARTING BY # AND/OR COLOR NAMES GIVEN BY grDevices::colors()", 
            collapse = NULL, 
            recycle0 = FALSE
        )
        text_check <- base::c(text_check, tempo_cat)
        argum_check <- base::c(argum_check, TRUE)
    }
    if( ! base::is.null(x = limit1)){
        tempo <- saferDev::arg_check(data = limit1, class = NULL, typeof = NULL, mode = "numeric", length = 2, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
        if(tempo$problem == FALSE & base::any(limit1 %in% base::c(Inf, -Inf), na.rm = FALSE)){
            # nocov start (unreachable: arg_check with inf_values = FALSE already rejects Inf in limit1, so the condition of the enclosing if is contradictory)
            tempo_cat <- base::paste0(
                arg_check_error_text, 
                "limit1 ARGUMENT CANNOT CONTAIN -Inf OR Inf VALUES", 
                collapse = NULL, 
                recycle0 = FALSE
            )
            text_check <- base::c(text_check, tempo_cat)
            argum_check <- base::c(argum_check, TRUE)
            # nocov end
        }
    }
    if( ! base::is.null(x = midpoint1)){
        tempo <- saferDev::arg_check(data = midpoint1, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    if( ! base::is.null(x = data2)){
        if(base::all(base::is.matrix(x = data2), na.rm = FALSE)){
            tempo <- saferDev::arg_check(data = data2, class = base::c("matrix", "array"), typeof = NULL, mode = "numeric", length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
            if(tempo$problem == FALSE & ! base::all(base::unique(x = data2, incomparables = FALSE) %in% base::c(0, 1), na.rm = FALSE)){
                tempo_cat <- base::paste0(
                    arg_check_error_text, 
                    "MATRIX IN data2 MUST BE MADE OF 0 AND 1 ONLY (MASK MATRIX)", 
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                text_check <- base::c(text_check, tempo_cat)
                argum_check <- base::c(argum_check, TRUE)
            }else if(tempo$problem == FALSE & base::all(base::is.matrix(x = data1), na.rm = FALSE) & ! base::identical(x = base::dim(x = data1), y = base::dim(x = data2), num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)){ # matrix and matrix
                tempo_cat <- base::paste0(
                    arg_check_error_text, 
                    "MATRIX DIMENSION IN data2 MUST BE IDENTICAL AS MATRIX DIMENSION IN data1. HERE IT IS RESPECTIVELY:\n", 
                    base::paste(base::dim(x = data2), sep = " ", collapse = " ", recycle0 = FALSE), 
                    "\n", 
                    base::paste(base::dim(x = data1), sep = " ", collapse = " ", recycle0 = FALSE), 
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                text_check <- base::c(text_check, tempo_cat)
                argum_check <- base::c(argum_check, TRUE)
            }else if(tempo$problem == FALSE & base::all(base::is.data.frame(x = data1), na.rm = FALSE) & base::nrow(x = data1) != base::prod(base::dim(x = data2), na.rm = FALSE)){ # reshape2 and matrix
                tempo_cat <- base::paste0(
                    arg_check_error_text, 
                    "DATA FRAME IN data2 MUST HAVE ROW NUMBER EQUAL TO PRODUCT OF DIMENSIONS OF data1 MATRIX. HERE IT IS RESPECTIVELY:\n", 
                    base::paste(base::nrow(x = data1), sep = " ", collapse = " ", recycle0 = FALSE), 
                    "\n", 
                    base::paste(base::prod(base::dim(x = data2), na.rm = FALSE), sep = " ", collapse = " ", recycle0 = FALSE), 
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                text_check <- base::c(text_check, tempo_cat)
                argum_check <- base::c(argum_check, TRUE)
            }
        }else if(base::all(base::is.data.frame(x = data2), na.rm = FALSE)){
            tempo <- saferDev::arg_check(data = data2, class = "data.frame", typeof = NULL, mode = NULL, length = 3, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
            if(tempo$problem == FALSE){
                # structure of reshape2::melt() data frame
                tempo <- saferDev::arg_check(data = data2[, 1], class = NULL, typeof = "integer", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = "COLUMN 1 OF data2 (reshape2::melt() DATA FRAME)", data_arg = FALSE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
                tempo <- saferDev::arg_check(data = data2[, 2], class = NULL, typeof = "integer", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = "COLUMN 2 OF data2 (reshape2::melt() DATA FRAME)", data_arg = FALSE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
                tempo <- saferDev::arg_check(data = data2[, 3], class = NULL, typeof = NULL, mode = "numeric", length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = "COLUMN 3 OF data2 (reshape2::melt() DATA FRAME)", data_arg = FALSE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
            }
            if(tempo$problem == FALSE & ! base::all(base::unique(x = data2[, 3], incomparables = FALSE) %in% base::c(0, 1), na.rm = FALSE)){
                tempo_cat <- base::paste0(
                    arg_check_error_text, 
                    "THIRD COLUMN OF DATA FRAME IN data2 MUST BE MADE OF 0 AND 1 ONLY (MASK DATA FRAME)", 
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                text_check <- base::c(text_check, tempo_cat)
                argum_check <- base::c(argum_check, TRUE)
            }else if(tempo$problem == FALSE & base::all(base::is.data.frame(x = data1), na.rm = FALSE) & ! base::identical(x = base::dim(x = data1), y = base::dim(x = data2), num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)){ # data frame and data frame
                tempo_cat <- base::paste0(
                    arg_check_error_text, 
                    "DATA FRAME DIMENSION IN data2 MUST BE IDENTICAL TO DATA FRAME DIMENSION IN data1. HERE IT IS RESPECTIVELY:\n", 
                    base::paste(base::dim(x = data2), sep = " ", collapse = " ", recycle0 = FALSE), 
                    "\n", 
                    base::paste(base::dim(x = data1), sep = " ", collapse = " ", recycle0 = FALSE), 
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                text_check <- base::c(text_check, tempo_cat)
                argum_check <- base::c(argum_check, TRUE)
            }else if(tempo$problem == FALSE & base::all(base::is.matrix(x = data1), na.rm = FALSE) & base::nrow(x = data2) != base::prod(base::dim(x = data1), na.rm = FALSE)){ # reshape2 and matrix
                tempo_cat <- base::paste0(
                    arg_check_error_text, 
                    "DATA FRAME IN data2 MUST HAVE ROW NUMBER EQUAL TO PRODUCT OF DIMENSION OF data1 MATRIX. HERE IT IS RESPECTIVELY:\n", 
                    base::paste(base::nrow(x = data2), sep = " ", collapse = " ", recycle0 = FALSE), 
                    "\n", 
                    base::paste(base::prod(base::dim(x = data1), na.rm = FALSE), sep = " ", collapse = " ", recycle0 = FALSE), 
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                text_check <- base::c(text_check, tempo_cat)
                argum_check <- base::c(argum_check, TRUE)
            }
        }else{
            tempo_check <- saferDev::arg_check(data = data2, class = base::c("matrix", "array"), typeof = NULL, mode = "numeric", length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
            tempo_check <- saferDev::arg_check(data = data2, class = "data.frame", typeof = NULL, mode = NULL, length = 3, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
            tempo_cat <- base::paste0(
                arg_check_error_text, 
                "THE data2 ARGUMENT MUST BE A NUMERIC MATRIX OR A DATA FRAME OUTPUT OF THE reshape2::melt() FUNCTION", 
                collapse = NULL, 
                recycle0 = FALSE
            )
            text_check <- base::c(text_check, tempo_cat)
            argum_check <- base::c(argum_check, TRUE)
        }
    }
    tempo <- saferDev::arg_check(data = color2, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if(tempo$problem == FALSE & ! (base::all(color2 %in% grDevices::colors(distinct = FALSE), na.rm = FALSE) | base::grepl(pattern = "^#", x = color2, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE))){ # check that all strings of color2 start by #
        tempo_cat <- base::paste0(
            arg_check_error_text, 
            "color2 ARGUMENT MUST BE A HEXADECIMAL COLOR VECTOR STARTING BY # AND/OR COLOR NAMES GIVEN BY grDevices::colors()", 
            collapse = NULL, 
            recycle0 = FALSE
        )
        text_check <- base::c(text_check, tempo_cat)
        argum_check <- base::c(argum_check, TRUE)
    }
    tempo <- saferDev::arg_check(data = alpha2, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = TRUE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = invert2, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = text_size, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = title, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = title_text_size, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = show_scale, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = return, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = plot, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(x = add)){
        tempo <- saferDev::arg_check(data = add, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
        if(tempo$problem == FALSE & ! base::grepl(pattern = "^\\+", x = add, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){ # check that the add string start by +
            tempo_cat <- base::paste0(
                arg_check_error_text, 
                "add ARGUMENT MUST START WITH \"+\": ", 
                base::paste(base::unique(x = add, incomparables = FALSE), sep = " ", collapse = " ", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            text_check <- base::c(text_check, tempo_cat)
            argum_check <- base::c(argum_check, TRUE)
        }else if(tempo$problem == FALSE & ! base::grepl(pattern = "ggplot2::", x = add, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){ #
            tempo_cat <- base::paste0(
                arg_check_error_text, 
                "add ARGUMENT MUST CONTAIN \"ggplot2::\" IN FRONT OF EACH GGPLOT2 FUNCTION: ", 
                base::paste(base::unique(x = add, incomparables = FALSE), sep = " ", collapse = " ", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            text_check <- base::c(text_check, tempo_cat)
            argum_check <- base::c(argum_check, TRUE)
        }else if(tempo$problem == FALSE & ! base::grepl(pattern = ")$", x = add, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){ # check that the add string  finished by )
            tempo_cat <- base::paste0(
                arg_check_error_text, 
                "add ARGUMENT MUST FINISH BY \")\": ", 
                base::paste(base::unique(x = add, incomparables = FALSE), sep = " ", collapse = " ", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            text_check <- base::c(text_check, tempo_cat)
            argum_check <- base::c(argum_check, TRUE)
        }
    }
    tempo <- saferDev::arg_check(data = warn_print, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    if( ! base::is.null(x = argum_check)){
        if(base::any(argum_check, na.rm = TRUE)){
            base::stop(base::paste0("\n\n================\n\n", base::paste0(text_check[argum_check], collapse = "\n\n", recycle0 = FALSE), "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    # check with r_debugging_tools
    # source("https://gitlab.pasteur.fr/gmillot/debugging_tools_for_r_dev/-/raw/v1.8/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using saferDev::arg_check()
    # end check with r_debugging_tools
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    # optional section: remove the code if you do not want to check if arguments of mode character of your own function cannot contain ""
    tempo_arg <- base::c(
        # "legend_name1", # inactivated because can be ""
        # "low_color1", # inactivated because checked above
        # "mid_color1", # inactivated because checked above
        # "high_color1", # inactivated because checked above
        # "color2", # inactivated because checked above
        # "title", # inactivated because can be ""
        # "add", # inactivated because checked above
        # "error_text" # inactivated because can be ""
    )
    ######## end management of "" in arguments of mode character

    #### end argument secondary checking

    #### second round of checking and data preparation

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
    # optional section: remove the code if no graphics used in your functions
    # check the number of graphic devices on exit
    dev_list <- grDevices::dev.list() 
    # This check is here in case the developer has not correctly fill tempo_arg
    # nocov start
    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
    base::on.exit(
        expr = if(base::length(x = dev_list) != base::length(x = grDevices::dev.list())){
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
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }, 
        add = TRUE, 
        after = TRUE
    )
    # nocov end
    # end check the number of graphic devices on exit
    # restore the graphic parameters on exit
    if(base::length(x = grDevices::dev.list()) > 0){
        par_ini <- base::suppressWarnings(expr = graphics::par(no.readonly = TRUE), classes = "warning") # to recover the present graphical parameters
        base::on.exit(expr = base::suppressWarnings(expr = graphics::par(par_ini, no.readonly = TRUE), classes = "warning"), add = TRUE, after = TRUE)
    }
    # end restore the graphic parameters on exit
    ######## end graphic device checking

    ######## other checkings
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    if(base::all(base::is.matrix(x = data1), na.rm = FALSE)){
        data1 <- reshape2::melt(data = data1, na.rm = FALSE, value.name = "value") # transform a matrix into a data frame with 2 coordinates columns and the third intensity column
    }
    if(rotate == TRUE){
        data1[, 1] <- base::rev(x = data1[, 1])
    }
    if(base::is.null(x = limit1)){
        if(base::any( ! base::is.finite(x = data1[, 3]), na.rm = FALSE)){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count, ") THE data1 ARGUMENT CONTAINS -Inf OR Inf VALUES IN THE THIRD COLUMN, THAT WILL NOT BE CONSIDERED IN THE PLOT RANGE", collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
        }
        limit1 <- base::range(data1[, 3], na.rm = TRUE, finite = TRUE) # finite = TRUE removes all the -Inf and Inf except if only this. In that case, whatever the -Inf and/or Inf present, output -Inf;Inf range. Idem with NA only
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count, ") THE limit1 ARGUMENT IS NULL -> RANGE OF data1 ARGUMENT HAS BEEN TAKEN: ", base::paste(saferTool::round2(data = limit1, dec_nb = 2, after_lead_zero = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text), sep = " ", collapse = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
        warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
        if(base::suppressWarnings(expr = base::any(limit1 %in% base::c(Inf, -Inf), na.rm = FALSE), classes = "warning")){
            # nocov start (unreachable: round2() at line 788 errors before this branch when the computed range is -Inf or Inf)
            tempo_cat <- base::paste0(
                "ERROR IN ", 
                function_name, 
                base::ifelse(test = error_text == "", yes = ".", no = error_text), 
                "\n\nCOMPUTED LIMIT CONTAINS Inf VALUES, BECAUSE VALUES FROM data1 ARGUMENTS ARE NA OR Inf ONLY", 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
            # nocov end
        }
    }else{
        limit1 <- base::sort(x = limit1, decreasing = FALSE)
    }
    if(base::is.null(x = midpoint1)){
        midpoint1 <- base::mean(x = limit1, na.rm = TRUE)
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count, ") THE midpoint1 ARGUMENT IS NULL -> MEAN OF limit1 ARGUMENT HAS BEEN TAKEN: ", base::paste(saferTool::round2(data = midpoint1, dec_nb = 2, after_lead_zero = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text), sep = " ", collapse = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
        warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
    }else if(saferTool::round2(data = midpoint1, dec_nb = 9, after_lead_zero = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) != saferTool::round2(data = base::mean(x = limit1, na.rm = TRUE), dec_nb = 9, after_lead_zero = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)){
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count, ") THE midpoint1 ARGUMENT (", saferTool::round2(data = base::mean(x = midpoint1, na.rm = TRUE), dec_nb = 9, after_lead_zero = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text), ") DOES NOT CORRESPOND TO THE MEAN OF THE limit1 ARGUMENT (", saferTool::round2(data = base::mean(x = limit1, na.rm = TRUE), dec_nb = 9, after_lead_zero = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text), "). COLOR SCALE IS NOT LINEAR", collapse = NULL, recycle0 = FALSE)
        warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
    }
    if( ! base::is.null(x = data2)){
        if(base::all(base::is.matrix(x = data2), na.rm = FALSE)){
            data2 <- reshape2::melt(data = data2, na.rm = FALSE, value.name = "value") # transform a matrix into a data frame with 2 coordinates columns and the third intensity column
        }
        if(rotate == TRUE){
            data2[, 1] <- base::rev(x = data2[, 1])
        }
        data2[, 3] <- base::factor(x = data2[, 3], exclude = NA, ordered = base::is.ordered(x = data2[, 3]), nmax = NA) # levels and labels not written: their defaults are computed from x inside base::factor(). # to converte continuous scale into discrete scale
    }
    tempo_gg_name <- "gg.indiv.plot."
    tempo_gg_count <- 0 # to facilitate debugging
    base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::ggplot(data = NULL, mapping = ggplot2::aes(), environment = base::environment(fun = NULL)), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
    base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_raster(mapping = ggplot2::aes(x = .data[[base::names(x = data1)[base::ifelse(test = rotate == FALSE, yes = 2, no = 1)]]], y = .data[[base::names(x = data1)[base::ifelse(test = rotate == FALSE, yes = 1, no = 2)]]], fill = .data[[base::names(x = data1)[3]]]), data = data1, stat = "identity", position = "identity", interpolate = FALSE, hjust = 0.5, vjust = 0.5, na.rm = FALSE, show.legend = show_scale, inherit.aes = TRUE), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # show.legend option do not remove the legend, only the aesthetic of the legend (dot, line, etc.)
    base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_fill_gradient2(name = legend_name1, limits = limit1, breaks = base::c(limit1[1], midpoint1, limit1[2]), labels = saferTool::round2(data = base::c(limit1[1], midpoint1, limit1[2]), dec_nb = 2, after_lead_zero = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text), low = low_color1, mid = mid_color1, high = high_color1, midpoint = midpoint1, space = "Lab", na.value = "grey50", transform = "identity", guide = "colourbar", aesthetics = "fill"), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
    if( ! base::is.null(x = data2)){
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_raster(mapping = ggplot2::aes(x = .data[[base::names(x = data2)[base::ifelse(test = rotate == FALSE, yes = 2, no = 1)]]], y = .data[[base::names(x = data2)[base::ifelse(test = rotate == FALSE, yes = 1, no = 2)]]], alpha = .data[[base::names(x = data2)[3]]]), data = data2, stat = "identity", position = "identity", fill = color2, interpolate = FALSE, hjust = 0.5, vjust = 0.5, na.rm = FALSE, show.legend = FALSE, inherit.aes = TRUE), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_discrete_manual(aesthetics = "alpha", values = if(invert2 == FALSE){base::c(0, alpha2)}else{base::c(alpha2, 0)}, breaks = ggplot2::waiver(), guide = "none"), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
        # assign(paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), ggplot2::geom_raster(data = data2, mapping = ggplot2::aes_string(x = names(data2)[ifelse(rotate == FALSE, 2, 1)], y = names(data2)[ifelse(rotate == FALSE, 1, 2)], group = names(data2)[3]), fill = data2[, 3], alpha = alpha2, show.legend = FALSE)) # BEWARE: this does not work if NA present, because geom_raster() has a tendency to complete empty spaces, and thus, behave differently than geom_tile(). See https://github.com/tidyverse/ggplot2/issues/3025
    }
    base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::coord_fixed(ratio = 1), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # x = y
    base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_y_reverse(), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
    base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::ggtitle(label = title, subtitle = ggplot2::waiver()), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
    add_check <- TRUE
    if( ! base::is.null(x = add)){ # if add is NULL, then = 0
        if(base::grepl(pattern = "ggplot2::theme", x = add, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) == TRUE){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count, ") \"ggplot2::theme\" STRING DETECTED IN THE add ARGUMENT -> INTERNAL GGPLOT2 THEME FUNCTIONS ggplot2::theme() AND ggplot2::theme_classic() HAVE BEEN INACTIVATED, TO BE USED BY THE USER", collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            add_check <- FALSE
        }
    }
    if(add_check == TRUE){
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::theme_classic(base_size = text_size, base_family = "", header_family = NULL, base_line_size = text_size / 22, base_rect_size = text_size / 22, ink = "black", paper = "white", accent = "#3366FF"), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::theme(
            text = ggplot2::element_text(family = NULL, face = NULL, colour = NULL, size = text_size, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL, color = NULL, margin = NULL, debug = NULL, inherit.blank = FALSE),
            plot.title = ggplot2::element_text(family = NULL, face = NULL, colour = NULL, size = title_text_size, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL, color = NULL, margin = NULL, debug = NULL, inherit.blank = FALSE), # stronger than text
            line = ggplot2::element_blank(), 
            axis.title = ggplot2::element_blank(), 
            axis.text = ggplot2::element_blank(), 
            axis.ticks = ggplot2::element_blank(), 
            panel.background = ggplot2::element_blank()
        ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
    }
    if(plot == TRUE){
        tempo_string <- base::paste(base::paste0(tempo_gg_name, 1:tempo_gg_count, collapse = NULL, recycle0 = FALSE), sep = " ", collapse = " + ", recycle0 = FALSE)
        if( ! base::is.null(x = add)){
            tempo_string <- base::paste(tempo_string, add, sep = " ", collapse = " ", recycle0 = FALSE)
        }
        base::print(x = base::eval(expr = base::parse(text = tempo_string, file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)))
    }else{
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count, ") PLOT NOT SHOWN AS REQUESTED", collapse = NULL, recycle0 = FALSE)
        warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
    }
    if(warn_print == TRUE & ! base::is.null(x = warn)){
        base::on.exit(expr = base::warning(base::paste0("FROM ", function_name, ":\n\n", warn, collapse = NULL, recycle0 = FALSE), call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, domain = NULL), add = TRUE, after = TRUE)
    }
    base::on.exit(expr = base::options(warning.length = ini_warning_length), add = TRUE, after = TRUE)
    if(return == TRUE){
        output <- ggplot2::ggplot_build(plot = base::eval(expr = base::parse(text = base::paste(base::paste0(tempo_gg_name, 1:tempo_gg_count, collapse = NULL, recycle0 = FALSE), sep = " ", collapse = " + ", recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)))
        output <- output$data
        base::names(x = output)[1] <- "heatmap"
        if( ! base::is.null(x = data2)){
            base::names(x = output)[2] <- "mask"
        }
        base::return(base::list(data = output, axes = output$layout$panel_params[[1]], scale = base::c(limit1[1], midpoint1, limit1[2]), warn = warn))
    }
    #end main code
    #### end main code

    #### warning output
    #### end warning output

    #### output
    base::return(base::invisible(x = NULL))
    #### end output
    #### end of function
}
