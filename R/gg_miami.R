#' @title gg_miami
#' @description
#' Plot two ggplot2 manhattan plots in mirror.
#' @param file single character string of the vcf.tsv file path. Example: "/mnt/c/Users/gmillot/Documents/Git_projects/fisher_for_vcf/dataset/other/fisher.tsv".
#' @param chr single character string of the human chromo infos file.tsv path. Example: chr_path = "/mnt/c/Users/gmillot/Documents/Git_projects/fisher_for_vcf/dataset/hg19_grch37p5_chr_size_cumul.txt".
#' @param top_y_column single character string of any of the quantitative column of the vcf.tsv file for the y-axis of the manhattan plot at the top of the miami plot. Can also be an added column through the tsv_extra_fields parameter.
#' @param bottom_y_column as the top_y_column parameter but for the bottom manhattan plot of the miami plot. NULL (default) generates a simple manhattan plot.
#' @param x_lim single character string of the x-axis limits. Either "whole" for the whole genome or a special character string to have the x-axis limited to the x_lim parameter. Write NULL to do not plot results. For special character strings,  write "chr1:11000-15000" for a single region, "chr1:11000-" for a single region non delimited on the right, "chr7:0-147000000,chr10:1000000-2000000" if two regions, "chr7" for a whole chromosome, "chr7,chr1" for two chromosomes.
#' @param vgrid single logical value. Display the chromosome separators in the miami plot? Example: vgrid = TRUE.
#' @param color_column single character string of one of the column name of the .tsv file (see bottom_y_column) in order to color the dots. Write NULL if not required (dots will be alternatively grey and blue, according to chromo order).
#' @param dot_border_color single color character string to color the border of the dots. Write NULL if not required.
#' @param y_lim1 2 numeric values indicating the range of the top y-axis. Order matters (for inverted axis). If NULL, the range of the values from top_y_column will be used.
#' @param y_lim2 2 numeric values indicating the range of the bottom y-axis. Order matters (for inverted axis). If NULL, the range of the values from bottom_y_column will be used.
#' @param reverse1 single logical value, y-axis coordinates flip for the top panel in the miami plot. Example: reverse1 = TRUE.
#' @param reverse2 single logical value, y-axis coordinates flip for the bottom panel in the miami plot. Example: reverse2 = TRUE.
#' @param y_threshold1 single numeric value for the y-axis threshold of the top panel in the miami plot, beyond which values are of interest. Example: y_threshold1 = 3. Write NULL for no particular threshold.
#' @param y_threshold2 single numeric value for the y-axis threshold of the bottom panel in the miami plot, beyond which values are of interest. Example: y_threshold2 = 3. Write NULL for no particular threshold. Not considered if bottom_y_column is NULL.
#' @param y_log1 single logical value TRUE or FALSE for the  y-axis log10 scale of the top panel in the miami plot. Example: y_log1 = TRUE.
#' @param y_log2 single logical value TRUE or FALSE for the  y-axis log10 scale of the bottom panel in the miami plot. Example: y_log2 = TRUE.
#' @param warn_print logical. Print warnings at the end of the execution? No print if no warning messages.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R packages are not installed in the default directories because of lack of admin rights. More precisely, \code{lib_path} is passed through the \code{new} argument of \code{.libPaths()} so that the new library paths are \code{unique(c(new, .Library.site, .Library))}. Warning: \code{.libPaths()} is restored to the initial paths, after function execution. Ignored if \code{NULL} (default): only the pathways specified by the current \code{.libPaths()} are used for package calling.
#' @param safer_check Single logical value. Perform the "safer" checks? If \code{TRUE}, checkings are performed before main code running (see the \href{https://github.com/safer-r}{safer-r project}): 1) required functions and related packages effectively present in local R libraries and 2) R classical operators (like \code{"<-"}) not overwritten by another package because of the R scope. Warning: must be set to \code{FALSE} if this function is used inside another "safer" function to avoid pointless multiple checkings.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: \code{error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."}. If \code{NULL}, converted into \code{""}. Of note, in \code{arg_check()}, \code{error_text} is also used at the end of the string returned when no problem is detected.
#' @returns NULL. Draws the miami plot (and writes a miami.png file in the current working directory, as done by the initial design of the function).
#' @examples
#' \dontrun{
#' gg_miami(file = "C:/Users/gmillot/Documents/Git_projects/fisher_for_vcf/dataset/other/fisher.tsv", chr = "C:/Users/gmillot/Documents/Git_projects/fisher_for_vcf/dataset/hg19_grch37p5_chr_size_cumul.txt", top_y_column = "NEG_LOG10_P_VALUE_CARRIER_MODEL", bottom_y_column = "AF")
#' }
#' @importFrom ggplot2 .data
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 annotation_custom
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggplotGrob
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 scale_color_gradient2
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 scale_fill_gradient2
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 waiver
#' @importFrom grid gpar
#' @importFrom grid linesGrob
#' @importFrom grid unit
#' @importFrom grid unit.pmax
#' @importFrom gridExtra grid.arrange
#' @importFrom grDevices dev.cur
#' @importFrom grDevices dev.off
#' @importFrom grDevices png
#' @importFrom scales math_format
#' @importFrom scales muted
#' @importFrom scales rescale_none
#' @importFrom scales trans_breaks
#' @importFrom scales trans_format
#' @importFrom utils read.table
#' @importFrom saferDev arg_check
#' @export
gg_miami <- function(
        file, 
        chr, 
        top_y_column, 
        bottom_y_column = NULL, 
        x_lim = "whole", 
        vgrid = FALSE, 
        color_column = NULL, 
        dot_border_color = NULL, 
        y_lim1 = NULL, 
        y_lim2 = NULL, 
        reverse1 = FALSE, 
        reverse2 = FALSE, 
        y_threshold1 = NULL, 
        y_threshold2 = NULL, 
        y_log1 = FALSE, 
        y_log2 = FALSE, 
        warn_print = FALSE, 
        lib_path = NULL, 
        safer_check = TRUE, 
        error_text = ""
){
    # DEBUGGING
    # file = "C:/Users/gmillot/Documents/Git_projects/fisher_for_vcf/dataset/other/fisher.tsv" ; chr = "C:/Users/gmillot/Documents/Git_projects/fisher_for_vcf/dataset/hg19_grch37p5_chr_size_cumul.txt" ; top_y_column = "NEG_LOG10_P_VALUE_CARRIER_MODEL" ; bottom_y_column = "AF" ; x_lim = "whole" ; vgrid = FALSE ; color_column = NULL ; dot_border_color = NULL ; y_lim1 = NULL ; y_lim2 = NULL ; reverse1 = FALSE ; reverse2 = FALSE ; y_threshold1 = NULL ; y_threshold2 = NULL ; y_log1 = FALSE ; y_log2 = FALSE ; warn_print = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = "" ; 

    #### package name
    package_name <- "saferGG" # write NULL if the function developed is not in a package
    #### end package name

    #### internal error report link
    internal_error_report_link <- base::paste0("https://github.com/safer-r/", package_name, "/issues/new", collapse = NULL, recycle0 = FALSE) # link where to post an issue indicated in an internal error message. Write NULL if no link to propose, or no internal error message
    #### end internal error report link

    #### function name
    tempo_settings <- base::as.list(x = base::match.call(definition = base::sys.function(which = base::sys.parent(n = 0)), call = base::sys.call(which = base::sys.parent(n = 0)), expand.dots = FALSE, envir = base::parent.frame(n = 2L))) # warning: I have written n = 0 to avoid error when a safer function is inside another functions. In addition, arguments values retrieved are not evaluated base::match.call, but this is solved with get() below
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
        "file", 
        "chr", 
        "top_y_column"
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
        "file", 
        "chr", 
        "top_y_column", 
        # "bottom_y_column", # inactivated because can be NULL
        "x_lim", 
        "vgrid", 
        # "color_column", # inactivated because can be NULL
        # "dot_border_color", # inactivated because can be NULL
        # "y_lim1", # inactivated because can be NULL
        # "y_lim2", # inactivated because can be NULL
        "reverse1", 
        "reverse2", 
        # "y_threshold1", # inactivated because can be NULL
        # "y_threshold2", # inactivated because can be NULL
        "y_log1", 
        "y_log2", 
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
        "file", 
        "chr", 
        "top_y_column", 
        "x_lim", 
        "vgrid", 
        "reverse1", 
        "reverse2", 
        "y_log1", 
        "y_log2", 
        "warn_print"
        # "bottom_y_column", # inactivated because can be NULL (checked in the main code)
        # "color_column", # inactivated because can be NULL (checked in the main code)
        # "dot_border_color", # inactivated because can be NULL (checked in the main code)
        # "y_lim1", # inactivated because can be NULL
        # "y_lim2", # inactivated because can be NULL
        # "y_threshold1", # inactivated because can be NULL
        # "y_threshold2", # inactivated because can be NULL
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
                # functions required in this code
                "ggplot2::aes",
                "ggplot2::annotation_custom", 
                "ggplot2::element_blank", 
                "ggplot2::element_line", 
                "ggplot2::element_text", 
                "ggplot2::geom_hline", 
                "ggplot2::geom_point", 
                "ggplot2::geom_vline", 
                "ggplot2::ggplot", 
                "ggplot2::ggplotGrob", 
                "ggplot2::ggtitle", 
                "ggplot2::scale_color_gradient2", 
                "ggplot2::scale_color_manual", 
                "ggplot2::scale_fill_gradient2", 
                "ggplot2::scale_fill_manual", 
                "ggplot2::scale_x_continuous", 
                "ggplot2::scale_y_continuous", 
                "ggplot2::theme", 
                "ggplot2::theme_bw",
                "ggplot2::waiver",
                "gridExtra::grid.arrange",
                "scales::math_format",
                "scales::muted",
                "scales::rescale_none", 
                "scales::trans_breaks", 
                "scales::trans_format",
                "saferDev::arg_check", 
                "saferGG::gg_empty"
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
    tempo <- saferDev::arg_check(data = file, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = chr, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = top_y_column, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(x = bottom_y_column)){
        tempo <- saferDev::arg_check(data = bottom_y_column, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    if( ! base::is.null(x = x_lim)){
        tempo <- saferDev::arg_check(data = x_lim, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    tempo <- saferDev::arg_check(data = vgrid, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(x = color_column)){
        tempo <- saferDev::arg_check(data = color_column, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    if( ! base::is.null(x = dot_border_color)){
        tempo <- saferDev::arg_check(data = dot_border_color, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    if( ! base::is.null(x = y_lim1)){
        tempo <- saferDev::arg_check(data = y_lim1, class = NULL, typeof = NULL, mode = "numeric", length = 2, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    if( ! base::is.null(x = y_lim2)){
        tempo <- saferDev::arg_check(data = y_lim2, class = NULL, typeof = NULL, mode = "numeric", length = 2, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    tempo <- saferDev::arg_check(data = reverse1, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = reverse2, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(x = y_threshold1)){
        tempo <- saferDev::arg_check(data = y_threshold1, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    if( ! base::is.null(x = y_threshold2)){
        tempo <- saferDev::arg_check(data = y_threshold2, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    tempo <- saferDev::arg_check(data = y_log1, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = y_log2, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
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
        "file", 
        "chr", 
        "top_y_column", 
        "bottom_y_column", 
        "x_lim", 
        "color_column", 
        "dot_border_color"
        # "vgrid", # inactivated because logical
        # "reverse1", # inactivated because logical
        # "reverse2", # inactivated because logical
        # "y_log1", # inactivated because logical
        # "y_log2", # inactivated because logical
        # "lib_path" # inactivated because already checked above
        # "error_text" # inactivated because can be ""
    )
    # nocov start
    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
    tempo_log <- ! base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){if(base::is.null(x = x)){base::return(TRUE)}else{base::all(base::mode(x = x) == "character", na.rm = TRUE)}}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because get() is evaluated inside the FUN closure of the inner lapply: parent.frame(n = 1) is lapply's frame, parent.frame(n = 2) is BACKBONE's frame.
    if(base::any(tempo_log, na.rm = TRUE)){
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
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }else{
        tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::any(x == "", na.rm = TRUE)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because get() is evaluated inside the FUN closure of the inner lapply: parent.frame(n = 1) is lapply's frame, parent.frame(n = 2) is BACKBONE's frame. # for character argument that can also be NULL, if NULL -> returns FALSE. Thus no need to test is.null()
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
    # nocov end
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
    # BEWARE: unlike most of the safer functions, this function opens a graphic device (grDevices::png()) in the main code. Thus, instead of checking the number of devices on exit, the graphic devices opened during the execution are closed on exit (in case of error after the device opening)
    # nocov start
    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
    dev_list <- grDevices::dev.list() 
    base::on.exit(
        expr = while(base::length(x = grDevices::dev.list()) > base::length(x = dev_list) & grDevices::dev.cur() != 1){
            grDevices::dev.off(which = grDevices::dev.cur()) # close the devices opened by the function, in case of error after the device opening
        }, 
        add = TRUE, 
        after = TRUE
    )
    # end close the devices opened by the function
    if(base::length(x = grDevices::dev.list()) > 0){
        par_ini <- base::suppressWarnings(expr = graphics::par(no.readonly = TRUE), classes = "warning") # to recover the present graphical parameters
        base::on.exit(expr = base::suppressWarnings(expr = graphics::par(par_ini, no.readonly = TRUE), classes = "warning"), add = TRUE, after = TRUE)
    }
    # end restore the graphic parameters on exit
    # nocov end
    ######## end graphic device checking

    ######## other checkings
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    # data import
    if( ! base::file.exists(file)){
        tempo_cat <- base::paste0(
            error_text_start, 
            "FILE INDICATED IN THE file ARGUMENT DOES NOT EXISTS: ", 
            file, 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }else{
        obs <- utils::read.table(file = file, header = TRUE, sep = "\t", quote = "\"'", dec = ".", numerals = "allow.loss", row.names = , col.names = , as.is = TRUE, tryLogical = TRUE, na.strings = "NA", colClasses = NA, nrows = -1, skip = 0, check.names = TRUE, fill = FALSE, strip.white = FALSE, blank.lines.skip = TRUE, comment.char = "", allowEscapes = FALSE, flush = FALSE, stringsAsFactors = FALSE, fileEncoding = "", encoding = "unknown", text = , skipNul = FALSE)
        if(base::length(x = obs) > 0 & base::nrow(x = obs) > 0){
            empty_obs <- FALSE
        }else{
            empty_obs <- TRUE
        }
    }
    if( ! base::file.exists(chr)){
        tempo_cat <- base::paste0(
            error_text_start, 
            "FILE INDICATED IN THE chr ARGUMENT DOES NOT EXISTS: ", 
            chr, 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }else{
        chr <- utils::read.table(file = chr, header = TRUE, sep = "\t", quote = "\"'", dec = ".", numerals = "allow.loss", row.names = , col.names = , as.is = TRUE, tryLogical = TRUE, na.strings = "NA", colClasses = NA, nrows = -1, skip = 0, check.names = TRUE, fill = FALSE, strip.white = FALSE, blank.lines.skip = TRUE, comment.char = "", allowEscapes = FALSE, flush = FALSE, stringsAsFactors = FALSE, fileEncoding = "", encoding = "unknown", text = , skipNul = FALSE)
    }
    # end data import
    # modifications of imported tables
    is_whole <- FALSE
    xmin_plot <- 0 # coordinates for plotting 
    if(base::length(x = obs) > 0 & base::nrow(x = obs) > 0){
        if(base::any(base::grepl(pattern = "chr", x = obs$CHROM, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), na.rm = FALSE)){
            obs$CHROM <- base::sub(pattern = "chr", replacement = "", x = obs$CHROM, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
        }
        if(base::any(base::grepl(pattern = "^X$", x = obs$CHROM, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), na.rm = FALSE)){
            obs$CHROM[base::grepl(pattern = "^X$", x = obs$CHROM, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)] <- "23"
        }
        if(base::any(base::grepl(pattern = "^Y$", x = obs$CHROM, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), na.rm = FALSE)){
            obs$CHROM[base::grepl(pattern = "^Y$", x = obs$CHROM, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)] <- "24"
        }
        if(base::any(base::grepl(pattern = "^MT$|^M$", x = obs$CHROM, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), na.rm = FALSE)){
            obs$CHROM[base::grepl(pattern = "^MT$|^M$", x = obs$CHROM, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)] <- "25"
        }
        if(base::any( ! base::grepl(pattern = "\\d", x = obs$CHROM, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), na.rm = FALSE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                "THE chr COLUMN OF THE .tsv FILE HAS LETTERS IN IT, OTHER THAN X, Y and MT:\n", 
                base::paste0(obs$CHROM[base::grepl(pattern = "^\\d", x = obs$CHROM, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)], collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            obs$CHROM <- base::as.integer(x = obs$CHROM)
        }
        # add the chr info to obs
        obs <- base::data.frame(obs, coord = base::vector(mode = "numeric", length = base::nrow(x = obs)), row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = TRUE)
        for(i0 in base::seq_along(along.with = chr$CHR_NB)){
            i1 <- chr$CHR_NB[i0]
            obs$coord[obs$CHROM == i1] <- obs$POS[obs$CHROM == i1] + chr$LENGTH_CUMUL_TO_ADD[i1]
        }
        # preparation of the x coordinates: three solutions: 1) whole object (see above), 2) single chromo "chr7" or "chr7:0-15", 3) several chromo chr7, chr8" or "chr7:0-15, chr8" or "chr7:0-15, chr8:0-20"
        # The idea is to select rows of chr and potentially restrict some chr limits
        if( ! base::is.null(x = x_lim)){
            if(x_lim == "whole"){ #at that stage, x_lim is a single character
                is_whole <- TRUE
            }else{
                tempo1 <- base::strsplit(x = x_lim, split = ",", fixed = FALSE, perl = FALSE, useBytes = FALSE)[[1]]
                tempo1 <- base::gsub(pattern = " ", replacement = "", x = tempo1, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
                chr_x_lim <- tempo1
                coord_x_lim <- NULL
                xmin_x_lim <- NULL
                xmax_x_lim <- NULL
                if( ! base::all(base::grepl(pattern = "^chr.+", x = tempo1, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), na.rm = FALSE)){
                    tempo_cat <- base::paste0(
                        error_text_start, 
                        "THE x_lim ARGUMENT MUST START WITH \"chr\" IF NOT \"whole\":\n", 
                        base::paste0(x_lim, collapse = " ", recycle0 = FALSE), 
                        collapse = NULL, 
                        recycle0 = FALSE
                    )
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                }
                if(base::any(base::grepl(pattern = ":", x = tempo1, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), na.rm = FALSE)){
                    # means that there are coordinates
                    if( ! base::all(base::grepl(pattern = "-", x = tempo1, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), na.rm = FALSE)){ # normally no NA with is.null()
                        tempo_cat <- base::paste0(
                            error_text_start, 
                            "THE x_lim ARGUMENT MUST BE WRITTEN LIKE THIS \"chr7:0-147000000, chr10:1000000-2000000\" IF COORDINATES ARE SPECIFIED: \n", 
                            base::paste0(x_lim, collapse = " ", recycle0 = FALSE), 
                            collapse = NULL, 
                            recycle0 = FALSE
                        )
                        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                    }
                    tempo2 <- base::strsplit(x = tempo1, split = ":", fixed = FALSE, perl = FALSE, useBytes = FALSE)
                    chr_x_lim <- base::sapply(X = tempo2, FUN = function(x){x[1]}, simplify = TRUE, USE.NAMES = TRUE)
                    chr_x_lim <- base::gsub(pattern = " ", replacement = "", x = chr_x_lim, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
                    coord_x_lim <- base::sapply(X = tempo2, FUN = function(x){x[2]}, simplify = TRUE, USE.NAMES = TRUE)
                    tempo3 <- base::strsplit(x = coord_x_lim, split = "-", fixed = FALSE, perl = FALSE, useBytes = FALSE)
                    xmin_x_lim <- base::sapply(X = tempo3, FUN = function(x){x[1]}, simplify = TRUE, USE.NAMES = TRUE)
                    xmin_x_lim <- base::gsub(pattern = " ", replacement = "", x = xmin_x_lim, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
                    xmax_x_lim <- base::sapply(X = tempo3, FUN = function(x){x[2]}, simplify = TRUE, USE.NAMES = TRUE)
                    xmax_x_lim <- base::gsub(pattern = " ", replacement = "", x = xmax_x_lim, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
                    if(base::any(base::grepl(pattern = "\\D", x = xmin_x_lim, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), na.rm = FALSE) | base::any(base::grepl(pattern = "\\D", x = xmax_x_lim, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), na.rm = FALSE)){ # normally no NA with is.null()
                        tempo_cat <- base::paste0(
                            error_text_start, 
                            "THE x_lim ARGUMENT MUST BE WRITTEN LIKE THIS \"chr7:0-147000000, chr10:1000000-2000000\" IF COORDINATES ARE SPECIFIED: \n", 
                            base::paste0(x_lim, collapse = " ", recycle0 = FALSE), 
                            collapse = NULL, 
                            recycle0 = FALSE
                        )
                        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                    }else{
                        xmin_x_lim <- base::as.integer(x = xmin_x_lim)
                        xmax_x_lim <- base::as.integer(x = xmax_x_lim)
                        if(base::any(xmax_x_lim - xmin_x_lim < 0, na.rm = FALSE)){
                            tempo_cat <- base::paste0(
                                error_text_start, 
                                "THE x_lim ARGUMENT MUST BE WRITTEN WITH ORDERED COORDINATES, LIKE THIS \"chr7:0-147000000, chr10:1000000-2000000\", IF COORDINATES ARE SPECIFIED: \n", 
                                base::paste0(x_lim, collapse = " ", recycle0 = FALSE), 
                                collapse = NULL, 
                                recycle0 = FALSE
                            )
                            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                        }
                    }
                }
                # modification of the chr object for restricted plotting
                tempo_coord <- base::which(x = chr$CHR %in% chr_x_lim, arr.ind = FALSE, useNames = TRUE) # which rows of chr to take for plotting
                if(base::length(x = tempo_coord) == 0){
                    tempo_cat <- base::paste0(
                        error_text_start, 
                        "THE x_lim ARGUMENT CONTAINS CHROMO NAMES THAT ARE NOT FOUND IN THE chr ARGUMENT FILE: ", 
                        base::paste0(chr_x_lim, collapse = " ", recycle0 = FALSE), 
                        collapse = NULL, 
                        recycle0 = FALSE
                    )
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                }
                if(base::any(chr$BP_LENGTH[tempo_coord] - xmax_x_lim < 0, na.rm = FALSE)){
                    tempo_cat <- base::paste0(
                        error_text_start, 
                        "THE x_lim ARGUMENT HAS AT LEAST ONE COORDINATE THAT IS ABOVE THE MAX LENGTH OF THE CHROMO.\nCHROMO LENGTH: ", 
                        base::paste0(chr$BP_LENGTH[tempo_coord], collapse = " ", recycle0 = FALSE), 
                        "\nMAX COORDINATE: ", 
                        base::paste0(xmax_x_lim, collapse = " ", recycle0 = FALSE), 
                        collapse = NULL, 
                        recycle0 = FALSE
                    )
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                }
                if(tempo_coord[1] > 1){
                    xmin_plot <- chr$LENGTH_CUMUL[tempo_coord[1] - 1]
                }
                chr <- chr[tempo_coord[1]:tempo_coord[base::length(x = tempo_coord)], ]
                if( ! base::is.null(x = coord_x_lim)){
                    xmin_plot <- xmin_plot + xmin_x_lim[1] # the left boundary of the plot is corrected
                    chr$LENGTH_CUMUL[base::nrow(x = chr)] <- chr$LENGTH_CUMUL[base::nrow(x = chr)] - chr$BP_LENGTH[base::nrow(x = chr)] + xmax_x_lim[base::length(x = xmax_x_lim)] # the right boundary of the plot is corrected
                    chr$CHR_NAME_POS <- (base::c(xmin_plot, chr$LENGTH_CUMUL[-base::nrow(x = chr)]) + chr$LENGTH_CUMUL) / 2 # the positions of names in the x-axis of the plot are corrected
                }
                # restriction of obs
                obs <- obs[obs$coord >= xmin_plot & obs$coord <= chr$LENGTH_CUMUL[base::nrow(x = chr)], ]
            }
        }else{
            # nocov start (unreachable: x_lim = NULL is rejected by arg_check, so this warn branch never runs)
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") x_lim IS NULL: NO PLOT DRAWN", collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            # nocov end
        }
    }
    if( ! top_y_column %in% base::names(x = obs)){
        tempo_cat <- base::paste0(
            error_text_start, 
            "THE top_y_column ARGUMENT MUST BE A COLUMN NAME OF THE TSV FILE.\n\ntop_y_column ARGUMENT:\n",
            base::paste0(top_y_column, collapse = " ", recycle0 = FALSE),
            "\n\nCOLUMN NAMES:\n",
            base::paste0(base::names(x = obs), collapse = "\n", recycle0 = FALSE),
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    if(base::is.null(x = bottom_y_column)){
        # bottom_y_column NULL: no check needed here
    }else if( ! bottom_y_column %in% base::names(x = obs)){
        tempo_cat <- base::paste0(
            error_text_start, 
            "THE bottom_y_column ARGUMENT MUST BE A COLUMN NAME OF THE TSV FILE.\n\nbottom_y_column ARGUMENT:\n",
            base::paste0(bottom_y_column, collapse = " ", recycle0 = FALSE),
            "\n\nCOLUMN NAMES:\n",
            base::paste0(base::names(x = obs), collapse = "\n", recycle0 = FALSE),
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    if(base::is.null(x = color_column)){
        # color_column NULL: no check needed here
    }else if( ! color_column %in% base::names(x = obs)){
        tempo_cat <- base::paste0(
            error_text_start, 
            "THE color_column ARGUMENT MUST BE A COLUMN NAME OF THE TSV FILE.\n\ncolor_column ARGUMENT:\n",
            base::paste0(color_column, collapse = " ", recycle0 = FALSE),
            "\n\nCOLUMN NAMES:\n",
            base::paste0(base::names(x = obs), collapse = "\n", recycle0 = FALSE),
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    # end modifications of imported tables

    # plotting
    # fun_open(width = 12, height = 4, pdf.name = base::paste0("plot_read_length_", kind)) # must be systematically opened for main.nf
    png_size <- 1800 # px
    grDevices::png(filename = "miami.png", width = png_size * 2, height = png_size, units = "px", pointsize = 12, bg = "white", res = 300, family = "sans", symbolfamily = "default") # restoreConsole (Windows only), type and antialias (default values depending on the operating system) not written: the arguments of grDevices::png() differ between Windows and the other systems
    if(empty_obs == TRUE){
        saferGG::gg_empty(text = "NO PLOT DRAWN\nTHE region PARAMETER\nMIGHT BE OUTSIDE\nOF THE RANGE OF THE VCF FILE", text_size = 12, title = NULL, title_size = 8, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text)
    }else if(base::length(x = obs) > 0 & base::nrow(x = obs) > 0 & ! base::is.null(x = x_lim)){
        marging <- (chr$LENGTH_CUMUL[base::nrow(x = chr)] - xmin_plot) * 0.005 # chr$LENGTH_CUMUL and xmin_plot have been corrected depending on x_lim boundaries
        y_min_pos <- base::ifelse(test = base::is.null(x = y_lim1), yes = base::min(obs[ , top_y_column], na.rm = TRUE), no = base::min(y_lim1, na.rm = FALSE))
        y_max_pos <- base::ifelse(test = base::is.null(x = y_lim1), yes = base::max(obs[ , top_y_column], na.rm = TRUE), no = base::max(y_lim1, na.rm = FALSE))
        tempo_gg_name <- "gg.indiv.plot."
        tempo_gg_count <- 0
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::ggplot(data = obs, mapping = ggplot2::aes(x = .data[["coord"]], y = .data[[top_y_column]]), environment = base::environment(fun = NULL)), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
        if(vgrid == TRUE){
            base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_vline(
                mapping = NULL, 
                data = NULL, 
                stat = "identity", 
                position = "identity", 
                na.rm = FALSE, 
                show.legend = NA, 
                inherit.aes = FALSE, 
                xintercept = base::c(xmin_plot, chr$LENGTH_CUMUL), 
                linewidth = 0.25, 
                color = "grey80"
            ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
        }
        if( ! base::is.null(x = y_threshold1)){
            base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_hline(
                mapping = NULL, 
                data = NULL, 
                stat = "identity", 
                position = "identity", 
                na.rm = FALSE, 
                show.legend = NA, 
                inherit.aes = FALSE, 
                yintercept = y_threshold1, 
                linetype = "22", 
                linewidth = 0.25, 
                color = "red"
            ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
        }
        if(base::is.null(x = color_column)){
            if(base::is.null(x = dot_border_color)){
                base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_point(
                    data = NULL, 
                    stat = "identity", 
                    position = "identity", 
                    na.rm = FALSE, 
                    show.legend = NA, 
                    inherit.aes = TRUE, 
                    mapping = ggplot2::aes(color = base::as.factor(x = .data[["CHROM"]]), x = , y = ), 
                    alpha = 1, 
                    pch = 16, 
                    size = 1
                ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
                base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_color_manual(values = base::rep(x = base::c("grey20", "skyblue"), times = 25), aesthetics = "colour", breaks = ggplot2::waiver(), na.value = "grey50"), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
            }else{
                base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_point(
                    data = NULL, 
                    stat = "identity", 
                    position = "identity", 
                    na.rm = FALSE, 
                    show.legend = NA, 
                    inherit.aes = TRUE, 
                    mapping = ggplot2::aes(fill = base::as.factor(x = .data[["CHROM"]]), x = , y = ), 
                    alpha = 1, 
                    color = dot_border_color, 
                    pch = 21, 
                    size = 1
                ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
                base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_fill_manual(values = base::rep(x = base::c("grey20", "skyblue"), times = 25), aesthetics = "fill", breaks = ggplot2::waiver(), na.value = "grey50"), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
            }
        }else{
            if(base::is.null(x = dot_border_color)){
                base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_point(
                    data = NULL, 
                    stat = "identity", 
                    position = "identity", 
                    na.rm = FALSE, 
                    show.legend = NA, 
                    inherit.aes = TRUE, 
                    mapping = ggplot2::aes(color = .data[[color_column]], x = , y = ), 
                    alpha = 1, 
                    pch = 16, 
                    size = 1
                ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
                base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_color_gradient2(name = ggplot2::waiver(), low = scales::muted(colour = "red", l = 30, c = 70), mid = "white", high = scales::muted(colour = "blue", l = 30, c = 70), midpoint = 0, space = "Lab", na.value = "grey50", transform = "identity", guide = "colourbar", aesthetics = "colour"), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
            }else{
                base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_point(
                    data = NULL, 
                    stat = "identity", 
                    position = "identity", 
                    na.rm = FALSE, 
                    show.legend = NA, 
                    inherit.aes = TRUE, 
                    mapping = ggplot2::aes(fill = .data[[color_column]], x = , y = ), 
                    alpha = 1, 
                    color = dot_border_color, 
                    pch = 21, 
                    size = 1
                ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
                base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_fill_gradient2(name = ggplot2::waiver(), low = scales::muted(colour = "red", l = 30, c = 70), mid = "white", high = scales::muted(colour = "blue", l = 30, c = 70), midpoint = 0, space = "Lab", na.value = "grey50", transform = "identity", guide = "colourbar", aesthetics = "fill"), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
            }
        }
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::ggtitle(
            label = base::paste0(
                "x_lim: ", 
                base::ifelse(test = is_whole, yes = "whole genome", no = x_lim), 
                base::ifelse(test = ! base::is.null(x = y_threshold1), yes = base::paste0(", top threshold: ", y_threshold1, collapse = NULL, recycle0 = FALSE), no = ""), 
                base::ifelse(test = ! (base::is.null(x = y_threshold2) & base::is.null(x = bottom_y_column)), yes = base::paste0(", bottom threshold: ", y_threshold2, collapse = NULL, recycle0 = FALSE), no = ""), 
                base::ifelse(test = y_log1 == TRUE, yes = ", top y-axis: log10", no = ""), 
                base::ifelse(test = y_log2 == TRUE, yes = ", bottom y-axis: log10", no = ""), 
                collapse = NULL, 
                recycle0 = FALSE
            ), 
            subtitle = ggplot2::waiver()
        ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_x_continuous(
            minor_breaks = ggplot2::waiver(), 
            n.breaks = NULL, 
            na.value = NA_real_, 
            transform = "identity", 
            guide = ggplot2::waiver(), 
            position = "bottom", 
            sec.axis = ggplot2::waiver(), 
            name = "CHR", 
            expand = base::c(0, 0), # remove space after after axis limits
            oob = scales::rescale_none, 
            labels = chr$CHR_NAME, 
            breaks = chr$CHR_NAME_POS, 
            # base::quote(expr = ): value returned by lifecycle::deprecated(), the default value of this deprecated argument (written without the lifecycle package, not imported by saferGG)
            limits = base::c(xmin_plot - marging, base::max(chr$LENGTH_CUMUL, na.rm = FALSE) + marging), trans = base::quote(expr = )
        ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
        if(y_log1 == TRUE){
            if(base::any(obs[ , top_y_column] <= 0, na.rm = FALSE)){
                tempo_cat <- base::paste0(
                    error_text_start, 
                    "THE y_log1 ARGUMENT CANNOT BE SET TO TRUE IF 0 OR NEG VALUES IN THE ",
                    top_y_column,
                    " FIELD OF THE TSV OR VCF",
                    collapse = NULL,
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_y_continuous(
                    name = ggplot2::waiver(), 
                    minor_breaks = ggplot2::waiver(), 
                    n.breaks = NULL, 
                    na.value = NA_real_, 
                    guide = ggplot2::waiver(), 
                    position = "left", 
                    sec.axis = ggplot2::waiver(), 
                    expand = base::c(0, 0), # remove space after after axis limits
                    limits = if(reverse1 == TRUE){base::c(y_max_pos, y_min_pos)}else{base::c(y_min_pos, y_max_pos)}, # NA indicate that limits must correspond to data limits but ylim() already used
                    oob = scales::rescale_none, 
                    transform = "log10", 
                    breaks = scales::trans_breaks(trans = "log10", inv = function(x){10^x}, n = 5), 
                    # base::quote(expr = ): value returned by lifecycle::deprecated(), the default value of this deprecated argument (written without the lifecycle package, not imported by saferGG)
                    labels = scales::trans_format(trans = "log10", format = scales::math_format(expr = 10^.x, format = base::force)), trans = base::quote(expr = )
                ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
                # assign(paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), ggplot2::annotation_logticks(outside = TRUE))
            }
        }else{
            base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_y_continuous(
                name = ggplot2::waiver(), 
                breaks = ggplot2::waiver(), 
                minor_breaks = ggplot2::waiver(), 
                n.breaks = NULL, 
                labels = ggplot2::waiver(), 
                na.value = NA_real_, 
                guide = ggplot2::waiver(), 
                position = "left", 
                sec.axis = ggplot2::waiver(), 
                expand = base::c(0, 0), # remove space after after axis limits
                limits = if(reverse1 == TRUE){base::c(y_max_pos, y_min_pos)}else{base::c(y_min_pos, y_max_pos)}, # NA indicate that limits must correspond to data limits but ylim() already used
                oob = scales::rescale_none, 
                # base::quote(expr = ): value returned by lifecycle::deprecated(), the default value of this deprecated argument (written without the lifecycle package, not imported by saferGG)
                transform = "identity", trans = base::quote(expr = )
            ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
        }
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::theme_bw(base_size = 11, base_family = "", header_family = NULL, base_line_size = 11 / 22, base_rect_size = 11 / 22, ink = "black", paper = "white", accent = "#3366FF"), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::theme(
            plot.title = ggplot2::element_text(family = NULL, face = NULL, colour = NULL, size = 8, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL, color = NULL, margin = NULL, debug = NULL, inherit.blank = FALSE), 
            legend.position = if(base::is.null(x = color_column)){"none"}else{"right"}, 
            panel.border = ggplot2::element_blank(), 
            panel.grid = ggplot2::element_blank(), 
            axis.ticks.x = ggplot2::element_blank(), 
            # base::quote(expr = ): value returned by lifecycle::deprecated(), the default value of this deprecated argument (written without the lifecycle package, not imported by saferGG)
            axis.ticks.y.left = ggplot2::element_line(colour = NULL, linewidth = 0.25, linetype = NULL, lineend = NULL, color = NULL, linejoin = NULL, arrow = NULL, arrow.fill = NULL, inherit.blank = FALSE, size = base::quote(expr = )), 
            # axis.line.x.bottom = ggplot2::element_line(linewidth = 0.25), # ugly, thus i added geom_hline below
            # base::quote(expr = ): value returned by lifecycle::deprecated(), the default value of this deprecated argument (written without the lifecycle package, not imported by saferGG)
            axis.line.y.left = ggplot2::element_line(colour = NULL, linewidth = 0.25, linetype = NULL, lineend = NULL, color = NULL, linejoin = NULL, arrow = NULL, arrow.fill = NULL, inherit.blank = FALSE, size = base::quote(expr = )), line = , rect = , text = , title = , point = , polygon = , geom = , spacing = , margins = , aspect.ratio = , axis.title = , axis.title.x = , axis.title.x.top = , axis.title.x.bottom = , axis.title.y = , axis.title.y.left = , axis.title.y.right = , axis.text = , axis.text.x = , axis.text.x.top = , axis.text.x.bottom = , axis.text.y = , axis.text.y.left = , axis.text.y.right = , axis.text.theta = , axis.text.r = , axis.ticks = , axis.ticks.x.top = , axis.ticks.x.bottom = , axis.ticks.y = , axis.ticks.y.right = , axis.ticks.theta = , axis.ticks.r = , axis.minor.ticks.x.top = , axis.minor.ticks.x.bottom = , axis.minor.ticks.y.left = , axis.minor.ticks.y.right = , axis.minor.ticks.theta = , axis.minor.ticks.r = , axis.ticks.length = , axis.ticks.length.x = , axis.ticks.length.x.top = , axis.ticks.length.x.bottom = , axis.ticks.length.y = , axis.ticks.length.y.left = , axis.ticks.length.y.right = , axis.ticks.length.theta = , axis.ticks.length.r = , axis.minor.ticks.length = , axis.minor.ticks.length.x = , axis.minor.ticks.length.x.top = , axis.minor.ticks.length.x.bottom = , axis.minor.ticks.length.y = , axis.minor.ticks.length.y.left = , axis.minor.ticks.length.y.right = , axis.minor.ticks.length.theta = , axis.minor.ticks.length.r = , axis.line = , axis.line.x = , axis.line.x.top = , axis.line.x.bottom = , axis.line.y = , axis.line.y.right = , axis.line.theta = , axis.line.r = , legend.background = , legend.margin = , legend.spacing = , legend.spacing.x = , legend.spacing.y = , legend.key = , legend.key.size = , legend.key.height = , legend.key.width = , legend.key.spacing = , legend.key.spacing.x = , legend.key.spacing.y = , legend.key.justification = , legend.frame = , legend.ticks = , legend.ticks.length = , legend.axis.line = , legend.text = , legend.text.position = , legend.title = , legend.title.position = , legend.position.inside = , legend.direction = , legend.byrow = , legend.justification = , legend.justification.top = , legend.justification.bottom = , legend.justification.left = , legend.justification.right = , legend.justification.inside = , legend.location = , legend.box = , legend.box.just = , legend.box.margin = , legend.box.background = , legend.box.spacing = , panel.background = , panel.spacing = , panel.spacing.x = , panel.spacing.y = , panel.grid.major = , panel.grid.minor = , panel.grid.major.x = , panel.grid.major.y = , panel.grid.minor.x = , panel.grid.minor.y = , panel.ontop = , panel.widths = , panel.heights = , plot.background = , plot.title.position = , plot.subtitle = , plot.caption = , plot.caption.position = , plot.tag = , plot.tag.position = , plot.tag.location = , plot.margin = , strip.background = , strip.background.x = , strip.background.y = , strip.clip = , strip.placement = , strip.text = , strip.text.x = , strip.text.x.bottom = , strip.text.x.top = , strip.text.y = , strip.text.y.left = , strip.text.y.right = , strip.switch.pad.grid = , strip.switch.pad.wrap = , complete = FALSE, validate = TRUE
        ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_hline(
            mapping = NULL, 
            data = NULL, 
            stat = "identity", 
            position = "identity", 
            na.rm = FALSE, 
            show.legend = NA, 
            inherit.aes = FALSE, 
            yintercept = base::ifelse(
                test = base::is.null(x = y_lim1), 
                yes = base::ifelse(test = reverse1, yes = base::max(obs[ , top_y_column], na.rm = TRUE), no = base::min(obs[ , top_y_column], na.rm = TRUE)), 
                no = base::ifelse(test = reverse1, yes = base::max(y_lim1, na.rm = FALSE), no = base::min(y_lim1, na.rm = FALSE))
            ), 
            linewidth = 0.25
        ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
        # add tick lines if vgrid is FALSE
        if(vgrid == FALSE){
            gline <- grid::linesGrob(x = grid::unit(x = base::c(0, 1), units = "npc", data = NULL), y = base::c(-0.02, 0), gp = grid::gpar(col = "black", lwd = 0.5), default.units = "npc", arrow = NULL, name = NULL, vp = NULL)
            tempo_vline_pos <- base::c(xmin_plot, chr$LENGTH_CUMUL)
            for(i0 in base::seq_along(along.with = tempo_vline_pos)){
                i2 <- tempo_vline_pos[i0]
                base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::annotation_custom(grob = gline, xmin = i2, xmax = i2, ymin = -Inf, ymax = Inf), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
            }
        }
        fin_plot1 <- base::suppressMessages(expr = base::suppressWarnings(expr = base::eval(expr = base::parse(text = base::paste0(tempo_gg_name, 1:tempo_gg_count, collapse = " + ", recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)), classes = "warning"), classes = "message")
        # tempo_output <- ggplot2::ggplot_build(fin_plot1)
        if(base::is.null(x = bottom_y_column)){
            base::suppressMessages(expr = base::suppressWarnings(expr = gridExtra::grid.arrange(fin_plot1, ncol = 1, nrow = 1, newpage = TRUE), classes = "warning"), classes = "message")
        }else{
            y_min_pos2 <- base::ifelse(test = base::is.null(x = y_lim2), yes = base::min(obs[ , bottom_y_column], na.rm = TRUE), no = base::min(y_lim2, na.rm = FALSE))
            y_max_pos2 <- base::ifelse(test = base::is.null(x = y_lim2), yes = base::max(obs[ , bottom_y_column], na.rm = TRUE), no = base::max(y_lim2, na.rm = FALSE))
            tempo_gg_name2 <- "gg.indiv.plot."
            tempo_gg_count2 <- 0
            base::assign(x = base::paste0(tempo_gg_name2, tempo_gg_count2 <- tempo_gg_count2 + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::ggplot(data = obs, mapping = ggplot2::aes(x = .data[["coord"]], y = .data[[bottom_y_column]]), environment = base::environment(fun = NULL)), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
            if(vgrid == TRUE){
                base::assign(x = base::paste0(tempo_gg_name2, tempo_gg_count2 <- tempo_gg_count2 + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_vline(
                    mapping = NULL, 
                    data = NULL, 
                    stat = "identity", 
                    position = "identity", 
                    na.rm = FALSE, 
                    show.legend = NA, 
                    inherit.aes = FALSE, 
                    xintercept = base::c(xmin_plot, chr$LENGTH_CUMUL), 
                    linewidth = 0.25, 
                    color = "grey80"
                ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
            }
            if( ! base::is.null(x = y_threshold2)){
                base::assign(x = base::paste0(tempo_gg_name2, tempo_gg_count2 <- tempo_gg_count2 + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_hline(
                    mapping = NULL, 
                    data = NULL, 
                    stat = "identity", 
                    position = "identity", 
                    na.rm = FALSE, 
                    show.legend = NA, 
                    inherit.aes = FALSE, 
                    yintercept = y_threshold2, 
                    linetype = "22", 
                    linewidth = 0.25, 
                    color = "red"
                ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
            }
            if(base::is.null(x = color_column)){
                if(base::is.null(x = dot_border_color)){
                    base::assign(x = base::paste0(tempo_gg_name2, tempo_gg_count2 <- tempo_gg_count2 + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_point(
                        data = NULL, 
                        stat = "identity", 
                        position = "identity", 
                        na.rm = FALSE, 
                        show.legend = NA, 
                        inherit.aes = TRUE, 
                        mapping = ggplot2::aes(color = base::as.factor(x = .data[["CHROM"]]), x = , y = ), 
                        alpha = 1, 
                        pch = 16, 
                        size = 1
                    ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
                    base::assign(x = base::paste0(tempo_gg_name2, tempo_gg_count2 <- tempo_gg_count2 + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_color_manual(values = base::rep(x = base::c("grey20", "skyblue"), times = 25), aesthetics = "colour", breaks = ggplot2::waiver(), na.value = "grey50"), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
                }else{
                    base::assign(x = base::paste0(tempo_gg_name2, tempo_gg_count2 <- tempo_gg_count2 + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_point(
                        data = NULL, 
                        stat = "identity", 
                        position = "identity", 
                        na.rm = FALSE, 
                        show.legend = NA, 
                        inherit.aes = TRUE, 
                        mapping = ggplot2::aes(fill = base::as.factor(x = .data[["CHROM"]]), x = , y = ), 
                        alpha = 1, 
                        color = dot_border_color, 
                        pch = 21, 
                        size = 1
                    ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
                    base::assign(x = base::paste0(tempo_gg_name2, tempo_gg_count2 <- tempo_gg_count2 + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_fill_manual(values = base::rep(x = base::c("grey20", "skyblue"), times = 25), aesthetics = "fill", breaks = ggplot2::waiver(), na.value = "grey50"), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
                }
            }else{
                if(base::is.null(x = dot_border_color)){
                    base::assign(x = base::paste0(tempo_gg_name2, tempo_gg_count2 <- tempo_gg_count2 + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_point(
                        data = NULL, 
                        stat = "identity", 
                        position = "identity", 
                        na.rm = FALSE, 
                        show.legend = NA, 
                        inherit.aes = TRUE, 
                        mapping = ggplot2::aes(color = .data[[color_column]], x = , y = ), 
                        alpha = 1, 
                        pch = 16, 
                        size = 1
                    ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
                    base::assign(x = base::paste0(tempo_gg_name2, tempo_gg_count2 <- tempo_gg_count2 + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_color_gradient2(name = ggplot2::waiver(), low = scales::muted(colour = "red", l = 30, c = 70), mid = "white", high = scales::muted(colour = "blue", l = 30, c = 70), midpoint = 0, space = "Lab", na.value = "grey50", transform = "identity", guide = "colourbar", aesthetics = "colour"), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
                }else{
                    base::assign(x = base::paste0(tempo_gg_name2, tempo_gg_count2 <- tempo_gg_count2 + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_point(
                        data = NULL, 
                        stat = "identity", 
                        position = "identity", 
                        na.rm = FALSE, 
                        show.legend = NA, 
                        inherit.aes = TRUE, 
                        mapping = ggplot2::aes(fill = .data[[color_column]], x = , y = ), 
                        alpha = 1, 
                        color = dot_border_color, 
                        pch = 21, 
                        size = 1
                    ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
                    base::assign(x = base::paste0(tempo_gg_name2, tempo_gg_count2 <- tempo_gg_count2 + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_fill_gradient2(name = ggplot2::waiver(), low = scales::muted(colour = "red", l = 30, c = 70), mid = "white", high = scales::muted(colour = "blue", l = 30, c = 70), midpoint = 0, space = "Lab", na.value = "grey50", transform = "identity", guide = "colourbar", aesthetics = "fill"), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
                }
            }
            base::assign(x = base::paste0(tempo_gg_name2, tempo_gg_count2 <- tempo_gg_count2 + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_x_continuous(
                name = ggplot2::waiver(), 
                minor_breaks = ggplot2::waiver(), 
                n.breaks = NULL, 
                na.value = NA_real_, 
                transform = "identity", 
                guide = ggplot2::waiver(), 
                position = "bottom", 
                sec.axis = ggplot2::waiver(), 
                expand = base::c(0, 0), # remove space after after axis limits
                oob = scales::rescale_none, 
                labels = chr$CHR_NAME, 
                breaks = chr$CHR_NAME_POS, 
                # base::quote(expr = ): value returned by lifecycle::deprecated(), the default value of this deprecated argument (written without the lifecycle package, not imported by saferGG)
                limits = base::c(xmin_plot - marging, base::max(chr$LENGTH_CUMUL, na.rm = FALSE) + marging), trans = base::quote(expr = )
            ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
            if(y_log2 == TRUE){
                if(base::any(obs[ , bottom_y_column] <= 0, na.rm = FALSE)){
                    tempo_cat <- base::paste0(
                        error_text_start, 
                        "THE y_log2 ARGUMENT CANNOT BE SET TO TRUE IF 0 OR NEG VALUES IN THE ",
                        bottom_y_column,
                        " FIELD OF THE TSV OR VCF",
                        collapse = NULL,
                        recycle0 = FALSE
                    )
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                }else{
                    base::assign(x = base::paste0(tempo_gg_name2, tempo_gg_count2 <- tempo_gg_count2 + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_y_continuous(
                        name = ggplot2::waiver(), 
                        minor_breaks = ggplot2::waiver(), 
                        n.breaks = NULL, 
                        na.value = NA_real_, 
                        guide = ggplot2::waiver(), 
                        position = "left", 
                        sec.axis = ggplot2::waiver(), 
                        expand = base::c(0, 0), # remove space after after axis limits
                        limits = if(reverse2 == TRUE){base::c(y_min_pos2, y_max_pos2)}else{base::c(y_max_pos2, y_min_pos2)}, # NA indicate that limits must correspond to data limits but ylim() already used
                        oob = scales::rescale_none, 
                        transform = "log10", 
                        breaks = scales::trans_breaks(trans = "log10", inv = function(x){10^x}, n = 5), 
                        # base::quote(expr = ): value returned by lifecycle::deprecated(), the default value of this deprecated argument (written without the lifecycle package, not imported by saferGG)
                        labels = scales::trans_format(trans = "log10", format = scales::math_format(expr = 10^.x, format = base::force)), trans = base::quote(expr = )
                    ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
                    # assign(paste0(tempo_gg_name2, tempo_gg_count2 <- tempo_gg_count2 + 1), ggplot2::annotation_logticks(outside = TRUE)) # 
                }
            }else{
                base::assign(x = base::paste0(tempo_gg_name2, tempo_gg_count2 <- tempo_gg_count2 + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_y_continuous(
                    name = ggplot2::waiver(), 
                    breaks = ggplot2::waiver(), 
                    minor_breaks = ggplot2::waiver(), 
                    n.breaks = NULL, 
                    labels = ggplot2::waiver(), 
                    na.value = NA_real_, 
                    guide = ggplot2::waiver(), 
                    position = "left", 
                    sec.axis = ggplot2::waiver(), 
                    expand = base::c(0, 0), # remove space after after axis limits
                    limits = if(reverse2 == TRUE){base::c(y_min_pos2, y_max_pos2)}else{base::c(y_max_pos2, y_min_pos2)}, # NA indicate that limits must correspond to data limits but ylim() already used
                    oob = scales::rescale_none, 
                    # base::quote(expr = ): value returned by lifecycle::deprecated(), the default value of this deprecated argument (written without the lifecycle package, not imported by saferGG)
                    transform = "identity", trans = base::quote(expr = ) # equivalent to ggplot2::scale_y_reverse() but create the problem of y-axis label disappearance with y_lim decreasing. Thus, do not use. Use ylim() below and after this
                ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
            }
            base::assign(x = base::paste0(tempo_gg_name2, tempo_gg_count2 <- tempo_gg_count2 + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::theme_bw(base_size = 11, base_family = "", header_family = NULL, base_line_size = 11 / 22, base_rect_size = 11 / 22, ink = "black", paper = "white", accent = "#3366FF"), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
            base::assign(x = base::paste0(tempo_gg_name2, tempo_gg_count2 <- tempo_gg_count2 + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::theme(
                legend.position = if(base::is.null(x = color_column)){"none"}else{"right"}, 
                panel.border = ggplot2::element_blank(), 
                panel.grid = ggplot2::element_blank(), 
                # base::quote(expr = ): value returned by lifecycle::deprecated(), the default value of this deprecated argument (written without the lifecycle package, not imported by saferGG)
                axis.ticks.y.left = ggplot2::element_line(colour = NULL, linewidth = 0.25, linetype = NULL, lineend = NULL, color = NULL, linejoin = NULL, arrow = NULL, arrow.fill = NULL, inherit.blank = FALSE, size = base::quote(expr = )), 
                # axis.line.x.top = ggplot2::element_line(linewidth = 0.25), # is not displayed. Thus, I add a geom_hline below
                # base::quote(expr = ): value returned by lifecycle::deprecated(), the default value of this deprecated argument (written without the lifecycle package, not imported by saferGG)
                axis.line.y.left = ggplot2::element_line(colour = NULL, linewidth = 0.25, linetype = NULL, lineend = NULL, color = NULL, linejoin = NULL, arrow = NULL, arrow.fill = NULL, inherit.blank = FALSE, size = base::quote(expr = )), 
                axis.title.x = ggplot2::element_blank(), 
                axis.text.x = ggplot2::element_blank(), 
                axis.ticks.x = ggplot2::element_blank(), line = , rect = , text = , title = , point = , polygon = , geom = , spacing = , margins = , aspect.ratio = , axis.title = , axis.title.x.top = , axis.title.x.bottom = , axis.title.y = , axis.title.y.left = , axis.title.y.right = , axis.text = , axis.text.x.top = , axis.text.x.bottom = , axis.text.y = , axis.text.y.left = , axis.text.y.right = , axis.text.theta = , axis.text.r = , axis.ticks = , axis.ticks.x.top = , axis.ticks.x.bottom = , axis.ticks.y = , axis.ticks.y.right = , axis.ticks.theta = , axis.ticks.r = , axis.minor.ticks.x.top = , axis.minor.ticks.x.bottom = , axis.minor.ticks.y.left = , axis.minor.ticks.y.right = , axis.minor.ticks.theta = , axis.minor.ticks.r = , axis.ticks.length = , axis.ticks.length.x = , axis.ticks.length.x.top = , axis.ticks.length.x.bottom = , axis.ticks.length.y = , axis.ticks.length.y.left = , axis.ticks.length.y.right = , axis.ticks.length.theta = , axis.ticks.length.r = , axis.minor.ticks.length = , axis.minor.ticks.length.x = , axis.minor.ticks.length.x.top = , axis.minor.ticks.length.x.bottom = , axis.minor.ticks.length.y = , axis.minor.ticks.length.y.left = , axis.minor.ticks.length.y.right = , axis.minor.ticks.length.theta = , axis.minor.ticks.length.r = , axis.line = , axis.line.x = , axis.line.x.top = , axis.line.x.bottom = , axis.line.y = , axis.line.y.right = , axis.line.theta = , axis.line.r = , legend.background = , legend.margin = , legend.spacing = , legend.spacing.x = , legend.spacing.y = , legend.key = , legend.key.size = , legend.key.height = , legend.key.width = , legend.key.spacing = , legend.key.spacing.x = , legend.key.spacing.y = , legend.key.justification = , legend.frame = , legend.ticks = , legend.ticks.length = , legend.axis.line = , legend.text = , legend.text.position = , legend.title = , legend.title.position = , legend.position.inside = , legend.direction = , legend.byrow = , legend.justification = , legend.justification.top = , legend.justification.bottom = , legend.justification.left = , legend.justification.right = , legend.justification.inside = , legend.location = , legend.box = , legend.box.just = , legend.box.margin = , legend.box.background = , legend.box.spacing = , panel.background = , panel.spacing = , panel.spacing.x = , panel.spacing.y = , panel.grid.major = , panel.grid.minor = , panel.grid.major.x = , panel.grid.major.y = , panel.grid.minor.x = , panel.grid.minor.y = , panel.ontop = , panel.widths = , panel.heights = , plot.background = , plot.title = , plot.title.position = , plot.subtitle = , plot.caption = , plot.caption.position = , plot.tag = , plot.tag.position = , plot.tag.location = , plot.margin = , strip.background = , strip.background.x = , strip.background.y = , strip.clip = , strip.placement = , strip.text = , strip.text.x = , strip.text.x.bottom = , strip.text.x.top = , strip.text.y = , strip.text.y.left = , strip.text.y.right = , strip.switch.pad.grid = , strip.switch.pad.wrap = , complete = FALSE, validate = TRUE
            ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
            # add x-axis line
            base::assign(x = base::paste0(tempo_gg_name2, tempo_gg_count2 <- tempo_gg_count2 + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_hline(
                mapping = NULL, 
                data = NULL, 
                stat = "identity", 
                position = "identity", 
                na.rm = FALSE, 
                show.legend = NA, 
                inherit.aes = FALSE, 
                yintercept = base::ifelse(
                    test = base::is.null(x = y_lim2), 
                    yes = base::ifelse(test = reverse2, yes = base::max(obs[ , bottom_y_column], na.rm = TRUE), no = base::min(obs[ , bottom_y_column], na.rm = TRUE)), 
                    no = base::ifelse(test = reverse2, yes = base::max(y_lim2, na.rm = FALSE), no = base::min(y_lim2, na.rm = FALSE))
                ), 
                linewidth = 0.25
            ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
            # add tick lines if vgrid is FALSE
            if(vgrid == FALSE){
                gline2 <- grid::linesGrob(x = grid::unit(x = base::c(0, 1), units = "npc", data = NULL), y = base::c(1, 1.02), gp = grid::gpar(col = "black", lwd = 0.5), default.units = "npc", arrow = NULL, name = NULL, vp = NULL)
                tempo_vline_pos <- base::c(xmin_plot, chr$LENGTH_CUMUL)
                for(i0 in base::seq_along(along.with = tempo_vline_pos)){
                    i3 <- tempo_vline_pos[i0]
                    base::assign(x = base::paste0(tempo_gg_name2, tempo_gg_count2 <- tempo_gg_count2 + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::annotation_custom(grob = gline2, xmin = i3, xmax = i3, ymin = -Inf, ymax = Inf), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
                }
            }
            fin_plot2 <- base::suppressMessages(expr = base::suppressWarnings(expr = base::eval(expr = base::parse(text = base::paste0(tempo_gg_name2, 1:tempo_gg_count2, collapse = " + ", recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)), classes = "warning"), classes = "message")
            gl <- base::lapply(X = base::list(fin_plot1, fin_plot2), FUN = ggplot2::ggplotGrob)
            wd <- base::do.call(what = grid::unit.pmax, args = base::lapply(X = gl, FUN = "[[", "widths"), quote = FALSE, envir = base::environment(fun = NULL))
            gl <- base::lapply(X = gl, FUN = function(x){x[["widths"]] <- wd ; x})
            if(vgrid == FALSE){
                gl[[1]]$layout$clip[gl[[1]]$layout$name == "panel"] <- "off"
                gl[[2]]$layout$clip[gl[[2]]$layout$name == "panel"] <- "off"
            }
            base::suppressMessages(expr = base::suppressWarnings(expr = gridExtra::grid.arrange(gl[[1]], gl[[2]], ncol = 1, nrow = 2, newpage = TRUE), classes = "warning"), classes = "message")
        }
    }else{
            # nocov start (unreachable: idem (x_lim = NULL rejected by arg_check))
        saferGG::gg_empty(text = "NO PLOT DRAWN\nTHE x_lim PARAMETER\nMIGHT BE OUTSIDE\nOF THE RANGE OF THE VCF FILE\nOR THE RANGE OF THE region PARAMETER\nOR NULL", text_size = 12, title = NULL, title_size = 8, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text)
            # nocov end
    } 
    # else already dealt above
    # close the graphic device opened above
    if(grDevices::dev.cur() != 1){
        grDevices::dev.off(which = grDevices::dev.cur())
    }
    # end main code
    if(warn_print == TRUE & ! base::is.null(x = warn)){
            # nocov start (unreachable: warn stays NULL because the only warn assignment (x_lim NULL branch) is dead)
        base::on.exit(expr = base::warning(base::paste0("FROM ", function_name, ":\n\n", warn, collapse = NULL, recycle0 = FALSE), call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, domain = NULL), add = TRUE, after = TRUE)
            # nocov end
    }
    base::on.exit(expr = base::options(warning.length = ini_warning_length), add = TRUE, after = TRUE)
    #end main code
    #### end main code

    #### warning output
    #### end warning output

    #### output
    base::return(base::invisible(x = NULL))
    #### end output
}
