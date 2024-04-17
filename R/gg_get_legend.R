#' @title gg_get_legend
#' @description
#' get legend of ggplot objects #from https://stackoverflow.com/questions/12539348/ggplot-separate-legend-and-plot
#' @param ggplot_built a ggplot build object.
#' @param fun.name single character string indicating the name of the function using gg_get_legend() for warning and error messages. Ignored if NULL.
#' @param lib.path character vector specifying the absolute pathways of the directories containing the required packages if not in the default directories. Ignored if NULL.
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @returns a list of class c("gtable", "gTree", "grob", "gDesc"), providing legend information of ggplot_built objet, or NULL if the ggplot_built object has no legend.
#' @examples
#' # Simple example
#' obs1 <- data.frame(time = 1:20, group = rep(c("CLASS_1", "CLASS_2"), times = 10), stringsAsFactors = TRUE) ; p <- ggplot2::ggplot() + ggplot2::geom_point(data = obs1, mapping = ggplot2::aes(x = group, y = time, fill = group)) ; gg_get_legend(ggplot_built = ggplot2::ggplot_build(p))  
#' #Error message because no legend in the ggplot
#' obs1 <- data.frame(time = 1:20, group = rep(c("CLASS_1", "CLASS_2"), times = 10), stringsAsFactors = TRUE) ; p <- ggplot2::ggplot() + ggplot2::geom_point(data = obs1, mapping = ggplot2::aes(x = group, y = time)) ; gg_get_legend(ggplot_built = ggplot2::ggplot_build(p))  
#' @importFrom ggplot2 ggplot_gtable
#' @importFrom saferDev arg_check
#' @export
gg_get_legend <- function(
    ggplot_built, 
    fun.name = NULL, 
    lib.path = NULL,
    safer_check = TRUE
    ){
    # DEBUGGING
    # obs1 <- data.frame(time = 1:20, group = rep(c("CLASS_1", "CLASS_2"), times = 10), stringsAsFactors = TRUE) ; p <- ggplot2::ggplot() + ggplot2::geom_point(data = obs1, mapping = ggplot2::aes(x = group, y = time)) ; ggplot_built = ggplot2::ggplot_build(p) ; fun.name = NULL ; lib.path = NULL ;safer_check = TRUE
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
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", base::paste(lib.path, collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else{
            base::.libPaths(new = base::sub(x = lib.path, pattern = "/$|\\\\$", replacement = "")) # base::.libPaths(new = ) add path to default path. BEWARE: base::.libPaths() does not support / at the end of a submitted path. Thus check and replace last / or \\ in path
            lib.path <- base::.libPaths()
        }
    }else{
        lib.path <- base::.libPaths() # base::.libPaths(new = lib.path) # or base::.libPaths(new = c(base::.libPaths(), lib.path))
    }
    # end check of lib.path
    # check of the required function from the required packages
    if(safer_check == TRUE){
        .pack_and_function_check(
        fun = base::c(
            "ggplot2::ggplot_gtable",
            "saferDev::arg_check"
    ),
        lib.path = lib.path,
        external.function.name = function.name,
        external.package.name = package.name
    )
    }
    # end required function checking

    # argument primary checking
    # arg with no default values
    mandat.args <- base::c(
        "ggplot_built"
    )
    tempo <- base::eval(base::parse(text = base::paste0("base::missing(", base::paste0(mandat.args, collapse = ") | base::missing("), ")")))
    if(base::any(tempo)){ # normally no NA for base::missing() output
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nFOLLOWING ARGUMENT", base::ifelse(base::sum(tempo, na.rm = TRUE) > 1, "S HAVE", "HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", base::paste0(mandat.args, collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    # argument checking with arg_check()    
    # argument checking
    argum.check<- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- base::expression(argum.check<- base::c(argum.check, tempo$problem) , text.check <- base::c(text.check, tempo$text) , checked.arg.names <- base::c(checked.arg.names, tempo$object.name))
    tempo <- saferDev::arg_check(data = ggplot_built, class = "ggplot_built", mode = "list", fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(fun.name)){
        tempo <- saferDev::arg_check(data = fun.name, class = "vector", mode = "character", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    }
    if( ! base::is.null(lib.path)){
        tempo <- saferDev::arg_check(data = lib.path, class = "vector", mode = "character", fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    }
    if( ! base::is.null(argum.check)){
        if(base::any(argum.check) == TRUE){
            base::stop(base::paste0("\n\n================\n\n", base::paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end using saferDev::arg_check()
    # check with r_debugging_tools
    # source("C:/Users/yhan/Documents/Git_projects/debugging_tools_for_r_dev/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using arg_check()
    # end check with r_debugging_tools
    # end argument primary checking

    # second round of checking
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    # management of NA
    if( ! (base::all(base::class(arg.user.setting) %in% base::c("list", "NULL"), na.rm = TRUE) & base::length(arg.user.setting) == 0)){
        tempo.arg <- base::names(arg.user.setting) # values provided by the user
        tempo.log <- base::suppressWarnings(base::sapply(base::lapply(base::lapply(tempo.arg, FUN = base::get, envir = base::sys.nframe(), inherits = FALSE), FUN = base::is.na), FUN = base::any)) & base::lapply(base::lapply(tempo.arg, FUN = base::get, envir = base::sys.nframe(), inherits = FALSE), FUN = base::length) == 1L # no argument provided by the user can be just NA
        if(base::any(tempo.log) == TRUE){ # normally no NA because is.na() used here
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", base::paste0(tempo.arg[tempo.log], collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # management of NULL arguments
    tempo.arg <-base::c(
        "ggplot_built" ,
        "fun.name", # inactivated because can be null
        "lib.path" # inactivated because can be null
    )
    tempo.log <- base::sapply(base::lapply(tempo.arg, FUN = base::get, envir = base::sys.nframe(), inherits = FALSE), FUN = base::is.null)
    if(base::any(tempo.log) == TRUE){# normally no NA with is.null()
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE:\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), base::paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end management of NULL arguments
    # code that protects set.seed() in the global environment
    # end code that protects set.seed() in the global environment
    # warning initiation
    # end warning initiation
    # other checkings
    # end other checkings
    # end second round of checking and data preparation

    # main code
    win.nb <- grDevices::dev.cur()
    grDevices::pdf(file = NULL)
    tmp <- ggplot2::ggplot_gtable(ggplot_built)
    # BEWARE with ggplot_gtable : open a blanck device https://stackoverflow.com/questions/17012518/why-does-this-r-ggplot2-code-bring-up-a-blank-display-device
    base::invisible(grDevices::dev.off())
    if(win.nb > 1){ # to go back to the previous active device, if == 1 means no opened device
        grDevices::dev.set(win.nb)
    }
    leg <- base::which(base::sapply(tmp$grobs, function(x) x$name) == "guide-box")
    if(base::length(leg) == 0L){
        legend <- NULL
    }else{
        legend <- tmp$grobs[[leg]]
    }
    base::return(legend)
    #end main code
}
