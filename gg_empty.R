#' @title gg_empty
#' @description
#' display an empty ggplot2 plot with a text in the middle of the window (for instance to specify that no plot can be drawn).
#' @param text character string of the message to display.
#' @param text.size numeric value of the text size (in points).
#' @param title character string of the graph title.
#' @param title.size numeric value of the title size (in points).
#' @param lib.path character vector specifying the absolute pathways of the directories containing the required packages if not in the default directories. Ignored if NULL.
#' @returns an empty plot.
#' @examples
#' ### simple example.
#' gg_empty(text = "NO GRAPH")    
#' ### white page.
#' gg_empty()
#' ### all the arguments.
#' gg_empty(text = "NO GRAPH", text.size = 8, title = "GRAPH1", title.size = 10, lib.path = NULL)   
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 theme_void
#' @importFrom utils find
#' @export
gg_empty <- function(
        text = NULL, 
        text.size = 12, 
        title = NULL, 
        title.size = 8, 
        lib.path = NULL
){
    # DEBUGGING
    # text = "NO GRAPH" ; text.size = 12 ; title = "GRAPH1" ; title.size = 8 ; lib.path = NULL
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
    if( ! base::is.null(lib.path)){
        if( ! base::all(base::typeof(lib.path) == "character")){ # no na.rm = TRUE with typeof
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT MUST BE A VECTOR OF CHARACTERS:\n", base::paste(lib.path, collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else if( ! base::all(base::dir.exists(lib.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", base::paste(lib.path, collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else{
            .libPaths(new = base::sub(x = lib.path, pattern = "/$|\\\\$", replacement = "")) # .libPaths(new = ) add path to default path. BEWARE: .libPaths() does not support / at the end of a submitted path. Thus check and replace last / or \\ in path
            lib.path <- .libPaths()
        }
    }else{
        lib.path <- .libPaths() # .libPaths(new = lib.path) # or .libPaths(new = c(.libPaths(), lib.path))
    }
    # end check of lib.path
    # check of the required function from the required packages
    .pack_and_function_check(
        fun = base::c(
            "ggplot2::aes",
            "ggplot2::element_rect",
            "ggplot2::element_text",
            "ggplot2::geom_text",
            "ggplot2::ggplot",
            "ggplot2::ggtitle",
            "ggplot2::theme_void",
            "saferDev::arg_check",
            "utils::find"         
        ),
        lib.path = lib.path,
        external.function.name = function.name
    )
    # end check of the required function from the required packages
    # end package checking

    # argument primary checking
    # arg with no default values
    # end arg with no default values
    # argument checking with arg_check()     
    # argument checking
    argum.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- base::expression(argum.check <- base::c(argum.check, tempo$problem) , text.check <- base::c(text.check, tempo$text) , checked.arg.names <- base::c(checked.arg.names, tempo$object.name))
    if( ! base::is.null(text)){
        tempo <- saferDev::arg_check(data = text, class = "vector", mode = "character", length = 1, fun.name = function.name) ; base::eval(ee)
    }
    tempo <- saferDev::arg_check(data = text.size, class = "vector", mode = "numeric", length = 1, fun.name = function.name) ; base::eval(ee)
    if( ! base::is.null(title)){
        tempo <- saferDev::arg_check(data = title, class = "vector", mode = "character", length = 1, fun.name = function.name) ; base::eval(ee)
    }
    tempo <- saferDev::arg_check(data = title.size, class = "vector", mode = "numeric", length = 1, fun.name = function.name) ; base::eval(ee)
    if( ! base::is.null(argum.check)){
        if(base::any(argum.check) == TRUE){
            base::stop(base::paste0("\n\n================\n\n", base::paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with arg_check()
    # check with r_debugging_tools
    # source("C:/Users/gmillot/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.7/r_debugging_tools-v1.7.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_saferDev::arg_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using saferDev::arg_check()
    # end check with r_debugging_tools
    # end argument primary checking

    # second round of checking and data preparation
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
        "text.size" ,
        "title.size" 
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
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    # end second round of checking and data preparation
    # main code
    tempo.gg.name <- "gg.indiv.plot."
    tempo.gg.count <- 0
    # no need loop part
    base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::ggplot())
    if( ! base::is.null(text)){
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_text(data = base::data.frame(x = 1, y = 1, stringsAsFactors = TRUE), ggplot2::aes(x = x, y = y, label = text), size = text.size))
    }
    base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::ggtitle(title))
    base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::theme_void())
    base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), m.gg <- ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "white", color = NA),
        plot.title = ggplot2::element_text(size = title.size) # stronger than text
    ))
    final <- base::suppressWarnings(base::eval(base::parse(text = base::paste(base::paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + "))))
    base::return(final)
}
