
gg_get_legend <- function(ggplot_built, fun.name = NULL, lib.path = NULL){
    # AIM
    # get legend of ggplot objects
    # # from https://stackoverflow.com/questions/12539348/ggplot-separate-legend-and-plot
    # ARGUMENTS
    # ggplot_built: a ggplot build object
    # fun.name: single character string indicating the name of the function using fun_gg_get_legend() for warning and error messages. Ignored if NULL
    # lib.path: character vector specifying the absolute pathways of the directories containing the required packages if not in the default directories. Ignored if NULL
    # RETURN
    # a list of class c("gtable", "gTree", "grob", "gDesc"), providing legend information of ggplot_built objet, or NULL if the ggplot_built object has no legend
    # REQUIRED PACKAGES
    # ggplot2
    # REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
    # fun_check()
    # fun_pack()
    # EXAMPLES
    # Simple example
    # obs1 <- data.frame(time = 1:20, group = rep(c("CLASS_1", "CLASS_2"), times = 10), stringsAsFactors = TRUE) ; p <- ggplot2::ggplot() + ggplot2::geom_point(data = obs1, mapping = ggplot2::aes(x = group, y = time, fill = group)) ; fun_gg_get_legend(ggplot_built = ggplot2::ggplot_build(p))
    # Error message because no legend in the ggplot
    # obs1 <- data.frame(time = 1:20, group = rep(c("CLASS_1", "CLASS_2"), times = 10), stringsAsFactors = TRUE) ; p <- ggplot2::ggplot() + ggplot2::geom_point(data = obs1, mapping = ggplot2::aes(x = group, y = time)) ; fun_gg_get_legend(ggplot_built = ggplot2::ggplot_build(p))
    # DEBUGGING
    # obs1 <- data.frame(time = 1:20, group = rep(c("CLASS_1", "CLASS_2"), times = 10), stringsAsFactors = TRUE) ; p <- ggplot2::ggplot() + ggplot2::geom_point(data = obs1, mapping = ggplot2::aes(x = group, y = time)) ; ggplot_built = ggplot2::ggplot_build(p) ; fun.name = NULL ; lib.path = NULL
    # function name
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], "()")
    # end function name
    # required function checking
    req.function <- c(
        "fun_check",
        "fun_pack"
    )
    for(i1 in req.function){
        if(length(find(i1, mode = "function")) == 0L){
            tempo.cat <- paste0("ERROR IN ", function.name, ": REQUIRED ", i1, "() FUNCTION IS MISSING IN THE R ENVIRONMENT")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end required function checking
    # argument primary checking
    # arg with no default values
    mandat.args <- c(
        "ggplot_built"
    )
    tempo <- eval(parse(text = paste0("c(missing(", paste0(mandat.args, collapse = "), missing("), "))")))
    if(any(tempo)){ # normally no NA for missing() output
        tempo.cat <- paste0("ERROR IN ", function.name, "\nFOLLOWING ARGUMENT", ifelse(sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", paste0(mandat.args[tempo], collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    # using fun_check()
    arg.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$object.name))
    tempo <- fun_check(data = ggplot_built, class = "ggplot_built", mode = "list", fun.name = function.name) ; eval(ee)
    if( ! is.null(fun.name)){
        tempo <- fun_check(data = fun.name, class = "vector", mode = "character", length = 1, fun.name = function.name) ; eval(ee)
    }
    if( ! is.null(lib.path)){
        tempo <- fun_check(data = lib.path, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
    }
    if( ! is.null(arg.check)){
        if(any(arg.check) == TRUE){
            stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end using fun_check()
    # source("C:/Users/gmillot/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.7/r_debugging_tools-v1.7.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
    # end argument primary checking
    # second round of checking
    # management of NA
    if(any(is.na(ggplot_built)) | any(is.na(fun.name)) | any(is.na(lib.path))){
        tempo.cat <- paste0("ERROR IN ", function.name, ": NO ARGUMENT CAN HAVE NA VALUES")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end management of NA
    # management of NULL
    if(is.null(ggplot_built)){
        tempo.cat <- paste0("ERROR IN ", function.name, "\nggplot_built ARGUMENT CANNOT BE NULL")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end management of NULL
    if( ! is.null(lib.path)){
        if( ! all(dir.exists(lib.path))){
            tempo.cat <- paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", paste(lib.path, collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end second round of checking
    # package checking
    fun_pack(req.package = c("ggplot2"), lib.path = lib.path)
    # end package checking
    # main code
    win.nb <- dev.cur()
    pdf(file = NULL)
    tmp <- ggplot2::ggplot_gtable(ggplot_built)
    # BEWARE with ggplot_gtable : open a blanck device https://stackoverflow.com/questions/17012518/why-does-this-r-ggplot2-code-bring-up-a-blank-display-device
    invisible(dev.off())
    if(win.nb > 1){ # to go back to the previous active device, if == 1 means no opened device
        dev.set(win.nb)
    }
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    if(length(leg) == 0L){
        legend <- NULL
    }else{
        legend <- tmp$grobs[[leg]]
    }
    return(legend)
}
