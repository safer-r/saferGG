

gg_empty <- function(
        text = NULL, 
        text.size = 12, 
        title = NULL, 
        title.size = 8, 
        lib.path = NULL
){
    # AIM
    # display an empty ggplot2 plot with a text in the middle of the window (for instance to specify that no plot can be drawn)
    # ARGUMENTS
    # text: character string of the message to display
    # text.size: numeric value of the text size (in points)
    # title: character string of the graph title
    # title.size: numeric value of the title size (in points)
    # lib.path: character vector specifying the absolute pathways of the directories containing the required packages if not in the default directories. Ignored if NULL
    # RETURN
    # an empty plot
    # REQUIRED PACKAGES
    # ggplot2
    # REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
    # fun_check()
    # fun_pack()
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
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], "()")
    # end function name
    # required function checking
    if(length(utils::find("fun_check", mode = "function")) == 0L){
        tempo.cat <- paste0("ERROR IN ", function.name, ": REQUIRED fun_check() FUNCTION IS MISSING IN THE R ENVIRONMENT")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if(length(utils::find("fun_pack", mode = "function")) == 0L){
        tempo.cat <- paste0("ERROR IN ", function.name, ": REQUIRED fun_pack() FUNCTION IS MISSING IN THE R ENVIRONMENT")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end required function checking
    # argument checking
    arg.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$object.name))
    if( ! is.null(text)){
        tempo <- fun_check(data = text, class = "vector", mode = "character", length = 1, fun.name = function.name) ; eval(ee)
    }
    tempo <- fun_check(data = text.size, class = "vector", mode = "numeric", length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(title)){
        tempo <- fun_check(data = title, class = "vector", mode = "character", length = 1, fun.name = function.name) ; eval(ee)
    }
    tempo <- fun_check(data = title.size, class = "vector", mode = "numeric", length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(arg.check)){
        if(any(arg.check) == TRUE){
            stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # source("C:/Users/gmillot/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.7/r_debugging_tools-v1.7.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
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
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_text(data = data.frame(x = 1, y = 1, stringsAsFactors = TRUE), ggplot2::aes(x = x, y = y, label = text), size = text.size))
    }
    assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::ggtitle(title))
    assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::theme_void())
    assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), m.gg <- ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "white", color = NA),
        plot.title = ggplot2::element_text(size = title.size) # stronger than text
    ))
    final <- suppressWarnings(eval(parse(text = paste(paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + "))))
    return(final)
}
