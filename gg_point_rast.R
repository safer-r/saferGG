

gg_point_rast <- function(
        data = NULL, 
        mapping = NULL, 
        stat = "identity", 
        position = "identity", 
        ..., 
        na.rm = FALSE, 
        show.legend = NA, 
        inherit.aes = TRUE, 
        raster.width = NULL, 
        raster.height = NULL, 
        raster.dpi = 300, 
        inactivate = TRUE, 
        lib.path = NULL
){
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
    # lib.path: character vector specifying the absolute pathways of the directories containing the required packages if not in the default directories. Ignored if NULL
    # RETURN
    # a raster scatter plot
    # REQUIRED PACKAGES
    # ggplot2
    # grid (included in the R installation packages but not automatically loaded)
    # Cairo
    # REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
    # fun_check()
    # fun_pack()
    # EXAMPLES
    # Two pdf in the current directory
    # set.seed(1) ; data1 = data.frame(x = rnorm(100000), y = rnorm(10000), stringsAsFactors = TRUE) ; fun_open(pdf.name = "Raster") ; ggplot2::ggplot() + fun_gg_point_rast(data = data1, mapping = ggplot2::aes(x = x, y = y)) ; fun_open(pdf.name = "Vectorial") ; ggplot2::ggplot() + ggplot2::geom_point(data = data1, mapping = ggplot2::aes(x = x, y = y)) ; dev.off() ; dev.off()
    # DEBUGGING
    # 
    # function name
    if(all(inactivate == FALSE)){ # inactivate has to be used here but will be fully checked below
        function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], "()")
    }else if(all(inactivate == TRUE)){
        function.name <- NULL
    }else{
        tempo.cat <- paste0("ERROR IN fun_gg_point_rast(): CODE INCONSISTENCY 1")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
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
    if( ! is.null(arg.check)){
        if(any(arg.check) == TRUE){
            stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # source("C:/Users/gmillot/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.7/r_debugging_tools-v1.7.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
    # end argument checking
    # package checking
    fun_pack(req.package = c("ggplot2"), lib.path = lib.path)
    fun_pack(req.package = c("grid"), lib.path = lib.path)
    fun_pack(req.package = c("Cairo"), lib.path = lib.path)
    # end package checking
    # additional functions
    DrawGeomPointRast <- function(data, panel_params, coord, na.rm = FALSE, raster.width = NULL, raster.height= NULL, raster.dpi = raster.dpi){
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
        invisible(dev.off(dev_id))
        invisible(dev.set(prev_dev_id))
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
