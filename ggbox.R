# Error: class order not good when a class is removed due to NA
# Error: line 136 in check 20201126 with add argument
# Solve this: sometimes error messages can be more than the max display (8170). Thus, check every paste0("ERROR IN ", function.name, and trunck the message if to big. In addition, add at the begining of the warning message that it is too long and see the $warn output for complete message. Add also this into fun_scatter
# add dot.shape ? See with available aesthetic layers
# rasterise: https://cran.r-project.org/web/packages/ggrastr/vignettes/Raster_geoms.html
# add horizontal argument and deal any conflict with vertical argument. Start with horizontal = NULL as default. If ! is.null() -> convert vertical if required
# time for excecution : microbenchmark package. See also in RStudio time per line of code. See also https://stackoverflow.com/questions/7561362/what-can-cause-a-program-to-run-much-faster-the-second-time


ggbox <- function(
        data1, 
        y, 
        categ, 
        categ.class.order = NULL, 
        categ.color = NULL, 
        box.legend.name = NULL, 
        box.fill = FALSE, 
        box.width = 0.5, 
        box.space = 0.1, 
        box.line.size = 0.75, 
        box.notch = FALSE, 
        box.alpha = 1, 
        box.mean = TRUE, 
        box.whisker.kind = "std", 
        box.whisker.width = 0, 
        dot.color = grey(0.25), 
        dot.categ = NULL, 
        dot.categ.class.order = NULL, 
        dot.legend.name = NULL, 
        dot.tidy = FALSE, 
        dot.tidy.bin.nb = 50, 
        dot.jitter = 0.5, 
        dot.seed = 2, 
        dot.size = 3, 
        dot.alpha = 0.5, 
        dot.border.size = 0.5, 
        dot.border.color = NULL, 
        x.lab = NULL, 
        x.angle = 0, 
        y.lab = NULL, 
        y.lim = NULL, 
        y.log = "no", 
        y.tick.nb = NULL, 
        y.second.tick.nb = 1, 
        y.include.zero = FALSE, 
        y.top.extra.margin = 0.05, 
        y.bottom.extra.margin = 0.05, 
        stat.pos = "top", 
        stat.mean = FALSE, 
        stat.size = 4, 
        stat.dist = 5, 
        stat.angle = 0, 
        vertical = TRUE, 
        text.size = 12, 
        title = "", 
        title.text.size = 8, 
        legend.show = TRUE, 
        legend.width = 0.5, 
        article = TRUE, 
        grid = FALSE, 
        add = NULL, 
        return = FALSE, 
        return.ggplot = FALSE,
        return.gtable = TRUE,
        plot = TRUE, 
        warn.print = FALSE, 
        lib.path = NULL
){
    # AIM
    # Plot ggplot2 boxplots + dots + means
    # For ggplot2 specifications, see: https://ggplot2.tidyverse.org/articles/ggplot2-specs.html
    # WARNINGS
    # Rows containing NA in data1[, c(y, categ)] will be removed before processing, with a warning (see below)
    # Hinges are not computed like in the classical boxplot() function of R. See https://ggplot2.tidyverse.org/reference/geom_boxplot.html
    # To have a single box, please create a factor column with a single class and specify the name of this column in the categ argument. For a single set of grouped boxes, create a factor column with a single class and specify this column in categ argument as first element (i.e., as categ1, knowing that categ2 must also be specified in this situation). See categ argument below
    # The dot.alpha argument can alter the display of the color boxes when using pdf output
    # Size arguments (box.line.size, dot.size, dot.border.size, stat.size, text.size and title.text.size) are in mm. See Hadley comment in https://stackoverflow.com/questions/17311917/ggplot2-the-unit-of-size. See also http://sape.inf.usi.ch/quick-reference/ggplot2/size). Unit object are not accepted, but conversion can be used (e.g., grid::convertUnit(grid::unit(0.2, "inches"), "mm", valueOnly = TRUE))
    # Display seems to be done twice on Windows devices (like a blink). However, no double plots on pdf devices. Thus, the blink remains mysterious
    # To remove boxes and have only dots, use box.alpha = 0
    # ARGUMENTS
    # data1: data frame containing one column of quantitative values (see the y argument below) and one or two columns of categories (see the categ argument below). Duplicated column names are not allowed
    # y: character string of the data1 column name for y-axis (column containing numeric values). Numeric values will be split according to the classes of the column names indicated in the categ argument to generate the boxes and will also be used to plot the dots
    # categ: vector of character strings of the data1 column name for categories (column of characters or factors). Must be either one or two column names. If a single column name (further referred to as categ1), then one box per class of categ1. If two column names (further referred to as categ1 and categ2), then one box per class of categ2, which form a group of boxes in each class of categ1. WARNING: no empty classes allowed. To have a single box, create a factor column with a single class and specify the name of this column in the categ argument (here, no categ2 in categ argument). For a single set of grouped boxes, create a factor column with a single class and specify this column in categ argument as first element (i.e., as categ1), in addition to the already used category (as categ2 in this situation)
    # categ.class.order: list indicating the order of the classes of categ1 and categ2 represented on the boxplot (the first compartment for categ1 and and the second for categ2). If categ.class.order == NULL, classes are represented according to the alphabetical order. Some compartments can be NULL and others not. See the categ argument for categ1 and categ2 description
    # categ.color: vector of color character string for box frames (see the categ argument for categ1 and categ2 description)
    # If categ.color == NULL, default colors of ggplot2, whatever categ1 and categ2
    # If categ.color is non-null and only categ1 in categ argument, categ.color can be either:
    # (1) a single color string. All the boxes will have this color, whatever the number of classes of categ1
    # (2) a vector of string colors, one for each class of categ1. Each color will be associated according to categ.class.order of categ1
    # (3) a vector or factor of string colors, like if it was one of the column of data1 data frame. WARNING: a single color per class of categ1 and a single class of categ1 per color must be respected
    # Color functions, like grey(), hsv(), etc., are also accepted
    # Positive integers are also accepted instead of character strings, as long as above rules about length are respected. Integers will be processed by ggpalette() using the maximal integer value among all the integers in categ.color (see ggpalette())
    # If categ.color is non-null and categ1 and categ2 are specified, all the rules described above will apply to categ2 instead of categ1 (colors will be determined for boxes inside a group of boxes)
    # box.legend.name: character string of the legend title. If box.legend.name is NULL, then box.legend.name <- categ1 if only categ1 is present, and box.legend.name <- categ2 if categ1 and categ2 are present in the categ argument. Write "" if no legend required. See the categ argument for categ1 and categ2 description
    # box.fill: logical. Fill the box? If TRUE, the categ.color argument will be used to generate filled boxplots (the box frames being black) as well as filled outlier dots (the dot border being controlled by the dot.border.color argument). If all the dots are plotted (argument dot.color other than NULL), they will be over the boxes. If FALSE, the categ.color argument will be used to color the box frames and the outlier dot borders. If all the dots are plotted, they will be beneath the boxes
    # box.width: single numeric value (from 0 to 1) of width of either boxes or group of boxes
    # When categ argument has a single categ1 element (i.e., separate boxes. See the categ argument for categ1 and categ2 description), then each class of categ1 is represented by a single box. In that case, box.width argument defines each box width, from 0 (no box width) to 1 (max box width), but also the space between boxes (the code uses 1 - box.width for the box spaces). Of note, xmin and xmax of the ggbox() output report the box boundaries (around x-axis unit 1, 2, 3, etc., for each box)
    # When categ argument has a two categ1 and categ2 elements (i.e., grouped boxes), box.width argument defines the width allocated for each set of grouped boxes, from 0 (no group width) to 1 (max group width), but also the space between grouped boxes (the code uses 1 - box.width for the spaces). Of note, xmin and xmax of the ggbox() output report the box boundaries (around x-axis unit 1, 2, 3, etc., for each set of grouped box)
    # box.space: single numeric value (from 0 to 1) indicating the box separation inside grouped boxes, when categ argument has a two categ1 and categ2 elements. 0 means no space and 1 means boxes shrunk to a vertical line. Ignored if categ argument has a single categ1 element
    # box.line.size: single numeric value of line width of boxes and whiskers in mm
    # box.notch: logical. Notched boxplot? It TRUE, display notched boxplot, notches corresponding approximately to the 95% confidence interval of the median (the notch interval is exactly 1.58 x Inter Quartile Range (IQR) / sqrt(n), with n the number of values that made the box). If notch intervals between two boxes do not overlap, it can be interpreted as significant median differences
    # box.alpha: single numeric value (from 0 to 1) of box transparency (full transparent to full opaque, respectively). To remove boxplots, use box.alpha = 0
    # box.mean: logical. Add mean value? If TRUE, a diamond-shaped dot, with the horizontal diagonal corresponding to the mean value, is displayed over each boxplot
    # box.whisker.kind: range of the whiskers. Either "no" (no whiskers), or "std" (length of each whisker equal to 1.5 x Inter Quartile Range (IQR)), or "max" (length of the whiskers up or down to the most distant dot)
    # box.whisker.width: single numeric value (from 0 to 1) of the whisker width, with 0 meaning no whiskers and 1 meaning a width equal to the box width
    # dot.color: vector of color character string ruling the dot colors and the dot display. See the example section below for easier understanding of the rules described here
    # If NULL, no dots plotted
    # If "same", the dots will have the same colors as the respective boxplots
    # Otherwise, as in the rule (1), (2) or (3) described in the categ.color argument, except that in the possibility (3), the rule "a single color per class of categ and a single class of categ per color", does not have to be respected (for instance, each dot can have a different color). Colors will also depend on the dot.categ argument. If dot.categ is NULL, then colors will be applied to each class of the last column name specified in categ. If dot.categ is non-NULL, colors will be applied to each class of the column name specified in dot.categ. See examples
    # dot.categ: optional single character string of a column name (further referred to as categ3) of the data1 argument. This column of data1 will be used to generate a legend for dots, in addition to the legend for boxes. See the dot.color argument for details about the way the legend is built using the two dot.categ and dot.color arguments. If NULL, no legend created and the colors of dots will depend on dot.color and categ arguments (as explained in the dot.color argument)
    # dot.categ.class.order: optional vector of character strings indicating the order of the classes of categ3 (see the dot.categ argument). If dot.categ is non-NULL and dot.categ.class.order is NULL, classes are displayed in the legend according to the alphabetical order. Ignored if dot.categ is NULL
    # dot.legend.name: optional character string of the legend title for categ3 (see the dot.categ argument). If dot.legend.name == NULL, dot.categ value is used (name of the column in data1). Write "" if no legend required. Ignored if dot.categ is NULL
    # dot.tidy: logical. Nice dot spreading? If TRUE, use the geom_dotplot() function for a nice representation. WARNING: change the true quantitative coordinates of dots (i.e., y-axis values for vertical display) because of binning. Thus, the gain in aestheticism is associated with a loss in precision that can be very important. If FALSE, dots are randomly spread on the qualitative axis, using the dot.jitter argument (see below) keeping the true quantitative coordinates
    # dot.tidy.bin.nb: positive integer indicating the number of bins (i.e., nb of separations) of the y.lim range. Each dot will then be put in one of the bin, with a diameter of the width of the bin. In other words, increase the number of bins to have smaller dots. Not considered if dot.tidy is FALSE
    # dot.jitter: numeric value (from 0 to 1) of random dot horizontal dispersion (for vertical display), with 0 meaning no dispersion and 1 meaning dispersion in the corresponding box width interval. Not considered if dot.tidy is TRUE
    # dot.seed: integer value that set the random seed. Using the same number will generate the same dot jittering. Write NULL to have different jittering each time the same instruction is run. Ignored if dot.tidy is TRUE
    # dot.size: numeric value of dot diameter in mm. Not considered if dot.tidy is TRUE
    # dot.alpha: numeric value (from 0 to 1) of dot transparency (full transparent to full opaque, respectively)
    # dot.border.size: numeric value of border dot width in mm. Write zero for no dot border. If dot.tidy is TRUE, value 0 remove the border and other values leave the border without size control (geom_doplot() feature)
    # dot.border.color: single character color string defining the color of the dot border (same color for all the dots, whatever their categories). If dot.border.color == NULL, the border color will be the same as the dot color. A single integer is also accepted instead of a character string, that will be processed by ggpalette()
    # x.lab: a character string or expression for x-axis legend. If NULL, character string of categ1 (see the categ argument for categ1 and categ2 description)
    # x.angle: integer value of the text angle for the x-axis numbers, using the same rules as in ggplot2. Positive values for counterclockwise rotation: 0 for horizontal, 90 for vertical, 180 for upside down etc. Negative values for clockwise rotation: 0 for horizontal, -90 for vertical, -180 for upside down etc.
    # y.lab: a character string or expression for y-axis legend. If NULL, character string of the y argument
    # y.lim: 2 numeric values indicating the range of the y-axis. Order matters (for inverted axis). If NULL, the range of the x column name of data1 will be used. 
    # y.log: either "no", "log2" (values in the y argument column of the data1 data frame will be log2 transformed and y-axis will be log2 scaled) or "log10" (values in the y argument column of the data1 data frame will be log10 transformed and y-axis will be log10 scaled). WARNING: not possible to have horizontal boxes with a log axis, due to a bug in ggplot2 (see https://github.com/tidyverse/ggplot2/issues/881)
    # y.tick.nb: approximate number of desired values labeling the y-axis (i.e., main ticks, see the n argument of the the cute::saferGraph::scale2() function). If NULL and if y.log is "no", then the number of labeling values is set by ggplot2. If NULL and if y.log is "log2" or "log10", then the number of labeling values corresponds to all the exposant integers in the y.lim range (e.g., 10^1, 10^2 and 10^3, meaning 3 main ticks for y.lim = c(9, 1200)). WARNING: if non-NULL and if y.log is "log2" or "log10", labeling can be difficult to read (e.g., ..., 10^2, 10^2.5, 10^3, ...)
    # y.second.tick.nb: number of desired secondary ticks between main ticks. Ignored if y.log is other than "no" (log scale plotted). Use argument return = TRUE and see $plot$y.second.tick.values to have the values associated to secondary ticks. IF NULL, no secondary ticks
    # y.include.zero: logical. Does y.lim range include 0? Ignored if y.log is "log2" or "log10"
    # y.top.extra.margin: single proportion (between 0 and 1) indicating if extra margins must be added to y.lim. If different from 0, add the range of the axis multiplied by y.top.extra.margin (e.g., abs(y.lim[2] - y.lim[1]) * y.top.extra.margin) to the top of y-axis
    # y.bottom.extra.margin: idem as y.top.extra.margin but to the bottom of y-axis
    # stat.pos: add the median number above the corresponding box. Either NULL (no number shown), "top" (at the top of the plot region) or "above" (above each box)
    # stat.mean: logical. Display mean numbers instead of median numbers? Ignored if stat.pos is NULL
    # stat.size: numeric value of the stat font size in mm. Ignored if stat.pos is NULL
    # stat.dist: numeric value of the stat distance in percentage of the y-axis range (stat.dist = 5 means move the number displayed at 5% of the y-axis range). Ignored if stat.pos is NULL or "top"
    # stat.angle: integer value of the angle of stat, using the same rules as in ggplot2. Positive values for counterclockwise rotation: 0 for horizontal, 90 for vertical, 180 for upside down etc. Negative values for clockwise rotation: 0 for horizontal, -90 for vertical, -180 for upside down etc.
    # vertical: logical. Vertical boxes? WARNING: will be automatically set to TRUE if y.log argument is other than "no". Indeed, not possible to have horizontal boxes with a log axis, due to a bug in ggplot2 (see https://github.com/tidyverse/ggplot2/issues/881)
    # text.size: numeric value of the font size of the (1) axis numbers, (2) axis labels and (3) texts in the graphic legend (in mm)
    # title: character string of the graph title
    # title.text.size: numeric value of the title font size in mm
    # legend.show: logical. Show legend? Not considered if categ argument is NULL, because this already generate no legend, excepted if legend.width argument is non-NULL. In that specific case (categ is NULL, legend.show is TRUE and legend.width is non-NULL), an empty legend space is created. This can be useful when desiring graphs of exactly the same width, whatever they have legends or not
    # legend.width: single proportion (between 0 and 1) indicating the relative width of the legend sector (on the right of the plot) relative to the width of the plot. Value 1 means that the window device width is split in 2, half for the plot and half for the legend. Value 0 means no room for the legend, which will overlay the plot region. Write NULL to inactivate the legend sector. In such case, ggplot2 will manage the room required for the legend display, meaning that the width of the plotting region can vary between graphs, depending on the text in the legend
    # article: logical. If TRUE, use an article theme (article like). If FALSE, use a classic related ggplot theme. Use the add argument (e.g., add = "+ggplot2::theme_classic()" for the exact classic ggplot theme
    # grid: logical. Draw lines in the background to better read the box values? Not considered if article == FALSE (grid systematically present)
    # add: character string allowing to add more ggplot2 features (dots, lines, themes, facet, etc.). Ignored if NULL
    # WARNING: (1) the string must start with "+", (2) the string must finish with ")" and (3) each function must be preceded by "ggplot2::". Example: "+ ggplot2::coord_flip() + ggplot2::theme_bw()"
    # If the character string contains the "ggplot2::theme" string, then the article argument of ggbox() (see above) is ignored with a warning. In addition, some arguments can be overwritten, like x.angle (check all the arguments)
    # Handle the add argument with caution since added functions can create conflicts with the preexisting internal ggplot2 functions
    # WARNING: the call of objects inside the quotes of add can lead to an error if the name of these objects are some of the ggbox() arguments. Indeed, the function will use the internal argument instead of the global environment object. Example article <- "a" in the working environment and add = '+ ggplot2::ggtitle(article)'. The risk here is to have TRUE as title. To solve this, use add = '+ ggplot2::ggtitle(get("article", envir = .GlobalEnv))'
    # return: logical. Return the graph parameters?
    # return.ggplot: logical. Return the ggplot object in the output list? Ignored if return argument is FALSE. WARNING: always assign the ggbox() function (e.g., a <- ggbox()) if return.ggplot argument is TRUE, otherwise, double plotting is performed. See $ggplot in the RETURN section below for more details
    # return.gtable: logical. Return the ggplot object as gtable of grobs in the output list? Ignored if plot argument is FALSE. Indeed, the graph must be plotted to get the grobs dispositions. See $gtable in the RETURN section below for more details
    # plot: logical. Plot the graphic? If FALSE and return argument is TRUE, graphical parameters and associated warnings are provided without plotting
    # warn.print: logical. Print warnings at the end of the execution? ? If FALSE, warning messages are never printed, but can still be recovered in the returned list. Some of the warning messages (those delivered by the internal ggplot2 functions) are not apparent when using the argument plot = FALSE
    # lib.path: character string indicating the absolute path of the required packages (see below). if NULL, the function will use the R library default folders
    # RETURN
    # A boxplot if plot argument is TRUE
    # A list of the graph info if return argument is TRUE:
    # $data: the initial data with graphic information added
    # $stat: the graphic statistics (mostly equivalent to ggplot_build()$data[[2]])
    # $removed.row.nb: which rows have been removed due to NA/Inf detection in y and categ columns (NULL if no row removed)
    # $removed.rows: removed rows (NULL if no row removed)
    # $plot: the graphic box and dot coordinates
    # $dots: dot coordinates
    # $main.box: coordinates of boxes
    # $median: median coordinates
    # $sup.whisker: coordinates of top whiskers (y for base and y.end for extremities)
    # $inf.whisker: coordinates of bottom whiskers (y for base and y.end for extremities)
    # $sup.whisker.edge: coordinates of top whisker edges (x and xend)
    # $inf.whisker.edge: coordinates of bottom whisker edges(x and xend)
    # $mean: diamond mean coordinates (only if box.mean argument is TRUE)
    # $stat.pos: coordinates of stat numbers (only if stat.pos argument is not NULL)
    # y.second.tick.positions: coordinates of secondary ticks (only if y.second.tick.nb argument is non-NULL or if y.log argument is different from "no")
    # y.second.tick.values: values of secondary ticks. NULL except if y.second.tick.nb argument is non-NULL or if y.log argument is different from "no")
    # $panel: the variable names used for the panels (NULL if no panels). WARNING: NA can be present according to ggplot2 upgrade to v3.3.0
    # $axes: the x-axis and y-axis info
    
    # $warn: the warning messages. Use cat() for proper display. NULL if no warning. WARNING: warning messages delivered by the internal ggplot2 functions are not apparent when using the argument plot = FALSE
    # $ggplot: ggplot object that can be used for reprint (use print(...$ggplot) or update (use ...$ggplot + ggplot2::...). NULL if return.ggplot argument is FALSE. Of note, a non-NULL $ggplot in the output list is sometimes annoying as the manipulation of this list prints the plot
    # $gtable: gtable object that can be used for reprint (use gridExtra::grid.arrange(...$ggplot) or with additionnal grobs (see the grob decomposition in the examples). NULL if return.ggplot argument is FALSE. Contrary to $ggplot, a non-NULL $gtable in the output list is not annoying as the manipulation of this list does not print the plot
    # REQUIRED PACKAGES
    # ggplot2
    # gridExtra
    # lemon (in case of use in the add argument)
    # scales
    # REQUIRED FUNCTIONS FROM THE cute PACKAGE
    # saferDev::arg_check()
    # fun_comp_1d()
    # saferTool::comp_2d()
    # fun_gg_just()
    # ggpalette()
    # saferGraph::inter_ticks()
    # saferTool::name_change()
    # fun_pack()
    # saferTool::round2()
    # saferGraph::scale2()
    # EXAMPLE
    # set.seed(1) ; obs1 <- data.frame(Time = c(rnorm(20, 100, 10), rnorm(20, 200, 50), rnorm(20, 500, 60), rnorm(20, 100, 50)), Categ1 = rep(c("CAT", "DOG"), times = 40), Categ2 = rep(c("A", "B", "C", "D"), each = 20), Color1 = rep(c("coral", "lightblue"), times = 40), Color2 = rep(c("#9F2108", "#306100", "#007479", "#8500C0"), each = 20), stringsAsFactors = TRUE) ; set.seed(NULL) ; ggbox(data1 = obs1, y = "Time", categ = "Categ1")
    # see http
    # DEBUGGING
    # set.seed(1) ; obs1 <- data.frame(Time = c(rnorm(10), rnorm(10) + 2), Categ1 = rep(c("G", "H"), each = 10), stringsAsFactors = TRUE) ; set.seed(NULL) ; obs1$Time[1:10] <- NA ; data1 = obs1 ; y = "Time" ; categ = c("Categ1") ; categ.class.order = NULL ; categ.color = NULL ; box.legend.name = NULL ; box.fill = FALSE ; box.width = 0.5 ; box.space = 0.1 ; box.line.size = 0.75 ; box.notch = FALSE ; box.alpha = 1 ; box.mean = TRUE ; box.whisker.kind = "std" ; box.whisker.width = 0 ; dot.color = grey(0.25) ; dot.categ = NULL ; dot.categ.class.order = NULL ; dot.legend.name = NULL ; dot.tidy = FALSE ; dot.tidy.bin.nb = 50 ; dot.jitter = 0.5 ; dot.seed = 2 ; dot.size = 3 ; dot.alpha = 0.5 ; dot.border.size = 0.5 ; dot.border.color = NULL ; x.lab = NULL ; x.angle = 0 ; y.lab = NULL ; y.lim = NULL ; y.log = "no" ; y.tick.nb = NULL ; y.second.tick.nb = 1 ; y.include.zero = FALSE ; y.top.extra.margin = 0.05 ; y.bottom.extra.margin = 0.05 ; stat.pos = "top" ; stat.mean = FALSE ; stat.size = 4 ; stat.dist = 5 ; stat.angle = 0 ; vertical = TRUE ; text.size = 12 ; title = "" ; title.text.size = 8 ; legend.show = TRUE ; legend.width = 0.5 ; article = TRUE ; grid = FALSE ; add = NULL ; return = FALSE ; return.ggplot = FALSE ; return.gtable = TRUE ; plot = TRUE ; warn.print = FALSE ; lib.path = NULL
    # package name
    package.name <- "ggcute"
    # end package name
    # function name
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], "()") # function name with "()" paste, which split into a vector of three: c("::()", "package()", "function()") if "package::function()" is used.
    if(function.name[1] == "::()"){
        function.name <- function.name[3]
    }
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name
    # critical operator checking
    .base_op_check(external.function.name = function.name)
    # end critical operator checking
    # package checking
    # check of lib.path
    if( ! is.null(lib.path)){
        if( ! all(typeof(lib.path) == "character")){ # no na.rm = TRUE with typeof
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT MUST BE A VECTOR OF CHARACTERS:\n", paste(lib.path, collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else if( ! all(dir.exists(lib.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", paste(lib.path, collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else{
            .libPaths(new = sub(x = lib.path, pattern = "/$|\\\\$", replacement = "")) # .libPaths(new = ) add path to default path. BEWARE: .libPaths() does not support / at the end of a submitted path. Thus check and replace last / or \\ in path
            lib.path <- .libPaths()
        }
    }else{
        lib.path <- .libPaths() # .libPaths(new = lib.path) # or .libPaths(new = c(.libPaths(), lib.path))
    }
    # end check of lib.path
    # check of the required function from the required packages
    .pack_and_function_check(
        fun = c(
            "ggplot2::aes",
            "ggplot2::aes_string",
            "ggplot2::annotate",
            "ggplot2::coord_cartesian",
            "ggplot2::coord_flip",
            "ggplot2::element_blank",
            "ggplot2::element_line",
            "ggplot2::element_rect",
            "ggplot2::element_text",
            "ggplot2::geom_boxplot",
            "ggplot2::geom_dotplot",
            "ggplot2::geom_path",
            "ggplot2::geom_point",
            "ggplot2::geom_polygon",
            "ggplot2::geom_segment",
            "ggplot2::geom_text",
            "ggplot2::ggplot",
            "ggplot2::ggplot_build",
            "ggplot2::ggtitle",
            "ggplot2::guides",
            "ggplot2::guide_legend",
            "ggplot2::position_dodge",
            "ggplot2::scale_discrete_manual",
            "ggplot2::scale_y_continuous",
            "ggplot2::theme",
            "ggplot2::theme_classic",
            "ggplot2::theme_void",
            "ggplot2::waiver",
            "ggplot2::xlab",
            "ggplot2::ylab",
            "gridExtra::grid.arrange",
            "scales::math_format",
            "scales::rescale_none",
            "scales::trans_format",
            "saferDev::arg_check", 
            "saferTool::comp_2d",  
            "saferTool::name_change", 
            "saferTool::round2", 
            "saferGraph::scale2",
            "saferGraph::inter_ticks"
        ),
        lib.path = lib.path,
        external.function.name = function.name
    )
    # end check of the required function from the required packages
    # end package checking









    # reserved words to avoid bugs (names of dataframe columns used in this function)
    reserved.words <- c("categ.check", "categ.color", "dot.color", "dot.categ", "dot.max", "dot.min", "group", "PANEL", "group.check", "MEAN", "tempo.categ1", "tempo.categ2", "text.max.pos", "text.min.pos", "x", "x.y", "y", "y.check", "y_from.dot.max", "ymax", "tidy_group", "binwidth")
    # end reserved words to avoid bugs (used in this function)
    # arg with no default values
    mandat.args <- c(
        "data1", 
        "y", 
        "categ"
    )
    tempo <- eval(parse(text = paste0("missing(", paste0(mandat.args, collapse = ") | missing("), ")")))
    if(any(tempo)){ # normally no NA for missing() output
        tempo.cat <- paste0("ERROR IN ", function.name, "\nFOLLOWING ARGUMENT", ifelse(sum(tempo, na.rm = TRUE) > 1, "S HAVE", "HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", paste0(mandat.args, collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    # argument primary checking
    argum.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- expression(argum.check <- c(argum.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$object.name))
    tempo <- saferDev::arg_check(data = data1, class = "data.frame", na.contain = TRUE, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = y, class = "vector", mode = "character", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = categ, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
    if( ! is.null(categ.class.order)){
        tempo <- saferDev::arg_check(data = categ.class.order, class = "list", fun.name = function.name) ; eval(ee)
    }else{
        # no saferDev::arg_check test here, it is just for checked.arg.names
        tempo <- saferDev::arg_check(data = categ.class.order, class = "vector")
        checked.arg.names <- c(checked.arg.names, tempo$object.name)
    }
    if( ! is.null(box.legend.name)){
        tempo <- saferDev::arg_check(data = box.legend.name, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
    }else{
        # no saferDev::arg_check test here, it is just for checked.arg.names
        tempo <- saferDev::arg_check(data = box.legend.name, class = "vector")
        checked.arg.names <- c(checked.arg.names, tempo$object.name)
    }
    if( ! is.null(categ.color)){
        tempo1 <- saferDev::arg_check(data = categ.color, class = "vector", mode = "character", na.contain = TRUE, fun.name = function.name)
        tempo2 <- saferDev::arg_check(data = categ.color, class = "factor", na.contain = TRUE, fun.name = function.name)
        checked.arg.names <- c(checked.arg.names, tempo2$object.name)
        if(tempo1$problem == TRUE & tempo2$problem == TRUE){
            tempo.check.color <- saferDev::arg_check(data = categ.color, class = "integer", double.as.integer.allowed = TRUE, na.contain = TRUE, neg.values = FALSE, fun.name = function.name)$problem
            if(tempo.check.color == TRUE){
                tempo.cat <- paste0("ERROR IN ", function.name, "\ncateg.color ARGUMENT MUST BE A FACTOR OR CHARACTER VECTOR OR POSITVE INTEGER VECTOR") # integer possible because dealt above
                text.check <- c(text.check, tempo.cat)
                argum.check <- c(argum.check, TRUE)
            }else if(any(categ.color == 0L, na.rm = TRUE)){
                tempo.cat <- paste0("ERROR IN ", function.name, "\ncateg.color ARGUMENT MUST BE A FACTOR OR CHARACTER VECTOR OR POSITVE INTEGER VECTOR") # integer possible because dealt above
                text.check <- c(text.check, tempo.cat)
                argum.check <- c(argum.check, TRUE)
            }
        }
    }else{
        # no saferDev::arg_check test here, it is just for checked.arg.names
        tempo <- saferDev::arg_check(data = categ.color, class = "vector")
        checked.arg.names <- c(checked.arg.names, tempo$object.name)
    }
    tempo <- saferDev::arg_check(data = box.fill, class = "vector", mode = "logical", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = box.width, prop = TRUE, length = 1, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = box.space, prop = TRUE, length = 1, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = box.line.size, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = box.notch, class = "vector", mode = "logical", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = box.alpha, prop = TRUE, length = 1, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = box.mean, class = "vector", mode = "logical", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = box.whisker.kind, options = c("no", "std", "max"), length = 1, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = box.whisker.width, prop = TRUE, length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(dot.color)){
        tempo1 <- saferDev::arg_check(data = dot.color, class = "vector", mode = "character", na.contain = TRUE, fun.name = function.name)
        tempo2 <- saferDev::arg_check(data = dot.color, class = "factor", na.contain = TRUE, fun.name = function.name)
        checked.arg.names <- c(checked.arg.names, tempo2$object.name)
        if(tempo1$problem == TRUE & tempo2$problem == TRUE){
            tempo.check.color <- saferDev::arg_check(data = dot.color, class = "integer", double.as.integer.allowed = TRUE, na.contain = TRUE, neg.values = FALSE, fun.name = function.name)$problem
            if(tempo.check.color == TRUE){
                tempo.cat <- paste0("ERROR IN ", function.name, "\ndot.color MUST BE A FACTOR OR CHARACTER VECTOR OR POSITVE INTEGER VECTOR") # integer possible because dealt above
                text.check <- c(text.check, tempo.cat)
                argum.check <- c(argum.check, TRUE)
            }else if(any(dot.color == 0L, na.rm = TRUE)){
                tempo.cat <- paste0("ERROR IN ", function.name, "\ndot.color ARGUMENT MUST BE A FACTOR OR CHARACTER VECTOR OR POSITVE INTEGER VECTOR") # integer possible because dealt above
                text.check <- c(text.check, tempo.cat)
                argum.check <- c(argum.check, TRUE)
            }
        }
    }else{
        # no saferDev::arg_check test here, it is just for checked.arg.names
        tempo <- saferDev::arg_check(data = dot.color, class = "vector")
        checked.arg.names <- c(checked.arg.names, tempo$object.name)
    }
    if( ! is.null(dot.categ)){
        tempo <- saferDev::arg_check(data = dot.categ, class = "vector", mode = "character", length = 1, fun.name = function.name) ; eval(ee)
    }else{
        # no saferDev::arg_check test here, it is just for checked.arg.names
        tempo <- saferDev::arg_check(data = dot.categ, class = "vector")
        checked.arg.names <- c(checked.arg.names, tempo$object.name)
    }
    if( ! is.null(dot.categ.class.order)){
        tempo <- saferDev::arg_check(data = dot.categ.class.order, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
    }else{
        # no saferDev::arg_check test here, it is just for checked.arg.names
        tempo <- saferDev::arg_check(data = dot.categ.class.order, class = "vector")
        checked.arg.names <- c(checked.arg.names, tempo$object.name)
    }
    if( ! is.null(dot.legend.name)){
        tempo <- saferDev::arg_check(data = dot.legend.name, class = "vector", mode = "character", length = 1, fun.name = function.name) ; eval(ee)
    }else{
        # no saferDev::arg_check test here, it is just for checked.arg.names
        tempo <- saferDev::arg_check(data = dot.legend.name, class = "vector")
        checked.arg.names <- c(checked.arg.names, tempo$object.name)
    }
    tempo <- saferDev::arg_check(data = dot.tidy, class = "vector", mode = "logical", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = dot.tidy.bin.nb, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, neg.values = FALSE, fun.name = function.name) ; eval(ee)
    if(tempo$problem == FALSE){
        if(dot.tidy.bin.nb == 0L){ # length and NA checked above
            tempo.cat <- paste0("ERROR IN ", function.name, "\ndot.tidy.bin.nb ARGUMENT MUST BE A NON-NULL AND POSITVE INTEGER VALUE") # integer possible because dealt above
            text.check <- c(text.check, tempo.cat)
            argum.check <- c(argum.check, TRUE)
        }
    }
    tempo <- saferDev::arg_check(data = dot.jitter, prop = TRUE, length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(dot.seed)){
        tempo <- saferDev::arg_check(data = dot.seed, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, neg.values = TRUE, fun.name = function.name) ; eval(ee)
    }else{
        # no saferDev::arg_check test here, it is just for checked.arg.names
        tempo <- saferDev::arg_check(data = dot.seed, class = "vector")
        checked.arg.names <- c(checked.arg.names, tempo$object.name)
    }
    tempo <- saferDev::arg_check(data = dot.size, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = dot.alpha, prop = TRUE, length = 1, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = dot.border.size, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
    if( ! is.null(dot.border.color)){
        tempo1 <- saferDev::arg_check(data = dot.border.color, class = "vector", mode = "character", length = 1, fun.name = function.name)
        tempo2 <- saferDev::arg_check(data = dot.border.color, class = "vector", typeof = "integer", double.as.integer.allowed = TRUE, length = 1, fun.name = function.name)
        checked.arg.names <- c(checked.arg.names, tempo2$object.name)
        if(tempo1$problem == TRUE & tempo2$problem == TRUE){
            tempo.cat <- paste0("ERROR IN ", function.name, "\ndot.border.color ARGUMENT MUST BE (1) A HEXADECIMAL COLOR STRING STARTING BY #, OR (2) A COLOR NAME GIVEN BY colors(), OR (3) AN INTEGER VALUE")
            text.check <- c(text.check, tempo.cat)
            argum.check <- c(argum.check, TRUE)
        }else if(tempo1$problem == FALSE & tempo2$problem == TRUE){
            if( ! all(dot.border.color %in% colors() | grepl(pattern = "^#", dot.border.color), na.rm = TRUE)){
                tempo.cat <- paste0("ERROR IN ", function.name, "\ndot.border.color ARGUMENT MUST BE (1) A HEXADECIMAL COLOR STRING STARTING BY #, OR (2) A COLOR NAME GIVEN BY colors(), OR (3) AN INTEGER VALUE")
                text.check <- c(text.check, tempo.cat)
                argum.check <- c(argum.check, TRUE)
            }
        }
    }else{
        # no saferDev::arg_check test here, it is just for checked.arg.names
        tempo <- saferDev::arg_check(data = dot.border.color, class = "vector")
        checked.arg.names <- c(checked.arg.names, tempo$object.name)
    }
    if( ! is.null(x.lab)){
        tempo1 <- saferDev::arg_check(data = x.lab, class = "expression", length = 1, fun.name = function.name)
        tempo2 <- saferDev::arg_check(data = x.lab,  class = "vector", mode = "character", length = 1, fun.name = function.name)
        checked.arg.names <- c(checked.arg.names, tempo2$object.name)
        if(tempo1$problem == TRUE & tempo2$problem == TRUE){
            tempo.cat <- paste0("ERROR IN ", function.name, "\nx.lab ARGUMENT MUST BE A SINGLE CHARACTER STRING OR EXPRESSION")
            text.check <- c(text.check, tempo.cat)
            argum.check <- c(argum.check, TRUE)
        }
    }else{
        # no saferDev::arg_check test here, it is just for checked.arg.names
        tempo <- saferDev::arg_check(data = x.lab, class = "vector")
        checked.arg.names <- c(checked.arg.names, tempo$object.name)
    }
    tempo <- saferDev::arg_check(data = x.angle, class = "vector", typeof = "integer", double.as.integer.allowed = TRUE, length = 1, neg.values = TRUE, fun.name = function.name) ; eval(ee)
    if( ! is.null(y.lab)){
        tempo1 <- saferDev::arg_check(data = y.lab, class = "expression", length = 1, fun.name = function.name)
        tempo2 <- saferDev::arg_check(data = y.lab,  class = "vector", mode = "character", length = 1, fun.name = function.name)
        checked.arg.names <- c(checked.arg.names, tempo2$object.name)
        if(tempo1$problem == TRUE & tempo2$problem == TRUE){
            tempo.cat <- paste0("ERROR IN ", function.name, "\ny.lab ARGUMENT MUST BE A SINGLE CHARACTER STRING OR EXPRESSION")
            text.check <- c(text.check, tempo.cat)
            argum.check <- c(argum.check, TRUE)
        }
    }else{
        # no saferDev::arg_check test here, it is just for checked.arg.names
        tempo <- saferDev::arg_check(data = y.lab, class = "vector")
        checked.arg.names <- c(checked.arg.names, tempo$object.name)
    }
    if( ! is.null(y.lim)){
        tempo <- saferDev::arg_check(data = y.lim, class = "vector", mode = "numeric", length = 2, fun.name = function.name) ; eval(ee)
        if(tempo$problem == FALSE){
            if(any(is.infinite(y.lim))){ # normally no NA for is.infinite() output
                tempo.cat <- paste0("ERROR IN ", function.name, "\ny.lim ARGUMENT CANNOT CONTAIN -Inf OR Inf VALUES")
                text.check <- c(text.check, tempo.cat)
                argum.check <- c(argum.check, TRUE)
            }
        }
    }else{
        # no saferDev::arg_check test here, it is just for checked.arg.names
        tempo <- saferDev::arg_check(data = y.lim, class = "vector")
        checked.arg.names <- c(checked.arg.names, tempo$object.name)
    }
    tempo <- saferDev::arg_check(data = y.log, options = c("no", "log2", "log10"), length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(y.tick.nb)){
        tempo <- saferDev::arg_check(data = y.tick.nb, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, fun.name = function.name) ; eval(ee)
        if(tempo$problem == FALSE){
            if(y.tick.nb < 0){
                tempo.cat <- paste0("ERROR IN ", function.name, "\ny.tick.nb ARGUMENT MUST BE A NON NULL POSITIVE INTEGER")
                text.check <- c(text.check, tempo.cat)
                argum.check <- c(argum.check, TRUE)
            }
        }
    }else{
        # no saferDev::arg_check test here, it is just for checked.arg.names
        tempo <- saferDev::arg_check(data = y.tick.nb, class = "vector")
        checked.arg.names <- c(checked.arg.names, tempo$object.name)
    }
    if( ! is.null(y.second.tick.nb)){
        tempo <- saferDev::arg_check(data = y.second.tick.nb, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, fun.name = function.name) ; eval(ee)
        if(tempo$problem == FALSE){
            if(y.second.tick.nb <= 0){
                tempo.cat <- paste0("ERROR IN ", function.name, "\ny.second.tick.nb ARGUMENT MUST BE A NON NULL POSITIVE INTEGER")
                text.check <- c(text.check, tempo.cat)
                argum.check <- c(argum.check, TRUE)
            }
        }
    }else{
        # no saferDev::arg_check test here, it is just for checked.arg.names
        tempo <- saferDev::arg_check(data = y.second.tick.nb, class = "vector")
        checked.arg.names <- c(checked.arg.names, tempo$object.name)
    }
    tempo <- saferDev::arg_check(data = y.include.zero, class = "vector", mode = "logical", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = y.top.extra.margin, prop = TRUE, length = 1, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = y.bottom.extra.margin, prop = TRUE, length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(stat.pos)){
        tempo <- saferDev::arg_check(data = stat.pos, options = c("top", "above"), length = 1, fun.name = function.name) ; eval(ee)
    }else{
        # no saferDev::arg_check test here, it is just for checked.arg.names
        tempo <- saferDev::arg_check(data = stat.pos, class = "vector")
        checked.arg.names <- c(checked.arg.names, tempo$object.name)
    }
    tempo <- saferDev::arg_check(data = stat.mean, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = stat.size, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = stat.dist, class = "vector", mode = "numeric", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = stat.angle, class = "vector", typeof = "integer", double.as.integer.allowed = TRUE, length = 1, neg.values = TRUE, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = vertical, class = "vector", mode = "logical", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = text.size, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = title, class = "vector", mode = "character", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = title.text.size, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = legend.show, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(legend.width)){
        tempo <- saferDev::arg_check(data = legend.width, prop = TRUE, length = 1, fun.name = function.name) ; eval(ee)
    }else{
        # no saferDev::arg_check test here, it is just for checked.arg.names
        tempo <- saferDev::arg_check(data = legend.width, class = "vector")
        checked.arg.names <- c(checked.arg.names, tempo$object.name)
    }
    tempo <- saferDev::arg_check(data = article, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = grid, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(add)){
        tempo <- saferDev::arg_check(data = add, class = "vector", mode = "character", length = 1, fun.name = function.name) ; eval(ee)
    }else{
        # no saferDev::arg_check test here, it is just for checked.arg.names
        tempo <- saferDev::arg_check(data = add, class = "vector")
        checked.arg.names <- c(checked.arg.names, tempo$object.name)
    }
    tempo <- saferDev::arg_check(data = return, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = return.ggplot, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = return.gtable, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = plot, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = warn.print, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(lib.path)){
        tempo <- saferDev::arg_check(data = lib.path, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
        if(tempo$problem == FALSE){
            if( ! all(dir.exists(lib.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
                tempo.cat <- paste0("ERROR IN ", function.name, "\nDIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", paste(lib.path, collapse = "\n"))
                text.check <- c(text.check, tempo.cat)
                argum.check <- c(argum.check, TRUE)
            }
        }
    }else{
        # no saferDev::arg_check test here, it is just for checked.arg.names
        tempo <- saferDev::arg_check(data = lib.path, class = "vector")
        checked.arg.names <- c(checked.arg.names, tempo$object.name)
    }
    if( ! is.null(argum.check)){
        if(any(argum.check) == TRUE){
            stop(paste0("\n\n================\n\n", paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.7/r_debugging_tools-v1.7.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_saferDev::arg_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using saferDev::arg_check()
    # end argument primary checking
    # second round of checking and data preparation
    # management of NA arguments
    if( ! (all(class(arg.user.setting) == "list") & length(arg.user.setting) == 0)){
        tempo.arg <- names(arg.user.setting) # values provided by the user
        tempo.log <- suppressWarnings(sapply(lapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = is.na), FUN = any)) & lapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = length) == 1L # no argument provided by the user can be just NA
        if(any(tempo.log) == TRUE){ # normally no NA because is.na() used here
            tempo.cat <- paste0("ERROR IN ", function.name, "\n", ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", paste0(tempo.arg[tempo.log], collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    # management of NULL arguments
    tempo.arg <-c(
        "data1", 
        "y", 
        "categ", 
        "box.fill", 
        "box.width", 
        "box.space", 
        "box.line.size", 
        "box.notch", 
        "box.alpha", 
        "box.mean", 
        "box.whisker.kind", 
        "box.whisker.width", 
        # "dot.color", # inactivated because can be null
        "dot.tidy", 
        "dot.tidy.bin.nb", 
        "dot.jitter", 
        # "dot.seed", # inactivated because can be null
        "dot.size", 
        "dot.alpha", 
        "dot.border.size", 
        "x.angle", 
        "y.log", 
        # "y.second.tick.nb", # inactivated because can be null
        "y.include.zero", 
        "y.top.extra.margin", 
        "y.bottom.extra.margin", 
        # "stat.pos", # inactivated because can be null
        "stat.mean", 
        "stat.size", 
        "stat.dist", 
        "stat.angle", 
        "vertical", 
        "text.size", 
        "title", 
        "title.text.size", 
        "legend.show", 
        # "legend.width", # inactivated because can be null
        "article", 
        "grid", 
        "return", 
        "return.ggplot", 
        "return.gtable", 
        "plot", 
        "warn.print"
    )
    tempo.log <- sapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = is.null)
    if(any(tempo.log) == TRUE){# normally no NA with is.null()
        tempo.cat <- paste0("ERROR IN ", function.name, ":\n", ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end management of NULL arguments
    # code that protects set.seed() in the global environment
    # see also Protocol 100-rev0 Parallelization in R.docx
    if(exists(".Random.seed", envir = .GlobalEnv)){ # if .Random.seed does not exists, it means that no random operation has been performed yet in any R environment
        tempo.random.seed <- .Random.seed
        on.exit(assign(".Random.seed", tempo.random.seed, env = .GlobalEnv))
    }else{
        on.exit(set.seed(NULL)) # inactivate seeding -> return to complete randomness
    }
    set.seed(dot.seed)
    # end code that protects set.seed() in the global environment
    # warning initiation
    ini.warning.length <- options()$warning.length
    options(warning.length = 8170)
    warn <- NULL
    warn.count <- 0
    # end warning initiation
    # other checkings
    if(any(duplicated(names(data1)), na.rm = TRUE)){
        tempo.cat <- paste0("ERROR IN ", function.name, "\nDUPLICATED COLUMN NAMES OF data1 ARGUMENT NOT ALLOWED:\n", paste(names(data1)[duplicated(names(data1))], collapse = " "))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if( ! (y %in% names(data1))){
        tempo.cat <- paste0("ERROR IN ", function.name, "\ny ARGUMENT MUST BE A COLUMN NAME OF data1")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }else{
        tempo <- saferDev::arg_check(data = data1[, y], data.name = "y COLUMN OF data1", class = "vector", mode = "numeric", na.contain = TRUE, fun.name = function.name)
        if(tempo$problem == TRUE){
            tempo.cat <- paste0("ERROR IN ", function.name, "\ny ARGUMENT MUST BE NUMERIC COLUMN IN data1")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    if(length(categ) > 2){
        tempo.cat <- paste0("ERROR IN ", function.name, "\ncateg ARGUMENT CANNOT HAVE MORE THAN 2 COLUMN NAMES OF data1")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }else if( ! all(categ %in% names(data1))){ # all() without na.rm -> ok because categ cannot be NA (tested above)
        tempo.cat <- paste0("ERROR IN ", function.name, "\ncateg ARGUMENT MUST BE COLUMN NAMES OF data1. HERE IT IS:\n", paste(categ, collapse = " "))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if(length(dot.categ) > 1){
        tempo.cat <- paste0("ERROR IN ", function.name, "\ndot.categ ARGUMENT CANNOT HAVE MORE THAN 1 COLUMN NAMES OF data1")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }else if( ! all(dot.categ %in% names(data1))){ # all() without na.rm -> ok because dot.categ cannot be NA (tested above)
        tempo.cat <- paste0("ERROR IN ", function.name, "\ndot.categ ARGUMENT MUST BE COLUMN NAMES OF data1. HERE IT IS:\n", paste(dot.categ, collapse = " "))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # reserved word checking
    if(any(names(data1) %in% reserved.words, na.rm = TRUE)){
        if(any(duplicated(names(data1)), na.rm = TRUE)){
            tempo.cat <- paste0("ERROR IN ", function.name, "\nDUPLICATED COLUMN NAMES OF data1 ARGUMENT NOT ALLOWED:\n", paste(names(data1)[duplicated(names(data1))], collapse = " "))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if( ! is.null(dot.categ)){
            if(dot.categ %in% categ){
                reserved.words <- c(reserved.words, paste0(dot.categ, "_DOT")) # paste0(dot.categ, "_DOT") is added to the reserved words because in such situation, a new column will be added to data1 that is named paste0(dot.categ, "_DOT")
            }
        }
        tempo.output <- saferTool::name_change(names(data1), reserved.words)
        for(i2 in 1:length(tempo.output$ini)){ # a loop to be sure to take the good ones
            names(data1)[names(data1) == tempo.output$ini[i2]] <- tempo.output$post[i2]
            if(any(y == tempo.output$ini[i2])){ # any() without na.rm -> ok because y cannot be NA (tested above)
                y[y == tempo.output$ini[i2]] <- tempo.output$post[i2]
                warn.count <- warn.count + 1
                tempo.warn <- paste0("(", warn.count,") IN y ARGUMENT (COLUMN NAMES OF data1 ARGUMENT),\n", tempo.output$ini[i2], " HAS BEEN REPLACED BY ", tempo.output$post[i2], "\nBECAUSE RISK OF BUG AS SOME NAMES IN y ARGUMENT ARE RESERVED WORD USED BY THE ", function.name, " FUNCTION")
                warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
            }
            # WARNING: names of y argument potentially replaced
            if(any(categ == tempo.output$ini[i2])){ # any() without na.rm -> ok because categ cannot be NA (tested above)
                categ[categ == tempo.output$ini[i2]] <- tempo.output$post[i2]
                warn.count <- warn.count + 1
                tempo.warn <- paste0("(", warn.count,") IN categ ARGUMENT (COLUMN NAMES OF data1 ARGUMENT),\n", tempo.output$ini[i2], " HAS BEEN REPLACED BY ", tempo.output$post[i2], "\nBECAUSE RISK OF BUG AS SOME NAMES IN categ ARGUMENT ARE RESERVED WORD USED BY THE ", function.name, " FUNCTION")
                warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
            }
            # WARNING: names of categ argument potentially replaced
            if( ! is.null(dot.categ)){
                if(any(dot.categ == tempo.output$ini[i2])){ # any() without na.rm -> ok because dot.categ cannot be NA (tested above)
                    dot.categ[dot.categ == tempo.output$ini[i2]] <- tempo.output$post[i2]
                    warn.count <- warn.count + 1
                    tempo.warn <- paste0("(", warn.count,") IN dot.categ ARGUMENT (COLUMN NAMES OF data1 ARGUMENT),\n", tempo.output$ini[i2], " HAS BEEN REPLACED BY ", tempo.output$post[i2], "\nBECAUSE RISK OF BUG AS SOME NAMES IN dot.categ ARGUMENT ARE RESERVED WORD USED BY THE ", function.name, " FUNCTION")
                    warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                }
            }
            # WARNING: names of dot.categ argument potentially replaced
        }
        warn.count <- warn.count + 1
        tempo.warn <- paste0("(", warn.count,") REGARDING COLUMN NAMES REPLACEMENT, THE NAMES\n", paste(tempo.output$ini, collapse = " "), "\nHAVE BEEN REPLACED BY\n", paste(tempo.output$post, collapse = " "))
        warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        if( ! (is.null(add) | is.null(tempo.output$ini))){
            if(grepl(x = add, pattern = paste(tempo.output$ini, collapse = "|"))){
                tempo.cat <- paste0("ERROR IN ", function.name, "\nDETECTION OF COLUMN NAMES OF data1 IN THE add ARGUMENT STRING, THAT CORRESPOND TO RESERVED STRINGS FOR ", function.name, "\nCOLUMN NAMES HAVE TO BE CHANGED\nTHE PROBLEMATIC COLUMN NAMES ARE SOME OF THESE NAMES:\n", paste(tempo.output$ini, collapse = " "), "\nIN THE DATA FRAME OF data1 AND IN THE STRING OF add ARGUMENT, TRY TO REPLACE NAMES BY:\n", paste(tempo.output$post, collapse = " "), "\n\nFOR INFORMATION, THE RESERVED WORDS ARE:\n", paste(reserved.words, collapse = "\n"))
                stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
        }
    }
    if( ! (is.null(add))){
        if(any(sapply(X = arg.names, FUN = grepl, x = add), na.rm = TRUE)){
            warn.count <- warn.count + 1
            tempo.warn <- paste0("(", warn.count,") NAMES OF ", function.name, " ARGUMENTS DETECTED IN THE add STRING:\n", paste(arg.names[sapply(X = arg.names, FUN = grepl, x = add)], collapse = "\n"), "\nRISK OF WRONG OBJECT USAGE INSIDE ", function.name)
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        }
    }
    # end reserved word checking
    # verif of add
    if( ! is.null(add)){
        if( ! grepl(pattern = "^\\s*\\+", add)){ # check that the add string start by +
            tempo.cat <- paste0("ERROR IN ", function.name, "\nadd ARGUMENT MUST START WITH \"+\": ", paste(unique(add), collapse = " "))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else if( ! grepl(pattern = "(ggplot2|lemon)\\s*::", add)){ #
            tempo.cat <- paste0("ERROR IN ", function.name, "\nFOR EASIER FUNCTION DETECTION, add ARGUMENT MUST CONTAIN \"ggplot2::\" OR \"lemon::\" IN FRONT OF EACH GGPLOT2 FUNCTION: ", paste(unique(add), collapse = " "))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else if( ! grepl(pattern = ")\\s*$", add)){ # check that the add string finished by )
            tempo.cat <- paste0("ERROR IN ", function.name, "\nadd ARGUMENT MUST FINISH BY \")\": ", paste(unique(add), collapse = " "))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end verif of add
    # management of add containing facet
    facet.categ <- NULL
    if( ! is.null(add)){
        facet.check <- TRUE
        tempo <- unlist(strsplit(x = add, split = "\\s*\\+\\s*(ggplot2|lemon)\\s*::\\s*")) #
        tempo <- sub(x = tempo, pattern = "^facet_wrap", replacement = "ggplot2::facet_wrap")
        tempo <- sub(x = tempo, pattern = "^facet_grid", replacement = "ggplot2::facet_grid")
        tempo <- sub(x = tempo, pattern = "^facet_rep", replacement = "lemon::facet_rep")
        if(any(grepl(x = tempo, pattern = "ggplot2::facet_wrap|lemon::facet_rep_wrap"), na.rm = TRUE)){
            tempo1 <- suppressWarnings(eval(parse(text = tempo[grepl(x = tempo, pattern = "ggplot2::facet_wrap|lemon::facet_rep_wrap")])))
            facet.categ <- names(tempo1$params$facets)
            tempo.text <- "facet_wrap OR facet_rep_wrap"
            facet.check <- FALSE
        }else if(grepl(x = add, pattern = "ggplot2::facet_grid|lemon::facet_rep_grid")){
            tempo1 <- suppressWarnings(eval(parse(text = tempo[grepl(x = tempo, pattern = "ggplot2::facet_grid|lemon::facet_rep_grid")])))
            facet.categ <- c(names(tempo1$params$rows), names(tempo1$params$cols))
            tempo.text <- "facet_grid OR facet_rep_grid"
            facet.check <- FALSE
        }
        if(facet.check == FALSE & ! all(facet.categ %in% names(data1))){ # WARNING: all(facet.categ %in% names(data1)) is TRUE when facet.categ is NULL # all() without na.rm -> ok because facet.categ cannot be NA (tested above)
            tempo.cat <- paste0("ERROR IN ", function.name, "\nDETECTION OF \"", tempo.text, "\" STRING IN THE add ARGUMENT BUT PROBLEM OF VARIABLE DETECTION (COLUMN NAMES OF data1)\nTHE DETECTED VARIABLES ARE:\n", paste(facet.categ, collapse = " "), "\nTHE data1 COLUMN NAMES ARE:\n", paste(names(data1), collapse = " "), "\nPLEASE REWRITE THE add STRING AND RERUN")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of add containing facet
    # conversion of categ columns in data1 into factors
    for(i1 in 1:length(categ)){
        tempo1 <- saferDev::arg_check(data = data1[, categ[i1]], data.name = paste0("categ NUMBER ", i1, " OF data1"), class = "vector", mode = "character", na.contain = TRUE, fun.name = function.name)
        tempo2 <- saferDev::arg_check(data = data1[, categ[i1]], data.name = paste0("categ NUMBER ", i1, " OF data1"), class = "factor", na.contain = TRUE, fun.name = function.name)
        if(tempo1$problem == TRUE & tempo2$problem == TRUE){
            tempo.cat <- paste0("ERROR IN ", function.name, "\n", paste0("categ NUMBER ", i1, " OF data1"), " MUST BE A FACTOR OR CHARACTER VECTOR")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else if(tempo1$problem == FALSE){ # character vector
            if(box.alpha != 0){
                warn.count <- warn.count + 1
                tempo.warn <- paste0("(", warn.count,") IN categ NUMBER ", i1, " IN data1, THE CHARACTER COLUMN HAS BEEN CONVERTED TO FACTOR, WITH LEVELS ACCORDING TO THE ALPHABETICAL ORDER")
                warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
            }
        }
        data1[, categ[i1]] <- factor(data1[, categ[i1]]) # if already a factor, change nothing, if characters, levels according to alphabetical order
    }
    # OK: all the categ columns of data1 are factors from here
    # end conversion of categ columns in data1 into factors
    
    
    
    # management of log scale and Inf removal
    if(any(( ! is.finite(data1[, y])) & ( ! is.na(data1[, y])))){ # is.finite also detects NA: ( ! is.finite(data1[, y])) & ( ! is.na(data1[, y])) detects only Inf  # normally no NA with is.finite0() and is.na()
        warn.count <- warn.count + 1
        tempo.warn <- paste0("(", warn.count,") PRESENCE OF -Inf OR Inf VALUES IN THE ", y, " COLUMN OF THE data1 ARGUMENT AND CORRESPONDING ROWS REMOVED (SEE $removed.row.nb AND $removed.rows)")
        warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
    }
    data1.ini <- data1 # strictly identical to data1 except that in data1 y is log converted if and only if y.log != "no"
    if(y.log != "no"){
        tempo1 <- ! is.finite(data1[, y]) # where are initial NA and Inf
        data1[, y] <- suppressWarnings(get(y.log)(data1[, y]))# no env = sys.nframe(), inherit = FALSE in get() because look for function in the classical scope
        if(any( ! (tempo1 | is.finite(data1[, y])))){ # normally no NA with is.finite
            warn.count <- warn.count + 1
            tempo.warn <- paste0("(", warn.count,") LOG CONVERSION INTRODUCED -Inf OR Inf OR NaN VALUES IN THE ", y, " COLUMN OF THE data1 ARGUMENT AND CORRESPONDING ROWS REMOVED (SEE $removed.row.nb AND $removed.rows)")
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        }
    }
    # Inf removal
    if(any(( ! is.finite(data1[, y])) & ( ! is.na(data1[, y])))){ # is.finite also detects NA: ( ! is.finite(data1[, y])) & ( ! is.na(data1[, y])) detects only Inf # normally no NA with is.finite
        removed.row.nb <- which(( ! is.finite(data1[, y])) & ( ! is.na(data1[, y])))
        removed.rows <- data1.ini[removed.row.nb, ] # here data1.ini used to have the y = O rows that will be removed because of Inf creation after log transformation
        data1 <- data1[-removed.row.nb, ] #
        data1.ini <- data1.ini[-removed.row.nb, ] #
    }else{
        removed.row.nb <- NULL
        removed.rows <- data.frame(stringsAsFactors = FALSE)
    }
    # From here, data1 and data.ini have no more Inf
    # end Inf removal
    if(y.log != "no" & ! is.null(y.lim)){
        if(any(y.lim <= 0)){ # any() without na.rm -> ok because y.lim cannot be NA (tested above)
            tempo.cat <- paste0("ERROR IN ", function.name, "\ny.lim ARGUMENT CANNOT HAVE ZERO OR NEGATIVE VALUES WITH THE y.log ARGUMENT SET TO ", y.log, ":\n", paste(y.lim, collapse = " "))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else if(any( ! is.finite(if(y.log == "log10"){log10(y.lim)}else{log2(y.lim)}))){ # normally no NA with is.finite
            tempo.cat <- paste0("ERROR IN ", function.name, "\ny.lim ARGUMENT RETURNS INF/NA WITH THE y.log ARGUMENT SET TO ", y.log, "\nAS SCALE COMPUTATION IS ", ifelse(y.log == "log10", "log10", "log2"), ":\n", paste(if(y.log == "log10"){log10(y.lim)}else{log2(y.lim)}, collapse = " "))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    if(y.log != "no" & y.include.zero == TRUE){
        warn.count <- warn.count + 1
        tempo.warn <- paste0("(", warn.count,") y.log ARGUMENT SET TO ", y.log, " AND y.include.zero ARGUMENT SET TO TRUE -> y.include.zero ARGUMENT RESET TO FALSE BECAUSE 0 VALUE CANNOT BE REPRESENTED IN LOG SCALE")
        warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        y.include.zero <- FALSE
    }
    if(y.log != "no" & vertical == FALSE){
        vertical <- TRUE
        warn.count <- warn.count + 1
        tempo.warn <- paste0("(", warn.count,") BECAUSE OF A BUG IN ggplot2, CANNOT FLIP BOXES HORIZONTALLY WITH A Y.LOG SCALE -> vertical ARGUMENT RESET TO TRUE")
        warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
    }
    # end management of log scale and Inf removal
    # na detection and removal (done now to be sure of the correct length of categ)
    column.check <- unique(c(y, categ, if( ! is.null(dot.color) & ! is.null(dot.categ)){dot.categ}, if( ! is.null(facet.categ)){facet.categ})) # dot.categ because can be a 3rd column of data1, categ.color and dot.color will be tested later
    if(any(is.na(data1[, column.check]))){ # data1 used here instead of data1.ini in case of new NaN created by log conversion (neg values) # normally no NA with is.na
        warn.count <- warn.count + 1
        tempo.warn <- paste0("(", warn.count,") NA DETECTED IN COLUMNS OF data1 AND CORRESPONDING ROWS REMOVED (SEE $removed.row.nb AND $removed.rows)")
        warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        for(i2 in 1:length(column.check)){
            if(any(is.na(data1[, column.check[i2]]))){ # normally no NA with is.na
                tempo.warn <- paste0("NA REMOVAL DUE TO COLUMN ", column.check[i2], " OF data1")
                warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n", tempo.warn)))
            }
        }
        tempo <- unique(unlist(lapply(lapply(c(data1[column.check]), FUN = is.na), FUN = which)))
        removed.row.nb <- c(removed.row.nb, tempo) # removed.row.nb created to remove Inf
        removed.rows <- rbind(removed.rows, data1.ini[tempo, ], stringsAsFactors = FALSE) # here data1.ini used to have the non NA rows that will be removed because of NAN creation after log transformation (neg values for instance)
        column.check <- column.check[ ! column.check == y] # remove y to keep quali columns
        if(length(tempo) != 0){
            data1 <- data1[-tempo, ] # WARNING tempo here and not removed.row.nb because the latter contain more numbers thant the former
            data1.ini <- data1.ini[-tempo, ] # WARNING tempo here and not removed.row.nb because the latter contain more numbers than the former
            for(i3 in 1:length(column.check)){
                if(any( ! unique(removed.rows[, column.check[i3]]) %in% unique(data1[, column.check[i3]]), na.rm = TRUE)){
                    warn.count <- warn.count + 1
                    tempo.warn <- paste0("(", warn.count,") IN COLUMN ", column.check[i3], " OF data1, THE FOLLOWING CLASSES HAVE DISAPPEARED AFTER NA/Inf REMOVAL (IF COLUMN USED IN THE PLOT, THIS CLASS WILL NOT BE DISPLAYED):\n", paste(unique(removed.rows[, column.check[i3]])[ ! unique(removed.rows[, column.check[i3]]) %in% unique(data1[, column.check[i3]])], collapse = " "))
                    warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                }
            }
        }
        count.categ <- 0
        for(i2 in 1:length(column.check)){
            if(column.check[i2] %in% categ){
                count.categ <- count.categ + 1
            }
            if(column.check[i2] == categ[count.categ]){
                categ.class.order[count.categ] <- list(levels(data1[, column.check[i2]])[levels(data1[, column.check[i2]]) %in% unique(data1[, column.check[i2]])]) # remove the absent color in the character vector
                data1[, column.check[i2]] <- factor(as.character(data1[, column.check[i2]]), levels = unique(categ.class.order[[count.categ]]))
            }
            if( ! is.null(dot.color) & ! is.null(dot.categ)){ # reminder : dot.categ cannot be a column name of categ anymore (because in that case dot.categ name is changed into "..._DOT"
                if(column.check[i2] == dot.categ){
                    dot.categ.class.order <- levels(data1[, column.check[i2]])[levels(data1[, column.check[i2]]) %in% unique(data1[, column.check[i2]])] # remove the absent color in the character vector
                    data1[, column.check[i2]] <- factor(as.character(data1[, column.check[i2]]), levels = unique(dot.categ.class.order))
                }
            }
            if(column.check[i2] %in% facet.categ){ # works if facet.categ == NULL this method should keep the order of levels when removing some levels
                tempo.levels <- levels(data1[, column.check[i2]])[levels(data1[, column.check[i2]]) %in% unique(as.character(data1[, column.check[i2]]))]
                data1[, column.check[i2]] <- factor(as.character(data1[, column.check[i2]]), levels = tempo.levels)
            }
        }
    }
    # end na detection and removal (done now to be sure of the correct length of categ)
    # From here, data1 and data.ini have no more NA or NaN in y, categ, dot.categ (if dot.color != NULL) and facet.categ
    
    
    
    if( ! is.null(categ.class.order)){
        if(length(categ.class.order) != length(categ)){
            tempo.cat <- paste0("ERROR IN ", function.name, "\ncateg.class.order ARGUMENT MUST BE A LIST OF LENGTH EQUAL TO LENGTH OF categ\nHERE IT IS LENGTH: ", length(categ.class.order), " VERSUS ", length(categ))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else{
            for(i3 in 1:length(categ.class.order)){
                if(is.null(categ.class.order[[i3]])){
                    warn.count <- warn.count + 1
                    tempo.warn <- paste0("(", warn.count,") THE categ.class.order COMPARTMENT ", i3, " IS NULL. ALPHABETICAL ORDER WILL BE APPLIED")
                    warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                    data1[, categ[i3]] <- factor(as.character(data1[, categ[i3]])) # if already a factor, change nothing, if characters, levels according to alphabetical order
                    categ.class.order[[i3]] <- levels(data1[, categ[i3]]) # character vector that will be used later
                }else{
                    tempo <- saferDev::arg_check(data = categ.class.order[[i3]], data.name = paste0("COMPARTMENT ", i3 , " OF categ.class.order ARGUMENT"), class = "vector", mode = "character", length = length(levels(data1[, categ[i3]])), fun.name = function.name) # length(data1[, categ[i1]) -> if data1[, categ[i1] was initially character vector, then conversion as factor after the NA removal, thus class number ok. If data1[, categ[i1] was initially factor, no modification after the NA removal, thus class number ok
                    if(tempo$problem == TRUE){
                        stop(paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
                    }
                }
                if(any(duplicated(categ.class.order[[i3]]), na.rm = TRUE)){
                    tempo.cat <- paste0("ERROR IN ", function.name, "\nCOMPARTMENT ", i3, " OF categ.class.order ARGUMENT CANNOT HAVE DUPLICATED CLASSES: ", paste(categ.class.order[[i3]], collapse = " "))
                    stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
                }else if( ! (all(categ.class.order[[i3]] %in% unique(data1[, categ[i3]]), na.rm = TRUE) & all(unique(data1[, categ[i3]]) %in% categ.class.order[[i3]], na.rm = TRUE))){
                    tempo.cat <- paste0("ERROR IN ", function.name, "\nCOMPARTMENT ", i3, " OF categ.class.order ARGUMENT MUST BE CLASSES OF ELEMENT ", i3, " OF categ ARGUMENT\nHERE IT IS:\n", paste(categ.class.order[[i3]], collapse = " "), "\nFOR COMPARTMENT ", i3, " OF categ.class.order AND IT IS:\n", paste(unique(data1[, categ[i3]]), collapse = " "), "\nFOR COLUMN ", categ[i3], " OF data1")
                    stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
                }else{
                    data1[, categ[i3]] <- factor(data1[, categ[i3]], levels = categ.class.order[[i3]]) # reorder the factor
                    
                }
                names(categ.class.order)[i3] <- categ[i3]
            }
        }
    }else{
        categ.class.order <- vector("list", length = length(categ))
        tempo.categ.class.order <- NULL
        for(i2 in 1:length(categ.class.order)){
            categ.class.order[[i2]] <- levels(data1[, categ[i2]])
            names(categ.class.order)[i2] <- categ[i2]
            tempo.categ.class.order <- c(tempo.categ.class.order, ifelse(i2 != 1, "\n", ""), categ.class.order[[i2]])
        }
        if(box.alpha != 0){
            warn.count <- warn.count + 1
            tempo.warn <- paste0("(", warn.count,") THE categ.class.order SETTING IS NULL. ALPHABETICAL ORDER WILL BE APPLIED FOR BOX ORDERING:\n", paste(tempo.categ.class.order, collapse = " "))
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        }
    }
    # categ.class.order not NULL anymore (list)
    if(is.null(box.legend.name) & box.alpha != 0){
        warn.count <- warn.count + 1
        tempo.warn <- paste0("(", warn.count,") THE box.legend.name SETTING IS NULL. NAMES OF categ WILL BE USED: ", paste(categ, collapse = " "))
        warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        box.legend.name <- categ[length(categ)] # if only categ1, then legend name of categ1, if length(categ) == 2L, then legend name of categ2
    }
    # box.legend.name not NULL anymore (character string)
    # management of categ.color
    if( ! is.null(categ.color)){
        # check the nature of color
        # integer colors into gg_palette
        tempo.check.color <- saferDev::arg_check(data = categ.color, class = "integer", double.as.integer.allowed = TRUE, na.contain = TRUE, fun.name = function.name)$problem
        if(tempo.check.color == FALSE){
            # convert integers into colors
            categ.color <- ggpalette(max(categ.color, na.rm = TRUE))[categ.color]
        }
        # end integer colors into gg_palette
        if( ! (all(categ.color %in% colors() | grepl(pattern = "^#", categ.color)))){ # check that all strings of low.color start by #, # all() without na.rm -> ok because categ.color cannot be NA (tested above)
            tempo.cat <- paste0("ERROR IN ", function.name, "\ncateg.color ARGUMENT MUST BE A HEXADECIMAL COLOR VECTOR STARTING BY # AND/OR COLOR NAMES GIVEN BY colors() OR A COLUMN NAME OF THE data1 PARAMETER: ", paste(unique(categ.color), collapse = " "))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if(any(is.na(categ.color)) & box.alpha != 0){ # normally no NA with is.na
            warn.count <- warn.count + 1
            tempo.warn <- paste0("(", warn.count,") categ.color ARGUMENT CONTAINS NA")
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        }
        # end check the nature of color
        # check the length of color
        categ.len <- length(categ) # if only categ1, then colors for classes of categ1, if length(categ) == 2L, then colors for classes of categ2
        if(length(data1[, categ[categ.len]]) == length(levels(data1[, categ[categ.len]])) & length(categ.color) == length(data1[, categ[categ.len]])){
            warn.count <- warn.count + 1
            tempo.warn <- paste0("(", warn.count,") THE NUMBER OF CLASSES OF THE COLUMN ", categ[categ.len], " THE NUMBER OF ROWS OF THIS COLUMN AND THE NUMBER OF COLORS OF THE categ.color ARGUMENT ARE ALL EQUAL. BOX COLORS WILL BE ATTRIBUTED ACCORDING THE LEVELS OF ", categ[categ.len], ", NOT ACCORDING TO THE ROWS OF ", categ[categ.len])
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        }
        if(length(categ.color) == length(levels(data1[, categ[categ.len]]))){ # here length(categ.color) is equal to the different number of categ
            # data1[, categ[categ.len]] <- factor(data1[, categ[categ.len]]) # not required because sure that is is a factor
            data1 <- data.frame(data1, categ.color = data1[, categ[categ.len]], stringsAsFactors = TRUE)  # no need stringsAsFactors here for stat.nolog as factors remain factors
            data1$categ.color <- factor(data1$categ.color, labels = categ.color) # replace the characters of data1[, categ[categ.len]] put in the categ.color column by the categ.color (can be write like this because categ.color is length of levels of data1[, categ[categ.len]])
            if(box.alpha != 0){
                warn.count <- warn.count + 1
                tempo.warn <- paste0("(", warn.count,") IN ", categ[categ.len], " OF categ ARGUMENT, THE FOLLOWING COLORS:\n", paste(categ.color, collapse = " "), "\nHAVE BEEN ATTRIBUTED TO THESE CLASSES:\n", paste(levels(factor(data1[, categ[categ.len]])), collapse = " "))
                warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
            }
        }else if(length(categ.color) == length(data1[, categ[categ.len]])){# here length(categ.color) is equal to nrow(data1) -> Modif to have length(categ.color) equal to the different number of categ (length(categ.color) == length(levels(data1[, categ[categ.len]])))
            data1 <- data.frame(data1, categ.color = categ.color, stringsAsFactors = TRUE)
            tempo.check <- unique(data1[ , c(categ[categ.len], "categ.color")])
            if( ! (nrow(tempo.check) == length(unique(categ.color)) & nrow(tempo.check) == length(unique(data1[ , categ[categ.len]])))){
                tempo.cat <- paste0("ERROR IN ", function.name, "\ncateg.color ARGUMENT HAS THE LENGTH OF data1 ROW NUMBER\nBUT IS INCORRECTLY ASSOCIATED TO EACH CLASS OF categ ", categ[categ.len], ":\n", paste(unique(mapply(FUN = "paste", data1[ ,categ[categ.len]], data1[ ,"categ.color"])), collapse = "\n"))
                stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }else{
                # data1[, categ[categ.len]] <- factor(data1[, categ[categ.len]]) # not required because sure that is is a factor
                categ.color <- unique(data1$categ.color[order(data1[, categ[categ.len]])]) # Modif to have length(categ.color) equal to the different number of categ (length(categ.color) == length(levels(data1[, categ[categ.len]])))
                if(box.alpha != 0){
                    warn.count <- warn.count + 1
                    tempo.warn <- paste0("(", warn.count,") categ.color ARGUMENT HAS THE LENGTH OF data1 ROW NUMBER\nCOLORS HAVE BEEN RESPECTIVELY ASSOCIATED TO EACH CLASS OF categ ", categ[categ.len], " AS:\n", paste(levels(factor(data1[, categ[categ.len]])), collapse = " "), "\n", paste(categ.color, collapse = " "))
                    warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                }
            }
        }else if(length(categ.color) == 1L){
            # data1[, categ[categ.len]] <- factor(data1[, categ[categ.len]]) # not required because sure that is is a factor
            data1 <- data.frame(data1, categ.color = categ.color, stringsAsFactors = TRUE)
            categ.color <- rep(categ.color, length(levels(data1[, categ[categ.len]])))
            if(box.alpha != 0){
                warn.count <- warn.count + 1
                tempo.warn <- paste0("(", warn.count,") categ.color ARGUMENT HAS LENGTH 1, MEANING THAT ALL THE DIFFERENT CLASSES OF ", categ[categ.len], "\n", paste(levels(factor(data1[, categ[categ.len]])), collapse = " "), "\nWILL HAVE THE SAME COLOR\n", paste(categ.color, collapse = " "))
                warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
            }
        }else{
            tempo.cat <- paste0("ERROR IN ", function.name, "\ncateg.color ARGUMENT MUST BE (1) LENGTH 1, OR (2) THE LENGTH OF data1 NROWS AFTER NA/Inf REMOVAL, OR (3) THE LENGTH OF THE CLASSES IN THE categ ", categ[categ.len], " COLUMN. HERE IT IS COLOR LENGTH ", length(categ.color), " VERSUS CATEG LENGTH ", length(data1[, categ[categ.len]]), " AND CATEG CLASS LENGTH ", length(unique(data1[, categ[categ.len]])), "\nPRESENCE OF NA/Inf COULD BE THE PROBLEM")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }else{
        categ.len <- length(categ) # if only categ1, then colors for classes of categ1, if length(categ) == 2L, then colors for classes of categ2
        # data1[, categ[categ.len]] <- factor(data1[, categ[categ.len]]) # not required because sure that is is a factor
        categ.color <- ggpalette(length(levels(data1[, categ[categ.len]])))
        data1 <- data.frame(data1, categ.color = data1[, categ[categ.len]], stringsAsFactors = TRUE)
        data1$categ.color <- factor(data1$categ.color, labels = categ.color)  # replace the characters of data1[, categ[categ.len]] put in the categ.color column by the categ.color (can be write like this because categ.color is length of levels of data1[, categ[categ.len]])
        if(box.alpha != 0){
            warn.count <- warn.count + 1
            tempo.warn <- paste0("(", warn.count,") NULL categ.color ARGUMENT -> COLORS RESPECTIVELY ATTRIBUTED TO EACH CLASS OF ", categ[categ.len], " IN data1:\n", paste(categ.color, collapse = " "), "\n", paste(levels(data1[, categ[categ.len]]), collapse = " "))
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        }
    }
    # categ.color not NULL anymore
    categ.color <- as.character(categ.color)
    # categ.color is a character string representing the diff classes
    data1$categ.color <- factor(data1$categ.color, levels = unique(categ.color)) # ok because if categ.color is a character string, the order make class 1, class 2, etc. unique() because no duplicates allowed
    # data1$categ.color is a factor with order of levels -> categ.color
    # end management of categ.color
    # management of dot.color
    if( ! is.null(dot.color)){
        # optional legend of dot colors
        if( ! is.null(dot.categ)){
            ini.dot.categ <- dot.categ
            if( ! dot.categ %in% names(data1)){ # no need to use all() because length(dot.categ) = 1
                tempo.cat <- paste0("ERROR IN ", function.name, "\ndot.categ ARGUMENT MUST BE A COLUMN NAME OF data1. HERE IT IS:\n", dot.categ)
                stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }else if(dot.categ %in% categ){ # no need to use all() because length(dot.categ) = 1. Do not use dot.categ %in% categ[length(categ)] -> error
                # management of dot legend if dot.categ %in% categ (because legends with the same name are joined in ggplot2) 
                warn.count <- warn.count + 1
                tempo.warn <- paste0("(", warn.count,") THE COLUMN NAME OF data1 INDICATED IN THE dot.categ ARGUMENT (", dot.categ, ") HAS BEEN REPLACED BY ", paste0(dot.categ, "_DOT"), " TO AVOID MERGED LEGEND BY GGPLOT2")
                warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                data1 <- data.frame(data1, dot.categ = data1[, dot.categ], stringsAsFactors = TRUE) # dot.categ is not a column name of data1 (checked above with reserved words)
                dot.categ <- paste0(dot.categ, "_DOT")
                names(data1)[names(data1) == "dot.categ"] <- dot.categ # paste0(dot.categ, "_DOT") is not a column name of data1 (checked above with reserved words)
                # tempo.cat <- paste0("ERROR IN ", function.name, "\ndot.categ ARGUMENT CANNOT BE A COLUMN NAME OF data1 ALREADY SPECIFIED IN THE categ ARGUMENT:\n", dot.categ, "\nINDEED, dot.categ ARGUMENT IS MADE TO HAVE MULTIPLE DOT COLORS NOT RELATED TO THE BOXPLOT CATEGORIES")
                # stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            tempo1 <- saferDev::arg_check(data = data1[, dot.categ], data.name = paste0(dot.categ, " COLUMN OF data1"), class = "vector", mode = "character", na.contain = TRUE, fun.name = function.name)
            tempo2 <- saferDev::arg_check(data = data1[, dot.categ], data.name = paste0(dot.categ, " COLUMN OF data1"), class = "factor", na.contain = TRUE, fun.name = function.name)
            if(tempo1$problem == TRUE & tempo2$problem == TRUE){
                tempo.cat <- paste0("ERROR IN ", function.name, "\ndot.categ COLUMN MUST BE A FACTOR OR CHARACTER VECTOR") #
                stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            data1[, dot.categ] <- factor(data1[, dot.categ]) # if already a factor, change nothing, if characters, levels according to alphabetical order
            # dot.categ column of data1 is factor from here
            if( ! is.null(dot.categ.class.order)){
                if(any(duplicated(dot.categ.class.order), na.rm = TRUE)){
                    tempo.cat <- paste0("ERROR IN ", function.name, "\ndot.categ.class.order ARGUMENT CANNOT HAVE DUPLICATED CLASSES: ", paste(dot.categ.class.order, collapse = " "))
                    stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
                }else if( ! (all(dot.categ.class.order %in% levels(data1[, dot.categ])) & all(levels(data1[, dot.categ]) %in% dot.categ.class.order, na.rm = TRUE))){
                    tempo.cat <- paste0("ERROR IN ", function.name, "\ndot.categ.class.order ARGUMENT MUST BE CLASSES OF dot.categ ARGUMENT\nHERE IT IS:\n", paste(dot.categ.class.order, collapse = " "), "\nFOR dot.categ.class.order AND IT IS:\n", paste(levels(data1[, dot.categ]), collapse = " "), "\nFOR dot.categ COLUMN (", ini.dot.categ, ") OF data1")
                    stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
                }else{
                    data1[, dot.categ] <- factor(data1[, dot.categ], levels = dot.categ.class.order) # reorder the factor
                }
            }else{
                if(all(dot.color == "same") & length(dot.color)== 1L){ # all() without na.rm -> ok because dot.color cannot be NA (tested above)
                    dot.categ.class.order <- unlist(categ.class.order[length(categ)])
                    data1[, dot.categ] <- factor(data1[, dot.categ], levels = dot.categ.class.order) # reorder the factor
                    warn.count <- warn.count + 1
                    tempo.warn <- paste0("(", warn.count,") THE dot.categ.class.order SETTING IS NULL AND dot.color IS \"same\". ORDER OF categ.class.order WILL BE APPLIED FOR LEGEND DISPLAY: ", paste(dot.categ.class.order, collapse = " "))
                    warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                }else{
                    dot.categ.class.order <- sort(levels(data1[, dot.categ]))
                    data1[, dot.categ] <- factor(data1[, dot.categ], levels = dot.categ.class.order) # reorder the factor
                    warn.count <- warn.count + 1
                    tempo.warn <- paste0("(", warn.count,") THE dot.categ.class.order SETTING IS NULL. ALPHABETICAL ORDER WILL BE APPLIED FOR LEGEND DISPLAY: ", paste(dot.categ.class.order, collapse = " "))
                    warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                }
            }
            # dot.categ.class.order not NULL anymore (character string) if dot.categ is not NULL
            if(all(dot.color == "same") & length(dot.color)== 1L){ # all() without na.rm -> ok because dot.color cannot be NA (tested above)
                if( ! identical(ini.dot.categ, categ[length(categ)])){
                    tempo.cat <- paste0("ERROR IN ", function.name, "\nWHEN dot.color ARGUMENT IS \"same\", THE COLUMN NAME IN dot.categ ARGUMENT MUST BE IDENTICAL TO THE LAST COLUMN NAME IN categ ARGUMENT. HERE IT IS:\ndot.categ: ", paste(ini.dot.categ, collapse = " "), "\ncateg: ", paste(categ, collapse = " "))
                    stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
                }else if( ! fun_comp_1d(unlist(categ.class.order[length(categ)]), dot.categ.class.order)$identical.content){
                    tempo.cat <- paste0("ERROR IN ", function.name, "\nWHEN dot.color ARGUMENT IS \"same\",\nLAST COMPARTMENT OF categ.class.order ARGUMENT AND dot.categ.class.order ARGUMENT CANNOT BE DIFFERENT:\nLAST COMPARTMENT OF categ.class.order: ", paste(unlist(categ.class.order[length(categ)]), collapse = " "), "\ndot.categ.class.order: ", paste(dot.categ.class.order, collapse = " "))
                    stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
                }
            }
            for(i3 in 1:length(categ)){
                if(identical(categ[i3], ini.dot.categ) & ! identical(unlist(categ.class.order[i3]), dot.categ.class.order) & identical(sort(unlist(categ.class.order[i3])), sort(dot.categ.class.order))){
                    warn.count <- warn.count + 1
                    tempo.warn <- paste0("(", warn.count,") THE dot.categ ARGUMENT SETTING IS PRESENT IN THE categ ARGUMENT SETTING, BUT ORDER OF THE CLASSES IS NOT THE SAME:\ncateg.class.order: ", paste(unlist(categ.class.order[i3]), collapse = " "), "\ndot.categ.class.order: ", paste(dot.categ.class.order, collapse = " "), "\nNOTE THAT ORDER OF categ.class.order IS THE ONE USED FOR THE AXIS REPRESENTATION")
                    warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                }
            }
            if(is.null(dot.legend.name)){
                dot.legend.name <- if(ini.dot.categ %in% categ[length(categ)]){dot.categ}else{ini.dot.categ} #
                warn.count <- warn.count + 1
                tempo.warn <- paste0("(", warn.count,") THE dot.legend.name SETTING IS NULL -> ", dot.legend.name, " WILL BE USED AS LEGEND TITLE OF DOTS")
                warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
            }
            # dot.legend.name not NULL anymore (character string)
        }else{
            if( ! is.null(dot.categ.class.order)){
                warn.count <- warn.count + 1
                tempo.warn <- paste0("(", warn.count,") THE dot.categ.class.order ARGUMENT IS NOT NULL, BUT IS THE dot.categ ARGUMENT\n-> dot.categ.class.order NOT CONSIDERED AS NO LEGEND WILL BE DRAWN")
                warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
            }
            # But dot.categ.class.order will be converted to NULL below (not now)
        }
        # end optional legend of dot colors
        # check the nature of color
        # integer colors into gg_palette
        tempo.check.color <- saferDev::arg_check(data = dot.color, class = "integer", double.as.integer.allowed = TRUE, na.contain = TRUE, fun.name = function.name)$problem
        if(tempo.check.color == FALSE){
            # convert integers into colors
            dot.color <- ggpalette(max(dot.color, na.rm = TRUE))[dot.color]
        }
        # end integer colors into gg_palette
        if(all(dot.color == "same") & length(dot.color)== 1L){# all() without na.rm -> ok because dot.color cannot be NA (tested above)
            dot.color <- categ.color # same color of the dots as the corresponding box color
            warn.count <- warn.count + 1
            tempo.warn <- paste0("(", warn.count,") dot.color ARGUMENT HAS BEEN SET TO \"same\"\nTHUS, DOTS WILL HAVE THE SAME COLORS AS THE CORRESPONDING BOXPLOT")
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        }else if( ! (all(dot.color %in% colors() | grepl(pattern = "^#", dot.color)))){ # check that all strings of low.color start by #, # all() without na.rm -> ok because dot.color cannot be NA (tested above)
            tempo.cat <- paste0("ERROR IN ", function.name, "\ndot.color ARGUMENT MUST BE (1) A HEXADECIMAL COLOR VECTOR STARTING BY #, OR (2) COLOR NAMES GIVEN BY colors(), OR (3) INTEGERS, OR THE STRING \"same\"\nHERE IT IS: ", paste(unique(dot.color), collapse = " "))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if(any(is.na(dot.color))){ # normally no NA with is.finite
            warn.count <- warn.count + 1
            tempo.warn <- paste0("(", warn.count,") dot.color ARGUMENT CONTAINS NA")
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        }
        # end check the nature of color
        # check the length of color
        if( ! is.null(dot.categ)){
            # optional legend of dot colors
            if(length(data1[, dot.categ]) == length(levels(data1[, dot.categ])) & length(dot.color) == length(data1[, dot.categ])){
                warn.count <- warn.count + 1
                tempo.warn <- paste0("(", warn.count,") THE NUMBER OF CLASSES OF THE COLUMN ", dot.categ, " THE NUMBER OF ROWS OF THIS COLUMN AND THE NUMBER OF COLORS OF THE dot.color ARGUMENT ARE ALL EQUAL. DOT COLORS WILL BE ATTRIBUTED ACCORDING THE LEVELS OF ", dot.categ, ", NOT ACCORDING TO THE ROWS OF ", dot.categ)
                warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
            }
            if(length(dot.color) > 1 & ! (length(dot.color) == length(unique(data1[, dot.categ])) | length(dot.color) == length(data1[, dot.categ]))){
                tempo.cat <- paste0("ERROR IN ", function.name, "\nWHEN LENGTH OF THE dot.color ARGUMENT IS MORE THAN 1, IT MUST BE EQUAL TO THE NUMBER OF 1) ROWS OR 2) LEVELS OF dot.categ COLUMN (", dot.categ, "):\ndot.color: ", paste(dot.color, collapse = " "), "\ndot.categ LEVELS: ", paste(levels(data1[, dot.categ]), collapse = " "))
                stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }else if(length(dot.color) > 1 & length(dot.color) == length(unique(data1[, dot.categ]))){
                data1 <- data.frame(data1, dot.color = data1[, dot.categ], stringsAsFactors = TRUE)
                data1$dot.color <- factor(data1$dot.color, labels = dot.color) # do not use labels = unique(dot.color). Otherwise, we can have green1 green2 when dot.color is c("green", "green")
            }else if(length(dot.color) > 1 & length(dot.color) == length(data1[, dot.categ])){
                data1 <- data.frame(data1, dot.color = dot.color, stringsAsFactors = TRUE)
            }else if(length(dot.color)== 1L){ # to deal with single color. Warning: & length(dot.categ.class.order) > 1 removed because otherwise, the data1 is not with dot.color column when length(dot.categ.class.order) == 1
                data1 <- data.frame(data1, dot.color = dot.color, stringsAsFactors = TRUE)
            }
            dot.color <- as.character(unique(data1$dot.color[order(data1[, dot.categ])])) # reorder the dot.color character vector
            if(length(dot.color)== 1L & length(dot.categ.class.order) > 1){ # to deal with single color
                dot.color <- rep(dot.color, length(dot.categ.class.order))
            }
            tempo.check <- unique(data1[ , c(dot.categ, "dot.color")])
            if(length(unique(data1[ , "dot.color"])) > 1 & ( ! (nrow(tempo.check) == length(unique(data1[ , "dot.color"])) & nrow(tempo.check) == length(unique(data1[ , dot.categ]))))){ # length(unique(data1[ , "dot.color"])) > 1 because if only one color, can be attributed to each class of dot.categ
                tempo.cat <- paste0("ERROR IN ", function.name, "\ndot.color ARGUMENT IS INCORRECTLY ASSOCIATED TO EACH CLASS OF dot.categ (", dot.categ, ") COLUMN:\n", paste(unique(mapply(FUN = "paste", data1[ , dot.categ], data1[ ,"dot.color"])), collapse = "\n"))
                stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }else{
                warn.count <- warn.count + 1
                tempo.warn <- paste0("(", warn.count,") IN dot.categ ARGUMENT (", ini.dot.categ, "), THE FOLLOWING COLORS OF DOTS:\n", paste(dot.color, collapse = " "), "\nHAVE BEEN ATTRIBUTED TO THESE CLASSES:\n", paste(levels(data1[, dot.categ]), collapse = " "))
                warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
            }
            # dot.color is a character string representing the diff classes of dot.categ
            # data1$dot.color is a factor with order of levels -> dot.categ
            # end optional legend of dot colors
        }else{
            categ.len <- length(categ) # if only categ1, then colors for classes of categ1, if length(categ) == 2L, then colors for classes of categ2
            if(length(dot.color) == length(levels(data1[, categ[categ.len]]))){ # here length(dot.color) is equal to the different number of categ
                # data1[, categ[categ.len]] <- factor(data1[, categ[categ.len]]) # not required because sure that is is a factor
                data1 <- data.frame(data1, dot.color = data1[, categ[categ.len]], stringsAsFactors = TRUE)
                data1$dot.color <- factor(data1$dot.color, labels = dot.color)
                if(box.alpha != 0){
                    warn.count <- warn.count + 1
                    tempo.warn <- paste0("(", warn.count,") IN ", categ[categ.len], " OF categ ARGUMENT, THE FOLLOWING COLORS:\n", paste(dot.color, collapse = " "), "\nHAVE BEEN ATTRIBUTED TO THESE CLASSES:\n", paste(levels(factor(data1[, categ[categ.len]])), collapse = " "))
                    warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                }
            }else if(length(dot.color) == length(data1[, categ[categ.len]])){# here length(dot.color) is equal to nrow(data1) -> Modif to have length(dot.color) equal to the different number of categ (length(dot.color) == length(levels(data1[, categ[categ.len]])))
                data1 <- data.frame(data1, dot.color = dot.color, stringsAsFactors = TRUE)
            }else if(length(dot.color)== 1L & ! all(dot.color == "same")){ # all() without na.rm -> ok because dot.color cannot be NA (tested above)
                # data1[, categ[categ.len]] <- factor(data1[, categ[categ.len]]) # not required because sure that is is a factor
                data1 <- data.frame(data1, dot.color = dot.color, stringsAsFactors = TRUE)
                dot.color <- rep(dot.color, length(levels(data1[, categ[categ.len]])))
                warn.count <- warn.count + 1
                tempo.warn <- paste0("(", warn.count,") dot.color ARGUMENT HAS LENGTH 1, MEANING THAT ALL THE DIFFERENT CLASSES OF ", categ[categ.len], "\n", paste(levels(factor(data1[, categ[categ.len]])), collapse = " "), "\nWILL HAVE THE SAME COLOR\n", paste(dot.color, collapse = " "))
                warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
            }else{
                tempo.cat <- paste0("ERROR IN ", function.name, "\ndot.color ARGUMENT MUST BE (1) LENGTH 1, OR (2) THE LENGTH OF data1 NROWS AFTER NA/Inf REMOVAL, OR (3) THE LENGTH OF THE CLASSES IN THE categ ", categ[categ.len], " COLUMN. HERE IT IS COLOR LENGTH ", length(dot.color), " VERSUS CATEG LENGTH ", length(data1[, categ[categ.len]]), " AND CATEG CLASS LENGTH ", length(unique(data1[, categ[categ.len]])), "\nPRESENCE OF NA/Inf COULD BE THE PROBLEM")
                stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            # end check the length of color
            dot.color <- as.character(dot.color)
            # dot.color is a character string representing the diff classes
            data1$dot.color <- factor(data1$dot.color, levels = unique(dot.color)) # ok because if dot.color is a character string, the order make class 1, class 2, etc. If dot.color is a column of data1, then levels will be created, without incidence, except if dot.categ specified (see below). unique() because no duplicates allowed
            # data1$dot.color is a factor with order of levels -> dot.color
        }
        # end optional legend of dot colors
    }else if(is.null(dot.color) & ! (is.null(dot.categ) & is.null(dot.categ.class.order) & is.null(dot.legend.name))){
        warn.count <- warn.count + 1
        tempo.warn <- paste0("(", warn.count,") dot.categ OR dot.categ.class.order OR dot.legend.name ARGUMENT HAS BEEN SPECIFIED BUT dot.color ARGUMENT IS NULL (NO DOT PLOTTED)")
        warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
    }
    # dot.color either NULL (no dot plotted) or character string (potentially representing the diff classes of dot.categ)
    # data1$dot.color is either NA or a factor (with order of levels -> depending on dot.categ or categ[length(categ)], or other
    if(is.null(dot.categ)){
        dot.categ.class.order <- NULL # because not used anyway
    }
    # dot.categ.class.order either NULL if dot.categ is NULL (no legend displayed) or character string (potentially representing the diff classes of dot.categ)
    # end management of dot.color
    if(is.null(dot.color) & box.fill == FALSE & dot.alpha <= 0.025){
        warn.count <- warn.count + 1
        tempo.warn <- paste0("(", warn.count,") THE FOLLOWING ARGUMENTS WERE SET AS:\ndot.color = NULL (NOT ALL DOTS BUT ONLY POTENTIAL OUTLIER DOTS DISPLAYED)\nbox.fill = FALSE (NO FILLING COLOR FOR BOTH BOXES AND POTENTIAL OUTLIER DOTS)\ndot.alpha = ", saferTool::round2(dot.alpha, 4), "\n-> POTENTIAL OUTLIER DOTS MIGHT NOT BE VISIBLE BECAUSE ALMOST TRANSPARENT")
        warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
    }
    if(is.null(dot.color) & box.fill == FALSE & dot.border.size == 0){
        tempo.cat <- paste0("ERROR IN ", function.name, "\nTHE FOLLOWING ARGUMENTS WERE SET AS:\ndot.color = NULL (NOT ALL DOTS BUT ONLY POTENTIAL OUTLIER DOTS DISPLAYED)\nbox.fill = FALSE (NO FILLING COLOR FOR BOTH BOXES AND POTENTIAL OUTLIER DOTS)\ndot.border.size = 0 (NO BORDER FOR POTENTIAL OUTLIER DOTS)\n-> THESE SETTINGS ARE NOT ALLOWED BECAUSE THE POTENTIAL OUTLIER DOTS WILL NOT BE VISIBLE")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # integer dot.border.color into gg_palette
    if( ! is.null(dot.border.color)){
        tempo <- saferDev::arg_check(data = dot.border.color, class = "vector", typeof = "integer", double.as.integer.allowed = TRUE, length = 1, fun.name = function.name)
        if(tempo$problem == FALSE){ # convert integers into colors
            dot.border.color <- ggpalette(max(dot.border.color, na.rm = TRUE))[dot.border.color]
        }
    }
    # end integer dot.border.color into gg_palette
    # na detection and removal (done now to be sure of the correct length of categ)
    column.check <- c("categ.color", if( ! is.null(dot.color)){"dot.color"}) # 
    if(any(is.na(data1[, column.check]))){ # data1 used here instead of data1.ini in case of new NaN created by log conversion (neg values) # normally no NA with is.na
        warn.count <- warn.count + 1
        tempo.warn <- paste0("(", warn.count,") NA DETECTED IN COLUMNS ", paste(column.check, collapse = " "), " OF data1 AND CORRESPONDING ROWS REMOVED (SEE $removed.row.nb AND $removed.rows)")
        warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        for(i2 in 1:length(column.check)){
            if(any(is.na(data1[, column.check[i2]]))){ # normally no NA with is.na
                tempo.warn <- paste0("NA REMOVAL DUE TO COLUMN ", column.check[i2], " OF data1")
                warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n", tempo.warn)))
            }
        }
        tempo <- unique(unlist(lapply(lapply(c(data1[column.check]), FUN = is.na), FUN = which)))
        removed.row.nb <- c(removed.row.nb, tempo)
        removed.rows <- rbind(removed.rows, data1[tempo, ], stringsAsFactors = FALSE) # here data1 used because categorical columns tested
        if(length(tempo) != 0){
            data1 <- data1[-tempo, ] # WARNING tempo here and not removed.row.nb because the latter contain more numbers thant the former
            data1.ini <- data1.ini[-tempo, ] # WARNING tempo here and not removed.row.nb because the latter contain more numbers thant the former
            for(i3 in 1:length(column.check)){
                if(any( ! unique(removed.rows[, column.check[i3]]) %in% unique(data1[, column.check[i3]]), na.rm = TRUE)){
                    warn.count <- warn.count + 1
                    tempo.warn <- paste0("(", warn.count,") IN COLUMN ", column.check[i3], " OF data1, THE FOLLOWING CLASSES HAVE DISAPPEARED AFTER NA/Inf REMOVAL (IF COLUMN USED IN THE PLOT, THIS CLASS WILL NOT BE DISPLAYED):\n", paste(unique(removed.rows[, column.check[i3]])[ ! unique(removed.rows[, column.check[i3]]) %in% unique(data1[, column.check[i3]])], collapse = " "))
                    warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                }
            }
        }
        for(i2 in 1:length(column.check)){
            if(column.check[i2] == "categ.color"){
                categ.color <- levels(data1[, column.check[i2]])[levels(data1[, column.check[i2]]) %in% unique(data1[, column.check[i2]])] # remove the absent color in the character vector
                if(length(categ.color)== 1L & length(unlist(categ.class.order[length(categ)])) > 1){ # to deal with single color
                    categ.color <- rep(categ.color, length(unlist(categ.class.order[length(categ)])))
                }
                data1[, column.check[i2]] <- factor(as.character(data1[, column.check[i2]]), levels = unique(categ.color))
            }
            if(column.check[i2] == "dot.color"){
                dot.color <- levels(data1[, column.check[i2]])[levels(data1[, column.check[i2]]) %in% unique(data1[, column.check[i2]])] # remove the absent color in the character vector
                if(length(dot.color)== 1L & length(dot.categ.class.order) > 1){ # to deal with single color. If dot.categ.class.order == NULL (which is systematically the case if dot.categ == NULL), no rep(dot.color, length(dot.categ.class.order)
                    dot.color <- rep(dot.color, length(dot.categ.class.order))
                }
                data1[, column.check[i2]] <- factor(as.character(data1[, column.check[i2]]), levels = unique(dot.color))
            }
        }
    }
    # end na detection and removal (done now to be sure of the correct length of categ)
    # From here, data1 and data.ini have no more NA or NaN
    # end other checkings
    # reserved word checking
    #already done above
    # end reserved word checking
    # end second round of checking and data preparation
    
    
    # package checking
    fun_pack(req.package = c(
        "ggplot2", 
        "gridExtra", 
        "lemon", 
        "scales"
    ), load = FALSE, lib.path = lib.path)
    # end package checking
    
    
    
    
    
    # main code
    # y coordinates recovery (create ini.box.coord, dot.coord and modify data1)
    if(length(categ)== 1L){
        # width commputations
        box.width2 <- box.width
        box.space <- 0 # to inactivate the shrink that add space between grouped boxes, because no grouped boxes here
        # end width commputations
        # data1 check categ order for dots coordinates recovery
        data1 <- data.frame(data1, categ.check = data1[, categ[1]], stringsAsFactors = TRUE)
        data1$categ.check <- as.integer(data1$categ.check) # to check that data1[, categ[1]] and dot.coord$group are similar, during merging
        # end data1 check categ order for dots coordinates recovery
        # per box dots coordinates recovery
        tempo.gg.name <- "gg.indiv.plot."
        tempo.gg.count <- 0
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), eval(parse(text = paste0("ggplot2::ggplot()", if(is.null(add)){""}else{add})))) # add added here to have the facets
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_point(data = data1, mapping = ggplot2::aes_string(x = categ[1], y = y, color = categ[1]), stroke = dot.border.size, size = dot.size, alpha = dot.alpha, shape = 21))
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "color", name = box.legend.name, values = if(is.null(categ.color)){rep(NA, length(unique(data1[, categ[1]])))}else if(length(categ.color)== 1L){rep(categ.color, length(unique(data1[, categ[1]])))}else{categ.color})) # categ.color used for dot colors because at that stage, we do not care about colors
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_boxplot(data = data1, mapping = ggplot2::aes_string(x = categ[1], y = y, fill = categ[1]), coef = if(box.whisker.kind == "no"){0}else if(box.whisker.kind == "std"){1.5}else if(box.whisker.kind == "max"){Inf})) # fill because this is what is used with geom_box # to easily have the equivalent of the grouped boxes
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "fill", name = box.legend.name, values = if(length(categ.color)== 1L){rep(categ.color, length(unique(data1[, categ[1]])))}else{categ.color}))
        # end per box dots coordinates recovery
    }else if(length(categ) == 2L){
        # width commputations
        box.width2 <- box.width / length(unique(data1[, categ[length(categ)]])) # real width of each box in x-axis unit, among the set of grouped box. Not relevant if no grouped boxes length(categ)== 1L
        # end width commputations
        # data1 check categ order for dots coordinates recovery
        tempo.factor <- paste0(data1[order(data1[, categ[2]], data1[, categ[1]]), categ[2]], "_", data1[order(data1[, categ[2]], data1[, categ[1]]), categ[1]])
        data1 <- data.frame(data1[order(data1[, categ[2]], data1[, categ[1]]), ], categ.check = factor(tempo.factor, levels = unique(tempo.factor)), stringsAsFactors = TRUE)
        data1$categ.check <- as.integer(data1$categ.check)
        # end data1 check categ order for dots coordinates recovery
        # per box dots coordinates recovery
        tempo.gg.name <- "gg.indiv.plot."
        tempo.gg.count <- 0
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), eval(parse(text = paste0("ggplot2::ggplot()", if(is.null(add)){""}else{add})))) # add added here to have the facets
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_point(data = data1, mapping = ggplot2::aes_string(x = categ[1], y = y, color = categ[2]), stroke = dot.border.size, size = dot.size, alpha = dot.alpha, shape = 21))
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "color", name = box.legend.name, values = if(is.null(categ.color)){rep(NA, length(unique(data1[, categ[2]])))}else if(length(categ.color)== 1L){rep(categ.color, length(unique(data1[, categ[2]])))}else{categ.color})) # categ.color used for dot colors because at that stage, we do not care about colors
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_boxplot(data = data1, mapping = ggplot2::aes_string(x = categ[1], y = y, fill = categ[2]), coef = if(box.whisker.kind == "no"){0}else if(box.whisker.kind == "std"){1.5}else if(box.whisker.kind == "max"){Inf})) # fill because this is what is used with geom_box # to easily have the equivalent of the grouped boxes
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "fill", name = box.legend.name, values = if(length(categ.color)== 1L){rep(categ.color, length(unique(data1[, categ[2]])))}else{categ.color}))
        # end per box dots coordinates recovery
    }else{
        tempo.cat <- paste0("INTERNAL CODE ERROR IN ", function.name, "\nCODE INCONSISTENCY 1")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if( ! is.null(stat.pos)){
        stat.just <- fun_gg_just(
            angle = stat.angle, 
            pos = ifelse(
                vertical == TRUE, 
                ifelse(stat.pos == "top", "bottom", "top"), # "bottom" because we want justification for text that are below the ref point which is the top of the graph. The opposite for "above"
                ifelse(stat.pos == "top", "left", "right") # "left" because we want justification for text that are on the left of the ref point which is the right border of the graph. The opposite for "above"
            ), 
            kind = "text"
        )
    }
    # has in fact no interest because ggplot2 does not create room for geom_text()
    tempo.data.max <- data1[which.max(data1[, y]), ]
    tempo.data.max <- data.frame(tempo.data.max, label = formatC(tempo.data.max[, y], digit = 2, drop0trailing = TRUE, format = "f"), stringsAsFactors = TRUE)
    # end has in fact no interest because ggplot2 does not create room for geom_text()
    tempo.graph.info.ini <- ggplot2::ggplot_build(eval(parse(text = paste(paste(paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + "), if( ! is.null(stat.pos)){' + ggplot2::geom_text(data = tempo.data.max, mapping = ggplot2::aes_string(x = 1, y = y, label = "label"), size = stat.size, color = "black", angle = stat.angle, hjust = stat.just$hjust, vjust = stat.just$vjust)'})))) # added here to have room for annotation
    dot.coord <- tempo.graph.info.ini$data[[1]]
    dot.coord$x <- as.numeric(dot.coord$x) # because weird class
    dot.coord$PANEL <- as.numeric(dot.coord$PANEL) # because numbers as levels. But may be a problem is facet are reordered ?
    tempo.mean <- aggregate(x = dot.coord$y, by = list(dot.coord$group, dot.coord$PANEL), FUN = mean, na.rm = TRUE)
    names(tempo.mean)[names(tempo.mean) == "x"] <- "MEAN"
    names(tempo.mean)[names(tempo.mean) == "Group.1"] <- "BOX"
    names(tempo.mean)[names(tempo.mean) == "Group.2"] <- "PANEL"
    dot.coord <- data.frame(
        dot.coord[order(dot.coord$group, dot.coord$y), ], # dot.coord$PANEL deals below
        y.check = as.double(data1[order(data1$categ.check, data1[, y]), y]), 
        categ.check = data1[order(data1$categ.check, data1[, y]), "categ.check"], 
        dot.color = if(is.null(dot.color)){NA}else{data1[order(data1$categ.check, data1[, y]), "dot.color"]}, 
        data1[order(data1$categ.check, data1[, y]), ][categ], # avoid the renaming below
        stringsAsFactors = TRUE
    ) # y.check to be sure that the order is the same between the y of data1 and the y of dot.coord
    # names(dot.coord)[names(dot.coord) == "tempo.categ1"] <- categ[1]
    if( ! is.null(dot.categ)){
        dot.coord <- data.frame(dot.coord, data1[order(data1$categ.check, data1[, y]), ][dot.categ], stringsAsFactors = TRUE) # avoid the renaming
    }
    if( ! is.null(facet.categ)){
        dot.coord <- data.frame(dot.coord, data1[order(data1$categ.check, data1[, y]), ][facet.categ], stringsAsFactors = TRUE) # for facet panels
        tempo.test <- NULL
        for(i2 in 1:length(facet.categ)){
            tempo.test <- paste0(tempo.test, ".", formatC(as.numeric(dot.coord[, facet.categ[i2]]), width = nchar(max(as.numeric(dot.coord[, facet.categ[i2]]), na.rm = TRUE)), flag = "0")) # convert factor into numeric with leading zero for proper ranking # merge the formatC() to create a new factor. The convertion to integer should recreate the correct group number. Here as.numeric is used and not as.integer in case of numeric in facet.categ (because comes from add and not checked by saferDev::arg_check, contrary to categ)
        }
        tempo.test <- as.integer(factor(tempo.test))
        if( ! identical(as.integer(dot.coord$PANEL), tempo.test)){
            tempo.cat <- paste0("INTERNAL CODE ERROR IN ", function.name, "\nas.integer(dot.coord$PANEL) AND tempo.test MUST BE IDENTICAL. CODE HAS TO BE MODIFIED")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    if(dot.tidy == TRUE){
        if( ! is.null(dot.categ)){
            dot.coord <- data.frame(dot.coord, tidy_group = data1[order(data1$categ.check, data1[, y]), ][, dot.categ], stringsAsFactors = TRUE) # avoid the renaming
            # tidy_group_coord is to be able to fuse table when creating the table for dot coordinates
            if(dot.categ %in% categ){
                dot.coord <- data.frame(dot.coord, tidy_group_coord = dot.coord$group, stringsAsFactors = TRUE)
            }else{
                dot.coord <- data.frame(dot.coord, tidy_group_coord = as.integer(factor(paste0(
                    formatC(as.integer(dot.coord[, categ[1]]), width = nchar(max(as.integer(dot.coord[, categ[1]]), na.rm = TRUE)), flag = "0"), # convert factor into numeric with leading zero for proper ranking
                    ".", 
                    if(length(categ) == 2L){formatC(as.integer(dot.coord[, categ[2]]), width = nchar(max(as.integer(dot.coord[, categ[2]]), na.rm = TRUE)), flag = "0")}, # convert factor into numeric with leading zero for proper ranking
                    if(length(categ) == 2L){"."}, 
                    formatC(as.integer(dot.coord[, dot.categ]), width = nchar(max(as.integer(dot.coord[, dot.categ]), na.rm = TRUE)), flag = "0") # convert factor into numeric with leading zero for proper ranking
                )), stringsAsFactors = TRUE) # merge the 2 or 3 formatC() to create a new factor. The convertion to integer should recreate the correct group number
                ) # for tidy dot plots
            }
        }else{
            dot.coord <- data.frame(dot.coord, tidy_group = if(length(categ)== 1L){
                dot.coord[, categ]}else{as.integer(factor(paste0(
                    formatC(as.integer(dot.coord[, categ[1]]), width = nchar(max(as.integer(dot.coord[, categ[1]]), na.rm = TRUE)), flag = "0"), # convert factor into numeric with leading zero for proper ranking
                    ".", 
                    formatC(as.integer(dot.coord[, categ[2]]), width = nchar(max(as.integer(dot.coord[, categ[2]]), na.rm = TRUE)), flag = "0")# convert factor into numeric with leading zero for proper ranking
                )), stringsAsFactors = TRUE) # merge the 2 formatC() to create a new factor. The convertion to integer should recreate the correct group number
                }) # for tidy dot plots
            # tidy_group_coord is to be able to fuse table when creating the table for dot coordinates
            dot.coord <- data.frame(dot.coord, tidy_group_coord = dot.coord$group, stringsAsFactors = TRUE)
        }
    }
    if( ! (identical(dot.coord$y, dot.coord$y.check) & identical(dot.coord$group, dot.coord$categ.check))){
        tempo.cat <- paste0("INTERNAL CODE ERROR IN ", function.name, "\n(dot.coord$y AND dot.coord$y.check) AS WELL AS (dot.coord$group AND dot.coord$categ.check) MUST BE IDENTICAL. CODE HAS TO BE MODIFIED")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }else{
        if( ! identical(tempo.mean[order(tempo.mean$BOX, tempo.mean$PANEL), ]$BOX, unique(dot.coord[order(dot.coord$group, dot.coord$PANEL), c("group", "PANEL")])$group)){
            tempo.cat <- paste0("INTERNAL CODE ERROR IN ", function.name, "\n(tempo.mean$BOX, tempo.mean$PANEL) AND (dot.coord$group, dot.coord$PANEL) MUST BE IDENTICAL. CODE HAS TO BE MODIFIED")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else{
            tempo <- unique(dot.coord[order(dot.coord$group, dot.coord$PANEL), c(categ, if( ! is.null(dot.color) & ! is.null(dot.categ)){if(dot.categ != ini.dot.categ){dot.categ}}, if( ! is.null(facet.categ)){facet.categ}), drop = FALSE])
            # names(tempo) <- paste0(names(tempo), ".mean")
            tempo.mean <- data.frame(tempo.mean[order(tempo.mean$BOX, tempo.mean$PANEL), ], tempo, stringsAsFactors = TRUE)
        }
    }
    # at that stage, categ color and dot color are correctly attributed in data1, box.coord and dot.coord
    # end y dot coordinates recovery (create ini.box.coord, dot.coord and modify data1)
    # ylim range
    if(is.null(y.lim)){
        y.lim <- tempo.graph.info.ini$layout$panel_params[[1]]$y.range # finite = TRUE removes all the -Inf and Inf except if only this. In that case, whatever the -Inf and/or Inf present, output -Inf;Inf range. Idem with NA only
        if(any(( ! is.finite(y.lim)) | is.na(y.lim)) | length(y.lim) != 2){ # kept but normally no more Inf in data1 # normally no NA with is.finite, etc.
            tempo.cat <- paste0("INTERNAL CODE ERROR IN ", function.name, "\ntempo.graph.info.ini$layout$panel_params[[1]]$y.range[1] CONTAINS NA OR Inf OR HAS LENGTH 1")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }else if(y.log != "no"){
        y.lim <- get(y.log)(y.lim) # no env = sys.nframe(), inherit = FALSE in get() because look for function in the classical scope
    }
    if(y.log != "no"){
        # normally this control is not necessary anymore
        if(any( ! is.finite(y.lim))){ # normally no NA with is.finite
            tempo.cat <- paste0("ERROR IN ", function.name, "\ny.lim ARGUMENT CANNOT HAVE ZERO OR NEGATIVE VALUES WITH THE y.log ARGUMENT SET TO ", y.log, ":\n", paste(y.lim, collapse = " "), "\nPLEASE, CHECK DATA VALUES (PRESENCE OF ZERO OR INF VALUES)")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    if(suppressWarnings(all(y.lim %in% c(Inf, -Inf)))){ # all() without na.rm -> ok because y.lim cannot be NA (tested above)
        # normally this control is not necessary anymore
        tempo.cat <- paste0("ERROR IN ", function.name, " y.lim CONTAINS Inf VALUES, MAYBE BECAUSE VALUES FROM data1 ARGUMENTS ARE NA OR Inf ONLY OR BECAUSE OF LOG SCALE REQUIREMENT")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if(suppressWarnings(any(is.na(y.lim)))){ # normally no NA with is.na
        # normally this control is not necessary anymore
        tempo.cat <- paste0("ERROR IN ", function.name, " y.lim CONTAINS NA OR NaN VALUES, MAYBE BECAUSE VALUES FROM data1 ARGUMENTS ARE NA OR Inf ONLY OR BECAUSE OF LOG SCALE REQUIREMENT")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    y.lim.order <- order(y.lim) # to deal with inverse axis
    y.lim <- sort(y.lim)
    y.lim[1] <- y.lim[1] - abs(y.lim[2] - y.lim[1]) * ifelse(diff(y.lim.order) > 0, y.bottom.extra.margin, y.top.extra.margin) # diff(y.lim.order) > 0 medians not inversed axis
    y.lim[2] <- y.lim[2] + abs(y.lim[2] - y.lim[1]) * ifelse(diff(y.lim.order) > 0, y.top.extra.margin, y.bottom.extra.margin) # diff(y.lim.order) > 0 medians not inversed axis
    if(y.include.zero == TRUE){ # no need to check y.log != "no" because done before
        y.lim <- range(c(y.lim, 0), na.rm = TRUE, finite = TRUE) # finite = TRUE removes all the -Inf and Inf except if only this. In that case, whatever the -Inf and/or Inf present, output -Inf;Inf range. Idem with NA only
    }
    y.lim <- y.lim[y.lim.order]
    if(any(is.na(y.lim))){ # normally no NA with is.na
        tempo.cat <- paste0("INTERNAL CODE ERROR IN ", function.name, "\nCODE INCONSISTENCY 2")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end ylim range
    
    
    
    
    
    
    # drawing
    # constant part
    tempo.gg.name <- "gg.indiv.plot."
    tempo.gg.count <- 0
    assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), eval(parse(text = paste0("ggplot2::ggplot()", if(is.null(add)){""}else{add})))) # add is directly put here to deal with additional variable of data, like when using facet_grid. No problem if add is a theme, will be dealt below
    assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::xlab(if(is.null(x.lab)){categ[1]}else{x.lab}))
    assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::ylab(if(is.null(y.lab)){y}else{y.lab}))
    assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::ggtitle(title))
    # text angle management
    axis.just <- fun_gg_just(angle = x.angle, pos = ifelse(vertical == TRUE, "bottom", "left"), kind = "axis")
    # end text angle management
    add.check <- TRUE
    if( ! is.null(add)){ # if add is NULL, then = 0
        if(grepl(pattern = "ggplot2\\s*::\\s*theme", add) == TRUE){
            warn.count <- warn.count + 1
            tempo.warn <- paste0("(", warn.count,") \"ggplot2::theme\" STRING DETECTED IN THE add ARGUMENT\n-> INTERNAL GGPLOT2 THEME FUNCTIONS theme() AND theme_classic() HAVE BEEN INACTIVATED, TO BE USED BY THE USER\n-> article ARGUMENT WILL BE IGNORED")
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
            add.check <- FALSE
        }
    }
    if(add.check == TRUE & article == TRUE){
        # WARNING: not possible to add theme()several times. NO message but the last one overwrites the others
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::theme_classic(base_size = text.size))
        if(grid == TRUE){
            assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), m.gg <- ggplot2::theme(
                text = ggplot2::element_text(size = text.size), 
                plot.title = ggplot2::element_text(size = title.text.size), # stronger than text
                line = ggplot2::element_line(size = 0.5), 
                legend.key = ggplot2::element_rect(color = "white", size = 1.5), # size of the frame of the legend
                axis.line.y.left = ggplot2::element_line(colour = "black"), # draw lines for the y axis
                axis.line.x.bottom = ggplot2::element_line(colour = "black"), # draw lines for the x axis
                panel.grid.major.x = if(vertical == TRUE){NULL}else{ggplot2::element_line(colour = "grey85", size = 0.75)},
                panel.grid.major.y = if(vertical == TRUE){ggplot2::element_line(colour = "grey85", size = 0.75)}else{NULL},
                panel.grid.minor.y = if(vertical == TRUE){ggplot2::element_line(colour = "grey90", size = 0.25)}else{NULL},
                axis.text.x = if(vertical == TRUE){ggplot2::element_text(angle = axis.just$angle, hjust = axis.just$hjust, vjust = axis.just$vjust)}else{NULL},
                axis.text.y = if(vertical == TRUE){NULL}else{ggplot2::element_text(angle = axis.just$angle, hjust = axis.just$hjust, vjust = axis.just$vjust)},
                strip.background = ggplot2::element_rect(fill = NA, colour = NA) # for facet background
            ))
        }else{
            assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), m.gg <- ggplot2::theme(
                text = ggplot2::element_text(size = text.size), 
                plot.title = ggplot2::element_text(size = title.text.size), # stronger than text
                line = ggplot2::element_line(size = 0.5), 
                legend.key = ggplot2::element_rect(color = "white", size = 1.5), # size of the frame of the legend
                axis.line.y.left = ggplot2::element_line(colour = "black"), 
                axis.line.x.bottom = ggplot2::element_line(colour = "black"),
                axis.text.x = if(vertical == TRUE){ggplot2::element_text(angle = axis.just$angle, hjust = axis.just$hjust, vjust = axis.just$vjust)}else{NULL},
                axis.text.y = if(vertical == TRUE){NULL}else{ggplot2::element_text(angle = axis.just$angle, hjust = axis.just$hjust, vjust = axis.just$vjust)},
                strip.background = ggplot2::element_rect(fill = NA, colour = NA)
            ))
        }
    }else if(add.check == TRUE & article == FALSE){
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), m.gg <- ggplot2::theme(
            text = ggplot2::element_text(size = text.size), 
            plot.title = ggplot2::element_text(size = title.text.size), # stronger than text
            line = ggplot2::element_line(size = 0.5), 
            legend.key = ggplot2::element_rect(color = "white", size = 1.5), # size of the frame of the legend
            panel.background = ggplot2::element_rect(fill = "grey95"), 
            axis.line.y.left = ggplot2::element_line(colour = "black"), 
            axis.line.x.bottom = ggplot2::element_line(colour = "black"), 
            panel.grid.major.x = ggplot2::element_line(colour = "grey85", size = 0.75), 
            panel.grid.major.y = ggplot2::element_line(colour = "grey85", size = 0.75), 
            panel.grid.minor.x = ggplot2::element_blank(), 
            panel.grid.minor.y = ggplot2::element_line(colour = "grey90", size = 0.25), 
            strip.background = ggplot2::element_rect(fill = NA, colour = NA),
            axis.text.x = if(vertical == TRUE){ggplot2::element_text(angle = axis.just$angle, hjust = axis.just$hjust, vjust = axis.just$vjust)}else{NULL},
            axis.text.y = if(vertical == TRUE){NULL}else{ggplot2::element_text(angle = axis.just$angle, hjust = axis.just$hjust, vjust = axis.just$vjust)}
        ))
    }
    # Contrary to fun_gg_bar(), cannot plot the boxplot right now, because I need the dots plotted first
    assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_boxplot(data = data1, mapping = ggplot2::aes_string(x = categ[1], y = y, group = categ[length(categ)]), position = ggplot2::position_dodge(width = NULL), color = NA, width = box.width, fill = NA)) # this is to set the graph (i.e., a blanck boxplot to be able to use x coordinates to plot dots before boxes)
    # end constant part
    
    
    
    
    # graphic info recovery (including means)
    tempo.graph.info <- ggplot2::ggplot_build(eval(parse(text = paste0(paste(paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + "), ' + ggplot2::geom_boxplot(data = data1, mapping = ggplot2::aes_string(x = categ[1], y = y, fill = categ[length(categ)]), position = ggplot2::position_dodge(width = NULL), width = box.width, notch = box.notch, coef = if(box.whisker.kind == "no"){0}else if(box.whisker.kind == "std"){1.5}else if(box.whisker.kind == "max"){Inf}) + ggplot2::scale_discrete_manual(aesthetics = "fill", name = box.legend.name, values = if(length(categ.color)== 1L){rep(categ.color, length(unique(data1[, categ[length(categ)]])))}else{categ.color})')))) # will be recovered later again, when ylim will be considered
    tempo.yx.ratio <- (tempo.graph.info$layout$panel_params[[1]]$y.range[2] - tempo.graph.info$layout$panel_params[[1]]$y.range[1]) / (tempo.graph.info$layout$panel_params[[1]]$x.range[2] - tempo.graph.info$layout$panel_params[[1]]$x.range[1])
    box.coord <- tempo.graph.info$data[[2]] # to have the summary statistics of the plot. Contrary to ini.box.plot, now integrates ylim Here because can be required for stat.pos when just box are plotted
    box.coord$x <- as.numeric(box.coord$x) # because x is of special class that block comparison of values using identical
    box.coord$PANEL <- as.numeric(box.coord$PANEL) # because numbers as levels. But may be a problem is facet are reordered ?
    box.coord <- box.coord[order(box.coord$group, box.coord$PANEL), ]
    if( ! (identical(tempo.mean$BOX, box.coord$group) & identical(tempo.mean$PANEL, box.coord$PANEL))){
        tempo.cat <- paste0("INTERNAL CODE ERROR IN ", function.name, "\nidentical(tempo.mean$BOX, box.coord$group) & identical(tempo.mean$PANEL, box.coord$PANEL) DO NOT HAVE THE SAME VALUE ORDER")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }else{
        # tempo <- c(categ, if( ! is.null(dot.color) & ! is.null(dot.categ)){if(dot.categ != ini.dot.categ){dot.categ}}, if( ! is.null(facet.categ)){facet.categ})
        if(any(names(tempo.mean) %in% names(box.coord), na.rm = TRUE)){
            names(tempo.mean)[names(tempo.mean) %in% names(box.coord)] <- paste0(names(tempo.mean)[names(tempo.mean) %in% names(box.coord)], ".mean")
        }
        box.coord <- data.frame(box.coord, tempo.mean, stringsAsFactors = TRUE)
    }
    # end graphic info recovery (including means)
    
    
    
    # stat output (will also serve for boxplot and mean display)
    # x not added now (to do not have them in stat.nolog)
    stat <- data.frame(
        MIN = box.coord$ymin_final, 
        QUART1 = box.coord$lower, 
        MEDIAN = box.coord$middle, 
        MEAN = box.coord$MEAN, 
        QUART3 = box.coord$upper, 
        MAX = box.coord$ymax_final, 
        WHISK_INF = box.coord$ymin, 
        BOX_INF = box.coord$lower, 
        NOTCH_INF = box.coord$notchlower, 
        NOTCH_SUP = box.coord$notchupper, 
        BOX_SUP = box.coord$upper, 
        WHISK_SUP = box.coord$ymax, 
        OUTLIERS = box.coord["outliers"], 
        tempo.mean[colnames(tempo.mean) != "MEAN"], 
        COLOR = box.coord$fill, 
        stringsAsFactors = TRUE
    ) # box.coord["outliers"] written like this because it is a list. X coordinates not put now because several features to set
    names(stat)[names(stat) == "outliers"] <- "OUTLIERS"
    stat.nolog <- stat # stat.nolog ini will serve for outputs
    if(y.log != "no"){
        stat.nolog[c("MIN", "QUART1", "MEDIAN", "MEAN", "QUART3", "MAX", "WHISK_INF", "BOX_INF", "NOTCH_INF", "NOTCH_SUP", "BOX_SUP", "WHISK_SUP")] <- ifelse(y.log == "log2", 2, 10)^(stat.nolog[c("MIN", "QUART1", "MEDIAN", "MEAN", "QUART3", "MAX", "WHISK_INF", "BOX_INF", "NOTCH_INF", "NOTCH_SUP", "BOX_SUP", "WHISK_SUP")])
        stat.nolog$OUTLIERS <- lapply(stat.nolog$OUTLIERS, FUN = function(X){ifelse(y.log == "log2", 2, 10)^X})
    }
    # end stat output (will also serve for boxplot and mean display)
    
    
    
    
    
    
    # x coordinates management (for random plotting and for stat display)
    # width commputations
    width.ini <- c(box.coord$xmax - box.coord$xmin)[1] # all the box widths are equal here. Only the first one taken
    width.correct <- width.ini * box.space / 2
    if( ! (identical(stat$BOX, box.coord$group) & identical(stat$PANEL, box.coord$PANEL))){
        tempo.cat <- paste0("INTERNAL CODE ERROR IN ", function.name, "\nidentical(stat$BOX, box.coord$group) & identical(stat$PANEL, box.coord$PANEL) MUST BE IDENTICAL. CODE HAS TO BE MODIFIED")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }else{
        stat <- data.frame(
            stat, 
            X = box.coord$x, 
            X_BOX_INF = box.coord$xmin + width.correct, 
            X_BOX_SUP = box.coord$xmax - width.correct, 
            X_NOTCH_INF = box.coord$x - (box.coord$x - (box.coord$xmin + width.correct)) / 2, 
            X_NOTCH_SUP = box.coord$x + (box.coord$x - (box.coord$xmin + width.correct)) / 2, 
            X_WHISK_INF = box.coord$x - (box.coord$x - (box.coord$xmin + width.correct)) * box.whisker.width, 
            X_WHISK_SUP = box.coord$x + (box.coord$x - (box.coord$xmin + width.correct)) * box.whisker.width, 
            # tempo.mean[colnames(tempo.mean) != "MEAN"], # already added above
            stringsAsFactors = TRUE
        )
        stat$COLOR <- factor(stat$COLOR, levels = unique(categ.color))
        if( ! all(stat$NOTCH_SUP < stat$BOX_SUP & stat$NOTCH_INF > stat$BOX_INF, na.rm = TRUE) & box.notch == TRUE){
            warn.count <- warn.count + 1
            tempo.warn <- paste0("(", warn.count,") SOME NOTCHES ARE BEYOND BOX HINGES. TRY ARGUMENT box.notch = FALSE")
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        }
    }
    dot.jitter <- c((box.coord$xmax - width.correct) - (box.coord$xmin + width.correct))[1] * dot.jitter # real dot.jitter. (box.coord$xmin + width.correct) - (box.coord$xmax - width.correct))[1] is the width of the box. Is equivalent to (box.coord$x - (box.coord$xmin + width.correct))[1] * 2
    # end width commputations
    if( ! is.null(dot.color)){
        # random dots
        if(dot.tidy == FALSE){
            dot.coord.rd1 <- merge(dot.coord, box.coord[c("fill", "PANEL", "group", "x")], by = c("PANEL", "group"), sort = FALSE) # rd for random. Send the coord of the boxes into the coord data.frame of the dots (in the column x.y). WARNING: by = c("PANEL", "group") without fill column because PANEL & group columns are enough as only one value of x column per group number in box.coord. Thus, no need to consider fill column
            if(nrow(dot.coord.rd1) != nrow(dot.coord)){
                tempo.cat <- paste0("INTERNAL CODE ERROR IN ", function.name, "\nTHE merge() FUNCTION DID NOT RETURN A CORRECT dot.coord.rd1 DATA FRAME. CODE HAS TO BE MODIFIED")
                stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            sampled.dot.jitter <- if(nrow(dot.coord.rd1)== 1L){runif(n = nrow(dot.coord.rd1), min = - dot.jitter / 2, max = dot.jitter / 2)}else{sample(x = runif(n = nrow(dot.coord.rd1), min = - dot.jitter / 2, max = dot.jitter / 2), size = nrow(dot.coord.rd1), replace = FALSE)}
            dot.coord.rd2 <- data.frame(dot.coord.rd1, dot.x = dot.coord.rd1$x.y + sampled.dot.jitter, stringsAsFactors = TRUE) # set the dot.jitter thanks to runif and dot.jitter range. Then, send the coord of the boxes into the coord data.frame of the dots (in the column x.y)
            if(length(categ)== 1L){
                tempo.data1 <- unique(data.frame(data1[categ[1]], group = as.integer(data1[, categ[1]]), stringsAsFactors = TRUE)) # categ[1] is factor
                names(tempo.data1)[names(tempo.data1) == categ[1]] <- paste0(categ[1], ".check")
                verif <- paste0(categ[1], ".check")
            }else if(length(categ) == 2L){
                tempo.data1 <- unique(
                    data.frame(
                        data1[c(categ[1], categ[2])], 
                        group = as.integer(factor(paste0(
                            formatC(as.integer(data1[, categ[2]]), width = nchar(max(as.integer(data1[, categ[2]]), na.rm = TRUE)), flag = "0"), # convert factor into numeric with leading zero for proper ranking
                            ".", 
                            formatC(as.integer(data1[, categ[1]]), width = nchar(max(as.integer(data1[, categ[1]]), na.rm = TRUE)), flag = "0")# convert factor into numeric with leading zero for proper ranking
                        )), stringsAsFactors = TRUE) # merge the 2 formatC() to create a new factor. The convertion to integer should recreate the correct group number
                    )
                ) # categ[2] first if categ[2] is used to make the categories in ggplot and categ[1] is used to make the x-axis
                names(tempo.data1)[names(tempo.data1) == categ[1]] <- paste0(categ[1], ".check")
                names(tempo.data1)[names(tempo.data1) == categ[2]] <- paste0(categ[2], ".check")
                verif <- c(paste0(categ[1], ".check"), paste0(categ[2], ".check"))
            }else{
                tempo.cat <- paste0("INTERNAL CODE ERROR IN ", function.name, "\nCODE INCONSISTENCY 3")
                stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            dot.coord.rd3 <- merge(dot.coord.rd2, tempo.data1, by = intersect("group", "group"), sort = FALSE) # send the factors of data1 into coord. WARNING: I have replaced by = "group" by intersect("group", "group") because of an error due to wrong group group merging in dot.coord.rd3
            if(nrow(dot.coord.rd3) != nrow(dot.coord) | ( ! saferTool::comp_2d(dot.coord.rd3[categ], dot.coord.rd3[verif])$identical.content)){
                tempo.cat <- paste0("INTERNAL CODE ERROR IN ", function.name, "\nTHE merge() FUNCTION DID NOT RETURN A CORRECT dot.coord.rd3 DATA FRAME. CODE HAS TO BE MODIFIED")
                stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            # end random dots
        }
        # tidy dots
        # coordinates are recovered during plotting (see dot.coord.tidy1 below)
        # end tidy dots
    }
    # end x coordinates management (for random plotting and for stat display)
    
    
    
    
    
    # boxplot display before dot display if box.fill = TRUE
    coord.names <- NULL
    # creation of the data frame for (main box + legend) and data frame for means
    if(box.notch == FALSE){
        for(i3 in 1:length(categ)){
            if(i3== 1L){
                tempo.polygon <- data.frame(GROUPX = c(t(stat[, rep(categ[i3], 5)])), stringsAsFactors = TRUE)
            }else{
                tempo.polygon <- cbind(tempo.polygon, c(t(stat[, rep(categ[i3], 5)])), stringsAsFactors = TRUE)
            }
        }
        names(tempo.polygon) <- categ
        tempo.polygon <- data.frame(X = c(t(stat[, c("X_BOX_INF", "X_BOX_SUP", "X_BOX_SUP", "X_BOX_INF", "X_BOX_INF")])), Y = c(t(stat[, c("BOX_INF", "BOX_INF", "BOX_SUP", "BOX_SUP", "BOX_INF")])), COLOR = c(t(stat[, c("COLOR", "COLOR", "COLOR", "COLOR", "COLOR")])), BOX = as.character(c(t(stat[, c("BOX", "BOX", "BOX", "BOX", "BOX")]))), tempo.polygon, stringsAsFactors = TRUE)
        if( ! is.null(facet.categ)){
            for(i4 in 1:length(facet.categ)){
                tempo.polygon <- data.frame(tempo.polygon, c(t(stat[, c(facet.categ[i4], facet.categ[i4], facet.categ[i4], facet.categ[i4], facet.categ[i4])])), stringsAsFactors = TRUE)
                names(tempo.polygon)[length(names(tempo.polygon))] <- facet.categ[i4]
            }
        }
    }else{
        for(i3 in 1:length(categ)){
            if(i3== 1L){
                tempo.polygon <- data.frame(GROUPX = c(t(stat[, rep(categ[i3], 11)])), stringsAsFactors = TRUE)
            }else{
                tempo.polygon <- cbind(tempo.polygon, c(t(stat[, rep(categ[i3], 11)])), stringsAsFactors = TRUE)
            }
        }
        names(tempo.polygon) <- categ
        tempo.polygon <- data.frame(X = c(t(stat[, c("X_BOX_INF", "X_BOX_SUP", "X_BOX_SUP", "X_NOTCH_SUP", "X_BOX_SUP", "X_BOX_SUP", "X_BOX_INF", "X_BOX_INF", "X_NOTCH_INF", "X_BOX_INF", "X_BOX_INF")])), Y = c(t(stat[, c("BOX_INF", "BOX_INF", "NOTCH_INF", "MEDIAN", "NOTCH_SUP", "BOX_SUP", "BOX_SUP", "NOTCH_SUP", "MEDIAN", "NOTCH_INF", "BOX_INF")])), COLOR = c(t(stat[, c("COLOR", "COLOR", "COLOR", "COLOR", "COLOR", "COLOR", "COLOR", "COLOR", "COLOR", "COLOR", "COLOR")])), BOX = as.character(c(t(stat[, c("BOX", "BOX", "BOX", "BOX", "BOX", "BOX", "BOX", "BOX", "BOX", "BOX", "BOX")]))), tempo.polygon, stringsAsFactors = TRUE)
        if( ! is.null(facet.categ)){
            for(i4 in 1:length(facet.categ)){
                tempo.polygon <- data.frame(tempo.polygon, c(t(stat[, c(facet.categ[i4], facet.categ[i4], facet.categ[i4], facet.categ[i4], facet.categ[i4], facet.categ[i4], facet.categ[i4], facet.categ[i4], facet.categ[i4], facet.categ[i4], facet.categ[i4])])), stringsAsFactors = TRUE)
                names(tempo.polygon)[length(names(tempo.polygon))] <- facet.categ[i4]
            }
        }
    }
    tempo.polygon$COLOR <- factor(tempo.polygon$COLOR, levels = unique(categ.color))
    if( ! is.null(categ.class.order)){
        for(i3 in 1:length(categ)){
            tempo.polygon[, categ[i3]] <- factor(tempo.polygon[, categ[i3]], levels = categ.class.order[[i3]])
        }
    }
    # modified name of dot.categ column (e.g., "Categ1_DOT") must be included for boxplot using ridy dots
    if( ! is.null(dot.color) & ! is.null(dot.categ)){
        if(dot.categ != ini.dot.categ){
            tempo.polygon <- data.frame(tempo.polygon, GROUPX = tempo.polygon[, ini.dot.categ], stringsAsFactors = TRUE)
            names(tempo.polygon)[names(tempo.polygon) == "GROUPX"] <- dot.categ
            
        }
    }
    tempo.diamon.mean <- data.frame(X = c(t(stat[, c("X", "X_NOTCH_INF", "X", "X_NOTCH_SUP", "X")])), Y = c(t(cbind(stat["MEAN"] - (stat[, "X"] - stat[, "X_NOTCH_INF"]) * tempo.yx.ratio, stat["MEAN"], stat["MEAN"] + (stat[, "X"] - stat[, "X_NOTCH_INF"]) * tempo.yx.ratio, stat["MEAN"], stat["MEAN"] - (stat[, "X"] - stat[, "X_NOTCH_INF"]) * tempo.yx.ratio, stringsAsFactors = TRUE))), COLOR = c(t(stat[, c("COLOR", "COLOR", "COLOR", "COLOR", "COLOR")])), GROUP = c(t(stat[, c("BOX", "BOX", "BOX", "BOX", "BOX")])), stringsAsFactors = TRUE) # stringsAsFactors = TRUE for cbind() because stat["MEAN"] is a data frame. Otherwise, stringsAsFactors is not an argument for cbind() on vectors
    if( ! is.null(facet.categ)){
        for(i3 in 1:length(facet.categ)){
            tempo.diamon.mean <- data.frame(tempo.diamon.mean, c(t(stat[, c(facet.categ[i3], facet.categ[i3], facet.categ[i3], facet.categ[i3], facet.categ[i3])])), stringsAsFactors = TRUE)
            names(tempo.diamon.mean)[length(names(tempo.diamon.mean))] <- facet.categ[i3]
        }
    }
    tempo.diamon.mean$COLOR <- factor(tempo.diamon.mean$COLOR, levels = unique(categ.color))
    # end creation of the data frame for (main box + legend) and data frame for means
    if(box.fill == TRUE){
        # assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_boxplot(data = data1, mapping = ggplot2::aes_string(x = categ[1], y = y, color = categ[length(categ)], fill = categ[length(categ)]), position = ggplot2::position_dodge(width = NULL), width = box.width, size = box.line.size, notch = box.notch, coef = if(box.whisker.kind == "no"){0}else if(box.whisker.kind == "std"){1.5}else if(box.whisker.kind == "max"){Inf}, alpha = box.alpha, outlier.shape = if( ! is.null(dot.color)){NA}else{21}, outlier.color = if( ! is.null(dot.color)){NA}else{dot.border.color}, outlier.fill = if( ! is.null(dot.color)){NA}else{NULL}, outlier.size = if( ! is.null(dot.color)){NA}else{dot.size}, outlier.stroke = if( ! is.null(dot.color)){NA}else{dot.border.size}, outlier.alpha = if( ! is.null(dot.color)){NA}else{dot.alpha})) # the color, size, etc. of the outliers are dealt here. outlier.color = NA to do not plot outliers when dots are already plotted. Finally, boxplot redrawn (see below)
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_polygon(
            data = tempo.polygon, 
            mapping = ggplot2::aes_string(x = "X", y = "Y", group = "BOX", fill = categ[length(categ)], color = categ[length(categ)]), 
            size = box.line.size, 
            alpha = box.alpha # works only for fill, not for color
        ))
        coord.names <- c(coord.names, "main.box")
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = X, xend = X, y = BOX_SUP, yend = WHISK_SUP, group = categ[length(categ)]), color = "black", size = box.line.size, alpha = box.alpha)) # 
        coord.names <- c(coord.names, "sup.whisker")
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = X, xend = X, y = BOX_INF, yend = WHISK_INF, group = categ[length(categ)]), color = "black", size = box.line.size, alpha = box.alpha)) # 
        coord.names <- c(coord.names, "inf.whisker")
        if(box.whisker.width > 0){
            assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = X_WHISK_INF, xend = X_WHISK_SUP, y = WHISK_SUP, yend = WHISK_SUP, group = categ[length(categ)]), color = "black", size = box.line.size, alpha = box.alpha, lineend = "round")) # 
            coord.names <- c(coord.names, "sup.whisker.edge")
            assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = X_WHISK_INF, xend = X_WHISK_SUP, y = WHISK_INF, yend = WHISK_INF, group = categ[length(categ)]), color = "black", size = box.line.size, alpha = box.alpha, lineend = "round")) # 
            coord.names <- c(coord.names, "inf.whisker.edge")
        }
        if(box.mean == TRUE){
            # assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_point(data = stat, mapping = ggplot2::aes_string(x = "X", y = "MEAN", group = categ[length(categ)]), shape = 23, stroke = box.line.size * 2, fill = stat$COLOR, size = box.mean.size, color = "black", alpha = box.alpha)) # group used in aesthetic to do not have it in the legend
            assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_polygon(
                data = tempo.diamon.mean, 
                mapping = ggplot2::aes(x = X, y = Y, group = GROUP), 
                fill = tempo.diamon.mean[, "COLOR"], 
                color = hsv(0, 0, 0, alpha = box.alpha), # outline of the polygon in black but with alpha
                size = box.line.size, 
                alpha = box.alpha
            ))
            coord.names <- c(coord.names, "mean")
        }
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = if(box.notch == FALSE){X_BOX_INF}else{X_NOTCH_INF}, xend = if(box.notch == FALSE){X_BOX_SUP}else{X_NOTCH_SUP}, y = MEDIAN, yend = MEDIAN, group = categ[length(categ)]), color = "black", size = box.line.size * 2, alpha = box.alpha)) # 
        coord.names <- c(coord.names, "median")
    }
    # end boxplot display before dot display if box.fill = TRUE
    
    
    
    
    
    
    # dot display
    if( ! is.null(dot.color)){
        if(dot.tidy == FALSE){
            if(is.null(dot.categ)){
                if(dot.border.size == 0){
                    assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_point(
                        data = dot.coord.rd3, 
                        mapping = ggplot2::aes_string(x = "dot.x", y = "y", group = categ[length(categ)]), 
                        size = dot.size, 
                        shape = 19, 
                        color = dot.coord.rd3$dot.color, 
                        alpha = dot.alpha
                    )) # group used in aesthetic to do not have it in the legend. Here ggplot2::scale_discrete_manual() cannot be used because of the group easthetic
                }else{
                    assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_point(
                        data = dot.coord.rd3, 
                        mapping = ggplot2::aes_string(x = "dot.x", y = "y", group = categ[length(categ)]), 
                        shape = 21, 
                        stroke = dot.border.size, 
                        color = if(is.null(dot.border.color)){dot.coord.rd3$dot.color}else{rep(dot.border.color, nrow(dot.coord.rd3))}, 
                        size = dot.size, 
                        fill = dot.coord.rd3$dot.color, 
                        alpha = dot.alpha
                    )) # group used in aesthetic to do not have it in the legend. Here ggplot2::scale_discrete_manual() cannot be used because of the group easthetic
                }
            }else{
                if(dot.border.size == 0){
                    assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_point(
                        data = dot.coord.rd3, 
                        mapping = ggplot2::aes_string(x = "dot.x", y = "y", alpha = dot.categ), 
                        size = dot.size, 
                        shape = 19, 
                        color = dot.coord.rd3$dot.color
                    )) # group used in aesthetic to do not have it in the legend. Here ggplot2::scale_discrete_manual() cannot be used because of the group easthetic
                }else{
                    assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_point(
                        data = dot.coord.rd3, 
                        mapping = ggplot2::aes_string(x = "dot.x", y = "y", alpha = dot.categ), 
                        size = dot.size, 
                        shape = 21, 
                        stroke = dot.border.size, 
                        color = if(is.null(dot.border.color)){dot.coord.rd3$dot.color}else{rep(dot.border.color, nrow(dot.coord.rd3))}, 
                        fill = dot.coord.rd3$dot.color
                    )) # group used in aesthetic to do not have it in the legend. Here ggplot2::scale_discrete_manual() cannot be used because of the group easthetic
                }
                assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "alpha", name = dot.legend.name, values = rep(dot.alpha, length(dot.categ.class.order)), guide = ggplot2::guide_legend(override.aes = list(fill = dot.color, color = if(is.null(dot.border.color)){dot.color}else{dot.border.color}, stroke = dot.border.size, alpha = dot.alpha)))) # values are the values of color (which is the border color in geom_box. WARNING: values = categ.color takes the numbers to make the colors if categ.color is a factor
            }
            coord.names <- c(coord.names, "dots")
        }else if(dot.tidy == TRUE){
            # here plot using group -> no scale
            assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_dotplot(
                data = dot.coord, 
                mapping = ggplot2::aes_string(x = categ[1], y = "y", group = "group"), # not dot.categ here because the classes of dot.categ create new separations
                position = ggplot2::position_dodge(width = box.width), 
                binpositions = "all", 
                binaxis = "y", 
                stackdir = "center", 
                alpha = dot.alpha, 
                fill = dot.coord$dot.color, 
                stroke = dot.border.size, 
                color = if(is.null(dot.border.color)){dot.coord$dot.color}else{rep(dot.border.color, nrow(dot.coord))}, 
                show.legend = FALSE, # WARNING: do not use show.legend = TRUE because it uses the arguments outside aes() as aesthetics (here color and fill). Thus I must find a way using ggplot2::scale_discrete_manual()
                binwidth = (y.lim[2] - y.lim[1]) / dot.tidy.bin.nb
            )) # geom_dotplot ggplot2 v3.3.0: I had to remove rev() in fill and color # very weird behavior of geom_dotplot ggplot2 v3.2.1, (1) because with aes group = (to avoid legend), the dot plotting is not good in term of coordinates, and (2) because data1 seems reorderer according to x = categ[1] before plotting. Thus, I have to use fill = dot.coord[rev(order(dot.coord[, categ[1]], decreasing = TRUE)), "dot.color"] to have the good corresponding colors # show.legend option do not remove the legend, only the aesthetic of the legend (dot, line, etc.)
            coord.names <- c(coord.names, "dots")
            if( ! is.null(dot.categ)){
                assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_dotplot(
                    data = dot.coord, 
                    mapping = ggplot2::aes_string(x = categ[1], y = "y", alpha = dot.categ), # not dot.categ here because the classes of dot.categ create new separations
                    position = ggplot2::position_dodge(width = box.width), 
                    binpositions = "all", 
                    binaxis = "y", 
                    stackdir = "center", 
                    fill = NA, 
                    stroke = NA, 
                    color = NA, 
                    # WARNING: do not use show.legend = TRUE because it uses the arguments outside aes() as aesthetics (here color and fill). Thus I must find a way using ggplot2::scale_discrete_manual()
                    binwidth = (y.lim[2] - y.lim[1]) / dot.tidy.bin.nb
                )) # geom_dotplot ggplot2 v3.3.0: I had to remove rev() in fill and color # very weird behavior of geom_dotplot ggplot2 v3.2.1, (1) because with aes group = (to avoid legend), the dot plotting is not good in term of coordinates, and (2) because data1 seems reorderer according to x = categ[1] before plotting. Thus, I have to use fill = dot.coord[rev(order(dot.coord[, categ[1]], decreasing = TRUE)), "dot.color"] to have the good corresponding colors # show.legend option do not remove the legend, only the aesthetic of the legend (dot, line, etc.)
                # assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "linetype", name = dot.legend.name, values = rep(1, length(categ.color)))) # values = rep("black", length(categ.color)) are the values of color (which is the border color of dots), and this modify the border color on the plot. WARNING: values = categ.color takes the numbers to make the colors if categ.color is a factor
                coord.names <- c(coord.names, "bad_remove")
                assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "alpha", name = dot.legend.name, values = rep(dot.alpha, length(dot.categ.class.order)), labels = dot.categ.class.order, guide = ggplot2::guide_legend(title = if(ini.dot.categ == categ[length(categ)]){dot.categ}else{ini.dot.categ}, override.aes = list(fill = levels(dot.coord$dot.color), color = if(is.null(dot.border.color)){levels(dot.coord$dot.color)}else{dot.border.color}, stroke = dot.border.size, alpha = dot.alpha)))) # values are the values of color (which is the border color in geom_box. WARNING: values = categ.color takes the numbers to make the colors if categ.color is a factor
            }
            # coordinates of tidy dots
            tempo.coord <- ggplot2::ggplot_build(eval(parse(text = paste(paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + "))))$data # to have the tidy dot coordinates
            if(length(which(sapply(X = tempo.coord, FUN = function(X){any(names(X) == "binwidth", na.rm = TRUE)}))) != 1){ # detect the compartment of tempo.coord which is the binned data frame
                # if(length(which(sapply(tempo.coord, FUN = nrow) == nrow(data1))) > if(is.null(dot.categ)){1}else{2}){ # this does not work if only one dot per class, thus replaced by above # if(is.null(dot.categ)){1}else{2} because 1 dotplot if dot.categ is NULL and 2 dotplots if not, with the second being a blank dotplot with wrong coordinates. Thus take the first in that situation
                tempo.cat <- paste0("INTERNAL CODE ERROR IN ", function.name, "\nEITHER MORE THAN 1 OR NO COMPARTMENT HAVING A DATA FRAME WITH binwidth AS COLUMN NAME IN THE tempo.coord LIST (FOR TIDY DOT COORDINATES). CODE HAS TO BE MODIFIED")
                stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }else{
                # dot.coord.tidy1 <- tempo.coord[[which(sapply(tempo.coord, FUN = nrow) == nrow(data1))[1]]] # this does not work if only one dot per class, thus replaced by above # the second being a blank dotplot with wrong coordinates. Thus take the first whatever situation
                dot.coord.tidy1 <- tempo.coord[[which(sapply(X = tempo.coord, FUN = function(X){any(names(X) == "binwidth", na.rm = TRUE)}))]] # detect the compartment of tempo.coord which is the binned data frame
                dot.coord.tidy1$x <- as.numeric(dot.coord.tidy1$x) # because weird class
                dot.coord.tidy1$PANEL <- as.numeric(dot.coord.tidy1$PANEL) # because numbers as levels. But may be a problem is facet are reordered ?
            }
            # tempo.box.coord <- merge(box.coord, unique(dot.coord[, c("PANEL", "group", categ)]), by = c("PANEL", "group"), sort = FALSE) # not required anymore because box.coord already contains categ do not add dot.categ and tidy_group_coord here because the coordinates are for stats. Add the categ in box.coord. WARNING: by = c("PANEL", "group") without fill column because PANEL & group columns are enough as only one value of x column per group number in box.coord. Thus, no need to consider fill column
            # below inactivated because not true when dealing with dot.categ different from categ
            # if(nrow(tempo.box.coord) != nrow(box.coord)){
            # tempo.cat <- paste0("INTERNAL CODE ERROR IN ", function.name, "\nTHE merge() FUNCTION DID NOT RETURN A CORRECT tempo.box.coord DATA FRAME. CODE HAS TO BE MODIFIED")
            # stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            # }
            dot.coord.tidy2 <- merge(dot.coord.tidy1, box.coord[c("fill", "PANEL", "group", "x", categ)], by = c("PANEL", "group"), sort = FALSE) # send the coord of the boxes into the coord data.frame of the dots (in the column x.y).WARNING: by = c("PANEL", "group") without fill column because PANEL & group columns are enough as only one value of x column per group number in tempo.box.coord. Thus, no need to consider fill colum # DANGER: from here the fill.y and x.y (from tempo.box.coord) are not good in dot.coord.tidy2. It is ok because Categ1 Categ2 from tempo.box.coord are ok with the group column from dot.coord.tidy1. This is due to the fact that dot.coord.tidy resulting from geom_dotplot does not make the same groups as the other functions
            if(nrow(dot.coord.tidy2) != nrow(dot.coord)){
                tempo.cat <- paste0("INTERNAL CODE ERROR IN ", function.name, "\nTHE merge() FUNCTION DID NOT RETURN A CORRECT dot.coord.tidy2 DATA FRAME. CODE HAS TO BE MODIFIED")
                stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            # From here, check for dot.coord.tidy3 which wil be important for stat over the plot. WARNING: dot.categ has nothing to do here for stat coordinates. Thus, not in tempo.data1
            if(length(categ)== 1L){
                tempo.data1 <- unique(data.frame(data1[categ[1]], group = as.integer(data1[, categ[1]]), stringsAsFactors = TRUE)) # categ[1] is factor
                names(tempo.data1)[names(tempo.data1) == categ[1]] <- paste0(categ[1], ".check")
                verif <- paste0(categ[1], ".check")
            }else if(length(categ) == 2L){
                tempo.data1 <- unique(
                    data.frame(
                        data1[c(categ[1], categ[2])], 
                        group = as.integer(factor(paste0(
                            formatC(as.integer(data1[, categ[2]]), width = nchar(max(as.integer(data1[, categ[2]]), na.rm = TRUE)), flag = "0"), # convert factor into numeric with leading zero for proper ranking
                            ".", 
                            formatC(as.integer(data1[, categ[1]]), width = nchar(max(as.integer(data1[, categ[1]]), na.rm = TRUE)), flag = "0")# convert factor into numeric with leading zero for proper ranking
                        )), stringsAsFactors = TRUE) # merge the 2 formatC() to create a new factor. The convertion to integer should recreate the correct group number
                    )
                ) # categ[2] first if categ[2] is used to make the categories in ggplot and categ[1] is used to make the x-axis
                names(tempo.data1)[names(tempo.data1) == categ[1]] <- paste0(categ[1], ".check")
                names(tempo.data1)[names(tempo.data1) == categ[2]] <- paste0(categ[2], ".check")
                verif <- c(paste0(categ[1], ".check"), paste0(categ[2], ".check"))
            }else{
                tempo.cat <- paste0("INTERNAL CODE ERROR IN ", function.name, "\nCODE INCONSISTENCY 4")
                stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            dot.coord.tidy3 <- merge(dot.coord.tidy2, tempo.data1, by = intersect("group", "group"), sort = FALSE) # send the factors of data1 into coord. WARNING: I have tested intersect("group", "group") instead of by = "group". May be come back to by = "group" in case of error. But I did this because of an error in dot.coord.rd3 above
            if(nrow(dot.coord.tidy3) != nrow(dot.coord) | ( ! saferTool::comp_2d(dot.coord.tidy3[categ], dot.coord.tidy3[verif])$identical.content)){
                tempo.cat <- paste0("INTERNAL CODE ERROR IN ", function.name, "\nTHE merge() FUNCTION DID NOT RETURN A CORRECT dot.coord.tidy3 DATA FRAME. CODE HAS TO BE MODIFIED")
                stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            # end coordinates of tidy dots
        }
    }
    # end dot display
    
    
    
    # boxplot display (if box.fill = FALSE, otherwise, already plotted above)
    if(box.fill == TRUE){
        # overcome "work only for the filling of boxes, not for the frame. See https://github.com/tidyverse/ggplot2/issues/252"
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "fill", name = box.legend.name, values = if(length(categ.color)== 1L){rep(categ.color, length(unique(data1[, categ[length(categ)]])))}else{categ.color}, guide = ggplot2::guide_legend(order = 1))) #, guide = ggplot2::guide_legend(override.aes = list(fill = levels(tempo.polygon$COLOR), color = "black")))) # values are the values of color (which is the border color in geom_box. WARNING: values = categ.color takes the numbers to make the colors if categ.color is a factor
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "color", name = box.legend.name, values = rep(hsv(0, 0, 0, alpha = box.alpha), length(unique(data1[, categ[length(categ)]]))), guide = ggplot2::guide_legend(order = 1))) # , guide = ggplot2::guide_legend(override.aes = list(color = "black", alpha = box.alpha)))) # values are the values of color (which is the border color in geom_box. WARNING: values = categ.color takes the numbers to make the colors if categ.color is a factor # outline of the polygon in black but with alpha
    }else{
        # assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_boxplot(data = data1, mapping = ggplot2::aes_string(x = categ[1], y = y, color = categ[length(categ)], fill = categ[length(categ)]), position = ggplot2::position_dodge(width = NULL), width = box.width, size = box.line.size, notch = box.notch, alpha = box.alpha, coef = if(box.whisker.kind == "no"){0}else if(box.whisker.kind == "std"){1.5}else if(box.whisker.kind == "max"){Inf}, outlier.shape = if( ! is.null(dot.color)){NA}else{21}, outlier.color = if( ! is.null(dot.color)){NA}else{if(dot.border.size == 0){NA}else{dot.border.color}}, outlier.fill = if( ! is.null(dot.color)){NA}else{NULL}, outlier.size = if( ! is.null(dot.color)){NA}else{dot.size}, outlier.stroke = if( ! is.null(dot.color)){NA}else{dot.border.size}, outlier.alpha = if( ! is.null(dot.color)){NA}else{dot.alpha})) # the color, size, etc. of the outliers are dealt here. outlier.color = NA to do not plot outliers when dots are already plotted
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_path(
            data = tempo.polygon, 
            mapping = ggplot2::aes_string(x = "X", y = "Y", group = "BOX", color = categ[length(categ)]), 
            size = box.line.size, 
            alpha = box.alpha, 
            lineend = "round", 
            linejoin = "round"
        ))
        coord.names <- c(coord.names, "main.box")
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = if(box.notch == FALSE){X_BOX_INF}else{X_NOTCH_INF}, xend = if(box.notch == FALSE){X_BOX_SUP}else{X_NOTCH_SUP}, y = MEDIAN, yend = MEDIAN, group = categ[length(categ)]), color = stat$COLOR, size = box.line.size * 2, alpha = box.alpha)) # 
        coord.names <- c(coord.names, "median")
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = X, xend = X, y = BOX_SUP, yend = WHISK_SUP, group = categ[length(categ)]), color = stat$COLOR, size = box.line.size, alpha = box.alpha)) # 
        coord.names <- c(coord.names, "sup.whisker")
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = X, xend = X, y = BOX_INF, yend = WHISK_INF, group = categ[length(categ)]), color = stat$COLOR, size = box.line.size, alpha = box.alpha)) # 
        coord.names <- c(coord.names, "inf.whisker")
        if(box.whisker.width > 0){
            assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = X_WHISK_INF, xend = X_WHISK_SUP, y = WHISK_SUP, yend = WHISK_SUP, group = categ[length(categ)]), color = stat$COLOR, size = box.line.size, alpha = box.alpha, lineend = "round")) # 
            coord.names <- c(coord.names, "sup.whisker.edge")
            assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = X_WHISK_INF, xend = X_WHISK_SUP, y = WHISK_INF, yend = WHISK_INF, group = categ[length(categ)]), color = stat$COLOR, size = box.line.size, alpha = box.alpha, lineend = "round")) # 
            coord.names <- c(coord.names, "inf.whisker.edge")
        }
        if(box.mean == TRUE){
            # assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_point(data = stat, mapping = ggplot2::aes_string(x = "X", y = "MEAN", group = categ[length(categ)]), shape = 23, stroke = box.line.size * 2, color = stat$COLOR, size = box.mean.size, fill = NA, alpha = box.alpha)) # group used in aesthetic to do not have it in the legend. Here ggplot2::scale_discrete_manual() cannot be used because of the group easthetic
            assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_path(
                data = tempo.diamon.mean, 
                mapping = ggplot2::aes(x = X, y = Y, group = GROUP), 
                color = tempo.diamon.mean[, "COLOR"], 
                size = box.line.size, 
                alpha = box.alpha, 
                lineend = "round", 
                linejoin = "round"
            ))
            coord.names <- c(coord.names, "mean")
        }
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "fill", name = box.legend.name, values = rep(NA, length(unique(data1[, categ[length(categ)]]))))) #, guide = ggplot2::guide_legend(override.aes = list(color = categ.color)))) # values are the values of color (which is the border color in geom_box. WARNING: values = categ.color takes the numbers to make the colors if categ.color is a factor
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "color", name = box.legend.name, values = if(length(categ.color)== 1L){rep(categ.color, length(unique(data1[, categ[length(categ)]])))}else{categ.color}, guide = ggplot2::guide_legend(override.aes = list(alpha = if(plot == TRUE & ((length(dev.list()) > 0 & names(dev.cur()) == "windows") | (length(dev.list()) == 0L & Sys.info()["sysname"] == "Windows"))){1}else{box.alpha})))) # , guide = ggplot2::guide_legend(override.aes = list(color = as.character(categ.color))))) # values are the values of color (which is the border color in geom_box. WARNING: values = categ.color takes the numbers to make the colors if categ.color is a factor
        if(plot == TRUE & ((length(dev.list()) > 0 & names(dev.cur()) == "windows") | (length(dev.list()) == 0L & Sys.info()["sysname"] == "Windows"))){ # if any Graph device already open and this device is "windows", or if no Graph device opened yet and we are on windows system -> prevention of alpha legend bug on windows using value 1
            # to avoid a bug on windows: if alpha argument is different from 1 for lines (transparency), then lines are not correctly displayed in the legend when using the R GUI (bug https://github.com/tidyverse/ggplot2/issues/2452). No bug when using a pdf
            warn.count <- warn.count + 1
            tempo.warn <- paste0("(", warn.count,") GRAPHIC DEVICE USED ON A WINDOWS SYSTEM ->\nTRANSPARENCY OF THE LINES IS INACTIVATED IN THE LEGEND TO PREVENT A WINDOWS DEPENDENT BUG (SEE https://github.com/tidyverse/ggplot2/issues/2452)\nTO OVERCOME THIS ON WINDOWS, USE ANOTHER DEVICE (pdf() FOR INSTANCE)")
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        }
    }
    if(box.alpha == 0){ # remove box legend because no boxes drawn
        # add this after the scale_xxx_manual() for boxplots
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::guides(fill = "none", color = "none")) # inactivate the legend
    }
    # end boxplot display (if box.fill = FALSE, otherwise, already plotted above)
    
    
    
    
    # stat display
    # layer after dots but ok, behind dots on the plot
    if( ! is.null(stat.pos)){
        warn.count <- warn.count + 1
        tempo.warn <- paste0("(", warn.count,") NUMBERS DISPLAYED ARE ", ifelse(stat.mean == FALSE, "MEDIANS", "MEANS"))
        warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        if(stat.pos == "top"){
            tempo.stat <- data.frame(stat, Y = y.lim[2], stringsAsFactors = TRUE) # I had to create a data frame for geom_tex() so that facet is taken into account, (ggplot2::annotate() does not deal with facet because no data and mapping arguments). Of note, facet.categ is in tempo.stat, via tempo.mean, via dot.coord
            if(stat.mean == FALSE){tempo.stat$MEDIAN <- formatC(stat.nolog$MEDIAN, digit = 2, drop0trailing = TRUE, format = "f")}else{tempo.stat$MEAN <- formatC(stat.nolog$MEAN, digit = 2, drop0trailing = TRUE, format = "f")}
            assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_text(
                data = tempo.stat, 
                mapping = ggplot2::aes_string(x = "X", y = "Y", label = ifelse(stat.mean == FALSE, "MEDIAN", "MEAN")),
                size = stat.size, 
                color = "black", 
                angle = stat.angle, 
                hjust = stat.just$hjust, 
                vjust = stat.just$vjust
            )) # stat$X used here because identical to stat.nolog but has the X. WARNING: no need of order() for labels because box.coord$x set the order. For justification, see https://stackoverflow.com/questions/7263849/what-do-hjust-and-vjust-do-when-making-a-plot-using-ggplot
            coord.names <- c(coord.names, "stat.pos")
        }else if(stat.pos == "above"){
            # stat coordinates
            if( ! is.null(dot.color)){ # for text just above max dot
                if(dot.tidy == FALSE){
                    tempo.stat.ini <- dot.coord.rd3
                }else if(dot.tidy == TRUE){
                    tempo.stat.ini <- dot.coord.tidy3
                    tempo.stat.ini$x.y <- tempo.stat.ini$x.x # this is just to be able to use tempo.stat.ini$x.y for untidy or tidy dots (remember that dot.coord.tidy3$x.y is not good, see above)
                }
                stat.coord1 <- aggregate(x = tempo.stat.ini["y"], by = {x.env <- if(length(categ)== 1L){list(tempo.stat.ini$group, tempo.stat.ini$PANEL, tempo.stat.ini$x.y, tempo.stat.ini[, categ[1]])}else if(length(categ) == 2L){list(tempo.stat.ini$group, tempo.stat.ini$PANEL, tempo.stat.ini$x.y, tempo.stat.ini[, categ[1]], tempo.stat.ini[, categ[2]])} ; names(x.env) <- if(length(categ)== 1L){c("group", "PANEL", "x.y", categ[1])}else if(length(categ) == 2L){c("group", "PANEL", "x.y", categ[1], categ[2])} ; x.env}, FUN = min, na.rm = TRUE)
                names(stat.coord1)[names(stat.coord1) == "y"] <- "dot.min"
                stat.coord2 <- aggregate(x = tempo.stat.ini["y"], by = {x.env <- if(length(categ)== 1L){list(tempo.stat.ini$group, tempo.stat.ini$PANEL, tempo.stat.ini$x.y, tempo.stat.ini[, categ[1]])}else if(length(categ) == 2L){list(tempo.stat.ini$group, tempo.stat.ini$PANEL, tempo.stat.ini$x.y, tempo.stat.ini[, categ[1]], tempo.stat.ini[, categ[2]])} ; names(x.env) <- if(length(categ)== 1L){c("group", "PANEL", "x.y", categ[1])}else if(length(categ) == 2L){c("group", "PANEL", "x.y", categ[1], categ[2])} ; x.env}, FUN = max, na.rm = TRUE)
                names(stat.coord2) <- paste0(names(stat.coord2), "_from.dot.max")
                names(stat.coord2)[names(stat.coord2) == "y_from.dot.max"] <- "dot.max"
                stat.coord3 <- cbind(box.coord[order(box.coord$group, box.coord$PANEL), ], stat.coord1[order(stat.coord1$group, stat.coord1$x.y), ], stat.coord2[order(stat.coord2$group, stat.coord2$x.y), ], stringsAsFactors = TRUE) # 
                if( ! all(identical(round(stat.coord3$x, 9), round(as.numeric(stat.coord3$x.y), 9)), na.rm = TRUE)){ # as.numeric() because stat.coord3$x is class "mapped_discrete" "numeric"
                    tempo.cat <- paste0("INTERNAL CODE ERROR IN ", function.name, "\nFUSION OF box.coord, stat.coord1 AND stat.coord2 ACCORDING TO box.coord$x, stat.coord1$x.y AND stat.coord2$x.y IS NOT CORRECT. CODE HAS TO BE MODIFIED")
                    stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
                }
                # text.coord <- stat.coord3[, c("x", "group", "dot.min", "dot.max")]
                # names(text.coord)[names(text.coord) == "dot.min"] <- "text.min.pos"
                #names(text.coord)[names(text.coord) == "dot.max"] <- "text.max.pos"
                box.coord <- box.coord[order(box.coord$x, box.coord$group, box.coord$PANEL), ]
                # text.coord <- text.coord[order(text.coord$x), ] # to be sure to have the two objects in the same order for x. WARNING: cannot add identical(as.integer(text.coord$group), as.integer(box.coord$group)) because with error, the correspondence between x and group is not the same
                stat.coord3 <- stat.coord3[order(stat.coord3$x, stat.coord3$group, stat.coord3$PANEL), ] # to be sure to have the two objects in the same order for x. WARNING: cannot add identical(as.integer(text.coord$group), as.integer(box.coord$group)) because with error, the correspondence between x and group is not the same
                if( ! (identical(box.coord$x, stat.coord3$x) & identical(box.coord$group, stat.coord3$group) & identical(box.coord$PANEL, stat.coord3$PANEL))){
                    tempo.cat <- paste0("INTERNAL CODE ERROR IN ", function.name, "\ntext.coord AND box.coord DO NOT HAVE THE SAME x, group AND PANEL COLUMN CONTENT")
                    stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
                }
            }else{
                stat.coord3 <- box.coord
            }
            stat.coord3 <- data.frame(
                stat.coord3, 
                Y = stat.coord3[, ifelse(
                    is.null(dot.color), 
                    ifelse(diff(y.lim) > 0, "ymax", "ymin"), 
                    ifelse(diff(y.lim) > 0, "ymax_final", "ymin_final")
                )], 
                stringsAsFactors = TRUE
            ) # ymax is top whisker, ymax_final is top dot
            # stat.coord3 <- data.frame(stat.coord3, Y = vector("numeric", length = nrow(stat.coord3)), stringsAsFactors = TRUE)
            # check.Y <- as.logical(stat.coord3$Y) # convert everything in Y into FALSE (because Y is full of zero)
            # end stat coordinates
            # stat display
            # performed twice: first for y values >=0, then y values < 0, because only a single value allowed for hjust anf vjust
            if(stat.mean == FALSE){
                tempo.center.ref <- "middle"
            }else{
                tempo.center.ref <- "MEAN"
            }
            # if(is.null(dot.color)){
            # tempo.low.ref <- "ymin"
            # tempo.high.ref <- "ymax"
            # }else{
            # tempo.low.ref <- "ymin_final"
            # tempo.high.ref <- "ymax_final"
            # }
            # tempo.log.high <- if(diff(y.lim) > 0){stat.coord3[, tempo.center.ref] >= 0}else{stat.coord3[, tempo.center.ref] < 0}
            # tempo.log.low <- if(diff(y.lim) > 0){stat.coord3[, tempo.center.ref] < 0}else{stat.coord3[, tempo.center.ref] >= 0}
            # stat.coord3$Y[tempo.log.high] <- stat.coord3[tempo.log.high, tempo.high.ref]
            # stat.coord3$Y[tempo.log.low] <- stat.coord3[tempo.log.low, tempo.low.ref]
            # add distance
            stat.coord3$Y <- stat.coord3$Y + diff(y.lim) * stat.dist / 100
            # end add distance
            # correct median or mean text format
            if(y.log != "no"){
                stat.coord3[, tempo.center.ref] <- ifelse(y.log == "log2", 2, 10)^(stat.coord3[, tempo.center.ref])
            }
            stat.coord3[, tempo.center.ref] <- formatC(stat.coord3[, tempo.center.ref], digit = 2, drop0trailing = TRUE, format = "f")
            # end correct median or mean text format
            # if(any(tempo.log.high) == TRUE){
            # tempo.stat <- stat.coord3[tempo.log.high,]
            assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_text(
                data = stat.coord3, 
                mapping = ggplot2::aes_string(x = "x", y = "Y", label = tempo.center.ref),
                size = stat.size, 
                color = "black", 
                angle = stat.angle, 
                hjust = stat.just$hjust, 
                vjust = stat.just$vjust
            )) # WARNING: no need of order() for labels because box.coord$x set the order
            coord.names <- c(coord.names, "stat.pos")
            # }
            # if(any(tempo.log.low) == TRUE){
            # tempo.stat <- stat.coord3[tempo.log.low,]
            # assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_text(
            # data = tempo.stat, 
            # mapping = ggplot2::aes_string(x = "x", y = "Y", label = tempo.center.ref),
            # size = stat.size, 
            # color = "black", 
            # hjust = ifelse(vertical == TRUE, 0.5, 0.5 + stat.dist), 
            # vjust = ifelse(vertical == TRUE, 0.5 + stat.dist, 0.5)
            # )) # WARNING: no need of order() for labels because box.coord$x set the order
            # coord.names <- c(coord.names, "stat.pos.negative")
            # }
            # end stat display
        }else{
            tempo.cat <- paste0("INTERNAL CODE ERROR IN ", function.name, "\nCODE INCONSISTENCY 5")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end stat display
    # legend management
    if(legend.show == FALSE){ # must be here because must be before bef.final.plot <- 
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::guides(fill = "none", color = "none", alpha = "none")) # inactivate the initial legend
    }
    # end legend management
    
    
    
    # y scale management (cannot be before dot plot management)
    # the rescaling aspect is complicated and not intuitive. See:
    # explaination: https://github.com/tidyverse/ggplot2/issues/3948
    # the oob argument of scale_y_continuous() https://ggplot2.tidyverse.org/reference/scale_continuous.html
    # see also https://github.com/rstudio/cheatsheets/blob/master/data-visualization-2.1.pdf
    # secondary ticks
    bef.final.plot <- ggplot2::ggplot_build(eval(parse(text = paste(paste(paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + "), ' + if(vertical == TRUE){ggplot2::scale_y_continuous(expand = c(0, 0), limits = sort(y.lim), oob = scales::rescale_none)}else{ggplot2::coord_flip(ylim = y.lim)}')))) # here I do not need the x-axis and y-axis orientation, I just need the number of main ticks and the legend. I DI NOT UNDERSTAND THE COMMENT HERE BECAUSE WE NEED COORD_FLiP
    tempo.coord <- bef.final.plot$layout$panel_params[[1]]
    # y.second.tick.positions: coordinates of secondary ticks (only if y.second.tick.nb argument is non NULL or if y.log argument is different from "no")
    if(y.log != "no"){ # integer main ticks for log2 and log10
        tempo.scale <- (as.integer(min(y.lim, na.rm = TRUE)) - 1):(as.integer(max(y.lim, na.rm = TRUE)) + 1)
    }else{
        tempo <- if(is.null(attributes(tempo.coord$y$breaks))){tempo.coord$y$breaks}else{unlist(attributes(tempo.coord$y$breaks))}
        if(all(is.na(tempo))){# all() without na.rm -> ok because is.na() cannot be NA
            tempo.cat <- paste0("INTERNAL CODE ERROR IN ", function.name, "\nONLY NA IN tempo.coord$y$breaks")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        tempo.scale <- saferGraph::scale2(lim = y.lim, n = ifelse(is.null(y.tick.nb), length(tempo[ ! is.na(tempo)]), y.tick.nb)) # in ggplot 3.3.0, tempo.coord$y.major_source replaced by tempo.coord$y$breaks. If fact: n = ifelse(is.null(y.tick.nb), length(tempo[ ! is.na(tempo)]), y.tick.nb)) replaced by n = ifelse(is.null(y.tick.nb), 4, y.tick.nb))
    }
    y.second.tick.values <- NULL
    y.second.tick.pos <- NULL
    if(y.log != "no"){
        tempo <- saferGraph::inter_ticks(lim = y.lim, log = y.log)
        y.second.tick.values <- tempo$values
        y.second.tick.pos <- tempo$coordinates
        # if(vertical == TRUE){ # do not remove in case the bug is fixed
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::annotate(geom = "segment", y = y.second.tick.pos, yend = y.second.tick.pos, x = tempo.coord$x.range[1], xend = tempo.coord$x.range[1] + diff(tempo.coord$x.range) / 80))
        # }else{ # not working because of the ggplot2 bug
        # assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::annotate(geom = "segment", x = y.second.tick.pos, xend = y.second.tick.pos, y = tempo.coord$y.range[1], yend = tempo.coord$y.range[1] + diff(tempo.coord$y.range) / 80))
        # }
        coord.names <- c(coord.names, "y.second.tick.positions")
    }else if(( ! is.null(y.second.tick.nb)) & y.log == "no"){
        # if(y.second.tick.nb > 0){ #inactivated because already checked before
        if(length(tempo.scale) < 2){
            tempo.cat1 <- c("y.tick.nb", "y.second.tick.nb")
            tempo.cat2 <- sapply(list(y.tick.nb, y.second.tick.nb), FUN = paste0, collapse = " ")
            tempo.sep <- sapply(mapply(" ", max(nchar(tempo.cat1)) - nchar(tempo.cat1) + 3, FUN = rep, SIMPLIFY = FALSE), FUN = paste0, collapse = "")
            tempo.cat <- paste0("ERROR IN ", function.name, "\nTHE NUMBER OF GENERATED TICKS FOR THE Y-AXIS IS NOT CORRECT: ", length(tempo.scale), "\nUSING THESE ARGUMENT SETTINGS (NO DISPLAY MEANS NULL VALUE):\n", paste0(tempo.cat1, tempo.sep, tempo.cat2, collapse = "\n"), "\nPLEASE, TEST OTHER VALUES")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else{
            tempo <- saferGraph::inter_ticks(lim = y.lim, log = y.log, breaks = tempo.scale, n = y.second.tick.nb)
        }
        y.second.tick.values <- tempo$values
        y.second.tick.pos <- tempo$coordinates
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::annotate(
            geom = "segment", 
            y = y.second.tick.pos, 
            yend = y.second.tick.pos, 
            x = if(vertical == TRUE){tempo.coord$x.range[1]}else{tempo.coord$y.range[1]}, 
            xend = if(vertical == TRUE){tempo.coord$x.range[1] + diff(tempo.coord$x.range) / 80}else{tempo.coord$y.range[1] + diff(tempo.coord$y.range) / 80}
        ))
        coord.names <- c(coord.names, "y.second.tick.positions")
    }
    # end y.second.tick.positions
    # for the ggplot2 bug with y.log, this does not work: eval(parse(text = ifelse(vertical == FALSE & y.log == "log10", "ggplot2::scale_x_continuous", "ggplot2::scale_y_continuous")))
    assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_y_continuous(
        breaks = tempo.scale, 
        minor_breaks = y.second.tick.pos, 
        labels = if(y.log == "log10"){scales::trans_format("identity", scales::math_format(10^.x))}else if(y.log == "log2"){scales::trans_format("identity", scales::math_format(2^.x))}else if(y.log == "no"){ggplot2::waiver()}else{tempo.cat <- paste0("INTERNAL CODE ERROR IN ", function.name, "\nCODE INCONSISTENCY 6") ; stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)}, # == in stop() to be able to add several messages between ==
        expand = c(0, 0), # remove space after after axis limits
        limits = sort(y.lim), # NA indicate that limits must correspond to data limits but ylim() already used
        oob = scales::rescale_none, 
        trans = ifelse(diff(y.lim) < 0, "reverse", "identity") # equivalent to ggplot2::scale_y_reverse() but create the problem of y-axis label disappearance with y.lim decreasing. Thus, do not use. Use ylim() below and after this
    ))
    if(vertical == TRUE){
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::coord_cartesian(ylim = y.lim)) # problem of ggplot2::ylim() is that it redraws new breaks # coord_cartesian(ylim = y.lim)) not used because bug -> y-axis label disappearance with y.lim decreasing I DO NOT UNDERSTAND THIS MESSAGE WHILE I USE COORD_CARTESIAN # clip = "off" to have secondary ticks outside plot region does not work
    }else{
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::coord_flip(ylim = y.lim)) # clip = "off" to have secondary ticks outside plot region does not work # create the problem of y-axis label disappearance with y.lim decreasing. IDEM ABOVE
        
    }
    # end y scale management (cannot be before dot plot management)
    
    
    # legend management
    if( ! is.null(legend.width)){
        legend.final <- fun_gg_get_legend(ggplot_built = bef.final.plot, fun.name = function.name, lib.path = lib.path) # get legend
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::guides(fill = "none", color = "none", alpha = "none")) # inactivate the initial legend
        if(is.null(legend.final) & plot == TRUE){ # even if any(unlist(legend.disp)) is TRUE
            legend.final <- ggplot2::ggplot()+ggplot2::theme_void() # empty graph instead of legend
            warn.count <- warn.count + 1
            tempo.warn <- paste0("(", warn.count,") LEGEND REQUESTED (NON NULL categ ARGUMENT OR legend.show ARGUMENT SET TO TRUE)\nBUT IT SEEMS THAT THE PLOT HAS NO LEGEND -> EMPTY LEGEND SPACE CREATED BECAUSE OF THE NON NULL legend.width ARGUMENT\n")
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        }
    }
    # end legend management
    
    
    # drawing
    fin.plot <- suppressMessages(suppressWarnings(eval(parse(text = paste(paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + ")))))
    grob.save <- NULL
    if(plot == TRUE){
        # following lines inactivated because of problem in warn.recov and message.recov
        # assign("env_fun_get_message", new.env())
        # assign("tempo.gg.name", tempo.gg.name, envir = env_fun_get_message)
        # assign("tempo.gg.count", tempo.gg.count, envir = env_fun_get_message)
        # assign("add", add, envir = env_fun_get_message)
        # two next line: for the moment, I cannot prevent the warning printing
        # warn.recov <- fun_get_message(paste(paste(paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + "), if(is.null(add)){NULL}else{add}), kind = "warning", header = FALSE, print.no = FALSE, env = env_fun_get_message) # for recovering warnings printed by ggplot() functions
        # message.recov <- fun_get_message('print(eval(parse(text = paste(paste(paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + "), if(is.null(add)){NULL}else{add}))))', kind = "message", header = FALSE, print.no = FALSE, env = env_fun_get_message) # for recovering messages printed by ggplot() functions
        # if( ! (return == TRUE & return.ggplot == TRUE)){ # because return() plots when return.ggplot is TRUE # finally not used -> see return.ggplot description
        if(is.null(legend.width)){
            grob.save <- suppressMessages(suppressWarnings(gridExtra::grid.arrange(fin.plot)))
        }else{
            grob.save <-suppressMessages(suppressWarnings(gridExtra::grid.arrange(fin.plot, legend.final, ncol=2, widths=c(1, legend.width))))
        }
        # }
        # suppressMessages(suppressWarnings(print(eval(parse(text = paste(paste(paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + "), if(is.null(add)){NULL}else{add}))))))
    }else{
        # following lines inactivated because of problem in warn.recov and message.recov
        # message.recov <- NULL
        # warn.recov <- NULL
        warn.count <- warn.count + 1
        tempo.warn <- paste0("(", warn.count,") PLOT NOT SHOWN AS REQUESTED")
        warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
    }
    # end drawing
    
    
    
    # output
    # following lines inactivated because of problem in warn.recov and message.recov
    # if( ! (is.null(warn) & is.null(warn.recov) & is.null(message.recov))){
    # warn <- paste0(warn, "\n\n", if(length(warn.recov) > 0 | length(message.recov) > 0){paste0(paste0("MESSAGES FROM ggplot2 FUNCTIONS: ", ifelse( ! is.null(warn.recov), unique(message.recov), ""), ifelse( ! is.null(message.recov), unique(message.recov), ""), collapse = "\n\n"), "\n\n")})
    # }else if( ! (is.null(warn) & is.null(warn.recov)) & is.null(message.recov)){
    # warn <- paste0(warn, "\n\n", if(length(warn.recov) > 0){paste0(paste0("MESSAGES FROM ggplot2 FUNCTIONS: ", unique(warn.recov), collapse = "\n\n"), "\n\n")})
    # }else if( ! (is.null(warn) & is.null(message.recov)) & is.null(warn.recov)){
    # warn <- paste0(warn, "\n\n", if(length(message.recov) > 0){paste0(paste0("MESSAGES FROM ggplot2 FUNCTIONS: ", unique(message.recov), collapse = "\n\n"), "\n\n")})
    # }
    if(warn.print == TRUE & ! is.null(warn)){
        on.exit(warning(paste0("FROM ", function.name, ":\n\n", warn), call. = FALSE))
    }
    on.exit(exp = options(warning.length = ini.warning.length), add = TRUE)
    if(return == TRUE){
        tempo.output <- ggplot2::ggplot_build(fin.plot)
        tempo.output$data <- tempo.output$data[-1] # remove the first data because corresponds to the initial empty boxplot
        if(length(tempo.output$data) != length(coord.names)){
            tempo.cat <- paste0("INTERNAL CODE ERROR IN ", function.name, "\nlength(tempo.output$data) AND length(coord.names) MUST BE IDENTICAL. CODE HAS TO BE MODIFIED")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else{
            names(tempo.output$data) <- coord.names
            tempo.output$data <- tempo.output$data[coord.names != "bad_remove"]
        }
        tempo <- tempo.output$layout$panel_params[[1]]
        output <- list(
            data = data1.ini, 
            stat = stat.nolog, 
            removed.row.nb = removed.row.nb, 
            removed.rows = removed.rows, 
            plot = c(tempo.output$data, y.second.tick.values = list(y.second.tick.values)), 
            panel = facet.categ, 
            axes = list(
                x.range = tempo$x.range, 
                x.labels = if(is.null(attributes(tempo$x$breaks))){tempo$x$breaks}else{tempo$x$scale$get_labels()}, # is.null(attributes(tempo$x$breaks)) test if it is number (TRUE) or character (FALSE)
                x.positions = if(is.null(attributes(tempo$x$breaks))){tempo$x$breaks}else{unlist(attributes(tempo$x$breaks))}, 
                y.range = tempo$y.range, 
                y.labels = if(is.null(attributes(tempo$y$breaks))){tempo$y$breaks}else{tempo$y$scale$get_labels()}, 
                y.positions = if(is.null(attributes(tempo$y$breaks))){tempo$y$breaks}else{unlist(attributes(tempo$y$breaks))}
            ), 
            warn = paste0("\n", warn, "\n\n"), 
            ggplot = if(return.ggplot == TRUE){fin.plot}else{NULL}, # fin.plot plots the graph if return == TRUE
            gtable = if(return.gtable == TRUE){grob.save}else{NULL} 
        )
        return(output) # this plots the graph if return.ggplot is TRUE and if no assignment
    }
    # end output
    # end main code
}





