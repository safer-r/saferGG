#' @title gg_scatter
#' @description
#' Plot ggplot2 scatterplot with the possibility to overlay dots from up to 3 different data frames (-> three different legends) and lines from up to 3 different data frames (-> three different legends) -> up to 6 overlays totally.
#' 
#' For ggplot2 specifications, see: https://ggplot2.tidyverse.org/articles/ggplot2-specs.html.
#' @param data1 a dataframe compatible with ggplot2, or a list of data frames. Order matters for the order of the legend and for the layer staking (starting from below to top)
#' @param x single character string of the data1 column name for x-axis coordinates. If data1 is a list, then x must be a list of single character strings, of same size as data1, with compartment 1 related to compartment 1 of data1, etc. Write NULL for each "geom_hline" in geom argument
#' @param y single character string of the data1 column name for y-axis coordinates. If data1 is a list, then y must be a list of single character strings, of same size as data1, with compartment 1 related to compartment 1 of data1, etc. Write NULL for each "geom_vline" in geom argument
#' @param categ either NULL or a single character string or a list of single character strings, indicating the data1 column names to use for categories which creates legend display
#' If categ == NULL, no categories -> no legend displayed
#' If data1 is a data frame, categ must be a single character string of the data1 column name for categories
#' If data1 is a list, then categ must be a list of single character strings, of same size as data1, with compartment 1 related to compartment 1 of data1, etc. Some of the list compartments can be NULL (no legend display for these compartments), and other not
#' @param categ_class_order either (1) NULL or (2) a vector of character strings or (3) a list of these vectors, setting the order of the classes of categ in the legend display
#' If categ_class_order is NULL, classes are represented according to the alphabetical order
#' If data1 is a data frame, categ_class_order must be a vector of character strings specifying the different classes in the categ column name of data1
#' If data1 is a list, then categ_class_order must be a list of vector of character strings, of same size as data1, with compartment 1 related to compartment 1 of data1, etc. Some of the list compartments can be NULL (alphabetical order for these compartments), and other not
#' @param color either (1) NULL, or (2) a vector of character strings or integers, or (3) a list of vectors of character strings or integers
#' If color is NULL, default colors of ggplot2
#' If data1 is a data frame, color argument can be either:
#' (1) a single color string. All the dots of the corresponding data1 will have this color, whatever the categ value (NULL or not)
#' (2) if categ is non-null, a vector of string colors, one for each class of categ. Each color will be associated according to the categ_class_order argument if specified, or to the alphabetical order of categ classes otherwise
#' (3) if categ is non-null, a vector or factor of string colors, like if it was one of the column of data1 data frame. WARNING: a single color per class of categ and a single class of categ per color must be respected
#' Positive integers are also accepted instead of character strings, as long as above rules about length are respected. Integers will be processed by gg_palette() using the max integer value among all the integers in color (see gg_palette())
#' If data1 is a list, then color argument must be either: 
#' (1) a list of character strings or integers, of same size as data1, with compartment 1 related to compartment 1 of data1, etc.
#' (2) a single character string or a single integer
#' With a list (first possibility), the rules described for when data1 is a data frame apply to each compartment of the list. Some of the compartments can be NULL. In that case, a different grey color will be used for each NULL compartment. With a single value (second possibility), the same color will be used for all the dots and lines, whatever the data1 list
#' @param geom single character string of the kind of plot, or a list of single character strings
#' Either:
#' "geom_point" (scatterplot)
#' "geom_line" (coordinates plotted then line connection, from the lowest to highest x coordinates first and from the lowest to highest y coordinates thenafter)
#' "geom_path" (coordinates plotted then line connection respecting the row order in data1)
#' "geom_step" coordinates plotted then line connection respecting the row order in data1 but drawn in steps). See the geom_step_dir argument
#' "geom_hline" (horizontal line, no x value provided)
#' "geom_vline" (vertical line, no y value provided)
#' "geom_stick" (dots as vertical bars)
#' If data1 is a list, then geom must be either:
#' (1) a list of single character strings, of same size as data1, with compartment 1 related to compartment 1 of data1, etc.
#' (2) a single character string. In that case the same kind of plot will apply for the different compartments of the data1 list
#' WARNING concerning "geom_hline" or "geom_vline":
#' (1) x or y argument must be NULL, respectively
#' (2) x_lim or y_lim argument must NOT be NULL, respectively, if only these kind of lines are drawn (if other geom present, then x_lim = NULL and y_lim = NULL will generate x_lim and y_lim defined by these other geom, which is not possible with "geom_hline" or "geom_vline" alone)
#' (3) the function will draw n lines for n values in the x argument column name of the data1 data frame. If several colors required, the categ argument must be specified and the corresponding categ column name must exist in the data1 data frame with a different class name for each row
#' @param geom_step_dir single character string indicating the direction when using "geom_step" of the geom argument, or a list of single character strings
#' Either:
#' "vh" (vertical then horizontal)
#' "hv" (horizontal then vertical)
#' "mid" (step half-way between adjacent x-values)
#' See https://ggplot2.tidyverse.org/reference/geom_path.html
#' If data1 is a list, then geom_step_dir must be either:
#' (1) a list of single character strings, of same size as data1, with compartment 1 related to compartment 1 of data1, etc. The value in compartments related to other geom values than "geom_step" will be ignored
#' (2) a single character string, which will be used for all the "geom_step" values of the geom argument, whatever the data1 list
#' @param geom_stick_base either (1) NULL or (2) a single numeric value or (3) a list of single numeric values, setting the base of the sticks when using "geom_stick" of the geom argument
#' If geom_stick_base is NULL, the bottom limit of the y-axis is taken as the base
#' If data1 is a list, then geom_stick_base must be either (1) a list of single numeric values, of same size as data1, with compartment 1 related to compartment 1 of data1, etc., or (2) a single numeric value. With a list (former possibility), the values in compartments related to other geom values than "geom_stick" will be ignored. With a single value (latter possibility), the same base will be used for all the sticks, whatever the data1 list
#' Warning: the y-axis limits are not modified by the value of geom_stick_base, meaning that this value can be outside of the range of y_lim. Add the value of geom_stick_base also in the y_lim argument if required
#' Warning: if geom_stick_base is NULL, the bottom limit of the y-axis is taken as the base. Thus, be careful with inverted y-axis
#' @param alpha single numeric value (from 0 to 1) of transparency. If data1 is a list, then alpha must be either (1) a list of single numeric values, of same size as data1, with compartment 1 related to compartment 1 of data1, etc., or (2) a single numeric value. In that case the same transparency will apply for the different compartments of the data1 list
#' @param dot_size single numeric value of dot shape radius? in mm. If data1 is a list, then dot_size must be either (1) a list of single numeric values, of same size as data1, with compartment 1 related to compartment 1 of data1, etc., or (2) a single numeric value. With a list (former possibility), the value in compartments related to lines will be ignored. With a single value (latter possibility), the same dot_size will be used for all the dots, whatever the data1 list
#' @param dot_shape value indicating the shape of the dots (see https://ggplot2.tidyverse.org/articles/ggplot2-specs.html) If data1 is a list, then dot_shape must be either (1) a list of single shape values, of same size as data1, with compartment 1 related to compartment 1 of data1, etc., or (2) a single shape value. With a list (former possibility), the value in compartments related to lines will be ignored. With a single value (latter possibility), the same dot_shape will be used for all the dots, whatever the data1 list
#' @param dot_border_size single numeric value of border dot width in mm. Write zero for no dot border. If data1 is a list, then dot_border_size must be either (1) a list of single numeric values, of same size as data1, with compartment 1 related to compartment 1 of data1, etc., or (2) a single numeric value. With a list (former possibility), the value in compartments related to lines will be ignored. With a single value (latter possibility), the same dot_border_size will be used for all the dots, whatever the data1 list
#' @param dot_border_color single character color string defining the color of the dot border (same border color for all the dots, whatever their categories). If dot_border_color == NULL, the border color will be the same as the dot color. A single integer is also accepted instead of a character string, that will be processed by gg_palette()
#' @param line_size single numeric value of line width in mm. If data1 is a list, then line_size must be either (1) a list of single numeric values, of same size as data1, with compartment 1 related to compartment 1 of data1, etc., or (2) a single numeric value. With a list (former possibility), the value in compartments related to dots will be ignored. With a single value (latter possibility), the same line_size will be used for all the lines, whatever the data1 list
#' @param line_type value indicating the kind of lines (see https://ggplot2.tidyverse.org/articles/ggplot2-specs.html) If data1 is a list, then line_type must be either (1) a list of single line kind values, of same size as data1, with compartment 1 related to compartment 1 of data1, etc., or (2) a single line kind value. With a list (former possibility), the value in compartments related to dots will be ignored. With a single value (latter possibility), the same line_type will be used for all the lines, whatever the data1 list
#' @param x_lim 2 numeric values setting the x-axis range. Order of the 2 values matters (for inverted axis). If NULL, the range of the x column name of data1 will be used
#' @param x_lab a character string or expression for x-axis label. If NULL, will use the first value of x (x column name of the first data frame in data1). Warning message if the elements in x are different between data frames in data1
#' @param x_log either "no", "log2" (values in the x column name of the data1 data frame will be log2 transformed and x-axis will be log2 scaled) or "log10" (values in the x column name of the data1 data frame will be log10 transformed and x-axis will be log10 scaled)
#' @param x_tick_nb approximate number of desired values labeling the x-axis (i.e., main ticks, see the n argument of the the saferGraph::scale2() function). If NULL and if x_log is "no", then the number of labeling values is set by ggplot2. If NULL and if x_log is "log2" or "log10", then the number of labeling values corresponds to all the exposant integers in the x_lim range (e.g., 10^1, 10^2 and 10^3, meaning 3 main ticks for x_lim = c(9, 1200)). WARNING: if non-NULL and if x_log is "log2" or "log10", labeling can be difficult to read (e.g., ..., 10^2, 10^2.5, 10^3, ...)
#' @param x_second_tick_nb number of desired secondary ticks between main ticks. Ignored if x_log is other than "no" (log scale plotted). Use argument return = TRUE and see $plot$x_second_tick_values to have the values associated to secondary ticks. IF NULL, no secondary ticks
#' @param x_include_zero logical. Does x_lim range include 0? Ignored if x_log is "log2" or "log10"
#' @param x_left_extra_margin single proportion (between 0 and 1) indicating if extra margins must be added to x_lim. If different from 0, add the range of the axis multiplied by x_left_extra_margin (e.g., base::abs(x_lim[2] - x_lim[1]) * x_left_extra_margin) to the left of x-axis
#' @param x_right_extra_margin idem as x_left_extra_margin but to the right of x-axis
#' @param x_text_angle integer value of the text angle for the x-axis labeling values, using the same rules as in ggplot2. Use positive value for clockwise rotation: 0 for horizontal, 90 for vertical, 180 for upside down etc. Use negative values for counterclockwise rotation: 0 for horizontal, -90 for vertical, -180 for upside down etc.
#' @param y_lim 2 numeric values setting the y-axis range. Order of the 2 values matters (for inverted axis). If NULL, the range of the y column name of data1 will be used
#' @param y_lab a character string or expression for y-axis label. If NULL, will use the first value of y (y column name of the first data frame in data1). Warning message if the elements in y are different between data frames in data1
#' @param y_log either "no", "log2" (values in the y column name of the data1 data frame will be log2 transformed and y-axis will be log2 scaled) or "log10" (values in the y column name of the data1 data frame will be log10 transformed and y-axis will be log10 scaled)
#' @param y_tick_nb approximate number of desired values labeling the y-axis (i.e., main ticks, see the n argument of the the saferGraph::scale2() function). If NULL and if y_log is "no", then the number of labeling values is set by ggplot2. If NULL and if y_log is "log2" or "log10", then the number of labeling values corresponds to all the exposant integers in the y_lim range (e.g., 10^1, 10^2 and 10^3, meaning 3 main ticks for y_lim = c(9, 1200)). WARNING: if non-NULL and if y_log is "log2" or "log10", labeling can be difficult to read (e.g., ..., 10^2, 10^2.5, 10^3, ...)
#' @param y_second_tick_nb number of desired secondary ticks between main ticks. Ignored if y_log is other than "no" (log scale plotted). Use argument return = TRUE and see $plot$y_second_tick_values to have the values associated to secondary ticks. IF NULL, no secondary ticks
#' @param y_include_zero logical. Does y_lim range include 0? Ignored if y_log is "log2" or "log10"
#' @param y_top_extra_margin single proportion (between 0 and 1) indicating if extra margins must be added to y_lim. If different from 0, add the range of the axis multiplied by y_top_extra_margin (e.g., base::abs(y_lim[2] - y_lim[1]) * y_top_extra_margin) to the top of y-axis
#' @param y_bottom_extra_margin idem as y_top_extra_margin but to the bottom of y-axis
#' @param y_text_angle integer value of the text angle for the y-axis labeling values, using the same rules as in ggplot2. Use positive value for clockwise rotation: 0 for horizontal, 90 for vertical, 180 for upside down etc. Use negative values for counterclockwise rotation: 0 for horizontal, -90 for vertical, -180 for upside down etc.
#' @param raster logical. Dots in raster mode? If FALSE, dots from each "geom_point" from geom argument are plotted in vectorial mode (bigger pdf and long to display if lots of dots). If TRUE, dots from each "geom_point" from geom argument are plotted in matricial mode (smaller pdf and easy display if lots of dots, but it takes time to generate the layer). If TRUE, the raster_ratio argument is used to avoid an ellipsoid representation of the dots. If TRUE, solve the transparency problem with some GUI. Overriden by the non-NULL raster_threshold argument
#' @param raster_ratio single numeric value indicating the height / width ratio of the graphic device used (for instance provided by the $dim compartment in the output of the fun_open() function). The default value is 1 because by default R opens a square graphic device. But this argument has to be set when using other device dimensions. Ignored if raster == FALSE
#' @param raster_threshold positive integer value indicating the limit of the dot number above which "geom_point" layers from the geom argument switch from vectorial mode to matricial mode (see the raster argument). If any layer is matricial, then the raster_ratio argument is used to avoid an ellipsoid representation of the dots. If non-NULL, it overrides the raster argument
#' @param text_size numeric value of the font size of the (1) axis numbers and axis legends and (2) texts in the graphic legend (in mm)
#' @param title character string of the graph title
#' @param title_text_size numeric value of the title font size in mm
#' @param legend_show logical. Show legend? Not considered if categ argument is NULL, because this already generate no legend, excepted if legend_width argument is non-NULL. In that specific case (categ is NULL, legend_show is TRUE and legend_width is non-NULL), an empty legend space is created. This can be useful when desiring graphs of exactly the same width, whatever they have legends or not
#' @param legend_width single proportion (between 0 and 1) indicating the relative width of the legend sector (on the right of the plot) relative to the width of the plot. Value 1 means that the window device width is split in 2, half for the plot and half for the legend. Value 0 means no room for the legend, which will overlay the plot region. Write NULL to inactivate the legend sector. In such case, ggplot2 will manage the room required for the legend display, meaning that the width of the plotting region can vary between graphs, depending on the text in the legend
#' @param legend_name character string of the legend title. If legend_name is NULL and categ argument is not NULL, then legend_name <- categ. If data1 is a list, then legend_name must be a list of character strings, of same size as data1, with compartment 1 related to compartment 1 of data1, etc. Some of the list compartments can be NULL, and other not
#' @param article logical. If TRUE, use an article theme (article like). If FALSE, use a classic related ggplot theme. Use the add argument (e.g., add = "+ggplot2::theme_classic()" for the exact classic ggplot theme
#' @param grid logical. Draw lines in the background to better read the box values? Not considered if article == FALSE (grid systematically present)
#' @param add character string allowing to add more ggplot2 features (dots, lines, themes, facet, etc.). Ignored if NULL. Layers of add (e.g., "+ ggplot2::geom_hline(yintercept = 0)", "+ ggplot2::annotate(geom = 'text', x = 1, y = 0, label = 'a')") are drawn above the other layers, and their data are not returned in the $plot compartment of the output
#' WARNING: (1) the string must start with "+", (2) the string must finish with ")" and (3) each function must be preceded by "ggplot2::". Example: "+ ggplot2::coord_flip() + ggplot2::theme_bw()"
#' If the character string contains the "ggplot2::theme" string, then the article argument of gg_scatter() (see above) is ignored with a warning. In addition, some arguments can be overwritten, like x_angle (check all the arguments)
#' Handle the add argument with caution since added functions can create conflicts with the preexisting internal ggplot2 functions
#' WARNING: the call of objects inside the quotes of add can lead to an error if the name of these objects are some of the gg_scatter() arguments. Indeed, the function will use the internal argument instead of the global environment object. Example article <- "a" in the working environment and add = '+ ggplot2::ggtitle(article)'. The risk here is to have TRUE as title. To solve this, use add = '+ ggplot2::ggtitle(get("article", envir = .GlobalEnv))'
#' @param return logical. Return the graph parameters?
#' @param return_ggplot logical. Return the ggplot object in the output list? Ignored if return argument is FALSE. WARNING: always assign the gg_scatter() function (e.g., a <- gg_scatter()) if return_ggplot argument is TRUE, otherwise, double plotting is performed. See $ggplot in the RETURN section below for more details
#' @param return_gtable logical. Return the ggplot object as gtable of grobs in the output list? Ignored if plot argument is FALSE. Indeed, the graph must be plotted to get the grobs dispositions. See $gtable in the RETURN section below for more details
#' @param plot logical. Plot the graphic? If FALSE and return argument is TRUE, graphical parameters and associated warnings are provided without plotting
#' @param warn_print logical. Print warnings at the end of the execution? ? If FALSE, warning messages are never printed, but can still be recovered in the returned list. Some of the warning messages (those delivered by the internal ggplot2 functions) are not apparent when using the argument plot = FALSE
#' @param safer_check Single logical value. Perform the "safer" checks? If \code{TRUE}, checkings are performed before main code running (see the \href{https://github.com/safer-r}{safer-r project}): 1) required functions and related packages effectively present in local R libraries and 2) R classical operators (like \code{"<-"}) not overwritten by another package because of the R scope. Warning: must be set to \code{FALSE} if this function is used inside another "safer" function to avoid pointless multiple checkings.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R packages are not installed in the default directories because of lack of admin rights. More precisely, \code{lib_path} is passed through the \code{new} argument of \code{.libPaths()} so that the new library paths are \code{unique(c(new, .Library.site, .Library))}. Warning: \code{.libPaths()} is restored to the initial paths, after function execution. Ignored if \code{NULL} (default): only the pathways specified by the current \code{.libPaths()} are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>.". If NULL, converted into "".
#' @returns  An error message if at least one of the checked packages is missing in lib_path, or if at least one of the checked functions is missing in the required package, nothing otherwise.
#' @returns
#' a scatter plot if plot argument is TRUE
#' a list of the graph info if return argument is TRUE:
#' $data: the initial data with graphic information added. WARNING: if the x_log or y_log argument is not "no", x or y argument column of the data1 data frame are log2 or log10 converted in $data, respectively. Use 2^values or 10^$values to recover the initial values
#' $removed_row_nb: a list of the removed rows numbers in data frames (because of NA). NULL if no row removed
#' $removed_rows: a list of the removed rows in data frames (because of NA). NULL if no row removed
#' $plot: the graphic box and dot coordinates
#' $dots: dot coordinates
#' y_second_tick_positions: coordinates of secondary ticks (only if y_second_tick_nb argument is non-null or if y_log argument is different from "no")
#' y_second_tick_values: values of secondary ticks. NULL except if y_second_tick_nb argument is non-null or if y_log argument is different from "no")
#' $panel: the variable names used for the panels (NULL if no panels). WARNING: NA can be present according to ggplot2 upgrade to v3.3.0
#' $axes: the x-axis and y-axis info
#' $warn: the warning messages. Use cat() for proper display. NULL if no warning. WARNING: warning messages delivered by the internal ggplot2 functions are not apparent when using the argument plot = FALSE
#' $ggplot: ggplot object that can be used for reprint (use print($ggplot) or update (use $ggplot + ggplot2::...). NULL if return_ggplot argument is FALSE. Of note, a non-null $ggplot in the output list is sometimes annoying as the manipulation of this list prints the plot
#' $gtable: gtable object that can be used for reprint (use gridExtra::grid.arrange(...$ggplot) or with additionnal grobs (see the grob decomposition in the examples). NULL if return_ggplot argument is FALSE. Contrary to $ggplot, a non-NULL $gtable in the output list is not annoying as the manipulation of this list does not print the plot
#' @details 
#' WARNINGS
#' 
#' Rows containing NA in data1[, c(x, y, categ)] will be removed before processing, with a warning (see below).
#' 
#' Size arguments (dot_size, dot_border_size, line_size, text_size and title_text_size) are in mm. See Hadley comment in https://stackoverflow.com/questions/17311917/ggplot2-the-unit-of-size. See also http://sape.inf.usi.ch/quick-reference/ggplot2/size). Unit object are not accepted, but conversion can be used (e.g., grid::convertUnit(grid::unit(0.2, "inches"), "mm", valueOnly = TRUE)).
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @examples
#' set.seed(1)
#' obs1 <- base::data.frame(
#'     Km = c(2, 1, 6, 5, 4, 7),
#'     Time = c(2, 1, 6, 5, 4, 7)^2,
#'     Car = c("TUUT", "TUUT", "TUUT", "WIIM", "WIIM", "WIIM"),
#'     Color1 = base::rep(c("coral", "lightblue"), each = 3),
#'     stringsAsFactors = TRUE
#' )
#' gg_scatter(data1 = obs1, x = "Km", y = "Time")
#' @importFrom Cairo Cairo
#' @importFrom ggplot2 .data aes annotate coord_cartesian coord_flip element_line element_rect element_text facet_grid facet_wrap geom_point ggplot ggplot_build ggtitle guide_legend guides scale_discrete_manual scale_fill_manual scale_shape_manual scale_x_continuous scale_x_reverse scale_y_continuous scale_y_reverse theme theme_bw theme_classic theme_void waiver xlab ylab
#' @importFrom grDevices dev.list dev.cur
#' @importFrom graphics par
#' @importFrom grid convertUnit unit
#' @importFrom gridExtra grid.arrange
#' @importFrom scales math_format rescale_none trans_format
#' @importFrom saferGraph scale2
#' @export
gg_scatter <- function(
    data1, 
    x, 
    y, 
    categ = NULL, 
    categ_class_order = NULL, 
    color = NULL, 
    geom = "geom_point", 
    geom_step_dir = "hv", 
    geom_stick_base = NULL, 
    alpha = 0.5, 
    dot_size = 2, 
    dot_shape = 21, 
    dot_border_size = 0.5, 
    dot_border_color = NULL, 
    line_size = 0.5, 
    line_type = "solid", 
    x_lim = NULL, 
    x_lab = NULL, 
    x_log = "no", 
    x_tick_nb = NULL, 
    x_second_tick_nb = NULL, 
    x_include_zero = FALSE, 
    x_left_extra_margin = 0.05, 
    x_right_extra_margin = 0.05, 
    x_text_angle = 0, 
    y_lim = NULL, 
    y_lab = NULL, 
    y_log = "no", 
    y_tick_nb = NULL, 
    y_second_tick_nb = NULL, 
    y_include_zero = FALSE, 
    y_top_extra_margin = 0.05, 
    y_bottom_extra_margin = 0.05, 
    y_text_angle = 0, 
    raster = FALSE, 
    raster_ratio = 1, 
    raster_threshold = NULL, 
    text_size = 12, 
    title = "", 
    title_text_size = 12, 
    legend_show = TRUE, 
    legend_width = 0.5, 
    legend_name = NULL, 
    article = TRUE, 
    grid = FALSE, 
    add = NULL, 
    return = FALSE, 
    return_ggplot = FALSE,
    return_gtable = TRUE,
    plot = TRUE, 
    warn_print = FALSE, 
    safer_check = TRUE, 
    lib_path = NULL, 
    error_text = ""
){
    # DEBUGGING
    # set.seed(1) ; obs1 <- base::data.frame(km = rnorm(1000, 10, 3), time = rnorm(1000, 10, 3), group1 = base::rep(c("A1", "A2"), 500), stringsAsFactors = TRUE) ; obs2 <-base::data.frame(km = rnorm(1000, 15, 3), time = rnorm(1000, 15, 3), group2 = base::rep(c("G1", "G2"), 500), stringsAsFactors = TRUE) ; set.seed(NULL) ; obs1$km[2:3] <- NA ; data1 = list(L1 = obs1, L2 = obs2) ; x = list(L1 = "km", L2 = "km") ; y = list(L1 = "time", L2 = "time") ; categ = list(L1 = "group1", L2 = "group2") ; categ = NULL ; categ_class_order = NULL ; color = NULL ; geom = "geom_point" ; geom_step_dir = "hv" ; geom_stick_base = NULL ; alpha = 0.5 ; dot_size = 2 ; dot_shape = 21 ; dot_border_size = 0.5 ; dot_border_color = NULL ; line_size = 0.5 ; line_type = "solid" ; x_lim = NULL ; x_lab = NULL ; x_log = "no" ; x_tick_nb = NULL ; x_second_tick_nb = NULL ; x_include_zero = FALSE ; x_left_extra_margin = 0.05 ; x_right_extra_margin = 0.05 ; x_text_angle = 0 ; y_lim = NULL ; y_lab = NULL ; y_log = "no" ; y_tick_nb = NULL ; y_second_tick_nb = NULL ; y_include_zero = FALSE ; y_top_extra_margin = 0.05 ; y_bottom_extra_margin = 0.05 ; y_text_angle = 0 ; raster = FALSE ; raster_ratio = 1 ; raster_threshold = NULL ; text_size = 12 ; title = "" ; title_text_size = 12 ; legend_show = TRUE ; legend_width = 0.5 ; legend_name = NULL ; article = TRUE ; grid = FALSE ; add = NULL ; return = FALSE ; return_ggplot = FALSE ; return_gtable = TRUE ; plot = TRUE ; warn_print = FALSE ; lib.path = NULL
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
    arg_user_setting <- tempo_settings[-1] # list of the argument settings (excluding default values not provided by the user). Always a list, even if 1 argument. So ok for base::lapply() usage (management of NA section)
    arg_user_setting_names <- base::names(x = arg_user_setting)
    # evaluation of values if they are espression, call, etc.
    if(base::length(x = arg_user_setting) != 0){
        arg_user_setting_eval <- base::lapply(
            X = arg_user_setting_names, 
            FUN = function(x){
                base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = TRUE) # n = 2 because of base::lapply(), inherit = TRUE to be sure to correctly evaluate
            }
        )
        base::names(x = arg_user_setting_eval) <- arg_user_setting_names
    }
    # end evaluation of values if they are espression, call, etc.
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
    no_def_args <- base::c(
        "data1",
        "x",
        "y"
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
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    ######## end arg with no default values

    ######## management of NULL arguments
    # before NA checking because base::is.na(NULL) return logical(0) and base::all(logical(0)) is TRUE (but secured with & base::length(x = x) > 0)
    tempo_arg <-base::c(
        "data1", 
        # "x", # inactivated because can be NULL when the geom argument of the same compartment is "geom_hline" (dedicated check in the main code)
        # "y", # inactivated because can be NULL when the geom argument of the same compartment is "geom_vline" (dedicated check in the main code)
        # "categ", # inactivated because can be NULL
        # "categ_class_order", # inactivated because can be NULL
        # "color", # inactivated because can be NULL
        "geom", 
        "geom_step_dir", 
        # "geom_stick_base", # inactivated because can be NULL
        "alpha", 
        "dot_size", 
        "dot_shape", 
        "dot_border_size", 
        # "dot_border_color", # inactivated because can be NULL
        "line_size", 
        "line_type", 
        # "x_lim", # inactivated because can be NULL
        # "x_lab", # inactivated because can be NULL
        "x_log", 
        # "x_tick_nb", # inactivated because can be NULL
        # "x_second_tick_nb", # inactivated because can be NULL
        "x_include_zero", 
        "x_left_extra_margin", 
        "x_right_extra_margin", 
        "x_text_angle", 
        # "y_lim", # inactivated because can be NULL
        # "y_lab", # inactivated because can be NULL
        "y_log", 
        # "y_tick_nb", # inactivated because can be NULL
        # "y_second_tick_nb", # inactivated because can be NULL
        "y_include_zero", 
        "y_top_extra_margin", 
        "y_bottom_extra_margin", 
        "y_text_angle", 
        "raster", 
        "raster_ratio", 
        # "raster_threshold", # inactivated because can be NULL
        "text_size", 
        "title", 
        "title_text_size", 
        "legend_show", 
        "legend_width", 
        # "legend_name", # inactivated because can be NULL
        "article", 
        "grid", 
        # "add", # inactivated because can be NULL
        "return", 
        "return_ggplot", 
        "return_gtable", 
        "plot", 
        "warn_print", 
        "safer_check" 
        # "lib_path", # inactivated because can be NULL
        # "error_text" # inactivated because NULL converted to "" above
    )
    tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::is.null(x = x)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because base::sapply(base::lapply())
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
    # # before NA checking because base::is.na(logical()) is logical(0) (but secured with & base::length(x = x) > 0)
    tempo_arg <-base::c(
        "data1", 
        "x", 
        "y", 
        "categ", 
        "categ_class_order", 
        "color", 
        "geom", 
        "geom_step_dir", 
        "geom_stick_base", 
        "alpha", 
        "dot_size", 
        "dot_shape", 
        "dot_border_size", 
        "dot_border_color", 
        "line_size", 
        "line_type", 
        "x_lim", 
        "x_lab", 
        "x_log", 
        "x_tick_nb", 
        "x_second_tick_nb", 
        "x_include_zero", 
        "x_left_extra_margin", 
        "x_right_extra_margin", 
        "x_text_angle", 
        "y_lim", 
        "y_lab", 
        "y_log", 
        "y_tick_nb", 
        "y_second_tick_nb", 
        "y_include_zero", 
        "y_top_extra_margin", 
        "y_bottom_extra_margin", 
        "y_text_angle", 
        "raster", 
        "raster_ratio", 
        "raster_threshold", 
        "text_size", 
        "title", 
        "title_text_size", 
        "legend_show", 
        "legend_width", 
        "legend_name", 
        "article", 
        "grid", 
        "add", 
        "return", 
        "return_ggplot", 
        "return_gtable", 
        "plot", 
        "warn_print", 
        "safer_check", 
        "lib_path"
        # "error_text" # inactivated because empty value converted to "" above
    )
    tempo_arg_user_setting_eval <- arg_user_setting_eval[base::names(arg_user_setting_eval) %in% tempo_arg]
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
        ) # no argument provided by the user can be empty non NULL object. Warning: would not work if arg_user_setting_eval is a vector (because treat each element as a compartment), but ok because it is always a list, even is 0 or 1 argument in the developed function
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
                " CANNOT BE AN EMPTY NON NULL OBJECT:\n", 
                base::paste0(tempo_arg_user_setting_eval[tempo_log], collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        }
    }
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
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
        ) # no argument provided by the user can be just made of NA. base::is.na(NULL) returns logical(0), the reason why base::length(x = x) > 0 is used # warning: base::all(x, na.rm = TRUE) but normally no NA because base::is.na() used here. Warning: would not work if arg_user_setting_eval is a vector (because treat each element as a compartment), but ok because it is always a list, even is 0 or 1 argument in the developed function
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
                " CANNOT BE MADE OF NA ONLY:\n", 
                base::paste0(arg_user_setting_names[tempo_log], collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
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
            base::ifelse(test = base::length(x = safer_check) == 0 | base::all(base::suppressWarnings(expr = safer_check == base::quote(expr = ), classes = "warning"), na.rm = TRUE) | base::all(base::suppressWarnings(expr = safer_check == "", classes = "warning"), na.rm = TRUE), yes = "<NULL, \"\", EMPTY OBJECT OR EMPTY NAME>", no = base::paste0(safer_check, collapse = "\n", recycle0 = FALSE)),
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
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
                "ggplot2::annotate",
                "scales::alpha",
                "ggplot2::coord_cartesian",
                "ggplot2::element_line",
                "ggplot2::element_rect",
                "ggplot2::element_text",
                "ggplot2::facet_grid",
                "ggplot2::facet_wrap",
                "ggplot2::geom_point",
                "ggplot2::ggplot",
                "ggplot2::ggplot_build",
                "ggplot2::ggtitle",
                "ggplot2::guide_legend",
                "ggplot2::guides",
                "ggplot2::scale_discrete_manual",
                "ggplot2::scale_fill_manual",
                "ggplot2::scale_shape_manual",
                "ggplot2::scale_x_continuous",
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
                "saferGraph::scale2", 
                "saferGraph::inter_ticks",
                "saferTool::round2",
                "saferGG::gg_palette",
                "saferGG::gg_just",
                "saferGG::gg_point_rast",
                "saferGG::gg_get_legend",
                "saferGG::gg_empty",
                "saferDev::arg_check",
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

    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    tempo_arg <- base::c(
        # "x", # inactivated because can be a list of character strings (each compartment checked in the dedicated x check below)
        # "y", # inactivated because can be a list of character strings (each compartment checked in the dedicated y check below)
        # "categ", # inactivated because can be a list of character strings (each compartment checked in the dedicated categ check below)
        # "categ_class_order", # inactivated because can be a list of character vectors (each compartment checked in the dedicated categ_class_order check below)
        # "color", # inactivated because can be a list or a vector of non character values (checked in the dedicated color check below)
        # "geom", # inactivated because can be a list of character strings (each compartment checked in the dedicated geom check below)
        # "geom_step_dir", # inactivated because can be a list of character strings (each compartment checked in the dedicated geom_step_dir check below)
        # "dot_border_color", # inactivated because can be a list or a vector of non character values (checked in the dedicated dot_border_color check below)
        "x_lab",
        "x_log",
        "y_lab",
        "y_log",
        # "title",  # inactivated because can be ""
        # "legend_name", # inactivated because can be a list of character strings (each compartment checked in the dedicated legend_name check below)
        "add"
        # "lib_path" # inactivated because already checked above
        # "error_text" # inactivated because can be ""
    )
    tempo_log <- ! base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){if(base::is.null(x = x)){base::return(TRUE)}else{base::all(base::mode(x = x) == "character", na.rm = TRUE)}}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because base::sapply(base::lapply())  #  need to test base::is.null() here
    if(base::any(tempo_log, na.rm = TRUE)){
            # nocov start (unreachable: developer-error guard - all tempo_arg entries here are character by construction)
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
        tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::any(x == "", na.rm = TRUE)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because base::sapply(base::lapply()).  # for character argument that can also be NULL, if NULL -> returns FALSE. Thus no need to test base::is.null()
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS\n", no = "THIS ARGUMENT\n"), 
                base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE),
                "\nCANNOT CONTAIN EMPTY STRING \"\".", 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        }
    }
    ######## end management of "" in arguments of mode character

    #### end argument secondary checking

    #### second round of checking and data preparation

    ######## reserved words
    reserved_words <- base::c("fake_x", "fake_y", "fake_categ")
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
    # nocov start
    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
    dev_list <- grDevices::dev.list()
    base::on.exit(
        expr = if(base::length(x = grDevices::dev.list()) != base::length(x = dev_list)){
            tempo_cat <- base::paste0(
                "INTERNAL ERROR IN THE BACKBONE PART OF ",
                intern_error_text_start,
                "SOME GRAPHIC DEVICES WERE OPENED BUT NOT CLOSED.\nNUMBER OF DEVICES AT START:",
                base::length(x = dev_list),
                "\nNUMBER OF DEVICES AT END: ",
                base::length(x = grDevices::dev.list()),
                "\n",
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
    # check list lengths (and names of data1 compartments if present)
    list_color <- NULL
    list_geom <- NULL
    list_geom_step_dir <- NULL
    list_geom_stick_base <- NULL
    list_alpha <- NULL
    list_dot_size <- NULL
    list_dot_shape <- NULL
    list_dot_border_size <- NULL
    list_dot_border_color <- NULL
    list_line_size <- NULL
    list_line_type <- NULL
    if(base::all(base::class(x = data1) == "list", na.rm = FALSE)){
        if(base::length(x = data1) > 6){
            tempo_cat <- base::paste0(error_text_start, "data1 ARGUMENT MUST BE A LIST OF 6 DATA FRAMES MAXIMUM (6 OVERLAYS MAX).", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        if(base::is.null(x = base::names(x = data1))){
            base::names(x = data1) <- base::paste0("L", 1:base::length(x = data1), collapse = NULL, recycle0 = FALSE)
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") NULL NAME COMPARTMENT OF data1 LIST -> NAMES RESPECTIVELY ATTRIBUTED TO EACH COMPARTMENT:\n", base::paste(base::names(x = data1), collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
        }
        if( ! base::is.null(x = x)){
            if( ! (base::all(base::class(x = x) == "list", na.rm = FALSE) & base::length(x = data1) == base::length(x = x))){
                tempo_cat <- base::paste0(error_text_start, "x ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST.", collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
        }else{
            x <- base::vector(mode = "list", length = base::length(x = data1))
        }
        if( ! base::is.null(x = y)){
            if( ! (base::all(base::class(x = y) == "list", na.rm = FALSE) & base::length(x = data1) == base::length(x = y))){
                tempo_cat <- base::paste0(error_text_start, "y ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST", collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
        }else{
            y <- base::vector(mode = "list", length = base::length(x = data1))
        }
        if( ! base::is.null(x = categ)){
            if( ! (base::all(base::class(x = categ) == "list", na.rm = FALSE) & base::length(x = data1) == base::length(x = categ))){
                tempo_cat <- base::paste0(error_text_start, "categ ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST", collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
        }
        if( ! base::is.null(x = categ_class_order)){
            if( ! (base::all(base::class(x = categ_class_order) == "list", na.rm = FALSE) & base::length(x = data1) == base::length(x = categ_class_order))){
                tempo_cat <- base::paste0(error_text_start, "categ_class_order ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST", collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
        }
        if( ! base::is.null(x = color)){
            if( ! ((base::all(base::class(x = color) == "list", na.rm = FALSE) & base::length(x = data1) == base::length(x = color)) | ((base::all(base::mode(x = color) == "character", na.rm = FALSE) | base::all(base::mode(x = color) == "numeric", na.rm = FALSE)) & base::length(x = color)== 1L))){ # list of same length as data1 or single value
                tempo_cat <- base::paste0(error_text_start, "color ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST, OR A SINGLE CHARACTER STRING OR INTEGER", collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else if((base::all(base::mode(x = color) == "character", na.rm = FALSE) | base::all(base::mode(x = color) == "numeric", na.rm = FALSE)) & base::length(x = color)== 1L){ # convert the single value into a list of single value
                list_color <- base::vector(mode = "list", length = base::length(x = data1))
                list_color[] <- color
            }
        }
        if( ! ((base::all(base::class(x = geom) == "list", na.rm = FALSE) & base::length(x = data1) == base::length(x = geom)) | (base::all(base::mode(x = geom) == "character", na.rm = FALSE) & base::length(x = geom)== 1L))){ # list of same length as data1 or single value
            tempo_cat <- base::paste0(error_text_start, "geom ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST, OR A SINGLE CHARACTER VALUE", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(base::all(base::mode(x = geom) == "character", na.rm = FALSE) & base::length(x = geom)== 1L){ # convert the single value into a list of single value
            list_geom <- base::vector(mode = "list", length = base::length(x = data1))
            list_geom[] <- geom
        }
        if( ! ((base::all(base::class(x = geom_step_dir) == "list", na.rm = FALSE) & base::length(x = data1) == base::length(x = geom_step_dir)) | (base::all(base::mode(x = geom_step_dir) == "character", na.rm = FALSE) & base::length(x = geom_step_dir)== 1L))){ # list of same length as data1 or single value
            tempo_cat <- base::paste0(error_text_start, "geom_step_dir ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST, OR A SINGLE CHARACTER VALUE", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(base::all(base::mode(x = geom_step_dir) == "character", na.rm = FALSE) & base::length(x = geom_step_dir)== 1L){ # convert the single value into a list of single value
            list_geom_step_dir <- base::vector(mode = "list", length = base::length(x = data1))
            list_geom_step_dir[] <- geom_step_dir
        }
        if( ! base::is.null(x = geom_stick_base)){
            if( ! ((base::all(base::class(x = geom_stick_base) == "list", na.rm = FALSE) & base::length(x = data1) == base::length(x = geom_stick_base)) | (base::all(base::mode(x = geom_stick_base) == "numeric", na.rm = FALSE) & base::length(x = geom_stick_base)== 1L))){ # list of same length as data1 or single value
                tempo_cat <- base::paste0(error_text_start, "geom_stick_base ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST, OR A SINGLE NUMERIC VALUE", collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else if(base::all(base::mode(x = geom_stick_base) == "numeric", na.rm = FALSE) & base::length(x = geom_stick_base)== 1L){ # convert the single value into a list of single value
                list_geom_stick_base <- base::vector(mode = "list", length = base::length(x = data1))
                list_geom_stick_base[] <- geom_stick_base
            }
        }
        if( ! ((base::all(base::class(x = alpha) == "list", na.rm = FALSE) & base::length(x = data1) == base::length(x = alpha)) | (base::all(base::mode(x = alpha) == "numeric", na.rm = FALSE) & base::length(x = alpha)== 1L))){ # list of same length as data1 or single value
            tempo_cat <- base::paste0(error_text_start, "alpha ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST, OR A SINGLE NUMERIC VALUE", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(base::all(base::mode(x = alpha) == "numeric", na.rm = FALSE) & base::length(x = alpha)== 1L){ # convert the single value into a list of single value
            list_alpha <- base::vector(mode = "list", length = base::length(x = data1))
            list_alpha[] <- alpha
        }
        if( ! ((base::all(base::class(x = dot_size) == "list", na.rm = FALSE) & base::length(x = data1) == base::length(x = dot_size)) | (base::all(base::mode(x = dot_size) == "numeric", na.rm = FALSE) & base::length(x = dot_size)== 1L))){ # list of same length as data1 or single value
            tempo_cat <- base::paste0(error_text_start, "dot_size ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST, OR A SINGLE NUMERIC VALUE", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(base::all(base::mode(x = dot_size) == "numeric", na.rm = FALSE) & base::length(x = dot_size)== 1L){ # convert the single value into a list of single value
            list_dot_size <- base::vector(mode = "list", length = base::length(x = data1))
            list_dot_size[] <- dot_size
        }
        if( ! ((base::all(base::class(x = dot_shape) == "list", na.rm = FALSE) & base::length(x = data1) == base::length(x = dot_shape)) | (base::all(base::mode(x = dot_shape) != "list", na.rm = FALSE) & base::length(x = dot_shape)== 1L))){ # list of same length as data1 or single value
            tempo_cat <- base::paste0(error_text_start, "dot_shape ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST, OR A SINGLE SHAPE VALUE", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(base::all(base::mode(x = dot_shape) != "list", na.rm = FALSE) & base::length(x = dot_shape)== 1L){ # convert the single value into a list of single value
            list_dot_shape <- base::vector(mode = "list", length = base::length(x = data1))
            list_dot_shape[] <- dot_shape
        }
        if( ! ((base::all(base::class(x = dot_border_size) == "list", na.rm = FALSE) & base::length(x = data1) == base::length(x = dot_border_size)) | (base::all(base::mode(x = dot_border_size) == "numeric", na.rm = FALSE) & base::length(x = dot_border_size)== 1L))){ # list of same length as data1 or single value
            tempo_cat <- base::paste0(error_text_start, "dot_border_size ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST, OR A SINGLE NUMERIC VALUE", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(base::all(base::mode(x = dot_border_size) == "numeric", na.rm = FALSE) & base::length(x = dot_border_size)== 1L){ # convert the single value into a list of single value
            list_dot_border_size <- base::vector(mode = "list", length = base::length(x = data1))
            list_dot_border_size[] <- dot_border_size
        }
        if( ! base::is.null(x = dot_border_color)){
            if( ! ((base::all(base::class(x = dot_border_color) == "list", na.rm = FALSE) & base::length(x = data1) == base::length(x = dot_border_color)) | ((base::all(base::mode(x = dot_border_color) == "character", na.rm = FALSE) | base::all(base::mode(x = dot_border_color) == "numeric", na.rm = FALSE)) & base::length(x = dot_border_color)== 1L))){ # list of same length as data1 or single value
                tempo_cat <- base::paste0(error_text_start, "dot_border_color ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST, OR A SINGLE CHARACTER STRING OR INTEGER", collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else if((base::all(base::mode(x = dot_border_color) == "character", na.rm = FALSE) | base::all(base::mode(x = dot_border_color) == "numeric", na.rm = FALSE)) & base::length(x = dot_border_color)== 1L){ # convert the single value into a list of single value
                list_dot_border_color <- base::vector(mode = "list", length = base::length(x = data1))
                list_dot_border_color[] <- dot_border_color
            }
        }
        if( ! ((base::all(base::class(x = line_size) == "list", na.rm = FALSE) & base::length(x = data1) == base::length(x = line_size)) | (base::all(base::mode(x = line_size) == "numeric", na.rm = FALSE) & base::length(x = line_size)== 1L))){ # list of same length as data1 or single value
            tempo_cat <- base::paste0(error_text_start, "line_size ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST, OR A SINGLE NUMERIC VALUE", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(base::all(base::mode(x = line_size) == "numeric", na.rm = FALSE) & base::length(x = line_size)== 1L){ # convert the single value into a list of single value
            list_line_size <- base::vector(mode = "list", length = base::length(x = data1))
            list_line_size[] <- line_size
        }
        if( ! ((base::all(base::class(x = line_type) == "list", na.rm = FALSE) & base::length(x = data1) == base::length(x = line_type)) | (base::all(base::mode(x = line_type) != "list", na.rm = FALSE) & base::length(x = line_type)== 1L))){ # list of same length as data1 or single value
            tempo_cat <- base::paste0(error_text_start, "line_type ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST, OR A SINGLE LINE KIND VALUE", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(base::all(base::mode(x = line_type) != "list", na.rm = FALSE) & base::length(x = line_type)== 1L){ # convert the single value into a list of single value
            list_line_type <- base::vector(mode = "list", length = base::length(x = data1))
            list_line_type[] <- line_type
        }
        if( ! base::is.null(x = legend_name)){
            if( ! (base::all(base::class(x = legend_name) == "list", na.rm = FALSE) & base::length(x = data1) == base::length(x = legend_name))){
                tempo_cat <- base::paste0(error_text_start, "legend_name ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST", collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
        }
    }
    # end check list lengths (and names of data1 compartments if present)
    # conversion into lists
    if(base::all(base::is.data.frame(x = data1), na.rm = FALSE)){
        data1 <- base::list(L1 = data1)
        if(base::all(base::class(x = x) == "list", na.rm = FALSE)){
            tempo_cat <- base::paste0(error_text_start, "x ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            x <- base::list(L1 = x)
        }
        if(base::all(base::class(x = y) == "list", na.rm = FALSE)){
            tempo_cat <- base::paste0(error_text_start, "y ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            y <- base::list(L1 = y)
        }
        if( ! base::is.null(x = categ)){
            if(base::all(base::class(x = categ) == "list", na.rm = FALSE)){
                tempo_cat <- base::paste0(error_text_start, "categ ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME", collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                categ <- base::list(L1 = categ)
            }
        }
        if( ! base::is.null(x = categ_class_order)){
            if(base::all(base::class(x = categ_class_order) == "list", na.rm = FALSE)){
                tempo_cat <- base::paste0(error_text_start, "categ_class_order ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME", collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                categ_class_order <- base::list(L1 = categ_class_order)
            }
        }
        if( ! base::is.null(x = color)){
            if(base::all(base::class(x = color) == "list", na.rm = FALSE)){
                tempo_cat <- base::paste0(error_text_start, "color ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME", collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                color <- base::list(L1 = color)
            }
        }
        if(base::all(base::class(x = geom) == "list", na.rm = FALSE)){
            tempo_cat <- base::paste0(error_text_start, "geom ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            geom <- base::list(L1 = geom)
        }
        if(base::all(base::class(x = geom_step_dir) == "list", na.rm = FALSE)){
            tempo_cat <- base::paste0(error_text_start, "geom_step_dir ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            geom_step_dir <- base::list(L1 = geom_step_dir)
        }
        if( ! base::is.null(x = geom_stick_base)){
            if(base::all(base::class(x = geom_stick_base) == "list", na.rm = FALSE)){
                tempo_cat <- base::paste0(error_text_start, "geom_stick_base ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME", collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                geom_stick_base <- base::list(L1 = geom_stick_base)
            }
        }
        if(base::all(base::class(x = alpha) == "list", na.rm = FALSE)){
            tempo_cat <- base::paste0(error_text_start, "alpha ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            alpha <- base::list(L1 = alpha)
        }
        if(base::all(base::class(x = dot_size) == "list", na.rm = FALSE)){
            tempo_cat <- base::paste0(error_text_start, "dot_size ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            dot_size <- base::list(L1 = dot_size)
        }
        if(base::all(base::class(x = dot_shape) == "list", na.rm = FALSE)){
            tempo_cat <- base::paste0(error_text_start, "dot_shape ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            dot_shape <- base::list(L1 = dot_shape)
        }
        if(base::all(base::class(x = dot_border_size) == "list", na.rm = FALSE)){
            tempo_cat <- base::paste0(error_text_start, "dot_border_size ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            dot_border_size <- base::list(L1 = dot_border_size)
        }
        if( ! base::is.null(x = dot_border_color)){
            if(base::all(base::class(x = dot_border_color) == "list", na.rm = FALSE)){
                tempo_cat <- base::paste0(error_text_start, "dot_border_color ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME", collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                dot_border_color <- base::list(L1 = dot_border_color)
            }
        }
        if(base::all(base::class(x = line_size) == "list", na.rm = FALSE)){
            tempo_cat <- base::paste0(error_text_start, "line_size ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            line_size <- base::list(L1 = line_size)
        }
        if(base::all(base::class(x = line_type) == "list", na.rm = FALSE)){
            tempo_cat <- base::paste0(error_text_start, "line_type ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            line_type <- base::list(L1 = line_type)
        }
        if( ! base::is.null(x = legend_name)){
            if(base::all(base::class(x = legend_name) == "list", na.rm = FALSE)){
                tempo_cat <- base::paste0(error_text_start, "legend_name ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME", collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                legend_name <- base::list(L1 = legend_name)
            }
        }
    }else if( ! base::all(base::sapply(X = data1, FUN = base::class, simplify = TRUE, USE.NAMES = TRUE) == "data.frame", na.rm = FALSE)){ # if not a data frame, data1 can only be a list, as tested above
        tempo_cat <- base::paste0(error_text_start, "data1 ARGUMENT MUST BE A DATA FRAME OR A LIST OF DATA FRAMES", collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    # single value converted into list now reattributed to the argument name
    if( ! base::is.null(x = color)){
        if( ! base::is.null(x = list_color)){
            color <- list_color
        }
    }
    if( ! base::is.null(x = list_geom)){
        geom <- list_geom
    }
    if( ! base::is.null(x = list_geom_step_dir)){
        geom_step_dir <- list_geom_step_dir
    }
    if( ! base::is.null(x = geom_stick_base)){
        if( ! base::is.null(x = list_geom_stick_base)){
            geom_stick_base <- list_geom_stick_base
        }
    }
    if( ! base::is.null(x = list_alpha)){
        alpha <- list_alpha
    }
    if( ! base::is.null(x = list_dot_size)){
        dot_size <- list_dot_size
    }
    if( ! base::is.null(x = list_dot_shape)){
        dot_shape <- list_dot_shape
    }
    if( ! base::is.null(x = list_dot_border_size)){
        dot_border_size <- list_dot_border_size
    }
    if( ! base::is.null(x = dot_border_color)){
        if( ! base::is.null(x = list_dot_border_color)){
            dot_border_color <- list_dot_border_color
        }
    }
    if( ! base::is.null(x = list_line_size)){
        line_size <- list_line_size
    }
    if( ! base::is.null(x = list_line_type)){
        line_type <- list_line_type
    }
    # end single value converted into list now reattributed to the argument name
    # data, x, y, geom, alpha, dot_size, shape, dot_border_size, line_size, line_type, legend_name are list now
    # if non-null, categ, categ_class_order, legend_name, color, dot_border_color are list now
    # end conversion into lists
    # verif of add
    if( ! base::is.null(x = add)){
        if( ! base::grepl(pattern = "^\\s*\\+", x = add, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){ # check that the add string start by +
            tempo_cat <- base::paste0(error_text_start, "add ARGUMENT MUST START WITH \"+\": ", base::paste(base::unique(x = add, incomparables = FALSE), collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            
        }else if( ! base::grepl(pattern = "(ggplot2|lemon)\\s*::", x = add, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){ #
            tempo_cat <- base::paste0(error_text_start, "FOR EASIER FUNCTION DETECTION, add ARGUMENT MUST CONTAIN \"ggplot2::\" OR \"lemon::\" IN FRONT OF EACH GGPLOT2 FUNCTION: ", base::paste(base::unique(x = add, incomparables = FALSE), collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if( ! base::grepl(pattern = ")\\s*$", x = add, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){ # check that the add string finished by )
            tempo_cat <- base::paste0(error_text_start, "add ARGUMENT MUST FINISH BY \")\": ", base::paste(base::unique(x = add, incomparables = FALSE), collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    # end verif of add
    # management of add containing facet
    facet_categ <- NULL
    if( ! base::is.null(x = add)){
        facet_check <- TRUE
        tempo <- base::unlist(x = base::strsplit(x = add, split = "\\s*\\+\\s*(ggplot2|lemon)\\s*::\\s*", fixed = FALSE, perl = FALSE, useBytes = FALSE), recursive = TRUE, use.names = TRUE) #
        tempo <- base::sub(x = tempo, pattern = "^facet_wrap", replacement = "ggplot2::facet_wrap", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
        tempo <- base::sub(x = tempo, pattern = "^facet_grid", replacement = "ggplot2::facet_grid", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
        tempo <- base::sub(x = tempo, pattern = "^facet_rep", replacement = "lemon::facet_rep", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
        if(base::length(x = data1) > 1 & (base::any(base::grepl(x = tempo, pattern = "ggplot2::facet_wrap|lemon::facet_rep_wrap", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), na.rm = FALSE) | base::grepl(x = add, pattern = "ggplot2::facet_grid|lemon::facet_rep_grid", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE))){
            tempo_cat <- base::paste0(error_text_start, "facet PANELS CANNOT BE USED IF MORE THAN ONE DATA FRAME IN THE data1 ARGUMENT\nPLEASE REWRITE THE add STRING AND RERUN", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            if(base::any(base::grepl(x = tempo, pattern = "ggplot2::facet_wrap|lemon::facet_rep_wrap", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), na.rm = FALSE)){
                tempo1 <- base::suppressWarnings(expr = base::eval(expr = base::parse(text = tempo[base::grepl(x = tempo, pattern = "ggplot2::facet_wrap|lemon::facet_rep_wrap", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)], file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)), classes = "warning")
                facet_categ <- base::list(base::names(x = tempo1$params$facets)) # list of length 1
                tempo_text <- "facet_wrap OR facet_rep_wrap"
                facet_check <- FALSE
            }else if(base::grepl(x = add, pattern = "ggplot2::facet_grid|lemon::facet_rep_grid", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){
                tempo1 <- base::suppressWarnings(expr = base::eval(expr = base::parse(text = tempo[base::grepl(x = tempo, pattern = "ggplot2::facet_grid|lemon::facet_rep_grid", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)], file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)), classes = "warning")
                facet_categ <- base::list(base::c(base::names(x = tempo1$params$rows), base::names(x = tempo1$params$cols))) # list of length 1
                tempo_text <- "facet_grid OR facet_rep_grid"
                facet_check <- FALSE
            }
            if(facet_check == FALSE & ! base::all(facet_categ %in% base::names(x = data1[[1]]), na.rm = FALSE)){ # WARNING: base::all(facet_categ %in% base::names(data1)) is TRUE when facet_categ is NULL
                tempo_cat <- base::paste0(error_text_start, "DETECTION OF \"", tempo_text, "\" STRING IN THE add ARGUMENT BUT PROBLEM OF VARIABLE DETECTION (COLUMN NAMES OF data1)\nTHE DETECTED VARIABLES ARE:\n", base::paste(facet_categ, collapse = " ", sep = " ", recycle0 = FALSE), "\nTHE data1 COLUMN NAMES ARE:\n", base::paste(base::names(x = data1[[1]]), collapse = " ", sep = " ", recycle0 = FALSE), "\nPLEASE REWRITE THE add STRING AND RERUN", collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
        }
    }
    # if facet_categ is not NULL, it is a list of length 1 now
    # end management of add containing facet
    # legend name filling
    if(base::is.null(x = legend_name) & ! base::is.null(x = categ)){
        legend_name <- categ
    }else if(base::is.null(x = legend_name) & base::is.null(x = categ)){
        legend_name <- base::vector(mode = "list", length = base::length(x = data1)) # null list
    }
    # legend_name not NULL anymore (list)
    # end legend name filling
    # ini categ for legend display
    fin_lg_disp <- base::vector(mode = "list", length = 6) # will be used at the end to display or not legends
    fin_lg_disp[] <- FALSE
    legend_disp <- base::vector(mode = "list", length = base::length(x = data1))
    if(base::is.null(x = categ) | legend_show == FALSE){
        legend_disp[] <- FALSE
    }else{
        for(i2 in base::seq_along(along.with = data1)){
            if(base::is.null(x = categ[[i2]])){
                legend_disp[[i2]] <- FALSE
            }else{
                legend_disp[[i2]] <- TRUE
            }
        }
    }
    # end ini categ for legend display
    # integer colors into gg_palette
    tempo_check_color <- NULL
    for(i1 in base::seq_along(along.with = data1)){
        if(base::any(base::is.na(x = color[[i1]]), na.rm = FALSE)){
            tempo_cat <- base::paste0(error_text_start, "", base::ifelse(test = base::length(x = color)== 1L, yes = "color", no = base::paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT", collapse = NULL, recycle0 = FALSE)), ": color ARGUMENT CANNOT CONTAIN NA", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        tempo_check_color <- base::c(tempo_check_color, saferDev::arg_check(data = color[[i1]], data_name = base::ifelse(test = base::length(x = color)== 1L, yes = "color", no = base::paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT", collapse = NULL, recycle0 = FALSE)), class = NULL, typeof = "integer", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)$problem)
    }
    tempo_check_color <- ! tempo_check_color # invert TRUE and FALSE because if integer, then problem = FALSE
    if(base::any(tempo_check_color == TRUE, na.rm = FALSE)){ # convert integers into colors
        tempo_integer <- base::unlist(x = color[tempo_check_color], recursive = TRUE, use.names = TRUE)
        tempo_color <- saferGG::gg_palette(n = base::max(tempo_integer, na.rm = TRUE), kind = "std", safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        for(i1 in base::seq_along(along.with = data1)){
            if(tempo_check_color[i1] == TRUE){
                color[[i1]] <-tempo_color[color[[i1]]]
            }
        }
    }
    # end integer colors into gg_palette
    # loop (checking inside list compartment)
    compart_null_color <- 0 # will be used to attribute a color when color is non-null but a compartment of color is NULL
    data1_ini <- data1 # to report NA removal
    removed_row_nb <- base::vector(mode = "list", length = base::length(x = data1)) # to report NA removal. Contains NULL
    removed_rows <- base::vector(mode = "list", length = base::length(x = data1)) # to report NA removal. Contains NULL
    for(i1 in base::seq_along(along.with = data1)){
        tempo <- saferDev::arg_check(data = data1[[i1]], data_name = base::ifelse(test = base::length(x = data1)== 1L, yes = "data1 ARGUMENT", no = base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT", collapse = NULL, recycle0 = FALSE)), class = "data.frame", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
            # nocov start (unreachable: subsumed by the earlier data-frame check of data1 (MUST BE A DATA FRAME OR A LIST OF DATA FRAMES))
        if(tempo$problem == TRUE){
            base::stop(base::paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
            # nocov end
        # reserved word checking
        if(base::any(base::names(x = data1[[i1]]) %in% reserved_words, na.rm = FALSE)){ # I do not use fun_name_change() because cannot control y before creating "fake_y". But ok because reserved are not that common
            tempo_cat <- base::paste0(error_text_start, "COLUMN NAMES OF ", base::ifelse(test = base::length(x = data1)== 1L, yes = "data1 ARGUMENT", no = base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT", collapse = NULL, recycle0 = FALSE)), " ARGUMENT CANNOT BE ONE OF THESE WORDS\n", base::paste(reserved_words, collapse = " ", sep = " ", recycle0 = FALSE), "\nTHESE ARE RESERVED FOR THE ", function_name, " FUNCTION", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        if( ! (base::is.null(x = add))){
            if(base::any(base::sapply(X = reserved_words, FUN = base::grepl, x = add, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE, simplify = TRUE, USE.NAMES = TRUE), na.rm = FALSE)){
                tempo_cat <- base::paste0(error_text_start, "DETECTION OF COLUMN NAMES OF data1 IN THE add ARGUMENT STRING, THAT CORRESPOND TO RESERVED STRINGS FOR ", function_name, "\nFOLLOWING COLUMN NAMES HAVE TO BE CHANGED:\n", base::paste(arg_names[base::sapply(X = reserved_words, FUN = base::grepl, x = add, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE, simplify = TRUE, USE.NAMES = TRUE)], collapse = "\n", sep = " ", recycle0 = FALSE), "\nFOR INFORMATION, THE RESERVED WORDS ARE:\n", base::paste(reserved_words, collapse = "\n", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else if(base::any(base::sapply(X = arg_names, FUN = base::grepl, x = add, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE, simplify = TRUE, USE.NAMES = TRUE), na.rm = FALSE)){
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") NAMES OF ", function_name, " ARGUMENTS DETECTED IN THE add STRING:\n", base::paste(arg_names[base::sapply(X = arg_names, FUN = base::grepl, x = add, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE, simplify = TRUE, USE.NAMES = TRUE)], collapse = "\n", sep = " ", recycle0 = FALSE), "\nRISK OF WRONG OBJECT USAGE INSIDE ", function_name, collapse = NULL, recycle0 = FALSE)
                warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            }
        }
        # end reserved word checking
        # check of geom now because required for y argument
        tempo <- saferDev::arg_check(data = geom[[i1]], data_name = base::ifelse(test = base::length(x = geom)== 1L, yes = "geom", no = base::paste0("geom NUMBER ", i1, collapse = NULL, recycle0 = FALSE)), options = base::c("geom_point", "geom_line", "geom_path", "geom_step", "geom_hline", "geom_vline", "geom_stick"), class = NULL, typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        if(tempo$problem == TRUE){
            base::stop(base::paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
            # nocov start (unreachable: a NULL geom_step_dir is already rejected by the secondary-checking NULL/empty-string block)
        if(geom[[i1]] == "geom_step" & base::is.null(x = geom_step_dir[[i1]])){
            tempo_cat <- base::paste0(error_text_start, "", base::ifelse(test = base::length(x = geom_step_dir)== 1L, yes = "geom_step_dir", no = base::paste0("ELEMENT ", i1, " OF geom_step_dir ARGUMENT", collapse = NULL, recycle0 = FALSE)), ": geom_step_dir ARGUMENT CANNOT BE NULL IF ", base::ifelse(test = base::length(x = geom)== 1L, yes = "geom", no = base::paste0("ELEMENT ", i1, " OF geom", collapse = NULL, recycle0 = FALSE)), " ARGUMENT IS \"geom_step\"\nHERE geom_step_dir ARGUMENT IS: ", base::paste(geom_step_dir[[i1]], collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }else if(geom[[i1]] == "geom_step" & ! base::is.null(x = geom_step_dir[[i1]])){
            tempo <- saferDev::arg_check(data = geom_step_dir[[i1]], data_name = base::ifelse(test = base::length(x = geom_step_dir)== 1L, yes = "geom_step_dir", no = base::paste0("geom_step_dir NUMBER ", i1, collapse = NULL, recycle0 = FALSE)), options = base::c("vh", "hv", "mid"), class = NULL, typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
            if(tempo$problem == TRUE){
                base::stop(base::paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
        }
        if( ! (base::is.null(x = geom_stick_base))){
            if(geom[[i1]] == "geom_stick" & ! base::is.null(x = geom_stick_base[[i1]])){
                tempo <- saferDev::arg_check(data = geom_stick_base[[i1]], data_name = base::ifelse(test = base::length(x = geom_stick_base)== 1L, yes = "geom_stick_base", no = base::paste0("geom_stick_base NUMBER ", i1, collapse = NULL, recycle0 = FALSE)), class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
                if(tempo$problem == TRUE){
                    base::stop(base::paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                }
            }
        }
        # end check of geom now because required for y argument
        if(base::is.null(x = x[[i1]])){
            if(base::all(geom[[i1]] != "geom_hline", na.rm = FALSE)){
                tempo_cat <- base::paste0(error_text_start, "", base::ifelse(test = base::length(x = x)== 1L, yes = "x", no = base::paste0("ELEMENT ", i1, " OF x ARGUMENT", collapse = NULL, recycle0 = FALSE)), " IN ", base::ifelse(test = base::length(x = data1)== 1L, yes = "data1 ARGUMENT", no = base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT", collapse = NULL, recycle0 = FALSE)), ": x ARGUMENT CANNOT BE NULL EXCEPT IF ", base::ifelse(test = base::length(x = geom)== 1L, yes = "x", no = base::paste0("geom NUMBER ", i1, collapse = NULL, recycle0 = FALSE)), " ARGUMENT IS \"geom_hline\"\nHERE geom ARGUMENT IS: ", base::paste(geom[[i1]], collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                x[[i1]] <- "fake_x"
                data1[[i1]] <- base::cbind(data1[[i1]], fake_x = NA, stringsAsFactors = TRUE, deparse.level = 1)
                data1[[i1]][, "fake_x"] <- base::as.numeric(x = data1[[i1]][, "fake_x"])
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") NULL ", base::ifelse(test = base::length(x = x)== 1L, yes = "x", no = base::paste0("ELEMENT ", i1, " OF x", collapse = NULL, recycle0 = FALSE)), " ARGUMENT ASSOCIATED TO ", base::ifelse(test = base::length(x = geom)== 1L, yes = "geom", no = base::paste0("geom NUMBER ", i1, collapse = NULL, recycle0 = FALSE)), " ARGUMENT ", geom[[i1]], " -> FAKE COLUMN ADDED TO DATA FRAME ", base::ifelse(test = base::length(x = data1)== 1L, yes = "data1 ARGUMENT", no = base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT", collapse = NULL, recycle0 = FALSE)), ", NAMED \"fake_x\" FOR FINAL DRAWING", collapse = NULL, recycle0 = FALSE)
                warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            }
        }else{
            if(base::all(geom[[i1]] == "geom_hline", na.rm = FALSE)){
                tempo_cat <- base::paste0(error_text_start, "", base::ifelse(test = base::length(x = x)== 1L, yes = "x", no = base::paste0("ELEMENT ", i1, " OF x ARGUMENT", collapse = NULL, recycle0 = FALSE)), " IN ", base::ifelse(test = base::length(x = data1)== 1L, yes = "data1 ARGUMENT", no = base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT", collapse = NULL, recycle0 = FALSE)), ": x ARGUMENT MUST BE NULL IF ", base::ifelse(test = base::length(x = geom)== 1L, yes = "geom", no = base::paste0("geom NUMBER ", i1, collapse = NULL, recycle0 = FALSE)), " ARGUMENT IS \"geom_hline\"", collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
            tempo <- saferDev::arg_check(data = x[[i1]], data_name = base::ifelse(test = base::length(x = x)== 1L, yes = "x", no = base::paste0("ELEMENT ", i1, " OF x ARGUMENT", collapse = NULL, recycle0 = FALSE)), class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
            if(tempo$problem == TRUE){
                base::stop(base::paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
        }
        if(base::is.null(x = y[[i1]])){
            if(base::all(geom[[i1]] != "geom_vline", na.rm = FALSE)){
                tempo_cat <- base::paste0(error_text_start, "", base::ifelse(test = base::length(x = y)== 1L, yes = "y", no = base::paste0("ELEMENT ", i1, " OF y ARGUMENT", collapse = NULL, recycle0 = FALSE)), " IN ", base::ifelse(test = base::length(x = data1)== 1L, yes = "data1 ARGUMENT", no = base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT", collapse = NULL, recycle0 = FALSE)), ": y ARGUMENT CANNOT BE NULL EXCEPT IF ", base::ifelse(test = base::length(x = geom)== 1L, yes = "y", no = base::paste0("geom NUMBER ", i1, collapse = NULL, recycle0 = FALSE)), " ARGUMENT IS \"geom_vline\"\nHERE geom ARGUMENT IS: ", base::paste(geom[[i1]], collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                y[[i1]] <- "fake_y"
                data1[[i1]] <- base::cbind(data1[[i1]], fake_y = NA, stringsAsFactors = TRUE, deparse.level = 1)
                data1[[i1]][, "fake_y"] <- base::as.numeric(x = data1[[i1]][, "fake_y"])
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") NULL ", base::ifelse(test = base::length(x = y)== 1L, yes = "y", no = base::paste0("ELEMENT ", i1, " OF y", collapse = NULL, recycle0 = FALSE)), " ARGUMENT ASSOCIATED TO ", base::ifelse(test = base::length(x = geom)== 1L, yes = "geom", no = base::paste0("geom NUMBER ", i1, collapse = NULL, recycle0 = FALSE)), " ARGUMENT ", geom[[i1]], " -> FAKE COLUMN ADDED TO DATA FRAME ", base::ifelse(test = base::length(x = data1)== 1L, yes = "data1 ARGUMENT", no = base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT", collapse = NULL, recycle0 = FALSE)), ", NAMED \"fake_y\" FOR FINAL DRAWING", collapse = NULL, recycle0 = FALSE)
                warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            }
        }else{
            if(base::all(geom[[i1]] == "geom_vline", na.rm = FALSE)){
                tempo_cat <- base::paste0(error_text_start, "", base::ifelse(test = base::length(x = y)== 1L, yes = "y", no = base::paste0("ELEMENT ", i1, " OF y ARGUMENT", collapse = NULL, recycle0 = FALSE)), " IN ", base::ifelse(test = base::length(x = data1)== 1L, yes = "data1 ARGUMENT", no = base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT", collapse = NULL, recycle0 = FALSE)), ": y ARGUMENT MUST BE NULL IF ", base::ifelse(test = base::length(x = geom)== 1L, yes = "geom", no = base::paste0("geom NUMBER ", i1, collapse = NULL, recycle0 = FALSE)), " ARGUMENT IS \"geom_vline\"", collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
            tempo <- saferDev::arg_check(data = y[[i1]], data_name = base::ifelse(test = base::length(x = y)== 1L, yes = "y", no = base::paste0("ELEMENT ", i1, " OF y ARGUMENT", collapse = NULL, recycle0 = FALSE)), class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
            if(tempo$problem == TRUE){
                base::stop(base::paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
        }
        # x[[i1]] and y[[i1]] not NULL anymore
        if( ! (x[[i1]] %in% base::names(x = data1[[i1]]))){
            tempo_cat <- base::paste0(error_text_start, "", base::ifelse(test = base::length(x = x)== 1L, yes = "x", no = base::paste0("ELEMENT ", i1, " OF x", collapse = NULL, recycle0 = FALSE)), " ARGUMENT MUST BE A COLUMN NAME OF ", base::ifelse(test = base::length(x = data1)== 1L, yes = "data1 ARGUMENT", no = base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT\nHERE IT IS: ", base::paste(x[[i1]], collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        if( ! (y[[i1]] %in% base::names(x = data1[[i1]]))){
            tempo_cat <- base::paste0(error_text_start, "", base::ifelse(test = base::length(x = y)== 1L, yes = "y", no = base::paste0("ELEMENT ", i1, " OF y", collapse = NULL, recycle0 = FALSE)), " ARGUMENT MUST BE A COLUMN NAME OF ", base::ifelse(test = base::length(x = data1)== 1L, yes = "data1 ARGUMENT", no = base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT\nHERE IT IS: ", base::paste(y[[i1]], collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        if(x[[i1]] != "fake_x"){ # fake_x column is all-NA by design (added when x is NULL with geom_hline), so the NA-only arg_check would fail: skip it in this case
            if(base::inherits(x = data1[[i1]][, x[[i1]]], what = "factor", which = FALSE)){ # factors have mode "numeric" but cannot be plotted as coordinates: dedicated check to avoid the obscure ggplot2 "'range' not meaningful for factors" error
                tempo_cat <- base::paste0(error_text_start, "", base::ifelse(test = base::length(x = x)== 1L, yes = "x ARGUMENT (AS COLUMN NAME OF data1 DATA FRAME)", no = base::paste0("ELEMENT ", i1, " OF x ARGUMENT (AS COLUMN NAME OF data1 DATA FRAME NUMBER ", i1, ")", collapse = NULL, recycle0 = FALSE)), " CANNOT BE A FACTOR BUT MUST BE MADE OF NUMBERS", collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
            tempo <- saferDev::arg_check(data = data1[[i1]][, x[[i1]]], data_name = base::ifelse(test = base::length(x = x)== 1L, yes = "x ARGUMENT (AS COLUMN NAME OF data1 DATA FRAME)", no = base::paste0("ELEMENT ", i1, " OF x ARGUMENT", " (AS COLUMN NAME OF data1 DATA FRAME NUMBER ", i1, ")", collapse = NULL, recycle0 = FALSE)), class = NULL, typeof = NULL, mode = "numeric", length = NULL, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        if(tempo$problem == TRUE){
            base::stop(base::paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        }
        if(y[[i1]] != "fake_y"){ # fake_y column is all-NA by design (added when y is NULL with geom_vline), so the NA-only arg_check would fail: skip it in this case
            if(base::inherits(x = data1[[i1]][, y[[i1]]], what = "factor", which = FALSE)){ # factors have mode "numeric" but cannot be plotted as coordinates: dedicated check to avoid the obscure ggplot2 "'range' not meaningful for factors" error
                tempo_cat <- base::paste0(error_text_start, "", base::ifelse(test = base::length(x = y)== 1L, yes = "y ARGUMENT (AS COLUMN NAME OF data1 DATA FRAME)", no = base::paste0("ELEMENT ", i1, " OF y ARGUMENT (AS COLUMN NAME OF data1 DATA FRAME NUMBER ", i1, ")", collapse = NULL, recycle0 = FALSE)), " CANNOT BE A FACTOR BUT MUST BE MADE OF NUMBERS", collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
            tempo <- saferDev::arg_check(data = data1[[i1]][, y[[i1]]], data_name = base::ifelse(test = base::length(x = y)== 1L, yes = "y ARGUMENT (AS COLUMN NAME OF data1 DATA FRAME)", no = base::paste0("ELEMENT ", i1, " OF y ARGUMENT", " (AS COLUMN NAME OF data1 DATA FRAME NUMBER ", i1, ")", collapse = NULL, recycle0 = FALSE)), class = NULL, typeof = NULL, mode = "numeric", length = NULL, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        if(tempo$problem == TRUE){
            base::stop(base::paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        }
        if(x[[i1]] == "fake_x" & y[[i1]] == "fake_y"){ # because the code cannot accept to be both "fake_x" and "fake_y" at the same time
            # nocov start (unreachable: x and y cannot both be fake in the same data frame (fake_x requires geom_hline, fake_y requires geom_vline, and geom is a single value))
            tempo_cat <- base::paste0(error_text_start, "CODE INCONSISTENCY 2\nTHE CODE CANNOT ACCEPT x AND y TO BE \"fake_x\" AND \"fake_y\" IN THE SAME DATA FRAME ", i1, " ", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }
        
        if(( ! base::is.null(x = categ)) & ( ! base::is.null(x = categ[[i1]]))){ # base::is.null(categ[[i1]]) works even if categ is NULL # base::is.null(categ[[i1]]) works even if categ is NULL # if categ[[i1]] = NULL, fake_categ will be created later on
            tempo <- saferDev::arg_check(data = categ[[i1]], data_name = base::ifelse(test = base::length(x = categ)== 1L, yes = "categ", no = base::paste0("ELEMENT ", i1, " OF categ ARGUMENT", collapse = NULL, recycle0 = FALSE)), class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
            if(tempo$problem == TRUE){
                base::stop(base::paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
            if( ! (categ[[i1]] %in% base::names(x = data1[[i1]]))){
                tempo_cat <- base::paste0(error_text_start, "", base::ifelse(test = base::length(x = categ)== 1L, yes = "categ", no = base::paste0("ELEMENT ", i1, " OF categ", collapse = NULL, recycle0 = FALSE)), " ARGUMENT MUST BE A COLUMN NAME OF ", base::ifelse(test = base::length(x = data1)== 1L, yes = "data1 ARGUMENT", no = base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT\nHERE IT IS: ", base::paste(categ[[i1]], collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
            tempo1 <- saferDev::arg_check(data = data1[[i1]][, categ[[i1]]], data_name = base::ifelse(test = base::length(x = categ)== 1L, yes = "categ OF data1 ARGUMENT", no = base::paste0("ELEMENT ", i1, " OF categ ARGUMENT IN DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT", collapse = NULL, recycle0 = FALSE)), class = NULL, typeof = "character", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
            tempo2 <- saferDev::arg_check(data = data1[[i1]][, categ[[i1]]], data_name = base::ifelse(test = base::length(x = categ)== 1L, yes = "categ OF data1 ARGUMENT", no = base::paste0("ELEMENT ", i1, " OF categ ARGUMENT IN DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT", collapse = NULL, recycle0 = FALSE)), class = "factor", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
            if(tempo1$problem == TRUE & tempo2$problem == TRUE){
                tempo_cat <- base::paste0(error_text_start, "", base::ifelse(test = base::length(x = categ)== 1L, yes = "categ OF data1 ARGUMENT", no = base::paste0("ELEMENT ", i1, " OF categ ARGUMENT IN DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT", collapse = NULL, recycle0 = FALSE)), " MUST BE A FACTOR OR CHARACTER VECTOR", collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else if(tempo1$problem == FALSE){
                data1[[i1]][, categ[[i1]]] <- base::factor(x = data1[[i1]][, categ[[i1]]], exclude = NA, ordered = base::is.ordered(x = data1[[i1]][, categ[[i1]]]), nmax = NA) # if already a factor, change nothing, if characters, levels according to alphabetical order
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") IN ", base::ifelse(test = base::length(x = categ)== 1L, yes = "categ", no = base::paste0("ELEMENT ", i1, " OF categ ARGUMENT", collapse = NULL, recycle0 = FALSE)), " IN ", base::ifelse(test = base::length(x = data1)== 1L, yes = "data1 ARGUMENT", no = base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT", collapse = NULL, recycle0 = FALSE)), ", THE CHARACTER COLUMN HAS BEEN CONVERTED TO FACTOR", collapse = NULL, recycle0 = FALSE)
                warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                
            }
            if(geom[[i1]] == "geom_vline" | geom[[i1]] == "geom_hline"){
                if(base::length(x = base::unique(x = data1[[i1]][, categ[[i1]]], incomparables = FALSE)) != base::nrow(x = data1[[i1]])){
                    tempo_cat <- base::paste0(error_text_start, "", base::ifelse(test = base::length(x = geom)== 1L, yes = "geom OF data1 ARGUMENT", no = base::paste0("geom NUMBER ", i1, " OF DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT", collapse = NULL, recycle0 = FALSE)), " ARGUMENT IS ", geom[[i1]], ", MEANING THAT ", base::ifelse(test = base::length(x = categ)== 1L, yes = "categ OF data1 ARGUMENT", no = base::paste0("ELEMENT ", i1, " OF categ ARGUMENT IN DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT", collapse = NULL, recycle0 = FALSE)), " MUST HAVE A DIFFERENT CLASS PER LINE OF data1 (ONE x VALUE PER CLASS)", collapse = NULL, recycle0 = FALSE)
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                }
            }
        }else if(( ! base::is.null(x = categ)) & base::is.null(x = categ[[i1]])){ # base::is.null(categ[[i1]]) works even if categ is NULL # if categ[[i1]] = NULL, fake_categ will be created. WARNING: base::is.null(categ[[i1]]) means no legend display (see above), because categ has not been precised. This also means a single color for data1[[i1]]
            if(base::length(x = color[[i1]]) > 1){ # 0 means base::is.null(color[[i1]]) or base::is.null(color) and 1 is ok -> single color for data1[[i1]]
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") NULL ", base::ifelse(test = base::length(x = categ)== 1L, yes = "categ", no = base::paste0("ELEMENT ", i1, " OF categ", collapse = NULL, recycle0 = FALSE)), " ARGUMENT BUT CORRESPONDING COLORS IN ", base::ifelse(test = base::length(x = color)== 1L, yes = "color", no = base::paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT", collapse = NULL, recycle0 = FALSE)), " HAS LENGTH OVER 1\n", base::paste(color[[i1]], collapse = " ", sep = " ", recycle0 = FALSE), "\nWHICH IS NOT COMPATIBLE WITH NULL CATEG -> COLOR RESET TO A SINGLE COLOR", collapse = NULL, recycle0 = FALSE)
                warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                color[i1] <- base::list(NULL) # will provide a single color below # Warning color[[i1]] <- NULL removes the compartment
            }
            categ[[i1]] <- "fake_categ"
            data1[[i1]] <- base::cbind(data1[[i1]], fake_categ = "", stringsAsFactors = TRUE, deparse.level = 1)
            # inactivated because give a different color to different "Line_" categ while a single color for all the data1[[i1]] required. Thus, put back after the color management
            # if(geom[[i1]] == "geom_hline" | geom[[i1]] == "geom_vline"){
            # data1[[i1]][, "fake_categ"] <- base::paste0("Line_", 1:nrow(data1[[i1]]))
            # }else{
            data1[[i1]][, "fake_categ"] <- data1[[i1]][, "fake_categ"] # base::as.numeric("") create a vector of NA but class numeric
            # }
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") NULL ", base::ifelse(test = base::length(x = categ)== 1L, yes = "categ", no = base::paste0("ELEMENT ", i1, " OF categ", collapse = NULL, recycle0 = FALSE)), " ARGUMENT -> FOR DATA FRAME ", base::ifelse(test = base::length(x = data1)== 1L, yes = "data1 ARGUMENT:", no = base::paste0("NUMBER ", i1, " OF data1 ARGUMENT:", collapse = NULL, recycle0 = FALSE)), "\n- FAKE \"fake_categ\" COLUMN ADDED FILLED WITH \"\"(OR WITH \"Line_...\" FOR LINES)\n- SINGLE COLOR USED FOR PLOTTING\n- NO LEGEND DISPLAYED", collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
        }
        # OK: if categ is not NULL, all the non-null categ columns of data1 are factors from here
        
        # management of log scale and Inf removal
        if(x[[i1]] != "fake_x"){
            if(base::any(( ! base::is.finite(x = data1[[i1]][, x[[i1]]])) & ( ! base::is.na(x = data1[[i1]][, x[[i1]]])), na.rm = FALSE)){ # is.finite also detects NA: ( ! base::is.finite(data1[, y])) & ( ! base::is.na(data1[, y])) detects only Inf
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") PRESENCE OF -Inf OR Inf VALUES IN ", base::ifelse(test = base::length(x = categ)== 1L, yes = "x", no = base::paste0("ELEMENT ", i1, " OF x ARGUMENT", collapse = NULL, recycle0 = FALSE)), " IN ", base::ifelse(test = base::length(x = data1)== 1L, yes = "data1 ARGUMENT", no = base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT", collapse = NULL, recycle0 = FALSE)), " AND CORRESPONDING ROWS REMOVED (SEE $removed_row_nb AND $removed_rows)", collapse = NULL, recycle0 = FALSE)
                warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            }
        }
        if(y[[i1]] != "fake_y"){
            if(base::any(( ! base::is.finite(x = data1[[i1]][, y[[i1]]])) & ( ! base::is.na(x = data1[[i1]][, y[[i1]]])), na.rm = FALSE)){ # is.finite also detects NA: ( ! base::is.finite(data1[, y])) & ( ! base::is.na(data1[, y])) detects only Inf
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") PRESENCE OF -Inf OR Inf VALUES IN ", base::ifelse(test = base::length(x = categ)== 1L, yes = "y", no = base::paste0("ELEMENT ", i1, " OF y ARGUMENT", collapse = NULL, recycle0 = FALSE)), " IN ", base::ifelse(test = base::length(x = data1)== 1L, yes = "data1 ARGUMENT", no = base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT", collapse = NULL, recycle0 = FALSE)), " AND CORRESPONDING ROWS REMOVED (SEE $removed_row_nb AND $removed_rows)", collapse = NULL, recycle0 = FALSE)
                warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            }
        }
        # log conversion
        if(x_log != "no"){
            tempo1 <- ! base::is.finite(x = data1[[i1]][, x[[i1]]]) # where are initial NA and Inf
            data1[[i1]][, x[[i1]]] <- base::suppressWarnings(expr = base::get(x = x_log, pos = -1L, envir = base::environment(fun = NULL), mode = "any", inherits = TRUE)(data1[[i1]][, x[[i1]]]), classes = "warning")# no env = sys.nframe(), inherit = FALSE in get() because look for function in the classical scope
            if(base::any( ! (tempo1 | base::is.finite(x = data1[[i1]][, x[[i1]]])), na.rm = FALSE)){
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") LOG CONVERSION INTRODUCED -Inf OR Inf OR NaN VALUES IN ", base::ifelse(test = base::length(x = categ)== 1L, yes = "x", no = base::paste0("ELEMENT ", i1, " OF x ARGUMENT", collapse = NULL, recycle0 = FALSE)), " IN ", base::ifelse(test = base::length(x = data1)== 1L, yes = "data1 ARGUMENT", no = base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT", collapse = NULL, recycle0 = FALSE)), " AND CORRESPONDING ROWS REMOVED (SEE $removed_row_nb AND $removed_rows)", collapse = NULL, recycle0 = FALSE)
                warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            }
        }
        if(y_log != "no"){
            tempo1 <- ! base::is.finite(x = data1[[i1]][, y[[i1]]]) # where are initial NA and Inf
            data1[[i1]][, y[[i1]]] <- base::suppressWarnings(expr = base::get(x = y_log, pos = -1L, envir = base::environment(fun = NULL), mode = "any", inherits = TRUE)(data1[[i1]][, y[[i1]]]), classes = "warning")# no env = sys.nframe(), inherit = FALSE in get() because look for function in the classical scope
            if(base::any( ! (tempo1 | base::is.finite(x = data1[[i1]][, y[[i1]]])), na.rm = FALSE)){
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") LOG CONVERSION INTRODUCED -Inf OR Inf OR NaN VALUES IN ", base::ifelse(test = base::length(x = categ)== 1L, yes = "y", no = base::paste0("ELEMENT ", i1, " OF y ARGUMENT", collapse = NULL, recycle0 = FALSE)), " IN ", base::ifelse(test = base::length(x = data1)== 1L, yes = "data1 ARGUMENT", no = base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT", collapse = NULL, recycle0 = FALSE)), " AND CORRESPONDING ROWS REMOVED (SEE $removed_row_nb AND $removed_rows)", collapse = NULL, recycle0 = FALSE)
                warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            }
        }
        # Inf removal
        # removed_row_nb[[i1]] <- NULL # already NULL and Warning this removes the compartment
        removed_rows[[i1]] <- base::data.frame(stringsAsFactors = FALSE, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE)
        if(base::any(( ! base::is.finite(x = data1[[i1]][, x[[i1]]])) & ( ! base::is.na(x = data1[[i1]][, x[[i1]]])), na.rm = FALSE)){ # is.finite also detects NA: ( ! base::is.finite(data1[[i1]][, x[[i1]]])) & ( ! base::is.na(data1[[i1]][, x[[i1]]])) detects only Inf
            removed_row_nb[[i1]] <- base::c(removed_row_nb[[i1]], base::which(x = ( ! base::is.finite(x = data1[[i1]][, x[[i1]]])) & ( ! base::is.na(x = data1[[i1]][, x[[i1]]])), arr.ind = FALSE, useNames = TRUE))
        }
        if(base::any(( ! base::is.finite(x = data1[[i1]][, y[[i1]]])) & ( ! base::is.na(x = data1[[i1]][, y[[i1]]])), na.rm = FALSE)){ # is.finite also detects NA: ( ! base::is.finite(data1[[i1]][, y[[i1]]])) & ( ! base::is.na(data1[[i1]][, y[[i1]]])) detects only Inf
            removed_row_nb[[i1]] <- base::c(removed_row_nb[[i1]], base::which(x = ( ! base::is.finite(x = data1[[i1]][, y[[i1]]])) & ( ! base::is.na(x = data1[[i1]][, y[[i1]]])), arr.ind = FALSE, useNames = TRUE))
        }
        if( ! base::is.null(x = removed_row_nb[[i1]])){
            removed_row_nb[[i1]] <- base::unique(x = removed_row_nb[[i1]], incomparables = FALSE) # to remove the duplicated positions (NA in both x and y)
            removed_rows[[i1]] <- base::rbind(removed_rows[[i1]], data1_ini[[i1]][removed_row_nb[[i1]], ], deparse.level = 1) # here data1_ini used to have the y = O rows that will be removed because of Inf creation after log transformation
            data1[[i1]] <- data1[[i1]][-removed_row_nb[[i1]], ]
            data1_ini[[i1]] <- data1_ini[[i1]][-removed_row_nb[[i1]], ] #
        }
        # From here, data1 and data_ini have no more Inf
        # end Inf removal
        # x_lim and y_lim dealt later on, after the end f the loop
        # end management of log scale and Inf removal
        # na detection and removal
        column_check <- base::unique(x = base::unlist(x = base::c( # unlist because creates a list
            if(x[[i1]] == "fake_x"){NULL}else{x[[i1]]}, 
            if(y[[i1]] == "fake_y"){NULL}else{y[[i1]]}, 
            if( ! base::is.null(x = categ)){if(base::is.null(x = categ[[i1]])){
                # nocov start (unreachable: NULL categ compartments are converted to "fake_categ" earlier in this loop, so categ[[i1]] is never NULL here)
                NULL
                # nocov end
            }else{categ[[i1]]}}, 
            if( ! base::is.null(x = facet_categ)){if(base::is.null(x = facet_categ[[i1]])){
                # nocov start (unreachable: facet_categ is a length-1 list and can only be non-NULL with a single data frame, so facet_categ[[i1]] is never NULL here)
                NULL
                # nocov end
            }else{facet_categ[[i1]]}}
        ), recursive = TRUE, use.names = TRUE), incomparables = FALSE) # dot_categ because can be a 3rd column of data1
        if(base::any(base::is.na(x = data1[[i1]][, column_check]), na.rm = FALSE)){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") NA DETECTED IN COLUMNS ", base::paste(column_check, collapse = " ", sep = " ", recycle0 = FALSE), " OF ", base::ifelse(test = base::length(x = data1)== 1L, yes = "data1 ARGUMENT", no = base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT", collapse = NULL, recycle0 = FALSE)), " AND CORRESPONDING ROWS REMOVED (SEE $removed_row_nb AND $removed_rows)", collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            for(i3 in base::seq_along(along.with = column_check)){
                if(base::any(base::is.na(x = data1[[i1]][, column_check[i3]]), na.rm = FALSE)){
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("NA REMOVAL DUE TO COLUMN ", column_check[i3], " OF ", base::ifelse(test = base::length(x = data1)== 1L, yes = "data1 ARGUMENT", no = base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT", collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                    warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                }
            }
            tempo <- base::unique(x = base::unlist(x = base::lapply(X = base::lapply(X = base::c(data1[[i1]][column_check]), FUN = base::is.na), FUN = base::which, arr.ind = FALSE, useNames = TRUE), recursive = TRUE, use.names = TRUE), incomparables = FALSE)
            removed_row_nb[[i1]] <- base::c(removed_row_nb[[i1]], tempo)
            removed_rows[[i1]] <- base::rbind(removed_rows[[i1]], data1_ini[[i1]][tempo, ], deparse.level = 1) #  # tempo used because removed_row_nb is not empty. Here data1_ini used to have the non NA rows that will be removed because of NAN creation after log transformation (neg values for instance)
            column_check <- column_check[ ! (column_check == x[[i1]] | column_check == y[[i1]])] # remove x and y to keep quali columns
            if(base::length(x = tempo) != 0){
                data1[[i1]] <- data1[[i1]][-tempo, ] # WARNING tempo here and not removed_row_nb because the latter contain more numbers thant the former
                data1_ini[[i1]] <- data1_ini[[i1]][-tempo, ] # WARNING tempo here and not removed_row_nb because the latter contain more numbers than the former
                for(i4 in base::seq_len(length.out = base::length(x = column_check))){ # seq_len() because column_check can be empty (when only x and y columns had NA), and 1:length() would then loop on c(1, 0)
                    if(base::any( ! base::unique(x = removed_rows[[i1]][, column_check[i4]], incomparables = FALSE) %in% base::unique(x = data1[[i1]][, column_check[i4]], incomparables = FALSE), na.rm = FALSE)){
                        warn_count <- warn_count + 1
                        tempo_warn <- base::paste0("(", warn_count,") IN COLUMN ", column_check[i4], " OF ", base::ifelse(test = base::length(x = data1)== 1L, yes = "data1 ARGUMENT", no = base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT", collapse = NULL, recycle0 = FALSE)), ", THE FOLLOWING CLASSES HAVE DISAPPEARED AFTER NA REMOVAL\n(IF COLUMN USED IN THE PLOT, THIS CLASS WILL NOT BE DISPLAYED):\n", base::paste(base::unique(x = removed_rows[[i1]][, column_check[i4]], incomparables = FALSE)[ ! base::unique(x = removed_rows[[i1]][, column_check[i4]], incomparables = FALSE) %in% base::unique(x = data1[[i1]][, column_check[i4]], incomparables = FALSE)], collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
                        warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                        tempo_levels <- base::levels(x = data1[[i1]][, column_check[i4]])[base::levels(x = data1[[i1]][, column_check[i4]]) %in% base::unique(x = base::as.character(x = data1[[i1]][, column_check[i4]]), incomparables = FALSE)]
                        data1[[i1]][, column_check[i4]] <- base::factor(x = base::as.character(x = data1[[i1]][, column_check[i4]]), levels = tempo_levels, exclude = NA, ordered = FALSE, nmax = NA)
                        if(column_check[i4] %in% categ[[i1]] & ! base::is.null(x = categ_class_order)){
                            categ_class_order[[i1]] <- base::levels(x = data1[[i1]][, column_check[i4]])[base::levels(x = data1[[i1]][, column_check[i4]]) %in% base::unique(x = data1[[i1]][, column_check[i4]], incomparables = FALSE)] # remove the absent class in the categ_class_order vector
                            data1[[i1]][, column_check[i4]] <- base::factor(x = base::as.character(x = data1[[i1]][, column_check[i4]]), levels = base::unique(x = categ_class_order[[i1]], incomparables = FALSE), exclude = NA, ordered = FALSE, nmax = NA)
                        }
                    }
                }
            }
        }
        # end na detection and removal
        # From here, data1 and data_ini have no more NA or NaN in x, y, categ (if categ != NULL) and facet_categ (if categ != NULL)
        if( ! base::is.null(x = categ_class_order)){
            # the following check will be done several times but I prefer to keep it here, after the creation of categ
            if(base::is.null(x = categ[[i1]]) & ! base::is.null(x = categ_class_order[[i1]])){
                tempo_cat <- base::paste0(error_text_start, "COMPARTMENT ", i1, " OF categ ARGUMENT CANNOT BE NULL IF COMPARTMENT ", i1, " OF categ_class_order ARGUMENT IS NOT NULL: ", base::paste(categ_class_order, collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                if(base::is.null(x = categ_class_order[[i1]])){
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") THE categ_class_order COMPARTMENT ", i1, " IS NULL. ALPHABETICAL ORDER WILL BE APPLIED", collapse = NULL, recycle0 = FALSE)
                    warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                    data1[[i1]][, categ[[i1]]] <- base::factor(x = base::as.character(x = data1[[i1]][, categ[[i1]]]), exclude = NA, ordered = FALSE, nmax = NA) # if already a factor, change nothing, if characters, levels according to alphabetical order
                    categ_class_order[[i1]] <- base::levels(x = data1[[i1]][, categ[[i1]]]) # character vector that will be used later
                }else{
                    tempo <- saferDev::arg_check(data = categ_class_order[[i1]], data_name = base::paste0("COMPARTMENT ", i1 , " OF categ_class_order ARGUMENT", collapse = NULL, recycle0 = FALSE), class = NULL, typeof = "character", mode = NULL, length = base::length(x = base::levels(x = data1[[i1]][, categ[[i1]]])), prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) # base::length(data1[, categ[i1]) -> if data1[, categ[i1] was initially character vector, then conversion as factor after the NA removal, thus class number ok. If data1[, categ[i1] was initially factor, no modification after the NA removal, thus class number ok
                    if(tempo$problem == TRUE){
                        base::stop(base::paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                    }
                }
                if(base::any(base::duplicated(x = categ_class_order[[i1]], incomparables = FALSE), na.rm = FALSE)){
                    tempo_cat <- base::paste0(error_text_start, "COMPARTMENT ", i1, " OF categ_class_order ARGUMENT CANNOT HAVE DUPLICATED CLASSES: ", base::paste(categ_class_order[[i1]], collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                }else if( ! (base::all(categ_class_order[[i1]] %in% base::unique(x = data1[[i1]][, categ[[i1]]], incomparables = FALSE), na.rm = FALSE) & base::all(base::unique(x = data1[[i1]][, categ[[i1]]], incomparables = FALSE) %in% categ_class_order[[i1]], na.rm = FALSE))){
                    tempo_cat <- base::paste0(error_text_start, "COMPARTMENT ", i1, " OF categ_class_order ARGUMENT MUST BE CLASSES OF COMPARTMENT ", i1, " OF categ ARGUMENT\nHERE IT IS:\n", base::paste(categ_class_order[[i1]], collapse = " ", sep = " ", recycle0 = FALSE), "\nFOR COMPARTMENT ", i1, " OF categ_class_order AND IT IS:\n", base::paste(base::unique(x = data1[[i1]][, categ[[i1]]], incomparables = FALSE), collapse = " ", sep = " ", recycle0 = FALSE), "\nFOR COLUMN ", categ[[i1]], " OF data1 NUMBER ", i1, collapse = NULL, recycle0 = FALSE)
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                }else{
                    data1[[i1]][, categ[[i1]]] <- base::factor(x = data1[[i1]][, categ[[i1]]], levels = categ_class_order[[i1]], exclude = NA, ordered = base::is.ordered(x = data1[[i1]][, categ[[i1]]]), nmax = NA) # reorder the factor
                }
                base::names(x = categ_class_order)[i1] <- categ[[i1]]
            }
        }
        # OK: if categ_class_order is not NULL, all the NULL categ_class_order columns of data1 are character from here
        
        if( ! base::is.null(x = legend_name[[i1]])){
            tempo <- saferDev::arg_check(data = legend_name[[i1]], data_name = base::ifelse(test = base::length(x = legend_name)== 1L, yes = "legend_name", no = base::paste0("legend_name NUMBER ", i1, collapse = NULL, recycle0 = FALSE)), class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
            if(tempo$problem == TRUE){
                base::stop(base::paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
        }
        if( ! base::is.null(x = color)){ # if color is NULL, will be filled later on
            # check the nature of color
            if(base::is.null(x = color[[i1]])){
                compart_null_color <- compart_null_color + 1
                color[[i1]] <- grDevices::grey(level = compart_null_color / 8, alpha = NULL) # cannot be more than 7 overlays. Thus 7 different greys. 8/8 is excluded because white dots
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") NULL COLOR IN ", base::ifelse(test = base::length(x = color)== 1L, yes = "color", no = base::paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT", collapse = NULL, recycle0 = FALSE)), " ASSOCIATED TO ", base::ifelse(test = base::length(x = data1)== 1L, yes = "data1 ARGUMENT", no = base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT", collapse = NULL, recycle0 = FALSE)), ", SINGLE COLOR ", base::paste(color[[i1]], collapse = " ", sep = " ", recycle0 = FALSE), " HAS BEEN ATTRIBUTED", collapse = NULL, recycle0 = FALSE)
                warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            }
            tempo1 <- saferDev::arg_check(data = color[[i1]], data_name = base::ifelse(test = base::length(x = color)== 1L, yes = "color", no = base::paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT", collapse = NULL, recycle0 = FALSE)), class = NULL, typeof = "character", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) # na_contain = TRUE in case of colum of data1
            tempo2 <- saferDev::arg_check(data = color[[i1]], data_name = base::ifelse(test = base::length(x = color)== 1L, yes = "color", no = base::paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT", collapse = NULL, recycle0 = FALSE)), class = "factor", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) # idem
            if(tempo1$problem == TRUE & tempo2$problem == TRUE){
                tempo_cat <- base::paste0(error_text_start, "", base::ifelse(test = base::length(x = color)== 1L, yes = "color", no = base::paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT", collapse = NULL, recycle0 = FALSE)), " MUST BE A FACTOR OR CHARACTER VECTOR OR INTEGER VECTOR", collapse = NULL, recycle0 = FALSE) # integer possible because dealt above
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else if( ! (base::all(color[[i1]] %in% grDevices::colors(distinct = FALSE) | base::grepl(pattern = "^#", x = color[[i1]], ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), na.rm = FALSE))){ # check that all strings of color start by #
                tempo_cat <- base::paste0(error_text_start, "", base::ifelse(test = base::length(x = color)== 1L, yes = "color", no = base::paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT", collapse = NULL, recycle0 = FALSE)), " MUST BE A HEXADECIMAL COLOR VECTOR STARTING BY # AND/OR COLOR NAMES GIVEN BY colors() OR A COLUMN NAME OF THE data1 PARAMETER: ", base::paste(base::unique(x = color[[i1]], incomparables = FALSE), collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
            # nocov start (unreachable: the is.na(color) check at the top of the integer-colors loop (before color management) errors for any NA in color)
            if(base::any(base::is.na(x = color[[i1]]), na.rm = FALSE)){
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") IN ", base::ifelse(test = base::length(x = color)== 1L, yes = "color", no = base::paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT", collapse = NULL, recycle0 = FALSE)), ", THE COLORS:\n", base::paste(base::unique(x = color[[i1]], incomparables = FALSE), collapse = " ", sep = " ", recycle0 = FALSE), "\nCONTAINS NA", collapse = NULL, recycle0 = FALSE)
                warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            # nocov end
            }
            # end check the nature of color
            # check the length of color
            if(base::is.null(x = categ) & base::length(x = color[[i1]]) != 1){
                tempo_cat <- base::paste0(error_text_start, "", base::ifelse(test = base::length(x = color)== 1L, yes = "color", no = base::paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT", collapse = NULL, recycle0 = FALSE)), " MUST BE A SINGLE COLOR IF categ IS NULL", collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else if( ! base::is.null(x = categ)){
                # No problem of NA management by ggplot2 because already removed
            # nocov start (unreachable: color is reset to NULL when categ is fake (see the categ NULL management above), so its length is always 1 after the grey attribution)
                if(categ[[i1]] == "fake_categ" & base::length(x = color[[i1]]) != 1){
                    tempo_cat <- base::paste0(error_text_start, "", base::ifelse(test = base::length(x = color)== 1L, yes = "color", no = base::paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT", collapse = NULL, recycle0 = FALSE)), " MUST BE A SINGLE COLOR IF ", base::ifelse(test = base::length(x = categ)== 1L, yes = "categ", no = base::paste0("ELEMENT ", i1, " OF categ ARGUMENT", collapse = NULL, recycle0 = FALSE)), " IS NULL", collapse = NULL, recycle0 = FALSE)
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
                }else if(base::length(x = color[[i1]]) == base::length(x = base::unique(x = data1[[i1]][, categ[[i1]]], incomparables = FALSE))){ # here base::length(color) is equal to the different number of categ
                    data1[[i1]][, categ[[i1]]] <- base::factor(x = data1[[i1]][, categ[[i1]]], exclude = NA, ordered = base::is.ordered(x = data1[[i1]][, categ[[i1]]]), nmax = NA) # if already a factor, change nothing, if characters, levels according to alphabetical order
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") IN ", base::ifelse(test = base::length(x = categ)== 1L, yes = "categ", no = base::paste0("ELEMENT ", i1, " OF categ ARGUMENT", collapse = NULL, recycle0 = FALSE)), " IN ", base::ifelse(test = base::length(x = data1)== 1L, yes = "data1 ARGUMENT", no = base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT", collapse = NULL, recycle0 = FALSE)), ", THE FOLLOWING COLORS:\n", base::paste(color[[i1]], collapse = " ", sep = " ", recycle0 = FALSE), "\nHAVE BEEN ATTRIBUTED TO THESE CLASSES:\n", base::paste(base::levels(x = base::factor(x = data1[[i1]][, categ[[i1]]], exclude = NA, ordered = base::is.ordered(x = data1[[i1]][, categ[[i1]]]), nmax = NA)), collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
                    warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                }else if(base::length(x = color[[i1]]) == base::length(x = data1[[i1]][, categ[[i1]]])){# here base::length(color) is equal to base::nrow(data1[[i1]]) -> Modif to have base::length(color) equal to the different number of categ (base::length(color) == base::length(base::levels(data1[[i1]][, categ[[i1]]])))
                    data1[[i1]] <- base::cbind(data1[[i1]], color = color[[i1]], stringsAsFactors = TRUE, deparse.level = 1)
                    tempo_check <- base::unique(x = data1[[i1]][ , base::c(categ[[i1]], "color")], incomparables = FALSE)
                    if( ! (base::nrow(x = data1[[i1]]) == base::length(x = color[[i1]]) & base::nrow(x = tempo_check) == base::length(x = base::unique(x = data1[[i1]][ , categ[[i1]]], incomparables = FALSE)))){
                        tempo_cat <- base::paste0(error_text_start, "", base::ifelse(test = base::length(x = color)== 1L, yes = "color", no = base::paste0("ELEMENT NUMBER ", i1, " OF color", collapse = NULL, recycle0 = FALSE)), " ARGUMENT HAS THE LENGTH OF ", base::ifelse(test = base::length(x = categ)== 1L, yes = "categ", no = base::paste0("ELEMENT ", i1, " OF categ ARGUMENT", collapse = NULL, recycle0 = FALSE)), " IN ", base::ifelse(test = base::length(x = data1)== 1L, yes = "data1 ARGUMENT", no = base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT", collapse = NULL, recycle0 = FALSE)), "\nBUT IS INCORRECTLY ASSOCIATED TO EACH CLASS OF THIS categ:\n", base::paste(base::unique(x = base::mapply(FUN = base::paste, data1[[i1]][ ,categ[[i1]]], data1[[i1]][ ,"color"], MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE), incomparables = FALSE), collapse = "\n", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
                        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                    }else{
                        data1[[i1]][, categ[[i1]]] <- base::factor(x = data1[[i1]][, categ[[i1]]], exclude = NA, ordered = base::is.ordered(x = data1[[i1]][, categ[[i1]]]), nmax = NA) # if already a factor, change nothing, if characters, levels according to alphabetical order
                        color[[i1]] <- base::unique(x = color[[i1]][base::order(data1[[i1]][, categ[[i1]]], na.last = TRUE, decreasing = FALSE, method = base::c("auto", "shell", "radix"))], incomparables = FALSE) # Modif to have base::length(color) equal to the different number of categ (base::length(color) == base::length(base::levels(data1[[i1]][, categ[[i1]]])))
                        warn_count <- warn_count + 1
                        tempo_warn <- base::paste0("(", warn_count, ") FROM FUNCTION ", function_name, ": ", base::ifelse(test = base::length(x = color)== 1L, yes = "color", no = base::paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT", collapse = NULL, recycle0 = FALSE)), " HAS THE LENGTH OF ", base::ifelse(test = base::length(x = categ)== 1L, yes = "categ", no = base::paste0("ELEMENT ", i1, " OF categ ARGUMENT", collapse = NULL, recycle0 = FALSE)), " IN ", base::ifelse(test = base::length(x = data1)== 1L, yes = "data1 ARGUMENT", no = base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT", collapse = NULL, recycle0 = FALSE)), " COLUMN VALUES\nCOLORS HAVE BEEN RESPECTIVELY ASSOCIATED TO EACH CLASS OF categ AS:\n", base::paste(base::levels(x = base::factor(x = data1[[i1]][, categ[[i1]]], exclude = NA, ordered = base::is.ordered(x = data1[[i1]][, categ[[i1]]]), nmax = NA)), collapse = " ", sep = " ", recycle0 = FALSE), "\n", base::paste(color[[i1]], collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
                        warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                    }
                }else if(base::length(x = color[[i1]])== 1L){
                    data1[[i1]][, categ[[i1]]] <- base::factor(x = data1[[i1]][, categ[[i1]]], exclude = NA, ordered = base::is.ordered(x = data1[[i1]][, categ[[i1]]]), nmax = NA) # if already a factor, change nothing, if characters, levels according to alphabetical order
                    color[[i1]] <- base::rep(x = color[[i1]], times = base::length(x = base::levels(x = data1[[i1]][, categ[[i1]]])))
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") IN ", base::ifelse(test = base::length(x = categ)== 1L, yes = "categ", no = base::paste0("ELEMENT ", i1, " OF categ ARGUMENT", collapse = NULL, recycle0 = FALSE)), " IN ", base::ifelse(test = base::length(x = data1)== 1L, yes = "data1 ARGUMENT", no = base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT", collapse = NULL, recycle0 = FALSE)), ", COLOR HAS LENGTH 1 MEANING THAT ALL THE DIFFERENT CLASSES OF ", base::ifelse(test = base::length(x = categ)== 1L, yes = "categ", no = base::paste0("ELEMENT ", i1, " OF categ ARGUMENT", collapse = NULL, recycle0 = FALSE)), "\n", base::paste(base::levels(x = base::factor(x = data1[[i1]][, categ[[i1]]], exclude = NA, ordered = base::is.ordered(x = data1[[i1]][, categ[[i1]]]), nmax = NA)), collapse = " ", sep = " ", recycle0 = FALSE), "\nWILL HAVE THE SAME COLOR\n", base::paste(color[[i1]], collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
                    warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                }else{
                    tempo_cat <- base::paste0(error_text_start, "", base::ifelse(test = base::length(x = color)== 1L, yes = "color", no = base::paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT", collapse = NULL, recycle0 = FALSE)), " MUST BE\n(1) LENGTH 1\nOR (2) THE LENGTH OF ", base::ifelse(test = base::length(x = categ)== 1L, yes = "categ", no = base::paste0("ELEMENT ", i1, " OF categ ARGUMENT", collapse = NULL, recycle0 = FALSE)), " IN ", base::ifelse(test = base::length(x = data1)== 1L, yes = "data1 ARGUMENT", no = base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT", collapse = NULL, recycle0 = FALSE)), " COLUMN VALUES\nOR (3) THE LENGTH OF THE CLASSES IN THIS COLUMN\nHERE IT IS COLOR LENGTH ", base::length(x = color[[i1]]), " VERSUS CATEG LENGTH ", base::length(x = data1[[i1]][, categ[[i1]]]), " AND CATEG CLASS LENGTH ", base::length(x = base::unique(x = data1[[i1]][, categ[[i1]]], incomparables = FALSE)), "\nPRESENCE OF NA IN THE COLUMN x, y OR categ OF data1 COULD BE THE PROBLEME", collapse = NULL, recycle0 = FALSE)
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                }
            }
        }
        if((geom[[i1]] == "geom_hline" | geom[[i1]] == "geom_vline") & ! base::is.null(x = categ[[i1]])){ # add here after the color management, to deal with the different lines to plot inside any data[[i1]]
            if(categ[[i1]] == "fake_categ"){
                data1[[i1]][, "fake_categ"] <- base::factor(x = base::paste0("Line_", base::formatC(x = 1:base::nrow(x = data1[[i1]]), width = base::nchar(x = base::nrow(x = data1[[i1]]), type = "chars", allowNA = FALSE, keepNA = NA), flag = "0", digits = NULL, format = NULL, mode = NULL, big.mark = "", big.interval = 3L, small.mark = "", small.interval = 5L, decimal.mark = base::getOption(x = "OutDec", default = NULL), preserve.width = "individual", zero.print = NULL, replace.zero = TRUE, drop0trailing = FALSE), collapse = NULL, recycle0 = FALSE), exclude = NA, ordered = FALSE, nmax = NA)
            }
        }
        tempo <- saferDev::arg_check(data = alpha[[i1]], data_name = base::ifelse(test = base::length(x = alpha)== 1L, yes = "alpha", no = base::paste0("alpha NUMBER ", i1, collapse = NULL, recycle0 = FALSE)), prop = TRUE, class = NULL, typeof = NULL, mode = NULL, length = 1, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        if(tempo$problem == TRUE){
            base::stop(base::paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    # end loop (checking inside list compartment)
    if(base::length(x = data1) > 1){
        if(base::length(x = base::unique(x = base::unlist(x = x, recursive = TRUE, use.names = TRUE)[ ! x == "fake_x"], incomparables = FALSE)) > 1){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") THE x ARGUMENT DOES NOT CONTAIN IDENTICAL COLUMN NAMES:\n", base::paste(base::unlist(x = x, recursive = TRUE, use.names = TRUE), collapse = " ", sep = " ", recycle0 = FALSE), "\nX-AXIS OVERLAYING DIFFERENT VARIABLES?", collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
        }
    }
    if(base::length(x = data1) > 1){
        if(base::length(x = base::unique(x = base::unlist(x = y, recursive = TRUE, use.names = TRUE)[ ! y == "fake_y"], incomparables = FALSE)) > 1){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") THE y ARGUMENT DOES NOT CONTAIN IDENTICAL COLUMN NAMES:\n", base::paste(base::unlist(x = y, recursive = TRUE, use.names = TRUE), collapse = " ", sep = " ", recycle0 = FALSE), "\nY-AXIS OVERLAYING DIFFERENT VARIABLES?", collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
        }
    }
    if(base::sum(geom %in% "geom_point", na.rm = FALSE) > 3){
        tempo_cat <- base::paste0(error_text_start, "geom ARGUMENT CANNOT HAVE MORE THAN THREE \"geom_point\" ELEMENTS", collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }else if(base::length(x = geom) - base::sum(geom %in% "geom_point", na.rm = FALSE) > 3){
        tempo_cat <- base::paste0(error_text_start, "geom ARGUMENT CANNOT HAVE MORE THAN THREE LINE ELEMENTS", collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    # x_lim management before transfo by x_log
    if(x_log != "no" & ! base::is.null(x = x_lim)){
        if(base::any(x_lim <= 0, na.rm = FALSE)){
            tempo_cat <- base::paste0(error_text_start, "x_lim ARGUMENT CANNOT HAVE ZERO OR NEGATIVE VALUES WITH THE x_log ARGUMENT SET TO ", x_log, ":\n", base::paste(x_lim, collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(base::any( ! base::is.finite(x = if(x_log == "log10"){base::log10(x = x_lim)}else{base::log2(x = x_lim)}), na.rm = FALSE)){
            tempo_cat <- base::paste0(error_text_start, "x_lim ARGUMENT RETURNS INF/NA WITH THE x_log ARGUMENT SET TO ", x_log, "\nAS SCALE COMPUTATION IS ", base::ifelse(test = x_log == "log10", yes = "log10", no = "log2"), ":\n", base::paste(if(x_log == "log10"){base::log10(x = x_lim)}else{base::log2(x = x_lim)}, collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    if(x_log != "no" & x_include_zero == TRUE){
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count,") x_log ARGUMENT SET TO ", x_log, " AND x_include_zero ARGUMENT SET TO TRUE -> x_include_zero ARGUMENT RESET TO FALSE BECAUSE 0 VALUE CANNOT BE REPRESENTED IN LOG SCALE", collapse = NULL, recycle0 = FALSE)
        warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
        x_include_zero <- FALSE
    }
    # end x_lim management before transfo by x_log
    # y_lim management before transfo by y_log
    if(y_log != "no" & ! base::is.null(x = y_lim)){
        if(base::any(y_lim <= 0, na.rm = FALSE)){
            tempo_cat <- base::paste0(error_text_start, "y_lim ARGUMENT CANNOT HAVE ZERO OR NEGATIVE VALUES WITH THE y_log ARGUMENT SET TO ", y_log, ":\n", base::paste(y_lim, collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(base::any( ! base::is.finite(x = if(y_log == "log10"){base::log10(x = y_lim)}else{base::log2(x = y_lim)}), na.rm = FALSE)){
            tempo_cat <- base::paste0(error_text_start, "y_lim ARGUMENT RETURNS INF/NA WITH THE y_log ARGUMENT SET TO ", y_log, "\nAS SCALE COMPUTATION IS ", base::ifelse(test = y_log == "log10", yes = "log10", no = "log2"), ":\n", base::paste(if(y_log == "log10"){base::log10(x = y_lim)}else{base::log2(x = y_lim)}, collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    if(y_log != "no" & y_include_zero == TRUE){
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count,") y_log ARGUMENT SET TO ", y_log, " AND y_include_zero ARGUMENT SET TO TRUE -> y_include_zero ARGUMENT RESET TO FALSE BECAUSE 0 VALUE CANNOT BE REPRESENTED IN LOG SCALE", collapse = NULL, recycle0 = FALSE)
        warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
        y_include_zero <- FALSE
    }
    # end y_lim management before transfo by y_log
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    # axes management
    if(base::is.null(x = x_lim)){
        if(base::any(base::unlist(x = base::mapply(FUN = "[[", data1, x, SIMPLIFY = FALSE, MoreArgs = NULL, USE.NAMES = TRUE), recursive = TRUE, use.names = TRUE) %in% base::c(Inf, -Inf), na.rm = FALSE)){
            # nocov start (unreachable: -Inf and Inf values of the x columns are removed by the per-compartment loop above, so no -Inf/Inf value can remain here)
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") THE x COLUMN IN data1 CONTAINS -Inf OR Inf VALUES THAT WILL NOT BE CONSIDERED IN THE PLOT RANGE", collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            # nocov end
        }
        x_lim <- base::suppressWarnings(expr = base::range(base::unlist(x = base::mapply(FUN = "[[", data1, x, SIMPLIFY = FALSE, MoreArgs = NULL, USE.NAMES = TRUE), recursive = TRUE, use.names = TRUE), na.rm = TRUE, finite = TRUE), classes = "warning") # finite = TRUE removes all the -Inf and Inf except if only this. In that case, whatever the -Inf and/or Inf present, output -Inf;Inf range. Idem with NA only. y_lim added here. If NULL, ok if y argument has values
    }else if(x_log != "no"){
        x_lim <- base::get(x = x_log, pos = -1L, envir = base::environment(fun = NULL), mode = "any", inherits = TRUE)(x_lim) # no env = sys.nframe(), inherit = FALSE in get() because look for function in the classical scope
    }
    if(x_log != "no"){
        if(base::any( ! base::is.finite(x = x_lim), na.rm = FALSE)){
            # nocov start (unreachable: x_lim computed from data is finite after the Inf/NaN row removal (an all-non-finite column empties data1, which errors earlier))
            tempo_cat <- base::paste0(error_text_start, "x_lim ARGUMENT CANNOT HAVE ZERO OR NEGATIVE VALUES WITH THE x_log ARGUMENT SET TO ", x_log, ":\n", base::paste(x_lim, collapse = " ", sep = " ", recycle0 = FALSE), "\nPLEASE, CHECK DATA VALUES (PRESENCE OF ZERO OR INF VALUES)", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }
    }
    if(base::suppressWarnings(expr = base::all(x_lim %in% base::c(Inf, -Inf), na.rm = FALSE), classes = "warning")){ # happen when x is only NULL
        if(base::all(base::unlist(x = geom, recursive = TRUE, use.names = TRUE) %in% base::c("geom_vline", "geom_stick"), na.rm = FALSE)){
            tempo_cat <- base::paste0(error_text_start, "NOT POSSIBLE TO DRAW geom_vline OR geom_stick KIND OF LINES ALONE IF x_lim ARGUMENT IS SET TO NULL, SINCE NO X-AXIS DEFINED (", base::ifelse(test = base::length(x = x)== 1L, yes = "x", no = base::paste0("ELEMENT ", i1, " OF x", collapse = NULL, recycle0 = FALSE)), " ARGUMENT MUST BE NULL FOR THESE KIND OF LINES)", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            tempo_cat <- base::paste0(error_text_start, "x_lim ARGUMENT MADE OF NA, -Inf OR Inf ONLY: ", base::paste(x_lim, collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    x_lim_order <- base::order(x_lim, na.last = TRUE, decreasing = FALSE, method = base::c("auto", "shell", "radix")) # to deal with inverse axis
    # print(x_lim)
    x_lim <- base::sort(x = x_lim, decreasing = FALSE)
    x_lim[1] <- x_lim[1] - base::abs(x = x_lim[2] - x_lim[1]) * base::ifelse(test = base::diff(x = x_lim_order) > 0, yes = x_right_extra_margin, no = x_left_extra_margin) # diff(x_lim_order) > 0 means not inversed axis
    x_lim[2] <- x_lim[2] + base::abs(x = x_lim[2] - x_lim[1]) * base::ifelse(test = base::diff(x = x_lim_order) > 0, yes = x_left_extra_margin, no = x_right_extra_margin) # diff(x_lim_order) > 0 means not inversed axis
    if(x_include_zero == TRUE){ # no need to check x_log != "no" because done before
        x_lim <- base::range(base::c(x_lim, 0), na.rm = TRUE, finite = TRUE) # finite = TRUE removes all the -Inf and Inf except if only this. In that case, whatever the -Inf and/or Inf present, output -Inf;Inf range. Idem with NA only
    }
    x_lim <- x_lim[x_lim_order]
    if(base::any(base::is.na(x = x_lim), na.rm = FALSE)){
            # nocov start (unreachable: x_lim is finite at this stage (checked above) and the margin computation cannot create NA)
        tempo_cat <- base::paste0(error_text_start, "CODE INCONSISTENCY 3", collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
    }
    if(base::is.null(x = y_lim)){
        if(base::any(base::unlist(x = base::mapply(FUN = "[[", data1, y, SIMPLIFY = FALSE, MoreArgs = NULL, USE.NAMES = TRUE), recursive = TRUE, use.names = TRUE) %in% base::c(Inf, -Inf), na.rm = FALSE)){
            # nocov start (unreachable: -Inf and Inf values of the y columns are removed by the per-compartment loop above, so no -Inf/Inf value can remain here)
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") THE y COLUMN IN data1 CONTAINS -Inf OR Inf VALUES THAT WILL NOT BE CONSIDERED IN THE PLOT RANGE", collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            # nocov end
        }
        y_lim <- base::suppressWarnings(expr = base::range(base::unlist(x = base::mapply(FUN = "[[", data1, y, SIMPLIFY = FALSE, MoreArgs = NULL, USE.NAMES = TRUE), recursive = TRUE, use.names = TRUE), na.rm = TRUE, finite = TRUE), classes = "warning") # finite = TRUE removes all the -Inf and Inf except if only this. In that case, whatever the -Inf and/or Inf present, output -Inf;Inf range. Idem with NA only. y_lim added here. If NULL, ok if y argument has values
    }else if(y_log != "no"){
        y_lim <- base::get(x = y_log, pos = -1L, envir = base::environment(fun = NULL), mode = "any", inherits = TRUE)(y_lim) # no env = sys.nframe(), inherit = FALSE in get() because look for function in the classical scope
    }
    if(y_log != "no"){
        if(base::any( ! base::is.finite(x = y_lim), na.rm = FALSE)){
            # nocov start (unreachable: idem as x_lim (see line 1596))
            tempo_cat <- base::paste0(error_text_start, "y_lim ARGUMENT CANNOT HAVE ZERO OR NEGATIVE VALUES WITH THE y_log ARGUMENT SET TO ", y_log, ":\n", base::paste(y_lim, collapse = " ", sep = " ", recycle0 = FALSE), "\nPLEASE, CHECK DATA VALUES (PRESENCE OF ZERO OR INF VALUES)", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }
    }
    if(base::suppressWarnings(expr = base::all(y_lim %in% base::c(Inf, -Inf), na.rm = FALSE), classes = "warning")){ # happen when y is only NULL
        if(base::all(base::unlist(x = geom, recursive = TRUE, use.names = TRUE) == "geom_vline", na.rm = FALSE)){
            tempo_cat <- base::paste0(error_text_start, "NOT POSSIBLE TO DRAW geom_vline KIND OF LINES ALONE IF y_lim ARGUMENT IS SET TO NULL, SINCE NO Y-AXIS DEFINED (", base::ifelse(test = base::length(x = y)== 1L, yes = "y", no = base::paste0("ELEMENT ", i1, " OF y", collapse = NULL, recycle0 = FALSE)), " ARGUMENT MUST BE NULL FOR THESE KIND OF LINES)", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            # nocov start (unreachable: y_lim contains Inf only when y is all-NA or all-Inf, which empties data1 and errors at the x_lim all-Inf check above)
            tempo_cat <- base::paste0(error_text_start, "y_lim ARGUMENT MADE OF NA, -Inf OR Inf ONLY: ", base::paste(y_lim, collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }
    }
    y_lim_order <- base::order(y_lim, na.last = TRUE, decreasing = FALSE, method = base::c("auto", "shell", "radix")) # to deal with inverse axis
    y_lim <- base::sort(x = y_lim, decreasing = FALSE)
    y_lim[1] <- y_lim[1] - base::abs(x = y_lim[2] - y_lim[1]) * base::ifelse(test = base::diff(x = y_lim_order) > 0, yes = y_bottom_extra_margin, no = y_top_extra_margin) # diff(y_lim_order) > 0 means not inversed axis
    y_lim[2] <- y_lim[2] + base::abs(x = y_lim[2] - y_lim[1]) * base::ifelse(test = base::diff(x = y_lim_order) > 0, yes = y_top_extra_margin, no = y_bottom_extra_margin) # diff(y_lim_order) > 0 means not inversed axis
    if(y_include_zero == TRUE){ # no need to check y_log != "no" because done before
        y_lim <- base::range(base::c(y_lim, 0), na.rm = TRUE, finite = TRUE) # finite = TRUE removes all the -Inf and Inf except if only this. In that case, whatever the -Inf and/or Inf present, output -Inf;Inf range. Idem with NA only
    }
    y_lim <- y_lim[y_lim_order]
            # nocov start (unreachable: idem as x_lim (see line 1619))
    if(base::any(base::is.na(x = y_lim), na.rm = FALSE)){
        tempo_cat <- base::paste0(error_text_start, "CODE INCONSISTENCY 4", collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
    }
    # end axes management
    
    
    
    
    # create a fake categ if NULL to deal with legend display
    if(base::is.null(x = categ)){
        categ <- base::vector(mode = "list", length = base::length(x = data1))
        categ[] <- "fake_categ"
        for(i2 in base::seq_along(along.with = data1)){
            data1[[i2]] <- base::cbind(data1[[i2]], fake_categ = "", stringsAsFactors = TRUE, deparse.level = 1)
            if(geom[[i2]] == "geom_hline" | geom[[i2]] == "geom_vline"){
                data1[[i2]][, "fake_categ"] <- base::factor(x = base::paste0("Line_", 1:base::nrow(x = data1[[i2]]), collapse = NULL, recycle0 = FALSE), exclude = NA, ordered = FALSE, nmax = NA)
            }
        }
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count,") NULL categ ARGUMENT -> FAKE \"fake_categ\" COLUMN ADDED TO EACH DATA FRAME OF data1, AND FILLED WITH \"\"", collapse = NULL, recycle0 = FALSE)
        warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
    }
    # categ is not NULL anymore
    if(base::is.null(x = categ_class_order)){
        categ_class_order <- base::vector(mode = "list", length = base::length(x = data1))
        tempo_categ_class_order <- NULL
        for(i2 in base::seq_along(along.with = categ_class_order)){
            categ_class_order[[i2]] <- base::levels(x = data1[[i2]][, categ[[i2]]])
            base::names(x = categ_class_order)[i2] <- categ[[i2]]
            tempo_categ_class_order <- base::c(tempo_categ_class_order, base::ifelse(test = i2 != 1, yes = "\n", no = ""), categ_class_order[[i2]])
        }
        if(base::any(base::unlist(x = legend_disp, recursive = TRUE, use.names = TRUE), na.rm = FALSE)){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") THE categ_class_order SETTING IS NULL. ALPHABETICAL ORDER WILL BE APPLIED FOR CLASS ORDERING:\n", base::paste(tempo_categ_class_order, collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
        }
    }
    # end create a fake categ if NULL to deal with legend display
    # categ_class_order is not NULL anymore
    
    
    # vector of color with length as in base::levels(categ) of data1
    if(base::is.null(x = color)){
        color <- base::vector(mode = "list", length = base::length(x = data1))
        length_categ_list <- base::lapply(X = base::lapply(X = base::mapply(FUN = "[[", data1, categ, SIMPLIFY = FALSE, MoreArgs = NULL, USE.NAMES = TRUE), FUN = base::unique, incomparables = FALSE), FUN = function(x){base::length(x = x[ ! base::is.na(x = x)])})
        length_categ_list[base::sapply(X = categ, FUN = "==", e2 = "fake_categ", simplify = TRUE, USE.NAMES = TRUE)] <- 1 # when base::is.null(color), a single color for all the dots or lines of data[[i1]] that contain "fake_categ" category
        total_categ_length <- base::sum(base::unlist(x = length_categ_list, recursive = TRUE, use.names = TRUE), na.rm = TRUE)
        tempo_color <- saferGG::gg_palette(n = total_categ_length, kind = "std", safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        tempo_count <- 0
        for(i2 in base::seq_along(along.with = data1)){
            color[[i2]] <- tempo_color[(1:length_categ_list[[i2]]) + tempo_count]
            tempo_count <- tempo_count + length_categ_list[[i2]]
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") NULL color ARGUMENT -> COLORS RESPECTIVELY ATTRIBUTED TO EACH CLASS OF ", base::ifelse(test = base::length(x = categ)== 1L, yes = "categ", no = base::paste0("ELEMENT ", i2, " OF categ ARGUMENT", collapse = NULL, recycle0 = FALSE)), " (", categ[[i2]], ") IN ", base::ifelse(test = base::length(x = data1)== 1L, yes = "data1 ARGUMENT", no = base::paste0("DATA FRAME NUMBER ", i2, " OF data1 ARGUMENT", collapse = NULL, recycle0 = FALSE)), ":\n", base::paste(color[[i2]], collapse = " ", sep = " ", recycle0 = FALSE), "\n", base::paste(if(base::all(base::levels(x = data1[[i2]][, categ[[i2]]]) == "", na.rm = FALSE)){'\"\"'}else{base::levels(x = data1[[i2]][, categ[[i2]]])}, collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
        }
    }
    # end vector of color with length as in base::levels(categ) of data1
    # color is not NULL anymore
    
    
    
    
    
    # last check
    for(i1 in base::seq_along(along.with = data1)){
            # nocov start (unreachable: color length always matches the number of classes after the color management above)
        if(categ[[i1]] != "fake_categ" & base::length(x = color[[i1]]) != base::length(x = base::unique(x = data1[[i1]][, categ[[i1]]], incomparables = FALSE))){
            tempo_cat <- base::paste0(error_text_start, "LAST CHECK: ", base::ifelse(test = base::length(x = color)== 1L, yes = "color", no = base::paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT", collapse = NULL, recycle0 = FALSE)), " MUST HAVE THE LENGTH OF LEVELS OF ", base::ifelse(test = base::length(x = categ)== 1L, yes = "categ", no = base::paste0("ELEMENT ", i1, " OF categ ARGUMENT", collapse = NULL, recycle0 = FALSE)), " IN ", base::ifelse(test = base::length(x = data1)== 1L, yes = "data1 ARGUMENT", no = base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT", collapse = NULL, recycle0 = FALSE)), "\nHERE IT IS COLOR LENGTH ", base::length(x = color[[i1]]), " VERSUS CATEG LEVELS LENGTH ", base::length(x = base::unique(x = data1[[i1]][, categ[[i1]]], incomparables = FALSE)), "\nREMINDER: A SINGLE COLOR PER CLASS OF CATEG AND A SINGLE CLASS OF CATEG PER COLOR MUST BE RESPECTED", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(categ[[i1]] == "fake_categ" & base::length(x = color[[i1]]) != 1){
            tempo_cat <- base::paste0(error_text_start, "LAST CHECK: ", base::ifelse(test = base::length(x = color)== 1L, yes = "color", no = base::paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT", collapse = NULL, recycle0 = FALSE)), " MUST HAVE LENGTH 1 WHEN ", base::ifelse(test = base::length(x = categ)== 1L, yes = "categ", no = base::paste0("ELEMENT ", i1, " OF categ ARGUMENT", collapse = NULL, recycle0 = FALSE)), " IS NULL\nHERE IT IS COLOR LENGTH ", base::length(x = color[[i1]]), collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }
    }
    # end last check
    
    
    
    
    
    # conversion of geom_hline and geom_vline
    for(i1 in base::seq_along(along.with = data1)){
        if(geom[[i1]] == "geom_hline" | geom[[i1]] == "geom_vline"){
            final_data_frame <- base::data.frame(check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = FALSE)
            for(i3 in base::seq_len(length.out = base::nrow(x = data1[[i1]]))){
                tempo_data_frame <- base::rbind(data1[[i1]][i3, ], data1[[i1]][i3, ], stringsAsFactors = TRUE, deparse.level = 1)
                if(geom[[i1]] == "geom_hline"){
                    tempo_data_frame[, x[[i1]]] <- x_lim
                }else if(geom[[i1]] == "geom_vline"){
                    tempo_data_frame[, y[[i1]]] <- y_lim
                }else{
            # nocov start (unreachable: geom is checked to be geom_hline or geom_vline before entering the loop)
                    tempo_cat <- base::paste0(error_text_start, "CODE INCONSISTENCY 5", collapse = NULL, recycle0 = FALSE)
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
                }
                # 3 lines below inactivated because I put that above
                # if(base::is.null(categ[[i1]])){
                # data1[, "fake_categ"] <- base::paste0("Line_", i3)
                # }
                final_data_frame <- base::rbind(final_data_frame, tempo_data_frame, stringsAsFactors = TRUE, deparse.level = 1)
            }
            data1[[i1]] <- final_data_frame
            geom[[i1]] <- "geom_line"
            if(base::length(x = color[[i1]])== 1L){
                color[[i1]] <- base::rep(x = color[[i1]], times = base::length(x = base::unique(x = data1[[i1]][ , categ[[i1]]], incomparables = FALSE)))
            # nocov start (unreachable: the last check above ensures color length equals the number of classes)
            }else if(base::length(x = color[[i1]]) != base::length(x = base::unique(x = data1[[i1]][ , categ[[i1]]], incomparables = FALSE))){
                tempo_cat <- base::paste0(error_text_start, "geom_hline AND geom_vline CONVERSION TO FIT THE XLIM AND YLIM LIMITS OF THE DATA: ", base::ifelse(test = base::length(x = color)== 1L, yes = "color", no = base::paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT", collapse = NULL, recycle0 = FALSE)), " MUST HAVE THE LENGTH OF LEVELS OF ", base::ifelse(test = base::length(x = categ)== 1L, yes = "categ", no = base::paste0("ELEMENT ", i1, " OF categ ARGUMENT", collapse = NULL, recycle0 = FALSE)), " IN ", base::ifelse(test = base::length(x = data1)== 1L, yes = "data1 ARGUMENT", no = base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT", collapse = NULL, recycle0 = FALSE)), "\nHERE IT IS COLOR LENGTH ", base::length(x = color[[i1]]), " VERSUS CATEG LEVELS LENGTH ", base::length(x = base::unique(x = data1[[i1]][, categ[[i1]]], incomparables = FALSE)), collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
            }
        }
    }
    # end conversion of geom_hline and geom_vline
    
    
    
    
    # kind of geom_point (vectorial or raster)
    scatter_kind <- base::vector(mode = "list", length = base::length(x = data1)) # list of same length as data1, that will be used to use either ggplot2::geom_point() (vectorial dot layer) or gg_point_rast() (raster dot layer)
    fix_ratio <- FALSE
    if(base::is.null(x = raster_threshold)){
        if(raster == TRUE){
            scatter_kind[] <- "gg_point_rast" # not important to fill everything: will be only used when geom == "geom_point"
            fix_ratio <- TRUE
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") RASTER PLOT GENERATED -> ASPECT RATIO OF THE PLOT REGION SET BY THE raster_ratio ARGUMENT (", saferTool::round2(data = raster_ratio, dec_nb = 2, after_lead_zero = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text), ") TO AVOID A BUG OF ELLIPSOID DOT DRAWING", collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
        }else{
            scatter_kind[] <- "ggplot2::geom_point"
        }
    }else{
        for(i2 in base::seq_along(along.with = data1)){
            if(geom[[i2]] == "geom_point"){
                if(base::nrow(x = data1[[i2]]) <= raster_threshold){
                    scatter_kind[[i2]] <- "ggplot2::geom_point"
                }else{
                    scatter_kind[[i2]] <- "gg_point_rast"
                    fix_ratio <- TRUE
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") ", base::ifelse(test = base::length(x = data1)== 1L, yes = "data1 ARGUMENT", no = base::paste0("DATA FRAME NUMBER ", i2, " OF data1 ARGUMENT", collapse = NULL, recycle0 = FALSE)), " LAYER AS RASTER (NOT VECTORIAL)", collapse = NULL, recycle0 = FALSE)
                    warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                }
            }
        }
        if(base::any(base::unlist(x = scatter_kind, recursive = TRUE, use.names = TRUE) == "gg_point_rast", na.rm = FALSE)){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") RASTER PLOT GENERATED -> ASPECT RATIO OF THE PLOT REGION SET BY THE raster_ratio ARGUMENT (", saferTool::round2(data = raster_ratio, dec_nb = 2, after_lead_zero = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text), ") TO AVOID A BUG OF ELLIPSOID DOT DRAWING", collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
        }
    }
    # end kind of geom_point (vectorial or raster)
    
    
    
    
    # add management
    # the add string is evaluated once here. Its non layer components (facets, scales, coords, labs, theme) are used as the first element of the plot (add_gg), while its layers (geom_hline(), annotate(), etc.) are removed from add_gg and added at the end of the final plot only (add_layers), because the names of the returned layer data (coord_names) are built in parallel with the internal layers
    add_gg <- base::eval(expr = base::parse(text = base::paste0("ggplot2::ggplot()", if(base::is.null(x = add)){""}else{add}, collapse = NULL, recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    add_layers <- add_gg$layers
    add_gg$layers <- base::list()
    # end add management
    # no need loop part
    coord_names <- NULL
    tempo_gg_name <- "gg.indiv.plot."
    tempo_gg_count <- 0
    base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = add_gg, pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # add added here to have the facets
    base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::xlab(label = if(base::is.null(x = x_lab)){x[[1]]}else{x_lab}), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
    base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::ylab(label = if(base::is.null(x = y_lab)){y[[1]]}else{y_lab}), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
    base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::ggtitle(label = title, subtitle = ggplot2::waiver()), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
    # text angle management
    x_tempo_just <- saferGG::gg_just(angle = x_text_angle, pos = "bottom", kind = "axis", safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
    y_tempo_just <- saferGG::gg_just(angle = y_text_angle, pos = "left", kind = "axis", safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
    # end text angle management
    add_check <- TRUE
    if( ! base::is.null(x = add)){ # if add is NULL, then = 0
        if(base::grepl(pattern = "ggplot2::theme", x = add, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) == TRUE){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") \"ggplot2::theme\" STRING DETECTED IN THE add ARGUMENT\n-> INTERNAL GGPLOT2 THEME FUNCTIONS theme() AND theme_classic() HAVE BEEN INACTIVATED, TO BE USED BY THE USER\n-> article ARGUMENT WILL BE IGNORED\nIT IS RECOMMENDED TO USE \"+ theme(aspect.ratio = raster_ratio)\" IF RASTER MODE IS ACTIVATED", collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            add_check <- FALSE
        }
    }
    if(add_check == TRUE & article == TRUE){
        # WARNING: not possible to add several times theme(). NO message but the last one overwrites the others
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::theme_classic(base_size = text_size, base_family = "", header_family = NULL, base_line_size = text_size / 22, base_rect_size = text_size / 22, ink = "black", paper = "white", accent = "#3366FF"), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
        if(grid == TRUE){
            base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = m_gg <- ggplot2::theme(
                text = ggplot2::element_text(size = text_size, family = NULL, face = NULL, colour = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL, color = NULL, margin = NULL, debug = NULL, inherit.blank = FALSE), 
                plot.title = ggplot2::element_text(size = title_text_size, family = NULL, face = NULL, colour = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL, color = NULL, margin = NULL, debug = NULL, inherit.blank = FALSE), # stronger than text
                legend.key = ggplot2::element_rect(color = "white", linewidth = 1.5, fill = NULL, colour = NULL, linetype = NULL, linejoin = NULL, inherit.blank = FALSE), # size of the frame of the legend
                line = ggplot2::element_line(linewidth = 0.5, colour = NULL, linetype = NULL, lineend = NULL, color = NULL, linejoin = NULL, arrow = NULL, arrow.fill = NULL, inherit.blank = FALSE), 
                axis.line.y.left = ggplot2::element_line(colour = "black", linewidth = NULL, linetype = NULL, lineend = NULL, color = NULL, linejoin = NULL, arrow = NULL, arrow.fill = NULL, inherit.blank = FALSE), # draw lines for the y axis
                axis.line.x.bottom = ggplot2::element_line(colour = "black", linewidth = NULL, linetype = NULL, lineend = NULL, color = NULL, linejoin = NULL, arrow = NULL, arrow.fill = NULL, inherit.blank = FALSE), # draw lines for the x axis
                panel.grid.major.x = ggplot2::element_line(colour = "grey85", linewidth = 0.75, linetype = NULL, lineend = NULL, color = NULL, linejoin = NULL, arrow = NULL, arrow.fill = NULL, inherit.blank = FALSE), 
                panel.grid.minor.x = ggplot2::element_line(colour = "grey90", linewidth = 0.25, linetype = NULL, lineend = NULL, color = NULL, linejoin = NULL, arrow = NULL, arrow.fill = NULL, inherit.blank = FALSE), 
                panel.grid.major.y = ggplot2::element_line(colour = "grey85", linewidth = 0.75, linetype = NULL, lineend = NULL, color = NULL, linejoin = NULL, arrow = NULL, arrow.fill = NULL, inherit.blank = FALSE), 
                panel.grid.minor.y = ggplot2::element_line(colour = "grey90", linewidth = 0.25, linetype = NULL, lineend = NULL, color = NULL, linejoin = NULL, arrow = NULL, arrow.fill = NULL, inherit.blank = FALSE), 
                axis.text.x = ggplot2::element_text(angle = x_tempo_just$angle, hjust = x_tempo_just$hjust, vjust = x_tempo_just$vjust, family = NULL, face = NULL, colour = NULL, size = NULL, lineheight = NULL, color = NULL, margin = NULL, debug = NULL, inherit.blank = FALSE),
                axis.text.y = ggplot2::element_text(angle = y_tempo_just$angle, hjust = y_tempo_just$hjust, vjust = y_tempo_just$vjust, family = NULL, face = NULL, colour = NULL, size = NULL, lineheight = NULL, color = NULL, margin = NULL, debug = NULL, inherit.blank = FALSE), 
                aspect.ratio = if(fix_ratio == TRUE){raster_ratio}else{NULL},  # for raster
                complete = FALSE, 
                validate = TRUE
            ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
        }else{
            base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = m_gg <- ggplot2::theme(
                text = ggplot2::element_text(size = text_size, family = NULL, face = NULL, colour = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL, color = NULL, margin = NULL, debug = NULL, inherit.blank = FALSE), 
                plot.title = ggplot2::element_text(size = title_text_size, family = NULL, face = NULL, colour = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL, color = NULL, margin = NULL, debug = NULL, inherit.blank = FALSE), # stronger than text
                line = ggplot2::element_line(linewidth = 0.5, colour = NULL, linetype = NULL, lineend = NULL, color = NULL, linejoin = NULL, arrow = NULL, arrow.fill = NULL, inherit.blank = FALSE), 
                legend.key = ggplot2::element_rect(color = "white", linewidth = 1.5, fill = NULL, colour = NULL, linetype = NULL, linejoin = NULL, inherit.blank = FALSE), # size of the frame of the legend
                axis.line.y.left = ggplot2::element_line(colour = "black", linewidth = NULL, linetype = NULL, lineend = NULL, color = NULL, linejoin = NULL, arrow = NULL, arrow.fill = NULL, inherit.blank = FALSE), 
                axis.line.x.bottom = ggplot2::element_line(colour = "black", linewidth = NULL, linetype = NULL, lineend = NULL, color = NULL, linejoin = NULL, arrow = NULL, arrow.fill = NULL, inherit.blank = FALSE), 
                axis.text.x = ggplot2::element_text(angle = x_tempo_just$angle, hjust = x_tempo_just$hjust, vjust = x_tempo_just$vjust, family = NULL, face = NULL, colour = NULL, size = NULL, lineheight = NULL, color = NULL, margin = NULL, debug = NULL, inherit.blank = FALSE),
                axis.text.y = ggplot2::element_text(angle = y_tempo_just$angle, hjust = y_tempo_just$hjust, vjust = y_tempo_just$vjust, family = NULL, face = NULL, colour = NULL, size = NULL, lineheight = NULL, color = NULL, margin = NULL, debug = NULL, inherit.blank = FALSE), 
                aspect.ratio = if(fix_ratio == TRUE){raster_ratio}else{NULL},  # for raster
                complete = FALSE, 
                validate = TRUE
            ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
        }
    }else if(add_check == TRUE & article == FALSE){
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = m_gg <- ggplot2::theme(
            text = ggplot2::element_text(size = text_size, family = NULL, face = NULL, colour = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL, color = NULL, margin = NULL, debug = NULL, inherit.blank = FALSE), 
            plot.title = ggplot2::element_text(size = title_text_size, family = NULL, face = NULL, colour = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL, color = NULL, margin = NULL, debug = NULL, inherit.blank = FALSE), # stronger than text
            line = ggplot2::element_line(linewidth = 0.5, colour = NULL, linetype = NULL, lineend = NULL, color = NULL, linejoin = NULL, arrow = NULL, arrow.fill = NULL, inherit.blank = FALSE), 
            legend.key = ggplot2::element_rect(color = "white", linewidth = 1.5, fill = NULL, colour = NULL, linetype = NULL, linejoin = NULL, inherit.blank = FALSE), # size of the frame of the legend
            panel.background = ggplot2::element_rect(fill = "grey95", colour = NULL, linewidth = NULL, linetype = NULL, color = NULL, linejoin = NULL, inherit.blank = FALSE), 
            axis.line.y.left = ggplot2::element_line(colour = "black", linewidth = NULL, linetype = NULL, lineend = NULL, color = NULL, linejoin = NULL, arrow = NULL, arrow.fill = NULL, inherit.blank = FALSE), 
            axis.line.x.bottom = ggplot2::element_line(colour = "black", linewidth = NULL, linetype = NULL, lineend = NULL, color = NULL, linejoin = NULL, arrow = NULL, arrow.fill = NULL, inherit.blank = FALSE), 
            panel.grid.major.x = ggplot2::element_line(colour = "grey85", linewidth = 0.75, linetype = NULL, lineend = NULL, color = NULL, linejoin = NULL, arrow = NULL, arrow.fill = NULL, inherit.blank = FALSE), 
            panel.grid.minor.x = ggplot2::element_line(colour = "grey90", linewidth = 0.25, linetype = NULL, lineend = NULL, color = NULL, linejoin = NULL, arrow = NULL, arrow.fill = NULL, inherit.blank = FALSE), 
            panel.grid.major.y = ggplot2::element_line(colour = "grey85", linewidth = 0.75, linetype = NULL, lineend = NULL, color = NULL, linejoin = NULL, arrow = NULL, arrow.fill = NULL, inherit.blank = FALSE), 
            panel.grid.minor.y = ggplot2::element_line(colour = "grey90", linewidth = 0.25, linetype = NULL, lineend = NULL, color = NULL, linejoin = NULL, arrow = NULL, arrow.fill = NULL, inherit.blank = FALSE), 
            strip.background = ggplot2::element_rect(fill = "white", colour = "black", linewidth = NULL, linetype = NULL, color = NULL, linejoin = NULL, inherit.blank = FALSE), 
            axis.text.x = ggplot2::element_text(angle = x_tempo_just$angle, hjust = x_tempo_just$hjust, vjust = x_tempo_just$vjust, family = NULL, face = NULL, colour = NULL, size = NULL, lineheight = NULL, color = NULL, margin = NULL, debug = NULL, inherit.blank = FALSE),
            axis.text.y = ggplot2::element_text(angle = y_tempo_just$angle, hjust = y_tempo_just$hjust, vjust = y_tempo_just$vjust, family = NULL, face = NULL, colour = NULL, size = NULL, lineheight = NULL, color = NULL, margin = NULL, debug = NULL, inherit.blank = FALSE), 
            aspect.ratio = if(fix_ratio == TRUE){raster_ratio}else{NULL},  # for raster
            complete = FALSE, 
            validate = TRUE
            # do not work -> legend.position = "none" # to remove the legend completely: https://www.datanovia.com/en/blog/how-to-remove-legend-from-a-ggplot/
        ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
    }
    # end no need loop part
    
    
    # loop part
    point_count <- 0
    line_count <- 0
    lg_order <- base::vector(mode = "list", length = 6) # order of the legend
    lg_order <- base::lapply(X = lg_order, FUN = base::as.numeric) # order of the legend
    lg_color <- base::vector(mode = "list", length = 6) # color of the legend
    lg_dot_shape <- base::vector(mode = "list", length = 6) # etc.
    lg_dot_size <- base::vector(mode = "list", length = 6) # etc.
    lg_dot_size <- base::lapply(X = lg_dot_size, FUN = base::as.numeric) # etc.
    lg_dot_border_size <- base::vector(mode = "list", length = 6) # etc.
    lg_dot_border_size <- base::lapply(X = lg_dot_border_size, FUN = base::as.numeric) # etc.
    lg_dot_border_color <- base::vector(mode = "list", length = 6) # etc.
    lg_line_size <- base::vector(mode = "list", length = 6) # etc.
    lg_line_size <- base::lapply(X = lg_line_size, FUN = base::as.numeric) # etc.
    lg_line_type <- base::vector(mode = "list", length = 6) # etc.
    lg_alpha <- base::vector(mode = "list", length = 6) # etc.
    lg_alpha <- base::lapply(X = lg_alpha, FUN = base::as.numeric) # etc.
    for(i1 in base::seq_along(along.with = data1)){
        if(geom[[i1]] == "geom_point"){
            point_count <- point_count + 1
            if(point_count== 1L){
                fin_lg_disp[[1]] <- legend_disp[[point_count + line_count]]
                lg_order[[1]] <- point_count + line_count
                lg_color[[1]] <- color[[i1]] # if color == NULL -> NULL
                lg_dot_shape[[1]] <- dot_shape[[i1]]
                lg_dot_size[[1]] <- dot_size[[i1]]
                lg_dot_border_size[[1]] <- dot_border_size[[i1]]
                lg_dot_border_color[[1]] <- dot_border_color[[i1]] # if dot_border_color == NULL -> NULL
                if(plot == TRUE & fin_lg_disp[[1]] == TRUE & dot_shape[[1]] %in% 0:14 & ((base::length(x = grDevices::dev.list()) > 0 & base::names(x = grDevices::dev.cur()) == "windows") | (base::length(x = grDevices::dev.list())== 0L & base::Sys.info()["sysname"] == "Windows"))){ # if any Graph device already open and this device is "windows", or if no Graph device opened yet and we are on windows system -> prevention of alpha legend bug on windows using value 1
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") GRAPHIC DEVICE USED ON A WINDOWS SYSTEM ->\nTRANSPARENCY OF THE DOTS (DOT LAYER NUMBER ", point_count, ") IS INACTIVATED IN THE LEGEND TO PREVENT A WINDOWS DEPENDENT BUG (SEE https://github.com/tidyverse/ggplot2/issues/2452)\nTO OVERCOME THIS ON WINDOWS, USE ANOTHER DEVICE (pdf() FOR INSTANCE)", collapse = NULL, recycle0 = FALSE)
                    warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                    lg_alpha[[1]] <- 1 # to avoid a bug on windows: if alpha argument is different from 1 for lines (transparency), then lines are not correctly displayed in the legend when using the R GUI (bug https://github.com/tidyverse/ggplot2/issues/2452). No bug when using a pdf
                }else{
                    lg_alpha[[1]] <- alpha[[i1]]
                }
                class_categ <- base::levels(x = base::factor(x = data1[[i1]][, categ[[i1]]], exclude = NA, ordered = base::is.ordered(x = data1[[i1]][, categ[[i1]]]), nmax = NA))
                for(i5 in base::seq_along(along.with = color[[i1]])){ # or base::length(class_categ). It is the same because already checked that lengths are the same
                    tempo_data_frame <- data1[[i1]][data1[[i1]][, categ[[i1]]] == class_categ[i5], ]
                    tempo_layer_args <- base::list(data = tempo_data_frame, mapping = ggplot2::aes(x = .data[[!!x[[i1]]]], y = .data[[!!y[[i1]]]], fill = .data[[!!categ[[i1]]]]), shape = dot_shape[[i1]], size = dot_size[[i1]], stroke = dot_border_size[[i1]], color = if(dot_shape[[i1]] %in% 21:24 & ! base::is.null(x = dot_border_color)){dot_border_color[[i1]]}else{color[[i1]][i5]}, alpha = alpha[[i1]]) ; tempo_layer_args[[base::ifelse(test = base::grepl(x = scatter_kind[[i1]], pattern = "gg_point_rast", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE), yes = "show_legend", no = "show.legend")]] <- if(i5== 1L){TRUE}else{FALSE} ; base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = base::do.call(what = base::ifelse(test = base::grepl(x = scatter_kind[[i1]], pattern = "gg_point_rast", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE), yes = "gg_point_rast", no = base::get(x = "geom_point", envir = base::asNamespace(ns = "ggplot2", base.OK = TRUE), pos = -1L, mode = "any", inherits = TRUE)), args = tempo_layer_args, quote = FALSE, envir = base::environment(fun = NULL)), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # WARNING: a single color allowed for color argument outside aesthetic, but here a single color for border --> loop could be inactivated but kept for commodity # legend_show option do not remove the legend, only the aesthetic of the legend (dot, line, etc.). Used here to avoid multiple layers of legend which corrupt transparency
                    coord_names <- base::c(coord_names, base::paste0(geom[[i1]], ".", class_categ[i5], collapse = NULL, recycle0 = FALSE))
                }
                base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_fill_manual(
                    name = if(base::is.null(x = legend_name)){
                        # nocov start (unreachable: legend_name is converted to a non-NULL list by the legend_name management block, so is.null(legend_name) is always FALSE when the scale is built)
                        NULL
                        # nocov end
                    }else{legend_name[[i1]]}, values = base::as.character(x = color[[i1]]), breaks = class_categ, aesthetics = "fill", na.value = "grey50"), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # values are the values of fill, breaks reorder the classes according to class_categ in the legend, order argument of guide_legend determines the order of the different aesthetics in the legend (not order of classes). See guide_legend settings of scale_..._manual below
            }
            if(point_count== 2L){
                fin_lg_disp[[2]] <- legend_disp[[point_count + line_count]]
                lg_order[[2]] <- point_count + line_count
                lg_color[[2]] <- color[[i1]] # if color == NULL -> NULL
                lg_dot_shape[[2]] <- dot_shape[[i1]]
                lg_dot_size[[2]] <- dot_size[[i1]]
                lg_dot_border_size[[2]] <- dot_border_size[[i1]]
                lg_dot_border_color[[2]] <- dot_border_color[[i1]] # if dot_border_color == NULL -> NULL
                if(plot == TRUE & fin_lg_disp[[2]] == TRUE & dot_shape[[2]] %in% 0:14 & ((base::length(x = grDevices::dev.list()) > 0 & base::names(x = grDevices::dev.cur()) == "windows") | (base::length(x = grDevices::dev.list())== 0L & base::Sys.info()["sysname"] == "Windows"))){ # if any Graph device already open and this device is "windows", or if no Graph device opened yet and we are on windows system -> prevention of alpha legend bug on windows using value 1
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") GRAPHIC DEVICE USED ON A WINDOWS SYSTEM ->\nTRANSPARENCY OF THE DOTS (DOT LAYER NUMBER ", point_count, ") IS INACTIVATED IN THE LEGEND TO PREVENT A WINDOWS DEPENDENT BUG (SEE https://github.com/tidyverse/ggplot2/issues/2452)\nTO OVERCOME THIS ON WINDOWS, USE ANOTHER DEVICE (pdf() FOR INSTANCE)", collapse = NULL, recycle0 = FALSE)
                    warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                    lg_alpha[[2]] <- 1 # to avoid a bug on windows: if alpha argument is different from 1 for lines (transparency), then lines are not correctly displayed in the legend when using the R GUI (bug https://github.com/tidyverse/ggplot2/issues/2452). No bug when using a pdf
                }else{
                    lg_alpha[[2]] <- alpha[[i1]]
                }
                class_categ <- base::levels(x = base::factor(x = data1[[i1]][, categ[[i1]]], exclude = NA, ordered = base::is.ordered(x = data1[[i1]][, categ[[i1]]]), nmax = NA))
                for(i5 in base::seq_along(along.with = color[[i1]])){ # or base::length(class_categ). It is the same because already checked that lengths are the same
                    tempo_data_frame <- data1[[i1]][data1[[i1]][, categ[[i1]]] == class_categ[i5], ]
                    tempo_layer_args <- base::list(data = tempo_data_frame, mapping = ggplot2::aes(x = .data[[!!x[[i1]]]], y = .data[[!!y[[i1]]]], shape = .data[[!!categ[[i1]]]]), size = dot_size[[i1]], stroke = dot_border_size[[i1]], fill = color[[i1]][i5], color = if(dot_shape[[i1]] %in% 21:24 & ! base::is.null(x = dot_border_color)){dot_border_color[[i1]]}else{color[[i1]][i5]}, alpha = alpha[[i1]]) ; tempo_layer_args[[base::ifelse(test = base::grepl(x = scatter_kind[[i1]], pattern = "gg_point_rast", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE), yes = "show_legend", no = "show.legend")]] <- FALSE ; base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = base::do.call(what = base::ifelse(test = base::grepl(x = scatter_kind[[i1]], pattern = "gg_point_rast", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE), yes = "gg_point_rast", no = base::get(x = "geom_point", envir = base::asNamespace(ns = "ggplot2", base.OK = TRUE), pos = -1L, mode = "any", inherits = TRUE)), args = tempo_layer_args, quote = FALSE, envir = base::environment(fun = NULL)), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # WARNING: a single color allowed for fill argument outside aesthetic, hence the loop # legend_show option do not remove the legend, only the aesthetic of the legend (dot, line, etc.). Used here to avoid multiple layers of legend which corrupt transparency
                    coord_names <- base::c(coord_names, base::paste0(geom[[i1]], ".", class_categ[i5], collapse = NULL, recycle0 = FALSE))
                }
                base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_shape_manual(
                    name = if(base::is.null(x = legend_name)){
                        # nocov start (unreachable: idem as scale_fill_manual)
                        NULL
                        # nocov end
                    }else{legend_name[[i1]]}, values = base::rep(x = dot_shape[[i1]], times = base::length(x = color[[i1]])), breaks = class_categ, na.value = NA, aesthetics = "shape"), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # values are the values of shape, breaks reorder the classes according to class_categ in the legend. See guide_legend settings of scale_..._manual below
                
            }
            if(point_count== 3L){
                fin_lg_disp[[3]] <- legend_disp[[point_count + line_count]]
                lg_order[[3]] <- point_count + line_count
                lg_color[[3]] <- color[[i1]] # if color == NULL -> NULL
                lg_dot_shape[[3]] <- dot_shape[[i1]]
                lg_dot_size[[3]] <- dot_size[[i1]]
                lg_dot_border_size[[3]] <- dot_border_size[[i1]]
                lg_dot_border_color[[3]] <- dot_border_color[[i1]] # if dot_border_color == NULL -> NULL
                if(plot == TRUE & fin_lg_disp[[3]] == TRUE & dot_shape[[3]] %in% 0:14 & ((base::length(x = grDevices::dev.list()) > 0 & base::names(x = grDevices::dev.cur()) == "windows") | (base::length(x = grDevices::dev.list())== 0L & base::Sys.info()["sysname"] == "Windows"))){ # if any Graph device already open and this device is "windows", or if no Graph device opened yet and we are on windows system -> prevention of alpha legend bug on windows using value 1
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") GRAPHIC DEVICE USED ON A WINDOWS SYSTEM ->\nTRANSPARENCY OF THE DOTS (DOT LAYER NUMBER ", point_count, ") IS INACTIVATED IN THE LEGEND TO PREVENT A WINDOWS DEPENDENT BUG (SEE https://github.com/tidyverse/ggplot2/issues/2452)\nTO OVERCOME THIS ON WINDOWS, USE ANOTHER DEVICE (pdf() FOR INSTANCE)", collapse = NULL, recycle0 = FALSE)
                    warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                    lg_alpha[[3]] <- 1 # to avoid a bug on windows: if alpha argument is different from 1 for lines (transparency), then lines are not correctly displayed in the legend when using the R GUI (bug https://github.com/tidyverse/ggplot2/issues/2452). No bug when using a pdf
                }else{
                    lg_alpha[[3]] <- alpha[[i1]]
                }
                class_categ <- base::levels(x = base::factor(x = data1[[i1]][, categ[[i1]]], exclude = NA, ordered = base::is.ordered(x = data1[[i1]][, categ[[i1]]]), nmax = NA))
                for(i5 in base::seq_along(along.with = color[[i1]])){ # or base::length(class_categ). It is the same because already checked that lengths are the same
                    tempo_data_frame <- data1[[i1]][data1[[i1]][, categ[[i1]]] == class_categ[i5], ]
                    tempo_layer_args <- base::list(data = tempo_data_frame, mapping = ggplot2::aes(x = .data[[!!x[[i1]]]], y = .data[[!!y[[i1]]]], stroke = .data[[!!categ[[i1]]]]), shape = dot_shape[[i1]], size = dot_size[[i1]], fill = color[[i1]][i5], stroke = dot_border_size[[i1]], color = if(dot_shape[[i1]] %in% 21:24 & ! base::is.null(x = dot_border_color)){dot_border_color[[i1]]}else{color[[i1]][i5]}, alpha = alpha[[i1]]) ; tempo_layer_args[[base::ifelse(test = base::grepl(x = scatter_kind[[i1]], pattern = "gg_point_rast", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE), yes = "show_legend", no = "show.legend")]] <- FALSE ; base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = base::do.call(what = base::ifelse(test = base::grepl(x = scatter_kind[[i1]], pattern = "gg_point_rast", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE), yes = "gg_point_rast", no = base::get(x = "geom_point", envir = base::asNamespace(ns = "ggplot2", base.OK = TRUE), pos = -1L, mode = "any", inherits = TRUE)), args = tempo_layer_args, quote = FALSE, envir = base::environment(fun = NULL)), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # WARNING: a single color allowed for color argument outside aesthetic, hence the loop # legend_show option do not remove the legend, only the aesthetic of the legend (dot, line, etc.). Used here to avoid multiple layers of legend which corrupt transparency
                    coord_names <- base::c(coord_names, base::paste0(geom[[i1]], ".", class_categ[i5], collapse = NULL, recycle0 = FALSE))
                }
                base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_discrete_manual(aesthetics = "stroke",
                    name = if(base::is.null(x = legend_name)){
                        # nocov start (unreachable: idem as scale_fill_manual)
                        NULL
                        # nocov end
                    }else{legend_name[[i1]]}, values = base::rep(x = dot_border_size[[i1]], times = base::length(x = color[[i1]])), breaks = class_categ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # values are the values of stroke, breaks reorder the classes according to class_categ in the legend. See guide_legend settings of scale_..._manual below
                
            }
        }else{
            line_count <- line_count + 1
            if(line_count== 1L){
                fin_lg_disp[[4]] <- legend_disp[[point_count + line_count]]
                lg_order[[4]] <- point_count + line_count
                lg_color[[4]] <- color[[i1]] # if color == NULL -> NULL
                lg_line_size[[4]] <- line_size[[i1]]
                lg_line_type[[4]] <- line_type[[i1]]
                if(plot == TRUE & fin_lg_disp[[4]] == TRUE & ((base::length(x = grDevices::dev.list()) > 0 & base::names(x = grDevices::dev.cur()) == "windows") | (base::length(x = grDevices::dev.list())== 0L & base::Sys.info()["sysname"] == "Windows"))){ # if any Graph device already open and this device is "windows", or if no Graph device opened yet and we are on windows system -> prevention of alpha legend bug on windows using value 1
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") GRAPHIC DEVICE USED ON A WINDOWS SYSTEM ->\nTRANSPARENCY OF THE LINES (LINE LAYER NUMBER ", line_count, ") IS INACTIVATED IN THE LEGEND TO PREVENT A WINDOWS DEPENDENT BUG (SEE https://github.com/tidyverse/ggplot2/issues/2452)\nTO OVERCOME THIS ON WINDOWS, USE ANOTHER DEVICE (pdf() FOR INSTANCE)", collapse = NULL, recycle0 = FALSE)
                    warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                    lg_alpha[[4]] <- 1 # to avoid a bug on windows: if alpha argument is different from 1 for lines (transparency), then lines are not correctly displayed in the legend when using the R GUI (bug https://github.com/tidyverse/ggplot2/issues/2452). No bug when using a pdf
                }else{
                    lg_alpha[[4]] <- alpha[[i1]]
                }
                class_categ <- base::levels(x = base::factor(x = data1[[i1]][, categ[[i1]]], exclude = NA, ordered = base::is.ordered(x = data1[[i1]][, categ[[i1]]]), nmax = NA))
                for(i5 in base::seq_along(along.with = color[[i1]])){ # or base::length(class_categ). It is the same because already checked that lengths are the same
                    tempo_data_frame <- data1[[i1]][data1[[i1]][, categ[[i1]]] == class_categ[i5], ]
                    base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = base::eval(expr = base::parse(text = base::paste0("ggplot2::", # no CR here te0("ggpl
                                                                                                                 base::ifelse(test = geom[[i1]] == 'geom_stick', yes = 'geom_segment', no = geom[[i1]]), # geom_segment because geom_stick converted to geom_segment for plotting
                                                                                                                 "(data = tempo_data_frame, mapping = ggplot2::aes(x = ", 
                                                                                                                 x[[i1]], 
                                                                                                                 base::ifelse(test = geom[[i1]] == 'geom_stick', yes = ", yend = ", no = ", y = "), 
                                                                                                                 y[[i1]], 
                                                                                                                 if(geom[[i1]] == 'geom_stick'){base::paste0(', xend = ', x[[i1]], ', y = ', if(base::is.null(x = geom_stick_base) || base::is.null(x = geom_stick_base[[i1]])){y_lim[1]}else{geom_stick_base[[i1]]}, collapse = NULL, recycle0 = FALSE)}, # ifelse() would crash with "replacement has length zero" when geom_stick_base is a list with a NULL compartment
                                                                                                                 ", linetype = ", 
                                                                                                                 categ[[i1]], 
                                                                                                                 "), color = \"", 
                                                                                                                 color[[i1]][i5], 
                                                                                                                 "\", linewidth = ", 
                                                                                                                 line_size[[i1]], 
                                                                                                                 base::ifelse(test = geom[[i1]] == 'geom_path', yes = ', lineend = \"round\"', no = ''), 
                                                                                                                 base::ifelse(test = geom[[i1]] == 'geom_step', yes = base::paste0(', direction = \"', geom_step_dir[[i1]], '\"', collapse = NULL, recycle0 = FALSE), no = ''), 
                                                                                                                 ", alpha = ", 
                                                                                                                 alpha[[i1]], 
                                                                                                                 ", show.legend = ", 
                                                                                                                 base::ifelse(test = i5== 1L, yes = TRUE, no = FALSE), 
                                                                                                                 ")", collapse = NULL, recycle0 = FALSE
                    ), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # WARNING: a single color allowed for color argument outside aesthetic, hence the loop # legend_show option do not remove the legend, only the aesthetic of the legend (dot, line, etc.). Used here to avoid multiple layers of legend which corrupt transparency
                    coord_names <- base::c(coord_names, base::paste0(geom[[i1]], ".", class_categ[i5], collapse = NULL, recycle0 = FALSE))
                }
                base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_discrete_manual(aesthetics = "linetype",
                    name = if(base::is.null(x = legend_name)){
                        # nocov start (unreachable: idem as scale_fill_manual)
                        NULL
                        # nocov end
                    }else{legend_name[[i1]]}, values = base::rep(x = line_type[[i1]], times = base::length(x = color[[i1]])), breaks = class_categ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # values are the values of linetype. 1 means solid. Regarding the alpha bug, I have tried different things without success: alpha in guide alone, in geom alone, in both, with different values, breaks reorder the classes according to class_categ in the legend
            }
            if(line_count== 2L){
                fin_lg_disp[[5]] <- legend_disp[[point_count + line_count]]
                lg_order[[5]] <- point_count + line_count
                lg_color[[5]] <- color[[i1]] # if color == NULL -> NULL
                lg_line_size[[5]] <- line_size[[i1]]
                lg_line_type[[5]] <- line_type[[i1]]
                if(plot == TRUE & fin_lg_disp[[5]] == TRUE & ((base::length(x = grDevices::dev.list()) > 0 & base::names(x = grDevices::dev.cur()) == "windows") | (base::length(x = grDevices::dev.list())== 0L & base::Sys.info()["sysname"] == "Windows"))){ # if any Graph device already open and this device is "windows", or if no Graph device opened yet and we are on windows system -> prevention of alpha legend bug on windows using value 1
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") GRAPHIC DEVICE USED ON A WINDOWS SYSTEM ->\nTRANSPARENCY OF THE LINES (LINE LAYER NUMBER ", line_count, ") IS INACTIVATED IN THE LEGEND TO PREVENT A WINDOWS DEPENDENT BUG (SEE https://github.com/tidyverse/ggplot2/issues/2452)\nTO OVERCOME THIS ON WINDOWS, USE ANOTHER DEVICE (pdf() FOR INSTANCE)", collapse = NULL, recycle0 = FALSE)
                    warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                    lg_alpha[[5]] <- 1 # to avoid a bug on windows: if alpha argument is different from 1 for lines (transparency), then lines are not correctly displayed in the legend when using the R GUI (bug https://github.com/tidyverse/ggplot2/issues/2452). No bug when using a pdf
                }else{
                    lg_alpha[[5]] <- alpha[[i1]]
                }
                class_categ <- base::levels(x = base::factor(x = data1[[i1]][, categ[[i1]]], exclude = NA, ordered = base::is.ordered(x = data1[[i1]][, categ[[i1]]]), nmax = NA))
                for(i5 in base::seq_along(along.with = color[[i1]])){ # or base::length(class_categ). It is the same because already checked that lengths are the same
                    tempo_data_frame <- data1[[i1]][data1[[i1]][, categ[[i1]]] == class_categ[i5], ]
                    base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = base::eval(expr = base::parse(text = base::paste0("ggplot2::", # no CR here te0("ggpl
                                                                                                                 base::ifelse(test = geom[[i1]] == 'geom_stick', yes = 'geom_segment', no = geom[[i1]]), # geom_segment because geom_stick converted to geom_segment for plotting
                                                                                                                 "(data = tempo_data_frame, mapping = ggplot2::aes(x = ", 
                                                                                                                 x[[i1]], 
                                                                                                                 base::ifelse(test = geom[[i1]] == 'geom_stick', yes = ", yend = ", no = ", y = "), 
                                                                                                                 y[[i1]], 
                                                                                                                 if(geom[[i1]] == 'geom_stick'){base::paste0(', xend = ', x[[i1]], ', y = ', if(base::is.null(x = geom_stick_base) || base::is.null(x = geom_stick_base[[i1]])){y_lim[1]}else{geom_stick_base[[i1]]}, collapse = NULL, recycle0 = FALSE)}, # ifelse() would crash with "replacement has length zero" when geom_stick_base is a list with a NULL compartment
                                                                                                                 ", alpha = ", 
                                                                                                                 categ[[i1]], 
                                                                                                                 "), color = \"", 
                                                                                                                 color[[i1]][i5], 
                                                                                                                 "\", linewidth = ", 
                                                                                                                 line_size[[i1]], 
                                                                                                                 ", linetype = ", 
                                                                                                                 base::ifelse(test = base::is.numeric(x = line_type[[i1]]), yes = "", no = "\""), 
                                                                                                                 line_type[[i1]], 
                                                                                                                 base::ifelse(test = base::is.numeric(x = line_type[[i1]]), yes = "", no = "\""), 
                                                                                                                 base::ifelse(test = geom[[i1]] == 'geom_path', yes = ', lineend = \"round\"', no = ''), 
                                                                                                                 base::ifelse(test = geom[[i1]] == 'geom_step', yes = base::paste0(', direction = \"', geom_step_dir[[i1]], '\"', collapse = NULL, recycle0 = FALSE), no = ''), 
                                                                                                                 ", show.legend = FALSE)", collapse = NULL, recycle0 = FALSE
                    ), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # WARNING: a single color allowed for color argument outside aesthetic, hence the loop # legend_show option do not remove the legend, only the aesthetic of the legend (dot, line, etc.). Used here to avoid multiple layers of legend which corrupt transparency
                    coord_names <- base::c(coord_names, base::paste0(geom[[i1]], ".", class_categ[i5], collapse = NULL, recycle0 = FALSE))
                }
                base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_discrete_manual(aesthetics = "alpha",
                    name = if(base::is.null(x = legend_name)){
                        # nocov start (unreachable: idem as scale_fill_manual)
                        NULL
                        # nocov end
                    }else{legend_name[[i1]]}, values = base::rep(x = alpha[[i1]], times = base::length(x = color[[i1]])), breaks = class_categ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # values are the values of linetype. 1 means solid. Regarding the alpha bug, I have tried different things without success: alpha in guide alone, in geom alone, in both, with different values, breaks reorder the classes according to class_categ in the legend
            }
            if(line_count== 3L){
                fin_lg_disp[[6]] <- legend_disp[[point_count + line_count]]
                lg_order[[6]] <- point_count + line_count
                lg_color[[6]] <- color[[i1]] # if color == NULL -> NULL
                lg_line_size[[6]] <- line_size[[i1]]
                lg_line_type[[6]] <- line_type[[i1]]
                if(plot == TRUE & fin_lg_disp[[6]] == TRUE & ((base::length(x = grDevices::dev.list()) > 0 & base::names(x = grDevices::dev.cur()) == "windows") | (base::length(x = grDevices::dev.list())== 0L & base::Sys.info()["sysname"] == "Windows"))){ # if any Graph device already open and this device is "windows", or if no Graph device opened yet and we are on windows system -> prevention of alpha legend bug on windows using value 1
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") GRAPHIC DEVICE USED ON A WINDOWS SYSTEM ->\nTRANSPARENCY OF THE LINES (LINE LAYER NUMBER ", line_count, ") IS INACTIVATED IN THE LEGEND TO PREVENT A WINDOWS DEPENDENT BUG (SEE https://github.com/tidyverse/ggplot2/issues/2452)\nTO OVERCOME THIS ON WINDOWS, USE ANOTHER DEVICE (pdf() FOR INSTANCE)", collapse = NULL, recycle0 = FALSE)
                    warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                    lg_alpha[[6]] <- 1 # to avoid a bug on windows: if alpha argument is different from 1 for lines (transparency), then lines are not correctly displayed in the legend when using the R GUI (bug https://github.com/tidyverse/ggplot2/issues/2452). No bug when using a pdf
                }else{
                    lg_alpha[[6]] <- alpha[[i1]]
                }
                class_categ <- base::levels(x = base::factor(x = data1[[i1]][, categ[[i1]]], exclude = NA, ordered = base::is.ordered(x = data1[[i1]][, categ[[i1]]]), nmax = NA))
                for(i5 in base::seq_along(along.with = color[[i1]])){ # or base::length(class_categ). It is the same because already checked that lengths are the same
                    tempo_data_frame <- data1[[i1]][data1[[i1]][, categ[[i1]]] == class_categ[i5], ]
                    base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = base::eval(expr = base::parse(text = base::paste0("ggplot2::", # no CR here te0("ggpl
                                                                                                                 base::ifelse(test = geom[[i1]] == 'geom_stick', yes = 'geom_segment', no = geom[[i1]]), # geom_segment because geom_stick converted to geom_segment for plotting
                                                                                                                 "(data = tempo_data_frame, mapping = ggplot2::aes(x = ", 
                                                                                                                 x[[i1]], 
                                                                                                                 base::ifelse(test = geom[[i1]] == 'geom_stick', yes = ", yend = ", no = ", y = "), 
                                                                                                                 y[[i1]], 
                                                                                                                 if(geom[[i1]] == 'geom_stick'){base::paste0(', xend = ', x[[i1]], ', y = ', if(base::is.null(x = geom_stick_base) || base::is.null(x = geom_stick_base[[i1]])){y_lim[1]}else{geom_stick_base[[i1]]}, collapse = NULL, recycle0 = FALSE)}, # ifelse() would crash with "replacement has length zero" when geom_stick_base is a list with a NULL compartment
                                                                                                                 ", linewidth = ", 
                                                                                                                 categ[[i1]], 
                                                                                                                 "), color = \"", 
                                                                                                                 color[[i1]][i5], 
                                                                                                                 "\", linetype = ", 
                                                                                                                 base::ifelse(test = base::is.numeric(x = line_type[[i1]]), yes = "", no = "\""), 
                                                                                                                 line_type[[i1]], 
                                                                                                                 base::ifelse(test = base::is.numeric(x = line_type[[i1]]), yes = "", no = "\""), 
                                                                                                                 base::ifelse(test = geom[[i1]] == 'geom_path', yes = ', lineend = \"round\"', no = ''), 
                                                                                                                 base::ifelse(test = geom[[i1]] == 'geom_step', yes = base::paste0(', direction = \"', geom_step_dir[[i1]], '\"', collapse = NULL, recycle0 = FALSE), no = ''), 
                                                                                                                 ", alpha = ", 
                                                                                                                 alpha[[i1]], 
                                                                                                                 ", show.legend = FALSE)", collapse = NULL, recycle0 = FALSE
                    ), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # WARNING: a single color allowed for color argument outside aesthetic, hence the loop # legend_show option do not remove the legend, only the aesthetic of the legend (dot, line, etc.). Used here to avoid multiple layers of legend which corrupt transparency
                    coord_names <- base::c(coord_names, base::paste0(geom[[i1]], ".", class_categ[i5], collapse = NULL, recycle0 = FALSE))
                }
                base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_discrete_manual(aesthetics = "linewidth",
                    name = if(base::is.null(x = legend_name)){
                        # nocov start (unreachable: idem as scale_fill_manual)
                        NULL
                        # nocov end
                    }else{legend_name[[i1]]}, values = base::rep(x = line_size[[i1]], times = base::length(x = color[[i1]])), breaks = class_categ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # values are the values of linetype. 1 means solid. Regarding the alpha bug, I have tried different things without success: alpha in guide alone, in geom alone, in both, breaks reorder the classes according to class_categ in the legend
            }
        }
    }
    # end loop part
    
    
    
    
    # legend display
    tempo_legend_final <- 'ggplot2::guides(
fill = if(fin_lg_disp[[1]] == TRUE){
ggplot2::guide_legend(
order = lg_order[[1]], 
override.aes = base::list(
fill = lg_color[[1]], 
colour = if(lg_dot_shape[[1]] %in% 21:24 & ! base::is.null(dot_border_color)){lg_dot_border_color[[1]]}else{lg_color[[1]]}, # lg_dot_shape[[1]] %in% 21:24 are the only one that can be filled
shape = lg_dot_shape[[1]], 
size = lg_dot_size[[1]], 
stroke = lg_dot_border_size[[1]], 
alpha = lg_alpha[[1]], 
linetype = 0
)
)
}else{
"none"
}, 
shape = if(fin_lg_disp[[2]] == TRUE){
ggplot2::guide_legend(
order = lg_order[[2]], 
override.aes = base::list(
fill = lg_color[[2]], 
colour = if(lg_dot_shape[[2]] %in% 21:24 & ! base::is.null(dot_border_color)){lg_dot_border_color[[2]]}else{lg_color[[2]]}, # lg_dot_shape[[2]] %in% 21:24 are the only one that can be filled
shape = lg_dot_shape[[2]], 
size = lg_dot_size[[2]], 
stroke = lg_dot_border_size[[2]], 
alpha = lg_alpha[[2]], 
linetype = 0
)
)
}else{
"none"
}, 
stroke = if(fin_lg_disp[[3]] == TRUE){
ggplot2::guide_legend(
order = lg_order[[3]], 
override.aes = base::list(
fill = lg_color[[3]], 
colour = if(lg_dot_shape[[3]] %in% 21:24 & ! base::is.null(dot_border_color)){lg_dot_border_color[[3]]}else{lg_color[[3]]}, # lg_dot_shape[[3]] %in% 21:24 are the only one that can be filled
shape = lg_dot_shape[[3]], 
size = lg_dot_size[[3]], 
stroke = lg_dot_border_size[[3]], 
alpha = lg_alpha[[3]], 
linetype = 0
)
)
}else{
"none"
}, 
linetype = if(fin_lg_disp[[4]] == TRUE){
ggplot2::guide_legend(
order = lg_order[[4]], 
override.aes = base::list(
color = lg_color[[4]], 
linewidth = lg_line_size[[4]], 
linetype = lg_line_type[[4]], 
alpha = lg_alpha[[4]], 
shape = NA
)
)
}else{
"none"
}, 
alpha = if(fin_lg_disp[[5]] == TRUE){
ggplot2::guide_legend(
order = lg_order[[5]], 
override.aes = base::list(
color = lg_color[[5]], 
linewidth = lg_line_size[[5]], 
linetype = lg_line_type[[5]], 
alpha = lg_alpha[[5]], 
shape = NA
)
)
}else{
"none"
}, 
linewidth = if(fin_lg_disp[[6]] == TRUE){
ggplot2::guide_legend(
order = lg_order[[6]], 
override.aes = base::list(
color = lg_color[[6]], 
linewidth = lg_line_size[[6]], 
linetype = lg_line_type[[6]], 
alpha = lg_alpha[[6]], 
shape = NA
)
)
}else{
"none"
}
)' # clip = "off" to have secondary ticks outside plot region does not work
    if( ! base::is.null(x = legend_width)){
        if(base::any(base::unlist(x = legend_disp, recursive = TRUE, use.names = TRUE), na.rm = FALSE)){ # means some TRUE
            tempo_graph_info <- base::suppressMessages(expr = ggplot2::ggplot_build(plot = base::eval(expr = base::parse(text = base::paste0(base::paste(base::paste0(tempo_gg_name, 1:tempo_gg_count, collapse = NULL, recycle0 = FALSE), collapse = " + ", sep = " ", recycle0 = FALSE), ' + ', tempo_legend_final, collapse = NULL, recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))), classes = "message") # will be recovered later again, when ylim will be considered
            legend_final <- saferGG::gg_get_legend(ggplot_built = tempo_graph_info, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) # get legend
            fin_lg_disp[] <- FALSE # remove all the legends. Must be done even if fin_lg_disp is not appearing in the code thenafter. Otherwise twice the legend
            if(base::is.null(x = legend_final) & plot == TRUE){ # even if base::any(base::unlist(legend_disp)) is TRUE
            # nocov start (unreachable: with any(legend_disp) TRUE, the guides create a legend, so gg_get_legend() never returns NULL)
                legend_final <- saferGG::gg_empty(text = " ", lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text, text_size = 12, title = NULL, title_size = 8) # empty graph instead of legend
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") LEGEND REQUESTED (NON-NULL categ ARGUMENT OR legend_show ARGUMENT SET TO TRUE)\nBUT IT SEEMS THAT THE PLOT HAS NO LEGEND -> EMPTY LEGEND SPACE CREATED BECAUSE OF THE NON-NULL legend_width ARGUMENT\n", collapse = NULL, recycle0 = FALSE)
                warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            # nocov end
            }
        }else if(plot == TRUE){ # means all FALSE
            legend_final <- ggplot2::ggplot(data = NULL, mapping = ggplot2::aes(), environment = base::environment(fun = NULL))+ggplot2::theme_void(base_size = 11, base_family = "", header_family = NULL, base_line_size = 11 / 22, base_rect_size = 11 / 22, ink = "black", paper = scales::alpha(colour = "black", alpha = 0), accent = "#3366FF") # empty graph instead of legend
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") LEGEND REQUESTED (NON-NULL categ ARGUMENT OR legend_show ARGUMENT SET TO TRUE)\nBUT IT SEEMS THAT THE PLOT HAS NO LEGEND -> EMPTY LEGEND SPACE CREATED BECAUSE OF THE NON-NULL legend_width ARGUMENT\n", collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
        }
    }
    if( ! base::any(base::unlist(x = legend_disp, recursive = TRUE, use.names = TRUE), na.rm = FALSE)){
        fin_lg_disp[] <- FALSE # remove all the legends. Must be done even if fin_lg_disp is not appearing in the code thenafter. Otherwise twice the legend
    }
    base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = base::eval(expr = base::parse(text = tempo_legend_final, file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
    # end legend display





    # scale management
    tempo_coord <- base::suppressMessages(expr = ggplot2::ggplot_build(plot = base::eval(expr = base::parse(text = base::paste(base::paste0(tempo_gg_name, 1:tempo_gg_count, collapse = NULL, recycle0 = FALSE), collapse = " + ", ' + ggplot2::scale_x_continuous(expand = base::c(0, 0), limits = base::sort(x = x_lim, decreasing = FALSE), oob = scales::rescale_none) + ggplot2::scale_y_continuous(expand = base::c(0, 0), limits = base::sort(x = y_lim, decreasing = FALSE), oob = scales::rescale_none)', sep = " ", recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)))$layout$panel_params[[1]], classes = "message") # here I do not need the x-axis and y-axis orientation, I just need the number of main ticks
    # x_second_tick_positions # coordinates of secondary ticks (only if x_second_tick_nb argument is non-null or if x_log argument is different from "no")
    if(x_log != "no"){ # integer main ticks for log2 and log10
        tempo_scale <- (base::as.integer(x = base::min(x_lim, na.rm = TRUE)) - 1):(base::as.integer(x = base::max(x_lim, na.rm = TRUE)) + 1)
    }else{
        tempo <- if(base::is.null(x = base::attributes(x = tempo_coord$x$breaks))){tempo_coord$x$breaks}else{
            # nocov start (unreachable: continuous scales have no break attributes in this ggplot2 version, probed empirically)
            base::unlist(x = base::attributes(x = tempo_coord$x$breaks), recursive = TRUE, use.names = TRUE)
            # nocov end
        }
        if(base::all(base::is.na(x = tempo), na.rm = FALSE)){
            # nocov start (unreachable: internal consistency guard - ggplot2 always returns at least one non-NA break for finite x_lim)
            tempo_cat <- base::paste0(
                "INTERNAL ERROR 1 IN ", 
                intern_error_text_start, 
                "ONLY NA IN tempo_coord$x$breaks.", 
                intern_error_text_end, 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }
        if(base::length(x = base::unique(x = x_lim, incomparables = FALSE)) <= 1){
            tempo_cat <- base::paste0(error_text_start, "IT SEEMS THAT X-AXIS VALUES HAVE A NULL RANGE: ", base::paste(x_lim, collapse = " ", sep = " ", recycle0 = FALSE), "\nPLEASE, USE THE x_lim ARGUMENT WITH 2 DIFFERENT VALUES TO SOLVE THIS", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            tempo_scale <- saferGraph::scale2(lim = x_lim, n = base::ifelse(test = base::is.null(x = x_tick_nb), yes = base::length(x = tempo[ ! base::is.na(x = tempo)]), no = x_tick_nb), kind = "approx", safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) # in ggplot 3.3.0, tempo_coord$x.major_source replaced by tempo_coord$x$breaks. If fact: n = base::ifelse(base::is.null(x_tick_nb), base::length(tempo[ ! base::is.na(tempo)]), x_tick_nb)) replaced by n = base::ifelse(base::is.null(x_tick_nb), 4, x_tick_nb))
        }
    }
    x_second_tick_values <- NULL
    x_second_tick_pos <- NULL
    if(x_log != "no"){
        tempo <- saferGraph::inter_ticks(lim = x_lim, log = x_log, warn_print = FALSE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text, breaks = NULL, n = NULL)
        x_second_tick_values <- tempo$values
        x_second_tick_pos <- tempo$coordinates
        # if(vertical == TRUE){ # do not remove in case the bug is fixed
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::annotate(
            geom = "segment", 
            x = x_second_tick_pos, 
            xend = x_second_tick_pos, 
            y = if(base::diff(x = y_lim) > 0){tempo_coord$y.range[1]}else{tempo_coord$y.range[2]}, 
            yend = if(base::diff(x = y_lim) > 0){tempo_coord$y.range[1] + base::abs(x = base::diff(x = tempo_coord$y.range)) / 80}else{tempo_coord$y.range[2] - base::abs(x = base::diff(x = tempo_coord$y.range)) / 80}, 
            xmin = NULL, 
            xmax = NULL, 
            ymin = NULL, 
            ymax = NULL, 
            na.rm = FALSE
        ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
        # }else{ # not working because of the ggplot2 bug
        # base::assign(base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), ggplot2::annotate(geom = "segment", y = x_second_tick_pos, yend = x_second_tick_pos, x = tempo_coord$x.range[1], xend = tempo_coord$x.range[1] + diff(tempo_coord$x.range) / 80))
        # }
        coord_names <- base::c(coord_names, "x_second_tick_positions")
    }else if(( ! base::is.null(x = x_second_tick_nb)) & x_log == "no"){
        # if(x_second_tick_nb > 0){ #inactivated because already checked before
        if(base::length(x = tempo_scale) < 2){
            tempo_cat1 <- base::c("x_tick_nb", "x_second_tick_nb")
            tempo_cat2 <- base::sapply(X = base::list(x_tick_nb, x_second_tick_nb), FUN = base::paste0, collapse = " ", recycle0 = FALSE, simplify = TRUE, USE.NAMES = TRUE)
            tempo_sep <- base::sapply(X = base::mapply(x = " ", times = base::max(base::nchar(x = tempo_cat1, type = "chars", allowNA = FALSE, keepNA = NA), na.rm = FALSE) - base::nchar(x = tempo_cat1, type = "chars", allowNA = FALSE, keepNA = NA) + 3, FUN = base::rep, SIMPLIFY = FALSE, MoreArgs = NULL, USE.NAMES = TRUE), FUN = base::paste0, collapse = "", recycle0 = FALSE, simplify = TRUE, USE.NAMES = TRUE)
            tempo_cat <- base::paste0(error_text_start, "THE NUMBER OF GENERATED TICKS FOR THE X-AXIS IS NOT CORRECT: ", base::length(x = tempo_scale), "\nUSING THESE ARGUMENT SETTINGS (NO DISPLAY MEANS NULL VALUE):\n", base::paste0(tempo_cat1, tempo_sep, tempo_cat2, collapse = "\n", recycle0 = FALSE), "\nPLEASE, TEST OTHER VALUES", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        }else{
            tempo <- saferGraph::inter_ticks(lim = x_lim, log = x_log, breaks = tempo_scale, n = x_second_tick_nb, warn_print = FALSE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        }
        x_second_tick_values <- tempo$values
        x_second_tick_pos <- tempo$coordinates
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::annotate(
            geom = "segment", 
            x = x_second_tick_pos, 
            xend = x_second_tick_pos, 
            y = if(base::diff(x = y_lim) > 0){tempo_coord$y.range[1]}else{tempo_coord$y.range[2]}, 
            yend = if(base::diff(x = y_lim) > 0){tempo_coord$y.range[1] + base::abs(x = base::diff(x = tempo_coord$y.range)) / 80}else{tempo_coord$y.range[2] - base::abs(x = base::diff(x = tempo_coord$y.range)) / 80}, 
            xmin = NULL, 
            xmax = NULL, 
            ymin = NULL, 
            ymax = NULL, 
            na.rm = FALSE
        ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
        coord_names <- base::c(coord_names, "x_second_tick_positions")
    }
    # for the ggplot2 bug with x_log, this does not work: base::eval(base::parse(text = base::ifelse(vertical == FALSE & x_log == "log10", "ggplot2::scale_x_continuous", "ggplot2::scale_x_continuous")))
    base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_x_continuous(
        breaks = tempo_scale, 
        minor_breaks = x_second_tick_pos, 
            # nocov start (unreachable: internal consistency guard - x_log only accepts log10, log2 or no (arg_check))
        labels = if(x_log == "log10"){scales::trans_format(trans = "identity", format = scales::math_format(expr = 10^.x, format = base::force))}else if(x_log == "log2"){scales::trans_format(trans = "identity", format = scales::math_format(expr = 2^.x, format = base::force))}else if(x_log == "no"){ggplot2::waiver()}else{tempo_cat <- base::paste0("INTERNAL ERROR 2 IN ", intern_error_text_start, "CODE INCONSISTENCY 10.", 
            intern_error_text_end, 
            collapse = NULL, 
            recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)}, 
            # nocov end
        expand = base::c(0, 0), # remove space after after axis limits
        limits = base::sort(x = x_lim, decreasing = FALSE), # NA indicate that limits must correspond to data limits but xlim() already used
        oob = scales::rescale_none, 
        transform = base::ifelse(test = base::diff(x = x_lim) < 0, yes = "reverse", no = "identity"),  # equivalent to ggplot2::scale_x_reverse() but create the problem of x-axis label disappearance with x_lim decreasing. Thus, do not use. Use xlim() below and after this
        name = ggplot2::waiver(),
        n.breaks = NULL,
        na.value = NA_real_,
        guide = ggplot2::waiver(), 
        position = "bottom", 
        sec.axis = ggplot2::waiver()
    ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
    # end x_second_tick_positions
    # y_second_tick_positions # coordinates of secondary ticks (only if y_second_tick_nb argument is non-null or if y_log argument is different from "no")
    if(y_log != "no"){ # integer main ticks for log2 and log10
        tempo_scale <- (base::as.integer(x = base::min(y_lim, na.rm = TRUE)) - 1):(base::as.integer(x = base::max(y_lim, na.rm = TRUE)) + 1)
    }else{
        tempo <- if(base::is.null(x = base::attributes(x = tempo_coord$y$breaks))){tempo_coord$y$breaks}else{
            # nocov start (unreachable: idem as x breaks)
            base::unlist(x = base::attributes(x = tempo_coord$y$breaks), recursive = TRUE, use.names = TRUE)
            # nocov end
        }
        if(base::all(base::is.na(x = tempo), na.rm = FALSE)){
            # nocov start (unreachable: internal consistency guard - idem as x breaks)
            tempo_cat <- base::paste0("INTERNAL ERROR 3 IN ", intern_error_text_start, "ONLY NA IN tempo_coord$y$breaks.", 
            intern_error_text_end, 
            collapse = NULL, 
            recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }
        if(base::length(x = base::unique(x = y_lim, incomparables = FALSE)) <= 1){
            tempo_cat <- base::paste0(error_text_start, "IT SEEMS THAT Y-AXIS VALUES HAVE A NULL RANGE: ", base::paste(y_lim, collapse = " ", sep = " ", recycle0 = FALSE), "\nPLEASE, USE THE y_lim ARGUMENT WITH 2 DIFFERENT VALUES TO SOLVE THIS", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            tempo_scale <- saferGraph::scale2(lim = y_lim, n = base::ifelse(test = base::is.null(x = y_tick_nb), yes = base::length(x = tempo[ ! base::is.na(x = tempo)]), no = y_tick_nb), kind = "approx", safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) # in ggplot 3.3.0, tempo_coord$y.major_source replaced by tempo_coord$y$breaks. If fact: n = base::ifelse(base::is.null(y_tick_nb), base::length(tempo[ ! base::is.na(tempo)]), y_tick_nb)) replaced by n = base::ifelse(base::is.null(y_tick_nb), 4, y_tick_nb))
        }
    }
    y_second_tick_values <- NULL
    y_second_tick_pos <- NULL
    if(y_log != "no"){
        tempo <- saferGraph::inter_ticks(lim = y_lim, log = y_log, warn_print = FALSE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text, breaks = NULL, n = NULL)
        y_second_tick_values <- tempo$values
        y_second_tick_pos <- tempo$coordinates
        # if(vertical == TRUE){ # do not remove in case the bug is fixed
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::annotate(
            geom = "segment", 
            y = y_second_tick_pos, 
            yend = y_second_tick_pos, 
            x = if(base::diff(x = x_lim) > 0){tempo_coord$x.range[1]}else{tempo_coord$x.range[2]}, 
            xend = if(base::diff(x = x_lim) > 0){tempo_coord$x.range[1] + base::abs(x = base::diff(x = tempo_coord$x.range)) / 80}else{tempo_coord$x.range[2] - base::abs(x = base::diff(x = tempo_coord$x.range)) / 80}, 
            xmin = NULL, 
            xmax = NULL, 
            ymin = NULL, 
            ymax = NULL, 
            na.rm = FALSE
        ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
        # }else{ # not working because of the ggplot2 bug
        # base::assign(base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), ggplot2::annotate(geom = "segment", x = y_second_tick_pos, xend = y_second_tick_pos, y = tempo_coord$y.range[1], yend = tempo_coord$y.range[1] + diff(tempo_coord$y.range) / 80))
        # }
        coord_names <- base::c(coord_names, "y_second_tick_positions")
    }else if(( ! base::is.null(x = y_second_tick_nb)) & y_log == "no"){
        # if(y_second_tick_nb > 0){ #inactivated because already checked before
        if(base::length(x = tempo_scale) < 2){
            tempo_cat1 <- base::c("y_tick_nb", "y_second_tick_nb")
            tempo_cat2 <- base::sapply(X = base::list(y_tick_nb, y_second_tick_nb), FUN = base::paste0, collapse = " ", recycle0 = FALSE, simplify = TRUE, USE.NAMES = TRUE)
            tempo_sep <- base::sapply(X = base::mapply(x = " ", times = base::max(base::nchar(x = tempo_cat1, type = "chars", allowNA = FALSE, keepNA = NA), na.rm = FALSE) - base::nchar(x = tempo_cat1, type = "chars", allowNA = FALSE, keepNA = NA) + 3, FUN = base::rep, SIMPLIFY = FALSE, MoreArgs = NULL, USE.NAMES = TRUE), FUN = base::paste0, collapse = "", recycle0 = FALSE, simplify = TRUE, USE.NAMES = TRUE)
            tempo_cat <- base::paste0(error_text_start, "THE NUMBER OF GENERATED TICKS FOR THE Y-AXIS IS NOT CORRECT: ", base::length(x = tempo_scale), "\nUSING THESE ARGUMENT SETTINGS (NO DISPLAY MEANS NULL VALUE):\n", base::paste0(tempo_cat1, tempo_sep, tempo_cat2, collapse = "\n", recycle0 = FALSE), "\nPLEASE, TEST OTHER VALUES", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        }else{
            tempo <- saferGraph::inter_ticks(lim = y_lim, log = y_log, breaks = tempo_scale, n = y_second_tick_nb, warn_print = FALSE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        }
        y_second_tick_values <- tempo$values
        y_second_tick_pos <- tempo$coordinates
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::annotate(
            geom = "segment", 
            y = y_second_tick_pos, 
            yend = y_second_tick_pos, 
            x = if(base::diff(x = x_lim) > 0){tempo_coord$x.range[1]}else{tempo_coord$x.range[2]}, 
            xend = if(base::diff(x = x_lim) > 0){tempo_coord$x.range[1] + base::abs(x = base::diff(x = tempo_coord$x.range)) / 80}else{tempo_coord$x.range[2] - base::abs(x = base::diff(x = tempo_coord$x.range)) / 80}, 
            xmin = NULL, 
            xmax = NULL, 
            ymin = NULL, 
            ymax = NULL, 
            na.rm = FALSE
        ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
        coord_names <- base::c(coord_names, "y_second_tick_positions")
    }
    # for the ggplot2 bug with y_log, this does not work: base::eval(base::parse(text = base::ifelse(vertical == FALSE & y_log == "log10", "ggplot2::scale_x_continuous", "ggplot2::scale_y_continuous")))
    base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_y_continuous(
        breaks = tempo_scale, 
        minor_breaks = y_second_tick_pos, 
            # nocov start (unreachable: internal consistency guard - y_log only accepts log10, log2 or no (arg_check))
        labels = if(y_log == "log10"){scales::trans_format(trans = "identity", format = scales::math_format(expr = 10^.x, format = base::force))}else if(y_log == "log2"){scales::trans_format(trans = "identity", format = scales::math_format(expr = 2^.x, format = base::force))}else if(y_log == "no"){ggplot2::waiver()}else{tempo_cat <- base::paste0("INTERNAL ERROR 4 IN ", intern_error_text_start, "CODE INCONSISTENCY 10.", 
            intern_error_text_end, 
            collapse = NULL, 
            recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)}, 
            # nocov end
        expand = base::c(0, 0), # remove space after axis limits
        limits = base::sort(x = y_lim, decreasing = FALSE), # NA indicate that limits must correspond to data limits but ylim() already used
        oob = scales::rescale_none, 
        transform = base::ifelse(test = base::diff(x = y_lim) < 0, yes = "reverse", no = "identity"),  # equivalent to ggplot2::scale_y_reverse() but create the problem of y-axis label disappearance with y_lim decreasing. Thus, do not use. Use ylim() below and after this
        name = ggplot2::waiver(),
        n.breaks = NULL,
        na.value = NA_real_,
        guide = ggplot2::waiver(), 
        position = "left", 
        sec.axis = ggplot2::waiver()
    ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
    # end y_second_tick_positions
    base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::coord_cartesian(xlim = x_lim, ylim = y_lim, expand = TRUE, default = FALSE, clip = "on", reverse = "none", ratio = NULL), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # clip = "off" to have secondary ticks outside plot region. The problem is that points out of bounds are also drawn outside the plot region. Thus, I cannot use it # at that stage, x_lim and y_lim not NULL anymore
    # end scale management


    # add layers management
    if(base::length(x = add_layers) > 0L){
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = add_layers, pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # layers of add, drawn above the other layers (see the add management section)
    }
    # end add layers management


    # drawing
    fin_plot <- base::eval(expr = base::parse(text = base::paste(base::paste0(tempo_gg_name, 1:tempo_gg_count, collapse = NULL, recycle0 = FALSE), collapse = " + ", sep = " ", recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    grob_save <- NULL
    if(plot == TRUE){
        if( ! base::is.null(x = legend_width)){ # base::any(base::unlist(legend_disp)) == TRUE removed to have empty legend space # not & base::any(base::unlist(fin_lg_disp)) == TRUE here because converted to FALSE
            grob_save <- base::suppressMessages(expr = base::suppressWarnings(expr = gridExtra::grid.arrange(grobs = base::list(fin_plot, legend_final), ncol = 2, nrow = 1, widths = base::c(1, legend_width), heights = grid::unit(x = 1, units = "null", data = NULL), as.table = FALSE, respect = FALSE, clip = "on", padding = grid::unit(x = 0.1, units = "line", data = NULL), newpage = TRUE), classes = "warning"), classes = "message")
        }else{
            # nocov start (unreachable: legend_width is rejected when NULL by the NULL-argument management above, so legend_width is always non-NULL here)
            grob_save <- base::suppressMessages(expr = base::suppressWarnings(expr = base::print(x = fin_plot), classes = "warning"), classes = "message")
            # nocov end
        }
    }else{
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count,") PLOT NOT SHOWN AS REQUESTED", collapse = NULL, recycle0 = FALSE)
        warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
    }
    # end drawing
    #### end main code

    #### warning output
    # must be before return()
    if( ! base::is.null(x = warn) & warn_print == TRUE){
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
        output <- base::suppressMessages(expr = ggplot2::ggplot_build(plot = fin_plot), classes = "message")
        output$data <- output$data[base::seq_len(length.out = base::length(x = output$data) - base::length(x = add_layers))] # remove the last data that correspond to the layers of add
        # output$data <- output$data[-1] # yes for boxplot but not for scatter # remove the first data because corresponds to the initial empty boxplot
        if(base::length(x = output$data) != base::length(x = coord_names)){
            # nocov start (unreachable: internal consistency guard - coord_names is built in parallel with the layers)
            tempo_cat <- base::paste0(
                "INTERNAL ERROR 5 IN ", 
                intern_error_text_start, 
                "base::length(output$data) AND base::length(coord_names) MUST BE IDENTICAL. CODE HAS TO BE MODIFIED", 
                intern_error_text_end, 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }else{
            base::names(x = output$data) <- coord_names
        }
        if(base::is.null(x = base::unlist(x = removed_row_nb, recursive = TRUE, use.names = TRUE))){
            removed_row_nb <- NULL
            removed_rows <- NULL
        }else{
            for(i3 in base::seq_along(along.with = data1)){
                if( ! base::is.null(x = removed_row_nb[[i3]])){
                    removed_row_nb[[i3]] <- base::sort(x = removed_row_nb[[i3]], decreasing = FALSE)
                    removed_rows[[i3]] <- data1_ini[[i3]][removed_row_nb[[i3]], ]
                }
            }
        }
        tempo <- output$layout$panel_params[[1]]
        output <- base::list(
            data = data1, 
            removed_row_nb = removed_row_nb, 
            removed_rows = removed_rows, 
            plot = base::c(output$data, x_second_tick_values = base::list(x_second_tick_values), y_second_tick_values = base::list(y_second_tick_values)), 
            panel = facet_categ, 
            axes = base::list(
                x.range = tempo$x.range, 
                x.labels = if(base::is.null(x = base::attributes(x = tempo$x$breaks))){tempo$x$breaks}else{ # base::is.null(attributes(tempo$x$breaks)) test if it is number (TRUE) or character (FALSE)
                    # nocov start (unreachable: continuous scales have no break attributes in this ggplot2 version, probed empirically)
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
                    # nocov start (unreachable: idem as x.labels)
                    tempo$y$scale$get_labels()
                    # nocov end
                }, 
                y.positions = if(base::is.null(x = base::attributes(x = tempo$y$breaks))){tempo$y$breaks}else{
                    # nocov start (unreachable: idem as x.labels)
                    base::unlist(x = base::attributes(x = tempo$y$breaks), recursive = TRUE, use.names = TRUE)
                    # nocov end
                }
            ), 
            warn = base::paste0("\n", warn, "\n\n", collapse = NULL, recycle0 = FALSE), 
            ggplot = if(return_ggplot == TRUE){fin_plot}else{NULL}, # fin_plot plots the graph if return == TRUE
            gtable = if(return_gtable == TRUE){grob_save}else{NULL} #
        )
        base::return(output) # this plots the graph if return_ggplot is TRUE and if no assignment
    }
    #### end output
}


