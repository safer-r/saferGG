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
#' @param add character string allowing to add more ggplot2 features (dots, lines, themes, facet, etc.). Ignored if NULL
#' WARNING: (1) the string must start with "+", (2) the string must finish with ")" and (3) each function must be preceded by "ggplot2::". Example: "+ ggplot2::coord_flip() + ggplot2::theme_bw()"
#' If the character string contains the "ggplot2::theme" string, then the article argument of gg_scatter() (see above) is ignored with a warning. In addition, some arguments can be overwritten, like x_angle (check all the arguments)
#' Handle the add argument with caution since added functions can create conflicts with the preexisting internal ggplot2 functions
#' WARNING: the call of objects inside the quotes of add can lead to an error if the name of these objects are some of the gg_scatter() arguments. Indeed, the function will use the internal argument instead of the global environment object. Example article <- "a" in the working environment and add = '+ ggplot2::ggtitle(article)'. The risk here is to have TRUE as title. To solve this, use add = '+ ggplot2::ggtitle(get("article", envir = .GlobalEnv))'
#' @param return logical. Return the graph parameters?
#' @param return_ggplot logical. Return the ggplot object in the output list? Ignored if return argument is FALSE. WARNING: always assign the gg_scatter() function (e.g., a <- gg_scatter()) if return_ggplot argument is TRUE, otherwise, double plotting is performed. See $ggplot in the RETURN section below for more details
#' @param return_gtable logical. Return the ggplot object as gtable of grobs in the output list? Ignored if plot argument is FALSE. Indeed, the graph must be plotted to get the grobs dispositions. See $gtable in the RETURN section below for more details
#' @param plot logical. Plot the graphic? If FALSE and return argument is TRUE, graphical parameters and associated warnings are provided without plotting
#' @param warn_print logical. Print warnings at the end of the execution? ? If FALSE, warning messages are never printed, but can still be recovered in the returned list. Some of the warning messages (those delivered by the internal ggplot2 functions) are not apparent when using the argument plot = FALSE
#' @param safer_check Single logical value. Perform some "safer" checks? If TRUE, checkings are performed before main code running (see https://github.com/safer-r): 1) correct lib_path argument value 2) required functions and related packages effectively present in local R lybraries and 3) R classical operators (like "<-") not overwritten by another package because of the R scope. Must be set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R package are not installed in the default directories because of lack of admin rights.  More precisely, lib_path is passed through the new argument of .libPaths() so that the new library paths are base::unique(c(new, .Library.site, .Library)). Warning: .libPaths() is restored to the initial paths, after function execution. Ignored if NULL (default) or if the safer_check argument is FALSE: only the pathways specified by the current .libPaths() are used for package calling.
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
#' set.seed(1) ; obs1 <- base::data.frame(Km = c(2, 1, 6, 5, 4, 7), Time = c(2, 1, 6, 5, 4, 7)^2, Car = c("TUUT", "TUUT", "TUUT", "WIIM", "WIIM", "WIIM"), Color1 = base::rep(c("coral", "lightblue"), each = 3), stringsAsFactors = TRUE) ; gg_scatter(data1 = obs1, x = "Km", y = "Time")
#' @importFrom Cairo Cairo
#' @importFrom ggplot2 aes aes_string annotate coord_cartesian coord_flip element_line element_rect element_text facet_grid facet_wrap geom_point ggplot ggplot_build ggtitle guide_legend guides scale_discrete_manual scale_fill_manual scale_shape_manual scale_x_continuous scale_x_reverse scale_y_continuous scale_y_reverse theme theme_bw theme_classic theme_void waiver xlab ylab
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

    ######## arg with no default values
    mandat_args <- base::c(
        "data1", 
        "x", 
        "y"
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

    ######## mandatory arg of safer-r functions
    mandat_args <- base::c("lib_path", "safer_check", "error_text")
    tempo_log <- ! mandat_args %in% arg_names
    if(base::any(x = tempo_log, na.rm = TRUE)) {
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
                    base::all(x = x, na.rm = TRUE) & base::length(x = x) > 0 # if x is empty, return FALSE, so OK
                }, 
                simplify = TRUE, 
                USE.NAMES = TRUE
            ), 
            classes = "warning"
        ) # no argument provided by the user can be just made of NA. base::is.na(NULL) returns logical(0), the reason why base::length(x = x) > 0 is used # warning: base::all(x = x, na.rm = TRUE) but normally no NA because base::is.na() used here. Warning: would not work if arg_user_setting_eval is a vector (because treat each element as a compartment), but ok because it is always a list, even is 0 or 1 argument in the developed function
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
    if(safer_check == TRUE){
        if( ! base::is.null(x = lib_path)){ #  base::is.null(NA) returns FALSE so OK.
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
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
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
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
            }else{
                ini_lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
                base::on.exit(expr = base::.libPaths(new = ini_lib_path, include.site = TRUE), add = TRUE, after = TRUE) # return to the previous libPaths()
                base::.libPaths(new = base::sub(x = lib_path, pattern = "/$|\\\\$", replacement = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), include.site = TRUE) # base::.libPaths(new = ) add path to default path. BEWARE: base::.libPaths() does not support / at the end of a submitted path. The reason of the check and replacement of the last / or \\ in path
                lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
            }
        }else{
            lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument # base::.libPaths(new = lib_path) # or base::.libPaths(new = base::c(base::.libPaths(), lib_path))
        }
    }
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    if(safer_check == TRUE){
        saferDev:::.pack_and_function_check(
            fun = base::c(
                # functions required in this code
                "ggplot2::aes",
                "ggplot2::aes_string",
                "ggplot2::annotate",
                "ggplot2::coord_cartesian",
                "ggplot2::coord_flip",
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
                "ggplot2::scale_x_reverse",
                "ggplot2::scale_y_continuous",
                "ggplot2::scale_y_reverse",
                "ggplot2::theme",
                "ggplot2::theme_bw",
                "ggplot2::theme_classic",
                "ggplot2::theme_void",
                "ggplot2::waiver",
                "ggplot2::xlab",
                "ggplot2::ylab",
                "grDevices::dev.list",
                "grid::convertUnit",
                "grid::unit",
                "gridExtra::grid.arrange",
                "scales::math_format",
                "scales::rescale_none",
                "scales::trans_format",
                "saferGraph::scale2", 
                "saferGraph::inter_ticks",
                "Cairo::Cairo",
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
    reserved_words <- c("fake_x", "fake_y", "fake_categ")
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
    if(base::all(class(data1) == "list")){
        if(base::length(data1) > 6){
            tempo_cat <- base::paste0(error_text_start, "data1 ARGUMENT MUST BE A LIST OF 6 DATA FRAMES MAXIMUM (6 OVERLAYS MAX).")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        if(base::is.null(base::names(data1))){
            base::names(data1) <- base::paste0("L", 1:length(data1))
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") NULL NAME COMPARTMENT OF data1 LIST -> NAMES RESPECTIVELY ATTRIBUTED TO EACH COMPARTMENT:\n", base::paste(base::names(data1), collapse = " "))
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
        }
        if( ! base::is.null(x)){
            if( ! (base::all(class(x) == "list") & base::length(data1) == base::length(x))){
                tempo_cat <- base::paste0(error_text_start, "x ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST.")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
        }else{
            x <- base::vector("list", base::length(data1))
        }
        if( ! base::is.null(y)){
            if( ! (base::all(class(y) == "list") & base::length(data1) == base::length(y))){
                tempo_cat <- base::paste0(error_text_start, "y ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
        }else{
            y <- base::vector("list", base::length(data1))
        }
        if( ! base::is.null(categ)){
            if( ! (base::all(class(categ) == "list") & base::length(data1) == base::length(categ))){
                tempo_cat <- base::paste0(error_text_start, "categ ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
        }
        if( ! base::is.null(categ_class_order)){
            if( ! (base::all(class(categ_class_order) == "list") & base::length(data1) == base::length(categ_class_order))){
                tempo_cat <- base::paste0(error_text_start, "categ_class_order ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
        }
        if( ! base::is.null(color)){
            if( ! ((base::all(class(color) == "list") & base::length(data1) == base::length(color)) | ((base::all(mode(color) == "character") | base::all(mode(color) == "numeric")) & base::length(color)== 1L))){ # list of same length as data1 or single value
                tempo_cat <- base::paste0(error_text_start, "color ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST, OR A SINGLE CHARACTER STRING OR INTEGER")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else if((base::all(mode(color) == "character") | base::all(mode(color) == "numeric")) & base::length(color)== 1L){ # convert the single value into a list of single value
                list_color <- base::vector(mode = "list", length = base::length(data1))
                list_color[] <- color
            }
        }
        if( ! ((base::all(class(geom) == "list") & base::length(data1) == base::length(geom)) | (base::all(mode(geom) == "character") & base::length(geom)== 1L))){ # list of same length as data1 or single value
            tempo_cat <- base::paste0(error_text_start, "geom ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST, OR A SINGLE CHARACTER VALUE")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(base::all(mode(geom) == "character") & base::length(geom)== 1L){ # convert the single value into a list of single value
            list_geom <- base::vector(mode = "list", length = base::length(data1))
            list_geom[] <- geom
        }
        if( ! ((base::all(class(geom_step_dir) == "list") & base::length(data1) == base::length(geom_step_dir)) | (base::all(mode(geom_step_dir) == "character") & base::length(geom_step_dir)== 1L))){ # list of same length as data1 or single value
            tempo_cat <- base::paste0(error_text_start, "geom_step_dir ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST, OR A SINGLE CHARACTER VALUE")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(base::all(mode(geom_step_dir) == "character") & base::length(geom_step_dir)== 1L){ # convert the single value into a list of single value
            list_geom_step_dir <- base::vector(mode = "list", length = base::length(data1))
            list_geom_step_dir[] <- geom_step_dir
        }
        if( ! base::is.null(geom_stick_base)){
            if( ! ((base::all(class(geom_stick_base) == "list") & base::length(data1) == base::length(geom_stick_base)) | (base::all(mode(geom_stick_base) == "numeric") & base::length(geom_stick_base)== 1L))){ # list of same length as data1 or single value
                tempo_cat <- base::paste0(error_text_start, "geom_stick_base ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST, OR A SINGLE NUMERIC VALUE")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else if(base::all(mode(geom_stick_base) == "numeric") & base::length(geom_stick_base)== 1L){ # convert the single value into a list of single value
                list_geom_stick_base <- base::vector(mode = "list", length = base::length(data1))
                list_geom_stick_base[] <- geom_stick_base
            }
        }
        if( ! ((base::all(class(alpha) == "list") & base::length(data1) == base::length(alpha)) | (base::all(mode(alpha) == "numeric") & base::length(alpha)== 1L))){ # list of same length as data1 or single value
            tempo_cat <- base::paste0(error_text_start, "alpha ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST, OR A SINGLE NUMERIC VALUE")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(base::all(mode(alpha) == "numeric") & base::length(alpha)== 1L){ # convert the single value into a list of single value
            list_alpha <- base::vector(mode = "list", length = base::length(data1))
            list_alpha[] <- alpha
        }
        if( ! ((base::all(class(dot_size) == "list") & base::length(data1) == base::length(dot_size)) | (base::all(mode(dot_size) == "numeric") & base::length(dot_size)== 1L))){ # list of same length as data1 or single value
            tempo_cat <- base::paste0(error_text_start, "dot_size ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST, OR A SINGLE NUMERIC VALUE")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(base::all(mode(dot_size) == "numeric") & base::length(dot_size)== 1L){ # convert the single value into a list of single value
            list_dot_size <- base::vector(mode = "list", length = base::length(data1))
            list_dot_size[] <- dot_size
        }
        if( ! ((base::all(class(dot_shape) == "list") & base::length(data1) == base::length(dot_shape)) | (base::all(mode(dot_shape) != "list") & base::length(dot_shape)== 1L))){ # list of same length as data1 or single value
            tempo_cat <- base::paste0(error_text_start, "dot_shape ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST, OR A SINGLE SHAPE VALUE")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(base::all(mode(dot_shape) != "list") & base::length(dot_shape)== 1L){ # convert the single value into a list of single value
            list_dot_shape <- base::vector(mode = "list", length = base::length(data1))
            list_dot_shape[] <- dot_shape
        }
        if( ! ((base::all(class(dot_border_size) == "list") & base::length(data1) == base::length(dot_border_size)) | (base::all(mode(dot_border_size) == "numeric") & base::length(dot_border_size)== 1L))){ # list of same length as data1 or single value
            tempo_cat <- base::paste0(error_text_start, "dot_border_size ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST, OR A SINGLE NUMERIC VALUE")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(base::all(mode(dot_border_size) == "numeric") & base::length(dot_border_size)== 1L){ # convert the single value into a list of single value
            list_dot_border_size <- base::vector(mode = "list", length = base::length(data1))
            list_dot_border_size[] <- dot_border_size
        }
        if( ! base::is.null(dot_border_color)){
            if( ! ((base::all(class(dot_border_color) == "list") & base::length(data1) == base::length(dot_border_color)) | ((base::all(mode(dot_border_color) == "character") | base::all(mode(dot_border_color) == "numeric")) & base::length(dot_border_color)== 1L))){ # list of same length as data1 or single value
                tempo_cat <- base::paste0(error_text_start, "dot_border_color ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST, OR A SINGLE CHARACTER STRING OR INTEGER")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else if((base::all(mode(dot_border_color) == "character") | base::all(mode(dot_border_color) == "numeric")) & base::length(dot_border_color)== 1L){ # convert the single value into a list of single value
                list_dot_border_color <- base::vector(mode = "list", length = base::length(data1))
                list_dot_border_color[] <- dot_border_color
            }
        }
        if( ! ((base::all(class(line_size) == "list") & base::length(data1) == base::length(line_size)) | (base::all(mode(line_size) == "numeric") & base::length(line_size)== 1L))){ # list of same length as data1 or single value
            tempo_cat <- base::paste0(error_text_start, "line_size ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST, OR A SINGLE NUMERIC VALUE")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(base::all(mode(line_size) == "numeric") & base::length(line_size)== 1L){ # convert the single value into a list of single value
            list_line_size <- base::vector(mode = "list", length = base::length(data1))
            list_line_size[] <- line_size
        }
        if( ! ((base::all(class(line_type) == "list") & base::length(data1) == base::length(line_type)) | (base::all(mode(line_type) != "list") & base::length(line_type)== 1L))){ # list of same length as data1 or single value
            tempo_cat <- base::paste0(error_text_start, "line_type ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST, OR A SINGLE LINE KIND VALUE")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(base::all(mode(line_type) != "list") & base::length(line_type)== 1L){ # convert the single value into a list of single value
            list_line_type <- base::vector(mode = "list", length = base::length(data1))
            list_line_type[] <- line_type
        }
        if( ! base::is.null(legend_name)){
            if( ! (base::all(class(legend_name) == "list") & base::length(data1) == base::length(legend_name))){
                tempo_cat <- base::paste0(error_text_start, "legend_name ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
        }
    }
    # end check list lengths (and names of data1 compartments if present)
    # conversion into lists
    if(base::all(is.data.frame(data1))){
        data1 <- list(L1 = data1)
        if(base::all(class(x) == "list")){
            tempo_cat <- base::paste0(error_text_start, "x ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            x <- list(L1 = x)
        }
        if(base::all(class(y) == "list")){
            tempo_cat <- base::paste0(error_text_start, "y ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            y <- list(L1 = y)
        }
        if( ! base::is.null(categ)){
            if(base::all(class(categ) == "list")){
                tempo_cat <- base::paste0(error_text_start, "categ ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                categ <- list(L1 = categ)
            }
        }
        if( ! base::is.null(categ_class_order)){
            if(base::all(class(categ_class_order) == "list")){
                tempo_cat <- base::paste0(error_text_start, "categ_class_order ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                categ_class_order <- list(L1 = categ_class_order)
            }
        }
        if( ! base::is.null(color)){
            if(base::all(class(color) == "list")){
                tempo_cat <- base::paste0(error_text_start, "color ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                color <- list(L1 = color)
            }
        }
        if(base::all(class(geom) == "list")){
            tempo_cat <- base::paste0(error_text_start, "geom ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            geom <- list(L1 = geom)
        }
        if(base::all(class(geom_step_dir) == "list")){
            tempo_cat <- base::paste0(error_text_start, "geom_step_dir ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            geom_step_dir <- list(L1 = geom_step_dir)
        }
        if( ! base::is.null(geom_stick_base)){
            if(base::all(class(geom_stick_base) == "list")){
                tempo_cat <- base::paste0(error_text_start, "geom_stick_base ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                geom_stick_base <- list(L1 = geom_stick_base)
            }
        }
        if(base::all(class(alpha) == "list")){
            tempo_cat <- base::paste0(error_text_start, "alpha ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            alpha <- list(L1 = alpha)
        }
        if(base::all(class(dot_size) == "list")){
            tempo_cat <- base::paste0(error_text_start, "dot_size ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            dot_size <- list(L1 = dot_size)
        }
        if(base::all(class(dot_shape) == "list")){
            tempo_cat <- base::paste0(error_text_start, "dot_shape ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            dot_shape <- list(L1 = dot_shape)
        }
        if(base::all(class(dot_border_size) == "list")){
            tempo_cat <- base::paste0(error_text_start, "dot_border_size ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            dot_border_size <- list(L1 = dot_border_size)
        }
        if( ! base::is.null(dot_border_color)){
            if(base::all(class(dot_border_color) == "list")){
                tempo_cat <- base::paste0(error_text_start, "dot_border_color ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                dot_border_color <- list(L1 = dot_border_color)
            }
        }
        if(base::all(class(line_size) == "list")){
            tempo_cat <- base::paste0(error_text_start, "line_size ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            line_size <- list(L1 = line_size)
        }
        if(base::all(class(line_type) == "list")){
            tempo_cat <- base::paste0(error_text_start, "line_type ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            line_type <- list(L1 = line_type)
        }
        if( ! base::is.null(legend_name)){
            if(base::all(class(legend_name) == "list")){
                tempo_cat <- base::paste0(error_text_start, "legend_name ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                legend_name <- list(L1 = legend_name)
            }
        }
    }else if( ! base::all(base::sapply(data1, FUN = "class") == "data.frame")){ # if not a data frame, data1 can only be a list, as tested above
        tempo_cat <- base::paste0(error_text_start, "data1 ARGUMENT MUST BE A DATA FRAME OR A LIST OF DATA FRAMES")
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    # single value converted into list now reattributed to the argument name
    if( ! base::is.null(color)){
        if( ! base::is.null(list_color)){
            color <- list_color
        }
    }
    if( ! base::is.null(list_geom)){
        geom <- list_geom
    }
    if( ! base::is.null(list_geom_step_dir)){
        geom_step_dir <- list_geom_step_dir
    }
    if( ! base::is.null(geom_stick_base)){
        if( ! base::is.null(list_geom_stick_base)){
            geom_stick_base <- list_geom_stick_base
        }
    }
    if( ! base::is.null(list_alpha)){
        alpha <- list_alpha
    }
    if( ! base::is.null(list_dot_size)){
        dot_size <- list_dot_size
    }
    if( ! base::is.null(list_dot_shape)){
        dot_shape <- list_dot_shape
    }
    if( ! base::is.null(list_dot_border_size)){
        dot_border_size <- list_dot_border_size
    }
    if( ! base::is.null(dot_border_color)){
        if( ! base::is.null(list_dot_border_color)){
            dot_border_color <- list_dot_border_color
        }
    }
    if( ! base::is.null(list_line_size)){
        line_size <- list_line_size
    }
    if( ! base::is.null(list_line_type)){
        line_type <- list_line_type
    }
    # end single value converted into list now reattributed to the argument name
    # data, x, y, geom, alpha, dot_size, shape, dot_border_size, line_size, line_type, legend_name are list now
    # if non-null, categ, categ_class_order, legend_name, color, dot_border_color are list now
    # end conversion into lists
    # verif of add
    if( ! base::is.null(add)){
        if( ! base::grepl(pattern = "^\\s*\\+", add)){ # check that the add string start by +
            tempo_cat <- base::paste0(error_text_start, "add ARGUMENT MUST START WITH \"+\": ", base::paste(base::unique(add), collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            
        }else if( ! base::grepl(pattern = "(ggplot2|lemon)\\s*::", add)){ #
            tempo_cat <- base::paste0(error_text_start, "FOR EASIER FUNCTION DETECTION, add ARGUMENT MUST CONTAIN \"ggplot2::\" OR \"lemon::\" IN FRONT OF EACH GGPLOT2 FUNCTION: ", base::paste(base::unique(add), collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if( ! base::grepl(pattern = ")\\s*$", add)){ # check that the add string finished by )
            tempo_cat <- base::paste0(error_text_start, "add ARGUMENT MUST FINISH BY \")\": ", base::paste(base::unique(add), collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    # end verif of add
    # management of add containing facet
    facet_categ <- NULL
    if( ! base::is.null(add)){
        facet_check <- TRUE
        tempo <- base::unlist(base::strsplit(x = add, split = "\\s*\\+\\s*(ggplot2|lemon)\\s*::\\s*")) #
        tempo <- base::sub(x = tempo, pattern = "^facet_wrap", replacement = "ggplot2::facet_wrap")
        tempo <- base::sub(x = tempo, pattern = "^facet_grid", replacement = "ggplot2::facet_grid")
        tempo <- base::sub(x = tempo, pattern = "^facet_rep", replacement = "lemon::facet_rep")
        if(base::length(data1) > 1 & (base::any(base::grepl(x = tempo, pattern = "ggplot2::facet_wrap|lemon::facet_rep_wrap")) | base::grepl(x = add, pattern = "ggplot2::facet_grid|lemon::facet_rep_grid"))){
            tempo_cat <- base::paste0(error_text_start, "facet PANELS CANNOT BE USED IF MORE THAN ONE DATA FRAME IN THE data1 ARGUMENT\nPLEASE REWRITE THE add STRING AND RERUN")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            if(base::any(base::grepl(x = tempo, pattern = "ggplot2::facet_wrap|lemon::facet_rep_wrap"))){
                tempo1 <- base::suppressWarnings(expr = base::eval(base::parse(text = tempo[base::grepl(x = tempo, pattern = "ggplot2::facet_wrap|lemon::facet_rep_wrap")])))
                facet_categ <- list(base::names(tempo1$params$facets)) # list of length 1
                tempo_text <- "facet_wrap OR facet_rep_wrap"
                facet_check <- FALSE
            }else if(base::grepl(x = add, pattern = "ggplot2::facet_grid|lemon::facet_rep_grid")){
                tempo1 <- base::suppressWarnings(expr = base::eval(base::parse(text = tempo[base::grepl(x = tempo, pattern = "ggplot2::facet_grid|lemon::facet_rep_grid")])))
                facet_categ <- list(c(base::names(tempo1$params$rows), base::names(tempo1$params$cols))) # list of length 1
                tempo_text <- "facet_grid OR facet_rep_grid"
                facet_check <- FALSE
            }
            if(facet_check == FALSE & ! base::all(facet_categ %in% base::names(data1[[1]]))){ # WARNING: base::all(facet_categ %in% base::names(data1)) is TRUE when facet_categ is NULL
                tempo_cat <- base::paste0(error_text_start, "DETECTION OF \"", tempo_text, "\" STRING IN THE add ARGUMENT BUT PROBLEM OF VARIABLE DETECTION (COLUMN NAMES OF data1)\nTHE DETECTED VARIABLES ARE:\n", base::paste(facet_categ, collapse = " "), "\nTHE data1 COLUMN NAMES ARE:\n", base::paste(base::names(data1[[1]]), collapse = " "), "\nPLEASE REWRITE THE add STRING AND RERUN")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
        }
    }
    # if facet_categ is not NULL, it is a list of length 1 now
    # end management of add containing facet
    # legend name filling
    if(base::is.null(legend_name) & ! base::is.null(categ)){
        legend_name <- categ
    }else if(base::is.null(legend_name) & base::is.null(categ)){
        legend_name <- base::vector("list", base::length(data1)) # null list
    }
    # legend_name not NULL anymore (list)
    # end legend name filling
    # ini categ for legend display
    fin_lg_disp <- base::vector("list", 6) # will be used at the end to display or not legends
    fin_lg_disp[] <- FALSE
    legend_disp <- base::vector("list", base::length(data1))
    if(base::is.null(categ) | legend_show == FALSE){
        legend_disp[] <- FALSE
    }else{
        for(i2 in 1:length(data1)){
            if(base::is.null(categ[[i2]])){
                legend_disp[[i2]] <- FALSE
            }else{
                legend_disp[[i2]] <- TRUE
            }
        }
    }
    # end ini categ for legend display
    # integer colors into gg_palette
    tempo_check_color <- NULL
    for(i1 in 1:length(data1)){
        if(base::any(base::is.na(color[[i1]]))){
            tempo_cat <- base::paste0(error_text_start, "", base::ifelse(base::length(color)== 1L, "color", base::paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT")), ": color ARGUMENT CANNOT CONTAIN NA")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        tempo_check_color <- c(tempo_check_color, saferDev::arg_check(data = color[[i1]], data_name = base::ifelse(base::length(color)== 1L, "color", base::paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT")), class = NULL, typeof = "integer", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)$problem)
    }
    tempo_check_color <- ! tempo_check_color # invert TRUE and FALSE because if integer, then problem = FALSE
    if(base::any(tempo_check_color == TRUE)){ # convert integers into colors
        tempo_integer <- base::unlist(color[tempo_check_color])
        tempo_color <- gg_palette(base::max(tempo_integer, na.rm = TRUE))
        for(i1 in 1:length(data1)){
            if(tempo_check_color[i1] == TRUE){
                color[[i1]] <-tempo_color[color[[i1]]]
            }
        }
    }
    # end integer colors into gg_palette
    # loop (checking inside list compartment)
    compart_null_color <- 0 # will be used to attribute a color when color is non-null but a compartment of color is NULL
    data1_ini <- data1 # to report NA removal
    removed_row_nb <- base::vector("list", length = base::length(data1)) # to report NA removal. Contains NULL
    removed_rows <- base::vector("list", length = base::length(data1)) # to report NA removal. Contains NULL
    for(i1 in 1:length(data1)){
        tempo <- saferDev::arg_check(data = data1[[i1]], data_name = base::ifelse(base::length(data1)== 1L, "data1 ARGUMENT", base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), class = "data.frame", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
            # nocov start (unreachable: subsumed by the earlier data-frame check of data1 (MUST BE A DATA FRAME OR A LIST OF DATA FRAMES))
        if(tempo$problem == TRUE){
            base::stop(base::paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
            # nocov end
        # reserved word checking
        if(base::any(base::names(data1[[i1]]) %in% reserved_words)){ # I do not use fun_name_change() because cannot control y before creating "fake_y". But ok because reserved are not that common
            tempo_cat <- base::paste0(error_text_start, "COLUMN NAMES OF ", base::ifelse(base::length(data1)== 1L, "data1 ARGUMENT", base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), " ARGUMENT CANNOT BE ONE OF THESE WORDS\n", base::paste(reserved_words, collapse = " "), "\nTHESE ARE RESERVED FOR THE ", function_name, " FUNCTION")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        if( ! (base::is.null(add))){
            if(base::any(base::sapply(X = reserved_words, FUN = grepl, x = add))){
                tempo_cat <- base::paste0(error_text_start, "DETECTION OF COLUMN NAMES OF data1 IN THE add ARGUMENT STRING, THAT CORRESPOND TO RESERVED STRINGS FOR ", function_name, "\nFOLLOWING COLUMN NAMES HAVE TO BE CHANGED:\n", base::paste(arg_names[base::sapply(X = reserved_words, FUN = grepl, x = add)], collapse = "\n"), "\nFOR INFORMATION, THE RESERVED WORDS ARE:\n", base::paste(reserved_words, collapse = "\n"))
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else if(base::any(base::sapply(X = arg_names, FUN = grepl, x = add))){
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") NAMES OF ", function_name, " ARGUMENTS DETECTED IN THE add STRING:\n", base::paste(arg_names[base::sapply(X = arg_names, FUN = grepl, x = add)], collapse = "\n"), "\nRISK OF WRONG OBJECT USAGE INSIDE ", function_name)
                warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
            }
        }
        # end reserved word checking
        # check of geom now because required for y argument
        tempo <- saferDev::arg_check(data = geom[[i1]], data_name = base::ifelse(base::length(geom)== 1L, "geom", base::paste0("geom NUMBER ", i1)), options = c("geom_point", "geom_line", "geom_path", "geom_step", "geom_hline", "geom_vline", "geom_stick"), class = NULL, typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        if(tempo$problem == TRUE){
            base::stop(base::paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
            # nocov start (unreachable: a NULL geom_step_dir is already rejected by the secondary-checking NULL/empty-string block)
        if(geom[[i1]] == "geom_step" & base::is.null(geom_step_dir[[i1]])){
            tempo_cat <- base::paste0(error_text_start, "", base::ifelse(base::length(geom_step_dir)== 1L, "geom_step_dir", base::paste0("ELEMENT ", i1, " OF geom_step_dir ARGUMENT")), ": geom_step_dir ARGUMENT CANNOT BE NULL IF ", base::ifelse(base::length(geom)== 1L, "geom", base::paste0("ELEMENT ", i1, " OF geom")), " ARGUMENT IS \"geom_step\"\nHERE geom_step_dir ARGUMENT IS: ", base::paste(geom_step_dir[[i1]], collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }else if(geom[[i1]] == "geom_step" & ! base::is.null(geom_step_dir[[i1]])){
            tempo <- saferDev::arg_check(data = geom_step_dir[[i1]], data_name = base::ifelse(base::length(geom_step_dir)== 1L, "geom_step_dir", base::paste0("geom_step_dir NUMBER ", i1)), options = c("vh", "hv", "mid"), class = NULL, typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
            if(tempo$problem == TRUE){
                base::stop(base::paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
            }
        }
        if( ! (base::is.null(geom_stick_base))){
            if(geom[[i1]] == "geom_stick" & ! base::is.null(geom_stick_base[[i1]])){
                tempo <- saferDev::arg_check(data = geom_stick_base[[i1]], data_name = base::ifelse(base::length(geom_stick_base)== 1L, "geom_stick_base", base::paste0("geom_stick_base NUMBER ", i1)), class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
                if(tempo$problem == TRUE){
                    base::stop(base::paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
                }
            }
        }
        # end check of geom now because required for y argument
        if(base::is.null(x[[i1]])){
            if(base::all(geom[[i1]] != "geom_hline")){
                tempo_cat <- base::paste0(error_text_start, "", base::ifelse(base::length(x)== 1L, "x", base::paste0("ELEMENT ", i1, " OF x ARGUMENT")), " IN ", base::ifelse(base::length(data1)== 1L, "data1 ARGUMENT", base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), ": x ARGUMENT CANNOT BE NULL EXCEPT IF ", base::ifelse(base::length(geom)== 1L, "x", base::paste0("geom NUMBER ", i1)), " ARGUMENT IS \"geom_hline\"\nHERE geom ARGUMENT IS: ", base::paste(geom[[i1]], collapse = " "))
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                x[[i1]] <- "fake_x"
                data1[[i1]] <- cbind(data1[[i1]], fake_x = NA, stringsAsFactors = TRUE)
                data1[[i1]][, "fake_x"] <- base::as.numeric(data1[[i1]][, "fake_x"])
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") NULL ", base::ifelse(base::length(x)== 1L, "x", base::paste0("ELEMENT ", i1, " OF x")), " ARGUMENT ASSOCIATED TO ", base::ifelse(base::length(geom)== 1L, "geom", base::paste0("geom NUMBER ", i1)), " ARGUMENT ", geom[[i1]], " -> FAKE COLUMN ADDED TO DATA FRAME ", base::ifelse(base::length(data1)== 1L, "data1 ARGUMENT", base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), ", NAMED \"fake_x\" FOR FINAL DRAWING")
                warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
            }
        }else{
            if(base::all(geom[[i1]] == "geom_hline")){
                tempo_cat <- base::paste0(error_text_start, "", base::ifelse(base::length(x)== 1L, "x", base::paste0("ELEMENT ", i1, " OF x ARGUMENT")), " IN ", base::ifelse(base::length(data1)== 1L, "data1 ARGUMENT", base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), ": x ARGUMENT MUST BE NULL IF ", base::ifelse(base::length(geom)== 1L, "geom", base::paste0("geom NUMBER ", i1)), " ARGUMENT IS \"geom_hline\"")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
            tempo <- saferDev::arg_check(data = x[[i1]], data_name = base::ifelse(base::length(x)== 1L, "x", base::paste0("ELEMENT ", i1, " OF x ARGUMENT")), class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
            if(tempo$problem == TRUE){
                base::stop(base::paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
            }
        }
        if(base::is.null(y[[i1]])){
            if(base::all(geom[[i1]] != "geom_vline")){
                tempo_cat <- base::paste0(error_text_start, "", base::ifelse(base::length(y)== 1L, "y", base::paste0("ELEMENT ", i1, " OF y ARGUMENT")), " IN ", base::ifelse(base::length(data1)== 1L, "data1 ARGUMENT", base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), ": y ARGUMENT CANNOT BE NULL EXCEPT IF ", base::ifelse(base::length(geom)== 1L, "y", base::paste0("geom NUMBER ", i1)), " ARGUMENT IS \"geom_vline\"\nHERE geom ARGUMENT IS: ", base::paste(geom[[i1]], collapse = " "))
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                y[[i1]] <- "fake_y"
                data1[[i1]] <- cbind(data1[[i1]], fake_y = NA, stringsAsFactors = TRUE)
                data1[[i1]][, "fake_y"] <- base::as.numeric(data1[[i1]][, "fake_y"])
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") NULL ", base::ifelse(base::length(y)== 1L, "y", base::paste0("ELEMENT ", i1, " OF y")), " ARGUMENT ASSOCIATED TO ", base::ifelse(base::length(geom)== 1L, "geom", base::paste0("geom NUMBER ", i1)), " ARGUMENT ", geom[[i1]], " -> FAKE COLUMN ADDED TO DATA FRAME ", base::ifelse(base::length(data1)== 1L, "data1 ARGUMENT", base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), ", NAMED \"fake_y\" FOR FINAL DRAWING")
                warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
            }
        }else{
            if(base::all(geom[[i1]] == "geom_vline")){
                tempo_cat <- base::paste0(error_text_start, "", base::ifelse(base::length(y)== 1L, "y", base::paste0("ELEMENT ", i1, " OF y ARGUMENT")), " IN ", base::ifelse(base::length(data1)== 1L, "data1 ARGUMENT", base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), ": y ARGUMENT MUST BE NULL IF ", base::ifelse(base::length(geom)== 1L, "geom", base::paste0("geom NUMBER ", i1)), " ARGUMENT IS \"geom_vline\"")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
            tempo <- saferDev::arg_check(data = y[[i1]], data_name = base::ifelse(base::length(y)== 1L, "y", base::paste0("ELEMENT ", i1, " OF y ARGUMENT")), class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
            if(tempo$problem == TRUE){
                base::stop(base::paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
            }
        }
        # x[[i1]] and y[[i1]] not NULL anymore
        if( ! (x[[i1]] %in% base::names(data1[[i1]]))){
            tempo_cat <- base::paste0(error_text_start, "", base::ifelse(base::length(x)== 1L, "x", base::paste0("ELEMENT ", i1, " OF x")), " ARGUMENT MUST BE A COLUMN NAME OF ", base::ifelse(base::length(data1)== 1L, "data1 ARGUMENT", base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT\nHERE IT IS: ", base::paste(x[[i1]], collapse = " "))))
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        if( ! (y[[i1]] %in% base::names(data1[[i1]]))){
            tempo_cat <- base::paste0(error_text_start, "", base::ifelse(base::length(y)== 1L, "y", base::paste0("ELEMENT ", i1, " OF y")), " ARGUMENT MUST BE A COLUMN NAME OF ", base::ifelse(base::length(data1)== 1L, "data1 ARGUMENT", base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT\nHERE IT IS: ", base::paste(y[[i1]], collapse = " "))))
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        if(x[[i1]] != "fake_x"){ # fake_x column is all-NA by design (added when x is NULL with geom_hline), so the NA-only arg_check would fail: skip it in this case
            if(base::inherits(data1[[i1]][, x[[i1]]], what = "factor")){ # factors have mode "numeric" but cannot be plotted as coordinates: dedicated check to avoid the obscure ggplot2 "'range' not meaningful for factors" error
                tempo_cat <- base::paste0(error_text_start, "", base::ifelse(base::length(x)== 1L, "x ARGUMENT (AS COLUMN NAME OF data1 DATA FRAME)", base::paste0("ELEMENT ", i1, " OF x ARGUMENT (AS COLUMN NAME OF data1 DATA FRAME NUMBER ", i1, ")")), " CANNOT BE A FACTOR BUT MUST BE MADE OF NUMBERS")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
            }
            tempo <- saferDev::arg_check(data = data1[[i1]][, x[[i1]]], data_name = base::ifelse(base::length(x)== 1L, "x ARGUMENT (AS COLUMN NAME OF data1 DATA FRAME)", base::paste0("ELEMENT ", i1, " OF x ARGUMENT", " (AS COLUMN NAME OF data1 DATA FRAME NUMBER ", i1, ")")), class = NULL, typeof = NULL, mode = "numeric", length = NULL, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        if(tempo$problem == TRUE){
            base::stop(base::paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
        }
        if(y[[i1]] != "fake_y"){ # fake_y column is all-NA by design (added when y is NULL with geom_vline), so the NA-only arg_check would fail: skip it in this case
            if(base::inherits(data1[[i1]][, y[[i1]]], what = "factor")){ # factors have mode "numeric" but cannot be plotted as coordinates: dedicated check to avoid the obscure ggplot2 "'range' not meaningful for factors" error
                tempo_cat <- base::paste0(error_text_start, "", base::ifelse(base::length(y)== 1L, "y ARGUMENT (AS COLUMN NAME OF data1 DATA FRAME)", base::paste0("ELEMENT ", i1, " OF y ARGUMENT (AS COLUMN NAME OF data1 DATA FRAME NUMBER ", i1, ")")), " CANNOT BE A FACTOR BUT MUST BE MADE OF NUMBERS")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
            }
            tempo <- saferDev::arg_check(data = data1[[i1]][, y[[i1]]], data_name = base::ifelse(base::length(y)== 1L, "y ARGUMENT (AS COLUMN NAME OF data1 DATA FRAME)", base::paste0("ELEMENT ", i1, " OF y ARGUMENT", " (AS COLUMN NAME OF data1 DATA FRAME NUMBER ", i1, ")")), class = NULL, typeof = NULL, mode = "numeric", length = NULL, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        if(tempo$problem == TRUE){
            base::stop(base::paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
        }
        if(x[[i1]] == "fake_x" & y[[i1]] == "fake_y"){ # because the code cannot accept to be both "fake_x" and "fake_y" at the same time
            # nocov start (unreachable: x and y cannot both be fake in the same data frame (fake_x requires geom_hline, fake_y requires geom_vline, and geom is a single value))
            tempo_cat <- base::paste0(error_text_start, "CODE INCONSISTENCY 2\nTHE CODE CANNOT ACCEPT x AND y TO BE \"fake_x\" AND \"fake_y\" IN THE SAME DATA FRAME ", i1, " ")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }
        
        if(( ! base::is.null(categ)) & ( ! base::is.null(categ[[i1]]))){ # base::is.null(categ[[i1]]) works even if categ is NULL # base::is.null(categ[[i1]]) works even if categ is NULL # if categ[[i1]] = NULL, fake_categ will be created later on
            tempo <- saferDev::arg_check(data = categ[[i1]], data_name = base::ifelse(base::length(categ)== 1L, "categ", base::paste0("ELEMENT ", i1, " OF categ ARGUMENT")), class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
            if(tempo$problem == TRUE){
                base::stop(base::paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
            }
            if( ! (categ[[i1]] %in% base::names(data1[[i1]]))){
                tempo_cat <- base::paste0(error_text_start, "", base::ifelse(base::length(categ)== 1L, "categ", base::paste0("ELEMENT ", i1, " OF categ")), " ARGUMENT MUST BE A COLUMN NAME OF ", base::ifelse(base::length(data1)== 1L, "data1 ARGUMENT", base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT\nHERE IT IS: ", base::paste(categ[[i1]], collapse = " "))))
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
            tempo1 <- saferDev::arg_check(data = data1[[i1]][, categ[[i1]]], data_name = base::ifelse(base::length(categ)== 1L, "categ OF data1 ARGUMENT", base::paste0("ELEMENT ", i1, " OF categ ARGUMENT IN DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), class = NULL, typeof = "character", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
            tempo2 <- saferDev::arg_check(data = data1[[i1]][, categ[[i1]]], data_name = base::ifelse(base::length(categ)== 1L, "categ OF data1 ARGUMENT", base::paste0("ELEMENT ", i1, " OF categ ARGUMENT IN DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), class = "factor", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
            if(tempo1$problem == TRUE & tempo2$problem == TRUE){
                tempo_cat <- base::paste0(error_text_start, "", base::ifelse(base::length(categ)== 1L, "categ OF data1 ARGUMENT", base::paste0("ELEMENT ", i1, " OF categ ARGUMENT IN DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), " MUST BE A FACTOR OR CHARACTER VECTOR")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else if(tempo1$problem == FALSE){
                data1[[i1]][, categ[[i1]]] <- base::factor(data1[[i1]][, categ[[i1]]]) # if already a factor, change nothing, if characters, levels according to alphabetical order
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") IN ", base::ifelse(base::length(categ)== 1L, "categ", base::paste0("ELEMENT ", i1, " OF categ ARGUMENT")), " IN ", base::ifelse(base::length(data1)== 1L, "data1 ARGUMENT", base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), ", THE CHARACTER COLUMN HAS BEEN CONVERTED TO FACTOR")
                warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
                
            }
            if(geom[[i1]] == "geom_vline" | geom[[i1]] == "geom_hline"){
                if(base::length(base::unique(data1[[i1]][, categ[[i1]]])) != base::nrow(data1[[i1]])){
                    tempo_cat <- base::paste0(error_text_start, "", base::ifelse(base::length(geom)== 1L, "geom OF data1 ARGUMENT", base::paste0("geom NUMBER ", i1, " OF DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), " ARGUMENT IS ", geom[[i1]], ", MEANING THAT ", base::ifelse(base::length(categ)== 1L, "categ OF data1 ARGUMENT", base::paste0("ELEMENT ", i1, " OF categ ARGUMENT IN DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), " MUST HAVE A DIFFERENT CLASS PER LINE OF data1 (ONE x VALUE PER CLASS)")
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                }
            }
        }else if(( ! base::is.null(categ)) & base::is.null(categ[[i1]])){ # base::is.null(categ[[i1]]) works even if categ is NULL # if categ[[i1]] = NULL, fake_categ will be created. WARNING: base::is.null(categ[[i1]]) means no legend display (see above), because categ has not been precised. This also means a single color for data1[[i1]]
            if(base::length(color[[i1]]) > 1){ # 0 means base::is.null(color[[i1]]) or base::is.null(color) and 1 is ok -> single color for data1[[i1]]
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") NULL ", base::ifelse(base::length(categ)== 1L, "categ", base::paste0("ELEMENT ", i1, " OF categ")), " ARGUMENT BUT CORRESPONDING COLORS IN ", base::ifelse(base::length(color)== 1L, "color", base::paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT")), " HAS LENGTH OVER 1\n", base::paste(color[[i1]], collapse = " "), "\nWHICH IS NOT COMPATIBLE WITH NULL CATEG -> COLOR RESET TO A SINGLE COLOR")
                warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
                color[i1] <- list(NULL) # will provide a single color below # Warning color[[i1]] <- NULL removes the compartment
            }
            categ[[i1]] <- "fake_categ"
            data1[[i1]] <- cbind(data1[[i1]], fake_categ = "", stringsAsFactors = TRUE)
            # inactivated because give a different color to different "Line_" categ while a single color for all the data1[[i1]] required. Thus, put back after the color management
            # if(geom[[i1]] == "geom_hline" | geom[[i1]] == "geom_vline"){
            # data1[[i1]][, "fake_categ"] <- base::paste0("Line_", 1:nrow(data1[[i1]]))
            # }else{
            data1[[i1]][, "fake_categ"] <- data1[[i1]][, "fake_categ"] # base::as.numeric("") create a vector of NA but class numeric
            # }
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") NULL ", base::ifelse(base::length(categ)== 1L, "categ", base::paste0("ELEMENT ", i1, " OF categ")), " ARGUMENT -> FOR DATA FRAME ", base::ifelse(base::length(data1)== 1L, "data1 ARGUMENT:", base::paste0("NUMBER ", i1, " OF data1 ARGUMENT:")), "\n- FAKE \"fake_categ\" COLUMN ADDED FILLED WITH \"\"(OR WITH \"Line_...\" FOR LINES)\n- SINGLE COLOR USED FOR PLOTTING\n- NO LEGEND DISPLAYED")
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
        }
        # OK: if categ is not NULL, all the non-null categ columns of data1 are factors from here
        
        # management of log scale and Inf removal
        if(x[[i1]] != "fake_x"){
            if(base::any(( ! base::is.finite(data1[[i1]][, x[[i1]]])) & ( ! base::is.na(data1[[i1]][, x[[i1]]])))){ # is.finite also detects NA: ( ! base::is.finite(data1[, y])) & ( ! base::is.na(data1[, y])) detects only Inf
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") PRESENCE OF -Inf OR Inf VALUES IN ", base::ifelse(base::length(categ)== 1L, "x", base::paste0("ELEMENT ", i1, " OF x ARGUMENT")), " IN ", base::ifelse(base::length(data1)== 1L, "data1 ARGUMENT", base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), " AND CORRESPONDING ROWS REMOVED (SEE $removed_row_nb AND $removed_rows)")
                warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
            }
        }
        if(y[[i1]] != "fake_y"){
            if(base::any(( ! base::is.finite(data1[[i1]][, y[[i1]]])) & ( ! base::is.na(data1[[i1]][, y[[i1]]])))){ # is.finite also detects NA: ( ! base::is.finite(data1[, y])) & ( ! base::is.na(data1[, y])) detects only Inf
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") PRESENCE OF -Inf OR Inf VALUES IN ", base::ifelse(base::length(categ)== 1L, "y", base::paste0("ELEMENT ", i1, " OF y ARGUMENT")), " IN ", base::ifelse(base::length(data1)== 1L, "data1 ARGUMENT", base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), " AND CORRESPONDING ROWS REMOVED (SEE $removed_row_nb AND $removed_rows)")
                warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
            }
        }
        # log conversion
        if(x_log != "no"){
            tempo1 <- ! base::is.finite(data1[[i1]][, x[[i1]]]) # where are initial NA and Inf
            data1[[i1]][, x[[i1]]] <- base::suppressWarnings(expr = get(x_log)(data1[[i1]][, x[[i1]]]))# no env = sys.nframe(), inherit = FALSE in get() because look for function in the classical scope
            if(base::any( ! (tempo1 | base::is.finite(data1[[i1]][, x[[i1]]])))){
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") LOG CONVERSION INTRODUCED -Inf OR Inf OR NaN VALUES IN ", base::ifelse(base::length(categ)== 1L, "x", base::paste0("ELEMENT ", i1, " OF x ARGUMENT")), " IN ", base::ifelse(base::length(data1)== 1L, "data1 ARGUMENT", base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), " AND CORRESPONDING ROWS REMOVED (SEE $removed_row_nb AND $removed_rows)")
                warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
            }
        }
        if(y_log != "no"){
            tempo1 <- ! base::is.finite(data1[[i1]][, y[[i1]]]) # where are initial NA and Inf
            data1[[i1]][, y[[i1]]] <- base::suppressWarnings(expr = get(y_log)(data1[[i1]][, y[[i1]]]))# no env = sys.nframe(), inherit = FALSE in get() because look for function in the classical scope
            if(base::any( ! (tempo1 | base::is.finite(data1[[i1]][, y[[i1]]])))){
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") LOG CONVERSION INTRODUCED -Inf OR Inf OR NaN VALUES IN ", base::ifelse(base::length(categ)== 1L, "y", base::paste0("ELEMENT ", i1, " OF y ARGUMENT")), " IN ", base::ifelse(base::length(data1)== 1L, "data1 ARGUMENT", base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), " AND CORRESPONDING ROWS REMOVED (SEE $removed_row_nb AND $removed_rows)")
                warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
            }
        }
        # Inf removal
        # removed_row_nb[[i1]] <- NULL # already NULL and Warning this removes the compartment
        removed_rows[[i1]] <- base::data.frame(stringsAsFactors = FALSE)
        if(base::any(( ! base::is.finite(data1[[i1]][, x[[i1]]])) & ( ! base::is.na(data1[[i1]][, x[[i1]]])))){ # is.finite also detects NA: ( ! base::is.finite(data1[[i1]][, x[[i1]]])) & ( ! base::is.na(data1[[i1]][, x[[i1]]])) detects only Inf
            removed_row_nb[[i1]] <- c(removed_row_nb[[i1]], base::which(( ! base::is.finite(data1[[i1]][, x[[i1]]])) & ( ! base::is.na(data1[[i1]][, x[[i1]]]))))
        }
        if(base::any(( ! base::is.finite(data1[[i1]][, y[[i1]]])) & ( ! base::is.na(data1[[i1]][, y[[i1]]])))){ # is.finite also detects NA: ( ! base::is.finite(data1[[i1]][, y[[i1]]])) & ( ! base::is.na(data1[[i1]][, y[[i1]]])) detects only Inf
            removed_row_nb[[i1]] <- c(removed_row_nb[[i1]], base::which(( ! base::is.finite(data1[[i1]][, y[[i1]]])) & ( ! base::is.na(data1[[i1]][, y[[i1]]]))))
        }
        if( ! base::is.null(removed_row_nb[[i1]])){
            removed_row_nb[[i1]] <- base::unique(removed_row_nb[[i1]]) # to remove the duplicated positions (NA in both x and y)
            removed_rows[[i1]] <- base::rbind(removed_rows[[i1]], data1_ini[[i1]][removed_row_nb[[i1]], ]) # here data1_ini used to have the y = O rows that will be removed because of Inf creation after log transformation
            data1[[i1]] <- data1[[i1]][-removed_row_nb[[i1]], ]
            data1_ini[[i1]] <- data1_ini[[i1]][-removed_row_nb[[i1]], ] #
        }
        # From here, data1 and data_ini have no more Inf
        # end Inf removal
        # x_lim and y_lim dealt later on, after the end f the loop
        # end management of log scale and Inf removal
        # na detection and removal
        column_check <- base::unique(base::unlist(c( # unlist because creates a list
            if(x[[i1]] == "fake_x"){NULL}else{x[[i1]]}, 
            if(y[[i1]] == "fake_y"){NULL}else{y[[i1]]}, 
            if( ! base::is.null(categ)){if(base::is.null(categ[[i1]])){
                # nocov start (unreachable: NULL categ compartments are converted to "fake_categ" earlier in this loop, so categ[[i1]] is never NULL here)
                NULL
                # nocov end
            }else{categ[[i1]]}}, 
            if( ! base::is.null(facet_categ)){if(base::is.null(facet_categ[[i1]])){
                # nocov start (unreachable: facet_categ is a length-1 list and can only be non-NULL with a single data frame, so facet_categ[[i1]] is never NULL here)
                NULL
                # nocov end
            }else{facet_categ[[i1]]}}
        ))) # dot_categ because can be a 3rd column of data1
        if(base::any(base::is.na(data1[[i1]][, column_check]))){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") NA DETECTED IN COLUMNS ", base::paste(column_check, collapse = " "), " OF ", base::ifelse(base::length(data1)== 1L, "data1 ARGUMENT", base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), " AND CORRESPONDING ROWS REMOVED (SEE $removed_row_nb AND $removed_rows)")
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
            for(i3 in 1:length(column_check)){
                if(base::any(base::is.na(data1[[i1]][, column_check[i3]]))){
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("NA REMOVAL DUE TO COLUMN ", column_check[i3], " OF ", base::ifelse(base::length(data1)== 1L, "data1 ARGUMENT", base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")))
                    warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
                }
            }
            tempo <- base::unique(base::unlist(base::lapply(base::lapply(c(data1[[i1]][column_check]), FUN = is.na), FUN = which)))
            removed_row_nb[[i1]] <- c(removed_row_nb[[i1]], tempo)
            removed_rows[[i1]] <- base::rbind(removed_rows[[i1]], data1_ini[[i1]][tempo, ]) #  # tempo used because removed_row_nb is not empty. Here data1_ini used to have the non NA rows that will be removed because of NAN creation after log transformation (neg values for instance)
            column_check <- column_check[ ! (column_check == x[[i1]] | column_check == y[[i1]])] # remove x and y to keep quali columns
            if(base::length(tempo) != 0){
                data1[[i1]] <- data1[[i1]][-tempo, ] # WARNING tempo here and not removed_row_nb because the latter contain more numbers thant the former
                data1_ini[[i1]] <- data1_ini[[i1]][-tempo, ] # WARNING tempo here and not removed_row_nb because the latter contain more numbers than the former
                for(i4 in seq_len(length(x = column_check))){ # seq_len() because column_check can be empty (when only x and y columns had NA), and 1:length() would then loop on c(1, 0)
                    if(base::any( ! base::unique(removed_rows[[i1]][, column_check[i4]]) %in% base::unique(data1[[i1]][, column_check[i4]]))){
                        warn_count <- warn_count + 1
                        tempo_warn <- base::paste0("(", warn_count,") IN COLUMN ", column_check[i4], " OF ", base::ifelse(base::length(data1)== 1L, "data1 ARGUMENT", base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), ", THE FOLLOWING CLASSES HAVE DISAPPEARED AFTER NA REMOVAL\n(IF COLUMN USED IN THE PLOT, THIS CLASS WILL NOT BE DISPLAYED):\n", base::paste(base::unique(removed_rows[[i1]][, column_check[i4]])[ ! base::unique(removed_rows[[i1]][, column_check[i4]]) %in% base::unique(data1[[i1]][, column_check[i4]])], collapse = " "))
                        warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
                        tempo_levels <- base::levels(data1[[i1]][, column_check[i4]])[base::levels(data1[[i1]][, column_check[i4]]) %in% base::unique(base::as.character(data1[[i1]][, column_check[i4]]))]
                        data1[[i1]][, column_check[i4]] <- base::factor(base::as.character(data1[[i1]][, column_check[i4]]), levels = tempo_levels)
                        if(column_check[i4] %in% categ[[i1]] & ! base::is.null(categ_class_order)){
                            categ_class_order[[i1]] <- base::levels(data1[[i1]][, column_check[i4]])[base::levels(data1[[i1]][, column_check[i4]]) %in% base::unique(data1[[i1]][, column_check[i4]])] # remove the absent class in the categ_class_order vector
                            data1[[i1]][, column_check[i4]] <- base::factor(base::as.character(data1[[i1]][, column_check[i4]]), levels = base::unique(categ_class_order[[i1]]))
                        }
                    }
                }
            }
        }
        # end na detection and removal
        # From here, data1 and data_ini have no more NA or NaN in x, y, categ (if categ != NULL) and facet_categ (if categ != NULL)
        if( ! base::is.null(categ_class_order)){
            # the following check will be done several times but I prefer to keep it here, after the creation of categ
            if(base::is.null(categ[[i1]]) & ! base::is.null(categ_class_order[[i1]])){
                tempo_cat <- base::paste0(error_text_start, "COMPARTMENT ", i1, " OF categ ARGUMENT CANNOT BE NULL IF COMPARTMENT ", i1, " OF categ_class_order ARGUMENT IS NOT NULL: ", base::paste(categ_class_order, collapse = " "))
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                if(base::is.null(categ_class_order[[i1]])){
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") THE categ_class_order COMPARTMENT ", i1, " IS NULL. ALPHABETICAL ORDER WILL BE APPLIED")
                    warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
                    data1[[i1]][, categ[[i1]]] <- base::factor(base::as.character(data1[[i1]][, categ[[i1]]])) # if already a factor, change nothing, if characters, levels according to alphabetical order
                    categ_class_order[[i1]] <- base::levels(data1[[i1]][, categ[[i1]]]) # character vector that will be used later
                }else{
                    tempo <- saferDev::arg_check(data = categ_class_order[[i1]], data_name = base::paste0("COMPARTMENT ", i1 , " OF categ_class_order ARGUMENT"), class = NULL, typeof = "character", mode = NULL, length = base::length(base::levels(data1[[i1]][, categ[[i1]]])), prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) # base::length(data1[, categ[i1]) -> if data1[, categ[i1] was initially character vector, then conversion as factor after the NA removal, thus class number ok. If data1[, categ[i1] was initially factor, no modification after the NA removal, thus class number ok
                    if(tempo$problem == TRUE){
                        base::stop(base::paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
                    }
                }
                if(base::any(base::duplicated(categ_class_order[[i1]]))){
                    tempo_cat <- base::paste0(error_text_start, "COMPARTMENT ", i1, " OF categ_class_order ARGUMENT CANNOT HAVE DUPLICATED CLASSES: ", base::paste(categ_class_order[[i1]], collapse = " "))
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                }else if( ! (base::all(categ_class_order[[i1]] %in% base::unique(data1[[i1]][, categ[[i1]]])) & base::all(base::unique(data1[[i1]][, categ[[i1]]]) %in% categ_class_order[[i1]]))){
                    tempo_cat <- base::paste0(error_text_start, "COMPARTMENT ", i1, " OF categ_class_order ARGUMENT MUST BE CLASSES OF COMPARTMENT ", i1, " OF categ ARGUMENT\nHERE IT IS:\n", base::paste(categ_class_order[[i1]], collapse = " "), "\nFOR COMPARTMENT ", i1, " OF categ_class_order AND IT IS:\n", base::paste(base::unique(data1[[i1]][, categ[[i1]]]), collapse = " "), "\nFOR COLUMN ", categ[[i1]], " OF data1 NUMBER ", i1)
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                }else{
                    data1[[i1]][, categ[[i1]]] <- base::factor(data1[[i1]][, categ[[i1]]], levels = categ_class_order[[i1]]) # reorder the factor
                }
                base::names(categ_class_order)[i1] <- categ[[i1]]
            }
        }
        # OK: if categ_class_order is not NULL, all the NULL categ_class_order columns of data1 are character from here
        
        if( ! base::is.null(legend_name[[i1]])){
            tempo <- saferDev::arg_check(data = legend_name[[i1]], data_name = base::ifelse(base::length(legend_name)== 1L, "legend_name", base::paste0("legend_name NUMBER ", i1)), class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
            if(tempo$problem == TRUE){
                base::stop(base::paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
            }
        }
        if( ! base::is.null(color)){ # if color is NULL, will be filled later on
            # check the nature of color
            if(base::is.null(color[[i1]])){
                compart_null_color <- compart_null_color + 1
                color[[i1]] <- grey(compart_null_color / 8) # cannot be more than 7 overlays. Thus 7 different greys. 8/8 is excluded because white dots
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") NULL COLOR IN ", base::ifelse(base::length(color)== 1L, "color", base::paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT")), " ASSOCIATED TO ", base::ifelse(base::length(data1)== 1L, "data1 ARGUMENT", base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), ", SINGLE COLOR ", base::paste(color[[i1]], collapse = " "), " HAS BEEN ATTRIBUTED")
                warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
            }
            tempo1 <- saferDev::arg_check(data = color[[i1]], data_name = base::ifelse(base::length(color)== 1L, "color", base::paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT")), class = NULL, typeof = "character", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) # na_contain = TRUE in case of colum of data1
            tempo2 <- saferDev::arg_check(data = color[[i1]], data_name = base::ifelse(base::length(color)== 1L, "color", base::paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT")), class = "factor", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) # idem
            if(tempo1$problem == TRUE & tempo2$problem == TRUE){
                tempo_cat <- base::paste0(error_text_start, "", base::ifelse(base::length(color)== 1L, "color", base::paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT")), " MUST BE A FACTOR OR CHARACTER VECTOR OR INTEGER VECTOR") # integer possible because dealt above
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else if( ! (base::all(color[[i1]] %in% colors() | base::grepl(pattern = "^#", color[[i1]])))){ # check that all strings of color start by #
                tempo_cat <- base::paste0(error_text_start, "", base::ifelse(base::length(color)== 1L, "color", base::paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT")), " MUST BE A HEXADECIMAL COLOR VECTOR STARTING BY # AND/OR COLOR NAMES GIVEN BY colors() OR A COLUMN NAME OF THE data1 PARAMETER: ", base::paste(base::unique(color[[i1]]), collapse = " "))
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
            # nocov start (unreachable: the is.na(color) check at the top of the integer-colors loop (before color management) errors for any NA in color)
            if(base::any(base::is.na(color[[i1]]))){
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") IN ", base::ifelse(base::length(color)== 1L, "color", base::paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT")), ", THE COLORS:\n", base::paste(base::unique(color[[i1]]), collapse = " "), "\nCONTAINS NA")
                warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
            # nocov end
            }
            # end check the nature of color
            # check the length of color
            if(base::is.null(categ) & base::length(color[[i1]]) != 1){
                tempo_cat <- base::paste0(error_text_start, "", base::ifelse(base::length(color)== 1L, "color", base::paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT")), " MUST BE A SINGLE COLOR IF categ IS NULL")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else if( ! base::is.null(categ)){
                # No problem of NA management by ggplot2 because already removed
            # nocov start (unreachable: color is reset to NULL when categ is fake (see the categ NULL management above), so its length is always 1 after the grey attribution)
                if(categ[[i1]] == "fake_categ" & base::length(color[[i1]]) != 1){
                    tempo_cat <- base::paste0(error_text_start, "", base::ifelse(base::length(color)== 1L, "color", base::paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT")), " MUST BE A SINGLE COLOR IF ", base::ifelse(base::length(categ)== 1L, "categ", base::paste0("ELEMENT ", i1, " OF categ ARGUMENT")), " IS NULL")
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
                }else if(base::length(color[[i1]]) == base::length(base::unique(data1[[i1]][, categ[[i1]]]))){ # here base::length(color) is equal to the different number of categ
                    data1[[i1]][, categ[[i1]]] <- base::factor(data1[[i1]][, categ[[i1]]]) # if already a factor, change nothing, if characters, levels according to alphabetical order
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") IN ", base::ifelse(base::length(categ)== 1L, "categ", base::paste0("ELEMENT ", i1, " OF categ ARGUMENT")), " IN ", base::ifelse(base::length(data1)== 1L, "data1 ARGUMENT", base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), ", THE FOLLOWING COLORS:\n", base::paste(color[[i1]], collapse = " "), "\nHAVE BEEN ATTRIBUTED TO THESE CLASSES:\n", base::paste(base::levels(base::factor(data1[[i1]][, categ[[i1]]])), collapse = " "))
                    warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
                }else if(base::length(color[[i1]]) == base::length(data1[[i1]][, categ[[i1]]])){# here base::length(color) is equal to base::nrow(data1[[i1]]) -> Modif to have base::length(color) equal to the different number of categ (base::length(color) == base::length(base::levels(data1[[i1]][, categ[[i1]]])))
                    data1[[i1]] <- cbind(data1[[i1]], color = color[[i1]], stringsAsFactors = TRUE)
                    tempo_check <- base::unique(data1[[i1]][ , c(categ[[i1]], "color")])
                    if( ! (base::nrow(data1[[i1]]) == base::length(color[[i1]]) & base::nrow(tempo_check) == base::length(base::unique(data1[[i1]][ , categ[[i1]]])))){
                        tempo_cat <- base::paste0(error_text_start, "", base::ifelse(base::length(color)== 1L, "color", base::paste0("ELEMENT NUMBER ", i1, " OF color")), " ARGUMENT HAS THE LENGTH OF ", base::ifelse(base::length(categ)== 1L, "categ", base::paste0("ELEMENT ", i1, " OF categ ARGUMENT")), " IN ", base::ifelse(base::length(data1)== 1L, "data1 ARGUMENT", base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), "\nBUT IS INCORRECTLY ASSOCIATED TO EACH CLASS OF THIS categ:\n", base::paste(base::unique(base::mapply(FUN = "paste", data1[[i1]][ ,categ[[i1]]], data1[[i1]][ ,"color"])), collapse = "\n"))
                        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                    }else{
                        data1[[i1]][, categ[[i1]]] <- base::factor(data1[[i1]][, categ[[i1]]]) # if already a factor, change nothing, if characters, levels according to alphabetical order
                        color[[i1]] <- base::unique(color[[i1]][base::order(data1[[i1]][, categ[[i1]]])]) # Modif to have base::length(color) equal to the different number of categ (base::length(color) == base::length(base::levels(data1[[i1]][, categ[[i1]]])))
                        warn_count <- warn_count + 1
                        tempo_warn <- base::paste0("(", warn_count, ") FROM FUNCTION ", function_name, ": ", base::ifelse(base::length(color)== 1L, "color", base::paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT")), " HAS THE LENGTH OF ", base::ifelse(base::length(categ)== 1L, "categ", base::paste0("ELEMENT ", i1, " OF categ ARGUMENT")), " IN ", base::ifelse(base::length(data1)== 1L, "data1 ARGUMENT", base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), " COLUMN VALUES\nCOLORS HAVE BEEN RESPECTIVELY ASSOCIATED TO EACH CLASS OF categ AS:\n", base::paste(base::levels(base::factor(data1[[i1]][, categ[[i1]]])), collapse = " "), "\n", base::paste(color[[i1]], collapse = " "))
                        warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
                    }
                }else if(base::length(color[[i1]])== 1L){
                    data1[[i1]][, categ[[i1]]] <- base::factor(data1[[i1]][, categ[[i1]]]) # if already a factor, change nothing, if characters, levels according to alphabetical order
                    color[[i1]] <- base::rep(color[[i1]], base::length(base::levels(data1[[i1]][, categ[[i1]]])))
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") IN ", base::ifelse(base::length(categ)== 1L, "categ", base::paste0("ELEMENT ", i1, " OF categ ARGUMENT")), " IN ", base::ifelse(base::length(data1)== 1L, "data1 ARGUMENT", base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), ", COLOR HAS LENGTH 1 MEANING THAT ALL THE DIFFERENT CLASSES OF ", base::ifelse(base::length(categ)== 1L, "categ", base::paste0("ELEMENT ", i1, " OF categ ARGUMENT")), "\n", base::paste(base::levels(base::factor(data1[[i1]][, categ[[i1]]])), collapse = " "), "\nWILL HAVE THE SAME COLOR\n", base::paste(color[[i1]], collapse = " "))
                    warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
                }else{
                    tempo_cat <- base::paste0(error_text_start, "", base::ifelse(base::length(color)== 1L, "color", base::paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT")), " MUST BE\n(1) LENGTH 1\nOR (2) THE LENGTH OF ", base::ifelse(base::length(categ)== 1L, "categ", base::paste0("ELEMENT ", i1, " OF categ ARGUMENT")), " IN ", base::ifelse(base::length(data1)== 1L, "data1 ARGUMENT", base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), " COLUMN VALUES\nOR (3) THE LENGTH OF THE CLASSES IN THIS COLUMN\nHERE IT IS COLOR LENGTH ", base::length(color[[i1]]), " VERSUS CATEG LENGTH ", base::length(data1[[i1]][, categ[[i1]]]), " AND CATEG CLASS LENGTH ", base::length(base::unique(data1[[i1]][, categ[[i1]]])), "\nPRESENCE OF NA IN THE COLUMN x, y OR categ OF data1 COULD BE THE PROBLEME")
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                }
            }
        }
        if((geom[[i1]] == "geom_hline" | geom[[i1]] == "geom_vline") & ! base::is.null(categ[[i1]])){ # add here after the color management, to deal with the different lines to plot inside any data[[i1]]
            if(categ[[i1]] == "fake_categ"){
                data1[[i1]][, "fake_categ"] <- base::factor(base::paste0("Line_", formatC(1:nrow(data1[[i1]]), width = base::nchar(base::nrow(data1[[i1]])), flag = "0")))
            }
        }
        tempo <- saferDev::arg_check(data = alpha[[i1]], data_name = base::ifelse(base::length(alpha)== 1L, "alpha", base::paste0("alpha NUMBER ", i1)), prop = TRUE, class = NULL, typeof = NULL, mode = NULL, length = 1, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        if(tempo$problem == TRUE){
            base::stop(base::paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
    }
    # end loop (checking inside list compartment)
    if(base::length(data1) > 1){
        if(base::length(base::unique(base::unlist(x)[ ! x == "fake_x"])) > 1){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") THE x ARGUMENT DOES NOT CONTAIN IDENTICAL COLUMN NAMES:\n", base::paste(base::unlist(x), collapse = " "), "\nX-AXIS OVERLAYING DIFFERENT VARIABLES?")
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
        }
    }
    if(base::length(data1) > 1){
        if(base::length(base::unique(base::unlist(y)[ ! y == "fake_y"])) > 1){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") THE y ARGUMENT DOES NOT CONTAIN IDENTICAL COLUMN NAMES:\n", base::paste(base::unlist(y), collapse = " "), "\nY-AXIS OVERLAYING DIFFERENT VARIABLES?")
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
        }
    }
    if(base::sum(geom %in% "geom_point") > 3){
        tempo_cat <- base::paste0(error_text_start, "geom ARGUMENT CANNOT HAVE MORE THAN THREE \"geom_point\" ELEMENTS")
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }else if(base::length(geom) - base::sum(geom %in% "geom_point") > 3){
        tempo_cat <- base::paste0(error_text_start, "geom ARGUMENT CANNOT HAVE MORE THAN THREE LINE ELEMENTS")
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    # x_lim management before transfo by x_log
    if(x_log != "no" & ! base::is.null(x_lim)){
        if(base::any(x_lim <= 0)){
            tempo_cat <- base::paste0(error_text_start, "x_lim ARGUMENT CANNOT HAVE ZERO OR NEGATIVE VALUES WITH THE x_log ARGUMENT SET TO ", x_log, ":\n", base::paste(x_lim, collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(base::any( ! base::is.finite(if(x_log == "log10"){base::log10(x_lim)}else{log2(x_lim)}))){
            tempo_cat <- base::paste0(error_text_start, "x_lim ARGUMENT RETURNS INF/NA WITH THE x_log ARGUMENT SET TO ", x_log, "\nAS SCALE COMPUTATION IS ", base::ifelse(x_log == "log10", "log10", "log2"), ":\n", base::paste(if(x_log == "log10"){base::log10(x_lim)}else{log2(x_lim)}, collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    if(x_log != "no" & x_include_zero == TRUE){
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count,") x_log ARGUMENT SET TO ", x_log, " AND x_include_zero ARGUMENT SET TO TRUE -> x_include_zero ARGUMENT RESET TO FALSE BECAUSE 0 VALUE CANNOT BE REPRESENTED IN LOG SCALE")
        warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
        x_include_zero <- FALSE
    }
    # end x_lim management before transfo by x_log
    # y_lim management before transfo by y_log
    if(y_log != "no" & ! base::is.null(y_lim)){
        if(base::any(y_lim <= 0)){
            tempo_cat <- base::paste0(error_text_start, "y_lim ARGUMENT CANNOT HAVE ZERO OR NEGATIVE VALUES WITH THE y_log ARGUMENT SET TO ", y_log, ":\n", base::paste(y_lim, collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(base::any( ! base::is.finite(if(y_log == "log10"){base::log10(y_lim)}else{log2(y_lim)}))){
            tempo_cat <- base::paste0(error_text_start, "y_lim ARGUMENT RETURNS INF/NA WITH THE y_log ARGUMENT SET TO ", y_log, "\nAS SCALE COMPUTATION IS ", base::ifelse(y_log == "log10", "log10", "log2"), ":\n", base::paste(if(y_log == "log10"){base::log10(y_lim)}else{log2(y_lim)}, collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    if(y_log != "no" & y_include_zero == TRUE){
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count,") y_log ARGUMENT SET TO ", y_log, " AND y_include_zero ARGUMENT SET TO TRUE -> y_include_zero ARGUMENT RESET TO FALSE BECAUSE 0 VALUE CANNOT BE REPRESENTED IN LOG SCALE")
        warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
        y_include_zero <- FALSE
    }
    # end y_lim management before transfo by y_log
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    # axes management
    if(base::is.null(x_lim)){
        if(base::any(base::unlist(base::mapply(FUN = "[[", data1, x, SIMPLIFY = FALSE)) %in% c(Inf, -Inf))){
            # nocov start (unreachable: -Inf and Inf values of the x columns are removed by the per-compartment loop above, so no -Inf/Inf value can remain here)
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") THE x COLUMN IN data1 CONTAINS -Inf OR Inf VALUES THAT WILL NOT BE CONSIDERED IN THE PLOT RANGE")
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
            # nocov end
        }
        x_lim <- base::suppressWarnings(expr = range(base::unlist(base::mapply(FUN = "[[", data1, x, SIMPLIFY = FALSE)), na.rm = TRUE, finite = TRUE)) # finite = TRUE removes all the -Inf and Inf except if only this. In that case, whatever the -Inf and/or Inf present, output -Inf;Inf range. Idem with NA only. y_lim added here. If NULL, ok if y argument has values
    }else if(x_log != "no"){
        x_lim <- get(x_log)(x_lim) # no env = sys.nframe(), inherit = FALSE in get() because look for function in the classical scope
    }
    if(x_log != "no"){
        if(base::any( ! base::is.finite(x_lim))){
            # nocov start (unreachable: x_lim computed from data is finite after the Inf/NaN row removal (an all-non-finite column empties data1, which errors earlier))
            tempo_cat <- base::paste0(error_text_start, "x_lim ARGUMENT CANNOT HAVE ZERO OR NEGATIVE VALUES WITH THE x_log ARGUMENT SET TO ", x_log, ":\n", base::paste(x_lim, collapse = " "), "\nPLEASE, CHECK DATA VALUES (PRESENCE OF ZERO OR INF VALUES)")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }
    }
    if(base::suppressWarnings(expr = base::all(x_lim %in% c(Inf, -Inf)))){ # happen when x is only NULL
        if(base::all(base::unlist(geom) %in% c("geom_vline", "geom_stick"))){
            tempo_cat <- base::paste0(error_text_start, "NOT POSSIBLE TO DRAW geom_vline OR geom_stick KIND OF LINES ALONE IF x_lim ARGUMENT IS SET TO NULL, SINCE NO X-AXIS DEFINED (", base::ifelse(base::length(x)== 1L, "x", base::paste0("ELEMENT ", i1, " OF x")), " ARGUMENT MUST BE NULL FOR THESE KIND OF LINES)")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            tempo_cat <- base::paste0(error_text_start, "x_lim ARGUMENT MADE OF NA, -Inf OR Inf ONLY: ", base::paste(x_lim, collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    x_lim_order <- base::order(x_lim) # to deal with inverse axis
    # print(x_lim)
    x_lim <- base::sort(x_lim)
    x_lim[1] <- x_lim[1] - base::abs(x_lim[2] - x_lim[1]) * base::ifelse(diff(x_lim_order) > 0, x_right_extra_margin, x_left_extra_margin) # diff(x_lim_order) > 0 means not inversed axis
    x_lim[2] <- x_lim[2] + base::abs(x_lim[2] - x_lim[1]) * base::ifelse(diff(x_lim_order) > 0, x_left_extra_margin, x_right_extra_margin) # diff(x_lim_order) > 0 means not inversed axis
    if(x_include_zero == TRUE){ # no need to check x_log != "no" because done before
        x_lim <- range(c(x_lim, 0), na.rm = TRUE, finite = TRUE) # finite = TRUE removes all the -Inf and Inf except if only this. In that case, whatever the -Inf and/or Inf present, output -Inf;Inf range. Idem with NA only
    }
    x_lim <- x_lim[x_lim_order]
    if(base::any(base::is.na(x_lim))){
            # nocov start (unreachable: x_lim is finite at this stage (checked above) and the margin computation cannot create NA)
        tempo_cat <- base::paste0(error_text_start, "CODE INCONSISTENCY 3")
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
    }
    if(base::is.null(y_lim)){
        if(base::any(base::unlist(base::mapply(FUN = "[[", data1, y, SIMPLIFY = FALSE)) %in% c(Inf, -Inf))){
            # nocov start (unreachable: -Inf and Inf values of the y columns are removed by the per-compartment loop above, so no -Inf/Inf value can remain here)
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") THE y COLUMN IN data1 CONTAINS -Inf OR Inf VALUES THAT WILL NOT BE CONSIDERED IN THE PLOT RANGE")
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
            # nocov end
        }
        y_lim <- base::suppressWarnings(expr = range(base::unlist(base::mapply(FUN = "[[", data1, y, SIMPLIFY = FALSE)), na.rm = TRUE, finite = TRUE)) # finite = TRUE removes all the -Inf and Inf except if only this. In that case, whatever the -Inf and/or Inf present, output -Inf;Inf range. Idem with NA only. y_lim added here. If NULL, ok if y argument has values
    }else if(y_log != "no"){
        y_lim <- get(y_log)(y_lim) # no env = sys.nframe(), inherit = FALSE in get() because look for function in the classical scope
    }
    if(y_log != "no"){
        if(base::any( ! base::is.finite(y_lim))){
            # nocov start (unreachable: idem as x_lim (see line 1596))
            tempo_cat <- base::paste0(error_text_start, "y_lim ARGUMENT CANNOT HAVE ZERO OR NEGATIVE VALUES WITH THE y_log ARGUMENT SET TO ", y_log, ":\n", base::paste(y_lim, collapse = " "), "\nPLEASE, CHECK DATA VALUES (PRESENCE OF ZERO OR INF VALUES)")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }
    }
    if(base::suppressWarnings(expr = base::all(y_lim %in% c(Inf, -Inf)))){ # happen when y is only NULL
        if(base::all(base::unlist(geom) == "geom_vline")){
            tempo_cat <- base::paste0(error_text_start, "NOT POSSIBLE TO DRAW geom_vline KIND OF LINES ALONE IF y_lim ARGUMENT IS SET TO NULL, SINCE NO Y-AXIS DEFINED (", base::ifelse(base::length(y)== 1L, "y", base::paste0("ELEMENT ", i1, " OF y")), " ARGUMENT MUST BE NULL FOR THESE KIND OF LINES)")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            # nocov start (unreachable: y_lim contains Inf only when y is all-NA or all-Inf, which empties data1 and errors at the x_lim all-Inf check above)
            tempo_cat <- base::paste0(error_text_start, "y_lim ARGUMENT MADE OF NA, -Inf OR Inf ONLY: ", base::paste(y_lim, collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }
    }
    y_lim_order <- base::order(y_lim) # to deal with inverse axis
    y_lim <- base::sort(y_lim)
    y_lim[1] <- y_lim[1] - base::abs(y_lim[2] - y_lim[1]) * base::ifelse(diff(y_lim_order) > 0, y_bottom_extra_margin, y_top_extra_margin) # diff(y_lim_order) > 0 means not inversed axis
    y_lim[2] <- y_lim[2] + base::abs(y_lim[2] - y_lim[1]) * base::ifelse(diff(y_lim_order) > 0, y_top_extra_margin, y_bottom_extra_margin) # diff(y_lim_order) > 0 means not inversed axis
    if(y_include_zero == TRUE){ # no need to check y_log != "no" because done before
        y_lim <- range(c(y_lim, 0), na.rm = TRUE, finite = TRUE) # finite = TRUE removes all the -Inf and Inf except if only this. In that case, whatever the -Inf and/or Inf present, output -Inf;Inf range. Idem with NA only
    }
    y_lim <- y_lim[y_lim_order]
            # nocov start (unreachable: idem as x_lim (see line 1619))
    if(base::any(base::is.na(y_lim))){
        tempo_cat <- base::paste0(error_text_start, "CODE INCONSISTENCY 4")
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
    }
    # end axes management
    
    
    
    
    # create a fake categ if NULL to deal with legend display
    if(base::is.null(categ)){
        categ <- base::vector("list", base::length(data1))
        categ[] <- "fake_categ"
        for(i2 in 1:length(data1)){
            data1[[i2]] <- cbind(data1[[i2]], fake_categ = "", stringsAsFactors = TRUE)
            if(geom[[i2]] == "geom_hline" | geom[[i2]] == "geom_vline"){
                data1[[i2]][, "fake_categ"] <- base::factor(base::paste0("Line_", 1:nrow(data1[[i2]])))
            }
        }
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count,") NULL categ ARGUMENT -> FAKE \"fake_categ\" COLUMN ADDED TO EACH DATA FRAME OF data1, AND FILLED WITH \"\"")
        warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
    }
    # categ is not NULL anymore
    if(base::is.null(categ_class_order)){
        categ_class_order <- base::vector("list", length = base::length(data1))
        tempo_categ_class_order <- NULL
        for(i2 in 1:length(categ_class_order)){
            categ_class_order[[i2]] <- base::levels(data1[[i2]][, categ[[i2]]])
            base::names(categ_class_order)[i2] <- categ[[i2]]
            tempo_categ_class_order <- c(tempo_categ_class_order, base::ifelse(i2 != 1, "\n", ""), categ_class_order[[i2]])
        }
        if(base::any(base::unlist(legend_disp))){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") THE categ_class_order SETTING IS NULL. ALPHABETICAL ORDER WILL BE APPLIED FOR CLASS ORDERING:\n", base::paste(tempo_categ_class_order, collapse = " "))
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
        }
    }
    # end create a fake categ if NULL to deal with legend display
    # categ_class_order is not NULL anymore
    
    
    # vector of color with length as in base::levels(categ) of data1
    if(base::is.null(color)){
        color <- base::vector("list", base::length(data1))
        length_categ_list <- base::lapply(base::lapply(base::mapply(FUN = "[[", data1, categ, SIMPLIFY = FALSE), FUN = unique), FUN = function(x){base::length(x[ ! base::is.na(x)])})
        length_categ_list[base::sapply(categ, FUN = "==", "fake_categ")] <- 1 # when base::is.null(color), a single color for all the dots or lines of data[[i1]] that contain "fake_categ" category
        total_categ_length <- base::sum(base::unlist(length_categ_list), na.rm = TRUE)
        tempo_color <- gg_palette(total_categ_length)
        tempo_count <- 0
        for(i2 in 1:length(data1)){
            color[[i2]] <- tempo_color[(1:length_categ_list[[i2]]) + tempo_count]
            tempo_count <- tempo_count + length_categ_list[[i2]]
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") NULL color ARGUMENT -> COLORS RESPECTIVELY ATTRIBUTED TO EACH CLASS OF ", base::ifelse(base::length(categ)== 1L, "categ", base::paste0("ELEMENT ", i2, " OF categ ARGUMENT")), " (", categ[[i2]], ") IN ", base::ifelse(base::length(data1)== 1L, "data1 ARGUMENT", base::paste0("DATA FRAME NUMBER ", i2, " OF data1 ARGUMENT")), ":\n", base::paste(color[[i2]], collapse = " "), "\n", base::paste(if(base::all(base::levels(data1[[i2]][, categ[[i2]]]) == "")){'\"\"'}else{base::levels(data1[[i2]][, categ[[i2]]])}, collapse = " "))
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
        }
    }
    # end vector of color with length as in base::levels(categ) of data1
    # color is not NULL anymore
    
    
    
    
    
    # last check
    for(i1 in 1:length(data1)){
            # nocov start (unreachable: color length always matches the number of classes after the color management above)
        if(categ[[i1]] != "fake_categ" & base::length(color[[i1]]) != base::length(base::unique(data1[[i1]][, categ[[i1]]]))){
            tempo_cat <- base::paste0(error_text_start, "LAST CHECK: ", base::ifelse(base::length(color)== 1L, "color", base::paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT")), " MUST HAVE THE LENGTH OF LEVELS OF ", base::ifelse(base::length(categ)== 1L, "categ", base::paste0("ELEMENT ", i1, " OF categ ARGUMENT")), " IN ", base::ifelse(base::length(data1)== 1L, "data1 ARGUMENT", base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), "\nHERE IT IS COLOR LENGTH ", base::length(color[[i1]]), " VERSUS CATEG LEVELS LENGTH ", base::length(base::unique(data1[[i1]][, categ[[i1]]])), "\nREMINDER: A SINGLE COLOR PER CLASS OF CATEG AND A SINGLE CLASS OF CATEG PER COLOR MUST BE RESPECTED")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(categ[[i1]] == "fake_categ" & base::length(color[[i1]]) != 1){
            tempo_cat <- base::paste0(error_text_start, "LAST CHECK: ", base::ifelse(base::length(color)== 1L, "color", base::paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT")), " MUST HAVE LENGTH 1 WHEN ", base::ifelse(base::length(categ)== 1L, "categ", base::paste0("ELEMENT ", i1, " OF categ ARGUMENT")), " IS NULL\nHERE IT IS COLOR LENGTH ", base::length(color[[i1]]))
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }
    }
    # end last check
    
    
    
    
    
    # conversion of geom_hline and geom_vline
    for(i1 in 1:length(data1)){
        if(geom[[i1]] == "geom_hline" | geom[[i1]] == "geom_vline"){
            final_data_frame <- base::data.frame()
            for(i3 in 1:nrow(data1[[i1]])){
                tempo_data_frame <- base::rbind(data1[[i1]][i3, ], data1[[i1]][i3, ], stringsAsFactors = TRUE)
                if(geom[[i1]] == "geom_hline"){
                    tempo_data_frame[, x[[i1]]] <- x_lim
                }else if(geom[[i1]] == "geom_vline"){
                    tempo_data_frame[, y[[i1]]] <- y_lim
                }else{
            # nocov start (unreachable: geom is checked to be geom_hline or geom_vline before entering the loop)
                    tempo_cat <- base::paste0(error_text_start, "CODE INCONSISTENCY 5")
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
                }
                # 3 lines below inactivated because I put that above
                # if(base::is.null(categ[[i1]])){
                # data1[, "fake_categ"] <- base::paste0("Line_", i3)
                # }
                final_data_frame <- base::rbind(final_data_frame, tempo_data_frame, stringsAsFactors = TRUE)
            }
            data1[[i1]] <- final_data_frame
            geom[[i1]] <- "geom_line"
            if(base::length(color[[i1]])== 1L){
                color[[i1]] <- base::rep(color[[i1]], base::length(base::unique(data1[[i1]][ , categ[[i1]]])))
            # nocov start (unreachable: the last check above ensures color length equals the number of classes)
            }else if(base::length(color[[i1]]) != base::length(base::unique(data1[[i1]][ , categ[[i1]]]))){
                tempo_cat <- base::paste0(error_text_start, "geom_hline AND geom_vline CONVERSION TO FIT THE XLIM AND YLIM LIMITS OF THE DATA: ", base::ifelse(base::length(color)== 1L, "color", base::paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT")), " MUST HAVE THE LENGTH OF LEVELS OF ", base::ifelse(base::length(categ)== 1L, "categ", base::paste0("ELEMENT ", i1, " OF categ ARGUMENT")), " IN ", base::ifelse(base::length(data1)== 1L, "data1 ARGUMENT", base::paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), "\nHERE IT IS COLOR LENGTH ", base::length(color[[i1]]), " VERSUS CATEG LEVELS LENGTH ", base::length(base::unique(data1[[i1]][, categ[[i1]]])))
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
            }
        }
    }
    # end conversion of geom_hline and geom_vline
    
    
    
    
    # kind of geom_point (vectorial or raster)
    scatter_kind <- base::vector("list", length = base::length(data1)) # list of same length as data1, that will be used to use either ggplot2::geom_point() (vectorial dot layer) or gg_point_rast() (raster dot layer)
    fix_ratio <- FALSE
    if(base::is.null(raster_threshold)){
        if(raster == TRUE){
            scatter_kind[] <- "gg_point_rast" # not important to fill everything: will be only used when geom == "geom_point"
            fix_ratio <- TRUE
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") RASTER PLOT GENERATED -> ASPECT RATIO OF THE PLOT REGION SET BY THE raster_ratio ARGUMENT (", saferTool::round2(data = raster_ratio, dec_nb = 2, after_lead_zero = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text), ") TO AVOID A BUG OF ELLIPSOID DOT DRAWING")
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
        }else{
            scatter_kind[] <- "ggplot2::geom_point"
        }
    }else{
        for(i2 in 1:length(data1)){
            if(geom[[i2]] == "geom_point"){
                if(base::nrow(data1[[i2]]) <= raster_threshold){
                    scatter_kind[[i2]] <- "ggplot2::geom_point"
                }else{
                    scatter_kind[[i2]] <- "gg_point_rast"
                    fix_ratio <- TRUE
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") ", base::ifelse(base::length(data1)== 1L, "data1 ARGUMENT", base::paste0("DATA FRAME NUMBER ", i2, " OF data1 ARGUMENT")), " LAYER AS RASTER (NOT VECTORIAL)")
                    warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
                }
            }
        }
        if(base::any(base::unlist(scatter_kind) == "gg_point_rast")){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") RASTER PLOT GENERATED -> ASPECT RATIO OF THE PLOT REGION SET BY THE raster_ratio ARGUMENT (", saferTool::round2(data = raster_ratio, dec_nb = 2, after_lead_zero = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text), ") TO AVOID A BUG OF ELLIPSOID DOT DRAWING")
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
        }
    }
    # end kind of geom_point (vectorial or raster)
    
    
    
    
    # no need loop part
    coord_names <- NULL
    tempo_gg_name <- "gg.indiv.plot."
    tempo_gg_count <- 0
    base::assign(base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), base::eval(base::parse(text = base::paste0("ggplot2::ggplot()", if(base::is.null(add)){""}else{add})))) # add added here to have the facets
    base::assign(base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), ggplot2::xlab(if(base::is.null(x_lab)){x[[1]]}else{x_lab}))
    base::assign(base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), ggplot2::ylab(if(base::is.null(y_lab)){y[[1]]}else{y_lab}))
    base::assign(base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), ggplot2::ggtitle(title))
    # text angle management
    x_tempo_just <- gg_just(angle = x_text_angle, pos = "bottom", kind = "axis")
    y_tempo_just <- gg_just(angle = y_text_angle, pos = "left", kind = "axis")
    # end text angle management
    add_check <- TRUE
    if( ! base::is.null(add)){ # if add is NULL, then = 0
        if(base::grepl(pattern = "ggplot2::theme", add) == TRUE){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") \"ggplot2::theme\" STRING DETECTED IN THE add ARGUMENT\n-> INTERNAL GGPLOT2 THEME FUNCTIONS theme() AND theme_classic() HAVE BEEN INACTIVATED, TO BE USED BY THE USER\n-> article ARGUMENT WILL BE IGNORED\nIT IS RECOMMENDED TO USE \"+ theme(aspect_ratio = raster_ratio)\" IF RASTER MODE IS ACTIVATED")
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
            add_check <- FALSE
        }
    }
    if(add_check == TRUE & article == TRUE){
        # WARNING: not possible to add several times theme(). NO message but the last one overwrites the others
        base::assign(base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), ggplot2::theme_classic(base_size = text_size))
        if(grid == TRUE){
            base::assign(base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), m_gg <- ggplot2::theme(
                text = ggplot2::element_text(size = text_size), 
                plot.title = ggplot2::element_text(size = title_text_size), # stronger than text
                legend.key = ggplot2::element_rect(color = "white", linewidth = 1.5), # size of the frame of the legend
                line = ggplot2::element_line(linewidth = 0.5), 
                axis.line.y.left = ggplot2::element_line(colour = "black"), # draw lines for the y axis
                axis.line.x.bottom = ggplot2::element_line(colour = "black"), # draw lines for the x axis
                panel.grid.major.x = ggplot2::element_line(colour = "grey85", linewidth = 0.75), 
                panel.grid.minor.x = ggplot2::element_line(colour = "grey90", linewidth = 0.25), 
                panel.grid.major.y = ggplot2::element_line(colour = "grey85", linewidth = 0.75), 
                panel.grid.minor.y = ggplot2::element_line(colour = "grey90", linewidth = 0.25), 
                axis.text.x = ggplot2::element_text(angle = x_tempo_just$angle, hjust = x_tempo_just$hjust, vjust = x_tempo_just$vjust),
                axis.text.y = ggplot2::element_text(angle = y_tempo_just$angle, hjust = y_tempo_just$hjust, vjust = y_tempo_just$vjust), 
                aspect_ratio = if(fix_ratio == TRUE){raster_ratio}else{NULL} # for raster
            ))
        }else{
            base::assign(base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), m_gg <- ggplot2::theme(
                text = ggplot2::element_text(size = text_size), 
                plot.title = ggplot2::element_text(size = title_text_size), # stronger than text
                line = ggplot2::element_line(linewidth = 0.5), 
                legend.key = ggplot2::element_rect(color = "white", linewidth = 1.5), # size of the frame of the legend
                axis.line.y.left = ggplot2::element_line(colour = "black"), 
                axis.line.x.bottom = ggplot2::element_line(colour = "black"), 
                axis.text.x = ggplot2::element_text(angle = x_tempo_just$angle, hjust = x_tempo_just$hjust, vjust = x_tempo_just$vjust),
                axis.text.y = ggplot2::element_text(angle = y_tempo_just$angle, hjust = y_tempo_just$hjust, vjust = y_tempo_just$vjust), 
                aspect_ratio = if(fix_ratio == TRUE){raster_ratio}else{NULL} # for raster
            ))
        }
    }else if(add_check == TRUE & article == FALSE){
        base::assign(base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), m_gg <- ggplot2::theme(
            text = ggplot2::element_text(size = text_size), 
            plot.title = ggplot2::element_text(size = title_text_size), # stronger than text
            line = ggplot2::element_line(linewidth = 0.5), 
            legend.key = ggplot2::element_rect(color = "white", linewidth = 1.5), # size of the frame of the legend
            panel.background = ggplot2::element_rect(fill = "grey95"), 
            axis.line.y.left = ggplot2::element_line(colour = "black"), 
            axis.line.x.bottom = ggplot2::element_line(colour = "black"), 
            panel.grid.major.x = ggplot2::element_line(colour = "grey85", linewidth = 0.75), 
            panel.grid.minor.x = ggplot2::element_line(colour = "grey90", linewidth = 0.25), 
            panel.grid.major.y = ggplot2::element_line(colour = "grey85", linewidth = 0.75), 
            panel.grid.minor.y = ggplot2::element_line(colour = "grey90", linewidth = 0.25), 
            strip.background = ggplot2::element_rect(fill = "white", colour = "black"), 
            axis.text.x = ggplot2::element_text(angle = x_tempo_just$angle, hjust = x_tempo_just$hjust, vjust = x_tempo_just$vjust),
            axis.text.y = ggplot2::element_text(angle = y_tempo_just$angle, hjust = y_tempo_just$hjust, vjust = y_tempo_just$vjust), 
            aspect_ratio = if(fix_ratio == TRUE){raster_ratio}else{NULL} # for raster
            # do not work -> legend.position = "none" # to remove the legend completely: https://www.datanovia.com/en/blog/how-to-remove-legend-from-a-ggplot/
        ))
    }
    # end no need loop part
    
    
    # loop part
    point_count <- 0
    line_count <- 0
    lg_order <- base::vector(mode = "list", length = 6) # order of the legend
    lg_order <- base::lapply(lg_order, as.numeric) # order of the legend
    lg_color <- base::vector(mode = "list", length = 6) # color of the legend
    lg_dot_shape <- base::vector(mode = "list", length = 6) # etc.
    lg_dot_size <- base::vector(mode = "list", length = 6) # etc.
    lg_dot_size <- base::lapply(lg_dot_size, as.numeric) # etc.
    lg_dot_border_size <- base::vector(mode = "list", length = 6) # etc.
    lg_dot_border_size <- base::lapply(lg_dot_border_size, as.numeric) # etc.
    lg_dot_border_color <- base::vector(mode = "list", length = 6) # etc.
    lg_line_size <- base::vector(mode = "list", length = 6) # etc.
    lg_line_size <- base::lapply(lg_line_size, as.numeric) # etc.
    lg_line_type <- base::vector(mode = "list", length = 6) # etc.
    lg_alpha <- base::vector(mode = "list", length = 6) # etc.
    lg_alpha <- base::lapply(lg_alpha, as.numeric) # etc.
    for(i1 in 1:length(data1)){
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
                if(plot == TRUE & fin_lg_disp[[1]] == TRUE & dot_shape[[1]] %in% 0:14 & ((base::length(grDevices::dev.list()) > 0 & base::names(grDevices::dev.cur()) == "windows") | (base::length(grDevices::dev.list())== 0L & Sys.info()["sysname"] == "Windows"))){ # if any Graph device already open and this device is "windows", or if no Graph device opened yet and we are on windows system -> prevention of alpha legend bug on windows using value 1
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") GRAPHIC DEVICE USED ON A WINDOWS SYSTEM ->\nTRANSPARENCY OF THE DOTS (DOT LAYER NUMBER ", point_count, ") IS INACTIVATED IN THE LEGEND TO PREVENT A WINDOWS DEPENDENT BUG (SEE https://github.com/tidyverse/ggplot2/issues/2452)\nTO OVERCOME THIS ON WINDOWS, USE ANOTHER DEVICE (pdf() FOR INSTANCE)")
                    warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
                    lg_alpha[[1]] <- 1 # to avoid a bug on windows: if alpha argument is different from 1 for lines (transparency), then lines are not correctly displayed in the legend when using the R GUI (bug https://github.com/tidyverse/ggplot2/issues/2452). No bug when using a pdf
                }else{
                    lg_alpha[[1]] <- alpha[[i1]]
                }
                class_categ <- base::levels(base::factor(data1[[i1]][, categ[[i1]]]))
                for(i5 in 1:length(color[[i1]])){ # or base::length(class_categ). It is the same because already checked that lengths are the same
                    tempo_data_frame <- data1[[i1]][data1[[i1]][, categ[[i1]]] == class_categ[i5], ]
                    tempo_layer_fun <- base::eval(base::parse(text = scatter_kind[[i1]])) ; tempo_layer_args <- base::list(data = tempo_data_frame, mapping = ggplot2::aes_string(x = x[[i1]], y = y[[i1]], fill = categ[[i1]]), shape = dot_shape[[i1]], size = dot_size[[i1]], stroke = dot_border_size[[i1]], color = if(dot_shape[[i1]] %in% 21:24 & ! base::is.null(dot_border_color)){dot_border_color[[i1]]}else{color[[i1]][i5]}, alpha = alpha[[i1]]) ; tempo_layer_args[[base::ifelse(test = base::grepl(x = scatter_kind[[i1]], pattern = "gg_point_rast", fixed = TRUE), yes = "show_legend", no = "show.legend")]] <- if(i5== 1L){TRUE}else{FALSE} ; base::assign(base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), base::do.call(what = base::ifelse(test = base::grepl(x = scatter_kind[[i1]], pattern = "gg_point_rast", fixed = TRUE), yes = "gg_point_rast", no = base::get(x = "geom_point", envir = base::asNamespace(ns = "ggplot2"))), args = tempo_layer_args)) # WARNING: a single color allowed for color argument outside aesthetic, but here a single color for border --> loop could be inactivated but kept for commodity # legend_show option do not remove the legend, only the aesthetic of the legend (dot, line, etc.). Used here to avoid multiple layers of legend which corrupt transparency
                    coord_names <- c(coord_names, base::paste0(geom[[i1]], ".", class_categ[i5]))
                }
                base::assign(base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), ggplot2::scale_fill_manual(
                    name = if(base::is.null(legend_name)){
                        # nocov start (unreachable: legend_name is converted to a non-NULL list by the legend_name management block, so is.null(legend_name) is always FALSE when the scale is built)
                        NULL
                        # nocov end
                    }else{legend_name[[i1]]}, values = base::as.character(color[[i1]]), breaks = class_categ)) # values are the values of fill, breaks reorder the classes according to class_categ in the legend, order argument of guide_legend determines the order of the different aesthetics in the legend (not order of classes). See guide_legend settings of scale_..._manual below
            }
            if(point_count== 2L){
                fin_lg_disp[[2]] <- legend_disp[[point_count + line_count]]
                lg_order[[2]] <- point_count + line_count
                lg_color[[2]] <- color[[i1]] # if color == NULL -> NULL
                lg_dot_shape[[2]] <- dot_shape[[i1]]
                lg_dot_size[[2]] <- dot_size[[i1]]
                lg_dot_border_size[[2]] <- dot_border_size[[i1]]
                lg_dot_border_color[[2]] <- dot_border_color[[i1]] # if dot_border_color == NULL -> NULL
                if(plot == TRUE & fin_lg_disp[[2]] == TRUE & dot_shape[[2]] %in% 0:14 & ((base::length(grDevices::dev.list()) > 0 & base::names(grDevices::dev.cur()) == "windows") | (base::length(grDevices::dev.list())== 0L & Sys.info()["sysname"] == "Windows"))){ # if any Graph device already open and this device is "windows", or if no Graph device opened yet and we are on windows system -> prevention of alpha legend bug on windows using value 1
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") GRAPHIC DEVICE USED ON A WINDOWS SYSTEM ->\nTRANSPARENCY OF THE DOTS (DOT LAYER NUMBER ", point_count, ") IS INACTIVATED IN THE LEGEND TO PREVENT A WINDOWS DEPENDENT BUG (SEE https://github.com/tidyverse/ggplot2/issues/2452)\nTO OVERCOME THIS ON WINDOWS, USE ANOTHER DEVICE (pdf() FOR INSTANCE)")
                    warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
                    lg_alpha[[2]] <- 1 # to avoid a bug on windows: if alpha argument is different from 1 for lines (transparency), then lines are not correctly displayed in the legend when using the R GUI (bug https://github.com/tidyverse/ggplot2/issues/2452). No bug when using a pdf
                }else{
                    lg_alpha[[2]] <- alpha[[i1]]
                }
                class_categ <- base::levels(base::factor(data1[[i1]][, categ[[i1]]]))
                for(i5 in 1:length(color[[i1]])){ # or base::length(class_categ). It is the same because already checked that lengths are the same
                    tempo_data_frame <- data1[[i1]][data1[[i1]][, categ[[i1]]] == class_categ[i5], ]
                    tempo_layer_fun <- base::eval(base::parse(text = scatter_kind[[i1]])) ; tempo_layer_args <- base::list(data = tempo_data_frame, mapping = ggplot2::aes_string(x = x[[i1]], y = y[[i1]], shape = categ[[i1]]), size = dot_size[[i1]], stroke = dot_border_size[[i1]], fill = color[[i1]][i5], color = if(dot_shape[[i1]] %in% 21:24 & ! base::is.null(dot_border_color)){dot_border_color[[i1]]}else{color[[i1]][i5]}, alpha = alpha[[i1]]) ; tempo_layer_args[[base::ifelse(test = base::grepl(x = scatter_kind[[i1]], pattern = "gg_point_rast", fixed = TRUE), yes = "show_legend", no = "show.legend")]] <- FALSE ; base::assign(base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), base::do.call(what = base::ifelse(test = base::grepl(x = scatter_kind[[i1]], pattern = "gg_point_rast", fixed = TRUE), yes = "gg_point_rast", no = base::get(x = "geom_point", envir = base::asNamespace(ns = "ggplot2"))), args = tempo_layer_args)) # WARNING: a single color allowed for fill argument outside aesthetic, hence the loop # legend_show option do not remove the legend, only the aesthetic of the legend (dot, line, etc.). Used here to avoid multiple layers of legend which corrupt transparency
                    coord_names <- c(coord_names, base::paste0(geom[[i1]], ".", class_categ[i5]))
                }
                base::assign(base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), ggplot2::scale_shape_manual(
                    name = if(base::is.null(legend_name)){
                        # nocov start (unreachable: idem as scale_fill_manual)
                        NULL
                        # nocov end
                    }else{legend_name[[i1]]}, values = base::rep(dot_shape[[i1]], base::length(color[[i1]])), breaks = class_categ)) # values are the values of shape, breaks reorder the classes according to class_categ in the legend. See guide_legend settings of scale_..._manual below
                
            }
            if(point_count== 3L){
                fin_lg_disp[[3]] <- legend_disp[[point_count + line_count]]
                lg_order[[3]] <- point_count + line_count
                lg_color[[3]] <- color[[i1]] # if color == NULL -> NULL
                lg_dot_shape[[3]] <- dot_shape[[i1]]
                lg_dot_size[[3]] <- dot_size[[i1]]
                lg_dot_border_size[[3]] <- dot_border_size[[i1]]
                lg_dot_border_color[[3]] <- dot_border_color[[i1]] # if dot_border_color == NULL -> NULL
                if(plot == TRUE & fin_lg_disp[[3]] == TRUE & dot_shape[[3]] %in% 0:14 & ((base::length(grDevices::dev.list()) > 0 & base::names(grDevices::dev.cur()) == "windows") | (base::length(grDevices::dev.list())== 0L & Sys.info()["sysname"] == "Windows"))){ # if any Graph device already open and this device is "windows", or if no Graph device opened yet and we are on windows system -> prevention of alpha legend bug on windows using value 1
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") GRAPHIC DEVICE USED ON A WINDOWS SYSTEM ->\nTRANSPARENCY OF THE DOTS (DOT LAYER NUMBER ", point_count, ") IS INACTIVATED IN THE LEGEND TO PREVENT A WINDOWS DEPENDENT BUG (SEE https://github.com/tidyverse/ggplot2/issues/2452)\nTO OVERCOME THIS ON WINDOWS, USE ANOTHER DEVICE (pdf() FOR INSTANCE)")
                    warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
                    lg_alpha[[3]] <- 1 # to avoid a bug on windows: if alpha argument is different from 1 for lines (transparency), then lines are not correctly displayed in the legend when using the R GUI (bug https://github.com/tidyverse/ggplot2/issues/2452). No bug when using a pdf
                }else{
                    lg_alpha[[3]] <- alpha[[i1]]
                }
                class_categ <- base::levels(base::factor(data1[[i1]][, categ[[i1]]]))
                for(i5 in 1:length(color[[i1]])){ # or base::length(class_categ). It is the same because already checked that lengths are the same
                    tempo_data_frame <- data1[[i1]][data1[[i1]][, categ[[i1]]] == class_categ[i5], ]
                    tempo_layer_fun <- base::eval(base::parse(text = scatter_kind[[i1]])) ; tempo_layer_args <- base::list(data = tempo_data_frame, mapping = ggplot2::aes_string(x = x[[i1]], y = y[[i1]], stroke = categ[[i1]]), shape = dot_shape[[i1]], size = dot_size[[i1]], fill = color[[i1]][i5], stroke = dot_border_size[[i1]], color = if(dot_shape[[i1]] %in% 21:24 & ! base::is.null(dot_border_color)){dot_border_color[[i1]]}else{color[[i1]][i5]}, alpha = alpha[[i1]]) ; tempo_layer_args[[base::ifelse(test = base::grepl(x = scatter_kind[[i1]], pattern = "gg_point_rast", fixed = TRUE), yes = "show_legend", no = "show.legend")]] <- FALSE ; base::assign(base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), base::do.call(what = base::ifelse(test = base::grepl(x = scatter_kind[[i1]], pattern = "gg_point_rast", fixed = TRUE), yes = "gg_point_rast", no = base::get(x = "geom_point", envir = base::asNamespace(ns = "ggplot2"))), args = tempo_layer_args)) # WARNING: a single color allowed for color argument outside aesthetic, hence the loop # legend_show option do not remove the legend, only the aesthetic of the legend (dot, line, etc.). Used here to avoid multiple layers of legend which corrupt transparency
                    coord_names <- c(coord_names, base::paste0(geom[[i1]], ".", class_categ[i5]))
                }
                base::assign(base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), ggplot2::scale_discrete_manual(aesthetics = "stroke",
                    name = if(base::is.null(legend_name)){
                        # nocov start (unreachable: idem as scale_fill_manual)
                        NULL
                        # nocov end
                    }else{legend_name[[i1]]}, values = base::rep(dot_border_size[[i1]], base::length(color[[i1]])), breaks = class_categ)) # values are the values of stroke, breaks reorder the classes according to class_categ in the legend. See guide_legend settings of scale_..._manual below
                
            }
        }else{
            line_count <- line_count + 1
            if(line_count== 1L){
                fin_lg_disp[[4]] <- legend_disp[[point_count + line_count]]
                lg_order[[4]] <- point_count + line_count
                lg_color[[4]] <- color[[i1]] # if color == NULL -> NULL
                lg_line_size[[4]] <- line_size[[i1]]
                lg_line_type[[4]] <- line_type[[i1]]
                if(plot == TRUE & fin_lg_disp[[4]] == TRUE & ((base::length(grDevices::dev.list()) > 0 & base::names(grDevices::dev.cur()) == "windows") | (base::length(grDevices::dev.list())== 0L & Sys.info()["sysname"] == "Windows"))){ # if any Graph device already open and this device is "windows", or if no Graph device opened yet and we are on windows system -> prevention of alpha legend bug on windows using value 1
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") GRAPHIC DEVICE USED ON A WINDOWS SYSTEM ->\nTRANSPARENCY OF THE LINES (LINE LAYER NUMBER ", line_count, ") IS INACTIVATED IN THE LEGEND TO PREVENT A WINDOWS DEPENDENT BUG (SEE https://github.com/tidyverse/ggplot2/issues/2452)\nTO OVERCOME THIS ON WINDOWS, USE ANOTHER DEVICE (pdf() FOR INSTANCE)")
                    warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
                    lg_alpha[[4]] <- 1 # to avoid a bug on windows: if alpha argument is different from 1 for lines (transparency), then lines are not correctly displayed in the legend when using the R GUI (bug https://github.com/tidyverse/ggplot2/issues/2452). No bug when using a pdf
                }else{
                    lg_alpha[[4]] <- alpha[[i1]]
                }
                class_categ <- base::levels(base::factor(data1[[i1]][, categ[[i1]]]))
                for(i5 in 1:length(color[[i1]])){ # or base::length(class_categ). It is the same because already checked that lengths are the same
                    tempo_data_frame <- data1[[i1]][data1[[i1]][, categ[[i1]]] == class_categ[i5], ]
                    base::assign(base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), base::eval(base::parse(text = base::paste0("ggplot2::", # no CR here te0("ggpl
                                                                                                                 base::ifelse(geom[[i1]] == 'geom_stick', 'geom_segment', geom[[i1]]), # geom_segment because geom_stick converted to geom_segment for plotting
                                                                                                                 "(data = tempo_data_frame, mapping = ggplot2::aes(x = ", 
                                                                                                                 x[[i1]], 
                                                                                                                 base::ifelse(geom[[i1]] == 'geom_stick', ", yend = ", ", y = "), 
                                                                                                                 y[[i1]], 
                                                                                                                 if(geom[[i1]] == 'geom_stick'){base::paste0(', xend = ', x[[i1]], ', y = ', if(base::is.null(x = geom_stick_base) || base::is.null(x = geom_stick_base[[i1]])){y_lim[1]}else{geom_stick_base[[i1]]})}, # ifelse() would crash with "replacement has length zero" when geom_stick_base is a list with a NULL compartment
                                                                                                                 ", linetype = ", 
                                                                                                                 categ[[i1]], 
                                                                                                                 "), color = \"", 
                                                                                                                 color[[i1]][i5], 
                                                                                                                 "\", size = ", 
                                                                                                                 line_size[[i1]], 
                                                                                                                 base::ifelse(geom[[i1]] == 'geom_path', ', lineend = \"round\"', ''), 
                                                                                                                 base::ifelse(geom[[i1]] == 'geom_step', base::paste0(', direction = \"', geom_step_dir[[i1]], '\"'), ''), 
                                                                                                                 ", alpha = ", 
                                                                                                                 alpha[[i1]], 
                                                                                                                 ", show.legend = ", 
                                                                                                                 base::ifelse(i5== 1L, TRUE, FALSE), 
                                                                                                                 ")"
                    )))) # WARNING: a single color allowed for color argument outside aesthetic, hence the loop # legend_show option do not remove the legend, only the aesthetic of the legend (dot, line, etc.). Used here to avoid multiple layers of legend which corrupt transparency
                    coord_names <- c(coord_names, base::paste0(geom[[i1]], ".", class_categ[i5]))
                }
                base::assign(base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), ggplot2::scale_discrete_manual(aesthetics = "linetype",
                    name = if(base::is.null(legend_name)){
                        # nocov start (unreachable: idem as scale_fill_manual)
                        NULL
                        # nocov end
                    }else{legend_name[[i1]]}, values = base::rep(line_type[[i1]], base::length(color[[i1]])), breaks = class_categ)) # values are the values of linetype. 1 means solid. Regarding the alpha bug, I have tried different things without success: alpha in guide alone, in geom alone, in both, with different values, breaks reorder the classes according to class_categ in the legend
            }
            if(line_count== 2L){
                fin_lg_disp[[5]] <- legend_disp[[point_count + line_count]]
                lg_order[[5]] <- point_count + line_count
                lg_color[[5]] <- color[[i1]] # if color == NULL -> NULL
                lg_line_size[[5]] <- line_size[[i1]]
                lg_line_type[[5]] <- line_type[[i1]]
                if(plot == TRUE & fin_lg_disp[[5]] == TRUE & ((base::length(grDevices::dev.list()) > 0 & base::names(grDevices::dev.cur()) == "windows") | (base::length(grDevices::dev.list())== 0L & Sys.info()["sysname"] == "Windows"))){ # if any Graph device already open and this device is "windows", or if no Graph device opened yet and we are on windows system -> prevention of alpha legend bug on windows using value 1
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") GRAPHIC DEVICE USED ON A WINDOWS SYSTEM ->\nTRANSPARENCY OF THE LINES (LINE LAYER NUMBER ", line_count, ") IS INACTIVATED IN THE LEGEND TO PREVENT A WINDOWS DEPENDENT BUG (SEE https://github.com/tidyverse/ggplot2/issues/2452)\nTO OVERCOME THIS ON WINDOWS, USE ANOTHER DEVICE (pdf() FOR INSTANCE)")
                    warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
                    lg_alpha[[5]] <- 1 # to avoid a bug on windows: if alpha argument is different from 1 for lines (transparency), then lines are not correctly displayed in the legend when using the R GUI (bug https://github.com/tidyverse/ggplot2/issues/2452). No bug when using a pdf
                }else{
                    lg_alpha[[5]] <- alpha[[i1]]
                }
                class_categ <- base::levels(base::factor(data1[[i1]][, categ[[i1]]]))
                for(i5 in 1:length(color[[i1]])){ # or base::length(class_categ). It is the same because already checked that lengths are the same
                    tempo_data_frame <- data1[[i1]][data1[[i1]][, categ[[i1]]] == class_categ[i5], ]
                    base::assign(base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), base::eval(base::parse(text = base::paste0("ggplot2::", # no CR here te0("ggpl
                                                                                                                 base::ifelse(geom[[i1]] == 'geom_stick', 'geom_segment', geom[[i1]]), # geom_segment because geom_stick converted to geom_segment for plotting
                                                                                                                 "(data = tempo_data_frame, mapping = ggplot2::aes(x = ", 
                                                                                                                 x[[i1]], 
                                                                                                                 base::ifelse(geom[[i1]] == 'geom_stick', ", yend = ", ", y = "), 
                                                                                                                 y[[i1]], 
                                                                                                                 if(geom[[i1]] == 'geom_stick'){base::paste0(', xend = ', x[[i1]], ', y = ', if(base::is.null(x = geom_stick_base) || base::is.null(x = geom_stick_base[[i1]])){y_lim[1]}else{geom_stick_base[[i1]]})}, # ifelse() would crash with "replacement has length zero" when geom_stick_base is a list with a NULL compartment
                                                                                                                 ", alpha = ", 
                                                                                                                 categ[[i1]], 
                                                                                                                 "), color = \"", 
                                                                                                                 color[[i1]][i5], 
                                                                                                                 "\", size = ", 
                                                                                                                 line_size[[i1]], 
                                                                                                                 ", linetype = ", 
                                                                                                                 base::ifelse(is.numeric(line_type[[i1]]), "", "\""), 
                                                                                                                 line_type[[i1]], 
                                                                                                                 base::ifelse(is.numeric(line_type[[i1]]), "", "\""), 
                                                                                                                 base::ifelse(geom[[i1]] == 'geom_path', ', lineend = \"round\"', ''), 
                                                                                                                 base::ifelse(geom[[i1]] == 'geom_step', base::paste0(', direction = \"', geom_step_dir[[i1]], '\"'), ''), 
                                                                                                                 ", show.legend = FALSE)"
                    )))) # WARNING: a single color allowed for color argument outside aesthetic, hence the loop # legend_show option do not remove the legend, only the aesthetic of the legend (dot, line, etc.). Used here to avoid multiple layers of legend which corrupt transparency
                    coord_names <- c(coord_names, base::paste0(geom[[i1]], ".", class_categ[i5]))
                }
                base::assign(base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), ggplot2::scale_discrete_manual(aesthetics = "alpha",
                    name = if(base::is.null(legend_name)){
                        # nocov start (unreachable: idem as scale_fill_manual)
                        NULL
                        # nocov end
                    }else{legend_name[[i1]]}, values = base::rep(alpha[[i1]], base::length(color[[i1]])), breaks = class_categ)) # values are the values of linetype. 1 means solid. Regarding the alpha bug, I have tried different things without success: alpha in guide alone, in geom alone, in both, with different values, breaks reorder the classes according to class_categ in the legend
            }
            if(line_count== 3L){
                fin_lg_disp[[6]] <- legend_disp[[point_count + line_count]]
                lg_order[[6]] <- point_count + line_count
                lg_color[[6]] <- color[[i1]] # if color == NULL -> NULL
                lg_line_size[[6]] <- line_size[[i1]]
                lg_line_type[[6]] <- line_type[[i1]]
                if(plot == TRUE & fin_lg_disp[[6]] == TRUE & ((base::length(grDevices::dev.list()) > 0 & base::names(grDevices::dev.cur()) == "windows") | (base::length(grDevices::dev.list())== 0L & Sys.info()["sysname"] == "Windows"))){ # if any Graph device already open and this device is "windows", or if no Graph device opened yet and we are on windows system -> prevention of alpha legend bug on windows using value 1
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") GRAPHIC DEVICE USED ON A WINDOWS SYSTEM ->\nTRANSPARENCY OF THE LINES (LINE LAYER NUMBER ", line_count, ") IS INACTIVATED IN THE LEGEND TO PREVENT A WINDOWS DEPENDENT BUG (SEE https://github.com/tidyverse/ggplot2/issues/2452)\nTO OVERCOME THIS ON WINDOWS, USE ANOTHER DEVICE (pdf() FOR INSTANCE)")
                    warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
                    lg_alpha[[6]] <- 1 # to avoid a bug on windows: if alpha argument is different from 1 for lines (transparency), then lines are not correctly displayed in the legend when using the R GUI (bug https://github.com/tidyverse/ggplot2/issues/2452). No bug when using a pdf
                }else{
                    lg_alpha[[6]] <- alpha[[i1]]
                }
                class_categ <- base::levels(base::factor(data1[[i1]][, categ[[i1]]]))
                for(i5 in 1:length(color[[i1]])){ # or base::length(class_categ). It is the same because already checked that lengths are the same
                    tempo_data_frame <- data1[[i1]][data1[[i1]][, categ[[i1]]] == class_categ[i5], ]
                    base::assign(base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), base::eval(base::parse(text = base::paste0("ggplot2::", # no CR here te0("ggpl
                                                                                                                 base::ifelse(geom[[i1]] == 'geom_stick', 'geom_segment', geom[[i1]]), # geom_segment because geom_stick converted to geom_segment for plotting
                                                                                                                 "(data = tempo_data_frame, mapping = ggplot2::aes(x = ", 
                                                                                                                 x[[i1]], 
                                                                                                                 base::ifelse(geom[[i1]] == 'geom_stick', ", yend = ", ", y = "), 
                                                                                                                 y[[i1]], 
                                                                                                                 if(geom[[i1]] == 'geom_stick'){base::paste0(', xend = ', x[[i1]], ', y = ', if(base::is.null(x = geom_stick_base) || base::is.null(x = geom_stick_base[[i1]])){y_lim[1]}else{geom_stick_base[[i1]]})}, # ifelse() would crash with "replacement has length zero" when geom_stick_base is a list with a NULL compartment
                                                                                                                 ", size = ", 
                                                                                                                 categ[[i1]], 
                                                                                                                 "), color = \"", 
                                                                                                                 color[[i1]][i5], 
                                                                                                                 "\", linetype = ", 
                                                                                                                 base::ifelse(is.numeric(line_type[[i1]]), "", "\""), 
                                                                                                                 line_type[[i1]], 
                                                                                                                 base::ifelse(is.numeric(line_type[[i1]]), "", "\""), 
                                                                                                                 base::ifelse(geom[[i1]] == 'geom_path', ', lineend = \"round\"', ''), 
                                                                                                                 base::ifelse(geom[[i1]] == 'geom_step', base::paste0(', direction = \"', geom_step_dir[[i1]], '\"'), ''), 
                                                                                                                 ", alpha = ", 
                                                                                                                 alpha[[i1]], 
                                                                                                                 ", show.legend = FALSE)"
                    )))) # WARNING: a single color allowed for color argument outside aesthetic, hence the loop # legend_show option do not remove the legend, only the aesthetic of the legend (dot, line, etc.). Used here to avoid multiple layers of legend which corrupt transparency
                    coord_names <- c(coord_names, base::paste0(geom[[i1]], ".", class_categ[i5]))
                }
                base::assign(base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), ggplot2::scale_discrete_manual(aesthetics = "size",
                    name = if(base::is.null(legend_name)){
                        # nocov start (unreachable: idem as scale_fill_manual)
                        NULL
                        # nocov end
                    }else{legend_name[[i1]]}, values = base::rep(line_size[[i1]], base::length(color[[i1]])), breaks = class_categ)) # values are the values of linetype. 1 means solid. Regarding the alpha bug, I have tried different things without success: alpha in guide alone, in geom alone, in both, breaks reorder the classes according to class_categ in the legend
            }
        }
    }
    # end loop part
    
    
    
    
    # legend display
    tempo_legend_final <- 'ggplot2::guides(
fill = if(fin_lg_disp[[1]] == TRUE){
ggplot2::guide_legend(
order = lg_order[[1]], 
override.aes = list(
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
override.aes = list(
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
override.aes = list(
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
override.aes = list(
color = lg_color[[4]], 
size = lg_line_size[[4]], 
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
override.aes = list(
color = lg_color[[5]], 
size = lg_line_size[[5]], 
linetype = lg_line_type[[5]], 
alpha = lg_alpha[[5]], 
shape = NA
)
)
}else{
"none"
}, 
size = if(fin_lg_disp[[6]] == TRUE){
ggplot2::guide_legend(
order = lg_order[[6]], 
override.aes = list(
color = lg_color[[6]], 
size = lg_line_size[[6]], 
linetype = lg_line_type[[6]], 
alpha = lg_alpha[[6]], 
shape = NA
)
)
}else{
"none"
}
)' # clip = "off" to have secondary ticks outside plot region does not work
    if( ! base::is.null(legend_width)){
        if(base::any(base::unlist(legend_disp))){ # means some TRUE
            tempo_graph_info <- base::suppressMessages(expr = ggplot2::ggplot_build(base::eval(base::parse(text = base::paste0(base::paste(base::paste0(tempo_gg_name, 1:tempo_gg_count), collapse = " + "), ' + ', tempo_legend_final))))) # will be recovered later again, when ylim will be considered
            legend_final <- gg_get_legend(ggplot_built = tempo_graph_info, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) # get legend
            fin_lg_disp[] <- FALSE # remove all the legends. Must be done even if fin_lg_disp is not appearing in the code thenafter. Otherwise twice the legend
            if(base::is.null(legend_final) & plot == TRUE){ # even if base::any(base::unlist(legend_disp)) is TRUE
            # nocov start (unreachable: with any(legend_disp) TRUE, the guides create a legend, so gg_get_legend() never returns NULL)
                legend_final <- gg_empty(text = " ", lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) # empty graph instead of legend
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") LEGEND REQUESTED (NON-NULL categ ARGUMENT OR legend_show ARGUMENT SET TO TRUE)\nBUT IT SEEMS THAT THE PLOT HAS NO LEGEND -> EMPTY LEGEND SPACE CREATED BECAUSE OF THE NON-NULL legend_width ARGUMENT\n")
                warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
            # nocov end
            }
        }else if(plot == TRUE){ # means all FALSE
            legend_final <- ggplot2::ggplot()+ggplot2::theme_void() # empty graph instead of legend
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") LEGEND REQUESTED (NON-NULL categ ARGUMENT OR legend_show ARGUMENT SET TO TRUE)\nBUT IT SEEMS THAT THE PLOT HAS NO LEGEND -> EMPTY LEGEND SPACE CREATED BECAUSE OF THE NON-NULL legend_width ARGUMENT\n")
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
        }
    }
    if( ! base::any(base::unlist(legend_disp))){
        fin_lg_disp[] <- FALSE # remove all the legends. Must be done even if fin_lg_disp is not appearing in the code thenafter. Otherwise twice the legend
    }
    base::assign(base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), base::eval(base::parse(text = tempo_legend_final)))
    # end legend display





    # scale management
    tempo_coord <- base::suppressMessages(expr = ggplot2::ggplot_build(base::eval(base::parse(text = base::paste(base::paste0(tempo_gg_name, 1:tempo_gg_count), collapse = " + ", ' + ggplot2::scale_x_continuous(expand = c(0, 0), limits = base::sort(x_lim), oob = scales::rescale_none) + ggplot2::scale_y_continuous(expand = c(0, 0), limits = base::sort(y_lim), oob = scales::rescale_none)'))))$layout$panel_params[[1]]) # here I do not need the x-axis and y-axis orientation, I just need the number of main ticks
    # x_second_tick_positions # coordinates of secondary ticks (only if x_second_tick_nb argument is non-null or if x_log argument is different from "no")
    if(x_log != "no"){ # integer main ticks for log2 and log10
        tempo_scale <- (base::as.integer(base::min(x_lim, na.rm = TRUE)) - 1):(base::as.integer(base::max(x_lim, na.rm = TRUE)) + 1)
    }else{
        tempo <- if(base::is.null(attributes(tempo_coord$x$breaks))){tempo_coord$x$breaks}else{
            # nocov start (unreachable: continuous scales have no break attributes in this ggplot2 version, probed empirically)
            base::unlist(attributes(tempo_coord$x$breaks))
            # nocov end
        }
        if(base::all(base::is.na(tempo))){
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
        if(base::length(base::unique(x_lim)) <= 1){
            tempo_cat <- base::paste0(error_text_start, "IT SEEMS THAT X-AXIS VALUES HAVE A NULL RANGE: ", base::paste(x_lim, collapse = " "), "\nPLEASE, USE THE x_lim ARGUMENT WITH 2 DIFFERENT VALUES TO SOLVE THIS")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            tempo_scale <- saferGraph::scale2(lim = x_lim, n = base::ifelse(base::is.null(x_tick_nb), base::length(tempo[ ! base::is.na(tempo)]), x_tick_nb), kind = "approx", safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) # in ggplot 3.3.0, tempo_coord$x.major_source replaced by tempo_coord$x$breaks. If fact: n = base::ifelse(base::is.null(x_tick_nb), base::length(tempo[ ! base::is.na(tempo)]), x_tick_nb)) replaced by n = base::ifelse(base::is.null(x_tick_nb), 4, x_tick_nb))
        }
    }
    x_second_tick_values <- NULL
    x_second_tick_pos <- NULL
    if(x_log != "no"){
        tempo <- saferGraph::inter_ticks(lim = x_lim, log = x_log, warn_print = FALSE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        x_second_tick_values <- tempo$values
        x_second_tick_pos <- tempo$coordinates
        # if(vertical == TRUE){ # do not remove in case the bug is fixed
        base::assign(base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), ggplot2::annotate(
            geom = "segment", 
            x = x_second_tick_pos, 
            xend = x_second_tick_pos, 
            y = if(diff(y_lim) > 0){tempo_coord$y.range[1]}else{tempo_coord$y.range[2]}, 
            yend = if(diff(y_lim) > 0){tempo_coord$y.range[1] + base::abs(diff(tempo_coord$y.range)) / 80}else{tempo_coord$y.range[2] - base::abs(diff(tempo_coord$y.range)) / 80}
        ))
        # }else{ # not working because of the ggplot2 bug
        # base::assign(base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), ggplot2::annotate(geom = "segment", y = x_second_tick_pos, yend = x_second_tick_pos, x = tempo_coord$x.range[1], xend = tempo_coord$x.range[1] + diff(tempo_coord$x.range) / 80))
        # }
        coord_names <- c(coord_names, "x_second_tick_positions")
    }else if(( ! base::is.null(x_second_tick_nb)) & x_log == "no"){
        # if(x_second_tick_nb > 0){ #inactivated because already checked before
        if(base::length(tempo_scale) < 2){
            tempo_cat1 <- c("x_tick_nb", "x_second_tick_nb")
            tempo_cat2 <- base::sapply(list(x_tick_nb, x_second_tick_nb), FUN = paste0, collapse = " ")
            tempo_sep <- base::sapply(base::mapply(" ", base::max(base::nchar(tempo_cat1)) - base::nchar(tempo_cat1) + 3, FUN = rep, SIMPLIFY = FALSE), FUN = paste0, collapse = "")
            tempo_cat <- base::paste0(error_text_start, "THE NUMBER OF GENERATED TICKS FOR THE X-AXIS IS NOT CORRECT: ", base::length(tempo_scale), "\nUSING THESE ARGUMENT SETTINGS (NO DISPLAY MEANS NULL VALUE):\n", base::paste0(tempo_cat1, tempo_sep, tempo_cat2, collapse = "\n"), "\nPLEASE, TEST OTHER VALUES")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        }else{
            tempo <- saferGraph::inter_ticks(lim = x_lim, log = x_log, breaks = tempo_scale, n = x_second_tick_nb, warn_print = FALSE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        }
        x_second_tick_values <- tempo$values
        x_second_tick_pos <- tempo$coordinates
        base::assign(base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), ggplot2::annotate(
            geom = "segment", 
            x = x_second_tick_pos, 
            xend = x_second_tick_pos, 
            y = if(diff(y_lim) > 0){tempo_coord$y.range[1]}else{tempo_coord$y.range[2]}, 
            yend = if(diff(y_lim) > 0){tempo_coord$y.range[1] + base::abs(diff(tempo_coord$y.range)) / 80}else{tempo_coord$y.range[2] - base::abs(diff(tempo_coord$y.range)) / 80}
        ))
        coord_names <- c(coord_names, "x_second_tick_positions")
    }
    # for the ggplot2 bug with x_log, this does not work: base::eval(base::parse(text = base::ifelse(vertical == FALSE & x_log == "log10", "ggplot2::scale_x_continuous", "ggplot2::scale_x_continuous")))
    base::assign(base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), ggplot2::scale_x_continuous(
        breaks = tempo_scale, 
        minor_breaks = x_second_tick_pos, 
            # nocov start (unreachable: internal consistency guard - x_log only accepts log10, log2 or no (arg_check))
        labels = if(x_log == "log10"){scales::trans_format("identity", scales::math_format(10^.x))}else if(x_log == "log2"){scales::trans_format("identity", scales::math_format(2^.x))}else if(x_log == "no"){ggplot2::waiver()}else{tempo_cat <- base::paste0("INTERNAL ERROR 2 IN ", intern_error_text_start, "CODE INCONSISTENCY 10.", 
            intern_error_text_end, 
            collapse = NULL, 
            recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)}, 
            # nocov end
        expand = c(0, 0), # remove space after after axis limits
        limits = base::sort(x_lim), # NA indicate that limits must correspond to data limits but xlim() already used
        oob = scales::rescale_none, 
        trans = base::ifelse(diff(x_lim) < 0, "reverse", "identity") # equivalent to ggplot2::scale_x_reverse() but create the problem of x-axis label disappearance with x_lim decreasing. Thus, do not use. Use xlim() below and after this
    ))
    # end x_second_tick_positions
    # y_second_tick_positions # coordinates of secondary ticks (only if y_second_tick_nb argument is non-null or if y_log argument is different from "no")
    if(y_log != "no"){ # integer main ticks for log2 and log10
        tempo_scale <- (base::as.integer(base::min(y_lim, na.rm = TRUE)) - 1):(base::as.integer(base::max(y_lim, na.rm = TRUE)) + 1)
    }else{
        tempo <- if(base::is.null(attributes(tempo_coord$y$breaks))){tempo_coord$y$breaks}else{
            # nocov start (unreachable: idem as x breaks)
            base::unlist(attributes(tempo_coord$y$breaks))
            # nocov end
        }
        if(base::all(base::is.na(tempo))){
            # nocov start (unreachable: internal consistency guard - idem as x breaks)
            tempo_cat <- base::paste0("INTERNAL ERROR 3 IN ", intern_error_text_start, "ONLY NA IN tempo_coord$y$breaks.", 
            intern_error_text_end, 
            collapse = NULL, 
            recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }
        if(base::length(base::unique(y_lim)) <= 1){
            tempo_cat <- base::paste0(error_text_start, "IT SEEMS THAT Y-AXIS VALUES HAVE A NULL RANGE: ", base::paste(y_lim, collapse = " "), "\nPLEASE, USE THE y_lim ARGUMENT WITH 2 DIFFERENT VALUES TO SOLVE THIS")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            tempo_scale <- saferGraph::scale2(lim = y_lim, n = base::ifelse(base::is.null(y_tick_nb), base::length(tempo[ ! base::is.na(tempo)]), y_tick_nb), kind = "approx", safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) # in ggplot 3.3.0, tempo_coord$y.major_source replaced by tempo_coord$y$breaks. If fact: n = base::ifelse(base::is.null(y_tick_nb), base::length(tempo[ ! base::is.na(tempo)]), y_tick_nb)) replaced by n = base::ifelse(base::is.null(y_tick_nb), 4, y_tick_nb))
        }
    }
    y_second_tick_values <- NULL
    y_second_tick_pos <- NULL
    if(y_log != "no"){
        tempo <- saferGraph::inter_ticks(lim = y_lim, log = y_log, warn_print = FALSE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        y_second_tick_values <- tempo$values
        y_second_tick_pos <- tempo$coordinates
        # if(vertical == TRUE){ # do not remove in case the bug is fixed
        base::assign(base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), ggplot2::annotate(
            geom = "segment", 
            y = y_second_tick_pos, 
            yend = y_second_tick_pos, 
            x = if(diff(x_lim) > 0){tempo_coord$x.range[1]}else{tempo_coord$x.range[2]}, 
            xend = if(diff(x_lim) > 0){tempo_coord$x.range[1] + base::abs(diff(tempo_coord$x.range)) / 80}else{tempo_coord$x.range[2] - base::abs(diff(tempo_coord$x.range)) / 80}
        ))
        # }else{ # not working because of the ggplot2 bug
        # base::assign(base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), ggplot2::annotate(geom = "segment", x = y_second_tick_pos, xend = y_second_tick_pos, y = tempo_coord$y.range[1], yend = tempo_coord$y.range[1] + diff(tempo_coord$y.range) / 80))
        # }
        coord_names <- c(coord_names, "y_second_tick_positions")
    }else if(( ! base::is.null(y_second_tick_nb)) & y_log == "no"){
        # if(y_second_tick_nb > 0){ #inactivated because already checked before
        if(base::length(tempo_scale) < 2){
            tempo_cat1 <- c("y_tick_nb", "y_second_tick_nb")
            tempo_cat2 <- base::sapply(list(y_tick_nb, y_second_tick_nb), FUN = paste0, collapse = " ")
            tempo_sep <- base::sapply(base::mapply(" ", base::max(base::nchar(tempo_cat1)) - base::nchar(tempo_cat1) + 3, FUN = rep, SIMPLIFY = FALSE), FUN = paste0, collapse = "")
            tempo_cat <- base::paste0(error_text_start, "THE NUMBER OF GENERATED TICKS FOR THE Y-AXIS IS NOT CORRECT: ", base::length(tempo_scale), "\nUSING THESE ARGUMENT SETTINGS (NO DISPLAY MEANS NULL VALUE):\n", base::paste0(tempo_cat1, tempo_sep, tempo_cat2, collapse = "\n"), "\nPLEASE, TEST OTHER VALUES")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        }else{
            tempo <- saferGraph::inter_ticks(lim = y_lim, log = y_log, breaks = tempo_scale, n = y_second_tick_nb, warn_print = FALSE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        }
        y_second_tick_values <- tempo$values
        y_second_tick_pos <- tempo$coordinates
        base::assign(base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), ggplot2::annotate(
            geom = "segment", 
            y = y_second_tick_pos, 
            yend = y_second_tick_pos, 
            x = if(diff(x_lim) > 0){tempo_coord$x.range[1]}else{tempo_coord$x.range[2]}, 
            xend = if(diff(x_lim) > 0){tempo_coord$x.range[1] + base::abs(diff(tempo_coord$x.range)) / 80}else{tempo_coord$x.range[2] - base::abs(diff(tempo_coord$x.range)) / 80}
        ))
        coord_names <- c(coord_names, "y_second_tick_positions")
    }
    # for the ggplot2 bug with y_log, this does not work: base::eval(base::parse(text = base::ifelse(vertical == FALSE & y_log == "log10", "ggplot2::scale_x_continuous", "ggplot2::scale_y_continuous")))
    base::assign(base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), ggplot2::scale_y_continuous(
        breaks = tempo_scale, 
        minor_breaks = y_second_tick_pos, 
            # nocov start (unreachable: internal consistency guard - y_log only accepts log10, log2 or no (arg_check))
        labels = if(y_log == "log10"){scales::trans_format("identity", scales::math_format(10^.x))}else if(y_log == "log2"){scales::trans_format("identity", scales::math_format(2^.x))}else if(y_log == "no"){ggplot2::waiver()}else{tempo_cat <- base::paste0("INTERNAL ERROR 4 IN ", intern_error_text_start, "CODE INCONSISTENCY 10.", 
            intern_error_text_end, 
            collapse = NULL, 
            recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)}, 
            # nocov end
        expand = c(0, 0), # remove space after axis limits
        limits = base::sort(y_lim), # NA indicate that limits must correspond to data limits but ylim() already used
        oob = scales::rescale_none, 
        trans = base::ifelse(diff(y_lim) < 0, "reverse", "identity") # equivalent to ggplot2::scale_y_reverse() but create the problem of y-axis label disappearance with y_lim decreasing. Thus, do not use. Use ylim() below and after this
    ))
    # end y_second_tick_positions
    base::assign(base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), ggplot2::coord_cartesian(xlim = x_lim, ylim = y_lim)) # clip = "off" to have secondary ticks outside plot region. The problem is that points out of bounds are also drawn outside the plot region. Thus, I cannot use it # at that stage, x_lim and y_lim not NULL anymore
    # end scale management




    # drawing
    fin_plot <- base::eval(base::parse(text = base::paste(base::paste0(tempo_gg_name, 1:tempo_gg_count), collapse = " + ")))
    grob_save <- NULL
    if(plot == TRUE){
        if( ! base::is.null(legend_width)){ # base::any(base::unlist(legend_disp)) == TRUE removed to have empty legend space # not & base::any(base::unlist(fin_lg_disp)) == TRUE here because converted to FALSE
            grob_save <- base::suppressMessages(expr = base::suppressWarnings(expr = gridExtra::grid.arrange(grobs = base::list(fin_plot, legend_final), ncol = 2, nrow = 1, widths = base::c(1, legend_width), heights = grid::unit(x = 1, units = "null"), as.table = FALSE, respect = FALSE, clip = "on", padding = grid::unit(x = 0.1, units = "line")), classes = "warning"), classes = "message")
        }else{
            # nocov start (unreachable: legend_width is rejected when NULL by the NULL-argument management above, so legend_width is always non-NULL here)
            grob_save <- base::suppressMessages(expr = base::suppressWarnings(expr = print(fin_plot)))
            # nocov end
        }
    }else{
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count,") PLOT NOT SHOWN AS REQUESTED")
        warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
    }
    # end drawing
    #### end main code

    #### output
    if(return == TRUE){
        output <- base::suppressMessages(expr = ggplot2::ggplot_build(plot = fin_plot), classes = "message")
        # output$data <- output$data[-1] # yes for boxplot but not for scatter # remove the first data because corresponds to the initial empty boxplot
        if(base::length(output$data) != base::length(coord_names)){
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
            base::names(output$data) <- coord_names
        }
        if(base::is.null(base::unlist(removed_row_nb))){
            removed_row_nb <- NULL
            removed_rows <- NULL
        }else{
            for(i3 in 1:length(data1)){
                if( ! base::is.null(removed_row_nb[[i3]])){
                    removed_row_nb[[i3]] <- base::sort(removed_row_nb[[i3]])
                    removed_rows[[i3]] <- data1_ini[[i3]][removed_row_nb[[i3]], ]
                }
            }
        }
        tempo <- output$layout$panel_params[[1]]
        output <- list(
            data = data1, 
            removed_row_nb = removed_row_nb, 
            removed_rows = removed_rows, 
            plot = c(output$data, x_second_tick_values = list(x_second_tick_values), y_second_tick_values = list(y_second_tick_values)), 
            panel = facet_categ, 
            axes = list(
                x.range = tempo$x.range, 
                x.labels = if(base::is.null(attributes(tempo$x$breaks))){tempo$x$breaks}else{ # base::is.null(attributes(tempo$x$breaks)) test if it is number (TRUE) or character (FALSE)
                    # nocov start (unreachable: continuous scales have no break attributes in this ggplot2 version, probed empirically)
                    tempo$x$scale$get_labels()
                    # nocov end
                }, 
                x.positions = if(base::is.null(attributes(tempo$x$breaks))){tempo$x$breaks}else{
                    # nocov start (unreachable: idem as x.labels)
                    base::unlist(attributes(tempo$x$breaks))
                    # nocov end
                }, 
                y.range = tempo$y.range, 
                y.labels = if(base::is.null(attributes(tempo$y$breaks))){tempo$y$breaks}else{
                    # nocov start (unreachable: idem as x.labels)
                    tempo$y$scale$get_labels()
                    # nocov end
                }, 
                y.positions = if(base::is.null(attributes(tempo$y$breaks))){tempo$y$breaks}else{
                    # nocov start (unreachable: idem as x.labels)
                    base::unlist(attributes(tempo$y$breaks))
                    # nocov end
                }
            ), 
            warn = base::paste0("\n", warn, "\n\n"), 
            ggplot = if(return_ggplot == TRUE){fin_plot}else{NULL}, # fin_plot plots the graph if return == TRUE
            gtable = if(return_gtable == TRUE){grob_save}else{NULL} #
        )
        return(output) # this plots the graph if return_ggplot is TRUE and if no assignment
    }
    #### end output

    #### warning output
    if( ! base::is.null(x = warn)){
        base::on.exit(
            expr = base::warning(
                message = base::paste0(
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
}


