# Error: class order not good when a class is removed due to NA
# Error: line 136 in check 20201126 with add argument
# Solve this: sometimes error messages can be more than the max display (8170). Thus, check every ERROR MESSAGE, and trunck the message if to big. In addition, add at the begining of the warning message that it is too long and see the $warn output for complete message. Add also this into gg_scatter
# add dot_shape ? See with available aesthetic layers
# rasterise: https://cran.r-project.org/web/packages/ggrastr/vignettes/Raster_geoms.html
# add horizontal argument and deal any conflict with vertical argument. Start with horizontal = NULL as default. If ! is.null() -> convert vertical if required
# time for excecution : microbenchmark package. See also in RStudio time per line of code. See also https://stackoverflow.com/questions/7561362/what-can-cause-a-program-to-run-much-faster-the-second-time

#' @title gg_box
#' @description
#' Plot ggplot2 boxplots + dots + means. For ggplot2 specifications, see: https://ggplot2.tidyverse.org/articles/ggplot2-specs.html.
#' @param data1 data frame containing one column of quantitative values (see the y argument below) and one or two columns of categories (see the categ argument below). Duplicated column names are not allowed.
#' @param y character string of the data1 column name for y-axis (column containing numeric values). Numeric values will be split according to the classes of the column names indicated in the categ argument to generate the boxes and will also be used to plot the dots.
#' @param categ vector of character strings of the data1 column name for categories (column of characters or factors). Must be either one or two column names. If a single column name (further referred to as categ1), then one box per class of categ1. If two column names (further referred to as categ1 and categ2), then one box per class of categ2, which form a group of boxes in each class of categ1. WARNING: no empty classes allowed. To have a single box, create a factor column with a single class and specify the name of this column in the categ argument (here, no categ2 in categ argument). For a single set of grouped boxes, create a factor column with a single class and specify this column in categ argument as first element (i.e., as categ1), in addition to the already used category (as categ2 in this situation).
#' @param categ_class_order list indicating the order of the classes of categ1 and categ2 represented on the boxplot (the first compartment for categ1 and and the second for categ2). If categ_class_order == NULL, classes are represented according to the alphabetical order. Some compartments can be NULL and others not. See the categ argument for categ1 and categ2 description.
#' @param categ_color vector of color character string for box frames (see the categ argument for categ1 and categ2 description). If categ_color == NULL, default colors of ggplot2, whatever categ1 and categ2. If categ_color is non-null and only categ1 in categ argument, categ_color can be either: (1) a single color string. All the boxes will have this color, whatever the number of classes of categ1; (2) a vector of string colors, one for each class of categ1. Each color will be associated according to categ_class_order of categ1; (3) a vector or factor of string colors, like if it was one of the column of data1 data frame. WARNING: a single color per class of categ1 and a single class of categ1 per color must be respected; Color functions, like grey(), hsv(), etc., are also accepted; Positive integers are also accepted instead of character strings, as long as above rules about length are respected. Integers will be processed by saferGG::gg_palette() using the maximal integer value among all the integers in categ_color (see saferGG::gg_palette()); If categ_color is non-null and categ1 and categ2 are specified, all the rules described above will apply to categ2 instead of categ1 (colors will be determined for boxes inside a group of boxes)
#' @param box_legend_name character string of the legend title. If box_legend_name is NULL, then box_legend_name <- categ1 if only categ1 is present, and box_legend_name <- categ2 if categ1 and categ2 are present in the categ argument. Write "" if no legend required. See the categ argument for categ1 and categ2 description.
#' @param box_fill logical. Fill the box? If TRUE, the categ_color argument will be used to generate filled boxplots (the box frames being black) as well as filled outlier dots (the dot border being controlled by the dot_border_color argument). If all the dots are plotted (argument dot_color other than NULL), they will be over the boxes. If FALSE, the categ_color argument will be used to color the box frames and the outlier dot borders. If all the dots are plotted, they will be beneath the boxes.
#' @param box_width single numeric value (from 0 to 1) of width of either boxes or group of boxes. When categ argument has a single categ1 element (i.e., separate boxes. See the categ argument for categ1 and categ2 description), then each class of categ1 is represented by a single box. In that case, box_width argument defines each box width, from 0 (no box width) to 1 (max box width), but also the space between boxes (the code uses 1 - box_width for the box spaces). Of note, xmin and xmax of the gg_box() output report the box boundaries (around x-axis unit 1, 2, 3, etc., for each box); When categ argument has a two categ1 and categ2 elements (i.e., grouped boxes), box_width argument defines the width allocated for each set of grouped boxes, from 0 (no group width) to 1 (max group width), but also the space between grouped boxes (the code uses 1 - box_width for the spaces). Of note, xmin and xmax of the gg_box() output report the box boundaries (around x-axis unit 1, 2, 3, etc., for each set of grouped box)
#' @param box_space single numeric value (from 0 to 1) indicating the box separation inside grouped boxes, when categ argument has a two categ1 and categ2 elements. 0 means no space and 1 means boxes shrunk to a vertical line. Ignored if categ argument has a single categ1 element.
#' @param box_line_size single numeric value of line width of boxes and whiskers in mm.
#' @param box_notch logical. Notched boxplot? It TRUE, display notched boxplot, notches corresponding approximately to the 95\% confidence interval of the median (the notch interval is exactly 1.58 x Inter Quartile Range (IQR) / sqrt(n), with n the number of values that made the box). If notch intervals between two boxes do not overlap, it can be interpreted as significant median differences.
#' @param box_alpha single numeric value (from 0 to 1) of box transparency (full transparent to full opaque, respectively). To remove boxplots, use box_alpha = 0.
#' @param box_mean logical. Add mean value? If TRUE, a diamond-shaped dot, with the horizontal diagonal corresponding to the mean value, is displayed over each boxplot.
#' @param box_whisker_kind range of the whiskers. Either "no" (no whiskers), or "std" (length of each whisker equal to 1.5 x Inter Quartile Range (IQR)), or "max" (length of the whiskers up or down to the most distant dot).
#' @param box_whisker_width single numeric value (from 0 to 1) of the whisker width, with 0 meaning no whiskers and 1 meaning a width equal to the box width.
#' @param dot_color single character color string defining the color of the dots (same color for all the dots, whatever their categories). If dot_color == NULL, the dots are not plotted. A single integer is also accepted instead of a character string, that will be processed by saferGG::gg_palette(). A vector of colors or a factor or a column name of data1 is also accepted, allowing a color per dot class (see the dot_categ argument). Color functions, like grey(), hsv(), etc., are also accepted.
#' @param dot_categ optional single character string of a column name (further referred to as categ3) of the data1 argument. This column of data1 will be used to generate a legend for dots, in addition to the legend for boxes. See the dot_color argument for details about the way the legend is built using the two dot_categ and dot_color arguments. If NULL, no legend created and the colors of dots will depend on dot_color and categ arguments (as explained in the dot_color argument).
#' @param dot_categ_class_order optional vector of character strings indicating the order of the classes of categ3 (see the dot_categ argument). If dot_categ is non-NULL and dot_categ_class_order is NULL, classes are displayed in the legend according to the alphabetical order. Ignored if dot_categ is NULL.
#' @param dot_legend_name optional character string of the legend title for categ3 (see the dot_categ argument). If dot_legend_name == NULL, dot_categ value is used (name of the column in data1). Write "" if no legend required. Ignored if dot_categ is NULL.
#' @param dot_tidy logical. Nice dot spreading? If TRUE, use the geom_dotplot() function for a nice representation. WARNING: change the true quantitative coordinates of dots (i.e., y-axis values for vertical display) because of binning. Thus, the gain in aestheticism is associated with a loss in precision that can be very important. If FALSE, dots are randomly spread on the qualitative axis, using the dot_jitter argument (see below) keeping the true quantitative coordinates.
#' @param dot_tidy_bin_nb positive integer indicating the number of bins (i.e., nb of separations) of the y_lim range. Each dot will then be put in one of the bin, with a diameter of the width of the bin. In other words, increase the number of bins to have smaller dots. Not considered if dot_tidy is FALSE.
#' @param dot_jitter numeric value (from 0 to 1) of random dot horizontal dispersion (for vertical display), with 0 meaning no dispersion and 1 meaning dispersion in the corresponding box width interval. Not considered if dot_tidy is TRUE.
#' @param dot_seed integer value that set the random seed. Using the same number will generate the same dot jittering. Write NULL to have different jittering each time the same instruction is run. Ignored if dot_tidy is TRUE.
#' @param dot_size numeric value of dot diameter in mm. Not considered if dot_tidy is TRUE.
#' @param dot_alpha numeric value (from 0 to 1) of dot transparency (full transparent to full opaque, respectively).
#' @param dot_border_size numeric value of border dot width in mm. Write zero for no dot border. If dot_tidy is TRUE, value 0 remove the border and other values leave the border without size control (geom_doplot() feature).
#' @param dot_border_color single character color string defining the color of the dot border (same color for all the dots, whatever their categories). If dot_border_color == NULL, the border color will be the same as the dot color. A single integer is also accepted instead of a character string, that will be processed by saferGG::gg_palette().
#' @param x_lab a character string or expression for x-axis legend. If NULL, character string of categ1 (see the categ argument for categ1 and categ2 description).
#' @param x_angle integer value of the text angle for the x-axis numbers, using the same rules as in ggplot2. Positive values for counterclockwise rotation: 0 for horizontal, 90 for vertical, 180 for upside down etc. Negative values for clockwise rotation: 0 for horizontal, -90 for vertical, -180 for upside down etc.
#' @param y_lab a character string or expression for y-axis legend. If NULL, character string of the y argument.
#' @param y_lim 2 numeric values indicating the range of the y-axis. Order matters (for inverted axis). If NULL, the range of the x column name of data1 will be used.
#' @param y_log either "no", "log2" (values in the y argument column of the data1 data frame will be log2 transformed and y-axis will be log2 scaled) or "log10" (values in the y argument column of the data1 data frame will be log10 transformed and y-axis will be log10 scaled). WARNING: not possible to have horizontal boxes with a log axis, due to a bug in ggplot2 (see https://github.com/tidyverse/ggplot2/issues/881).
#' @param y_tick_nb approximate number of desired values labeling the y-axis (i.e., main ticks, see the n argument of the the cute::saferGraph::scale2() function). If NULL and if y_log is "no", then the number of labeling values is set by ggplot2. If NULL and if y_log is "log2" or "log10", then the number of labeling values corresponds to all the exposant integers in the y_lim range (e.g., 10^1, 10^2 and 10^3, meaning 3 main ticks for y_lim = c(9, 1200)). WARNING: if non-NULL and if y_log is "log2" or "log10", labeling can be difficult to read (e.g., ..., 10^2, 10^2.5, 10^3, ...).
#' @param y_second_tick_nb number of desired secondary ticks between main ticks. Ignored if y_log is other than "no" (log scale plotted). Use argument return = TRUE and see $plot$y_second_tick_values to have the values associated to secondary ticks. IF NULL, no secondary ticks.
#' @param y_include_zero logical. Does y_lim range include 0? Ignored if y_log is "log2" or "log10".
#' @param y_top_extra_margin single proportion (between 0 and 1) indicating if extra margins must be added to y_lim. If different from 0, add the range of the axis multiplied by y_top_extra_margin (e.g., abs(y_lim[2] - y_lim[1]) * y_top_extra_margin) to the top of y-axis.
#' @param y_bottom_extra_margin idem as y_top_extra_margin but to the bottom of y-axis.
#' @param stat_pos add the median number above the corresponding box. Either NULL (no number shown), "top" (at the top of the plot region) or "above" (above each box).
#' @param stat_mean logical. Display mean numbers instead of median numbers? Ignored if stat_pos is NULL.
#' @param stat_size numeric value of the stat font size in mm. Ignored if stat_pos is NULL.
#' @param stat_dist numeric value of the stat distance in percentage of the y-axis range (stat_dist = 5 means move the number displayed at 5\% of the y-axis range). Ignored if stat_pos is NULL or "top".
#' @param stat_angle integer value of the angle of stat, using the same rules as in ggplot2. Positive values for counterclockwise rotation: 0 for horizontal, 90 for vertical, 180 for upside down etc. Negative values for clockwise rotation: 0 for horizontal, -90 for vertical, -180 for upside down etc.
#' @param vertical logical. Vertical boxes? WARNING: will be automatically set to TRUE if y_log argument is other than "no". Indeed, not possible to have horizontal boxes with a log axis, due to a bug in ggplot2 (see https://github.com/tidyverse/ggplot2/issues/881).
#' @param text_size numeric value of the font size of the (1) axis numbers, (2) axis labels and (3) texts in the graphic legend (in mm).
#' @param title character string of the graph title.
#' @param title_text_size numeric value of the title font size in mm.
#' @param legend_show logical. Show legend? Not considered if categ argument is NULL, because this already generate no legend, excepted if legend_width argument is non-NULL. In that specific case (categ is NULL, legend_show is TRUE and legend_width is non-NULL), an empty legend space is created. This can be useful when desiring graphs of exactly the same width, whatever they have legends or not.
#' @param legend_width single proportion (between 0 and 1) indicating the relative width of the legend sector (on the right of the plot) relative to the width of the plot. Value 1 means that the window device width is split in 2, half for the plot and half for the legend. Value 0 means no room for the legend, which will overlay the plot region. Write NULL to inactivate the legend sector. In such case, ggplot2 will manage the room required for the legend display, meaning that the width of the plotting region can vary between graphs, depending on the text in the legend.
#' @param article logical. If TRUE, use an article theme (article like). If FALSE, use a classic related ggplot theme. Use the add argument (e.g., add = "+ggplot2::theme_classic()" for the exact classic ggplot theme.
#' @param grid logical. Draw lines in the background to better read the box values? Not considered if article == FALSE (grid systematically present).
#' @param add character string allowing to add more ggplot2 features (dots, lines, themes, facet, etc.). Ignored if NULL. WARNING: (1) the string must start with "+", (2) the string must finish with ")" and (3) each function must be preceded by "ggplot2::". Example: "+ ggplot2::coord_flip() + ggplot2::theme_bw()". If the character string contains the "ggplot2::theme" string, then the article argument of gg_box() (see above) is ignored with a warning. In addition, some arguments can be overwritten, like x_angle (check all the arguments). Layers of add (e.g., "+ ggplot2::geom_hline(yintercept = 0)", "+ ggplot2::annotate(geom = 'text', x = 1, y = 0, label = 'a')") are drawn above the boxes and dots, and are not used to compute the y-axis limits (use y_lim if necessary). Their data are not returned in the $plot compartment of the output. Handle the add argument with caution since added functions can create conflicts with the preexisting internal ggplot2 functions. The call of objects inside the quotes of add can lead to an error if the name of these objects are some of the gg_box() arguments. Indeed, the function will use the internal argument instead of the global environment object. Example article <- "a" in the working environment and add = '+ ggplot2::ggtitle(article)'. The risk here is to have TRUE as title. To solve this, use add = '+ ggplot2::ggtitle(get("article", envir = .GlobalEnv))'    
#' @param return logical. Return the graph parameters?.
#' @param return_ggplot logical. Return the ggplot object in the output list? Ignored if return argument is FALSE. WARNING: always assign the gg_box() function (e.g., a <- gg_box()) if return_ggplot argument is TRUE, otherwise, double plotting is performed. See $ggplot in the RETURN section below for more details.
#' @param return_gtable logical. Return the ggplot object as gtable of grobs in the output list? Ignored if plot argument is FALSE. Indeed, the graph must be plotted to get the grobs dispositions. See $gtable in the RETURN section below for more details.
#' @param plot logical. Plot the graphic? If FALSE and return argument is TRUE, graphical parameters and associated warnings are provided without plotting.
#' @param warn_print logical. Print warnings at the end of the execution? ? If FALSE, warning messages are never printed, but can still be recovered in the returned list. Some of the warning messages (those delivered by the internal ggplot2 functions) are not apparent when using the argument plot = FALSE.
#' @param safer_check Single logical value. Perform the "safer" checks? If \code{TRUE}, checkings are performed before main code running (see the \href{https://github.com/safer-r}{safer-r project}): 1) required functions and related packages effectively present in local R libraries and 2) R classical operators (like \code{"<-"}) not overwritten by another package because of the R scope. Warning: must be set to \code{FALSE} if this function is used inside another "safer" function to avoid pointless multiple checkings.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R packages are not installed in the default directories because of lack of admin rights. More precisely, \code{lib_path} is passed through the \code{new} argument of \code{.libPaths()} so that the new library paths are \code{c(lib_path, .libPaths())}. Warning: \code{.libPaths()} is restored to the initial paths, after function execution. Ignored if \code{NULL} (default): only the pathways specified by the current \code{.libPaths()} are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: \code{error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."}. If \code{NULL}, converted into \code{""}.
#' @returns A boxplot if plot argument is TRUE.
#' @returns A list of the graph info if return argument is TRUE: $data: the initial data with graphic information added; $stat: the graphic statistics (mostly equivalent to ggplot_build()$data[[2]]); $removed_row_nb: which rows have been removed due to NA/Inf detection in y and categ columns (NULL if no row removed); $removed_rows: removed rows (NULL if no row removed); $plot: the graphic box and dot coordinates; $dots: dot coordinates; $main.box: coordinates of boxes; $median: median coordinates; $sup.whisker: coordinates of top whiskers (y for base and y.end for extremities); $inf.whisker: coordinates of bottom whiskers (y for base and y.end for extremities); $sup.whisker.edge: coordinates of top whisker edges (x and xend); $inf.whisker.edge: coordinates of bottom whisker edges(x and xend); $mean: diamond mean coordinates (only if box_mean argument is TRUE); $stat_pos: coordinates of stat numbers (only if stat_pos argument is not NULL); $panel: the variable names used for the panels (NULL if no panels). WARNING: NA can be present according to ggplot2 upgrade to v3.3.0; $axes: the x-axis and y-axis info; $warn: the warning messages. Use cat() for proper display. NULL if no warning. WARNING: warning messages delivered by the internal ggplot2 functions are not apparent when using the argument plot = FALSE; $ggplot: ggplot object that can be used for reprint (use print(...$ggplot) or update (use ...$ggplot + ggplot2::...). NULL if return_ggplot argument is FALSE. Of note, a non-NULL $ggplot in the output list is sometimes annoying as the manipulation of this list prints the plot; $gtable: gtable object that can be used for reprint (use gridExtra::grid.arrange(...$ggplot) or with additionnal grobs (see the grob decomposition in the examples). NULL if return_ggplot argument is FALSE. Contrary to $ggplot, a non-NULL $gtable in the output list is not annoying as the manipulation of this list does not print the plot; 
#' @examples
#' set.seed(1)
#' obs1 <- data.frame(
#'     Time = c(rnorm(20, 100, 10), rnorm(20, 200, 50), rnorm(20, 500, 60), rnorm(20, 100, 50)),
#'     Categ1 = rep(c("CAT", "DOG"), times = 40),
#'     Categ2 = rep(c("A", "B", "C", "D"), each = 20),
#'     Color1 = rep(c("coral", "lightblue"), times = 40),
#'     Color2 = rep(c("#9F2108", "#306100", "#007479", "#8500C0"), each = 20),
#'     stringsAsFactors = TRUE
#' )
#' set.seed(NULL)
#' gg_box(data1 = obs1, y = "Time", categ = "Categ1")
#' @importFrom ggplot2 .data
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 annotate
#' @importFrom ggplot2 coord_cartesian
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 geom_dotplot
#' @importFrom ggplot2 geom_path
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_polygon
#' @importFrom ggplot2 geom_segment
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggplot_build
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 position_dodge
#' @importFrom ggplot2 scale_discrete_manual
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 theme_void
#' @importFrom ggplot2 waiver
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom grDevices colors
#' @importFrom gridExtra grid.arrange
#' @importFrom scales alpha
#' @importFrom scales math_format
#' @importFrom scales rescale_none
#' @importFrom scales trans_format
#' @importFrom saferDev arg_check
#' @importFrom saferTool comp_2d
#' @importFrom saferTool name_change
#' @importFrom saferTool round2
#' @importFrom saferGraph scale2
#' @importFrom saferGraph inter_ticks
#' @details
#' - Rows containing NA in data1[, c(freq, categ)] will be removed before processing, with a warning (see below).
#' -Hinges are not computed like in the classical boxplot() function of R. See https://ggplot2.tidyverse.org/reference/geom_boxplot.html.
#' -To have a single box, please create a factor column with a single class and specify the name of this column in the categ argument. For a single set of grouped boxes, create a factor column with a single class and specify this column in categ argument as first element (i.e., as categ1, knowing that categ2 must also be specified in this situation). See categ argument below.
#" -The dot_alpha argument can alter the display of the color boxes when using pdf output"
#' - Size arguments (hole_text_size, border_size, title_text_size and annotation_size) are in mm. See Hadley comment in https://stackoverflow.com/questions/17311917/ggplot2-the-unit-of-size. See also http://sape.inf.usi.ch/quick-reference/ggplot2/size). Unit object are not accepted, but conversion can be used (e.g., grid::convertUnit(grid::unit(0.2, "inches"), "mm", valueOnly = TRUE)).
#' -Display seems to be done twice on Windows devices (like a blink). However, no double plots on pdf devices. Thus, the blink remains mysterious.
#' -To remove boxes and have only dots, use box_alpha = 0
#' @export
gg_box <- function(
    data1, 
    y, 
    categ, 
    categ_class_order = NULL, 
    categ_color = NULL, 
    box_legend_name = NULL, 
    box_fill = FALSE, 
    box_width = 0.5, 
    box_space = 0.1, 
    box_line_size = 0.75, 
    box_notch = FALSE, 
    box_alpha = 1, 
    box_mean = TRUE, 
    box_whisker_kind = "std", 
    box_whisker_width = 0, 
    dot_color = grDevices::grey(level = 0.25), 
    dot_categ = NULL, 
    dot_categ_class_order = NULL, 
    dot_legend_name = NULL, 
    dot_tidy = FALSE, 
    dot_tidy_bin_nb = 50, 
    dot_jitter = 0.5, 
    dot_seed = 2, 
    dot_size = 3, 
    dot_alpha = 0.5, 
    dot_border_size = 0.5, 
    dot_border_color = NULL, 
    x_lab = NULL, 
    x_angle = 0, 
    y_lab = NULL, 
    y_lim = NULL, 
    y_log = "no", 
    y_tick_nb = NULL, 
    y_second_tick_nb = 1, 
    y_include_zero = FALSE, 
    y_top_extra_margin = 0.05, 
    y_bottom_extra_margin = 0.05, 
    stat_pos = "top", 
    stat_mean = FALSE, 
    stat_size = 4, 
    stat_dist = 5, 
    stat_angle = 0, 
    vertical = TRUE, 
    text_size = 12, 
    title = "", 
    title_text_size = 8, 
    legend_show = TRUE, 
    legend_width = 0.5, 
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
    # set.seed(1) ; obs1 <- data.frame(Time = c(rnorm(10), rnorm(10) + 2), Categ1 = rep(c("G", "H"), each = 10), stringsAsFactors = TRUE) ; set.seed(NULL) ; obs1$Time[1:10] <- NA ; data1 = obs1 ; y = "Time" ; categ = c("Categ1") ; categ_class_order = NULL ; categ_color = NULL ; box_legend_name = NULL ; box_fill = FALSE ; box_width = 0.5 ; box_space = 0.1 ; box_line_size = 0.75 ; box_notch = FALSE ; box_alpha = 1 ; box_mean = TRUE ; box_whisker_kind = "std" ; box_whisker_width = 0 ; dot_color = grey(0.25) ; dot_categ = NULL ; dot_categ_class_order = NULL ; dot_legend_name = NULL ; dot_tidy = FALSE ; dot_tidy_bin_nb = 50 ; dot_jitter = 0.5 ; dot_seed = 2 ; dot_size = 3 ; dot_alpha = 0.5 ; dot_border_size = 0.5 ; dot_border_color = NULL ; x_lab = NULL ; x_angle = 0 ; y_lab = NULL ; y_lim = NULL ; y_log = "no" ; y_tick_nb = NULL ; y_second_tick_nb = 1 ; y_include_zero = FALSE ; y_top_extra_margin = 0.05 ; y_bottom_extra_margin = 0.05 ; stat_pos = "top" ; stat_mean = FALSE ; stat_size = 4 ; stat_dist = 5 ; stat_angle = 0 ; vertical = TRUE ; text_size = 12 ; title = "" ; title_text_size = 8 ; legend_show = TRUE ; legend_width = 0.5 ; article = TRUE ; grid = FALSE ; add = NULL ; return = FALSE ; return_ggplot = FALSE ; return_gtable = TRUE ; plot = TRUE ; warn_print = FALSE ; safer_check = FALSE ; lib_path = NULL ; error_text = ""




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
        "data1", 
        "y", 
        "categ"
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
    tempo_arg <-base::c(
        "data1", 
        "y", 
        "categ", 
        # "categ_class_order", 
        # "categ_color", 
        # "box_legend_name", 
        "box_fill", 
        "box_width", 
        "box_space", 
        "box_line_size", 
        "box_notch", 
        "box_alpha", 
        "box_mean", 
        "box_whisker_kind", 
        "box_whisker_width", 
        # "dot_color", 
        # "dot_categ", 
        # "dot_categ_class_order", 
        # "dot_legend_name", 
        "dot_tidy", 
        "dot_tidy_bin_nb", 
        "dot_jitter", 
        # "dot_seed", 
        "dot_size", 
        "dot_alpha", 
        "dot_border_size", 
        # "dot_border_color", 
        # "x_lab", 
        "x_angle", 
        # "y_lab", 
        # "y_lim", 
        "y_log", 
        # "y_tick_nb", 
        # "y_second_tick_nb", 
        "y_include_zero", 
        "y_top_extra_margin", 
        "y_bottom_extra_margin", 
        # "stat_pos", 
        "stat_mean", 
        "stat_size", 
        "stat_dist", 
        "stat_angle", 
        "vertical", 
        "text_size", 
        "title", 
        "title_text_size", 
        "legend_show", 
        # "legend_width", 
        "article", 
        "grid", 
        # "add", 
        "return", 
        "return_ggplot", 
        "return_gtable", 
        "plot", 
        "warn_print", 
        "safer_check" 
        # "lib_path", # inactivated because can be NULL
        # "error_text" # inactivated because NULL converted to "" above
    )
    tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::is.null(x = x)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because get() is evaluated inside the FUN closure of the inner lapply: parent.frame(n = 1) is lapply's frame, parent.frame(n = 2) is BACKBONE's frame.
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
    # # before NA checking because is.na(logical()) is logical(0) (but secured with & base::length(x = x) > 0)
    tempo_arg <-base::c(
        "data1", 
        "y", 
        "categ", 
        "categ_class_order", 
        "categ_color", 
        "box_legend_name", 
        "box_fill", 
        "box_width", 
        "box_space", 
        "box_line_size", 
        "box_notch", 
        "box_alpha", 
        "box_mean", 
        "box_whisker_kind", 
        "box_whisker_width", 
        "dot_color", 
        "dot_categ", 
        "dot_categ_class_order", 
        "dot_legend_name", 
        "dot_tidy", 
        "dot_tidy_bin_nb", 
        "dot_jitter", 
        "dot_seed", 
        "dot_size", 
        "dot_alpha", 
        "dot_border_size", 
        "dot_border_color", 
        "x_lab", 
        "x_angle", 
        "y_lab", 
        "y_lim", 
        "y_log", 
        "y_tick_nb", 
        "y_second_tick_nb", 
        "y_include_zero", 
        "y_top_extra_margin", 
        "y_bottom_extra_margin", 
        "stat_pos", 
        "stat_mean", 
        "stat_size", 
        "stat_dist", 
        "stat_angle", 
        "vertical", 
        "text_size", 
        "title", 
        "title_text_size", 
        "legend_show", 
        "legend_width", 
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
    # Mandatory section : argument of safer-r functions cannot have NA as only value, to prevent all(, na.rm = TRUE) or any(, na.rm = TRUE) to return a logical value
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
                "scales::alpha",
                "scales::math_format",
                "scales::rescale_none",
                "scales::trans_format",
                "saferDev::arg_check", 
                "saferTool::comp_1d", 
                "saferTool::comp_2d", 
                "saferTool::name_change", 
                "saferTool::round2", 
                "saferGraph::scale2",
                "saferGraph::inter_ticks", 
                "saferGG::gg_palette", 
                "saferGG::gg_just", 
                "saferGG::gg_get_legend", 
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
    argum_check <- NULL
    text_check <- NULL
    checked_arg_names <- NULL # for function debbuging: used by r_debugging_tools
    arg_check_error_text <- base::paste0("ERROR ", embed_error_text, "\n\n", collapse = NULL, recycle0 = FALSE) # must be used instead of error_text = embed_error_text when several arg_check are performed on the same argument (tempo1, tempo2, see below)
    ee <- base::expression(argum_check <- base::c(argum_check, tempo$problem) , text_check <- base::c(text_check, tempo$text) , checked_arg_names <- base::c(checked_arg_names, tempo$object_name))
    # copy - paste ththe line below as much, one for each of the arguments of your own function
    tempo <- saferDev::arg_check(data = data1, class = "data.frame", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = y, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = categ, class = NULL, typeof = "character", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(x = categ_class_order)){
        tempo <- saferDev::arg_check(data = categ_class_order, class = "list", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }else{
        checked_arg_names <- base::c(checked_arg_names, "categ_class_order")
    }
    if( ! base::is.null(x = box_legend_name)){
        tempo <- saferDev::arg_check(data = box_legend_name, class = NULL, typeof = "character", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }else{
        checked_arg_names <- base::c(checked_arg_names, "box_legend_name")
    }
    if( ! base::is.null(x = categ_color)){
        tempo1 <- saferDev::arg_check(data = categ_color, class = NULL, typeof = "character", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        tempo2 <- saferDev::arg_check(data = categ_color, class = "factor", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        checked_arg_names <- base::c(checked_arg_names, tempo2$object_name)
        if(tempo1$problem == TRUE & tempo2$problem == TRUE){
            tempo_check_color <- saferDev::arg_check(data = categ_color, class = "integer", typeof = "integer", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)$problem
            if(tempo_check_color == TRUE){
                tempo_cat <- base::paste0(
                    arg_check_error_text, 
                    "categ_color ARGUMENT MUST BE A FACTOR OR CHARACTER VECTOR OR POSITVE INTEGER VECTOR",
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                text_check <- base::c(text_check, tempo_cat)
                argum_check <- base::c(argum_check, TRUE)
            }else if(base::any(categ_color == 0L, na.rm = TRUE)){
                tempo_cat <- base::paste0(
                    arg_check_error_text, 
                    "categ_color ARGUMENT MUST BE A FACTOR OR CHARACTER VECTOR OR POSITVE INTEGER VECTOR",
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                text_check <- base::c(text_check, tempo_cat)
                argum_check <- base::c(argum_check, TRUE)
            }
        }
    }else{
        checked_arg_names <- base::c(checked_arg_names, "categ_color")
    }
    tempo <- saferDev::arg_check(data = box_fill, class = NULL, typeof = NULL, mode = "logical", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = box_width, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = TRUE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = box_space, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = TRUE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = box_line_size, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = box_notch, class = NULL, typeof = NULL, mode = "logical", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = box_alpha, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = TRUE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = box_mean, class = NULL, typeof = NULL, mode = "logical", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = box_whisker_kind, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = base::c("no", "std", "max"), all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = "") ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = box_whisker_width, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = TRUE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(x = dot_color)){
        tempo1 <- saferDev::arg_check(data = dot_color, class = NULL, typeof = "character", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        tempo2 <- saferDev::arg_check(data = dot_color, class = "factor", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        checked_arg_names <- base::c(checked_arg_names, tempo2$object_name)
        if(tempo1$problem == TRUE & tempo2$problem == TRUE){
            tempo_check_color <- saferDev::arg_check(data = dot_color, class = "integer", typeof = "integer", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)$problem
            if(tempo_check_color == TRUE){
                tempo_cat <- base::paste0(
                    arg_check_error_text, 
                    "dot_color MUST BE A FACTOR OR CHARACTER VECTOR OR POSITVE INTEGER VECTOR",
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                text_check <- base::c(text_check, tempo_cat)
                argum_check <- base::c(argum_check, TRUE)
            }else if(base::any(dot_color == 0L, na.rm = TRUE)){
                tempo_cat <- base::paste0(
                    arg_check_error_text, 
                    "dot_color MUST BE A FACTOR OR CHARACTER VECTOR OR POSITVE INTEGER VECTOR",
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                text_check <- base::c(text_check, tempo_cat)
                argum_check <- base::c(argum_check, TRUE)
            }
        }
    }else{
        checked_arg_names <- base::c(checked_arg_names, "dot_color")
    }
    if( ! base::is.null(x = dot_categ)){
        tempo <- saferDev::arg_check(data = dot_categ, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }else{
        checked_arg_names <- base::c(checked_arg_names, "dot_categ")
    }
    if( ! base::is.null(x = dot_categ_class_order)){
        tempo <- saferDev::arg_check(data = dot_categ_class_order, class = NULL, typeof = "character", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }else{
        checked_arg_names <- base::c(checked_arg_names, "dot_categ_class_order")
    }
    if( ! base::is.null(x = dot_legend_name)){
        tempo <- saferDev::arg_check(data = dot_legend_name, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }else{
        checked_arg_names <- base::c(checked_arg_names, "dot_legend_name")
    }
    tempo <- saferDev::arg_check(data = dot_tidy, class = NULL, typeof = NULL, mode = "logical", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = dot_tidy_bin_nb, class = "integer", typeof = "integer", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if(tempo$problem == FALSE){
        if(dot_tidy_bin_nb == 0L){ # length and NA checked above
            tempo_cat <- base::paste0(
                arg_check_error_text, 
                "dot_tidy_bin_nb ARGUMENT MUST BE A NON-NULL AND POSITVE INTEGER VALUE",
                collapse = NULL, 
                recycle0 = FALSE
            )
            text_check <- base::c(text_check, tempo_cat)
            argum_check <- base::c(argum_check, TRUE)
        }
    }
    tempo <- saferDev::arg_check(data = dot_jitter, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = TRUE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(x = dot_seed)){
        tempo <- saferDev::arg_check(data = dot_seed, class = "integer", typeof = "integer", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }else{
        checked_arg_names <- base::c(checked_arg_names, "dot_seed")
    }
    tempo <- saferDev::arg_check(data = dot_size, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = dot_alpha, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = TRUE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = dot_border_size, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(x = dot_border_color)){
        tempo1 <- saferDev::arg_check(data = dot_border_color, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        tempo2 <- saferDev::arg_check(data = dot_border_color, class = "integer", typeof = "integer", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        checked_arg_names <- base::c(checked_arg_names, tempo2$object_name)
        if(tempo1$problem == TRUE & tempo2$problem == TRUE){
            tempo_cat <- base::paste0(
                arg_check_error_text, 
                "dot_border_color ARGUMENT MUST BE (1) A HEXADECIMAL COLOR STRING STARTING BY #, OR (2) A COLOR NAME GIVEN BY grDevices::colors(), OR (3) AN INTEGER VALUE",
                collapse = NULL, 
                recycle0 = FALSE
            )
            text_check <- base::c(text_check, tempo_cat)
            argum_check <- base::c(argum_check, TRUE)
        }else if(tempo1$problem == FALSE & tempo2$problem == TRUE){
            if( ! base::all(dot_border_color %in% grDevices::colors(distinct = FALSE) | base::grepl(pattern = "^#", x = dot_border_color, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), na.rm = TRUE)){
                tempo_cat <- base::paste0(
                    arg_check_error_text, 
                    "dot_border_color ARGUMENT MUST BE (1) A HEXADECIMAL COLOR STRING STARTING BY #, OR (2) A COLOR NAME GIVEN BY grDevices::colors(), OR (3) AN INTEGER VALUE",
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                text_check <- base::c(text_check, tempo_cat)
                argum_check <- base::c(argum_check, TRUE)
            }
        }
    }else{
        checked_arg_names <- base::c(checked_arg_names, "dot_border_color")
    }
    if( ! base::is.null(x = x_lab)){
        tempo1 <- saferDev::arg_check(data = x_lab, class = "expression", typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        tempo2 <- saferDev::arg_check(data = x_lab, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        checked_arg_names <- base::c(checked_arg_names, tempo2$object_name)
        if(tempo1$problem == TRUE & tempo2$problem == TRUE){
            tempo_cat <- base::paste0(
                arg_check_error_text, 
                "x_lab ARGUMENT MUST BE A SINGLE CHARACTER STRING OR EXPRESSION",
                collapse = NULL, 
                recycle0 = FALSE
            )
            text_check <- base::c(text_check, tempo_cat)
            argum_check <- base::c(argum_check, TRUE)
        }
    }else{
        checked_arg_names <- base::c(checked_arg_names, "x_lab")
    }
    tempo <- saferDev::arg_check(data = x_angle, class = "integer", typeof = "integer", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(x = y_lab)){
        tempo1 <- saferDev::arg_check(data = y_lab, class = "expression", typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        tempo2 <- saferDev::arg_check(data = y_lab, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        checked_arg_names <- base::c(checked_arg_names, tempo2$object_name)
        if(tempo1$problem == TRUE & tempo2$problem == TRUE){
            tempo_cat <- base::paste0(
                arg_check_error_text, 
                "y_lab ARGUMENT MUST BE A SINGLE CHARACTER STRING OR EXPRESSION",
                collapse = NULL, 
                recycle0 = FALSE
            )
            text_check <- base::c(text_check, tempo_cat)
            argum_check <- base::c(argum_check, TRUE)
        }
    }else{
        checked_arg_names <- base::c(checked_arg_names, "y_lab")
    }
    if( ! base::is.null(x = y_lim)){
        tempo <- saferDev::arg_check(data = y_lim, class = NULL, typeof = NULL, mode = "numeric", length = 2, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }else{
        checked_arg_names <- base::c(checked_arg_names, "y_lim")
    }
    tempo <- saferDev::arg_check(data = y_log, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = base::c("no", "log2", "log10"), all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = "") ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(x = y_tick_nb)){
        tempo <- saferDev::arg_check(data = y_tick_nb, class = "integer", typeof = "integer", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
        if(tempo$problem == FALSE){
            if(y_tick_nb < 0){
                tempo_cat <- base::paste0(
                    arg_check_error_text, 
                    "y_tick_nb ARGUMENT MUST BE A NON NULL POSITIVE INTEGER",
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                text_check <- base::c(text_check, tempo_cat)
                argum_check <- base::c(argum_check, TRUE)
            }
        }
    }else{
        checked_arg_names <- base::c(checked_arg_names, "y_tick_nb")
    }
    if( ! base::is.null(x = y_second_tick_nb)){
        tempo <- saferDev::arg_check(data = y_second_tick_nb, class = "integer", typeof = "integer", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
        if(tempo$problem == FALSE){
            if(y_second_tick_nb <= 0){
                tempo_cat <- base::paste0(
                    arg_check_error_text, 
                    "y_second_tick_nb ARGUMENT MUST BE A NON NULL POSITIVE INTEGER",
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                text_check <- base::c(text_check, tempo_cat)
                argum_check <- base::c(argum_check, TRUE)
            }
        }
    }else{
        checked_arg_names <- base::c(checked_arg_names, "y_second_tick_nb")
    }
    tempo <- saferDev::arg_check(data = y_include_zero, class = NULL, typeof = NULL, mode = "logical", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = y_top_extra_margin, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = TRUE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = y_bottom_extra_margin, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = TRUE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(x = stat_pos)){
        tempo <- saferDev::arg_check(data = stat_pos, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = base::c("top", "above"), all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = "") ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }else{
        checked_arg_names <- base::c(checked_arg_names, "stat_pos")
    }
    tempo <- saferDev::arg_check(data = stat_mean, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = stat_size, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = stat_dist, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = stat_angle, class = "integer", typeof = "integer", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = vertical, class = NULL, typeof = NULL, mode = "logical", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = text_size, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = title, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = title_text_size, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = legend_show, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(x = legend_width)){
        tempo <- saferDev::arg_check(data = legend_width, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = TRUE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }else{
        checked_arg_names <- base::c(checked_arg_names, "legend_width")
    }
    tempo <- saferDev::arg_check(data = article, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = grid, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(x = add)){
        tempo <- saferDev::arg_check(data = add, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }else{
        checked_arg_names <- base::c(checked_arg_names, "add")
    }
    tempo <- saferDev::arg_check(data = return, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = return_ggplot, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = return_gtable, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = plot, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
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
        "y", 
        "categ", 
        "box_legend_name", 
        "dot_categ", 
        "dot_categ_class_order", 
        "dot_legend_name", 
        # "title", 
        "add"
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
            "\nIf saferMake::saferMake() HAS BEEN USED TO MODIFY YOUR FUNCTION, PLEASE RESTART WITH YOUR MAIN CODE AND SELECT PROPER CLASS, TYPE AND/OR MODE FOR YOUR ARGUMENTS\n.HERE, ARGUMENTS ARE TYPE:\n", 
            base::paste0(base::sapply(X = tempo_arg, FUN = function(x){base::paste0(x, ": \"", base::typeof(x = base::get(x = x, pos = -1L, envir = base::parent.frame(n = 3), mode = "any", inherits = FALSE)), "\"", collapse = NULL, recycle0 = FALSE)}, simplify = TRUE, USE.NAMES = TRUE), collapse = "\n", recycle0 = FALSE), # parent.frame(n = 3) because the get() is evaluated inside the FUN closure of sapply. But sapply(FUN) calls lapply(FUN) internally !! So parent.frame(n = 1) is the internal lapply() frame called by sapply, parent.frame(n = 2) is sapply's frame, parent.frame(n = 3) is BACKBONE's frame. NB: paste0() and typeof() are primitives and add no frame.
            intern_error_text_end, 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        # nocov end
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
    # Warning: backbone modified here
    tempo_arg <- base::c(
        "categ_color", 
        "dot_color", 
        "dot_border_color"
        # "x_lab", 
        # "y_lab", 
    )
    # Warning: backbone modified here
    tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){if(base::is.null(x = x)){base::return(TRUE)}else{base::all(base::mode(x = x) == "character", na.rm = TRUE)}}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because get() is evaluated inside the FUN closure of the inner lapply: parent.frame(n = 1) is lapply's frame, parent.frame(n = 2) is BACKBONE's frame.
    if(base::any(tempo_log, na.rm = TRUE)){
        tempo_arg <- tempo_arg[tempo_log]
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
    ######## end management of "" in arguments of mode character

    #### end argument secondary checking

    #### second round of checking and data preparation

    ######## code that protects set.seed() in the global environment
    # optional section: remove the code if your own function has no seed argument
    # Warning: seeding is always at the .GlobalEnv level, whenever the seeding is applied inside a function, another envir, etc.
    # Warning: code changed compared to backbone seed -> dot_seed
    if (base::exists(x = ".Random.seed", where = -1, envir = .GlobalEnv, frame = , mode = "any", inherits = TRUE)) {
        # if .Random.seed does not exists, it means that no random operation has been performed yet in any R environment
        tempo_random_seed <- .Random.seed
        base::on.exit(expr = base::assign(x = ".Random.seed", value = tempo_random_seed, pos = -1, envir = .GlobalEnv, inherits = FALSE, immediate = TRUE), add = TRUE, after = TRUE)
    }else{
        base::on.exit(expr = base::set.seed(seed = NULL, kind = NULL, normal.kind = NULL, sample.kind = NULL), add = TRUE, after = TRUE) # inactivate seeding -> return to complete randomness
    }
    base::set.seed(seed = dot_seed, kind = NULL, normal.kind = NULL, sample.kind = NULL) # seed value is the seed argument of the function
    ######## end code that protects set.seed() in the global environment

    ######## warning initiation
    ini_warning_length <- base::options()$warning.length # required to have the max characters of output messages
    base::options(warning.length = 8170)
    base::on.exit(expr = base::options(warning.length = ini_warning_length), add = TRUE, after = TRUE) # registered here so that warning.length is restored even if an error occurs before the warning-output on.exit() registrations below
    warn <- NULL
    warn_count <- 0
    ######## end warning initiation

    ######## graphic device checking
    #Does not wrok for gg_box
    ######## end graphic device checking

    ######## other checkings
    # reserved word checking to avoid bugs (names of dataframe columns used in this function)
    reserved_words <- base::c("categ_check", "categ_color", "dot_color", "dot_categ", "dot.max", "dot.min", "group", "PANEL", "group.check", "MEAN", "tempo_categ1", "tempo_categ2", "text.max.pos", "text.min.pos", "x", "x.y", "y", "y_check", "y_from.dot.max", "ymax", "tidy_group", "binwidth")
    if(base::any(base::names(x = data1) %in% reserved_words, na.rm = TRUE)){
        if(base::any(base::duplicated(x = base::names(x = data1), incomparables = FALSE), na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                "DUPLICATED COLUMN NAMES OF data1 ARGUMENT NOT ALLOWED:\n", base::paste(base::names(x = data1)[base::duplicated(x = base::names(x = data1), incomparables = FALSE)], collapse = " ", sep = " ", recycle0 = FALSE),
                "\nCANNOT CONTAIN EMPTY STRING \"\".", 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
        }
        if( ! base::is.null(x = dot_categ)){
            if(dot_categ %in% categ){
                reserved_words <- base::c(reserved_words, base::paste0(dot_categ, "_DOT", collapse = NULL, recycle0 = FALSE)) # paste0(dot_categ, "_DOT") is added to the reserved words because in such situation, a new column will be added to data1 that is named paste0(dot_categ, "_DOT")
            }
        }
        tempo_output <- saferTool::name_change(data1 = base::names(x = data1), data2 = reserved_words, added_string = "_modif", duplicate = TRUE, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text)
        for(i2 in base::seq_along(along.with = tempo_output$ini)){ # a loop to be sure to take the good ones
            base::names(x = data1)[base::names(x = data1) == tempo_output$ini[i2]] <- tempo_output$post[i2]
            if(base::any(y == tempo_output$ini[i2], na.rm = FALSE)){ # any() without na.rm -> ok because y cannot be NA (tested above)
                y[y == tempo_output$ini[i2]] <- tempo_output$post[i2]
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") IN y ARGUMENT (COLUMN NAMES OF data1 ARGUMENT),\n", tempo_output$ini[i2], " HAS BEEN REPLACED BY ", tempo_output$post[i2], "\nBECAUSE RISK OF BUG AS SOME NAMES IN y ARGUMENT ARE RESERVED WORD USED BY THE ", function_name, " FUNCTION", collapse = NULL, recycle0 = FALSE)
                warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            }
            # WARNING: names of y argument potentially replaced
            if(base::any(categ == tempo_output$ini[i2], na.rm = FALSE)){ # any() without na.rm -> ok because categ cannot be NA (tested above)
                categ[categ == tempo_output$ini[i2]] <- tempo_output$post[i2]
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") IN categ ARGUMENT (COLUMN NAMES OF data1 ARGUMENT),\n", tempo_output$ini[i2], " HAS BEEN REPLACED BY ", tempo_output$post[i2], "\nBECAUSE RISK OF BUG AS SOME NAMES IN categ ARGUMENT ARE RESERVED WORD USED BY THE ", function_name, " FUNCTION", collapse = NULL, recycle0 = FALSE)
                warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            }
            # WARNING: names of categ argument potentially replaced
            if( ! base::is.null(x = dot_categ)){
                if(base::any(dot_categ == tempo_output$ini[i2], na.rm = FALSE)){ # any() without na.rm -> ok because dot_categ cannot be NA (tested above)
                    dot_categ[dot_categ == tempo_output$ini[i2]] <- tempo_output$post[i2]
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") IN dot_categ ARGUMENT (COLUMN NAMES OF data1 ARGUMENT),\n", tempo_output$ini[i2], " HAS BEEN REPLACED BY ", tempo_output$post[i2], "\nBECAUSE RISK OF BUG AS SOME NAMES IN dot_categ ARGUMENT ARE RESERVED WORD USED BY THE ", function_name, " FUNCTION", collapse = NULL, recycle0 = FALSE)
                    warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                }
            }
            # WARNING: names of dot_categ argument potentially replaced
        }
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count,") REGARDING COLUMN NAMES REPLACEMENT, THE NAMES\n", base::paste(tempo_output$ini, collapse = " ", sep = " ", recycle0 = FALSE), "\nHAVE BEEN REPLACED BY\n", base::paste(tempo_output$post, collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
        warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
        if( ! (base::is.null(x = add) | base::is.null(x = tempo_output$ini))){
            if(base::grepl(x = add, pattern = base::paste(tempo_output$ini, collapse = "|", sep = " ", recycle0 = FALSE), ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){
                tempo_cat <- base::paste0("ERROR IN ", function_name, "\nDETECTION OF COLUMN NAMES OF data1 IN THE add ARGUMENT STRING, THAT CORRESPOND TO RESERVED STRINGS FOR ", function_name, "\nCOLUMN NAMES HAVE TO BE CHANGED\nTHE PROBLEMATIC COLUMN NAMES ARE SOME OF THESE NAMES:\n", base::paste(tempo_output$ini, collapse = " ", sep = " ", recycle0 = FALSE), "\nIN THE DATA FRAME OF data1 AND IN THE STRING OF add ARGUMENT, TRY TO REPLACE NAMES BY:\n", base::paste(tempo_output$post, collapse = " ", sep = " ", recycle0 = FALSE), "\n\nFOR INFORMATION, THE RESERVED WORDS ARE:\n", base::paste(reserved_words, collapse = "\n", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
            }
        }
    }
    if( ! (base::is.null(x = add))){
        if(base::any(base::sapply(X = arg_names, FUN = grepl, x = add, simplify = TRUE, USE.NAMES = TRUE), na.rm = TRUE)){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") NAMES OF ", function_name, " ARGUMENTS DETECTED IN THE add STRING:\n", base::paste(arg_names[base::sapply(X = arg_names, FUN = grepl, x = add, simplify = TRUE, USE.NAMES = TRUE)], collapse = "\n", sep = " ", recycle0 = FALSE), "\nRISK OF WRONG OBJECT USAGE INSIDE ", function_name, collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
        }
    }
    # end reserved word checking
    if(base::any(base::duplicated(x = base::names(x = data1), incomparables = FALSE), na.rm = TRUE)){
        tempo_cat <- base::paste0("ERROR IN ", function_name, "\nDUPLICATED COLUMN NAMES OF data1 ARGUMENT NOT ALLOWED:\n", base::paste(base::names(x = data1)[base::duplicated(x = base::names(x = data1), incomparables = FALSE)], collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
    }
    if( ! (y %in% base::names(x = data1))){
        tempo_cat <- base::paste0("ERROR IN ", function_name, "\ny ARGUMENT MUST BE A COLUMN NAME OF data1", collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
    }else{

        tempo <- saferDev::arg_check(data = data1[, y], class = NULL, typeof = NULL, mode = "numeric", length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = "y COLUMN OF data1", data_arg = FALSE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        if(tempo$problem == TRUE){
            tempo_cat <- base::paste0("ERROR IN ", function_name, "\ny ARGUMENT MUST BE NUMERIC COLUMN IN data1", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
        }
    }
    if(base::length(x = categ) > 2){
        tempo_cat <- base::paste0("ERROR IN ", function_name, "\ncateg ARGUMENT CANNOT HAVE MORE THAN 2 COLUMN NAMES OF data1", collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
    }else if( ! base::all(categ %in% base::names(x = data1), na.rm = FALSE)){ # all() without na.rm -> ok because categ cannot be NA (tested above)
        tempo_cat <- base::paste0("ERROR IN ", function_name, "\ncateg ARGUMENT MUST BE COLUMN NAMES OF data1. HERE IT IS:\n", base::paste(categ, collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
    }
        # nocov start (unreachable: arg_check enforces dot_categ length 1 in the mandatory argument section, so this branch can never fire)
    if(base::length(x = dot_categ) > 1){
        tempo_cat <- base::paste0("ERROR IN ", function_name, "\ndot_categ ARGUMENT CANNOT HAVE MORE THAN 1 COLUMN NAMES OF data1", collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
        # nocov end
    }else if( ! base::all(dot_categ %in% base::names(x = data1), na.rm = FALSE)){ # all() without na.rm -> ok because dot_categ cannot be NA (tested above)
        tempo_cat <- base::paste0("ERROR IN ", function_name, "\ndot_categ ARGUMENT MUST BE COLUMN NAMES OF data1. HERE IT IS:\n", base::paste(dot_categ, collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
    }
    # verif of add
    if( ! base::is.null(x = add)){
        if( ! base::grepl(pattern = "^\\s*\\+", x = add, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){ # check that the add string start by +
            tempo_cat <- base::paste0("ERROR IN ", function_name, "\nadd ARGUMENT MUST START WITH \"+\": ", base::paste(base::unique(x = add, incomparables = FALSE), collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
        }else if( ! base::grepl(pattern = "(ggplot2|lemon)\\s*::", x = add, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){ #
            tempo_cat <- base::paste0("ERROR IN ", function_name, "\nFOR EASIER FUNCTION DETECTION, add ARGUMENT MUST CONTAIN \"ggplot2::\" OR \"lemon::\" IN FRONT OF EACH GGPLOT2 FUNCTION: ", base::paste(base::unique(x = add, incomparables = FALSE), collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
        }else if( ! base::grepl(pattern = ")\\s*$", x = add, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){ # check that the add string finished by )
            tempo_cat <- base::paste0("ERROR IN ", function_name, "\nadd ARGUMENT MUST FINISH BY \")\": ", base::paste(base::unique(x = add, incomparables = FALSE), collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
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
        if(base::any(base::grepl(x = tempo, pattern = "ggplot2::facet_wrap|lemon::facet_rep_wrap", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), na.rm = TRUE)){
            tempo1 <- base::suppressWarnings(expr = base::eval(expr = base::parse(text = tempo[base::grepl(x = tempo, pattern = "ggplot2::facet_wrap|lemon::facet_rep_wrap", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)], file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)), classes = "warning")
            facet_categ <- base::names(x = tempo1$params$facets)
            tempo_text <- "facet_wrap OR facet_rep_wrap"
            facet_check <- FALSE
        }else if(base::grepl(x = add, pattern = "ggplot2::facet_grid|lemon::facet_rep_grid", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){
            tempo1 <- base::suppressWarnings(expr = base::eval(expr = base::parse(text = tempo[base::grepl(x = tempo, pattern = "ggplot2::facet_grid|lemon::facet_rep_grid", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)], file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)), classes = "warning")
            facet_categ <- base::c(base::names(x = tempo1$params$rows), base::names(x = tempo1$params$cols))
            tempo_text <- "facet_grid OR facet_rep_grid"
            facet_check <- FALSE
        }
        if(facet_check == FALSE & ! base::all(facet_categ %in% base::names(x = data1), na.rm = FALSE)){ # WARNING: all(facet_categ %in% names(data1)) is TRUE when facet_categ is NULL # all() without na.rm -> ok because facet_categ cannot be NA (tested above)
            tempo_cat <- base::paste0("ERROR IN ", function_name, "\nDETECTION OF \"", tempo_text, "\" STRING IN THE add ARGUMENT BUT PROBLEM OF VARIABLE DETECTION (COLUMN NAMES OF data1)\nTHE DETECTED VARIABLES ARE:\n", base::paste(facet_categ, collapse = " ", sep = " ", recycle0 = FALSE), "\nTHE data1 COLUMN NAMES ARE:\n", base::paste(base::names(x = data1), collapse = " ", sep = " ", recycle0 = FALSE), "\nPLEASE REWRITE THE add STRING AND RERUN", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of add containing facet
    # conversion of categ columns in data1 into factors
    for(i1 in base::seq_along(along.with = categ)){
        tempo1 <- saferDev::arg_check(data = data1[, categ[i1]], class = NULL, typeof = NULL, mode = "character", length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = base::paste0("categ NUMBER ", i1, " OF data1", collapse = NULL, recycle0 = FALSE), data_arg = FALSE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        tempo2 <- saferDev::arg_check(data = data1[, categ[i1]], class = "factor", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = base::paste0("categ NUMBER ", i1, " OF data1", collapse = NULL, recycle0 = FALSE), data_arg = FALSE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        if(tempo1$problem == TRUE & tempo2$problem == TRUE){
            tempo_cat <- base::paste0(
                arg_check_error_text, 
                base::paste0("categ NUMBER ", i1, " OF data1", collapse = NULL, recycle0 = FALSE), 
                " MUST BE A FACTOR OR CHARACTER VECTOR.",
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
        }else if(tempo1$problem == FALSE){ # character vector
            if(box_alpha != 0){
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") IN categ NUMBER ", i1, " IN data1, THE CHARACTER COLUMN HAS BEEN CONVERTED TO FACTOR, WITH LEVELS ACCORDING TO THE ALPHABETICAL ORDER", collapse = NULL, recycle0 = FALSE)
                warn <-base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            }
        }
        data1[, categ[i1]] <- base::factor(x = data1[, categ[i1]], exclude = NA, ordered = base::is.ordered(x = data1[, categ[i1]]), nmax = NA) # if already a factor, change nothing, if characters, levels according to alphabetical order
    }
    # OK: all the categ columns of data1 are factors from here
    # end conversion of categ columns in data1 into factors
    
    # management of log scale and Inf removal
    if(base::any(( ! base::is.finite(x = data1[, y])) & ( ! base::is.na(x = data1[, y])), na.rm = FALSE)){ # is.finite also detects NA: ( ! is.finite(data1[, y])) & ( ! is.na(data1[, y])) detects only Inf  # normally no NA with is.finite0() and is.na()
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count,") PRESENCE OF -Inf OR Inf VALUES IN THE ", y, " COLUMN OF THE data1 ARGUMENT AND CORRESPONDING ROWS REMOVED (SEE $removed_row_nb AND $removed_rows)", collapse = NULL, recycle0 = FALSE)
        warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
    }
    data1_ini <- data1 # strictly identical to data1 except that in data1 y is log converted if and only if y_log != "no"
    if(y_log != "no"){
        tempo1 <- ! base::is.finite(x = data1[, y]) # where are initial NA and Inf
        data1[, y] <- base::suppressWarnings(expr = base::get(x = y_log, pos = -1L, envir = base::environment(fun = NULL), mode = "any", inherits = TRUE)(data1[, y]), classes = "warning")# no env = sys.nframe(), inherit = FALSE in get() because look for function in the classical scope
        if(base::any( ! (tempo1 | base::is.finite(x = data1[, y])), na.rm = FALSE)){ # normally no NA with is.finite
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") LOG CONVERSION INTRODUCED -Inf OR Inf OR NaN VALUES IN THE ", y, " COLUMN OF THE data1 ARGUMENT AND CORRESPONDING ROWS REMOVED (SEE $removed_row_nb AND $removed_rows)", collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
        }
    }
    # Inf removal
    if(base::any(( ! base::is.finite(x = data1[, y])) & ( ! base::is.na(x = data1[, y])), na.rm = FALSE)){ # is.finite also detects NA: ( ! is.finite(data1[, y])) & ( ! is.na(data1[, y])) detects only Inf # normally no NA with is.finite
        removed_row_nb <- base::which(x = ( ! base::is.finite(x = data1[, y])) & ( ! base::is.na(x = data1[, y])), arr.ind = FALSE, useNames = TRUE)
        removed_rows <- data1_ini[removed_row_nb, ] # here data1_ini used to have the y = O rows that will be removed because of Inf creation after log transformation
        data1 <- data1[-removed_row_nb, ] #
        data1_ini <- data1_ini[-removed_row_nb, ] #
    }else{
        removed_row_nb <- NULL
        removed_rows <- base::data.frame(stringsAsFactors = FALSE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE)
    }
    # From here, data1 and data_ini have no more Inf
    # end Inf removal
    if(y_log != "no" & ! base::is.null(x = y_lim)){
        if(base::any(y_lim <= 0, na.rm = FALSE)){ # any() without na.rm -> ok because y_lim cannot be NA (tested above)
            tempo_cat <- base::paste0("ERROR IN ", function_name, "\ny_lim ARGUMENT CANNOT HAVE ZERO OR NEGATIVE VALUES WITH THE y_log ARGUMENT SET TO ", y_log, ":\n", base::paste(y_lim, collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
            # nocov start (unreachable: y_lim <= 0 is caught by the previous branch, and log10/log2 of a positive finite value is always finite)
        }else if(base::any( ! base::is.finite(x = if(y_log == "log10"){base::log10(x = y_lim)}else{base::log2(x = y_lim)}), na.rm = FALSE)){ # normally no NA with is.finite
            tempo_cat <- base::paste0("ERROR IN ", function_name, "\ny_lim ARGUMENT RETURNS INF/NA WITH THE y_log ARGUMENT SET TO ", y_log, "\nAS SCALE COMPUTATION IS ", base::ifelse(test = y_log == "log10", yes = "log10", no = "log2"), ":\n", base::paste(if(y_log == "log10"){base::log10(x = y_lim)}else{base::log2(x = y_lim)}, collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
            # nocov end
        }
    }
    if(y_log != "no" & y_include_zero == TRUE){
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count,") y_log ARGUMENT SET TO ", y_log, " AND y_include_zero ARGUMENT SET TO TRUE -> y_include_zero ARGUMENT RESET TO FALSE BECAUSE 0 VALUE CANNOT BE REPRESENTED IN LOG SCALE", collapse = NULL, recycle0 = FALSE)
        warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
        y_include_zero <- FALSE
    }
    if(y_log != "no" & vertical == FALSE){
        vertical <- TRUE
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count,") BECAUSE OF A BUG IN ggplot2, CANNOT FLIP BOXES HORIZONTALLY WITH A Y_LOG2 SCALE -> vertical ARGUMENT RESET TO TRUE", collapse = NULL, recycle0 = FALSE)
        warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
    }
    # end management of log scale and Inf removal
    # na detection and removal (done now to be sure of the correct length of categ)
    column_check <- base::unique(x = base::c(y, categ, if( ! base::is.null(x = dot_color) & ! base::is.null(x = dot_categ)){dot_categ}, if( ! base::is.null(x = facet_categ)){facet_categ}), incomparables = FALSE) # dot_categ because can be a 3rd column of data1, categ_color and dot_color will be tested later
    if(base::any(base::is.na(x = data1[, column_check]), na.rm = FALSE)){ # data1 used here instead of data1_ini in case of new NaN created by log conversion (neg values) # normally no NA with is.na
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count,") NA DETECTED IN COLUMNS OF data1 AND CORRESPONDING ROWS REMOVED (SEE $removed_row_nb AND $removed_rows)", collapse = NULL, recycle0 = FALSE)
        warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
        for(i2 in base::seq_along(along.with = column_check)){
            if(base::any(base::is.na(x = data1[, column_check[i2]]), na.rm = FALSE)){ # normally no NA with is.na
                tempo_warn <- base::paste0("NA REMOVAL DUE TO COLUMN ", column_check[i2], " OF data1", collapse = NULL, recycle0 = FALSE)
                warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            }
        }
        tempo <- base::unique(x = base::unlist(x = base::lapply(X = base::lapply(X = base::c(data1[column_check]), FUN = is.na), FUN = which), recursive = TRUE, use.names = TRUE), incomparables = FALSE)
        removed_row_nb <- base::c(removed_row_nb, tempo) # removed_row_nb created to remove Inf
        removed_rows <- base::rbind(removed_rows, data1_ini[tempo, ], stringsAsFactors = FALSE, deparse.level = 1) # here data1_ini used to have the non NA rows that will be removed because of NAN creation after log transformation (neg values for instance)
        column_check <- column_check[ ! column_check == y] # remove y to keep quali columns
        if(base::length(x = tempo) != 0){
            data1 <- data1[-tempo, ] # WARNING tempo here and not removed_row_nb because the latter contain more numbers thant the former
            data1_ini <- data1_ini[-tempo, ] # WARNING tempo here and not removed_row_nb because the latter contain more numbers than the former
            for(i3 in base::seq_along(along.with = column_check)){
                if(base::any( ! base::unique(x = removed_rows[, column_check[i3]], incomparables = FALSE) %in% base::unique(x = data1[, column_check[i3]], incomparables = FALSE), na.rm = TRUE)){
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") IN COLUMN ", column_check[i3], " OF data1, THE FOLLOWING CLASSES HAVE DISAPPEARED AFTER NA/Inf REMOVAL (IF COLUMN USED IN THE PLOT, THIS CLASS WILL NOT BE DISPLAYED):\n", base::paste(base::unique(x = removed_rows[, column_check[i3]], incomparables = FALSE)[ ! base::unique(x = removed_rows[, column_check[i3]], incomparables = FALSE) %in% base::unique(x = data1[, column_check[i3]], incomparables = FALSE)], collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
                    warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                }
            }
        }
        count_categ <- 0
        for(i2 in base::seq_along(along.with = column_check)){
            if(column_check[i2] %in% categ){
                count_categ <- count_categ + 1
            }
            if(column_check[i2] == categ[count_categ]){
                categ_class_order[count_categ] <- base::list(base::levels(x = data1[, column_check[i2]])[base::levels(x = data1[, column_check[i2]]) %in% base::unique(x = data1[, column_check[i2]], incomparables = FALSE)]) # remove the absent color in the character vector
                data1[, column_check[i2]] <- base::factor(x = base::as.character(x = data1[, column_check[i2]]), levels = base::unique(x = categ_class_order[[count_categ]], incomparables = FALSE), exclude = NA, ordered = FALSE, nmax = NA)
            }
            if( ! base::is.null(x = dot_color) & ! base::is.null(x = dot_categ)){ # reminder : dot_categ cannot be a column name of categ anymore (because in that case dot_categ name is changed into "..._DOT"
                if(column_check[i2] == dot_categ){
                    dot_categ_class_order <- base::levels(x = data1[, column_check[i2]])[base::levels(x = data1[, column_check[i2]]) %in% base::unique(x = data1[, column_check[i2]], incomparables = FALSE)] # remove the absent color in the character vector
                    data1[, column_check[i2]] <- base::factor(x = base::as.character(x = data1[, column_check[i2]]), levels = base::unique(x = dot_categ_class_order, incomparables = FALSE), exclude = NA, ordered = FALSE, nmax = NA)
                }
            }
            if(column_check[i2] %in% facet_categ){ # works if facet_categ == NULL this method should keep the order of levels when removing some levels
                tempo_levels <- base::levels(x = data1[, column_check[i2]])[base::levels(x = data1[, column_check[i2]]) %in% base::unique(x = base::as.character(x = data1[, column_check[i2]]), incomparables = FALSE)]
                data1[, column_check[i2]] <- base::factor(x = base::as.character(x = data1[, column_check[i2]]), levels = tempo_levels, exclude = NA, ordered = FALSE, nmax = NA)
            }
        }
    }
    # end na detection and removal (done now to be sure of the correct length of categ)
    # From here, data1 and data_ini have no more NA or NaN in y, categ, dot_categ (if dot_color != NULL) and facet_categ
    
    if( ! base::is.null(x = categ_class_order)){
        if(base::length(x = categ_class_order) != base::length(x = categ)){
            tempo_cat <- base::paste0("ERROR IN ", function_name, "\ncateg_class_order ARGUMENT MUST BE A LIST OF LENGTH EQUAL TO LENGTH OF categ\nHERE IT IS LENGTH: ", base::length(x = categ_class_order), " VERSUS ", base::length(x = categ), collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
        }else{
            for(i3 in base::seq_along(along.with = categ_class_order)){
                if(base::is.null(x = categ_class_order[[i3]])){
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") THE categ_class_order COMPARTMENT ", i3, " IS NULL. ALPHABETICAL ORDER WILL BE APPLIED", collapse = NULL, recycle0 = FALSE)
                    warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                    data1[, categ[i3]] <- base::factor(x = base::as.character(x = data1[, categ[i3]]), exclude = NA, ordered = FALSE, nmax = NA) # if already a factor, change nothing, if characters, levels according to alphabetical order
                    categ_class_order[[i3]] <- base::levels(x = data1[, categ[i3]]) # character vector that will be used later
                }else{
                    tempo <- saferDev::arg_check(data = categ_class_order[[i3]], class = NULL, typeof = NULL, mode = "character", length = base::length(x = base::levels(x = data1[, categ[i3]])), prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = base::paste0("COMPARTMENT ", i3 , " OF categ_class_order ARGUMENT", collapse = NULL, recycle0 = FALSE), data_arg = FALSE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)# length(data1[, categ[i1]) -> if data1[, categ[i1] was initially character vector, then conversion as factor after the NA removal, thus class number ok. If data1[, categ[i1] was initially factor, no modification after the NA removal, thus class number ok
                    if(tempo$problem == TRUE){
                        tempo_cat <- tempo$text
                        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                    }
                }
                if(base::any(base::duplicated(x = categ_class_order[[i3]], incomparables = FALSE), na.rm = TRUE)){
                    tempo_cat <- base::paste0("ERROR IN ", function_name, "\nCOMPARTMENT ", i3, " OF categ_class_order ARGUMENT CANNOT HAVE DUPLICATED CLASSES: ", base::paste(categ_class_order[[i3]], collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
                }else if( ! (base::all(categ_class_order[[i3]] %in% base::unique(x = data1[, categ[i3]], incomparables = FALSE), na.rm = TRUE) & base::all(base::unique(x = data1[, categ[i3]], incomparables = FALSE) %in% categ_class_order[[i3]], na.rm = TRUE))){
                    tempo_cat <- base::paste0("ERROR IN ", function_name, "\nCOMPARTMENT ", i3, " OF categ_class_order ARGUMENT MUST BE CLASSES OF ELEMENT ", i3, " OF categ ARGUMENT\nHERE IT IS:\n", base::paste(categ_class_order[[i3]], collapse = " ", sep = " ", recycle0 = FALSE), "\nFOR COMPARTMENT ", i3, " OF categ_class_order AND IT IS:\n", base::paste(base::unique(x = data1[, categ[i3]], incomparables = FALSE), collapse = " ", sep = " ", recycle0 = FALSE), "\nFOR COLUMN ", categ[i3], " OF data1", collapse = NULL, recycle0 = FALSE)
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
                }else{
                    data1[, categ[i3]] <- base::factor(x = data1[, categ[i3]], levels = categ_class_order[[i3]], exclude = NA, ordered = base::is.ordered(x = data1[, categ[i3]]), nmax = NA) # reorder the factor
                    
                }
                base::names(x = categ_class_order)[i3] <- categ[i3]
            }
        }
    }else{
        categ_class_order <- base::vector(mode = "list", length = base::length(x = categ))
        tempo_categ_class_order <- NULL
        for(i2 in base::seq_along(along.with = categ_class_order)){
            categ_class_order[[i2]] <- base::levels(x = data1[, categ[i2]])
            base::names(x = categ_class_order)[i2] <- categ[i2]
            tempo_categ_class_order <- base::c(tempo_categ_class_order, base::ifelse(test = i2 != 1, yes = "\n", no = ""), categ_class_order[[i2]])
        }
        if(box_alpha != 0){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") THE categ_class_order SETTING IS NULL. ALPHABETICAL ORDER WILL BE APPLIED FOR BOX ORDERING:\n", base::paste(tempo_categ_class_order, collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
        }
    }
    # categ_class_order not NULL anymore (list)
    if(base::is.null(x = box_legend_name) & box_alpha != 0){
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count,") THE box_legend_name SETTING IS NULL. NAMES OF categ WILL BE USED: ", base::paste(categ, collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
        warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
        box_legend_name <- categ[base::length(x = categ)] # if only categ1, then legend name of categ1, if length(categ) == 2L, then legend name of categ2
    }
    # box_legend_name not NULL anymore (character string)
    # management of categ_color
    if( ! base::is.null(x = categ_color)){
        # check the nature of color
        # integer colors into gg_palette
        tempo_check_color <- saferDev::arg_check(data = categ_color, class = "integer", typeof = "integer", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = "y COLUMN OF data1", data_arg = FALSE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)$problem
        if(tempo_check_color == FALSE){
            # convert integers into colors
            categ_color <- saferGG::gg_palette(n = base::max(categ_color, na.rm = TRUE), kind = "std", safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)[categ_color]
        }
        # end integer colors into gg_palette
        if( ! (base::all(categ_color %in% grDevices::colors(distinct = FALSE) | base::grepl(pattern = "^#", x = categ_color, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) | base::is.na(x = categ_color), na.rm = FALSE))){ # check that all strings of low.color start by #. NA allowed (grepl() returns FALSE for NA, so na.rm would not help): NA warned below
            tempo_cat <- base::paste0("ERROR IN ", function_name, "\ncateg_color ARGUMENT MUST BE A HEXADECIMAL COLOR VECTOR STARTING BY # AND/OR COLOR NAMES GIVEN BY grDevices::colors() OR A COLUMN NAME OF THE data1 PARAMETER: ", base::paste(base::unique(x = categ_color, incomparables = FALSE), collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
        }
        if(base::any(base::is.na(x = categ_color), na.rm = FALSE) & box_alpha != 0){ # normally no NA with is.na
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") categ_color ARGUMENT CONTAINS NA", collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
        }
        # end check the nature of color
        # check the length of color
        categ_len <- base::length(x = categ) # if only categ1, then colors for classes of categ1, if length(categ) == 2L, then colors for classes of categ2
        if(base::length(x = data1[, categ[categ_len]]) == base::length(x = base::levels(x = data1[, categ[categ_len]])) & base::length(x = categ_color) == base::length(x = data1[, categ[categ_len]])){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") THE NUMBER OF CLASSES OF THE COLUMN ", categ[categ_len], " THE NUMBER OF ROWS OF THIS COLUMN AND THE NUMBER OF COLORS OF THE categ_color ARGUMENT ARE ALL EQUAL. BOX COLORS WILL BE ATTRIBUTED ACCORDING THE LEVELS OF ", categ[categ_len], ", NOT ACCORDING TO THE ROWS OF ", categ[categ_len], collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
        }
        if(base::length(x = categ_color) == base::length(x = base::levels(x = data1[, categ[categ_len]]))){ # here length(categ_color) is equal to the different number of categ
            # data1[, categ[categ_len]] <- factor(data1[, categ[categ_len]]) # not required because sure that is is a factor
            data1 <- base::data.frame(data1, categ_color = data1[, categ[categ_len]], stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE)  # no need stringsAsFactors here for stat_nolog as factors remain factors
            data1$categ_color <- base::factor(x = base::as.character(x = data1$categ_color), levels = base::levels(x = data1[, categ[categ_len]]), labels = categ_color, exclude = NA, ordered = FALSE, nmax = NA) # replace the characters of data1[, categ[categ_len]] put in the categ_color column by the categ_color (explicit levels keep the labels length-matched if some classes have disappeared after NA/Inf removal)
            if(box_alpha != 0){
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") IN ", categ[categ_len], " OF categ ARGUMENT, THE FOLLOWING COLORS:\n", base::paste(categ_color, collapse = " ", sep = " ", recycle0 = FALSE), "\nHAVE BEEN ATTRIBUTED TO THESE CLASSES:\n", base::paste(base::levels(x = base::factor(x = data1[, categ[categ_len]], exclude = NA, ordered = base::is.ordered(x = data1[, categ[categ_len]]), nmax = NA)), collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
                warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            }
        }else if(base::length(x = categ_color) == base::length(x = data1[, categ[categ_len]])){# here length(categ_color) is equal to nrow(data1) -> Modif to have length(categ_color) equal to the different number of categ (length(categ_color) == length(levels(data1[, categ[categ_len]])))
            data1 <- base::data.frame(data1, categ_color = categ_color, stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE)
            tempo_check <- base::unique(x = data1[ , base::c(categ[categ_len], "categ_color")], incomparables = FALSE)
            if( ! (base::nrow(x = tempo_check) == base::length(x = base::unique(x = categ_color, incomparables = FALSE)) & base::nrow(x = tempo_check) == base::length(x = base::unique(x = data1[ , categ[categ_len]], incomparables = FALSE)))){
                tempo_cat <- base::paste0("ERROR IN ", function_name, "\ncateg_color ARGUMENT HAS THE LENGTH OF data1 ROW NUMBER\nBUT IS INCORRECTLY ASSOCIATED TO EACH CLASS OF categ ", categ[categ_len], ":\n", base::paste(base::unique(x = base::mapply(FUN = base::paste,data1[ ,categ[categ_len]], data1[ ,"categ_color"], MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE), incomparables = FALSE), collapse = "\n", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
            }else{
                # data1[, categ[categ_len]] <- factor(data1[, categ[categ_len]]) # not required because sure that is is a factor
                categ_color <- base::unique(x = data1$categ_color[base::order(data1[, categ[categ_len]], na.last = TRUE, decreasing = FALSE, method = "auto")], incomparables = FALSE) # Modif to have length(categ_color) equal to the different number of categ (length(categ_color) == length(levels(data1[, categ[categ_len]])))
                if(box_alpha != 0){
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") categ_color ARGUMENT HAS THE LENGTH OF data1 ROW NUMBER\nCOLORS HAVE BEEN RESPECTIVELY ASSOCIATED TO EACH CLASS OF categ ", categ[categ_len], " AS:\n", base::paste(base::levels(x = base::factor(x = data1[, categ[categ_len]], exclude = NA, ordered = base::is.ordered(x = data1[, categ[categ_len]]), nmax = NA)), collapse = " ", sep = " ", recycle0 = FALSE), "\n", base::paste(categ_color, collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
                    warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                }
            }
        }else if(base::length(x = categ_color) == 1L){
            # data1[, categ[categ_len]] <- factor(data1[, categ[categ_len]]) # not required because sure that is is a factor
            data1 <- base::data.frame(data1, categ_color = categ_color, stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE)
            categ_color <- base::rep(x = categ_color, base::length(x = base::levels(x = data1[, categ[categ_len]])))
            if(box_alpha != 0){
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") categ_color ARGUMENT HAS LENGTH 1, MEANING THAT ALL THE DIFFERENT CLASSES OF ", categ[categ_len], "\n", base::paste(base::levels(x = base::factor(x = data1[, categ[categ_len]], exclude = NA, ordered = base::is.ordered(x = data1[, categ[categ_len]]), nmax = NA)), collapse = " ", sep = " ", recycle0 = FALSE), "\nWILL HAVE THE SAME COLOR\n", base::paste(categ_color, collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
                warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            }
        }else{
            tempo_cat <- base::paste0("ERROR IN ", function_name, "\ncateg_color ARGUMENT MUST BE (1) LENGTH 1, OR (2) THE LENGTH OF data1 NROWS AFTER NA/Inf REMOVAL, OR (3) THE LENGTH OF THE CLASSES IN THE categ ", categ[categ_len], " COLUMN. HERE IT IS COLOR LENGTH ", base::length(x = categ_color), " VERSUS CATEG LENGTH ", base::length(x = data1[, categ[categ_len]]), " AND CATEG CLASS LENGTH ", base::length(x = base::unique(x = data1[, categ[categ_len]], incomparables = FALSE)), "\nPRESENCE OF NA/Inf COULD BE THE PROBLEM", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
        }
    }else{
        categ_len <- base::length(x = categ) # if only categ1, then colors for classes of categ1, if length(categ) == 2L, then colors for classes of categ2
        # data1[, categ[categ_len]] <- factor(data1[, categ[categ_len]]) # not required because sure that is is a factor
        categ_color <- saferGG::gg_palette(n = base::length(x = base::levels(x = data1[, categ[categ_len]])), kind = "std", safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        data1 <- base::data.frame(data1, categ_color = data1[, categ[categ_len]], stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE)
        data1$categ_color <- base::factor(x = base::as.character(x = data1$categ_color), levels = base::levels(x = data1[, categ[categ_len]]), labels = categ_color, exclude = NA, ordered = FALSE, nmax = NA)  # replace the characters of data1[, categ[categ_len]] put in the categ_color column by the categ_color (explicit levels keep the labels length-matched if some classes have disappeared after NA/Inf removal)
        if(box_alpha != 0){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") NULL categ_color ARGUMENT -> COLORS RESPECTIVELY ATTRIBUTED TO EACH CLASS OF ", categ[categ_len], " IN data1:\n", base::paste(categ_color, collapse = " ", sep = " ", recycle0 = FALSE), "\n", base::paste(base::levels(x = data1[, categ[categ_len]]), collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
        }
    }
    # categ_color not NULL anymore
    categ_color <- base::as.character(x = categ_color)
    # categ_color is a character string representing the diff classes
    data1$categ_color <- base::factor(x = data1$categ_color, levels = base::unique(x = categ_color, incomparables = FALSE), exclude = NA, ordered = base::is.ordered(x = data1$categ_color), nmax = NA) # ok because if categ_color is a character string, the order make class 1, class 2, etc. unique() because no duplicates allowed
    # data1$categ_color is a factor with order of levels -> categ_color
    # end management of categ_color
    # management of dot_color
    if( ! base::is.null(x = dot_color)){
        # optional legend of dot colors
        if( ! base::is.null(x = dot_categ)){
            ini_dot_categ <- dot_categ
            # nocov start (unreachable: the dot_categ checking block above catches all missing columns; reserved-word renames keep dot_categ aligned with data1 column names)
        if( ! dot_categ %in% base::names(x = data1)){ # no need to use all() because length(dot_categ) = 1
                tempo_cat <- base::paste0("ERROR IN ", function_name, "\ndot_categ ARGUMENT MUST BE A COLUMN NAME OF data1. HERE IT IS:\n", dot_categ, collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
            }else if(dot_categ %in% categ){ # no need to use all() because length(dot_categ) = 1. Do not use dot_categ %in% categ[length(categ)] -> error
            # nocov end
                # management of dot legend if dot_categ %in% categ (because legends with the same name are joined in ggplot2) 
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") THE COLUMN NAME OF data1 INDICATED IN THE dot_categ ARGUMENT (", dot_categ, ") HAS BEEN REPLACED BY ", base::paste0(dot_categ, "_DOT", collapse = NULL, recycle0 = FALSE), " TO AVOID MERGED LEGEND BY GGPLOT2", collapse = NULL, recycle0 = FALSE)
                warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                data1 <- base::data.frame(data1, dot_categ = data1[, dot_categ], stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE) # dot_categ is not a column name of data1 (checked above with reserved words)
                dot_categ <- base::paste0(dot_categ, "_DOT", collapse = NULL, recycle0 = FALSE)
                base::names(x = data1)[base::names(x = data1) == "dot_categ"] <- dot_categ # paste0(dot_categ, "_DOT") is not a column name of data1 (checked above with reserved words)
                # tempo_cat <- paste0("ERROR IN ", function_name, "\ndot_categ ARGUMENT CANNOT BE A COLUMN NAME OF data1 ALREADY SPECIFIED IN THE categ ARGUMENT:\n", dot_categ, "\nINDEED, dot_categ ARGUMENT IS MADE TO HAVE MULTIPLE DOT COLORS NOT RELATED TO THE BOXPLOT CATEGORIES")
                # stop(paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
        # nocov start (unreachable: data1[, categ] is converted to factor at the categ conversion block above, so tempo2$problem can never be TRUE here)
            tempo1 <- saferDev::arg_check(data = data1[, dot_categ], class = NULL, typeof = NULL, mode = "character", length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = base::paste0(dot_categ, " COLUMN OF data1", collapse = NULL, recycle0 = FALSE), data_arg = FALSE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
            tempo2 <- saferDev::arg_check(data = data1[, categ[i1]], class = "factor", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = base::paste0(dot_categ, " COLUMN OF data1", collapse = NULL, recycle0 = FALSE), data_arg = FALSE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
            if(tempo1$problem == TRUE & tempo2$problem == TRUE){
                tempo_cat <- base::paste0(
                    arg_check_error_text, 
                    "dot_categ COLUMN MUST BE A FACTOR OR CHARACTER VECTOR.",
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
            }
        # nocov end
            data1[, dot_categ] <- base::factor(x = data1[, dot_categ], exclude = NA, ordered = base::is.ordered(x = data1[, dot_categ]), nmax = NA) # if already a factor, change nothing, if characters, levels according to alphabetical order
            # dot_categ column of data1 is factor from here
            if( ! base::is.null(x = dot_categ_class_order)){
                if(base::any(base::duplicated(x = dot_categ_class_order, incomparables = FALSE), na.rm = TRUE)){
                    tempo_cat <- base::paste0(
                        error_text_start, 
                        "dot_categ_class_order ARGUMENT CANNOT HAVE DUPLICATED CLASSES: ", base::paste(dot_categ_class_order, collapse = " ", recycle0 = FALSE, sep = " "),
                        collapse = NULL, 
                        recycle0 = FALSE
                    )
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
                }else if( ! (base::all(dot_categ_class_order %in% base::levels(x = data1[, dot_categ]), na.rm = FALSE) & base::all(base::levels(x = data1[, dot_categ]) %in% dot_categ_class_order, na.rm = TRUE))){
                    tempo_cat <- base::paste0(
                        error_text_start, 
                        "dot_categ_class_order ARGUMENT MUST BE CLASSES OF dot_categ ARGUMENT\nHERE IT IS:\n", 
                        base::paste(dot_categ_class_order, collapse = " ", sep = " ", recycle0 = FALSE), 
                        "\nFOR dot_categ_class_order AND IT IS:\n", 
                        base::paste(base::levels(x = data1[, dot_categ]), collapse = " ", sep = " ", recycle0 = FALSE), 
                        "\nFOR dot_categ COLUMN (", 
                        ini_dot_categ, 
                        ") OF data1",
                        collapse = NULL, 
                        recycle0 = FALSE
                    )
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
                }else{
                    data1[, dot_categ] <- base::factor(x = data1[, dot_categ], levels = dot_categ_class_order, exclude = NA, ordered = base::is.ordered(x = data1[, dot_categ]), nmax = NA) # reorder the factor
                }
            }else{
                if(base::all(dot_color == "same", na.rm = FALSE) & base::length(x = dot_color)== 1L){ # all() without na.rm -> ok because dot_color cannot be NA (tested above)
                    dot_categ_class_order <-base::unlist(x = categ_class_order[base::length(x = categ)], recursive = TRUE, use.names = TRUE)
                    data1[, dot_categ] <- base::factor(x = data1[, dot_categ], levels = dot_categ_class_order, exclude = NA, ordered = base::is.ordered(x = data1[, dot_categ]), nmax = NA) # reorder the factor
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") THE dot_categ_class_order SETTING IS NULL AND dot_color IS \"same\". ORDER OF categ_class_order WILL BE APPLIED FOR LEGEND DISPLAY: ", base::paste(dot_categ_class_order, collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
                    warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                }else{
                    dot_categ_class_order <- base::sort(x = base::levels(x = data1[, dot_categ]), decreasing = FALSE)
                    data1[, dot_categ] <- base::factor(x = data1[, dot_categ], levels = dot_categ_class_order, exclude = NA, ordered = base::is.ordered(x = data1[, dot_categ]), nmax = NA) # reorder the factor
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") THE dot_categ_class_order SETTING IS NULL. ALPHABETICAL ORDER WILL BE APPLIED FOR LEGEND DISPLAY: ", base::paste(dot_categ_class_order, collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
                    warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                }
            }
            # dot_categ_class_order not NULL anymore (character string) if dot_categ is not NULL
            if(base::all(dot_color == "same", na.rm = FALSE) & base::length(x = dot_color)== 1L){ # all() without na.rm -> ok because dot_color cannot be NA (tested above)
                if( ! base::identical(x = ini_dot_categ, y = categ[base::length(x = categ)], num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)){
                    tempo_cat <- base::paste0("ERROR IN ", function_name, "\nWHEN dot_color ARGUMENT IS \"same\", THE COLUMN NAME IN dot_categ ARGUMENT MUST BE IDENTICAL TO THE LAST COLUMN NAME IN categ ARGUMENT. HERE IT IS:\ndot_categ: ", base::paste(ini_dot_categ, collapse = " ", sep = " ", recycle0 = FALSE), "\ncateg: ", base::paste(categ, collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
                }else if( ! saferTool::comp_1d(data1 = base::unlist(x = categ_class_order[base::length(x = categ)], recursive = TRUE, use.names = TRUE), data2 = dot_categ_class_order, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)$identical_content){
                    tempo_cat <- base::paste0("ERROR IN ", function_name, "\nWHEN dot_color ARGUMENT IS \"same\",\nLAST COMPARTMENT OF categ_class_order ARGUMENT AND dot_categ_class_order ARGUMENT CANNOT BE DIFFERENT:\nLAST COMPARTMENT OF categ_class_order: ", base::paste(base::unlist(x = categ_class_order[base::length(x = categ)], recursive = TRUE, use.names = TRUE), collapse = " ", sep = " ", recycle0 = FALSE), "\ndot_categ_class_order: ", base::paste(dot_categ_class_order, collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
                }
            }
            for(i3 in base::seq_along(along.with = categ)){
                if(base::identical(x = categ[i3], y = ini_dot_categ, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE) & ! base::identical(x = base::as.character(x = base::unlist(x = categ_class_order[i3], recursive = TRUE, use.names = TRUE)), y = dot_categ_class_order, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE) & base::identical(x = base::sort(x = base::as.character(x = base::unlist(x = categ_class_order[i3], recursive = TRUE, use.names = TRUE)), decreasing = FALSE), y = base::sort(x = dot_categ_class_order, decreasing = FALSE), num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)){
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") THE dot_categ ARGUMENT SETTING IS PRESENT IN THE categ ARGUMENT SETTING, BUT ORDER OF THE CLASSES IS NOT THE SAME:\ncateg_class_order: ", base::paste(base::unlist(x = categ_class_order[i3], recursive = TRUE, use.names = TRUE), collapse = " ", sep = " ", recycle0 = FALSE), "\ndot_categ_class_order: ", base::paste(dot_categ_class_order, collapse = " ", sep = " ", recycle0 = FALSE), "\nNOTE THAT ORDER OF categ_class_order IS THE ONE USED FOR THE AXIS REPRESENTATION", collapse = NULL, recycle0 = FALSE)
                    warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                }
            }
            if(base::is.null(x = dot_legend_name)){
                dot_legend_name <- if(ini_dot_categ %in% categ[base::length(x = categ)]){dot_categ}else{ini_dot_categ} #
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") THE dot_legend_name SETTING IS NULL -> ", dot_legend_name, " WILL BE USED AS LEGEND TITLE OF DOTS", collapse = NULL, recycle0 = FALSE)
                warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            }
            # dot_legend_name not NULL anymore (character string)
        }else{
            if( ! base::is.null(x = dot_categ_class_order)){
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") THE dot_categ_class_order ARGUMENT IS NOT NULL, BUT IS THE dot_categ ARGUMENT\n-> dot_categ_class_order NOT CONSIDERED AS NO LEGEND WILL BE DRAWN", collapse = NULL, recycle0 = FALSE)
                warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            }
            # But dot_categ_class_order will be converted to NULL below (not now)
        }
        # end optional legend of dot colors
        # check the nature of color
        # integer colors into gg_palette
        tempo_check_color <- saferDev::arg_check(data = dot_color, class = "integer", typeof = "integer", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = base::paste0(dot_categ, " COLUMN OF data1", collapse = NULL, recycle0 = FALSE), data_arg = FALSE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)$problem
        if(tempo_check_color == FALSE){
            # convert integers into colors
            dot_color <- saferGG::gg_palette(n = base::max(dot_color, na.rm = TRUE), kind = "std", safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)[dot_color]
        }
        # end integer colors into gg_palette
        if(base::all(dot_color == "same", na.rm = FALSE) & base::length(x = dot_color)== 1L){# all() without na.rm -> ok because dot_color cannot be NA (tested above)
            dot_color <- categ_color # same color of the dots as the corresponding box color
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") dot_color ARGUMENT HAS BEEN SET TO \"same\"\nTHUS, DOTS WILL HAVE THE SAME COLORS AS THE CORRESPONDING BOXPLOT", collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
        }else if( ! (base::all(dot_color %in% grDevices::colors(distinct = FALSE) | base::grepl(pattern = "^#", x = dot_color, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) | base::is.na(x = dot_color), na.rm = FALSE))){ # check that all strings of low.color start by #. NA allowed (grepl() returns FALSE for NA, so na.rm would not help): NA warned below
            tempo_cat <- base::paste0(
                error_text_start, 
                "dot_color ARGUMENT MUST BE (1) A HEXADECIMAL COLOR VECTOR STARTING BY #, OR (2) COLOR NAMES GIVEN BY grDevices::colors(), OR (3) INTEGERS, OR THE STRING \"same\"\nHERE IT IS: ", base::paste(base::unique(x = dot_color, incomparables = FALSE), collapse = " ", recycle0 = FALSE, sep = " "),
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
        }
        if(base::any(base::is.na(x = dot_color), na.rm = FALSE)){ # normally no NA with is.finite
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") dot_color ARGUMENT CONTAINS NA", collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
        }
        # end check the nature of color
        # check the length of color
        if( ! base::is.null(x = dot_categ)){
            # optional legend of dot colors
            if(base::length(x = data1[, dot_categ]) == base::length(x = base::levels(x = data1[, dot_categ])) & base::length(x = dot_color) == base::length(x = data1[, dot_categ])){
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") THE NUMBER OF CLASSES OF THE COLUMN ", dot_categ, " THE NUMBER OF ROWS OF THIS COLUMN AND THE NUMBER OF COLORS OF THE dot_color ARGUMENT ARE ALL EQUAL. DOT COLORS WILL BE ATTRIBUTED ACCORDING THE LEVELS OF ", dot_categ, ", NOT ACCORDING TO THE ROWS OF ", dot_categ, collapse = NULL, recycle0 = FALSE)
                warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            }
            if(base::length(x = dot_color) > 1 & ! (base::length(x = dot_color) == base::length(x = base::unique(x = data1[, dot_categ], incomparables = FALSE)) | base::length(x = dot_color) == base::length(x = data1[, dot_categ]))){
                tempo_cat <- base::paste0(
                    error_text_start, 
                    "WHEN LENGTH OF THE dot_color ARGUMENT IS MORE THAN 1, IT MUST BE EQUAL TO THE NUMBER OF 1) ROWS OR 2) LEVELS OF dot_categ COLUMN (", dot_categ, "):\ndot_color: ", base::paste(dot_color, collapse = " ", sep = " ", recycle0 = FALSE), "\ndot_categ LEVELS: ", base::paste(base::levels(x = data1[, dot_categ]), collapse = " ", recycle0 = FALSE, sep = " "),
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
            }else if(base::length(x = dot_color) > 1 & base::length(x = dot_color) == base::length(x = base::unique(x = data1[, dot_categ], incomparables = FALSE))){
                data1 <-base::data.frame(data1, dot_color = data1[, dot_categ], stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE)
                data1$dot_color <- base::factor(x = data1$dot_color, labels = dot_color, exclude = NA, ordered = base::is.ordered(x = data1$dot_color), nmax = NA) # do not use labels = unique(dot_color). Otherwise, we can have green1 green2 when dot_color is c("green", "green")
            }else if(base::length(x = dot_color) > 1 & base::length(x = dot_color) == base::length(x = data1[, dot_categ])){
                data1 <- base::data.frame(data1, dot_color = dot_color, stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE)
            }else if(base::length(x = dot_color)== 1L){ # to deal with single color. Warning: & length(dot_categ_class_order) > 1 removed because otherwise, the data1 is not with dot_color column when length(dot_categ_class_order) == 1
                data1 <- base::data.frame(data1, dot_color = dot_color, stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE)
            }
            dot_color <- base::as.character(x = base::unique(x = data1$dot_color[base::order(data1[, dot_categ], na.last = TRUE, decreasing = FALSE, method = "auto")], incomparables = FALSE)) # reorder the dot_color character vector
            if(base::length(x = dot_color)== 1L & base::length(x = dot_categ_class_order) > 1){ # to deal with single color
                dot_color <- base::rep(x = dot_color, base::length(x = dot_categ_class_order))
            }
            tempo_check <- base::unique(x = data1[ , base::c(dot_categ, "dot_color")], incomparables = FALSE)
            if(base::length(x = base::unique(x = data1[ , "dot_color"], incomparables = FALSE)) > 1 & ( ! (base::nrow(x = tempo_check) == base::length(x = base::unique(x = data1[ , "dot_color"], incomparables = FALSE)) & base::nrow(x = tempo_check) == base::length(x = base::unique(x = data1[ , dot_categ], incomparables = FALSE))))){ # length(unique(data1[ , "dot_color"])) > 1 because if only one color, can be attributed to each class of dot_categ
                tempo_cat <- base::paste0("ERROR IN ", function_name, "\ndot_color ARGUMENT IS INCORRECTLY ASSOCIATED TO EACH CLASS OF dot_categ (", dot_categ, ") COLUMN:\n", base::paste(base::unique(x = base::mapply(FUN = base::paste,data1[ , dot_categ], data1[ ,"dot_color"], MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE), incomparables = FALSE), collapse = "\n", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
            }else{
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") IN dot_categ ARGUMENT (", ini_dot_categ, "), THE FOLLOWING COLORS OF DOTS:\n", base::paste(dot_color, collapse = " ", sep = " ", recycle0 = FALSE), "\nHAVE BEEN ATTRIBUTED TO THESE CLASSES:\n", base::paste(base::levels(x = data1[, dot_categ]), collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
                warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            }
            # dot_color is a character string representing the diff classes of dot_categ
            # data1$dot_color is a factor with order of levels -> dot_categ
            # end optional legend of dot colors
        }else{
            categ_len <- base::length(x = categ) # if only categ1, then colors for classes of categ1, if length(categ) == 2L, then colors for classes of categ2
            if(base::length(x = dot_color) == base::length(x = base::levels(x = data1[, categ[categ_len]]))){ # here length(dot_color) is equal to the different number of categ
                # data1[, categ[categ_len]] <- factor(data1[, categ[categ_len]]) # not required because sure that is is a factor
                data1 <- base::data.frame(data1, dot_color = data1[, categ[categ_len]], stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE)
                data1$dot_color <- base::factor(x = base::as.character(x = data1$dot_color), levels = base::levels(x = data1[, categ[categ_len]]), labels = dot_color, exclude = NA, ordered = FALSE, nmax = NA)
                if(box_alpha != 0){
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") IN ", categ[categ_len], " OF categ ARGUMENT, THE FOLLOWING COLORS:\n", base::paste(dot_color, collapse = " ", sep = " ", recycle0 = FALSE), "\nHAVE BEEN ATTRIBUTED TO THESE CLASSES:\n", base::paste(base::levels(x = base::factor(x = data1[, categ[categ_len]], exclude = NA, ordered = base::is.ordered(x = data1[, categ[categ_len]]), nmax = NA)), collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
                    warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                }
            }else if(base::length(x = dot_color) == base::length(x = data1[, categ[categ_len]])){# here length(dot_color) is equal to nrow(data1) -> Modif to have length(dot_color) equal to the different number of categ (length(dot_color) == length(levels(data1[, categ[categ_len]])))
                data1 <- base::data.frame(data1, dot_color = dot_color, stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE)
            }else if(base::length(x = dot_color)== 1L & ! base::all(dot_color == "same", na.rm = FALSE)){ # all() without na.rm -> ok because dot_color cannot be NA (tested above)
                # data1[, categ[categ_len]] <- factor(data1[, categ[categ_len]]) # not required because sure that is is a factor
                data1 <- base::data.frame(data1, dot_color = dot_color, stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE)
                dot_color <- base::rep(x = dot_color, base::length(x = base::levels(x = data1[, categ[categ_len]])))
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") dot_color ARGUMENT HAS LENGTH 1, MEANING THAT ALL THE DIFFERENT CLASSES OF ", categ[categ_len], "\n", base::paste(base::levels(x = base::factor(x = data1[, categ[categ_len]], exclude = NA, ordered = base::is.ordered(x = data1[, categ[categ_len]]), nmax = NA)), collapse = " ", sep = " ", recycle0 = FALSE), "\nWILL HAVE THE SAME COLOR\n", base::paste(dot_color, collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
                warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            }else{
                tempo_cat <- base::paste0("ERROR IN ", function_name, "\ndot_color ARGUMENT MUST BE (1) LENGTH 1, OR (2) THE LENGTH OF data1 NROWS AFTER NA/Inf REMOVAL, OR (3) THE LENGTH OF THE CLASSES IN THE categ ", categ[categ_len], " COLUMN. HERE IT IS COLOR LENGTH ", base::length(x = dot_color), " VERSUS CATEG LENGTH ", base::length(x = data1[, categ[categ_len]]), " AND CATEG CLASS LENGTH ", base::length(x = base::unique(x = data1[, categ[categ_len]], incomparables = FALSE)), "\nPRESENCE OF NA/Inf COULD BE THE PROBLEM", collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
            }
            # end check the length of color
            dot_color <- base::as.character(x = dot_color)
            # dot_color is a character string representing the diff classes
            data1$dot_color <- base::factor(x = data1$dot_color, levels = base::unique(x = dot_color, incomparables = FALSE), exclude = NA, ordered = base::is.ordered(x = data1$dot_color), nmax = NA) # ok because if dot_color is a character string, the order make class 1, class 2, etc. If dot_color is a column of data1, then levels will be created, without incidence, except if dot_categ specified (see below). unique() because no duplicates allowed
            # data1$dot_color is a factor with order of levels -> dot_color
        }
        # end optional legend of dot colors
    }else if(base::is.null(x = dot_color) & ! (base::is.null(x = dot_categ) & base::is.null(x = dot_categ_class_order) & base::is.null(x = dot_legend_name))){
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count,") dot_categ OR dot_categ_class_order OR dot_legend_name ARGUMENT HAS BEEN SPECIFIED BUT dot_color ARGUMENT IS NULL (NO DOT PLOTTED)", collapse = NULL, recycle0 = FALSE)
        warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
    }
    # dot_color either NULL (no dot plotted) or character string (potentially representing the diff classes of dot_categ)
    # data1$dot_color is either NA or a factor (with order of levels -> depending on dot_categ or categ[length(categ)], or other
    if(base::is.null(x = dot_categ)){
        dot_categ_class_order <- NULL # because not used anyway
    }
    # dot_categ_class_order either NULL if dot_categ is NULL (no legend displayed) or character string (potentially representing the diff classes of dot_categ)
    # end management of dot_color
    if(base::is.null(x = dot_color) & box_fill == FALSE & dot_alpha <= 0.025){
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count,") THE FOLLOWING ARGUMENTS WERE SET AS:\ndot_color = NULL (NOT ALL DOTS BUT ONLY POTENTIAL OUTLIER DOTS DISPLAYED)\nbox_fill = FALSE (NO FILLING COLOR FOR BOTH BOXES AND POTENTIAL OUTLIER DOTS)\nbox_alpha = ", saferTool::round2(data = box_alpha, dec_nb = 4, after_lead_zero = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text), "\ndot_alpha = ", saferTool::round2(data = dot_alpha, dec_nb = 4, after_lead_zero = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text), "\n-> POTENTIAL OUTLIER DOTS MIGHT NOT BE VISIBLE BECAUSE ALMOST TRANSPARENT", collapse = NULL, recycle0 = FALSE)
        warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
    }
    if(base::is.null(x = dot_color) & box_fill == FALSE & dot_border_size == 0){
        tempo_cat <- base::paste0("ERROR IN ", function_name, "\nTHE FOLLOWING ARGUMENTS WERE SET AS:\ndot_color = NULL (NOT ALL DOTS BUT ONLY POTENTIAL OUTLIER DOTS DISPLAYED)\nbox_fill = FALSE (NO FILLING COLOR FOR BOTH BOXES AND POTENTIAL OUTLIER DOTS)\ndot_border_size = 0 (NO BORDER FOR POTENTIAL OUTLIER DOTS)\n-> THESE SETTINGS ARE NOT ALLOWED BECAUSE THE POTENTIAL OUTLIER DOTS WILL NOT BE VISIBLE", collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
    }
    # integer dot_border_color into gg_palette
    if( ! base::is.null(x = dot_border_color)){
        tempo <- saferDev::arg_check(data = dot_border_color, class = "integer", typeof = "integer", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = FALSE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        if(tempo$problem == FALSE){ # convert integers into colors
            dot_border_color <- saferGG::gg_palette(n = base::max(dot_border_color, na.rm = TRUE), kind = "std", safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)[dot_border_color]
        }
    }
    # end integer dot_border_color into gg_palette
    # na detection and removal (done now to be sure of the correct length of categ)
    column_check <- base::c("categ_color", if( ! base::is.null(x = dot_color)){"dot_color"}) # 
    if(base::any(base::is.na(x = data1[, column_check]), na.rm = FALSE)){ # data1 used here instead of data1_ini in case of new NaN created by log conversion (neg values) # normally no NA with is.na
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count,") NA DETECTED IN COLUMNS ", base::paste(column_check, collapse = " ", sep = " ", recycle0 = FALSE), " OF data1 AND CORRESPONDING ROWS REMOVED (SEE $removed_row_nb AND $removed_rows)", collapse = NULL, recycle0 = FALSE)
        warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
        for(i2 in base::seq_along(along.with = column_check)){
            if(base::any(base::is.na(x = data1[, column_check[i2]]), na.rm = FALSE)){ # normally no NA with is.na
                tempo_warn <- base::paste0("NA REMOVAL DUE TO COLUMN ", column_check[i2], " OF data1", collapse = NULL, recycle0 = FALSE)
                warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            }
        }
        tempo <- base::unique(x = base::unlist(x = base::lapply(X = base::lapply(X = base::c(data1[column_check]), FUN = is.na), FUN = which), recursive = TRUE, use.names = TRUE), incomparables = FALSE)
        removed_row_nb <- base::c(removed_row_nb, tempo)
        tempo_removed <- data1[tempo, ] # data1 used here (and not data1_ini) because the categ_color/dot_color columns do not exist in data1_ini and are needed for the class-disappearance check below. CAUTION: done before the data1 subsetting at line below, because the row indexes of tempo correspond to the non subsetted data1
        removed_rows <- base::rbind(removed_rows, data1_ini[tempo, ], stringsAsFactors = FALSE, deparse.level = 1) # data1_ini used (and not data1, that has the extra categ_color/dot_color columns) to keep the column set of $removed_rows constant whatever the removal pass, and to avoid a rbind error when this pass and the previous NA-removal pass (line above) both remove rows
        if(base::length(x = tempo) != 0){
            data1 <- data1[-tempo, ] # WARNING tempo here and not removed_row_nb because the latter contain more numbers thant the former
            data1_ini <- data1_ini[-tempo, ] # WARNING tempo here and not removed_row_nb because the latter contain more numbers thant the former
            for(i3 in base::seq_along(along.with = column_check)){
                if(base::any( ! base::unique(x = tempo_removed[, column_check[i3]], incomparables = FALSE) %in% base::unique(x = data1[, column_check[i3]], incomparables = FALSE), na.rm = TRUE)){
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") IN COLUMN ", column_check[i3], " OF data1, THE FOLLOWING CLASSES HAVE DISAPPEARED AFTER NA/Inf REMOVAL (IF COLUMN USED IN THE PLOT, THIS CLASS WILL NOT BE DISPLAYED):\n", base::paste(base::unique(x = tempo_removed[, column_check[i3]], incomparables = FALSE)[ ! base::unique(x = tempo_removed[, column_check[i3]], incomparables = FALSE) %in% base::unique(x = data1[, column_check[i3]], incomparables = FALSE)], collapse = " ", sep = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
                    warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                }
            }
        }
        if( ! base::is.null(x = dot_categ) & ! base::is.null(x = dot_categ_class_order)){
            # pass-2 can remove all rows of a dot_categ class (their dot_color was NA):
            # rebuild dot_categ_class_order and the dot_categ factor from the remaining classes,
            # otherwise the alpha scale below gets more labels than remaining data levels
            dot_categ_class_order <- dot_categ_class_order[dot_categ_class_order %in% base::unique(x = base::as.character(x = data1[, dot_categ]), incomparables = FALSE)]
            data1[, dot_categ] <- base::factor(x = base::as.character(x = data1[, dot_categ]), levels = dot_categ_class_order, exclude = NA, ordered = FALSE, nmax = NA)
        }
        for(i2 in base::seq_along(along.with = column_check)){
            if(column_check[i2] == "categ_color"){
                categ_color <- base::levels(x = data1[, column_check[i2]])[base::levels(x = data1[, column_check[i2]]) %in% base::unique(x = data1[, column_check[i2]], incomparables = FALSE)] # remove the absent color in the character vector
                if(base::length(x = categ_color)== 1L & base::length(x = base::unlist(x = categ_class_order[base::length(x = categ)], recursive = TRUE, use.names = TRUE)) > 1){ # to deal with single color
                    categ_color <- base::rep(x = categ_color, base::length(x = base::unlist(x = categ_class_order[base::length(x = categ)], recursive = TRUE, use.names = TRUE)))
                }
                data1[, column_check[i2]] <- base::factor(x = base::as.character(x = data1[, column_check[i2]]), levels = base::unique(x = categ_color, incomparables = FALSE), exclude = NA, ordered = FALSE, nmax = NA)
            }
            if(column_check[i2] == "dot_color"){
                dot_color <- base::levels(x = data1[, column_check[i2]])[base::levels(x = data1[, column_check[i2]]) %in% base::unique(x = data1[, column_check[i2]], incomparables = FALSE)] # remove the absent color in the character vector
                if(base::length(x = dot_color)== 1L & base::length(x = dot_categ_class_order) > 1){ # to deal with single color. If dot_categ_class_order == NULL (which is systematically the case if dot_categ == NULL), no rep(dot_color, length(dot_categ_class_order)
                    dot_color <- base::rep(x = dot_color, base::length(x = dot_categ_class_order))
                }
                data1[, column_check[i2]] <- base::factor(x = base::as.character(x = data1[, column_check[i2]]), levels = base::unique(x = dot_color, incomparables = FALSE), exclude = NA, ordered = FALSE, nmax = NA)
            }
        }
    }
    # end na detection and removal (done now to be sure of the correct length of categ)
    # From here, data1 and data_ini have no more NA or NaN

    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code

    # add management
    # the add string is evaluated once here. Its non layer components (facets, scales, coords, labs, theme) are used as the first element of every plot below (add_gg), while its layers (geom_hline(), annotate(), etc.) are removed from add_gg and added at the end of the final plot only (add_layers), because the code below relies on the layer order of the built plots (e.g., ggplot_build()$data[[1]] must be the dots) and on a discrete x-axis scale, set by the first layer
    add_gg <- base::eval(expr = base::parse(text = base::paste0("ggplot2::ggplot()", if(base::is.null(x = add)){""}else{add}, collapse = NULL, recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    add_layers <- add_gg$layers
    add_gg$layers <- base::list()
    # end add management

    # y coordinates recovery (create ini_box_coord, dot_coord and modify data1)
    if(base::length(x = categ)== 1L){
        # width commputations
        box_width2 <- box_width
        box_space <- 0 # to inactivate the shrink that add space between grouped boxes, because no grouped boxes here
        # end width commputations
        # data1 check categ order for dots coordinates recovery
        data1 <- base::data.frame(data1, categ_check = data1[, categ[1]], stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE)
        data1$categ_check <- base::as.integer(x = data1$categ_check) # to check that data1[, categ[1]] and dot_coord$group are similar, during merging
        # CAUTION: if classes disappeared during the NA/Inf removal of the color columns above (e.g., categ_color with NA), ggplot2 builds groups only for the remaining classes, with renumbered ids (1, 2, ...), whereas categ_check keeps the ids of the initial factor levels (e.g., 1, 3). Remap categ_check to the renumbered ids, keeping the initial order
        data1$categ_check <- base::as.integer(x = base::factor(x = data1$categ_check, levels = base::sort(x = base::unique(x = data1$categ_check, incomparables = FALSE), decreasing = FALSE), exclude = NA, ordered = base::is.ordered(x = data1$categ_check), nmax = NA))
        # end data1 check categ order for dots coordinates recovery
        # per box dots coordinates recovery
        tempo_gg_name <- "gg.indiv.plot."
        tempo_gg_count <- 0
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = add_gg, pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # add added here to have the facets
        # in this function, ggplot2::aes(...)[base::c(<other aesthetics>, "x", "y")] reproduces the aesthetic order of the former ggplot2::aes_string() (x and y last): ggplot2 numbers the default groups according to the order of the discrete aesthetics, and the internal checks of the dot and box coordinates rely on this numbering
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_point(data = data1, mapping = ggplot2::aes(x = .data[[categ[1]]], y = .data[[y]], color = .data[[categ[1]]])[base::c("colour", "x", "y")], stroke = dot_border_size, size = dot_size, alpha = dot_alpha, shape = 21, stat = "identity", position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_discrete_manual(aesthetics = "color", name = box_legend_name, values = if(base::is.null(x = categ_color)){ # categ_color used for dot colors because at that stage, we do not care about colors
            # nocov start (unreachable: categ_color is converted to a non-NULL character vector by the categ_color management block above (comment "categ_color not NULL anymore"))
            base::rep(x = NA, base::length(x = base::unique(x = data1[, categ[1]], incomparables = FALSE)))
            # nocov end
        }else if(base::length(x = categ_color)== 1L){base::rep(x = categ_color, base::length(x = base::unique(x = data1[, categ[1]], incomparables = FALSE)))}else{categ_color}, breaks = ggplot2::waiver()), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_boxplot(data = data1, mapping = ggplot2::aes(x = .data[[categ[1]]], y = .data[[y]], fill = .data[[categ[1]]])[base::c("fill", "x", "y")], coef = if(box_whisker_kind == "no"){0}else if(box_whisker_kind == "std"){1.5}else if(box_whisker_kind == "max"){Inf}, stat = "boxplot", position = "dodge2", outliers = TRUE, outlier.colour = NULL, outlier.color = NULL, outlier.fill = NULL, outlier.shape = NULL, outlier.size = NULL, outlier.stroke = 0.5, outlier.alpha = NULL, whisker.colour = NULL, whisker.color = NULL, whisker.linetype = NULL, whisker.linewidth = NULL, staple.colour = NULL, staple.color = NULL, staple.linetype = NULL, staple.linewidth = NULL, median.colour = NULL, median.color = NULL, median.linetype = NULL, median.linewidth = NULL, box.colour = NULL, box.color = NULL, box.linetype = NULL, box.linewidth = NULL, notch = FALSE, notchwidth = 0.5, staplewidth = 0, varwidth = FALSE, na.rm = FALSE, orientation = NA, show.legend = NA, inherit.aes = TRUE), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # fill because this is what is used with geom_box # to easily have the equivalent of the grouped boxes
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_discrete_manual(aesthetics = "fill", name = box_legend_name, values = if(base::length(x = categ_color)== 1L){base::rep(x = categ_color, base::length(x = base::unique(x = data1[, categ[1]], incomparables = FALSE)))}else{categ_color}, breaks = ggplot2::waiver()), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
        # end per box dots coordinates recovery
    }else if(base::length(x = categ) == 2L){
        # width commputations
        box_width2 <- box_width / base::length(x = base::unique(x = data1[, categ[base::length(x = categ)]], incomparables = FALSE)) # real width of each box in x-axis unit, among the set of grouped box. Not relevant if no grouped boxes length(categ)== 1L
        # end width commputations
        # data1 check categ order for dots coordinates recovery
        tempo_factor <- base::paste0(data1[base::order(data1[, categ[2]], data1[, categ[1]], na.last = TRUE, decreasing = FALSE, method = "auto"), categ[2]], "_", data1[base::order(data1[, categ[2]], data1[, categ[1]], na.last = TRUE, decreasing = FALSE, method = "auto"), categ[1]], collapse = NULL, recycle0 = FALSE)
        data1 <- base::data.frame(data1[base::order(data1[, categ[2]], data1[, categ[1]], na.last = TRUE, decreasing = FALSE, method = "auto"), ], categ_check = base::factor(x = tempo_factor, levels = base::unique(x = tempo_factor, incomparables = FALSE), exclude = NA, ordered = base::is.ordered(x = tempo_factor), nmax = NA), stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE)
        data1$categ_check <- base::as.integer(x = data1$categ_check) # to check that data1[, categ[1]] and dot_coord$group are similar, during merging
        # CAUTION: if classes disappeared during the NA/Inf removal of the color columns above (e.g., categ_color with NA), ggplot2 builds groups only for the remaining classes, with renumbered ids (1, 2, ...), whereas categ_check keeps the ids of the initial factor levels (e.g., 1, 3). Remap categ_check to the renumbered ids, keeping the initial order
        data1$categ_check <- base::as.integer(x = base::factor(x = data1$categ_check, levels = base::sort(x = base::unique(x = data1$categ_check, incomparables = FALSE), decreasing = FALSE), exclude = NA, ordered = base::is.ordered(x = data1$categ_check), nmax = NA))
        # per box dots coordinates recovery
        tempo_gg_name <- "gg.indiv.plot."
        tempo_gg_count <- 0
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = add_gg, pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # add added here to have the facets
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_point(data = data1, mapping = ggplot2::aes(x = .data[[categ[1]]], y = .data[[y]], color = .data[[categ[2]]])[base::c("colour", "x", "y")], stroke = dot_border_size, size = dot_size, alpha = dot_alpha, shape = 21, stat = "identity", position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_discrete_manual(aesthetics = "color", name = box_legend_name, values = if(base::is.null(x = categ_color)){ # categ_color used for dot colors because at that stage, we do not care about colors
            # nocov start (unreachable: categ_color is converted to a non-NULL character vector by the categ_color management block above (comment "categ_color not NULL anymore"))
            base::rep(x = NA, base::length(x = base::unique(x = data1[, categ[2]], incomparables = FALSE)))
            # nocov end
        }else if(base::length(x = categ_color)== 1L){base::rep(x = categ_color, base::length(x = base::unique(x = data1[, categ[2]], incomparables = FALSE)))}else{categ_color}, breaks = ggplot2::waiver()), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_boxplot(data = data1, mapping = ggplot2::aes(x = .data[[categ[1]]], y = .data[[y]], fill = .data[[categ[2]]])[base::c("fill", "x", "y")], coef = if(box_whisker_kind == "no"){0}else if(box_whisker_kind == "std"){1.5}else if(box_whisker_kind == "max"){Inf}, stat = "boxplot", position = "dodge2", outliers = TRUE, outlier.colour = NULL, outlier.color = NULL, outlier.fill = NULL, outlier.shape = NULL, outlier.size = NULL, outlier.stroke = 0.5, outlier.alpha = NULL, whisker.colour = NULL, whisker.color = NULL, whisker.linetype = NULL, whisker.linewidth = NULL, staple.colour = NULL, staple.color = NULL, staple.linetype = NULL, staple.linewidth = NULL, median.colour = NULL, median.color = NULL, median.linetype = NULL, median.linewidth = NULL, box.colour = NULL, box.color = NULL, box.linetype = NULL, box.linewidth = NULL, notch = FALSE, notchwidth = 0.5, staplewidth = 0, varwidth = FALSE, na.rm = FALSE, orientation = NA, show.legend = NA, inherit.aes = TRUE), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # fill because this is what is used with geom_box # to easily have the equivalent of the grouped boxes
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_discrete_manual(aesthetics = "fill", name = box_legend_name, values = if(base::length(x = categ_color)== 1L){base::rep(x = categ_color, base::length(x = base::unique(x = data1[, categ[2]], incomparables = FALSE)))}else{categ_color}, breaks = ggplot2::waiver()), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
        # end per box dots coordinates recovery
    }else{
            # nocov start (unreachable: internal consistency guard - both branches above produce identical dot/box data structures by construction)
        tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nCODE INCONSISTENCY 1", collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
            # nocov end
    }
    if( ! base::is.null(x = stat_pos)){
        stat_just <- saferGG::gg_just(
            angle = stat_angle, 
            pos = base::ifelse(
                test = vertical == TRUE, 
                yes = base::ifelse(test = stat_pos == "top", yes = "bottom", no = "top"), # "bottom" because we want justification for text that are below the ref point which is the top of the graph. The opposite for "above"
                no = base::ifelse(test = stat_pos == "top", yes = "left", no = "right") # "left" because we want justification for text that are on the left of the ref point which is the right border of the graph. The opposite for "above"
            ), 
            kind = "text", safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text
        )
    }
    # has in fact no interest because ggplot2 does not create room for geom_text()
    tempo_data_max <- data1[base::which.max(x = data1[, y]), ]
    tempo_data_max <- base::data.frame(tempo_data_max, label = base::formatC(x = tempo_data_max[, y], digits = 2, drop0trailing = TRUE, format = "f", width = NULL, flag = "", mode = NULL, big.mark = "", big.interval = 3L, small.mark = "", small.interval = 5L, decimal.mark = base::getOption(x = "OutDec", default = NULL), preserve.width = "individual", zero.print = NULL, replace.zero = TRUE), stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE)
    # end has in fact no interest because ggplot2 does not create room for geom_text()
    tempo_graph_info_ini <- ggplot2::ggplot_build(plot = base::eval(expr = base::parse(text = base::paste(base::paste(base::paste0(tempo_gg_name, 1:tempo_gg_count, collapse = NULL, recycle0 = FALSE), collapse = " + ", sep = " ", recycle0 = FALSE), if( ! base::is.null(x = stat_pos)){' + ggplot2::geom_text(data = tempo_data_max, mapping = ggplot2::aes(x = 1, y = .data[[y]], label = .data[["label"]])[base::c("label", "x", "y")], size = stat_size, color = "black", angle = stat_angle, hjust = stat_just$hjust, vjust = stat_just$vjust)'}, sep = " ", collapse = NULL, recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))) # added here to have room for annotation
    dot_coord <- tempo_graph_info_ini$data[[1]]
    dot_coord$x <- base::as.numeric(x = dot_coord$x) # because weird class
    dot_coord$PANEL <- base::as.numeric(x = dot_coord$PANEL) # because numbers as levels. But may be a problem is facet are reordered ?
    tempo_mean <- stats::aggregate(x = dot_coord$y, by = base::list(dot_coord$group, dot_coord$PANEL), FUN = mean, na.rm = TRUE)
    base::names(x = tempo_mean)[base::names(x = tempo_mean) == "x"] <- "MEAN"
    base::names(x = tempo_mean)[base::names(x = tempo_mean) == "Group.1"] <- "BOX"
    base::names(x = tempo_mean)[base::names(x = tempo_mean) == "Group.2"] <- "PANEL"
    dot_coord <- base::data.frame(
        dot_coord[base::order(dot_coord$group, dot_coord$y, na.last = TRUE, decreasing = FALSE, method = "auto"), ], # dot_coord$PANEL deals below
        y_check = base::as.double(x = data1[base::order(data1$categ_check, data1[, y], na.last = TRUE, decreasing = FALSE, method = "auto"), y]), 
        categ_check = data1[base::order(data1$categ_check, data1[, y], na.last = TRUE, decreasing = FALSE, method = "auto"), "categ_check"], 
        dot_color = if(base::is.null(x = dot_color)){NA}else{data1[base::order(data1$categ_check, data1[, y], na.last = TRUE, decreasing = FALSE, method = "auto"), "dot_color"]}, 
        data1[base::order(data1$categ_check, data1[, y], na.last = TRUE, decreasing = FALSE, method = "auto"), ][categ], # avoid the renaming below
        stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE
    ) # y_check to be sure that the order is the same between the y of data1 and the y of dot_coord
    # names(dot_coord)[names(dot_coord) == "tempo_categ1"] <- categ[1]
    if( ! base::is.null(x = dot_categ)){
        dot_coord <- base::data.frame(dot_coord, data1[base::order(data1$categ_check, data1[, y], na.last = TRUE, decreasing = FALSE, method = "auto"), ][dot_categ], stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE) # avoid the renaming
    }
    if( ! base::is.null(x = facet_categ)){
        dot_coord <- base::data.frame(dot_coord, data1[base::order(data1$categ_check, data1[, y], na.last = TRUE, decreasing = FALSE, method = "auto"), ][facet_categ], stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE) # for facet panels
        tempo_test <- NULL
        for(i2 in base::seq_along(along.with = facet_categ)){
            if(base::is.factor(x = dot_coord[, facet_categ[i2]])){
                tempo_test <- base::paste0(tempo_test, ".", base::formatC(x = base::as.numeric(x = dot_coord[, facet_categ[i2]]), width = base::nchar(x = base::max(base::as.numeric(x = dot_coord[, facet_categ[i2]]), na.rm = TRUE), type = "chars", allowNA = FALSE, keepNA = NA), flag = "0", digits = NULL, format = NULL, mode = NULL, big.mark = "", big.interval = 3L, small.mark = "", small.interval = 5L, decimal.mark = base::getOption(x = "OutDec", default = NULL), preserve.width = "individual", zero.print = NULL, replace.zero = TRUE, drop0trailing = FALSE), collapse = NULL, recycle0 = FALSE) # convert factor into numeric with leading zero for proper ranking # merge the formatC() to create a new factor. The convertion to integer should recreate the correct group number. Here as.numeric is used and not as.integer in case of numeric in facet_categ (because comes from add and not checked by saferDev::arg_check, contrary to categ)
            }else{ # character facet column (facet columns of add are not checked by saferDev::arg_check and can be character): ggplot2 orders the PANEL by the alphabetical order of the values (equivalent to levels of factor(as.character())). Must match this ordering
                tempo_test <- base::paste0(tempo_test, ".", base::formatC(x = base::match(x = dot_coord[, facet_categ[i2]], table = base::sort(x = base::unique(x = base::as.character(x = dot_coord[, facet_categ[i2]]), incomparables = FALSE), decreasing = FALSE), nomatch = NA_integer_, incomparables = NULL), width = base::nchar(x = base::length(x = base::unique(x = base::as.character(x = dot_coord[, facet_categ[i2]]), incomparables = FALSE)), type = "chars", allowNA = FALSE, keepNA = NA), flag = "0", digits = NULL, format = NULL, mode = NULL, big.mark = "", big.interval = 3L, small.mark = "", small.interval = 5L, decimal.mark = base::getOption(x = "OutDec", default = NULL), preserve.width = "individual", zero.print = NULL, replace.zero = TRUE, drop0trailing = FALSE), collapse = NULL, recycle0 = FALSE)
            }
        }
        tempo_test <- base::as.integer(x = base::factor(x = tempo_test, exclude = NA, ordered = base::is.ordered(x = tempo_test), nmax = NA))
        if( ! base::identical(x = base::as.integer(x = dot_coord$PANEL), y = tempo_test, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)){
            # nocov start (unreachable: internal consistency guard - PANEL is always the box_facet factor index for correctly formed dot_coord)
            tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nas_integer(dot_coord$PANEL) AND tempo_test MUST BE IDENTICAL. CODE HAS TO BE MODIFIED", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
            # nocov end
        }
    }
    if(dot_tidy == TRUE){
        if( ! base::is.null(x = dot_categ)){
            dot_coord <- base::data.frame(dot_coord, tidy_group = data1[base::order(data1$categ_check, data1[, y], na.last = TRUE, decreasing = FALSE, method = "auto"), ][, dot_categ], stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE) # avoid the renaming
            # tidy_group_coord is to be able to fuse table when creating the table for dot coordinates
            if(dot_categ %in% categ){
                dot_coord <- base::data.frame(dot_coord, tidy_group_coord = dot_coord$group, stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE)
            }else{
                dot_coord <- base::data.frame(dot_coord, tidy_group_coord = base::as.integer(x = base::factor(x = base::paste0(
                    base::formatC(x = base::as.integer(x = dot_coord[, categ[1]]), width = base::nchar(x = base::max(base::as.integer(x = dot_coord[, categ[1]]), na.rm = TRUE), type = "chars", allowNA = FALSE, keepNA = NA), flag = "0", digits = NULL, format = NULL, mode = NULL, big.mark = "", big.interval = 3L, small.mark = "", small.interval = 5L, decimal.mark = base::getOption(x = "OutDec", default = NULL), preserve.width = "individual", zero.print = NULL, replace.zero = TRUE, drop0trailing = FALSE), # convert factor into numeric with leading zero for proper ranking
                    ".", 
                    if(base::length(x = categ) == 2L){base::formatC(x = base::as.integer(x = dot_coord[, categ[2]]), width = base::nchar(x = base::max(base::as.integer(x = dot_coord[, categ[2]]), na.rm = TRUE), type = "chars", allowNA = FALSE, keepNA = NA), flag = "0", digits = NULL, format = NULL, mode = NULL, big.mark = "", big.interval = 3L, small.mark = "", small.interval = 5L, decimal.mark = base::getOption(x = "OutDec", default = NULL), preserve.width = "individual", zero.print = NULL, replace.zero = TRUE, drop0trailing = FALSE)}, # convert factor into numeric with leading zero for proper ranking
                    if(base::length(x = categ) == 2L){"."}, 
                    base::formatC(x = base::as.integer(x = dot_coord[, dot_categ]), width = base::nchar(x = base::max(base::as.integer(x = dot_coord[, dot_categ]), na.rm = TRUE), type = "chars", allowNA = FALSE, keepNA = NA), flag = "0", digits = NULL, format = NULL, mode = NULL, big.mark = "", big.interval = 3L, small.mark = "", small.interval = 5L, decimal.mark = base::getOption(x = "OutDec", default = NULL), preserve.width = "individual", zero.print = NULL, replace.zero = TRUE, drop0trailing = FALSE), collapse = NULL, recycle0 = FALSE # convert factor into numeric with leading zero for proper ranking
                ), exclude = NA, ordered = FALSE, nmax = NA)), row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = FALSE # merge the 2 or 3 formatC() to create a new factor. The convertion to integer should recreate the correct group number
                ) # for tidy dot plots
            }
        }else{
            dot_coord <- base::data.frame(dot_coord, tidy_group = if(base::length(x = categ)== 1L){
                dot_coord[, categ]}else{base::as.integer(x = base::factor(x = base::paste0(
                    base::formatC(x = base::as.integer(x = dot_coord[, categ[1]]), width = base::nchar(x = base::max(base::as.integer(x = dot_coord[, categ[1]]), na.rm = TRUE), type = "chars", allowNA = FALSE, keepNA = NA), flag = "0", digits = NULL, format = NULL, mode = NULL, big.mark = "", big.interval = 3L, small.mark = "", small.interval = 5L, decimal.mark = base::getOption(x = "OutDec", default = NULL), preserve.width = "individual", zero.print = NULL, replace.zero = TRUE, drop0trailing = FALSE), # convert factor into numeric with leading zero for proper ranking
                    ".", 
                    base::formatC(x = base::as.integer(x = dot_coord[, categ[2]]), width = base::nchar(x = base::max(base::as.integer(x = dot_coord[, categ[2]]), na.rm = TRUE), type = "chars", allowNA = FALSE, keepNA = NA), flag = "0", digits = NULL, format = NULL, mode = NULL, big.mark = "", big.interval = 3L, small.mark = "", small.interval = 5L, decimal.mark = base::getOption(x = "OutDec", default = NULL), preserve.width = "individual", zero.print = NULL, replace.zero = TRUE, drop0trailing = FALSE), collapse = NULL, recycle0 = FALSE# convert factor into numeric with leading zero for proper ranking
                ), exclude = NA, ordered = FALSE, nmax = NA)) # merge the 2 formatC() to create a new factor. The convertion to integer should recreate the correct group number
                }, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = FALSE) # for tidy dot plots
            # tidy_group_coord is to be able to fuse table when creating the table for dot coordinates
            dot_coord <- base::data.frame(dot_coord, tidy_group_coord = dot_coord$group, stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE)
        }
    }
    if( ! (base::identical(x = dot_coord$y, y = dot_coord$y_check, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE) & base::identical(x = dot_coord$group, y = dot_coord$categ_check, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE))){
        tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\n(dot_coord$y AND dot_coord$y_check) AS WELL AS (dot_coord$group AND dot_coord$categ_check) MUST BE IDENTICAL. CODE HAS TO BE MODIFIED", collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
    }else{
        if( ! base::identical(x = tempo_mean[base::order(tempo_mean$BOX, tempo_mean$PANEL, na.last = TRUE, decreasing = FALSE, method = "auto"), ]$BOX, y = base::unique(x = dot_coord[base::order(dot_coord$group, dot_coord$PANEL, na.last = TRUE, decreasing = FALSE, method = "auto"), base::c("group", "PANEL")], incomparables = FALSE)$group, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)){
            # nocov start (unreachable: internal consistency guard - tempo_mean is derived from dot_coord)
            tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\n(tempo_mean$BOX, tempo_mean$PANEL) AND (dot_coord$group, dot_coord$PANEL) MUST BE IDENTICAL. CODE HAS TO BE MODIFIED", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
            # nocov end
        }else{
            tempo <- base::unique(x = dot_coord[base::order(dot_coord$group, dot_coord$PANEL, na.last = TRUE, decreasing = FALSE, method = "auto"), base::c(categ, if( ! base::is.null(x = dot_color) & ! base::is.null(x = dot_categ)){if(dot_categ != ini_dot_categ){dot_categ}}, if( ! base::is.null(x = facet_categ)){facet_categ}), drop = FALSE], incomparables = FALSE)
            # names(tempo) <- paste0(names(tempo), ".mean")
            tempo_mean <- base::data.frame(tempo_mean[base::order(tempo_mean$BOX, tempo_mean$PANEL, na.last = TRUE, decreasing = FALSE, method = "auto"), ], tempo, stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE)
        }
    }
    # at that stage, categ color and dot color are correctly attributed in data1, box_coord and dot_coord
    # end y dot coordinates recovery (create ini_box_coord, dot_coord and modify data1)
    # ylim range
    if(base::is.null(x = y_lim)){
        y_lim <- tempo_graph_info_ini$layout$panel_params[[1]]$y.range # finite = TRUE removes all the -Inf and Inf except if only this. In that case, whatever the -Inf and/or Inf present, output -Inf;Inf range. Idem with NA only
        if(base::any(( ! base::is.finite(x = y_lim)) | base::is.na(x = y_lim), na.rm = FALSE) | base::length(x = y_lim) != 2){ # kept but normally no more Inf in data1 # normally no NA with is.finite, etc.
            # nocov start (unreachable: internal consistency guard - y.range from ggplot_build is always finite for finite data)
            tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\ntempo_graph_info_ini$layout$panel_params[[1]]$y.range[1] CONTAINS NA OR Inf OR HAS LENGTH 1", collapse = NULL, recycle0 = FALSE)
           base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
            # nocov end
        }
    }else if(y_log != "no"){
        y_lim <- base::get(x = y_log, pos = -1L, envir = base::environment(fun = NULL), mode = "any", inherits = TRUE)(y_lim) # no env = sys.nframe(), inherit = FALSE in get() because look for function in the classical scope
    }
    if(y_log != "no"){
        # normally this control is not necessary anymore
        if(base::any( ! base::is.finite(x = y_lim), na.rm = FALSE)){ # normally no NA with is.finite
            # nocov start (unreachable: y_lim non-finite values are caught by arg_check, and y_lim computed from data is finite after the Inf/NaN row removal)
            tempo_cat <- base::paste0("ERROR IN ", function_name, "\ny_lim ARGUMENT CANNOT HAVE ZERO OR NEGATIVE VALUES WITH THE y_log ARGUMENT SET TO ", y_log, ":\n", base::paste(y_lim, collapse = " ", sep = " ", recycle0 = FALSE), "\nPLEASE, CHECK DATA VALUES (PRESENCE OF ZERO OR INF VALUES)", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
            # nocov end
        }
    }
    if(base::suppressWarnings(expr = base::all(y_lim %in% base::c(Inf, -Inf), na.rm = FALSE), classes = "warning")){ # all() without na.rm -> ok because y_lim cannot be NA (tested above)
        # nocov start (unreachable: Inf in y_lim is rejected by arg_check (inf_values = FALSE), and y_lim from ggplot_build is finite for non-empty data1, which is itself checked)
        # normally this control is not necessary anymore
        tempo_cat <- base::paste0("ERROR IN ", function_name, " y_lim CONTAINS Inf VALUES, MAYBE BECAUSE VALUES FROM data1 ARGUMENTS ARE NA OR Inf ONLY OR BECAUSE OF LOG SCALE REQUIREMENT", collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
        # nocov end
    }
    if(base::suppressWarnings(expr = base::any(base::is.na(x = y_lim), na.rm = FALSE), classes = "warning")){ # normally no NA with is.na
        # normally this control is not necessary anymore
            # nocov start (unreachable: y_lim cannot be NA at this stage (checked above))
        tempo_cat <- base::paste0("ERROR IN ", function_name, " y_lim CONTAINS NA OR NaN VALUES, MAYBE BECAUSE VALUES FROM data1 ARGUMENTS ARE NA OR Inf ONLY OR BECAUSE OF LOG SCALE REQUIREMENT", collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
            # nocov end
    }
    y_lim_order <- base::order(y_lim, na.last = TRUE, decreasing = FALSE, method = "auto") # to deal with inverse axis
    y_lim <- base::sort(x = y_lim, decreasing = FALSE)
    y_lim[1] <- y_lim[1] - base::abs(x = y_lim[2] - y_lim[1]) * base::ifelse(test = base::diff(x = y_lim_order) > 0, yes = y_bottom_extra_margin, no = y_top_extra_margin) # diff(y_lim_order) > 0 medians not inversed axis
    y_lim[2] <- y_lim[2] + base::abs(x = y_lim[2] - y_lim[1]) * base::ifelse(test = base::diff(x = y_lim_order) > 0, yes = y_top_extra_margin, no = y_bottom_extra_margin) # diff(y_lim_order) > 0 medians not inversed axis
    if(y_include_zero == TRUE){ # no need to check y_log != "no" because done before
        y_lim <- base::range(base::c(y_lim, 0), na.rm = TRUE, finite = TRUE) # finite = TRUE removes all the -Inf and Inf except if only this. In that case, whatever the -Inf and/or Inf present, output -Inf;Inf range. Idem with NA only
    }
    y_lim <- y_lim[y_lim_order]
    if(base::any(base::is.na(x = y_lim), na.rm = FALSE)){ # normally no NA with is.na
            # nocov start (unreachable: CODE INCONSISTENCY 2 internal guard - y_lim remains finite)
        tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nCODE INCONSISTENCY 2", collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
            # nocov end
    }
    # end ylim range
    
    
    
    
    
    
    # drawing
    # constant part
    tempo_gg_name <- "gg.indiv.plot."
    tempo_gg_count <- 0
    base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = add_gg, pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # add is directly put here to deal with additional variable of data, like when using facet_grid. No problem if add is a theme, will be dealt below
    base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::xlab(label = if(base::is.null(x = x_lab)){categ[1]}else{x_lab}), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
    base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::ylab(label = if(base::is.null(x = y_lab)){y}else{y_lab}), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
    base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::ggtitle(label = title, subtitle = ggplot2::waiver()), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
    # text angle management
    axis_just <- saferGG::gg_just(angle = x_angle, pos = base::ifelse(test = vertical == TRUE, yes = "bottom", no = "left"), kind = "axis", safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
    # end text angle management
    add_check <- TRUE
    if( ! base::is.null(x = add)){ # if add is NULL, then = 0
        if(base::grepl(pattern = "ggplot2\\s*::\\s*theme", x = add, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) == TRUE){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") \"ggplot2::theme\" STRING DETECTED IN THE add ARGUMENT\n-> INTERNAL GGPLOT2 THEME FUNCTIONS theme() AND theme_classic() HAVE BEEN INACTIVATED, TO BE USED BY THE USER\n-> article ARGUMENT WILL BE IGNORED", collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            add_check <- FALSE
        }
    }
    if(add_check == TRUE & article == TRUE){
        # WARNING: not possible to add theme()several times. NO message but the last one overwrites the others
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::theme_classic(base_size = text_size, base_family = "", header_family = NULL, base_line_size = text_size / 22, base_rect_size = text_size / 22, ink = "black", paper = "white", accent = "#3366FF"), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
        if(grid == TRUE){
            base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = m_gg <- ggplot2::theme(
                text = ggplot2::element_text(size = text_size, family = NULL, face = NULL, colour = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL, color = NULL, margin = NULL, debug = NULL, inherit.blank = FALSE), 
                plot.title = ggplot2::element_text(size = title_text_size, family = NULL, face = NULL, colour = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL, color = NULL, margin = NULL, debug = NULL, inherit.blank = FALSE), # stronger than text
                line = ggplot2::element_line(linewidth = 0.5, colour = NULL, linetype = NULL, lineend = NULL, color = NULL, linejoin = NULL, arrow = NULL, arrow.fill = NULL, inherit.blank = FALSE), 
                legend.key = ggplot2::element_rect(color = "white", linewidth = 1.5, fill = NULL, colour = NULL, linetype = NULL, linejoin = NULL, inherit.blank = FALSE), # size of the frame of the legend
                axis.line.y.left = ggplot2::element_line(colour = "black", linewidth = NULL, linetype = NULL, lineend = NULL, color = NULL, linejoin = NULL, arrow = NULL, arrow.fill = NULL, inherit.blank = FALSE), # draw lines for the y axis
                axis.line.x.bottom = ggplot2::element_line(colour = "black", linewidth = NULL, linetype = NULL, lineend = NULL, color = NULL, linejoin = NULL, arrow = NULL, arrow.fill = NULL, inherit.blank = FALSE), # draw lines for the x axis
                panel.grid.major.x = if(vertical == TRUE){NULL}else{ggplot2::element_line(colour = "grey85", linewidth = 0.75, linetype = NULL, lineend = NULL, color = NULL, linejoin = NULL, arrow = NULL, arrow.fill = NULL, inherit.blank = FALSE)},
                panel.grid.major.y = if(vertical == TRUE){ggplot2::element_line(colour = "grey85", linewidth = 0.75, linetype = NULL, lineend = NULL, color = NULL, linejoin = NULL, arrow = NULL, arrow.fill = NULL, inherit.blank = FALSE)}else{NULL},
                panel.grid.minor.y = if(vertical == TRUE){ggplot2::element_line(colour = "grey90", linewidth = 0.25, linetype = NULL, lineend = NULL, color = NULL, linejoin = NULL, arrow = NULL, arrow.fill = NULL, inherit.blank = FALSE)}else{NULL},
                axis.text.x = if(vertical == TRUE){ggplot2::element_text(angle = axis_just$angle, hjust = axis_just$hjust, vjust = axis_just$vjust, family = NULL, face = NULL, colour = NULL, size = NULL, lineheight = NULL, color = NULL, margin = NULL, debug = NULL, inherit.blank = FALSE)}else{NULL},
                axis.text.y = if(vertical == TRUE){NULL}else{ggplot2::element_text(angle = axis_just$angle, hjust = axis_just$hjust, vjust = axis_just$vjust, family = NULL, face = NULL, colour = NULL, size = NULL, lineheight = NULL, color = NULL, margin = NULL, debug = NULL, inherit.blank = FALSE)},
                strip.background = ggplot2::element_rect(fill = NA, colour = NA, linewidth = NULL, linetype = NULL, color = NULL, linejoin = NULL, inherit.blank = FALSE), complete = FALSE, validate = TRUE # for facet background
            ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
        }else{
            base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = m_gg <- ggplot2::theme(
                text = ggplot2::element_text(size = text_size, family = NULL, face = NULL, colour = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL, color = NULL, margin = NULL, debug = NULL, inherit.blank = FALSE), 
                plot.title = ggplot2::element_text(size = title_text_size, family = NULL, face = NULL, colour = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL, color = NULL, margin = NULL, debug = NULL, inherit.blank = FALSE), # stronger than text
                line = ggplot2::element_line(linewidth = 0.5, colour = NULL, linetype = NULL, lineend = NULL, color = NULL, linejoin = NULL, arrow = NULL, arrow.fill = NULL, inherit.blank = FALSE), 
                legend.key = ggplot2::element_rect(color = "white", linewidth = 1.5, fill = NULL, colour = NULL, linetype = NULL, linejoin = NULL, inherit.blank = FALSE), # size of the frame of the legend
                axis.line.y.left = ggplot2::element_line(colour = "black", linewidth = NULL, linetype = NULL, lineend = NULL, color = NULL, linejoin = NULL, arrow = NULL, arrow.fill = NULL, inherit.blank = FALSE), 
                axis.line.x.bottom = ggplot2::element_line(colour = "black", linewidth = NULL, linetype = NULL, lineend = NULL, color = NULL, linejoin = NULL, arrow = NULL, arrow.fill = NULL, inherit.blank = FALSE),
                axis.text.x = if(vertical == TRUE){ggplot2::element_text(angle = axis_just$angle, hjust = axis_just$hjust, vjust = axis_just$vjust, family = NULL, face = NULL, colour = NULL, size = NULL, lineheight = NULL, color = NULL, margin = NULL, debug = NULL, inherit.blank = FALSE)}else{NULL},
                axis.text.y = if(vertical == TRUE){NULL}else{ggplot2::element_text(angle = axis_just$angle, hjust = axis_just$hjust, vjust = axis_just$vjust, family = NULL, face = NULL, colour = NULL, size = NULL, lineheight = NULL, color = NULL, margin = NULL, debug = NULL, inherit.blank = FALSE)},
                strip.background = ggplot2::element_rect(fill = NA, colour = NA, linewidth = NULL, linetype = NULL, color = NULL, linejoin = NULL, inherit.blank = FALSE), complete = FALSE, validate = TRUE
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
            panel.grid.major.y = ggplot2::element_line(colour = "grey85", linewidth = 0.75, linetype = NULL, lineend = NULL, color = NULL, linejoin = NULL, arrow = NULL, arrow.fill = NULL, inherit.blank = FALSE), 
            panel.grid.minor.x = ggplot2::element_blank(), 
            panel.grid.minor.y = ggplot2::element_line(colour = "grey90", linewidth = 0.25, linetype = NULL, lineend = NULL, color = NULL, linejoin = NULL, arrow = NULL, arrow.fill = NULL, inherit.blank = FALSE), 
            strip.background = ggplot2::element_rect(fill = NA, colour = NA, linewidth = NULL, linetype = NULL, color = NULL, linejoin = NULL, inherit.blank = FALSE),
            axis.text.x = if(vertical == TRUE){ggplot2::element_text(angle = axis_just$angle, hjust = axis_just$hjust, vjust = axis_just$vjust, family = NULL, face = NULL, colour = NULL, size = NULL, lineheight = NULL, color = NULL, margin = NULL, debug = NULL, inherit.blank = FALSE)}else{NULL},
            axis.text.y = if(vertical == TRUE){NULL}else{ggplot2::element_text(angle = axis_just$angle, hjust = axis_just$hjust, vjust = axis_just$vjust, family = NULL, face = NULL, colour = NULL, size = NULL, lineheight = NULL, color = NULL, margin = NULL, debug = NULL, inherit.blank = FALSE)}, complete = FALSE, validate = TRUE
        ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
    }
    # Contrary to fun_gg_bar(), cannot plot the boxplot right now, because I need the dots plotted first
    base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_boxplot(data = data1, mapping = ggplot2::aes(x = .data[[categ[1]]], y = .data[[y]], group = .data[[categ[base::length(x = categ)]]])[base::c("group", "x", "y")], position = ggplot2::position_dodge(width = NULL, preserve = "total", orientation = "x", reverse = FALSE), color = NA, width = box_width, fill = NA, stat = "boxplot", outliers = TRUE, outlier.colour = NULL, outlier.color = NULL, outlier.fill = NULL, outlier.shape = NULL, outlier.size = NULL, outlier.stroke = 0.5, outlier.alpha = NULL, whisker.colour = NULL, whisker.color = NULL, whisker.linetype = NULL, whisker.linewidth = NULL, staple.colour = NULL, staple.color = NULL, staple.linetype = NULL, staple.linewidth = NULL, median.colour = NULL, median.color = NULL, median.linetype = NULL, median.linewidth = NULL, box.colour = NULL, box.color = NULL, box.linetype = NULL, box.linewidth = NULL, notch = FALSE, notchwidth = 0.5, staplewidth = 0, varwidth = FALSE, na.rm = FALSE, orientation = NA, show.legend = NA, inherit.aes = TRUE), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # this is to set the graph (i.e., a blanck boxplot to be able to use x coordinates to plot dots before boxes)
    # end constant part
    
    
    
    
    # graphic info recovery (including means)
    tempo_graph_info <- ggplot2::ggplot_build(plot = base::eval(expr = base::parse(text = base::paste0(base::paste(base::paste0(tempo_gg_name, 1:tempo_gg_count, collapse = NULL, recycle0 = FALSE), collapse = " + ", sep = " ", recycle0 = FALSE), ' + ggplot2::geom_boxplot(data = data1, mapping = ggplot2::aes(x = .data[[categ[1]]], y = .data[[y]], fill = .data[[categ[base::length(categ)]]])[base::c("fill", "x", "y")], position = ggplot2::position_dodge(width = NULL), width = box_width, notch = box_notch, coef = if(box_whisker_kind == "no"){0}else if(box_whisker_kind == "std"){1.5}else if(box_whisker_kind == "max"){Inf}) + ggplot2::scale_discrete_manual(aesthetics = "fill", name = box_legend_name, values = if(base::length(categ_color)== 1L){base::rep(categ_color, base::length(base::unique(data1[, categ[base::length(categ)]])))}else{categ_color})', collapse = NULL, recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))) # will be recovered later again, when ylim will be considered
    tempo_yx_ratio <- (tempo_graph_info$layout$panel_params[[1]]$y.range[2] - tempo_graph_info$layout$panel_params[[1]]$y.range[1]) / (tempo_graph_info$layout$panel_params[[1]]$x.range[2] - tempo_graph_info$layout$panel_params[[1]]$x.range[1])
    box_coord <- tempo_graph_info$data[[2]] # to have the summary statistics of the plot. Contrary to ini_box_plot, now integrates ylim Here because can be required for stat_pos when just box are plotted
    box_coord$x <- base::as.numeric(x = box_coord$x) # because x is of special class that block comparison of values using identical
    box_coord$PANEL <- base::as.numeric(x = box_coord$PANEL) # because numbers as levels. But may be a problem is facet are reordered ?
    box_coord <- box_coord[base::order(box_coord$group, box_coord$PANEL, na.last = TRUE, decreasing = FALSE, method = "auto"), ]
    if( ! (base::identical(x = tempo_mean$BOX, y = box_coord$group, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE) & base::identical(x = tempo_mean$PANEL, y = box_coord$PANEL, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE))){
            # nocov start (unreachable: internal consistency guard - tempo_mean and box_coord are derived from the same data)
        tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nidentical(tempo_mean$BOX, box_coord$group) & identical(tempo_mean$PANEL, box_coord$PANEL) DO NOT HAVE THE SAME VALUE ORDER", collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
            # nocov end
    }else{
        # tempo <- base::c(categ, if( ! base::is.null(dot_color) & ! base::is.null(dot_categ)){if(dot_categ != ini_dot_categ){dot_categ}}, if( ! base::is.null(facet_categ)){facet_categ})
        if(base::any(base::names(x = tempo_mean) %in% base::names(x = box_coord), na.rm = TRUE)){
            base::names(x = tempo_mean)[base::names(x = tempo_mean) %in% base::names(x = box_coord)] <- base::paste0(base::names(x = tempo_mean)[base::names(x = tempo_mean) %in% base::names(x = box_coord)], ".mean", collapse = NULL, recycle0 = FALSE)
        }
        box_coord <- base::data.frame(box_coord, tempo_mean, stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE)
    }
    # end graphic info recovery (including means)
    
    
    
    # stat output (will also serve for boxplot and mean display)
    # x not added now (to do not have them in stat_nolog)
    stat <- base::data.frame(
        MIN = box_coord$ymin_final, 
        QUART1 = box_coord$lower, 
        MEDIAN = box_coord$middle, 
        MEAN = box_coord$MEAN, 
        QUART3 = box_coord$upper, 
        MAX = box_coord$ymax_final, 
        WHISK_INF = box_coord$ymin, 
        BOX_INF = box_coord$lower, 
        NOTCH_INF = box_coord$notchlower, 
        NOTCH_SUP = box_coord$notchupper, 
        BOX_SUP = box_coord$upper, 
        WHISK_SUP = box_coord$ymax, 
        OUTLIERS = box_coord["outliers"], 
        tempo_mean[base::colnames(x = tempo_mean, do.NULL = TRUE, prefix = "col") != "MEAN"], 
        COLOR = box_coord$fill, 
        stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE
    ) # box_coord["outliers"] written like this because it is a list. X coordinates not put now because several features to set
    base::names(x = stat)[base::names(x = stat) == "outliers"] <- "OUTLIERS"
    stat_nolog <- stat # stat_nolog ini will serve for outputs
    if(y_log != "no"){
        stat_nolog[base::c("MIN", "QUART1", "MEDIAN", "MEAN", "QUART3", "MAX", "WHISK_INF", "BOX_INF", "NOTCH_INF", "NOTCH_SUP", "BOX_SUP", "WHISK_SUP")] <- base::ifelse(test = y_log == "log2", yes = 2, no = 10)^(stat_nolog[base::c("MIN", "QUART1", "MEDIAN", "MEAN", "QUART3", "MAX", "WHISK_INF", "BOX_INF", "NOTCH_INF", "NOTCH_SUP", "BOX_SUP", "WHISK_SUP")])
        stat_nolog$OUTLIERS <- base::lapply(X = stat_nolog$OUTLIERS, FUN = function(X){base::ifelse(test = y_log == "log2", yes = 2, no = 10)^X})
    }
    # end stat output (will also serve for boxplot and mean display)
    
    
    
    
    
    
    # x coordinates management (for random plotting and for stat display)
    # width commputations
    width_ini <- base::c(box_coord$xmax - box_coord$xmin)[1] # all the box widths are equal here. Only the first one taken
    width_correct <- width_ini * box_space / 2
    if( ! (base::identical(x = stat$BOX, y = box_coord$group, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE) & base::identical(x = stat$PANEL, y = box_coord$PANEL, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE))){
            # nocov start (unreachable: internal consistency guard - stat and box_coord are derived from the same data)
        tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nidentical(stat$BOX, box_coord$group) & identical(stat$PANEL, box_coord$PANEL) MUST BE IDENTICAL. CODE HAS TO BE MODIFIED", collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
            # nocov end
    }else{
        stat <- base::data.frame(
            stat, 
            X = box_coord$x, 
            X_BOX_INF = box_coord$xmin + width_correct, 
            X_BOX_SUP = box_coord$xmax - width_correct, 
            X_NOTCH_INF = box_coord$x - (box_coord$x - (box_coord$xmin + width_correct)) / 2, 
            X_NOTCH_SUP = box_coord$x + (box_coord$x - (box_coord$xmin + width_correct)) / 2, 
            X_WHISK_INF = box_coord$x - (box_coord$x - (box_coord$xmin + width_correct)) * box_whisker_width, 
            X_WHISK_SUP = box_coord$x + (box_coord$x - (box_coord$xmin + width_correct)) * box_whisker_width, 
            # tempo_mean[colnames(tempo_mean) != "MEAN"], # already added above
            stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE
        )
        stat$COLOR <- base::factor(x = stat$COLOR, levels = base::unique(x = categ_color, incomparables = FALSE), exclude = NA, ordered = base::is.ordered(x = stat$COLOR), nmax = NA)
        if( ! base::all(stat$NOTCH_SUP < stat$BOX_SUP & stat$NOTCH_INF > stat$BOX_INF, na.rm = TRUE) & box_notch == TRUE){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") SOME NOTCHES ARE BEYOND BOX HINGES. TRY ARGUMENT box_notch = FALSE", collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
        }
    }
    dot_jitter <- base::c((box_coord$xmax - width_correct) - (box_coord$xmin + width_correct))[1] * dot_jitter # real dot_jitter. (box_coord$xmin + width_correct) - (box_coord$xmax - width_correct))[1] is the width of the box. Is equivalent to (box_coord$x - (box_coord$xmin + width_correct))[1] * 2
    # end width commputations
    if( ! base::is.null(x = dot_color)){
        # random dots
        if(dot_tidy == FALSE){
            dot_coord_rd1 <- base::merge(x = dot_coord, y = box_coord[base::c("fill", "PANEL", "group", "x")], by = base::c("PANEL", "group"), sort = FALSE) # rd for random. Send the coord of the boxes into the coord data.frame of the dots (in the column x.y). WARNING: by = c("PANEL", "group") without fill column because PANEL & group columns are enough as only one value of x column per group number in box_coord. Thus, no need to consider fill column
            if(base::nrow(x = dot_coord_rd1) != base::nrow(x = dot_coord)){
            # nocov start (unreachable: internal consistency guard - merge on PANEL and group keys preserves row count)
                tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nTHE merge() FUNCTION DID NOT RETURN A CORRECT dot_coord_rd1 DATA FRAME. CODE HAS TO BE MODIFIED", collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
            # nocov end
            }
            sampled_dot_jitter <- if(base::nrow(x = dot_coord_rd1)== 1L){stats::runif(n = base::nrow(x = dot_coord_rd1), min = - dot_jitter / 2, max = dot_jitter / 2)}else{base::sample(x = stats::runif(n = base::nrow(x = dot_coord_rd1), min = - dot_jitter / 2, max = dot_jitter / 2), size = base::nrow(x = dot_coord_rd1), replace = FALSE, prob = NULL)}
            dot_coord_rd2 <- base::data.frame(dot_coord_rd1, dot.x = dot_coord_rd1$x.y + sampled_dot_jitter, stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE) # set the dot_jitter thanks to runif and dot_jitter range. Then, send the coord of the boxes into the coord data.frame of the dots (in the column x.y)
            if(base::length(x = categ)== 1L){
                # as.integer(data1[, categ[1]]) keeps the ids of the initial factor levels: remap to the renumbered ids of the remaining classes (same issue as for categ_check above, when classes disappeared during the NA/Inf removal of the color columns)
                tempo_data1 <- base::unique(x = base::data.frame(data1[categ[1]], group = base::as.integer(x = base::factor(x = base::as.integer(x = data1[, categ[1]]), levels = base::sort(x = base::unique(x = base::as.integer(x = data1[, categ[1]]), incomparables = FALSE), decreasing = FALSE), exclude = NA, ordered = FALSE, nmax = NA)), stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE), incomparables = FALSE) # categ[1] is factor
                base::names(x = tempo_data1)[base::names(x = tempo_data1) == categ[1]] <- base::paste0(categ[1], ".check", collapse = NULL, recycle0 = FALSE)
                verif <- base::paste0(categ[1], ".check", collapse = NULL, recycle0 = FALSE)
            }else if(base::length(x = categ) == 2L){
                tempo_data1 <- base::unique(
                    x = base::data.frame(
                        data1[base::c(categ[1], categ[2])], 
                        group = base::as.integer(x = base::factor(x = base::paste0(
                            base::formatC(x = base::as.integer(x = data1[, categ[2]]), width = base::nchar(x = base::max(base::as.integer(x = data1[, categ[2]]), na.rm = TRUE), type = "chars", allowNA = FALSE, keepNA = NA), flag = "0", digits = NULL, format = NULL, mode = NULL, big.mark = "", big.interval = 3L, small.mark = "", small.interval = 5L, decimal.mark = base::getOption(x = "OutDec", default = NULL), preserve.width = "individual", zero.print = NULL, replace.zero = TRUE, drop0trailing = FALSE), # convert factor into numeric with leading zero for proper ranking
                            ".", 
                            base::formatC(x = base::as.integer(x = data1[, categ[1]]), width = base::nchar(x = base::max(base::as.integer(x = data1[, categ[1]]), na.rm = TRUE), type = "chars", allowNA = FALSE, keepNA = NA), flag = "0", digits = NULL, format = NULL, mode = NULL, big.mark = "", big.interval = 3L, small.mark = "", small.interval = 5L, decimal.mark = base::getOption(x = "OutDec", default = NULL), preserve.width = "individual", zero.print = NULL, replace.zero = TRUE, drop0trailing = FALSE), collapse = NULL, recycle0 = FALSE# convert factor into numeric with leading zero for proper ranking
                        ), exclude = NA, ordered = FALSE, nmax = NA)), row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = FALSE # merge the 2 formatC() to create a new factor. The convertion to integer should recreate the correct group number
                    ), incomparables = FALSE
                ) # categ[2] first if categ[2] is used to make the categories in ggplot and categ[1] is used to make the x-axis
                base::names(x = tempo_data1)[base::names(x = tempo_data1) == categ[1]] <- base::paste0(categ[1], ".check", collapse = NULL, recycle0 = FALSE)
                base::names(x = tempo_data1)[base::names(x = tempo_data1) == categ[2]] <- base::paste0(categ[2], ".check", collapse = NULL, recycle0 = FALSE)
                verif <- base::c(base::paste0(categ[1], ".check", collapse = NULL, recycle0 = FALSE), base::paste0(categ[2], ".check", collapse = NULL, recycle0 = FALSE))
            }else{
            # nocov start (unreachable: CODE INCONSISTENCY 3 internal guard - categ length is 1 or 2 (arg_check))
                tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nCODE INCONSISTENCY 3", collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
            # nocov end
            }
            dot_coord_rd3 <- base::merge(x = dot_coord_rd2, y = tempo_data1, by = base::intersect(x = "group", y = "group"), sort = FALSE) # send the factors of data1 into coord. WARNING: I have replaced by = "group" by intersect("group", "group") because of an error due to wrong group group merging in dot_coord_rd3
            if(base::nrow(x = dot_coord_rd3) != base::nrow(x = dot_coord) | ( ! saferTool::comp_2d(data1 = dot_coord_rd3[categ], data2 = dot_coord_rd3[verif], safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)$identical_content)){
            # nocov start (unreachable: internal consistency guard - merge on group keys preserves row count)
                tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nTHE merge() FUNCTION DID NOT RETURN A CORRECT dot_coord_rd3 DATA FRAME. CODE HAS TO BE MODIFIED", collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
            # nocov end
            }
            # end random dots
        }
        # tidy dots
        # coordinates are recovered during plotting (see dot_coord_tidy1 below)
        # end tidy dots
    }
    # end x coordinates management (for random plotting and for stat display)
    
    
    
    
    
    # boxplot display before dot display if box_fill = TRUE
    coord_names <- NULL
    # creation of the data frame for (main box + legend) and data frame for means
    if(box_notch == FALSE){
        for(i3 in base::seq_along(along.with = categ)){
            if(i3== 1L){
                tempo_polygon <- base::data.frame(GROUPX = base::c(base::t(x = stat[, base::rep(x = categ[i3], 5)])), stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE)
            }else{
                tempo_polygon <- base::cbind(tempo_polygon, base::c(base::t(x = stat[, base::rep(x = categ[i3], 5)])), stringsAsFactors = TRUE, deparse.level = 1)
            }
        }
        base::names(x = tempo_polygon) <- categ
        tempo_polygon <- base::data.frame(X = base::c(base::t(x = stat[, base::c("X_BOX_INF", "X_BOX_SUP", "X_BOX_SUP", "X_BOX_INF", "X_BOX_INF")])), Y = base::c(base::t(x = stat[, base::c("BOX_INF", "BOX_INF", "BOX_SUP", "BOX_SUP", "BOX_INF")])), COLOR = base::c(base::t(x = stat[, base::c("COLOR", "COLOR", "COLOR", "COLOR", "COLOR")])), BOX = base::as.character(x = base::c(base::t(x = stat[, base::c("BOX", "BOX", "BOX", "BOX", "BOX")]))), tempo_polygon, stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE)
        if( ! base::is.null(x = facet_categ)){
            for(i4 in base::seq_along(along.with = facet_categ)){
                if( ! facet_categ[i4] %in% base::names(x = tempo_polygon)){ # facet column already present in tempo_polygon if it is also a categ column: do not append a second copy (duplicate column names break ggplot2::geom_path())
                    tempo_polygon <- base::data.frame(tempo_polygon, base::c(base::t(x = stat[, base::c(facet_categ[i4], facet_categ[i4], facet_categ[i4], facet_categ[i4], facet_categ[i4])])), stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE)
                    base::names(x = tempo_polygon)[base::length(x = base::names(x = tempo_polygon))] <- facet_categ[i4]
                }
            }
        }
    }else{
        for(i3 in base::seq_along(along.with = categ)){
            if(i3== 1L){
                tempo_polygon <- base::data.frame(GROUPX = base::c(base::t(x = stat[, base::rep(x = categ[i3], 11)])), stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE)
            }else{
                tempo_polygon <- base::cbind(tempo_polygon, base::c(base::t(x = stat[, base::rep(x = categ[i3], 11)])), stringsAsFactors = TRUE, deparse.level = 1)
            }
        }
        base::names(x = tempo_polygon) <- categ
        tempo_polygon <- base::data.frame(X = base::c(base::t(x = stat[, base::c("X_BOX_INF", "X_BOX_SUP", "X_BOX_SUP", "X_NOTCH_SUP", "X_BOX_SUP", "X_BOX_SUP", "X_BOX_INF", "X_BOX_INF", "X_NOTCH_INF", "X_BOX_INF", "X_BOX_INF")])), Y = base::c(base::t(x = stat[, base::c("BOX_INF", "BOX_INF", "NOTCH_INF", "MEDIAN", "NOTCH_SUP", "BOX_SUP", "BOX_SUP", "NOTCH_SUP", "MEDIAN", "NOTCH_INF", "BOX_INF")])), COLOR = base::c(base::t(x = stat[, base::c("COLOR", "COLOR", "COLOR", "COLOR", "COLOR", "COLOR", "COLOR", "COLOR", "COLOR", "COLOR", "COLOR")])), BOX = base::as.character(x = base::c(base::t(x = stat[, base::c("BOX", "BOX", "BOX", "BOX", "BOX", "BOX", "BOX", "BOX", "BOX", "BOX", "BOX")]))), tempo_polygon, stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE)
        if( ! base::is.null(x = facet_categ)){
            for(i4 in base::seq_along(along.with = facet_categ)){
                if( ! facet_categ[i4] %in% base::names(x = tempo_polygon)){ # facet column already present in tempo_polygon if it is also a categ column: do not append a second copy (duplicate column names break ggplot2::geom_path())
                    tempo_polygon <- base::data.frame(tempo_polygon, base::c(base::t(x = stat[, base::c(facet_categ[i4], facet_categ[i4], facet_categ[i4], facet_categ[i4], facet_categ[i4], facet_categ[i4], facet_categ[i4], facet_categ[i4], facet_categ[i4], facet_categ[i4], facet_categ[i4])])), stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE)
                    base::names(x = tempo_polygon)[base::length(x = base::names(x = tempo_polygon))] <- facet_categ[i4]
                }
            }
        }
    }
    tempo_polygon$COLOR <- base::factor(x = tempo_polygon$COLOR, levels = base::unique(x = categ_color, incomparables = FALSE), exclude = NA, ordered = base::is.ordered(x = tempo_polygon$COLOR), nmax = NA)
    if( ! base::is.null(x = categ_class_order)){
        for(i3 in base::seq_along(along.with = categ)){
            tempo_polygon[, categ[i3]] <- base::factor(x = tempo_polygon[, categ[i3]], levels = categ_class_order[[i3]], exclude = NA, ordered = base::is.ordered(x = tempo_polygon[, categ[i3]]), nmax = NA)
        }
    }
    # modified name of dot_categ column (e.g., "Categ1_DOT") must be included for boxplot using ridy dots
    if( ! base::is.null(x = dot_color) & ! base::is.null(x = dot_categ)){
        if(dot_categ != ini_dot_categ){
            tempo_polygon <- base::data.frame(tempo_polygon, GROUPX = tempo_polygon[, ini_dot_categ], stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE)
            base::names(x = tempo_polygon)[base::names(x = tempo_polygon) == "GROUPX"] <- dot_categ
            
        }
    }
    tempo_diamon_mean <- base::data.frame(X = base::c(base::t(x = stat[, base::c("X", "X_NOTCH_INF", "X", "X_NOTCH_SUP", "X")])), Y = base::c(base::t(x = base::cbind(stat["MEAN"] - (stat[, "X"] - stat[, "X_NOTCH_INF"]) * tempo_yx_ratio, stat["MEAN"], stat["MEAN"] + (stat[, "X"] - stat[, "X_NOTCH_INF"]) * tempo_yx_ratio, stat["MEAN"], stat["MEAN"] - (stat[, "X"] - stat[, "X_NOTCH_INF"]) * tempo_yx_ratio, stringsAsFactors = TRUE, deparse.level = 1))), COLOR = base::c(base::t(x = stat[, base::c("COLOR", "COLOR", "COLOR", "COLOR", "COLOR")])), GROUP = base::c(base::t(x = stat[, base::c("BOX", "BOX", "BOX", "BOX", "BOX")])), stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE) # stringsAsFactors = TRUE for cbind() because stat["MEAN"] is a data frame. Otherwise, stringsAsFactors is not an argument for cbind() on vectors
    if( ! base::is.null(x = facet_categ)){
        for(i3 in base::seq_along(along.with = facet_categ)){
            tempo_diamon_mean <- base::data.frame(tempo_diamon_mean, base::c(base::t(x = stat[, base::c(facet_categ[i3], facet_categ[i3], facet_categ[i3], facet_categ[i3], facet_categ[i3])])), stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE)
            base::names(x = tempo_diamon_mean)[base::length(x = base::names(x = tempo_diamon_mean))] <- facet_categ[i3]
        }
    }
    tempo_diamon_mean$COLOR <- base::factor(x = tempo_diamon_mean$COLOR, levels = base::unique(x = categ_color, incomparables = FALSE), exclude = NA, ordered = base::is.ordered(x = tempo_diamon_mean$COLOR), nmax = NA)
    # end creation of the data frame for (main box + legend) and data frame for means
    if(box_fill == TRUE){
        # assign(paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), ggplot2::geom_boxplot(data = data1, mapping = ggplot2::aes_string(x = categ[1], y = y, color = categ[length(categ)], fill = categ[length(categ)]), position = ggplot2::position_dodge(width = NULL), width = box_width, size = box_line_size, notch = box_notch, coef = if(box_whisker_kind == "no"){0}else if(box_whisker_kind == "std"){1.5}else if(box_whisker_kind == "max"){Inf}, alpha = box_alpha, outlier.shape = if( ! base::is.null(dot_color)){NA}else{21}, outlier.color = if( ! base::is.null(dot_color)){NA}else{dot_border_color}, outlier.fill = if( ! base::is.null(dot_color)){NA}else{NULL}, outlier.size = if( ! base::is.null(dot_color)){NA}else{dot_size}, outlier.stroke = if( ! base::is.null(dot_color)){NA}else{dot_border_size}, outlier.alpha = if( ! base::is.null(dot_color)){NA}else{dot_alpha})) # the color, size, etc. of the outliers are dealt here. outlier.color = NA to do not plot outliers when dots are already plotted. Finally, boxplot redrawn (see below)
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_polygon(
            data = tempo_polygon, 
            mapping = ggplot2::aes(x = .data[["X"]], y = .data[["Y"]], group = .data[["BOX"]], fill = .data[[categ[base::length(x = categ)]]], color = .data[[categ[base::length(x = categ)]]])[base::c("group", "fill", "colour", "x", "y")], 
            linewidth = box_line_size, 
            alpha = box_alpha, stat = "identity", position = "identity", rule = "evenodd", lineend = "butt", linejoin = "round", linemitre = 10, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE # works only for fill, not for color
        ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
        coord_names <- base::c(coord_names, "main.box")
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = .data[["X"]], xend = .data[["X"]], y = .data[["BOX_SUP"]], yend = .data[["WHISK_SUP"]], group = categ[base::length(x = categ)]), color = "black", linewidth = box_line_size, alpha = box_alpha, stat = "identity", position = "identity", arrow = NULL, arrow.fill = NULL, lineend = "butt", linejoin = "round", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # 
        coord_names <- base::c(coord_names, "sup.whisker")
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = .data[["X"]], xend = .data[["X"]], y = .data[["BOX_INF"]], yend = .data[["WHISK_INF"]], group = categ[base::length(x = categ)]), color = "black", linewidth = box_line_size, alpha = box_alpha, stat = "identity", position = "identity", arrow = NULL, arrow.fill = NULL, lineend = "butt", linejoin = "round", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # 
        coord_names <- base::c(coord_names, "inf.whisker")
        if(box_whisker_width > 0){
            base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = .data[["X_WHISK_INF"]], xend = .data[["X_WHISK_SUP"]], y = .data[["WHISK_SUP"]], yend = .data[["WHISK_SUP"]], group = categ[base::length(x = categ)]), color = "black", linewidth = box_line_size, alpha = box_alpha, lineend = "round", stat = "identity", position = "identity", arrow = NULL, arrow.fill = NULL, linejoin = "round", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # 
            coord_names <- base::c(coord_names, "sup.whisker.edge")
            base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = .data[["X_WHISK_INF"]], xend = .data[["X_WHISK_SUP"]], y = .data[["WHISK_INF"]], yend = .data[["WHISK_INF"]], group = categ[base::length(x = categ)]), color = "black", linewidth = box_line_size, alpha = box_alpha, lineend = "round", stat = "identity", position = "identity", arrow = NULL, arrow.fill = NULL, linejoin = "round", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # 
            coord_names <- base::c(coord_names, "inf.whisker.edge")
        }
        if(box_mean == TRUE){
            # assign(paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), ggplot2::geom_point(data = stat, mapping = ggplot2::aes_string(x = "X", y = "MEAN", group = categ[length(categ)]), shape = 23, stroke = box_line_size * 2, fill = stat$COLOR, size = box_mean_size, color = "black", alpha = box_alpha)) # group used in aesthetic to do not have it in the legend
            base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_polygon(
                data = tempo_diamon_mean, 
                mapping = ggplot2::aes(x = .data[["X"]], y = .data[["Y"]], group = .data[["GROUP"]]), 
                fill = tempo_diamon_mean[, "COLOR"], 
                color = grDevices::hsv(h = 0, s = 0, v = 0, alpha = box_alpha), # outline of the polygon in black but with alpha
                linewidth = box_line_size, 
                alpha = box_alpha, stat = "identity", position = "identity", rule = "evenodd", lineend = "butt", linejoin = "round", linemitre = 10, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE
            ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
            coord_names <- base::c(coord_names, "mean")
        }
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = if(box_notch == FALSE){.data[["X_BOX_INF"]]}else{.data[["X_NOTCH_INF"]]}, xend = if(box_notch == FALSE){.data[["X_BOX_SUP"]]}else{.data[["X_NOTCH_SUP"]]}, y = .data[["MEDIAN"]], yend = .data[["MEDIAN"]], group = categ[base::length(x = categ)]), color = "black", linewidth = box_line_size * 2, alpha = box_alpha, stat = "identity", position = "identity", arrow = NULL, arrow.fill = NULL, lineend = "butt", linejoin = "round", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # 
        coord_names <- base::c(coord_names, "median")
    }
    # end boxplot display before dot display if box_fill = TRUE
    
    
    
    
    
    
    # dot display
    if( ! base::is.null(x = dot_color)){
        if(dot_tidy == FALSE){
            if(base::is.null(x = dot_categ)){
                if(dot_border_size == 0){
                    base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_point(
                        data = dot_coord_rd3, 
                        mapping = ggplot2::aes(x = .data[["dot.x"]], y = .data[["y"]], group = .data[[categ[base::length(x = categ)]]])[base::c("group", "x", "y")], 
                        size = dot_size, 
                        shape = 19, 
                        color = dot_coord_rd3$dot_color, 
                        alpha = dot_alpha, stat = "identity", position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE
                    ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # group used in aesthetic to do not have it in the legend. Here ggplot2::scale_discrete_manual() cannot be used because of the group easthetic
                }else{
                    base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_point(
                        data = dot_coord_rd3, 
                        mapping = ggplot2::aes(x = .data[["dot.x"]], y = .data[["y"]], group = .data[[categ[base::length(x = categ)]]])[base::c("group", "x", "y")], 
                        shape = 21, 
                        stroke = dot_border_size, 
                        color = if(base::is.null(x = dot_border_color)){dot_coord_rd3$dot_color}else{base::rep(x = dot_border_color, base::nrow(x = dot_coord_rd3))}, 
                        size = dot_size, 
                        fill = dot_coord_rd3$dot_color, 
                        alpha = dot_alpha, stat = "identity", position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE
                    ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # group used in aesthetic to do not have it in the legend. Here ggplot2::scale_discrete_manual() cannot be used because of the group easthetic
                }
            }else{
                if(dot_border_size == 0){
                    base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_point(
                        data = dot_coord_rd3, 
                        mapping = ggplot2::aes(x = .data[["dot.x"]], y = .data[["y"]], alpha = .data[[dot_categ]])[base::c("alpha", "x", "y")], 
                        size = dot_size, 
                        shape = 19, 
                        color = dot_coord_rd3$dot_color, stat = "identity", position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE
                    ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # group used in aesthetic to do not have it in the legend. Here ggplot2::scale_discrete_manual() cannot be used because of the group easthetic
                }else{
                    base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_point(
                        data = dot_coord_rd3, 
                        mapping = ggplot2::aes(x = .data[["dot.x"]], y = .data[["y"]], alpha = .data[[dot_categ]])[base::c("alpha", "x", "y")], 
                        size = dot_size, 
                        shape = 21, 
                        stroke = dot_border_size, 
                        color = if(base::is.null(x = dot_border_color)){dot_coord_rd3$dot_color}else{base::rep(x = dot_border_color, base::nrow(x = dot_coord_rd3))}, 
                        fill = dot_coord_rd3$dot_color, stat = "identity", position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE
                    ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # group used in aesthetic to do not have it in the legend. Here ggplot2::scale_discrete_manual() cannot be used because of the group easthetic
                }
                base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_discrete_manual(aesthetics = "alpha", name = dot_legend_name, values = base::rep(x = dot_alpha, base::length(x = dot_categ_class_order)), guide = ggplot2::guide_legend(override.aes = base::list(fill = dot_color, color = if(base::is.null(x = dot_border_color)){dot_color}else{dot_border_color}, stroke = dot_border_size, alpha = dot_alpha), title = ggplot2::waiver(), theme = NULL, position = NULL, direction = NULL, nrow = NULL, ncol = NULL, reverse = FALSE, order = 0), breaks = ggplot2::waiver()), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # values are the values of color (which is the border color in geom_box. WARNING: values = categ_color takes the numbers to make the colors if categ_color is a factor
            }
            coord_names <- base::c(coord_names, "dots")
        }else if(dot_tidy == TRUE){
            # here plot using group -> no scale
            base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_dotplot(
                data = dot_coord, 
                mapping = ggplot2::aes(x = .data[[categ[1]]], y = .data[["y"]], group = .data[["group"]])[base::c("group", "x", "y")], # not dot_categ here because the classes of dot_categ create new separations
                position = ggplot2::position_dodge(width = box_width, preserve = "total", orientation = "x", reverse = FALSE), 
                binpositions = "all", 
                binaxis = "y", 
                stackdir = "center", 
                alpha = dot_alpha, 
                fill = dot_coord$dot_color, 
                stroke = dot_border_size, 
                color = if(base::is.null(x = dot_border_color)){dot_coord$dot_color}else{base::rep(x = dot_border_color, base::nrow(x = dot_coord))}, 
                show.legend = FALSE, # WARNING: do not use show.legend = TRUE because it uses the arguments outside aes() as aesthetics (here color and fill). Thus I must find a way using ggplot2::scale_discrete_manual()
                binwidth = (y_lim[2] - y_lim[1]) / dot_tidy_bin_nb, method = "dotdensity", stackratio = 1, dotsize = 1, stackgroups = FALSE, origin = NULL, right = TRUE, width = 0.9, drop = FALSE, na.rm = FALSE, inherit.aes = TRUE
            ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # geom_dotplot ggplot2 v3.3.0: I had to remove rev() in fill and color # very weird behavior of geom_dotplot ggplot2 v3.2.1, (1) because with aes group = (to avoid legend), the dot plotting is not good in term of coordinates, and (2) because data1 seems reorderer according to x = categ[1] before plotting. Thus, I have to use fill = dot_coord[rev(order(dot_coord[, categ[1]], decreasing = TRUE)), "dot_color"] to have the good corresponding colors # show_legend option do not remove the legend, only the aesthetic of the legend (dot, line, etc.)
            coord_names <- base::c(coord_names, "dots")
            if( ! base::is.null(x = dot_categ)){
                base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_dotplot(
                    data = dot_coord, 
                    mapping = ggplot2::aes(x = .data[[categ[1]]], y = .data[["y"]], alpha = .data[[dot_categ]])[base::c("alpha", "x", "y")], # not dot_categ here because the classes of dot_categ create new separations
                    position = ggplot2::position_dodge(width = box_width, preserve = "total", orientation = "x", reverse = FALSE), 
                    binpositions = "all", 
                    binaxis = "y", 
                    stackdir = "center", 
                    fill = NA, 
                    stroke = NA, 
                    color = NA, 
                    # WARNING: do not use show.legend = TRUE because it uses the arguments outside aes() as aesthetics (here color and fill). Thus I must find a way using ggplot2::scale_discrete_manual()
                    binwidth = (y_lim[2] - y_lim[1]) / dot_tidy_bin_nb, method = "dotdensity", stackratio = 1, dotsize = 1, stackgroups = FALSE, origin = NULL, right = TRUE, width = 0.9, drop = FALSE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE
                ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # geom_dotplot ggplot2 v3.3.0: I had to remove rev() in fill and color # very weird behavior of geom_dotplot ggplot2 v3.2.1, (1) because with aes group = (to avoid legend), the dot plotting is not good in term of coordinates, and (2) because data1 seems reorderer according to x = categ[1] before plotting. Thus, I have to use fill = dot_coord[rev(order(dot_coord[, categ[1]], decreasing = TRUE)), "dot_color"] to have the good corresponding colors # show_legend option do not remove the legend, only the aesthetic of the legend (dot, line, etc.)
                # assign(paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), ggplot2::scale_discrete_manual(aesthetics = "linetype", name = dot_legend_name, values = rep(1, length(categ_color)))) # values = rep("black", length(categ_color)) are the values of color (which is the border color of dots), and this modify the border color on the plot. WARNING: values = categ_color takes the numbers to make the colors if categ_color is a factor
                coord_names <- base::c(coord_names, "bad_remove")
                base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_discrete_manual(aesthetics = "alpha", name = dot_legend_name, values = base::rep(x = dot_alpha, base::length(x = dot_categ_class_order)), labels = dot_categ_class_order, guide = ggplot2::guide_legend(title = if(ini_dot_categ == categ[base::length(x = categ)]){dot_categ}else{ini_dot_categ}, override.aes = base::list(fill = base::levels(x = dot_coord$dot_color), color = if(base::is.null(x = dot_border_color)){base::levels(x = dot_coord$dot_color)}else{dot_border_color}, stroke = dot_border_size, alpha = dot_alpha), theme = NULL, position = NULL, direction = NULL, nrow = NULL, ncol = NULL, reverse = FALSE, order = 0), breaks = ggplot2::waiver()), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # values are the values of color (which is the border color in geom_box. WARNING: values = categ_color takes the numbers to make the colors if categ_color is a factor
            }
            # coordinates of tidy dots
            tempo_coord <- ggplot2::ggplot_build(plot = base::eval(expr = base::parse(text = base::paste(base::paste0(tempo_gg_name, 1:tempo_gg_count, collapse = NULL, recycle0 = FALSE), collapse = " + ", sep = " ", recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)))$data # to have the tidy dot coordinates
            if(base::length(x = base::which(x = base::sapply(X = tempo_coord, FUN = function(X){base::any(base::names(x = X) == "binwidth", na.rm = TRUE)}, simplify = TRUE, USE.NAMES = TRUE), arr.ind = FALSE, useNames = TRUE)) < 1){ # detect the compartment having the dot coordinates (at least one: when dot_categ is not NULL, a second geom_dotplot layer devoted to the alpha legend also has a binwidth column, and the first compartment is the one with the coordinates) of tempo_coord which is the binned data frame
                # if(length(which(sapply(tempo_coord, FUN = nrow) == nrow(data1))) > if(base::is.null(dot_categ)){1}else{2}){ # this does not work if only one dot per class, thus replaced by above # if(base::is.null(dot_categ)){1}else{2} because 1 dotplot if dot_categ is NULL and 2 dotplots if not, with the second being a blank dotplot with wrong coordinates. Thus take the first in that situation
            # nocov start (unreachable: internal consistency guard - the dotplot layer always provides a binwidth column)
                tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nEITHER MORE THAN 1 OR NO COMPARTMENT HAVING A DATA FRAME WITH binwidth AS COLUMN NAME IN THE tempo_coord LIST (FOR TIDY DOT COORDINATES). CODE HAS TO BE MODIFIED", collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
            # nocov end
            }else{
                # dot_coord_tidy1 <- tempo_coord[[which(sapply(tempo_coord, FUN = nrow) == nrow(data1))[1]]] # this does not work if only one dot per class, thus replaced by above # the second being a blank dotplot with wrong coordinates. Thus take the first whatever situation
                dot_coord_tidy1 <- tempo_coord[[base::which(x = base::sapply(X = tempo_coord, FUN = function(X){base::any(base::names(x = X) == "binwidth", na.rm = TRUE)}, simplify = TRUE, USE.NAMES = TRUE), arr.ind = FALSE, useNames = TRUE)[1]]] # detect the compartment of tempo_coord which is the binned data frame (the first one: when dot_categ is not NULL, the second matching compartment is a blank legend layer)
                dot_coord_tidy1$x <- base::as.numeric(x = dot_coord_tidy1$x) # because weird class
                dot_coord_tidy1$PANEL <- base::as.numeric(x = dot_coord_tidy1$PANEL) # because numbers as levels. But may be a problem is facet are reordered ?
            }
            # tempo_box_coord <- merge(box_coord, unique(dot_coord[, c("PANEL", "group", categ)]), by = c("PANEL", "group"), sort = FALSE) # not required anymore because box_coord already contains categ do not add dot_categ and tidy_group_coord here because the coordinates are for stats. Add the categ in box_coord. WARNING: by = c("PANEL", "group") without fill column because PANEL & group columns are enough as only one value of x column per group number in box_coord. Thus, no need to consider fill column
            # below inactivated because not true when dealing with dot_categ different from categ
            # if(nrow(tempo_box_coord) != nrow(box_coord)){
            # tempo_cat <- paste0("INTERNAL CODE ERROR IN ", function_name, "\nTHE merge() FUNCTION DID NOT RETURN A CORRECT tempo_box_coord DATA FRAME. CODE HAS TO BE MODIFIED")
            # stop(paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            # }
            dot_coord_tidy2 <- base::merge(x = dot_coord_tidy1, y = box_coord[base::c("fill", "PANEL", "group", "x", categ)], by = base::c("PANEL", "group"), sort = FALSE) # send the coord of the boxes into the coord data.frame of the dots (in the column x.y).WARNING: by = c("PANEL", "group") without fill column because PANEL & group columns are enough as only one value of x column per group number in tempo_box_coord. Thus, no need to consider fill colum # DANGER: from here the fill.y and x.y (from tempo_box_coord) are not good in dot_coord_tidy2. It is ok because Categ1 Categ2 from tempo_box_coord are ok with the group column from dot_coord_tidy1. This is due to the fact that dot_coord.tidy resulting from geom_dotplot does not make the same groups as the other functions
            if(base::nrow(x = dot_coord_tidy2) != base::nrow(x = dot_coord)){
            # nocov start (unreachable: internal consistency guard - merge on PANEL and group keys preserves row count)
                tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nTHE merge() FUNCTION DID NOT RETURN A CORRECT dot_coord_tidy2 DATA FRAME. CODE HAS TO BE MODIFIED", collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
            # nocov end
            }
            # From here, check for dot_coord_tidy3 which wil be important for stat over the plot. WARNING: dot_categ has nothing to do here for stat coordinates. Thus, not in tempo_data1
            if(base::length(x = categ)== 1L){
                # as.integer(data1[, categ[1]]) keeps the ids of the initial factor levels: remap to the renumbered ids of the remaining classes (same issue as for categ_check above, when classes disappeared during the NA/Inf removal of the color columns)
                tempo_data1 <- base::unique(x = base::data.frame(data1[categ[1]], group = base::as.integer(x = base::factor(x = base::as.integer(x = data1[, categ[1]]), levels = base::sort(x = base::unique(x = base::as.integer(x = data1[, categ[1]]), incomparables = FALSE), decreasing = FALSE), exclude = NA, ordered = FALSE, nmax = NA)), stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE), incomparables = FALSE) # categ[1] is factor
                base::names(x = tempo_data1)[base::names(x = tempo_data1) == categ[1]] <- base::paste0(categ[1], ".check", collapse = NULL, recycle0 = FALSE)
                verif <- base::paste0(categ[1], ".check", collapse = NULL, recycle0 = FALSE)
            }else if(base::length(x = categ) == 2L){
                tempo_data1 <- base::unique(
                    x = base::data.frame(
                        data1[base::c(categ[1], categ[2])], 
                        group = base::as.integer(x = base::factor(x = base::paste0(
                            base::formatC(x = base::as.integer(x = data1[, categ[2]]), width = base::nchar(x = base::max(base::as.integer(x = data1[, categ[2]]), na.rm = TRUE), type = "chars", allowNA = FALSE, keepNA = NA), flag = "0", digits = NULL, format = NULL, mode = NULL, big.mark = "", big.interval = 3L, small.mark = "", small.interval = 5L, decimal.mark = base::getOption(x = "OutDec", default = NULL), preserve.width = "individual", zero.print = NULL, replace.zero = TRUE, drop0trailing = FALSE), # convert factor into numeric with leading zero for proper ranking
                            ".", 
                            base::formatC(x = base::as.integer(x = data1[, categ[1]]), width = base::nchar(x = base::max(base::as.integer(x = data1[, categ[1]]), na.rm = TRUE), type = "chars", allowNA = FALSE, keepNA = NA), flag = "0", digits = NULL, format = NULL, mode = NULL, big.mark = "", big.interval = 3L, small.mark = "", small.interval = 5L, decimal.mark = base::getOption(x = "OutDec", default = NULL), preserve.width = "individual", zero.print = NULL, replace.zero = TRUE, drop0trailing = FALSE), collapse = NULL, recycle0 = FALSE# convert factor into numeric with leading zero for proper ranking
                        ), exclude = NA, ordered = FALSE, nmax = NA)), row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = FALSE # merge the 2 formatC() to create a new factor. The convertion to integer should recreate the correct group number
                    ), incomparables = FALSE
                ) # categ[2] first if categ[2] is used to make the categories in ggplot and categ[1] is used to make the x-axis
                base::names(x = tempo_data1)[base::names(x = tempo_data1) == categ[1]] <- base::paste0(categ[1], ".check", collapse = NULL, recycle0 = FALSE)
                base::names(x = tempo_data1)[base::names(x = tempo_data1) == categ[2]] <- base::paste0(categ[2], ".check", collapse = NULL, recycle0 = FALSE)
                verif <- base::c(base::paste0(categ[1], ".check", collapse = NULL, recycle0 = FALSE), base::paste0(categ[2], ".check", collapse = NULL, recycle0 = FALSE))
            }else{
            # nocov start (unreachable: CODE INCONSISTENCY 4 internal guard - categ length is 1 or 2 (arg_check))
                tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nCODE INCONSISTENCY 4", collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
            # nocov end
            }
            dot_coord_tidy3 <- base::merge(x = dot_coord_tidy2, y = tempo_data1, by = base::intersect(x = "group", y = "group"), sort = FALSE) # send the factors of data1 into coord. WARNING: I have tested intersect("group", "group") instead of by = "group". May be come back to by = "group" in case of error. But I did this because of an error in dot_coord_rd3 above
            if(base::nrow(x = dot_coord_tidy3) != base::nrow(x = dot_coord) | ( ! saferTool::comp_2d(data1 = dot_coord_tidy3[categ], data2 = dot_coord_tidy3[verif], safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)$identical_content)){
            # nocov start (unreachable: internal consistency guard - merge on group keys preserves row count)
                tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nTHE merge() FUNCTION DID NOT RETURN A CORRECT dot_coord_tidy3 DATA FRAME. CODE HAS TO BE MODIFIED", collapse = NULL, recycle0 = FALSE)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
            # nocov end
            }
            # end coordinates of tidy dots
        }
    }
    # end dot display
    
    
    
    # boxplot display (if box_fill = FALSE, otherwise, already plotted above)
    if(box_fill == TRUE){
        # overcome "work only for the filling of boxes, not for the frame. See https://github.com/tidyverse/ggplot2/issues/252"
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_discrete_manual(aesthetics = "fill", name = box_legend_name, values = if(base::length(x = categ_color)== 1L){base::rep(x = categ_color, base::length(x = base::unique(x = data1[, categ[base::length(x = categ)]], incomparables = FALSE)))}else{categ_color}, guide = ggplot2::guide_legend(order = 1, title = ggplot2::waiver(), theme = NULL, position = NULL, direction = NULL, override.aes = base::list(), nrow = NULL, ncol = NULL, reverse = FALSE), breaks = ggplot2::waiver()), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) #, guide = ggplot2::guide_legend(override.aes = list(fill = levels(tempo_polygon$COLOR), color = "black")))) # values are the values of color (which is the border color in geom_box. WARNING: values = categ_color takes the numbers to make the colors if categ_color is a factor
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_discrete_manual(aesthetics = "color", name = box_legend_name, values = base::rep(x = grDevices::hsv(h = 0, s = 0, v = 0, alpha = box_alpha), base::length(x = base::unique(x = data1[, categ[base::length(x = categ)]], incomparables = FALSE))), guide = ggplot2::guide_legend(order = 1, title = ggplot2::waiver(), theme = NULL, position = NULL, direction = NULL, override.aes = base::list(), nrow = NULL, ncol = NULL, reverse = FALSE), breaks = ggplot2::waiver()), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # , guide = ggplot2::guide_legend(override.aes = list(color = "black", alpha = box_alpha)))) # values are the values of color (which is the border color in geom_box. WARNING: values = categ_color takes the numbers to make the colors if categ_color is a factor # outline of the polygon in black but with alpha
    }else{
        # assign(paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), ggplot2::geom_boxplot(data = data1, mapping = ggplot2::aes_string(x = categ[1], y = y, color = categ[length(categ)], fill = categ[length(categ)]), position = ggplot2::position_dodge(width = NULL), width = box_width, size = box_line_size, notch = box_notch, alpha = box_alpha, coef = if(box_whisker_kind == "no"){0}else if(box_whisker_kind == "std"){1.5}else if(box_whisker_kind == "max"){Inf}, outlier.shape = if( ! base::is.null(dot_color)){NA}else{21}, outlier.color = if( ! is.null(dot_color)){NA}else{if(dot_border_size == 0){NA}else{dot_border_color}}, outlier.fill = if( ! base::is.null(dot_color)){NA}else{NULL}, outlier.size = if( ! base::is.null(dot_color)){NA}else{dot_size}, outlier.stroke = if( ! base::is.null(dot_color)){NA}else{dot_border_size}, outlier.alpha = if( ! base::is.null(dot_color)){NA}else{dot_alpha})) # the color, size, etc. of the outliers are dealt here. outlier.color = NA to do not plot outliers when dots are already plotted
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_path(
            data = tempo_polygon, 
            mapping = ggplot2::aes(x = .data[["X"]], y = .data[["Y"]], group = .data[["BOX"]], color = .data[[categ[base::length(x = categ)]]])[base::c("group", "colour", "x", "y")], 
            linewidth = box_line_size, 
            alpha = box_alpha, 
            lineend = "round", 
            linejoin = "round", stat = "identity", position = "identity", arrow = NULL, arrow.fill = NULL, linemitre = 10, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE
        ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
        coord_names <- base::c(coord_names, "main.box")
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = if(box_notch == FALSE){.data[["X_BOX_INF"]]}else{.data[["X_NOTCH_INF"]]}, xend = if(box_notch == FALSE){.data[["X_BOX_SUP"]]}else{.data[["X_NOTCH_SUP"]]}, y = .data[["MEDIAN"]], yend = .data[["MEDIAN"]], group = categ[base::length(x = categ)]), color = stat$COLOR, linewidth = box_line_size * 2, alpha = box_alpha, stat = "identity", position = "identity", arrow = NULL, arrow.fill = NULL, lineend = "butt", linejoin = "round", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # 
        coord_names <- base::c(coord_names, "median")
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = .data[["X"]], xend = .data[["X"]], y = .data[["BOX_SUP"]], yend = .data[["WHISK_SUP"]], group = categ[base::length(x = categ)]), color = stat$COLOR, linewidth = box_line_size, alpha = box_alpha, stat = "identity", position = "identity", arrow = NULL, arrow.fill = NULL, lineend = "butt", linejoin = "round", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # 
        coord_names <- base::c(coord_names, "sup.whisker")
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = .data[["X"]], xend = .data[["X"]], y = .data[["BOX_INF"]], yend = .data[["WHISK_INF"]], group = categ[base::length(x = categ)]), color = stat$COLOR, linewidth = box_line_size, alpha = box_alpha, stat = "identity", position = "identity", arrow = NULL, arrow.fill = NULL, lineend = "butt", linejoin = "round", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # 
        coord_names <- base::c(coord_names, "inf.whisker")
        if(box_whisker_width > 0){
            base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = .data[["X_WHISK_INF"]], xend = .data[["X_WHISK_SUP"]], y = .data[["WHISK_SUP"]], yend = .data[["WHISK_SUP"]], group = categ[base::length(x = categ)]), color = stat$COLOR, linewidth = box_line_size, alpha = box_alpha, lineend = "round", stat = "identity", position = "identity", arrow = NULL, arrow.fill = NULL, linejoin = "round", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # 
            coord_names <- base::c(coord_names, "sup.whisker.edge")
            base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = .data[["X_WHISK_INF"]], xend = .data[["X_WHISK_SUP"]], y = .data[["WHISK_INF"]], yend = .data[["WHISK_INF"]], group = categ[base::length(x = categ)]), color = stat$COLOR, linewidth = box_line_size, alpha = box_alpha, lineend = "round", stat = "identity", position = "identity", arrow = NULL, arrow.fill = NULL, linejoin = "round", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # 
            coord_names <- base::c(coord_names, "inf.whisker.edge")
        }
        if(box_mean == TRUE){
            # assign(paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), ggplot2::geom_point(data = stat, mapping = ggplot2::aes_string(x = "X", y = "MEAN", group = categ[length(categ)]), shape = 23, stroke = box_line_size * 2, color = stat$COLOR, size = box_mean_size, fill = NA, alpha = box_alpha)) # group used in aesthetic to do not have it in the legend. Here ggplot2::scale_discrete_manual() cannot be used because of the group easthetic
            base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_path(
                data = tempo_diamon_mean, 
                mapping = ggplot2::aes(x = .data[["X"]], y = .data[["Y"]], group = .data[["GROUP"]]), 
                color = tempo_diamon_mean[, "COLOR"], 
                linewidth = box_line_size, 
                alpha = box_alpha, 
                lineend = "round", 
                linejoin = "round", stat = "identity", position = "identity", arrow = NULL, arrow.fill = NULL, linemitre = 10, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE
            ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
            coord_names <- base::c(coord_names, "mean")
        }
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_discrete_manual(aesthetics = "fill", name = box_legend_name, values = base::rep(x = NA, base::length(x = base::unique(x = data1[, categ[base::length(x = categ)]], incomparables = FALSE))), breaks = ggplot2::waiver()), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) #, guide = ggplot2::guide_legend(override.aes = list(color = categ_color)))) # values are the values of color (which is the border color in geom_box. WARNING: values = categ_color takes the numbers to make the colors if categ_color is a factor
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_discrete_manual(aesthetics = "color", name = box_legend_name, values = if(base::length(x = categ_color)== 1L){base::rep(x = categ_color, base::length(x = base::unique(x = data1[, categ[base::length(x = categ)]], incomparables = FALSE)))}else{categ_color}, guide = ggplot2::guide_legend(override.aes = base::list(alpha = if(plot == TRUE & ((base::length(x = grDevices::dev.list()) > 0 & base::names(x = grDevices::dev.cur()) == "windows") | (base::length(x = grDevices::dev.list()) == 0L & base::Sys.info()["sysname"] == "Windows"))){1}else{box_alpha}), title = ggplot2::waiver(), theme = NULL, position = NULL, direction = NULL, nrow = NULL, ncol = NULL, reverse = FALSE, order = 0), breaks = ggplot2::waiver()), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # , guide = ggplot2::guide_legend(override.aes = list(color = as.character(categ_color))))) # values are the values of color (which is the border color in geom_box. WARNING: values = categ_color takes the numbers to make the colors if categ_color is a factor
        if(plot == TRUE & ((base::length(x = grDevices::dev.list()) > 0 & base::names(x = grDevices::dev.cur()) == "windows") | (base::length(x = grDevices::dev.list()) == 0L & base::Sys.info()["sysname"] == "Windows"))){ # if any Graph device already open and this device is "windows", or if no Graph device opened yet and we are on windows system -> prevention of alpha legend bug on windows using value 1
            # to avoid a bug on windows: if alpha argument is different from 1 for lines (transparency), then lines are not correctly displayed in the legend when using the R GUI (bug https://github.com/tidyverse/ggplot2/issues/2452). No bug when using a pdf
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") GRAPHIC DEVICE USED ON A WINDOWS SYSTEM ->\nTRANSPARENCY OF THE LINES IS INACTIVATED IN THE LEGEND TO PREVENT A WINDOWS DEPENDENT BUG (SEE https://github.com/tidyverse/ggplot2/issues/2452)\nTO OVERCOME THIS ON WINDOWS, USE ANOTHER DEVICE (pdf() FOR INSTANCE)", collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
        }
    }
    if(box_alpha == 0){ # remove box legend because no boxes drawn
        # add this after the scale_xxx_manual() for boxplots
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::guides(fill = "none", color = "none"), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # inactivate the legend
    }
    # end boxplot display (if box_fill = FALSE, otherwise, already plotted above)
    
    
    
    
    # stat display
    # layer after dots but ok, behind dots on the plot
    if( ! base::is.null(x = stat_pos)){
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count,") NUMBERS DISPLAYED ARE ", base::ifelse(test = stat_mean == FALSE, yes = "MEDIANS", no = "MEANS"), collapse = NULL, recycle0 = FALSE)
        warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
        if(stat_pos == "top"){
            tempo_stat <- base::data.frame(stat, Y = y_lim[2], stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE) # I had to create a data frame for geom_tex() so that facet is taken into account, (ggplot2::annotate() does not deal with facet because no data and mapping arguments). Of note, facet_categ is in tempo_stat, via tempo_mean, via dot_coord
            if(stat_mean == FALSE){tempo_stat$MEDIAN <- base::formatC(x = stat_nolog$MEDIAN, digits = 2, drop0trailing = TRUE, format = "f", width = NULL, flag = "", mode = NULL, big.mark = "", big.interval = 3L, small.mark = "", small.interval = 5L, decimal.mark = base::getOption(x = "OutDec", default = NULL), preserve.width = "individual", zero.print = NULL, replace.zero = TRUE)}else{tempo_stat$MEAN <- base::formatC(x = stat_nolog$MEAN, digits = 2, drop0trailing = TRUE, format = "f", width = NULL, flag = "", mode = NULL, big.mark = "", big.interval = 3L, small.mark = "", small.interval = 5L, decimal.mark = base::getOption(x = "OutDec", default = NULL), preserve.width = "individual", zero.print = NULL, replace.zero = TRUE)}
            base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_text(
                data = tempo_stat, 
                mapping = ggplot2::aes(x = .data[["X"]], y = .data[["Y"]], label = .data[[base::ifelse(test = stat_mean == FALSE, yes = "MEDIAN", no = "MEAN")]])[base::c("label", "x", "y")],
                size = stat_size, 
                color = "black", 
                angle = stat_angle, 
                hjust = stat_just$hjust, 
                vjust = stat_just$vjust, stat = "identity", position = "nudge", parse = FALSE, check_overlap = FALSE, size.unit = "mm", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE
            ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # stat$X used here because identical to stat_nolog but has the X. WARNING: no need of order() for labels because box_coord$x set the order. For justification, see https://stackoverflow.com/questions/7263849/what-do-hjust-and-vjust-do-when-making-a-plot-using-ggplot
            coord_names <- base::c(coord_names, "stat_pos")
        }else if(stat_pos == "above"){
            # stat coordinates
            if( ! base::is.null(x = dot_color)){ # for text just above max dot
                if(dot_tidy == FALSE){
                    tempo_stat_ini <- dot_coord_rd3
                }else if(dot_tidy == TRUE){
                    tempo_stat_ini <- dot_coord_tidy3
                    tempo_stat_ini$x.y <- tempo_stat_ini$x.x # this is just to be able to use tempo_stat.ini$x.y for untidy or tidy dots (remember that dot_coord_tidy3$x.y is not good, see above)
                }
                stat_coord1 <- stats::aggregate(x = tempo_stat_ini["y"], by = {x_env <- if(base::length(x = categ)== 1L){base::list(tempo_stat_ini$group, tempo_stat_ini$PANEL, tempo_stat_ini$x.y, tempo_stat_ini[, categ[1]])}else if(base::length(x = categ) == 2L){base::list(tempo_stat_ini$group, tempo_stat_ini$PANEL, tempo_stat_ini$x.y, tempo_stat_ini[, categ[1]], tempo_stat_ini[, categ[2]])} ;base::names(x = x_env) <- if(base::length(x = categ)== 1L){base::c("group", "PANEL", "x.y", categ[1])}else if(base::length(x = categ) == 2L){base::c("group", "PANEL", "x.y", categ[1], categ[2])} ; x_env}, FUN = min, na.rm = TRUE)
                base::names(x = stat_coord1)[base::names(x = stat_coord1) == "y"] <- "dot.min"
                stat_coord2 <- stats::aggregate(x = tempo_stat_ini["y"], by = {x_env <- if(base::length(x = categ)== 1L){base::list(tempo_stat_ini$group, tempo_stat_ini$PANEL, tempo_stat_ini$x.y, tempo_stat_ini[, categ[1]])}else if(base::length(x = categ) == 2L){base::list(tempo_stat_ini$group, tempo_stat_ini$PANEL, tempo_stat_ini$x.y, tempo_stat_ini[, categ[1]], tempo_stat_ini[, categ[2]])} ; base::names(x = x_env) <- if(base::length(x = categ)== 1L){base::c("group", "PANEL", "x.y", categ[1])}else if(base::length(x = categ) == 2L){base::c("group", "PANEL", "x.y", categ[1], categ[2])} ; x_env}, FUN = max, na.rm = TRUE)
                base::names(x = stat_coord2) <- base::paste0(base::names(x = stat_coord2), "_from.dot.max", collapse = NULL, recycle0 = FALSE)
                base::names(x = stat_coord2)[base::names(x = stat_coord2) == "y_from.dot.max"] <- "dot.max"
                stat_coord3 <- base::cbind(box_coord[base::order(box_coord$group, box_coord$PANEL, na.last = TRUE, decreasing = FALSE, method = "auto"), ], stat_coord1[base::order(stat_coord1$group, stat_coord1$x.y, na.last = TRUE, decreasing = FALSE, method = "auto"), ], stat_coord2[base::order(stat_coord2$group, stat_coord2$x.y, na.last = TRUE, decreasing = FALSE, method = "auto"), ], stringsAsFactors = TRUE, deparse.level = 1) # 
                if( ! base::all(base::identical(x = base::round(x = stat_coord3$x, digits = 9), y = base::round(x = base::as.numeric(x = stat_coord3$x.y), digits = 9), num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE), na.rm = TRUE)){ # as.numeric() because stat_coord3$x is class "mapped_discrete" "numeric"
            # nocov start (unreachable: internal consistency guard - cbind of aligned coordinates preserves x values)
                    tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nFUSION OF box_coord, stat_coord1 AND stat_coord2 ACCORDING TO box_coord$x, stat_coord1$x.y AND stat_coord2$x.y IS NOT CORRECT. CODE HAS TO BE MODIFIED", collapse = NULL, recycle0 = FALSE)
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
            # nocov end
                }
                # text_coord <- stat_coord3[, c("x", "group", "dot.min", "dot.max")]
                # names(text_coord)[names(text_coord) == "dot.min"] <- "text.min.pos"
                #names(text_coord)[names(text_coord) == "dot.max"] <- "text.max.pos"
                box_coord <- box_coord[base::order(box_coord$x, box_coord$group, box_coord$PANEL, na.last = TRUE, decreasing = FALSE, method = "auto"), ]
                # text_coord <- text_coord[order(text_coord$x), ] # to be sure to have the two objects in the same order for x. WARNING: cannot add identical(as.integer(text_coord$group), as.integer(box_coord$group)) because with error, the correspondence between x and group is not the same
                stat_coord3 <- stat_coord3[base::order(stat_coord3$x, stat_coord3$group, stat_coord3$PANEL, na.last = TRUE, decreasing = FALSE, method = "auto"), ] # to be sure to have the two objects in the same order for x. WARNING: cannot add identical(as.integer(text_coord$group), as.integer(box_coord$group)) because with error, the correspondence between x and group is not the same
                if( ! (base::identical(x = box_coord$x, y = stat_coord3$x, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE) & base::identical(x = box_coord$group, y = stat_coord3$group, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE) & base::identical(x = box_coord$PANEL, y = stat_coord3$PANEL, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE))){
            # nocov start (unreachable: internal consistency guard - idem after ordering)
                    tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\ntext_coord AND box_coord DO NOT HAVE THE SAME x, group AND PANEL COLUMN CONTENT", collapse = NULL, recycle0 = FALSE)
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
            # nocov end
                }
            }else{
                stat_coord3 <- box_coord
            }
            stat_coord3 <- base::data.frame(
                stat_coord3, 
                Y = stat_coord3[, base::ifelse(
                    test = base::is.null(x = dot_color), 
                    yes = base::ifelse(test = base::diff(x = y_lim) > 0, yes = "ymax", no = "ymin"), 
                    no = base::ifelse(test = base::diff(x = y_lim) > 0, yes = "ymax_final", no = "ymin_final")
                )], 
                stringsAsFactors = TRUE, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE
            ) # ymax is top whisker, ymax_final is top dot
            # stat_coord3 <- data.frame(stat_coord3, Y = vector("numeric", length = nrow(stat_coord3)), stringsAsFactors = TRUE)
            # check_Y <- as.logical(stat_coord3$Y) # convert everything in Y into FALSE (because Y is full of zero)
            # end stat coordinates
            # stat display
            # performed twice: first for y values >=0, then y values < 0, because only a single value allowed for hjust anf vjust
            if(stat_mean == FALSE){
                tempo_center_ref <- "middle"
            }else{
                tempo_center_ref <- "MEAN"
            }
            # if(base::is.null(dot_color)){
            # tempo_low_ref <- "ymin"
            # tempo_high_ref <- "ymax"
            # }else{
            # tempo_low_ref <- "ymin_final"
            # tempo_high_ref <- "ymax_final"
            # }
            # tempo_log_high <- if(diff(y_lim) > 0){stat_coord3[, tempo_center_ref] >= 0}else{stat_coord3[, tempo_center_ref] < 0}
            # tempo_log_low <- if(diff(y_lim) > 0){stat_coord3[, tempo_center_ref] < 0}else{stat_coord3[, tempo_center_ref] >= 0}
            # stat_coord3$Y[tempo_log_high] <- stat_coord3[tempo_log_high, tempo_high_ref]
            # stat_coord3$Y[tempo_log_low] <- stat_coord3[tempo_log_low, tempo_low_ref]
            # add distance
            stat_coord3$Y <- stat_coord3$Y + base::diff(x = y_lim) * stat_dist / 100
            # end add distance
            # correct median or mean text format
            if(y_log != "no"){
                stat_coord3[, tempo_center_ref] <- base::ifelse(test = y_log == "log2", yes = 2, no = 10)^(stat_coord3[, tempo_center_ref])
            }
            stat_coord3[, tempo_center_ref] <- base::formatC(x = stat_coord3[, tempo_center_ref], digits = 2, drop0trailing = TRUE, format = "f", width = NULL, flag = "", mode = NULL, big.mark = "", big.interval = 3L, small.mark = "", small.interval = 5L, decimal.mark = base::getOption(x = "OutDec", default = NULL), preserve.width = "individual", zero.print = NULL, replace.zero = TRUE)
            # end correct median or mean text format
            # if(any(tempo_log_high) == TRUE){
            # tempo_stat <- stat_coord3[tempo_log_high,]
            base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::geom_text(
                data = stat_coord3, 
                mapping = ggplot2::aes(x = .data[["x"]], y = .data[["Y"]], label = .data[[tempo_center_ref]])[base::c("label", "x", "y")],
                size = stat_size, 
                color = "black", 
                angle = stat_angle, 
                hjust = stat_just$hjust, 
                vjust = stat_just$vjust, stat = "identity", position = "nudge", parse = FALSE, check_overlap = FALSE, size.unit = "mm", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE
            ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # WARNING: no need of order() for labels because box_coord$x set the order
            coord_names <- base::c(coord_names, "stat_pos")
            # }
            # if(any(tempo_log_low) == TRUE){
            # tempo_stat <- stat_coord3[tempo_log_low,]
            # assign(paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), ggplot2::geom_text(
            # data = tempo_stat, 
            # mapping = ggplot2::aes_string(x = "x", y = "Y", label = tempo_center_ref),
            # size = stat_size, 
            # color = "black", 
            # hjust = base::ifelse(vertical == TRUE, 0.5, 0.5 + stat_dist), 
            # vjust = base::ifelse(vertical == TRUE, 0.5 + stat_dist, 0.5)
            # )) # WARNING: no need of order() for labels because box_coord$x set the order
            # coord_names <- c(coord_names, "stat_pos_negative")
            # }
            # end stat display
        }else{
            # nocov start (unreachable: CODE INCONSISTENCY 5 internal guard - stat display path is always stat_pos or the else is unreachable)
            tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nCODE INCONSISTENCY 5", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
            # nocov end
        }
    }
    # end stat display
    # legend management
    if(legend_show == FALSE){ # must be here because must be before bef_final_plot <- 
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::guides(fill = "none", color = "none", alpha = "none"), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # inactivate the initial legend
    }
    # end legend management
    
    
    
    # y scale management (cannot be before dot plot management)
    # the rescaling aspect is complicated and not intuitive. See:
    # explaination: https://github.com/tidyverse/ggplot2/issues/3948
    # the oob argument of scale_y_continuous() https://ggplot2.tidyverse.org/reference/scale_continuous.html
    # see also https://github.com/rstudio/cheatsheets/blob/master/data-visualization-2.1.pdf
    # secondary ticks
    bef_final_plot <- ggplot2::ggplot_build(plot = base::eval(expr = base::parse(text = base::paste(base::paste(base::paste0(tempo_gg_name, 1:tempo_gg_count, collapse = NULL, recycle0 = FALSE), collapse = " + ", sep = " ", recycle0 = FALSE), ' + if(vertical == TRUE){ggplot2::scale_y_continuous(expand = base::c(0, 0), limits = base::sort(y_lim), oob = scales::rescale_none)}else{ggplot2::coord_flip(ylim = y_lim)}', sep = " ", collapse = NULL, recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))) # here I do not need the x-axis and y-axis orientation, I just need the number of main ticks and the legend. I DI NOT UNDERSTAND THE COMMENT HERE BECAUSE WE NEED COORD_FLiP
    tempo_coord <- bef_final_plot$layout$panel_params[[1]]
    # y_second_tick_positions: coordinates of secondary ticks (only if y_second_tick_nb argument is non NULL or if y_log argument is different from "no")
    if(y_log != "no"){ # integer main ticks for log2 and log10
        tempo_scale <- (base::as.integer(x = base::min(y_lim, na.rm = TRUE)) - 1):(base::as.integer(x = base::max(y_lim, na.rm = TRUE)) + 1)
    }else{
        tempo <- if(base::is.null(x = base::attributes(x = tempo_coord$y$breaks))){tempo_coord$y$breaks}else{base::unlist(x = base::attributes(x = tempo_coord$y$breaks), recursive = TRUE, use.names = TRUE)}
        if(base::all(base::is.na(x = tempo), na.rm = FALSE)){# all() without na.rm -> ok because is.na() cannot be NA
            # nocov start (unreachable: internal consistency guard - ggplot2 always returns at least one non-NA break for finite y_lim)
            tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nONLY NA IN tempo_coord$y$breaks", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
            # nocov end
        }
        tempo_scale <- saferGraph::scale2(lim = y_lim, n = base::ifelse(test = base::is.null(x = y_tick_nb), yes = base::length(x = tempo[ ! base::is.na(x = tempo)]), no = y_tick_nb), kind = "approx", safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) # in ggplot 3.3.0, tempo_coord$y.major_source replaced by tempo_coord$y$breaks. If fact: n = base::ifelse(base::is.null(y_tick_nb), length(tempo[ ! is.na(tempo)]), y_tick_nb)) replaced by n = base::ifelse(base::is.null(y_tick_nb), 4, y_tick_nb))
    }
    y_second_tick_values <- NULL
    y_second_tick_pos <- NULL
    if(y_log != "no"){
        tempo <- saferGraph::inter_ticks(lim = y_lim, log = y_log, breaks = NULL, n = NULL, warn_print = FALSE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) # warn_print = FALSE: the inter_ticks() warnings are added to the warn of this function, printed or not according to the warn_print argument
        y_second_tick_values <- tempo$values
        y_second_tick_pos <- tempo$coordinates
        if( ! base::is.null(x = tempo$warn)){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") FROM saferGraph::inter_ticks():\n", tempo$warn, collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
        }
        # if(vertical == TRUE){ # do not remove in case the bug is fixed
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::annotate(geom = "segment", y = y_second_tick_pos, yend = y_second_tick_pos, x = tempo_coord$x.range[1], xend = tempo_coord$x.range[1] + base::diff(x = tempo_coord$x.range) / 80, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL, na.rm = FALSE), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
        # }else{ # not working because of the ggplot2 bug
        # assign(paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1), ggplot2::annotate(geom = "segment", x = y_second_tick_pos, xend = y_second_tick_pos, y = tempo_coord$y.range[1], yend = tempo_coord$y.range[1] + diff(tempo_coord$y.range) / 80))
        # }
        coord_names <- base::c(coord_names, "y_second_tick_positions")
    }else if(( ! base::is.null(x = y_second_tick_nb)) & y_log == "no"){
        # if(y_second_tick_nb > 0){ #inactivated because already checked before
        if(base::length(x = tempo_scale) < 2){
            tempo_cat1 <- base::c("y_tick_nb", "y_second_tick_nb")
            tempo_cat2 <- base::sapply(X = base::list(y_tick_nb, y_second_tick_nb), FUN = base::paste0, collapse = " ", simplify = TRUE, USE.NAMES = TRUE)
            tempo_sep <- base::sapply(X = base::mapply(x = " ", times = base::max(base::nchar(x = tempo_cat1, type = "chars", allowNA = FALSE, keepNA = NA), na.rm = FALSE) - base::nchar(x = tempo_cat1, type = "chars", allowNA = FALSE, keepNA = NA) + 3, FUN = base::rep, SIMPLIFY = FALSE, MoreArgs = NULL, USE.NAMES = TRUE), FUN = base::paste0, collapse = "", simplify = TRUE, USE.NAMES = TRUE)
            tempo_cat <- base::paste0("ERROR IN ", function_name, "\nTHE NUMBER OF GENERATED TICKS FOR THE Y-AXIS IS NOT CORRECT: ", base::length(x = tempo_scale), "\nUSING THESE ARGUMENT SETTINGS (NO DISPLAY MEANS NULL VALUE):\n", base::paste0(tempo_cat1, tempo_sep, tempo_cat2, collapse = "\n", recycle0 = FALSE), "\nPLEASE, TEST OTHER VALUES", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
        }else{
            tempo <- saferGraph::inter_ticks(lim = y_lim, log = y_log, breaks = tempo_scale, n = y_second_tick_nb, warn_print = FALSE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) # warn_print = FALSE: see above
        }
        y_second_tick_values <- tempo$values
        y_second_tick_pos <- tempo$coordinates
        if( ! base::is.null(x = tempo$warn)){
            # nocov start (not reached in tests: the breaks from saferGraph::scale2() follow the y_lim order and span y_lim, so inter_ticks() returns no warning)
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") FROM saferGraph::inter_ticks():\n", tempo$warn, collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            # nocov end
        }
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::annotate(
            geom = "segment", 
            y = y_second_tick_pos, 
            yend = y_second_tick_pos, 
            x = if(vertical == TRUE){tempo_coord$x.range[1]}else{tempo_coord$y.range[1]}, 
            xend = if(vertical == TRUE){tempo_coord$x.range[1] + base::diff(x = tempo_coord$x.range) / 80}else{tempo_coord$y.range[1] + base::diff(x = tempo_coord$y.range) / 80}, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL, na.rm = FALSE
        ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
        coord_names <- base::c(coord_names, "y_second_tick_positions")
    }
    # end y_second_tick_positions
    # for the ggplot2 bug with y_log, this does not work: eval(parse(text = base::ifelse(vertical == FALSE & y_log == "log10", "ggplot2::scale_x_continuous", "ggplot2::scale_y_continuous")))
    base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::scale_y_continuous(
        breaks = tempo_scale, 
        minor_breaks = y_second_tick_pos, 
        labels = if(y_log == "log10"){scales::trans_format(trans = "identity", format = scales::math_format(expr = 10^.x, format = base::force))}else if(y_log == "log2"){scales::trans_format(trans = "identity", format = scales::math_format(expr = 2^.x, format = base::force))}else if(y_log == "no"){ggplot2::waiver()}else{
            # nocov start (unreachable: y_log is restricted to the options c("no", "log2", "log10") by the arg_check() block above)
            tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nCODE INCONSISTENCY 6", collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }, # == in stop() to be able to add several messages between ==
        expand = base::c(0, 0), # remove space after after axis limits
        limits = base::sort(x = y_lim, decreasing = FALSE), # NA indicate that limits must correspond to data limits but ylim() already used
        oob = scales::rescale_none, 
        transform = base::ifelse(test = base::diff(x = y_lim) < 0, yes = "reverse", no = "identity"), name = ggplot2::waiver(), n.breaks = NULL, na.value = NA_real_, guide = ggplot2::waiver(), position = "left", sec.axis = ggplot2::waiver() # equivalent to ggplot2::scale_y_reverse() but create the problem of y-axis label disappearance with y_lim decreasing. Thus, do not use. Use ylim() below and after this
    ), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE)
    if(vertical == TRUE){
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::coord_cartesian(ylim = y_lim, xlim = NULL, expand = TRUE, default = FALSE, clip = "on", reverse = "none", ratio = NULL), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # problem of ggplot2::ylim() is that it redraws new breaks # coord_cartesian(ylim = y_lim)) not used because bug -> y-axis label disappearance with y_lim decreasing I DO NOT UNDERSTAND THIS MESSAGE WHILE I USE COORD_CARTESIAN # clip = "off" to have secondary ticks outside plot region does not work
    }else{
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::coord_flip(ylim = y_lim, xlim = NULL, expand = TRUE, clip = "on"), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # clip = "off" to have secondary ticks outside plot region does not work # create the problem of y-axis label disappearance with y_lim decreasing. IDEM ABOVE
        
    }
    # end y scale management (cannot be before dot plot management)
    
    
    # legend management
    if( ! base::is.null(x = legend_width)){
        legend_final <- saferGG::gg_get_legend(ggplot_built = bef_final_plot, lib_path = lib_path, safer_check = FALSE, error_text = embed_error_text) # get legend
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = ggplot2::guides(fill = "none", color = "none", alpha = "none"), pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # inactivate the initial legend
        if(base::is.null(x = legend_final) & plot == TRUE){ # even if any(unlist(legend_disp)) is TRUE
            # nocov start (unreachable: the box legend (scale_discrete_manual) always exists when legend_width is non-NULL)
            legend_final <- ggplot2::ggplot(data = NULL, mapping = ggplot2::aes(), environment = base::environment(fun = NULL))+ggplot2::theme_void(base_size = 11, base_family = "", header_family = NULL, base_line_size = 11 / 22, base_rect_size = 11 / 22, ink = "black", paper = scales::alpha(colour = "black", alpha = 0), accent = "#3366FF") # empty graph instead of legend
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") LEGEND REQUESTED (NON NULL categ ARGUMENT OR legend_show ARGUMENT SET TO TRUE)\nBUT IT SEEMS THAT THE PLOT HAS NO LEGEND -> EMPTY LEGEND SPACE CREATED BECAUSE OF THE NON NULL legend_width ARGUMENT\n", collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            # nocov end
        }
    }
    # end legend management


    # add layers management
    if(base::length(x = add_layers) > 0L){
        base::assign(x = base::paste0(tempo_gg_name, tempo_gg_count <- tempo_gg_count + 1, collapse = NULL, recycle0 = FALSE), value = add_layers, pos = -1, envir = base::environment(fun = NULL), inherits = FALSE, immediate = TRUE) # layers of add, drawn above the boxes and dots (see the add management section)
    }
    # end add layers management
    
    
    # drawing
    fin_plot <- base::suppressMessages(expr = base::suppressWarnings(expr = base::eval(expr = base::parse(text = base::paste(base::paste0(tempo_gg_name, 1:tempo_gg_count, collapse = NULL, recycle0 = FALSE), collapse = " + ", sep = " ", recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)), classes = "warning"), classes = "message")
    grob_save <- NULL
    if(plot == TRUE){
        # following lines inactivated because of problem in warn_recov and message_recov
        # assign("env_fun_get_message", new.env())
        # assign("tempo_gg_name", tempo_gg_name, envir = env_fun_get_message)
        # assign("tempo_gg_count", tempo_gg_count, envir = env_fun_get_message)
        # assign("add", add, envir = env_fun_get_message)
        # two next line: for the moment, I cannot prevent the warning printing
        # warn_recov <- fun_get_message(paste(paste(paste0(tempo_gg_name, 1:tempo_gg_count), collapse = " + "), if(base::is.null(add)){NULL}else{add}), kind = "warning", header = FALSE, print.no = FALSE, env = env_fun_get_message) # for recovering warnings printed by ggplot() functions
        # message_recov <- fun_get_message('print(eval(parse(text = paste(paste(paste0(tempo_gg_name, 1:tempo_gg_count), collapse = " + "), if(base::is.null(add)){NULL}else{add}))))', kind = "message", header = FALSE, print.no = FALSE, env = env_fun_get_message) # for recovering messages printed by ggplot() functions
        # if( ! (return == TRUE & return_ggplot == TRUE)){ # because return() plots when return_ggplot is TRUE # finally not used -> see return_ggplot description
        if(base::is.null(x = legend_width)){
            grob_save <- base::suppressMessages(expr = base::suppressWarnings(expr = gridExtra::grid.arrange(fin_plot, newpage = TRUE), classes = "warning"), classes = "message")
        }else{
            grob_save <-base::suppressMessages(expr = base::suppressWarnings(expr = gridExtra::grid.arrange(fin_plot, legend_final, ncol=2, widths=base::c(1, legend_width), newpage = TRUE), classes = "warning"), classes = "message")
        }
        # }
        # suppressMessages(suppressWarnings(print(eval(parse(text = paste(paste(paste0(tempo_gg_name, 1:tempo_gg_count), collapse = " + "), if(base::is.null(add)){NULL}else{add}))))))
    }else{
        # following lines inactivated because of problem in warn_recov and message_recov
        # message_recov <- NULL
        # warn_recov <- NULL
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
    
    # output
    # following lines inactivated because of problem in warn_recov and message_recov
    # if( ! (base::is.null(warn) & base::is.null(warn_recov) & base::is.null(message_recov))){
    # warn <- paste0(warn, "\n\n", if(length(warn_recov) > 0 | length(message_recov) > 0){paste0(paste0("MESSAGES FROM ggplot2 FUNCTIONS: ", base::ifelse( ! base::is.null(warn_recov), unique(message_recov), ""), base::ifelse( ! base::is.null(message_recov), unique(message_recov), ""), collapse = "\n\n"), "\n\n")})
    # }else if( ! (base::is.null(warn) & base::is.null(warn_recov)) & base::is.null(message_recov)){
    # warn <- paste0(warn, "\n\n", if(length(warn_recov) > 0){paste0(paste0("MESSAGES FROM ggplot2 FUNCTIONS: ", unique(warn_recov), collapse = "\n\n"), "\n\n")})
    # }else if( ! (base::is.null(warn) & base::is.null(message_recov)) & base::is.null(warn_recov)){
    # warn <- paste0(warn, "\n\n", if(length(message_recov) > 0){paste0(paste0("MESSAGES FROM ggplot2 FUNCTIONS: ", unique(message_recov), collapse = "\n\n"), "\n\n")})
    # }
    if(return == TRUE){
        tempo_output <- ggplot2::ggplot_build(plot = fin_plot)
        tempo_output$data <- tempo_output$data[base::seq_len(length.out = base::length(x = tempo_output$data) - base::length(x = add_layers))] # remove the last data that correspond to the layers of add
        tempo_output$data <- tempo_output$data[-1] # remove the first data because corresponds to the initial empty boxplot
        if(base::length(x = tempo_output$data) != base::length(x = coord_names)){
            # nocov start (unreachable: internal consistency guard - coord_names is built in parallel with the layers)
            tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nlength(tempo_output$data) AND length(coord_names) MUST BE IDENTICAL. CODE HAS TO BE MODIFIED", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
            # nocov end
        }else{
            base::names(x = tempo_output$data) <- coord_names
            tempo_output$data <- tempo_output$data[coord_names != "bad_remove"]
        }
        tempo <- tempo_output$layout$panel_params[[1]]
        output <- base::list(
            data = data1_ini, 
            stat = stat_nolog, 
            removed_row_nb = removed_row_nb, 
            removed_rows = removed_rows, 
            plot = base::c(tempo_output$data, y_second_tick_values = base::list(y_second_tick_values)), 
            panel = facet_categ, 
            axes = base::list(
                x.range = tempo$x.range, 
                x.labels = if(base::is.null(x = base::attributes(x = tempo$x$breaks))){tempo$x$breaks}else{tempo$x$scale$get_labels()}, # is.null(attributes(tempo$x$breaks)) test if it is number (TRUE) or character (FALSE)
                x.positions = if(base::is.null(x = base::attributes(x = tempo$x$breaks))){tempo$x$breaks}else{base::unlist(x = base::attributes(x = tempo$x$breaks), recursive = TRUE, use.names = TRUE)}, 
                y.range = tempo$y.range, 
                y.labels = if(base::is.null(x = base::attributes(x = tempo$y$breaks))){tempo$y$breaks}else{tempo$y$scale$get_labels()}, 
                y.positions = if(base::is.null(x = base::attributes(x = tempo$y$breaks))){tempo$y$breaks}else{base::unlist(x = base::attributes(x = tempo$y$breaks), recursive = TRUE, use.names = TRUE)}
            ), 
            warn = base::paste0("\n", warn, "\n\n", collapse = NULL, recycle0 = FALSE), 
            ggplot = if(return_ggplot == TRUE){fin_plot}else{NULL}, # fin_plot plots the graph if return == TRUE
            gtable = if(return_gtable == TRUE){grob_save}else{NULL} 
        )
        #### output
        base::return(output)
        #### end output
    }

}




