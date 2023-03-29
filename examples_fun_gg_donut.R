# EXAMPLES

## Data set
obs1 <- data.frame(
    Km = c(20, 10, 1, 5), 
    Car = c("TUUT", "WIIM", "BIP", "WROUM"), 
    Color1 = c(1:3, NA), 
    Color2 = c("burlywood1", "cadetblue1", "coral", "darkmagenta"), 
    Color3 = c("#F8766D", "#00BA38", NA, "#619CFF"), 
    Country = c("FR", "UK", "US", NA), 
    stringsAsFactors = TRUE)
obs1
fun_open(pdf = FALSE, width = 5, height = 5)

## Mandatory arguments
# Note that the order is systematically in the decreasing order of frequencies, starting at the top and turning clockwise
fun_gg_donut(data1 = obs1, freq = "Km", categ = "Car")

## Color modification
# Default colors
fun_gg_donut(data1 = obs1, freq = "Km", categ = "Car",
    fill.palette = NULL,
    fill.color = NULL
)
# Using the fill.palette argument
fun_gg_donut(data1 = obs1, freq = "Km", categ = "Car",
    fill.palette = "BrBG",
    fill.color = NULL
)
# Using the fill.color argument (as positive integers). NA are white
fun_gg_donut(data1 = obs1, freq = "Km", categ = "Car",
    fill.palette = NULL,
    fill.color = obs1$Color1
)

# The fill.color argument overrides the fill.palette argument
fun_gg_donut(data1 = obs1, freq = "Km", categ = "Car",
    fill.palette = "BrBG",
    fill.color = obs1$Color1
)
# Using the fill.color argument (as elements from colors())
fun_gg_donut(data1 = obs1, freq = "Km", categ = "Car",
    fill.palette = NULL,
    fill.color = obs1$Color2
)
# Warning: the colors order in fill.color is the one according to the decreasing values of the freq argument. Thus, to have the correct association between obs1$Car and obs1$Color2, obs1 must be sorted
obs1 <- obs1[order(obs1$Km, decreasing = TRUE), ]
fun_gg_donut(data1 = obs1, freq = "Km", categ = "Car",
    fill.palette = NULL,
    fill.color = obs1$Color2
)
# Using the fill.color argument, as hexadecimal values
fun_gg_donut(data1 = obs1, freq = "Km", categ = "Car",
    fill.palette = NULL,
    fill.color = obs1$Color3
)

## hole of the donut
# hole being 25% of the radius of the donut
fun_gg_donut(data1 = obs1, freq = "Km", categ = "Car",
    hole.size = 0.25,
    hole.text.size = 8 # size in mm
)

## border of slices
# positive integer
fun_gg_donut(data1 = obs1, freq = "Km", categ = "Car",
    border.color = 2, 
    border.size = 2 # in mm
)
# element of colors()
fun_gg_donut(data1 = obs1, freq = "Km", categ = "Car",
    border.color = "green", 
    border.size = 2 # in mm
)
# hexadecimal value
fun_gg_donut(data1 = obs1, freq = "Km", categ = "Car",
    border.color = "#619CFF", 
    border.size = 2 # in mm
)

## title of the plot
# hole being 25% of the radius of the donut
fun_gg_donut(data1 = obs1, freq = "Km", categ = "Car",
    title = "DONUT", 
    title.text.size = 20, 
)

## Annotation of the slices
fun_gg_donut(data1 = obs1, freq = "Km", categ = "Car",
    annotation = "Country",
    annotation.distance = 1, # annotation.distance = 0 means center of the slice, 0.5 means at the edge. Above 0.5, the donut will be reduced to make place for the annotation
    annotation.size = 5, # size of the texte in mm
    annotation.force = 0, # force of repulsion between overlapping text labels
    annotation.force.pull = 0, # force of attraction between a text label and its corresponding data point
)

## Management of the legend area
fun_gg_donut(data1 = obs1, freq = "Km", categ = "Car", 
    legend.show = TRUE, # FALSE remove the legend, not the area of the legend
    legend.width = 0.5, # between 0 (no area for the legend) to 1 (half the device width for the legend area)
    legend.name = "Noise", # legend title added
    legend.limit = NULL, # all the slices lower that 0.2 in proportion are not displayed in the legend
    legend.add.prop = TRUE # proportion after the class names added
)

## the add argument
fun_gg_donut(data1 = obs1, freq = "Km", categ = "Car", 
               add = "+ggplot2::theme_gray()"
)
fun_gg_donut(data1 = obs1, freq = "Km", categ = "Car", 
               add = "+ggplot2::facet_wrap(facets = 'Country', labeller = 'label_both') + ggplot2::theme(strip.background = ggplot2::element_rect(color = 'grey', size = 0.5), strip.text = ggplot2::element_text(size = 10, face = 'bold'), panel.spacing = ggplot2::unit(0.5, 'lines'))"  # or ggplot2::vars(Country) instead of 'Country'. See https://ggplot2.tidyverse.org/reference/labeller.html
)


## Other parameters
res <- fun_gg_donut(data1 = obs1, freq = "Km", categ = "Car", 
                      return = TRUE, # output returned and assigned into res. If FALSE, res is NULL
                      return.ggplot = TRUE, # ggplot object added in res (without the legend
                      return.gtable = TRUE, # ggplot object as gtable of grobs in res ($gtable is NULL if plot = FALSE)
                      plot = TRUE, # plot displayed during asignation into res
                      warn.print = TRUE, 
                      lib.path = NULL
)
# plot the result
fun_open(pdf = FALSE)
res$ggplot
# The advantage of $ggplot is that it is easy to update the plot
res$ggplot + ggplot2::annotate(geom = "text", x = -0.5, y = 0, label = "NOT GOOD", size = 20, angle = 45)
# However, manipulation of res triggers plotting when $ggplot is not NULL (NULL $ggplot obtained using return.ggplot = FALSE), which is annoying, as explain in the function description. Thus, it is preferable to use return.ggplot = FALSE


## Notes about the gtable output
res2 <- fun_gg_donut(data1 = obs1, freq = "Km", categ = "Car", 
                       return = TRUE, 
                       return.ggplot = FALSE,
                       return.gtable = TRUE, # return.gtable must be TRUE to have a non NULL $gtable output into res2
                       plot = TRUE, # plot must be TRUE to have a non NULL $gtable output into res2
                       warn.print = FALSE, 
                       lib.path = NULL
)

# display the results (does not plot the graph, contrary to $ggplot)
fun_open(pdf = FALSE)
res2

# replot
fun_open(pdf = FALSE)
gridExtra::grid.arrange(res2$gtable) # Contrary to $ggplot, $gtable cannot be easily updated (see https://stackoverflow.com/questions/26499608/inverse-of-ggplotgrob)
# plot the first grob
fun_open(pdf = FALSE)
gridExtra::grid.arrange(res2$gtable[1,1])
# plot the second grob
fun_open(pdf = FALSE)
gridExtra::grid.arrange(res2$gtable[1,2])


## All the arguments
fun_gg_donut(
    data1 = obs1, 
    freq = "Km", 
    categ = "Car", 
    fill.palette = NULL,
    fill.color = NULL, 
    hole.size = 0.5, 
    hole.text.size = 14, 
    border.color = "gray50", 
    border.size = 0.2, 
    title = "", 
    title.text.size = 12, 
    annotation = NULL,
    annotation.distance = 0,
    annotation.size = 3,
    annotation.force = 1,
    annotation.force.pull = 100,
    legend.show = TRUE, 
    legend.width = 0.25, 
    legend.name = NULL, 
    legend.limit = NULL, 
    legend.add.prop = FALSE,
    add = NULL, 
    return = FALSE, 
    return.ggplot = FALSE,
    return.gtable = TRUE,
    plot = TRUE, 
    warn.print = FALSE, 
    lib.path = NULL
)





