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

## The return and plot arguments
fun_gg_donut(data1 = obs1, freq = "Km", categ = "Car", 
    return = FALSE,
    plot = FALSE,
)
# nothing is returned and nothing is plotted
res <- fun_gg_donut(data1 = obs1, freq = "Km", categ = "Car", 
    return = TRUE, # output returned and assigned into res. If FALSE, res is NULL
    plot = TRUE, # plot displayed during assignation into res
)
res # info of the plot are stored in the res object. The plot is also displayed during the assignation into res (plot = TRUE) 

## The return.ggplot argument (return argument must be TRUE)
res2 <- fun_gg_donut(data1 = obs1, freq = "Km", categ = "Car", 
    return = TRUE,
    return.ggplot = TRUE,
    plot = FALSE
)
# Nothing plotted during the assignation into res2
res2 # the pain with return.ggplot = TRUE is that each time res2 is called, a plot appears (because res2$ggplot is not NULL, as when return.ggplot = FALSE is used)
res2$panel # however, calling a element of res2 does not plot the graph
res2$ggplot #except when using $ggplot
# The advantage of $ggplot is that it is easy to update the plot
res$ggplot + ggplot2::annotate(geom = "text", x = -0.5, y = 0, label = "NOT GOOD", size = 20, angle = 45)

## The return.gtable argument (return argument must be TRUE)
res3 <- fun_gg_donut(data1 = obs1, freq = "Km", categ = "Car", 
    title = "DONUT",
    return = TRUE,
    return.gtable = TRUE, # gtable of the full graph
    plot = FALSE
)
# Nothing plotted during the assignation into res3
res3 # no plotting, contrary to with return.ggplot = TRUE
# replotting
gridExtra::grid.arrange(res3$gtable) # full graph
# But contrary to $ggplot, $gtable cannot be easily updated (see https://stackoverflow.com/questions/26499608/inverse-of-ggplotgrob)
# plot the first grob
gridExtra::grid.arrange(res3$gtable[ ,1]) # made of the main plot + title
# plot the second grob
gridExtra::grid.arrange(res3$gtable[ ,2]) # legend
# plot the main plot without the title
gridExtra::grid.arrange(res3$gtable[,1]$grob[[1]][2])
#plot the title
gridExtra::grid.arrange(res3$gtable[,1]$grob[[1]][1])

## The warn.print argument
fun_gg_donut(data1 = obs1, freq = "Km", categ = "Country", 
    warn.print = TRUE
)
# Warning messages shown
res4 <- fun_gg_donut(data1 = obs1, freq = "Km", categ = "Country", 
    return = TRUE,
    warn.print = FALSE
)
# Warning messages not shown
cat(res4$warn) # but can be recover this way using return = TRUE

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





