# EXAMPLES

### Data set
set.seed(1)
obs1 <- data.frame(
    Time = c(rnorm(20, 100, 10), rnorm(20, 200, 50), rnorm(20, 500, 60), rnorm(20, 100, 50)), 
    Categ1 = rep(c("CAT", "DOG"), times = 40), 
    Categ2 = rep(c("A", "B", "C", "D"), each = 20), 
    Color1 = rep(c("coral", "lightblue"), times = 40), 
    Color2 = rep(c("#9F2108", "#306100", "#007479", "#8500C0"), each = 20), 
    stringsAsFactors = TRUE
)
set.seed(NULL)
fun_info(obs1)

## Mandatory arguments
### separate boxes
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1")
### grouped boxes
fun_gg_boxplot(data1 = obs1, y = "Time", categ = c("Categ1", "Categ2"))
# Order matters
fun_gg_boxplot(data1 = obs1, y = "Time", categ = c("Categ2", "Categ1"))
### single class
fun_gg_boxplot(data1 = obs1[1:20, ], y = "Time", categ = "Categ2")
fun_gg_boxplot(data1 = obs1[1:20, ], y = "Time", categ = c("Categ1", "Categ2"))

### Changing the order of the boxes
# separate boxes
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
               categ.class.order = list(c("DOG", "CAT"))
)
# grouped boxes
fun_gg_boxplot(data1 = obs1, y = "Time", categ = c("Categ1", "Categ2"), 
               categ.class.order = list(c("DOG", "CAT"), c("D", "C", "B", "A"))
)

### Box color
# Using a single color value
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
               categ.color = "coral"
)
# Using one color value par class of Categ1
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
               categ.color = c("coral", "lightblue")
)
# Using a vector of color values (e.g., data frame column), with respect of the correspondence between Categ1 and box.color columns
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
               categ.color = obs1$Color1
)
# Using integers instead of color strings
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
               categ.color = 1
)
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
               categ.color = 1:2
)
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
               categ.color = as.numeric(obs1$Color1)
)
# With grouped boxes, we generate the same effects but for the second category
fun_gg_boxplot(data1 = obs1, y = "Time", categ = c("Categ1", "Categ2"), 
               categ.color = "coral"
)
fun_gg_boxplot(data1 = obs1, y = "Time", categ = c("Categ1", "Categ2"), 
               categ.color = 1:4
)
fun_gg_boxplot(data1 = obs1, y = "Time", categ = c("Categ1", "Categ2"), 
               categ.color = obs1$Color2
)

### Other parameters of boxes
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
               box.legend.name = "ANIMALS", 
               box.fill = TRUE, 
               box.width = 0.6, # separate boxes: between 0 (no box width <-> max boxes separation) and 1 (max box width <-> no boxes separation). Grouped boxes: between 0 (no group width <-> max group separation) and 1 (max group width <-> no group separation)
               box.space = 0, # between 0 (no separation) and 1 (max separation) but only to separate boxes inside groups of boxes
               box.line.size = 0.75, 
               box.notch = TRUE, 
               box.alpha = 1, 
               box.mean = FALSE, 
               box.whisker.kind = "max", 
               box.whisker.width = 0.5 # between 0 (no whisker extremities) and 1 (whisker extremities the width of the boxes)
)

### Box removal
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
               box.alpha = 0
)


### Dot colors
# Dot removal
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
               dot.color = NULL
)
# Same color as the boxes
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
               dot.color = "same"
)
# Single color sting
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
               dot.color = "green" # a single integer also works
)
# Same number of Categ1 classes
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
               dot.color = c("green", "brown") # test also 1:2 (result is idem as "same")
)
# Using a vector of color values of the same length as the number of rows in data1 (e.g., data frame column). No correspondence with Categ1 classes is required
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
               dot.color = 1:nrow(obs1)
)
# With grouped boxes, we generate the same effects but for the second category
fun_gg_boxplot(data1 = obs1, y = "Time", categ = c("Categ1", "Categ2"), 
               dot.color = NULL
)
fun_gg_boxplot(data1 = obs1, y = "Time", categ = c("Categ1", "Categ2"), 
               dot.color = "same"
)
fun_gg_boxplot(data1 = obs1, y = "Time", categ = c("Categ1", "Categ2"), 
               dot.color = "green" # a single integer also works
)
fun_gg_boxplot(data1 = obs1, y = "Time", categ = c("Categ1", "Categ2"), 
               dot.color = c("green", "brown", "red", "blue") # test also 1:2 (result is idem as "same")
)
fun_gg_boxplot(data1 = obs1, y = "Time", categ = c("Categ1", "Categ2"), 
               dot.color = 1:nrow(obs1)
)

### Legend for dots
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
               dot.color = c("green", "brown", "red", "blue"), # single color or same number of dot.categ classes in that case ("same" authorized if dot.categ is the last element of categ)
               dot.categ = "Categ2", 
               dot.categ.class.order = c("D", "A", "C", "B"), 
               dot.legend.name = "ANIMAL GROUP"
)

### Tidy or random dot spreading
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
               dot.tidy = TRUE, 
               dot.tidy.bin.nb = 50, # from 0 to Inf. Only if dot.tidy = TRUE
               dot.jitter = 0.5, # from 0 to 1. Only if dot.tidy = FALSE
               dot.seed = 55 # if the dot shuffling is not satisfying, test another integer.Ssee below an example with dot.seed = NULL
)

### Other parameters of dots
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
               dot.color = c("green", "brown", "red", "blue"), 
               dot.categ = "Categ2", # to see the dot legend
               dot.size = 5, # ignored if dot.tidy = TRUE
               dot.alpha = 0.3, 
               dot.border.size = 2, 
               dot.border.color = "green"
)

### Reshuffling of dots
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
               dot.seed = NULL # rerun several times, and test the same with dot.seed = 1 for instance 
)


### X-axis parameters
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
               x.lab = "ANIMALS", 
               x.angle = 90
)

### Y-axis parameters
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
               y.lab = "SIZE", 
               y.lim = c(1000, 0.1), # order matters
               y.log = "log10", # try "no"
               y.tick.nb = 10, # approximate number
               y.second.tick.nb = 2, 
               y.include.zero = FALSE, 
               y.top.extra.margin = 0, 
               y.bottom.extra.margin = 0, 
)

### Stat numbers above boxes
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
               stat.pos = "above", # try "top"
               stat.mean = FALSE, 
               stat.size = 4, 
               stat.dist = 2, 
               stat.angle = 90
)

### Plot orientation
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
               vertical = FALSE # with log2 and log10 scales, horizontal orientation is blocked because of a bug in ggplot2 (https://github.com/tidyverse/ggplot2/issues/881)
)


### Text management
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
               text.size = 20
)

### Title
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
               title = "FIRST EXPERIMENT", 
               title.text.size = 20
)

### Management of the legend area
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
               legend.show = FALSE, # FALSE remove the legend, not the area of the legend
               legend.width = 1 # between 0 (no area for the legend) to 1 (half the device width for the legend area). Use NULL for default management
)

### Appearance
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
               article = FALSE, 
               grid = TRUE
)

### the add argument
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
               add = "+ggplot2::theme_classic()"
)
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
               add = "+ggplot2::facet_wrap(facets = 'Categ2', labeller = 'label_both') + ggplot2::theme(strip.background = ggplot2::element_rect(color = 'grey', size = 0.5), strip.text = ggplot2::element_text(size = 10, face = 'bold'), panel.spacing = ggplot2::unit(0.5, 'lines'))"  # or ggplot2::vars(Categ2) instead of 'Categ2'. See https://ggplot2.tidyverse.org/reference/labeller.html
) # use legend.show = FALSE, legend.width = 0 to remove the legend area


### Other parameters
res <- fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
                      return = TRUE, # output returned (and assigned into res)
                      return.ggplot = TRUE,
                      return.gtable = FALSE,
                      plot = FALSE, # no plot displayed
                      warn.print = FALSE, 
                      lib.path = NULL
)
# plot the result
fun_open(pdf = FALSE)
res$ggplot

# The advantage of $ggplot is that it is easy to update the plot
res$ggplot + ggplot2::annotate(geom = "text", x = 1.5, y = 400, label = "NOT GOOD", size = 20, angle = 45)

# However, manipulation of res triggers plotting because of the presence of non NULL $ggplot, which is annoying, as explain in the function description. Thus, it is preferable to use return.ggplot = FALSE
fun_open(pdf = FALSE)
res




### Notes about the gtable output
res2 <- fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
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


### All the arguments
fun_gg_boxplot(
    data1 = obs1, 
    y = "Time", 
    categ = "Categ1", 
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
)





