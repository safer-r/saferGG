# EXAMPLES

### Data set
set.seed(1)
# straight relationship
obs1 <- data.frame(
	Km = 2:7, 
	Time = (2:7)^2, 
    Car = c("TUTUT", "TUTUT", "TUTUT", "WIM-WIM", "WIM-WIM", "WIM-WIM"), 
    Color1 = rep(c("coral", "lightblue"), each = 3), 
    stringsAsFactors = TRUE
)
# first scatter
obs2 <- data.frame(
	Km = rnorm(1000, 20, 3), 
	Time = rnorm(1000, 20, 3), 
    Animal = rep(c("CAT", "DOG"), 500), 
    Color1 = rep(c("coral", "lightblue"), times = 500), 
    stringsAsFactors = TRUE
)
# second scatter
obs3 <- data.frame(
	Km = rnorm(1000, 30, 3), 
	Time = rnorm(1000, 30, 3), 
    Animal = rep(c("LION", "ZEBRA"), 500), 
    Color1 = rep(1:2, times = 500), 
    stringsAsFactors = TRUE
)
set.seed(NULL)
fun_info(obs1)
fun_info(obs2)
fun_info(obs3)

## Mandatory arguments
### single dataset
fun_gg_scatter(
	data1 = obs1, 
	x = "Km", 
	y = "Time"
)
### single dataset submitted as list
fun_gg_scatter(
	data1 = list(obs1), 
	x = list("Km"), 
	y = list("Time")
)
# multiple dataset. Elements in list have names (L1, L2, etc.) just to show the correspondence between the arguments data1, x, y, categ, etc.
fun_gg_scatter(
    data1 = list(
        L1 = obs2,
        L2 = obs3
    ), 
    x = list(
        L1 = "Km",
        L2 = "Km"
    ), 
    y = list(
        L1 = "Time",
        L2 = "Time"
    )
)







### Changing the order of the boxes
# separate boxes
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", categ.class.order = list(c("DOG", "CAT")))
# grouped boxes
fun_gg_boxplot(data1 = obs1, y = "Time", categ = c("Categ1", "Categ2"), categ.class.order = list(c("DOG", "CAT"), c("D", "C", "B", "A")))

### Box color
# Using a single color value
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", categ.color = "coral")
# Using one color value par class of Categ1
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", categ.color = c("coral", "lightblue"))
# Using a vector of color values (e.g., data frame column), with respect of the correspondence between Categ1 and box.color columns
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", categ.color = obs1$Color1)
# Using integers instead of color strings
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", categ.color = 1)
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", categ.color = 1:2)
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", categ.color = as.numeric(obs1$Color1))
# With grouped boxes, we generate the same effects but for the second category
fun_gg_boxplot(data1 = obs1, y = "Time", categ = c("Categ1", "Categ2"), categ.color = "coral")
fun_gg_boxplot(data1 = obs1, y = "Time", categ = c("Categ1", "Categ2"), categ.color = 1:4)
fun_gg_boxplot(data1 = obs1, y = "Time", categ = c("Categ1", "Categ2"), categ.color = obs1$Color2)

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

### Dot colors
# Dot removal
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", dot.color = NULL)
# Same color as the boxes
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", dot.color = "same")
# Single color sting
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", dot.color = "green") # a single integer also works
# Same number of Categ1 classes
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", dot.color = c("green", "brown")) # test also 1:2 (result is idem as "same")
# Using a vector of color values of the same length as the number of rows in data1 (e.g., data frame column). No correspondence with Categ1 classes is required
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", dot.color = 1:nrow(obs1))
# With grouped boxes, we generate the same effects but for the second category
fun_gg_boxplot(data1 = obs1, y = "Time", categ = c("Categ1", "Categ2"), dot.color = NULL)
fun_gg_boxplot(data1 = obs1, y = "Time", categ = c("Categ1", "Categ2"), dot.color = "same")
fun_gg_boxplot(data1 = obs1, y = "Time", categ = c("Categ1", "Categ2"), dot.color = "green") # a single integer also works
fun_gg_boxplot(data1 = obs1, y = "Time", categ = c("Categ1", "Categ2"), dot.color = c("green", "brown", "red", "blue")) # test also 1:2 (result is idem as "same")
fun_gg_boxplot(data1 = obs1, y = "Time", categ = c("Categ1", "Categ2"), dot.color = 1:nrow(obs1))

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
dot.jitter = 0.5 # from 0 to 1. Only if dot.tidy = FALSE
)

### Other parameters of dots
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
dot.categ = "Categ2", # to see the dot legend
dot.color = c("green", "brown", "red", "blue"), 
dot.size = 5, # ignored if dot.tidy = TRUE
dot.alpha = 0.3, 
dot.border.size = 2, 
dot.border.color = "green", 
)

### X-axis parameter
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
x.lab = "ANIMALS" 
)

### Y-axis parameter
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
stat.disp = "above", # try "top"
stat.disp.mean = FALSE, 
stat.size = 4, 
stat.dist = 2
)

### Plot orientation
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
vertical = FALSE # with log2 and log10 scales, horizontal orientation is blocked because of a bug in ggplot2 (https://github.com/tidyverse/ggplot2/issues/881)
)


### Text management
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
text.size = 20, 
text.angle = 90
)

### Title
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
title = "FIRST EXPERIMENT", 
title.text.size = 20
)

### Management of the legend area
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
legend.show = FALSE, # remove the legend, not the area of the legend
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
)


### Other parameters
fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Categ1", 
return = TRUE, 
return.ggplot = TRUE,
plot = FALSE, 
warn.print = FALSE, 
lib.path = NULL
)











