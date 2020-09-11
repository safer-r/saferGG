# EXAMPLES

### Data set
set.seed(1)
# Curves
obs1 <- data.frame(
    Km = 2:7, 
    Time = (2:7)^2, 
    Car = c("TUUT", "TUUT", "TUUT", "WIM-WIM", "WIM-WIM", "WIM-WIM"), 
    Color1 = rep(c("coral", "lightblue"), each = 3), 
    stringsAsFactors = TRUE
)
# First scattering
obs2 <- data.frame(
    Km = rnorm(1000, 20, 3), 
    Time = rnorm(1000, 20, 3), 
    Animal = rep(c("CAT", "DOG"), 500), 
    Color2 = rep(c("darkblue", "darkred"), times = 500), 
    stringsAsFactors = TRUE
)
# Second scattering
obs3 <- data.frame(
    Distance = rnorm(1000, 30, 3), 
    Time_lapse = rnorm(1000, 30, 3), 
    Beast = rep(c("LION", "ZEBRA"), 500), 
    Color3 = rep(3:4, times = 500), 
    stringsAsFactors = TRUE
)
set.seed(NULL)
fun_info(obs1)
fun_info(obs2)
fun_info(obs3)
fun_open(pdf = FALSE, width = 5, height = 5, rescale = "R")









## Mandatory arguments
### single dataset
fun_gg_scatter(data1 = obs1, x = "Km", y = "Time")
### single dataset submitted as list
fun_gg_scatter(
    data1 = list(obs1), 
    x = list("Km"), 
    y = list("Time")
)
### multiple dataset
# Elements in list have names (L1, L2, etc.) just to show the correspondence between the arguments data1, x, y, categ, etc.
# Of note, no legend is displayed. For a legend, please merge the two data frames, or create a new categ column in each data frame (see below)
fun_gg_scatter(
    data1 = list(
        L1 = obs2,
        L2 = obs3
    ), 
    x = list(
        L1 = "Km",
        L2 = "Distance"
    ), 
    y = list(
        L1 = "Time",
        L2 = "Time_lapse"
    )
)

### The categ argument to generate legends. Create a categ column in data frames if a legend is required
### single dataset
fun_gg_scatter(data1 = obs1, x = "Km", y = "Time", categ = "Car")
### multiple datasets
fun_gg_scatter(
    data1 = list(
        L1 = obs2,
        L2 = obs3
    ), 
    x = list(
        L1 = "Km",
        L2 = "Distance"
    ), 
    y = list(
        L1 = "Time",
        L2 = "Time_lapse"
    ), 
    categ = list(
        L1 = "Animal",
        L2 = "Beast"
    )
)
# Another way for multiple datasets representation, depending on legend required
obs4 <- data.frame(
    Km = c(obs2$Km, obs3$Distance), 
    Time = c(obs2$Time, obs3$Time_lapse), 
    Type = c(as.character(obs2$Animal), as.character(obs3$Beast)), 
    stringsAsFactors = TRUE
)
fun_info(obs4)
fun_gg_scatter(data1 = obs4, x = "Km", y = "Time", categ = "Type")


### Order in the legend
### single dataset
fun_gg_scatter(
    data1 = obs1, 
    x = "Km", 
    y = "Time", 
    categ = "Car", 
    categ.class.order = c("WIM-WIM", "TUUT")
)
### multiple datasets
fun_gg_scatter(
    data1 = list(
        L1 = obs2,
        L2 = obs3
    ), 
    x = list(
        L1 = "Km",
        L2 = "Distance"
    ), 
    y = list(
        L1 = "Time",
        L2 = "Time_lapse"
    ), 
    categ = list(
        L1 = "Animal",
        L2 = "Beast"
    ), 
    categ.class.order = list(
        c("DOG", "CAT"),
        c("ZEBRA", "LION")
    )
)



### Colors
### single dataset
# Using a single color value
fun_gg_scatter(data1 = obs1, x = "Km", y = "Time", color = "black") # replace "black" by 2 to test integer values
# Using one color value par class of Categ1
fun_gg_scatter(
    data1 = obs1, 
    x = "Km", 
    y = "Time", 
    categ = "Car", # mandatory categ argument when several color required
    color = c("darkblue", "darkgreen") # replace c("darkblue", "darkgreen") by 2:3 to test integer values
)
# Using a vector of color values (e.g., data frame column), with respect of the correspondence between Car and Color1 columns
fun_gg_scatter(
    data1 = obs1, 
    x = "Km", 
    y = "Time", 
    categ = "Car", # mandatory categ argument when several color required
    color = obs1$Color1 # # replace obs1$Color1 by as.numeric(obs1$Color1) to test integer values
)
### multiple datasets
# single color -> same color for all the datasets and all the classes of categ if non NULL
fun_gg_scatter(
    data1 = list(
        L1 = obs2,
        L2 = obs3
    ), 
    x = list(
        L1 = "Km",
        L2 = "Distance"
    ), 
    y = list(
        L1 = "Time",
        L2 = "Time_lapse"
    ), 
    categ = list( # mandatory categ argument when several color required per dataset
        L1 = "Animal",
        L2 = "Beast"
    ), 
    color = "darkblue" # replace "darkblue" by 2 to test integer values
)

# list of single colors -> each dataset with the same color
fun_gg_scatter(
    data1 = list(
        L1 = obs2,
        L2 = obs3
    ), 
    x = list(
        L1 = "Km",
        L2 = "Distance"
    ), 
    y = list(
        L1 = "Time",
        L2 = "Time_lapse"
    ), 
    categ = list( # mandatory categ argument when several color required per dataset
        L1 = "Animal",
        L2 = "Beast"
    ), 
    color = list(
        L1 = "darkblue", # replace "darkblue" by 2 to test integer values
        L2 = "darkgreen" # replace "darkgreen" by 3 to test integer values
    )
)
# list of single colors for each class of the categ argument
fun_gg_scatter(
    data1 = list(
        L1 = obs2,
        L2 = obs3
    ), 
    x = list(
        L1 = "Km",
        L2 = "Distance"
    ), 
    y = list(
        L1 = "Time",
        L2 = "Time_lapse"
    ), 
    categ = list( # mandatory categ argument when several color required per dataset
        L1 = "Animal",
        L2 = "Beast"
    ), 
    color = list(
        L1 = c("darkblue", "darkred"), # replace c("darkblue", "darkred") by 2:3 to test integer values
        L2 = c("darkgreen", "darkorange") # replace c("darkgreen", "darkviolet") by 4:5 to test integer values
    )
)
# list of vectors of color values (e.g., data frame column), with respect of the correspondence between the categorical and the color columns
fun_gg_scatter(
    data1 = list(
        L1 = obs2,
        L2 = obs3
    ), 
    x = list(
        L1 = "Km",
        L2 = "Distance"
    ), 
    y = list(
        L1 = "Time",
        L2 = "Time_lapse"
    ), 
    categ = list( # mandatory categ argument when several color required per dataset
        L1 = "Animal",
        L2 = "Beast"
    ), 
    color = list(
        L1 = obs2$Color2, # Replace by NULL to see that different levels of grey is used per dataset (not per class of the categ argument)
        L2 = obs3$Color3 # color of integer value can be used. Replace by NULL to see that different levels of grey is used per dataset (not per class of the categ argument)
    )
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











