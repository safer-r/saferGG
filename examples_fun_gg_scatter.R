# EXAMPLES

### Data set
set.seed(1)
# Line
obs1 <- data.frame(
    Km = c(2, 1, 6, 5, 4, 7), 
    Time = c(2, 1, 6, 5, 4, 7)^2, 
    Car = c("TUUT", "TUUT", "TUUT", "WIIM", "WIIM", "WIIM"), 
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
# obs4 assembles ob2 and obs3
obs4 <- data.frame(
    Km = c(obs2$Km, obs3$Distance), 
    Time = c(obs2$Time, obs3$Time_lapse), 
    Type = c(as.character(obs2$Animal), as.character(obs3$Beast)), 
    stringsAsFactors = TRUE
)
fun_info(obs4)
set.seed(NULL)
fun_info(obs1)
fun_info(obs2)
fun_info(obs3)
fun_open(pdf.disp = FALSE, width = 5, height = 5)









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
# Elements in list are named L1, L2, etc., just to show the correspondence between the arguments data1, x, y, categ, etc.
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
fun_gg_scatter(data1 = obs1, x = "Km", y = "Time", 
    categ = "Car"
)
### multiple datasets
fun_gg_scatter(data1 = list(obs2, obs3), x = list("Km", "Distance"), y = list("Time", "Time_lapse"), 
    categ = list(
        L1 = "Animal",
        L2 = "Beast"
    )
)
# Another way for multiple datasets representation, depending on legend required
fun_gg_scatter(data1 = obs4, x = "Km", y = "Time",
    categ = "Type"
)


### Order of layers in the legend (multiple datasets)
# order of layer depends on the data1 order (obs3 before obs2, and then all the associated parameters in the same order in the arguments)
fun_gg_scatter(
    data1 = list(
        L1 = obs3,
        L2 = obs2
    ), 
    x = list(
        L1 = "Distance",
        L2 = "Km"
    ), 
    y = list(
        L1 = "Time_lapse",
        L2 = "Time"
    ), 
    categ = list(
        L1 = "Beast",
        L2 = "Animal"
    )
)

### Order of classes in each layer
### single dataset
fun_gg_scatter(data1 = obs1, x = "Km", y = "Time", categ = "Car", 
    categ.class.order = c("WIIM", "TUUT")
)
### multiple datasets
fun_gg_scatter(data1 = list(obs2, obs3), x = list("Km", "Distance"), y = list("Time", "Time_lapse"), categ = list("Animal", "Beast"), 
    categ.class.order = list(
        L1 = c("DOG", "CAT"),
        L2 = c("ZEBRA", "LION")
    )
)



### Colors
### single dataset
# Using a single color value
fun_gg_scatter(data1 = obs1, x = "Km", y = "Time", categ = "Car", 
    color = "black"
) # replace "black" by 2 to test integer values
# Using one color value par class of Categ1
fun_gg_scatter(data1 = obs1, x = "Km", y = "Time", categ = "Car", # mandatory categ argument when several color required
    color = c("darkblue", "darkgreen") # replace c("darkblue", "darkgreen") by 2:3 to test integer values
)
# Using a vector of color values (e.g., data frame column), with respect of the correspondence between Car and Color1 columns
fun_gg_scatter(data1 = obs1, x = "Km", y = "Time", categ = "Car",  # replace "black" by 2 to test integer values
    color = obs1$Color1 # # replace obs1$Color1 by as.numeric(obs1$Color1) to test integer values
)
### multiple datasets
# single color -> same color for all the datasets and all the classes of categ if non NULL
fun_gg_scatter(data1 = list(obs2, obs3), x = list("Km", "Distance"), y = list("Time", "Time_lapse"), categ = list("Animal", "Beast"), 
    color = "darkblue" # replace "darkblue" by 2 to test integer values
)

# list of single colors -> each dataset with the same color
fun_gg_scatter(data1 = list(obs2, obs3), x = list("Km", "Distance"), y = list("Time", "Time_lapse"), categ = list("Animal", "Beast"), 
    color = list(
        L1 = "darkblue", # replace "darkblue" by 2 to test integer values
        L2 = "darkgreen" # replace "darkgreen" by 3 to test integer values
    )
)
# list of single colors for each class of the categ argument
fun_gg_scatter(data1 = list(obs2, obs3), x = list("Km", "Distance"), y = list("Time", "Time_lapse"), categ = list("Animal", "Beast"), 
    color = list(
        L1 = c("darkblue", "darkred"), # replace c("darkblue", "darkred") by 2:3 to test integer values
        L2 = c("darkgreen", "darkorange") # replace c("darkgreen", "darkviolet") by 4:5 to test integer values
    )
)
# list of vectors of color values (e.g., data frame column), with respect of the correspondence between the categorical and the color columns
fun_gg_scatter(data1 = list(obs2, obs3), x = list("Km", "Distance"), y = list("Time", "Time_lapse"), categ = list("Animal", "Beast"), 
    color = list(
        L1 = obs2$Color2, # Replace by NULL to see that different levels of grey is used per dataset (not per class of the categ argument)
        L2 = obs3$Color3 # color of integer value can be used. Replace by NULL to see that different levels of grey is used per dataset (not per class of the categ argument)
    )
)


### geometry
### single dataset
# scatterplot
fun_gg_scatter(data1 = obs1, x = "Km", y = "Time", categ = "Car", 
    geom = "geom_point"
)
# line: coordinates plotted then line connection, from the lowest to highest x coordinates first and from the lowest to highest y coordinates thenafter
fun_gg_scatter(data1 = obs1, x = "Km", y = "Time", categ = "Car", 
    geom = "geom_line"
)
# line: coordinates plotted then line connection respecting the row order in data1
fun_gg_scatter(data1 = obs1, x = "Km", y = "Time", categ = "Car", 
    geom = "geom_path"
)
# step: coordinates plotted then line connection respecting the row order in data1 but drawn in steps
fun_gg_scatter(data1 = obs1, x = "Km", y = "Time", categ = "Car", 
    geom = "geom_step",
    geom.step.dir = "vh" # test "vh" (vertical then horizontal), "hv" (horizontal then vertical) and "mid" (step half-way between adjacent x-values)
)
# horizontal line
fun_gg_scatter(data1 = obs1[c(1, 4), ], y = "Time", categ = "Car", # c(1, 4) because a single value of y per class of categ. Otherwise use data1 = obs1, x = "Km", categ = NULL
    x = NULL, # must be NULL for geom_hline
    geom = "geom_hline",
    x.lim = c(0, 1) # mandatory because no x-axis limits in the dataset
)
# vertical line
fun_gg_scatter(data1 = obs1[c(1, 4), ], x = "Km", categ = "Car", # c(1, 4) because a single value of x per class of categ. Otherwise use data1 = obs1, x = "Km", categ = NULL
    y = NULL, # must be NULL for geom_hline
    geom = "geom_vline", 
    y.lim = c(0, 1) # mandatory because no y-axis limits in the dataset
)
# stick: dots as vertical bars
fun_gg_scatter(data1 = obs1, x = "Km", y = "Time", categ = "Car", 
    geom = "geom_stick", 
    geom.stick.base = 20 # set the base of the sticks when using "geom_stick" of the geom argument. if NULL, use the bottom of the y-axis
)
### multiple dataset
fun_gg_scatter(data1 = list(obs2, obs3), x = list("Km", "Distance"), y = list("Time", "Time_lapse"), categ = list("Animal", "Beast"), 
    geom = list(
        L1 = "geom_point", 
        L2 = "geom_line"
    )
)
### multiple dataset: complex example
fun_gg_scatter(
    data1 = list(
        L1 = obs1, 
        L2 = obs2, 
        L3 = obs3, 
        L4 = obs1[c(1, 4), ]
    ), 
    x = list(
        L1 = "Km", 
        L2 = "Km", 
        L3 = "Distance",
        L4 = NULL # because of L4 = "geom_hline" below
    ), 
    y = list(
        L1 = "Time", 
        L2 = "Time",
        L3 = "Time_lapse",
        L4 = "Time"
    ), 
    categ = list(
        L1 = "Car",
        L2 = "Animal", 
        L3 = "Beast",
        L4 = "Car"
    ), 
    geom = list( # no more than 3 "geom_point", and no more than 3 kind of lines
        L1 = "geom_point", 
        L2 = "geom_line", 
        L3 = "geom_step", 
        L4 = "geom_hline"
    ),
    geom.step.dir = "hv" # or geom.step.dir = list(L1 = NULL, L2 = NULL, L3 = "hv", L4 = NULL) # NULL or anything else, since only the 3rd compartment will be used, because only the 3rd compartment of geom is "geom_step"
)




### arguments of dots
### single dataset
fun_gg_scatter(data1 = obs1, x = "Km", y = "Time", categ = "Car", 
    alpha = 0.7, 
    dot.size = 5, # shape radius (?) in mm
    dot.shape = 22, # shape of the dots (see https://ggplot2.tidyverse.org/articles/ggplot2-specs.html)
    dot.border.size = 3, # border dot width in mm, use 0 for no border
    dot.border.color = "grey"
)
### multiple dataset
# single value. If one geom argument is not "geom_point" (default geom value), these arguments are ignored, except alpha (see below)
fun_gg_scatter(data1 = list(obs2, obs3), x = list("Km", "Distance"), y = list("Time", "Time_lapse"), categ = list("Animal", "Beast"), 
    alpha = 0.2, 
    dot.size = 5, # shape radius (?) in mm
    dot.shape = 22, # shape of the dots (see https://ggplot2.tidyverse.org/articles/ggplot2-specs.html)
    dot.border.size = 3, # border dot width in mm, use 0 for no border
    dot.border.color = 7 # Integer is allowed
)
# list of values. If one geom argument is not "geom_point" (default geom value), the corresponding compartments of these arguments are ignored, except alpha (see below)
fun_gg_scatter(data1 = list(obs2, obs3), x = list("Km", "Distance"), y = list("Time", "Time_lapse"), categ = list("Animal", "Beast"), 
    alpha = list(0.2, 0.7), 
    dot.size = list(4, 5), # shape radius (?) in mm
    dot.shape = list(21, 22), # shape of the dots (see https://ggplot2.tidyverse.org/articles/ggplot2-specs.html)
    dot.border.size = list(4, 3), # border dot width in mm, use 0 for no border
    dot.border.color = "grey" # no list here -> same border color whatever the shapes
)



### arguments of lines
### single dataset
fun_gg_scatter(data1 = obs1, x = "Km", y = "Time", categ = "Car", geom = "geom_line",
    alpha = 0.7, 
    line.size = 2, # line width in mm
    line.type = "dashed" # kind of lines (see https://ggplot2.tidyverse.org/articles/ggplot2-specs.html)
)
### multiple dataset
# single value. If one geom argument is not "geom_point" (default geom value), these arguments are ignored, except alpha (see below)
fun_gg_scatter(data1 = list(obs2, obs3), x = list("Km", "Distance"), y = list("Time", "Time_lapse"), categ = list("Animal", "Beast"), geom = list("geom_line", "geom_stick"), 
    alpha = 0.2, 
    line.size = 1, # line width in mm
    line.type = "solid" # kind of lines (see https://ggplot2.tidyverse.org/articles/ggplot2-specs.html)
)
# list of values. If one geom argument is not "geom_point" (default geom value), the corresponding compartments of these arguments are ignored, except alpha (see below)
fun_gg_scatter(data1 = list(obs2, obs3), x = list("Km", "Distance"), y = list("Time", "Time_lapse"), categ = list("Animal", "Beast"), geom = list("geom_line", "geom_stick"), 
    alpha = list(0.5, 0.05), 
    line.size = list(1, 3), # line width in mm
    line.type = list("81", "solid") # kind of lines (see https://ggplot2.tidyverse.org/articles/ggplot2-specs.html)
)

### arguments of x-axis
fun_gg_scatter(data1 = obs1, x = "Km", y = "Time", categ = "Car", 
    x.lim = c(1000, 0.1), # order matters
    x.lab = "SIZE", 
    x.log = "log10", # try "no"
    x.tick.nb = 10, # approximate number
    x.second.tick.nb = 2, 
    x.include.zero = FALSE, 
    x.left.extra.margin = 0, 
    x.right.extra.margin = 0, 
    x.text.angle = 45
)


### arguments of y-axis
fun_gg_scatter(data1 = obs1, x = "Km", y = "Time", categ = "Car", 
    y.lim = c(1000, 0.1), # order matters
    y.lab = "SIZE", 
    y.log = "log10", # try "no"
    y.tick.nb = 10, # approximate number
    y.second.tick.nb = 2, 
    y.include.zero = FALSE, 
    y.top.extra.margin = 0, 
    y.bottom.extra.margin = 0, 
    y.text.angle = 45
)

### raster plot (easier to display and export as it convert vectorial dot layer into matricial dot layer)
res <- fun_open(pdf = FALSE, width = 7, height = 4, return.output = TRUE)
fun_gg_scatter(data1 = obs1, x = "Km", y = "Time", categ = "Car", 
    raster = TRUE, 
    raster.ratio = res$dim[2] / res$dim[1], # test raster.ratio = 1 to see the distorsion
    raster.threshold = 1
)


### Text management
fun_gg_scatter(data1 = obs1, x = "Km", y = "Time", categ = "Car", 
text.size = 20
)

### Title
fun_gg_scatter(data1 = obs1, x = "Km", y = "Time", categ = "Car", 
title = "FIRST EXPERIMENT", 
title.text.size = 20
)

### Management of the legend area and legend titles
fun_gg_scatter(data1 = obs1, x = "Km", y = "Time", categ = "Car", 
legend.show = TRUE, # FALSE remove the legend, not the area of the legend
legend.width = 1, # between 0 (no area for the legend) to 1 (half the device width for the legend area). Use NULL for default management
legend.name = "CORVETTE" # if categ = "Car", then the legend name is "Car" by default. Use "" to remove the legend name
)

### Appearance
fun_gg_scatter(data1 = obs1, x = "Km", y = "Time", categ = "Car", 
article = FALSE, 
grid = TRUE
)

### the add argument
fun_gg_scatter(data1 = obs1, x = "Km", y = "Time", categ = "Car", 
add = "+ggplot2::theme_classic()"
)
fun_gg_scatter(data1 = obs1, x = "Km", y = "Time", categ = "Car", 
add = "+ggplot2::facet_wrap(facets = 'Car', labeller = 'label_both') + ggplot2::theme(strip.background = ggplot2::element_rect(color = 'grey', size = 0.5), strip.text = ggplot2::element_text(size = 10, face = 'bold'), panel.spacing = ggplot2::unit(0.5, 'lines'))"  # or ggplot2::vars(Car) instead of 'Car'. See https://ggplot2.tidyverse.org/reference/labeller.html
) # use legend.show = FALSE, legend.width = 0 to remove the legend area


### Other parameters
res <- fun_gg_scatter(data1 = obs1, x = "Km", y = "Time", categ = "Car", 
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
res2 <- fun_gg_scatter(data1 = obs1, x = "Km", y = "Time", categ = "Car", 
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
fun_gg_scatter(
data1 = obs1, 
x = "Km", 
y = "Time",
categ = NULL, 
categ.class.order = NULL, 
color = NULL, 
geom = "geom_point", 
geom.step.dir = "hv", 
geom.stick.base = NULL, 
alpha = 0.5, 
dot.size = 2, 
dot.shape = 21, 
dot.border.size = 0.5, 
dot.border.color = NULL, 
line.size = 0.5, 
line.type = "solid", 
x.lim = NULL, 
x.lab = NULL, 
x.log = "no", 
x.tick.nb = NULL, 
x.second.tick.nb = NULL, 
x.include.zero = FALSE, 
x.left.extra.margin = 0.05, 
x.right.extra.margin = 0.05, 
x.text.angle = 0, 
y.lim = NULL, 
y.lab = NULL, 
y.log = "no", 
y.tick.nb = NULL, 
y.second.tick.nb = NULL, 
y.include.zero = FALSE, 
y.top.extra.margin = 0.05, 
y.bottom.extra.margin = 0.05, 
y.text.angle = 0, 
raster = FALSE, 
raster.ratio = 1, 
raster.threshold = NULL, 
text.size = 12, 
title = "", 
title.text.size = 12, 
legend.show = TRUE, 
legend.width = 0.5, 
legend.name = NULL, 
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












