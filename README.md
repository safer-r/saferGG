[//]: # "#to make links in gitlab: example with racon https://github.com/isovic/racon"
[//]: # "tricks in markdown: https://openclassrooms.com/fr/courses/1304236-redigez-en-markdown"

| usage | R dependencies |
| --- | --- |
| [![R Version](https://img.shields.io/badge/code-R-blue?style=plastic)](https://cran.r-project.org/mirrors.html) | [![Dependencies: R Version](https://img.shields.io/badge/R-v4.0.2-blue?style=plastic)](https://cran.r-project.org/mirrors.html) |
| [![License: GPL-3.0](https://img.shields.io/badge/licence-GPL%20(%3E%3D3)-green?style=plastic)](https://www.gnu.org/licenses) | [![Dependencies: R Package](https://img.shields.io/badge/package-Cairo%20v1.5_12.2-blue?style=plastic)](https://cran.r-project.org/web/packages/Cairo/index.html) |
|  | [![Dependencies: R Package](https://img.shields.io/badge/package-ggplot2%20v3.3.2-blue?style=plastic)](https://github.com/tidyverse/ggplot2) |
|  | [![Dependencies: R Package](https://img.shields.io/badge/package-lubridate%20v1.7.9-blue?style=plastic)](https://github.com/tidyverse/lubridate) |
|  | [![Dependencies: R Package](https://img.shields.io/badge/package-pdftools%20v2.3.1-blue?style=plastic)](https://cran.r-project.org/web/packages/pdftools/index.html) |
|  | [![Dependencies: R Package](https://img.shields.io/badge/package-reshape2%20v1.4.4-blue?style=plastic)](https://cran.r-project.org/web/packages/reshape2/index.html) |
|  | [![Dependencies: R Package](https://img.shields.io/badge/package-reticulate%20v1.16-blue?style=plastic)](https://cran.r-project.org/web/packages/reticulate/index.html)|
|  | [![Dependencies: R Package](https://img.shields.io/badge/package-scales%20v1.1.1-blue?style=plastic)](https://cran.r-project.org/web/packages/scales/index.html) |


## TABLE OF CONTENTS

   - [AIM](#aim)
   - [REPOSITORY CONTENT](#repository-content)
   - [DESCRIPTIONS OF THE FUNCTIONS](#description-of-the-functions)
   - [HOW TO HOW TO LOAD AND MANIPULATE](#how-to-load-and-manipulate)
   - [VERSIONS](#versions)
   - [LICENCE](#licence)
   - [CITATION](#citation)
   - [CREDITS](#credits)
   - [ACKNOWLEDGEMENTS](#Acknowledgements)
   - [WHAT'S NEW IN](#what's-new-in)


## AIM


Set of functions that facilitate basic procedures in 1) object analysis, 2) object modification, 3) graphic handling and 4) log file management.

The present repository of Cute Little R functions is for beta testing. Ultimately, functions will be provided as packages


## REPOSITORY CONTENT

| | |
| --- | --- |
| **cute_little_R_functions.R** | file that has to be sourced in R or RStudio |
| **cute_little_R_functions.docx** | same as cute_little_R_functions.R but for better reading (interactive outline at the beginning of the file) |
| **fun_gg_boxplot.docx** | for better reading of the argument description |
| **fun_gg_scatter.docx** | for better reading of the argument description |
| **examples_xxx** | examples of the xxx cute function, that can be sourced or copied-pasted |
| **check** | folder containing the files that challenge the cute functions |
| **dev** | folder containing avorted or in progress developments |


## DESCRIPTIONS OF THE FUNCTIONS


| gg graphics | |
| --- | --- |
| **fun_gg_palette()** | select colors on the ggplot2 default color palette |
| **fun_gg_just()** | ggplot2 justification of annotated text or axis labeling, depending on angle |
| **fun_gg_get_legend()** | get the legend of ggplot objects |
| **fun_gg_point_rast()** | ggplot2 raster scatterplot layer |
| **fun_gg_boxplot()** | ggplot2 boxplot + dots + means + median/mean values |
| **fun_gg_scatter()** | ggplot2 scatterplot + lines (up to 6 overlays totally) |
| **fun_gg_donut()** | ggplot2 donut |
| **fun_gg_empty_graph()** | generate an empty graphic device with text in the middle |


## HOW TO LOAD AND MANIPULATE cute_little_R_functions


Download the desired Tagged version, never the current master, at https://gitlab.pasteur.fr/gmillot/cute_little_R_functions/-/tags

For that:

1) Select the desired tagged release

2) Browse files

3) Display the cute_little_R_functions.R as raw file

4) Source the file using the url address. Example:

`  source("https://gitlab.pasteur.fr/gmillot/cute_little_R_functions/-/raw/557976d693e37b499b89c76e4b2142846eb89d5b/cute_little_R_functions.R")  `

Complete description of each function is at the beginning of the function body. To obtain it:

1) Either use the cute_little_R_functions.docx.

2) Or in the R/RStudio console, type the name of the function without brackets. Example : `fun_info`.


## VERSIONS

The different releases are tagged [here](https://gitlab.pasteur.fr/gmillot/cute_little_R_functions/-/tags)


## LICENCE

This package of scripts can be redistributed and/or modified under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
Distributed in the hope that it will be useful, but without any warranty; without even the implied warranty of merchandability or fitness for a particular purpose.
See the GNU General Public License for more details at https://www.gnu.org/licenses.


## CITATION

Not yet published


## CREDITS

[Gael A. Millot](https://gitlab.pasteur.fr/gmillot), Hub-CBD, Institut Pasteur, USR 3756 IP CNRS, Paris, France


## ACKNOWLEDGEMENTS

R and R packages developers

Gitlab developers


## WHAT'S NEW IN

### v12.7.0

- bug fixed for the arg.check object inside all the functions
- bug fixed for the arg.user.setting object inside all the functions


### v12.6.0

- minor corrections
- files ready for starting package configuration into cute and ggcute folders


### v12.5.0

1) bug fixed in fun_gg_donut


### v12.4.0

1) fun_gg_empty_graph improved so that no need to use pdf(NULL) anymore


### v12.3.0

1) fun_gg_empty_graph with a true white background (to prevent some display bugs in png and pdf devices) 


### v12.2.0

1) bug removed in the title.text.size argument in gg_donut() 


### v12.1.0

1) bug removed in the title.text.size argument in gg_donut() 


### v12.0.0

1) several arguments added in gg_donut()


### v11.10.0

1) hole.text argument added in gg_donut()


### v11.9.0

1) Argument inf.values added in the check() function

2) gg_donut added


### v11.8.0

1) fun_comp_2d debugged


### v11.7.0

1) fun_comp_2d modified such that now it compares 2D objects without considering the type od the elements


### v11.6.0

1) bug fixed in fun_comp_2d, since identical(data1, data2) returns FALSE if both are identical but data1 is typeof double and data2 is typeof integer


### v11.5.0

1) modification of ggplot2::guides() in fun_gg_boxplot() and fun_gg_scatter() to do not have the warning message about FALSE replaced by "none"


### v11.4.0

1) fun_slide() now correctly works (argument env removed from the parallelization)


### v11.3.0

1) fun_gg_scatter() now correctly plot log2 and log10 scales, as in fun_gg_boxplot()


### v11.2.0

1) fun_open() improved for parallelization


### v11.1.0

1) bug removed in fun_gg_boxplot when having a single color in dot.plot argument


### v11.0.0

1) fun_slide() debugged and fun_test() improved


### v10.9.0

1) no.overwrite argument of fun_report() replaced by overwrite


### v10.8.3

1) fun_test() and fun_slide() debbuged for cute.path


### v10.8.2

1) forgot to add fun_gg_scatter() and fun_gg_boxplot() to v10.8.1. Now ok


### v10.8.1

1) fun_check() and fun_info() readded for practical purposes but must not be changed. See the cute package for that

2) cute.path parameter improved for fun_test() and fun_slide()


### v10.8.0

1) fun_check() and fun_info() removed from this repo and transferred into the cute package

2) Warning: fun_check() must be conveted into cute::check() in all the functions of this repo

3) Warning: fun_info() must be conveted into cute::info() in all the functions of this repo


### v10.7.0

1) fun_gg_boxplot : name of arguments stat.disp and stat.disp.mean changed to stat.pos and stat.mean


### v10.6.0

1) fun_comp_2d() debugged : did not give good results when comparing rows of large data frames


### v10.5.0

1) fun_codon_finder() improved


### v10.4.0

1) fun_codon_finder(): start argument replaced by begin


### v10.3.0

1) fun_codon_finder() added


### v10.2.0

1) fun_codon2aa() added


### v10.1.0

1) in fun_mat_fill: bug fixed. Now works again


### v10.0.0

1) in fun_comp_2d: bug fixed and match positions added


### v9.0.0

1) bug fixed in fun_pack. Now really load using load = TRUE

2) bug fixed for the dot colors of fun_gg_boxplot

3) some of the functions improved (see commits)


### v8.0.0

1) Examples improved

2) Some functions debugged


### v7.0.0

1) Upgrade to R-4.0.2, among other things


### v6.0.0

1) name of functions changed:

fun_param_check()	fun_check()

fun_object_info()	fun_info()

fun_1D_comp()	fun_comp_1d()

fun_2D_comp()	fun_comp_2d()

fun_list_comp()	fun_comp_list()

fun_2D_head()	fun_head()

fun_2D_tail()	fun_tail()

fun_dataframe_remodeling()	fun_df_remod()

fun_by_case_matrix_op()	fun_mat_op()

fun_rounding()	fun_round()

fun_90clock_matrix_rot()	fun_mat_rotate()

fun_hexa_hsv_color_matrix()	fun_mat_num2color()

fun_graph_param_prior_plot()	fun_prior_plot()

fun_feature_post_plot()	fun_post_plot()

fun_window_width_resizing()	fun_width()

fun_open_window()	fun_open()

fun_close_specif_window()	fun_close()

fun_var_trim_display()	fun_trim()

fun_export_data()	fun_report()



2) new functions added:

fun_name_change()

fun_mat_fill()

fun_permut()

fun_permut_consec()

fun_empty_graph()

fun_gg_palette()

fun_gg_just()

fun_gg_point_rast()

fun_gg_scatter()

fun_gg_empty_graph()

fun_segmentation()

fun_pack()

fun_python_pack()

fun_warning()



3) text error modified in fun_head() and fun_tail(), + deals without all objects but use head() and tail() if not 2D object

4) in fun_param_check(): (1) has now the class = "vector", (2) argument fun.name added

5) writting and debugging message errors improved in all the functions

6) Functions checked for R version 3.6.1

7) function deprecated:

fun_refactorization()


### v5.1.0

1) bugs corrected in fun_2D_head() and fun_2D_tail() functions


### v5.0.0

1) fun_2D_head() function added

2) fun_2D_tail() function added

3) argument "noquote" added in fun_export_data()


### v4.9.0

1) fun_mat_inv() function added


### v4.8.0

1) magnific argument removed in fun_feature_post_plot() because no need anymore


### v4.7.0

1) check of numeric arguments: class = "numeric" -> mode = "numeric" to allow for integers values in these arguments


### v4.6.0

1) fun_by_case_matrix_op() added


### v4.5.0

1) fun_open_window() improved to deal with Linux systems

2) fun_graph_param_prior_plot() improved to deal with Linux systems


### v4.4.0

1) fun_dataframe_remodeling() now add a ini_rowname column in the output data frame when it is coherent and when initial row names are available (not NULL)


### v4.3.0

1) fun_object_info() now manages object class "ordered" "factor"


### v4.2.0

1) bug in the fun_2D_comp() function fixed


### v4.1.0

1) text.corner replaced by corner.text everywhere


### v4.0.0

1) fun_var_trim_display() function added

2) fun_feature_post_plot() function now provides additional coordinates when there are margins between the figure region and the device region

3) fun_2D_comp() function now provide the common row and column names, and a bug fixed in the detection of identical row or column content

4) error messages now mention the function that generates this message 


### v3.1.0

1) fun_export_data() function modified: argument data cannot be NULL

2) the debugging line containing r_debugging_tools-v1.2.R updated for the new position of this script


### v3.0.0

1) cute_little_functions.R renamed cute_little_R_functions.R

2) function fun_dataframe_flipping() has been renamed fun_dataframe_remodeling()

3) more examples added, also draws added in the .docx file for fun_dataframe_remodeling()


### v2.0.1

1) functions that use other functions from this toolset now check if these required functions are indeed present in the R environment


### v2.0.0

1) fun_feature_post_plot() function improved: Now independent magnification for x axis/labels and y axis/labels. BEWARE: argument names have been modified

2) Bug corrected in fun_graph_param_prior_plot() function

3) Bug corrected in fun_refactorization() function


### v1.3

1) fun_1D_comp() function improved: provide the common elements, common names and common levels if exist


### v1

Everything
