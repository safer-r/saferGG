#### DESCRIPTION

Cute Little R Functions contains 40 functions for R/RStudio that facilitate basic procedures in 1) object analysis, 2) object modification, 3) graphic handling and 4) log file management.

The function names are:

## Object analysis

fun_param_check() #### check class, type, length, etc., of objects
fun_object_info() #### recover object information
fun_1D_comp() #### comparison of two 1D datasets (vectors, factors, 1D tables)
fun_2D_comp() #### comparison of two 2D datasets (row & col names, dimensions, etc.)
fun_2D_head() #### head of the left or right of big 2D objects
fun_2D_tail() #### tail of the left or right of big 2D objects
fun_list_comp() #### comparison of two lists


## Object modification

fun_name_change() #### check a vector of character strings and modify any string if present in another vector
fun_dataframe_remodeling() #### remodeling a data frame to have column name as a qualitative values and vice-versa
fun_refactorization() #### remove classes that are not anymore present in factors or factor columns in data frames
fun_round() #### rounding number if decimal present
fun_90clock_matrix_rot() #### 90° clockwise matrix rotation
fun_num2color_mat() #### convert a numeric matrix into hexadecimal color matrix
fun_by_case_matrix_op() #### assemble several matrices with operation
fun_mat_inv() #### return the inverse of a square matrix
fun_mat_fill() #### fill the empty half part of a symmetric square matrix
fun_consec_pos_perm() #### progressively breaks a vector order


## Graphics management

fun_window_width_resizing() #### window width depending on classes to plot
fun_open_window() #### open a GUI or pdf graphic window
fun_prior_plot() #### set graph param before plotting
fun_scale() #### select nice numbers when setting breaks on an axis
fun_post_plot() #### set graph param after plotting
fun_close_specif_window() #### close specific graphic windows


## Standard graphics

fun_empty_graph() #### text to display for empty graphs


## gg graphics

fun_gg_palette() #### ggplot2 default color palette
fun_gg_just() #### ggplot2 justification of the axis labeling, depending on angle
fun_gg_scatter() #### ggplot2 scatterplot + lines (up to 6 overlays totally)
fun_gg_bar_mean() #### ggplot2 mean barplot + overlaid dots if required
fun_gg_boxplot() #### ggplot2 boxplot + background dots if required
fun_gg_bar_prop() #### ggplot2 proportion barplot
fun_gg_strip() #### ggplot2 stripchart + mean/median
fun_gg_violin() #### ggplot2 violins
fun_gg_line() #### ggplot2 lines + background dots and error bars
fun_gg_heatmap() #### ggplot2 heatmap + overlaid mask if required
fun_gg_empty_graph() #### text to display for empty graphs


## Graphic extraction

fun_var_trim_display() #### display values from a quantitative variable and trim according to defined cut-offs
fun_segmentation() #### segment a dot cloud on a scatterplot and define the dots from another cloud outside the segmentation


## Import

fun_pack_import() #### check if R packages are present and import into the working environment
fun_python_pack_import() #### check if python packages are present


## Exporting results (text & tables)

fun_export_data() #### print string or data object into output file




#### LICENCE

This package of scripts can be redistributed and/or modified under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
Distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details at https://www.gnu.org/licenses.




#### CREDITS

Gael A. Millot, Hub-C3BI, Institut Pasteur, USR 3756 IP CNRS, Paris, France




#### HOW TO USE IT

Download the desired Tagged version, never the current master.

Source the cute_little_R_functions.R into R/RStudio to have the functions available in the working environment.

Description of the functions is at the beginning of the function body. To obtain it:

	Either read the cute_little_functions.docx (or open the cute_little_R_functions.R file).
	
	Or in the R/RStudio console, type the name of the function without brackets. Example : fun_object_info.




#### FILE DESCRIPTIONS

cute_little_R_functions.R	file that has to be sourced
cute_little_R_functions.docx	just for easier code reading
examples_alone.txt	compile all the examples of the functions into a single file




#### WEB LOCATION

Check for updated versions (most recent tags) at https://gitlab.pasteur.fr/gmillot/cute_little_functions/tags




#### WHAT'S NEW IN


## v6.0.0

1) name of functions changed:
fun_param_check()	fun_check()
fun_object_info()	fun_info()
fun_1D_comp()	fun_1d_comp()
fun_2D_comp()	fun_2d_comp()
fun_2D_head()	fun_2d_head()
fun_2D_tail()	fun_2d_tail()
fun_dataframe_remodeling()	fun_df_remod()
fun_refactorization()	fun_refact()
fun_by_case_matrix_op()	fun_mat_op()
fun_rounding()	fun_round()
fun_hexa_hsv_color_matrix()	fun_num2color_mat()
fun_graph_param_prior_plot()	fun_prior_plot()
fun_feature_post_plot()	fun_post_plot()
fun_window_width_resizing()	fun_width()
fun_open_window()	fun_open()
fun_close_specif_window()	fun_close()
fun_var_trim_display()	fun_trim()
fun_data_export()	fun_report()

2) new functions added:
fun_name_change()
fun_mat_fill()
fun_consec_pos_perm()
fun_empty_graph()
fun_gg_palette()
fun_gg_just()
fun_gg_scatter()
fun_gg_bar_mean()
fun_gg_heatmap()
fun_gg_empty_graph()
fun_segmentation()
fun_pack()
fun_python_pack()

3) text error modified in fun_2D_head() and fun_2D_tail()

4) in fun_param_check(): (1) has now the class = "vector", (2) argument fun.name added

5) writting and debugging message errors improved in all the functions

6) Functions checked for R version 3.5.3


## v5.1.0

1) bugs corrected in fun_2D_head() and fun_2D_tail() functions


## v5.0.0

1) fun_2D_head() function added
2) fun_2D_tail() function added
3) argument "noquote" added in fun_export_data()


## v4.9.0

1) fun_mat_inv() function added


## v4.8.0

1) magnific argument removed in fun_feature_post_plot() because no need anymore


## v4.7.0

1) check of numeric arguments: class = "numeric" -> mode = "numeric" to allow for integers values in these arguments


## v4.6.0

1) fun_by_case_matrix_op() added


## v4.5.0

1) fun_open_window() improved to deal with Linux systems

2) fun_graph_param_prior_plot() improved to deal with Linux systems


## v4.4.0

1) fun_dataframe_remodeling() now add a ini_rowname column in the output data frame when it is coherent and when initial row names are available (not NULL)


## v4.3.0

1) fun_object_info() now manages object class "ordered" "factor"


## v4.2.0

1) bug in the fun_2D_comp() function fixed


## v4.1.0

1) text.corner replaced by corner.text everywhere


## v4.0.0

1) fun_var_trim_display() function added

2) fun_feature_post_plot() function now provides additional coordinates when there are margins between the figure region and the device region

3) fun_2D_comp() function now provide the common row and column names, and a bug fixed in the detection of identical row or column content

4) error messages now mention the function that generates this message 


## v3.1.0

1) fun_export_data() function modified: argument data cannot be NULL

2) the debugging line containing r_debugging_tools-v1.2.R updated for the new position of this script


## v3.0.0

1) cute_little_functions.R renamed cute_little_R_functions.R

2) function fun_dataframe_flipping() has been renamed fun_dataframe_remodeling()

3) more examples added, also draws added in the .docx file for fun_dataframe_remodeling()


## v2.0.1

1) functions that use other functions from this toolset now check if these required functions are indeed present in the R environment


## v2.0.0

1) fun_feature_post_plot() function improved: Now independent magnification for x axis/labels and y axis/labels. BEWARE: argument names have been modified

2) Bug corrected in fun_graph_param_prior_plot() function

3) Bug corrected in fun_refactorization() function


## v1.3

1) fun_1D_comp() function improved: provide the common elements, common names and common levels if exist


