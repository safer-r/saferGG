#### DESCRIPTION

Cute Little R Functions contains 16 functions for R/RStudio that facilitate basic procedures in 1) object analysis, 2) object modification, 3) graphic handling and 4) log file management.
The function names are:
fun_param_check()
fun_object_info()
fun_1D_comp()
fun_2D_comp()
fun_list_comp()
fun_dataframe_flipping()
fun_refactorization()
fun_rounding()
fun_90clock_matrix_rot()
fun_hexa_hsv_color_matrix()
fun_window_width_resizing()
fun_open_window()
fun_graph_param_prior_plot()
fun_feature_post_plot()
fun_close_specif_window()
fun_export_data()


#### HOW TO USE IT

Download the desired Tagged version, never the current master.

Source the cute_little_R_functions.R into R/RStudio to have the functions available in the working environment.

Description of the functions is at the beginning of the function body. To obtain it:

	Either read the cute_little_functions.docx (or open the cute_little_R_functions.R file).
	
	Or in the R/RStudio console, type the name of the function without brackets. Example : fun_object_info.


#### FILE DESCRIPTIONS

cute_little_R_functions.R	file that has to be sourced
cute_little_R_functions.docx	just for easier code reading
examples_alone.txt	compile all the examples of each of the 16 functions into a single file


#### WEB LOCATION

Check for updated versions (more recent release tags) at https://gitlab.pasteur.fr/gmillot/cute_little_functions


#### WHAT'S NEW IN

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

1) fun_1D_comp() function improved: provide the common elements, common names and common levels if exist.
