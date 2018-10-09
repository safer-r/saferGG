#### DESCRIPTION

Cute Little R Functions contains 17 functions for R/RStudio that facilitate basic procedures in 1) object analysis, 2) object modification, 3) graphic handling and 4) log file management.
The function names are:
fun_param_check()	Check the class, type, mode and length, prop, neg values, na.contains, etc., of an object
fun_object_info()	provide a full description of the object
fun_1D_comp()	compare two 1D datasets (vector of factor or 1D table) of the same class or not
fun_2D_comp()	compare two 2D datasets of the same class or not
fun_list_comp()	compare two lists
fun_dataframe_remodeling()	remodel data frames
fun_refactorization()	refactorize a factor or the factor columns of a data frame, such as only the class present are in the levels (no empty levels). The class order in levels is kept
fun_rounding()	round a vector of values, if decimal, with the desired number of decimal digits after the decimal leading zeros
fun_90clock_matrix_rot()	90° clockwise matrix rotation
fun_hexa_hsv_color_matrix()	convert a matrix made of numbers into a hexadecimal matrix for rgb colorization
fun_window_width_resizing()	rescale the width of a window to open depending on the number of classes to plot
fun_open_window()	open a pdf or screen (GUI) graphic window
fun_graph_param_prior_plot()	very convenient to erase the axes for post plot axis redrawing using fun_feature_post_plot()
fun_feature_post_plot()	redesign axis and provide convenients coordinates for adding elements on the drawn graph
fun_close_specif_window()	close only specific graphic windows (devices)
fun_var_trim_display()	trim and display values from a numeric vector or matrix
fun_export_data()	log file function: print a character string or a data object into a same output file


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

1) fun_1D_comp() function improved: provide the common elements, common names and common levels if exist.


