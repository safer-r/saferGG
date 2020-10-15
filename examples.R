

######## fun_check() #### check class, type, length, etc., of objects

### Datasets
vec1 <- -1:3 # vector of integers
vec2 <- 1:3 / 3 # vector of proportions
vec3 <- c(1, 2, 3) # vector of integers but stored as "double"
vec4 <- "pearson"
vec5 <- c("a", "b","a", "b")
mat1 <- matrix(vec1) # matrix of integers
mat2 <- matrix(c(1:3 / 3, NA)) # matrix of proportions with NA


### Datasets info
vec1
vec2
vec3
vec4
vec5
mat1
mat2


### Simple examples
# Check that vec1 is of class integer (means that it is also a vector) -> okdensity(x, bw = bandwidth)
fun_check(data = vec1, class = "integer")
# Check that vec1 is a numeric vector -> error because vec1 is a vector of integers
fun_check(data = vec1, class = "numeric")
# Check that vec1 is an integer vector of length 3 without negative values and without NA -> error because of length 5 and negative values inside vec1
fun_check(data = vec1, class = "vector", typeof = "integer", length = 3, neg.values = FALSE, na.contain = FALSE)
# No result displayed because the output list is assigned into res (see below the print argument)
res <- fun_check(data = vec1, class = "integer")
res

### Argument class, typeof, mode and length are the same as the corresponding R function, except class which 1) has also "vector" and 2) remains "matrix" for matrices
# Example
fun_check(data = vec1, 
    class = "vector", 
    typeof = "integer", 
    mode = "numeric", 
    length = 5, 
)
# Warning: the function odes not check for inconsistencies between arguments. It just checks if everything is ok between arguments values and data
fun_check(data = vec1, 
    typeof = "integer", 
    mode = "character", # the mode "character" exists but is inconsistant with typeof "integer". However, this aspect is not signaled by the function
)
# Error message due to wrong value in the class and length arguments
fun_check(data = vec1, 
    mode = "integer", # the mode "integer" does not exist in the mode() function of R
)

### Argument prop
fun_check(data = mat2, 
    prop = TRUE # Check for values between 0 and 1 only
)

### Argument double.as.integer.allowed
fun_check(data = vec3, typeof = "integer",
    double.as.integer.allowed = TRUE # with TRUE, integers stored as double are accepted
)

### Argument options
fun_check(data = vec4, 
    options = c("pearson", "spearman", "kendall")
)

### Argument all.options.in.data
# No error
fun_check(data = vec5,
    options = c("a", "b"), 
    all.options.in.data = TRUE
)
# No error
fun_check(data = vec5,
    options = c("a", "b", "c"), 
    all.options.in.data = FALSE
)
# Error
fun_check(data = vec5,
    options = c("a", "b", "c"), 
    all.options.in.data = TRUE
)

### Argument na.contain
fun_check(data = mat2, class = "matrix", prop = TRUE,
    na.contain = FALSE # with TRUE, integers stored as double are accepted
)

### Argument neg.values
# Warning: only considered if set to FALSE, to check for non negative values when class is set to "vector", "numeric", "matrix", "array", "data.frame", "table", or typeof is set to "double", "integer", or mode is set to "numeric"
fun_check(data = mat1, class = "matrix",
    neg.values = FALSE # with TRUE, integers stored as double are accepted
)

### Argument print
# No error message because print is FALSE
res <- fun_check(data = mat1, class = "data.frame",
    print = FALSE
)
# Error message
res <- fun_check(data = mat1, class = "data.frame",
    print = TRUE
)
# No error message because no error
res <- fun_check(data = mat1, class = "matrix",
    print = TRUE
)


### Arguments data.name and fun.name
# Example
tempo <- fun_check(data = vec1, class = "integer", 
    data.name = "OBSERVATION_1", 
    fun.name = "FUNCTION_1"
)
tempo$text
# In fact, these two arguments are interesting when fun_check() is used inside functions
fun1 <- function(arg1){
    tempo <- fun_check(data = arg1, class = "integer", 
        data.name = NULL, # if NULL, the name displayed is arg1
        fun.name = NULL # if NULL, no name displayed
    )
    if(tempo$problem == TRUE){
        cat(paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n"))
    }
}
fun1(arg1 = vec4)



### All the arguments
fun_check(
    data = vec1, 
    class = "integer", 
    typeof = NULL, 
    mode = NULL, 
    length = NULL, 
    prop = FALSE, 
    double.as.integer.allowed = FALSE, 
    options = NULL, 
    all.options.in.data = FALSE, 
    na.contain = FALSE, 
    neg.values = TRUE, 
    print = FALSE, 
    data.name = NULL, 
    fun.name = NULL
)


