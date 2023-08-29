

######## check() #### check class, type, length, etc., of objects

### Datasets
vec1 <- -1:3 # vector of integers
vec2 <- 1:3 / 3 # vector of proportions
vec3 <- c(1, 2, 3) # vector of integers but stored as "double"
vec4 <- "pearson" # vector of characters
vec5 <- c("a", "b","a", "b") # vector of characters
mat1 <- matrix(vec1) # matrix of integers
mat2 <- matrix(c(1:3 / 3, NA)) # matrix of proportions with NA


### Datasets info
vec1 # vector of integers
vec2 # vector of proportions
vec3 # vector of integers but stored as "double"
vec4 # vector of characters
vec5 # vector of characters
mat1 # matrix of integers
mat2 # matrix of proportions with NA


### Simple examples
# Check that vec1 is of class integer (means that it is also a vector) -> ok
check(data = vec1, class = "integer")
# Check that vec1 is a numeric vector -> error because vec1 is a vector of integers
check(data = vec1, class = "numeric")
# Check that vec1 is an integer vector of length 3 without negative values and without NA -> error because of length 5 and negative values inside vec1
check(data = vec1, class = "vector", typeof = "integer", length = 3, neg.values = FALSE, na.contain = FALSE)
# No result displayed because the output list is assigned into res (see below the print argument)
res <- check(data = vec1, class = "integer")

# with data = NULL, the function systematically report a checking problem
check(data = NULL, class = "integer")

### Argument class, typeof, mode and length are the same as the corresponding R function, except class which 1) has also "vector" and 2) remains "matrix" for matrices and not "matrix" "array"
# Example
check(data = vec1, 
    class = "vector", 
    typeof = "integer", 
    mode = "numeric", 
    length = 5, 
)
# Warning: the function does not check for inconsistencies between arguments. It just checks if everything is ok between arguments values and data
check(data = vec1, 
    typeof = "integer", 
    mode = "character", # the mode "character" exists but is inconsistant with typeof "integer". However, this aspect is not signaled by the function
)
# Error message due to wrong value in the class and length arguments
check(data = vec1, 
    mode = "integer", # the mode "integer" does not exist in the mode() function of R
)

### Argument prop
check(data = mat2, 
    prop = TRUE # Check for values between 0 and 1 only
)

### Argument double.as.integer.allowed
check(data = vec3, typeof = "integer",
    double.as.integer.allowed = TRUE # with TRUE, integers stored as double are accepted
)

### Argument options
check(data = vec4, 
    options = c("pearson", "spearman", "kendall")
)

### Argument all.options.in.data
# No error
check(data = vec5,
    options = c("a", "b"), 
    all.options.in.data = TRUE
)
# No error
check(data = vec5,
    options = c("a", "b", "c"), 
    all.options.in.data = FALSE
)
# Error
check(data = vec5,
    options = c("a", "b", "c"), 
    all.options.in.data = TRUE
)

### Argument na.contain
check(data = mat2, class = "matrix", prop = TRUE,
    na.contain = FALSE # with TRUE, integers stored as double are accepted
)

### Argument neg.values
# Warning: only considered if set to FALSE, to check for non negative values when class is set to "vector", "numeric", "matrix", "array", "data.frame", "table", or typeof is set to "double", "integer", or mode is set to "numeric"
check(data = mat1, class = "matrix",
    neg.values = FALSE # with TRUE, integers stored as double are accepted
)

### Argument inf.values
# Warning: only considered if set to FALSE, to check for non infinite values when class is set to "vector", "numeric", "matrix", "array", "data.frame", "table", or typeof is set to "double", "integer", or mode is set to "numeric"
check(data = mat1, class = "matrix",
    inf.values = FALSE
)

### Argument print
# No error message printed because print is FALSE
res <- check(data = mat1, class = "data.frame",
    print = FALSE
)
# Error message printed
res <- check(data = mat1, class = "data.frame",
    print = TRUE
)
# Even if print is TRUE, no error message printed because no error
res <- check(data = mat1, class = "matrix",
    print = TRUE
)


### Arguments data.name and fun.name
# Example
tempo <- check(data = vec1, class = "integer", 
    data.name = "OBSERVATION_1", 
    fun.name = "FUNCTION_1"
)
tempo$text
# In fact, these two arguments are interesting when check() is used inside functions
fun1 <- function(arg1){
    tempo <- check(data = arg1, class = "integer", 
        data.name = NULL, # if NULL, the name displayed is arg1
        fun.name = NULL # if NULL, no name displayed
    )
    if(tempo$problem == TRUE){
        cat(paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n"))
    }
}
fun1(arg1 = vec4) # error message because arg1 requires a vector of integers



### All the arguments
# See the examples of fun_info() to test different classes of objects
check(
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
    inf.values = TRUE, 
    print = FALSE, 
    data.name = NULL, 
    fun.name = NULL
)

