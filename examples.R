

######## fun_check() #### check class, type, length, etc., of objects

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
# See the examples of fun_info() to test different classes of objects
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







######## fun_info() #### recover object information

### Datasets
vec1 <- -1:3 # vector of integers
vec2 <- 1:3 / 3 # vector of proportions
vec3 <- c(1, 2, NA, -Inf) # vector of integers but stored as "double", with NA and Inf
vec4 <- "pearson" # vector of characters
vec5 <- c("a", "b","a", NA) # vector of characters with NA
cpx <- as.complex(1) # complex
mat1 <- matrix(vec1) # 1D matrix of integers
mat2 <- matrix(c(1:5, NA), ncol = 2, dimnames = list(c("ROW1", "ROW2", "ROW3"), c("M1", "M2"))) # 2D matrix of floats with NA
df1 <- as.data.frame(mat2) # data.frame
l1 <- list(L1 = 1:3, L2 = letters[1:3]) # list
fac1 <- factor(rep(letters[4:6], c(4:6))) # factor
tab1 <- table(fac1) # 1D table
tab2 <- table(fac1, fac1) # 2D table
exp1 <- expression("a") # expression
name1 <- substitute(exp1) # object of class "name", mode "name" & type "symbol"
fun1 <- mean # function type "closure"
fun2 <- sum # function primitive type "builtin"
fun3 <- get("<-") # function primitive type "special"
env1 <- new.env() # environment
s4 <- show # S4 object


### Datasets info
vec1 # vector of integers
vec2 # vector of proportions
vec3 # vector of integers but stored as "double", with NA
vec4 # vector of characters
vec5 # vector of characters with NA
mat1 # 1D matrix of integers
mat2 # 2D matrix of floats with NA
df1 # data.frame
l1 # list
fac1 # factor
tab1 # 1D table
tab2 # 2D table
exp1 # expression
name1 # object of class "name", mode "name" & type "symbol"
fun1 # function type "closure"
fun2 # function primitive type "builtin"
fun3 # function primitive type "special"
env1 # environment
s4 # S4 object

### Simple example
fun_info(data = vec1) # vector of integers
fun_info(data = vec2) # vector of proportions
fun_info(data = vec3) # vector of integers but stored as "double", with NA and Inf
fun_info(data = vec4) # vector of characters
fun_info(data = vec5) # vector of characters with NA
fun_info(data = mat1) # 1D matrix of integers
fun_info(data = mat2) # 2D matrix of floats with NA
fun_info(data = df1) # data.frame
fun_info(data = l1) # list
fun_info(data = fac1) # factor
fun_info(data = tab1) # 1D table
fun_info(data = tab2) # 2D table
fun_info(data = exp1) # expression
fun_info(data = name1) # object of class "name", mode "name" & type "symbol"
fun_info(data = fun1) # function type "closure"
fun_info(data = fun2) # function primitive type "builtin"
fun_info(data = fun3) # function primitive type "special"
fun_info(data = env1) # environment
fun_info(data = s4) # S4 object


### All the arguments
fun_info(
    data = vec1, 
    n = 1, # number of element to display per compartment of the output list (i.e., head(..., n))
    warn.print = FALSE
)




