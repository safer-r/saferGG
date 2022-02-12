


######### fun_head() #### head of the left or right of big 2D objects

### Datasets
vec1 <- 1:100 # vector of integers
mat1 <- diag(1:20) ;  dimnames(mat1) <- list(letters[1:20], LETTERS[1:20]) # diagonal matrix 20 * 20 with row names and column names


### Datasets info
vec1 # vector of integers
mat1 # diagonal matrix 20 * 20 with row names and column names


### Simple example
fun_head(data1 = vec1) # fun_head() works like head() on non 2D objects
fun_head(data1 = mat1)


### Argument n
fun_head(data1 = mat1, n = 5) # number of dimension to print (5 means 5 rows and columns)


### Argument side
fun_head(data1 = mat1, side = "r") # left or right side of the 2D object (only for matrix, data frame or table)


### All the arguments
fun_head(
    data1 = mat1, 
    n = 6, 
    side = "l"
)





