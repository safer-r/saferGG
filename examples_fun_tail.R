


######### fun_tail() #### tail of the left or right of big 2D objects

### Datasets
mat1 <- matrix(1:30, ncol = 5, dimnames = list(letters[1:6], LETTERS[1:5]))


### Datasets info
mat1 # matrix of integers


### Simple example
fun_tail(data1 = mat1)


### Argument n
fun_tail(data1 = mat1, n = 2) # number of dimension to print (2 means 2 rows and columns)


### Argument side
fun_tail(data1 = mat1, side = "r") # left or right side of the 2D object (only for matrix, data frame or table)


### All the arguments
fun_tail(
    data1 = mat1, 
    n = 10, 
    side = "l"
)





