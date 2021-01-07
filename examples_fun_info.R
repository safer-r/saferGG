




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
exp1 <- expression("a") # object of class "expression", mode "expression" & type "expression"
name1 <- substitute(exp1) # object of class "name", mode "name" & type "symbol"
fun1 <- mean # closure function of class "function", mode "function" & type "closure"
fun2 <- sum # primitive function of class "function", mode "function" & type "builtin"
fun3 <- get("<-") # primitive function of class "function", mode "function" &  type "special"
env1 <- new.env() # environment
s4 <- show # S4 object
call1 <- call("call1") # object of class "call", mode "call" & type "language"

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
exp1 # object of class "expression", mode "expression" & type "expression"
name1 # object of class "name", mode "name" & type "symbol"
fun1 # closure function of class "function", mode "function" & type "closure"
fun2 # primitive function of class "function", mode "function" & type "builtin"
fun3 # primitive function of class "function", mode "function" &  type "special"
env1 # environment
s4 # S4 object
call1 # object of class "call", mode "call" &  type "language"


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
fun_info(data = exp1) # object of class "expression", mode "expression" & type "expression"
fun_info(data = name1) # object of class "name", mode "name" & type "symbol"
fun_info(data = fun1) # closure function of class "function", mode "function" & type "closure"
fun_info(data = fun2) # primitive function of class "function", mode "function" & type "builtin"
fun_info(data = fun3) # primitive function of class "function", mode "function" &  type "special"
fun_info(data = env1) # environment
fun_info(data = s4) # S4 object
fun_info(data = call1) # object of class "call", mode "call" &  type "language"



### All the arguments
fun_info(
    data = vec1, 
    n = 1, # number of element to display per compartment of the output list (i.e., head(..., n))
    warn.print = FALSE
)




