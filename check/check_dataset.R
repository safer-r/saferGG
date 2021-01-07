################ CREATION OF THE CHECKING DATASET


######## object list t25_20201108 (25 objects created 20201108)


vec1 <- -1:3 # vector of integers
vec2 <- 1:3 / 3 # vector of proportions
vec3 <- c(1, 2, NA, -Inf) # vector of integers but stored as "double", with NA and Inf
vec4 <- "pearson" # vector of characters
vec5 <- c("a", "b","a", NA) # vector of characters with NA
cpx1 <- as.complex(1) # complex
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
s4.1 <- show # S4 object
call1 <- call("call1") # object of class "call", mode "call" &  type "language"

t25_20201108 <- list( # test list
    NULL, 
    NA, 
    1, 
    TRUE, 
    vec1, 
    vec2, 
    vec3, 
    vec4, 
    vec5, 
    cpx1, 
    mat1, 
    mat2, 
    df1, 
    l1, 
    fac1, 
    tab1, 
    tab2, 
    exp1, 
    name1, 
    fun1, 
    fun2, 
    fun3, 
    env1, 
    s4.1, 
    call1
)


save(list = c(
    "vec1", 
    "vec2", 
    "vec3", 
    "vec4", 
    "vec5", 
    "cpx1", 
    "mat1", 
    "mat2", 
    "df1", 
    "l1", 
    "fac1", 
    "tab1", 
    "tab2", 
    "exp1", 
    "name1", 
    "fun1", 
    "fun2", 
    "fun3", 
    "env1", 
    "s4.1", 
    "call1",
    "t25_20201108"
), file = "C:/Users/gael/Documents/Git_projects/cute_little_R_functions/check/check_dataset.t25_20201108.RData")


######## object list t26_20201124 (26 objects created 20201124)


vec1 <- -1:3 # vector of integers
vec2 <- 1:3 / 3 # vector of proportions
vec3 <- c(1, 2, NA, -Inf) # vector of integers but stored as "double", with NA and Inf
vec4 <- "pearson" # vector of characters
vec5 <- c("a", "b","a", NA) # vector of characters with NA
cpx1 <- as.complex(1) # complex
mat1 <- matrix(vec1) # 1D matrix of integers
mat2 <- matrix(c(1:5, NA), ncol = 2, dimnames = list(c("ROW1", "ROW2", "ROW3"), c("M1", "M2"))) # 2D matrix of floats with NA
df1 <- as.data.frame(mat2) # data.frame
df2 <- data.frame(df1, CAT = letters[1:nrow(df1)]) # data.frame
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
s4.1 <- show # S4 object
call1 <- call("call1") # object of class "call", mode "call" &  type "language"

t26_20201124 <- list( # test list
    NULL, 
    NA, 
    1, 
    TRUE, 
    vec1, 
    vec2, 
    vec3, 
    vec4, 
    vec5, 
    cpx1, 
    mat1, 
    mat2, 
    df1, 
    df2, 
    l1, 
    fac1, 
    tab1, 
    tab2, 
    exp1, 
    name1, 
    fun1, 
    fun2, 
    fun3, 
    env1, 
    s4.1, 
    call1
)


save(list = c(
    "vec1", 
    "vec2", 
    "vec3", 
    "vec4", 
    "vec5", 
    "cpx1", 
    "mat1", 
    "mat2", 
    "df1", 
    "df2", 
    "l1", 
    "fac1", 
    "tab1", 
    "tab2", 
    "exp1", 
    "name1", 
    "fun1", 
    "fun2", 
    "fun3", 
    "env1", 
    "s4.1", 
    "call1",
    "t26_20201124"
), file = "C:/Users/gael/Documents/Git_projects/cute_little_R_functions/check/check_dataset.t26_20201124.RData")


######## object list t8_20201126 (8 objects created 20201126)


cor1 <- list(-1, 0, 1) # vector of integers
prop1 <- list(0, 0.5, 1) # vector of proportions
int1 <- list(0, 2, 4) # vector of positive integers
int2 <- list(0, 10, 20) # vector of positive integers
angle1 <- list(-90, 0, 45, 90, 180) # vector of positive integers
log1 <- list("no", "log2", "log10") # vector of characters
logic1 <- list(TRUE, FALSE) # vector of logic
path1 <- list("C:\\Program Files\\R\\R-4.0.2\\library\\")

t8_20201126 <- list( # test list
    cor1, 
    prop1, 
    int1, 
    int2, 
    angle1, 
    log1, 
    logic1, 
    path1
)


save(list = c(
    "cor1", 
    "prop1", 
    "int1", 
    "int2", 
    "angle1", 
    "log1", 
    "logic1", 
    "path1", 
    "t8_20201126"
), file = "C:/Users/gael/Documents/Git_projects/cute_little_R_functions/check/check_dataset.t8_20201126.RData")


################ END CREATION OF THE CHECKING DATASET
