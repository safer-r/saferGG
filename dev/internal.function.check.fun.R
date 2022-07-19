internal.function.check.fun <- function(expect.arg.values_fun, req.functions_fun, function.name_fun){
    # AIM
    # check the arguments of the other local functions of this script
    # WARNING
    # use the object of the checked function
    # ARGUMENTS
    # expect.arg.values_fun: a list of character strings made of the authorized values of the arguments of the checked function
    # req.functions_fun: a vector of character strings made of the names of the cute little toolbox required functions. Write NULL is no functions required
    # function.name_fun: a character string of the name of the parental function
    # REQUIRED PACKAGES
    # none
    # REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
    # none
    # RETURN
    # error messages
    # EXAMPLES
    # internal.function.check.fun(expect.arg.values_fun = list("a"), req.functions_fun = "b")
    # DEBUGGING
    # expect.arg.values_fun = list("a") ; req.functions_fun = "b"
    # function name
    function.name_intern <- paste0(as.list(match.call(expand.dots=FALSE))[[1]], "()")
    function.name_intern <- paste0(function.name_intern, " INTERNAL FUNCTION OF SLITHERINE")
    # end function name
    # argument checking
    if( ! all(class(expect.arg.values_fun) == "list")){
        tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name_intern, ":\nARGUMENT expect.arg.values_fun MUST BE CLASS LIST AND NOT:\n", paste(class(expect.arg.values_fun) , collapse = "\n"), "\n\n================\n\n")
        stop(tempo.cat)
    }else{
        for(i2 in 1:length(expect.arg.values_fun)){
            if( ! all(class(expect.arg.values_fun[[i2]]) == "character")){
                tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name_intern, ":\nCOMPARTMENT ", i2, " OF ARGUMENT expect.arg.values_fun MUST BE A VECTOR OF CLASS CHARACTER AND NOT:\n", paste(class(expect.arg.values_fun[[i2]]) , collapse = "\n"), "\n\n================\n\n")
                stop(tempo.cat)
            }
        }
    }
    if( ! is.null(req.functions_fun)){
        if( ! all(class(req.functions_fun) == "character")){
            tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name_intern, ":\nARGUMENT req.functions_fun MUST BE A VECTOR OF CLASS CHARACTER AND NOT:\n", paste(class(req.functions_fun) , collapse = "\n"), "\n\n================\n\n")
            stop(tempo.cat)
        }
    }
    if( ! all(class(function.name_fun) == "character")){
        tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name_intern, ":\nARGUMENT function.name_fun MUST BE A VECTOR OF CLASS CHARACTER AND NOT:\n", paste(class(function.name_fun) , collapse = "\n"), "\n\n================\n\n")
        stop(tempo.cat)
    }
    # end argument checking
    # main code using the object of the function evaluated
    if( ! is.null(req.functions_fun)){
        for(i2 in 1:length(req.functions_fun)){
            if(length(find(req.functions_fun[i2], mode = "function")) == 0){
                tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name, ": REQUIRED ", req.functions_fun[i2], " FUNCTION IS MISSING IN THE R ENVIRONMENTS\n\n================\n\n")
                stop(tempo.cat)
            }
        }
    }
    arg.list <- { # this call the arguments of the parental function. To have the arguments of the current function: arg.list <- as.list(match.call(definition = sys.function(sys.parent(n = 2))))[-1] # recover the argument names with attributed values. -1 to remove the function name compartment. sys.function(sys.parent(n = 2)) to recover the agument beyond sys.function(). See also debugging_tools_for_r_dev for other kind of calling
        cl <- sys.call(-1) ;
        f <- get(x = as.character(cl[[1]]), mode = "function", envir = sys.frame(-2)) ; # no env = sys.nframe(), inherit = TRUE here
        cl <- match.call(definition = f, call = cl) ;
        as.list(cl)[-1]
    }
    arg.names <- names(arg.list)
    arg.values <- unlist(as.character(arg.list))
    default.arg.list <- formals(fun = sys.function(sys.parent()), envir = parent.frame(n = 1)) # list of all the arguments of the parental function with their default values (not the values of the user !). Use formals(fun = sys.function(sys.parent(n = 2))) for arguments of the current function
    arg.with.default.value <- ! sapply(default.arg.list, is.symbol) & sapply(sapply(default.arg.list, as.character), identical, "") # logical indicating which of the default.arg.list have default values
    if(any(arg.with.default.value) == TRUE){
        tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name_fun, ":\nARGUMENTS WITH DEFAULT VALUE NOT ALLOWED IN INTERNAL FUNCTIONS (FOR CODE SAFETY):\n", paste(names(default.arg.list)[arg.with.default.value] , collapse = "\n"), "\n\n================\n\n")
        stop(tempo.cat)
    }
    expect.arg.names <- names(default.arg.list)
    tempo.test <- expect.arg.names[expect.arg.names %in% unlist(mapply(FUN = ls, pos = 1:10))]
    if(length(tempo.test) > 0){ # check that argument name are not in the R evironment
        tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name_fun, ":\nARGUMENT NAME OF THIS INTERNAL FUNCTION ALSO PRESENT IN THE R ENVIRONMENT, WHICH IS NOT ALLOWED FOR CODE SAFETY:\n", paste(tempo.test, collapse = "\n"), "\n\n================\n\n")
        stop(tempo.cat)
    }
    if( ! (all(arg.names %in% expect.arg.names) & length(arg.names) == length(expect.arg.names))){
        tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name_fun, ": ARGUMENTS OF THIS FUNCTION MUST BE LENGTH:\n", length(expect.arg.names), "\nAND MUST HAVE THESE NAMES:\n", paste(expect.arg.names, collapse = "\n"), "\n\nHERE, IT IS LENGTH:\n", length(arg.names), "\nWITH NAMES:\n", paste(arg.names, collapse = "\n"), "\n\n================\n\n")
        stop(tempo.cat)
    }
    if(length(arg.values) != length(expect.arg.values_fun)){
        tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name_fun, ": ARGUMENTS OF THIS FUNCTION MUST BE LENGTH:\n", length(expect.arg.values_fun), "\n\nBUT HERE, IT IS LENGTH:\n", length(arg.values), "\n\n================\n\n")
        stop(tempo.cat)
    }
    for(i1 in 1:length(expect.arg.values_fun)){
        if( ! all(arg.values[i1] %in% expect.arg.values_fun[[i1]])){
            tempo.cat <- paste0("\n\n================\n\nERROR IN ", function.name_fun, ": VALUE OF ARGUMENT ", arg.names[i1], " MUST BE:\n", paste(dQuote(expect.arg.values_fun[[i1]], q = FALSE), collapse = "\n"), "\n\nBUT HERE, IT IS VALUE:\n", paste(arg.values[i1], collapse = "\n"), "\n\n================\n\n")
            stop(tempo.cat)
        }
    }
}

