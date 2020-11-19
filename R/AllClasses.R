#' Python object
#'
#' @rdname pyclass
#' @param x A python class
#' @importFrom utils head
#' @export
setClass("Py", slots = list(py = "ANY"))
Py <- function(x){
    new("Py", py = x)
}

.validPy <- function(object){
    is(object@py, "python.builtin.object")
}
setValidity("Py", .validPy)

setMethod(show, "Py", function(object){
    cat(as.character(object@py$"__class__"), "\n")
    cat("length:", length(names(object@py)), "\n")
    cat("names:", paste(head(names(object@py), 3), collapse = " "), "...\n")
})

#' Extract input values by name
#'
#' @rdname pyclass
#' @param x A pyclass object
#' @param name The class name to extract
#' @export
setMethod("$", "Py", function(x, name){
    if(name %in% names(x@py)){
        x@py[[name]]
    }else{
        stop("the '", name, "' does not exist")
    }
})

#' @rdname pyclass
#' @param value The value to assign
#' @export
setReplaceMethod("$", "Py", function(x, name, value){
    if(name %in% names(x@py)){
        x@py[[name]] <- value
        return(x)
    }else{
        stop("the '", name, "' does not exist")
    }
})

#' @rdname pyclass
#' @export
setMethod("names", "Py", function(x){
    names(x@py)
})
setGeneric("$")
setGeneric("names")

#' Resource class
#'
#' @export
setClass("Resource", contains = "Py")

#' get py slot
#'
#' @rdname pyclass
#' @param x Resource object
#' @return python object
#' @export
py <- function(x)x@py

#' Create
#'
#' @param ct The `Client` returned object.
#' @param x The resource to create.
#' @export
Create <- function(ct, x){
    stopifnot(is(x, "Resource"))
    x@py$create(ct$server)
}


## setGeneric("Create")

#' Read
#'
#' @param ct The `Client` returned object.
#' @param x The resource to Read.
#' @export
Read <- function(ct, x){
    stopifnot(is(x, "Resource"))
    r <- x@py$read(x@py$id, ct$server)
    new("Resource", py = r)
}

## setMethod("Read", signature("Resource"), function(ct, x){
##     r <- x@py$read(x@py$id, ct$server)
##     new("Resource", py = r)
## })

## setGeneric("Read")

#' Update
#'
#' @param ct The `Client` returned object.
#' @param x The resource to update.
#' @export
Update <- function(ct, x){
    stopifnot(is(x, "Resource"))
    x@py$update(ct$server)
}

#' Delete
#'
#' @param ct The `Client` returned object.
#' @param x The resource to update.
#' @export
Delete <- function(ct, x){
    stopifnot(is(x, "Resource"))
    x@py$delete(ct$server)
}

#' as_json
#'
#' @param x A Resource object.
#' @export
as_json <- function(x){
    stopifnot(is(x, "Resource"))
    x@py$as_json()
}


