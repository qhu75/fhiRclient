#' Python object
#'
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
#' @importFrom S4Vectors wmsg
#' @export
setMethod("$", "Py", function(x, name){
    if(name %in% names(x@py)){
        x@py[[name]]
    }else{
        stop(wmsg("the '", name, "' does not exist"))
    }
})

setReplaceMethod("$", "Py", function(x, name, value){
    if(name %in% names(x@py)){
        x@py[[name]] <- value
        return(x)
    }else{
        stop(wmsg("the '", name, "' does not exist"))
    }
})

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
    x@py$create(ct$server)
}
setMethod("Create", signature("Resource"), Create)

## setGeneric("Create")

#' Read
#'
#' @param ct The `Client` returned object.
#' @param x The resource to Read.
#' @export
Read <- function(ct, x){
    r <- x@py$read(x@py$id, ct$server)
    new("Resource", py = r)
}
setMethod("Read", "Resource", Read)
## setMethod("Read", signature("Resource"), function(ct, x){
##     r <- x@py$read(x@py$id, ct$server)
##     new("Resource", py = r)
## })

## setGeneric("Read")

#' Update
#'
#' @param ct The `Client` returned object.
#' @param x The resource to update.
Update <- function(ct, x){
    x@py$update(ct$server)
}
setMethod("Update", "Resource", Update)

#' Delete
#'
#' @param ct The `Client` returned object.
#' @param x The resource to update.
Delete <- function(ct, x){
    x@py$delete(ct$server)
}
setMethod("Delete", "Resource", Delete)

#' as json
#'
#' @export
as_json <- function(x)x@py
setMethod("as_json", signature("Resource"), function(x){
    x@py$as_json()
})
