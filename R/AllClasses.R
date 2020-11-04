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

setMethod("names", "Py", function(x){
    names(x@py)
})
setGeneric("$")
setGeneric("names")

#' Resource
#'
#' @export
setClass("Resource", contains = "Py")

#' Create
#'
#' @export
Create <- function(server, x)x
setMethod("Create", signature("Resource"), function(server, x){
    r <- x@py$create(server)
    new("Resource", py = r)
})

## setGeneric("Create")

#' Read
#'
#' @export
Read <- function(server, x){
    r <- x@py$read(x@py$id, server)
    new("Resource", py = r)
}
setMethod("Read", signature("Resource"), function(server, x){
    r <- x@py$read(x@py$id, server)
    new("Resource", py = r)
})

## setGeneric("Read")

#' as json
#'
#' @export
as_json <- function(x)x@py
setMethod("as_json", signature("Resource"), function(x){
    x@py$as_json()
})
