#' Resources
#'
setClass("Resource",
         slots = list(py = "ANY"))

create <- function(x, server)x
setMethod("create", signature("Resource"), function(x, server){
    x@py$create(server)
})

setGeneric("create")

## as_json <- function(x){
##     x@py$as_json()
## }
as_json <- function(x)x@py
setMethod("as_json", signature("Resource"), function(x){
    x@py$as_json()
})
setGeneric("as_json")
