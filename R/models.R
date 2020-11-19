#' getModel classes
#'
#' @param model The resource model to retrieve
#' @return A list of classes in the model
#' @export
#' @examples
#' getModelClass('patient')
getModelClass <- function(model){
    md <- .fhirclient()$models[[model]]
    pyn <- names(md)
    tid <- sapply(pyn, function(x)is(md[[x]], "python.builtin.type"))
    sapply(pyn[tid], function(x)md[[x]])
}

#' Model
#'
#' @param model The resource model to build
#' @param class The class of the resource model
#' @param jsonDict The json list to build the data model object. If
#'     NULL, then it returns the class.
#' @return A `Resource` object
#' @export
#' @examples
#' getModelClass('patient')
#' Model("patient", "Patient", list(id = "test"))
Model <- function(model, class, jsonDict = list()){
    mds <- getModelClass(model)
    stopifnot(class %in% names(mds))
    cl <- mds[[class]]
    new("Resource", py = cl(jsonDict))
}
