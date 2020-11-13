#' getModel
#'
#' @param model The resource model to retrieve
#' @return A list of classes in the model
#' @export
#' @examples
#' getModel('patient')
getModel <- function(model){
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
#' getModel('patient')
#' Model("patient", "Patient", list(id = "test"))
Model <- function(model, class, jsonDict = list()){
    mds <- getModel(model)
    cl <- mds[[class]]
    new("Resource", py = cl(jsonDict))
}
