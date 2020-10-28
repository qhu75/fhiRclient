#' Patient model
#' @export
#' @import 
setClass("Patient",
         contains = "Resource")

#' Patient class
#'
#' @param jsonDict A list of items for the Patient resource.
#' @return A object of Patient
#' @export
#' @examples
#' Patient(list(id = "pt1"))
Patient <- function(jsonDict = list()){
    pt <- .fhirclient()$models$patient$Patient(jsonDict)
    new("Patient", py = pt)
}

#' @importFrom S4Vectors SimpleList
setMethod(show, "Patient", function(object){
    cat(as.character(object@py$"__class__"), "\n")
    show(SimpleList(object@py$as_json()))
})
