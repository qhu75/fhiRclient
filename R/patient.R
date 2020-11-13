#' Patient model
#' @export
setClass("Patient", contains = "Resource")

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

## #' @importFrom S4Vectors SimpleList
## setMethod(show, "Patient", function(object){
##     cat(as.character(object@py$"__class__"), "\n")
##     show(SimpleList(object@py$as_json()))
## })

#' Birth Date
#'
#' @param pt A `Resource` object from `Read`.
#' @export
BirthDate <- function(pt){
    stopifnot(is(pt, "Resource"))
    pt$birthDate$isostring
}

#' getName
#'
#' @param pt Extract full name
getName <- function(pt){
    stopifnot(is(pt, "Resource"))
    ct <- Client(api_base = "example.com")
    lapply(pt$name, function(x)ct$human_name(x))
}
