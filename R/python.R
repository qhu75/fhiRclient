# Python package
#' @importFrom reticulate import py_run_string
#' @import reticulate
.fhirclient <- local({
    .fhirclient <- NULL
    function() {
        if (is.null(.fhirclient))
            .fhirclient <<- import("fhirclient")
        .fhirclient
    }
})
