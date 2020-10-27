#' @importFrom reticulate import py_run_string
.fhirclient <- local({
    .fhirclient <- NULL
    function() {
        if (is.null(.fhirclient))
            .fhirclient <<- import("fhirclient")
        .fhirclient
    }
})
