.onLoad <- function(libname, pkgname) {
    ti <- tryCatch(import("fhirclient"), error = function(e)NULL)
    if(is.null(ti)){
        warning("Trying to install fhirclient")
        install_fhirclient(method = "pip")
    }
    invisible()
}
