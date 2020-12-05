#' install fhirclient python package
#'
#' @param envname Virtual environment to install the `fhirclient`
#'     python module
#' @param method Use conda or virtualenv to install `fhirclient`.
#' @param version The version of `fhirclient`. By default, it will
#'     install the latest version from the github master branch.
#' @return install `fhirclient` python module
#' @importFrom reticulate virtualenv_create virtualenv_install
#'     virtualenv_list use_virtualenv conda_create conda_install
#'     use_condaenv
#' @export

install_fhirclient <- function(envname = "fhirclient",
                               method = "conda", version = "latest") {
    stopifnot(is.character(envname) && length(envname) == 1)

    if (!envname %in% virtualenv_list() | !envname %in% conda_list()$name) {
        if(version == "latest"){
            ## lastest
            pkgs <- "git+https://github.com/smart-on-fhir/client-py.git"
        }else{
            pkgs <- "fhirclient"
        }
        if(method == "virtualenv"){
            virtualenv_create(envname)
            virtualenv_install(envname, pkgs)
        }else if(method == "conda"){
            conda_create(envname)
            conda_install(envname, pkgs, pip = TRUE)
        }else if(method == "pip"){
            system(paste(method, "install --user", pkgs))
        }
    }
    if(method == "conda"){
        use_condaenv(envname)
    }else if(method == "virtualenv"){
        use_virtualenv(virtualenv = envname)
    }
    invisible(.fhirclient())
}
