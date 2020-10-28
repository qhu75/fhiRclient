#' FHIR client connection
#'

fclient <- function(app_id = NULL, app_secret = NULL,
                    api_base, redirect_uri = NULL,
                    patient_id = NULL, scope = list(),
                    launch_token = NULL, state = NULL){
    client <- .fhirclient()$client$FHIRClient
    settings <- list(app_id = app_id,
                     app_secret = app_secret,
                     api_base = api_base,
                     redirect_uri = redirect_uri,
                     patient_id = patient_id,
                     scope = scope,
                     launch_token = launch_token)
    client(settings, state)
}

