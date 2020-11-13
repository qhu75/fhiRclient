#' FHIR client connection
#'
#' @param app_id The app ID.
#' @param app_secret The client secret.
#' @param api_base The FHIR service to connect.
#' @param redirect_uri The callback/redirect URL for your app.
#' @param patient_id The patient id against which to operate, if already known.
#' @param scope Space-separated list of scopes to request, if other than default.
#' @param launch_token The launch token.
#' @param state The client state.
#' @export
#' @examples
#' 
#' settings = list('app_id'= 'my_web_app', 'api_base'= 'https://r4.smarthealthit.org')
#' smart <- Client("my_app", api_base = "https://r4.smarthealthit.org")
#' 

Client <- function(app_id = NULL, app_secret = NULL,
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
    ct <- client(settings, state)
    return(Py(ct))
}

