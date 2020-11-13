#' Search records
#'
#' @param ct The object from Client. The server to perform search.
#' @param model The resource model to build.
#' @param class The class of the resource model.
#' @param where The search parameters.
#' @param include The `include` parameter to include other resources.
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom httr content
#' @export
#' @examples
#' \donotrun{
#' Search(ct, "observation", "Observation",
#' list(code = "http://loinc.org|2339-0"), include = "subject")
#' }
Search <- function(ct, model, class,
                   where = list(), include = NULL,
                   page = c("first", "all"),
                   tidy = TRUE){
    Obs <- Model(model, class, NULL)
    sc <- Obs$where(where)
    if(!is.null(include)){
        sc <- sc$include(include)
    }
    bundle <- sc$perform(ct$server)$as_json()
    
    page <- match.arg(page)
    if(page == "all" && !is.null(bundle) && length(bundle$link) >= 2){
        link <- bundle$link
        relations <- unlist(lapply(link, function(x)x$relation))
        urls <- unlist(lapply(link, function(x)x$url))
        bundle <- list(bundle)

        while(length(link)>=2 & "next" %in% relations) {
            u1 <- urls[relations == "next"]
            bd <- fromJSON(content(GET(u1), "text"), simplifyVector = FALSE)
            bundle <- c(bundle, list(bd))
            
            link <- bd$link
            relations <- unlist(lapply(link, function(x)x$relation))
            urls <- unlist(lapply(link, function(x)x$url))
        }
    }
    if(tidy){
        return(as_json_tbl(bundle))
    }else{
        return(bundle)
    }
}

#' json to tbl
#'
#' @importFrom tibble enframe
#' @param jsonlist The list from fromJSON
#' @export
as_json_tbl <- function(jsonlist){
    return(enframe(unlist(jsonlist)))
}

#' filter by names
#'
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
filterBundle <- function(tibble, pattern){
    stopifnot(is(tibble, "tbl_df"))
    stopifnot("name" %in% colnames(tibble))
    filter(tibble, str_detect(.data[["name"]], pattern))
}
