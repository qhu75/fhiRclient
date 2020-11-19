#' Search records
#'
#' @param ct The object from Client. The server to perform search.
#' @param model The resource model to build.
#' @param class The class of the resource model.
#' @param where The search parameters.
#' @param include The `include` parameter to include other resources.
#' @param page To return first or all pages of the search results.
#' @param tidy Whether to return a tibble object.
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom httr content
#' @export
#' @examples
#' \dontrun{
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
#' @param tibble The `tibble` object from search.
#' @param pattern The filter pattern to match in the name column.
#' @param column The default pattern match column.
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom rlang .data
filterBundle <- function(tibble, pattern, column = "name"){
    stopifnot(is(tibble, "tbl_df"))
    stopifnot(column %in% colnames(tibble))
    filter(tibble, str_detect(.data[[column]], pattern))
}


#' Add group by
#'
#' @param tibble The `tibble` object to group
#' @param id The index id for each group
#' @param column The column to group.
#' @importFrom dplyr group_by_at
group_index <- function(tibble, id, column = "name"){
    val <- filterBundle(tibble, id)$value
    first_idx <- grep(tibble[[column]][1], tibble[[column]])
    group_length <- c(first_idx[-1], nrow(tibble)+1) - first_idx
    idx  <- rep(val, group_length)
    tibble$idx <- idx
    colnames(tibble)[match("idx", colnames(tibble))] <- id
    group_by_at(tibble, id)
}
