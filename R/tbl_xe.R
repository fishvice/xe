#' @title Get Oracel xe table
#'
#' @param con Connection to Oracle xe
#' @param tbl Table name
#'
#'
#' @export
#'

tbl_xe <- function(con, tbl) {
  x <- strsplit(tbl,'\\.') %>% unlist()
  dplyr::tbl(con, dbplyr::in_schema(x[1],x[2])) %>%
    dplyr::select_all(tolower)
}
