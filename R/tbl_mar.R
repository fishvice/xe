#' @title tbl_mar
#'
#' @param con tenging við oraclegrunn
#' @param tbl nafn oracle töflu
#'
#'
#' @export
#'

tbl_mar <- function(con, tbl) {
  x <- strsplit(tbl,'\\.') %>% unlist()
  dplyr::tbl(con, dbplyr::in_schema(x[1],x[2])) %>%
    dplyr::select_all(tolower)
}
