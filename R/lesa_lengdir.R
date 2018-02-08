#' Title
#'
#' Bla, bla, ...
#'
#' @param con XXX
#'
#' @export
#'
lesa_lengdir <- function(con) {

  mar::lesa_lengdir(con) %>%
    dplyr::union(mar::tbl_mar(con, "hafvog.lengdir"))

}
