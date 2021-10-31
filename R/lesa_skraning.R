#' Title
#'
#' @param con Oracle xe connection
#'
#' @export
lesa_skraning <- function(con) {
  tbl_mar(con, "hafvog.skraning") %>%
    dplyr::select(-c(snt:sbn))
}
