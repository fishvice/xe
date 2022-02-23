#' Title
#'
#' @param con XX
#' @param schema XX
#'
#' @return A query
#' @export
#'
xe_views <- function (con, schema) {
  d <-
    dplyr::tbl(con, dplyr::sql("select OWNER, VIEW_NAME from sys.all_views")) %>%
    dplyr::rename(owner = OWNER, view_name = VIEW_NAME)
  if (!missing(schema)) {
    d <- d %>% dplyr::filter(owner %in% toupper(schema))
  }
  d %>% dplyr::mutate(owner = tolower(owner), view_name = tolower(view_name))
}