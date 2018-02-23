#' Title
#'
#' @param con XXX
#' @param table XXX
#'
#' @export
ora_fields <- function (con, table) {

  x <- strsplit(table, "\\.") %>% unlist()
  ora_fields(con, "sys.all_col_comments") %>%
    dplyr::filter(owner == toupper(x[1]), table_name == toupper(x[2])) %>%
    dplyr::transmute(owner = lower(owner),
                     table_name = lower(table_name),
                     column_name = lower(column_name),
                     comments = comments)

}