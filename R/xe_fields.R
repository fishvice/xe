#' Get fields/column names of Oracle xe table
#'
#' @param con Connection to Oracle xe
#' @param table Table name as schema.tablename
#'
#' @export
xe_fields <- function (con, table) {

  x <- strsplit(table, "\\.") %>% unlist() %>% toupper()
  own <- x[1]
  tab <- x[2]

  tbl_xe(con, "sys.all_col_comments") %>%
    dplyr::filter(owner == own, table_name == tab) %>%
    dplyr::transmute(owner = tolower(owner),
                     table_name = tolower(table_name),
                     column_name = tolower(column_name),
                     comments = comments)

}
