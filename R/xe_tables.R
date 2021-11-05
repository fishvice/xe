#' Get Oracle xe table names for a scheme
#'
#' @param con Connection to Oracle xe
#' @param schema Scheme name
#'
#' @export
xe_tables <- function (con, schema)
{
  d <- tbl_xe(con, "sys.all_tables")
  if (!missing(schema)) {
    d <-
      d %>%
      dplyr::filter(owner %in% toupper(schema))
  }
  d %>%
    dplyr::select(owner, table_name, tablespace_name, num_rows,
                  last_analyzed) %>% dplyr::left_join(tbl_xe(con, "sys.all_tab_comments")) %>%
    dplyr::mutate(owner = lower(owner), table_name = lower(table_name)) %>%
    dplyr::select(owner, table_name, comments, tablespace_name,
                  num_rows, last_analyzed)
}
