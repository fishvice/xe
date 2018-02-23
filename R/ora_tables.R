#' Title
#'
#' @param con XXX
#' @param schema XXX
#'
#' @export
ora_tables <- function (con, schema)
{
  d <- tbl_mar(con, "sys.all_tables")
  if (!missing(schema)) {
    d <- d %>% filter(owner %in% toupper(schema))
  }
  d %>% dplyr::select(owner, table_name, tablespace_name, num_rows,
                      last_analyzed) %>% dplyr::left_join(tbl_mar(con, "sys.all_tab_comments")) %>%
    dplyr::mutate(owner = lower(owner), table_name = lower(table_name)) %>%
    dplyr::select(owner, table_name, comments, tablespace_name,
                  num_rows, last_analyzed)
}