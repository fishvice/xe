#' @title mar_tables
#'
#' @description Fallid myndar tengingu við schema all_tables
#'
#' @param mar src_oracle tenging við oracle
#' @param schema character vector specifying schema name, e.g. "fiskar"
#'
#' @export
mar_tables <- function(mar, schema) {

  d <- tbl_mar(mar, "sys.all_tables")

  if(!missing(schema)) {
    d <-
      d %>%
      dplyr::filter(owner %in% toupper(schema))
  }

  d %>%
    dplyr::select(owner, table_name, tablespace_name, num_rows, last_analyzed) %>%
    dplyr::left_join(tbl_mar(mar,'sys.all_tab_comments')) %>%
    dplyr::mutate(owner = lower(owner),
                  table_name = lower(table_name)) %>%
    dplyr::select(owner, table_name, comments, tablespace_name, num_rows, last_analyzed)

}

#' @title mar_fields
#'
#' @description Fallid myndar tengingu
#'
#' @param mar src_oracle tenging við oracle
#' @param table character vector specifying table names, e.g. "fiskar.stodvar"
#'
#' @export
mar_fields <- function(mar, table) {

  x <- strsplit(table,'\\.') %>% unlist()

  tbl_mar(mar,'sys.all_col_comments') %>%
    dplyr::filter(owner == toupper(x[1]),
                  table_name == toupper(x[2])) %>%
    dplyr::transmute(owner = lower(owner),
                     table_name = lower(table_name),
                     column_name = lower(column_name),
                     comments = comments)
}
