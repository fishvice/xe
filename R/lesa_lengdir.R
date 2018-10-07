#' Title
#'
#' Bla, bla, ...
#'
#' @param con XXX
#' @param schema XXX
#'
#' @export
#'
lesa_lengdir <- function(con, schema = "fiskar") {

  tbl_mar(con, paste0(schema, ".lengdir")) %>%
    dplyr::select(synis_id:kynthroski) %>%
    dplyr::mutate(synis_id = dplyr::if_else(schema == "fiskar", synis_id, -synis_id))

}
