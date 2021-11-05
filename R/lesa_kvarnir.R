#' Title
#'
#' Some text
#'
#' @param con XXX
#' @param schema XXX
#'
#' @export
#'
lesa_kvarnir <- function(con, schema = "fiskar") {

  tbl_xe(con, paste0(schema, ".kvarnir")) %>%
    dplyr::select(synis_id:kynfaeri, lifur, magi, syking = sy) %>%
    dplyr::mutate(synis_id = if_else(schema == "fiskar", synis_id, -synis_id))

}
