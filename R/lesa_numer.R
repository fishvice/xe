#' Title
#'
#' Bla, bla,
#'
#' @param con XXX
#' @param schema XXX
#'
#' @export
#'
lesa_numer <- function(con, schema = "fiskar") {

  tbl_mar(con, paste0(schema, ".numer")) %>%
    dplyr::select(synis_id, tegund, fj_maelt, fj_talid, fj_kyngreint,
                  fj_vigtad, fj_magasyna) %>%
    dplyr::mutate(synis_id = if_else(schema == "fiskar", synis_id, -synis_id),
                  fj_maelt = if_else(is.na(fj_maelt), 0, fj_maelt),
                  fj_talid = if_else(is.na(fj_talid), 0, fj_talid),
                  fj_kyngreint = if_else(is.na(fj_kyngreint), 0, fj_kyngreint),
                  fj_vigtad = if_else(is.na(fj_vigtad), 0, fj_vigtad),
                  fj_magasyna = if_else(is.na(fj_magasyna), 0, fj_vigtad),
                  fj_alls = fj_maelt + fj_talid)

}
