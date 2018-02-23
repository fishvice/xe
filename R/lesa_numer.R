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
    select(synis_id, tegund, fj_maelt, fj_talid, fj_kyngreint,
           fj_vigtad, fj_magasyna) %>%
    mutate(synis_id = if_else(schema == "fiskar", synis_id, -synis_id))

}
