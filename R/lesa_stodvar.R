#' Title
#'
#' The current form is just mimicry
#'
#' @param con XX
#' @param schema XX
#'
#' @export
#'
lesa_stodvar <- function(con, schema = "fiskar") {

  tbl_mar(con, paste0(schema, ".stodvar")) %>%
    dplyr::select(synis_id:heildarafli, synaflokkur) %>%
    dplyr::mutate(ar = to_number(to_char(dags, "YYYY"))) %>%
    dplyr::left_join(tbl_mar(con, paste0(schema, ".togstodvar")) %>%
                       dplyr::select(synis_id:eykt), by = "synis_id") %>%
    dplyr::left_join(tbl_mar(con, paste0(schema, ".umhverfi")) %>%
                       dplyr::select(synis_id:sjondypi), by = "synis_id") %>%
    dplyr::mutate(synis_id = dplyr::if_else(schema == "fiskar", synis_id, -synis_id),
                  index = reitur * 100 + tognumer)

}
