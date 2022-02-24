#' Title
#'
#' The current form is just mimicry
#'
#' @param con XX
#' @param schema XX
#'
#' @export
#'
lesa_stodvar <- function(con, schema = "hafvog") {

  tbl_xe(con, paste0(schema, ".stodvar")) %>%
    dplyr::select(synis_id:heildarafli, synaflokkur, fishing_gear_no) %>%
    dplyr::mutate(ar = to_number(to_char(dags, "YYYY")),
                  veidarfaeri = ifelse(is.na(veidarfaeri),
                                       fishing_gear_no,
                                       veidarfaeri)) %>%
    dplyr::left_join(tbl_xe(con, paste0(schema, ".togstodvar")) %>%
                       dplyr::select(synis_id:eykt), by = "synis_id") %>%
    dplyr::left_join(tbl_xe(con, paste0(schema, ".umhverfi")) %>%
                       dplyr::select(synis_id:sjondypi), by = "synis_id") %>%
    dplyr::mutate(synis_id = ifelse(schema == "fiskar", synis_id, -synis_id),
                  index = reitur * 100 + tognumer)

}
