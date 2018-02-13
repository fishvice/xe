#' Title
#'
#' The current form is just mimicry
#'
#' @param con XX
#'
#' @export
#'
lesa_stodvar <- function(con) {

  d1 <-
    tbl_mar(con, "fiskar.stodvar") %>%
    dplyr::select(synis_id:heildarafli, synaflokkur) %>%
    dplyr::mutate(ar = to_number(to_char(dags, "YYYY"))) %>%
    #dplyr::filter(synaflokkur == 30, ar < 2017) %>%
    dplyr::left_join(tbl_mar(con, "fiskar.togstodvar") %>%
                       dplyr::select(synis_id:eykt), by = "synis_id") %>%
    dplyr::left_join(tbl_mar(con, "fiskar.umhverfi") %>%
                       dplyr::select(synis_id:sjondypi), by = "synis_id") %>%
    dplyr::mutate(source = "fiskar")

  d2 <-
    mar::tbl_mar(con, "hafvog.stodvar") %>%
    dplyr::select(synis_id:heildarafli, synaflokkur) %>%
    dplyr::mutate(ar = to_number(to_char(dags, "YYYY"))) %>%
    #dplyr::filter(synaflokkur == 30, ar == 2017) %>%
    dplyr::left_join(tbl_mar(con, "hafvog.togstodvar") %>%
                       dplyr::select(synis_id:eykt), by = "synis_id") %>%
    dplyr::left_join(tbl_mar(con, "hafvog.umhverfi") %>%
                       dplyr::select(synis_id:sjondypi), by = "synis_id") %>%
    dplyr::mutate(source = "hafvog")

  d1 %>%
    dplyr::union(d2) %>%
    dplyr::mutate(index = reitur * 100 + tognumer)

}
