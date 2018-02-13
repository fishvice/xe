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
    mar::tbl_mar(con, "fiskar.stodvar") %>%
    dplyr::select(synis_id:heildarafli, synaflokkur) %>%
    dplyr::mutate(ar = to_number(to_char(dags, "YYYY"))) %>%
    #dplyr::filter(synaflokkur == 30, ar < 2017) %>%
    dplyr::left_join(mar::tbl_mar(con, "fiskar.togstodvar") %>%
                       dplyr::select(synis_id, toglengd, tognumer), by = "synis_id") %>%
    dplyr::mutate(source = "fiskar")

  d2 <-
    mar::tbl_mar(con, "hafvog.stodvar") %>%
    dplyr::select(synis_id:heildarafli, synaflokkur) %>%
    dplyr::mutate(ar = to_number(to_char(dags, "YYYY"))) %>%
    #dplyr::filter(synaflokkur == 30, ar == 2017) %>%
    dplyr::left_join(mar::tbl_mar(con, "hafvog.togstodvar") %>%
                dplyr::select(synis_id, toglengd, tognumer), by = "synis_id") %>%
    dplyr::mutate(source = "hafvog")

  d1 %>%
    dplyr::union(d2) %>%
    rename(lon1 = kastad_v_lengd,
           lon2 = hift_v_lengd,
           lat1 = kastad_n_breidd,
           lat2 = hift_n_breidd) %>%
    mar:::geoconvert(col.names = c("lat1", "lon1")) %>%
    mar:::geoconvert(col.names = c("lat2", "lon2")) %>%
    dplyr::mutate(index = reitur * 100 + tognumer)

}
