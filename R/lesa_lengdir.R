#' Title
#'
#' Bla, bla, ...
#'
#' @param con XXX
#'
#' @export
#'
lesa_lengdir <- function(con) {

  d1 <-
    mar::tbl_mar(con, "fiskar.lengdir") %>%
    dplyr::select(-c(snn:sbt)) %>%
    right_join(mar::tbl_mar(con, "fiskar.stodvar") %>%
                 dplyr::mutate(ar = to_number(to_char(dags, "YYYY"))) %>%
                 dplyr::select(synis_id), by = "synis_id") %>%
    left_join(mar::tbl_mar(con, "fiskar.numer") %>%
                dplyr::mutate(r = ifelse(fj_talid==0 | is.na(fj_talid), 1, 1 + (fj_talid / ifelse(fj_maelt == 0 | is.na(fj_talid), 1, fj_maelt)))) %>%
                dplyr::select(synis_id, tegund, r), by = c("synis_id", "tegund")) %>%
    mutate(source = "fiskar")


  d2 <-
    mar::tbl_mar(con, "hafvog.lengdir") %>%
    right_join(mar::tbl_mar(con, "hafvog.stodvar") %>%
                 dplyr::mutate(ar = to_number(to_char(dags, "YYYY"))) %>%
                 dplyr::select(synis_id), by = "synis_id") %>%
    left_join(mar::tbl_mar(con, "hafvog.numer") %>%
                dplyr::mutate(r = ifelse(fj_talid==0 | is.na(fj_talid), 1, 1 + (fj_talid / ifelse(fj_maelt == 0 | is.na(fj_talid), 1, fj_maelt)))) %>%
                dplyr::select(synis_id, tegund, r), by = c("synis_id", "tegund")) %>%
    mutate(source = "hafvog")

  d1 %>%
    dplyr::union(d2)

}
