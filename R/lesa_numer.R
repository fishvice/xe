#' Title
#'
#' Bla, bla,
#'
#' @param con
#'
#' @export
#'
lesa_numer <- function(con) {

  mar::lesa_numer(con) %>%
    select(synis_id:fj_kvarnad, fj_talid:fj_magasyna) %>%
    mutate(source = "fiskar") %>%
    dplyr::union(mar::tbl_mar(con, "hafvog.numer") %>%
                   select(synis_id:fj_kvarnad, fj_talid:fj_magasyna) %>%
                   mutate(source = "hafvog"))

}
