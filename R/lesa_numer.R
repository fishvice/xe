#' Title
#'
#' Bla, bla,
#'
#' @param con
#'
#' @export
#'
lesa_numer <- function(con) {

  tbl_mar(con, "fiskar.numer") %>%
    select(synis_id:fj_kvarnad, fj_talid:fj_magasyna) %>%
    mutate(source = "fiskar") %>%
    dplyr::union(tbl_mar(con, "hafvog.numer") %>%
                   select(synis_id:fj_kvarnad, fj_talid:fj_magasyna) %>%
                   mutate(synis_id = -synis_id,
                          source = "hafvog"))


}
