#' Title
#'
#' bla, bla
#'
#' @param con XX
#'
#' @export
#'
lesa_stadla_lw <- function(con) {

  d <-
    tbl_mar(con, "hafvog.fiskteg_lengd_thyngd") %>%
    dplyr::rename(tegund = fisktegund_id) %>%
    dplyr::mutate(fravik = fravik/100) %>%
    dplyr::collect(n = Inf)
  x <-
    d %>%
    dplyr::group_by(tegund) %>%
    dplyr::summarise(l.max = max(lengd))

  expand.grid(tegund = unique(d$tegund),
              lengd = 1:1500) %>%
    dplyr::as_tibble() %>%
    dplyr::left_join(x, by = "tegund") %>%
    dplyr::filter(lengd <= l.max) %>%
    dplyr::select(-l.max) %>%
    dplyr::left_join(d, by = c("tegund", "lengd")) %>%
    dplyr::arrange(tegund, -lengd) %>%
    dplyr::group_by(tegund) %>%
    tidyr::fill(oslaegt_a:fravik) %>%
    dplyr::ungroup() %>%
    dplyr:: mutate(osl = oslaegt_a * lengd^oslaegt_b,
                   sl = slaegt_a * lengd^slaegt_b)

}
