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
    rename(tegund = fisktegund_id) %>%
    mutate(fravik = fravik/100) %>%
    collect(n = Inf)
  x <- d %>% group_by(tegund) %>% summarise(l.max = max(lengd))

  expand.grid(tegund = unique(d$tegund),
                lengd = 1:1500) %>%
    as_tibble() %>%
    left_join(x) %>%
    filter(lengd <= l.max) %>%
    select(-l.max) %>%
    left_join(d) %>%
    arrange(tegund, -lengd) %>%
    group_by(tegund) %>%
    fill(oslaegt_a:fravik) %>%
    ungroup() %>%
    mutate(osl = oslaegt_a * lengd^oslaegt_b,
           sl = slaegt_a * lengd^slaegt_b)

}
