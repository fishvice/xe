#' Title
#'
#' The current form is just mimicry
#'
#' @param con XX
#'
#' @export
#'
lesa_stadla_rallstodvar <- function(con) {

  tbl_xe(con, "hafvog.sti_rallstodvar") %>%
    dplyr::left_join(tbl_xe(con, "hafvog.sti_leidangrar"), by = "leidangur_id") %>%
    dplyr::mutate(kastad_v = -kastad_v,
                  hift_v = -hift_v,
                  index = reitur * 100 + tognumer)

}
