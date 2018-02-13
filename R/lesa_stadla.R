#' Title
#'
#' The current form is just mimicry
#'
#' @param con XX
#'
#' @export
#'
lesa_stadla <- function(con) {

  tbl_mar(con, "hafvog.sti_rallstodvar") %>%
    left_join(tbl_mar(con, "hafvog.sti_leidangrar"), by = "leidangur_id")

}
