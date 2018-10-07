#' Title
#'
#' bla, bla
#'
#' @param con XX
#'
#' @export
#'
lesa_stadla_tegund_smb <- function(con) {

  tbl_mar(con, "hafvog.fiskteg_tegundir") %>%
    dplyr::rename(tegund = fisktegund_id)

}


