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
    rename(tegund = fisktegund_id)

}


