#' Title
#'
#' Some text
#'
#' @param con XX
#'
#' @export
#'
lesa_kvarnir <- function(con) {

  tbl_mar(con, "fiskar.kvarnir") %>%
    dplyr::select(synis_id:kynfaeri, lifur, magi, syking = sy) %>%
    mutate(source = "fiskar") %>%
    dplyr::union(tbl_mar(con, "hafvog.kvarnir") %>%
                   dplyr::select(synis_id:kynfaeri, lifur, magi, syking = sy) %>%
                   mutate(source = "hafvog"))

}
