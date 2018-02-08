#' Title
#'
#' Some text
#'
#' @param con XX
#'
#' @export
#'
lesa_kvarnir <- function(con) {

  mar::lesa_kvarnir(con) %>%
    dplyr::select(synis_id:kynfaeri, lifur, magi) %>%
    dplyr::union(tbl_mar(con, "hafvog.kvarnir") %>%
            dplyr::select(synis_id:kynfaeri, lifur, magi))

}
