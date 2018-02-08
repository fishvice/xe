#' Title
#'
#' Some text
#'
#' @param con XX
#'
#' @export
#'
lesa_stodvar <- function(con) {

  mar::tbl_mar(con, "fiskar.stodvar") %>%
    dplyr::select(synis_id:heildarafli) %>%
    dplyr::union(mar::tbl_mar(con, "hafvog.stodvar") %>%
                   dplyr::select(synis_id:heildarafli)) %>%
    mutate(ar = to_number(to_char(dags, "YYYY")))

}
