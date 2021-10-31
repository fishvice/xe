#' Title
#'
#' @param con Oracle xe connection
#'
#' @export
lesa_ranfiskar <- function(con) {

  lesa_skraning(con) %>%
    dplyr::filter(maeliadgerd %in% c(1:3),
                  # assume stomach is "sampled" if magastand in not NA
                  !is.na(magaastand)) %>%
    dplyr::select(synis_id:kynfaeri, x = magaastand) %>%
    dplyr::left_join(tbl_mar(con, "hafvog.magaastand") %>%
                       dplyr::select(x = astand,
                                     astand = lysing_astands)) %>%
    dplyr::select(-x) %>%
    dplyr::mutate(synis_id = -synis_id)
}
