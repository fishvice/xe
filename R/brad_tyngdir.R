#' Title
# ATH: Þarf að bæta við þyngdum úr sparimælingu (maeliadgerd 22)
#'
#' @param con
#'
#' @export
#'
brad_thyngdir <- function(con) {
  lesa_skraning(con) %>%
    dplyr::filter(maeliadgerd %in% c(20:21)) %>%
    dplyr::select(synis_id,
                  # prey id
                  pid = tegund,
                  # prey count
                  pN = fjoldi,
                  # prey weight
                  pW = heildarthyngd,
                  tegund = ranfiskurteg,
                  nr = kvarnanr) %>%
    dplyr::mutate(synis_id = -synis_id)
}
