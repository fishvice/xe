#' Title
#'
#' @param con
#'
#' @export
#'
brad_lengdir <- function(con) {
  lesa_skraning(con) %>%
    dplyr::filter(maeliadgerd %in% c(22)) %>%
    dplyr::select(synis_id,
                  # prey id
                  pid = tegund,
                  # prey count
                  pN = fjoldi,
                  # prey length
                  pL = lengd,
                  # prey weight
                  pW = heildarthyngd,
                  tegund = ranfiskurteg,
                  nr = kvarnanr)
}
