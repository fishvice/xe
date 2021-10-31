# predator prey functions

predator_faeda <- function(con) {

  tbl_mar(con,'faeda.f_fiskar') %>%
    dplyr::mutate(astand = dplyr::case_when(fj_omelt == 1 ~ 4L,
                                            fj_tomra == 1 ~ 2L,
                                            fj_aelt  == 1 ~ 3L,
                                            fj_uthverfir == 1 ~ 5L,
                                            TRUE ~ 1L)) %>%
    dplyr::select(synis_id,
                  prey.id = flokk_id,
                  pred = ranfiskur,
                  length = lengd,
                  knr = kvarnanr,
                  astand)

}

prey_faeda <- function(con) {

  tbl_mar(con,'faeda.f_hopar') %>%
    dplyr::mutate(prey.id = flokk_id,
                  prey = faeduhopur,
                  n.prey = fjoldi,
                  w.prey = thyngd)

}

predator_hafvog <- function(con) {

  lesa_skraning(con) %>%
    filter(!is.na(magaastand)) %>%
    dplyr::select(synis_id,
           pred = tegund,
           nr,
           knr = kvarnanr,
           length = lengd,
           w = oslaegt,
           gW = slaegt,
           astand = magaastand)
}



prey_hafvog <- function(con) {
  lesa_skraning(con) %>%
    dplyr::filter(maeliadgerd %in% c(20, 21)) %>%
    dplyr::select(synis_id,
           pred = ranfiskurteg,
           knr = kvarnanr,
           prey = tegund,
           pnr = nr,
           n.prey = fjoldi,
           length.prey = lengd,
           sex.prey = kyn,
           w.prey = heildarthyngd)
}
