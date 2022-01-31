# space for hafvog

# # want in the end tables from faeda, analogous to something like:
# res <- read_rds("~/ShinyApps/smbfyrirkongenekkiprest/data2/res.rds")
# res$other.stuff$pred
# res$other.stuff$prey
# pp <- read_rds("~/ShinyApps/smbfyrirkongenekkiprest/data2/pp.rds")



hv_pred <- function(con) {
  lesa_skraning(con) %>%
    dplyr::filter(!is.na(magaastand)) %>%
    dplyr::select(synis_id,
           pred = tegund,
           nr,
           oslaegt,
           slaegt,
           astand = magaastand) %>%
    dplyr::left_join(tbl_xe(con, "hafvog.magaastand") %>%
                dplyr::select(astand, lysing_astands),
                by = "astand") %>%
    dplyr::select(-astand) %>%
    dplyr::rename(astand = lysing_astands)
}


hv_prey <- function(con) {
  prey <-
    lesa_skraning(con) %>%
    dplyr::filter(maeliadgerd %in% c(20, 21)) %>%
    dplyr::rename(prey = tegund,
           pred = ranfiskurteg,
           pnr = nr,
           nr = kvarnanr) %>%
    #left_join(lesa_tegundir(con) %>% select(prey = tegund, heiti)) %>%
    dplyr::select(synis_id,
           pred,
           nr,
           prey,
           #heiti,
           pnr,
           n = fjoldi,
           lengd,
           kyn,
           thyngd = heildarthyngd)
  return(prey)
}
