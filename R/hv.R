# space for hafvog

hv_pred <- function(con) {
  lesa_skraning(con) %>%
    filter(!is.na(magaastand)) %>%
    select(synis_id,
           pred = tegund,
           nr,
           oslaegt,
           slaegt,
           astand = magaastand) %>%
    left_join(tbl_mar(con, "hafvog.magaastand") %>%
                select(astand, lysing_astands)) %>%
    select(-astand) %>%
    rename(astand = lysing_astands)
}


hv_prey <- function(con) {
  prey <-
    lesa_skraning(con) %>%
    filter(maeliadgerd %in% c(20, 21)) %>%
    rename(prey = tegund,
           pred = ranfiskurteg,
           pnr = nr,
           nr = kvarnanr) %>%
    #left_join(lesa_tegundir(con) %>% select(prey = tegund, heiti)) %>%
    select(synis_id,
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
