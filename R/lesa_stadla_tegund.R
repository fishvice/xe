#' Title
#'
#' bla, bla
#'
#' @param con XX
#'
#' @export
#'
lesa_stadla_tegund_smb <- function(con) {

  d <-
    tbl_mar(con, "hafvog.fiskteg_tegundir") %>%
    filter(leidangur_id == 1) %>%
    rename(tegund = fisktegund_id) %>%
    arrange(tegund) %>%
    mutate(r.sl.osl_low = oslaegt_slaegt_low/100,
           r.sl.osl_high = oslaegt_slaegt_high/100,
           r.vigtad.osl_low = oslaegt_vigtad_low/100,
           r.vigtad.osl_high = oslaegt_vigtad_high/100) %>%
    select(tegund, lengd_low:magi_high, kynkirtlar_low, kynkirtlar_high:r.vigtad.osl_high) %>%
    collect(n = Inf)
  #d %>%
  #  gather(variable, value, -tegund) %>%
  #  separate(variable, c("variable", "limit"), sep = "_") %>%
  #  spread(limit, value)

  return(d)

}


