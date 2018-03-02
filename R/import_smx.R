#' Import data from the Oracle xe-database
#'
#' @param con Connection to Oracle, either mar or xe
#' @param schema default is "fiskar" and "hafvog". If only interested in reading
#' from one of them, specify which.
#' @param id synaflokkur. Default is set to 30
#' @param gid veidarfaeri. Default is set to 73
#'
#' @export
import_smx <- function(con, schema = c("fiskar", "hafvog"), id = 30, gid = 73) {


  #print(warning("This function will no longer be supported, use munge_to_smxapp"))

  #if(missing(cruise)) stop("Need to specify the current cruise (Leidangur)")

  # ----------------------------------------------------------------------------
  # Constants

  now.year <- lubridate::now() %>% lubridate::year()
  #now.year <- now.year - debug
  #min.towlength <- 2             # Minimum "acceptable" towlength
  #max.towlength <- 8             # Maximum "acceptable" towlength
  #std.towlength <- 4             # Standard tow length is 4 nautical miles


  # ----------------------------------------------------------------------------
  # IMPORT
  print("Lesa úr Hafvog")

  # A. FISKAR ------------------------------------------------------------------
  st.list <- nu.list <- le.list <- kv.list <- list()
  for(i in 1:length(schema)) {

    st <-
      lesa_stodvar(con, schema[i]) %>%
      filter(synaflokkur %in% id, veidarfaeri %in% gid)

    if(schema[i] == "fiskar") {
      st <-
        st %>%
        filter(ar < now.year)
    } else {
      st <-
        st %>%
        filter(ar == now.year)
    }

    nu.list[[i]] <-
      st %>%
      select(synis_id) %>%
      left_join(lesa_numer(con, schema[i]), by = "synis_id") %>%
      select(synis_id, tegund, fj_maelt, fj_talid, fj_alls) %>%
      collect(n = Inf) %>%
      filter(!is.na(tegund)) #%>%
      #complete(synis_id, tegund) %>%
      #replace_na(list(fj_maelt = 0, fj_talid = 0, fj_alls = 0))

    le.list[[i]] <-
      st %>%
      select(synis_id) %>%
      left_join(lesa_lengdir(con, schema[i]) %>%
                  group_by(synis_id, tegund, lengd) %>%
                  summarise(fjoldi = sum(fjoldi, na.rm = TRUE)) %>%
                  ungroup(), by = "synis_id") %>%
      collect(n = Inf)  %>%
      filter(!is.na(tegund)) #%>%
      #complete(synis_id, tegund) %>%
      #replace_na(list(lengd = 0, fjoldi = 0))

    kv.list[[i]] <-
      st %>%
      select(synis_id) %>%
      left_join(lesa_kvarnir(con, schema[i]), by = "synis_id") %>%
      collect(n = Inf)

    st.list[[i]] <-
      st %>%
      collect(n = Inf) %>%
      mutate(lon1 = -kastad_v_lengd,
             lat1 = kastad_n_breidd,
             lon2 = -hift_v_lengd,
             lat2 = hift_n_breidd) %>%
      geo::geoconvert(col.names = c("lat1", "lon1")) %>%
      geo::geoconvert(col.names = c("lat2", "lon2")) %>%
      mutate(lon = (lon1 + lon2) / 2,
             lat = (lat1 + lat2) / 2,
             toglengd = ifelse(is.na(toglengd), 4, toglengd))

    #le.list[[i]] <-
    #  le.list[[i]] %>%
    #  left_join(nu.list[[i]], by = c("synis_id", "tegund")) %>%
    #  mutate(r = ifelse(fjoldi == 0, 1, fj_alls/fj_maelt),
    #         n.rai = ifelse(fjoldi != 0, fjoldi * r, fj_alls)) %>%
    #  left_join(st.list[[i]] %>% select(synis_id, ar, reitur, tognumer, toglengd), by = "synis_id") %>%
    #  mutate(toglengd = if_else(toglengd > max.towlength, max.towlength, toglengd),
    #         toglengd = if_else(toglengd < min.towlength, min.towlength, toglengd),
    #         n.std = n.rai * std.towlength/toglengd,
    #         b.std  = ifelse(is.na(n.std), 0, n.rai) * 0.00001 * lengd^3) %>%
    #  select(synis_id, ar, reitur, tognumer, toglengd, tegund:n.rai, n.std, b.std)

  }

  st <- bind_rows(st.list) %>% mutate(index = reitur * 100 + tognumer)
  nu <- bind_rows(nu.list)
  le <- bind_rows(le.list)
  kv <- bind_rows(kv.list)

  ret <- list(st = st, nu = nu, le = le, kv = kv)

  return(ret)
}

