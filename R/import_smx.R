#' Import data from the Oracle xe-database
#'
#' @param con Connection to Oracle, either mar or xe
#' @param year Current cruise year
#' @param schema default is "fiskar" and "hafvog". If only interested in reading
#' from one of them, specify which.
#' @param id synaflokkur. Default is set to 30
#' @param gid veidarfaeri. Default is set to 73
#'
#' @export
import_smx <- function(con, year = year(now()), schema = c("fiskar", "hafvog"), id = 30, gid = 73) {


  # ----------------------------------------------------------------------------
  # Constants

  now.year <- year
  #now.year <- now.year - debug
  #min.towlength <- 2             # Minimum "acceptable" towlength
  #max.towlength <- 8             # Maximum "acceptable" towlength
  #std.towlength <- 4             # Standard tow length is 4 nautical miles


  # ----------------------------------------------------------------------------
  # IMPORT
  print("Lesa ur XE-gagnagrunninum")

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

  skraning <-
    tbl_mar(con, "hafvog.skraning") %>%
    collect(n = Inf) %>%
    mutate(synis_id = -synis_id)

  # ----------------------------------------------------------------------------
  # Other stuff needed from hafvog
  # B. STADLAR -----------------------------------------------------------------

  stadlar.rallstodvar <-
    lesa_stadla_rallstodvar(con) %>%
    filter(veidarfaeri_id == gid,
           synaflokkur == id) %>%
    collect(n = Inf) %>%
    geo::geoconvert(col.names = c("kastad_v", "kastad_n")) %>%
    geo::geoconvert(col.names = c("hift_v",   "hift_n"))

  stadlar.tegundir <-
    lesa_stadla_tegund_smb(con) %>%
    filter(leidangur_id == 0) %>%
    arrange(tegund) %>%
    collect(n = Inf) %>%
    gather(variable, value, lifur_low:kynkirtlar_high) %>%
    mutate(value = value / 100) %>%
    spread(variable, value)

  stadlar.lw <-
    lesa_stadla_lw(con) %>%
    collect(n = Inf) %>%
    mutate(osl1 = osl * (1 - fravik),
           osl2 = osl * (1 + fravik),
           sl1 = sl * (1 - fravik),
           sl2 = sl * (1 + fravik)) %>%
    select(tegund, lengd, osl1:sl2)

  fisktegundir <-
    tbl_mar(con, "hafvog.fisktegundir") %>%
    select(tegund, heiti) %>%
    arrange(tegund) %>%
    collect(n = Inf)

  aid <-
    tbl_mar(con, "hafvog.maeliatridi") %>%
    collect() %>%
    rename(aid = id, adgerd = heiti) %>%
    collect(n = Inf)
  sid <-
    tbl_mar(con, "hafvog.fisktegundir") %>%
    select(sid = tegund, tegund = heiti) %>%
    arrange(tegund) %>%
    collect(n = Inf)
  prey <-
    tbl_mar(con, "hafvog.f_tegundir") %>%
    collect(n = Inf)

  other.stuff <- list(stadlar.rallstodvar = stadlar.rallstodvar,
                      stadlar.tegundir = stadlar.tegundir,
                      stadlar.lw = stadlar.lw,
                      fisktegundir = fisktegundir,
                      aid = aid,
                      sid = sid,
                      prey = prey)

  ret <- list(st = st, nu = nu, le = le, kv = kv, skraning = skraning,
              other.stuff = other.stuff)

  return(ret)
}

