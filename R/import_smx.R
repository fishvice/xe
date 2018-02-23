#' Title
#'
#' @param con XXX
#' @param schema XXX
#' @param id synaflokkur
#' @param gid veidarfaeri
#' @param cruise Heiti leiðangurs
#' @param debug XXX
#'
#' @export
import_smx <- function(con, schema = c("fiskar", "hafvog"), id = 30, gid = 73,
                       cruise, debug = 0) {

  if(missing(cruise)) stop("Need to specify the current cruise (Leidangur)")

  now.year <- lubridate::now() %>% lubridate::year()
  now.year <- now.year - debug

  min.towlength <- 2             # Minimum "acceptable" towlength
  max.towlength <- 8             # Maximum "acceptable" towlength
  std.towlength <- 4             # Standard tow length is 4 nautical miles

  st.list <- nu.list <- le.list <- list()
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
      select(synis_id, ar) %>%
      left_join(lesa_numer(con, schema[i])) %>%
      mutate(fj_alls = fj_maelt + fj_talid) %>%
      select(synis_id, ar, tegund, fj_maelt, fj_talid, fj_alls) %>%
      collect(n = Inf) %>%
      filter(!is.na(tegund)) %>%
      complete(synis_id, tegund) %>%
      replace_na(list(fj_maelt = 0, fj_talid = 0, fj_alls = 0))

    le.list[[i]] <-
      st %>%
      select(synis_id, ar, leidangur, reitur, tognumer, toglengd) %>%
      left_join(lesa_lengdir(con, schema[i]) %>%
                  group_by(synis_id, tegund, lengd) %>%
                  summarise(fjoldi = sum(fjoldi, na.rm = TRUE)) %>%
                  ungroup()) %>%
      collect(n = Inf)  %>%
      filter(!is.na(tegund)) %>%
      complete(synis_id, tegund) %>%
      replace_na(list(lengd = 0, fjoldi = 0))




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
             lat = (lat1 + lat2) / 2)

    le.list[[i]] <-
      le.list[[i]] %>%
      left_join(nu.list[[i]]) %>%
      mutate(r = ifelse(fjoldi == 0, 1, fj_alls/fj_maelt),
             n.rai = ifelse(fjoldi != 0, fjoldi * r, fj_alls)) %>%
      left_join(st.list[[i]] %>% select(synis_id, ar, reitur, tognumer, toglengd)) %>%
      mutate(toglengd = if_else(toglengd > max.towlength, max.towlength, toglengd),
             toglengd = if_else(toglengd < min.towlength, min.towlength, toglengd),
             n.std = n.rai * std.towlength/toglengd,
             b.std  = ifelse(is.na(n.std), 0, n.rai) * 0.00001 * lengd^3) %>%
      select(synis_id, ar, reitur, tognumer, toglengd, tegund:n.rai, n.std, b.std)

  }

  st <- bind_rows(st.list) %>% mutate(index = reitur * 10 + tognumer)
  nu <- bind_rows(nu.list)
  le <- bind_rows(le.list)

  tows.done <-
    st %>%
    filter(leidangur %in% cruise) %>%
    mutate(index = reitur * 10 + tognumer) %>%
    pull(index)
  st.done <-
    st %>%
    filter(index %in% tows.done)

  stadlar_rallstodvar <<- lesa_stadla_rallstodvar(con) %>%
    filter(veidarfaeri_id == gid,
           synaflokkur == id) %>%
    collect(n = Inf) %>%
    geo::geoconvert(col.names = c("kastad_v", "kastad_n")) %>%
    geo::geoconvert(col.names = c("hift_v",   "hift_n"))

  stadlar_tegundir <<- lesa_stadla_tegund_smb(con) %>% collect(n = Inf)

  stadlar_lw <<- lesa_stadla_lw(con) %>% collect(n = Inf)

  fisktegundir <-
    tbl_mar(con, "hafvog.fisktegundir") %>%
    select(tegund, heiti) %>%
    arrange(tegund) %>%
    collect(n = Inf)

  by.tegund.lengd.ar <-
    st.done %>%
    select(synis_id) %>%
    left_join(le) %>%
    group_by(tegund, ar, lengd) %>%
    summarise(n.std = sum(n.std, na.rm = TRUE),
              b.std = sum(b.std, na.rm = TRUE)) %>%
    ungroup()
  x <-
    by.tegund.lengd.ar %>%
    group_by(tegund) %>%
    summarise(n = n(),
              l.min = min(lengd),
              l.max = max(lengd))
  res <- list()
  for(i in 1:length(x$tegund)) {
    res[[i]] <- expand.grid(tegund = x$tegund[i],
                            lengd = x$l.min[i]:x$l.max[i],
                            ar = unique(by.tegund.lengd.ar$ar))
  }
  x <- bind_rows(res) %>% as_tibble()

  by.tegund.lengd.ar <-
    x %>%
    left_join(by.tegund.lengd.ar) %>%
    mutate(n.std = ifelse(is.na(n.std), 0, n.std),
           b.std = ifelse(is.na(b.std), 0, b.std))

  by.tegund.lengd.ar.m <-
    by.tegund.lengd.ar %>%
    filter(ar %in% 2010:2018) %>%
    group_by(tegund, lengd) %>%
    summarise(n.year = n_distinct(ar),
              n.std = sum(n.std, na.rm = TRUE) / n.year,
              b.std = sum(b.std, na.rm = TRUE) / n.year) %>%
    ungroup()

  by.station <-
    st.done %>%
    select(synis_id, lon, lat, index) %>%
    left_join(le) %>%
    group_by(ar, index, lon, lat, tegund) %>%
    summarise(n.std = sum(n.std, na.rm = TRUE),
              b.std = sum(b.std, na.rm = TRUE)) %>%
    ungroup()

  dir.create("data")
  save(by.tegund.lengd.ar, by.tegund.lengd.ar.m,
       by.station, fisktegundir, file = "data/smb_dashboard.rda")



  st <<- st
  le <<- le
  nu <<- nu
  st.done <<- st.done


}
