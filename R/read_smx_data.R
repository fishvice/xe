#' @export
read_smx_data <- function(con, Leidangur, id = 30, gid = 73) {

  if(missing(Leidangur)) stop("You need to specify the current cruise name")

  min.towlength <- 2             # Minimum "acceptable" towlength
  max.towlength <- 8             # Maximum "acceptable" towlength
  std.towlength <- 4             # Standard tow length is 4 nautical miles

  st <-
    lesa_stodvar(con, Leidangur = Leidangur) %>%
    filter(synaflokkur == id,
           veidarfaeri == gid)

  # ----------------------------------------------------------------
  # in test case, the 2017 data both xe.fiskar.stodvar
  #    and xe.hafvog.stodvar a dummy filter is applied here. Would
  #    not be needed once smb2018 is up.
  if(dummy) {
    st <-
      st %>%
      dplyr::filter((ar < year.now & source == "fiskar") |
               (ar == year.now & source == "hafvog"))
  }
  # ----------------------------------------------------------------

  le <<-
    st %>%
    select(synis_id, ar, source, toglengd) %>%
    left_join(lesa_lengdir(con), by = c("synis_id", "source")) %>%
    group_by(synis_id, ar, source, toglengd, tegund, lengd) %>%
    summarise(fjoldi = sum(fjoldi, na.rm = TRUE)) %>%
    ungroup() %>%
    left_join(lesa_numer(con) %>%
                dplyr::mutate(r = ifelse(fj_talid==0 | is.na(fj_talid),
                                         1,
                                         1 + fj_talid / ifelse(fj_maelt == 0 | is.na(fj_maelt), 1, fj_maelt))) %>%
                dplyr::select(synis_id, tegund, r),
              by = c("synis_id", "tegund")) %>%
    mutate(n = r * fjoldi / 1e3) %>%
    mutate(toglengd = if_else(toglengd > max.towlength, max.towlength, toglengd),
           toglengd = if_else(toglengd < min.towlength, min.towlength, toglengd)) %>%
    mutate(n.std = n * std.towlength/toglengd,
           b.std  = ifelse(is.na(n.std), 0, n) * 0.01 * lengd^3/1e3) %>%
    collect(n = Inf)

  kv <<-
    st %>%
    select(synis_id, ar, source) %>%
    left_join(lesa_kvarnir(con), by = c("synis_id", "source")) %>%
    collect(n = Inf)

  nu <<-
    st %>%
    select(synis_id, ar, source) %>%
    left_join(lesa_numer(con), by = c("synis_id", "source")) %>%
    collect(n = Inf)

  st <-
    st %>%
    rename(lon1 = kastad_v_lengd,
           lat1 = kastad_n_breidd,
           lon2 = hift_v_lengd,
           lat2 = hift_n_breidd)

  index.current <-
    st %>%
    filter(ar == year.now) %>%
    # test fix
    mutate(dd = to_number(to_char(dags, "DD")),
           mm = to_number(to_char(dags, "MM"))) %>%
    #filter(mm == 3 & dd <= 16 | mm == 2) %>%
    select(index) %>%
    filter(!is.na(index)) %>%
    collect(n = Inf) %>%
    pull()

  # only tows (index) that have so far been taken this year
  st.done <<-
    st %>%
    filter(index %in% index.current) %>%
    collect(n = Inf) %>%
    geo::geoconvert(col.names = c("lat1", "lon1")) %>%
    geo::geoconvert(col.names = c("lat2", "lon2")) %>%
    mutate(lon = (lon1 + lon2) / 2,
           lat = (lat1 + lat2) / 2)

  st <<-
    st %>%
    collect(n = Inf) %>%
    geo::geoconvert(col.names = c("lat1", "lon1")) %>%
    geo::geoconvert(col.names = c("lat2", "lon2")) %>%
    mutate(lon = (lon1 + lon2) / 2,
           lat = (lat1 + lat2) / 2)

  stadlar_rallstodvar <<- lesa_stadla_rallstodvar(con) %>%
    filter(veidarfaeri_id == gid,
           synaflokkur == id) %>%
    collect(n = Inf) %>%
    geo::geoconvert(col.names = c("kastad_v", "kastad_n")) %>%
    geo::geoconvert(col.names = c("hift_v",   "hift_n"))

  stadlar_tegundir <<- lesa_stadla_tegund_smb(con) %>% collect(n = Inf)

  stadlar_lw <<- lesa_stadla_lw(con) %>% collect(n = Inf)

  fisktegundir <<-
    tbl_mar(con, "hafvog.fisktegundir") %>%
    select(tegund, heiti) %>%
    arrange(tegund) %>%
    collect()

}
