#' Import data from the Oracle xe-database
#'
#' @param id synaflokkur. Default is set to 30
#' @param gid veidarfaeri. Default is set to 73
#' @param year Current cruise year. If not specificed (default) use current (computer) year.
#' @param store A boolean, if TRUE then the returned returned object is also
#' saved as hafvog.rds in directory data
#'
#' @export
import_smx <- function(id = 30, gid = 73, year, store = FALSE) {

  schema <- c("fiskar", "hafvog")

  # ----------------------------------------------------------------------------
  # Constants
  if(missing(year)) {
    now.year <- lubridate::now() %>% lubridate::year()
  } else {
    now.year <- year
  }


  # ----------------------------------------------------------------------------
  # IMPORT
  print("Lesa ur XE-gagnagrunninum")

  # A. FISKAR ------------------------------------------------------------------
  st.list <- nu.list <- le.list <- kv.list <- list()
  for(i in 1:length(schema)) {

    #if(schema[i] == "fiskar") {
    #  user <- password <- "gagnasja"
    #} else {
      user <- "hafvog_user"
      password <- "hafvog"
    #}
    con <- DBI::dbConnect(DBI::dbDriver("Oracle"),
                            user = user,
                            password = password,
                            host = "localhost",
                            port = 1521,
                            dbname = "xe")


    st <-
      lesa_stodvar(con, schema[i]) %>%
      dplyr::filter(synaflokkur %in% id, veidarfaeri %in% gid)

    if(st %>% dplyr::count() %>% dplyr::collect() == 0) {
      stop("There is no data for specified 'synaflokkur' (id) and 'veidarfaeri' (gid)")
    }

    if(schema[i] == "fiskar") {
      st <-
        st %>%
        dplyr::filter(ar < now.year)
    } else {
      st <-
        st %>%
        dplyr::filter(ar == now.year)
    }

    nu.list[[i]] <-
      st %>%
      dplyr::select(synis_id) %>%
      dplyr::left_join(lesa_numer(con, schema[i]), by = "synis_id") %>%
      dplyr::select(synis_id, tegund, fj_maelt, fj_talid, fj_alls) %>%
      dplyr::collect(n = Inf) %>%
      dplyr::filter(!is.na(tegund))

    le.list[[i]] <-
      st %>%
      dplyr::select(synis_id) %>%
      dplyr::left_join(lesa_lengdir(con, schema[i]) %>%
                         dplyr::group_by(synis_id, tegund, lengd) %>%
                         dplyr::summarise(fjoldi = sum(fjoldi, na.rm = TRUE)) %>%
                         dplyr::ungroup(), by = "synis_id") %>%
      dplyr::collect(n = Inf)  %>%
      dplyr::filter(!is.na(tegund))

    kv.list[[i]] <-
      st %>%
      dplyr::select(synis_id) %>%
      dplyr::left_join(lesa_kvarnir(con, schema[i]), by = "synis_id") %>%
      dplyr::collect(n = Inf)

    st.list[[i]] <-
      st %>%
      dplyr::collect(n = Inf) %>%
      dplyr::mutate(lon1 = -kastad_v_lengd,
                    lat1 = kastad_n_breidd,
                    lon2 = -hift_v_lengd,
                    lat2 = hift_n_breidd) %>%
      geo::geoconvert(col.names = c("lat1", "lon1")) %>%
      geo::geoconvert(col.names = c("lat2", "lon2")) %>%
      dplyr::mutate(lon = (lon1 + lon2) / 2,
                    lat = (lat1 + lat2) / 2,
                    toglengd = ifelse(is.na(toglengd), 4, toglengd))

  }

  st <-
    dplyr::bind_rows(st.list) %>%
    dplyr::mutate(index = reitur * 100 + tognumer)
  nu <- dplyr::bind_rows(nu.list)
  le <- dplyr::bind_rows(le.list)
  kv <- dplyr::bind_rows(kv.list)

  skraning <-
    tbl_xe(con, "hafvog.stodvar") %>%
    dplyr::select(synis_id:heildarafli, synaflokkur) %>%
    dplyr::mutate(ar = to_number(to_char(dags, "YYYY"))) %>%
    dplyr::left_join(tbl_xe(con, "hafvog.togstodvar") %>%
                       dplyr::select(synis_id:eykt), by = "synis_id") %>%
    dplyr::left_join(tbl_xe(con, "hafvog.umhverfi") %>%
                       dplyr::select(synis_id:sjondypi), by = "synis_id") %>%
    dplyr::mutate(index = reitur * 100 + tognumer) %>%
    dplyr::filter(synaflokkur %in% id,
                  veidarfaeri %in% gid,
                  ar == now.year) %>%
    dplyr::select(synis_id) %>%
    dplyr::left_join(tbl_xe(con, "hafvog.skraning"),
                     by = "synis_id") %>%
    dplyr::collect(n = Inf) %>%
    dplyr::mutate(synis_id = -synis_id)

  # ----------------------------------------------------------------------------
  # Other stuff needed from hafvog
  # B. STADLAR -----------------------------------------------------------------

  stadlar.rallstodvar <-
    lesa_stadla_rallstodvar(con) %>%
    dplyr::filter(veidarfaeri_id %in% gid,
                  synaflokkur %in% id) %>%
    dplyr::collect(n = Inf) %>%
    # fix an error in hift_v for SMH, should be corrected in database
    dplyr::mutate(hift_v = ifelse(hift_v == -2444550, -244455, hift_v)) %>%
    geo::geoconvert(col.names = c("kastad_v", "kastad_n")) %>%
    geo::geoconvert(col.names = c("hift_v",   "hift_n"))

  lid <- stadlar.rallstodvar$leidangur_id[[1]]
  stadlar.tegundir <-
    lesa_stadla_tegund_smb(con) %>%
    dplyr::filter(leidangur_id == lid) %>%
    dplyr::arrange(tegund) %>%
    dplyr::collect(n = Inf) %>%
    tidyr::gather(variable, value, lifur_low:kynkirtlar_high) %>%
    dplyr::mutate(value = value / 100) %>%
    tidyr::spread(variable, value)

  stadlar.lw <-
    lesa_stadla_lw(con) %>%
    dplyr::collect(n = Inf) %>%
    dplyr::mutate(osl1 = osl * (1 - fravik),
                  osl2 = osl * (1 + fravik),
                  sl1 = sl * (1 - fravik),
                  sl2 = sl * (1 + fravik)) %>%
    dplyr::select(tegund, lengd, osl1:sl2)

  fisktegundir <-
    tbl_xe(con, "hafvog.fisktegundir") %>%
    dplyr::select(tegund, heiti) %>%
    dplyr::arrange(tegund) %>%
    dplyr::collect(n = Inf)

  aid <-
    tbl_xe(con, "hafvog.maeliatridi") %>%
    dplyr::collect() %>%
    dplyr::rename(aid = id, adgerd = heiti) %>%
    dplyr::collect(n = Inf)
  sid <-
    tbl_xe(con, "hafvog.fisktegundir") %>%
    dplyr::select(sid = tegund, tegund = heiti) %>%
    dplyr::arrange(tegund) %>%
    dplyr::collect(n = Inf)
  prey_names <-
    tbl_xe(con, "hafvog.f_tegundir") %>%
    dplyr::collect(n = Inf)


  pred <-
    hv_pred(con) %>%
    dplyr::collect(n = Inf)
  prey <-
    hv_prey(con) %>%
    dplyr::collect(n = Inf)

  other.stuff <- list(stadlar.rallstodvar = stadlar.rallstodvar,
                      stadlar.tegundir = stadlar.tegundir,
                      stadlar.lw = stadlar.lw,
                      fisktegundir = fisktegundir,
                      aid = aid,
                      sid = sid,
                      prey_names = prey_names,
                      prey = prey,
                      pred = pred)



  ret <- list(st = st, nu = nu, le = le, kv = kv, skraning = skraning,
              other.stuff = other.stuff)

  if(store) {

    if(!dir.exists("data2")) dir.create("data2")
    ret %>% readr::write_rds(path = "data2/hafvog.rds")
  }

  return(ret)
}

