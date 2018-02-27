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
#cruise = c("A5-2017", "B3-2017", "TB1-2017", "TL1-2017"); schema = "fiskar"; debug = 0 ; id = 30; gid = 73
import_smx <- function(con, schema = c("fiskar", "hafvog"), id = 30, gid = 73,
                       cruise, debug = 0) {

  if(missing(cruise)) stop("Need to specify the current cruise (Leidangur)")

  # ----------------------------------------------------------------------------
  # Constants

  now.year <- lubridate::now() %>% lubridate::year()
  now.year <- now.year - debug
  min.towlength <- 2             # Minimum "acceptable" towlength
  max.towlength <- 8             # Maximum "acceptable" towlength
  std.towlength <- 4             # Standard tow length is 4 nautical miles


  # ----------------------------------------------------------------------------
  # IMPORT

  # A. FISKAR ------------------------------------------------------------------
  st.list <- nu.list <- le.list <- kv.list <- list()
  for(i in 1:length(schema)) {

    print(i)

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
      left_join(lesa_numer(con, schema[i])) %>%
      mutate(fj_alls = fj_maelt + fj_talid) %>%
      select(synis_id, tegund, fj_maelt, fj_talid, fj_alls) %>%
      collect(n = Inf) %>%
      filter(!is.na(tegund)) %>%
      complete(synis_id, tegund) %>%
      replace_na(list(fj_maelt = 0, fj_talid = 0, fj_alls = 0))

    le.list[[i]] <-
      st %>%
      select(synis_id) %>%
      left_join(lesa_lengdir(con, schema[i]) %>%
                  group_by(synis_id, tegund, lengd) %>%
                  summarise(fjoldi = sum(fjoldi, na.rm = TRUE)) %>%
                  ungroup()) %>%
      collect(n = Inf)  %>%
      filter(!is.na(tegund)) %>%
      complete(synis_id, tegund) %>%
      replace_na(list(lengd = 0, fjoldi = 0))

    kv.list[[i]] <-
      st %>%
      select(synis_id) %>%
      left_join(lesa_kvarnir(con)) %>%
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
  kv <- bind_rows(kv.list)

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
    filter(leidangur_id == 1) %>%
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

  # IMPORT
  # ----------------------------------------------------------------------------


  # ----------------------------------------------------------------------------
  # DATA MUNGING

  # A. STATIONS DONE - FOR DASHBOARD
  tows.done <-
    st %>%
    filter(leidangur %in% cruise) %>%
    pull(index)
  st.done <-
    st %>%
    filter(index %in% tows.done)
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
    #filter(tegund %in% tegund.dashboard) %>%
    group_by(ar, index, lon, lat, tegund) %>%
    summarise(n.std = sum(n.std, na.rm = TRUE),
              b.std = sum(b.std, na.rm = TRUE)) %>%
    ungroup()


  my_boot = function(x, times=100) {

    # Get column name from input object
    var = deparse(substitute(x))
    var = gsub("^\\.\\$","", var)

    # Bootstrap 95% CI
    cis = quantile(replicate(times, mean(sample(x, replace=TRUE))), probs=c(0.025,0.975))

    # Return data frame of results
    data.frame(var, n=length(x), mean=mean(x), lower.ci=cis[1], upper.ci=cis[2])
  }

  by.station.boot.n <-
    by.station %>%
    group_by(tegund, ar) %>%
    do(my_boot(.$n.std)) %>%
    mutate(variable = "n")

  by.station.boot.b <-
    by.station %>%
    group_by(tegund, ar) %>%
    do(my_boot(.$b.std)) %>%
    mutate(variable = "b")

  by.station.boot <-
    bind_rows(by.station.boot.n, by.station.boot.b)

  kv.this.year <-
    st.done %>%
    filter(ar == 2017) %>%
    select(synis_id, index) %>%
    left_join(kv) %>%
    mutate(lab = paste0(index, "-", nr)) %>%
    left_join(stadlar.lw) %>%
    mutate(ok.osl = if_else(oslaegt >= osl1 & oslaegt <= osl2, TRUE, FALSE, TRUE),
           ok.sl = if_else(slaegt >= sl1 & slaegt <= sl2, TRUE, FALSE, TRUE)) %>%
    select(-c(osl1:sl2)) %>%
    left_join(stadlar.tegundir %>%
                select(tegund, kynkirtlar_high:oslaegt_vigtad_low)) %>%
    mutate(ok.sl.osl = if_else(slaegt/oslaegt >= oslaegt_slaegt_low & slaegt/oslaegt <= oslaegt_slaegt_high, TRUE, FALSE, TRUE),
           ok.kirtlar.osl = if_else(kynfaeri/oslaegt >= kynkirtlar_low & kynfaeri/oslaegt <= kynkirtlar_high, TRUE, FALSE, TRUE),
           ok.lifur.osl = if_else(lifur/oslaegt >= lifur_low & lifur/oslaegt <= lifur_high, TRUE, FALSE, TRUE),
           ok.magir.osl = if_else(magi/oslaegt  >= magi_low & magi/oslaegt <= magi_high, TRUE, FALSE, TRUE)) %>%
    select(-c(kynkirtlar_high:oslaegt_vigtad_low))

  library(sp)
  tows <- stadlar.rallstodvar

  tows$id2 <- 1:nrow(tows)
  x1 <-
    tows %>%
    select(id2, kastad_v, hift_v) %>%
    gather(variable, value, -id2)
  x2 <-
    tows %>%
    select(id2, kastad_n, hift_n) %>%
    gather(variable, value, -id2)
  x <- data.frame(id2 = x1$id2, lon = x1$value, lat = x2$value)
  lines_list <- list()
  for (i in 1:max(tows$id2)) {
    x2 <- Line(x[x$id2 == i,c("lon","lat")])
    lines_list[[i]] <- Lines(list(x2),ID=as.character(tows$id2[i]))
  }
  sp <-
    lines_list %>%
    SpatialLines(proj4string = gisland::PRO) %>%
    SpatialLinesDataFrame(data.frame(id = as.character(tows$id2)))
  sp@data <- cbind(sp@data, tows)
  stadlar.rallstodvar.sp <- sp

  tows <-
    st.done %>%
    filter(!is.na(lon1), !is.na(lat1), !is.na(lon2), !is.na(lat2))
  tows$id2 <- 1:nrow(tows)
  x1 <-
    tows %>%
    select(id2, lon1, lon2) %>%
    gather(variable, value, -id2)
  x2 <-
    tows %>%
    select(id2, lat1, lat2) %>%
    gather(variable, value, -id2)
  x <- data.frame(id2 = x1$id2, lon = x1$value, lat = x2$value)
  lines_list <- list()
  for (i in 1:max(tows$id2)) {
    x2 <- Line(x[x$id2 == i,c("lon","lat")])
    lines_list[[i]] <- Lines(list(x2),ID=as.character(tows$id2[i]))
  }
  sp <-
    lines_list %>%
    SpatialLines(proj4string = gisland::PRO) %>%
    SpatialLinesDataFrame(data.frame(id = as.character(tows$id2)))
  sp@data <- cbind(sp@data, tows)
  st.done.sp <- sp

  timi <- lubridate::now() %>% as.character()

  dir.create("data2")
  save(stadlar.rallstodvar.sp,
       st.done.sp,
       stadlar.tegundir,
       stadlar.lw,
       sp,
       timi, kv.this.year, by.tegund.lengd.ar, by.tegund.lengd.ar.m,
       by.station, fisktegundir, by.station.boot, file = "data2/smb_dashboard2.rda")

  st <<- st
  le <<- le
  kv <<- kv
  nu <<- nu
  st.done <<- st.done
  stadlar.rallstodvar <<- stadlar.rallstodvar
  stadlar.tegundir <<- stadlar.tegundir
  stadlar.lw <<- stadlar.lw


}
