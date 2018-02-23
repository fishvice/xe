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
                       cruise = "TB1-2017", debug = 0) {

  now.year <- lubridate::now() %>% lubridate::year()
  now.year <- now.year - debug

  min.towlength <- 2             # Minimum "acceptable" towlength
  max.towlength <- 8             # Maximum "acceptable" towlength
  std.towlength <- 4             # Standard tow length is 4 nautical miles

  schema <- c("fiskar", "hafvog")
  st.list <- nu.list <- le.list <- list()
  for(i in 1:length(schema)) {

    st <-
      lesa_stodvar(con, schema[i]) %>%
      filter(synaflokkur %in% id, veidarfaeri %in% gid) %>%
      select(synis_id, ar, leidangur, reitur, tognumer, toglengd)

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
      left_join(lesa_lengdir(con, schema[i]) %>%
                  group_by(synis_id, tegund, lengd) %>%
                  summarise(fjoldi = sum(fjoldi, na.rm = TRUE)) %>%
                  ungroup()) %>%
      collect(n = Inf) %>%
      filter(!is.na(tegund)) %>%
      complete(synis_id, tegund) %>%
      replace_na(list(lengd = 0, fjoldi = 0))

    st.list[[i]] <-
      st %>%
      collect(n = Inf)

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
    filter(leidangur == cruise) %>%
    mutate(index = reitur * 10 + tognumer) %>%
    pull(index)
  st.done <-
    st %>%
    filter(index %in% tows.done)
  st <<- st
  le <<- le
  nu <<- nu
  st.done <<- st.done

}
