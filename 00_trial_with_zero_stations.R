library(tidyverse)
library(xe)

read_smx_data <- function(cruise = "TB1-2017", debug = 0) {

  con <- mar::connect_mar()

  now.year <- lubridate::now() %>% lubridate::year()
  now.year <- now.year - debug

  min.towlength <- 2             # Minimum "acceptable" towlength
  max.towlength <- 8             # Maximum "acceptable" towlength
  std.towlength <- 4             # Standard tow length is 4 nautical miles

  schema <- c("fiskar", "hafvog")
  st.list <- nu.list <- le.list <- list()
  for(i in 1:length(schema)) {

    st <-
      tbl_mar(con, paste0(schema[[i]], ".stodvar")) %>%
      filter(synaflokkur == 30, veidarfaeri == 73) %>%
      left_join(tbl_mar(con, paste0(schema[i], ".togstodvar")) %>%
                  select(synis_id, tognumer, toglengd))

    if(schema[i] == "fiskar") {
      st <-
        st %>%
        filter(to_number(to_char(dags, "YYYY")) < now.year)
    } else {
      st <-
        st %>%
        filter(to_number(to_char(dags, "YYYY")) == now.year)
    }

    nu.list[[i]] <-
      st %>%
      select(synis_id) %>%
      left_join(tbl_mar(con, paste0(schema[i], ".numer")) %>%
                  mutate(fj_alls = fj_maelt + fj_talid) %>%
                  select(synis_id, tegund, fj_maelt, fj_talid, fj_alls)) %>%
      collect(n = Inf) %>%
      complete(synis_id, tegund) %>%
      replace_na(list(fj_maelt = 0, fj_talid = 0, fj_alls = 0)) %>%
      mutate(source = schema[i])

    le.list[[i]] <-
      st %>%
      select(synis_id) %>%
      left_join(tbl_mar(con, paste0(schema[i], ".lengdir")) %>%
                  group_by(synis_id, tegund, lengd) %>%
                  summarise(fjoldi = sum(fjoldi, na.rm = TRUE)) %>%
                  ungroup()) %>%
      collect(n = Inf) %>%
      complete(synis_id, tegund) %>%
      replace_na(list(lengd = 0, fjoldi = 0)) %>%
      mutate(source = schema[i])

    st.list[[i]] <-
      st %>%
      mutate(ar = to_number(to_char(dags, "YYYY"))) %>%
      collect(n = Inf) %>%
      mutate(source = schema[i])

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
read_smx_data(debug = 1)

st.done %>%
  select(synis_id) %>%
  left_join(le) %>%
  group_by(ar, tegund, synis_id) %>%
  summarise(n.std = sum(n.std),
            b.std = sum(b.std)) %>%
  filter(tegund == 1) %>%
  ggplot(aes(ar, b.std)) +
  stat_summary(fun.data = "mean_cl_boot") +
  expand_limits(y = 0)
