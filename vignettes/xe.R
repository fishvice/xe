## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, eval = FALSE)

## ------------------------------------------------------------------------
#  library(tidyverse)
#  library(ROracle)
#  library(xe)
#  con <- connect_xe()

## ------------------------------------------------------------------------
#  st.sql <- lesa_stodvar(con, schema = "hafvog")
#  le.sql <- lesa_lengdir(con, schema = "hafvog")
#  nu.sql <- lesa_numer(con, schema = "hafvog")

## ------------------------------------------------------------------------
#  st.sql <- lesa_stodvar(con, schema = "fiskar")
#  le.sql <- lesa_lengdir(con, schema = "fiskar")
#  nu.sql <- lesa_numer(con, schema = "fiskar")

## ------------------------------------------------------------------------
#  st <- st.sql %>% collect(n = Inf)

## ------------------------------------------------------------------------
#  st.sql <-
#    lesa_stodvar(con, "hafvog") %>%
#    filter(synaflokkur == 30)
#  le <-
#    st.sql %>%
#    select(synis_id, ar) %>%
#    left_join(lesa_lengdir(con, "hafvog")) %>%
#    collect(n = Inf)
#  nu <-
#    st.sql %>%
#    select(synis_id) %>%
#    left_join(lesa_numer(con, "hafvog")) %>%
#    collect(n = Inf)
#  kv <-
#    st.sql %>%
#    select(synis_id, ar) %>%
#    left_join(lesa_kvarnir(con, "hafvog")) %>%
#    collect(n = Inf)

## ------------------------------------------------------------------------
#  res <- import_smx(con)

## ------------------------------------------------------------------------
#  st <- res$st
#  nu <- res$nu
#  le <- res$le
#  kv <- res$kv

