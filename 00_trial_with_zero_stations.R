#devtools::install_github("fishvice/xe",  dependencies = FALSE,  args='--no-multiarch', ref = "revamp")
library(tidyverse)
library(lubridate)
library(xe)
devtools::session_info()
con <- connect_xe(user = "gagnasja")
#lesa_stodvar(con, schema = "hafvog") %>%
#  filter(leidangur == "TB1-2017") %>%
#  select(synis_id) %>%
#  left_join(lesa_lengdir(con, schema = "hafvog"))

source("R/import_smx.R")
import_smx(con, debug = 1)
st.done %>%
  select(synis_id) %>%
  left_join(le) %>%
  filter(lengd != 0) %>%
  group_by(ar, tegund, synis_id) %>%
  summarise(n.std = sum(n.std),
            b.std = sum(b.std)) %>%
  filter(tegund == 1) %>%
  ggplot(aes(ar, b.std)) +
  stat_summary(fun.data = "mean_cl_boot") +
  expand_limits(y = 0)
