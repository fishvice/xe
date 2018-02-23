library(tidyverse)
library(xe)
devtools::install_github("fishvice/xe",  dependencies = FALSE, ref = "revamp")
library(tidyverse)
library(lubridate)
library(xe)
#devtools::session_info()
con <- mar::connect_mar()
lesa_stodvar(con, "hafvog")


import_smx(con, debug = 1, schema = "fiskar")
st %>%
  select(synis_id) %>%
  left_join(le) %>%
  group_by(ar, tegund, synis_id) %>%
  summarise(n.std = sum(n.std),
            b.std = sum(b.std)) %>%
  filter(tegund == 74) %>%
  ggplot(aes(ar, n.std)) +
  stat_summary(fun.data = "mean_cl_boot") +
  expand_limits(y = 0)
