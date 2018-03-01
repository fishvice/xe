Communicating with hafvog
=========================

Preamble
--------

The xe-package allows seamless connection to the MRI Oracle xe-database
via R. Unlike the mar-database the xe-database resides on personal
computers and is what the software Hafvog communicates with. The
xe-database has at minimum one schema - hafvog. This schema contains all
cruises that are visible in the Hafvog-software. Normally an additional
schema is included in the xe-database - fiskar. This contains a copy of
most tables that are stored in schema fiskar in the MRI Oracle
mar-database.

The xe-package is supposed to mimic a proportion of the functional calls
that reside in the mar-package (it is likely that functionality of the
former will soon be moved into the latter package, i.e. xe will soon
become redundant :-).

Installing:
-----------

    devtools::install_github("fishvice/xe",  dependencies = FALSE, args='--no-multiarch')

Basic functionality
===================

Once an R session is started the connection to xe is done via:

    library(tidyverse)
    library(ROracle)
    library(xe)
    con <- connect_xe()

To access the data in the hafvog-schema (i.e. the cruises currently
visible as "Leidangrar" in the Hafvog-software) one simply does:

    st.sql <- lesa_stodvar(con, schema = "hafvog")
    le.sql <- lesa_lengdir(con, schema = "hafvog")
    nu.sql <- lesa_numer(con, schema = "hafvog")

To take a peek of what variables are in the data use the
`glimpse`-funtion. E.g.

    glimpse(st.sql)

If one is interested in working with the data in schema fiskar one
simply does:

    st.sql <- lesa_stodvar(con, schema = "fiskar")
    le.sql <- lesa_lengdir(con, schema = "fiskar")
    nu.sql <- lesa_numer(con, schema = "fiskar")

Take note that in the above the objects xx.sql are not yet local
R-dataframes, just an sql-script that also gives a peek at the first 10
records in the database. To create a local dataframe one can import the
data via the collect-function. E.g.

    st <- st.sql %>% collect(n = Inf)

In general things then should work the same as in the mar-package,
except that lon and lat in the station-table are still in the
DDMMmm-format. Here one could use the `geoconvert`-function from the
geo-package.

Working with survey data
========================

Here is an example on how one would get the spring survey data from the
database into R.

First we provide and import from schema hafvog:

    st.sql <- 
      lesa_stodvar(con, "hafvog") %>% 
      filter(synaflokkur == 30)
    le <-
      st.sql %>% 
      select(synis_id, ar) %>% 
      left_join(lesa_lengdir(con, "hafvog")) %>% 
      collect(n = Inf)
    nu <- 
      st.sql %>% 
      select(synis_id) %>% 
      left_join(lesa_numer(con, "hafvog")) %>% 
      collect(n = Inf)
    kv <- 
      st.sql %>% 
      select(synis_id, ar) %>% 
      left_join(lesa_kvarnir(con, "hafvog")) %>% 
      collect(n = Inf)
    st <- 
      st.sql %>% 
      collect(n = Inf)

One could repeat this process for data in schema fiskar and then combine
(rbind) the data from the two schema sources. A little wrapper for doing
that is provided by the function `import_smx`:

    res <- import_smx(con)

The function returns a list of the fiskar dataframe from both schema
hafvog and schema fiskar (synaflokkur = 30 and veidarfaeri = 73 are the
default arguments in the call). Individual dataframes can be "retrieved"
by:

    st <- res$st
    nu <- res$nu
    le <- res$le
    kv <- res$kv
