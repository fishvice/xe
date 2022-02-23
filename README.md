
# The quick installation guide

The purpose of the {xe} package is to create a connection and some
convenient functions to the MRI Oracle XE-database via R. Unlike the
mar-database the XE-database resides on personal computers and is what
the software [hafvog](https://heima.hafro.is/~darri/hafvog_vefur)
communicates with. The {xe} packages stands on its own and can be used
for …, further details are provide
[here](https://heima.hafro.is/~einarhj/xe/articles/overview_xe.html). To
install it do:

``` r
remotes::install_github("fishvice/xe",  dependencies = FALSE, args='--no-multiarch')
```

**As of 2022-02-22 the version of Hafvog is 4.1.8, including the setup
(stillingar) and support tables (stoðtöflur).** Since the XE database is
run on Oracle 11g one can not not use dbplyr versions \>2.0. Hence we
need to install older versions:

``` r
remotes::install_github("tidyverse/dbplyr@v1.4.4", force = TRUE)
```

**Side effects**: Installing an earlier version of {dbplyr} means that
one can not load the tidyverse package only its child packages (dplyr,
ggplot2, tidyr, tibble, ….). It also means that if you want to access
data on the main Oracle database via {mar} one has to reinstall the
latest version of {dbplyr}. By `install.packages("dbplyr"}` one
overwrites {dbplyr} version 1.4.4 with the latest one available on cran.

The most common usage of the {xe} packages is to serve as a basis for
the “smxapp”, a quality control shiny app that is used during the
conduction of the Icelandic bottom trawl surveys. In order to provide
also a historical comparisons older archieved survey data must also be
available. These reside (as of 2022) in the {mardata} package. Since
these data are not publicly available as of yet, that package is not
distributed on github but rather internally … To install it do:

``` r
remotes::install_local("R:/R/Pakkar/mardata", force = TRUE)
```

**Note**: This assumes that you are connected to MFRI instute network,
either at the office or via VPN. Hence it is strongly suggested that you
install this package prior to leaving on a cruise.

## The smxapp

A template for the smxapp is available via the {xe} package. To access
the template one does as follows within RStudio:

    File -> New File -> R Markdown -> From template -> smx dashboard
