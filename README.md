
# The installation

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

# The smxapp

Suggest you do:

- Create a new project (i.e. a directory) in RStudio for the current
  year (here I will just call it SMB)
- Create a directory data2 within that directory
  - In this directory data from Hafvog will be writen that is then used
    when “Run”-ing the app
- A template for the smxapp is available via the {xe} package. To access
  the template one does as follows within RStudio:

<!-- -->

    File -> New File -> R Markdown -> From template -> smx dashboard

- Save this template as e.g SMB-2023.Rmd in the root directory of the
  project (which I called just SMB aka above)
- Run the top code-chunk of the document (the last line is
  ‘munge_for_smxapp’. Do not run the rest of the document, that happens
  in the next step below.
  - What happens is that some files in the data2-directory get
    created/are updated.
- Once done, press ‘Run Document’ and you should have some pretty
  pictures.

Note: If you have added data to the xe-database you need to run the top
code-chunk again. If not you can just press ‘Run Document’ at your
leasure.

## 2023 SMB testfile

A test file is availble on hafro ftp-site, you can download it directly
via:

``` r
download.file("ftp.hafro.is/pub/data/TESTO-2023.zip", destfile = "TESTO-2023.zip")
```

Read this file into “hafvog” and then run the top code snippet in the
smx dashboard. Once done you should be able to Run the smx dashboard and
see example of the 2023 dummy results. If no errors pop up you are ready
to go out to sea.

# A general recommendation on RStudio setup

0)  This is just a generic recomendation when working in RStudio:
    - Go to Tools -\> Global option … -\> General
      - Make sure that “Restore .RData into workspace at startup” is
        **not selected**
      - Set “Save workspace to .RData on exit” to “**Never**”
    - Go to Tools -\> Global option … -\> R Markdown
      - Make sure that “Show output inline for all R Markdown documents”
        is **not selected**

# A general recomendation on updating packages

1)  Make sure you have only one RStudio open and that it is a “fresh”
    session
2)  Do as described on
    [gitlab](https://gitlab.hafogvatn.is/-/snippets/3):

``` r
options(repos = c(CRAN = "https://cran.hafro.is"))
update.packages(ask=FALSE, checkBuilt = TRUE,lib=Sys.getenv("R_LIBS_USER"))
```
