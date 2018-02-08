---
title: "xe"
output: 
  html_document: 
    keep_md: yes
---


```r
knitr::opts_chunk$set(echo = TRUE, eval = FALSE)
```

Communicating with hafvog

Installing:


```r
devtools::install_github("fishvice/xe",  dependencies = FALSE, args='--no-multiarch')
```

connection to the xe would be:


```r
library(mar)
library(xe)
con <- connect_xe(user = "xxxx")
```

and then buisness as usual:

```r
lesa_stodvar(con)
lesa_lengdir(con)
lesa_kvarnir(con)
lesa_numer(con)
```





