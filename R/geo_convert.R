
geo_convert <- function (x, inverse = FALSE, na = FALSE) {
  if (!inverse) {
    i <- sign(x)
    x <- abs(x)
    min <- (x/100) - trunc(x/10000) * 100
    if (na)
      min <- ifelse(min > 60, NA, min)
    return((i * (x + (200/3) * min))/10000)
  }
  else {
    i <- sign(x)
    x <- abs(x)
    p1 <- floor(x)
    p2 <- floor((x - p1) * 60)
    p3 <- round((x - p1 - p2/60) * 100 * 60)
    return(i * (p1 * 10000 + p2 * 100 + p3))
  }
}
