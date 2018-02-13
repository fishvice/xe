geoconvert <- function(data, inverse = FALSE, col.names = c("lat", "lon")){
  if(!('tbl_sql' %in% class(data))){
    return(geo::geoconvert(data,inverse,col.names))
  }
  if(!inverse){
    tmp <- sprintf('geoconvert1(%s)',col.names)
    dplyr::mutate_(data,.dots = setNames(tmp,col.names)) %>%
      select_(.dots=colnames(data))
  } else {
    tmp <- sprintf('geoconvert2(%s)',col.names)
    dplyr::mutate_(data,.dots = setNames(tmp,col.names)) %>%
      select_(.dots=colnames(data))
  }
}