# PROBLEM: schema orri and schema faeda are likely not in the xe-database
#' Title
#'
#' @param con Oracle xe connection
#'
#' @export
#'
brad_tegund <- function(con) {

  tbl_xe(con,'faeda.f_tegundir') %>%
    dplyr::left_join(tbl_xe(con, "orri.fisktegundir") %>%
                       dplyr::select(-c(snt:sbn)),
                     by='tegund') %>%
    dplyr::select(-c(heiti,yfir_flokkur,visindaheiti)) %>%
    dplyr::mutate(enskt_heiti = ifelse(faeduhopur == 'pisces','Unid fishes',
                                       ifelse(faeduhopur == 'ammodytx','Sandeel',
                                              ifelse(faeduhopur == 'pand bor', "Northern shrimp",
                                                     ifelse(faeduhopur == "heteroso","Flat fishes",
                                                            ifelse(faeduhopur == "natantia", 'Unid shrimp',
                                                                   ifelse(faeduhopur == 'rest', 'Unid remains',
                                                                          ifelse(faeduhopur == 'euphausi', 'Krill',
                                                                                 nvl2(enskt_heiti,enskt_heiti,lat_heiti)))))))))
}

