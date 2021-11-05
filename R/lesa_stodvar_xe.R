lesa_stodvar_xe <- function(con, schema) {

  if(missing(con)) stop("You need to provide a connection (con) see conect_xe")
  if(missing(schema)) stop("You need to provide a schema, e.g. 'fiskar' or 'hafvog'")

  tbl_xe(con, paste0(schema, ".stodvar")) %>%
    dplyr::select(synis_id:heildarafli, synaflokkur) %>%
    dplyr::mutate(ar = to_number(to_char(dags, "YYYY"))) %>%
    dplyr::left_join(tbl_xe(con, paste0(schema, ".togstodvar")) %>%
                       dplyr::select(synis_id:eykt), by = "synis_id") %>%
    dplyr::left_join(tbl_xe(con, paste0(schema, ".umhverfi")) %>%
                       dplyr::select(synis_id:sjondypi), by = "synis_id") %>%
    dplyr::mutate(source = schema,
                  synis_id = ifelse(schema == "hafvog", -synis_id, synis_id),
                  index = reitur * 100 + tognumer)

}
