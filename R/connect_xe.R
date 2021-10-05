#' Title
#'
#' bla, bla, bla, ...
#'
#' @param user XX
#' @param password XX
#' @export
#'
connect_xe <- function(user = "hafvog_user", password = "hafvog") {

  DBI::dbConnect(DBI::dbDriver("Oracle"),
                 user = user,
                 password = password,
                 host = "localhost",
                 port = 1521,
                 dbname = "xe")

}
