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


#' Title
#'
#' bla, bla, bla, ...
#'
#' @param dbname The name of the database (dbname), normally "xe" or "XEPDB1"
#' @param user XX
#' @param password XX
#' @export
#'
connect_oracle <- function(dbname = "xe", user = "hafvog_user", password = "hafvog") {

  DBI::dbConnect(DBI::dbDriver("Oracle"),
                 user = user,
                 password = password,
                 host = "localhost",
                 port = 1521,
                 dbname = dbname)

}