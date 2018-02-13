#' Title
#'
#' bla, bla, bla, ...
#'
#' @param user XX
#' @param password XX
#' @export
#'
connect_xe <- function(user, password) {

  if(user == "hafvog_user") stop("sorry, not allowed")

  if(missing(password)) password <- user

  DBI::dbConnect(DBI::dbDriver("Oracle"),
                 user = user,
                 password = password,
                 host = "localhost",
                 port = 1521,
                 dbname = "xe")

}