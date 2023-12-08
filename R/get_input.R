# Adding a short cut function to get the input for each day!
# thanks - https://github.com/dgrtwo/adventdrob/tree/main/R


#' Read in an Advent of Code input
#'
#' @param day Day within December
#' @param year By default, 2023
#' @param parse function to parse input (e.g. read.csv) by default just returns each line
#'
#' @export
get_input <- function(day = lubridate::day(Sys.Date()),
                         year = 2023,
                         parse = NULL, ...) {
 session <- Sys.getenv("ADVENT_SESSION")
 if (session == "") {
  stop("Must set ADVENT_SESSION in .Renviron")
 }
 
 url <- paste0("https://adventofcode.com/", year, "/day/", day, "/input")
 
 req <- httr::GET(url,
                  httr::set_cookies(session = session))
 httr::stop_for_status(req)
 
 if (!is.null(parse)) {
  data <- httr::content(req, as = "raw")
  lines <- do.call(parse, list(data, ...))
 } else {
  txt <- httr::content(req, encoding = "UTF-8")
  lines <- stringr::str_split(txt, "\n")[[1]]
  lines <- head(lines, -1)
 }
 
 lines
}
