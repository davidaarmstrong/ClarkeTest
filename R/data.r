#' Conflict Data
#'
#' A country-year dataset containing information on
#'  conflict and other country attributes.  These data
#'  come from multiple sources and are simply for
#'  the purposes of demonstrating how the functions
#'  in the package work. The data contain the following
#'  variables
#'
#' @format A data frame with 4381 rows and 9 variables
#' \describe{
#'   \item{GWNo}{ Gleditsch and Ward country number}
#'   \item{Year}{ year}
#'   \item{StateName}{ Country name}
#'   \item{conflict_binary}{ Binary indicator of conflict}
#'   \item{polity2}{ Polity IV indicator of regime type}
#'   \item{Amnesty}{ Amnesty International Political
#'   Terror Scale Rating}
#'   \item{riots}{ Number of riots in each country-yar}
#'   \item{pop}{ Population in country (in thousands)}
#'   \item{rgdpna_pc}{ PWT measure of GDP/capita}
#' }
#'
"conflictData"
