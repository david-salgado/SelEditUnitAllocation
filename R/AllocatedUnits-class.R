#' @title S4 class for the units allocated
#'
#' @description Definition of the S4 class named \code{AllocatedUnits} units allocated to be edited
#' interactively in each population domain under the optimization approach to selective editing.
#'
#' @slot Domains \linkS4class{data.table} with a row per domain with the values of the variables
#' determining the domains of the data set.
#'
#' @slot Units list of \linkS4class{data.table}s with the statistical units per domain.
#'
#'
#' @examples
#' new(Class = 'AllocatedUnits')
#'
#' @import data.table
#'
#' @export
setClass(Class = "AllocatedUnits",
         slots = c(Domains = 'data.table',
                   Units = 'list'),
         prototype = list(Domains = data.table::data.table(NULL),
                          Units = list()),
         validity = function(object){


           return(TRUE)
         }
)
