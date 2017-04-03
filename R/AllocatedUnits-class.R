#' @title S4 class for the units allocated
#'
#' @description Definition of the S4 class named \code{UnitAllocParam} for the parameters for
#' the allocation of units to be edited interactively in each population domain under the
#' optimization approach to selective editing.
#'
#' @slot Domains \linkS4class{data.table} with a row per domain with the values of the variables
#' determining the domains of the data set.
#'
#' @slot MaxAlloc Integer vector with the maximum number of units to be allocated in each domain.
#'
#' @slot MinAlloc Integer vector with the minimum number of units to be allocated in each domain.
#'
#' @slot AllocFactors List with numeric vectors as components containing the proportionality factor
#' of each domain for the allocation.
#'
#' @slot MaxUnits Integer vector of length 1 with the maximal number of units to allocate.
#'
#' @examples
#' UnitAllocParam <- new(Class = 'UnitAllocParam',
#'                       MaxAlloc = as.integer(c(23, 23, 45, 23, 12, 23, 26, 76, 12, 1, 1, 1)),
#'                       MinAlloc = rep(2L, length = 12),
#'                       AllocFactors = list(c(0.43, 0.12, 0.234, 0.123, 0.934, 0.23, 0.754, 0.423, 0.923, 0.345, 0.512, 0.623),
#'                                           c(0.43, 0.123, 0.34, 0.3, 0.34, 0.13, 0.954, 0.523, 0.223, 0.445, 0.522, 0.323)),
#'                       DomainWeights = c(0.34, 0.12, 0.1, 1, 123, 0.12, 56, 123, 98, 12, 23, 87),
#'                       MaxUnits = 265L )
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


           if (dim(object@Domains)[1] != length(object@Units)) stop()

           return(TRUE)
         }
)
