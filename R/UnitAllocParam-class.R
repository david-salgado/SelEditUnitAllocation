#' @title S4 class for the parameters of the allocation of units
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
#' @examples
#' UnitAllocParam <- new(Class = 'UnitAllocParam',
#'                       MaxAlloc = as.integer(c(23, 23, 45, 23, 12, 23, 26, 76, 12, 1, 1, 1)),
#'                       MinAlloc = rep(2L, length = 12),
#'                       AllocFactors = list(c(0.43, 0.12, 0.234, 0.123, 0.934, 0.23, 0.754, 0.423, 0.923, 0.345, 0.512, 0.623),
#'                                           c(0.43, 0.123, 0.34, 0.3, 0.34, 0.13, 0.954, 0.523, 0.223, 0.445, 0.522, 0.323)),
#'                       DomainWeights = c(0.34, 0.12, 0.1, 1, 123, 0.12, 56, 123, 98, 12, 23, 87),
#'                       MaxUnits = 265L,
#'                       ExcUnits = data.table(NULL))
#'
#' @import data.table
#'
#' @export
setClass(Class = "UnitAllocParam",
         slots = c(Domains = 'data.table',
                   MaxAlloc = 'integer',
                   MinAlloc = 'integer',
                   AllocFactors = 'list',
                   DomainWeights = 'numeric',
                   MaxUnits = 'integer',
                   ExcUnits = 'data.table'),
         prototype = list(Domains = data.table::data.table(NULL),
                          MaxAlloc = integer(0),
                          MinAlloc = integer(0),
                          AllocFactors = list(),
                          DomainWeights = numeric(0),
                          MaxUnits = integer(0),
                          ExcUnits = data.table::data.table(NULL)),
         validity = function(object){


           return(TRUE)
         }
)
