#' @title S4 class for the parameters of the Cook-related allocation proportional factor.
#'
#' @description Definition of the S4 class named \code{CookAllocParam} for the parameters for
#' the allocation of units to be edited interactively in each population domain under the
#' optimization approach to selective editing using the Cook distance in a linear regression model.
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
#' CookAllocParam <- new(Class = 'CookAllocParam',
#'                       ObjVariables = c('CifraNeg_13.___', 'Personal_07._____705:1.-2.'),
#'                       Domains = 'ActivEcono_35._4._2.1.4._0',
#'                       Prob = 0.95,
#'                       alpha = 2 )
#'
#' @import data.table
#'
#' @export
setClass(Class = "CookAllocParam",
         slots = c(ObjVariables = 'character',
                   Domains = 'data.table',
                   Prob = 'numeric',
                   alpha = 'numeric'),
         prototype = list(ObjVariables = character(0),
                          Domains = data.table(NULL),
                          Prob = integer(0),
                          alpha = integer(0)),
         validity = function(object){

           if (length(object@Prob) != 1 && length(object@Prob) != dim(object@Domains[1])) stop()
           if (length(object@alpha) != 1 && length(object@alpha) != dim(object@Domains[1])) stop()

           return(TRUE)
         }
)
