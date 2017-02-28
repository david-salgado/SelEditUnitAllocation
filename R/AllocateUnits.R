#' Method to allocate units per population domain
#'
#' @param object Object upon which the allocation is to be computed.
#'
#' @param Param Parameters to allocate the units in each domain.
#'
#' @return Integer vector with the number of allocated units per domain.
#'
#' @examples
#' \dontrun{
#'
#' UnitAllocParam <- new(Class = 'UnitAllocParam',
#'                       MaxAlloc = as.integer(c(23, 23, 45, 23, 12, 23, 26, 76, 12, 1, 1, 1)),
#'                       MinAlloc = rep(2L, length = 12),
#'                       AllocFactors = list(c(0.43, 0.12, 0.234, 0.123, 0.934, 0.23, 0.754, 0.423, 0.923, 0.345, 0.512, 0.623),
#'                                           c(0.43, 0.123, 0.34, 0.3, 0.34, 0.13, 0.954, 0.523, 0.223, 0.445, 0.522, 0.323)),
#'                       DomainWeights = c(0.34, 0.12, 0.1, 1, 123, 0.12, 56, 123, 98, 12, 23, 87),
#'                       MaxUnits = 265L )
#'
#' AllocateUnits(PriorUnits, UnitAllocParam)
#' }
#' @export
setGeneric("AllocateUnits", function(object, Param){standardGeneric("AllocateUnits")})

#' @rdname AllocateUnits
#'
#' @include UnitAllocParam-class.R
#'
#' @import SelEditUnitAllocation
#'
#' @export
setMethod(
  f = "AllocateUnits",
  signature = c("UnitPrioritization", "UnitAllocParam"),
  function(object, Param){


    CellSize <- as.integer(unlist(lapply(object@Units, function(x){dim(x)[[1]]})))
    nCells <- length(CellSize)

    if (Param@MaxUnits == 0) {

      output <- integer(nCells)
      return(output)

    }

    nmin <- Param@MinAlloc
    nmax <- Param@MaxAlloc
    ndep <- Param@MaxUnits

    if (length(nmin) == 1) nmin <- rep(nmin, times = nCells)
    if (length(nmin) != nCells) stop('[SelEditUnitAllocation::AllocateUnits] MinAlloc slot of Param must have the same number of domains as object.')
    if (length(nmax) == 1) nmax <- rep(nmax, times = nCells)
    if (length(nmax) != nCells) stop('[SelEditUnitAllocation::AllocateUnits] MaxAlloc slot of Param must have the same number of domains as object.')

    nmin[nmin > CellSize] <- CellSize[nmin > CellSize]

    FactorList <- lapply(Param@AllocFactors, function(x){return(x / sum(x))})

    Weights <- Param@DomainWeights / sum(Param@DomainWeights)

    geo.mean <- function(x){

      if (any(abs(x) < .Machine$double.eps)) {

        return(0)

      } else {

        return(exp(sum(Weights * log(x)) / sum(Weights)))

      }
    }


    nFactors <- length(FactorList)
#    nFactComp <- lapply(FactorList, length)


    if (all(ndep >= CellSize)) {

      output <- CellSize
      return(output)

    }

    if (sum(nmin) > ndep) stop("[SelEditUnitAllocation::AllocateUnits] The sum of AllocMin exceeds the maximum number of units to allocate MaxUnits.")

    Allocation <- nmin
#    names(Allocation) <- names(CellSize)

    CellSize <- pmin(CellSize, nmax)
    if (any(nmin >= CellSize)) Allocation[nmin >= CellSize] <- CellSize[nmin >= CellSize]

    remainder <- ndep - sum(Allocation)
    complete <- which(Allocation == CellSize)
    incomplete <- which(Allocation != CellSize)
    increment <- integer(nCells)

    Factors.matrix <- matrix(unlist(FactorList), ncol = nFactors)

    PropConst <- apply(Factors.matrix, 1, geo.mean)
    increment[incomplete] <- as.integer(floor(PropConst[incomplete] * remainder))
    increment[increment < 0] <- 0L
    Allocation <- Allocation + increment

    Allocation[Allocation >= CellSize] <- CellSize[Allocation >= CellSize]
    remainder <- ndep - sum(Allocation)
    while (any(increment > 0)){
      complete <- which(Allocation == CellSize)
      incomplete <- which(Allocation != CellSize)
      increment <- integer(nCells)
      increment[incomplete] <- as.integer(floor(PropConst[incomplete] * remainder))
      increment[increment < 0] <- 0L
      Allocation <- Allocation + increment

      Allocation[Allocation >= CellSize] <- CellSize[Allocation >= CellSize]
      remainder <- ndep - sum(Allocation)
    }
    if (remainder==0) return(Allocation)

    Order.PropConst <- order(PropConst, decreasing = T)
    names(Allocation) <- paste0('Dom', seq(along = Allocation))
    Order.Cells <- names(Allocation)[Order.PropConst]
    Counter <- 1L
    while (remainder > 0){
      increment <- integer(nCells)
      names(increment) <- names(Allocation)
      increment[Order.Cells[Counter]] <- 1L
      Allocation <- Allocation + increment
      remainder <- ndep - sum(Allocation)
      if (Counter == length(Allocation)) {Counter <- 1L
      } else Counter <- Counter + 1L
    }
    names(Allocation) <- NULL

    outputUnits <- lapply(seq(along = object@Units), function(indexDomain){

      indexSelectedUnits <- which(object@UnitPriority[[indexDomain]] <= Allocation[indexDomain])
      outLocal <- object@Units[[indexDomain]][indexSelectedUnits]
      return(outLocal)

    })

    output <- new(Class = 'AllocatedUnits', Domains = object@Domains, Units = outputUnits)

    return(output)
  }
)
