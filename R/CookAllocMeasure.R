#' Function to obtain the Cook-related allocation measure.
#'
#' Function to
#'
#' @param Variable numeric vector with the values of the variables
#'
#' @param ObsPredParam object of class \linkS4class{ObsPredParam}
#'
#' @param Factor data.table with the values of the variables working as factors
#'
#' @param Prob numeric vector of length 1
#'
#' @param alpha numeric vector of length 1
#'
#' @return It returns a numeric vector
#'
#' @examples
#' \dontrun{
#' ecfFun(c(1, 3, 5, 0, -1, 1))
#' }
#' @export
setGeneric("CookAllocMeasure", function(object, Param){standardGeneric("CookAllocMeasure")})

#' @rdname CookAllocMeasure
#'
#' @include CookAllocParam-class.R
#'
#' @import contObsPredModelParam StQ data.table
#'
#' @export
setMethod(
  f = "CookAllocMeasure",
  signature = c("contObsPredModelParam", "CookAllocParam"),
  function(object, Param){

    AllocDomains.DT <- copy(Param@Domains)
    DomainNames <- names(AllocDomains.DT)
    Variables <- c(Param@ObjVariables, paste0('Pred', Param@ObjVariables),
                   DomainNames, paste0('DesignW', Param@ObjVariables))
    IDQuals <- getIDQual(object@Data, 'MicroData')
    auxDT <- dcast_StQ(object@Data, ExtractNames(Variables))[, c(IDQuals, Variables), with = FALSE]

    AllocDomains.DT[, Prob := Param@Prob]
    AllocDomains.DT[, alpha := Param@alpha]
    auxDT <- merge(auxDT, AllocDomains.DT, all.x = TRUE, by = intersect(names(auxDT), names(AllocDomains.DT)))
    AllocDomains.DT[, Prob := NULL]
    AllocDomains.DT[, alpha := NULL]

    auxDT.list <- split(auxDT, auxDT[, DomainNames, with = FALSE])
    DomainNames <- names(auxDT.list)
    outputList <- lapply(Param@ObjVariables, function(Var){

      VarOutput <- lapply(auxDT.list, function(DT){

          if (dim(DT)[[1]] <= 1) return(0)
          Pred <- paste0('Pred', Var)
          fit <- lm(get(Var) ~ get(Pred), data = DT)
          wName <- paste0('DesignW', Var)
          Cook <- DT[, get(wName) * cooks.distance(fit)]
          alpha <- unique(DT[['alpha']])
          Cook <- Cook * (dim(DT)[[1]]) ^ alpha
          Prob <- unique(DT[['Prob']])
          output <- quantile(Cook, Prob, na.rm = T)
          return(output)

      })
    })

    Cook.matrix <- if (length(outputList) > 1) {

      Reduce(function(x, y){cbind(unlist(x), unlist(y))}, outputList)

    } else {

      matrix(outputList[[1]], ncol = 1)
    }

    colnames(Cook.matrix) <- Param@ObjVariables
    Cook.matrix[is.na(Cook.matrix)] <- 0

    output <- apply(Cook.matrix, 1, max, na.rm = TRUE)
    names(output) <- DomainNames
    output <- output / sum(output, na.rm = TRUE)
    AllocDomains.DT[, CookMeasure := output]
    return(AllocDomains.DT)
  }
)
