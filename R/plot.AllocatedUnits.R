#' @title Plot a summary of the selection of units in the optimization approach to selective editing.
#'
#' @description This method overloads the plot function to draw a picture with the information about
#' the selection of influential statistical units in the optimization approach to selective editing.
#'
#' @param x Object of class \linkS4class{AllocatedUnits}.
#'
#' @param y Charactec vector with the names of the variable names roots.
#'
#' @param ... Arguments to be passed to methods, especially including
#'
#' \itemize{
#'
#'  \item \code{Query} as a \linkS4class{data.table} with the values of either the identification
#'  variables of the queried statistical units or the domain variables of queried sample domain;
#'
#'  \item \code{ModelParam} as a \linkS4class{data.table} with the parameters of the
#'  observation-prediction model;
#'
#'  \item \code{UnitParam} as a \linkS4class{data.table} with the parameters of each statistical
#'  unit (sampling weights and unit scores).
#'}
#'
#' @return Returns an invisible \code{NULL} plotting the one scatterplot per variable.
#'
#' @examples
#'
#' @rdname plot
#'
#' @import data.table SelEditUnitPriorit ggplot2
#'
#'
#' @export
setMethod(f = "plot",
          signature = c("AllocatedUnits", "character"),
          function(x, y, ...) {

            ExtraPar <- list(...)
            if (!'Query' %in% names(ExtraPar)) stop('[SelEditUnitAllocation::plot] Query is a compulsory parameter.\n')
            if (!'ModelParam' %in% names(ExtraPar)) stop('[SelEditUnitAllocation::plot] ModelParam is a compulsory parameter.\n')
            if (!'UnitParam' %in% names(ExtraPar)) stop('[SelEditUnitAllocation::plot] UnitParam is a compulsory parameter.\n')

            IDQual <- names(x@Units[[1]])
            DomainNames <- names(x@Domains)

            if (all(IDQual %in% names(ExtraPar$Query))) {


              InDomainQuer <- unlist(lapply(x@Units, function(DT){

                auxDT <- merge(DT, ExtraPar$Query, by = IDQual)

                if (dim(auxDT)[1] > 0) {

                  return(TRUE)

                } else {

                  return(FALSE)
                }
              }))

              #if (sum(InDomain) == 0) stop('[SelEditUnitAllocation::plot] The statistical unit(s) specified in Query not present in x (AllocatedUnits).\n')
              if (sum(InDomainQuer) > 1)  stop('[SelEditUnitAllocation::plot] The statistical unit(s) specified in Query must belong to a single sample domain in x (AllocatedUnits).\n')
              QueriedDomain <- merge(ExtraPar$UnitParam, ExtraPar$Query, by = IDQual)[, DomainNames, with = FALSE]
              setkeyv(QueriedDomain, DomainNames)
              QueriedDomain <- QueriedDomain[!duplicated(QueriedDomain)]
              InDomainSel <- unlist(lapply(seq(along = x@Units), function(index){

                auxDT <- merge(x@Domains[index], QueriedDomain, by = DomainNames)

                if (dim(auxDT)[1] > 0) {

                  return(TRUE)

                } else {

                  return(FALSE)
                }
              }))

              SelectedUnits <- x@Units[[which(InDomainSel)]]
              setkeyv(SelectedUnits, IDQual)



              nVar <- length(y)
              if (nVar > 4) stop('[SelEditUnitAllocation::plot] At most 4 variables can be plotted.\n')
              Data <- ExtraPar$ModelParam[IDEdit %in% y]
              Data <- merge(Data, ExtraPar$UnitParam, all.x = TRUE, by = IDQual)
              setkeyv(Data, DomainNames)
              Data <- Data[QueriedDomain]
              SelectedUnits[, Flagged := 1]
              Data <- merge(Data, SelectedUnits, all.x = TRUE, by = IDQual)
              Data[is.na(Flagged), Flagged := 0]
              Data[, Flagged := factor(Flagged, levels = c(1, 0), labels = c('Selected', 'Not Selected'))]
              ExtraPar$Query[, Queried := 10]
              Data <- merge(Data, ExtraPar$Query, by = IDQual, all.x = TRUE)
              Data[is.na(Queried), Queried := 7]
              Data[, Queried := factor(Queried, levels = c(10, 7), labels = c('Queried', 'Not Queried'))]
              setnames(Data, 'QuantileDesignW', 'QuantileSamplingWeight')

              out.graph <- ggplot(Data, aes(x = Var, y = PredValues)) +
                geom_errorbar(aes(ymin = PredValues - PredErrorSTD, ymax = PredValues + PredErrorSTD), width = .1) +
                geom_point(aes(size = QuantileSamplingWeight, colour = Flagged, shape = Queried)) +
                scale_shape_discrete(solid = FALSE) +#values = Data$Shape) +
                scale_colour_brewer(palette = "Set1") +
                labs(size = expression(c[omega])) +
                xlab('Raw values') +
                ylab('Predicted values') +
                geom_abline(intercept = 0, slope = 1) +
                geom_blank(data = Data, aes(x = Var, y = PredValues)) +
                facet_wrap( ~ IDEdit, ncol = nVar, scales = 'free') +
                theme(plot.title = element_text(size = rel(1.5), lineheight = 1.5, face = 'bold', colour = 'black'),
                      panel.background = element_rect(fill = "white"))

              Data <- Data[Queried == 'Queried']
              Data <- Data[, c(IDQual, DomainNames, 'IDEdit', 'QuantileErrorMoment', 'QuantileSamplingWeight', 'QuantileGlobalScore'), with = FALSE]
              Data <- as.data.frame(Data)
              names(Data) <- c(IDQual, DomainNames, 'IDEdit', expression(c[M[kk]]), expression(c[omega[k]]), expression(S[k]))
              tblData <- gridExtra::tableGrob(Data, rows = NULL, theme = gridExtra::ttheme_default(base_size = 9, colhead = list(fg_params = list(parse=TRUE))))
              gridExtra::grid.arrange(out.graph, tblData,
                                      nrow = 2,
                                      as.table = TRUE,
                                      heights = c(3, 1))
              return(invisible(NULL))
            }

            if (all(DomainNames %in% names(ExtraPar$Query))) {

              QueriedDomain <- merge(x@Domains, ExtraPar$Query, by = DomainNames)
              if (dim(QueriedDomain)[1] == 0) stop('[SelEditUnitAllocation::plot] The population domain specified in Query not present in x (AllocatedUnits).\n')
              if (dim(QueriedDomain)[1] > 1)  stop('[SelEditUnitAllocation::plot] The population domain specified in Query must belong to a single sample domain in x (AllocatedUnits).\n')
              setkeyv(QueriedDomain, DomainNames)
              InDomainSel <- unlist(lapply(seq(along = x@Units), function(index){

                auxDT <- merge(x@Domains[index], QueriedDomain, by = DomainNames)

                if (dim(auxDT)[1] > 0) {

                  return(TRUE)

                } else {

                  return(FALSE)
                }
              }))

              SelectedUnits <- x@Units[[which(InDomainSel)]]
              setkeyv(SelectedUnits, IDQual)
              Units <- merge(ExtraPar$UnitParam, QueriedDomain, by = names(QueriedDomain))

              nVar <- length(y)
              if (nVar > 4) stop('[SelEditUnitAllocation::plot] At most 4 variables can be plotted.\n')
              Data <- ExtraPar$ModelParam[IDEdit %in% y]
              Data <- merge(Data, ExtraPar$UnitParam, all.x = TRUE, by = IDQual)
              setkeyv(Data, DomainNames)
              Data <- Data[QueriedDomain]
              SelectedUnits[, Flagged := 1]
              Data <- merge(Data, SelectedUnits, all.x = TRUE, by = IDQual)
              Data[is.na(Flagged), Flagged := 0]
              Data[, Flagged := factor(Flagged, levels = c(1, 0), labels = c('Selected', 'Not Selected'))]
              Units[, Queried := 10]
              Data <- merge(Data, Units, by = intersect(names(Data), names(Units)), all.x = TRUE)
              Data[is.na(Queried), Queried := 7]
              Data[, Queried := factor(Queried, levels = c(10, 7), labels = c('Queried', 'Not Queried'))]
              setnames(Data, 'QuantileDesignW', 'QuantileSamplingWeight')
#return(Data)
              out.graph <- ggplot(Data, aes(x = Var, y = PredValues)) +
                geom_errorbar(aes(ymin = PredValues - PredErrorSTD, ymax = PredValues + PredErrorSTD), width = .1) +
                geom_point(aes(size = QuantileSamplingWeight, colour = Flagged, shape = Queried)) +
                scale_shape_discrete(solid = FALSE) +#values = Data$Shape) +
                scale_colour_brewer(palette = "Set1") +
                labs(size = expression(c[omega])) +
                xlab('Raw values') +
                ylab('Predicted values') +
                geom_abline(intercept = 0, slope = 1) +
                geom_blank(data = Data, aes(x = Var, y = PredValues)) +
                facet_wrap( ~ IDEdit, ncol = nVar, scales = 'free') +
                theme(plot.title = element_text(size = rel(1.5), lineheight = 1.5, face = 'bold', colour = 'black'),
                      panel.background = element_rect(fill = "white"))
              print(out.graph)
              return(invisible(NULL))

              }







          #setnames(ModelParam.dt, '')


          Layout <- grid.layout(nrow = 2, ncol = 1, heights = unit(c(2, 0.25), c("null", "null")))
          grid.show.layout(Layout)
          subplot <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
          grid.newpage()
          pushViewport(viewport(layout = Layout))
          print(out.graph, vp = subplot(1, 1))
          print(Data, vp = subplot(2, 1))

return(invisible(NULL))
})
