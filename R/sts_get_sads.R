#' @title Get the Species Abundances Distributions per Group
#' 
#' @param x the SynTreeSys database
#' @param minN numerical. The minimum number of individuals per plot.
#'   Defaults to zero.
#' @param maxN numerical. The maximum number of individuals per plot.
#'   Defaults to Inf.
#' @param minCutOff numerical. The minimum size criteria cutoff of the
#'   plot (in mm). Defaults to zero.
#' @param group.name character. The STS column name with the categorie
#'   used for grouping abundances.
#' @param spp.name character. The STS column name with the
#'   (morpho)species name. Defaults to "organismNameCurated".
#' @param remove.indet logical. Should morphospecies not identified at
#'   the species level be removed? Defaults to TRUE
#' @param abund.metric character. The STS column name with the
#'   abundance metric to be used to build the SAD. Defaults to
#'   "countsMeasurement"
#' @param strata.names vector of characters. The names of the strata
#'   the should be kept in the analyses.
#' 
#' 
#' @importFrom stats aggregate reshape
#' 
#' 
#' @export
#' 
sts_get_sads <- function(x, 
                     minN = 0, 
                     maxN = Inf, 
                     minCutOff = 0, 
                     group.name = NULL,
                     spp.name = "organismNameCurated",
                     remove.indet = TRUE,
                     abund.metric = "countsMeasurement",
                     strata.names = NULL) {
  
  ids2keep <- x$individuals >= minN & x$sizeCutoffMin >= minCutOff
  plot2keep <- x$uniquePlotID[ids2keep]
  x1 <- x[x$uniquePlotID %in% plot2keep,]

  if (remove.indet)
    x1 <- x1[x1$organismNameTaxonRank %in% "species", ]
  
  if(!is.null(strata.names)){
    ## TO BE FINSHED ##
  }
  
  sad.per.group <- stats::aggregate(x1[[abund.metric]], 
                             list(x1[[spp.name]], x1[[group.name]]), 
                             sum, na.rm = TRUE)
  names(sad.per.group) <- c(spp.name, group.name, abund.metric)
  sad.per.group1 <- stats::reshape(sad.per.group, direction = "wide",
                              idvar = spp.name,
                              timevar = group.name,
                              v.names = abund.metric)
  colnames(sad.per.group1) <- gsub("countsMeasurement.", "",
                                     colnames(sad.per.group1))
  sad.per.group1[is.na(sad.per.group1)] <- 0

  return(sad.per.group1)  
}