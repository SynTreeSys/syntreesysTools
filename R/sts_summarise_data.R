#' @title Summary Stats of the SynTreeSys Database
#' 
#' @description Simple function to return the number of plots, their
#'   effort, the number of species reported in each group among other
#'   relevant information
#' 
#' @param x a list containing the database.
#' @param spp.names character. The name of the column containing the
#'   species names to be used in the data summary.
#'
#' @return A data frame with the network and total stats in the
#'   columns.
#' 
#' @importFrom stats aggregate
#'
#' @export
sts_summarise_data <- function (x, 
                                spp.names = "organismNameOriginal") {
  
  redes <- sort(unique(database[["Plots"]]$network))
  df <- data.frame(networks = c(redes, "Total"),
                   plot.numb = NA,
                   plot.area = NA,
                   tree.numb = NA,
                   plot.obs = NA,
                   tree.spp = NA,
                   people = NA,
                   countries = NA,
                   institutions = NA)
  
  ## Add number of institutions
  
  # Number of plots
  df$plot.numb[dim(df)[1]] <- 
    dim(database[["Plots"]])[1] # number of plots
  tmp <- stats::aggregate(database[["Plots"]]$plotID, 
                          list(database[["Plots"]]$network), length) # plots per network
  df$plot.numb[-dim(df)[1]] <- tmp$x[match(tmp$Group.1, df$networks, 
                                           nomatch = 0)]
  
  # Total effort (in hectares)
  df$plot.area[dim(df)[1]] <- 
    sum(database[["Plots"]]$samplingEffort, na.rm = TRUE) # sampled area
  tmp <- stats::aggregate(database[["Plots"]]$samplingEffort, 
                          list(database[["Plots"]]$network), 
                          sum, na.rm = TRUE) # area per network
  df$plot.area[-dim(df)[1]] <- round(tmp$x[match(tmp$Group.1, df$networks, 
                                                 nomatch = 0)], 3)
  
  # Total effort (number of trees)
  df$tree.numb[dim(df)[1]] <- 
    sum(database[["SpeciesObservations"]]$countsMeasurement, 
        na.rm = TRUE) # number of trees
  tmp <- stats::aggregate(database[["SpeciesObservations"]]$countsMeasurement, 
                          list(database[["SpeciesObservations"]]$network), 
                          sum, na.rm = TRUE)
  df$tree.numb[-dim(df)[1]] <- round(tmp$x[match(tmp$Group.1, 
                                                 df$networks, nomatch = 0)], 0)
  
  # Total number of morphotypes/species
  df$tree.spp[dim(df)[1]] <- 
    length(unique(database[["SpeciesObservations"]][[spp.names]], 
                  na.rm = TRUE)) # number of morphotypes
  tmp <- stats::aggregate(database[["SpeciesObservations"]][[spp.names]], 
                          list(database[["SpeciesObservations"]]$network), 
                          function(x) length(unique(x))) # spp per network
  df$tree.spp[-dim(df)[1]] <- round(tmp$x[match(tmp$Group.1, 
                                                df$networks, nomatch = 0)], 0)
  
  # Species/aundance observations per per plot per network
  df$plot.obs[dim(df)[1]] <- 
    dim(database[["SpeciesObservations"]])[1] # number of species x sites observations
  tmp <- tapply(database[["SpeciesObservations"]]$network, 
                database[["SpeciesObservations"]]$network, length) 
  df$plot.obs[-dim(df)[1]] <- tmp[match(names(tmp), df$networks)]
  
  # Number of people
  df$people[dim(df)[1]] <- length(unique(database[["People"]]$name))
  tmp <- stats::aggregate(database[["People"]]$name, 
                          list(database[["People"]]$network), 
                          function(x) length(unique(x)))
  for (i in seq_along(redes)) {
    tmp1 <- tmp[grepl(redes[i], tmp$Group.1, ignore.case = TRUE), ]
    df$people[df$networks %in% redes[i]] <- sum(tmp1$x)
  }
  
  # Countries
  df$countries[dim(df)[1]] <- length(unique(database[["Plots"]]$country))
  tmp <- stats::aggregate(database[["Plots"]]$country, 
                   list(database[["Plots"]]$network), 
                   function(x) length(unique(x)))
  df$countries[-dim(df)[1]] <- round(tmp$x[match(tmp$Group.1, df$networks, 
                                                 nomatch = 0)], 0)
  
  return(df)
}