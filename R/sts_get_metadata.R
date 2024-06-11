#' @title Prepare Data For Analyses
#' 
#' @description Simple function to get species abundances and plot
#'   metadata together for analyses
#' 
#' @param x the SynTreeSys database
#' @param output character. Should the plot metadat be returned (i.e.
#'   'Plots') or the the species table (i.e. 'SpeciesObservations')
#'   with the plot metadata? Defaults to 'SpeciesObservations'.
#'
#' @return A data frame
#' 
#' @importFrom dplyr left_join
#' 
#' @export
#' 
sts_get_metadata <- function(x, output = "SpeciesObservations") {
  
  ## Isolating the tables
  plots <- x$Plots
  comms <- x$CommunityObservations
  spps <- x$SpeciesObservations
  
  ## Checking table contents
  plots_plots <- unique(plots$uniquePlotID)
  plots_comms <- unique(comms$uniquePlotID)
  plots_spps <- unique(spps$uniquePlotID)
  
  check1 <- setdiff(plots_plots, plots_comms)
  if (length(check1) > 0L) {
    warning("One or more plots in the CommunityObservations are not in the Plots table. Removing them from the CommunityObservations table")
    comms <- comms[!comms$uniquePlotID %in% check1, ]
  }

  check2 <- setdiff(plots_plots, plots_spps)
  if (length(check2) > 0L) {
    warning("One or more plots in the SpeciesObservations are not in the Plots table. Removing them from the Plots table")
    plots <- plots[!plots$uniquePlotID %in% check2, ]
  }
  
  ## Checking for duplicated entries
  dup_comms_plot <- comms$uniquePlotID[duplicated(comms$uniquePlotID)]
  if  (length(dup_comms_plot) > 0L){
    warning("One or more plots in CommunityObservations are duplicated. Removing: ", 
            paste(dup_comms_plot,collapse=", "))
    comms <- comms[!duplicated(comms$uniquePlotID),]
  }
  
  ## Getting the necessary descriptions for the species observations
  dados1 <- dplyr::left_join(plots, comms,
                             by = "uniquePlotID")

  if (output == "Plots") {
    return(dados1)    
  }
  
  if (output == "SpeciesObservations") {
    spps1 <- dplyr::left_join(spps, dados1,
                              by = "uniquePlotID")
    return(spps1)    
  }
}