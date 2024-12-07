#' @title Filter data
#' 
#' @description This function filter the full database according to some criteria.
#' 
#' @param Data the rds file of the whole database
#' @param nIndivMin integer minimum number of trees in a plot, used for filtering (default 50)
#' @param propDetMin numeric minimum proportion of fully determined individuals in a plot, used for filtering (default 80)
#' @param Strat2keep vector listing all the strata to keep (default all the strata from 50mm DBH in database V6)
#' @param EFG2keep EFG to keep (default the 9 EFG used for paper on diversity patterns)
#' @param keepMorpho a boleean specifying if the morphospecies are kept (defaut is False)
#'
#' @return the rds file of the whole database after filtering 
#' (only the SpeciesObservations, Plots and CommunityObservations) are updated.
#' 
#' @details This function perform the following filters:
#' - filter out the plots with less than nIndivMin individuals and less than propDetMin fully identified individuals
#' - filter out the morpho-species 
#' - filter out the strata not in strata2keep
#' - filter out the plots without assigned EFG 
#' - filter out the EFG not in EFG to keep
#' 
#' @import data.table
#' 
#' @author Géraldine Derroire \email{geraldine.derroire@cirad.fr}
#' 
#' @export
#' 
#' 


filter_data <- function(nIndivMin = 50, # minimum number of trees in a plot to keep it
                        propDetMin = 0.8, # minimum proportion of fully determined sp in a plot
                        Data, # rds file of the whole database,
                        Strat2keep = c(">=100", ">=127", ">=150", ">=157", 
                                       ">=200", ">=50", ">=60" , ">=63.7", 
                                       ">=64", ">=80", ">=95.5", "50-100",
                                       ">=95", ">=75"),  # a vector of the strata to keep for the analysis
                        EFG2keep = c("T1.1 Tropical/Subtropical lowland rainforests",     
                                     "TF1.1 Tropical flooded forests and peat forests",             
                                     "T1.4 Tropical heath forests",                                 
                                     "T1.2 Tropical/Subtropical dry forests and thickets",          
                                     "T4.4 Temperate woodlands",                                    
                                     "T1.3 Tropical/Subtropical montane rainforests",  
                                     "T4.2 Pyric tussock savannas",                                 
                                     "T2.4 Warm temperate laurophyll forests",                      
                                     "T3.1 Seasonally dry tropical shrublands"), # the EFG to consider
                        keepMorpho = FALSE) # do we want to keep the morpho sp?
{
  # Retreive spobs and plot data sets
  Data_SpObs <- as.data.table(Data$SpeciesObservations)
  Data_Plot <- as.data.table(Data$Plots)
  # str(Data_Plot)
  # unique(Data_Plot$sts.iucn.efg) # efg in sts.iucn.efg
  # Data_Plot[is.na(sts.iucn.efg), .N] # NA in EFG
  #length(unique(Data_SpObs$organismNameCurated)) # raw number of species before filtering, including morphosp
  
  # Filter out the plots with less than nIndivMin individuals and less than propDetMin fully indentified
  NbIndTot <- Data_SpObs[, .(NbIndTot = sum(countsMeasurement)), by=uniquePlotID]
  NbIndFullDet <- Data_SpObs[organismNameTaxonRank=="species",
                             .(NbIndFullDet = sum(countsMeasurement)), by=uniquePlotID]
  Data4fitr <- merge(NbIndTot, NbIndFullDet, by="uniquePlotID", all.x=TRUE)
  Data4fitr$propFullDet <- Data4fitr[, NbIndFullDet/NbIndTot]
  Data4fitr[is.na(propFullDet), propFullDet:=0] # replace NA with 0 (cases of no fully determined sp)
  Plot2keep <- Data4fitr[NbIndTot>=nIndivMin &
                           propFullDet>=propDetMin, uniquePlotID]
  Data_SpObs <- Data_SpObs[uniquePlotID %in% Plot2keep]
  # length(unique(Data_SpObs$organismNameCurated))
  
  # filtering out the morpho-species 
  if (keepMorpho == FALSE) {
    Data_SpObs <- Data_SpObs[organismNameTaxonRank == "species"]
  }
  # length(unique(Data_SpObs$organismNameCurated)) 
  
  # filtering out the strata not in strata2keep
  Data_SpObs <- Data_SpObs[stratumName %in% Strat2keep]
  # length(unique(Data_SpObs$organismNameCurated))
  # length(unique(Data_SpObs$uniquePlotID))
  # sum(Data_SpObs$countsMeasurement) 
  
  # filtering out of data without assigned EFG 
  # add EFG to species obs
  Data_SpObs <- merge(Data_SpObs, Data_Plot[,.(uniquePlotID, sts.iucn.efg)], by="uniquePlotID")
  # filter out
  Data_SpObs <- Data_SpObs[!is.na(sts.iucn.efg)]
  # warning message if missing EFG  
  miss_EFG <- Data_SpObs[is.na(sts.iucn.efg), unique(uniquePlotID)]
  if(!(is.null(miss_EFG))) {
    warning(paste("There are", length(miss_EFG),
                  "plots with missing EFG. They have been filtered out the dataset."))
  }
  
  # filtering on the EFG
  Data_SpObs <- Data_SpObs[sts.iucn.efg %in% EFG2keep]
  # length(unique(Data_SpObs$organismNameCurated))
  # length(unique(Data_SpObs$uniquePlotID))
  # sum(Data_SpObs$countsMeasurement) 
  
  # filtering on the Plot table
  Data_Plot <- Data_Plot[uniquePlotID %in% unique(Data_SpObs$uniquePlotID)]
  # length(unique(Data_Plot$uniquePlotID))
  
  # filtering on the Community table
  Data_Com <- as.data.table(Data$CommunityObservations)
  Data_Com <- Data_Com[uniquePlotID %in% unique(Data_SpObs$uniquePlotID)]
  # length(unique(Data_Com$uniquePlotID))
  
  # make output list
  Data_fitr <- Data
  Data_fitr$SpeciesObservations <- as.data.frame(Data_SpObs)
  Data_fitr$Plots <- as.data.frame(Data_Plot)
  Data_fitr$CommunityObservations <- as.data.frame(Data_Com)
  
  # return filtered data
  return(Data_fitr) 
}







