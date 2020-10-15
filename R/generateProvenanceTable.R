#' Generate a provenance table for reforestation. This details what species are planted in what ecoregionGroups
#' and optionally allows for a climate-sensitive genetic modifier
#' @param ecoregionMap the ecoregion raster
#' @param transferTable a data.table containing height reductions by seed source and ecoregion. At the moment this is
#' BECvarfut_plantation, BECvar_seed, and HTp_pred. If not provided, this will return a table where
#' every species is planted wherever possible, according to ecoregionDT
#' @param BECkey a data.table that has fields BEC zone/subzone/variants and their corresponding raster values
#' @param projectedBEC the projected BEC zones. The zones MUST correspond to ecoregionKey and cohortData
#' @param sppEquiv the LandR sppEquivalency table for matchign species names
#' @param sppEquivCol the column in sppEquiv by which provenanceTable species should be named
#' @param method experimental - this will change how particular species are prioritized for planting
#' @return a provenance table object to be used with LandR_reforestation
#' @export
generateBCProvenanceTable <- function(transferTable, BECkey, projectedBEC, ecoregionMap,
                                      method = 'default', sppEquiv, sppEquivCol) {

  #get sppEquiv species name from transferTable
  TransferTable <- copy(transferTable)
  BECkey <- copy(BECkey) #to prevent problems arising from factor to integer conversions in other functions
  setkey(TransferTable, BECvarfut_plantation)


  #figure out what each pixel will become
  currentAndFutureBECs <- data.table(pixID = 1:ncell(ecoregionMap),
                                     ecoID = getValues(ecoregionMap),
                                     projEcoregion = getValues(projectedBEC)) %>%
    na.omit(.)
  ecoregionTable <- as.data.table(ecoregionMap@data@attributes[[1]])
  ecoregionTable[, ecoregion := as.factor(ecoregion)]
  currentAndFutureBECs <- currentAndFutureBECs[ecoregionTable, on = c('ecoID' = 'ID')]

  #Join BECkey with TransferTable to get the ecoregionID representation of BEC zones

  TransferTable <- BECkey[TransferTable, on = c('zsv'= 'BECvarfut_plantation')] %>%
    .[!is.na(ID)] #we only need  info re: variants that are actually on landscape

  TransferTable <- TransferTable[ID %in% currentAndFutureBECs$projEcoregion]

  #Current and future BECs must be reduced from pixel index.
  #It will be too much computational strain to find optimal provenance at pixel level
  # When scaling up, there may be multiple projected ecozones per ecozone - take mode
  currentAndFutureBECs <- currentAndFutureBECs[, .N, .(ecoID, projEcoregion, ecoregion, ecoregionGroup)] %>%
    .[, mode := max(N), .(ecoID)] %>%
    .[N == mode, .SD, .SDcols = c("ecoregionGroup", 'projEcoregion')]


  #remove ties - this is likely when there are 2 pixels each
  if (nrow(currentAndFutureBECs) > length(unique(currentAndFutureBECs$ecoregionGroup))) {
    #ties exist
    currentAndFutureBECs[, N := .N, .(ecoregionGroup)]
    noTies <- currentAndFutureBECs[N == 1]
    ties <- currentAndFutureBECs[N > 1]
    ties$foo <- sample(x = 1:nrow(ties), size = nrow(ties))
    setkey(ties, foo)
    ties <- ties[!duplicated(ties[, .(ecoregionGroup)])]
    ties[, foo := NULL]
    currentAndFutureBECs <- rbind(ties, noTies)
  }
  # Noww rojEcoregion = what it became , and ecoregion = the original ecoregion
  if (method == "default") {

    #subset each ecoregionGroup/species by the minimum height reduction (ie max when expressed as proportion) among provenances
    optimalProvenance <- TransferTable[, score := rank(HTp_pred, ties.method = 'random'), by = .(ID, speciesCode)]
    optimalProvenance <- optimalProvenance[, best := max(score), .(ID, speciesCode)]
    optimalProvenance <- optimalProvenance[score == best,]
    setnames(optimalProvenance, old = 'BECvar_seed', new = "Provenance")
    provenanceTable <- optimalProvenance[, .(ID, Provenance, speciesCode)]

  } else if (method == "noOptimization") {
    sameProvenance <- TransferTable[BECvar_seed == zsv]
    setnames(sameProvenance, old = c('BECvar_seed'), new = c("Provenance"))
    provenanceTable <- sameProvenance[, .(ID, Provenance, speciesCode)]

  } else {
    stop("unrecognized method")
  }

  #currentAndFutureBECs can now be joined with TransferTable to find the optimal provenance per ecoregion
  #each projected Ecoregion may have multiple species, while each ecoregion has many ecoregionGroups, hence cartesian

  provenanceTable <- provenanceTable[currentAndFutureBECs, on  = c("ID" = 'projEcoregion'), allow.cartesian = TRUE] %>%
    .[, .SD, .SDcols = c("ecoregionGroup", 'speciesCode', 'Provenance')]

  provenanceTable[, ecoregionGroup := as.factor(ecoregionGroup)]
  provenanceTable[, Provenance := as.factor(Provenance)]
  #these must be factors.

  return(provenanceTable)
}

