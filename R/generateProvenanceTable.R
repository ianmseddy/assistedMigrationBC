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
                                      pixelGroupMap, method = 'default', sppEquiv, sppEquivCol) {

  browser()
  #get sppEquiv species name from transferTable
  transferTable <- copy(transferTable)
  joinCol <- c('BC_Forestry', eval(sppEquivCol))
  sppEquivSubset <- sppEquiv[, .SD, .SDcols = joinCol]

  transferTable <- transferTable[sppEquivSubset, on = c("species" = "BC_Forestry")]
  transferTable[, species := NULL]
  setnames(transferTable, eval(sppEquivCol), 'speciesCode')

  setkey(transferTable, BECvarfut_plantation)


  #figure out what each pixel will become
  currentAndFutureBECs <- data.table(pixID = 1:ncell(ecoregionMap),
                                     ecoID = getValues(ecoregionMap),
                                     projEcoregion = getValues(projectedBEC)) %>%
    na.omit(.)
  ecoregionTable <- as.data.table(ecoregionMap@data@attributes[[1]])
  currentAndFutureBECs <- currentAndFutureBECs[ecoregionMap@data@attributes[[1]], on = c('ecoID' = 'ID')]

  #Join BECkey with transferTable to get the ecoregionID representation of BEC zones
  #Fix differences
  transferTable <- BECkey[transferTable, on = c('zsv' = 'BECvarfut_plantation')]
  #reduce TransferTable - for sanity
  transferTable <- transferTable[ID %in% currentAndFutureBECs$projEcoregion]

  #Current and future BECs must be reduced from pixel index. It will be too much computational strain to find optimal provenance at pixel level
  # When scaling up, there may be multiple projected ecozones per ecozone - take mode
  currentAndFutureBECs <- currentAndFutureBECs[, .N, .(ecoID, projEcoregion, ecoregion, ecoregionGroup)] %>%
    .[, mode := max(N), .(ecoID)] %>%
    .[N == mode, .SD, .SDcols = c("ecoregionGroup", 'projEcoregion')]

  #currentAndFutureBECs can now be joined with transferTable to find the optimal provenance per ecoregion
  transferTable <- transferTable[currentAndFutureBECs, on  = c("ID" = 'projEcoregion')]

  # Noww projEcoregion = what it will become, and ecoregion = the original ecoregion
  if (method == "default") {

    #subset each ecoregionGroup/species by the minimum height reduction (ie max when expressed as proportion) among provenances
    optimalProvenance <- transferTable[, score := rank(HTp_pred, ties.method = 'random'), by = .(ecoregionGroup, speciesCode)]# %>%
    .[, best := max(score), .(BECvarfut_plantation, speciesCode)] %>%
      .[score == best,]

    setnames(optimalProvenance, old = 'BECvar_seed', new = "Provenance")

    provenanceTable <- optimalProvenance[, .(ecoregionGroup, Provenance, speciesCode)]

  } else if (method == "Elizabeth's other ideas") {
    #implement Elizabeth's other ideas here
  } else {
    stop("unrecognized method")
  }
    #When time is available, scenarios that aren't relevant to CS - e.g. no hardwood reforestation, no migration, etc


  provenanceTable[, ecoregionGroup := as.factor(ecoregionGroup)]
  provenanceTable[, Provenance := as.factor(Provenance)]
  #these must be factors.

  return(provenanceTable)
}

