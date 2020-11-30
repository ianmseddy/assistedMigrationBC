landscapeCalc <- function(cohortData, pixelGroupMap, time) {
  summaryStats <- copy(cohortData)
  rm(cohortData)
  set(summaryStats, NULL, 'speciesCode', NULL) #make space
  set(summaryStats, NULL, 'ecoregionGroup', NULL)#make space
  pgs <- data.table('pixelGroup' = getValues(pixelGroupMap))
  pgs <- pgs[, .('N' = .N), .(pixelGroup)]

  #calculate biomass weighted growth/mort/HTp modifier in each pixelGroup. Then weight by pixels.
  plantedOnly <- summaryStats[planted == TRUE,]
  summaryStats <- summaryStats[, .(BweightedGrowth = sum(growthPred * B, na.rm = TRUE)/sum(B),
                                 BweightedMort = sum(mortPred * B, na.rm = TRUE)/sum(B),
                                 BweightedHTp = sum(HTp_pred * B, na.rm = TRUE)/sum(B),
                                 sumB = sum(B)),
                               .(pixelGroup)]
  summaryStats <- summaryStats[BweightedMort > 0] #removes pixels that are only newly established cohorts - these will have no meaningful data
  summaryStats <- pgs[summaryStats, on = c("pixelGroup")] #this will give the number of PGs in each pixelGroup  - we can biomass weight with the full amount
  summaryStats <- summaryStats[, .(BweightedGrowth = sum(BweightedGrowth * N, na.rm = TRUE)/sum(N),
                                 BweightedMort = sum(BweightedMort * N, na.rm = TRUE)/sum(N),
                                 BweightedHTp = sum(BweightedHTp * N, na.rm = TRUE)/sum(N))]

  plantedOnly <- plantedOnly[, .(BweightedGrowth = sum(growthPred * B, na.rm = TRUE)/sum(B),
                                 BweightedMort = sum(mortPred * B, na.rm = TRUE)/sum(B),
                                 BweightedHTp = sum(HTp_pred * B, na.rm = TRUE)/sum(B),
                                 sumB = sum(B)),
                             .(pixelGroup)]
  plantedOnly <- plantedOnly[BweightedMort > 0]
  if (nrow(plantedOnly) > 0) {
    plantedOnly <- pgs[plantedOnly, on = c("pixelGroup")] #this will give the number of PGs in each pixelGroup  - we can biomass weight with the full amount
    plantedOnly <- plantedOnly[, .(BweightedGrowth = sum(BweightedGrowth * N, na.rm = TRUE)/sum(N),
                                  BweightedMort = sum(BweightedMort * N, na.rm = TRUE)/sum(N),
                                  BweightedHTp = sum(BweightedHTp * N, na.rm = TRUE)/sum(N))]
    output <- data.table('year' = time,
                         landscapeGrowth = summaryStats$BweightedGrowth,
                         landscapeMort = summaryStats$BweightedMort,
                         landscapeHTp = summaryStats$BweightedHTp,
                         plantedGrowth = plantedOnly$BweightedGrowth,
                         plantedMort = plantedOnly$BweightedMort,
                         plantedHTp = plantedOnly$BweightedHTp)
  } else {
    output <- data.table('year' = time,
                         landscapeGrowth = summaryStats$BweightedGrowth,
                         landscapeMort = summaryStats$BweightedMort,
                         landscapeHTp = summaryStats$BweightedHTp,
                         plantedGrowth = NA,
                         plantedMort = NA,
                         plantedHTp = NA)
  }

  return(output)
}