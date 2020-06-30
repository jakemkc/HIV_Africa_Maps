## Custome functions


### ........ ---------------------------------------------------------------
### A. Plot map per country -------------

perCountryMap <- function(country, filterVar, outcomeY, outcomeYW) {
  
  # ## Input
  # country = "Angola" #names of countryVar
  # country = "Zambia" #names of countryVar
  # country = "Burkina_Faso" #names of countryVar
  # country = "Kenya" #names of countryVar
  # # HIV set
  # filterVar = 0 # "all", 0, 1; (0 = female, 1 = male)
  # outcomeY = "hiv03"
  # outcomeY = "hivPredProb"
  # outcomeY = "v012"
  # outcomeYW = "hiv05"
  # # predictor female set
  # filterVar = 0 # "all", 0, 1; (0 = female, 1 = male)
  # outcomeY = "v535_0"
  # outcomeYW = "v005"
  # predictor male set
  # filterVar = 1 # "all", 0, 1; (0 = female, 1 = male)
  # outcomeY = "hv116_0"
  # outcomeYW = "mv005"

  
  ## a Read data ----
  
  inCountry <- countryVars[[country]][1]
  inYear <- countryVars[[country]][2]
  # inGPS <- countryVars[[country]][3]
  # bondName <- countryVars[[country]][4]
  # inGPSdata <- sprintf("%s.dbf", inGPS)
  inPredHiv <- countryVars[[country]][5]
  
  
  dataPath <- file.path("data", "map", inCountry, inYear, "flattenedfile.encoded.rds")
  boundaryPath <- file.path("data", "map", inCountry, inYear, "dhs_boundary", "shps", "sdr_subnational_boundaries.shp")
  mPredHivPath <- file.path("data", "predicted_Prob_062420", sprintf("%s_10_m.csv", inPredHiv))
  fPredHivPath <- file.path("data", "predicted_Prob_062420", sprintf("%s_10_f.csv", inPredHiv))
  
  # read encoded data
  dhsDataFlat <- readRDS(dataPath)
  # read country boundary
  dhsShape <- rgdal::readOGR(boundaryPath)
  # # read country bounary
  # dhsBound <- create.boundary(bondName)

  # remove labelled
  # dhsDataFlat <- zap_labels(dhsDataFlat)
  
  ## **************************
  ## Join predict HIV
  # Load
  m <- readr::read_csv(mPredHivPath)
  f <- readr::read_csv(fPredHivPath)
  
  # Combine
  mf <- bind_rows(select(m, "ID", "hivPredProb"), select(f, "ID", "hivPredProb"))
  # Join
  dhsDataFlat <- dhsDataFlat %>% left_join(mf, by = "ID")
  
  
  ## b. prep X data -------------
  
  ## **************************
  # create partial shapefile df 1/2
  shapeDf <- data.frame(id = dhsShape$REGCODE, nameState = dhsShape$DHSREGEN)
  
  
  ## **************************
  ## Prepare dataframe
  # How many level of v024
  v024Length <- dhsDataFlat %>% select(starts_with("v024")) %>% length
  
  # CREATE v024 categorical
  dhsDataFlat <- dhsDataFlat %>% select(starts_with("v024"), ID) %>% 
    # setNames(c(as.character(1:v024Length), "ID")) %>% 
    setNames(str_replace(names(.), "^v024_", "")) %>%
    gather("v024", "value", -ID) %>% 
    filter(value == 1) %>% 
    left_join(dhsDataFlat %>% select(ID, !!sym(outcomeY), !!sym(outcomeYW), hv104_1), ., by = "ID") # "v024_" NA still as "v024" NA; v012 = age
  
  # Filter sex
  if (filterVar != "all") {
    dhsDataFlat <- dhsDataFlat %>% filter(hv104_1 == filterVar) # drop case not 0 , 1; it drops NA too; No need to care NSE in filterVar here (it reads 0 or "0")
  }
  
  
  ## **************************
  # create partial shapefile dataset 2/2
  
  # create hiv summary statistics by state
  mapSmy <- dhsDataFlat %>% group_by(v024) %>% 
    summarise(pHIV = weighted.mean(!!sym(outcomeY), !!sym(outcomeYW)/1000000, na.rm = TRUE) 
    ) # it created an NA row, OK
  
  # If filter == "all", Liberia, Sierra, Burkina, and Mali wont be accurate as they have new v024 columns
  if (inCountry == "Malawi") {
    mapSmy$v024 <- c("1", "2", "4", NA) # Malawi all dex
  }
  
  if (inCountry == "Ethiopia" & filterVar == 1) {
    mapSmy$v024 <- c("1", "12", "13", "14", "15", "7", "2", "3", "4", "5", "6", NA) # Ethiopia male
  }
  
  if (inCountry == "Liberia" & filterVar == 1) {
    mapSmy$v024 <- c("5", "1", "2", "3", "4", NA) # Liberia male
  }
  
  if (inCountry == "Sierra_Leone" & filterVar == 1) {
    mapSmy$v024 <- c("1", "2", "3", "4", NA) # Sierra_Leone male
  }
  
  if (inCountry == "Burkina_Faso" & filterVar == 1) {
    mapSmy$v024 <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", NA)  # Burkina_Faso male
  }
  
  if (inCountry == "Mali" & filterVar == 1) {
    mapSmy$v024 <- c("1", "2", "3", "4", "5", "9", NA) # Mali male
  }
  
  # Use state ID to match because both DHS boundary uses v024 information
  # join
  shapeDf$id <- shapeDf$id %>% as.character() # char id can't join numeric id
  shapeDf <- left_join(shapeDf, mapSmy, by = c("id" = "v024"))
  
  
  ## **************************
  # create plot full dataset
  # Fortify file
  fortify_shape <- fortify(dhsShape, region = "REGCODE") # it has warnings, OK
  
  # merge fortify to shapfile data and reorder
  merged_data <- left_join(fortify_shape, shapeDf, by = "id")
  
  mapPlot = merged_data[order(merged_data$order), ] # optional. it's in that order already
  
  
  ### c. Plotting with ggplot2 ---------------
  ## ggplot plot (take a while to load)
  out <- ggplot(mapPlot, aes(x = long, y = lat, group = group, fill = pHIV)) +
        geom_polygon(color = "black", size = 0.3) +
        coord_map() + # coord_map projects earth (approx. spherical) onto a flat 2D plane
        scale_fill_distiller(name = "Predicted \nProb. of HIV") + # better original color scale; name is legend name
        labs(title="Predicted probability of HIV, by admin level 1 boundary")

  return(out)
}




### B. Plot map object per country -------------


perCountryObj <- function(country, filterVar, outcomeY, outcomeYW) {
  
  # ## Input
  # country = "Angola" #names of countryVar
  # country = "Zambia" #names of countryVar
  # country = "Burkina_Faso" #names of countryVar
  # country = "Kenya" #names of countryVar
  # # HIV set
  # filterVar = 0 # "all", 0, 1; (0 = female, 1 = male)
  # outcomeY = "hiv03"
  # outcomeY = "hivPredProb"
  # outcomeY = "v012"
  # outcomeYW = "hiv05"
  # # predictor female set
  # filterVar = 0 # "all", 0, 1; (0 = female, 1 = male)
  # outcomeY = "v535_0"
  # outcomeYW = "v005"
  # predictor male set
  # filterVar = 1 # "all", 0, 1; (0 = female, 1 = male)
  # outcomeY = "hv116_0"
  # outcomeYW = "mv005"
  
  
  ## a Read data ----
  
  inCountry <- countryVars[[country]][1]
  inYear <- countryVars[[country]][2]
  # inGPS <- countryVars[[country]][3]
  # bondName <- countryVars[[country]][4]
  # inGPSdata <- sprintf("%s.dbf", inGPS)
  inPredHiv <- countryVars[[country]][5]
  
  dataPath <- file.path("data", "map", inCountry, inYear, "flattenedfile.encoded.rds")
  boundaryPath <- file.path("data", "map", inCountry, inYear, "dhs_boundary", "shps", "sdr_subnational_boundaries.shp")
  mPredHivPath <- file.path("data", "predicted_Prob_062420", sprintf("%s_10_m.csv", inPredHiv))
  fPredHivPath <- file.path("data", "predicted_Prob_062420", sprintf("%s_10_f.csv", inPredHiv))
  
  
  
  # read encoded data
  dhsDataFlat <- readRDS(dataPath)
  # read country boundary
  dhsShape <- rgdal::readOGR(boundaryPath)
  # # read country bounary
  # dhsBound <- create.boundary(bondName)
  
  # remove labelled
  # dhsDataFlat <- zap_labels(dhsDataFlat)
  
  ## **************************
  ## Join predict HIV
  # Load
  
  #` 062620 decided to have Kajal get Senegal even low HIV prevalence, so chirag and I both using 29 countries
  # # Senegal has no female
  # if (inCountry == "Senegal") {
  #   m <- readr::read_csv(mPredHivPath)
  #   mf <- select(m, "ID", "hivPredProb")
  # } else {
  #   m <- readr::read_csv(mPredHivPath)
  #   f <- readr::read_csv(fPredHivPath)
  #   
  #   # Combine
  #   mf <- bind_rows(select(m, "ID", "hivPredProb"), select(f, "ID", "hivPredProb"))
  # }
  
    m <- readr::read_csv(mPredHivPath)
    f <- readr::read_csv(fPredHivPath)

    # Combine
    mf <- bind_rows(select(m, "ID", "hivPredProb"), select(f, "ID", "hivPredProb"))
  
  # Join
  dhsDataFlat <- dhsDataFlat %>% left_join(mf, by = "ID")
  
  
  ## b. prep X data -------------
  
  ## **************************
  # create partial shapefile df 1/2
  shapeDf <- data.frame(id = dhsShape$REGCODE, nameState = dhsShape$DHSREGEN)
  
  
  ## **************************
  ## Prepare dataframe
  # How many level of v024
  v024Length <- dhsDataFlat %>% select(starts_with("v024")) %>% length
  
  # CREATE v024 categorical
  dhsDataFlat <- dhsDataFlat %>% select(starts_with("v024"), ID) %>% 
    # setNames(c(as.character(1:v024Length), "ID")) %>%
    setNames(str_replace(names(.), "^v024_", "")) %>%
    gather("v024", "value", -ID) %>% 
    filter(value == 1) %>% 
    left_join(dhsDataFlat %>% select(ID, !!sym(outcomeY), !!sym(outcomeYW), hv104_1), ., by = "ID") # "v024_" NA still as "v024" NA; v012 = age
  
  # Filter sex
  if (filterVar != "all") {
    dhsDataFlat <- dhsDataFlat %>% filter(hv104_1 == filterVar) # drop case not 0 , 1; it drops NA too; No need to care NSE in filterVar here (it reads 0 or "0")
  }
  
  
  ## **************************
  # create partial shapefile dataset 2/2
  
  # create hiv summary statistics by state
  mapSmy <- dhsDataFlat %>% group_by(v024) %>% 
    summarise(pHIV = weighted.mean(!!sym(outcomeY), !!sym(outcomeYW)/1000000, na.rm = TRUE) 
    ) # it created an NA row, OK
  
  
  # If filter == "all", Liberia, Sierra, Burkina, and Mali wont be accurate as they have new v024 columns
  if (inCountry == "Malawi") {
    mapSmy$v024 <- c("1", "2", "4", NA) # Malawi all dex
  }
  
  if (inCountry == "Ethiopia" & filterVar == 1) {
    mapSmy$v024 <- c("1", "12", "13", "14", "15", "7", "2", "3", "4", "5", "6", NA) # Ethiopia male
  }
  
  if (inCountry == "Liberia" & filterVar == 1) {
    mapSmy$v024 <- c("5", "1", "2", "3", "4", NA) # Liberia male
  }
  
  if (inCountry == "Sierra_Leone" & filterVar == 1) {
    mapSmy$v024 <- c("1", "2", "3", "4", NA) # Sierra_Leone male
  }
  
  if (inCountry == "Burkina_Faso" & filterVar == 1) {
    mapSmy$v024 <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", NA)  # Burkina_Faso male
  }
  
  if (inCountry == "Mali" & filterVar == 1) {
    mapSmy$v024 <- c("1", "2", "3", "4", "5", "9", NA) # Mali male
  }
  
  
  # Use state ID to match because both DHS boundary uses v024 information
  # join
  shapeDf$id <- shapeDf$id %>% as.character() # char id can't join numeric id
  shapeDf <- left_join(shapeDf, mapSmy, by = c("id" = "v024"))
  
  
  ## **************************
  # create plot full dataset
  # Fortify file
  fortify_shape <- fortify(dhsShape, region = "REGCODE") # it has warnings, OK
  
  # merge fortify to shapfile data and reorder
  merged_data <- left_join(fortify_shape, shapeDf, by = "id")
  
  mapPlot = merged_data[order(merged_data$order), ] # optional. it's in that order already
  
  return(mapPlot)
}




### C. Plot Africa map -------------

AfricaMap <- function(countryList, plotObj, filterMapAnnot, yMapAnnot) {
  ## Map annotation. Name on Map ONLY
  # INPUT
  # yMapAnnot = "hiv03"
  # filterMapAnnot = "all"
  
  ## GGPLOT 1 Africa map
  require(maptools)
  africaMapImport <- rgdal::readOGR("./data/Africa_SHP/Africa.shp") # http://www.maplibrary.org/library/stacks/Africa/index.htm
  
  # sp class to dataframe
  africaMap <- fortify(africaMapImport) 
  
  
  ## ggplot code
  p <- ggplot()
  p <- p + geom_path(data = africaMap, aes(x = long, y = lat, group = group), size = 0.2, color = "grey50")
  
  for(i in 1:length(countryList)) {
    var <- countryList[i]
    # plotData
    plotInput <- plotObj[[var]]
    
    p <- p + geom_polygon(data = plotInput, aes(x = long, y = lat, group = group, fill = pHIV))
    # p <- p + geom_path(data = plotInput, aes(x = long, y = lat, fill = NULL, group = group), size = 0.35, color = "black")
    p <- p + geom_path(data = plotInput, aes(x = long, y = lat, fill = NULL, group = group), size = 0.22, color = "black") # at home LCD default
    print(var)
  }
  
  p <- p + coord_quickmap() # ref: https://ggplot2.tidyverse.org/reference/coord_map.html
  ## adaptive or fixed scale
  # p <- p + scale_fill_gradientn(colours = prevR.colors.red(20), name = "Percentage")
  # p <- p + scale_fill_gradientn(colours = prevR.colors.red(20), breaks = c(0, 0.1, 0.2, 0.3, 0.4), limits = c(0, 0.5), name = "Predicted \nprobability \nof HIV")
  p <- p + scale_fill_distiller(limits = c(0, 0.5), name = "Predicted \nprobability \nof HIV")
  p <- p + ggtitle(sprintf("%s, %s", filterMapAnnot, yMapAnnot))
  p <- p + xlab("Longitude") + ylab("Latitude")
  # p <- p + theme_prevR_light() + # swtich if you don't want ggplot bg
  # p <- p + theme_cowplot(12)
  return(p)
}



