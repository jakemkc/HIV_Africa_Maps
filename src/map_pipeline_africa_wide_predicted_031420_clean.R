###' Mar 14 2020
###' Goal: Create Africa Map pipline


rm(list=ls())
cat("\014")
options(max.print = 3000)
options(warn=1)



# *****************************************************************************
###' Steps (Ref to SQL)
###' A. 
###' B. 
# *****************************************************************************

# *****************************************************************************
## Notes
#' 1. 
# *****************************************************************************

# *****************************************************************************
## Checkpt comments
#' 1. 
# *****************************************************************************


## LOAD
library(tidyverse)
library(sp)
library(haven)
library(ggplot2)
library(cowplot)
library(maptools)
library(rgdal)

library(furrr)
plan(multisession)


source("./src/prep/utilFunctionsJk_predicted.R") # functons and long vector names
source("./src/prep/variableSets_state.R")


### ........ ---------------------------------------------------------------
### A. Create Maps per Country -------------


## Country meta info
countryVars <- mget(c("Angola", "Burkina_Faso", "Burundi", "Cameroon", "Chad", "Congo", "Congo_Democratic_Republic", "Cote_dIvoire", "Ethiopia", "Gabon", "Ghana", "Guinea", "Kenya", "Lesotho", "Liberia", "Malawi", "Mali", "Mozambique", "Namibia", "Rwanda", "Sao_Tome_and_Principe", "Senegal", "Sierra_Leone", "Swaziland", "Tanzania", "Togo", "Uganda", "Zambia", "Zimbabwe"))

# HIV, all subject
# female
plotObjHIV <- future_map(names(countryVars), perCountryObj, 0, "hivPredProb", "hiv05") %>% setNames(names(countryVars)) # only #25 zambia has the warning, but OK
# male
plotObjHIV <- future_map(names(countryVars), perCountryObj, 1, "hivPredProb", "hiv05") %>% setNames(names(countryVars)) # only #25 zambia has the warning, but OK


### ........ ---------------------------------------------------------------
### B. Create HIV Africa Maps -------------


## Africa Map
p <- AfricaMap(names(plotObjHIV), plotObjHIV, "0_female", "p.hiv")
p <- AfricaMap(names(plotObjHIV), plotObjHIV, "1_male", "p.hiv")


### ........ ---------------------------------------------------------------
### S. save-rdata -------------

## saving
outdirectory <- "./results"
outfilename <- sprintf("Africa_latestSvy_%s_%s.png", "0_female", "p_hiv")
outfilename <- sprintf("Africa_latestSvy_%s_%s.png", "1_male", "p_hiv")
savingpath <- file.path(outdirectory,outfilename)
ggsave(savingpath, plot = p, width = 10, height = 10, units = "in", scale = 1, dpi = 600) 

### ........ ---------------------------------------------------------------

