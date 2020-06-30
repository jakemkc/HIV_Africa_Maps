## VariableSets for HIV mapping

## Note added on 032420
#' this is to get state admin level 1 map, I don't need GPS
#' and I found from Kajal's predicted HIV, she gets Senegal, asking her the flattened data
#' So I have 3 more countries compared to that prevR need GPS map


## Note
#' 1st is Folder country name
#' 2nd is path name (survey year)
#' 3rd is the GPS file name
#' 4th is the country name in the boundary file (if using PrevR for density map, run "create.boundary()")
#' 5th is predicted HIV partial file name (for state map)
#' 
#' Burkina_faso, Burundi, and Chad are added by Jake on top of Jessica's data folder
#' Then added a few more after HIV meeting on 100819: cameroon, mozambique, tanzania, uganda. 
#' - "Congo" and "Sao tome and Princpe" x2 have HIV and *.encoded.rds, but NO GPS files
#' - These are 2 different counties. "Congo" = Congo republic = The republic of Congo, NOT = "Congo_Democratic_Republic

## Total 26 countries

Angola <- c("Angola", "Standard_DHS_2015-16", "AOGE71FL", "Angola", "Angola_2015-16")
Burkina_Faso <- c("Burkina_Faso", "Standard_DHS_2010", "BFGE61FL", "Burkina Faso", "Burkina_Faso_2010")
Burundi <- c("Burundi", "Standard_DHS_2010", "BUGE61FL", "Burundi", "Burundi_2010")
Cameroon <- c("Cameroon", "Standard_DHS_2004", "CMGE42FL", "Cameroon", "Cameroon_2004")
Chad <- c("Chad", "Standard_DHS_2014-15", "TDGE71FL", "Chad", "Chad_2014-15")

# new for state map
Congo <-c("Congo", "Standard_AIS_2009", "NIL", "Congo", "Congo_2009") # new

Congo_Democratic_Republic <- c("Congo_Democratic_Republic", "Standard_DHS_2013-14", "CDGE61FL", "Democratic Republic of the Congo", "Congo_Democratic_Republic_2013-14")

# speical R cant handle that ' name
Cote_dIvoire <- c("Cote_d'Ivoire", "Standard_DHS_2011-12", "CIGE61FL", "Cote d'Ivoire", "Cote_d'Ivoire_2011-12")

Ethiopia <- c("Ethiopia", "Standard_DHS_2011", "ETGE61FL", "Ethiopia", "Ethiopia_2011")
Gabon <- c("Gabon", "Standard_DHS_2012", "GAGE61FL", "Gabon", "Gabon_2012")
Ghana <- c("Ghana", "Standard_DHS_2014", "GHGE71FL", "Ghana", "Ghana_2014")
Guinea <- c("Guinea", "Standard_DHS_2012", "GNGE61FL", "Guinea", "Guinea_2012")
Kenya <- c("Kenya", "Standard_DHS_2008-09", "KEGE52FL", "Kenya", "Kenya_2008-09")
Lesotho <- c("Lesotho", "Standard_DHS_2014", "LSGE71FL", "Lesotho", "Lesotho_2014")
Liberia <- c("Liberia", "Standard_DHS_2013", "LBGE6AFL", "Liberia", "Liberia_2013")
Malawi <- c("Malawi", "Standard_DHS_2015-16", "MWGE7AFL", "Malawi", "Malawi_2015-16")
Mali <- c("Mali", "Standard_DHS_2012-13", "MLGE6BFL", "Mali", "Mali_2012-13")
Mozambique <- c("Mozambique", "Standard_AIS_2009", "MZGE52FL", "Mozambique", "Mozambique_2009")
Namibia <- c("Namibia", "Standard_DHS_2013", "NMGE61FL", "Namibia", "Namibia_2013")
Rwanda <- c("Rwanda", "Standard_DHS_2014-15", "RWGE72FL", "Rwanda", "Rwanda_2014-15")

# new for state map
Sao_Tome_and_Principe <- c("Sao_Tome_and_Principe", "Standard_DHS_2008-09", "NIL", "Sao Tome and Principe", "Sao_Tome_and_Principe_2008-09")
Senegal <- c("Senegal", "Standard_DHS_2010-11", "NIL", "Senegal", "Senegal_2010-11")


Sierra_Leone <- c("Sierra_Leone", "Standard_DHS_2013", "SLGE61FL", "Sierra Leone", "Sierra_Leone_2013")
Swaziland <- c("Swaziland", "Standard_DHS_2006-07", "SZGE53FL", "Swaziland", "Swaziland_2006-07")
Tanzania <- c("Tanzania", "Standard_AIS_2011-12", "TZGE6AFL", "United Republic of Tanzania", "Tanzania_2011-12")
Togo <- c("Togo", "Standard_DHS_2013-14", "TGGE62FL", "Togo", "Togo_2013-14")
Uganda <- c("Uganda", "Standard_AIS_2011", "UGGE6AFL", "Uganda", "Uganda_2011")
Zambia <- c("Zambia", "Standard_DHS_2013-14", "ZMGE61FL", "Zambia", "Zambia_2013-14")
Zimbabwe <- c("Zimbabwe", "Standard_DHS_2015", "ZWGE72FL", "Zimbabwe", "Zimbabwe_2015")


