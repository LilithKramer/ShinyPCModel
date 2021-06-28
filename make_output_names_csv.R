#### ShinyPCModel ####

## make output_names.csv

## Author: L. Kramer
## Date:   23 November 2020



##== notes ====

## possibly need to fix the units for reading into R shiny plots
## something with the "_d_"'s? I vaguely remember



##== install packages and open libraries ====

library(readxl)
library(plyr)
library(data.table)


##== set paths ========

#!# directories
dirHome <- "C:/Users/Lilith Kramer/PCModel/PCModel-master/Licence_agreement/I_accept/PCModel1350/PCModel/3.00/Models/PCLake+/6.13.16/"
list.files(dirHome)

fnGetNames <- "PL613162PLUS_Rnet.xls"



##== load the PCLake+ excel sheets ========

xls_file <- paste0(dirHome, fnGetNames)


## from states sheet
states_sheet <- read_excel(xls_file, sheet = "states")

states_deriv <- data.frame(ExcelSheet  = "states_deriv", 
                           Excelname   = states_sheet$sDerivativeName,
                           Unit        = states_sheet$sDerivativeUnit,
                           Measurement = "")

states_init  <- data.frame(ExcelSheet  = "states_init", 
                           Excelname   = states_sheet$sInitialStateName,
                           Unit        = states_sheet$sInitialStateUnit,
                           Measurement = "")

states_name  <- data.frame(ExcelSheet  = "states_name", 
                           Excelname   = states_sheet$sStateName,
                           Unit        = states_sheet$sStateUnit,
                           Measurement = "")

## from parameters sheet
parameter_sheet <- read_excel(xls_file, sheet = "parameters")

parameters <- data.frame(ExcelSheet  = "parameters", 
                         Excelname   = parameter_sheet$sName,
                         Unit        = parameter_sheet$sUnit,
                         Measurement = "")

## from initial states sheet
initstates_sheet <- read_excel(xls_file, sheet = "initialstates")

initstates <- data.frame(ExcelSheet  = "initial_states", 
                         Excelname   = initstates_sheet$sName,
                         Unit        = initstates_sheet$sUnit,
                         Measurement = "")


## from derivatives sheet
deriv_sheet <- read_excel(xls_file, sheet = "derivatives")

deriv <- data.frame(ExcelSheet  = "derivatives", 
                    Excelname   = deriv_sheet$sName,
                    Unit        = deriv_sheet$sUnit,
                    Measurement = "")


## add the optional upstream stuff
upst_add <- data.frame(ExcelSheet = "derivatives",
                       Excelname  = c(paste0(c("wPTranPO4WEpi", "wPTranAIMWEpi", "wPTranGrenEpi", "wPTranDiatEpi", "wPTranBlueEpi", "wPTranDetWEpi", "wPTranZooEpi",
                                             "wNTranNH4WEpi", "wNTranNO3WEpi", "wNTranGrenEpi", "wNTranDiatEpi", "wNTranBlueEpi", "wNTranDetWEpi", "wNTranZooEpi",
                                             "wDTranDetWEpi", "wDTranIMWEpi", "wDTranGrenEpi", "wDTranDiatEpi", "wDTranBlueEpi", "wDTranZooEpi",
                                             "wSiTranSiO2Epi", "wSiTranDetWEpi",
                                             "wO2TranWEpi",
                                             
                                             "wPTranPO4WHyp", "wPTranAIMWHyp", "wPTranGrenHyp", "wPTranDiatHyp", "wPTranBlueHyp", "wPTranDetWHyp", "wPTranZooHyp",
                                             "wNTranNH4WHyp", "wNTranNO3WHyp", "wNTranGrenHyp", "wNTranDiatHyp", "wNTranBlueHyp", "wNTranDetWHyp", "wNTranZooHyp",
                                             "wDTranDetWHyp", "wDTranIMWHyp", "wDTranGrenHyp", "wDTranDiatHyp", "wDTranBlueHyp", "wDTranZooHyp",
                                             "wSiTranSiO2Hyp", "wSiTranDetWHyp",
                                             "wO2TranWHyp"), "Upst"), "mSurfArea"),
                       Unit        = c(rep("_m_^-3*_d_^-1", 46), "_m_"),
                       Measurement = "")


##== make into one dataframe ========

# output_names <- rbindlist(list(states_deriv, states_init, states_name, parameters, initstates, deriv))
output_names <- rbindlist(list(deriv, initstates, parameters,  states_init, states_name, states_deriv, upst_add))



##== add measurement matches ========

output_names$Measurement <- output_names$Excelname
output_names[!(output_names$Measurement %in% c("_oNH4WEpi_", "_oNO3WEpi_", "_oPO4WEpi_", "_oO2WEpi_", "_uTmEpi_", "_aSecchiEpi_", "_oPTotWEpi_", "_oNTotWEpi_", "_oChlaEpi_")), "Measurement"] <- ""
output_names$Measurement <- mapvalues(output_names$Measurement, 
                                      from = c("_oNH4WEpi_", "_oNO3WEpi_", "_oPO4WEpi_", "_oO2WEpi_", "_uTmEpi_", "_aSecchiEpi_", "_oPTotWEpi_", "_oNTotWEpi_", "_oChlaEpi_"),
                                      to   = c("NH4", "NO3", "PO4", "O2", "T", "ZICHT", "Ptot", "Ntot", "sCHLFa4"))

output_names_export <- output_names[-which(is.na(output_names$Excelname) | is.na(output_names$Unit)),]

write.table(x = output_names_export, 
            file = "C:/Users/Lilith Kramer/Documents/PhD/Documenten/_01. Onderzoek/01.06. Modellen/01.06.05. R/01.06.01. ShinyPCModel/settings/output_names_shiny.csv",
            row.names = F,
            sep = ",",
            quote = F)
