###################################################
## Data Preprocessing
## Author: Eduardo Zago-Cuevas (all errors are my own)
## Run before: same folder, a number before
## Output: Judicial panel dataset 2009-2012
##
###################################################

# install.packages('pacman')

pacman::p_load(tidyverse, arrow, foreign)

rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Paths:

pf <- '../../../data/01-judicial/00-sentencing/raw/'

# Expedientes
exp.ta <- read.dbf(paste0(pf, 
                          'TA_Expedientes_ehriij2015_dbf/Bases_Datos/EXPED_TA.DBF'), 
                   as.is = T) |> 
  mutate(date_auto = dmy(FEC_INGR),
         year_auto = year(date_auto),
         month_auto = month(date_auto),
         day_auto = day(date_auto))

# Procesados
proce.ta <- read.dbf(paste0(pf, 
                            'TA_Procesados_ehriij2015_dbf/Bases_Datos/PROCE_TA.DBF'), 
                     as.is = T) |> 
  mutate(muni_code = substr(as.character(MUNIRESI), 3, 5)) |> 
  filter(ENT_RESI != 0 & ENT_RESI != 99 & muni_code != 999) |> 
  mutate(code_inegi_rh = as.numeric(ENT_RESI)*1000 + as.numeric(muni_code))


# How many distinct municipalities do I have: 

dedupmunpro <- proce.ta |> distinct(ENT_RESI)
# Delitos procesados

proce.del.ta <- read.dbf(paste0(pf, 
                            'TA_Procesados_delito_ehriij2015_dbf/Bases_Datos/PRODE_TA.DBF'), 
                     as.is = T)

del.ta <- read.dbf(paste0(pf, 
                           'TA_Delitos_ehriij2015_dbf/Bases_Datos/DELIT_TA.DBF'), 
                         as.is = T)

concl.ta <- read.dbf(paste0(pf, 
                            'TA_Conclusiones_ehriij2015_dbf/Bases_Datos/CONCL_TA.DBF'), 
                     as.is = T) |> 
  mutate(date_sent = dmy(FEC_CONC),
         year_sent = year(date_sent),
         month_sent = month(date_sent),
         day_sent = day(date_sent))

concl.jo <- read.dbf(paste0(pf, 
                            'JO_Conclusiones_ehriij2015_dbf/Bases_Datos/CONCL_JO.DBF'), 
                     as.is = T) |> 
  mutate(date_sent = dmy(FEC_CONC),
         year_sent = year(date_sent),
         month_sent = month(date_sent),
         day_sent = day(date_sent))



