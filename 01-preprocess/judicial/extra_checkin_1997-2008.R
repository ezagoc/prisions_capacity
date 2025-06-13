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

pf <- '../../../data/01-judicial/00-sentencing/raw/judiciales_bd_catalogos_2000_dbf/judiciales_bd_catalogos_2000/'

# First read sreg, preg, pdel

# Import municipality panel

munpan <- read_parquet('../../../data/00-map/municipalities.parquet')

preg <- read.dbf(paste0(pf, 'TablasMicrodatos_2000/preg2000.DBF'), as.is = T)

pdel <- read.dbf(paste0(pf, 'TablasMicrodatos_2000/pdel2000.DBF'), as.is = T)

sreg <- read.dbf(paste0(pf, 'TablasMicrodatos_2000/sreg2000.DBF'), as.is = T)

sdel <- read.dbf(paste0(pf, 'TablasMicrodatos_2000/sdel2000.DBF'), as.is = T)

# From CENTIDAD.dbf we know: if state > 32 then it is NA (other countries, NA)

entidad <-  read.dbf(paste0(pf, 'CatalogosMicrodatos_2000/CENTIDAD.DBF'))

# Check pdel. What to do, first keep the important crime 

# Matches the length of the other data set (processed one)


# It matches the preg data set: However, we do not want to lose info on these crimes.
# Lets keep the important one as crime_1_id, and the others as crime_1_id
# Import coded crimes 

coded <- readxl::read_excel(paste0(pf, 
                                   'CatalogosMicrodatos_2000/cdel2000_coded.xlsx')) |>
  rename(B_DELITO = CVE_DEL)

# We take all the data from the important crime, that is our deduplicated dataset by year

pdel <- pdel |> left_join(coded, by = c('B_DELITO')) |> arrange(ID_PS)

pdel.ind <- pdel |> filter(IMPORTANTE == T)

# Pivot wider:
pdel.w <- pdel |> distinct(ID_PS, B_DELITO, .keep_all = T) |> group_by(ID_PS) |> 
  mutate(B_NUMDEL = row_number()) |> ungroup()|> arrange(ID_PS)

pdel.w <- pdel.w |> mutate(snieg_code = paste0('crime_', snieg_code)) |>
  select(ID_PS, snieg_code) |>
  mutate(dummy = 1)

pdel.w <- pdel.w |> distinct(ID_PS, snieg_code, .keep_all = T) |> pivot_wider(
  id_cols = ID_PS,
  names_from = snieg_code,
  values_from = dummy,
  values_fill = 0
)

# But we know which other crimes committed

pdel.ind <- pdel.ind |> left_join(pdel.w, by = ('ID_PS'))

# Now we clean it 

pdel.ind <- pdel.ind |> 
  mutate(date_ocu = dmy(B_FOCU),         # Convert to Date format
         day_ocu = day(date_ocu),
         month_ocu = month(date_ocu),
         year_ocu = year(date_ocu),
         CVE_ENT_ocu = ifelse(B_ENTOC != '99', as.numeric(B_ENTOC), NA),  
         CVE_MUN_ocu = ifelse(B_MUNOC != '999', as.numeric(B_MUNOC), NA), 
         code_inegi_ocu = CVE_ENT_ocu*1000 + CVE_MUN_ocu, 
         formal_prision = ifelse(B_AUTO == 1, 1, 0), 
         proceso = ifelse(B_AUTO == 2, 1, 0),
         preventive = ifelse(B_AUTO == 5, 1, 0),
         free = ifelse(B_AUTO %in% c(3, 4), 1, 0), 
         #petty = ifelse(B_CALIFICA == 2, 1, 0), 
         federal_imp = ifelse(B_FUERO == 2, 1, 0))

pdel.ind <- pdel.ind |> select(ID_PS, code_delito = B_DELITO, DESC_DEL:federal_imp)
# Now merge back 
# Check for NAs on relevant variables: sentence, mun_rh, st_rh, date sentence.

preg <- preg |> mutate(across(c(B_MESREG:B_TOTDEL), ~as.numeric(.x))) |> 
  mutate(across(c(B_EDOCIVIL:B_OCUPA), ~ifelse(.x %in% c(9, 99), NA, .x))) |> 
  mutate(federal = ifelse(B_CVEESTAD == 42, 1, 0),
         CVE_ENT_reg = ifelse(B_ENTREG < 33, B_ENTREG, NA),
         CVE_MUN_reg = B_MUNREG, 
         code_inegi_reg = CVE_ENT_reg*1000 + CVE_MUN_reg, 
         CVE_ENT_rh = ifelse(B_ENTRH < 33, B_ENTRH, NA),
         CVE_MUN_rh = ifelse(B_MUNRH == 999, NA, B_MUNRH), 
         code_inegi_rh = CVE_ENT_rh*1000 + CVE_MUN_rh,
         teen = ifelse(B_EDAD<18, 1, 0), 
         unemployed = ifelse(B_OCUPA == 2, 1, 0),
         #indigenous = ifelse(B_HABLALI == 1, 1, 0), # indigenous if the individual speaks an indigeneous language
         primaria = ifelse(B_NIVELIN %in% c(1, 2, 3), 1, 0), # Marginalized: max escolarity (primary school)
         illiterate = ifelse(B_CONALF == 2, 1, 0), 
         marg_condition = ifelse(teen == 1 | unemployed == 1 |  
                                   primaria == 1 | illiterate == 1, 1, 0), 
         marg_condition = ifelse(is.na(marg_condition) == T, 0, marg_condition), 
         date_auto = as.Date(B_FAUTO), 
         day_auto = day(date_auto),
         month_auto = month(date_auto),
         year_auto = year(date_auto), 
         man = ifelse(B_SEXO == 1, 1, 0)
  )

preg.final <- preg |> left_join(pdel.ind, by = 'ID_PS')

## PREG IS A THING OF HIS OWN, SREG IS A THING OF HIS OWN AS WELL. 

# WORK IN SDEL, SAME AS IN PDEL

sdel <- sdel |> left_join(coded, by = c('B_DELITO')) |> arrange(ID_PS)

sdel.ind <- sdel |> filter(IMPORTANTE == T)

# Pivot wider:
sdel.w <- sdel |> distinct(ID_PS, B_DELITO, .keep_all = T) |> group_by(ID_PS) |> 
  mutate(B_NUMDEL = row_number()) |> ungroup()|> arrange(ID_PS)

sdel.w <- sdel.w |> mutate(snieg_code = paste0('crime_', snieg_code)) |>
  select(ID_PS, snieg_code) |>
  mutate(dummy = 1)

sdel.w <- sdel.w |> distinct(ID_PS, snieg_code, .keep_all = T) |> pivot_wider(
  id_cols = ID_PS,
  names_from = snieg_code,
  values_from = dummy,
  values_fill = 0
)

# But we know which other crimes committed

sdel.ind <- sdel.ind |> left_join(sdel.w, by = ('ID_PS'))

#sdel.check <- sdel.ind |> filter(crime_5 == 1)
# Now we clean it 

sdel.ind <- sdel.ind |> 
  mutate(date_ocu = dmy(B_FOCU),         # Convert to Date format
         day_ocu = day(date_ocu),
         month_ocu = month(date_ocu),
         year_ocu = year(date_ocu),
         CVE_ENT_ocu = ifelse(B_ENTOC != '99', as.numeric(B_ENTOC), NA),  
         CVE_MUN_ocu = ifelse(B_MUNOC != '999', as.numeric(B_MUNOC), NA), 
         code_inegi_ocu = CVE_ENT_ocu*1000 + CVE_MUN_ocu, 
         sentenced_imp = ifelse(B_SENTEN %in% c(1), 1, 0), 
         #petty = ifelse(B_CALIFICA == 2, 1, 0), 
         federal_imp = ifelse(B_FUERO == 2, 1, 0))

sdel.ind <- sdel.ind |> select(ID_PS, code_delito = B_DELITO, DESC_DEL:federal_imp)
### After: Generate a marginalized index: LI, Ocupation, Conalf, Nivel In, 

# Now merge back 
# Check for NAs on relevant variables: sentence, mun_rh, st_rh, date sentence.

sreg <- sreg |> mutate(across(c(B_MESREG:B_MULTA), ~as.numeric(.x))) |> 
  mutate(across(c(B_EDOCIVIL:B_OCUPA), ~ifelse(.x %in% c(9, 99), NA, .x))) |> 
  mutate(federal = ifelse(B_CVEESTAD == 52, 1, 0),
         CVE_ENT_reg = ifelse(B_ENTREG < 33, B_ENTREG, NA),
         CVE_MUN_reg = B_MUNREG, 
         code_inegi_reg = CVE_ENT_reg*1000 + CVE_MUN_reg, 
         CVE_ENT_rh = ifelse(B_ENTRH < 33, B_ENTRH, NA),
         CVE_MUN_rh = ifelse(B_MUNRH == 999, NA, B_MUNRH), 
         code_inegi_rh = CVE_ENT_rh*1000 + CVE_MUN_rh,
         teen = ifelse(B_EDAD<18, 1, 0), 
         unemployed = ifelse(B_OCUPA == 2, 1, 0),
         primaria = ifelse(B_NIVELIN %in% c(1, 2, 3), 1, 0), # Marginalized: max escolarity (primary school)
         illiterate = ifelse(B_CONALF == 2, 1, 0), 
         marg_condition = ifelse(teen == 1 | unemployed == 1 | 
                                   primaria == 1 | illiterate == 1, 1, 0), 
         marg_condition = ifelse(is.na(marg_condition) == T, 0, marg_condition), 
         date_sent = as.Date(B_FSENTEN), 
         day_sent = day(date_sent),
         month_sent = month(date_sent),
         year_sent = year(date_sent), 
         man = ifelse(B_SEXO == 1, 1, 0), 
         sent_prison = ifelse(B_PRIVA > 0, 1, 0), # Resolución judicial que pone fin a un proceso o juicio en una instancia o a un recurso extraordinario.
         # En el caso particular de las Estadísticas judiciales en materia penal, está referida a un juicio 
         # en primera instancia.
         sent_money = ifelse(B_PECU > 0 | B_MULTA > 0, 1, 0) # NICE PLACEBO
  )

sreg.final <- sreg |> left_join(sdel.ind, by = 'ID_PS')
# check total number of different code inegis (residence)
sreg.federal <- sreg.final |> filter(federal == T)

# Aggregation of both: at the municipality month level
# For the processed, at the municipality of residence, municipality of register, and mun nac:

# For processed changes: fiscalia doing more stuff: 
preg.agg.p <- preg.final |> mutate(date_auto = date_auto - 3, #72 hours before the judge decides
                                   month_auto = month(date_auto)) |>
  filter(is.na(code_inegi_rh) == F & is.na(code_inegi_ocu) == F) |> 
  filter(man == 1)

preg.agg.rh <- preg.agg.p |> mutate(diff_ocu_auto = as.numeric(date_auto - date_ocu), 
                                    state = ifelse(federal == 0, 1, 0), 
                                    #state_petty = state*petty, 
                                    state_drugs = state*crime_8, 
                                    state_guns = state*crime_10, 
                                    state_theft = state*crime_5, 
                                    #federal_petty = federal*petty, 
                                    federal_drugs = federal*crime_8, 
                                    federal_guns = federal*crime_10, 
                                    federal_theft = federal*crime_5) |>
  group_by(code_inegi_rh, month_auto) |> 
  summarise(across(c(state:federal_theft, federal, crime_1:crime_14, 
                     marg_condition), ~sum(.x, na.rm = T)), 
            diff_ocu_auto = mean(diff_ocu_auto, na.rm = T), 
            total_processed = n()) |> ungroup()

preg.agg.ocu <- preg.agg.p |> mutate(diff_ocu_auto = as.numeric(date_auto - date_ocu), 
                                     state = ifelse(federal == 0, 1, 0), 
                                     #state_petty = state*petty, 
                                     state_drugs = state*crime_8, 
                                     state_guns = state*crime_10, 
                                     state_theft = state*crime_5, 
                                     #federal_petty = federal*petty, 
                                     federal_drugs = federal*crime_8, 
                                     federal_guns = federal*crime_10, 
                                     federal_theft = federal*crime_5) |>
  group_by(code_inegi_ocu, month_auto) |> 
  summarise(across(c(state:federal_theft, federal, crime_1:crime_14, 
                     marg_condition), ~sum(.x, na.rm = T)), 
            diff_ocu_auto = mean(diff_ocu_auto, na.rm = T), 
            total_processed = n()) |> ungroup()

colnames(preg.agg.ocu)[3:length(preg.agg.ocu)] <- paste0(colnames(preg.agg.ocu)[3:length(preg.agg.ocu)], '_ocu')

# Merge with panel: 

months_seq <- tibble(
  month = c(1:12)
)

munpan <- munpan |> crossing(months_seq)

final.panel <- munpan |> left_join(preg.agg.rh, 
                                   by = c('code_inegi' = 'code_inegi_rh', 
                                          'month' = 'month_auto')) |>
  left_join(preg.agg.ocu, 
            by = c('code_inegi' = 'code_inegi_ocu', 
                   'month' = 'month_auto')) |> 
  mutate(across(c(state:total_processed_ocu), ~ifelse(is.na(.x) == T, 0, .x)))

# For Judges first interaction with individuals:
preg.agg <- preg.final |> mutate(formal_prision_marg = formal_prision*marg_condition, 
                                 #formal_prision_petty = formal_prision*petty, 
                                 #preventive_marg = preventive*marg_condition, 
                                 #preventive_petty = preventive*petty, 
                                 formal_prision_drug = formal_prision*crime_8, 
                                 formal_prision_theft = formal_prision*crime_5) |> # I can add more later, for now these seem like interesting ones
  filter(man == 1 & federal == 1) |> filter(crime_11 == 0) |>
  filter(is.na(code_inegi_rh) == F)

preg.agg <- preg.agg |> group_by(code_inegi_rh, month_auto) |> 
  summarise(across(c(formal_prision_marg:formal_prision_theft, 
                     formal_prision:formal_prision_theft), ~sum(.x, na.rm = T))) |> 
  ungroup() |> mutate(proceso_formal = formal_prision + proceso)

# In this case we aggregate at date of auto determination (Juzgado de Control)

final.panel <- final.panel |> left_join(preg.agg, 
                                        by = c('code_inegi' = 'code_inegi_rh', 
                                               'month' = 'month_auto')) |>
  mutate(across(c(formal_prision_marg:proceso_formal), 
                ~ifelse(is.na(.x) == T, 0, .x)))

# Finally with actual sentences

sreg.agg <- sreg.final |> filter(federal == 1 & is.na(code_inegi_rh) == F & 
                                   man == 1 & crime_11 == 0)

sreg.agg <- sreg.agg |>
  mutate(sent_prison_marg = sent_prison*marg_condition, 
         #sent_prison_petty = sent_prison*petty, 
         sent_prison_drugs = sent_prison*crime_8,
         sent_prison_theft = sent_prison*crime_5, 
         sent_int_exc_ext = ifelse(sent_prison==0, NA, B_PRIVA)) |>
  group_by(code_inegi_rh, month_sent) |> 
  summarise(across(c(sent_prison_marg:sent_prison_theft, sent_prison, 
                     sent_money), ~sum(.x, na.rm = T)), 
            sent_intensive = mean(B_PRIVA, na.rm = T), 
            sent_intensive_incl = mean(sent_int_exc_ext, na.rm = T)) |>
  ungroup()

final.panel <- final.panel |> left_join(sreg.agg, 
                                        by = c('code_inegi' = 'code_inegi_rh', 
                                               'month' = 'month_sent')) |>
  mutate(across(c(sent_prison_marg:sent_intensive_incl), 
                ~ifelse(is.na(.x) == T, 0, .x)))
