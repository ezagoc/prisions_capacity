###################################################
## Data Processing: Control Dataset
## Author: Eduardo Zago-Cuevas (all errors are my own)
## Run before: same folder, a number before
## Output: Capacity panel for DiD
##
###################################################

# install.packages('pacman')

pacman::p_load(tidyverse, arrow, purrr, foreign,sf)

rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Paths:
# In

pf <- '../../data/04-controls/'

mun <- read_sf('../../data/00-map/shapefiles2/Muni_2012gw.shp') |> 
  select(-c(OID_1:cov_id)) |> st_set_geometry(NULL) |> 
  mutate(code_inegi = as.numeric(CVE_ENT) * 1000 + as.numeric(CVE_MUN)) |> 
  arrange(code_inegi)

# Municipality controls:

df <- read.dbf(paste0(pf, '00_nacional_2000_iter_dbf/ITER_NALDBF00.dbf'))

df <- df |> filter(NOM_LOC == 'TOTAL MUNICIPAL' | 
                     NOM_LOC == 'TOTAL DE LA DELEGACION') |> 
  mutate(ENTIDAD = as.character(ENTIDAD), 
         MUN = as.character(MUN), 
         ENTIDAD = as.numeric(ENTIDAD), 
         MUN = as.numeric(MUN), 
         code_inegi = ENTIDAD*1000 + MUN)

df <- df |> 
  select(-c(LOC:ALTITUD)) |> select(-starts_with('X')) |>
  select(-c(ENTIDAD:NOM_MUN)) |> arrange(code_inegi)

# df1 <- read.csv(paste0(pf, 
#                        'iter_nal_2010_csv/iter_00_cpv2010/conjunto_de_datos/iter_00_cpv2010.csv')) |> 
#   filter(nom_loc == 'Total del Municipio') 
# 
# colnames(df1) <- toupper(colnames(df1))
# 
# df1 <- df1 |> mutate(ENTIDAD = as.character(ENTIDAD), 
#                      MUN = as.character(MUN), 
#                      ENTIDAD = as.numeric(ENTIDAD), 
#                      MUN = as.numeric(MUN), 
#                      code_inegi = ENTIDAD*1000 + MUN) |>
#   select(-c(LOC:ALTITUD)) |> select(-starts_with('X')) |>
#   select(-c(ENTIDAD:NOM_MUN)) |> arrange(code_inegi)
# 
# missing_code <- setdiff(mun$code_inegi, df$code_inegi)

# Voting

vote <- read.csv(paste0(pf, 'municipality_voting_panel.csv')) |> 
  group_by(year, mun_code) |> 
  summarise(across(c(incumbent_party_vote, state_incumbent_party_vote, 
                     PRI_vote, PRD_vote, PAN_vote, MORENA_vote, 
                     runnerup_party_vote, registered_voters:turnout), 
                   ~sum(.x, na.rm = T))) |> ungroup() |>
  rename(code_inegi = mun_code)

voted <- vote |> distinct(code_inegi) |> arrange(code_inegi)

vote_2000 <- vote |> filter(year == 2000)

for(i in c(1999, 1998, 1997, 1996, 1995, 1994, 2001:2004)){
  to_get <- setdiff(mun$code_inegi, vote_2000$code_inegi)
  
  vote_y <- vote |> filter(year == i & code_inegi %in% to_get)
  
  vote_2000 <- rbind(vote_2000, vote_y)
  
  print(length(to_get))
}

checkd <- vote_2000 |> distinct(code_inegi)

# Now Magar data:

mag <- read.csv(paste0(pf, 'aymu.coalAgg2000s.csv'))

mag <- mag |> 
  rowwise() |> 
  mutate(pan = c(v01, v02, v03, v04, v05, v06, v07, 
                 v08, v09, v10)[
    which(str_detect(c(l01, l02, l03, l04, l05, l06, l07, 
                       l08, l09, l10), "pan"))[1]], 
    pri = c(v01, v02, v03, v04, v05, v06, v07, 
            v08, v09, v10)[
              which(str_detect(c(l01, l02, l03, l04, l05, l06, l07, 
                                 l08, l09, l10), "pri"))[1]], 
    prd = c(v01, v02, v03, v04, v05, v06, v07, 
                 v08, v09, v10)[
    which(str_detect(c(l01, l02, l03, l04, l05, l06, l07, 
                       l08, l09, l10), "prd"))[1]]) |> 
  ungroup()


mag <- mag |> select(inegi, yr, efec, lisnom, pan:prd) |> 
  rename(code_inegi = inegi)

mmd <- mag |> distinct(code_inegi)

mag2000s <- mag |> filter(yr == 2000) |> distinct(code_inegi, .keep_all = T)

# 1900s for imputation:
mag9 <- read.csv(paste0(pf, 'aymu.coalAgg1990s.csv'))

mag9 <- mag9 |> 
  rowwise() |> 
  mutate(pan = c(v01, v02, v03, v04, v05, v06, v07, 
                 v08, v09, v10)[
                   which(str_detect(c(l01, l02, l03, l04, l05, l06, l07, 
                                      l08, l09, l10), "pan"))[1]], 
         pri = c(v01, v02, v03, v04, v05, v06, v07, 
                 v08, v09, v10)[
                   which(str_detect(c(l01, l02, l03, l04, l05, l06, l07, 
                                      l08, l09, l10), "pri"))[1]], 
         prd = c(v01, v02, v03, v04, v05, v06, v07, 
                 v08, v09, v10)[
                   which(str_detect(c(l01, l02, l03, l04, l05, l06, l07, 
                                      l08, l09, l10), "prd"))[1]]) |> 
  ungroup() |> select(inegi, yr, efec, lisnom, pan:prd) |>
  rename(code_inegi = inegi)

for(i in c(1999, 1998, 1997, 1996, 1995, 1994, 1993, 1992, 1991, 1990)){
  to_get <- setdiff(mun$code_inegi, mag2000s$code_inegi)
  
  vote_y <- mag9 |> filter(yr == i & code_inegi %in% to_get)
  
  vote_y <- vote_y |> distinct(code_inegi, .keep_all = T)
  
  mag2000s <- rbind(mag2000s, vote_y)
  
  print(length(to_get))
}

mag2000s <- mag2000s |> mutate(across(c(lisnom, efec, pan, prd, pri), 
                                      ~ifelse(is.na(.x) == T, 0, .x)))

checkd <- mag2000s |> distinct(code_inegi)
# Finally luminosity data:

lum <- read.csv(paste0(pf, 'luminosity_panel.csv'))

lum <- lum |> filter(yr == 1999) |> 
  select(code_inegi = inegi, sd_lum = sd, median_lum = median, 
         mean_lum = mean)

checkd <- lum |> distinct(code_inegi)
# Final data set: 

df <- df |> mutate(across(-all_of(c("code_inegi")),
                          ~as.character(.x)), 
                   across(-all_of(c("code_inegi")),
                          ~as.numeric(.x)))

mun_final <- mun |> left_join(df, by = 'code_inegi') |> 
  left_join(lum, by = 'code_inegi') |> 
  left_join(vote_2000 |> rename(year_vote = year), by = 'code_inegi') |>
  left_join(mag2000s |> rename(year_magar = yr), by = 'code_inegi')

write_parquet(mun_final, '../../data/03-analysis/controls.parquet.gzip', 
              compression = 'gzip')
