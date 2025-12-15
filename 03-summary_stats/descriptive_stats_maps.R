###################################################
## Data Processing: capacity treatment generation
## Author: Eduardo Zago-Cuevas (all errors are my own)
## Run before: same folder, a number before
## Output: Capacity panel for DiD
##
###################################################

# install.packages('pacman')

pacman::p_load(tidyverse, arrow, purrr, sf)

rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pfo <- '../../data/03-analysis/'

pf <- '../../data/00-map/'

dfini <- read_parquet(paste0(pfo, 'panel_capacity.parquet.gzip'))

fed <- readxl::read_excel(paste0(pf, 'prisiones_federales.xlsx')) |> 
  select(name, date_opening, latitude, longitude, capacity, private, closed) |>
  mutate(date_opening = as.Date(date_opening))

prisd <- read_parquet(paste0(pfo, 
                             'individual_prisons_municipalities.parquet.gzip'))

dfini2 <- dfini |> select(prison_id, )
