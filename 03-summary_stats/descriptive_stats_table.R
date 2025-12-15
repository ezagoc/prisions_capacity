###################################################
## Data Analysis: Main results ATT (CSDID)
## Author: Eduardo Zago-Cuevas (all errors are my own)
## Run before: same folder, a number before
## Output: Capacity panel for DiD
##
###################################################

# install.packages('pacman')

pacman::p_load(tidyverse, arrow, purrr, lfe, stargazer, summarytools, xtable)

rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pfo <- '../../data/03-analysis/'

g_desc <- function(vars, df) {
  vars <- c(vars)
  df <- df %>% 
    select(all_of(vars)) %>%
    summarise_all(list(
      N = ~ sum(!is.na(.)),
      Mean = ~ round(mean(., na.rm = TRUE), 2),
      SD = ~ round(sd(., na.rm = TRUE), 2),
      Median = ~ round(median(., na.rm = TRUE), 2),
      Min = ~ round(min(., na.rm = TRUE), 2),
      Max = ~ round(max(., na.rm = TRUE), 2)
    )) |>
    mutate(name = vars)
  
  return(df)
}

# Dataset: 
# Judicial data


########################
# Desc Stats: start with judicial
#######################

p1 <- read_parquet('../../data/01-judicial/00-sentencing/final/panel_comun_1997_2008.parquet.gzip') |>
  filter(year > 1999)

p2 <- read_parquet('../../data/01-judicial/00-sentencing/final/panel_comun_2009_2012.parquet.gzip')

p <- bind_rows(p1, p2) #|> filter(!code_inegi %in% c(28025, 28027)) # Filter out noisy municipalities


monthly_conversion <- tibble(month = c(1:12)) |> 
  mutate(semester = ifelse(month %in% c(1:6), 1, 2), 
         bimonthly = case_when(month %in% c(1,2) ~ 1, 
                               month %in% c(3,4) ~ 2,
                               month %in% c(5,6) ~ 3,
                               month %in% c(7,8) ~ 4,
                               month %in% c(9,10) ~ 5,
                               month %in% c(11,12) ~ 6), 
         quarterly = case_when(month %in% c(1:4) ~ 1, 
                               month %in% c(5:8) ~ 2,
                               month %in% c(9:12) ~ 3))

p <- p |> left_join(monthly_conversion)

p <- p |> select(code_inegi, CVE_ENT, year, bimonthly, 
                 crime_5, sent_prison, total_processed, formal_prision, free, 
                 sent_intensive, sent_intensive_incl, n_sentenced, only_sent_money, 
                 absolutoria, condenado) |> 
  group_by(code_inegi, year, bimonthly) |> 
  summarise(across(c(total_processed, sent_prison, formal_prision, free, 
                     sent_intensive, sent_intensive_incl, n_sentenced, 
                     only_sent_money, absolutoria, condenado), ~sum(.x))) |> 
  ungroup() |> rename(actual_time = bimonthly)

p <- p |> mutate(sent_intensive = ifelse(n_sentenced != 0, 
                                                sent_intensive_incl/n_sentenced, 0))

judicial <- c('total_processed',  'formal_prision', 'free', 
               'n_sentenced', 'sent_prison', 'sent_intensive',
              'only_sent_money', 'absolutoria', 'condenado') |> 
  map_dfr(~g_desc(.x, p))

# Municipalities:
dfini <- read_parquet(paste0(pfo, 'treatment_judicial.parquet.gzip'))

vars <- c('min_dist_to_fed_km2', 'min_dist_to_state_km')

mun_dist <- vars |> 
  map_dfr(~g_desc(.x, dfini))

dfinilast <- dfini |> filter(year == 2012 & month == 12)

mun_dist2 <- vars |> 
  map_dfr(~g_desc(.x, dfinilast))

controls <- read_parquet(paste0(pfo, 'controls.parquet.gzip')) |> 
  select(code_inegi, POBTOT:PHOGJEFF)

vars <- c('POBTOT', 'PMASC18_', 'VP_TV', 'VP_RADIO', 'PNOTRABA')

controls_des <- vars |> 
  map_dfr(~g_desc(.x, controls))

# Prisons

pfo <- '../../data/03-analysis/'

monthly_conversion <- tibble(month = c(1:12)) |> 
  mutate(semester = ifelse(month %in% c(1:6), 1, 2), 
         bimonthly = case_when(month %in% c(1,2) ~ 1, 
                               month %in% c(3,4) ~ 2,
                               month %in% c(5,6) ~ 3,
                               month %in% c(7,8) ~ 4,
                               month %in% c(9,10) ~ 5,
                               month %in% c(11,12) ~ 6), 
         quarterly = case_when(month %in% c(1:4) ~ 1, 
                               month %in% c(5:8) ~ 2,
                               month %in% c(9:12) ~ 3))

dfini <- read_parquet(paste0(pfo, 'panel_capacity.parquet.gzip'))
dfini <- dfini |> mutate(perc_overcrowding = total_clean/capacity_clean, 
                         dummy_overcrowding = ifelse(overcrowding>0, 1, 0))

dfini <- dfini |> select(prison_id, year, month, center_name_clean, 
                         total_clean:relative_overcrowding, perc_overcrowding, 
                         dummy_overcrowding) 

vars <- c('total_clean', 'capacity_clean', 'comun_clean', 'federal_clean', 
          'relative_overcrowding')

prisons_des <- vars |> 
  map_dfr(~g_desc(.x, dfini))

lat <- rbind(judicial, prisons_des, controls_des, mun_dist, mun_dist2) |> 
  select(name, N:Max)
my_table <- xtable(lat)

# Print the LaTeX code for the table
print.xtable(my_table, floating = FALSE, hline.after = c(-1, 0, nrow(lat)))
