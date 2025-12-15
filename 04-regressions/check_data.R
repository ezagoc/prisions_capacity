
pacman::p_load(tidyverse, arrow, purrr, sf, fixest, didimputation, did)

rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pfo <- '../../data/03-analysis/'

# Dataset: 
# Judicial data
p1 <- read_parquet('../../data/01-judicial/00-sentencing/final/panel_comun_1997_2008.parquet.gzip') |>
  filter(year > 1999)

p2 <- read_parquet('../../data/01-judicial/00-sentencing/final/panel_comun_2009_2012.parquet.gzip')

df <- bind_rows(p1, p2)
# Treatment

dfini <- read_parquet(paste0(pfo, 'treatment_judicial.parquet.gzip'))

controls <- read_parquet(paste0(pfo, 'controls.parquet.gzip')) 

########################
# Function to Run Event Studies
#######################

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

df <- df |> left_join(monthly_conversion)

df <- df |> select(code_inegi, CVE_ENT, year, month, semester, bimonthly, quarterly,
                   marg_condition:sent_intensive_incl)

time_period <- 'semester'
if(time_period == 'monthly'){
  df <- df |> rename(actual_time = month)
  pretrend <- c(-80:-2)
  future <- c(0:80)
  ccf <- 80
  ccp <- -80
}else if(time_period == 'bimonthly'){
  pretrend <- c(-60:-2)
  future <- c(0:60)
  ccf <- 60
  ccp <- -60
  df <- df |> group_by(code_inegi, year, bimonthly) |> 
    summarise(across(c(marg_condition:sent_intensive_incl), ~sum(.x))) |> 
    ungroup() |> rename(actual_time = bimonthly)
}else if(time_period == 'semester'){
  ccf <- 23
  ccp <- -23
  pretrend <- T
  future <- T
  df <- df |> group_by(code_inegi, year, semester) |> 
    summarise(across(c(marg_condition:sent_intensive_incl), ~sum(.x))) |> 
    ungroup() |> rename(actual_time = semester)
}else if(time_period == 'yearly'){
  ccf <- 10
  ccp <- -10
  pretrend <- T
  future <- T
  df <- df |> group_by(code_inegi, year) |> 
    summarise(across(c(marg_condition:sent_intensive_incl), ~sum(.x))) |> 
    ungroup() |> mutate(actual_time = 1)
}else if(time_period == 'quarterly'){
  pretrend <- c(-30:-2)
  future <- c(0:30)
  ccf <- 30
  ccp <- -30
  df <- df |> group_by(code_inegi, year, quarterly) |> 
    summarise(across(c(marg_condition:sent_intensive_incl), ~sum(.x))) |> 
    ungroup() |> mutate(actual_time = quarterly)
}else{
  df <- df |> rename(actual_time = month)
  print('Choose between: monthly, bimonthly, semester, yearly, quarterly. Code is running at the monthly level')
}

df <- df |> mutate(diff_ocu_auto = ifelse(total_processed != 0, 
                                          diff_ocu_auto/total_processed, 0),
                   total_processed_0 = as.integer(total_processed == 0),
                   total_imputed = (formal_prision + proceso + free), 
                   total_imputed_0 = as.integer(total_imputed == 0),
                   rate_formal_prision = ifelse(total_imputed != 0, 
                                                formal_prision/total_imputed, 0), 
                   rate_free = ifelse(total_imputed != 0, free/total_imputed, 0), 
                   n_sentenced_0 = as.integer(n_sentenced == 0),
                   rate_sent_general = ifelse(n_sentenced != 0, 
                                              sent_prison/n_sentenced, 0), # sent to prison rate out of all 
                   condenado_0 = as.integer(condenado == 0),
                   rate_sent_cond = ifelse(condenado != 0, 
                                           sent_prison/condenado, 0), 
                   rate_absol = ifelse(n_sentenced != 0, 
                                       absolutoria/n_sentenced, 0)
) 


# df<- df |> left_join(controls |> select(code_inegi, POBTOT), by = 'code_inegi') |> 
#   mutate(sent_prison_100 = (sent_prison*100000)/POBTOT)

treat_post_p <- 'btreat_300KM2'

df <- df |> left_join(dfini |> select(-c(CVE_ENT:NOM_MUN)), 
                      by = c('code_inegi', 'year', 'actual_time'='month'))

time <- df |> distinct(year, actual_time) |> mutate(m_time = row_number())

df <- df |> left_join(time, by = c('year', 'actual_time'))

df <- df |> mutate(treat_post := .data[[treat_post_p]])

df <- df |> group_by(code_inegi) |> 
  mutate(event_time = m_time[treat_post > 0][1], 
         treat = ifelse(is.na(event_time) == F, 1, 0)) |>
  ungroup()

# Control group to 0
df$event_time[df$treat == 0] <- 0

# Time to Event
df <- df %>% group_by(code_inegi) %>% 
  mutate(time_to_event = ifelse(treat == 1, m_time - event_time, 
                                0)) |> ungroup()


dfp <- df |> filter(condenado != 0)

meanc <- mean(dfp$rate_sent_cond)

dfp <- df |> filter(time_to_event == 6) |> select(time_to_event, code_inegi, 
                                                  rate_sent_cond, sent_prison, 
                                                  condenado, only_sent_money)
