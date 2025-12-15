rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pacman::p_load(tidyverse, arrow, purrr, sf, fixest, haven, devtools, 
               DIDmultiplegtDYN, DIDmultiplegtSTAT)

# install_github("chaisemartinPackages/did_multiplegt_stat/R", force = TRUE)
# devtools::install_github("chaisemartinPackages/did_multiplegt_dyn/R")


gazoline <-  haven::read_dta("https://github.com/chaisemartinPackages/ApplicationData/raw/main/data_gazoline.dta")

# Example 1
summary(did_multiplegt_stat(df = gazoline, Y = "lngca", ID = "id", T = "year", 
                            D = "tau", order = 2, 
                            estimator = c("aoss", "waoss"), 
                            estimation_method = "dr", aoss_vs_waoss = TRUE, 
                            placebo = TRUE, noextrapolation = TRUE))

# Example 2
summary(did_multiplegt_stat(df = gazoline, Y = "lngpinc", ID = "id", T = "year", 
                            D = "tau", order = 2, estimator = c("aoss", "waoss"), 
                            estimation_method = "dr", aoss_vs_waoss = TRUE, 
                            placebo = TRUE, noextrapolation = TRUE))

# Example 3
summary(did_multiplegt_stat(df = gazoline, Y = "lngca", ID = "id", T = "year", 
                            D = "lngpinc", Z = "tau", order = 2, estimator = 
                              "iwaoss", estimation_method = "ra", placebo = TRUE, 
                            noextrapolation = TRUE))

# Some brute force consistency checks for the package #

# Standard twfe

# install.packages('pacman')

pfo <- '../../data/03-analysis/'

# Dataset: 
# Judicial data

p1 <- read_parquet('../../data/01-judicial/00-sentencing/final/panel_comun_1997_2008.parquet.gzip') |>
  filter(year > 1999)

p2 <- read_parquet('../../data/01-judicial/00-sentencing/final/panel_comun_2009_2012.parquet.gzip')

p <- bind_rows(p1, p2)
# Treatment

dfini <- read_parquet(paste0(pfo, 'treatment_judicial.parquet.gzip'))

# Controls

controls <- read_parquet(paste0(pfo, 'controls.parquet.gzip'))

pobtot <- controls |> select(code_inegi, POBTOT)

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

p <- p |> left_join(monthly_conversion)

preprocess_variable <- function(df, treat_post_p, time_period ='bimonthly', 
                          variable, type_variable = 'log'){
  df <- df |> select(code_inegi, CVE_ENT, year, month, semester, bimonthly, quarterly,
                     marg_condition:sent_intensive_incl)
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
  
  return(df)
  
}

df <- preprocess_variable(p, treat_post_p = 'btreat_300KM2',
                          time_period ='semester',
                          variable = 'rate_sent_cond', 
                          type_variable = 'levels')
variable <- 'rate_sent_general'

if (variable %in% c('rate_sent_general', 'rate_absol')){
  addonvar <- ' + n_sentenced_0 '
  addonvar2 <- ' ~ n_sentenced_0'
}else if (variable %in% c('rate_formal_prision', 'rate_free')){
  addonvar <- ' + total_imputed_0'
  addonvar2 <- ' ~ total_imputed_0'
}else if (variable %in% c('rate_sent_cond')){
  addonvar <- ' + condenado_0'
  addonvar2 <- ' ~ condenado_0'
}else if (variable %in% c('diff_ocu_auto')){
  addonvar <- ' + total_processed_0'
  addonvar2 <- ' ~ total_processed_0'
}else{addonvar2 <- ''
addonvar<-''}

df <- df |> mutate(log_dist_300 = log(ctreat_300KM+1))

df <- df |> mutate(rate_300 = ifelse(ctreat_300KM != 0, 1 - ctreat_300KM/300, ctreat_300KM))

check <- df |> select(code_inegi, m_time, log_dist_300, rate_200)

fmla <- as.formula(paste0(variable, ' ~ log_dist_300 ', 
                          addonvar, ' | code_inegi + m_time'))
regsentcond <- feols(fmla, cluster = "code_inegi", data = df)

summary(regsentcond)

fmla <- as.formula(paste0(variable, ' ~ rate_300 ', 
                          addonvar, ' | code_inegi + m_time'))
regsentcond <- feols(fmla, cluster = "code_inegi", data = df)

summary(regsentcond)

summary(did_multiplegt_dyn(
  df = df,
  outcome = 'rate_sent_general',
  group = "code_inegi",
  time = "m_time",
  treatment = "rate_300",
  controls = c('n_sentenced_0'),
  effects = 8,
  placebo = 3,
  cluster = "code_inegi",
  graph_off = TRUE
))

did_multiplegt_dyn(
  df = df,
  outcome = 'rate_sent_general',
  group = "code_inegi",
  time = "m_time",
  treatment = "rate_300",
  controls = c('n_sentenced_0'),
  effects = 2,
  placebo = 2,
  cluster = "code_inegi",
  graph_off = F
)

########### TWFE

data('favara_imbs')

pdf(paste0("../../results/events/test_judicial/twfe_",type_variable, 
           '_', variable,'_', treat_post_p, '_', 
           time_period, ".pdf"))
iplot(reg200k)
dev.off()

