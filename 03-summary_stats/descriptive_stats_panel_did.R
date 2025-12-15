###################################################
## Data Analysis: capacity treatment analysis DiD
## Author: Eduardo Zago-Cuevas (all errors are my own)
## Run before: same folder, a number before
## Output: Capacity panel for DiD
##
###################################################

# install.packages('pacman')

pacman::p_load(tidyverse, arrow, purrr, panelView)

rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pfo <- '../../data/03-analysis/'

p1 <- read_parquet('../../data/01-judicial/00-sentencing/final/panel_comun_1997_2008.parquet.gzip') |>
  filter(year > 1999)

p2 <- read_parquet('../../data/01-judicial/00-sentencing/final/panel_comun_2009_2012.parquet.gzip')

p <- bind_rows(p1, p2)

dfini <- read_parquet(paste0(pfo, 'treatment_judicial.parquet.gzip'))

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


event_view <- function(df, treat_post_p, time_period ='bimonthly', 
                          variable){
  df <- df |> select(code_inegi, CVE_ENT, year, month, semester, bimonthly, quarterly,
                     marg_condition:sent_intensive_incl)
  if(time_period == 'monthly'){
    df <- df |> rename(actual_time = month)
  }else if(time_period == 'bimonthly'){
    df <- df |> group_by(code_inegi, year, bimonthly) |> 
      summarise(across(c(marg_condition:sent_intensive_incl), ~sum(.x))) |> 
      ungroup() |> rename(actual_time = bimonthly)
  }else if(time_period == 'semester'){
    df <- df |> group_by(code_inegi, year, semester) |> 
      summarise(across(c(marg_condition:sent_intensive_incl), ~sum(.x))) |> 
      ungroup() |> rename(actual_time = semester)
  }else if(time_period == 'yearly'){
    df <- df |> group_by(code_inegi, year) |> 
      summarise(across(c(marg_condition:sent_intensive_incl), ~sum(.x))) |> 
      ungroup() |> mutate(actual_time = 1)
  }else if(time_period == 'quarterly'){
    df <- df |> group_by(code_inegi, year, quarterly) |> 
      summarise(across(c(marg_condition:sent_intensive_incl), ~sum(.x))) |> 
      ungroup() |> mutate(actual_time = quarterly)
  }else{
    df <- df |> rename(actual_time = month)
    print('Choose between: monthly, bimonthly, semester, yearly, quarterly. Code is running at the monthly level')
  }
  
  
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
  df <- df |> mutate(year_month = paste0(as.character(year), '-', 
                                         as.character(actual_time)))
  panelview(as.formula(paste0(variable, ' ~ treat_post')), data = df, 
            index = c("code_inegi", "m_time"), 
            xlab = "Time (Bimonthly)", ylab = "Unit", display.all = T,
            gridOff = F, by.timing = TRUE, 
            axis.lab.gap = c(2,10))
}


# check
event_view(p, treat_post_p = 'btreat_300KM2',
              time_period ='bimonthly',
              variable = 'sent_prison')

event_view(p, treat_post_p = 'btreat_400KM2',
           time_period ='bimonthly',
           variable = 'sent_prison')

event_view(p, treat_post_p = 'btreat_200KM2',
           time_period ='bimonthly',
           variable = 'sent_prison')

# With the control at the begining: 

event_view(p, treat_post_p = 'btreat_300KM',
           time_period ='bimonthly',
           variable = 'sent_prison')

p 