###################################################
## Data Analysis: capacity treatment analysis DiD
## Author: Eduardo Zago-Cuevas (all errors are my own)
## Run before: same folder, a number before
## Output: Capacity panel for DiD
##
###################################################

# install.packages('pacman')

pacman::p_load(tidyverse, arrow, purrr, sf, fixest, didimputation, did2s, 
               DescTools, fect, )

rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pfo <- '../../data/03-analysis/'

# Dataset: 
# Judicial data

p1 <- read_parquet('../../data/01-judicial/00-sentencing/final/panel_comun_1997_2008.parquet.gzip') |>
  filter(year > 1999)

p2 <- read_parquet('../../data/01-judicial/00-sentencing/final/panel_comun_2009_2012.parquet.gzip')

p <- bind_rows(p1, p2) |> filter(!code_inegi %in% c(28025, 28027)) # Filter out noisy municipalities
# Treatment

dfini <- read_parquet(paste0(pfo, 'treatment_judicial.parquet.gzip'))

controls <- read_parquet(paste0(pfo, 'controls.parquet.gzip')) 

lumipanel <- read.csv('../../data/04-controls/luminosity_panel.csv') |> 
  filter(yr>1999) |> select(code_inegi = inegi, year = yr, mean_lumi = mean, 
                            sd_lumi = sd, median_lumi = median) 

#check <- lumipanel |> distinct(code_inegi, year)
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

event_general <- function(df, treat_post_p, time_period ='bimonthly', 
                          variable, type_variable = 'log', winsor = 0.0){
  df <- df |> select(code_inegi, CVE_ENT, year, month, semester, bimonthly, quarterly,
                     crime_5, marg_condition:sent_intensive_incl)
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
      summarise(across(c(crime_5, marg_condition:sent_intensive_incl), ~sum(.x))) |> 
      ungroup() |> rename(actual_time = bimonthly)
  }else if(time_period == 'semester'){
    ccf <- 23
    ccp <- -23
    pretrend <- T
    future <- T
    df <- df |> group_by(code_inegi, year, semester) |> 
      summarise(across(c(crime_5,marg_condition:sent_intensive_incl), ~sum(.x))) |> 
      ungroup() |> rename(actual_time = semester)
  }else if(time_period == 'yearly'){
    ccf <- 10
    ccp <- -10
    pretrend <- T
    future <- T
    df <- df |> group_by(code_inegi, year) |> 
      summarise(across(c(crime_5,marg_condition:sent_intensive_incl), ~sum(.x))) |> 
      ungroup() |> mutate(actual_time = 1)
  }else if(time_period == 'quarterly'){
    pretrend <- c(-30:-2)
    future <- c(0:25)
    ccf <- 30
    ccp <- -30
    df <- df |> group_by(code_inegi, year, quarterly) |> 
      summarise(across(c(crime_5,marg_condition:sent_intensive_incl), ~sum(.x))) |> 
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
                                         absolutoria/n_sentenced, 0), 
                     rate_marg_formal_prision = ifelse(marg_condition != 0, 
                                                       formal_prision_marg/marg_condition, 0), 
                     marg_0 = as.integer(marg_condition == 0), 
                     rate_theft_formal_prision = ifelse(crime_5 != 0, 
                                                        formal_prision_theft/crime_5, 0), 
                     theft_0 = as.integer(crime_5 == 0), 
                     sent_intensive = ifelse(n_sentenced != 0, 
                                             sent_intensive/n_sentenced, 0)
  )
  
  if (winsor > 0){
    df <- df |> mutate(across(all_of(variable), 
                              ~Winsorize(.x, probs = c(winsor, 1-winsor), 
                                         na.rm = TRUE)))
  }else if(winsor < 0 | winsor > 1){
    df <- df
    print('Not available, choose between 0 and 1 preferably 0.01, 0.05, 0.1. No winsoring applied.')
  }else{
    df <- df
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
  
  if(type_variable == 'levels'){
    df <- df 
  }else if(type_variable == 'log'){
    df <- df |> mutate(across(c(marg_condition:sent_intensive_incl), ~log(.x+1)))
  }else if(type_variable == 'asinh'){
    df <- df |> mutate(across(c(marg_condition:sent_intensive_incl), ~asinh(.x)))
  }else{
    df <- df
    print('Choose between: levels, log, asinh. Code is running logs')
  }
  
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
  }else if (variable %in% c('rate_marg_formal_prision')){
    addonvar <- ' + marg_0'
    addonvar2 <- ' ~ marg_0'
  }else if (variable %in% c('rate_theft_formal_prision')){
    addonvar <- ' + theft_0'
    addonvar2 <- ' ~ theft_0'
  }else{addonvar2 <- '~0'
  addonvar <- ''}
  
  ####### FECT: FE
  # form<- as.formula(paste0(variable, ' ~ treat_post'))
  # out.fect <- fect(form, data = df, index = c("code_inegi","m_time"), 
  #                  method = "fe", force = "two-way", se = TRUE, parallel = TRUE,
  #                  nboots = 200)
  # print(class(out.fect))
  # pdf(paste0("../../results/events/test_judicial_imp/fectfe_",type_variable,
  #            '_', variable,'_', treat_post_p, '_',
  #            time_period, '_',winsor,  ".pdf"))
  # plot(out.fect, main = "Estimated ATT (FEct)", ylab = "Effect of TreatPost on Y", 
  #      cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8, stats = "F.p")
  # dev.off()
  
  # ###### FECT: iFE
  # form<- as.formula(paste0(variable, ' ~ treat_post'))
  # out.ife <- fect(form, data = df, index = c("code_inegi","m_time"), 
  #                 force = "two-way", method = "ife", CV = TRUE, r = c(0, 5), 
  #                 se = TRUE, nboots = 200, parallel = TRUE)
  # 
  # print('finished ife')
  # # pdf(paste0("../../results/events/test_judicial_imp/fectife_",type_variable,
  # #            '_', variable,'_', treat_post_p, '_',
  # #            time_period, '_',winsor,  ".pdf"))
  # plot(out.ife, main = "Estimated ATT (IFEct)", ylab = "Effect of TreatPost on Y", 
  #      cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8, stats = "F.p")
  # # dev.off()
  
  df <- df |> left_join(lumipanel, by = c('year', 'code_inegi'))
  
  ########### Borusyak:
  
  event_borus <- did_imputation(data = df, yname = variable, 
                                gname = "event_time",
                                first_stage = as.formula(paste0('~ 0', 
                                                                ' | code_inegi + m_time')),
                                tname = "m_time", idname = "code_inegi",
                                pretrends = pretrend, horizon = future) |> 
    mutate(term = as.numeric(term), type = ifelse(term < 0, "Pre", "Post"))
  
  aux_1 <- event_borus |> filter(term < 0)
  aux_2 <- event_borus |> filter(term >= 0)
  aux_3 <- tibble(lhs = "outcome", term = -1, estimate = 0, std.error = 0,
                  conf.low = 0, conf.high = 0, type = "Excluded")
  
  event_borus <- rbind(aux_1, aux_3, aux_2) |> filter(estimate != 'NaN')
  
  #colors <- c("#000000", "#0072B2","#D55E00")
  borus_plot <- ggplot(data = event_borus, mapping = aes(y = estimate, 
                                                         x = term)) +
    geom_point(size = 2) + 
    geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=0.2) +
    geom_hline(yintercept = 0, linetype="solid", color ="grey", 2) +
    geom_vline(xintercept = 0, linetype="dashed", color ="red", 2) +
    theme_bw() +
    ylab("Estimated Value (95% IC)") + 
    xlab("Time to Event") + 
    ggtitle(variable) 
  #scale_color_manual(name = "Periodo", values= colors) +
  #theme(legend.position = "none") 
  ggsave(borus_plot, 
         filename = paste0("../../results/events/test_judicial_imp/borus_",type_variable, 
                           '_', variable,'_', treat_post_p, '_', 
                           time_period,'_',winsor,  ".pdf"), 
         device = cairo_pdf, width = 9.22, height = 7, units = 'in')

  # Gardner
  
}

ps <- p |> filter(condenado>0)
event_general(ps, treat_post_p = 'btreat_300KM2',
              time_period ='bimonthly',
              variable = 'rate_sent_cond', 
              type_variable = 'levels')


df <-p
df <- df |> select(code_inegi, CVE_ENT, year, month, semester, bimonthly, quarterly,
                   crime_5, marg_condition:sent_intensive_incl)

time_period <- 'bimonthly'
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
    summarise(across(c(crime_5, marg_condition:sent_intensive_incl), ~sum(.x))) |> 
    ungroup() |> rename(actual_time = bimonthly)
}else if(time_period == 'semester'){
  ccf <- 23
  ccp <- -23
  pretrend <- T
  future <- T
  df <- df |> group_by(code_inegi, year, semester) |> 
    summarise(across(c(crime_5,marg_condition:sent_intensive_incl), ~sum(.x))) |> 
    ungroup() |> rename(actual_time = semester)
}else if(time_period == 'yearly'){
  ccf <- 10
  ccp <- -10
  pretrend <- T
  future <- T
  df <- df |> group_by(code_inegi, year) |> 
    summarise(across(c(crime_5,marg_condition:sent_intensive_incl), ~sum(.x))) |> 
    ungroup() |> mutate(actual_time = 1)
}else if(time_period == 'quarterly'){
  pretrend <- c(-30:-2)
  future <- c(0:30)
  ccf <- 30
  ccp <- -30
  df <- df |> group_by(code_inegi, year, quarterly) |> 
    summarise(across(c(crime_5,marg_condition:sent_intensive_incl), ~sum(.x))) |> 
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
                                       absolutoria/n_sentenced, 0), 
                   rate_marg_formal_prision = ifelse(marg_condition != 0, 
                                                     formal_prision_marg/marg_condition, 0), 
                   marg_0 = as.integer(marg_condition == 0), 
                   rate_theft_formal_prision = ifelse(crime_5 != 0, 
                                                      formal_prision_theft/crime_5, 0), 
                   theft_0 = as.integer(crime_5 == 0), 
                   sent_intensive = ifelse(n_sentenced != 0, 
                                           sent_intensive/n_sentenced, 0)
)


# df<- df |> left_join(controls |> select(code_inegi, POBTOT), by = 'code_inegi') |> 
#   mutate(sent_prison_100 = (sent_prison*100000)/POBTOT)

df <- df |> left_join(dfini |> select(-c(CVE_ENT:NOM_MUN)), 
                      by = c('code_inegi', 'year', 'actual_time'='month'))

time <- df |> distinct(year, actual_time) |> mutate(m_time = row_number())

df <- df |> left_join(time, by = c('year', 'actual_time'))

treat_post_p <- 'btreat_300KM2'

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

type_variable <- 'log'
if(type_variable == 'levels'){
  df <- df 
}else if(type_variable == 'log'){
  df <- df |> mutate(across(c(marg_condition:sent_intensive_incl), ~log(.x+1)))
}else if(type_variable == 'asinh'){
  df <- df |> mutate(across(c(marg_condition:sent_intensive_incl), ~asinh(.x)))
}else{
  df <- df
  print('Choose between: levels, log, asinh. Code is running logs')
}

df <- df |> left_join(lumipanel, by = c('year', 'code_inegi'))

df <- df |> mutate(below_capacity = ifelse(overcrowding_state < 0, 1, 0))

###### FECT: FE
form<- as.formula(paste0('total_processed', ' ~ below_capacity'))
out.fect <- fect(form, data = df, index = c("code_inegi","m_time"),
                 method = "fe", force = "two-way", se = TRUE, parallel = TRUE,
                 nboots = 200)

plot(out.fect, type = "exit")

dp <- dfini |> filter(is.na(total_clean_state) == T)
out.fect$att.avg
plot(out.fect, main = "Estimated ATT (FEct)", ylab = "Effect of TreatPost on Y",
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8, stats = "F.p")

plot(out.fect, type = "equiv", bound = "equiv", 
     tost.threshold = out.fect$att.avg, 
     ylim = c(-0.15, 0.15))
dev.off()

###### FECT: iFE
form<- as.formula(paste0('sent_prison', ' ~ treat_post'))
out.ife <- fect(form, data = df, index = c("code_inegi","m_time"), 
                force = "two-way", method = "ife", CV = TRUE, r = c(0, 5), 
                se = TRUE, nboots = 200, parallel = TRUE)

print('finished ife')
# pdf(paste0("../../results/events/test_judicial_imp/fectife_",type_variable,
#            '_', variable,'_', treat_post_p, '_',
#            time_period, '_',winsor,  ".pdf"))
plot(out.ife, main = "Estimated ATT (IFEct)", ylab = "Effect of TreatPost on Y", 
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8, stats = "F.p")