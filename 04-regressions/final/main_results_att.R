###################################################
## Data Analysis: Main results ATT (CSDID)
## Author: Eduardo Zago-Cuevas (all errors are my own)
## Run before: same folder, a number before
## Output: Capacity panel for DiD
##
###################################################

# install.packages('pacman')

pacman::p_load(tidyverse, arrow, purrr, sf, fixest, didimputation, did, 
               did2s, fect, patchwork)

rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pfo <- '../../../data/03-analysis/'

# Dataset: 
# Judicial data

p1 <- read_parquet('../../../data/01-judicial/00-sentencing/final/panel_comun_1997_2008.parquet.gzip') |>
  filter(year > 1999)

p2 <- read_parquet('../../../data/01-judicial/00-sentencing/final/panel_comun_2009_2012.parquet.gzip')

p <- bind_rows(p1, p2) #|> filter(!code_inegi %in% c(28025, 28027)) # Filter out noisy municipalities
# Treatment

dfini <- read_parquet(paste0(pfo, 'treatment_judicial.parquet.gzip'))

controls <- read_parquet(paste0(pfo, 'controls.parquet.gzip')) |> 
  select(code_inegi, POBTOT:PHOGJEFF) |>
  mutate(across(c(POBTOT:PHOGJEFF), ~log(.x+1)))

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

p <- p |> select(code_inegi, CVE_ENT, year, bimonthly, 
                 crime_5, sent_prison, total_processed, formal_prision, free, 
                 sent_intensive, sent_intensive_incl, n_sentenced, only_sent_money, 
                 absolutoria, condenado
                 ) |> 
  group_by(code_inegi, year, bimonthly) |> 
  summarise(across(c(total_processed, sent_prison, formal_prision, free, 
                     sent_intensive, sent_intensive_incl, n_sentenced, 
                     only_sent_money, absolutoria, condenado), ~sum(.x))) |> 
  ungroup() |> rename(actual_time = bimonthly)

p <- p |> mutate(rate_free = ifelse(total_processed != 0, 
                                    free/total_processed, 0), 
                 processed_0 = as.integer(total_processed == 0),
                 rate_formal = ifelse(total_processed != 0, 
                                      formal_prision/total_processed, 0),
                 sent_intensive = log(ifelse(n_sentenced != 0, 
                                             sent_intensive_incl/n_sentenced, 0)+1), 
                 rate_sent_cond = ifelse(condenado != 0, 
                                         sent_prison/condenado, 0), 
                 rate_cond = ifelse(n_sentenced != 0, 
                                    condenado/n_sentenced, 0),
                 rate_only_money = ifelse(condenado != 0, 
                                          only_sent_money/condenado, 0),
                 condenado_0 = as.integer(condenado == 0),
                 sentenced_0 = as.integer(n_sentenced == 0),
                 n_sentenced = log(n_sentenced + 1),
                 sent_prison = log(sent_prison + 1), 
                 total_processed = log(total_processed + 1), 
                 formal_prision = log(formal_prision + 1), 
                 free = log(free + 1), 
                 only_sent_money = log(only_sent_money + 1), 
                 absolutoria = log(absolutoria + 1)
                 )

# Define treatment status
treat_post_p <- 'btreat_300KM2'

p <- p |> left_join(dfini |> select(-c(CVE_ENT:NOM_MUN)), 
                      by = c('code_inegi', 'year', 'actual_time'='month'))

time <- p |> distinct(year, actual_time) |> mutate(m_time = row_number())

p <- p |> left_join(time, by = c('year', 'actual_time'))

p <- p |> mutate(treat_post := .data[[treat_post_p]])

p <- p |> group_by(code_inegi) |> 
  mutate(event_time = m_time[treat_post > 0][1], 
         treat = ifelse(is.na(event_time) == F, 1, 0)) |>
  ungroup()

# Control group to 0
p$event_time[p$treat == 0] <- 0

# Time to Event
p <- p %>% group_by(code_inegi) %>% 
  mutate(time_to_event = ifelse(treat == 1, m_time - event_time, 
                                0), 
         time_to_event_gard = ifelse(treat == 1, m_time - event_time, 
                                     Inf)) |> ungroup()

pcont <- p |> left_join(controls)

funct_atts <- function(variable){
  # TWFE
  fmla <- as.formula(paste0(variable, 
                            ' ~ treat_post | code_inegi + m_time'))
  twfe <- feols(fmla, cluster = "code_inegi", data = p)
  dftwfe <- tibble(variable = variable, type = "TWFE", 
                   estimate = c(twfe$coefficients), se = twfe$se, 
                   conf.low = estimate - 1.96*se, 
                   conf.high = estimate + 1.96*se,
                   conf.low1 = estimate - se*1.645, 
                   conf.high1 = estimate + se*1.645 
  )
  
  # #FEct
  # form<- as.formula(paste0(variable, ' ~ treat_post'))
  # out.fect <- fect(form, data = p, index = c("code_inegi","m_time"),
  #                  method = "fe", force = "two-way", se = TRUE, parallel = TRUE,
  #                  nboots = 500)
  # 
  # dffec <- tibble(variable = variable, type = "FEct", estimate = out.fect$est.avg[1],
  #                 se = out.fect$est.avg[2], conf.low = estimate - se*1.96, 
  #                 conf.high = estimate + se*1.96,
  #                 conf.low1 = estimate - se*1.645, 
  #                 conf.high1 = estimate + se*1.645)
  
  # CSDID
  
  cs21 = att_gt(yname = variable, tname = "m_time", idname = "code_inegi", 
                gname = "event_time", 
                #xformla = as.formula(addonvar2), 
                control_group = "nevertreated", # If Too few groups for "nevertreated" default, change to "notyettreated"
                clustervars = "code_inegi",  
                data = p)
  
  agg_att_simple <- aggte(MP = cs21, type = "simple")
  call_sant_coef <- agg_att_simple$overall.att
  call_sant_se <- agg_att_simple$overall.se
  dfcs <- tibble(variable = variable,
                 type = "CSDID", estimate = call_sant_coef,
                 se = call_sant_se, conf.low = estimate - se*1.96, 
                 conf.high = estimate + se*1.96, 
                 conf.low1 = estimate - se*1.645, 
                 conf.high1 = estimate + se*1.645)
  
  #CSDID Controls:
  
  cs21 = att_gt(yname = variable, tname = "m_time", idname = "code_inegi", 
                gname = "event_time", 
                xformla = ~ PMASC18_ + VP_TV + VP_RADIO + PNOTRABA, 
                control_group = "nevertreated", # If Too few groups for "nevertreated" default, change to "notyettreated"
                clustervars = "code_inegi",  
                data = pcont)
  
  agg_att_simple <- aggte(MP = cs21, type = "simple")
  call_sant_coef <- agg_att_simple$overall.att
  call_sant_se <- agg_att_simple$overall.se
  dfcsc <- tibble(variable = variable, 
                  type = "CSDID (Controls)", estimate = call_sant_coef,
                  se = call_sant_se, conf.low = estimate - se*1.96, 
                  conf.high = estimate + se*1.96, 
                  conf.low1 = estimate - se*1.645, 
                  conf.high1 = estimate + se*1.645)
  
  dffinal <- rbind(dftwfe, dfcs, dfcsc)
  return(dffinal)
}

coefs_sent <- c('absolutoria', 'sent_prison', 'sent_intensive', 
                'only_sent_money', 'n_sentenced') |> 
  map_dfr(~funct_atts(.x))

writexl::write_xlsx(coefs_sent, '../../../results/estimates/sent_coefs_final.xlsx')

coefs_process <- c('total_processed', 'formal_prision', 'free') |> 
  map_dfr(~funct_atts(.x))

writexl::write_xlsx(coefs_process, '../../../results/estimates/processed_coefs_final.xlsx')

# Rates: 

funct_atts_rate <- function(variable, control){
  # TWFE
  fmla <- as.formula(paste0(variable, 
                            ' ~ treat_post +', control, ' | code_inegi + m_time'))
  twfe <- feols(fmla, cluster = "code_inegi", data = p)
  dftwfe <- tibble(variable = variable, type = "TWFE", 
                   estimate = twfe$coefficients[1], se = twfe$se[1], 
                   conf.low = estimate - 1.96*se, 
                   conf.high = estimate + 1.96*se,
                   conf.low1 = estimate - se*1.645, 
                   conf.high1 = estimate + se*1.645 
  )
  
  # CSDID
  
  cs21 = att_gt(yname = variable, tname = "m_time", idname = "code_inegi", 
                gname = "event_time", 
                xformla = as.formula(paste0('~', control)), 
                control_group = "nevertreated", # If Too few groups for "nevertreated" default, change to "notyettreated"
                clustervars = "code_inegi",  
                data = p)
  
  agg_att_simple <- aggte(MP = cs21, type = "simple")
  call_sant_coef <- agg_att_simple$overall.att
  call_sant_se <- agg_att_simple$overall.se
  dfcs <- tibble(variable = variable,
                 type = "CSDID", estimate = call_sant_coef,
                 se = call_sant_se, conf.low = estimate - se*1.96, 
                 conf.high = estimate + se*1.96, 
                 conf.low1 = estimate - se*1.645, 
                 conf.high1 = estimate + se*1.645)
  print('CDDID')
  #CSDID Controls:
  
  cs21 = att_gt(yname = variable, tname = "m_time", idname = "code_inegi", 
                gname = "event_time", 
                xformla = as.formula(paste0('~ PMASC18_ + VP_TV + VP_RADIO + PNOTRABA + ', 
                                 control)), 
                control_group = "nevertreated", # If Too few groups for "nevertreated" default, change to "notyettreated"
                clustervars = "code_inegi",  
                data = pcont)
  
  agg_att_simple <- aggte(MP = cs21, type = "simple")
  call_sant_coef <- agg_att_simple$overall.att
  call_sant_se <- agg_att_simple$overall.se
  dfcsc <- tibble(variable = variable, 
                  type = "CSDID (Controls)", estimate = call_sant_coef,
                  se = call_sant_se, conf.low = estimate - se*1.96, 
                  conf.high = estimate + se*1.96, 
                  conf.low1 = estimate - se*1.645, 
                  conf.high1 = estimate + se*1.645)
  
  # #FEct
  # form<- as.formula(paste0(variable, ' ~ treat_post + ', control))
  # out.fect <- fect(form, data = p, index = c("code_inegi","m_time"),
  #                  method = "fe", force = "two-way", se = TRUE, parallel = TRUE,
  #                  nboots = 500)
  # 
  # dffec <- tibble(variable = variable, type = "FEct", estimate = out.fect$est.avg[1],
  #                 se = out.fect$est.avg[2], conf.low = estimate - se*1.96, 
  #                 conf.high = estimate + se*1.96,
  #                 conf.low1 = estimate - se*1.645, 
  #                 conf.high1 = estimate + se*1.645)
  
  
  
  dffinal <- rbind(dftwfe, dfcs, dfcsc)
  return(dffinal)
}

# 1st Instance

formal <- funct_atts_rate('rate_formal', 'processed_0')

free <- funct_atts_rate('rate_free', 'processed_0')

# 2nd Instance

cond <- funct_atts_rate('rate_cond', 'sentenced_0')

sent_cond <- funct_atts_rate('rate_sent_cond', 'condenado_0')

only_money <- funct_atts_rate('rate_only_money', 'condenado_0')

# Graphs: Processing:

coefs_process <- rbind(coefs_process, formal, free)

coefs_process <- coefs_process |> 
  mutate(Variable = case_when(variable == 'free' ~ 'log Released', 
                              variable == 'formal_prision' ~ 'log Pre-trial Detention', 
                              variable == 'total_processed' ~ 'log Total Processed', 
                              variable == 'rate_formal' ~ 'Pre-trial detention / processed', 
                              variable == 'rate_free' ~ 'Released / processed'))

coefs_process$Variable <- factor(coefs_process$Variable, 
                         levels = c('Released / processed', 
                                    'Pre-trial detention / processed',
                                    'log Released', 'log Pre-trial Detention', 
                                    'log Total Processed'))

coefs_process$type <- factor(coefs_process$type, 
                     levels = c('TWFE', 'CSDID', 'CSDID (Controls)'))

size_titles <- 11
results_plot <- ggplot(data = coefs_process, aes(x = estimate, y = factor(Variable))) + 
  geom_vline(xintercept = 0, linetype = "solid", color = "darkgrey", size = .8) +
  geom_point(aes(shape = factor(type), color = factor(type)), size = 2, 
             position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(xmin = conf.low1, xmax = conf.high1, 
                    color = factor(type)), position = position_dodge(width = 0.6), 
                width = 0.8, linetype = "solid") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high, 
                    color = factor(type)),
                position = position_dodge(width = 0.6), width = 0.4, 
                linetype = 'solid') +
  scale_shape_manual(values = c(4, 15, 16), name = 'Estimator') +
  scale_color_manual(values = rep('black', 4), name = 'Estimator') +  # Set custom fill colors for points # Set custom line colors for error bars
  theme_bw() +  
  ylab("Variable") + 
  xlab("ATT with 95%-90% Confidence Interval") +  # Change title color
  #ggtitle("Dynamic Effects of the Intervention: Verifiability Analysis") +
  theme(
    axis.text.x = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.x = element_text(size = size_titles), 
    axis.text.y = element_text(size = 11, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.y = element_text(size = size_titles)# X-axis title (e.g., "Event Time")
  ) + xlim(-.05, .12)

results_plot

ggsave(results_plot, 
       filename = paste0('../../../results/att/first_instance_final.pdf'), 
       device = cairo_pdf, width = 8.22, height = 6.59, units = 'in')


# Graphs: Sentenced

coefs_sent <- rbind(coefs_sent, cond, sent_cond, only_money)

coefs_sent <- coefs_sent |> 
  mutate(Variable = case_when(variable == 'sent_prison' ~ 'log Guilty (Prison)', 
                              variable == 'n_sentenced' ~ 'log Total Sentenced', 
                              variable == 'only_sent_money' ~ 'log Guilty (Money)',
                              variable == 'absolutoria' ~ 'log Not Guilty',
                              variable == 'sent_intensive' ~ 'Time Sentenced', 
                              variable == 'rate_cond' ~ 'Guilty / sentenced', 
                              variable == 'rate_sent_cond' ~ 'Prison / guilty', 
                              variable == 'rate_only_money' ~ 'Money / guilty'))

coefs_sent$Variable <- factor(coefs_sent$Variable, 
                         levels = c('Money / guilty', 'Prison / guilty', 
                                    'Guilty / sentenced', 'Time Sentenced',
                                    'log Not Guilty', 'log Guilty (Money)', 
                                    'log Guilty (Prison)', 'log Total Sentenced'))

coefs_sent$type <- factor(coefs_sent$type, 
                             levels = c('TWFE', 'CSDID', 'CSDID (Controls)'))

size_titles <- 11
results_plot <- ggplot(data = coefs_sent, aes(x = estimate, y = factor(Variable))) + 
  geom_vline(xintercept = 0, linetype = "solid", color = "darkgrey", size = .8) +
  geom_point(aes(shape = factor(type), color = factor(type)), size = 2, 
             position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(xmin = conf.low1, xmax = conf.high1, 
                    color = factor(type)), position = position_dodge(width = 0.6), 
                width = 0.8, linetype = "solid") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high, 
                    color = factor(type)),
                position = position_dodge(width = 0.6), width = 0.4, 
                linetype = 'solid') +
  scale_shape_manual(values = c(4, 15, 16), name = 'Estimator') +
  scale_color_manual(values = rep('black', 4), name = 'Estimator') +  # Set custom fill colors for points # Set custom line colors for error bars
  theme_bw() +  
  ylab("Variable") + 
  xlab("ATT with 95%-90% Confidence Intervals") +  # Change title color
  #ggtitle("Dynamic Effects of the Intervention: Verifiability Analysis") +
  theme(
    axis.text.x = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.x = element_text(size = size_titles), 
    axis.text.y = element_text(size = 10, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.y = element_text(size = size_titles)# X-axis title (e.g., "Event Time")
  ) + xlim(-.06, .10)

results_plot

ggsave(results_plot, 
       filename = paste0('../../../results/att/second_instance_final.pdf'), 
       device = cairo_pdf, width = 8.22, height = 7.6, units = 'in')
