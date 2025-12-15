###################################################
## Data Analysis: Main results ATT (CSDID) Robustness to different Treatments
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

#dfini <- read_parquet(paste0(pfo, 'treatment_judicial.parquet.gzip'))

controls <- read_parquet(paste0(pfo, 'controls.parquet.gzip')) |> 
  select(code_inegi, POBTOT:PHOGJEFF) |>
  mutate(across(c(POBTOT:PHOGJEFF), ~log(.x+1)))

treat <- read_parquet(paste0(pfo, 'treatment_judicial_robustness.parquet.gzip'))

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
                 absolutoria, condenado) |> 
  group_by(code_inegi, year, bimonthly) |> 
  summarise(across(c(total_processed, sent_prison, formal_prision, free, 
                     sent_intensive, sent_intensive_incl, n_sentenced, 
                     only_sent_money, absolutoria, condenado), ~sum(.x))) |> 
  ungroup() |> rename(actual_time = bimonthly)

p <- p |> mutate(log_n_sentenced = log(n_sentenced + 1),
                 log_sent_prison = log(sent_prison + 1), 
                 log_total_processed = log(total_processed + 1), 
                 log_formal_prision = log(formal_prision + 1), 
                 log_free = log(free + 1), 
                 log_only_sent_money = log(only_sent_money + 1), 
                 log_absolutoria = log(absolutoria + 1))

# # Treatment variables
# 
# treat_post_p <- paste0('btreat_', 250, 'KM2')
# 
# impo_treat <- treat |> select(code_inegi, year, month, 
#                               all_of(treat_post_p))

# Define treatment status
  
run_diff_thres <- function(thres, p1){
  treat_post_p <- paste0('btreat_', thres, 'KM2')
  
  impo_treat <- treat |> select(code_inegi, year, month, 
                                all_of(treat_post_p))
  
  p1 <- p1 |> left_join(impo_treat, 
                      by = c('code_inegi', 'year', 'actual_time'='month'))
  
  
  time <- p1 |> distinct(year, actual_time) |> mutate(m_time = row_number())
  
  p1 <- p1 |> left_join(time, by = c('year', 'actual_time'))
  
  p1 <- p1 |> mutate(treat_post := .data[[treat_post_p]])
  
  p1 <- p1 |> group_by(code_inegi) |> 
    mutate(event_time = m_time[treat_post > 0][1], 
           treat = ifelse(is.na(event_time) == F, 1, 0)) |>
    ungroup()
  
  # Control group to 0
  p1$event_time[p1$treat == 0] <- 0
  
  # Time to Event
  p1 <- p1 %>% group_by(code_inegi) %>% 
    mutate(time_to_event = ifelse(treat == 1, m_time - event_time, 
                                  0), 
           time_to_event_gard = ifelse(treat == 1, m_time - event_time, 
                                       Inf)) |> ungroup()
  
  pcont1 <- p1 |> left_join(controls, by = 'code_inegi')
  
  return(list(p1, pcont1))
}

funct_atts <- function(variable){
  # CSDID
  
  print(variable)
  dffinal <- tibble()
  for (i in seq(250, 350, by = 1)){
    print(i)
    list_df <- run_diff_thres(i, p)
    
    p1 <- list_df[1]
    pcont1 <- list_df[2]
    
    cs21 = att_gt(yname = variable, tname = "m_time", idname = "code_inegi", 
                  gname = "event_time", 
                  #xformla = as.formula(addonvar2), 
                  control_group = "nevertreated", # If Too few groups for "nevertreated" default, change to "notyettreated"
                  clustervars = "code_inegi",  
                  data = p1)
    
    agg_att_simple <- aggte(MP = cs21, type = "simple")
    call_sant_coef <- agg_att_simple$overall.att
    call_sant_se <- agg_att_simple$overall.se
    dfcs <- tibble(variable = variable,
                   type = "CSDID", estimate = call_sant_coef,
                   se = call_sant_se, conf.low = estimate - se*1.96, 
                   conf.high = estimate + se*1.96, 
                   conf.low1 = estimate - se*1.645, 
                   conf.high1 = estimate + se*1.645)
    
    # #CSDID Controls:
    # 
    # cs21 = att_gt(yname = variable, tname = "m_time", idname = "code_inegi", 
    #               gname = "event_time", 
    #               xformla = ~ PMASC18_ + VP_TV + VP_RADIO + PNOTRABA, 
    #               control_group = "nevertreated", # If Too few groups for "nevertreated" default, change to "notyettreated"
    #               clustervars = "code_inegi",  
    #               data = pcont1)
    # 
    # agg_att_simple <- aggte(MP = cs21, type = "simple")
    # call_sant_coef <- agg_att_simple$overall.att
    # call_sant_se <- agg_att_simple$overall.se
    # dfcsc <- tibble(variable = variable, 
    #                 type = "CSDID (Controls)", estimate = call_sant_coef,
    #                 se = call_sant_se, conf.low = estimate - se*1.96, 
    #                 conf.high = estimate + se*1.96, 
    #                 conf.low1 = estimate - se*1.645, 
    #                 conf.high1 = estimate + se*1.645)
    # dfint <- rbind(dfcs, dfcsc) |> 
    #   mutate(threshold = i)
    
    dfint <- dfcs |> mutate(threshold = i)
    
    dffinal <- rbind(dffinal, dfint) 
    
  }
  return(dffinal)
}

coefs_all <- c('log_sent_prison') |> 
  map_dfr(~funct_atts(.x))

writexl::write_xlsx(coefs_all, '../../../results/estimates/coefs_robust_sent_prison.xlsx')

coefs_all <- c('log_total_processed') |> 
  map_dfr(~funct_atts(.x))

writexl::write_xlsx(coefs_all, '../../../results/estimates/coefs_robust_processed.xlsx')

coefs_all <- c('log_free') |> 
  map_dfr(~funct_atts(.x))

writexl::write_xlsx(coefs_all, '../../../results/estimates/coefs_robust_free.xlsx')

coefs_all <- c('log_formal_prision') |> 
  map_dfr(~funct_atts(.x))

writexl::write_xlsx(coefs_all, '../../../results/estimates/coefs_robust_formal_prision.xlsx')


coefs_all <- c('log_only_sent_money') |> 
  map_dfr(~funct_atts(.x))

writexl::write_xlsx(coefs_all, '../../../results/estimates/coefs_robust_only_sent_money.xlsx')


# For just graph run from here:

coefs_all <- rbind(readxl::read_xlsx('../../../results/estimates/coefs_robust_sent_prison.xlsx'),
                   readxl::read_xlsx('../../../results/estimates/coefs_robust_processed.xlsx'), 
                   readxl::read_xlsx('../../../results/estimates/coefs_robust_free.xlsx'), 
                   readxl::read_xlsx('../../../results/estimates/coefs_robust_formal_prision.xlsx'), 
                   readxl::read_xlsx('../../../results/estimates/coefs_robust_only_sent_money.xlsx'))


size_titles <- 13
p1 <- ggplot(data = coefs_all |> filter(variable == 'log_sent_prison'),
                       aes(x = threshold, y = estimate)) + 
  geom_hline(yintercept = 0, linetype = "solid", color = "darkgrey", size = .8) +
  geom_vline(xintercept = 300, linetype = "dashed", color = "darkgrey", size = .8) +
  geom_point(aes()) +
  geom_errorbar(aes(ymin = conf.low1, ymax = conf.high1), 
                width = 0.8, linetype = "solid") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.4, 
                linetype = 'solid') +
  theme_bw() +  
  ylab("ATT with 95%-90% Confidence Interval") + 
  xlab("Threshold (KM)") +  # Change title color
  ggtitle("log Guilty (Prison)") +
  theme(
    axis.text.x = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.x = element_text(size = size_titles), 
    axis.text.y = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.y = element_text(size = size_titles), # X-axis title (e.g., "Event Time")
    plot.title = element_text(size = size_titles, hjust = 0.5)
  ) + ylim(-.15, .15)

p1

p2 <- ggplot(data = coefs_all |> filter(variable == 'log_total_processed'),
             aes(x = threshold, y = estimate)) + 
  geom_hline(yintercept = 0, linetype = "solid", color = "darkgrey", size = .8) +
  geom_vline(xintercept = 300, linetype = "dashed", color = "darkgrey", size = .8) +
  geom_point(aes()) +
  geom_errorbar(aes(ymin = conf.low1, ymax = conf.high1), 
                width = 0.8, linetype = "solid") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.4, 
                linetype = 'solid') +
  theme_bw() +  
  ylab("ATT with 95%-90% Confidence Interval") + 
  xlab("") +  # Change title color
  ggtitle("log Total Processed") +
  theme(
    axis.text.x = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.x = element_text(size = size_titles), 
    axis.text.y = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.y = element_text(size = size_titles), # X-axis title (e.g., "Event Time")
    plot.title = element_text(size = size_titles, hjust = 0.5)
  ) + ylim(-.15, .15)

p2


p3 <- ggplot(data = coefs_all |> filter(variable == 'log_formal_prision'),
             aes(x = threshold, y = estimate)) + 
  geom_hline(yintercept = 0, linetype = "solid", color = "darkgrey", size = .8) +
  geom_vline(xintercept = 300, linetype = "dashed", color = "darkgrey", size = .8) +
  geom_point(aes()) +
  geom_errorbar(aes(ymin = conf.low1, ymax = conf.high1), 
                width = 0.8, linetype = "solid") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.4, 
                linetype = 'solid') +
  theme_bw() +  
  ylab("") + 
  xlab("") +  # Change title color
  ggtitle("log Pre-trial Detention") +
  theme(
    axis.text.x = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.x = element_text(size = size_titles), 
    axis.text.y = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.y = element_text(size = size_titles), # X-axis title (e.g., "Event Time")
    plot.title = element_text(size = size_titles, hjust = 0.5)
  ) + ylim(-.15, .15)

p3


p4 <- ggplot(data = coefs_all |> filter(variable == 'log_only_sent_money'),
             aes(x = threshold, y = estimate)) + 
  geom_hline(yintercept = 0, linetype = "solid", color = "darkgrey", size = .8) +
  geom_vline(xintercept = 300, linetype = "dashed", color = "darkgrey", size = .8) +
  geom_point(aes()) +
  geom_errorbar(aes(ymin = conf.low1, ymax = conf.high1), 
                width = 0.8, linetype = "solid") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.4, 
                linetype = 'solid') +
  theme_bw() +  
  ylab("") + 
  xlab("Threshold (KM)") +  # Change title color
  ggtitle("log Guilty (Money)") +
  theme(
    axis.text.x = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.x = element_text(size = size_titles), 
    axis.text.y = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.y = element_text(size = size_titles), # X-axis title (e.g., "Event Time")
    plot.title = element_text(size = size_titles, hjust = 0.5)
  ) + ylim(-.15, .15)

p4

library(patchwork)

results_plot <- (p2 | p3) / (p1 | p4)
results_plot

ggsave(results_plot, 
       filename = paste0('../../../results/att/robustness_diff_threshold.pdf'), 
       device = cairo_pdf, width = 9, height = 9, units = 'in')
