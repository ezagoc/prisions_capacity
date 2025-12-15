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

p <- p |> mutate(asin_n_sentenced = asinh(n_sentenced),
                 asin_sent_prison = asinh(sent_prison), 
                 asin_total_processed = asinh(total_processed), 
                 asin_formal_prision = asinh(formal_prision), 
                 asin_free = asinh(free), 
                 asin_only_sent_money = asinh(only_sent_money), 
                 asin_absolutoria = asinh(absolutoria), 
                 condenado_0 = as.integer(condenado > 0),
                 sentenced_0 = as.integer(n_sentenced > 0),
                 formal_prision_0 = as.integer(formal_prision > 0),
                 processed_0 = as.integer(total_processed > 0),
                 sent_prison_0 = as.integer(sent_prison > 0),
                 free_0 = as.integer(free > 0),
                 only_sent_money_0 = as.integer(only_sent_money > 0),
                 absolutoria_0 = as.integer(absolutoria > 0),
                 log_n_sentenced = log(n_sentenced + 1),
                 log_sent_prison = log(sent_prison + 1), 
                 log_total_processed = log(total_processed + 1), 
                 log_formal_prision = log(formal_prision + 1), 
                 log_free = log(free + 1), 
                 log_only_sent_money = log(only_sent_money + 1), 
                 log_absolutoria = log(absolutoria + 1)) |> 
  group_by(year, actual_time) |> 
  mutate(roth_n_sentenced = ifelse(n_sentenced==0, 
                                   -min(n_sentenced[n_sentenced > 0], na.rm = TRUE), 
                                   log(n_sentenced)),
         roth_sent_prison = ifelse(sent_prison==0, 
                                   -min(sent_prison[sent_prison > 0], na.rm = TRUE), 
                                   log(sent_prison)),
         roth_total_processed = ifelse(total_processed==0, 
                                   -min(total_processed[total_processed > 0], na.rm = TRUE), 
                                   log(total_processed)),
         roth_formal_prision = ifelse(formal_prision==0, 
                                   -min(formal_prision[formal_prision > 0], na.rm = TRUE), 
                                   log(formal_prision)),
         roth_free = ifelse(free==0, 
                            -min(free[free > 0], na.rm = TRUE), 
                            log(free)),
         roth_only_sent_money = ifelse(only_sent_money==0, 
                            -min(only_sent_money[only_sent_money > 0], na.rm = TRUE), 
                            log(only_sent_money)),
         roth_absolutoria = ifelse(absolutoria==0, 
                            -min(absolutoria[absolutoria > 0], na.rm = TRUE), 
                            log(absolutoria))) |> ungroup()

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
  # CSDID
  
  print(variable)
  
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
  
  dffinal <- rbind(dfcs, dfcsc)
  return(dffinal)
}

coefs_all <- colnames(p)[14:42] |> 
  map_dfr(~funct_atts(.x))

writexl::write_xlsx(coefs_all, '../../../results/estimates/coefs_robust_log.xlsx')

# For just graph run from here:

coefs_all <- readxl::read_xlsx('../../../results/estimates/coefs_robust_log.xlsx')

coefs_all <- coefs_all |> 
  mutate(Variable = case_when(str_detect(variable, "free") == T ~ 'Released',
                              str_detect(variable, "formal_prision") == T ~ 'Pre-trial Detention',
                              str_detect(variable, "total_processed") == T ~ 'Total Processed',
                              str_detect(variable, "absolutoria") == T ~ 'Not Guilty',
                              str_detect(variable, "processed") == T ~ 'Total Processed',
                              str_detect(variable, "sentenced") == T ~ 'Total Sentenced',
                              str_detect(variable, "n_sentenced") == T ~ 'Total Sentenced',
                              str_detect(variable, "only_sent_money") == T ~ 'Guilty (Money)',
                              str_detect(variable, "sent_prison") == T ~ 'Guilty (Prison)')) |>
  filter(is.na(Variable) == F) |> 
  filter(type == 'CSDID (Controls)')

coefs_all <- coefs_all |> 
  mutate(Transformation = case_when(str_detect(variable, "asin_") == T ~ 'arcsinh(y)',
                              str_detect(variable, "_0") == T ~ '1{y > 0}',
                              str_detect(variable, "log_") == T ~ 'log(y + 1)',
                              str_detect(variable, "roth_") == T ~ 'Chen & Roth (2024)'))

coefs_all$Variable <- factor(coefs_all$Variable, 
                                 levels = c('Not Guilty', 
                                            'Guilty (Money)',
                                            'Guilty (Prison)', 
                                            'Total Sentenced',
                                            'Released', 'Pre-trial Detention', 
                                            'Total Processed'))

size_titles <- 11
results_plot <- ggplot(data = coefs_all, aes(x = estimate, 
                                             y = factor(Variable))) + 
  geom_vline(xintercept = 0, linetype = "solid", color = "darkgrey", size = .8) +
  geom_point(aes(shape = factor(Transformation), color = factor(Transformation)), size = 2, 
             position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(xmin = conf.low1, xmax = conf.high1, 
                    color = factor(Transformation)), position = position_dodge(width = 0.7), 
                width = 0.8, linetype = "solid") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high, 
                    color = factor(Transformation)),
                position = position_dodge(width = 0.7), width = 0.4, 
                linetype = 'solid') +
  scale_shape_manual(values = c(4, 15, 16, 9), name = 'Transformation') +
  scale_color_manual(values = rep('black', 4), name = 'Transformation') +  # Set custom fill colors for points # Set custom line colors for error bars
  theme_bw() +  
  ylab("Variable") + 
  xlab("ATT with 95%-90% Confidence Interval") +  # Change title color
  #ggtitle("Dynamic Effects of the Intervention: Verifiability Analysis") +
  theme(
    axis.text.x = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.x = element_text(size = size_titles), 
    axis.text.y = element_text(size = 11, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.y = element_text(size = size_titles)# X-axis title (e.g., "Event Time")
  ) + xlim(-.05, .13)

results_plot

ggsave(results_plot, 
       filename = paste0('../../../results/att/robustness_chen_roth.pdf'), 
       device = cairo_pdf, width = 8.22, height = 6.59, units = 'in')
