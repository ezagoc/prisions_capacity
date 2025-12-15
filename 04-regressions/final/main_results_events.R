###################################################
## Data Analysis: MAIN RESULTS (Event Studies)
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

funct_event <- function(variable){

  cs21 = att_gt(yname = variable, tname = "m_time", idname = "code_inegi", 
              gname = "event_time", 
              xformla = ~ PMASC18_ + VP_TV + VP_RADIO + PNOTRABA, 
              control_group = "nevertreated", # If Too few groups for "nevertreated" default, change to "notyettreated"
              clustervars = "code_inegi",  
              data = pcont)

  cs_event <- aggte(MP = cs21, type = "dynamic", min_e = -60, max_e = 60)

  df_call <- tibble(time = cs_event$egt, coef = cs_event$att.egt, 
                  se = cs_event$se.egt, variable = variable, 
                  ci_low = coef - se*(qnorm(1-(1-0.95)/2)), 
                  ci_up = coef + se*(qnorm(1-(1-0.95)/2)), 
                  ci_low1 = coef - se*(qnorm(1-(1-0.90)/2)), 
                  ci_up1 = coef + se*(qnorm(1-(1-0.90)/2)))
  
  return(df_call)
  print(variable)
}

coefs_sent <- c('only_sent_money','absolutoria', 'sent_prison', 'sent_intensive', 
                'n_sentenced') |> 
  map_dfr(~funct_event(.x)) |> 
  mutate(Variable = case_when(variable == 'sent_prison' ~ 'log Guilty (Prison)', 
                              variable == 'n_sentenced' ~ 'log Total Sentenced', 
                              variable == 'only_sent_money' ~ 'log Guilty (Money)',
                              variable == 'absolutoria' ~ 'log Not Guilty',
                              variable == 'sent_intensive' ~ 'Time Sentenced', 
                              variable == 'rate_cond' ~ 'Guilty / sentenced', 
                              variable == 'rate_sent_cond' ~ 'Prison / guilty', 
                              variable == 'rate_only_money' ~ 'Money / guilty'))

funct_events_rate <- function(variable, control){
  
  cs21 = att_gt(yname = variable, tname = "m_time", idname = "code_inegi", 
                gname = "event_time", 
                xformla = as.formula(paste0('~ PMASC18_ + VP_TV + VP_RADIO + PNOTRABA + ', 
                                            control)), 
                control_group = "nevertreated", # If Too few groups for "nevertreated" default, change to "notyettreated"
                clustervars = "code_inegi",  
                data = pcont)
  
  cs_event <- aggte(MP = cs21, type = "dynamic", min_e = -60, max_e = 60)
  
  df_call <- tibble(time = cs_event$egt, coef = cs_event$att.egt, 
                    se = cs_event$se.egt, variable = variable, 
                    ci_low = coef - se*(qnorm(1-(1-0.95)/2)), 
                    ci_up = coef + se*(qnorm(1-(1-0.95)/2)), 
                    ci_low1 = coef - se*(qnorm(1-(1-0.90)/2)), 
                    ci_up1 = coef + se*(qnorm(1-(1-0.90)/2)))
}

cond <- funct_events_rate('rate_cond', 'sentenced_0')

sent_cond <- funct_events_rate('rate_sent_cond', 'condenado_0')

only_money <- funct_events_rate('rate_only_money', 'condenado_0')

size_titles = 13
size_point = 1
ylimb = .7
ylima = .5
# First Levels variables:

p1 <- ggplot(data = coefs_sent |> filter(variable == 'n_sentenced'), 
                  mapping = aes(y = coef, x = time)) +
  geom_hline(yintercept = 0, linetype="solid", color ='darkgrey', 3) +
  geom_point(size = size_point) + 
  geom_vline(xintercept = 0, linetype="solid", color ="darkgrey", 2) +
  geom_errorbar(aes(ymin=ci_low, ymax=ci_up), 
                width=0.5, linetype = 'solid') +
  ggtitle('log Total Sentenced') +
  theme_bw() +
  ylab('Estimated Value (95% C.I.)') + 
  xlab("")  + 
  ylim(-ylimb, ylima) + 
  theme(legend.position = "none") + theme(
    axis.text.x = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.x = element_text(size = size_titles), 
    axis.text.y = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.y = element_text(size = size_titles), 
    plot.title = element_text(size = size_titles, hjust = 0.5)# X-axis title (e.g., "Event Time")
  ) 

p2 <- ggplot(data = coefs_sent |> filter(variable == 'sent_prison'), 
             mapping = aes(y = coef, x = time)) +
  geom_hline(yintercept = 0, linetype="solid", color ='darkgrey', 3) +
  geom_point(size = size_point) + 
  geom_vline(xintercept = 0, linetype="solid", color ="darkgrey", 2) +
  geom_errorbar(aes(ymin=ci_low, ymax=ci_up), 
                width=0.5, linetype = 'solid') +
  ggtitle('log Guilty (Prison)') +
  theme_bw() +
  ylab('') + 
  xlab("")  + 
  ylim(-ylimb, ylima) + 
  theme(legend.position = "none") + theme(
    axis.text.x = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.x = element_text(size = size_titles), 
    axis.text.y = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.y = element_text(size = size_titles), 
    plot.title = element_text(size = size_titles, hjust = 0.5)# X-axis title (e.g., "Event Time")
  ) 

p3 <- ggplot(data = coefs_sent |> filter(variable == 'only_sent_money'), 
             mapping = aes(y = coef, x = time)) +
  geom_hline(yintercept = 0, linetype="solid", color ='darkgrey', 3) +
  geom_point(size = size_point) + 
  geom_vline(xintercept = 0, linetype="solid", color ="darkgrey", 2) +
  geom_errorbar(aes(ymin=ci_low, ymax=ci_up), 
                width=0.5, linetype = 'solid') +
  ggtitle('log Guilty (Money)') +
  theme_bw() +
  ylab('Estimated Value (95% C.I.)') + 
  xlab("Time to Treatment (Bimonthly)")  + 
  ylim(-ylimb, ylima) + 
  theme(legend.position = "none") + theme(
    axis.text.x = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.x = element_text(size = size_titles), 
    axis.text.y = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.y = element_text(size = size_titles), 
    plot.title = element_text(size = size_titles, hjust = 0.5)# X-axis title (e.g., "Event Time")
  ) 

p4 <- ggplot(data = coefs_sent |> filter(variable == 'absolutoria'), 
             mapping = aes(y = coef, x = time)) +
  geom_hline(yintercept = 0, linetype="solid", color ='darkgrey', 3) +
  geom_point(size = size_point) + 
  geom_vline(xintercept = 0, linetype="solid", color ="darkgrey", 2) +
  geom_errorbar(aes(ymin=ci_low, ymax=ci_up), 
                width=0.5, linetype = 'solid') +
  ggtitle('log Not Guilty') +
  theme_bw() +
  ylab('') + 
  xlab("Time to Treatment (Bimonthly)")  + 
  ylim(-ylimb, ylima) + 
  theme(legend.position = "none") + theme(
    axis.text.x = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.x = element_text(size = size_titles), 
    axis.text.y = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.y = element_text(size = size_titles), 
    plot.title = element_text(size = size_titles, hjust = 0.5)# X-axis title (e.g., "Event Time")
  ) 

final <- (p1 | p2) / (p3 | p4)

ggsave(final, 
       filename = paste0("../../../results/events/final/sentence_levels_event.pdf"), 
       device = cairo_pdf, width = 11, height = 8, units = 'in')


#### Rates: 

size_titles = 13
size_point = 1
ylimb = .4
ylima = .4

p1 <- ggplot(data = coefs_sent |> filter(variable == 'sent_intensive'), 
             mapping = aes(y = coef, x = time)) +
  geom_hline(yintercept = 0, linetype="solid", color ='darkgrey', 3) +
  geom_point(size = size_point) + 
  geom_vline(xintercept = 0, linetype="solid", color ="darkgrey", 2) +
  geom_errorbar(aes(ymin=ci_low, ymax=ci_up), 
                width=0.5, linetype = 'solid') +
  ggtitle('Time Sentenced') +
  theme_bw() +
  ylab('Estimated Value (95% C.I.)') + 
  xlab("")  + 
  ylim(-ylimb, ylima) + 
  theme(legend.position = "none") + theme(
    axis.text.x = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.x = element_text(size = size_titles), 
    axis.text.y = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.y = element_text(size = size_titles), 
    plot.title = element_text(size = size_titles, hjust = 0.5)# X-axis title (e.g., "Event Time")
  ) 

p2 <- ggplot(data = cond, 
             mapping = aes(y = coef, x = time)) +
  geom_hline(yintercept = 0, linetype="solid", color ='darkgrey', 3) +
  geom_point(size = size_point) + 
  geom_vline(xintercept = 0, linetype="solid", color ="darkgrey", 2) +
  geom_errorbar(aes(ymin=ci_low, ymax=ci_up), 
                width=0.5, linetype = 'solid') +
  ggtitle('Guilty / sentenced') +
  theme_bw() +
  ylab('') + 
  xlab("")  + 
  ylim(-ylimb, ylima) + 
  theme(legend.position = "none") + theme(
    axis.text.x = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.x = element_text(size = size_titles), 
    axis.text.y = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.y = element_text(size = size_titles), 
    plot.title = element_text(size = size_titles, hjust = 0.5)# X-axis title (e.g., "Event Time")
  ) 

p2

p3 <- ggplot(data = sent_cond, 
             mapping = aes(y = coef, x = time)) +
  geom_hline(yintercept = 0, linetype="solid", color ='darkgrey', 3) +
  geom_point(size = size_point) + 
  geom_vline(xintercept = 0, linetype="solid", color ="darkgrey", 2) +
  geom_errorbar(aes(ymin=ci_low, ymax=ci_up), 
                width=0.5, linetype = 'solid') +
  ggtitle('Prison / guilty') +
  theme_bw() +
  ylab('Estimated Value (95% C.I.)') + 
  xlab("Time to Treatment (Bimonthly)")  + 
  ylim(-ylimb, ylima) + 
  theme(legend.position = "none") + theme(
    axis.text.x = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.x = element_text(size = size_titles), 
    axis.text.y = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.y = element_text(size = size_titles), 
    plot.title = element_text(size = size_titles, hjust = 0.5)# X-axis title (e.g., "Event Time")
  ) 

p3

p4 <- ggplot(data = only_money, 
             mapping = aes(y = coef, x = time)) +
  geom_hline(yintercept = 0, linetype="solid", color ='darkgrey', 3) +
  geom_point(size = size_point) + 
  geom_vline(xintercept = 0, linetype="solid", color ="darkgrey", 2) +
  geom_errorbar(aes(ymin=ci_low, ymax=ci_up), 
                width=0.5, linetype = 'solid') +
  ggtitle('Money / guilty') +
  theme_bw() +
  ylab('') + 
  xlab("Time to Treatment (Bimonthly)")  + 
  ylim(-.1, .1) + 
  theme(legend.position = "none") + theme(
    axis.text.x = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.x = element_text(size = size_titles), 
    axis.text.y = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.y = element_text(size = size_titles), 
    plot.title = element_text(size = size_titles, hjust = 0.5)# X-axis title (e.g., "Event Time")
  ) 

final <- (p1 | p2) / (p3 | p4)

final

ggsave(final, 
       filename = paste0("../../../results/events/final/sentence_rate_event.pdf"), 
       device = cairo_pdf, width = 11, height = 8, units = 'in')



#############################################

#Process: 

coefs_process <- c('total_processed', 'formal_prision', 'free') |> 
  map_dfr(~funct_event(.x))

#### Rates: 

formal <- funct_events_rate('rate_formal', 'processed_0')

free <- funct_events_rate('rate_free', 'processed_0')

size_titles = 13
size_point = 1
ylimb = .7
ylima = .7

p1 <- ggplot(data = coefs_process |> filter(variable == 'total_processed'), 
             mapping = aes(y = coef, x = time)) +
  geom_hline(yintercept = 0, linetype="solid", color ='darkgrey', 3) +
  geom_point(size = size_point) + 
  geom_vline(xintercept = 0, linetype="solid", color ="darkgrey", 2) +
  geom_errorbar(aes(ymin=ci_low, ymax=ci_up), 
                width=0.5, linetype = 'solid') +
  ggtitle('log Total Processed') +
  theme_bw() +
  ylab('Estimated Value (95% C.I.)') + 
  xlab("")  + 
  ylim(-ylimb, ylima) + 
  theme(legend.position = "none") + theme(
    axis.text.x = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.x = element_text(size = size_titles), 
    axis.text.y = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.y = element_text(size = size_titles), 
    plot.title = element_text(size = size_titles, hjust = 0.5)# X-axis title (e.g., "Event Time")
  ) 

p2 <- ggplot(data = coefs_process |> filter(variable == 'formal_prision'), 
             mapping = aes(y = coef, x = time)) +
  geom_hline(yintercept = 0, linetype="solid", color ='darkgrey', 3) +
  geom_point(size = size_point) + 
  geom_vline(xintercept = 0, linetype="solid", color ="darkgrey", 2) +
  geom_errorbar(aes(ymin=ci_low, ymax=ci_up), 
                width=0.5, linetype = 'solid') +
  ggtitle('log Pre-trial Detention') +
  theme_bw() +
  ylab('') + 
  xlab("")  + 
  ylim(-ylimb, ylima) + 
  theme(legend.position = "none") + theme(
    axis.text.x = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.x = element_text(size = size_titles), 
    axis.text.y = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.y = element_text(size = size_titles), 
    plot.title = element_text(size = size_titles, hjust = 0.5)# X-axis title (e.g., "Event Time")
  ) 

p3 <- ggplot(data = coefs_process |> filter(variable == 'free'), 
             mapping = aes(y = coef, x = time)) +
  geom_hline(yintercept = 0, linetype="solid", color ='darkgrey', 3) +
  geom_point(size = size_point) + 
  geom_vline(xintercept = 0, linetype="solid", color ="darkgrey", 2) +
  geom_errorbar(aes(ymin=ci_low, ymax=ci_up), 
                width=0.5, linetype = 'solid') +
  ggtitle('log Released') +
  theme_bw() +
  ylab('Estimated Value (95% C.I.)') + 
  xlab("Time to Treatment (Bimonthly)")  + 
  ylim(-ylimb, ylima) + 
  theme(legend.position = "none") + theme(
    axis.text.x = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.x = element_text(size = size_titles), 
    axis.text.y = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.y = element_text(size = size_titles), 
    plot.title = element_text(size = size_titles, hjust = 0.5)# X-axis title (e.g., "Event Time")
  ) 


final <- (p1 | p2) / (p3)

final

ggsave(final, 
       filename = paste0("../../../results/events/final/process_levels_event.pdf"), 
       device = cairo_pdf, width = 11, height = 8, units = 'in')

#### Rates: 

formal <- funct_events_rate('rate_formal', 'processed_0')

free <- funct_events_rate('rate_free', 'processed_0')

size_titles = 13
size_point = 1
ylimb = .4
ylima = .4

p1 <- ggplot(data = formal, 
             mapping = aes(y = coef, x = time)) +
  geom_hline(yintercept = 0, linetype="solid", color ='darkgrey', 3) +
  geom_point(size = size_point) + 
  geom_vline(xintercept = 0, linetype="solid", color ="darkgrey", 2) +
  geom_errorbar(aes(ymin=ci_low, ymax=ci_up), 
                width=0.5, linetype = 'solid') +
  ggtitle('Pre-trial detention / processed') +
  theme_bw() +
  ylab('Estimated Value (95% C.I.)') + 
  xlab("")  + 
  ylim(-ylimb, ylima) + 
  theme(legend.position = "none") + theme(
    axis.text.x = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.x = element_text(size = size_titles), 
    axis.text.y = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.y = element_text(size = size_titles), 
    plot.title = element_text(size = size_titles, hjust = 0.5)# X-axis title (e.g., "Event Time")
  ) 

p2 <- ggplot(free, 
             mapping = aes(y = coef, x = time)) +
  geom_hline(yintercept = 0, linetype="solid", color ='darkgrey', 3) +
  geom_point(size = size_point) + 
  geom_vline(xintercept = 0, linetype="solid", color ="darkgrey", 2) +
  geom_errorbar(aes(ymin=ci_low, ymax=ci_up), 
                width=0.5, linetype = 'solid') +
  ggtitle('Free / processed') +
  theme_bw() +
  ylab('Estimated Value (95% C.I.)') + 
  xlab("Time to Treatment (Bimonthly)")  + 
  ylim(-ylimb, ylima) + 
  theme(legend.position = "none") + theme(
    axis.text.x = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.x = element_text(size = size_titles), 
    axis.text.y = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.y = element_text(size = size_titles), 
    plot.title = element_text(size = size_titles, hjust = 0.5)# X-axis title (e.g., "Event Time")
  ) 

final <- (p1) / (p2)

final

ggsave(final, 
       filename = paste0("../../../results/events/final/process_rates_event.pdf"), 
       device = cairo_pdf, width = 9, height = 9, units = 'in')
