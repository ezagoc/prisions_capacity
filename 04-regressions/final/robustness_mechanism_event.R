###################################################
## Data Processing: capacity treatment generation
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

df <- read_parquet(paste0(pfo, 'panel_capacity.parquet.gzip'))
df <- df |> mutate(perc_overcrowding = total_clean/capacity_clean, 
                   dummy_overcrowding = ifelse(overcrowding>0, 1, 0))

#df <- df |> filter(month %in% c(4, 8, 12))

prison_id <- df |> distinct(prison_id) |> mutate(prison_id_num = row_number())

df <- df |> left_join(prison_id)

treat_post_p = 'btreat_300KM2'
federal = 'federal_p50'
time_period ='quarterly'

time <- df |> distinct(year, month) |> mutate(bim_time = row_number())

df <- df |> left_join(time, by = c('year', 'month'))

df <- df |> mutate(treat_post := .data[[treat_post_p]] * .data[[federal]])

df <- df |> group_by(prison_id) |> 
  mutate(event_time = bim_time[treat_post > 0][1], 
         treat = ifelse(is.na(event_time) == F, 1, 0)) |>
  ungroup()

# Control group to 0
df$event_time[df$treat == 0] <- 0

# Time to Event
df <- df %>% group_by(prison_id) %>% 
  mutate(time_to_event = ifelse(treat == 1, bim_time - event_time, 
                                0), 
         time_to_event_gard = ifelse(treat == 1, bim_time - event_time, 
                                     Inf)) |> ungroup()

df <- df |> mutate(relative_overcrowding = asinh(relative_overcrowding))

cs21 = att_gt(yname = 'relative_overcrowding', 
              tname = "bim_time", idname = "prison_id_num", 
              gname = "event_time", 
              #xformla = as.formula(addonvar2), 
              control_group = "nevertreated", # If Too few groups for "nevertreated" default, change to "notyettreated"
              clustervars = "prison_id_num",
              allow_unbalanced_panel = T,
              data = df)

cs_event <- aggte(MP = cs21, type = "dynamic", min_e = -60, max_e = 60)

agg_att_simple <- aggte(MP = cs21, type = "simple")
call_sant_coef <- agg_att_simple$overall.att
call_sant_se <- agg_att_simple$overall.se

callsant <- tibble(time = cs_event$egt, coef = cs_event$att.egt, 
                   se = cs_event$se.egt)

callsant <- callsant |> mutate(ci_low = coef - se*(qnorm(1-(1-0.95)/2)), 
                               ci_up = coef + se*(qnorm(1-(1-0.95)/2)))

size_titles <- 13
size_point <- 1
callsantplot <- ggplot(data = callsant, 
                       mapping = aes(y = coef, x = time)) +
  geom_hline(yintercept = 0, linetype="solid", color ='darkgrey', 3) +
  geom_point(size = size_point) + 
  geom_vline(xintercept = 0, linetype="solid", color ="darkgrey", 2) +
  geom_errorbar(aes(ymin=ci_low, ymax=ci_up), 
                width=0.5, linetype = 'solid') +
  ggtitle("300 KM") +
  theme_bw() +
  ylab("Estimated Value (95% C.I.)") + 
  xlab("") + 
  ylim(-.6, .4) + 
  theme(legend.position = "none") + theme(
    axis.text.x = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.x = element_text(size = size_titles), 
    axis.text.y = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.y = element_text(size = size_titles), 
    plot.title = element_text(size = size_titles, hjust = 0.5)# X-axis title (e.g., "Event Time")
  )

callsantplot


df <- read_parquet(paste0(pfo, 'panel_capacity.parquet.gzip'))
df <- df |> mutate(perc_overcrowding = total_clean/capacity_clean, 
                   dummy_overcrowding = ifelse(overcrowding>0, 1, 0))

#df <- df |> filter(month %in% c(4, 8, 12))

prison_id <- df |> distinct(prison_id) |> mutate(prison_id_num = row_number())

df <- df |> left_join(prison_id)

treat_post_p = 'btreat_500KM2'
federal = 'federal_p50'
time_period ='quarterly'

time <- df |> distinct(year, month) |> mutate(bim_time = row_number())

df <- df |> left_join(time, by = c('year', 'month'))

df <- df |> mutate(treat_post := .data[[treat_post_p]] * .data[[federal]])

df <- df |> group_by(prison_id) |> 
  mutate(event_time = bim_time[treat_post > 0][1], 
         treat = ifelse(is.na(event_time) == F, 1, 0)) |>
  ungroup()

# Control group to 0
df$event_time[df$treat == 0] <- 0

# Time to Event
df <- df %>% group_by(prison_id) %>% 
  mutate(time_to_event = ifelse(treat == 1, bim_time - event_time, 
                                0), 
         time_to_event_gard = ifelse(treat == 1, bim_time - event_time, 
                                     Inf)) |> ungroup()

df <- df |> mutate(relative_overcrowding = asinh(relative_overcrowding))

cs21 = att_gt(yname = 'relative_overcrowding', 
              tname = "bim_time", idname = "prison_id_num", 
              gname = "event_time", 
              #xformla = as.formula(addonvar2), 
              control_group = "nevertreated", # If Too few groups for "nevertreated" default, change to "notyettreated"
              clustervars = "prison_id_num",
              allow_unbalanced_panel = T,
              data = df)

cs_event <- aggte(MP = cs21, type = "dynamic", min_e = -60, max_e = 60)

agg_att_simple <- aggte(MP = cs21, type = "simple")
call_sant_coef <- agg_att_simple$overall.att
call_sant_se <- agg_att_simple$overall.se

callsant <- tibble(time = cs_event$egt, coef = cs_event$att.egt, 
                   se = cs_event$se.egt)

callsant <- callsant |> mutate(ci_low = coef - se*(qnorm(1-(1-0.95)/2)), 
                               ci_up = coef + se*(qnorm(1-(1-0.95)/2)))

size_titles <- 13
size_point <- 1
callsantplot2 <- ggplot(data = callsant, 
                       mapping = aes(y = coef, x = time)) +
  geom_hline(yintercept = 0, linetype="solid", color ='darkgrey', 3) +
  geom_point(size = size_point) + 
  geom_vline(xintercept = 0, linetype="solid", color ="darkgrey", 2) +
  geom_errorbar(aes(ymin=ci_low, ymax=ci_up), 
                width=0.5, linetype = 'solid') +
  ggtitle("500 KM") +
  theme_bw() +
  ylab("Estimated Value (95% C.I.)") + 
  xlab("Time to Treatment (Bimonthly)") + 
  ylim(-.6, .4) + 
  theme(legend.position = "none") + theme(
    axis.text.x = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.x = element_text(size = size_titles), 
    axis.text.y = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.y = element_text(size = size_titles), 
    plot.title = element_text(size = size_titles, hjust = 0.5)# X-axis title (e.g., "Event Time")
  )



library(patchwork)

final <- (callsantplot) / (callsantplot2)

final

ggsave(final, 
       filename = paste0("../../../results/events/final/relative_overcrowding_event_300_500.pdf"), 
       device = cairo_pdf, width = 8, height = 10, units = 'in')


#####


