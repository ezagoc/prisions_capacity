###################################################
## Data Analysis: capacity treatment analysis DiD
## Author: Eduardo Zago-Cuevas (all errors are my own)
## Run before: same folder, a number before
## Output: Capacity panel for DiD
##
###################################################

# install.packages('pacman')

pacman::p_load(tidyverse, arrow, purrr, sf, fixest, didimputation, did)

rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pfo <- '../../data/03-analysis/'

# Dataset: 

dfini <- read_parquet(paste0(pfo, 'panel_capacity.parquet.gzip'))
dfini <- dfini |> mutate(perc_overcrowding = total_clean/capacity_clean, 
                   dummy_overcrowding = ifelse(overcrowding>0, 1, 0))

########################
# Function to Run Event Studies
#######################

event_general <- function(df, treat_post_p, federal, time_period ='bimonthly'){
  if(time_period == 'bimonthly'){
    df <- df
  }else if(time_period == 'semester'){
    df <- df |> filter(month %in% c(2, 8))
  }else if(time_period == 'yearly'){
    df <- df |> filter(month %in% c(2))
  }else if(time_period == 'quarterly'){
    df <- df |> filter(month %in% c(4, 8, 12))
  }else{
    df <- df
    print('Choose between: bimonthly, semester, yearly, quarterly. Code is running at the bimonthly level')
  }
  
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
                                  0)) |> ungroup()
  
  df <- df |> mutate(relative_overcrowding = asinh(relative_overcrowding))
  
  ########### TWFE
  form<- as.formula(paste0('relative_overcrowding ~ i(time_to_event, treat, ref = -1) | prison_id + bim_time'))
  reg200k <- feols(form, 
                   cluster = "prison_id", data = df)
  
  pdf(paste0("../../results/events/test/twfe_relative_", treat_post_p, "_",
             federal, '_', 
             time_period, ".pdf"))
  iplot(reg200k)
  dev.off()
  
  reg200k <- feols(dummy_overcrowding ~ i(time_to_event, 
                                          treat, ref = -1) | 
                     prison_id + bim_time, 
                   cluster = "prison_id", data = df)
  
  pdf(paste0("../../results/events/test/twfe_dummy_", treat_post_p, "_",
             federal, '_', 
             time_period, ".pdf"))
  iplot(reg200k)
  dev.off()
  
  ########### Sun AB
  form<- as.formula(paste0('relative_overcrowding ~ sunab(event_time, bim_time) | prison_id + bim_time'))
  reg200k <- feols(form, cluster = "prison_id", data = df)
  
  pdf(paste0("../../results/events/test/sunab_relative_", treat_post_p, "_",
             federal, '_', 
             time_period,".pdf"))
  iplot(reg200k)
  dev.off()
  
  reg200k <- feols(dummy_overcrowding ~ sunab(event_time, 
                                              time_to_event, 
                                              ref.p = c(-1, .F)) | 
                     prison_id + bim_time, 
                   cluster = "prison_id", data = df)
  
  pdf(paste0("../../results/events/test/sunab_dummy_", treat_post_p, "_",
             federal, '_', 
             time_period, ".pdf"))
  iplot(reg200k)
  dev.off()
  
  ########### Borusyak:
  
  borus <- did_imputation(data = df, yname = "relative_overcrowding", 
                          gname = "event_time",
                          first_stage = ~ 0 | prison_id + bim_time,
                          tname = "bim_time", idname = "prison_id") |> 
    mutate(type = "Borusyak et al.") |> 
    select(type, coef = estimate, se = std.error, conf.low, conf.high)
  
  event_borus <- did_imputation(data = df, yname = "relative_overcrowding", 
                                gname = "event_time",
                                first_stage = ~ 0 | prison_id + bim_time,
                                tname = "bim_time", idname = "prison_id",
                                pretrends = T, horizon = T) |> 
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
    ggtitle('Relative Overcrowding') 
    #scale_color_manual(name = "Periodo", values= colors) +
    #theme(legend.position = "none") 
  ggsave(borus_plot, 
         filename = paste0('../../results/events/test/borus_relative_', 
                           treat_post_p, '_', 
                           federal, '_', 
                           time_period, '.pdf'), 
         device = cairo_pdf, width = 9.22, height = 7, units = 'in')
  
  ### Dummy Overcrowding
  
  borus <- did_imputation(data = df, yname = "dummy_overcrowding", 
                          gname = "event_time",
                          first_stage = ~ 0 | prison_id + bim_time,
                          tname = "bim_time", idname = "prison_id") |> 
    mutate(type = "Borusyak et al.") |> 
    select(type, coef = estimate, se = std.error, conf.low, conf.high)
  
  event_borus <- did_imputation(data = df, yname = "dummy_overcrowding", 
                                gname = "event_time",
                                first_stage = ~ 0 | prison_id + bim_time,
                                tname = "bim_time", idname = "prison_id",
                                pretrends = T, horizon = T) |> 
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
    geom_errorbar(aes(ymin=conf.low, ymax=conf.high), 
                  width=0.2) +
    geom_hline(yintercept = 0, linetype="solid", color ="grey", 2) +
    geom_vline(xintercept = 0, linetype="dashed", color ="red", 2) +
    theme_bw() +
    ylab("Estimated Value (95% IC)") + 
    xlab("Time to Event") +
    ggtitle('Dummy Overcrowding')
    #scale_color_manual(name = "Periodo", values= colors) +
    #theme(legend.position = "none") 
  ggsave(borus_plot, 
         filename = paste0('../../results/events/test/borus_dummy_', 
                           treat_post_p, '_', federal, '_', 
                           time_period, '.pdf'), 
         device = cairo_pdf, width = 9.22, height = 7, units = 'in')
}


event_general(df = dfini, treat_post_p = 'btreat_400KM', federal = 'federal_p50', 
              time_period = 'semester')

event_general(df = dfini, treat_post_p = 'btreat_500KM', federal = 'federal_p50', 
              time_period = 'semester')

event_general(df = dfini, treat_post_p = 'btreat_500KM', federal = 'federal_1', 
              time_period = 'semester')

### Run these: 

event_general(df = dfini, treat_post_p = 'btreat_400KM2', federal = 'federal_p50', 
              time_period = 'quarterly')

event_general(df = dfini, treat_post_p = 'btreat_500KM2', federal = 'federal_p50', 
              time_period = 'semester')

event_general(df = dfini, treat_post_p = 'btreat_300KM2', federal = 'federal_p50', 
              time_period = 'quarterly')

event_general(df = dfini, treat_post_p = 'btreat_200KM2', federal = 'federal_p50', 
              time_period = 'semester')

event_general(df = dfini, treat_post_p = 'treat_post_state', federal = 'federal_p50', 
              time_period = 'semester')

event_general(df = dfini, treat_post_p = 'treat_post_state', federal = 'federal_1', 
              time_period = 'semester')

event_general(df = dfini, treat_post_p = 'treat_post_region', federal = 'federal_p50', 
              time_period = 'semester')

event_general(df = dfini, treat_post_p = 'treat_post_region', federal = 'federal_25p', 
              time_period = 'semester')
