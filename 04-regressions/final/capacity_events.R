###################################################
## Data Analysis: capacity treatment analysis DiD
## Author: Eduardo Zago-Cuevas (all errors are my own)
## Run before: same folder, a number before
## Output: Capacity panel for DiD
##
###################################################

# install.packages('pacman')

# install.packages('pacman')

pacman::p_load(tidyverse, arrow, purrr, sf, fixest, didimputation, did, 
               did2s, fect, patchwork)

rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pfo <- '../../../data/03-analysis/'

# Dataset: 

dfini <- read_parquet(paste0(pfo, 'panel_capacity.parquet.gzip'))
dfini <- dfini |> mutate(perc_overcrowding = total_clean/capacity_clean, 
                         dummy_overcrowding = ifelse(overcrowding>0, 1, 0))

prison_id <- dfini |> distinct(prison_id) |> mutate(prison_id_num = row_number())

dfini <- dfini |> left_join(prison_id)

event_general <- function(df, treat_post_p, time_period ='bimonthly', federal, 
                          variable){
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
                                  0), 
           time_to_event_gard = ifelse(treat == 1, bim_time - event_time, 
                                       Inf)) |> ungroup()
  
  df <- df |> mutate(relative_overcrowding = asinh(relative_overcrowding))
  
  ########### TWFE
  fmla <- as.formula(paste0(variable, 
                            ' ~ i(time_to_event, treat, ref = -1) | prison_id + bim_time'))
  reg200k <- feols(fmla, cluster = "prison_id", data = df)
  
  twfe_tibble <- tibble(coef = coef(reg200k), se = se(reg200k), 
                        time = names(coef(reg200k)))
  
  print('TWFE')
  ########## Sun AB
  fmla <- as.formula(paste0(variable, ' ~ sunab(event_time,
                                                 bim_time) ',
                            ' | prison_id + bim_time'))
  reg200k <- feols(fmla, cluster = "prison_id", data = df)
  
  sunab_tibble <- tibble(coef = coef(reg200k), se = se(reg200k), 
                         Type = 'Sun & Abraham (2021)', 
                         time = names(coef(reg200k)))
  
  print('SunAb')
  ########### Borusyak:
  
  event_borus <- did_imputation(data = df, yname = variable, 
                                gname = "event_time",
                                tname = "bim_time", idname = "prison_id",
                                pretrends = T, horizon = T) |> 
    mutate(time = as.numeric(term)) |> 
    rename(coef = estimate, se = std.error) |> select(-c(lhs, term))
  
  print('Borus')
  
  cs21 = att_gt(yname = variable, tname = "bim_time", idname = "prison_id_num", 
                gname = "event_time", 
                #xformla = as.formula(addonvar2), 
                control_group = "nevertreated", # If Too few groups for "nevertreated" default, change to "notyettreated"
                clustervars = "prison_id_num",
                allow_unbalanced_panel = T,
                data = df)
  
  cs_event <- aggte(MP = cs21, type = "dynamic", min_e = -30, max_e = 30)
  
  df_call <- tibble(time = cs_event$egt, coef = cs_event$att.egt, 
                    se = cs_event$se.egt)
  
  
  ## FECT
  
  form<- as.formula(paste0(variable, ' ~ treat_post'))
  out.fect <- fect(form, data = df, index = c("prison_id","bim_time"),
                   method = "fe", force = "two-way", se = TRUE, parallel = TRUE,
                   nboots = 500)
  
  fect_boot <- as_tibble(out.fect$att.boot) %>%
    rename_with(~ paste0("col_", seq_along(.), "_column")) %>%
    rowwise() %>%
    mutate(row_sd = sd(c_across(everything()), na.rm = T)) %>%
    ungroup() |> select(row_sd)
  
  fectout <- tibble(coef = out.fect$att, time = out.fect$time, 
                    se = fect_boot$row_sd)
  
  ## Gardner:
  
  es <- did2s(df,
              yname = variable, first_stage = ~ 0 | prison_id + bim_time,
              second_stage = ~ i(time_to_event_gard, ref = Inf), 
              treatment = "treat_post",
              cluster_var = "prison_id"
  )
  
  gard_tibble <- tibble(coef = coef(es), se = se(es), 
                        time = names(coef(es)))
  
  #return(df |> filter(time_to_event == 56))
  
  return(list(twfe_tibble, sunab_tibble, df_call, 
              event_borus, fectout , gard_tibble))
}


# Event Study Final:
l_dfs <- event_general(dfini, treat_post_p = 'btreat_400KM2', 
                       federal = 'federal_p50',
                       time_period ='quarterly',
                       variable = 'relative_overcrowding')

do_plot <- function(var, size_point = 1, size_titles = 13, time_b = -50, 
                    time_a = 50, ylimb = -1, ylima = 1){
  
  #TWFE
  twfe <- l_dfs[[1]] |> mutate(ci_low = coef - se*(qnorm(1-(1-0.95)/2)), 
                               ci_up = coef + se*(qnorm(1-(1-0.95)/2)), 
                               time = as.numeric(sub(".*::(-?\\d+):.*", "\\1", time))) 
  
  twfeplot <- ggplot(data = twfe |> filter(between(time, time_b, time_a)), 
                     mapping = aes(y = coef, x = time)) +
    geom_hline(yintercept = 0, linetype="solid", color ='darkgrey', 3) +
    geom_point(size = size_point) + 
    geom_vline(xintercept = 0, linetype="solid", color ="darkgrey", 2) +
    geom_errorbar(aes(ymin=ci_low, ymax=ci_up), 
                  width=0.5, linetype = 'solid') +
    ggtitle("Two-Way Fixed Effects") +
    theme_bw() +
    ylab("Estimated Value (95% C.I.)") + 
    xlab("") +
    ylim(-ylimb, ylima) + 
    theme(legend.position = "none") + theme(
      axis.text.x = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
      axis.title.x = element_text(size = size_titles), 
      axis.text.y = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
      axis.title.y = element_text(size = size_titles), 
      plot.title = element_text(size = size_titles, hjust = 0.5)# X-axis title (e.g., "Event Time")
    )
  
  # Sun and Abraham
  sunab <- l_dfs[[2]] |> mutate(ci_low = coef - se*(qnorm(1-(1-0.95)/2)), 
                                ci_up = coef + se*(qnorm(1-(1-0.95)/2)), 
                                time = as.numeric(sub(".*::(-?\\d+)", "\\1", time))) 
  
  sunplot <- ggplot(data = sunab |> filter(between(time, time_b, time_a)), 
                    mapping = aes(y = coef, x = time)) +
    geom_hline(yintercept = 0, linetype="solid", color ='darkgrey', 3) +
    geom_point(size = size_point) + 
    geom_vline(xintercept = 0, linetype="solid", color ="darkgrey", 2) +
    geom_errorbar(aes(ymin=ci_low, ymax=ci_up), 
                  width=0.5, linetype = 'solid') +
    ggtitle("Sun and Abraham (2021)") +
    theme_bw() +
    ylab("") + 
    xlab("") + 
    ylim(-ylimb, ylima) + 
    theme(legend.position = "none") + theme(
      axis.text.x = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
      axis.title.x = element_text(size = size_titles), 
      axis.text.y = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
      axis.title.y = element_text(size = size_titles), 
      plot.title = element_text(size = size_titles, hjust = 0.5)# X-axis title (e.g., "Event Time")
    )
  
  # Callaway & Sant'Anna
  
  callsant <- l_dfs[[3]] |> mutate(ci_low = coef - se*(qnorm(1-(1-0.95)/2)), 
                                   ci_up = coef + se*(qnorm(1-(1-0.95)/2)))
  
  callsantplot <- ggplot(data = callsant |> filter(between(time, time_b, time_a)), 
                         mapping = aes(y = coef, x = time)) +
    geom_hline(yintercept = 0, linetype="solid", color ='darkgrey', 3) +
    geom_point(size = size_point) + 
    geom_vline(xintercept = 0, linetype="solid", color ="darkgrey", 2) +
    geom_errorbar(aes(ymin=ci_low, ymax=ci_up), 
                  width=0.5, linetype = 'solid') +
    ggtitle("Callaway & Sant'Anna (2021)") +
    theme_bw() +
    ylab("") + 
    xlab("") + 
    ylim(-ylimb, ylima) + 
    theme(legend.position = "none") + theme(
      axis.text.x = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
      axis.title.x = element_text(size = size_titles), 
      axis.text.y = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
      axis.title.y = element_text(size = size_titles), 
      plot.title = element_text(size = size_titles, hjust = 0.5)# X-axis title (e.g., "Event Time")
    )
  
  # Gardner
  
  gard <- l_dfs[[6]] |> mutate(ci_low = coef - se*(qnorm(1-(1-0.95)/2)), 
                               ci_up = coef + se*(qnorm(1-(1-0.95)/2)), 
                               time = as.numeric(sub(".*::", "", time)))
  
  gardplot <- ggplot(data = gard |> filter(between(time, time_b, time_a)), 
                     mapping = aes(y = coef, x = time)) +
    geom_hline(yintercept = 0, linetype="solid", color ='darkgrey', 3) +
    geom_point(size = size_point) + 
    geom_vline(xintercept = 0, linetype="solid", color ="darkgrey", 2) +
    geom_errorbar(aes(ymin=ci_low, ymax=ci_up), 
                  width=0.5, linetype = 'solid') +
    ggtitle("Gardner (2021)") +
    theme_bw() +
    ylab("Estimated Value (95% C.I.)") + 
    xlab("Time since Treatment Began") + 
    ylim(-ylimb, ylima) + 
    theme(legend.position = "none") + theme(
      axis.text.x = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
      axis.title.x = element_text(size = size_titles), 
      axis.text.y = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
      axis.title.y = element_text(size = size_titles), 
      plot.title = element_text(size = size_titles, hjust = 0.5)# X-axis title (e.g., "Event Time")
    ) 
  
  # Borusyak
  
  borus <- l_dfs[[4]] |> mutate(ci_low = coef - se*(qnorm(1-(1-0.95)/2)), 
                                ci_up = coef + se*(qnorm(1-(1-0.95)/2)))
  
  borusplot <- ggplot(data = borus |> filter(between(time, time_b, time_a)), 
                      mapping = aes(y = coef, x = time)) +
    geom_hline(yintercept = 0, linetype="solid", color ='darkgrey', 3) +
    geom_point(size = size_point) + 
    geom_vline(xintercept = 0, linetype="solid", color ="darkgrey", 2) +
    geom_errorbar(aes(ymin=ci_low, ymax=ci_up), 
                  width=0.5, linetype = 'solid') +
    ggtitle("Borusyak, Jaravel & Spiess (2021)") +
    theme_bw() +
    ylab("") + 
    xlab("Time since Treatment Began") + 
    ylim(-ylimb, ylima) + 
    theme(legend.position = "none") + theme(
      axis.text.x = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
      axis.title.x = element_text(size = size_titles), 
      axis.text.y = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
      axis.title.y = element_text(size = size_titles), 
      plot.title = element_text(size = size_titles, hjust = 0.5)# X-axis title (e.g., "Event Time")
    ) 
  
  ## FECT: 
  
  # FECT
  
  fec <- l_dfs[[5]] |> mutate(ci_low = coef - se*(qnorm(1-(1-0.95)/2)), 
                              ci_up = coef + se*(qnorm(1-(1-0.95)/2))) 
  
  fecplot <- ggplot(data = fec |> filter(between(time, time_b, time_a)), 
                    mapping = aes(y = coef, x = time)) +
    geom_hline(yintercept = 0, linetype="solid", color ='darkgrey', 3) +
    geom_point(size = size_point) + 
    geom_vline(xintercept = 0, linetype="solid", color ="darkgrey", 2) +
    geom_errorbar(aes(ymin=ci_low, ymax=ci_up), 
                  width=0.5, linetype = 'solid') +
    ggtitle("Liu, Wang and Xiu (2024)") +
    theme_bw() +
    ylab("") + 
    xlab("Time since Treatment Began")  + 
    ylim(-ylimb, ylima) + 
    theme(legend.position = "none") + theme(
      axis.text.x = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
      axis.title.x = element_text(size = size_titles), 
      axis.text.y = element_text(size = size_titles, colour = 'black'),      # X-axis tick labels (numbers)
      axis.title.y = element_text(size = size_titles), 
      plot.title = element_text(size = size_titles, hjust = 0.5)# X-axis title (e.g., "Event Time")
    ) 
  
  final <- (twfeplot | sunplot | callsantplot) / (gardplot | borusplot | fecplot)
  
  ggsave(final, 
         filename = paste0("../../../results/events/final/", 
                           var,
                           "_event.pdf"), 
         device = cairo_pdf, width = 13.24, height = 6.9, units = 'in')
}


do_plot(size_point = 1, size_titles = 13, time_b = -50, 
        time_a = 60, ylimb = 1, ylima = 1, var = 'relative_overcrowding')
