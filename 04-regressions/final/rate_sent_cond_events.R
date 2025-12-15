###################################################
## Data Analysis: analysis DiD
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

controls <- read_parquet(paste0(pfo, 'controls.parquet.gzip')) 

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
                          variable){
  df <- df |> select(code_inegi, CVE_ENT, year, month, semester, bimonthly, quarterly,
                     crime_5, sent_prison, condenado)
  if(time_period == 'monthly'){
    df <- df |> rename(actual_time = month)
    pretrend <- c(-80:-1)
    future <- c(0:80)
    ccf <- 80
    ccp <- -80
  }else if(time_period == 'bimonthly'){
    pretrend <- c(-50:-1)
    future <- c(0:50)
    ccf <- 60
    ccp <- -60
    df <- df |> group_by(code_inegi, year, bimonthly) |> 
      summarise(across(c(sent_prison, condenado), ~sum(.x))) |> 
      ungroup() |> rename(actual_time = bimonthly)
  }else if(time_period == 'semester'){
    ccf <- 23
    ccp <- -23
    pretrend <- T
    future <- T
    df <- df |> group_by(code_inegi, year, semester) |> 
      summarise(across(c(sent_prison, condenado), ~sum(.x))) |> 
      ungroup() |> rename(actual_time = semester)
  }else if(time_period == 'yearly'){
    ccf <- 10
    ccp <- -10
    pretrend <- T
    future <- T
    df <- df |> group_by(code_inegi, year) |> 
      summarise(across(c(sent_prison, condenado), ~sum(.x))) |> 
      ungroup() |> mutate(actual_time = 1)
  }else if(time_period == 'quarterly'){
    pretrend <- c(-30:-1)
    future <- c(0:30)
    ccf <- 30
    ccp <- -30
    df <- df |> group_by(code_inegi, year, quarterly) |> 
      summarise(across(c(sent_prison, condenado), ~sum(.x))) |> 
      ungroup() |> mutate(actual_time = quarterly)
  }else{
    df <- df |> rename(actual_time = month)
    print('Choose between: monthly, bimonthly, semester, yearly, quarterly. Code is running at the monthly level')
  }
  
  df <- df |> mutate(condenado_0 = as.integer(condenado == 0),
                     rate_sent_cond = ifelse(condenado != 0, 
                                             sent_prison/condenado, 0)
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
                                  0), 
           time_to_event_gard = ifelse(treat == 1, m_time - event_time, 
                                       Inf)) |> ungroup()
  
  ########### TWFE
  fmla <- as.formula(paste0(variable, 
                            ' ~ i(time_to_event, treat, ref = -1) + condenado_0 | code_inegi + m_time'))
  reg200k <- feols(fmla, cluster = "code_inegi", data = df)
  
  twfe_tibble <- tibble(coef = coef(reg200k), se = se(reg200k), 
                        time = names(coef(reg200k)))
  
  print('TWFE')
  ########## Sun AB
  fmla <- as.formula(paste0(variable, ' ~ sunab(event_time,
                                                 m_time) + condenado_0 ',
                            ' | code_inegi + m_time'))
  reg200k <- feols(fmla, cluster = "code_inegi", data = df)
  
  sunab_tibble <- tibble(coef = coef(reg200k), se = se(reg200k), 
                         Type = 'Sun & Abraham (2021)', 
                         time = names(coef(reg200k)))
  
  print('SunAb')
  ########### Borusyak:
  
  event_borus <- did_imputation(data = df, yname = variable, 
                                gname = "event_time",
                                first_stage = ~ condenado_0 | code_inegi + m_time, 
                                tname = "m_time", idname = "code_inegi",
                                pretrends = pretrend, horizon = future) |> 
    mutate(time = as.numeric(term)) |> 
    rename(coef = estimate, se = std.error) |> select(-c(lhs, term))
  
  print('Borus')
  
  cs21 = att_gt(yname = variable, tname = "m_time", idname = "code_inegi", 
                gname = "event_time", 
                xformla = ~ condenado_0, 
                control_group = "nevertreated", # If Too few groups for "nevertreated" default, change to "notyettreated"
                clustervars = "code_inegi",  
                data = df)
  
  cs_event <- aggte(MP = cs21, type = "dynamic", min_e = ccp, max_e = ccf)
  
  df_call <- tibble(time = cs_event$egt, coef = cs_event$att.egt, 
                    se = cs_event$se.egt)
  
  
  ## FECT
  
  form<- as.formula(paste0(variable, ' ~ treat_post + condenado_0'))
  out.fect <- fect(form, data = df, index = c("code_inegi","m_time"),
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
              yname = variable, first_stage = ~ condenado_0 | code_inegi + m_time,
              second_stage = ~ i(time_to_event_gard, ref = Inf), 
              treatment = "treat_post",
              cluster_var = "code_inegi"
  )
  
  gard_tibble <- tibble(coef = coef(es), se = se(es), 
                        time = names(coef(es)))
  
  #return(df |> filter(time_to_event == 56))
  
  return(list(twfe_tibble, sunab_tibble, df_call, 
              event_borus, fectout , gard_tibble))
}


# Event Study Final:
l_dfs <- event_general(p, treat_post_p = 'btreat_300KM2',
                       time_period ='bimonthly',
                       variable = 'rate_sent_cond')

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
    ylim(-.25, .25) + 
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


do_plot(var = 'rate_sent_cond', size_point = 1, size_titles = 13, time_b = -50, 
        time_a = 60, ylimb = .08, ylima = 0.08)
