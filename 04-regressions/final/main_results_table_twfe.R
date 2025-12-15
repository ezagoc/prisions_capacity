###################################################
## Data Analysis: Main results ATT (CSDID)
## Author: Eduardo Zago-Cuevas (all errors are my own)
## Run before: same folder, a number before
## Output: Capacity panel for DiD
##
###################################################

# install.packages('pacman')

pacman::p_load(tidyverse, arrow, purrr, lfe, stargazer)

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
                 n_sentenced1 = n_sentenced,
                 sent_prison1 = sent_prison, 
                 total_processed1 = total_processed, 
                 formal_prision1 = formal_prision, 
                 free1 = free, 
                 only_sent_money1 = only_sent_money, 
                 absolutoria1 = absolutoria,
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
                            ' ~ treat_post | code_inegi + m_time | 0 | code_inegi'))
  twfe <- felm(fmla, data = p)
  
  return(twfe)
}

vars <- c("total_processed1", "formal_prision1", "free1", "n_sentenced1",
          "absolutoria1", "sent_prison1", "sent_intensive", "only_sent_money1")

means_vec <- p %>%
  filter(treat == 0) %>%
  summarise(across(all_of(vars), ~mean(.x, na.rm = T))) %>%
  unlist(use.names = FALSE)

means_vec

coefs_sent <- c("total_processed", "formal_prision", "free", "n_sentenced",
                "absolutoria", "sent_prison", "sent_intensive", "only_sent_money") |> 
  map(~funct_atts(.x))


table <- stargazer(
  coefs_sent, # felm regressions
  
  label = paste0("tab:twfe_table"),
  header = FALSE,
  font.size = "scriptsize",
  dep.var.caption = "",
  dep.var.labels.include = FALSE,
  table.placement = "!htpb",
  column.labels = c(
    "\\shortstack{log Total \\\\ processed}",
    "\\shortstack{log Pre-trial \\\\ detention}",
    "\\shortstack{log Released}",
    "\\shortstack{log Total \\\\ sentenced}",
    "\\shortstack{log Not \\\\ guilty}",
    "\\shortstack{log Guilty \\\\ (prison)}",
    "\\shortstack{Time \\\\ sentenced}",
    "\\shortstack{log Guilty \\\\ (money)}"),
  covariate.labels = 'Treat times post',
  keep = c('treat_post'),
  omit.stat=c("f", "ser","adj.rsq"),
  column.sep.width = "0pt",
  add.lines = list(
    c("Municipality FEs", rep("Yes", 8)),
    c('Time FEs', rep("Yes", 8)), 
    c('Mean of Control (Never Treated)', round(means_vec, 3))),
  title = 'ATT results using TWFE',
  type = "latex")

note.latex <- paste0("\\multicolumn{9}{l} {\\parbox[t]{18cm}{ \\textit{Notes:}
Effect of the contruction of a federal prison on a 300km vicinity of a municipality on different sentencing outcomes.
Column 1 presents the results for the log total processed individuals at pre-trial court. Column 2 and 3 show the results for those processed sent to pre-trial detention and
 those released. Column 4 shows results for the total individuals sentenced, Column 5 forthe ones found not guilty, Column 6 for the oens found guilty and sent to prison 
  Column 7 for the time sentenced to prison, finally Column 8 for the ones sentenced to pay a fine. TWFE is implemented. Standard errors are clustered at the municipality level. * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../../results/tables/twfe_results.tex"))


writexl::write_xlsx(coefs_sent, '../../../results/tables/')

coefs_process <- c('total_processed', 'formal_prision', 'free') |> 
  map_dfr(~funct_atts(.x))