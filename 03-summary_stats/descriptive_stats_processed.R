###################################################
## Data Analysis
## Author: Eduardo Zago-Cuevas (all errors are my own)
## Run before: same folder, a number before
## Output: Judicial panel dataset 2009-2012
##
###################################################

# install.packages('pacman')

pacman::p_load(tidyverse, arrow, foreign, purrr, sf)

rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Paths:
# In
pf <- '../../data/01-judicial/00-sentencing/raw/'

# Out
pfo <- '../../data/01-judicial/00-sentencing/final/'
# Datasets used across all years

preprocess_judicial <- function(year){
  # Read all files
  if (year < 2003){
    inicio <- paste0('judiciales_bd_catalogos_', year, 
                     '_dbf/judiciales_bd_catalogos_', year)
  }else{
    inicio <- paste0('Judiciales_BD_Catalogos_', year, '_dbf')
  }
  preg <- read.dbf(paste0(pf, inicio, 
                          '/TablasMicrodatos_', year, '/preg', year, 
                          '.DBF'), as.is = T) # Registry: Processed
  sreg <- read.dbf(paste0(pf, inicio,
                          '/TablasMicrodatos_', year, '/sreg', year, 
                          '.DBF'), as.is = T) # Registry: Sentenced 
  
  preg <- preg |> mutate(date_auto = as.Date(B_FAUTO), 
                         day = day(date_auto),
                         month = month(date_auto),
                         year = year(date_auto), 
                         month_year = paste0(year, '-', month), 
                         federal = ifelse(B_CVEESTAD == 42, 1, 0))
  
  sreg <- sreg |> mutate(date_auto = as.Date(B_FSENTEN), 
                         day = day(date_auto),
                         month = month(date_auto),
                         year = year(date_auto), 
                         month_year = paste0(year, '-', month), 
                         federal = ifelse(B_CVEESTAD == 52, 1, 0))
  
  final <- tibble(month = c(1:12)) |> mutate(year = year)
  
  pregf <- preg |> filter(federal == 1) |> group_by(year, month) |> 
    summarise(n_processed_f = n()) |> ungroup()
  
  pregs <- preg |> filter(federal == 0) |> group_by(year, month) |> 
    summarise(n_processed_s = n()) |> ungroup()
  
  sregf <- sreg |> filter(federal == 1) |> group_by(year, month) |> 
    summarise(n_sentenced_f = n()) |> ungroup()
  
  sregs <- sreg |> filter(federal == 0) |> group_by(year, month) |> 
    summarise(n_sentenced_s = n()) |> ungroup()
  
  final <- final |> left_join(pregf, by = c('month', 'year')) |> 
    left_join(pregs, by = c('month', 'year')) |> 
    left_join(sregf, by = c('month', 'year')) |> 
    left_join(sregs, by = c('month', 'year'))
  
  return(final)
}

dfall <- c(1998:2012) |>
  map_dfr(~preprocess_judicial(.x))

dfall <- dfall |> mutate(id_row = row_number(), 
                         month_year = paste0(year, '-', month))

# dfall <- dfall |> filter(!month %in% c(12, 1))

f <- ggplot(dfall, aes(x = id_row, y = n_processed_f)) +
  geom_line(size = .6) +
  geom_point(size = 1) + 
  theme_bw() + 
  scale_x_continuous(
    breaks = seq(min(dfall$id_row), max(dfall$id_row), by = 12),
    labels = dfall$month_year[seq(1, max(dfall$id_row), by = 12)]
  ) +
  ylab("Federal") + 
  xlab("") +
  geom_vline(xintercept=109, color="black", linetype="dotted", 
             size = 1) +
  geom_text(aes(x = 86, y = 4000), 
            label = "Start Calderon's tenure", color = "black", 
            size = 4, alpha = 1) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, 
              color = "black") +
  theme(
    axis.text.x = element_text(size = 10, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.x = element_text(size = 13), 
    axis.text.y = element_text(size = 10, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.y = element_text(size = 13) # X-axis title (e.g., "Event Time")
  )
f


d <- ggplot(dfall, aes(x = id_row, y = n_processed_s)) +
  geom_line(size = .6) +
  geom_point(size = 1) + 
  theme_bw() + 
  scale_x_continuous(
    breaks = seq(min(dfall$id_row), max(dfall$id_row), by = 12),
    labels = dfall$month_year[seq(1, max(dfall$id_row), by = 12)]
  ) +
  geom_vline(xintercept=109, color="black", linetype="dotted", 
             size = 1) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, 
              color = "black") +
  ylab("Local (State)") + 
  xlab("Date") +  
  theme(
    axis.text.x = element_text(size = 10, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.x = element_text(size = 13), 
    axis.text.y = element_text(size = 10, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.y = element_text(size = 13) # X-axis title (e.g., "Event Time")
  )
d

library(patchwork)

final <- (f) / (d)


final

ggsave(final, 
       filename = paste0("../../results/summary_stats/total_processed.pdf"), 
       device = cairo_pdf, width = 9, height = 9, units = 'in')
