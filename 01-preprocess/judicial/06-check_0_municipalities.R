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

pfo <- '../../../data/03-analysis/'

# Dataset: 
# Judicial data

p1 <- read_parquet('../../../data/01-judicial/00-sentencing/final/panel_comun_1997_2008.parquet.gzip') |>
  filter(year > 1999)

p2 <- read_parquet('../../../data/01-judicial/00-sentencing/final/panel_comun_2009_2012.parquet.gzip')

p <- bind_rows(p1, p2)

## Check municipalities that always have 0s in processed:

p_p <- p |> group_by(code_inegi) |> 
  summarise(total_processed = sum(total_processed, na.rm = T), 
            total_sentenced = sum(n_sentenced, na.rm = T), 
            n_appear = n()) |>
  ungroup()

p_p <- p_p |> mutate(ratio_appear = log(total_processed/n_appear))

p_p_p <- p_p |> filter(ratio_appear >1)

ggplot(p_p, aes(x = ratio_appear)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Histogram of Your Variable",
       x = "Value",
       y = "Frequency") +
  theme_minimal()
