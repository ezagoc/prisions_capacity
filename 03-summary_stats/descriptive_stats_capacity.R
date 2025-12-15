###################################################
## Data Analysis: Descriptive statistics of prison capacity and overcrowding
## Author: Eduardo Zago-Cuevas (all errors are my own)
## Run before: same folder, a number before
## Output: Capacity panel for DiD
##
###################################################

# install.packages('pacman')

pacman::p_load(tidyverse, arrow, purrr)

rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pfo <- '../../data/03-analysis/'

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

dfini <- read_parquet(paste0(pfo, 'panel_capacity.parquet.gzip'))
dfini <- dfini |> mutate(perc_overcrowding = total_clean/capacity_clean, 
                         dummy_overcrowding = ifelse(overcrowding>0, 1, 0))

dfini <- dfini |> select(prison_id, year, month, center_name_clean, 
                         total_clean:relative_overcrowding, perc_overcrowding, 
                         dummy_overcrowding) 

dfinig <- dfini |> group_by(year, month) |> 
  summarise(total = sum(total_clean, na.rm = T), 
            capacity = sum(capacity_clean, na.rm = T))|> filter(year<2013) |> 
  ungroup() |>
  mutate(id_row = row_number())

dfinig <- rbind(dfinig |> select(year, month, id_row, var = capacity) |> 
                  mutate(Variable = 'Capacity'), 
                dfinig |> select(year, month, id_row, var = total) |> 
                  mutate(Variable = 'Population')) |>
  mutate(year_month = paste0(year, '-', month))

f <- ggplot(dfinig, aes(x = id_row, y = log(var), color = Variable)) +
  geom_line(size = .9) +
  geom_point(size = 1.2) + 
  theme_bw() + 
  scale_x_continuous(
    breaks = seq(min(dfinig$id_row), max(dfinig$id_row)+1, by = 8),
    labels = dfinig$year_month[seq(1, max(dfinig$id_row)+1, by = 8)]
  ) +
  scale_color_manual(values = c("Capacity" = "black",
                                "Population" = "grey50")) +
  ylab("Number of inmates / capacity") + 
  xlab("Date") +  
  theme(
    axis.text.x = element_text(size = 10, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.x = element_text(size = 13), 
    axis.text.y = element_text(size = 10, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.y = element_text(size = 13) # X-axis title (e.g., "Event Time")
  )

f

dfinig <- dfini |> group_by(year, month) |> 
  summarise(total = sum(total_clean, na.rm = T), 
            capacity = sum(capacity_clean, na.rm = T))|> filter(year<2013) |> 
  ungroup() |>
  mutate(id_row = row_number()) |> mutate(over_cap = (total/capacity)*100) |>
  mutate(year_month = paste0(year, '-', month))


d <- ggplot(dfinig, aes(x = id_row, y = over_cap)) +
  geom_line(size = .9) +
  geom_point(size = 1.2) + 
  theme_bw() + 
  scale_x_continuous(
    breaks = seq(min(dfinig$id_row), max(dfinig$id_row)+1, by = 8),
    labels = dfinig$year_month[seq(1, max(dfinig$id_row)+1, by = 8)]
  ) +
  ylab("Percentage over-capacity (%)") + 
  xlab("Date") +  
  theme(
    axis.text.x = element_text(size = 10, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.x = element_text(size = 13), 
    axis.text.y = element_text(size = 10, colour = 'black'),      # X-axis tick labels (numbers)
    axis.title.y = element_text(size = 13) # X-axis title (e.g., "Event Time")
  )

d

#### Some statistics: 

# Statistics on distance: 

dfinid <- dfini |> filter(year < 2013) |> 
  group_by(prison_id, year) |>
  summarise(min_dist_to_fed_km2 = min(min_dist_to_fed_km2, na.rm = T)) |>
  ungroup()

median(dfinid$min_dist_to_fed_km2)

dfinid2 <- dfini |> filter(year < 2013) |>
  group_by(prison_id) |>
  summarise(min_dist_to_fed_km2 = min(min_dist_to_fed_km2, na.rm = T)) |>
  ungroup()

median(dfinid2$min_dist_to_fed_km2)


dfinif <- dfini |> filter(year == 2012) |> 
  mutate(perc_federal = (federal_clean/total_clean) * 100) |>
  group_by(prison_id) |>
  summarise(perc_federal = max(perc_federal, na.rm = T)) |>
  ungroup()

dfinif <- dfinif |> filter(perc_federal != -Inf) |> 
  mutate(dummy_0 = as.integer(perc_federal>0), 
         dummy_30 = as.integer(perc_federal>30))

# perc at least one federal prisoner: 
mean(dfinif$dummy_0)

# perc at least 30%: 
mean(dfinif$dummy_30)

# max perc:
max(dfinif$perc_federal)
