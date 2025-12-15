###################################################
## Data Analysis: Linear intensive (margin) interaction filtering to less than 10 total influencers followed
## Author: Eduardo Zago-Cuevas
## Run before: same folder, a number before
## Output: Judicial panel dataset 2009-2012
##
###################################################

# 0.0 Set up the environment, clean it and set working directory to the code path
rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1.0 Import functions and packages
library(purrr)
library(fastDummies)
src_path <- c("../../../../src/utils/")             
source_files <- list(
  "funcs_analysis.R",
  "constants_final.R",
  "import_data.R"
)
map(paste0(src_path, source_files), source)
ipak(packages)
`%!in%` = Negate(`%in%`)

# 2.0 Define constants
country <- 'joint'
data_type <- 'Verifiability'
list_stages <- list('stage1_2','stage3_4', 'stage5_6')
list_types <- list('log_')
file_code <- 'intensive_interactions_filterp95_mayor_0_posts'
ini <- '../../../../data/04-analysis/joint/'
influencer_thr <- 8
n_posts_thr <- 0

# Fixed Effects dummies
fes <- read_parquet('../../../../data/04-analysis/joint/BlocksIntensive/original/intensive_fe.parquet')
int_fes <- paste0(colnames(fes |> select(starts_with('fe_'))), 
                  collapse = ' + ')

# Above p90, p95 dummies
belp90 <- read_parquet('../../../../data/04-analysis/joint/below_p90_p95_divider.parquet') |>
  select(-n_posts_base)


stage <- 'stage3_4'
f <- get_analysis_ver_final_winsor(stage = stage, batches = 'b1b2',
                                   initial_path = '../../../../') |> 
  left_join(belp90, by = c('follower_id', 'batch_id', 'pais')) 

### Filters

f <- f |> filter(below_p95 == 1) |> filter(n_posts_base>n_posts_thr) |> 
  filter(total_influencers < influencer_thr) |> 
  select(follower_id, pais, batch_id, total_treated, 
         total_influencers) 

# Generate interactions

interactions <- generate_interactions(f)

interactions <- interactions |> left_join(fes, 
                                          by = c('follower_id', 'pais', 'batch_id'))

rm(fes)

int_cols <- paste(grep("^tao_", colnames(interactions), value = TRUE),
                  collapse = ' + ')

# Prepare baseline divided and dummy for missing: 

at <- read_parquet('../Attrition/attrition.parquet') |> mutate(batch_id = 'b2')

baseline_div <- read_parquet('../../../../data/04-analysis/KE/baseline/baseline_divided_b1b2.parquet') |>
  mutate(pais = 'KE')

baseline_div2 <- read_parquet('../../../../data/04-analysis/SA/baseline/baseline_divided_b1b2.parquet') |>
  mutate(pais = 'SA')

baseline_div = rbind(baseline_div, baseline_div2) |> 
  select(follower_id, username, pais, batch_id, 
         total_shares_base_first:n_posts_no_rt_base_third) 

baseline_div1 <- read_parquet('../../../../data/04-analysis/KE/baseline/baseline_english_divided_b1b2.parquet') |>
  mutate(pais = 'KE')

baseline_div12 <- read_parquet('../../../../data/04-analysis/SA/baseline/baseline_english_divided_b1b2.parquet') |>
  mutate(pais = 'SA')

baseline_div1 = rbind(baseline_div1, baseline_div12)

baseline_div = baseline_div |> left_join(at, by = c('username', 'pais', 'batch_id'))

baseline_div <- baseline_div |> 
  mutate(dummy_second = ifelse(is.na(dummy_attrition) == F & 
                                 pais == 'SA' & dummy_attrition == 1, 1, 0), 
         dummy_third = ifelse(is.na(dummy_attrition) == F & 
                                dummy_attrition == 1, 1, 0))

baseline_div <- baseline_div |>
  left_join(baseline_div1 |> select(follower_id, pais, batch_id, 
                                    eng_base_first:n_eng_no_rt_base_third), 
            by = c('follower_id', 'pais', 'batch_id'))
baseline_div <- baseline_div |>
  mutate(across(contains("_base"), ~ log(as.numeric(.x) + 1))) %>%
  rename_with(~ paste0('log_', .x), contains("_base"))

for (stage in list_stages){
  
  # 3.0 Import data and manipulate
  df <- get_analysis_ver_final_winsor(stage = stage, batches = 'b1b2',
                                      initial_path = '../../../../')  |> 
    left_join(belp90, by = c('follower_id', 'batch_id', 'pais')) |>
    filter(below_p95 == 1) |>
    filter(n_posts_base>n_posts_thr) |> filter(total_influencers<influencer_thr)
  
  f <- get_analysis_english_winsor(stage = stage, batches = 'b1b2',
                                   initial_path = '../../../../') |> 
    select(follower_id, pais, batch_id, eng, eng_base, 
           log_eng, log_eng_base, log_eng_no_rt, log_eng_no_rt_base)
  
  df <- df |> left_join(f, by = c('follower_id', 'pais', 'batch_id')) 
  
  # Fix baseline
  
  df <- df |> left_join(baseline_div, by = c('follower_id', 'pais', 'batch_id')) 
  
  df <- df |> select(follower_id, pais, batch_id, total_influencers, total_treated, 
                     dummy_second, dummy_third,
                     starts_with(paste0('log_', aux_t_posts))) |> 
    left_join(interactions, by = c('follower_id', 'pais', 'batch_id'))
  
  for (type in list_types){
    aux <- paste0(type, aux_t_posts)
    
    # 4.0 Run original estimates
    aux_data <- df[aux]
    lm_list_ols <- list()
    count <- 1
    for (x in aux) {
      fmla1 <- as.formula(paste0(x, "~ total_treated + ",
                                 x, "_base_first + ",
                                 x, "_base_second + ",
                                 x, "_base_third + dummy_second + dummy_third + ", 
                                 int_fes, ' + ', int_cols, 
                                 "| total_influencers + pais + batch_id"))
      nam1 <- paste("lm_", count, "_ols", sep = "")
      assign(nam1, feols(fmla1, data = df))
      coefs <- data.frame(coeftable(get(nam1, envir = globalenv()))) |> 
        select(Estimate)
      names(coefs) <- paste0(x)
      coefs <- cbind('treatment' = rownames(coefs), coefs) |>
        filter(treatment == 'total_treated')
      rownames(coefs) <- 1:nrow(coefs)
      lm_list_ols[[count]] <- coefs
      count <- count + 1
    }
    coefs_all <- lm_list_ols %>% 
      reduce(left_join, by = "treatment")
    
    # Build matrix
    ver <- coefs_all %>% 
      select(ends_with(aux[1]))
    ver <- ver[1,]
    
    non_ver <- coefs_all %>% 
      select(ends_with(aux[2]))
    non_ver <- non_ver[1,]
    
    true <- coefs_all %>% 
      select(ends_with(aux[3]))
    true <- true[1,]
    
    fake <- coefs_all %>% 
      select(ends_with(aux[4]))
    fake <- fake[1,]
    
    n_posts <- coefs_all %>% 
      select(ends_with(aux[5]))
    n_posts <- n_posts[1,]
    
    coefs_perm <- data.frame(ver, non_ver, true, fake, n_posts)
    
    write_xlsx(
      coefs_perm, paste0("../../../../data/04-analysis/",country,
                         "/",stage,"/original/", data_type, '/', type, file_code,
                         ".xlsx"))
    ### 5.0 Run 1000 Permutations: 
    
    i <- 1
    coefs_fin <- tibble()
    for (m in 1:1000){
      print(i)
      followers <- read_parquet(paste0("../../../../data/04-analysis/joint/",
                                       'small_ties_b1b2p', "/small_tie", 
                                       i,".parquet"))
      
      data <- df
      
      data <- data |> select(-c(starts_with('tao_')))
      
      c1 = paste0("n_influencers_followed_control_no_weak_tie_p", i)
      c2 = paste0("n_influencers_followed_treatment_no_weak_tie_p", i)
      c3 = paste0("n_influencers_followed_treatment_weak_tie_p", i)
      c4 = paste0("n_influencers_followed_control_weak_tie_p", i)
      c5 = paste0("n_influencers_followed_control_strong_tie_p", i)
      c6 = paste0("n_influencers_followed_treatment_strong_tie_p", i)
      c7 = paste0("n_influencers_followed_control_no_strong_tie_p", i)
      c8 = paste0("n_influencers_followed_treatment_no_strong_tie_p", i)
      c9 = paste0("n_influencers_followed_control_p", i)
      c10 = paste0("n_influencers_followed_treatment_p", i)
      c11 = paste0("n_influencers_followed_p_", i)
      
      followers_iter <- followers %>% select(follower_id, c1, c2, c3, c4, 
                                             c5, c6, c7, c8, c9, c10, c11, pais, 
                                             batch_id) 
      
      data <- left_join(
        data, 
        followers_iter,
        by = c('follower_id', 'batch_id', 'pais')
      )
      # Pool treatment variables
      data <- poolTreatmentBalance2(data, c10, c11)
      
      interactions_perms <- generate_interactions(data)
      
      int_cols2 <- paste(grep("^tao_", colnames(interactions_perms), value = TRUE),
                         collapse = ' + ')
      
      data <- data |> left_join(interactions_perms, 
                                by = c('follower_id', 'pais', 'batch_id'))
      
      # Balance tables 
      aux_data <- data[aux]
      coefs_list <- list()
      lm_list_ols <- list()
      count <- 1
      for (au in aux) {
        fmla1 <- as.formula(paste0(au, "~ total_treated + ",
                                   au, "_base_first + ",
                                   au, "_base_second + ",
                                   au, "_base_third + dummy_second + dummy_third + ", 
                                   int_fes, ' + ', int_cols2,
                                   "| total_influencers"))
        nam1 <- paste("lm_", count, "_ols", sep = "")
        assign(nam1, feols(fmla1, data = data))
        coefs <- data.frame(coeftable(get(nam1, envir = globalenv()))) |> 
          select(Estimate)
        names(coefs) <- paste0(au)
        coefs <- cbind('treatment' = rownames(coefs), coefs) |> 
          filter(treatment == 'total_treated')
        rownames(coefs) <- 1:nrow(coefs)
        lm_list_ols[[count]] <- coefs
        count <- count + 1
      }
      coefs_list <- append(coefs_list, lm_list_ols)
      coefs_all <- coefs_list %>% reduce(left_join, by = "treatment")
      
      # Build matrix
      ver <- coefs_all %>% 
        select(ends_with(aux[1]))
      ver <- ver[1,]
      
      non_ver <- coefs_all %>% 
        select(ends_with(aux[2]))
      non_ver <- non_ver[1,]
      
      true <- coefs_all %>% 
        select(ends_with(aux[3]))
      true <- true[1,]
      
      fake <- coefs_all %>% 
        select(ends_with(aux[4]))
      fake <- fake[1,]
      
      n_posts <- coefs_all %>% 
        select(ends_with(aux[5]))
      n_posts <- n_posts[1,]
      
      coefs_perm <- data.frame(ver, non_ver, true, fake, n_posts)
      
      coefs_fin <- rbind(coefs_fin, coefs_perm)
      
      write_xlsx(coefs_fin, paste0("../../../../data/04-analysis/joint/", stage, 
                                   "/permutations/", data_type, '/', type, file_code,
                                   ".xlsx"))
      
      i <- i + 1}
    print(type)
    # write_xlsx(coefs_fin, paste0("../../../../data/04-analysis/joint/", stage, 
    #                              "/permutations/", data_type, '/', type, file_code,
    #                              ".xlsx"))
  }
  print(stage)
}

# 6.0 Make the graphs


ini <- '../../../../data/04-analysis/joint/'

proc_coefs <- function(stage, file){
  coef <- readxl::read_excel(paste0(ini, stage, '/original/', data_type, '/',
                                    file, '.xlsx')) |>
    pivot_longer(cols = everything(), names_to = 'var', values_to = 'coef') |> 
    mutate(stage = stage)
  return(coef)
}

proc_ses <- function(stage, file){
  
  perm <- readxl::read_excel(paste0(ini, stage, '/permutations/', data_type, '/',
                                    file, '.xlsx')) |> 
    summarise(across(everything(), ~sd(.x))) |> 
    pivot_longer(cols = everything(), names_to = 'var', values_to = 'sd') |> 
    mutate(stage = stage)
  
  return(perm)
}

for (type in list_types){
  file_coefs <- paste0(type, file_code)
  coefs <- c('stage5_6', 'stage3_4', 'stage1_2') |> 
    map_dfr(~proc_coefs(.x, file_coefs))
  ses <- c('stage5_6', 'stage3_4', 'stage1_2') |> 
    map_dfr(~proc_ses(.x, file_coefs))
  
  final <- coefs |> left_join(ses, by = c('stage', 'var')) 
  
  if (type == 'log_'){
    addon <- 'log '
  } else if(type == 'arc_'){
    addon <- 'arcsinh '
  }else {
    addon <- ''
  }
  
  final <- final |> 
    mutate(Variable = case_when(var == 'ver' ~ paste0(addon, 'Verifiable Posts + Shares'),
                                var == 'non_ver' ~ paste0(addon, 
                                                          'Non Verifiable Posts + Shares'),
                                var == 'true' ~ paste0(addon, 'True Posts + Shares'),
                                var == 'fake' ~ paste0(addon, 'Fake Posts + Shares'),
                                var == 'n_posts' ~ paste0(addon, 'Number of Posts + Shares (English)')), 
           Stage = case_when(stage == 'stage1_2' ~ 'Weeks 1-4',
                             stage == 'stage3_4' ~ 'Weeks 5-8',
                             stage == 'stage5_6' ~ 'Weeks 9-12'))
  
  
  final$Variable <- factor(final$Variable, levels = c(paste0(addon, 'Number of Posts + Shares (English)'),
                                                      paste0(addon, 
                                                             'Non Verifiable Posts + Shares'), 
                                                      paste0(addon, 'Verifiable Posts + Shares'), 
                                                      paste0(addon, 'True Posts + Shares'), 
                                                      paste0(addon, 'Fake Posts + Shares')))
  
  writexl::write_xlsx(final, paste0(ini, 'EstimatesFinal/',type,
                                    'verifiability_intensive_interactions9.xlsx'))
  
  results_plot <- ggplot(data = final, aes(x = factor(Stage), y = coef)) + 
    geom_point(aes(shape = factor(Variable), color = factor(Variable)), size = 3, 
               position = position_dodge(width = 0.5)) +
    geom_linerange(aes(ymin = coef - 1.96 * sd, ymax = coef + 1.96 * sd, 
                       color = factor(Variable)),
                   position = position_dodge(width = 0.5), size = 1) +
    scale_shape_manual(values = c(15, 16, 17, 4, 7), name = 'Outcome') +
    scale_color_manual(values = rep('black', 5), name = 'Outcome') +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = .5) +  # Set custom fill colors for points # Set custom line colors for error bars
    theme_bw() +  
    ylab("Total Treated Estimate with 95% Confidence Interval") + 
    xlab("Stage") +  # Change title color
    #ggtitle("Dynamic Effects of the Intervention: Verifiability Analysis") +
    theme(panel.grid.major = element_line(color = "gray", linetype = "dashed", size = 0.5),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
  # # if (addon == 'log '){
  #   results_plot <- results_plot + ylim(-.2, .2)
  # }else{
  #   results_plot <- results_plot
  # }
  ggsave(results_plot, 
         filename = paste0('../../../../results/01-regression_graphs/',
                           data_type, '/', type, file_code, '.pdf'), 
         device = cairo_pdf, width = 8.22, height = 6.59, units = 'in')
}

results_plot
