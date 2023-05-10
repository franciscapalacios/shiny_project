import.df <- function(){
  df <- read.csv('/Users/franciscapalacios/Downloads/HR_Analytics.csv')
  df <- df %>% 
    mutate_if(is.character,as.factor) 
  
  df$BusinessTravel <- factor(df$BusinessTravel, ordered = TRUE, 
                              levels = c("Non-Travel", "Travel_Rarely", "Travel_Frequently"))
  
  df <- df %>% 
    mutate(bin.YearsAtCompany = case_when(
      YearsAtCompany <= 1 ~ 1,
      YearsAtCompany <= 4 ~ 2,
      YearsAtCompany <= 10 ~ 3,
      TRUE ~ 4
    )) %>% 
    mutate(num.BusinessTravel = case_when(
      BusinessTravel == "Non-Travel" ~ 1,
      BusinessTravel == "Travel_Rarely" ~ 2,
      TRUE ~ 3
    )) 
  
  return(df)
}

import.corr.mat <- function(df){
  cor.mat <- df %>%
  select(Age, DailyRate, DistanceFromHome, Education, 
         EnvironmentSatisfaction, HourlyRate, JobInvolvement, JobLevel, 
         JobSatisfaction, MonthlyIncome, MonthlyRate, 
         NumCompaniesWorked, PercentSalaryHike, PerformanceRating, 
         RelationshipSatisfaction, StockOptionLevel, TotalWorkingYears, 
         TrainingTimesLastYear, WorkLifeBalance, YearsAtCompany, YearsInCurrentRole, 
         YearsSinceLastPromotion, YearsWithCurrManager) 

  df.cor = cor(cor.mat)
  return(df.cor)
}

import.corr.funnel <- function(df){
  corr.tbl.bin <- df %>%
    select(-EmployeeNumber) %>%
    binarize(n_bins = 4, thresh_infreq = 0.01, name_infreq = "OTHER", one_hot = TRUE)
  
  corr.tbl <- corr.tbl.bin %>%
    correlate(Attrition__Yes)
  
  return(corr.tbl)
}


import.gower.dist <- function(num.var, corr.tbl){
  
  var_selection <- sapply(as.list(corr.tbl[1]), unique)[1:num.var]
  gower_dist <- daisy(df %>%
                        select(one_of(var_selection)), metric = c("gower"))
  
  return(gower_dist)
}


import.sil.tbl <- function(gower_dist){
  
  sil_width <- purrr::map_dbl(2:10, function(k){
    model <- cluster::pam(gower_dist, k = k)
    model$silinfo$avg.width
  })
  
  sil_tbl <- tibble::tibble(
    k = 2:10,
    sil_width = sil_width
  )
  
  return(sil_tbl)
}

import.best.k <- function(sil_tbl, gower_dist){
  
  max_sil = max(sil_tbl$sil_width)
  k = sil_tbl[which(sil_tbl$sil_width == max_sil), ]$k
  
  pam_fit <- cluster::pam(gower_dist, diss = TRUE, k)
  
  hr_subset_tbl <- df %>%
    dplyr::mutate(cluster = pam_fit$clustering) 
  
  attrition_rate_tbl <- hr_subset_tbl %>%
    
    dplyr::select(cluster, Attrition) %>%
    dplyr::mutate(attrition_num = forcats::fct_relevel(Attrition, "No", "Yes") %>% base::as.numeric() - 1) %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(
      Cluster_Turnover_Rate  = (base::sum(attrition_num) / base::length(attrition_num)) %>% scales::percent(accuracy = 0.1),
      Turnover_Count = base::sum(attrition_num),
      Cluster_Size = base::length(attrition_num)
    ) %>%
    dplyr::ungroup() %>%
    
    dplyr::mutate(Population_Turnover_Rate = (Turnover_Count / base::sum(Turnover_Count)) %>% scales::percent(accuracy = 0.1))
  
  return(attrition_rate_tbl)
}
# Variables

df <- import.df()
corr.tbl <- import.corr.funnel(df)
df.cor <- import.corr.mat(df)

#attrition_rate_tbl <- import.best.k(sil_tbl)


a <- import.best.k(import.sil.tbl(import.gower.dist(10, corr.tbl)),
              import.gower.dist(10, corr.tbl))
t(as.matrix(a))

