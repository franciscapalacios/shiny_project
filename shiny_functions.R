library('tidyverse')
library("GGally")
library(corrplot)
library(cluster) 
library(correlationfunnel)
library(ggridges)
library("factoextra")

import.df <- function(){
  df <- read.csv('./HR_Analytics.csv')
  df <- df %>% 
    select(-c(EmployeeNumber, EmployeeCount, Over18, StandardHours)) %>%
    mutate_if(is.character,as.factor) 
  
  df$BusinessTravel <- factor(df$BusinessTravel, ordered = TRUE, 
                              levels = c("Non-Travel", "Travel_Rarely", "Travel_Frequently"))
  
  df$JobLevel <- factor(df$JobLevel, ordered = TRUE)
  df$PerformanceRating <- factor(df$PerformanceRating, ordered = TRUE)
  df$WorkLifeBalance <- factor(df$WorkLifeBalance, ordered = TRUE)
  df$StockOptionLevel <- factor(df$StockOptionLevel, ordered = TRUE)
  df$RelationshipSatisfaction <- factor(df$RelationshipSatisfaction, ordered = TRUE)
  df$JobSatisfaction <- factor(df$JobSatisfaction, ordered = TRUE)
  df$Education <- factor(df$Education, ordered = TRUE)
  df$EnvironmentSatisfaction <- factor(df$EnvironmentSatisfaction, ordered = TRUE)
  df$JobInvolvement <- factor(df$JobInvolvement, ordered = TRUE)
  df$TrainingTimesLastYear <- factor(df$TrainingTimesLastYear, ordered = TRUE)
  
  df <- df %>% 
    mutate(bin.YearsAtCompany = case_when(
      YearsAtCompany <= 1 ~ 1,
      YearsAtCompany <= 4 ~ 2,
      YearsAtCompany <= 10 ~ 3,
      TRUE ~ 4
    )) 
  
  return(df)
}


import.corr.mat <- function(df){
  
  cor.mat <- df %>%
    mutate_at(vars(JobLevel, 
                   PerformanceRating, WorkLifeBalance, StockOptionLevel, 
                   RelationshipSatisfaction, JobSatisfaction, JobInvolvement, 
                   Education, EnvironmentSatisfaction, TrainingTimesLastYear),
              as.numeric) %>%
    
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
  
  sil_width <- map_dbl(2:10, function(k){
    model <- pam(gower_dist, k = k)
    model$silinfo$avg.width
  })
  
  sil_tbl <- tibble(
    k = 2:10,
    sil_width = sil_width
  )
  
  return(sil_tbl)
}


import.cluster <- function(sil_tbl, gower_dist){
  
  max_sil = max(sil_tbl$sil_width)
  k = sil_tbl[which(sil_tbl$sil_width == max_sil), ]$k
  
  pam_fit <- pam(gower_dist, diss = TRUE, k)
  
  return(pam_fit)
}


import.best.k <- function(pam_fit){
  
  cluster_tbl <- df %>%
    mutate(cluster = pam_fit$clustering) 
  
  attrition_rate_tbl <- cluster_tbl %>%
    
    select(cluster, Attrition) %>%
    mutate(attrition_num = forcats::fct_relevel(Attrition, "No", "Yes") %>% base::as.numeric() - 1) %>%
    group_by(cluster) %>%
    summarise(
      Cluster_Turnover_Rate  = (base::sum(attrition_num) / base::length(attrition_num)) %>% scales::percent(accuracy = 0.1),
      Turnover_Count = base::sum(attrition_num),
      Cluster_Size = base::length(attrition_num)
    ) %>%
    ungroup() %>%
    
    mutate(Population_Turnover_Rate = (Turnover_Count / base::sum(Turnover_Count)) %>% 
             scales::percent(accuracy = 0.1))
  
  return(attrition_rate_tbl)
}


## Variables

df <- import.df()
corr.tbl <- import.corr.funnel(df)
corr.mat <- import.corr.mat(df)


## Plots

continous.plot <- function(variable){

  g = ggplot(df, aes_string(x = "Attrition", y = variable)) + 
    ggdist::stat_halfeye(
      aes(fill = Attrition),
      width = .6, 
      .width = 0, 
      justification = -.2, 
      point_colour = NA
    ) + 
    geom_boxplot(
      aes(color = Attrition),
      width = .15, 
      outlier.shape = NA
    ) +
    geom_point(
      aes(color = Attrition),
      shape = 95,
      size = 4,
      alpha = .2
    ) +
    coord_cartesian(xlim = c(1.2, NA), clip = "off") +
    theme_minimal() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14))

  return(g)
}


factor.plot <- function(variable){

  g = df[, c(variable, "Attrition")] %>%
    group_by(pick(everything())) %>%
    summarise(n = n()) %>%
    mutate(Percentage = n*100 / sum(n)) %>%
    ggplot(aes_string(x=variable, y="Percentage", fill="Attrition")) + 
    geom_bar(stat="identity") +
    geom_text(
      aes(y = Percentage, label = round(Percentage,1), group = Attrition),
      position = position_stack(vjust = .5), color = "white") +
    theme(panel.background = element_blank(),
          axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
    xlab("") +
    coord_flip() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14))

  return(g)
}


continous.plot2 <- function(df_clusters, variable){
  
  g = ggplot(
    df_clusters, 
    aes_string(x = variable, y = "as.factor(cluster)", fill = "after_stat(x)")
  ) +
    geom_density_ridges_gradient(scale = 2, size = 0.3, rel_min_height = 0.01) +
    scale_fill_viridis_c(name="", option = "C") +
    ylab('Cluster') +
    theme_minimal() +
    scale_fill_continuous(type = "gradient", trans = 'reverse') +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14)) +
    #labs(fill = variable) +
    guides(fill="none")
  
  return(g)
}


factor.plot2 <- function(df_clusters, variable){
  
  g = df_clusters[, c("cluster", variable)] %>%
    group_by(pick(everything())) %>%
    summarise(n = n()) %>%
    mutate(Percentage = n*100 / sum(n)) %>%
    ggplot(aes_string(x=variable, y="Percentage", fill=variable)) + 
    geom_bar(stat="identity") +
    geom_text(
      aes(y = Percentage, label = round(Percentage,1), group = variable),
      position = position_stack(vjust = .5), color = "white") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    facet_grid(rows = vars(cluster)) +
    
    theme(panel.background = element_blank(),
          axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
    coord_flip() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14)) +
    scale_fill_brewer(palette = "Dark2") +
    guides(fill="none") 
  
  return(g)
}


import.null.count <- function(df){
  
  na_count <- sapply(df, function(y) sum(length(which(is.na(y)))))
  
  return(sum(na_count))
}



desc.df <- data.frame(Variable = c('Age', 'Attrition', 'BusinessTravel', 'DailyRate',
                                   'Department', 'DistanceFromHome', 'Education', 'EducationField',
                                   'EnvironmentSatisfaction', 'Gender', 'HourlyRate', 'JobInvolvement',
                                   'JobLevel', 'JobRole', 'JobSatisfaction', 'MaritalStatus', 'MonthlyIncome',
                                   'MonthlyRate', 'NumCompaniesWorked', 'OverTime', 'PercentSalaryHike',
                                   'PerformanceRating', 'RelationshipSatisfaction', 'StandardHours',
                                   'StockOptionLevel', 'TotalWorkingYears', 'TrainingTimesLastYear',
                                   'WorkLifeBalance', 'YearsAtCompany', 'YearsInCurrentRole',
                                   'YearsSinceLastPromotion', 'YearsWithCurrManager'),
                      
                      Description = c('The age of the employee. (Numerical)', 'Whether or not the employee has left the organization. (Categorical)',
                                      'The frequency of business travel for the employee. (Categorical)', 'The daily rate of pay for the employee. (Numerical)',
                                      'The department the employee works in. (Categorical)', 'The distance from home in miles for the employee. (Numerical)',
                                      'The level of education achieved by the employee. (Categorical)', "The field of study for the employee's education. (Categorical)",
                                      "The employee's satisfaction with their work environment. (Categorical)", "The gender of the employee. (Categorical)",
                                      'The hourly rate of pay for the employee. (Numerical)', "The level of involvement required for the employee's job. (Categorical)",
                                      "The job level of the employee. (Categorical)", "The role of the employee in the organization. (Categorical)",
                                      "The employee's satisfaction with their job. (Categorical)", "The marital status of the employee. (Categorical)",
                                      "The monthly income of the employee. (Numerical)", "The monthly rate of pay for the employee. (Numerical)",
                                      "The number of companies the employee has worked for. (Numerical)", "Whether or not the employee works overtime. (Categorical)",
                                      "The percentage of salary hike for the employee. (Numerical)", "The performance rating of the employee. (Categorical)",
                                      "The employee's satisfaction with their relationships. (Categorical)", "The standard hours of work for the employee. (Numerical)",
                                      "The stock option level of the employee. (Numerical)", "The total number of years the employee has worked. (Numerical)",
                                      "The number of times the employee was taken for training in the last year. (Numerical)", "The employee's perception of their work-life balance. (Categorical)",
                                      "The number of years the employee has been with the company. (Numerical)", "The number of years the employee has been in their current role. (Numerical)",
                                      "The number of years since the employee's last promotion. (Numerical)", "The number of years the employee has been with their current manager. (Numerical)") )
