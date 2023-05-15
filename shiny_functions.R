import.df <- function(){
  df <- read.csv('./HR_Analytics.csv')
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
  
  sil_width <- map_dbl(2:10, function(k){
    model <- pam(gower_dist, k = k)
    model$silinfo$avg.width
  })
  
  sil_tbl <- tibble::tibble(
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
    coord_cartesian(xlim = c(1.2, NA), clip = "off") 

  return(g)
}


factor.plot <- function(variable){

  g = df[, c(variable, "Attrition")] %>%
    group_by(pick(everything())) %>%
    summarise(n = n()) %>%
    mutate(freq = n*100 / sum(n)) %>%
    ggplot(aes_string(x=variable, y="freq", fill="Attrition")) + 
    geom_bar(stat="identity") +
    geom_text(
      aes(y = freq, label = round(freq,1), group = Attrition),
      position = position_stack(vjust = .5), color = "white") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  return(g)
}


continous.plot2 <- function(df_clusters, variable){
  
  g = ggplot(
    df_clusters, 
    aes_string(x = variable, y = "as.factor(cluster)", fill = "stat(x)")
  ) +
    geom_density_ridges_gradient(scale = 2, size = 0.3, rel_min_height = 0.01) +
    scale_fill_viridis_c(name="", option = "C") +
    ylab('Cluster')
  
  return(g)
}


factor.plot2 <- function(df_clusters, variable){
  
  g = df_clusters[, c("cluster", variable)] %>%
    group_by(pick(everything())) %>%
    summarise(n = n()) %>%
    mutate(freq = n*100 / sum(n)) %>%
    ggplot(aes_string(x=variable, y="freq", fill=variable)) + 
    geom_bar(stat="identity") +
    geom_text(
      aes(y = freq, label = round(freq,1), group = variable),
      position = position_stack(vjust = .5), color = "white") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    facet_grid(rows = vars(cluster))
  
  return(g)
}


import.null.count <- function(df){
  
  na_count <- sapply(df, function(y) sum(length(which(is.na(y)))))
  
  return(sum(na_count))
}


